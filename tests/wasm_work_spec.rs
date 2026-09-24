//! Jobs, mixed socket/job waits, and coordinators across wasm targets.
//!
//! wasm-gc schedules host workers; WASI 0.2 retains inline begin. These
//! regressions pin shared lifecycle behavior and cross-backend replay while
//! allowing the scheduling races of independent jobs.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/loopback_peer.rs"]
mod loopback_peer;

use aver_cmd::{aver_bin, format_output, repo_root};
use loopback_peer::{free_port, loopback_peer};

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Instant, SystemTime, UNIX_EPOCH};

static UNIQUE: AtomicU64 = AtomicU64::new(0);

fn temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let n = UNIQUE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("aver-wasm-work-{prefix}-{nanos}-{n}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn fixture(name: &str) -> PathBuf {
    repo_root().join("tests/fixtures").join(name)
}

/// `aver run <fixture>/main.av` on one target. `target` is `&[]` for the VM,
/// `&["--wasm-gc"]` or `&["--wasip2"]` for a wasm one.
fn run(name: &str, target: &[&str], program_args: &[&str]) -> Result<String, String> {
    let dir = fixture(name);
    let mut command = Command::new(aver_bin());
    command
        .current_dir(repo_root())
        .arg("run")
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(&dir)
        .args(target);
    if !program_args.is_empty() {
        command.arg("--").args(program_args);
    }
    let out = command.output().expect("expected `aver run` to execute");
    if !out.status.success() {
        return Err(format!(
            "{name} on {} failed:\n{}",
            target_name(target),
            format_output(&out)
        ));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

fn target_name(target: &[&str]) -> &'static str {
    match target.first() {
        Some(&"--wasm-gc") => "wasm-gc",
        Some(&"--wasip2") => "wasip2",
        _ => "the VM",
    }
}

/// The wasm target printed exactly what the VM printed.
fn assert_same_stdout(name: &str, target: &[&str]) {
    let vm = run(name, &[], &[]).unwrap_or_else(|error| panic!("{error}"));
    let wasm = run(name, target, &[]).unwrap_or_else(|error| panic!("{error}"));
    assert_eq!(
        vm,
        wasm,
        "{name}: stdout differs between the VM and {}",
        target_name(target)
    );
}

/// The same, comparing the lines as a multiset.
///
/// A program whose processes wait on jobs or on wall-clock deadlines decides
/// the order of its own output by when each job settled, which is not a
/// property of the backend. What both backends owe is the same work.
fn same_lines(name: &str, vm: &str, wasm: &str) -> Result<(), String> {
    let sorted = |text: &str| {
        let mut lines = text.lines().collect::<Vec<_>>();
        lines.sort_unstable();
        lines.join("\n")
    };
    if sorted(vm) == sorted(wasm) {
        return Ok(());
    }
    Err(format!(
        "{name}: the two backends did different work\n--- VM ---\n{vm}\n--- wasm ---\n{wasm}"
    ))
}

// ── The job kinds ───────────────────────────────────────────────────────

/// Two jobs started off the turn, waited for in one wait set, and taken as
/// they land. Inline, both are done before the first wait, so the wait
/// reports both keys at once and the turn collects them in key order — the
/// same two lines the VM prints, whichever order its threads settled in.
#[test]
fn work_jobs_matches_the_vm_on_wasm_gc() {
    let vm = run("work_jobs", &[], &[]).unwrap_or_else(|error| panic!("{error}"));
    let wasm = run("work_jobs", &["--wasm-gc"], &[]).unwrap_or_else(|error| panic!("{error}"));
    same_lines("work_jobs", &vm, &wasm).unwrap_or_else(|error| panic!("{error}"));
}

#[cfg(feature = "wasip2")]
#[test]
fn work_jobs_matches_the_vm_on_wasip2() {
    let vm = run("work_jobs", &[], &[]).unwrap_or_else(|error| panic!("{error}"));
    let wasm = run("work_jobs", &["--wasip2"], &[]).unwrap_or_else(|error| panic!("{error}"));
    same_lines("work_jobs", &vm, &wasm).unwrap_or_else(|error| panic!("{error}"));
}

/// A nonblocking take observes a prefix of the same lifecycle on both hosts.
#[test]
fn immediate_takes_observe_the_job_lifecycle_on_wasm_gc() {
    let honest = [
        "nothing yet\nnothing yet",
        "nothing yet\nscore 5",
        "score 5\nwork: job already taken",
    ];
    for target in [&[][..], &["--wasm-gc"][..]] {
        let output = run("work_jobs_inline", target, &[]).unwrap_or_else(|error| panic!("{error}"));
        assert!(
            honest.contains(&output.as_str()),
            "unexpected lifecycle: {output}"
        );
    }
}

#[cfg(feature = "wasip2")]
#[test]
fn a_take_right_after_begin_answers_some_on_wasip2() {
    let wasm =
        run("work_jobs_inline", &["--wasip2"], &[]).unwrap_or_else(|error| panic!("{error}"));
    assert_eq!(wasm, "score 5\nwork: job already taken");
}

/// A job kind whose task is `Unit`. `Unit` has no wasm value at all, so the
/// task crosses as an `Option` the emitter writes no payload accessor for,
/// and both the run and the recording have to read it from the tag alone.
#[test]
fn a_unit_task_runs_and_records_on_wasm_gc() {
    let wasm =
        run("work_jobs_unit_task", &["--wasm-gc"], &[]).unwrap_or_else(|error| panic!("{error}"));
    assert_eq!(wasm, "tick 5");
    let ws = temp_dir("unit-task");
    let recordings = ws.join("recordings");
    fs::create_dir_all(&recordings).expect("create recordings dir");
    let result = (|| -> Result<(), String> {
        record("work_jobs_unit_task", &["--wasm-gc"], &recordings)?;
        let report = replay(&recordings, &["--wasm-gc"])?;
        if !report.contains("Output:  MATCH") {
            return Err(format!(
                "a Unit task's recording did not replay on wasm-gc:\n{report}"
            ));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// A job kind that names its dependencies' types, and those types reach
/// further ones: `Ledger.Request` holds a `Ledger.Origin`, `Ledger.Tx` holds
/// a `Meta.Info` written bare. The task crosses the boundary one way and the
/// answer the other, so both directions have to move a record the job kind's
/// own module never declares.
#[test]
fn a_job_kind_naming_nested_dependency_types_matches_the_vm_on_wasm_gc() {
    assert_same_stdout("work_jobs_dependency_types", &["--wasm-gc"]);
    let wasm = run("work_jobs_dependency_types", &["--wasm-gc"], &[])
        .unwrap_or_else(|error| panic!("{error}"));
    assert_eq!(wasm, "decoded block of 3 bytes from node");
}

#[cfg(feature = "wasip2")]
#[test]
fn a_unit_task_runs_on_wasip2() {
    let wasm =
        run("work_jobs_unit_task", &["--wasip2"], &[]).unwrap_or_else(|error| panic!("{error}"));
    assert_eq!(wasm, "tick 5");
}

/// `Work.cancel` drops the answer of a job nobody collected, so `take` says
/// `work: job cancelled` exactly as on the VM. Inline the countdown has
/// already run when the cancel lands — cancelling a job that is over is what
/// the VM's own `Work.cancel` does too, and what a program observes is the
/// answer.
#[test]
fn work_jobs_cancel_answers_as_the_vm_does_on_wasm_gc() {
    assert_same_stdout("work_jobs_cancel", &["--wasm-gc"]);
    let wasm =
        run("work_jobs_cancel", &["--wasm-gc"], &[]).unwrap_or_else(|error| panic!("{error}"));
    assert_eq!(wasm, "work: job cancelled");
}

#[cfg(feature = "wasip2")]
#[test]
fn work_jobs_cancel_answers_as_the_vm_does_on_wasip2() {
    assert_same_stdout("work_jobs_cancel", &["--wasip2"]);
}

/// Two job kinds over one module. `Work.Job` is one type, so a handle minted
/// by one kind type-checks as an argument to the other kind's `take`, and
/// only the runtime can tell them apart; the handle carries the kind that
/// minted it and the other kind refuses it in the VM's words.
#[test]
fn work_jobs_two_kinds_keeps_each_kind_to_its_own_handles_on_wasm_gc() {
    assert_same_stdout("work_jobs_two_kinds", &["--wasm-gc"]);
    let wasm =
        run("work_jobs_two_kinds", &["--wasm-gc"], &[]).unwrap_or_else(|error| panic!("{error}"));
    assert!(
        wasm.contains("work: this job was not started by job kind 'Beta'"),
        "expected the foreign-handle refusal, got:\n{wasm}"
    );
}

#[cfg(feature = "wasip2")]
#[test]
fn work_jobs_two_kinds_keeps_each_kind_to_its_own_handles_on_wasip2() {
    assert_same_stdout("work_jobs_two_kinds", &["--wasip2"]);
}

/// A job begun at the limit is queued, not refused: the wasm-gc host starts it
/// once the running one stops, and the program prints what the VM prints.
#[test]
fn work_jobs_limit_queues_on_wasm_gc() {
    assert_same_stdout("work_jobs_limit", &["--wasm-gc"]);
    let output =
        run("work_jobs_limit", &["--wasm-gc"], &[]).unwrap_or_else(|error| panic!("{error}"));
    assert!(
        output.contains("the second job was queued, not refused"),
        "{output}"
    );
}

/// `[work] max-jobs` decides nothing on wasip2: a job runs inline at `begin`
/// and is over before the next expression. The door says so, naming the key
/// and the target, and the program prints what the VM prints, because a
/// begin is never refused anywhere.
#[cfg(feature = "wasip2")]
#[test]
fn work_jobs_limit_says_the_manifest_key_changes_nothing_on_wasip2() {
    let dir = fixture("work_jobs_limit");
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(&dir)
        .arg("--wasip2")
        .output()
        .unwrap_or_else(|error| panic!("expected `aver run --wasip2` to execute: {error}"));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("warning[work-max-jobs-ignored]"),
        "the program door must say the key changes nothing:\n{}",
        format_output(&out)
    );
    assert!(stderr.contains("--wasip2"), "{stderr}");
    assert_same_stdout("work_jobs_limit", &["--wasip2"]);
}

/// A socket the host no longer knows is reported ready by the wait, on the
/// wasm-gc native host exactly as on the VM, so a request parked on a socket
/// another request closed does not end the run.
#[test]
fn a_closed_socket_in_a_wait_matches_the_vm_on_wasm_gc() {
    assert_same_stdout("run_closed_socket_wait", &["--wasm-gc"]);
}

/// One wait set holding a socket and a job together: the wait has to hand
/// the socket to the same reactor `Tcp.poll` uses and report the job's key
/// without ever calling the listener ready. The fixture binds port 0, so the
/// host picks a free port and the test cannot collide with anything else.
///
/// wasm-gc only: `Tcp.listen` is one of the seven operations wasip2 refuses.
#[test]
fn one_wait_set_over_a_socket_and_a_job_matches_the_vm_on_wasm_gc() {
    assert_same_stdout("work_jobs_socket_wait", &["--wasm-gc"]);
}

/// The same wait, keyed by a type the program declares instead of by an
/// arithmetic convention on whole numbers. `Wait.poll` takes a `Map<K,
/// Wait.Item>` for any key a map accepts, so the ready keys come back as
/// values the program matches on. Every backend has to agree about that: the
/// key crosses the provider boundary carried rather than read, and the
/// answer comes back in the order the program's own map puts its keys in.
#[test]
fn a_wait_keyed_by_a_program_type_matches_the_vm_on_wasm_gc() {
    assert_same_stdout("wait_keys_variant", &["--wasm-gc"]);
}

/// The same, keyed by a type a dependency module declares, with the wait set
/// written at the call. The key reaches the module's own boundary types by
/// name from the type the checker inferred for that map.
#[test]
fn a_wait_key_declared_in_a_dependency_module_matches_the_vm_on_wasm_gc() {
    assert_same_stdout("wait_key_in_dep", &["--wasm-gc"]);
}

/// A program that waits and binds no job kind at all. Before the wait's own
/// ABI was emitted for it, a compiled module carried no descriptor and no
/// helpers for its wait set, so an external host could not decode one.
#[test]
fn a_waiting_program_with_no_job_kind_matches_the_vm_on_wasm_gc() {
    assert_same_stdout("wait_socket_only", &["--wasm-gc"]);
}

// ── The generated coordinator ───────────────────────────────────────────

/// A job that never answers: the answer module that began it takes the
/// failure and answers the request with it, and the run goes on to its end.
///
/// wasm-gc only: the generated loop reads `Process.stopRequested`, which
/// WASI 0.2 has no binding for.
#[test]
fn run_failed_job_matches_the_vm_on_wasm_gc() {
    assert_same_stdout("run_failed_job", &["--wasm-gc"]);
}

/// Two job kinds and one keyed family under the generated loop, on wasm-gc:
/// one scorer per task, each waiting on a job its answer module began. The
/// VM lands the four tasks in wall-clock order, so the comparison is the
/// multiset of lines.
#[test]
fn two_job_kinds_under_one_generated_loop_match_the_vm_on_wasm_gc() {
    let name = "run_two_job_kinds";
    let vm = run(name, &[], &[]).unwrap_or_else(|error| panic!("{error}"));
    let wasm = run(name, &["--wasm-gc"], &[]).unwrap_or_else(|error| panic!("{error}"));
    same_lines(name, &vm, &wasm).unwrap_or_else(|error| panic!("{error}"));
}

/// Every answer function returns `Tuple<State, Result<R, Run.Wake>>`, and
/// every tuple gets an eager `List<Tuple<..>>` in case `List.zip` builds one.
/// Nothing compares those lists, so they must not demand an equality for a
/// state that has none: here the state holds a List, a Map and a store
/// handle only the provider can mint.
#[test]
fn an_answer_state_without_equality_compiles_on_wasm_gc() {
    let dir = fixture("run_answer_state_without_eq");
    let out_dir = temp_dir("answer-state-without-eq");
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("compile")
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(&dir)
        .args(["--target", "wasm-gc", "-o"])
        .arg(&out_dir)
        .output()
        .expect("expected `aver compile` to execute");
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        out_dir.join("main.wasm").is_file(),
        "{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(&out_dir);
}

/// The generated loop over five processes, three answer modules, two
/// policies and a job kind the answer module begins itself, answering `Wire`
/// over real sockets.
///
/// The comparison is the multiset of lines, as it is for the Rust backend
/// and for the same reason: the slice's answer modules park requests on
/// wall-clock deadlines, so which turn a finished job lands in depends on
/// how long the job took. Each backend gets its own loopback peer on its own
/// free port, so parity here is parity over a real socket conversation.
#[test]
fn run_all_slice_does_the_same_work_as_the_vm_on_wasm_gc() {
    let vm = with_peer("VM", |port| run("run_all_slice", &[], &[port]))
        .unwrap_or_else(|error| panic!("{error}"));
    let wasm = with_peer("wasm-gc", |port| {
        run("run_all_slice", &["--wasm-gc"], &[port])
    })
    .unwrap_or_else(|error| panic!("{error}"));
    same_lines("run_all_slice", &vm, &wasm).unwrap_or_else(|error| panic!("{error}"));
}

/// Runs one backend against a loopback peer, on a port nobody else holds.
fn with_peer(
    backend: &str,
    run: impl FnOnce(&str) -> Result<String, String>,
) -> Result<String, String> {
    let port = free_port();
    let peer = loopback_peer(port);
    let text = port.to_string();
    let started = Instant::now();
    let ran = run(&text);
    let elapsed = started.elapsed();
    let played = peer.join();
    let out = ran?;
    played.map_err(|_| {
        format!("the loopback peer did not play its whole part; {backend} ran for {elapsed:?}; program output:\n{out}")
    })?;
    Ok(out)
}

// ── Recording and replay ────────────────────────────────────────────────

/// A recording made on the VM, replayed on wasm-gc.
///
/// The recorded answers come back in the recorded turns: `begin` hands back
/// the handle it recorded, `Wait.poll` the keys it reported, `take` the score
/// it produced. The VM's first wait reported one job and the second the
/// other, and the replay keeps that shape even though inline both jobs were
/// over before the first wait.
#[test]
fn a_vm_recording_replays_on_wasm_gc() {
    let ws = temp_dir("replay-forward");
    let recordings = ws.join("recordings");
    fs::create_dir_all(&recordings).expect("create recordings dir");
    let result = (|| -> Result<(), String> {
        record("work_jobs", &[], &recordings)?;
        let report = replay(&recordings, &["--wasm-gc"])?;
        if !report.contains("Output:  MATCH") {
            return Err(format!(
                "the VM's recording did not replay on wasm-gc:\n{report}"
            ));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// The other direction: a recording made on wasm-gc, replayed by the VM,
/// effect for effect. The whole job is in it — `begin` with its task and the
/// handle it minted, `Wait.poll` with its keys, `take` with its answer — and
/// the VM reads it without knowing which backend wrote it.
#[test]
fn a_wasm_gc_recording_replays_on_the_vm() {
    let ws = temp_dir("replay-reverse");
    let recordings = ws.join("recordings");
    fs::create_dir_all(&recordings).expect("create recordings dir");
    let result = (|| -> Result<(), String> {
        record("work_jobs", &["--wasm-gc"], &recordings)?;
        let session = one_recording(&recordings)?;
        let text = fs::read_to_string(&session)
            .map_err(|error| format!("cannot read the recording: {error}"))?;
        for expected in [
            "\"Validation.begin\"",
            "\"Wait.poll\"",
            "\"$capabilityResource\"",
        ] {
            if !text.contains(expected) {
                return Err(format!(
                    "the wasm-gc recording is missing the job it was made for, expected {expected}:\n{text}"
                ));
            }
        }
        let report = replay(&recordings, &[])?;
        if !report.contains("Output:  MATCH") {
            return Err(format!(
                "the VM could not replay the wasm-gc recording:\n{report}"
            ));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// A job kind whose task and answer are records the capability owns.
/// `Scorer.Task`/`Scorer.Report` cross the boundary under their canonical
/// names, inline the same as on the VM, and the recording they make replays
/// back on wasm-gc.
#[test]
fn record_valued_jobs_run_record_and_replay_on_wasm_gc() {
    let wasm =
        run("work_jobs_record", &["--wasm-gc"], &[]).unwrap_or_else(|error| panic!("{error}"));
    for expected in ["scored 10 for alpha", "scored 24 for beta-two"] {
        assert!(wasm.contains(expected), "expected {expected:?} in:\n{wasm}");
    }
    let ws = temp_dir("record-jobs");
    let recordings = ws.join("recordings");
    fs::create_dir_all(&recordings).expect("create recordings dir");
    let result = (|| -> Result<(), String> {
        record("work_jobs_record", &["--wasm-gc"], &recordings)?;
        let session = one_recording(&recordings)?;
        let text = fs::read_to_string(&session)
            .map_err(|error| format!("cannot read the recording: {error}"))?;
        for expected in ["\"Scorer.Task\"", "\"Scorer.Report\""] {
            if !text.contains(expected) {
                return Err(format!(
                    "the wasm-gc recording must spell the capability's records canonically, expected {expected}:\n{text}"
                ));
            }
        }
        let report = replay(&recordings, &["--wasm-gc"])?;
        if !report.contains("Output:  MATCH") {
            return Err(format!(
                "a record-valued job's recording did not replay on wasm-gc:\n{report}"
            ));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// The interchange both ways over capability-owned records: the VM's
/// canonical tags replay on wasm-gc, and a wasm-gc recording replays on the
/// VM — each backend reading tags it did not write.
#[test]
fn record_valued_job_recordings_interchange_between_the_vm_and_wasm_gc() {
    let ws = temp_dir("record-interchange");
    let forward = ws.join("forward");
    let reverse = ws.join("reverse");
    fs::create_dir_all(&forward).expect("create forward dir");
    fs::create_dir_all(&reverse).expect("create reverse dir");
    let result = (|| -> Result<(), String> {
        record("work_jobs_record", &[], &forward)?;
        let report = replay(&forward, &["--wasm-gc"])?;
        if !report.contains("Output:  MATCH") {
            return Err(format!(
                "the VM's recording did not replay on wasm-gc:\n{report}"
            ));
        }
        record("work_jobs_record", &["--wasm-gc"], &reverse)?;
        let report = replay(&reverse, &[])?;
        if !report.contains("Output:  MATCH") {
            return Err(format!(
                "the wasm-gc recording did not replay on the VM:\n{report}"
            ));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// `aver run <fixture> [target] --record <dir>`.
fn record(name: &str, target: &[&str], dir: &Path) -> Result<(), String> {
    let fixture_dir = fixture(name);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(fixture_dir.join("main.av"))
        .arg("--module-root")
        .arg(&fixture_dir)
        .args(target)
        .arg("--record")
        .arg(dir)
        .output()
        .expect("expected `aver run --record` to execute");
    if !out.status.success() {
        return Err(format!(
            "recording {name} on {} failed:\n{}",
            target_name(target),
            format_output(&out)
        ));
    }
    Ok(())
}

/// `aver replay <dir> --test [target]`.
fn replay(dir: &Path, target: &[&str]) -> Result<String, String> {
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("replay")
        .arg(dir)
        .arg("--test")
        .args(target)
        .output()
        .expect("expected `aver replay` to execute");
    let report = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    if !out.status.success() {
        return Err(report);
    }
    Ok(report)
}

/// The one recording `aver run --record` wrote into `dir`.
fn one_recording(dir: &Path) -> Result<PathBuf, String> {
    let mut sessions = fs::read_dir(dir)
        .map_err(|error| format!("cannot read the recordings directory: {error}"))?
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|path| path.extension().is_some_and(|ext| ext == "json"))
        .collect::<Vec<_>>();
    sessions.sort();
    match sessions.len() {
        1 => Ok(sessions.remove(0)),
        other => Err(format!("expected exactly one recording, found {other}")),
    }
}
