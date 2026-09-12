//! Jobs, the one wait and the generated coordinator on the Rust backend
//! (jasisz/aver#1329).
//!
//! Everything the VM runs since the interception change runs here too, so
//! every case is the same shape: compile one `tests/fixtures/work_*` or
//! `run_all_slice` program with `--target rust`, `cargo build` the crate it
//! emits, run the binary, and compare with `aver run` on the same program.
//! A `cargo check` would prove nothing here — the job body is a thread over
//! a generated function, the job handle crosses three capabilities, and the
//! answers a program reads are the engine's, none of which a type-check
//! reaches.
//!
//! Gated on `runtime` (the default feature set) — needs the `aver` binary
//! plus the local `aver-rt` the generated `Cargo.toml` pins.

#![cfg(feature = "runtime")]

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
use std::time::{SystemTime, UNIX_EPOCH};

static UNIQUE: AtomicU64 = AtomicU64::new(0);

fn temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let n = UNIQUE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("aver-rust-work-{prefix}-{nanos}-{n}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

/// One `cargo build` target directory for this whole suite, so the runtime
/// dependency tree is compiled once and every later fixture is seconds. It is
/// this suite's own: a target directory shared with another test binary races
/// on its `.rmeta` outputs when both run under one `cargo test`.
fn shared_target_dir() -> PathBuf {
    repo_root().join("target").join("rust-work-spec-shared")
}

fn fixture(name: &str) -> PathBuf {
    repo_root().join("tests/fixtures").join(name)
}

/// `aver run <fixture>/main.av` on the VM — the parity oracle.
fn run_vm(name: &str) -> Result<String, String> {
    run_vm_with(name, &[])
}

/// The same, with arguments the program reads through `Args.get`.
fn run_vm_with(name: &str, program_args: &[&str]) -> Result<String, String> {
    let dir = fixture(name);
    let mut command = Command::new(aver_bin());
    command
        .current_dir(repo_root())
        .arg("run")
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(&dir);
    if !program_args.is_empty() {
        command.arg("--").args(program_args);
    }
    let out = command.output().expect("expected `aver run` to execute");
    if !out.status.success() {
        return Err(format!("VM run of {name} failed:\n{}", format_output(&out)));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

/// `aver compile <fixture>/main.av --target rust` into `project`.
fn compile_rust(
    name: &str,
    project: &Path,
    crate_name: &str,
    extra: &[&str],
) -> Result<(), String> {
    let dir = fixture(name);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("compile")
        .arg(dir.join("main.av"))
        .arg("--target")
        .arg("rust")
        .arg("--name")
        .arg(crate_name)
        .arg("-o")
        .arg(project)
        .arg("--module-root")
        .arg(&dir)
        .args(extra)
        .output()
        .expect("expected `aver compile --target rust` to spawn");
    if !out.status.success() {
        return Err(format!(
            "aver compile --target rust of {name} failed:\n{}",
            format_output(&out)
        ));
    }
    Ok(())
}

/// A real `cargo build` (not `cargo check`) of the emitted crate.
fn cargo_build(project: &Path, crate_name: &str) -> Result<PathBuf, String> {
    let target = shared_target_dir();
    fs::create_dir_all(&target).expect("create cargo target dir");
    let out = Command::new("cargo")
        .arg("build")
        .arg("-q")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(project.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", &target)
        .output()
        .expect("expected `cargo build` to spawn");
    if !out.status.success() {
        return Err(format!(
            "cargo build failed on the crate emitted for {crate_name}:\n{}",
            format_output(&out)
        ));
    }
    Ok(target
        .join("debug")
        .join(format!("{crate_name}{}", std::env::consts::EXE_SUFFIX)))
}

fn run_binary(bin: &Path) -> Result<String, String> {
    run_binary_with(bin, &[])
}

/// The same, with arguments the built binary reads through `Args.get`.
fn run_binary_with(bin: &Path, program_args: &[&str]) -> Result<String, String> {
    let out = Command::new(bin)
        .args(program_args)
        .output()
        .map_err(|error| format!("failed to run {}: {error}", bin.display()))?;
    if !out.status.success() {
        return Err(format!(
            "{} exited non-zero:\n{}",
            bin.display(),
            format_output(&out)
        ));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

/// Compile, build and run one fixture, handing its stdout and the VM's to
/// `compare`. The workspace is removed whatever the comparison says.
fn against_the_vm(
    name: &str,
    extra: &[&str],
    compare: impl Fn(&str, &str) -> Result<(), String>,
) -> Result<(), String> {
    let vm = run_vm(name)?;
    let ws = temp_dir(name);
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(name, &project, name, extra)?;
        let bin = cargo_build(&project, name)?;
        let rust = run_binary(&bin)?;
        compare(&vm, &rust)
    })();
    let _ = fs::remove_dir_all(&ws);
    result
}

fn assert_same_stdout(name: &str) {
    against_the_vm(name, &[], |vm, rust| {
        if vm == rust {
            return Ok(());
        }
        Err(format!(
            "{name}: stdout mismatch\n--- VM ---\n{vm}\n--- Rust ---\n{rust}"
        ))
    })
    .unwrap_or_else(|error| panic!("{error}"));
}

/// The same, comparing the lines as a multiset.
///
/// A program whose processes wait on jobs or on wall-clock deadlines decides
/// the order of its own output by when each job settled, which is not a
/// property of the backend: the same program reorders itself on one backend
/// under load. What both backends owe is the same work.
fn assert_same_lines(name: &str) {
    against_the_vm(name, &[], |vm, rust| same_lines(name, vm, rust))
        .unwrap_or_else(|error| panic!("{error}"));
}

/// The two runs did the same work when they printed the same lines, in
/// whatever order each of them settled on.
fn same_lines(name: &str, vm: &str, rust: &str) -> Result<(), String> {
    let sorted = |text: &str| {
        let mut lines = text.lines().collect::<Vec<_>>();
        lines.sort_unstable();
        lines.join("\n")
    };
    if sorted(vm) == sorted(rust) {
        return Ok(());
    }
    Err(format!(
        "{name}: the two backends did different work\n--- VM ---\n{vm}\n--- Rust ---\n{rust}"
    ))
}

// ── The job kinds ───────────────────────────────────────────────────────

/// Two jobs started off the turn, waited for in one wait set, and taken as
/// they land. This is the whole seam in one program: `begin` mints a handle
/// through the generated adapter, `Wait.poll` watches it through the engine,
/// and `take` answers `Ok(None)` until the job settles.
///
/// The two jobs settle independently and the wait reports whatever is ready,
/// so which score prints first is a race the program itself allows — on
/// either backend. The multiset of scores is the claim.
#[test]
fn work_jobs_matches_the_vm() {
    assert_same_lines("work_jobs");
}

/// `Work.cancel` on the Rust backend sets the job's cancellation flag and
/// drops its answer, so `take` says `work: job cancelled` exactly as on the
/// VM. The thread itself runs to completion: generated Rust carries no
/// cancellation check, so cancelling detaches the work rather than stopping
/// it. The fixture's countdown is long enough that a job which really had
/// stopped and one which merely detached are indistinguishable to the
/// program — which is the point: what a program observes is the answer.
#[test]
fn work_jobs_cancel_detaches_and_answers_as_the_vm_does() {
    against_the_vm("work_jobs_cancel", &[], |vm, rust| {
        if vm != rust {
            return Err(format!(
                "work_jobs_cancel: stdout mismatch\n--- VM ---\n{vm}\n--- Rust ---\n{rust}"
            ));
        }
        if rust != "work: job cancelled" {
            return Err(format!(
                "work_jobs_cancel: expected the cancelled-job answer, got {rust:?}"
            ));
        }
        Ok(())
    })
    .unwrap_or_else(|error| panic!("{error}"));
}

/// One wait set holding a socket and a job together. This is the crossing
/// the relaxed resource codec exists for: a `Wait.Item.Socket` carries a
/// `Tcp` resource into the `Wait` provider, so the Rust backend must hand a
/// listener minted by one capability to a poll answered by another, and
/// report the job's key without ever calling the listener ready. The fixture
/// binds port 0, so the host picks a free port and the test cannot collide
/// with anything else running on the machine.
#[test]
fn one_wait_set_over_a_socket_and_a_job_matches_the_vm() {
    assert_same_stdout("work_jobs_socket_wait");
}

/// At `[work] max-jobs = 1` a second `begin` refuses instead of blocking the
/// turn, with the engine's own message. The limit reaches the generated
/// bootstrap from the manifest at compile time, so this also proves the
/// manifest key crossed into the artifact.
#[test]
fn work_jobs_limit_matches_the_vm() {
    assert_same_stdout("work_jobs_limit");
}

/// Two job kinds over one engine. `Work.Job` is one type, so a handle minted
/// by one kind type-checks as an argument to the other kind's `take`, and
/// only the runtime can tell them apart; the Rust adapter remembers what it
/// started and refuses the rest in the VM's words.
#[test]
fn work_jobs_two_kinds_keeps_each_kind_to_its_own_handles() {
    against_the_vm("work_jobs_two_kinds", &[], |vm, rust| {
        if vm != rust {
            return Err(format!(
                "work_jobs_two_kinds: stdout mismatch\n--- VM ---\n{vm}\n--- Rust ---\n{rust}"
            ));
        }
        if !rust.contains("work: this job was not started by job kind 'Beta'") {
            return Err(format!(
                "work_jobs_two_kinds: expected the foreign-handle refusal, got:\n{rust}"
            ));
        }
        Ok(())
    })
    .unwrap_or_else(|error| panic!("{error}"));
}

// ── A capability the program answers ────────────────────────────────────

/// A program that answers a capability of its own, without asking for a
/// generated loop: the processes lower to state types and pure answer
/// functions, and the coordinator is the one the program wrote. Nothing here
/// reaches a job, so this is the other half of the refusal that was lifted —
/// the reply sums carrying `Wait.Wake` compile and run.
#[test]
fn an_answered_capability_matches_the_vm() {
    for fixture in ["yield_spike", "yield_continuations", "yield_cross_module"] {
        assert_same_stdout(fixture);
    }
}

/// The generated coordinator: five processes, five answer modules, three
/// policies and a job seam, all on the Rust backend.
///
/// The comparison is the multiset of lines, not their order, and that is a
/// statement about the program rather than a weakened assertion. Its answer
/// modules park requests on `Wait.Wake.After(2)` and `After(5)` — wall-clock
/// deadlines — so which turn a finished job lands in depends on how long the
/// job took. The VM runs a job body on a child VM and the Rust backend runs
/// the compiled function itself, which is faster than the smallest deadline
/// in the program, so the two interleave the same work differently. Both
/// run every process to its end and stop.
///
/// The slice answers `Wire` over real sockets, so each backend gets its own
/// loopback peer on its own free port: parity here is parity over a real
/// socket conversation, and what both backends owe is the same bodies
/// fetched in the same number of asks.
#[test]
fn run_all_slice_does_the_same_work_as_the_vm() {
    let name = "run_all_slice";
    let result = (|| {
        let vm = with_peer(|port| run_vm_with(name, &[port]))?;
        let ws = temp_dir(name);
        let project = ws.join("project");
        fs::create_dir_all(&project).expect("create project dir");
        let compared = (|| {
            compile_rust(name, &project, name, &[])?;
            let bin = cargo_build(&project, name)?;
            let rust = with_peer(|port| run_binary_with(&bin, &[port]))?;
            same_lines(name, &vm, &rust)
        })();
        let _ = fs::remove_dir_all(&ws);
        compared
    })();
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// Two job kinds under one generated coordinator on the Rust backend: one
/// `__Job` sum, one shared table, one shared `max-jobs` limit. The four
/// tasks' settle order is wall-clock, so the comparison is the multiset of
/// lines — and this program prints one line, the score that only all four
/// landings make.
#[test]
fn two_job_kinds_under_one_generated_loop_matches_the_vm() {
    assert_same_lines("run_two_job_kinds");
}

/// Runs one backend against a loopback peer, on a port nobody else holds.
fn with_peer(run: impl FnOnce(&str) -> Result<String, String>) -> Result<String, String> {
    let port = free_port();
    let peer = loopback_peer(port);
    let text = port.to_string();
    let ran = run(&text);
    let played = peer.join();
    let out = ran?;
    played.map_err(|_| "the loopback peer did not play its whole part".to_string())?;
    Ok(out)
}

// ── Recording and replay ────────────────────────────────────────────────

/// A recording made on the VM, replayed by the built Rust binary.
///
/// The recorded answers come back in the recorded turns: `begin` hands back
/// the handle it recorded, `Wait.poll` the keys it reported, `take` the score
/// it produced. The Rust backend does not recompute the job while replaying
/// it, so nothing here depends on a job being started at all.
#[test]
fn a_vm_recording_replays_on_the_rust_backend() {
    let ws = temp_dir("replay-forward");
    let project = ws.join("project");
    let recordings = ws.join("recordings");
    fs::create_dir_all(&project).expect("create project dir");
    fs::create_dir_all(&recordings).expect("create recordings dir");

    let result = (|| -> Result<(), String> {
        let dir = fixture("work_jobs");
        let recorded = Command::new(aver_bin())
            .current_dir(repo_root())
            .arg("run")
            .arg(dir.join("main.av"))
            .arg("--module-root")
            .arg(&dir)
            .arg("--record")
            .arg(&recordings)
            .output()
            .expect("expected `aver run --record` to execute");
        if !recorded.status.success() {
            return Err(format!(
                "recording the VM run failed:\n{}",
                format_output(&recorded)
            ));
        }
        let session = one_recording(&recordings)?;

        compile_rust(
            "work_jobs",
            &project,
            "work_jobs_replay",
            &["--with-replay"],
        )?;
        let bin = cargo_build(&project, "work_jobs_replay")?;
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|error| format!("failed to run the replaying binary: {error}"))?;
        if !replayed.status.success() {
            return Err(format!(
                "the Rust binary could not replay the VM's recording:\n{}",
                format_output(&replayed)
            ));
        }
        let stdout = String::from_utf8_lossy(&replayed.stdout);
        for expected in ["job 1 scored 5", "job 2 scored 8"] {
            if !stdout.contains(expected) {
                return Err(format!(
                    "the replay did not serve the recorded answers, expected {expected:?} in:\n{}",
                    format_output(&replayed)
                ));
            }
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// The other direction: a recording made by the Rust binary, replayed by the
/// VM, effect for effect.
///
/// The effect stream itself carries across untouched. What does not is the
/// recording's header: a generated binary has no idea which source file it
/// was compiled from, so `program_file` is empty and `module_root` is `"."`,
/// and `aver replay` needs both to load the program it is replaying. That is
/// a property of the generated replay runtime and not of jobs, so the test
/// fills the two fields in and then asserts the whole session matches —
/// including the job handles, whose trace tokens the two backends number
/// from different starting points.
#[test]
fn a_rust_recording_replays_on_the_vm() {
    let ws = temp_dir("replay-reverse");
    let project = ws.join("project");
    let recordings = ws.join("recordings");
    fs::create_dir_all(&project).expect("create project dir");
    fs::create_dir_all(&recordings).expect("create recordings dir");

    let result = (|| -> Result<(), String> {
        compile_rust(
            "work_jobs",
            &project,
            "work_jobs_record",
            &["--with-replay"],
        )?;
        let bin = cargo_build(&project, "work_jobs_record")?;
        let session = recordings.join("session.json");
        let recorded = Command::new(&bin)
            .env("AVER_REPLAY_RECORD", &session)
            .output()
            .map_err(|error| format!("failed to run the recording binary: {error}"))?;
        if !recorded.status.success() {
            return Err(format!(
                "the Rust binary could not record its run:\n{}",
                format_output(&recorded)
            ));
        }

        let text = fs::read_to_string(&session).map_err(|error| {
            format!("the Rust binary wrote no recording at {session:?}: {error}")
        })?;
        if !text.contains("\"Validation.begin\"") || !text.contains("\"$capabilityResource\"") {
            return Err(format!(
                "the recording is missing the job seam it was made for:\n{text}"
            ));
        }
        let located = text
            .replace(
                "\"program_file\": \"\"",
                "\"program_file\": \"tests/fixtures/work_jobs/main.av\"",
            )
            .replace(
                "\"module_root\": \".\"",
                "\"module_root\": \"tests/fixtures/work_jobs\"",
            );
        if located == text {
            return Err(format!(
                "the recording no longer carries the empty header this test fills in:\n{text}"
            ));
        }
        fs::write(&session, located).expect("write the located recording");

        let replayed = Command::new(aver_bin())
            .current_dir(repo_root())
            .arg("replay")
            .arg(&recordings)
            .arg("--test")
            .output()
            .expect("expected `aver replay` to execute");
        let report = format!(
            "{}{}",
            String::from_utf8_lossy(&replayed.stdout),
            String::from_utf8_lossy(&replayed.stderr)
        );
        if !replayed.status.success() || !report.contains("Output:  MATCH") {
            return Err(format!(
                "the VM could not replay the Rust binary's recording:\n{}",
                format_output(&replayed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
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
