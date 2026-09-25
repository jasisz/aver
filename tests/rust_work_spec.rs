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
    compile_rust_at(&dir.join("main.av"), &dir, project, crate_name, extra)
}

/// `aver compile <source> --target rust --module-root <module_root>` into
/// `project`. `source` and `module_root` go to the compiler exactly as
/// given — relative paths resolve against the repo root, which is what a
/// user compiling from their shell hands it.
fn compile_rust_at(
    source: &Path,
    module_root: &Path,
    project: &Path,
    crate_name: &str,
    extra: &[&str],
) -> Result<(), String> {
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("compile")
        .arg(source)
        .arg("--target")
        .arg("rust")
        .arg("--name")
        .arg(crate_name)
        .arg("-o")
        .arg(project)
        .arg("--module-root")
        .arg(module_root)
        .args(extra)
        .output()
        .expect("expected `aver compile --target rust` to spawn");
    if !out.status.success() {
        return Err(format!(
            "aver compile --target rust of {} failed:\n{}",
            source.display(),
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

/// The same wait, keyed by a type the program declares rather than by an
/// arithmetic convention on whole numbers. `Wait.poll` takes a `Map<K,
/// Wait.Item>` for any key a map accepts, so the generated crate has to name
/// that key in the operation's signature and emit a provider codec for it
/// beside the program's own types. The key crosses the provider boundary
/// carried rather than read, and the ready key comes back as a value the
/// program matches on.
#[test]
fn a_wait_keyed_by_a_program_type_matches_the_vm() {
    assert_same_stdout("wait_keys_variant");
}

/// The same, with the key type declared in a dependency module and the wait
/// set written at the call rather than bound to an annotated name. Nothing in
/// the entry module names `Watch`, so the generated crate has to reach the
/// type through the module that declares it and emit its codec there. The key
/// travels by name: an identity resolved in one table is not an identity in
/// another, and carrying one into the other is how this shape used to abort
/// the compile.
#[test]
fn a_wait_key_declared_in_a_dependency_module_matches_the_vm() {
    assert_same_stdout("wait_key_in_dep");
}

/// A job kind whose task and answer are a dependency's records carrying
/// `Bytes`, a `List<Bytes>` and a `Map<Bytes, Ledger.Chunk>`.
#[test]
fn a_job_kind_carrying_bytes_inside_dependency_types_matches_the_vm() {
    assert_same_stdout("work_jobs_dependency_bytes");
}

/// At `[work] max-jobs = 1` a second `begin` is queued instead of refused,
/// and starts once the first one stops. Generated Rust checks no cancellation
/// flag, so there the cancelled first job runs to its end before the queued
/// one starts; what the program prints is the same.
#[test]
fn work_jobs_limit_matches_the_vm() {
    assert_same_stdout("work_jobs_limit");
}

/// A request parked on a socket that another request closed: the next wait
/// reports the closed socket ready rather than failing, the parked request
/// learns it is gone, and the run ends normally, as on the VM.
#[test]
fn a_closed_socket_in_a_wait_matches_the_vm() {
    assert_same_stdout("run_closed_socket_wait");
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

/// Capability-owned records and a worker imported through nested module paths.
/// This is the same boundary used by the btc-listener consumer.
#[test]
fn nested_record_job_matches_the_vm() {
    assert_same_lines("work_jobs_nested_record");
}

// ── A capability the program answers ────────────────────────────────────

/// A program that answers a capability of its own and drives the protocol
/// from a `main` of its own: the processes lower to state types and pure
/// answer functions, and the coordinator is the one the program wrote.
#[test]
fn an_answered_capability_matches_the_vm() {
    for fixture in ["yield_spike", "yield_continuations", "yield_cross_module"] {
        assert_same_stdout(fixture);
    }
}

/// The generated loop: five processes, three answer modules, two policies and
/// a job kind the answer module begins itself, all on the Rust backend.
///
/// The comparison is the multiset of lines, not their order, and that is a
/// statement about the program rather than a weakened assertion. Its answer
/// modules park requests on wall-clock deadlines and on jobs, so which turn a
/// finished job lands in depends on how long the job took. The VM runs a job body on a child VM and the Rust backend runs
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

/// Two job kinds and one keyed family under the generated loop on the Rust
/// backend, sharing one `max-jobs` limit. The four tasks' settle order is
/// wall-clock, so the comparison is the multiset of lines: one per landing,
/// kind and score, so a task started twice would show as a duplicated line,
/// and the sum that only all four landings make.
#[test]
fn two_job_kinds_under_one_generated_loop_matches_the_vm() {
    assert_same_lines("run_two_job_kinds");
}

/// A keyed family seated per key and dropped when its key leaves, and a loop
/// run from a `main` of the program's own, on the Rust backend.
#[test]
fn keyed_families_and_run_all_match_the_vm() {
    assert_same_stdout("run_families");
}

/// The state an answer module holds reaches its answer function uniquely
/// owned, so every request's `Map.set` updates the Map in place instead of
/// copying it.
///
/// The loop hands the state out of the run (`__takeOwner`) before the answer
/// function sees it, and every function on the way there takes the run by
/// value. Were any of them to borrow it, the caller's copy would still hold
/// the Map during the answer, and each request would copy all of it.
#[test]
fn an_answer_modules_state_reaches_its_answer_function_uniquely_owned() {
    let name = "run_owned_answer_state";
    let ws = temp_dir(name);
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(name, &project, name, &[])?;
        let entry = fs::read_to_string(project.join("src/aver_generated/entry/mod.rs"))
            .map_err(|error| format!("read the generated entry module: {error}"))?;
        for by_value in [
            "pub fn __serveIf(mut run @ _: __Run,",
            "pub fn __serve(mut run @ _: __Run,",
            "pub fn __serveTicker(mut run @ _: __Run,",
            "pub fn __takeOwner(mut run @ _: __Run)",
            "let (__rest, __held) = __takeOwner(run);",
            "crate::aver_generated::owner::bump(__taken, __a0)",
        ] {
            if !entry.contains(by_value) {
                return Err(format!(
                    "{name}: the generated loop no longer hands the state over by value; missing `{by_value}` in:\n{entry}"
                ));
            }
        }
        let vm = run_vm(name)?;
        let bin = cargo_build(&project, name)?;
        let rust = run_binary(&bin)?;
        if vm != rust {
            return Err(format!(
                "{name}: stdout mismatch\n--- VM ---\n{vm}\n--- Rust ---\n{rust}"
            ));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
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
/// The binary writes the source it was compiled from into the recording's
/// header: the module root made absolute at compile time, and the program
/// file relative to that root, so it may run from a directory that is
/// neither the compile directory nor the project it was built in and `aver
/// replay` still loads the program with no header edits. The assertion is
/// the whole session matching — including the job handles, whose trace
/// tokens the two backends number from different starting points.
#[test]
fn a_rust_recording_replays_on_the_vm() {
    let ws = temp_dir("replay-reverse");
    let project = ws.join("project");
    let recordings = ws.join("recordings");
    let elsewhere = ws.join("elsewhere");
    fs::create_dir_all(&project).expect("create project dir");
    fs::create_dir_all(&recordings).expect("create recordings dir");
    fs::create_dir_all(&elsewhere).expect("create elsewhere dir");

    let result = (|| -> Result<(), String> {
        compile_rust_at(
            Path::new("tests/fixtures/work_jobs/main.av"),
            Path::new("tests/fixtures/work_jobs"),
            &project,
            "work_jobs_record",
            &["--with-replay"],
        )?;
        let bin = cargo_build(&project, "work_jobs_record")?;
        let session = recordings.join("session.json");
        let recorded = Command::new(&bin)
            .current_dir(&elsewhere)
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
        let header = aver::replay::session::parse_session_recording(&text)
            .map_err(|error| format!("the recording does not parse: {error}\n{text}"))?;
        if header.program_file != "main.av" {
            return Err(format!(
                "expected program_file \"main.av\", got {:?}",
                header.program_file
            ));
        }
        let module_root = Path::new(&header.module_root);
        if !module_root.is_absolute() {
            return Err(format!(
                "expected an absolute module_root, got {:?}",
                header.module_root
            ));
        }
        if !module_root.join(&header.program_file).is_file() {
            return Err(format!(
                "module_root + program_file does not name the compiled source: {} + {}",
                header.module_root, header.program_file
            ));
        }

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

/// A recording of `work_jobs_record` — whose task and answer are records the
/// capability owns — replayed by the built Rust binary. The binary reads the
/// canonical `Scorer.Task`/`Scorer.Report` tags the VM's ledger carries.
#[test]
fn a_vm_recording_of_record_valued_jobs_replays_on_the_rust_backend() {
    let ws = temp_dir("replay-record-forward");
    let project = ws.join("project");
    let recordings = ws.join("recordings");
    fs::create_dir_all(&project).expect("create project dir");
    fs::create_dir_all(&recordings).expect("create recordings dir");

    let result = (|| -> Result<(), String> {
        let dir = fixture("work_jobs_record");
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
            "work_jobs_record",
            &project,
            "work_jobs_record_replay",
            &["--with-replay"],
        )?;
        let bin = cargo_build(&project, "work_jobs_record_replay")?;
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
        for expected in ["job 1 scored 10 for alpha", "job 2 scored 24 for beta-two"] {
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

/// The other direction over capability-owned records: the binary writes the
/// canonical `Scorer.Task`/`Scorer.Report` tags, and the VM replays the
/// session effect for effect.
#[test]
fn a_rust_recording_of_record_valued_jobs_replays_on_the_vm() {
    let ws = temp_dir("replay-record-reverse");
    let project = ws.join("project");
    let recordings = ws.join("recordings");
    fs::create_dir_all(&project).expect("create project dir");
    fs::create_dir_all(&recordings).expect("create recordings dir");

    let result = (|| -> Result<(), String> {
        compile_rust(
            "work_jobs_record",
            &project,
            "work_jobs_record_rec",
            &["--with-replay"],
        )?;
        let bin = cargo_build(&project, "work_jobs_record_rec")?;
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

        let text = fs::read_to_string(&session)
            .map_err(|error| format!("the Rust binary wrote no recording: {error}"))?;
        for expected in ["\"type\": \"Scorer.Task\"", "\"type\": \"Scorer.Report\""] {
            if !text.contains(expected) {
                return Err(format!(
                    "the recording must spell the capability's records canonically, expected {expected}:\n{text}"
                ));
            }
        }

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

/// A recording whose record tags carry the types' own short names — the
/// spelling older writers produced — still replays on the Rust backend:
/// `Task`/`Report` are the canonical types' aliases, not other types.
#[test]
fn a_recording_spelling_its_records_by_short_names_replays_on_the_rust_backend() {
    let ws = temp_dir("replay-record-legacy");
    let project = ws.join("project");
    let recordings = ws.join("recordings");
    fs::create_dir_all(&project).expect("create project dir");
    fs::create_dir_all(&recordings).expect("create recordings dir");

    let result = (|| -> Result<(), String> {
        let dir = fixture("work_jobs_record");
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
        let text = fs::read_to_string(&session)
            .map_err(|error| format!("cannot read the recording: {error}"))?;
        let legacy = text
            .replace("\"type\": \"Scorer.Task\"", "\"type\": \"Task\"")
            .replace("\"type\": \"Scorer.Report\"", "\"type\": \"Report\"");
        if legacy == text {
            return Err("the rewrite must change the recording".to_string());
        }
        fs::write(&session, legacy)
            .map_err(|error| format!("cannot write the recording: {error}"))?;

        compile_rust(
            "work_jobs_record",
            &project,
            "work_jobs_record_legacy",
            &["--with-replay"],
        )?;
        let bin = cargo_build(&project, "work_jobs_record_legacy")?;
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|error| format!("failed to run the replaying binary: {error}"))?;
        if !replayed.status.success() {
            return Err(format!(
                "the Rust binary could not replay the short-name recording:\n{}",
                format_output(&replayed)
            ));
        }
        let stdout = String::from_utf8_lossy(&replayed.stdout);
        if !stdout.contains("job 1 scored 10 for alpha") {
            return Err(format!(
                "the replay did not serve the recorded answers:\n{}",
                format_output(&replayed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// A recording whose record tag names a different nominal type that happens
/// to share the short name is refused with a diagnostic — exit status and
/// `fail[replay-error]`, no panic.
#[test]
fn a_recording_naming_a_foreign_record_type_is_a_diagnostic_on_the_rust_backend() {
    let ws = temp_dir("replay-record-foreign");
    let project = ws.join("project");
    let recordings = ws.join("recordings");
    fs::create_dir_all(&project).expect("create project dir");
    fs::create_dir_all(&recordings).expect("create recordings dir");

    let result = (|| -> Result<(), String> {
        let dir = fixture("work_jobs_record");
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
        let text = fs::read_to_string(&session)
            .map_err(|error| format!("cannot read the recording: {error}"))?;
        let foreign = text.replace("\"type\": \"Scorer.Report\"", "\"type\": \"Other.Report\"");
        if foreign == text {
            return Err("the rewrite must change the recording".to_string());
        }
        fs::write(&session, foreign)
            .map_err(|error| format!("cannot write the recording: {error}"))?;

        compile_rust(
            "work_jobs_record",
            &project,
            "work_jobs_record_foreign",
            &["--with-replay"],
        )?;
        let bin = cargo_build(&project, "work_jobs_record_foreign")?;
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|error| format!("failed to run the replaying binary: {error}"))?;
        if replayed.status.success() {
            return Err(format!(
                "a foreign record tag must fail the replay, got:\n{}",
                format_output(&replayed)
            ));
        }
        let report = format!(
            "{}{}",
            String::from_utf8_lossy(&replayed.stdout),
            String::from_utf8_lossy(&replayed.stderr)
        );
        if !report.contains("fail[replay-error]")
            || !report.contains("Other.Report")
            || !report.contains("Scorer.Report")
        {
            return Err(format!(
                "the diagnostic must name the foreign and the expected type:\n{}",
                format_output(&replayed)
            ));
        }
        if report.contains("panicked") {
            return Err(format!(
                "a foreign record tag must be a diagnostic, not a panic:\n{}",
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

/// A real consumer carries Bytes-keyed UTXO maps in ordinary records even
/// when only its typed Work task crosses an effect boundary. --with-replay
/// must emit usable codecs for those records as well as Int/String maps.
#[test]
fn native_replay_round_trips_non_string_map_keys() {
    let ws = temp_dir("map-replay");
    let source = ws.join("main.av");
    fs::write(&source, r#"module MapReplay
    depends [Bytes]
    effects []

record Maps
    bytes: Map<Bytes, Int>
    flags: Map<Bool, Int>
    counts: Map<Int, Int>
    names: Map<String, Int>

fn sample() -> Maps
    Maps(bytes = Map.fromList([(Bytes.fromList([2]), 20), (Bytes.fromList([1]), 10)]), flags = Map.fromList([(false, 4), (true, 5)]), counts = Map.fromList([(2, 9), (-1, 7)]), names = Map.fromList([("hi", 6)]))

fn main() -> Int
    Map.len(sample().bytes)
"#).unwrap();
    let project = ws.join("project");
    let result = (|| -> Result<(), String> {
        compile_rust_at(&source, &ws, &project, "map_replay", &["--with-replay"])?;
        let lib = project.join("src/main.rs");
        let mut code = fs::read_to_string(&lib).unwrap();
        code.push_str(r#"
#[cfg(test)]
mod map_replay_regression {
    use super::replay_support::aver_replay::ReplayValue;
    #[test]
    fn typed_keys_round_trip() {
        let original = super::aver_generated::entry::sample();
        let encoded = original.to_replay_json();
        let decoded = super::aver_generated::entry::Maps::from_replay_json(&encoded).unwrap();
        assert_eq!(original, decoded);
        assert_eq!(original.flags.to_replay_json(), serde_json::json!({"$map": [[false, 4], [true, 5]]}));
        assert_eq!(original.counts.to_replay_json(), serde_json::json!({"$map": [[-1, 7], [2, 9]]}));
        assert_eq!(original.names.to_replay_json(), serde_json::json!({"hi": 6}));
        let empty = aver_rt::AverMap::<bool, aver_rt::AverInt>::new();
        assert_eq!(empty.to_replay_json(), serde_json::json!({}));
        assert_eq!(empty, aver_rt::AverMap::from_replay_json(&empty.to_replay_json()).unwrap());
        assert!(aver_rt::AverMap::<bool, aver_rt::AverInt>::from_replay_json(&serde_json::json!({"$map": [[true]]})).is_err());
    }
}
"#);
        fs::write(&lib, code).unwrap();
        let out = Command::new("cargo")
            .args([
                "test",
                "--offline",
                "--bin",
                "map_replay",
                "map_replay_regression",
            ])
            .arg("--manifest-path")
            .arg(project.join("Cargo.toml"))
            .env("CARGO_TARGET_DIR", shared_target_dir())
            .output()
            .expect("run generated codec tests");
        if !out.status.success() {
            return Err(format!(
                "generated map replay failed:\n{}",
                format_output(&out)
            ));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

#[path = "rust_work_spec/native_transfer.rs"]
mod native_transfer;

/// A program whose generated loop keys its wait by `Int` and whose own waits
/// are keyed by a sum, in the entry and in a dependency, compiles to Rust and
/// does the same work as the VM on both of its paths: the loop, and the two
/// jobs it collects by hand. The jobs land in whatever order they finish, so
/// the lines are compared as a multiset.
#[test]
fn waits_keyed_by_a_sum_beside_the_generated_loop_match_the_vm() {
    let name = "run_wait_own_key";
    let ws = temp_dir(name);
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(name, &project, name, &[])?;
        let bin = cargo_build(&project, name)?;
        for args in [&[][..], &["manual"][..]] {
            let vm = run_vm_with(name, args)?;
            let rust = run_binary_with(&bin, args)?;
            same_lines(name, &vm, &rust)?;
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}
