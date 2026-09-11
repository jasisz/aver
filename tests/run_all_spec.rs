//! The generated loop (jasisz/aver#1329, leg 2.3).
//!
//! `tests/fixtures/run_all_slice/` is the whole claim in one program: five
//! processes, three answer modules, one job kind and three policies, with
//! every line between them generated. This suite runs it the way a user
//! would — `aver run`, `aver verify`, a recording and its replay, a hostile
//! wait, and the dump that shows what was generated — and holds the two
//! refusals that say what the program has to declare for the loop to be
//! generated at all.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

const SLICE: &str = "run_all_slice";

fn fixture(name: &str) -> PathBuf {
    repo_root().join("tests/fixtures").join(name)
}

fn aver(fixture_name: &str, args: &[&str]) -> Output {
    let dir = fixture(fixture_name);
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.arg(args[0]).arg("main.av");
    command.arg("--module-root").arg(&dir);
    command.args(&args[1..]);
    command.output().expect("aver runs")
}

fn combined(out: &Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

fn scratch(name: &str) -> PathBuf {
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock after the epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver-run-all-{name}-{stamp}"));
    std::fs::create_dir_all(&dir).expect("scratch directory");
    dir
}

fn only_recording(dir: &Path) -> PathBuf {
    std::fs::read_dir(dir)
        .expect("recording directory")
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .find(|path| {
            path.extension()
                .is_some_and(|extension| extension == "json")
        })
        .expect("one recording file")
}

#[test]
fn the_slice_runs_to_the_end_on_the_vm_with_nothing_written_between_its_processes() {
    let out = aver(SLICE, &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = String::from_utf8_lossy(&out.stdout);
    // Every line of output comes from an in-place `Console.print` inside a
    // process, so counting them counts how far each process got: the peer
    // asks the pool four times (three heights and the Stop), and the walk
    // looks for a target four times (three commits and the empty chain).
    assert_eq!(
        text.matches("peer: asking the pool for work").count(),
        4,
        "{}",
        format_output(&out)
    );
    assert_eq!(
        text.matches("walk: looking for the next block to connect")
            .count(),
        4,
        "{}",
        format_output(&out)
    );
}

#[test]
fn the_slice_checks_clean() {
    let out = aver(SLICE, &["check"]);
    assert!(out.status.success(), "{}", format_output(&out));
}

#[test]
fn the_generated_invariants_and_the_programs_priority_law_hold() {
    let out = aver(SLICE, &["verify"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = combined(&out);
    for law in [
        "__park law laterKeepsTheInstance",
        "__park law laterLeavesLedgerAlone",
        "__nextInstance law nowRaisesTheInstance",
        "__settlePeer law lateAnswerIsDropped",
        "__settlePeer law lateAnswerIsRecorded",
        "__settlePeer law oneSlotPerProcess",
        "admit law readyPeerBeforeNewJob",
    ] {
        assert!(
            text.contains(law),
            "{law} missing from:\n{}",
            combined(&out)
        );
    }
    assert!(text.contains("0 failed"), "{}", format_output(&out));
}

#[test]
fn a_recorded_run_of_the_slice_replays_to_the_same_run() {
    let dir = scratch("replay");
    let recorded = aver(
        SLICE,
        &["run", "--record", dir.to_str().expect("utf-8 scratch path")],
    );
    assert!(recorded.status.success(), "{}", format_output(&recorded));

    let recording = only_recording(&dir);
    let mut command = Command::new(aver_bin());
    command.current_dir(fixture(SLICE));
    command.arg("replay").arg(&recording);
    command.arg("--check-args");
    let replayed = command.output().expect("aver replays");
    let text = combined(&replayed);
    assert!(replayed.status.success(), "{}", format_output(&replayed));
    assert!(
        text.contains("Output:  MATCH"),
        "{}",
        format_output(&replayed)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// A hostile world leaves every invariant of the loop standing, and it is
/// worth saying why it must: the loop's invariants are laws over pure
/// functions of the slot table, which no wait and no provider can reach. The
/// hostile profiles that do fire here are the ones a process performs in
/// place on its way to its first request — the sample run seats every
/// process, so every such profile is exercised under every law below it.
///
/// `Wait.poll`'s own hostile profiles are not among them, and cannot be
/// today: a law that reaches the wait has to be stated over a function that
/// performs `Wait.poll`, and such a law does not reach the Lean wall —
/// measured on `tests/fixtures/work_jobs`, whose `readyCount law` makes
/// `aver proof --backend lean` fail with five build errors, because an
/// oracle-lifted function's law renders its sample theorems without the
/// oracle arguments. Generating such a law into every program that asks for
/// a loop would break `aver proof` for all of them, so this leg does not.
#[test]
fn a_hostile_world_leaves_every_invariant_of_the_loop_standing() {
    let plain = aver(SLICE, &["verify"]);
    let hostile = aver(SLICE, &["verify", "--hostile"]);
    assert!(hostile.status.success(), "{}", format_output(&hostile));
    let text = combined(&hostile);
    assert!(text.contains("0 failed"), "{}", format_output(&hostile));
    assert!(
        text.contains("__settlePeer law lateAnswerIsDropped"),
        "{}",
        format_output(&hostile)
    );
    // The hostile run expands into strictly more cases than the honest one,
    // so the profiles are actually being installed rather than skipped.
    assert!(
        cases(&combined(&hostile)) > cases(&combined(&plain)),
        "hostile ran no more cases than the honest run:\n{}",
        format_output(&hostile)
    );
}

/// The `N/M cases passed` count from a verify summary line.
fn cases(text: &str) -> usize {
    text.lines()
        .find_map(|line| {
            let (_, rest) = line.split_once("| ")?;
            let (count, _) = rest.split_once(" cases passed")?;
            let (passed, _) = count.rsplit_once('/')?;
            passed.rsplit(' ').next()?.parse::<usize>().ok()
        })
        .unwrap_or(0)
}

#[test]
fn the_dump_shows_the_loop_that_was_generated() {
    let dir = fixture(SLICE);
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.env("AVER_YIELD_DUMP", "1");
    command.arg("check").arg("main.av");
    command.arg("--module-root").arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    for line in [
        "record __Run",
        "fn __turn(run: __Run) -> Result<__Run, String>",
        "fn __runAll(run: __Run) -> Result<__Run, String>",
        "fn main() -> Result<Unit, String>",
        "fn __servePeer(run: __Run, id: Int, seq: Int, request: __PeerRequest) -> __Run",
        "Ledger.claim(run.ledger)",
        "Validation.begin(payload)?",
        "verify __settlePeer law lateAnswerIsDropped",
    ] {
        assert!(text.contains(line), "{line} missing from the dump");
    }
}

#[test]
fn a_view_that_is_not_the_shape_the_loop_fills_is_refused_with_the_declaration_it_wants() {
    let out = aver("run_view_shape", &["check"]);
    let text = combined(&out);
    assert!(
        text.contains("error[view-shape]: record 'View' declares no field 'room'"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("        room: Int"),
        "the message prints the declaration the loop fills:\n{}",
        format_output(&out)
    );
}

#[test]
fn a_process_the_loop_cannot_seat_is_refused_at_every_door() {
    // The lowering refuses before anything runs, so `run` and `verify` see
    // the same sentence `check` slugs.
    let sentence = "aver.toml declares [run], so the generated loop seats one of every process this module writes, and it has nothing to seat 'looping' with";
    for command in ["check", "run", "verify"] {
        let out = aver("run_shape_parameters", &[command]);
        assert!(!out.status.success(), "{command}: {}", format_output(&out));
        assert!(
            combined(&out).contains(sentence),
            "{command}: {}",
            format_output(&out)
        );
    }
    let checked = aver("run_shape_parameters", &["check"]);
    assert!(
        combined(&checked).contains("error[run-binding]:"),
        "{}",
        format_output(&checked)
    );
}

/// The laws decision 7 names, on the Lean wall.
///
/// Sixteen obligations close as universals: I2 (a late answer changes
/// nothing and is counted) and I4's visible half (a `Later` moves neither
/// the instance number nor any answer state) for every process and every
/// answer module, I3's per-call half (an answer for the current instance
/// raises it), and the program's own priority law. The five that stay open
/// are I1's implication, one per process: it is a size comparison across one
/// `Map.set` or one `Map.remove`, and the Lean prelude carries
/// `AverMap.len_set_ge` — the length never shrinks — but no fact that a set
/// on a key already present keeps it. The proposal expected exactly this one
/// to stay open on the first cut.
#[test]
fn the_generated_invariants_reach_the_lean_wall() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping the Lean wall: `lake` is not available");
        return;
    }
    let dir = fixture(SLICE);
    let out_dir = scratch("lean");
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.arg("proof").arg("main.av");
    command.arg("--module-root").arg(&dir);
    command.arg("--backend").arg("lean");
    command.arg("-o").arg(&out_dir);
    command.arg("--check").arg("--check-json");
    command.arg("--sorry-budget").arg("5");
    let out = command.output().expect("aver proves");
    let json = String::from_utf8_lossy(&out.stdout);
    let line = json
        .lines()
        .rev()
        .find(|line| line.starts_with('{'))
        .unwrap_or_else(|| panic!("no JSON summary:\n{}", format_output(&out)));
    let summary: serde_json::Value = serde_json::from_str(line).expect("the summary is JSON");
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "{}",
        format_output(&out)
    );
    assert_eq!(
        summary["universal_laws"].as_u64(),
        Some(16),
        "universal-law drift:\n{}",
        format_output(&out)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(5),
        "sorry drift:\n{}",
        format_output(&out)
    );
    let obligations = &summary["obligations"];
    for closed in [
        "__park.laterKeepsTheInstance.implication",
        "__park.laterLeavesLedgerAlone.implication",
        "__settlePeer.lateAnswerIsDropped.implication",
        "__settlePeer.lateAnswerIsRecorded.implication",
        "admit.readyPeerBeforeNewJob.implication",
    ] {
        assert_eq!(
            obligations[closed].as_str(),
            Some("universal"),
            "{closed} is no longer universal:\n{}",
            format_output(&out)
        );
    }
    assert_eq!(
        obligations["__settlePeer.oneSlotPerProcess.implication"].as_str(),
        Some("failed"),
        "I1 closed — lower the budget and say so:\n{}",
        format_output(&out)
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}
