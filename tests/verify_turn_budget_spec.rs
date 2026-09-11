//! `[verify] turn-budget`: how long one turn of a case runs without waiting.
//!
//! The per-case step budget says how much work a case may be in total. A
//! server case can fit it comfortably and still be wrong in a way the total
//! cannot see: one turn — the run between two `Tcp.poll` waits — doing all
//! of the work while every other peer waits. The turn budget counts VM steps
//! since the last `Tcp.poll` (or since the case began) and, when a turn
//! crosses it, says so once per case and names the innermost function at
//! that moment. A warning, not a failure: the case was answered.
//!
//! Off by default, and off means nothing is counted: the VM derives a turn's
//! length from the step counter it already keeps, and only tests it while a
//! budget is installed.
#![cfg(feature = "runtime")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{format_output, repo_root};

mod support {
    pub mod scratch_dir;
}

use std::path::Path;
use std::process::Command;

use support::scratch_dir::{ScratchDir, temp_dir};

const SLUG: &str = "warning[turn-budget]:";

fn project(prefix: &str, aver_toml: &str) -> ScratchDir {
    project_with("serve.av", prefix, aver_toml)
}

fn project_with(fixture: &str, prefix: &str, aver_toml: &str) -> ScratchDir {
    let dir = temp_dir(prefix);
    let source = repo_root()
        .join("tests/fixtures/verify_turn_budget")
        .join(fixture);
    std::fs::copy(&source, dir.join("main.av")).expect("stage the fixture module");
    std::fs::write(dir.join("aver.toml"), aver_toml).expect("stage aver.toml");
    dir
}

fn verify(dir: &Path) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(dir)
        .args(["verify", "main.av", "--module-root", "."])
        .env("NO_COLOR", "1")
        .output()
        .expect("expected the `aver` binary to run")
}

fn stdout_of(out: &std::process::Output) -> String {
    String::from_utf8_lossy(&out.stdout).to_string()
}

#[test]
fn a_long_pure_loop_between_two_polls_trips_the_budget_once_per_case_and_names_the_loop() {
    let dir = project("turn-budget-trips", "[verify]\nturn-budget = 1000\n");
    let out = verify(&dir);
    let text = stdout_of(&out);

    assert!(text.contains(SLUG), "{}", format_output(&out));
    // The two cases are the same expression; the index tells them apart.
    assert!(
        text.contains("serve case 1 `serve(20000) == Result.Ok(20000)`: one turn ran "),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("serve case 2 `serve(20000) == Result.Ok(20000)`: one turn ran "),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains(" steps without waiting; deepest function on the stack at the limit: `spin`"),
        "{}",
        format_output(&out)
    );
    // Once per case, and no more: each case has one turn that crosses.
    assert_eq!(text.matches(SLUG).count(), 2, "{}", format_output(&out));
    // The case itself passed, and a warning does not fail the run.
    assert!(text.contains("✓ serve"), "{}", format_output(&out));
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

/// The count resets on every `Tcp.poll`, live or stubbed. The case runs
/// eleven turns of about forty thousand steps each — well past a budget of
/// 150,000 in total — while no single turn comes near it. Remove the
/// `turn_start` reset on `Tcp.poll` in `src/vm/execute/host.rs` and this
/// test fails: the count would then run from the start of the case and
/// cross the budget during the fourth turn.
#[test]
fn many_short_turns_stay_quiet_because_the_count_resets_on_every_poll() {
    let dir = project_with(
        "turns.av",
        "turn-budget-resets",
        "[verify]\nturn-budget = 150000\n",
    );
    let out = verify(&dir);
    let text = stdout_of(&out);
    assert!(!text.contains(SLUG), "{}", format_output(&out));
    assert!(text.contains("✓ serveMany"), "{}", format_output(&out));
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

/// The same eleven turns under a budget one turn does cross: the reset
/// is what keeps the test above quiet, not the size of the case.
#[test]
fn the_same_short_turns_are_reported_once_when_one_turn_alone_crosses_the_budget() {
    let dir = project_with(
        "turns.av",
        "turn-budget-one-turn",
        "[verify]\nturn-budget = 10000\n",
    );
    let out = verify(&dir);
    let text = stdout_of(&out);
    assert!(
        text.contains(
            "serveMany case 1 `serveMany(5000, 10, 0) == Result.Ok(55000)`: one turn ran "
        ),
        "{}",
        format_output(&out)
    );
    assert_eq!(text.matches(SLUG).count(), 1, "{}", format_output(&out));
    assert!(text.contains("✓ serveMany"), "{}", format_output(&out));
}

#[test]
fn the_same_case_under_the_budget_is_quiet() {
    let dir = project("turn-budget-under", "[verify]\nturn-budget = 100000000\n");
    let out = verify(&dir);
    let text = stdout_of(&out);
    assert!(!text.contains(SLUG), "{}", format_output(&out));
    assert!(text.contains("✓ serve"), "{}", format_output(&out));
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

#[test]
fn without_a_budget_nothing_is_measured_or_reported() {
    let dir = project("turn-budget-off", "");
    let out = verify(&dir);
    let text = stdout_of(&out);
    assert!(!text.contains(SLUG), "{}", format_output(&out));
    assert!(text.contains("✓ serve"), "{}", format_output(&out));
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

#[test]
fn a_budget_that_is_not_a_positive_integer_is_a_config_error() {
    let dir = project("turn-budget-zero", "[verify]\nturn-budget = 0\n");
    let out = verify(&dir);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("[verify] `turn-budget` must be a positive integer"),
        "{}",
        format_output(&out)
    );
}
