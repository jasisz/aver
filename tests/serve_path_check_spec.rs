//! `warning[serve-path]`: `aver check` reports a `Tcp.poll` loop that hands
//! one of its turns to an effectful loop.
//!
//! A function that polls is a turn of an event loop. When a turn reaches a
//! recursive function that does `Disk.*` or `Tcp.*` work outside the poller's
//! own loop, that function runs to completion before the next wait, and the
//! peers that became ready meanwhile are not served until it returns. The
//! condition is structural: the module's call graph, its recursive SCCs, and
//! the declared effect sets. Four fixtures pin its edges: the shape that
//! trips it, the same loop reached without a poll, the poller whose loop is
//! its own, and the `[[check.suppress]]` waiver.
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

const SLUG: &str = "warning[serve-path]";

/// Stage one fixture module and, when given, an `aver.toml`, in a fresh
/// project directory.
fn project(prefix: &str, fixture: &str, aver_toml: Option<&str>) -> ScratchDir {
    let dir = temp_dir(prefix);
    let source = repo_root().join("tests/fixtures/serve_path").join(fixture);
    std::fs::copy(&source, dir.join("main.av")).expect("stage the fixture module");
    if let Some(config) = aver_toml {
        std::fs::write(dir.join("aver.toml"), config).expect("stage aver.toml");
    }
    dir
}

fn check(dir: &Path) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(dir)
        .args(["check", "main.av", "--module-root", "."])
        .env("NO_COLOR", "1")
        .output()
        .expect("expected the `aver` binary to run")
}

fn stdout_of(out: &std::process::Output) -> String {
    String::from_utf8_lossy(&out.stdout).to_string()
}

#[test]
fn a_drain_reached_from_a_poll_turn_is_reported_at_the_call_into_it() {
    let dir = project("serve-path-trips", "trips.av", None);
    let out = check(&dir);
    let text = stdout_of(&out);

    assert!(text.contains(SLUG), "{}", format_output(&out));
    assert!(
        text.contains(
            "`drain` is an effectful loop that runs to completion inside one turn of `turn`; peers waiting on `Tcp.poll` are not served until it returns."
        ),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("Do one step of `drain` per turn, or run `drain` as its own command"),
        "{}",
        format_output(&out)
    );
    // One warning per (poller, loop) pair: `drainStep` is the same loop.
    assert_eq!(text.matches(SLUG).count(), 1, "{}", format_output(&out));
    // The warning sits on `turn`'s call into the path, not on the loop.
    assert!(text.contains("main.av:32"), "{}", format_output(&out));
    // Warnings never fail `check`.
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

#[test]
fn the_same_loop_without_a_poll_on_the_path_is_not_reported() {
    let dir = project("serve-path-no-poll", "no_poll.av", None);
    let out = check(&dir);
    assert!(!stdout_of(&out).contains(SLUG), "{}", format_output(&out));
}

#[test]
fn a_poller_whose_effectful_loop_is_its_own_is_not_reported() {
    let dir = project("serve-path-own-loop", "own_loop.av", None);
    let out = check(&dir);
    assert!(!stdout_of(&out).contains(SLUG), "{}", format_output(&out));
}

#[test]
fn a_check_suppress_rule_with_a_reason_waives_it() {
    let dir = project(
        "serve-path-suppressed",
        "trips.av",
        Some(
            "[[check.suppress]]\nslug = \"serve-path\"\nfn = \"turn\"\nreason = \"The queue is bounded to three entries by the caller.\"\n",
        ),
    );
    let out = check(&dir);
    let text = stdout_of(&out);
    assert!(!text.contains(SLUG), "{}", format_output(&out));
    assert!(
        text.contains("warning(s) suppressed by aver.toml"),
        "{}",
        format_output(&out)
    );
}
