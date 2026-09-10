//! `warning[serve-path]`: `aver check` reports a `Tcp.poll` loop that hands
//! one of its turns to an effectful loop.
//!
//! A function that polls is a turn of an event loop. When a turn reaches a
//! loop that reads from `Disk` or `Tcp` on every step and whose recursion
//! survives with the poller's node removed from the call graph, that loop
//! runs to completion before the next wait, and the peers that became ready
//! meanwhile are not served until it returns. The condition is structural:
//! the module's call graph without the poller, its recursive SCCs, and the
//! declared effect sets, minus two shapes: a loop that walks a list it was
//! handed, one element per step, is bounded by that list and is how a
//! server serves the keys a poll returned; and a loop that only writes what
//! it holds is bounded by this turn's data. The fixtures pin the edges: the
//! shape that trips it, the same loop reached without a poll, the poller
//! whose loop is its own, the `[[check.suppress]]` waiver, the reference
//! server (silent), that server plus a counter-driven walk that reads
//! (warns once), the same walk that only appends (silent), a poller and
//! handler that recurse through each other (the handler's drain warns), a
//! poller that hands off to another poller (silent), a shutdown that
//! walks the session list once (silent), a reader whose cycle passes
//! through a second poller's wait (silent), and a counter loop that dials
//! out with `Tcp.send` on every step (warns once).
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

#[test]
fn the_reference_server_serving_each_ready_key_once_is_silent() {
    let dir = project("serve-path-canonical", "canonical.av", None);
    let out = check(&dir);
    assert!(!stdout_of(&out).contains(SLUG), "{}", format_output(&out));
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

#[test]
fn a_counter_driven_walk_that_reads_called_from_a_handler_is_reported_once_and_named() {
    let dir = project("serve-path-counter-walk", "counter_walk.av", None);
    let out = check(&dir);
    let text = stdout_of(&out);
    assert!(
        text.contains(
            "`replayLog` is an effectful loop that runs to completion inside one turn of `serve`"
        ),
        "{}",
        format_output(&out)
    );
    // The ready-list walk `dispatch` is the server shape, not a second loop.
    assert!(
        !text.contains("`dispatch` is an effectful loop"),
        "{}",
        format_output(&out)
    );
    assert_eq!(text.matches(SLUG).count(), 1, "{}", format_output(&out));
    // On `serve`'s call into the path: `next = dispatch(pool, ready)?`.
    assert!(text.contains("main.av:18"), "{}", format_output(&out));
}

#[test]
fn a_counter_driven_walk_that_only_appends_is_silent() {
    // Same server and walk as `counter_walk.av`, but the loop only writes
    // what it already holds: bounded by this turn's data, so no warning.
    let dir = project("serve-path-writes-only", "writes_only.av", None);
    let out = check(&dir);
    assert!(!stdout_of(&out).contains(SLUG), "{}", format_output(&out));
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

#[test]
fn a_handler_that_recurses_through_the_poller_is_walked_and_its_drain_reported() {
    let dir = project("serve-path-mutual", "mutual.av", None);
    let out = check(&dir);
    let text = stdout_of(&out);
    assert!(
        text.contains(
            "`drain` is an effectful loop that runs to completion inside one turn of `serve`"
        ),
        "{}",
        format_output(&out)
    );
    assert_eq!(text.matches(SLUG).count(), 1, "{}", format_output(&out));
    // On `serve`'s call to its sibling `handle`, which is where the turn
    // hands control to the path.
    assert!(text.contains("main.av:15"), "{}", format_output(&out));
}

#[test]
fn a_poller_that_hands_off_to_another_poller_each_step_is_silent() {
    let dir = project("serve-path-handoff", "handoff.av", None);
    let out = check(&dir);
    assert!(!stdout_of(&out).contains(SLUG), "{}", format_output(&out));
}

#[test]
fn a_shutdown_that_walks_the_session_list_once_is_silent() {
    let dir = project("serve-path-shutdown", "shutdown.av", None);
    let out = check(&dir);
    assert!(!stdout_of(&out).contains(SLUG), "{}", format_output(&out));
}

#[test]
fn a_cycle_that_passes_through_a_second_pollers_wait_is_silent() {
    // `reader` and `poller` recurse through each other, but `poller` waits:
    // the cycle is cut at its turn boundary for `serve` as well.
    let dir = project("serve-path-poller-cycle", "poller_cycle.av", None);
    let out = check(&dir);
    assert!(!stdout_of(&out).contains(SLUG), "{}", format_output(&out));
    assert_eq!(out.status.code(), Some(0), "{}", format_output(&out));
}

#[test]
fn a_counter_loop_that_dials_out_on_every_step_is_reported_once() {
    let dir = project("serve-path-network-roundtrip", "network_roundtrip.av", None);
    let out = check(&dir);
    let text = stdout_of(&out);
    assert!(
        text.contains(
            "`notify` is an effectful loop that runs to completion inside one turn of `serve`"
        ),
        "{}",
        format_output(&out)
    );
    assert_eq!(text.matches(SLUG).count(), 1, "{}", format_output(&out));
    // On `serve`'s call into the path: `_notified = handle(List.len(ready))?`.
    assert!(text.contains("main.av:16"), "{}", format_output(&out));
}
