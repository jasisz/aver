//! Answered capabilities (jasisz/aver#1329, process layer v2).
//!
//! The `tests/fixtures/answer_*` family holds one program per rule: one way
//! of getting an `answers [...]` header or the shape of an answer module
//! wrong. Every case goes through the real CLI, because the rule's whole
//! point is that `aver check`, `aver run`, `aver verify` and `aver compile`
//! agree about what a header means before anything is lowered against it.
//!
//! An answer function returns `Tuple<S, Result<R, Run.Wake>>`, where `R` is
//! the operation's own result: `Ok` answers now and `Err` says when to ask
//! again. Both are types the language already has, so a capability declares
//! nothing beside its operations for a module to answer it. The accept path
//! is also checked in `src/capability/work.rs` against a synthetic signature
//! map.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::path::PathBuf;
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

fn fixture(name: &str) -> PathBuf {
    repo_root().join("tests/fixtures").join(name)
}

fn aver(fixture_name: &str, args: &[&str]) -> Output {
    let dir = fixture(fixture_name);
    let mut command = Command::new(aver_bin());
    command.current_dir(repo_root());
    command.arg(args[0]).arg(dir.join("main.av"));
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

fn assert_reports(fixture_name: &str, args: &[&str], expected: &str) {
    let out = aver(fixture_name, args);
    let text = combined(&out);
    assert!(
        text.contains(expected),
        "expected {expected:?} in:\n{}",
        format_output(&out)
    );
}

// ── answer-binding: which capability, which module ──────────────────────

#[test]
fn a_capability_the_compiler_ships_cannot_be_answered_by_a_module() {
    let expected = "error[answer-binding]: module 'Ledger' says `answers [Console]`, but 'Console' is a standard capability this compiler ships";
    assert_reports("answer_shape_standard_capability", &["check"], expected);
    assert_reports("answer_shape_standard_capability", &["run"], expected);
    assert_reports("answer_shape_standard_capability", &["verify"], expected);
}

#[test]
fn a_capability_module_cannot_answer_a_capability() {
    assert_reports(
        "answer_shape_capability_module",
        &["check"],
        "module 'Pool' says `answers [Pool]`, but 'Pool' is a capability module; an answer module is an ordinary module of the program that computes the answer",
    );
}

#[test]
fn the_state_an_answer_module_holds_is_its_first_parameter() {
    assert_reports(
        "answer_shape_foreign_state",
        &["check"],
        "'Ledger.claim' answers 'Pool.claim', so its first parameter is the state module 'Ledger' holds, a type that module declares; it is Int",
    );
}

#[test]
fn every_operation_of_an_answered_capability_needs_an_answer_function() {
    assert_reports(
        "answer_shape_missing_operation",
        &["check"],
        "capability 'Pool' is answered by module 'Ledger', so every one of its operations needs an answer function; this program has no function 'Ledger.gone'",
    );
}

/// The answer function returns the state it leaves behind and either the
/// operation's own result or the wake that says when to ask again. A
/// function that returns the bare result is told the whole signature it
/// needs, spelled with the types the language already has.
#[test]
fn an_answer_function_answers_with_the_operations_result_or_a_wake() {
    assert_reports(
        "answer_shape_wrong_reply",
        &["check"],
        "capability 'Pool' declares operation 'claim(key: Int) -> Pool.Assignment', so 'Ledger.claim' must be (Ledger.State, Int) -> Tuple<Ledger.State, Result<Pool.Assignment, Run.Wake>>; it is (Ledger.State, Int) -> Pool.Assignment",
    );
}

#[test]
fn one_module_answering_several_capabilities_holds_one_state() {
    assert_reports(
        "answer_shape_two_states",
        &["check"],
        "module 'Ledger' answers 'Pool' with state Ledger.State and 'Chain' with state Ledger.Store; one module answering several capabilities holds one state",
    );
}

#[test]
fn two_answered_capabilities_cannot_share_an_operation_name() {
    assert_reports(
        "answer_shape_shared_operation",
        &["check"],
        "module 'Ledger' answers 'Pool.claim' and 'Chain.claim' with one function 'Ledger.claim'; rename one of the operations",
    );
}

// ── answer-shape: what an answer function may do ────────────────────────

#[test]
fn an_answer_function_cannot_itself_be_a_process() {
    assert_reports(
        "answer_shape_yielding_answer",
        &["check"],
        "error[answer-shape]: module 'Ledger' answers capability 'Pool', and 'Ledger.claim' declares `yield`",
    );
}

#[test]
fn an_answer_function_with_effects_is_allowed_and_said_so() {
    let out = aver("answer_shape_effectful_answer", &["check"]);
    let text = combined(&out);
    assert!(
        text.contains("warning[answer-shape]: module 'Ledger' answers capability 'Pool', and 'Ledger.claim' declares effects [Console.print]"),
        "{}",
        format_output(&out)
    );
    assert!(
        !text.contains("error["),
        "an effectful answer is a warning, not a refusal:\n{}",
        format_output(&out)
    );
    // The warning belongs to the answer module; the entry that imports it
    // does not repeat it.
    assert_eq!(
        text.matches("warning[answer-shape]").count(),
        1,
        "{}",
        format_output(&out)
    );
}

/// The warning is about stalling the turn, so it names only effects that can
/// block: an answer that reads the clock returns at once and is not warned
/// about, and one that also prints is warned about its print alone.
#[test]
fn an_answer_whose_effects_return_at_once_is_not_warned_about() {
    let out = aver("answer_shape_nonblocking_answer", &["check"]);
    let text = combined(&out);
    assert!(
        !text.contains("'Ledger.claim' declares effects"),
        "an answer that only reads the clock cannot stall the turn:\n{}",
        format_output(&out)
    );
    assert!(
        text.contains("warning[answer-shape]: module 'Ledger' answers capability 'Pool', and 'Ledger.gone' declares effects [Console.print];"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn the_effectful_answer_warning_does_not_stop_the_run_door() {
    // A warning is something `aver check` tells the program's author; only an
    // error stops the command on its way to the backend.
    let out = aver("answer_shape_effectful_answer", &["run"]);
    let text = combined(&out);
    assert!(
        !text.contains("warning[answer-shape]"),
        "the run door reports errors, not warnings:\n{}",
        format_output(&out)
    );
}

/// A capability and its answer module both live under `slice/`, so the
/// program loads them as `Slice.Wire` and `Slice.Sockets`, while each declares
/// its short name. The header says `answers [Slice.Wire]`, the name the
/// module writes in its own `depends`, and the loop calls the module by the
/// name the program loads it under.
#[test]
fn a_nested_capability_naming_its_own_type_is_answered_like_any_other() {
    for command in ["check", "run"] {
        let out = aver("answer_nested_self_qualified", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
    }
    let out = aver("answer_nested_self_qualified", &["run"]);
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("gone done"),
        "{}",
        format_output(&out)
    );
}

// ── the manifest keys that are gone ─────────────────────────────────────

fn temp_project(label: &str, manifest: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock after epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver-answer-manifest-{label}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create temp project");
    let source = fixture("answer_shape_wrong_reply");
    for file in ["main.av", "ledger.av", "pool.av"] {
        std::fs::copy(source.join(file), dir.join(file)).expect("copy fixture module");
    }
    std::fs::write(dir.join("aver.toml"), manifest).expect("write aver.toml");
    dir
}

fn check_project(dir: &PathBuf) -> Output {
    let mut command = Command::new(aver_bin());
    command.current_dir(repo_root());
    command
        .arg("check")
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(dir);
    command.output().expect("aver runs")
}

fn assert_manifest_rejects(label: &str, manifest: &str, expected: &str) {
    let dir = temp_project(label, manifest);
    let out = check_project(&dir);
    let text = combined(&out);
    assert!(
        text.contains(expected),
        "expected {expected:?} in:\n{}",
        format_output(&out)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// The module that answers a capability says so in its own header, so the
/// manifest key that used to say it is refused with the header to write.
#[test]
fn an_answer_binding_in_the_manifest_is_refused_with_the_header_to_write() {
    assert_manifest_rejects(
        "answer-key",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Pool\"\nanswer = \"Ledger\"\n",
        "Write `answers [Pool]` in the header of module 'Ledger' and remove this binding",
    );
}

/// The job seam is gone: an answer module begins a job itself and parks the
/// request on it.
#[test]
fn the_job_seam_keys_are_refused_with_the_repair() {
    for key in ["task", "started", "landed"] {
        assert_manifest_rejects(
            key,
            &format!(
                "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Validation\"\nwork = \"Node.validate\"\n{key} = \"Ledger.f\"\n"
            ),
            &format!("declares `{key}`; the job seam is gone"),
        );
    }
}

#[test]
fn a_run_table_is_refused_with_the_repair() {
    assert_manifest_rejects(
        "run",
        "[run]\n",
        "error[run-binding]: aver.toml: [run] is gone",
    );
}

/// A capability the compiler ships cannot be answered, and every door says
/// so about the header itself and nothing else.
#[test]
fn answering_a_shipped_capability_reachable_from_wait_reports_the_header_only() {
    let expected = "error[answer-binding]: module 'Sockets' says `answers [Tcp]`";
    for door in ["check", "run", "verify"] {
        assert_reports("answer_shape_reserved_reachable", &[door], expected);
        let out = aver("answer_shape_reserved_reachable", &[door]);
        let text = combined(&out);
        assert!(
            !text.contains("Circular import"),
            "{door} reports the header, nothing else:\n{}",
            format_output(&out)
        );
    }
}

// ── intercept-outside-yield: a request is only a request in a process ────

/// An operation of an answered capability is a request, and only a `yield`
/// function makes one: the lowering cuts a process at the call and hands it
/// to the loop. A plain function calling the same operation has nobody to
/// answer it, so both program doors refuse it and the message names the
/// answer function to call instead.
#[test]
fn an_answered_operation_outside_a_process_is_refused_at_the_check_door() {
    assert_reports(
        "answer_request_outside_yield",
        &["check"],
        "error[intercept-outside-yield]: 'Pool.claim' is answered by this program",
    );
    assert_reports(
        "answer_request_outside_yield",
        &["check"],
        "Add `yield` to 'Main.seat', or call 'Ledger.claim(state, key)' directly",
    );
}

#[test]
fn an_answered_operation_outside_a_process_is_refused_at_the_run_door() {
    assert_reports(
        "answer_request_outside_yield",
        &["run"],
        "error[intercept-outside-yield]: 'Pool.claim' is answered by this program",
    );
}
