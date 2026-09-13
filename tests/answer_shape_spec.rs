//! Answered capabilities and the job seam (jasisz/aver#1329, leg 2.1).
//!
//! The `tests/fixtures/answer_*` family holds one program per rule: one way
//! of getting the `answer` binding, the shape of an answer module, or the two
//! ends of a job kind's seam wrong. Every case goes through the real CLI,
//! because the rule's whole point is that `aver check`, `aver run`,
//! `aver verify` and `aver compile` agree about what a binding means before
//! anything is lowered against it.
//!
//! An answer function returns `Tuple<S, Cap.<Op>Reply>`, and `Cap.<Op>Reply`
//! is a sum the program declares in the capability module beside the
//! operation: `Now` carrying the operation's result, `Later` carrying
//! `Wait.Wake`, nothing else. Nothing is generated into a capability module,
//! and the `answer_shape_reply_*` fixtures hold the door to that declaration.
//! The accept path — the expected parameters, the expected reply name, and a
//! well-typed job seam over them — is also checked in
//! `src/capability/work.rs` against a synthetic signature map.

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
    let expected = "error[answer-binding]: aver.toml: [[providers.bindings]] index 0 binds capability 'Console' with `answer`, but 'Console' is a standard capability this compiler ships";
    assert_reports("answer_shape_standard_capability", &["check"], expected);
    assert_reports("answer_shape_standard_capability", &["run"], expected);
    assert_reports("answer_shape_standard_capability", &["verify"], expected);
}

#[test]
fn an_answer_naming_no_module_of_the_program_is_refused() {
    assert_reports(
        "answer_shape_unknown_module",
        &["check"],
        "binds capability 'Pool' to answer = \"Ledger\", but this program has no module 'Ledger'",
    );
}

#[test]
fn a_capability_module_cannot_be_named_as_an_answer_module() {
    assert_reports(
        "answer_shape_capability_module",
        &["check"],
        "binds capability 'Pool' to answer = \"Pool\", but 'Pool' is a capability module; an answer module is an ordinary module of the program that computes the answer",
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

#[test]
fn an_answer_function_answers_with_the_operations_declared_reply() {
    assert_reports(
        "answer_shape_wrong_reply",
        &["check"],
        "capability 'Pool' declares operation 'claim(key: Int) -> Pool.Assignment', so 'Ledger.claim' must be (Ledger.State, Int) -> Tuple<Ledger.State, Pool.ClaimReply>; it is (Ledger.State, Int) -> Pool.Assignment",
    );
}

// ── answer-shape: the reply sum the capability declares ─────────────────

/// The declaration the door prints, in the capability's own names, indented
/// the way `view-shape` prints the view it wants.
const GONE_REPLY: &str = "    type GoneReply\n        Now(Unit)\n        Later(Wait.Wake)";

#[test]
fn a_capability_the_program_answers_declares_the_reply_sum_of_every_operation() {
    let expected = "error[answer-shape]: capability 'Pool' is answered by module 'Ledger', and it declares no type 'Pool.GoneReply' beside operation 'gone'; an answer to 'Pool.gone' is read through that sum, which is exactly:\n";
    for door in ["check", "run", "verify"] {
        assert_reports("answer_shape_reply_missing", &[door], expected);
        assert_reports("answer_shape_reply_missing", &[door], GONE_REPLY);
    }
}

/// The natural way to miss the declaration: the answer function is written
/// first, names `Pool.GoneReply` in its signature, and the program has an
/// unknown type before it has a gate. The declaration to paste is printed
/// on every door all the same, from the capability registry alone.
#[test]
fn an_answer_function_naming_an_undeclared_reply_sum_is_told_the_declaration() {
    let expected = "error[answer-shape]: capability 'Pool' is answered by module 'Ledger', and it declares no type 'Pool.GoneReply' beside operation 'gone'";
    for door in ["check", "run", "verify"] {
        let out = aver("answer_shape_reply_named", &[door]);
        let text = combined(&out);
        assert!(!out.status.success(), "{}", format_output(&out));
        assert!(
            text.contains(expected) && text.contains(GONE_REPLY),
            "expected the declaration on `{door}` in:\n{}",
            format_output(&out)
        );
    }
    // `check` reports every module through source analysis, so the unknown
    // type stands beside the declaration; `run` and `verify` stop at the
    // door and print the declaration alone.
    let out = aver("answer_shape_reply_named", &["check"]);
    assert!(
        combined(&out).contains("Unknown type 'Pool.GoneReply'"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn the_now_of_a_reply_sum_carries_exactly_the_operations_result() {
    assert_reports(
        "answer_shape_reply_payload",
        &["check"],
        "error[answer-shape]: capability 'Pool' is answered by module 'Ledger', and constructor 'Pool.ClaimReply.Now' carries (Int) where 'Pool.claim' answers Pool.Assignment; an answer to 'Pool.claim' is read through that sum, which is exactly:\n    type ClaimReply\n        Now(Pool.Assignment)\n        Later(Wait.Wake)",
    );
}

#[test]
fn a_reply_sum_has_no_third_constructor() {
    assert_reports(
        "answer_shape_reply_third",
        &["check"],
        "error[answer-shape]: capability 'Pool' is answered by module 'Ledger', and sum 'Pool.ClaimReply' declares constructor 'Never', which is neither Now nor Later",
    );
}

#[test]
fn the_later_of_a_reply_sum_carries_the_wake() {
    assert_reports(
        "answer_shape_reply_wake",
        &["check"],
        "error[answer-shape]: capability 'Pool' is answered by module 'Ledger', and constructor 'Pool.ClaimReply.Later' carries (Int) where a Later carries the wake, Wait.Wake",
    );
}

/// A reply sum is only held to its shape on a capability an `answer` binding
/// names: a host-provided capability has no answer module, so a `<Op>Reply`
/// beside its operation is an ordinary type of the module.
#[test]
fn a_reply_sum_on_a_capability_nothing_answers_is_an_ordinary_type() {
    let out = aver("answer_shape_reply_unanswered", &["check"]);
    let text = combined(&out);
    assert!(
        out.status.success() && !text.contains("error["),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("Checked 4 module(s): 4 passed"),
        "{}",
        format_output(&out)
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
        "error[answer-shape]: aver.toml marks capability 'Pool' as answered by 'Ledger', and 'Ledger.claim' declares `yield`",
    );
}

#[test]
fn an_answer_function_with_effects_is_allowed_and_said_so() {
    let out = aver("answer_shape_effectful_answer", &["check"]);
    let text = combined(&out);
    assert!(
        text.contains("warning[answer-shape]: aver.toml marks capability 'Pool' as answered by 'Ledger', and 'Ledger.claim' declares effects [Console.print]"),
        "{}",
        format_output(&out)
    );
    assert!(
        !text.contains("error[answer-shape]"),
        "an effectful answer is a warning, not a refusal:\n{}",
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

// ── work-binding: the three ends of the job seam ────────────────────────

#[test]
fn a_seam_end_is_typed_against_the_job_kinds_task() {
    assert_reports(
        "answer_seam_type_mismatch",
        &["check"],
        "job kind 'Validation' binds task = \"Ledger.nextTask\", so that function must be (Ledger.State) -> Option<String>; it is (Ledger.State) -> Option<Int>",
    );
}

#[test]
fn all_ends_of_the_seam_are_pure() {
    assert_reports(
        "answer_seam_effectful",
        &["check"],
        "job kind 'Validation' binds task = \"Ledger.nextTask\", but that function declares effects [Console.print]; the turn reads the seam between waits, so all three of its ends are pure",
    );
}

#[test]
fn a_seam_end_names_a_function_of_an_answer_module() {
    let out = aver("answer_seam_unanswered_module", &["check"]);
    let text = combined(&out);
    for field in [
        "task = \"Node.nextTask\"",
        "started = \"Node.taskStarted\"",
        "landed = \"Node.validated\"",
    ] {
        assert!(
            text.contains(&format!(
                "job kind 'Validation' binds {field}, but no `answer` binding in aver.toml names module 'Node'"
            )),
            "{}",
            format_output(&out)
        );
    }
}

#[test]
fn a_module_that_sees_the_job_kind_but_not_the_answer_module_is_not_accused() {
    // The seam is checked against the answer module's state, and a module
    // whose own closure does not reach that module simply cannot see it. The
    // manifest binds it all the same, so there is nothing to report here.
    let dir = fixture("answer_seam_partial_closure");
    let mut command = Command::new(aver_bin());
    command.current_dir(repo_root());
    command
        .arg("check")
        .arg(dir.join("runner.av"))
        .arg("--module-root")
        .arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    assert!(
        !text.contains("error["),
        "a partial closure is not a manifest error:\n{}",
        format_output(&out)
    );
}

#[test]
fn a_seam_end_naming_nothing_in_the_program_is_refused() {
    assert_reports(
        "answer_seam_unknown_function",
        &["check"],
        "job kind 'Validation' binds task = \"Ledger.nextTask\", but this program has no function 'Ledger.nextTask'",
    );
}

/// The `started` end records that the task `begin` was handed is now running,
/// so it takes the answer state and that task — not a task of a different
/// type, and nothing that leaves the state unwritten.
#[test]
fn the_started_end_is_typed_against_the_job_kinds_task() {
    assert_reports(
        "answer_seam_started_shape",
        &["check"],
        "job kind 'Validation' binds started = \"Ledger.taskStarted\", so that function must be (Ledger.State, String) -> Ledger.State; it is (Ledger.State, Int) -> Ledger.State",
    );
}

/// `task` and `started` are one seam over one state: the task is consumed
/// from the state that offered it. A `started` whose shape is right over
/// another answer module's state is refused all the same, and by this check
/// rather than by the loop generator, because a program without `[run]`
/// binds the seam too.
#[test]
fn the_task_and_started_ends_name_one_module() {
    let expected = "error[work-binding]: job kind 'Validation' binds task = \"Ledger.nextTask\" and started = \"Timer.taskStarted\", but those are functions of two modules, 'Ledger' and 'Timer'; the task is consumed from the state that offered it, so `task` and `started` name functions of one answer module";
    assert_reports("answer_seam_started_module", &["check"], expected);
    assert_reports("answer_seam_started_module", &["run"], expected);
}

// ── the manifest keys themselves ────────────────────────────────────────

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

#[test]
fn an_answer_binding_cannot_also_name_a_provider_package() {
    assert_manifest_rejects(
        "package",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Pool\"\nanswer = \"Ledger\"\ncrate = \"pool_provider\"\npackage = \"aver-pool-provider\"\nfactory = \"binding\"\nversion = \"=0.1.0\"\n",
        "declares both `answer` and `crate`",
    );
}

#[test]
fn an_answer_binding_cannot_also_be_a_work_binding() {
    assert_manifest_rejects(
        "work",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Pool\"\nanswer = \"Ledger\"\nwork = \"Ledger.claim\"\n",
        "declares both `answer` and `work`",
    );
}

#[test]
fn an_answer_value_must_name_one_module() {
    assert_manifest_rejects(
        "qualified",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Pool\"\nanswer = \"Ledger.claim\"\n",
        "answer 'Ledger.claim' must name one module of the program",
    );
}

#[test]
fn the_job_seam_lives_on_a_work_binding() {
    assert_manifest_rejects(
        "seam-on-answer",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Pool\"\nanswer = \"Ledger\"\ntask = \"Ledger.nextTask\"\nlanded = \"Ledger.validated\"\n",
        "declares `task` beside `answer`",
    );
}

#[test]
fn the_job_seam_has_three_ends() {
    assert_manifest_rejects(
        "half-seam",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Validation\"\nwork = \"Node.validate\"\ntask = \"Ledger.nextTask\"\n",
        "declares `task` without `started`",
    );
    assert_manifest_rejects(
        "unrecorded-seam",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Validation\"\nwork = \"Node.validate\"\ntask = \"Ledger.nextTask\"\nlanded = \"Ledger.validated\"\n",
        "declares `task` and `landed` without `started`",
    );
    assert_manifest_rejects(
        "unlanded-seam",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Validation\"\nwork = \"Node.validate\"\ntask = \"Ledger.nextTask\"\nstarted = \"Ledger.taskStarted\"\n",
        "declares `task` and `started` without `landed`",
    );
}

/// A capability the compiler ships cannot be answered, and the door says so
/// before it looks for reply sums: `Tcp` declares no `ReadReply`, and no door
/// asks it to, so every door reports the binding itself and nothing else.
#[test]
fn answering_a_shipped_capability_reachable_from_wait_reports_the_binding_only() {
    let expected = "error[answer-binding]: aver.toml: [[providers.bindings]] index 0 binds capability 'Tcp' with `answer`";
    for door in ["check", "run", "verify"] {
        assert_reports("answer_shape_reserved_reachable", &[door], expected);
        let out = aver("answer_shape_reserved_reachable", &[door]);
        let text = combined(&out);
        assert!(
            !text.contains("Circular import") && !text.contains("ReadReply"),
            "{door} reports the binding, not a reply sum Tcp was never asked for:\n{}",
            format_output(&out)
        );
    }
}

// ── intercept-outside-yield: a request is only a request in a process ────

/// An operation of an answered capability is a request, and only a `yield`
/// function makes one: the lowering cuts a process at the call and hands it
/// to the coordinator. A plain function calling the same operation has
/// nobody to answer it — the capability has no provider, and no request kind
/// was generated for the call — so both program doors refuse it and the
/// message names the answer function to call instead.
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
