//! The Work shape and its manifest binding (jasisz/aver#1329, leg 1.1).
//!
//! The `tests/fixtures/work_shape_*` family holds one program per rule: a
//! good job kind that runs, and one program per way of getting the shape,
//! the binding, or the target wrong. Every case goes through the real CLI,
//! because the rule's whole point is that `aver check`, `aver run`,
//! `aver verify` and `aver compile` agree about what a job kind means.

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

fn assert_no_work_diagnostic(fixture_name: &str, args: &[&str]) {
    let out = aver(fixture_name, args);
    let text = combined(&out);
    assert!(
        !text.contains("error[work-"),
        "unexpected work diagnostic in:\n{}",
        format_output(&out)
    );
}

// ── A job kind that fits ────────────────────────────────────────────────

#[test]
fn a_bound_job_kind_passes_check() {
    assert_no_work_diagnostic("work_shape_ok", &["check"]);
    let out = aver("work_shape_ok", &["check"]);
    assert!(out.status.success(), "{}", format_output(&out));
}

#[test]
fn a_bound_job_kind_runs_on_the_vm() {
    let out = aver("work_shape_ok", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("jobs ready"),
        "{}",
        format_output(&out)
    );
}

// ── work-shape ──────────────────────────────────────────────────────────

#[test]
fn a_third_operation_is_a_shape_error() {
    assert_reports(
        "work_shape_extra_operation",
        &["check"],
        "error[work-shape]: capability 'Validation' names the job handle Work.Job",
    );
    assert_reports(
        "work_shape_extra_operation",
        &["run"],
        "error[work-shape]: capability 'Validation' names the job handle Work.Job",
    );
}

#[test]
fn begin_must_return_a_result_carrying_the_job_handle() {
    assert_reports(
        "work_shape_begin_result",
        &["check"],
        "error[work-shape]: operation 'Validation.begin' must return Result<Work.Job, String>",
    );
}

#[test]
fn take_must_be_able_to_report_a_running_job() {
    assert_reports(
        "work_shape_take_result",
        &["check"],
        "error[work-shape]: operation 'Validation.take' must return Result<Option<R>, String>",
    );
}

// ── work-binding ────────────────────────────────────────────────────────

#[test]
fn a_job_kind_without_a_binding_is_refused_at_the_program_door() {
    let expected = "error[work-binding]: job kind 'Validation' has no `work` binding";
    assert_reports("work_shape_unbound", &["check"], expected);
    assert_reports("work_shape_unbound", &["run"], expected);
    assert_reports("work_shape_unbound", &["verify"], expected);
}

#[test]
fn a_capability_module_checked_alone_needs_no_binding() {
    let dir = fixture("work_shape_unbound");
    let mut command = Command::new(aver_bin());
    command.current_dir(repo_root());
    command
        .arg("check")
        .arg(dir.join("validation.av"))
        .arg("--module-root")
        .arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    assert!(
        !text.contains("error[work-binding]"),
        "a capability module is not yet a program:\n{}",
        format_output(&out)
    );
}

#[test]
fn a_bound_function_with_effects_is_refused() {
    assert_reports(
        "work_shape_effectful_worker",
        &["check"],
        "but that function declares effects [Console.print]",
    );
}

#[test]
fn a_bound_function_of_the_wrong_result_type_is_refused() {
    assert_reports(
        "work_shape_type_mismatch",
        &["check"],
        "which returns String; 'Validation.take' yields Int",
    );
}

// ── the manifest key itself ─────────────────────────────────────────────

fn temp_project(label: &str, manifest: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock after epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver-work-manifest-{label}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create temp project");
    let source = fixture("work_shape_ok");
    for file in ["main.av", "node.av", "validation.av"] {
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

#[test]
fn a_work_binding_cannot_also_name_a_provider_package() {
    let dir = temp_project(
        "conflict",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Validation\"\nwork = \"Node.validate\"\ncrate = \"validation_provider\"\npackage = \"aver-validation-provider\"\nfactory = \"binding\"\nversion = \"=0.1.0\"\n",
    );
    let out = check_project(&dir);
    let text = combined(&out);
    assert!(
        text.contains("declares both `work` and `crate`"),
        "{}",
        format_output(&out)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_work_value_must_name_a_module_qualified_function() {
    let dir = temp_project(
        "unqualified",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Validation\"\nwork = \"validate\"\n",
    );
    let out = check_project(&dir);
    let text = combined(&out);
    assert!(
        text.contains("must name one module-qualified function of the program"),
        "{}",
        format_output(&out)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_work_binding_on_a_capability_that_is_not_a_job_kind_is_refused() {
    let dir = temp_project(
        "not-a-job-kind",
        "[providers]\nschema = 1\n\n[[providers.bindings]]\ncapability = \"Console\"\nwork = \"Node.validate\"\n",
    );
    let out = check_project(&dir);
    let text = combined(&out);
    assert!(
        text.contains("with `work`, but that capability is not a job kind"),
        "{}",
        format_output(&out)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

// ── work-target ─────────────────────────────────────────────────────────

#[test]
fn the_rust_backend_refuses_a_program_with_a_job_kind() {
    let out = aver(
        "work_shape_ok",
        &[
            "compile",
            "--target",
            "rust",
            "-o",
            &std::env::temp_dir()
                .join("aver-work-target-rust")
                .to_string_lossy(),
        ],
    );
    let text = combined(&out);
    assert!(
        text.contains("error[work-target]: Work-bound capabilities run on the VM in this build"),
        "{}",
        format_output(&out)
    );
    assert!(text.contains("the requested target is rust"), "{text}");
    assert!(!out.status.success(), "{}", format_output(&out));
}

#[test]
fn the_wasm_gc_runner_refuses_a_program_with_a_job_kind() {
    let out = aver("work_shape_ok", &["run", "--wasm-gc"]);
    let text = combined(&out);
    assert!(
        text.contains("error[work-target]: Work-bound capabilities run on the VM in this build"),
        "{}",
        format_output(&out)
    );
    assert!(text.contains("the requested target is wasm-gc"), "{text}");
    assert!(!out.status.success(), "{}", format_output(&out));
}
