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

#[test]
fn a_job_kind_the_program_actually_calls_passes_every_door() {
    // Declaring a job kind is not the interesting case: calling it is. A
    // program that reaches `Validation.begin` must still pass the manifest
    // gate, because the `work` binding is what says who answers it.
    assert_no_work_diagnostic("work_shape_used", &["check"]);
    let out = aver("work_shape_used", &["check"]);
    assert!(out.status.success(), "{}", format_output(&out));

    // The VM answers the job kind from the manifest binding; what it must
    // never do is demand a Rust provider package for a capability the
    // manifest already binds to a function of the program.
    let out = aver("work_shape_used", &["run"]);
    let text = combined(&out);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        !text.contains("missing required custom capability binding"),
        "a work binding satisfies static composition:\n{}",
        format_output(&out)
    );
    assert!(
        !text.contains("capability provider missing for 'Validation.begin'"),
        "a bound job kind has its provider:\n{}",
        format_output(&out)
    );
    assert!(text.contains("job started"), "{}", format_output(&out));
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

#[test]
fn a_task_carrying_a_capability_resource_is_a_shape_error() {
    assert_reports(
        "work_shape_resource_task",
        &["check"],
        "error[work-shape]: operation 'Validation.begin' takes a task containing capability resource 'Tcp.Connection'",
    );
}

#[test]
fn begin_takes_exactly_the_task() {
    assert_reports(
        "work_shape_begin_arity",
        &["check"],
        "error[work-shape]: operation 'Validation.begin' must take exactly one task parameter; it takes 2",
    );
}

#[test]
fn take_takes_exactly_the_job_handle() {
    assert_reports(
        "work_shape_take_parameter",
        &["check"],
        "error[work-shape]: operation 'Validation.take' must take exactly one Work.Job parameter",
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
fn a_directory_is_the_same_program_door_as_a_file() {
    // `aver verify .` is the ordinary project form; it must refuse exactly
    // what `aver verify main.av` refuses.
    let dir = fixture("work_shape_unbound");
    let mut command = Command::new(aver_bin());
    command.current_dir(repo_root());
    command
        .arg("verify")
        .arg(&dir)
        .arg("--module-root")
        .arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    assert!(
        text.contains("error[work-binding]: job kind 'Validation' has no `work` binding"),
        "{}",
        format_output(&out)
    );
    assert_eq!(
        text.matches("error[work-binding]").count(),
        1,
        "one finding per program, however many files the door walked:\n{}",
        format_output(&out)
    );
    assert!(!out.status.success(), "{}", format_output(&out));
}

#[test]
fn a_bound_function_naming_nothing_in_the_program_is_refused() {
    assert_reports(
        "work_shape_unknown_function",
        &["check"],
        "binds work = \"Node.inspect\", but this program has no function 'Node.inspect'",
    );
}

#[test]
fn a_same_named_record_of_another_module_is_not_the_task_type() {
    // Nominal identity, not the last path segment: `Node.Task` and
    // `Validation.Task` are two types, and the bound function must take the
    // one `begin` hands it.
    assert_reports(
        "work_shape_nominal_task",
        &["check"],
        "whose parameter is Node.Task; 'Validation.begin' hands it Validation.Task",
    );
}

#[test]
fn a_record_named_after_a_standard_module_is_still_its_own_modules_type() {
    // A module name the compiler ships is not a type name. A program may
    // declare `record Http`, and two such records of two modules are two
    // types, exactly as `Task` is.
    assert_reports(
        "work_shape_stdlib_named_task",
        &["check"],
        "whose parameter is Node.Http; 'Validation.begin' hands it Validation.Http",
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

/// The Rust backend answers a job kind since jasisz/aver#1329: the job
/// engine and the job-kind adapter are in `aver-rt`, and the bound function
/// is compiled into the same crate. What the generated program then does
/// with a job is `tests/rust_work_spec.rs`; here the point is only that the
/// door opened.
#[test]
fn the_rust_backend_accepts_a_program_with_a_job_kind() {
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
    assert!(!text.contains("error[work-target]"), "{text}");
    assert!(out.status.success(), "{}", format_output(&out));
    let _ = std::fs::remove_dir_all(std::env::temp_dir().join("aver-work-target-rust"));
}

#[test]
fn the_wasm_gc_runner_refuses_a_program_with_a_job_kind() {
    let out = aver("work_shape_ok", &["run", "--wasm-gc"]);
    let text = combined(&out);
    assert!(
        text.contains(
            "error[work-target]: Work-bound capabilities run on the VM and the Rust backend in this build"
        ),
        "{}",
        format_output(&out)
    );
    assert!(text.contains("the requested target is wasm-gc"), "{text}");
    assert!(!out.status.success(), "{}", format_output(&out));
}

#[test]
fn the_wasip2_backend_refuses_a_program_with_a_job_kind() {
    let out = aver(
        "work_shape_ok",
        &[
            "compile",
            "--target",
            "wasip2",
            "-o",
            &std::env::temp_dir()
                .join("aver-work-target-wasip2")
                .to_string_lossy(),
        ],
    );
    let text = combined(&out);
    assert!(
        text.contains(
            "error[work-target]: Work-bound capabilities run on the VM and the Rust backend in this build"
        ),
        "{}",
        format_output(&out)
    );
    assert!(text.contains("the requested target is wasip2"), "{text}");
    assert!(!out.status.success(), "{}", format_output(&out));
}
