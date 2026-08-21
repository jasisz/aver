//! Regression — `[[check.suppress]]` must behave the same whichever way the
//! path was spelled on the command line, `aver audit` must honour it too, and
//! a waiver that removes nothing must say so.
//!
//! Suppression used to be matched against the display path, which keeps the
//! spelling the user typed: `aver check domain/version.av` suppressed, while
//! `aver check .` (which walks to `./domain/version.av`) and `--module-root .`
//! for dependency modules did not. The glob was anchored byte matching with no
//! `./` normalisation, so the pattern side was broken symmetrically — a rule
//! written `files = ["./domain/version.av"]` matched nothing at all.
//! `cmd_audit` never loaded `aver.toml`, so its check leg could not be made
//! green through project config in any spelling.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// Fixture module: no `intent`, no `effects`, and no `?` description.
/// That is exactly two `warning[check]` diagnostics plus one
/// `warning[missing-description]` — the latter is the control that proves a
/// waiver for `check` is targeted rather than blanket.
const VERSION_AV: &str = "module Version\n    exposes [bump]\n\nfn bump(n: Int) -> Int\n    n + 1\n\nverify bump\n    bump(1) => 2\n";

/// Entry module depending on the fixture, used for the dependency leg:
/// `aver check app.av` checks every module of app.av's program.
const APP_AV: &str = "module App\n    depends [Domain.Version]\n    intent =\n        \"Entry point for the suppression fixture.\"\n    effects []\n\nfn next(n: Int) -> Int\n    ? \"One step forward.\"\n    Domain.Version.bump(n)\n\nverify next\n    next(1) => 2\n";

/// A project rooted at a fresh temp directory, removed on drop so a failing
/// assertion does not leave fixtures behind.
struct Project {
    dir: PathBuf,
}

impl Project {
    fn new(tag: &str) -> Self {
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let dir = std::env::temp_dir().join(format!("aver_suppress_{tag}_{nanos}"));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(dir.join("domain")).expect("create fixture dirs");
        fs::write(dir.join("domain").join("version.av"), VERSION_AV).expect("write version.av");
        Project { dir }
    }

    fn write(&self, rel: &str, contents: &str) {
        let path = self.dir.join(rel);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("create parent dir");
        }
        fs::write(path, contents).expect("write fixture file");
    }

    fn remove(&self, rel: &str) {
        let _ = fs::remove_file(self.dir.join(rel));
    }

    fn abs(&self, rel: &str) -> String {
        self.dir
            .join(rel)
            .to_str()
            .expect("utf8 fixture path")
            .to_string()
    }

    /// Run `aver` from inside the project — the whole point is how the path
    /// argument is spelled relative to the working directory.
    fn run(&self, args: &[&str]) -> Run {
        let out: Output = Command::new(env!("CARGO_BIN_EXE_aver"))
            .args(args)
            .current_dir(&self.dir)
            .output()
            .expect("spawn aver");
        Run {
            code: out.status.code(),
            stdout: String::from_utf8_lossy(&out.stdout).to_string(),
            stderr: String::from_utf8_lossy(&out.stderr).to_string(),
        }
    }
}

impl Drop for Project {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

struct Run {
    code: Option<i32>,
    stdout: String,
    stderr: String,
}

impl Run {
    fn describe(&self, label: &str) -> String {
        format!(
            "{label}\n--- exit: {:?}\n--- stdout:\n{}\n--- stderr:\n{}",
            self.code, self.stdout, self.stderr
        )
    }
}

fn waiver_for(files: &str) -> String {
    format!(
        "[[check.suppress]]\nslug = \"check\"\nfiles = [{files}]\nreason = \"Fixture waiver for the suppression spelling matrix\"\n"
    )
}

/// Defect 1 — every spelling of the same file honours the same waiver.
#[test]
fn suppression_applies_under_every_path_spelling() {
    let project = Project::new("spelling");
    project.write("aver.toml", &waiver_for("\"domain/version.av\""));

    let absolute = project.abs("domain/version.av");
    let spellings = ["domain/version.av", "./domain/version.av", ".", &absolute];

    for spelling in spellings {
        let run = project.run(&["check", spelling]);
        assert!(
            run.stdout.contains("warning(s) suppressed by aver.toml"),
            "{}",
            run.describe(&format!("`aver check {spelling}` reported no suppression"))
        );
        assert!(
            !run.stdout.contains("warning[check]"),
            "{}",
            run.describe(&format!(
                "`aver check {spelling}` still printed warning[check]"
            ))
        );
        // The waiver names one slug; unrelated warnings must survive it.
        assert!(
            run.stdout.contains("warning[missing-description]"),
            "{}",
            run.describe(&format!(
                "`aver check {spelling}` swallowed an unwaived warning"
            ))
        );
    }
}

/// Defect 1, pattern side — a glob may itself be written with a leading `./`.
#[test]
fn suppression_glob_may_be_spelled_with_leading_dot_slash() {
    let project = Project::new("dotglob");
    project.write("aver.toml", &waiver_for("\"./domain/version.av\""));

    let run = project.run(&["check", "domain/version.av"]);
    assert!(
        run.stdout.contains("warning(s) suppressed by aver.toml"),
        "{}",
        run.describe("a `./`-prefixed glob suppressed nothing")
    );
    assert!(
        !run.stdout.contains("warning[check]"),
        "{}",
        run.describe("a `./`-prefixed glob left warning[check] in place")
    );
}

/// Defect 1, dependency leg — `--module-root .` builds dependency paths by
/// joining the root, so they arrive spelled `./domain/version.av`.
#[test]
fn deps_suppression_survives_module_root_dot() {
    let project = Project::new("deps");
    project.write("aver.toml", &waiver_for("\"domain/version.av\""));
    project.write("app.av", APP_AV);

    let run = project.run(&["check", "app.av", "--module-root", "."]);
    assert!(
        run.stdout.contains("warning(s) suppressed by aver.toml"),
        "{}",
        run.describe("dependency warnings were not suppressed under --module-root .")
    );
    assert!(
        !run.stdout.contains("warning[check]"),
        "{}",
        run.describe("dependency warning[check] survived the waiver")
    );
}

/// Defect 2 — `aver audit` consults the same `aver.toml` as `aver check`.
#[test]
fn audit_honours_check_suppress() {
    let project = Project::new("audit");
    project.write("aver.toml", &waiver_for("\"domain/version.av\""));

    let run = project.run(&["audit", "domain/version.av", "--json"]);
    let analysis = analysis_line(&run);
    assert!(
        !analysis.contains("\"slug\":\"check\""),
        "{}",
        run.describe("audit ignored the waiver")
    );
    assert!(
        analysis.contains("\"slug\":\"missing-description\""),
        "{}",
        run.describe("audit dropped an unwaived warning")
    );

    // Control: without the config the same run reports the warnings.
    project.remove("aver.toml");
    let control = project.run(&["audit", "domain/version.av", "--json"]);
    assert!(
        analysis_line(&control).contains("\"slug\":\"check\""),
        "{}",
        control.describe("control run should still report warning[check]")
    );
}

/// Defect 2, invariant — suppression is warnings-only, so routing audit
/// through it cannot change an exit code. A waiver naming an error slug is
/// inert.
#[test]
fn audit_suppression_cannot_hide_errors() {
    let project = Project::new("audit_error");
    project.write(
        "aver.toml",
        "[[check.suppress]]\nslug = \"verify-rhs\"\nfiles = [\"**\"]\nreason = \"A waiver must not be able to hide an error\"\n",
    );
    project.write(
        "domain/version.av",
        "module Version\n    intent =\n        \"Fixture: verify case calls the target on the right side.\"\n    exposes [double]\n    effects []\n\nfn double(x: Int) -> Int\n    ? \"Doubles the input\"\n    x * 2\n\nverify double\n    double(2) => double(2)\n",
    );

    let run = project.run(&["audit", "domain/version.av", "--json"]);
    assert!(
        run.stdout.contains("\"slug\":\"verify-rhs\""),
        "{}",
        run.describe("a waiver must not remove an error diagnostic")
    );
    let summary = run
        .stdout
        .lines()
        .find(|l| l.contains("\"kind\":\"summary\""))
        .unwrap_or_else(|| panic!("{}", run.describe("no summary line")));
    assert!(
        summary.contains("\"check_errors\":1"),
        "{}",
        run.describe("the error must still be counted")
    );
    assert_eq!(
        run.code,
        Some(1),
        "{}",
        run.describe("the error must still fail the audit")
    );
}

/// Defect 3 — a rule that removed nothing during a whole-directory run is
/// named on stderr, without touching the exit code or the live rule.
#[test]
fn dead_suppress_rule_warns_on_directory_run() {
    let project = Project::new("dead");
    project.write(
        "aver.toml",
        &format!(
            "{}\n[[check.suppress]]\nslug = \"check\"\nfiles = [\"nowhere/*.av\"]\nreason = \"Points at a path that no longer exists\"\n",
            waiver_for("\"domain/version.av\"")
        ),
    );

    let run = project.run(&["check", "."]);
    assert!(
        run.stderr.contains("check.suppress") && run.stderr.contains("nowhere/*.av"),
        "{}",
        run.describe("the dead rule was not reported")
    );
    assert!(
        !run.stderr.contains("domain/version.av"),
        "{}",
        run.describe("the live rule must not be reported as dead")
    );
    assert_eq!(
        run.code,
        Some(0),
        "{}",
        run.describe("waiver hygiene must not change the exit code")
    );
}

/// Defect 3, scope — checking one file legitimately leaves waivers for other
/// paths unexercised, so the warning is reserved for directory walks.
#[test]
fn dead_suppress_rule_is_silent_on_single_file_run() {
    let project = Project::new("dead_single");
    project.write(
        "aver.toml",
        &format!(
            "{}\n[[check.suppress]]\nslug = \"check\"\nfiles = [\"nowhere/*.av\"]\nreason = \"Points at a path that no longer exists\"\n",
            waiver_for("\"domain/version.av\"")
        ),
    );

    let run = project.run(&["check", "domain/version.av"]);
    assert!(
        !run.stderr.contains("check.suppress"),
        "{}",
        run.describe("a single-file run must not judge waivers it never exercised")
    );
}

/// `aver audit` now loads `aver.toml`, so a malformed one stops the run
/// instead of being silently ignored — same as `aver check` has always done.
#[test]
fn audit_rejects_a_malformed_config() {
    let project = Project::new("badconfig");
    project.write(
        "aver.toml",
        "[[check.suppress]]\nslug = \"check\"\nfiles = [\"domain/version.av\"]\n",
    );

    let run = project.run(&["audit", "domain/version.av"]);
    assert_eq!(
        run.code,
        Some(1),
        "{}",
        run.describe("a malformed aver.toml must stop the audit")
    );
    assert!(
        run.stderr.contains("reason"),
        "{}",
        run.describe("the config error must name the missing key")
    );
}

fn analysis_line(run: &Run) -> String {
    run.stdout
        .lines()
        .find(|l| l.contains("\"kind\":\"analysis\""))
        .unwrap_or_else(|| panic!("{}", run.describe("no analysis line in audit output")))
        .to_string()
}

/// Guard on the fixture itself: the assertions above are only meaningful if
/// the unsuppressed file really does emit `warning[check]`.
#[test]
fn fixture_emits_the_warning_that_is_being_waived() {
    let project = Project::new("fixture");
    assert!(!Path::new(&project.abs("aver.toml")).exists());

    let run = project.run(&["check", "domain/version.av"]);
    assert!(
        run.stdout.contains("warning[check]"),
        "{}",
        run.describe("fixture no longer emits warning[check]")
    );
    assert!(
        !run.stdout.contains("suppressed by aver.toml"),
        "{}",
        run.describe("fixture must not be suppressed without a config")
    );
}
