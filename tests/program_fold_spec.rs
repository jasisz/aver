//! A file names a program: the entry module plus everything it reaches
//! through `depends [...]`. `aver check`, `aver verify` and `aver audit`
//! report every module of that program, leaves-first, and fail when any
//! module fails.
//! Embedded standard modules are part of the program for typing but are not
//! units of the report: their own `verify` blocks are checked per release.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// The two-module program from the issue that motivated the fold: a
/// dependency carrying three cases and one law (nine samples), and a thin
/// entry with one case of its own.
const LIB_AV: &str = "module Lib\n    intent = \"Dependency carrying both a law and a plain verify.\"\n    exposes [double]\n    effects []\n\nfn double(n: Int) -> Int\n    ? \"Doubles n.\"\n    n * 2\n\nverify double\n    double(3) => 6\n    double(0) => 0\n    double(-4) => -8\n\nverify double law doubleAdditive\n    given a: Int = [0, 1, 7]\n    given b: Int = [0, 2, 5]\n    double(a + b) => double(a) + double(b)\n";

const ENTRY_AV: &str = "module Entry\n    intent = \"Pure entry that uses the dependency.\"\n    depends [Lib]\n    exposes [quad]\n    effects []\n\nfn quad(n: Int) -> Int\n    ? \"Quadruples n.\"\n    Lib.double(Lib.double(n))\n\nverify quad\n    quad(2) => 8\n";

/// A dependency two programs share: `double` is for Entry, `triple` for
/// Other, and nobody imports `spare`.
const SHARED_LIB_AV: &str = "module Lib\n    intent = \"Dependency shared by two programs.\"\n    exposes [double, triple, spare]\n    effects []\n\nfn double(n: Int) -> Int\n    ? \"Doubles n.\"\n    n * 2\n\nfn triple(n: Int) -> Int\n    ? \"Triples n.\"\n    n * 3\n\nfn spare(n: Int) -> Int\n    ? \"Exposed for nobody.\"\n    n\n\nverify double\n    double(2) => 4\n\nverify triple\n    triple(2) => 6\n\nverify spare\n    spare(1) => 1\n";

/// A second entry over the same dependency, using only `triple`.
const OTHER_AV: &str = "module Other\n    intent = \"Second entry that uses the other exposed name.\"\n    depends [Lib]\n    exposes [sext]\n    effects []\n\nfn sext(n: Int) -> Int\n    ? \"Sextuples n.\"\n    Lib.triple(n) * 2\n\nverify sext\n    sext(1) => 6\n";

/// The same entry without a single verify block of its own.
const THIN_ENTRY_AV: &str = "module Thin\n    intent = \"Entry with no cases of its own.\"\n    depends [Lib]\n    exposes [quad]\n    effects []\n\nfn quad(n: Int) -> Int\n    ? \"Quadruples n.\"\n    Lib.double(Lib.double(n))\n";

/// An entry that depends on the embedded `Bytes` module, which carries
/// verify blocks of its own.
const BYTES_APP_AV: &str = "module BytesApp\n    intent = \"Count validated bytes.\"\n    depends [Bytes]\n    exposes [byteCount]\n    effects []\n\nfn byteCount(values: List<Int>) -> Result<Int, String>\n    ? \"Validate bytes and count them.\"\n    bytes = Bytes.fromList(values)?\n    Result.Ok(List.len(Bytes.toList(bytes)))\n\nverify byteCount\n    byteCount([1, 2]) => Result.Ok(2)\n";

struct Project {
    dir: PathBuf,
}

impl Project {
    fn new(tag: &str) -> Self {
        let dir = std::env::temp_dir().join(format!(
            "aver-program-fold-{tag}-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("clock")
                .as_nanos()
        ));
        fs::create_dir_all(&dir).expect("create project dir");
        Project { dir }
    }

    fn write(&self, name: &str, contents: &str) -> &Self {
        fs::write(self.dir.join(name), contents).expect("write fixture");
        self
    }

    fn run(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_aver"))
            .current_dir(&self.dir)
            .args(args)
            .arg("--module-root")
            .arg(".")
            .output()
            .expect("run aver")
    }
}

impl Drop for Project {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

fn text(output: &Output) -> (String, String) {
    (
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

fn report(output: &Output) -> String {
    let (stdout, stderr) = text(output);
    format!("stdout:\n{stdout}\nstderr:\n{stderr}")
}

fn position(haystack: &str, needle: &str) -> usize {
    haystack
        .find(needle)
        .unwrap_or_else(|| panic!("expected {needle:?} in:\n{haystack}"))
}

/// The line printed right after each line that starts with `header`.
fn lines_after<'a>(stdout: &'a str, header: &str) -> Vec<&'a str> {
    let lines = stdout.lines().collect::<Vec<_>>();
    lines
        .iter()
        .enumerate()
        .filter(|(_, line)| line.starts_with(header))
        .map(|(idx, _)| lines.get(idx + 1).copied().unwrap_or_default())
        .collect()
}

#[test]
fn verify_entry_samples_every_module_of_the_program() {
    let project = Project::new("verify-program");
    project.write("lib.av", LIB_AV).write("entry.av", ENTRY_AV);

    let run = project.run(&["verify", "entry.av"]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert!(stdout.contains("13/13 cases passed"), "{stdout}");
    assert!(stdout.contains("Summary: 2 modules | 3 blocks"), "{stdout}");

    // Leaves first: the dependency is reported before the entry.
    assert!(
        position(&stdout, "Verify: ./lib.av") < position(&stdout, "Verify: entry.av"),
        "{stdout}"
    );

    let json = project.run(&["verify", "entry.av", "--json"]);
    assert!(json.status.success(), "{}", report(&json));
    let (stdout, _) = text(&json);
    let summary = stdout.lines().last().unwrap_or_default();
    assert!(
        summary.contains(
            "\"kind\":\"summary\",\"files\":1,\"modules\":2,\"blocks\":3,\"cases_passed\":13,\"cases_failed\":0"
        ),
        "{summary}"
    );
}

#[test]
fn verify_entry_without_cases_still_samples_its_dependencies() {
    let project = Project::new("verify-thin");
    project
        .write("lib.av", LIB_AV)
        .write("thin.av", THIN_ENTRY_AV);

    let run = project.run(&["verify", "thin.av"]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert!(stdout.contains("12/12 cases passed"), "{stdout}");
    assert!(stdout.contains("Summary: 1 module | 2 blocks"), "{stdout}");
    assert!(!stdout.contains("No verify blocks found"), "{stdout}");
}

#[test]
fn verify_fails_on_a_dependency_case_and_names_the_module() {
    let project = Project::new("verify-dep-fails");
    project
        .write(
            "lib.av",
            &LIB_AV.replace("double(3) => 6", "double(3) => 7"),
        )
        .write("entry.av", ENTRY_AV);

    let run = project.run(&["verify", "entry.av"]);
    assert!(!run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert!(stdout.contains("Verify: ./lib.av"), "{stdout}");
    assert!(stdout.contains("12/13 cases passed | 1 failed"), "{stdout}");
}

#[test]
fn directory_verify_reports_each_module_once() {
    let project = Project::new("verify-dir");
    project.write("lib.av", LIB_AV).write("entry.av", ENTRY_AV);

    let run = project.run(&["verify", "."]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert_eq!(stdout.matches("Verify: ").count(), 2, "{stdout}");
    assert!(stdout.contains("13/13 cases passed"), "{stdout}");
    assert!(stdout.contains("Summary: 2 modules"), "{stdout}");
    // No empty section: every `Verify:` header is followed by its blocks.
    let after_headers = lines_after(&stdout, "Verify: ");
    assert_eq!(after_headers.len(), 2, "{stdout}");
    assert!(
        after_headers.iter().all(|line| line.starts_with("  ")),
        "{stdout}"
    );
}

#[test]
fn parallel_verify_matches_j1_bytes_and_keeps_shared_module_ownership() {
    let project = Project::new("verify-parallel-deterministic");
    project
        .write("lib.av", SHARED_LIB_AV)
        .write("entry.av", ENTRY_AV)
        .write("other.av", OTHER_AV);

    let sequential = project.run(&["verify", ".", "--json", "-j", "1"]);
    let parallel = project.run(&["verify", ".", "--json", "-j", "4"]);
    assert!(sequential.status.success(), "{}", report(&sequential));
    assert!(parallel.status.success(), "{}", report(&parallel));
    assert_eq!(parallel.status.code(), sequential.status.code());
    assert_eq!(
        parallel.stdout, sequential.stdout,
        "parallel stdout drifted"
    );
    assert_eq!(
        parallel.stderr, sequential.stderr,
        "parallel stderr drifted"
    );

    let stdout = String::from_utf8_lossy(&parallel.stdout);
    assert_eq!(
        stdout.matches("\"kind\":\"analysis\"").count(),
        3,
        "{stdout}"
    );
    assert_eq!(
        stdout.matches("\"file_label\":\"./lib.av\"").count(),
        1,
        "{stdout}"
    );
}

#[test]
fn parallel_verify_preserves_failure_coordinates_and_rendering() {
    let project = Project::new("verify-parallel-failure");
    project
        .write(
            "lib.av",
            &LIB_AV.replace("double(-4) => -8", "double(-4) => -7"),
        )
        .write("entry.av", ENTRY_AV);

    let sequential = project.run(&["verify", ".", "--json", "-j", "1"]);
    let parallel = project.run(&["verify", ".", "--json", "-j", "4"]);
    assert!(
        !sequential.status.success(),
        "sequential unexpectedly passed"
    );
    assert!(!parallel.status.success(), "parallel unexpectedly passed");
    assert_eq!(parallel.status.code(), sequential.status.code());
    assert_eq!(
        parallel.stdout, sequential.stdout,
        "parallel stdout drifted"
    );
    assert_eq!(
        parallel.stderr, sequential.stderr,
        "parallel stderr drifted"
    );
    assert!(
        String::from_utf8_lossy(&parallel.stdout).contains("\"slug\":\"verify-mismatch\""),
        "{}",
        report(&parallel)
    );
}

#[test]
fn reused_verify_graph_still_rejects_a_dependency_name_mismatch() {
    let project = Project::new("verify-name-mismatch");
    project
        .write("lib.av", &LIB_AV.replace("module Lib", "module Wrong"))
        .write("entry.av", ENTRY_AV);

    let sequential = project.run(&["verify", "entry.av", "-j", "1"]);
    let parallel = project.run(&["verify", "entry.av", "-j", "4"]);
    assert!(!sequential.status.success(), "{}", report(&sequential));
    assert!(!parallel.status.success(), "{}", report(&parallel));
    assert_eq!(parallel.stdout, sequential.stdout);
    assert_eq!(parallel.stderr, sequential.stderr);
    assert!(
        String::from_utf8_lossy(&sequential.stderr).contains("Module name mismatch"),
        "{}",
        report(&sequential)
    );
}

#[test]
fn reused_verify_graph_propagates_dependency_type_errors_deterministically() {
    let project = Project::new("verify-dependency-type-error");
    project
        .write("lib.av", &LIB_AV.replace("n * 2", "n * \"two\""))
        .write("entry.av", ENTRY_AV);

    let sequential = project.run(&["verify", "entry.av", "-j", "1"]);
    let parallel = project.run(&["verify", "entry.av", "-j", "4"]);
    assert!(!sequential.status.success(), "{}", report(&sequential));
    assert!(!parallel.status.success(), "{}", report(&parallel));
    assert_eq!(parallel.stdout, sequential.stdout);
    assert_eq!(parallel.stderr, sequential.stderr);
    assert!(
        String::from_utf8_lossy(&sequential.stderr).contains("Arithmetic operator requires"),
        "{}",
        report(&sequential)
    );
}

#[test]
fn reused_verify_graph_still_rejects_a_dependency_cycle() {
    let project = Project::new("verify-cycle");
    project
        .write(
            "a.av",
            "module A\n    intent = \"First half of an invalid dependency cycle.\"\n    depends [B]\n    exposes [a]\n    effects []\n\nfn a(n: Int) -> Int\n    ? \"Returns the input.\"\n    n\n\nverify a\n    a(1) => 1\n",
        )
        .write(
            "b.av",
            "module B\n    intent = \"Second half of an invalid dependency cycle.\"\n    depends [A]\n    exposes [b]\n    effects []\n\nfn b(n: Int) -> Int\n    ? \"Returns the input.\"\n    n\n",
        );

    let sequential = project.run(&["verify", "a.av", "-j", "1"]);
    let parallel = project.run(&["verify", "a.av", "-j", "4"]);
    assert!(!sequential.status.success(), "{}", report(&sequential));
    assert!(!parallel.status.success(), "{}", report(&parallel));
    assert_eq!(parallel.stdout, sequential.stdout);
    assert_eq!(parallel.stderr, sequential.stderr);
    assert!(
        String::from_utf8_lossy(&sequential.stderr).contains("Circular import"),
        "{}",
        report(&sequential)
    );
}

#[test]
fn directory_check_reports_each_module_once() {
    let project = Project::new("check-dir");
    project.write("lib.av", LIB_AV).write("entry.av", ENTRY_AV);

    let run = project.run(&["check", "."]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert_eq!(stdout.matches("Check: ").count(), 2, "{stdout}");
    assert!(stdout.contains("Checked 2 module(s): 2 passed"), "{stdout}");
    // lib.av was reported under entry.av's program, so it gets no section
    // of its own: the one `Input:` header opens a non-empty section.
    let after_headers = lines_after(&stdout, "Input: ");
    assert_eq!(after_headers.len(), 1, "{stdout}");
    assert!(
        after_headers.iter().all(|line| line.starts_with("Check: ")),
        "{stdout}"
    );
}

#[test]
fn unused_exposes_are_judged_over_the_whole_directory() {
    let project = Project::new("check-union");
    project
        .write("lib.av", SHARED_LIB_AV)
        .write("entry.av", ENTRY_AV)
        .write("other.av", OTHER_AV);

    let run = project.run(&["check", "."]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert!(stdout.contains("Checked 3 module(s): 3 passed"), "{stdout}");
    // `double` is used by entry.av's program and `triple` by other.av's:
    // only the name no program imports is unused.
    assert_eq!(
        stdout
            .matches("exposes not used by the checked program(s):")
            .count(),
        1,
        "{stdout}"
    );
    assert!(
        stdout.contains("exposes not used by the checked program(s): spare"),
        "{stdout}"
    );
    let after_headers = lines_after(&stdout, "Input: ");
    assert_eq!(after_headers.len(), 2, "{stdout}");
    assert!(
        after_headers.iter().all(|line| line.starts_with("Check: ")),
        "{stdout}"
    );
}

#[test]
fn check_reports_every_module_leaves_first_and_counts_modules() {
    let project = Project::new("check-program");
    project.write("lib.av", LIB_AV).write("entry.av", ENTRY_AV);

    let run = project.run(&["check", "entry.av"]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert!(
        position(&stdout, "Check: ./lib.av") < position(&stdout, "Check: entry.av"),
        "{stdout}"
    );
    assert!(stdout.contains("Checked 2 module(s): 2 passed"), "{stdout}");

    let json = project.run(&["check", "entry.av", "--json"]);
    assert!(json.status.success(), "{}", report(&json));
    let (stdout, _) = text(&json);
    let summary = stdout.lines().last().unwrap_or_default();
    assert!(
        summary
            .contains("\"kind\":\"summary\",\"files\":1,\"modules\":2,\"passed\":2,\"failed\":0"),
        "{summary}"
    );

    // A type error in the dependency fails the command and is reported
    // under the dependency's own section.
    project.write("lib.av", &LIB_AV.replace("n * 2", "n * \"two\""));
    let run = project.run(&["check", "entry.av"]);
    assert!(!run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert!(
        stdout.contains("Checked 2 module(s): 1 passed, 1 failed"),
        "{stdout}"
    );
    assert!(stdout.contains("./lib.av"), "{stdout}");
    // The dependency reports its own error; the entry's section does not
    // repeat it, so the program shows the error exactly once.
    assert_eq!(stdout.matches("error[type-error]").count(), 1, "{stdout}");
}

#[test]
fn embedded_standard_modules_are_typed_but_not_sampled_or_listed() {
    let project = Project::new("stdlib-units");
    project.write("app.av", BYTES_APP_AV);

    let verify = project.run(&["verify", "app.av"]);
    assert!(verify.status.success(), "{}", report(&verify));
    let (stdout, _) = text(&verify);
    assert!(stdout.contains("1/1 cases passed"), "{stdout}");
    assert!(stdout.contains("Summary: 1 module | 1 block"), "{stdout}");
    assert!(!stdout.contains("bytes.av"), "{stdout}");
    assert!(!stdout.contains("<aver-stdlib>"), "{stdout}");

    let check = project.run(&["check", "app.av"]);
    assert!(check.status.success(), "{}", report(&check));
    let (stdout, _) = text(&check);
    assert_eq!(stdout.matches("Check: ").count(), 1, "{stdout}");
    assert!(!stdout.contains("bytes.av"), "{stdout}");
    assert!(!stdout.contains("Checked "), "{stdout}");
}

#[test]
fn audit_entry_audits_the_dependency_module_too() {
    let project = Project::new("audit-program");
    project.write("lib.av", LIB_AV).write("entry.av", ENTRY_AV);

    let run = project.run(&["audit", "entry.av"]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    // Leaves first: the dependency's cases and law run before the entry's.
    assert!(
        position(&stdout, "Audit: ./lib.av") < position(&stdout, "Audit: entry.av"),
        "{stdout}"
    );
    assert!(stdout.contains("verify double  3/3"), "{stdout}");
    assert!(stdout.contains("verify double  9/9"), "{stdout}");
    assert!(
        stdout.contains("Audit: 2 modules | 0 check errors | 0 verify failures | 0 format"),
        "{stdout}"
    );

    let json = project.run(&["audit", "entry.av", "--json"]);
    assert!(json.status.success(), "{}", report(&json));
    let (stdout, _) = text(&json);
    assert!(stdout.contains("\"file_label\":\"./lib.av\""), "{stdout}");
    let summary = stdout.lines().last().unwrap_or_default();
    assert!(
        summary.contains(
            "\"kind\":\"summary\",\"files\":1,\"modules\":2,\"audit\":{\"check_errors\":0,\"verify_failures\":0,\"format_needed\":0}"
        ),
        "{summary}"
    );

    // A failing case in the dependency fails the audit, under the
    // dependency's own section.
    project.write(
        "lib.av",
        &LIB_AV.replace("double(3) => 6", "double(3) => 7"),
    );
    let run = project.run(&["audit", "entry.av"]);
    assert!(!run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert!(stdout.contains("Audit: ./lib.av"), "{stdout}");
    assert!(
        stdout.contains("verify double  2/3 passed, 1 failed"),
        "{stdout}"
    );
    assert!(
        stdout.contains("Audit: 2 modules | 0 check errors | 1 verify failures | 0 format"),
        "{stdout}"
    );

    // A type error in the dependency counts once, under the dependency;
    // the entry's section does not repeat it.
    project.write("lib.av", &LIB_AV.replace("n * 2", "n * \"two\""));
    let run = project.run(&["audit", "entry.av"]);
    assert!(!run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert_eq!(stdout.matches("error[type-error]").count(), 1, "{stdout}");
    assert!(
        position(&stdout, "error[type-error]") < position(&stdout, "Audit: entry.av"),
        "{stdout}"
    );
    assert!(
        stdout.contains("Audit: 2 modules | 1 check errors | 0 verify failures | 0 format"),
        "{stdout}"
    );
}

#[test]
fn directory_audit_reports_each_module_once() {
    let project = Project::new("audit-dir");
    project.write("lib.av", LIB_AV).write("entry.av", ENTRY_AV);

    let run = project.run(&["audit", "."]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert_eq!(stdout.matches("Audit: ./lib.av").count(), 1, "{stdout}");
    assert_eq!(stdout.matches("Audit: ./entry.av").count(), 1, "{stdout}");
    assert!(
        position(&stdout, "Audit: ./lib.av") < position(&stdout, "Audit: ./entry.av"),
        "{stdout}"
    );
    assert!(
        stdout.contains("Audit: 2 modules | 0 check errors | 0 verify failures | 0 format"),
        "{stdout}"
    );
    // lib.av was audited under entry.av's program, so it gets no section
    // of its own: the one `Input:` header opens a non-empty section.
    let after_headers = lines_after(&stdout, "Input: ");
    assert_eq!(after_headers.len(), 1, "{stdout}");
    assert!(
        after_headers.iter().all(|line| line.starts_with("Audit: ")),
        "{stdout}"
    );
}

#[test]
fn audit_reaches_a_dependency_outside_the_directory() {
    let project = Project::new("audit-outside");
    fs::create_dir_all(project.dir.join("app")).expect("create app dir");
    project
        .write("lib.av", LIB_AV)
        .write("app/entry.av", ENTRY_AV);

    let run = project.run(&["audit", "app"]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert!(
        position(&stdout, "Audit: ./lib.av") < position(&stdout, "Audit: app/entry.av"),
        "{stdout}"
    );
    assert!(stdout.contains("verify double  9/9"), "{stdout}");
    assert!(stdout.contains("Audit: 2 modules"), "{stdout}");
}

#[test]
fn single_input_unused_exposes_name_the_checked_program_as_their_scope() {
    let project = Project::new("check-scope");
    project
        .write("lib.av", SHARED_LIB_AV)
        .write("entry.av", ENTRY_AV)
        .write("other.av", OTHER_AV);

    // Only entry.av's program is judged: `triple` is used by other.av,
    // which is outside the input, and the diagnostic says so.
    let run = project.run(&["check", "entry.av"]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, _) = text(&run);
    assert!(
        stdout.contains(
            "warning[unused-expose]: exposes not used by the checked program(s): triple, spare"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("at: ./lib.av:3:5"), "{stdout}");

    let json = project.run(&["check", "entry.av", "--json"]);
    assert!(json.status.success(), "{}", report(&json));
    let (stdout, _) = text(&json);
    assert!(
        stdout.contains(
            "\"slug\":\"unused-expose\",\"summary\":\"exposes not used by the checked program(s): triple, spare\",\"span\":{\"file\":\"./lib.av\",\"line\":3,\"col\":5}"
        ),
        "{stdout}"
    );
}

#[test]
fn run_no_longer_warns_about_dependency_verify_blocks() {
    let project = Project::new("run-silent");
    project.write("lib.av", LIB_AV).write(
        "main.av",
        "module Main\n    intent = \"Print a quadrupled number.\"\n    depends [Lib]\n    effects [Console.print]\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(String.fromInt(Lib.double(Lib.double(2))))\n",
    );

    let run = project.run(&["run", "main.av"]);
    assert!(run.status.success(), "{}", report(&run));
    let (stdout, stderr) = text(&run);
    assert_eq!(stdout.trim(), "8", "{stdout}");
    assert!(
        !stderr.contains("NOT checked") && !stderr.contains("not sampled"),
        "{stderr}"
    );
}

#[test]
fn the_removed_flags_are_rejected() {
    let project = Project::new("flags");
    project.write("lib.av", LIB_AV).write("entry.av", ENTRY_AV);
    for args in [
        ["check", "entry.av", "--deps"],
        ["verify", "entry.av", "--deps"],
        ["verify", "entry.av", "--providers"],
        ["run", "entry.av", "--providers"],
    ] {
        let run = project.run(&args);
        assert!(!run.status.success(), "accepted {args:?}: {}", report(&run));
        let (_, stderr) = text(&run);
        assert!(stderr.contains("unexpected argument"), "{stderr}");
    }
    assert!(Path::new(env!("CARGO_BIN_EXE_aver")).is_file());
}
