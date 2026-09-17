//! `aver effects` — the report, the rewriter and the reviewer's view.
//!
//! The command exists because effect propagation, not effect declaration, is
//! what costs a consumer lines: swapping one primitive at a leaf rewrites the
//! declared list of every function on every call chain above it while those
//! bodies stay byte-identical. The checker already computes the minimal set,
//! so each test here pins one half of handing that computation to the user:
//! the report names the drift, `--write` closes it, and `--since` tells a
//! reviewer which of the rewritten lists carry no decision of their own.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

fn fixture(name: &str) -> PathBuf {
    repo_root().join("tests/fixtures").join(name)
}

fn temp_dir(label: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver-effects-{label}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn copy_tree(from: &Path, to: &Path) {
    std::fs::create_dir_all(to).expect("create destination");
    for entry in std::fs::read_dir(from).expect("read fixture dir") {
        let entry = entry.expect("fixture entry");
        let target = to.join(entry.file_name());
        if entry.file_type().expect("entry file type").is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            std::fs::copy(entry.path(), &target).expect("copy fixture file");
        }
    }
}

/// The fixture tree copied somewhere writable, so `--write` has a tree of its
/// own to rewrite and the committed fixture stays the reference.
fn scratch_copy(fixture_name: &str, label: &str) -> PathBuf {
    let dir = temp_dir(label);
    copy_tree(&fixture(fixture_name), &dir);
    dir
}

fn run_effects(root: &Path, args: &[&str]) -> Output {
    run_effects_on(root, root, args)
}

/// The command pointed at one file while the module root stays the tree, which
/// is how a walk reaches a module the input did not name.
fn run_effects_on(input: &Path, root: &Path, args: &[&str]) -> Output {
    let mut command = Command::new(aver_bin());
    command
        .arg("effects")
        .arg(input)
        .arg("--module-root")
        .arg(root);
    for arg in args {
        command.arg(arg);
    }
    command.output().expect("run aver effects")
}

fn run_check(root: &Path) -> Output {
    Command::new(aver_bin())
        .arg("check")
        .arg(root)
        .arg("--module-root")
        .arg(root)
        .output()
        .expect("run aver check")
}

fn run_format_check(root: &Path) -> Output {
    Command::new(aver_bin())
        .arg("format")
        .arg(root)
        .arg("--check")
        .output()
        .expect("run aver format --check")
}

fn stdout_of(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).to_string()
}

fn read(root: &Path, relative: &str) -> String {
    std::fs::read_to_string(root.join(relative)).expect("read source file")
}

fn write(root: &Path, relative: &str, contents: &str) {
    std::fs::write(root.join(relative), contents).expect("write source file");
}

fn git(root: &Path, args: &[&str]) -> Output {
    let mut command = Command::new("git");
    command.arg("-C").arg(root);
    for arg in args {
        command.arg(arg);
    }
    command
        .env("GIT_AUTHOR_NAME", "aver test")
        .env("GIT_AUTHOR_EMAIL", "test@example.invalid")
        .env("GIT_COMMITTER_NAME", "aver test")
        .env("GIT_COMMITTER_EMAIL", "test@example.invalid")
        .output()
        .expect("run git")
}

/// The primitive swap the migration measured: the leaf stops calling
/// `Disk.appendText` and starts calling `Disk.appendBytes` plus `Disk.sync`.
/// Only this one body changes; every list above it is then wrong.
fn swap_the_leaf_primitive(root: &Path) {
    let source = read(root, "infra/store.av");
    let swapped = source.replace(
        "    ! [Disk.appendText]\n    Disk.appendText(file, line)\n",
        "    ! [Disk.appendText]\n    _written = Disk.appendText(file, line)?\n    Disk.sync(file)\n",
    );
    assert_ne!(source, swapped, "the leaf body fixture text moved");
    write(root, "infra/store.av", &swapped);
}

#[test]
fn report_names_every_function_whose_declared_list_is_not_the_minimum() {
    let root = fixture("effects_drift");
    let output = run_effects(&root, &[]);
    assert!(output.status.success(), "{}", format_output(&output));
    let stdout = stdout_of(&output);

    // Both halves of the mutually recursive pair over-declare, and so does the
    // module boundary that is their union.
    assert!(stdout.contains("fn walk"), "{stdout}");
    assert!(stdout.contains("fn stepped"), "{stdout}");
    assert!(
        stdout.contains("unused: Console.print, Time.unixMs"),
        "{stdout}"
    );
    assert!(stdout.contains("effects [...]"), "{stdout}");
}

#[test]
fn report_summarises_modules_with_nothing_to_say() {
    let root = fixture("effects_command");
    let output = run_effects(&root, &[]);
    assert!(output.status.success(), "{}", format_output(&output));
    let stdout = stdout_of(&output);

    assert!(
        stdout.contains("every declared list is the computed minimum"),
        "{stdout}"
    );
    // A module with nothing to say gets its summary line and no per-function
    // detail under it.
    assert!(!stdout.contains("fn writeThrough"), "{stdout}");
}

#[test]
fn report_names_what_a_primitive_swap_left_missing() {
    let root = scratch_copy("effects_command", "report-missing");
    swap_the_leaf_primitive(&root);

    let output = run_effects(&root, &[]);
    assert!(output.status.success(), "{}", format_output(&output));
    let stdout = stdout_of(&output);

    // The swap is at the leaf; every list above it is missing the same entry.
    assert!(stdout.contains("fn writeThrough"), "{stdout}");
    assert!(stdout.contains("fn save"), "{stdout}");
    assert!(stdout.contains("fn run"), "{stdout}");
    assert!(stdout.contains("fn main"), "{stdout}");
    assert!(stdout.contains("missing: Disk.sync"), "{stdout}");

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn write_propagates_a_primitive_swap_up_every_call_chain() {
    let root = scratch_copy("effects_command", "write-swap");
    swap_the_leaf_primitive(&root);

    let broken = run_check(&root);
    assert!(
        !broken.status.success(),
        "the swap should break the program first: {}",
        format_output(&broken)
    );

    let written = run_effects(&root, &["--write"]);
    assert!(written.status.success(), "{}", format_output(&written));

    let fixed = run_check(&root);
    assert!(fixed.status.success(), "{}", format_output(&fixed));

    // The swap reached the entry, four call links away from the primitive.
    assert!(read(&root, "main.av").contains("Disk.sync"), "entry list");
    assert!(read(&root, "app/cli.av").contains("Disk.sync"), "cli list");
    // The module boundary is the union, so it moved too.
    assert!(
        read(&root, "app/cli.av").contains("effects [Console.print, Disk.appendText, Disk.sync]"),
        "{}",
        read(&root, "app/cli.av")
    );

    // The rewritten lists go through the formatter's own layout.
    let formatted = run_format_check(&root);
    assert!(formatted.status.success(), "{}", format_output(&formatted));

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn write_removes_unused_effects_and_converges_on_a_recursion_group() {
    let root = scratch_copy("effects_drift", "write-drift");

    let written = run_effects(&root, &["--write"]);
    assert!(written.status.success(), "{}", format_output(&written));

    let ledger = read(&root, "infra/ledger.av");
    // Neither half of the recursive pair reaches these, but each kept the
    // other's copy alive until the minimum was computed from below.
    assert!(!ledger.contains("Console.print"), "{ledger}");
    assert!(!ledger.contains("Time.unixMs"), "{ledger}");
    assert_eq!(ledger.matches("! [Disk.appendText]").count(), 2, "{ledger}");
    assert!(ledger.contains("effects [Disk.appendText]"), "{ledger}");

    let main = read(&root, "main.av");
    assert!(main.contains("! [Disk.appendText]"), "{main}");
    assert!(main.contains("effects [Disk.appendText]"), "{main}");

    let checked = run_check(&root);
    assert!(checked.status.success(), "{}", format_output(&checked));
    let formatted = run_format_check(&root);
    assert!(formatted.status.success(), "{}", format_output(&formatted));

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn write_is_idempotent_and_leaves_an_already_minimal_tree_byte_identical() {
    let root = scratch_copy("effects_command", "write-idempotent");
    let before: Vec<String> = ["main.av", "app/cli.av", "infra/store.av"]
        .iter()
        .map(|p| read(&root, p))
        .collect();

    let first = run_effects(&root, &["--write"]);
    assert!(first.status.success(), "{}", format_output(&first));
    let after_first: Vec<String> = ["main.av", "app/cli.av", "infra/store.av"]
        .iter()
        .map(|p| read(&root, p))
        .collect();
    assert_eq!(before, after_first, "a minimal tree must not be rewritten");

    // And a tree that did need rewriting settles after one pass.
    let drift = scratch_copy("effects_drift", "write-idempotent-drift");
    assert!(
        run_effects(&drift, &["--write"]).status.success(),
        "first write"
    );
    let once: Vec<String> = ["main.av", "infra/ledger.av"]
        .iter()
        .map(|p| read(&drift, p))
        .collect();
    assert!(
        run_effects(&drift, &["--write"]).status.success(),
        "second write"
    );
    let twice: Vec<String> = ["main.av", "infra/ledger.av"]
        .iter()
        .map(|p| read(&drift, p))
        .collect();
    assert_eq!(once, twice, "--write is idempotent");

    std::fs::remove_dir_all(&root).ok();
    std::fs::remove_dir_all(&drift).ok();
}

#[test]
fn json_envelope_is_versioned_and_byte_stable() {
    let root = fixture("effects_drift");
    let first = run_effects(&root, &["--json"]);
    assert!(first.status.success(), "{}", format_output(&first));
    let second = run_effects(&root, &["--json"]);
    assert_eq!(
        stdout_of(&first),
        stdout_of(&second),
        "the JSON envelope is deterministic"
    );

    let value: serde_json::Value =
        serde_json::from_str(&stdout_of(&first)).expect("effects --json is JSON");
    assert_eq!(value["schemaVersion"], 1);
    assert_eq!(value["kind"], "effectSurface");
    let modules = value["modules"].as_array().expect("modules array");
    assert_eq!(modules.len(), 2);
    // Deterministic order: by module name.
    assert_eq!(modules[0]["module"], "Ledger");
    assert_eq!(modules[1]["module"], "Main");
    let walk = &modules[0]["functions"][0];
    assert_eq!(walk["function"], "walk");
    assert_eq!(
        walk["unused"],
        serde_json::json!(["Console.print", "Time.unixMs"])
    );
    assert_eq!(walk["minimum"], serde_json::json!(["Disk.appendText"]));
}

#[test]
fn since_separates_propagation_only_from_a_changed_body() {
    let root = scratch_copy("effects_command", "since");
    assert!(git(&root, &["init", "-q"]).status.success(), "git init");
    assert!(git(&root, &["add", "-A"]).status.success(), "git add");
    let committed = git(&root, &["commit", "-q", "-m", "baseline"]);
    assert!(committed.status.success(), "{}", format_output(&committed));

    swap_the_leaf_primitive(&root);
    assert!(
        run_effects(&root, &["--write"]).status.success(),
        "write after the swap"
    );

    let output = run_effects(&root, &["--since", "HEAD"]);
    assert!(output.status.success(), "{}", format_output(&output));
    let stdout = stdout_of(&output);

    // One body changed; the three lists above it changed for no reason of
    // their own, which is the line a reviewer needs.
    assert!(stdout.contains("body changed: writeThrough"), "{stdout}");
    assert!(stdout.contains("propagation only: save"), "{stdout}");
    assert!(stdout.contains("propagation only: run"), "{stdout}");
    assert!(stdout.contains("propagation only: main"), "{stdout}");

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn since_says_so_when_the_revision_is_not_there() {
    let root = scratch_copy("effects_command", "since-missing");
    assert!(git(&root, &["init", "-q"]).status.success(), "git init");
    assert!(git(&root, &["add", "-A"]).status.success(), "git add");
    assert!(
        git(&root, &["commit", "-q", "-m", "baseline"])
            .status
            .success(),
        "git commit"
    );

    let output = run_effects(&root, &["--since", "no-such-revision"]);
    assert!(!output.status.success(), "{}", format_output(&output));
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    assert!(stderr.contains("no-such-revision"), "{stderr}");

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn write_and_since_are_not_the_same_run() {
    let root = fixture("effects_command");
    let output = run_effects(&root, &["--write", "--since", "HEAD"]);
    assert!(!output.status.success(), "{}", format_output(&output));
}

/// A `//` comment after the leaf's own effect list. `check` and
/// `format --check` both accept it, and the formatter leaves a line carrying
/// one alone, so a rewriter has to read past it rather than through it.
fn comment_after_the_leaf_list(root: &Path) {
    let source = read(root, "infra/store.av");
    let commented = source.replace(
        "    ! [Disk.appendText]\n    writeThrough(file, line)\n",
        "    ! [Disk.appendText]  // the helper below reaches the disk\n    writeThrough(file, line)\n",
    );
    assert_ne!(source, commented, "the leaf list fixture text moved");
    write(root, "infra/store.av", &commented);
}

/// A `//` comment at column zero inside the leaf function, which is the shape
/// `examples/services/weather.av` already uses above its entry point.
fn comment_at_column_zero_inside_the_leaf(root: &Path) {
    let source = read(root, "infra/store.av");
    let commented = source.replace(
        "    ? \"The one call in this program that reaches the disk.\"\n",
        "    ? \"The one call in this program that reaches the disk.\"\n// the only disk touch in the tree\n",
    );
    assert_ne!(source, commented, "the leaf description fixture text moved");
    write(root, "infra/store.av", &commented);
}

#[test]
fn write_keeps_the_function_under_a_commented_effect_list() {
    let root = scratch_copy("effects_command", "write-comment-after-list");
    comment_after_the_leaf_list(&root);
    swap_the_leaf_primitive(&root);

    let written = run_effects(&root, &["--write"]);
    assert!(written.status.success(), "{}", format_output(&written));

    let store = read(&root, "infra/store.av");
    // The comment sits on `save`'s list; everything under it is a different
    // function and has to still be there.
    assert!(store.contains("fn writeThrough"), "{store}");
    assert!(store.contains("Disk.appendText(file, line)"), "{store}");
    assert!(
        store.contains("// the helper below reaches the disk"),
        "{store}"
    );

    let checked = run_check(&root);
    assert!(checked.status.success(), "{}", format_output(&checked));

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn write_finds_the_list_of_a_function_under_a_column_zero_comment() {
    let root = scratch_copy("effects_command", "write-comment-column-zero");
    comment_at_column_zero_inside_the_leaf(&root);
    swap_the_leaf_primitive(&root);

    let written = run_effects(&root, &["--write"]);
    assert!(written.status.success(), "{}", format_output(&written));

    let store = read(&root, "infra/store.av");
    // One list per function, each of them indented. A second list written at
    // column zero would be a function header's worth of nonsense.
    assert!(!store.contains("\n! ["), "{store}");
    assert_eq!(store.matches("! [").count(), 2, "{store}");

    let checked = run_check(&root);
    assert!(checked.status.success(), "{}", format_output(&checked));

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn write_inserts_a_first_list_at_the_body_indent_under_a_column_zero_comment() {
    let root = scratch_copy("effects_command", "write-insert-column-zero");
    let source = read(&root, "infra/store.av");
    let stripped = source.replace(
        "    ? \"The one call in this program that reaches the disk.\"\n    ! [Disk.appendText]\n",
        "    ? \"The one call in this program that reaches the disk.\"\n// the only disk touch in the tree\n",
    );
    assert_ne!(source, stripped, "the leaf list fixture text moved");
    write(&root, "infra/store.av", &stripped);

    let written = run_effects(&root, &["--write"]);
    assert!(written.status.success(), "{}", format_output(&written));

    // The comment is not the body's first line, so it does not decide where
    // the list goes or how far it is indented.
    let store = read(&root, "infra/store.av");
    assert!(store.contains("\n    ! [Disk.appendText]\n"), "{store}");
    assert!(!store.contains("\n! ["), "{store}");

    let checked = run_check(&root);
    assert!(checked.status.success(), "{}", format_output(&checked));

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn write_keeps_a_boundary_the_yielding_functions_need() {
    // `Mid.hop` reaches the capability only through an imported yielding
    // helper, and a yielding callee is lowered out of the signature map before
    // the surface is computed. The boundary is what its functions declare, not
    // what the lowered call chain still resolves to.
    let root = scratch_copy("effects_yield_boundary", "write-yield-boundary");

    let before = run_check(&root);
    assert!(before.status.success(), "{}", format_output(&before));

    let reported = run_effects(&root, &["--json"]);
    assert!(reported.status.success(), "{}", format_output(&reported));
    let value: serde_json::Value =
        serde_json::from_str(&stdout_of(&reported)).expect("effects --json is JSON");
    let mid = value["modules"]
        .as_array()
        .expect("modules array")
        .iter()
        .find(|module| module["module"] == "Mid")
        .expect("the Mid module");
    assert_eq!(
        mid["boundary"]["unused"],
        serde_json::json!([]),
        "{}",
        stdout_of(&reported)
    );

    let written = run_effects(&root, &["--write"]);
    assert!(written.status.success(), "{}", format_output(&written));
    assert!(
        read(&root, "main.av").contains("effects [Pool.claim, yield]"),
        "{}",
        read(&root, "main.av")
    );

    let after = run_check(&root);
    assert!(after.status.success(), "{}", format_output(&after));

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn write_leaves_a_tree_the_report_calls_minimal_byte_identical() {
    // Reordering a list and narrowing `Disk` to the one method under it are
    // both invisible to the report, so neither is `--write`'s to make.
    let root = scratch_copy("effects_spelling", "write-spelling");
    let paths = ["main.av", "infra/store.av"];
    let before: Vec<String> = paths.iter().map(|p| read(&root, p)).collect();

    let reported = run_effects(&root, &[]);
    assert!(reported.status.success(), "{}", format_output(&reported));
    let stdout = stdout_of(&reported);
    assert!(
        stdout.contains("2 modules, 2 functions, every declared list is the computed minimum"),
        "{stdout}"
    );

    let written = run_effects(&root, &["--write"]);
    assert!(written.status.success(), "{}", format_output(&written));
    let after: Vec<String> = paths.iter().map(|p| read(&root, p)).collect();
    assert_eq!(before, after, "{}", stdout_of(&written));

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn since_reads_the_body_under_a_column_zero_comment() {
    let root = scratch_copy("effects_command", "since-column-zero");
    comment_at_column_zero_inside_the_leaf(&root);
    assert!(git(&root, &["init", "-q"]).status.success(), "git init");
    assert!(git(&root, &["add", "-A"]).status.success(), "git add");
    assert!(
        git(&root, &["commit", "-q", "-m", "baseline"])
            .status
            .success(),
        "git commit"
    );

    // The one line that changes sits under the comment, and it is a body
    // change: this function reaches a primitive it did not reach before.
    let source = read(&root, "infra/store.av");
    let swapped = source.replace(
        "    ! [Disk.appendText]\n    Disk.appendText(file, line)\n",
        "    ! [Disk.appendText, Disk.sync]\n    _written = Disk.appendText(file, line)?\n    Disk.sync(file)\n",
    );
    assert_ne!(source, swapped, "the leaf body fixture text moved");
    write(&root, "infra/store.av", &swapped);

    let output = run_effects(&root, &["--since", "HEAD"]);
    assert!(output.status.success(), "{}", format_output(&output));
    let stdout = stdout_of(&output);
    assert!(stdout.contains("body changed: writeThrough"), "{stdout}");
    assert!(
        !stdout.contains("propagation only: writeThrough"),
        "{stdout}"
    );

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn since_names_a_function_that_was_added_or_removed() {
    let root = scratch_copy("effects_command", "since-renamed");
    assert!(git(&root, &["init", "-q"]).status.success(), "git init");
    assert!(git(&root, &["add", "-A"]).status.success(), "git add");
    assert!(
        git(&root, &["commit", "-q", "-m", "baseline"])
            .status
            .success(),
        "git commit"
    );

    let source = read(&root, "app/cli.av");
    write(
        &root,
        "app/cli.av",
        &source.replace("announced", "reported"),
    );

    let output = run_effects(&root, &["--since", "HEAD"]);
    assert!(output.status.success(), "{}", format_output(&output));
    let stdout = stdout_of(&output);
    // A rename carries the list with it, and the reviewer reads the text view.
    assert!(stdout.contains("added: reported"), "{stdout}");
    assert!(stdout.contains("removed: announced"), "{stdout}");

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn write_names_the_files_it_reached_as_a_dependency() {
    // The walk is one program's view and the rewrite is the tree's, so a
    // module the input did not name can be imported by a program this run
    // never read.
    let root = scratch_copy("effects_command", "write-dependency");
    swap_the_leaf_primitive(&root);

    let written = run_effects_on(&root.join("main.av"), &root, &["--write"]);
    assert!(written.status.success(), "{}", format_output(&written));
    let stdout = stdout_of(&written);
    assert!(
        stdout.contains("Reached as a dependency, so a program outside this walk changes with it:"),
        "{stdout}"
    );
    assert!(stdout.contains("infra/store.av"), "{stdout}");
    // The entry the input named is not one of them.
    let notice = stdout
        .split("Reached as a dependency")
        .nth(1)
        .expect("the dependency notice");
    assert!(!notice.contains("main.av"), "{stdout}");

    std::fs::remove_dir_all(&root).ok();
}

#[test]
fn write_refuses_while_a_name_does_not_resolve() {
    // An unresolved callee contributes no effects, so the computed minimum
    // would be too small and the rewrite would delete entries the program
    // needs. The report still runs; only the rewrite stops.
    let root = scratch_copy("effects_command", "write-unresolved");
    let source = read(&root, "infra/store.av");
    write(
        &root,
        "infra/store.av",
        &source.replace("writeThrough(file, line)", "writeThroguh(file, line)"),
    );

    let reported = run_effects(&root, &[]);
    assert!(reported.status.success(), "{}", format_output(&reported));

    let written = run_effects(&root, &["--write"]);
    assert!(!written.status.success(), "{}", format_output(&written));
    let stderr = String::from_utf8_lossy(&written.stderr).to_string();
    assert!(stderr.contains("writeThroguh"), "{stderr}");
    // And the source it refused to rewrite is untouched.
    assert!(
        read(&root, "main.av").contains("! [Console.print, Disk.appendText]"),
        "the entry list must not have moved"
    );

    std::fs::remove_dir_all(&root).ok();
}
