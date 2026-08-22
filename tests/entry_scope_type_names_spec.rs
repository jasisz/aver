//! What a bare type name means inside a dependency does not depend on which
//! file the command was pointed at, and a constructor that resolves to
//! nothing is said out loud.
//!
//! The entry is another module from a dependency's point of view: the entry
//! names its dependencies in `depends [...]` and they never name it. While
//! the resolver probed the entry's scope for everyone, a record declared in
//! the file you happened to run took over — or took away — a bare name
//! written inside a module that had never heard of it. Reported as
//! jasisz/aver#1076: `verify` refused a function in another file with an
//! `internal error`, and `check` passed.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn temp_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("aver-entry-scope-{tag}-{}", std::process::id()));
    if dir.exists() {
        fs::remove_dir_all(&dir).ok();
    }
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn write(path: &Path, content: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create fixture dir");
    }
    fs::write(path, content).unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
}

fn rust_sources(dir: &Path) -> Vec<(String, String)> {
    let mut out = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(current) = stack.pop() {
        let Ok(entries) = fs::read_dir(&current) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().is_some_and(|ext| ext == "rs")
                && let Ok(content) = fs::read_to_string(&path)
            {
                out.push((path.display().to_string(), content));
            }
        }
    }
    out
}

/// The reporter's shape, reduced: a dependency writes a bare sum
/// constructor of a type it imports, and the entry declares an unrelated
/// type of the same name.
fn write_reporter_shape(dir: &Path) {
    write(
        &dir.join("domain/scriptstate.av"),
        "module ScriptState\n    intent =\n        \"Declares the sum type the lock-time module builds values of.\"\n    depends []\n    effects []\n\ntype Step\n    Continue(Int)\n    Stop(String)\n",
    );
    write(
        &dir.join("domain/locktime.av"),
        "module LockTime\n    intent =\n        \"Spells `Step` bare for the type it imports, and never names the\"\n        \"entry that pulls it in.\"\n    depends [Domain.ScriptState]\n    effects []\n\nfn checked(n: Int) -> Step\n    ? \"Builds a Step in a module that does not declare it.\"\n    match n > 0\n        true -> Step.Continue(n)\n        false -> Step.Stop(\"none\")\n\nverify checked\n    checked(1) => Step.Continue(1)\n    checked(0) => Step.Stop(\"none\")\n\nfn size(n: Int) -> Int\n    ? \"Reads the payload back out of the Step this module built.\"\n    match checked(n)\n        Step.Continue(v) -> v\n        _ -> 0\n\nverify size\n    size(7) => 7\n    size(0) => 0\n",
    );
    write(
        &dir.join("app/sweep.av"),
        "module Sweep\n    intent =\n        \"Declares a record that happens to share the bare name `Step` with\"\n        \"a type its dependency imports from somewhere else.\"\n    depends [Domain.LockTime]\n    effects [Console.print]\n\nrecord Step\n    tally: Int\n\nfn tallyOf(step: Step) -> Int\n    ? \"Reads the tally out of this module's own Step.\"\n    step.tally\n\nverify tallyOf\n    tallyOf(Step(tally = 1)) => 1\n\nfn measured(n: Int) -> Int\n    ? \"Reads what the dependency computed.\"\n    Domain.LockTime.size(n)\n\nverify measured\n    measured(7) => 7\n    measured(0) => 0\n\nfn main() -> Unit\n    ? \"Prints both numbers.\"\n    ! [Console.print]\n    Console.print(\"{measured(7)} {tallyOf(Step(tally = 1))}\")\n",
    );
}

#[test]
fn a_record_in_the_entry_does_not_break_a_dependency_that_never_names_it() {
    // Before this, `aver verify` answered:
    //   VM compile error: Compile error: internal error: dep fn `checked`
    //   did not lower to MIR (an unsupported shape reached the VM backend)
    // and skipped the whole file, while `aver check` on the same program
    // reported nothing at all.
    let dir = temp_dir("reporter-shape");
    write_reporter_shape(&dir);

    for command in ["check", "verify", "run"] {
        let out = Command::new(aver_bin())
            .current_dir(&dir)
            .args([command, "app/sweep.av", "--module-root", "."])
            .output()
            .unwrap_or_else(|e| panic!("run aver {command}: {e}"));
        let stdout = String::from_utf8_lossy(&out.stdout);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(
            out.status.code(),
            Some(0),
            "aver {command} must pass\nstdout: {stdout}\nstderr: {stderr}"
        );
        assert!(
            !stdout.contains("did not lower to MIR") && !stderr.contains("did not lower to MIR"),
            "aver {command} refused a function in another file:\n{stdout}{stderr}"
        );
    }

    let compile = Command::new(aver_bin())
        .current_dir(&dir)
        .args(["compile", "app/sweep.av", "--target", "rust"])
        .args(["--module-root", ".", "-o", "out"])
        .output()
        .expect("run aver compile --target rust");
    assert_eq!(
        compile.status.code(),
        Some(0),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr),
    );
    let carrying: Vec<String> = rust_sources(&dir.join("out"))
        .into_iter()
        .filter(|(_, content)| content.contains("compile_error!"))
        .map(|(path, _)| path)
        .collect();
    assert!(
        carrying.is_empty(),
        "the emitted crate must carry no compile error, found in:\n  {}",
        carrying.join("\n  ")
    );

    let proof = Command::new(aver_bin())
        .current_dir(&dir)
        .args(["proof", "app/sweep.av", "--backend", "lean"])
        .args(["--module-root", ".", "-o", "lean"])
        .output()
        .expect("run aver proof --backend lean");
    assert_eq!(
        proof.status.code(),
        Some(0),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&proof.stdout),
        String::from_utf8_lossy(&proof.stderr),
    );

    fs::remove_dir_all(&dir).ok();
}

#[test]
fn which_entry_you_point_at_does_not_change_what_a_dependency_means() {
    // The sharper half. When the entry's own `Step` also declares a
    // `Continue`, the dependency's constructor RESOLVED — to the wrong
    // declaration — and nothing refused anything. The emitted code for one
    // unchanged module came out differently depending on which file the
    // command was pointed at.
    let dir = temp_dir("two-entries");
    write(
        &dir.join("domain/scriptstate.av"),
        "module ScriptState\n    intent =\n        \"Declares the sum type the lock-time module builds values of.\"\n    depends []\n    effects []\n\ntype Step\n    Continue(Int)\n    Stop(String)\n",
    );
    write(
        &dir.join("domain/locktime.av"),
        "module LockTime\n    intent =\n        \"Spells `Step` bare for the type it imports.\"\n    depends [Domain.ScriptState]\n    effects []\n\nfn checked(n: Int) -> Step\n    ? \"Builds a Step in a module that does not declare it.\"\n    match n > 0\n        true -> Step.Continue(n)\n        false -> Step.Stop(\"none\")\n\nverify checked\n    checked(1) => Step.Continue(1)\n    checked(0) => Step.Stop(\"none\")\n\nfn size(n: Int) -> Int\n    ? \"Reads the payload back out.\"\n    match checked(n)\n        Step.Continue(v) -> v\n        _ -> 0\n\nverify size\n    size(7) => 7\n    size(0) => 0\n",
    );
    write(
        &dir.join("app/named.av"),
        "module Named\n    intent =\n        \"An entry declaring a `Step` of its own, whose variant names are the\"\n        \"same as the ones its dependency writes.\"\n    depends [Domain.LockTime]\n    effects [Console.print]\n\ntype Step\n    Continue(String)\n    Stop(String)\n\nfn label(s: Step) -> String\n    ? \"Reads this module's own Step.\"\n    match s\n        Step.Continue(text) -> text\n        Step.Stop(text) -> text\n\nverify label\n    label(Step.Continue(\"go\")) => \"go\"\n\nfn shown() -> String\n    ? \"This module's own Step, read back.\"\n    label(Step.Continue(\"go\"))\n\nverify shown\n    shown() => \"go\"\n\nfn main() -> Unit\n    ? \"Prints both.\"\n    ! [Console.print]\n    Console.print(\"{Domain.LockTime.size(7)} {shown()}\")\n",
    );
    write(
        &dir.join("app/plain.av"),
        "module Plain\n    intent =\n        \"The same program entered from a file that declares no `Step`.\"\n    depends [Domain.LockTime]\n    effects [Console.print]\n\nfn main() -> Unit\n    ? \"Prints what the dependency computed.\"\n    ! [Console.print]\n    Console.print(\"{Domain.LockTime.size(7)}\")\n",
    );

    let shared = "src/aver_generated/domain/lock_time/mod.rs";
    let mut emitted = Vec::new();
    for (entry, out) in [("app/named.av", "out-named"), ("app/plain.av", "out-plain")] {
        let compile = Command::new(aver_bin())
            .current_dir(&dir)
            .args(["compile", entry, "--target", "rust"])
            .args(["--module-root", ".", "-o", out])
            .output()
            .unwrap_or_else(|e| panic!("run aver compile on {entry}: {e}"));
        assert_eq!(
            compile.status.code(),
            Some(0),
            "compiling {entry}\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&compile.stdout),
            String::from_utf8_lossy(&compile.stderr),
        );
        emitted.push(
            fs::read_to_string(dir.join(out).join(shared)).unwrap_or_else(|e| {
                panic!("read the shared module emitted for {entry}: {e}");
            }),
        );
    }
    assert_eq!(
        emitted[0], emitted[1],
        "the same dependency, compiled from two entries, must emit the same code"
    );
    assert!(
        emitted[0].contains("domain::script_state::Step"),
        "the dependency's `Step` is the one it imports:\n{}",
        emitted[0]
    );

    let run = Command::new(aver_bin())
        .current_dir(&dir)
        .args(["run", "app/named.av", "--module-root", "."])
        .output()
        .expect("run aver run");
    assert_eq!(
        run.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&run.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout).trim(), "7 go");

    fs::remove_dir_all(&dir).ok();
}

#[test]
fn a_constructor_that_names_no_variant_is_reported_where_it_is_written() {
    // The other half of #1076: a constructor the compiler really cannot
    // resolve must be named, at its own line, by the command that reads the
    // source — not turned into an `internal error` about a function in
    // another file once a backend notices the function went missing.
    let dir = temp_dir("unknown-variant");
    write(
        &dir.join("main.av"),
        "module Main\n    intent =\n        \"Writes a variant its own type does not declare.\"\n    depends []\n    effects []\n\ntype Step\n    Continue(Int)\n    Stop(String)\n\nfn made(n: Int) -> Step\n    ? \"Names a variant this type does not have.\"\n    Step.Kontinue(n)\n\nverify made\n    made(1) => Step.Continue(1)\n",
    );

    // `Step.Kontinue(n)` is on line 13; each command spells the location in
    // its own shape.
    for (command, at) in [("check", "main.av:13"), ("verify", "error[13:")] {
        let out = Command::new(aver_bin())
            .current_dir(&dir)
            .args([command, "main.av"])
            .output()
            .unwrap_or_else(|e| panic!("run aver {command}: {e}"));
        let both = format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        assert_ne!(
            out.status.code(),
            Some(0),
            "aver {command} must refuse this program:\n{both}"
        );
        assert!(
            both.contains("Unknown constructor 'Step.Kontinue'"),
            "aver {command} must name the constructor:\n{both}"
        );
        assert!(
            both.contains("Continue, Stop"),
            "aver {command} must say what the type does declare:\n{both}"
        );
        assert!(
            both.contains(at),
            "aver {command} must point at the line that writes it ({at}):\n{both}"
        );
        assert!(
            !both.contains("internal error"),
            "aver {command} must not report this as an internal error:\n{both}"
        );
    }

    fs::remove_dir_all(&dir).ok();
}
