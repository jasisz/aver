//! A bare type name is answered only by the modules the asking one can see.
//!
//! Declaring a type in a module nobody imports used to make its bare name
//! ambiguous for every module in the program. The lookup answered `None`,
//! and downstream `None` means "not a user type", so the Rust backend wrote
//! `compile_error!` into the generated crate while `check`, `verify` and
//! `compile` all reported success.
//!
//! The build-and-run half of this lives in `rust_codegen_differential`,
//! where a real `cargo build` settles it. What is here is the CLI contract:
//! a clean compile, and the two ways this can legitimately fail.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn temp_dir(tag: &str) -> PathBuf {
    let dir =
        std::env::temp_dir().join(format!("aver-bare-type-scope-{tag}-{}", std::process::id()));
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

#[test]
fn compiling_over_an_unimported_colliding_type_emits_no_compile_error() {
    let root = repo_root().join("tests/fixtures/bare_type_scope");
    let out_dir = temp_dir("clean");
    let project = out_dir.join("project");

    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .args(["compile", "tests/fixtures/bare_type_scope/app/entry.av"])
        .args(["--target", "rust"])
        .arg("--module-root")
        .arg(&root)
        .arg("-o")
        .arg(&project)
        .output()
        .expect("run aver compile --target rust");

    assert_eq!(
        out.status.code(),
        Some(0),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
    let carrying: Vec<String> = rust_sources(&project)
        .into_iter()
        .filter(|(_, content)| content.contains("compile_error!"))
        .map(|(path, _)| path)
        .collect();
    assert!(
        carrying.is_empty(),
        "a type in a module nobody imports must not turn other modules into compile errors, found in:\n  {}",
        carrying.join("\n  ")
    );

    fs::remove_dir_all(&out_dir).ok();
}

#[test]
fn a_collision_the_asking_module_can_actually_see_is_named() {
    // Both declarations are in scope here — `User` depends on both — so the
    // bare name really is ambiguous and the compiler must say which two
    // types it is between, rather than going quiet and writing a compile
    // error into the generated crate.
    let dir = temp_dir("collision");
    write(
        &dir.join("domain/state.av"),
        "module State\n    intent =\n        \"An ADT named Step.\"\n    depends []\n    effects []\n\ntype Step\n    Continue(Int)\n    Stop(String)\n",
    );
    write(
        &dir.join("domain/tally.av"),
        "module Tally\n    intent =\n        \"A record that shares the bare name.\"\n    depends []\n    effects []\n\nrecord Step\n    tally: Int\n",
    );
    write(
        &dir.join("domain/user.av"),
        "module User\n    intent =\n        \"Depends on both and spells the name bare.\"\n    depends [Domain.State, Domain.Tally]\n    effects []\n\nfn added(n: Int) -> Step\n    ? \"Ambiguous on purpose.\"\n    Step.Continue(n)\n",
    );

    let out = Command::new(aver_bin())
        .current_dir(&dir)
        .args(["compile", "domain/user.av", "--target", "rust"])
        .args(["--module-root", ".", "-o", "out"])
        .output()
        .expect("run aver compile --target rust");

    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        out.status.code(),
        Some(1),
        "stdout: {}\nstderr: {stderr}",
        String::from_utf8_lossy(&out.stdout)
    );
    assert!(
        stderr.contains("Ambiguous type name 'Step'")
            && stderr.contains("Domain.State.Step")
            && stderr.contains("Domain.Tally.Step"),
        "the ambiguity must name both candidates, got stderr:\n{stderr}"
    );
    assert!(
        !dir.join("out").exists(),
        "nothing should be emitted for a program the checker rejected"
    );

    fs::remove_dir_all(&dir).ok();
}

#[test]
fn the_vm_and_the_checker_agree_with_the_generated_code() {
    // The same program, run three ways. Before the fix `check` and `verify`
    // were green and only the generated crate was broken, so keeping all
    // three in one test is the point.
    let root = repo_root().join("tests/fixtures/bare_type_scope");
    for (command, expected) in [("check", None), ("verify", None), ("run", Some("7 1"))] {
        let out = Command::new(aver_bin())
            .current_dir(repo_root())
            .args([command, "tests/fixtures/bare_type_scope/app/entry.av"])
            .arg("--module-root")
            .arg(&root)
            .output()
            .unwrap_or_else(|e| panic!("run aver {command}: {e}"));
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert_eq!(
            out.status.code(),
            Some(0),
            "aver {command} must pass\nstdout: {stdout}\nstderr: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        if let Some(expected) = expected {
            assert_eq!(stdout.trim(), expected);
        }
    }
}
