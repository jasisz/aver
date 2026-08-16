//! The shadowing ban at the toolchain's front doors (issue #954).
//!
//! A binder — fn parameter, statement binding, or match-pattern
//! binding — may not spell a name already visible at that point: an
//! enclosing local, a top-level fn or operation of the same module, or
//! the enclosing function's own name. The resolver-side walk and the
//! full (binder position × shadowed kind) matrix live in
//! `src/resolver.rs` unit tests; this file pins that the refusal
//! reaches the user through the real commands — `aver run` and
//! `aver compile` — and that the program from issue #951, whose three
//! executors disagreed about a call through a binder spelling a module
//! fn's name, is now rejected before any executor sees it.
//!
//! Sibling match arms reusing a name stay LEGAL (they are never in
//! each other's scope; the per-arm slot machinery from #949 keeps them
//! sound) — the control below must keep running.

#![cfg(feature = "runtime")]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

static UNIQUE: AtomicU64 = AtomicU64::new(0);

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let n = UNIQUE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("aver-shadow-ban-{prefix}-{nanos}-{n}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    fs::write(&path, source).expect("write temp module source");
    path
}

fn cleanup(path: &Path) {
    let _ = fs::remove_dir_all(path.parent().expect("temp module has parent"));
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

/// The issue-#951 program, verbatim shape: the VM failed at runtime
/// while compiled Rust and the self-host answered 6. Now no executor
/// gets to have an opinion.
const DIVERGENCE_951_SRC: &str = r#"module Tmp

fn dbl(n: Int) -> Int
    n * 2

fn probe(x: Int) -> Int
    match Option.Some(x)
        Option.Some(dbl) -> dbl(3)
        Option.None -> 0

fn main()
    ! [Console.print]
    Console.print(String.fromInt(probe(3)))
"#;

const DIVERGENCE_951_ERROR: &str = "the pattern binding 'dbl' shadows the function 'dbl' \
     defined at line 3; every name means one thing in its scope — rename one of them";

#[test]
fn issue_951_divergence_program_is_rejected_by_run() {
    let path = temp_module("run-951", DIVERGENCE_951_SRC);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(&path)
        .output()
        .expect("expected `aver run` to execute");
    cleanup(&path);
    assert!(
        !out.status.success(),
        "the #951 program must be rejected, not executed:\n{}",
        format_output(&out)
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains(DIVERGENCE_951_ERROR),
        "the refusal must name the binder, what it shadows, and where it lives:\n{}",
        format_output(&out)
    );
    assert!(
        stderr.contains("error[8:0]"),
        "the error position must point at the BINDER's line:\n{}",
        format_output(&out)
    );
}

#[test]
fn issue_951_divergence_program_is_rejected_by_compile() {
    let path = temp_module("compile-951", DIVERGENCE_951_SRC);
    let module_root = path.parent().expect("temp module has parent").to_path_buf();
    let project = module_root.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("rust")
        .arg("--name")
        .arg("shadow_reject")
        .arg("-o")
        .arg(&project)
        .arg("--module-root")
        .arg(&module_root)
        .output()
        .expect("expected `aver compile` to execute");
    cleanup(&path);
    assert!(
        !out.status.success(),
        "the #951 program must be rejected by compile too:\n{}",
        format_output(&out)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains(DIVERGENCE_951_ERROR),
        "compile must report the same refusal run does:\n{}",
        format_output(&out)
    );
}

/// A parameter spelling a module fn's name is the same offence — this
/// is the shape the rogue example's `renderTile(…, isVisible, …)`
/// param had before its rename.
const PARAM_OVER_FN_SRC: &str = r#"module Tmp

fn area(s: Int) -> Int
    s * s

fn apply(area: Int) -> Int
    area + 1

fn main()
    ! [Console.print]
    Console.print(String.fromInt(apply(2)))
"#;

#[test]
fn a_param_spelling_a_module_fn_is_rejected_by_run() {
    let path = temp_module("run-param", PARAM_OVER_FN_SRC);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(&path)
        .output()
        .expect("expected `aver run` to execute");
    cleanup(&path);
    assert!(
        !out.status.success(),
        "a param over a module fn must be rejected:\n{}",
        format_output(&out)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains(
            "the parameter 'area' shadows the function 'area' defined at line 3; \
             every name means one thing in its scope — rename one of them"
        ),
        "the refusal must be the standard shadow error:\n{}",
        format_output(&out)
    );
}

/// SIBLING arms may reuse a name — the #949 per-arm slot machinery
/// keeps them sound, and the ban must not touch them.
const SIBLING_ARMS_SRC: &str = r#"module Tmp

type Shape
    Circle(Int)
    Square(Int)

fn eval(p: Shape) -> Int
    match p
        Shape.Circle(n) -> n + 1
        Shape.Square(n) -> n * 10

fn main()
    ! [Console.print]
    Console.print(String.fromInt(eval(Shape.Circle(5))))
    Console.print(String.fromInt(eval(Shape.Square(3))))
"#;

#[test]
fn sibling_arms_reusing_a_name_still_run() {
    let path = temp_module("run-siblings", SIBLING_ARMS_SRC);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(&path)
        .output()
        .expect("expected `aver run` to execute");
    cleanup(&path);
    assert!(
        out.status.success(),
        "sibling arms are not in each other's scope — this program is legal:\n{}",
        format_output(&out)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "6\n30",
        "the sibling-arm program must still answer correctly"
    );
}

/// A shadowing binder in a DEPENDENCY module is refused when the entry
/// is compiled — dep modules go through the same front door.
#[test]
fn a_shadowing_dep_module_is_rejected_by_run() {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let n = UNIQUE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("aver-shadow-ban-dep-{nanos}-{n}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    fs::write(
        dir.join("main.av"),
        "module Main\n    depends [Helper]\n\nfn main()\n    ! [Console.print]\n    Console.print(String.fromInt(Helper.apply(2)))\n",
    )
    .expect("write entry");
    fs::write(
        dir.join("helper.av"),
        "module Helper\n    exposes [apply]\n\nfn area(s: Int) -> Int\n    s * s\n\nfn apply(area: Int) -> Int\n    area + 1\n",
    )
    .expect("write dep");
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(&dir)
        .output()
        .expect("expected `aver run` to execute");
    let _ = fs::remove_dir_all(&dir);
    assert!(
        !out.status.success(),
        "a shadowing dep module must be rejected:\n{}",
        format_output(&out)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains(
            "the parameter 'area' shadows the function 'area' defined at line 4; \
             every name means one thing in its scope — rename one of them"
        ),
        "the dep refusal must carry the standard shadow error:\n{}",
        format_output(&out)
    );
}
