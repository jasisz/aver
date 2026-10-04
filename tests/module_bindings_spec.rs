//! Module-level bindings (`base = 40` outside any fn) read from fn bodies,
//! on every backend and in the proof export.
//!
//! What used to go wrong, one line each:
//! - VM: a dependency's binding was never compiled, so `aver run` on a
//!   program calling `Lib.shifted` (which reads `Lib`'s `base`) stopped at
//!   `undefined variable: base`, and `aver verify` skipped the file.
//! - wasm-gc: the flattener left a dependency fn reading `base` with no
//!   binding of that name, and the module refused to compile.
//! - Rust: a binding was a `let` inside `fn main`, so any other fn reading
//!   it named a variable that did not exist and rustc rejected the crate,
//!   entry module or dependency alike.
//! - Lean: `aver proof` emitted `def f (x : Int) : Int := (base + x)` with
//!   no `base`, without a warning; Lean answered `Unknown identifier`.
//!
//! Pinned here: VM, wasm-gc and Rust print the same lines and judge the
//! same `verify` blocks, with bindings in the entry and in dependencies
//! (two modules each holding a `base`, a binding whose value calls into
//! another module that reads its own binding); the proof export declares
//! each binding in its module's namespace, in dependency order, and the
//! laws over fns reading them are proved for every input.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

static UNIQUE: AtomicU64 = AtomicU64::new(0);

/// One module-level binding chain in the entry: `offset` is computed from
/// `base` through a fn, `label` interpolates `base`, and a `verify` case's
/// expected side reads a binding too.
const SINGLE: &str = r#"module Single
    intent = "Module-level bindings read from fns."

base = 40
offset = double(base)
label = "base={base}"
primes = [2, 3, 5]

fn double(n: Int) -> Int
    ? "Twice n."
    n + n

fn f(x: Int) -> Int
    ? "Adds base."
    base + x

fn g(x: Int) -> Int
    ? "Adds offset."
    offset + x

fn describe(x: Int) -> String
    ? "The label and a number."
    "{label}/{x}"

fn primeCount() -> Int
    ? "How many primes the module lists."
    List.len(primes)

verify f
    f(2) => 42
    f(0) => base

verify g
    g(1) => 81

verify describe
    describe(1) => "base=40/1"

verify primeCount
    primeCount() => 3

verify f law addsBase
    given x: Int = [0, 1, 2]
    f(x) => x + 40

verify g law addsOffset
    given x: Int = [0, 1, 2]
    g(x) => x + 80

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(f(2)))
    Console.print(String.fromInt(g(1)))
    Console.print(describe(3))
    Console.print(String.fromInt(primeCount()))
"#;

const SINGLE_OUT: &str = "42\n81\nbase=40/3\n3";

/// `Core` holds a binding its exposed fn reads.
const CORE: &str = r#"module Core
    exposes [bump]
    intent = "A dependency of a dependency with its own binding."

step = 5

fn bump(n: Int) -> Int
    ? "Adds step."
    n + step

verify bump
    bump(1) => 6
"#;

/// `Lib` holds a `base` (as the entry does), a binding computed from it,
/// and one computed by calling into `Core` — which reads `Core`'s binding,
/// so `Core`'s bindings must be filled before `Lib`'s.
const LIB: &str = r#"module Lib
    depends [Core]
    exposes [shifted, greeting]
    intent = "Exposes fns reading module-level bindings."

base = 40
offset = twice(base)
bumped = Core.bump(0)
label = "lib-{base}"

fn twice(n: Int) -> Int
    ? "Twice n."
    n + n

fn shifted(x: Int) -> Int
    ? "Adds base, offset and bumped."
    base + offset + bumped + x

fn greeting(name: String) -> String
    ? "The label and a name."
    "{label}:{name}"

verify shifted
    shifted(2) => 127

verify shifted law addsAll
    given x: Int = [0, 1, 2]
    shifted(x) => x + 125
"#;

const APP: &str = r#"module App
    depends [Lib]
    intent = "Calls dependency fns that read their module's bindings."

base = 7

fn h(x: Int) -> Int
    ? "Shifts through Lib, then adds this module's base."
    Lib.shifted(x) + base

verify h
    h(1) => 133
    h(0) => 125 + base

verify h law shiftsAll
    given x: Int = [0, 1, 2]
    h(x) => x + 132

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(h(1)))
    Console.print(Lib.greeting("x"))
"#;

const APP_OUT: &str = "133\nlib-40:x";

/// A fresh directory holding `files`; returns it.
fn project(prefix: &str, files: &[(&str, &str)]) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let n = UNIQUE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("aver-modbind-{prefix}-{nanos}-{n}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    for (name, source) in files {
        fs::write(dir.join(name), source).expect("write module");
    }
    dir
}

fn single() -> PathBuf {
    project("single", &[("main.av", SINGLE)])
}

fn multi() -> PathBuf {
    project(
        "multi",
        &[("core.av", CORE), ("lib.av", LIB), ("app.av", APP)],
    )
}

fn entry(dir: &Path) -> PathBuf {
    if dir.join("app.av").exists() {
        dir.join("app.av")
    } else {
        dir.join("main.av")
    }
}

/// `aver <args> <entry> --module-root <dir>`; stdout on success.
fn aver(dir: &Path, args: &[&str]) -> Result<String, String> {
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .args(args)
        .arg(entry(dir))
        .arg("--module-root")
        .arg(dir)
        .output()
        .expect("aver executes");
    if !out.status.success() {
        return Err(format_output(&out));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

fn assert_verify_passes(dir: &Path, args: &[&str], cases: usize) {
    let out = aver(dir, args).unwrap_or_else(|e| panic!("`aver {args:?}` failed:\n{e}"));
    assert!(
        out.contains(&format!("{cases}/{cases} cases passed | 0 failed"))
            && !out.contains("not checked"),
        "`aver {args:?}` did not check every case:\n{out}"
    );
}

#[test]
fn vm_runs_and_verifies_bindings_in_the_entry_and_in_dependencies() {
    let one = single();
    assert_eq!(aver(&one, &["run"]).unwrap(), SINGLE_OUT);
    // 5 plain cases + 2 laws × 3 samples.
    assert_verify_passes(&one, &["verify"], 11);

    let many = multi();
    assert_eq!(
        aver(&many, &["run"]).unwrap_or_else(|e| panic!("VM run failed:\n{e}")),
        APP_OUT
    );
    // App 2 + 3, Lib 1 + 3, Core 1.
    assert_verify_passes(&many, &["verify"], 10);

    let _ = fs::remove_dir_all(one);
    let _ = fs::remove_dir_all(many);
}

/// A false case over a dependency's binding fails rather than passing on a
/// value the binding never had.
#[test]
fn vm_verify_fails_a_false_case_over_a_dependency_binding() {
    let dir = project(
        "false",
        &[
            ("core.av", CORE),
            ("lib.av", LIB),
            (
                "app.av",
                &APP.replace("    h(1) => 133\n", "    h(1) => 8\n"),
            ),
        ],
    );
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("verify")
        .arg(dir.join("app.av"))
        .arg("--module-root")
        .arg(&dir)
        .output()
        .expect("aver executes");
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        !out.status.success() && text.contains("1 failed"),
        "a false case over `Lib`'s binding must fail:\n{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(dir);
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_agrees_with_the_vm_on_run_and_verify() {
    for (dir, expected, cases) in [(single(), SINGLE_OUT, 11), (multi(), APP_OUT, 10)] {
        assert_eq!(
            aver(&dir, &["run", "--wasm-gc"])
                .unwrap_or_else(|e| panic!("wasm-gc run failed:\n{e}")),
            expected
        );
        assert_verify_passes(&dir, &["verify", "--wasm-gc"], cases);
        let _ = fs::remove_dir_all(dir);
    }
}

/// `cargo build` / `cargo test` of an emitted project; one target directory
/// for the whole test so the runtime crate builds once.
fn cargo(project: &Path, target: &Path, subcommand: &str) -> std::process::Output {
    Command::new("cargo")
        .arg(subcommand)
        .arg("-q")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(project.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", target)
        .output()
        .expect("cargo runs")
}

#[test]
fn rust_agrees_with_the_vm_on_run_and_verify() {
    let target = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| repo_root().join("target"))
        .join("module-bindings-spec");
    for (dir, name, expected) in [
        (single(), "modbind_single", SINGLE_OUT),
        (multi(), "modbind_multi", APP_OUT),
    ] {
        let out_dir = dir.join("rust");
        let compiled = Command::new(aver_bin())
            .current_dir(repo_root())
            .arg("compile")
            .arg(entry(&dir))
            .arg("--module-root")
            .arg(&dir)
            .args(["--target", "rust", "--name", name, "-o"])
            .arg(&out_dir)
            .output()
            .expect("aver compile runs");
        assert!(
            compiled.status.success(),
            "aver compile --target rust failed:\n{}",
            format_output(&compiled)
        );
        let built = cargo(&out_dir, &target, "build");
        assert!(
            built.status.success(),
            "the emitted crate does not build:\n{}",
            format_output(&built)
        );
        let ran = Command::new(target.join("debug").join(name))
            .output()
            .expect("binary runs");
        assert!(ran.status.success(), "{}", format_output(&ran));
        assert_eq!(String::from_utf8_lossy(&ran.stdout).trim(), expected);
        // The entry's `verify` blocks become the crate's tests.
        let tested = cargo(&out_dir, &target, "test");
        assert!(
            tested.status.success(),
            "the emitted verify tests fail:\n{}",
            format_output(&tested)
        );
        let _ = fs::remove_dir_all(dir);
    }
}

/// The export declares every binding as a constant of its module, after the
/// fn its value calls and before the fns that read it, and the law proofs
/// unfold it.
#[test]
fn proof_export_declares_each_binding_in_its_module() {
    let dir = multi();
    let out_dir = dir.join("lean");
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("proof")
        .arg(dir.join("app.av"))
        .arg("--module-root")
        .arg(&dir)
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver proof runs");
    assert!(out.status.success(), "{}", format_output(&out));
    let lib = fs::read_to_string(out_dir.join("Lib.lean")).expect("Lib.lean");
    let position = |needle: &str| {
        lib.find(needle)
            .unwrap_or_else(|| panic!("`{needle}` missing from Lib.lean:\n{lib}"))
    };
    assert!(position("def twice") < position("def offset : Int"));
    assert!(position("def base : Int") < position("def offset : Int"));
    assert!(position("def bumped : Int") < position("def shifted"));
    assert!(lib.contains("def label : String"), "{lib}");
    assert!(
        lib.contains("simp only [shifted, base, offset, twice, bumped, Core.bump, Core.step]"),
        "the law over `shifted` must unfold the bindings it reads:\n{lib}"
    );
    let core = fs::read_to_string(out_dir.join("Core.lean")).expect("Core.lean");
    assert!(core.contains("def step : Int"), "{core}");
    let app = fs::read_to_string(out_dir.join("App.lean")).expect("App.lean");
    assert!(app.contains("def base : Int"), "{app}");
    let _ = fs::remove_dir_all(dir);
}

/// Through Lean: every law over a fn reading a binding is proved for every
/// input, in the entry and in the dependencies.
#[test]
fn proof_check_proves_the_laws_over_bindings() {
    if !lean_required::lake_available() {
        return;
    }
    for dir in [single(), multi()] {
        let out = Command::new(aver_bin())
            .current_dir(repo_root())
            .arg("proof")
            .arg(entry(&dir))
            .arg("--module-root")
            .arg(&dir)
            .arg("--check")
            .arg("-o")
            .arg(dir.join("lean"))
            .output()
            .expect("aver proof runs");
        let text = format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        assert!(
            out.status.success() && text.contains("0 sorries, universal: yes"),
            "laws over module-level bindings must be proved:\n{}",
            format_output(&out)
        );
        let _ = fs::remove_dir_all(dir);
    }
}
