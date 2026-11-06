//! String literals on wasm-gc are built once, into a global, and shared by
//! every later read (`src/codegen/wasm_gc/body/from_mir/strings.rs`); an
//! `Int` param a twin holds as its machine word is stringified on that word.
//! The VM is the ground truth: a bare literal read many times, an
//! interpolation's constant pieces, a literal a self loop grows from (the
//! loop must not write into the shared string), literal patterns, and Int
//! params at the ends of the machine word and past it.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use std::path::{Path, PathBuf};
use std::process::Command;

fn fixture() -> PathBuf {
    aver_cmd::repo_root().join("tests/fixtures/wasm_gc_string_literals/shapes.av")
}

fn run(file: &Path, wasm_gc: bool) -> String {
    let mut cmd = Command::new(aver_cmd::aver_bin());
    cmd.current_dir(aver_cmd::repo_root()).arg("run").arg(file);
    if wasm_gc {
        cmd.arg("--wasm-gc");
    }
    let out = cmd.output().expect("aver run executes");
    assert!(out.status.success(), "{}", aver_cmd::format_output(&out));
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// The module `file` compiles to, with the literal pool on or off.
fn module(file: &Path, pool: bool, certify: bool) -> Vec<u8> {
    let dir = tempfile::tempdir().expect("compile directory");
    let mut cmd = Command::new(aver_cmd::aver_bin());
    cmd.current_dir(aver_cmd::repo_root())
        .arg("compile")
        .arg(file)
        .args(["--target", "wasm-gc", "-o"])
        .arg(dir.path());
    if certify {
        cmd.arg("--certify");
    }
    if !pool {
        cmd.env("AVER_NO_STRING_LITERAL_POOL", "1");
    }
    let out = cmd.output().expect("aver compile executes");
    assert!(out.status.success(), "{}", aver_cmd::format_output(&out));
    std::fs::read(dir.path().join("shapes.wasm")).expect("module written")
}

#[test]
fn shared_literals_print_what_the_vm_prints() {
    let file = fixture();
    let vm = run(&file, false);
    assert!(
        vm.starts_with("hellohello\nitem 1 of 2, done\nitem -9223372036854775808 of"),
        "{vm}"
    );
    assert!(
        vm.contains("\nstart---\nstart--\nstart\na start\ngreeting\n"),
        "{vm}"
    );
    assert!(
        vm.contains("\n0;-1;9223372036854775807;-9223372036854775807;-9223372036854775808;9223372036854775808;"),
        "{vm}"
    );
    assert_eq!(vm, run(&file, true));
}

#[test]
fn only_plain_builds_share_literals() {
    let file = fixture();
    // Not vacuous: the plain module reads its literals from globals.
    assert_ne!(module(&file, true, false), module(&file, false, false));
    // A `--certify` build keeps every literal built where it is read.
    assert_eq!(module(&file, true, true), module(&file, false, true));
}
