//! A `String` param a self loop grows by appending, held as a buffer on
//! wasm-gc (`src/codegen/wasm_gc/body/from_mir/str_builder.rs`). The VM is
//! the ground truth: an empty accumulator and empty pieces, multi-byte
//! pieces, an interpolation that starts with the param, the result used
//! after the loop, the param read every step before the append, passed on
//! unchanged and replaced, the param read in another argument or in a piece
//! (kept plain), a builder called in another builder's piece, two held
//! params in one loop, a self call that is not a tail call, and loops long
//! enough to grow the buffer many times, with and without a twin.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use std::path::{Path, PathBuf};
use std::process::Command;

fn fixture() -> PathBuf {
    aver_cmd::repo_root().join("tests/fixtures/wasm_gc_string_builder/shapes.av")
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

/// The module `file` compiles to, with the buffers on or off.
fn module(file: &Path, builders: bool, certify: bool) -> Vec<u8> {
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
    if !builders {
        cmd.env("AVER_NO_STRING_BUILDERS", "1");
    }
    let out = cmd.output().expect("aver compile executes");
    assert!(out.status.success(), "{}", aver_cmd::format_output(&out));
    std::fs::read(dir.path().join("shapes.wasm")).expect("module written")
}

#[test]
fn held_string_params_print_what_the_vm_prints() {
    let file = fixture();
    let vm = run(&file, false);
    assert!(
        vm.starts_with("[]\n[]\nstart:ababab\n200000\n400000\nxyxyxy\n"),
        "{vm}"
    );
    assert!(vm.contains("\nxxx/xxxx\n"), "{vm}");
    assert!(
        vm.contains("\n[iiii][iii][ii][i]\naaa+bbbbbb\nmzzzq|mzzz\n"),
        "{vm}"
    );
    assert_eq!(vm, run(&file, true));
}

#[test]
fn only_plain_builds_hold_string_params() {
    let file = fixture();
    // Not vacuous: the plain module holds params as buffers.
    assert_ne!(module(&file, true, false), module(&file, false, false));
    // A `--certify` build keeps the plain form.
    assert_eq!(module(&file, true, true), module(&file, false, true));
}
