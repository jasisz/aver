//! `Int` trees on wasm-gc (`src/codegen/wasm_gc/body/from_mir/int_tree.rs`)
//! are computed on `i64` first and exactly when a leaf is Big or an
//! operation overflows. The VM is the ground truth: every shape must print
//! what it prints, including overflow in the middle of a tree, Big leaves,
//! `i64::MIN`, Euclidean `Int.div` / `Int.mod` by literals of both signs,
//! results on the edge of the machine word, carriers and record fields in
//! one tree, and leaves with effects, which run once each and in order.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use std::path::{Path, PathBuf};
use std::process::Command;

fn fixture(name: &str) -> PathBuf {
    aver_cmd::repo_root()
        .join("tests/fixtures/wasm_gc_int_tree")
        .join(name)
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

/// The module `file` compiles to, with the trees on or off.
fn module(file: &Path, trees: bool) -> Vec<u8> {
    let dir = std::env::temp_dir().join(format!(
        "aver-int-tree-{}-{}",
        std::process::id(),
        u8::from(trees)
    ));
    let mut cmd = Command::new(aver_cmd::aver_bin());
    cmd.current_dir(aver_cmd::repo_root())
        .arg("compile")
        .arg(file)
        .args(["--target", "wasm-gc", "-o"])
        .arg(&dir);
    if !trees {
        cmd.env("AVER_NO_INT_TREE", "1");
    }
    let out = cmd.output().expect("aver compile executes");
    assert!(out.status.success(), "{}", aver_cmd::format_output(&out));
    let stem = file
        .file_stem()
        .and_then(|s| s.to_str())
        .expect("file stem");
    let bytes = std::fs::read(dir.join(format!("{stem}.wasm"))).expect("module written");
    let _ = std::fs::remove_dir_all(&dir);
    bytes
}

#[test]
fn int_trees_print_what_the_vm_prints() {
    let file = fixture("shapes.av");
    let vm = run(&file, false);
    assert!(vm.contains("leaf 1\nleaf 2\nleaf 3\n"), "{vm}");
    assert_eq!(vm, run(&file, true));
    // Not vacuous: the module computes trees on `i64`.
    assert_ne!(module(&file, true), module(&file, false));
}

#[test]
fn fns_the_certificate_reads_keep_their_bytes() {
    let file = fixture("kept.av");
    assert_eq!(run(&file, false), run(&file, true));
    assert_eq!(module(&file, true), module(&file, false));
}
