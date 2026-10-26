//! Twins on wasm-gc (`src/codegen/wasm_gc/twins.rs`): a fn returns its `Int`,
//! `Option`, `Result` or small record unpacked, and takes its `Int` params as
//! machine words, to callers that use them so. The VM is the ground truth:
//! every shape must print what it prints, including Big results and Big
//! params in every position, `i64::MIN`, a Big param handed back as it is,
//! args with effects (run once each, in order), twin calls nested in the args
//! of twin calls, self and mutual tail calls (a long one must not grow the
//! stack), a function value, a record result stored in a field, a `Unit`
//! param beside an `Int` one, an `Int` param read as a string position, and
//! `?` and `?!` through twins: an `Err` returned early, a Big `Ok` payload,
//! a `?` in a long mutual tail loop; and `Int` locals a twin holds as words,
//! Big or not, compared with a literal, stored packed, bound by a match, and
//! a string's code points in a fused loop; and held words, Big or not,
//! matched against literals and used as a vector index (negative, past
//! `2^31`, Big).

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use std::path::{Path, PathBuf};
use std::process::Command;

fn fixture(name: &str) -> PathBuf {
    aver_cmd::repo_root()
        .join("tests/fixtures/wasm_gc_twins")
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

/// The module `file` compiles to, with the twins on or off.
fn module(file: &Path, twins: bool) -> Vec<u8> {
    let dir = std::env::temp_dir().join(format!(
        "aver-twins-{}-{}-{}",
        std::process::id(),
        file.file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("module"),
        u8::from(twins)
    ));
    let mut cmd = Command::new(aver_cmd::aver_bin());
    cmd.current_dir(aver_cmd::repo_root())
        .arg("compile")
        .arg(file)
        .args(["--target", "wasm-gc", "-o"])
        .arg(&dir);
    if !twins {
        cmd.env("AVER_NO_TWINS", "1");
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

// A held slot bound a second time in one frame must not keep the box of the
// value before (`twin::set_held` clears it). No program can show that yet: a
// name cannot be bound again, every binder has its own slot, and a loop is a
// tail call into a new frame, so the shapes here bind each held slot once.
#[test]
fn twins_print_what_the_vm_prints() {
    let file = fixture("shapes.av");
    let vm = run(&file, false);
    assert!(vm.contains("arg 1\narg 2\narg 3\norder 123\n"), "{vm}");
    assert!(vm.contains("countdown 500000500000 "), "{vm}");
    assert!(vm.contains(" ok 20000300000\n"), "{vm}");
    assert_eq!(vm, run(&file, true));
    // Not vacuous: the module has twins.
    assert_ne!(module(&file, true), module(&file, false));
}
