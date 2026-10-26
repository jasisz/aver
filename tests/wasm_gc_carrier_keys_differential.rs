//! A map keyed by a carrier on wasm-gc (`TypeRegistry::set_eligible_carriers`):
//! the key is the carrier's `i64`, boxed in the keys array only when a new key
//! goes in, and a carrier inside a key (a record field, a sum's payload, an
//! `Option`, a `Result`, a tuple or a list element) is an `i64` too. The VM is
//! the ground truth: `words.av` takes its carrier from another module and
//! covers keys at the bounds and outside them, `set` on new and present keys,
//! `get`, `has`, `remove`, `keys`, `values`, `entries`, `fromList`, `==` and
//! growth; `inside.av` looks keys of every other shape up.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use std::path::{Path, PathBuf};
use std::process::Command;

fn fixtures() -> PathBuf {
    aver_cmd::repo_root().join("tests/fixtures/wasm_gc_carrier_keys")
}

fn run(file: &Path, wasm_gc: bool) -> String {
    let mut cmd = Command::new(aver_cmd::aver_bin());
    cmd.current_dir(aver_cmd::repo_root())
        .arg("run")
        .arg(file)
        .arg("--module-root")
        .arg(fixtures());
    if wasm_gc {
        cmd.arg("--wasm-gc");
    }
    let out = cmd.output().expect("aver run executes");
    assert!(out.status.success(), "{}", aver_cmd::format_output(&out));
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// The module `file` compiles to, with carriers held as `i64` or not.
fn module(file: &Path, carriers: bool) -> Vec<u8> {
    let stem = file
        .file_stem()
        .and_then(|s| s.to_str())
        .expect("file stem");
    let dir = std::env::temp_dir().join(format!(
        "aver-carrier-keys-{}-{stem}-{}",
        std::process::id(),
        u8::from(carriers)
    ));
    let mut cmd = Command::new(aver_cmd::aver_bin());
    cmd.current_dir(aver_cmd::repo_root())
        .arg("compile")
        .arg(file)
        .arg("--module-root")
        .arg(fixtures())
        .args(["--target", "wasm-gc", "-o"])
        .arg(&dir);
    if !carriers {
        cmd.env("AVER_NO_CARRIER_I64", "1");
    }
    let out = cmd.output().expect("aver compile executes");
    assert!(out.status.success(), "{}", aver_cmd::format_output(&out));
    let bytes = std::fs::read(dir.join(format!("{stem}.wasm"))).expect("module written");
    let _ = std::fs::remove_dir_all(&dir);
    bytes
}

fn assert_like_the_vm(name: &str, expected: &[&str]) {
    let file = fixtures().join(name);
    let vm = run(&file, false);
    for line in expected {
        assert!(vm.contains(line), "{name}: {line:?} not in\n{vm}");
    }
    assert_eq!(vm, run(&file, true), "{name}: wasm-gc differs from the VM");
    // Not vacuous: the key carrier is held as an `i64`.
    assert_ne!(module(&file, true), module(&file, false), "{name}");
}

#[test]
fn a_carrier_map_key_prints_what_the_vm_prints() {
    assert_like_the_vm(
        "words.av",
        &[
            "keys,0,7,4611686018427387903\n",
            "entries 0=over 7=SEVEN 4611686018427387903=top\n",
            "remove 2 no 2 3\n",
            "eq yes no no\n",
            "grown 5000 10012497500\n",
        ],
    );
}

#[test]
fn a_carrier_inside_a_map_key_prints_what_the_vm_prints() {
    assert_like_the_vm(
        "inside.av",
        &[
            "pair c a - 2 3/1 3/2 4611686018427387903/1\n",
            "sum 1 2 0 e h2 h5 t5.1\n",
            "result ok8 err -\n",
            "eq true true false\n",
        ],
    );
}
