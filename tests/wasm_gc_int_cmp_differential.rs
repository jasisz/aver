//! `Int` comparisons on wasm-gc against the VM, over a value set that crosses
//! the machine-word edge: zero, small values of both signs, the smallest and
//! largest words and their neighbours, values just past them, values two limbs
//! wide, and a value computed past the word and back. Every pair goes through
//! `<`, `>`, `<=`, `>=`, `Int.min` and `Int.max`, and the set is also used as
//! `Int` and `(Int, Int)` map keys, read back in key order. The comparisons
//! reach `__aint_cmp` from call sites that test for Small operands first and
//! from the map key comparator; `tests/cert_intcmp_differential.rs` calls the
//! helper directly on every pair.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use std::process::Command;

fn run(wasm_gc: bool) -> String {
    let file = aver_cmd::repo_root().join("tests/fixtures/wasm_gc_int_cmp/cmp.av");
    let mut cmd = Command::new(aver_cmd::aver_bin());
    cmd.current_dir(aver_cmd::repo_root()).arg("run").arg(file);
    if wasm_gc {
        cmd.arg("--wasm-gc");
    }
    let out = cmd.output().expect("aver run executes");
    assert!(out.status.success(), "{}", aver_cmd::format_output(&out));
    String::from_utf8_lossy(&out.stdout).into_owned()
}

#[test]
fn int_comparisons_agree_with_the_vm() {
    let vm = run(false);
    // Not vacuous: one row per value plus the two map lines.
    assert_eq!(vm.lines().count(), 18, "unexpected VM output:\n{vm}");
    assert!(
        vm.contains("keys: ,-85070591730234615865843651857942052864,-9223372036854775809,-9223372036854775808,"),
        "the VM key order changed:\n{vm}"
    );
    assert_eq!(run(true), vm, "wasm-gc diverged from the VM");
}
