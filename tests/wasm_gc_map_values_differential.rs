//! Map values whose type holds a map on wasm-gc. Every `Map<K, V>` carries a
//! structural eq + hash that dispatches to `V`'s helpers, so a value record
//! with a `Map<_, _>` field reaches `emit_hash_record` / `emit_eq_record`
//! (`src/codegen/wasm_gc/maps.rs`), which used to refuse `Map<_, _>` fields
//! with "record-key field type not in …" although the same program ran on the
//! VM. The fixture stores records holding maps, maps of maps of `Bytes`,
//! records holding `Bytes` as values and as keys, and a record holding a map
//! of maps, and compares such maps with `==`. The VM is the ground truth, and
//! the answer is checked by hand.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use std::path::{Path, PathBuf};
use std::process::Command;

fn fixture() -> PathBuf {
    aver_cmd::repo_root().join("tests/fixtures/wasm_gc_map_values/values.av")
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

const ANSWER: &str = "\
facts: 2 4 -1 len 2
bytes: 20 30 -1 -1
blobs: 3 len 2
keyed: two len 2
equal: true false true false
stores: 1 equal true false
";

#[test]
fn map_values_holding_maps_run_on_wasm_gc_as_on_the_vm() {
    let file = fixture();
    let vm = run(&file, false);
    assert_eq!(vm, ANSWER, "the VM diverged from the checked answer");
    assert_eq!(run(&file, true), ANSWER, "wasm-gc diverged from the VM");
}
