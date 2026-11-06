//! `Int` map keys ordered on machine words on wasm-gc (`src/codegen/wasm_gc/
//! maps/word_order.rs` and the `Int` key comparator in `maps/order.rs`). The
//! VM is the ground truth, and the answer is checked by hand: negative keys,
//! zero, the smallest and largest machine words, keys past them mixed with
//! small ones, a key built past the word and brought back equal to a small
//! one, overwrite, removal back to an all-small map, a map of a thousand
//! scattered keys with and without one Big key, entries, tuple keys with Big
//! components, equality of maps built in different orders or grown and shrunk
//! back, and maps of maps keyed by Big and negative keys. A plain build holds
//! the `$AverInt` keys unboxed; a `--certify` build keeps them boxed.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use std::path::{Path, PathBuf};
use std::process::Command;

fn fixture() -> PathBuf {
    aver_cmd::repo_root().join("tests/fixtures/wasm_gc_map_int_keys/keys.av")
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

/// The module `file` compiles to, with the word order on or off.
fn module(file: &Path, words: bool, certify: bool) -> Vec<u8> {
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
    if !words {
        cmd.env("AVER_NO_WORD_KEYS", "1");
    }
    let out = cmd.output().expect("aver compile executes");
    assert!(out.status.success(), "{}", aver_cmd::format_output(&out));
    std::fs::read(dir.path().join("keys.wasm")).expect("module written")
}

const ANSWER: &str = "\
small: ,-9223372036854775808,-7,-1,0,5,9223372036854775807 len 6
same: ,5,6,2,3,1,40 len 6 get 40
mixed: ,-255211775190703847597530955573826158592,-9223372036854775809,-9223372036854775808,-7,-1,0,5,9223372036854775807,9223372036854775808,255211775190703847597530955573826158592 len 10
back: ,-9223372036854775808,-7,-1,0,5,9223372036854775807 values ,5,6,2,3,1,4
gone: ,-7,-1,5,9223372036854775807 has0 false hasMin false
many: 1009 sorted 1009 first ,-504,-503,-502,-501
manyBig: 1010 sorted 1010
entries:  -2=30 2=20
tuples: ,3,2,4,1
equal: true changed false shrunk true grown true has true false
nested: ,-1,9223372036854775808,255211775190703847597530955573826158592 inner 4 equal true gone 0
";

#[test]
fn int_keys_iterate_in_the_order_the_vm_gives() {
    let file = fixture();
    let vm = run(&file, false);
    assert_eq!(vm, ANSWER, "the VM diverged from the checked answer");
    assert_eq!(run(&file, true), ANSWER, "wasm-gc diverged from the VM");
}

#[test]
fn only_plain_builds_order_int_keys_on_words() {
    let file = fixture();
    // Not vacuous: the plain module orders on words.
    assert_ne!(module(&file, true, false), module(&file, false, false));
    // A `--certify` build keeps the plain form.
    assert_eq!(module(&file, true, true), module(&file, false, true));
}
