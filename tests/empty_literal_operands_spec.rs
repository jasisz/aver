//! Generic List, Map and Vector builtins applied to an empty literal that
//! nothing else in the program types: `List.len([])`, `Map.len({})`,
//! `List.contains([], Option.None)`, `[] == []`. The checker used to leave
//! such a literal `List<T>` / `Map<K, V>`; the VM does not mind, but wasm-gc
//! registers one helper per element type and rejected the module ("List op
//! called but `List<T>` helper wasn't registered"). The checker now settles
//! the element type, so every backend prints what the VM prints. The Rust
//! backend runs the same fixture in `rust_codegen_differential`.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};
use std::process::Command;

const FIXTURE: &str = "tests/fixtures/empty_literal_operands_app.av";

const EXPECTED: &str = "\
listLen 0
listReverse 0
listConcat 0
listTake 0
listDrop 0
listZip 0
listContains false
listContainsWord false
listContainsNone false
vectorLen 0
mapLen 0
mapKeys 0
mapValues 0
mapEntries 0
mapHas false
mapRemove 0
mapFromList 0
mapGet true
listsEqual true
mapsEqual true
nonesEqual true
reversedEqual true
";

fn run(extra: &[&str]) -> String {
    let out = Command::new(aver_bin())
        .arg("run")
        .arg(FIXTURE)
        .args(extra)
        .current_dir(repo_root())
        .output()
        .expect("aver runs");
    assert!(out.status.success(), "{}", format_output(&out));
    String::from_utf8_lossy(&out.stdout).into_owned()
}

#[test]
fn the_vm_evaluates_every_empty_operand() {
    assert_eq!(run(&[]), EXPECTED);
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_compiles_and_evaluates_every_empty_operand() {
    assert_eq!(run(&["--wasm-gc"]), EXPECTED);
}
