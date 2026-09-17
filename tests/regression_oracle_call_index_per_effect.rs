//! Regression — the call index a `given` stub receives counts the calls of ITS
//! operation, not every effect the function under test reaches.
//!
//! A scripted peer answers by call number: read 0 is 7, read 1 is 9. Under the
//! old shared counter a clock read between the two peer reads pushed the second
//! read to index 2, so the same stub answered 0 and the packed result dropped
//! from 7009 to 7000. Adding a log line or a clock read to the code under test
//! silently rewrote what every stub of every other operation saw.
//!
//! The runtime half and the proof-export half are pinned together in this one
//! file on purpose. The lifter writes the index into the emitted Lean and Dafny
//! as a literal, so the two interpreters only agree while both use the same
//! numbering. `tests/regression_oracle_counter_order.rs` pins the other
//! invariant these two halves share: arguments are charged before the call that
//! surrounds them.

use std::path::PathBuf;
use std::process::Command;

use aver::diagnostics::vm_verify::run_verify_for_items_vm_with_mode;
use aver::source::parse_source;
use aver::verify_law::expand::ExpansionMode;

/// One function reaching two operations, with a scripted stub for each. The
/// peer answers by call number and the clock is frozen, so the value the law
/// asserts is exactly the numbering the peer was handed.
const MIXED: &str = r#"module MixedCallIndex
    intent = "Each operation numbers its own calls from zero."
    exposes [twoReadsWithClock]
    effects [Random.int, Time.unixMs]

fn twoReadsWithClock() -> Int
    ? "Two peer reads with one clock read between them."
    ! [Random.int, Time.unixMs]
    a = Random.int(0, 100)
    _now = Time.unixMs()
    b = Random.int(0, 100)
    a * 1000 + b

fn peer(path: BranchPath, call: Int, low: Int, high: Int) -> Result<Int, String>
    ? "A scripted peer: says 7 on its first call and 9 on its second."
    match call
        0 -> Result.Ok(7)
        1 -> Result.Ok(9)
        _ -> Result.Ok(0)

fn clock(path: BranchPath, call: Int) -> Int
    ? "A frozen clock, so only the peer's numbering is observable."
    1000

verify twoReadsWithClock law scriptedPeer
    given rnd: Random.int = [peer]
    given now: Time.unixMs = [clock]
    twoReadsWithClock() => 7009
"#;

#[test]
fn vm_numbers_each_operation_from_zero() {
    let items = parse_source(MIXED).unwrap_or_else(|e| panic!("parse failed: {e:?}"));
    let results = run_verify_for_items_vm_with_mode(
        items,
        None,
        Some(env!("CARGO_MANIFEST_DIR")),
        "regression_oracle_call_index_per_effect.av",
        ExpansionMode::Declared,
    )
    .expect("verify run");

    assert_eq!(results.len(), 1, "one verify block");
    let result = &results[0];
    assert_eq!(
        (result.passed, result.failed),
        (1, 0),
        "the second Random.int must reach the peer as call 1 even though Time.unixMs ran \
         between the two reads — a reported actual of 7000 means the clock consumed the \
         peer's index and src/vm/runtime.rs take_oracle_coordinates is back to one counter \
         per case"
    );
}

fn export_mixed(backend: &str, file: &str) -> String {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("test-out")
        .join(format!("oracle-call-index-{backend}"));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).expect("create output dir");
    let source = root.join("mixed.av");
    std::fs::write(&source, MIXED).expect("write source");

    let output = Command::new(aver_bin)
        .arg("proof")
        .arg(&source)
        .arg("--backend")
        .arg(backend)
        .arg("-o")
        .arg(&root)
        .output()
        .expect("run aver proof");
    assert!(
        output.status.success(),
        "aver proof --backend {backend} failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    std::fs::read_to_string(root.join(file)).unwrap_or_else(|e| panic!("read {file}: {e}"))
}

#[test]
fn exported_lean_numbers_each_operation_from_zero() {
    let lean = export_mixed("lean", "MixedCallIndex.lean");
    for call in [
        "rnd_Random_int path 0 0 100",
        "rnd_Time_unixMs path 0",
        "rnd_Random_int path 1 0 100",
    ] {
        assert!(
            lean.contains(call),
            "the lifted proof must charge each operation its own index, so the clock is \
             index 0 and the second read index 1; `{call}` is missing from:\n{lean}"
        );
    }
    assert!(
        !lean.contains("rnd_Random_int path 2 0 100"),
        "index 2 means the lifter still shares one counter across operations:\n{lean}"
    );
}

#[test]
fn exported_dafny_numbers_each_operation_from_zero() {
    let dafny = export_mixed("dafny", "MixedCallIndex.dfy");
    for call in [
        "rnd_Random_int(path, 0, 0, 100)",
        "rnd_Time_unixMs(path, 0)",
        "rnd_Random_int(path, 1, 0, 100)",
    ] {
        assert!(
            dafny.contains(call),
            "the Dafny export must carry the same numbering as the Lean export and the VM; \
             `{call}` is missing from:\n{dafny}"
        );
    }
    assert!(
        !dafny.contains("rnd_Random_int(path, 2, 0, 100)"),
        "index 2 means the lifter still shares one counter across operations:\n{dafny}"
    );
}
