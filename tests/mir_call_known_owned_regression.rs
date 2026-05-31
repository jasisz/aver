//! Regression: `CALL_KNOWN_OWNED` must not desync the leaf classifier.
//!
//! The MIR walker used to emit `CALL_KNOWN_OWNED` for a known-fn call
//! whose argument carried a last-use slot read (`compute_owned_mask`
//! non-zero). The leaf / parent-thin classifier only recognized
//! `CALL_KNOWN` as a call, so a fn that called out via
//! `CALL_KNOWN_OWNED` was wrongly flagged `leaf = true`; a caller's
//! plain `CALL_KNOWN` to it was then upgraded to the frameless
//! `CALL_LEAF`, and invoking the non-leaf fn without a `CallFrame`
//! corrupted control flow into a VM out-of-bounds panic.
//!
//! This shells `aver run` so the program executes through the FULL
//! production pipeline (including `last_use`, which the in-process
//! `mir_vm_parity` harness skips — the reason this class of divergence
//! stayed invisible) on the MIR-default VM path. `aver run` actually
//! executes `main` through the VM dispatch (the `verify` executor takes
//! a different path that does not reproduce the frameless-leaf crash),
//! so the panic surfaces as a non-zero exit.

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn tempfile(prefix: &str, suffix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    std::env::temp_dir().join(format!("{prefix}-{nanos}{suffix}"))
}

/// A chain of single-call functions. `pair(a, b) -> bump(b)` reads `b`
/// (slot 1) as argument 0, so `compute_owned_mask`'s positional
/// `slot == index` rule leaves the call as a plain `CALL_KNOWN`
/// (upgradeable). `bump(x) -> bump2(x)` reads `x` (slot 0) as argument 0
/// — slot == index — so the pre-fix walker emitted `CALL_KNOWN_OWNED`,
/// the shape that mis-flagged `bump`/`bump2` as leaves. The `verify`
/// block forces compilation + execution on the VM (MIR-default) path.
const REPRO: &str = r#"module L
    intent = "CALL_KNOWN_OWNED leaf-misclassification regression"
    depends []

fn deep(y: Int) -> Int
    y

fn bump2(x: Int) -> Int
    deep(x)

fn bump(x: Int) -> Int
    bump2(x)

fn pair(a: Int, b: Int) -> Int
    bump(b)

fn topB(n: Int) -> Int
    pair(n, n)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(topB(5)))
"#;

#[test]
fn call_known_owned_chain_does_not_crash_the_vm() {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = tempfile("call-known-owned", ".av");
    fs::write(&path, REPRO).expect("write tempfile");
    let output = Command::new(aver_bin)
        .arg("run")
        .arg(&path)
        .output()
        .expect("invoke aver");
    fs::remove_file(&path).ok();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "aver run crashed (the leaf-classifier desync regression — VM out-of-bounds panic): \
         status={:?}\nstdout={stdout}\nstderr={stderr}",
        output.status.code()
    );
    // topB(5) = pair(5,5) = bump(5) = bump2(5) = deep(5) = 5
    assert!(
        stdout.contains('5'),
        "expected topB(5) = 5 on stdout, got: {stdout:?} (stderr={stderr:?})"
    );
}
