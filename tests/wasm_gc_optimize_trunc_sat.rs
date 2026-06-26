//! Regression: `aver compile --target wasm-gc --optimize` runs `wasm-opt` over
//! the whole module. The bignum prelude carries an f64->Int helper that emits
//! `i64.trunc_sat_f64_u` (the nontrapping-float-to-int proposal). The wasm-opt
//! invocation passes `--strip-target-features` (dropping the feature section),
//! so wasm-opt must be told the proposal is allowed via
//! `--enable-nontrapping-float-to-int` — otherwise it rejects the input with
//! "all used features should be allowed", failing the optimize step. That broke
//! `rebuild_playground.py` and `release.py` step 4 for every Int-heavy game.
//!
//! Fixture: the shipped `examples/games/life.av` reliably carries the helper
//! into a validate-before-DCE position (a trimmed synthetic program lets
//! wasm-opt strip it first, so it doesn't reproduce). A clean `aver compile
//! --optimize size` is the assertion — without the flag the command exits
//! non-zero (it may leave a partial pre-optimize wasm, so the EXIT CODE, not
//! file presence, is the signal).

#![cfg(feature = "wasm")]

use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

#[test]
fn wasm_gc_optimize_handles_trunc_sat_from_bignum_prelude() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let life = repo_root.join("examples/games/life.av");
    assert!(life.exists(), "fixture missing: {}", life.display());

    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let out_dir = std::env::temp_dir().join(format!("aver-trunc-sat-{nanos}"));

    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&life)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--optimize")
        .arg("size")
        .arg("-o")
        .arg(&out_dir)
        .output()
        .expect("aver compile executes");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let _ = std::fs::remove_dir_all(&out_dir);

    assert!(
        output.status.success(),
        "`aver compile --target wasm-gc --optimize size examples/games/life.av` failed — \
         likely wasm-opt rejecting `i64.trunc_sat_f64_u` because \
         `--enable-nontrapping-float-to-int` is missing from the wasm-opt invocation.\n\
         stdout:\n{stdout}\nstderr:\n{stderr}"
    );
}
