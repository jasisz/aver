//! Multi-module games MIR coverage + validity gate (Phase 6 #340/#252).
//!
//! The single-file gate (`wasm_gc_differential_mir.rs`) can't reach the
//! rich match shapes — user-variant, record, and collection
//! destructuring live almost entirely in the multi-module games (snake /
//! rogue / checkers / doom: sum types + records), which need the CLI's
//! `load_compile_deps` + `flatten_multimodule`. This test closes the gap
//! by driving the real `aver compile --target wasm-gc` pipeline as a
//! subprocess: for each game it compiles once (the production MIR path),
//! asserts the emit succeeds (the backend validates the bytes via
//! `wasmparser` before writing them, so a successful compile is a
//! validation pass), and holds a MIR-coverage floor.
//!
//! `AVER_WASMGC_MIR_COUNT=1` makes the compiler print how many fns the
//! MIR emitter rendered, so the coverage floor catches a covered
//! construct silently regressing to the `unreachable` trap stub on the
//! flattened game.
//!
//! The historical A/B byte-differential (MIR-on vs a forced
//! `ResolvedExpr` baseline) is gone with the HIR walker — there is no
//! baseline to diff against. Per-scenario size/speed is tracked by
//! `aver bench`; behavioural correctness by `aver run --wasm-gc` smoke
//! runs.

#![cfg(feature = "wasm")]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn examples_games_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("examples")
        .join("games")
}

/// Multi-module games — every `examples/games/<name>/main.av`.
fn multi_module_games() -> Vec<PathBuf> {
    let mut out = Vec::new();
    let Ok(read) = fs::read_dir(examples_games_dir()) else {
        return out;
    };
    for entry in read.flatten() {
        let dir = entry.path();
        if dir.is_dir() && dir.join("main.av").is_file() {
            out.push(dir);
        }
    }
    out.sort();
    out
}

/// Compile `<game>/main.av` to wasm-gc via the real CLI, returning the
/// emitted module bytes. A successful compile means the bytes passed the
/// backend's built-in validation.
fn compile_game(game_dir: &Path, out_dir: &Path) -> Result<Vec<u8>, String> {
    let main = game_dir.join("main.av");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("compile")
        .arg(&main)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--module-root")
        .arg(game_dir)
        .arg("-o")
        .arg(out_dir)
        .env("AVER_WASMGC_MIR_COUNT", "1")
        .output()
        .map_err(|e| format!("spawn `aver compile` failed: {e}"))?;
    if !output.status.success() {
        return Err(format!(
            "`aver compile` exited {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    let wasm = out_dir.join("main.wasm");
    fs::read(&wasm).map_err(|e| format!("read {}: {e}", wasm.display()))
}

/// Parse `AVER_WASMGC_MIR_EMITTED=N` out of the compile run's stderr.
fn mir_emitted_count(game_dir: &Path, out_dir: &Path) -> Result<usize, String> {
    let main = game_dir.join("main.av");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("compile")
        .arg(&main)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--module-root")
        .arg(game_dir)
        .arg("-o")
        .arg(out_dir)
        .env("AVER_WASMGC_MIR_COUNT", "1")
        .output()
        .map_err(|e| format!("spawn failed: {e}"))?;
    let stderr = String::from_utf8_lossy(&output.stderr);
    stderr
        .lines()
        .find_map(|l| l.strip_prefix("AVER_WASMGC_MIR_EMITTED="))
        .and_then(|n| n.trim().parse::<usize>().ok())
        .ok_or_else(|| format!("no AVER_WASMGC_MIR_EMITTED in stderr:\n{stderr}"))
}

#[test]
fn mir_body_emitter_compiles_and_validates_every_game() {
    let games = multi_module_games();
    assert!(
        !games.is_empty(),
        "no multi-module games found under examples/games/*/main.av"
    );

    let tmp = std::env::temp_dir().join("aver_wasmgc_games_diff");
    let _ = fs::remove_dir_all(&tmp);
    fs::create_dir_all(&tmp).expect("create temp dir");

    let mut failures: Vec<String> = Vec::new();
    let mut total_mir_emitted = 0usize;
    let mut compiled = 0usize;

    for game in &games {
        let name = game.file_name().unwrap().to_string_lossy().into_owned();
        let out_dir = tmp.join(&name);

        let bytes = match compile_game(game, &out_dir) {
            Ok(b) => b,
            Err(e) => {
                failures.push(format!("{name}: compile: {e}"));
                continue;
            }
        };
        // Non-empty module — a sanity check beyond "compile didn't error".
        assert!(!bytes.is_empty(), "{name}: emitted an empty wasm module");

        match mir_emitted_count(game, &out_dir) {
            Ok(n) => total_mir_emitted += n,
            Err(e) => failures.push(format!("{name}: mir count: {e}")),
        }
        compiled += 1;
    }

    if !failures.is_empty() {
        panic!(
            "{} of {} games failed to compile / validate through the MIR path:\n  - {}",
            failures.len(),
            games.len(),
            failures.join("\n  - ")
        );
    }

    // Coverage floor on the flattened games (richer shapes than the
    // single-file corpus: variant / record / collection / Vector). A
    // drop below the floor means a covered construct silently regressed
    // to the `unreachable` trap stub — fail CI rather than pass quietly.
    // Raise `MIN_MIR_EMITTED` when new coverage lands; never lower it
    // without a deliberate reason.
    const MIN_MIR_EMITTED: usize = 681;
    assert!(
        total_mir_emitted >= MIN_MIR_EMITTED,
        "MIR emitter rendered {total_mir_emitted} fns across {compiled} games, below the floor \
         of {MIN_MIR_EMITTED} — MIR coverage regressed. If this drop is intentional, lower the floor."
    );

    let _ = fs::remove_dir_all(&tmp);
    eprintln!(
        "mir_body_emitter_compiles_and_validates_every_game: {compiled} games compiled + \
         validated; MIR rendered {total_mir_emitted} fns total"
    );
}
