//! Multi-module games byte-differential for the wasm-gc → MIR port
//! (Phase 5 #340).
//!
//! The single-file differential (`wasm_gc_differential_mir.rs`)
//! saturates for the rich match shapes — user-variant, record, and
//! collection destructuring live almost entirely in the multi-module
//! games (snake / rogue / checkers / doom: sum types + records), which
//! that test can't reach (multi-module compilation needs the CLI's
//! `load_compile_deps` + `flatten_multimodule`). This test closes the
//! gap by driving the real `aver compile --target wasm-gc` pipeline as
//! a subprocess: for each game it compiles twice —
//!   * default — the MIR body emitter per-fn (with `ResolvedExpr`
//!     fallback);
//!   * `AVER_WASMGC_FORCE_NO_MIR=1` — the `ResolvedExpr` emitter
//!     everywhere;
//! and asserts the emitted `.wasm` bytes are **identical**. (The games
//! compile byte-deterministically, so two independent runs are
//! comparable.) `AVER_WASMGC_MIR_COUNT=1` makes the compiler print how
//! many fns the MIR emitter actually rendered, so the identity check
//! can't pass vacuously with the MIR path dead on the flattened game.
//!
//! This is the gate that verifies the richer shapes (variant / record /
//! collection / Vector) byte-for-byte on the programs that exercise them.

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
/// emitted module bytes plus how many fns the MIR emitter rendered
/// (`None` when MIR is force-disabled). `force_no_mir` toggles the
/// `ResolvedExpr`-only path.
fn compile_game(game_dir: &Path, out_dir: &Path, force_no_mir: bool) -> Result<Vec<u8>, String> {
    let main = game_dir.join("main.av");
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_aver"));
    cmd.arg("compile")
        .arg(&main)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--module-root")
        .arg(game_dir)
        .arg("-o")
        .arg(out_dir)
        .env("AVER_WASMGC_MIR_COUNT", "1");
    if force_no_mir {
        cmd.env("AVER_WASMGC_FORCE_NO_MIR", "1");
    }
    let output = cmd
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

/// Parse `AVER_WASMGC_MIR_EMITTED=N` out of the MIR-on run's stderr.
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
fn mir_and_resolved_emitters_agree_byte_for_byte_on_games() {
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
    let mut compared = 0usize;

    for game in &games {
        let name = game.file_name().unwrap().to_string_lossy().into_owned();
        let on_dir = tmp.join(format!("{name}_on"));
        let off_dir = tmp.join(format!("{name}_off"));

        let via_mir = match compile_game(game, &on_dir, false) {
            Ok(b) => b,
            Err(e) => {
                failures.push(format!("{name}: compile (mir on): {e}"));
                continue;
            }
        };
        let via_resolved = match compile_game(game, &off_dir, true) {
            Ok(b) => b,
            Err(e) => {
                failures.push(format!("{name}: compile (mir off): {e}"));
                continue;
            }
        };

        if via_mir != via_resolved {
            let first_diff = via_mir
                .iter()
                .zip(via_resolved.iter())
                .position(|(a, b)| a != b);
            failures.push(format!(
                "{name}: MIR vs ResolvedExpr emit diverged — {} vs {} bytes, first diff at {:?}",
                via_mir.len(),
                via_resolved.len(),
                first_diff
            ));
            continue;
        }

        match mir_emitted_count(game, &on_dir) {
            Ok(n) => total_mir_emitted += n,
            Err(e) => failures.push(format!("{name}: mir count: {e}")),
        }
        compared += 1;
    }

    if !failures.is_empty() {
        panic!(
            "{} of {} games diverged between the MIR and ResolvedExpr wasm-gc emitters:\n  - {}",
            failures.len(),
            games.len(),
            failures.join("\n  - ")
        );
    }

    // Coverage floor on the flattened games (richer shapes than the
    // single-file corpus: variant / record / collection / Vector). A
    // drop below the floor means a covered construct silently regressed
    // to the HIR fallback — fail CI rather than pass quietly. Raise
    // `MIN_MIR_EMITTED` when new coverage lands; never lower it without
    // a deliberate reason.
    const MIN_MIR_EMITTED: usize = 549;
    assert!(
        total_mir_emitted >= MIN_MIR_EMITTED,
        "MIR emitter rendered {total_mir_emitted} fns across {compared} games, below the floor \
         of {MIN_MIR_EMITTED} — MIR coverage regressed. If this drop is intentional, lower the floor."
    );

    let _ = fs::remove_dir_all(&tmp);
    eprintln!(
        "mir_and_resolved_emitters_agree_byte_for_byte_on_games: {compared} games byte-identical; \
         MIR rendered {total_mir_emitted} fns total"
    );
}
