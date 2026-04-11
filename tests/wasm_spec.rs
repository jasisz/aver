#![cfg(feature = "wasm")]

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn temp_output_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    std::env::temp_dir().join(format!("{prefix}-{nanos}"))
}

fn format_output(output: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn wasm_opt_available() -> bool {
    Command::new("wasm-opt")
        .arg("--version")
        .output()
        .map(|output| output.status.success())
        .unwrap_or(false)
}

#[test]
fn wasm_compile_supports_multimodule_game_examples() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let output_dir = temp_output_dir("aver-wasm-multimodule");

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/games/doom/main.av")
        .arg("--target")
        .arg("wasm")
        .arg("--module-root")
        .arg("examples/games/doom")
        .arg("--name")
        .arg("doom_smoke")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver compile --target wasm` to run");

    assert!(
        compile.status.success(),
        "multi-module WASM compile failed:\n{}",
        format_output(&compile)
    );
    assert!(
        output_dir.join("doom_smoke.wasm").exists(),
        "expected doom_smoke.wasm to be emitted"
    );

    let _ = fs::remove_dir_all(&output_dir);
}

#[test]
fn wasm_opt_oz_does_not_increase_size_for_snake() {
    if !wasm_opt_available() {
        eprintln!("skipping WASM size smoke test: `wasm-opt` not available");
        return;
    }

    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let raw_output_dir = temp_output_dir("aver-wasm-raw");
    let opt_output_dir = temp_output_dir("aver-wasm-opt");

    let raw = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/games/snake.av")
        .arg("--target")
        .arg("wasm")
        .arg("--name")
        .arg("snake_raw")
        .arg("-o")
        .arg(&raw_output_dir)
        .output()
        .expect("expected raw WASM compile to run");
    assert!(
        raw.status.success(),
        "raw WASM compile failed:\n{}",
        format_output(&raw)
    );

    let opt = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg("examples/games/snake.av")
        .arg("--target")
        .arg("wasm")
        .arg("--wasm-opt")
        .arg("oz")
        .arg("--name")
        .arg("snake_opt")
        .arg("-o")
        .arg(&opt_output_dir)
        .output()
        .expect("expected optimized WASM compile to run");
    assert!(
        opt.status.success(),
        "optimized WASM compile failed:\n{}",
        format_output(&opt)
    );

    let raw_size = raw_output_dir
        .join("snake_raw.wasm")
        .metadata()
        .expect("read raw WASM metadata")
        .len();
    let opt_size = opt_output_dir
        .join("snake_opt.wasm")
        .metadata()
        .expect("read optimized WASM metadata")
        .len();

    assert!(
        opt_size <= raw_size,
        "expected -Oz output to be no larger than raw output, got raw={} opt={}",
        raw_size,
        opt_size
    );

    let _ = fs::remove_dir_all(&raw_output_dir);
    let _ = fs::remove_dir_all(&opt_output_dir);
}
