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

fn write_temp_module(prefix: &str, source: &str) -> PathBuf {
    let dir = temp_output_dir(prefix);
    fs::create_dir_all(&dir).expect("create temp module dir");
    let path = dir.join("main.av");
    fs::write(&path, source).expect("write temp module");
    path
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
        .arg("edge-wasm")
        .arg("--module-root")
        .arg("examples/games/doom")
        .arg("--name")
        .arg("doom_smoke")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver compile --target edge-wasm` to run");

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
        .arg("edge-wasm")
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
        .arg("edge-wasm")
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

#[test]
fn wasm_run_preserves_recursive_linked_lists_across_compaction() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let module_path = write_temp_module(
        "aver-wasm-linked-list",
        r#"module Tmp

fn repeat(n: Int) -> List<Int>
    match n <= 0
        true -> []
        false -> List.concat(repeat(n - 1), [n])

fn main()
    ! [Console.print]
    Console.print("{repeat(8)}")
"#,
    );

    let output = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&module_path)
        .arg("--wasm")
        .output()
        .expect("expected `aver run --wasm` to execute");

    assert!(
        output.status.success(),
        "recursive linked-list WASM run failed:\n{}",
        format_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "[1, 2, 3, 4, 5, 6, 7, 8]"
    );

    let _ = fs::remove_dir_all(module_path.parent().expect("temp module dir"));
}

#[test]
fn wasm_run_handles_large_map_build_without_stack_overflow() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let module_path = write_temp_module(
        "aver-wasm-large-map",
        r#"module Tmp

fn build(n: Int, acc: Map<String, Int>) -> Map<String, Int>
    match n <= 0
        true -> acc
        false -> build(n - 1, Map.set(acc, Int.toString(n), n))

fn main()
    ! [Console.print]
    m = build(12000, Map.empty())
    Console.print("Entries: {Int.toString(List.len(Map.entries(m)))}")
"#,
    );

    let output = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&module_path)
        .arg("--wasm")
        .output()
        .expect("expected large-map `aver run --wasm` to execute");

    assert!(
        output.status.success(),
        "large map WASM run failed:\n{}",
        format_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "Entries: 12000"
    );

    let _ = fs::remove_dir_all(module_path.parent().expect("temp module dir"));
}

#[test]
fn wasm_compile_rejects_int_map_keys_with_validation_error() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let module_path = write_temp_module(
        "aver-wasm-validate-int-map",
        r#"module Tmp

fn main()
    ! [Console.print]
    m = Map.set(Map.empty(), 1, "one")
    Console.print("{Map.len(m)}")
"#,
    );
    let output_dir = temp_output_dir("aver-wasm-validate-int-map-out");

    let output = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&module_path)
        .arg("--target")
        .arg("wasm")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver compile --target wasm` to run");

    assert!(
        !output.status.success(),
        "expected Map<Int, V> WASM compile to fail validation, but it succeeded:\n{}",
        format_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("WASM emit produced invalid bytecode"),
        "expected emit-time validation error, got stderr:\n{}",
        stderr
    );
    assert!(
        !output_dir.join("main.wasm").exists(),
        "expected no .wasm artifact when validation fails"
    );

    let _ = fs::remove_dir_all(module_path.parent().expect("temp module dir"));
    let _ = fs::remove_dir_all(&output_dir);
}

#[test]
fn wasm_compile_rejects_float_map_keys_with_validation_error() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let module_path = write_temp_module(
        "aver-wasm-validate-float-map",
        r#"module Tmp

fn main()
    ! [Console.print]
    m = Map.set(Map.empty(), 1.5, "one-and-a-half")
    Console.print("{Map.len(m)}")
"#,
    );
    let output_dir = temp_output_dir("aver-wasm-validate-float-map-out");

    let output = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&module_path)
        .arg("--target")
        .arg("wasm")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver compile --target wasm` to run");

    assert!(
        !output.status.success(),
        "expected Map<Float, V> WASM compile to fail validation, but it succeeded:\n{}",
        format_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("WASM emit produced invalid bytecode"),
        "expected emit-time validation error, got stderr:\n{}",
        stderr
    );

    let _ = fs::remove_dir_all(module_path.parent().expect("temp module dir"));
    let _ = fs::remove_dir_all(&output_dir);
}

#[test]
fn wasm_compile_validates_string_map_keys_successfully() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let module_path = write_temp_module(
        "aver-wasm-validate-string-map",
        r#"module Tmp

fn main()
    ! [Console.print]
    m = Map.set(Map.empty(), "k", 1)
    Console.print("{Map.len(m)}")
"#,
    );
    let output_dir = temp_output_dir("aver-wasm-validate-string-map-out");

    let output = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&module_path)
        .arg("--target")
        .arg("edge-wasm")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver compile --target edge-wasm` to run");

    assert!(
        output.status.success(),
        "expected Map<String, Int> WASM compile to pass validation:\n{}",
        format_output(&output)
    );
    assert!(
        output_dir.join("main.wasm").exists(),
        "expected main.wasm to exist when validation passes"
    );

    let _ = fs::remove_dir_all(module_path.parent().expect("temp module dir"));
    let _ = fs::remove_dir_all(&output_dir);
}
