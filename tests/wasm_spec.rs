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

fn wasmtime_available() -> bool {
    Command::new("wasmtime")
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

fn runtime_import_function_index(bytes: &[u8], import_name: &str) -> Option<u32> {
    let mut function_index = 0u32;
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload = payload.expect("parse wasm payload");
        if let wasmparser::Payload::ImportSection(reader) = payload {
            for import in reader {
                let import = import.expect("parse import");
                if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                    if import.module == "aver_runtime" && import.name == import_name {
                        return Some(function_index);
                    }
                    function_index += 1;
                }
            }
        }
    }
    None
}

fn runtime_export_function_index(bytes: &[u8], export_name: &str) -> Option<u32> {
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload = payload.expect("parse wasm payload");
        if let wasmparser::Payload::ExportSection(reader) = payload {
            for export in reader {
                let export = export.expect("parse export");
                if export.name == export_name && export.kind == wasmparser::ExternalKind::Func {
                    return Some(export.index);
                }
            }
        }
    }
    None
}

fn runtime_function_index(bytes: &[u8], name: &str) -> Option<u32> {
    runtime_import_function_index(bytes, name)
        .or_else(|| runtime_export_function_index(bytes, name))
}

fn wasm_calls_function(bytes: &[u8], function_index: u32) -> bool {
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload = payload.expect("parse wasm payload");
        if let wasmparser::Payload::CodeSectionEntry(body) = payload {
            let mut reader = body.get_operators_reader().expect("operator reader");
            while !reader.eof() {
                if let wasmparser::Operator::Call {
                    function_index: called,
                } = reader.read().expect("operator")
                {
                    if called == function_index {
                        return true;
                    }
                }
            }
        }
    }
    false
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
fn wasm_vector_set_with_same_default_uses_or_keep_runtime_path() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let source = write_temp_module(
        "aver-wasm-vector-set-or-keep",
        r#"
module Main

fn updateOrKeep(vec: Vector<Int>, idx: Int, value: Int) -> Vector<Int>
    Option.withDefault(Vector.set(vec, idx, value), vec)

fn main() -> Int
    vec = updateOrKeep(Vector.fromList([1, 2, 3]), 1, 9)
    Option.withDefault(Vector.get(vec, 1), 0)
"#,
    );
    let output_dir = temp_output_dir("aver-wasm-vector-set-or-keep-out");

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&source)
        .arg("--target")
        .arg("wasm")
        .arg("--name")
        .arg("vector_set_or_keep")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver compile --target wasm` to run");

    assert!(
        compile.status.success(),
        "WASM compile failed:\n{}",
        format_output(&compile)
    );

    let bytes = fs::read(output_dir.join("vector_set_or_keep.wasm")).expect("read wasm");
    let set_or_keep_idx = runtime_function_index(&bytes, "rt_vec_set_or_keep")
        .expect("rt_vec_set_or_keep function should exist");

    assert!(
        wasm_calls_function(&bytes, set_or_keep_idx),
        "expected fused Vector.set + Option.withDefault to call rt_vec_set_or_keep"
    );
    if let Some(set_idx) = runtime_function_index(&bytes, "rt_vec_set") {
        assert!(
            !wasm_calls_function(&bytes, set_idx),
            "fused Vector.set + Option.withDefault should not call copying rt_vec_set"
        );
    }

    let _ = fs::remove_dir_all(source.parent().expect("temp source has parent"));
    let _ = fs::remove_dir_all(&output_dir);
}

/// Patch-vector correctness across a chain of `Vector.set` calls.
/// Each fused `Option.withDefault(Vector.set(v, i, x), v)` builds a
/// new patch node on top of the previous version; reads must walk
/// the chain and return the most recent value. Without patch-chain
/// support in `rt_vec_get_cell` (or with a broken `rt_vec_set`) the
/// result diverges from the expected reference.
#[test]
fn wasm_run_patch_vector_chain_returns_latest_writes() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let module_path = write_temp_module(
        "aver-wasm-patch-chain",
        r#"module Tmp

fn write(v: Vector<Int>, i: Int, n: Int) -> Vector<Int>
    match i >= n
        true  -> v
        false -> write(Option.withDefault(Vector.set(v, i, i * i), v), i + 1, n)

fn sum(v: Vector<Int>, i: Int, n: Int, acc: Int) -> Int
    match i >= n
        true  -> acc
        false -> sum(v, i + 1, n, acc + Option.withDefault(Vector.get(v, i), 0))

fn main()
    ! [Console.print]
    base = Vector.fromList([0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
    final = write(base, 0, 10)
    Console.print(Int.toString(sum(final, 0, 10, 0)))
"#,
    );

    let output = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&module_path)
        .arg("--wasm")
        .output()
        .expect("expected patch-chain `aver run --wasm` to execute");

    assert!(
        output.status.success(),
        "patch-chain WASM run failed:\n{}",
        format_output(&output)
    );
    // 0² + 1² + … + 9² = 285
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "285",
        "patch-chain WASM produced wrong sum:\n{}",
        format_output(&output)
    );

    let _ = fs::remove_dir_all(module_path.parent().expect("temp module dir"));
}

/// Drive the boundary GC across patch vectors. The TCO loop walks
/// well past the 16 KiB compaction threshold, every iteration
/// allocates a 32-byte patch on top of a long chain. If the GC
/// mishandles the patch layout (wrong size step, walking flat
/// elements that aren't there, missing the base/value pointers),
/// the survivor copy gets corrupted and the final read diverges.
#[test]
fn wasm_run_patch_vector_survives_gc_compaction() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    // 1500 iterations × 32-byte patch ≈ 48 KiB of patch nodes alone,
    // forcing multiple compaction passes through the patch chain.
    let module_path = write_temp_module(
        "aver-wasm-patch-gc",
        r#"module Tmp

fn build(v: Vector<Int>, i: Int, n: Int) -> Vector<Int>
    match i >= n
        true  -> v
        false -> build(Option.withDefault(Vector.set(v, 0, i), v), i + 1, n)

fn main()
    ! [Console.print]
    base = Vector.fromList([0])
    final = build(base, 0, 1500)
    Console.print(Int.toString(Option.withDefault(Vector.get(final, 0), -1)))
"#,
    );

    let output = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&module_path)
        .arg("--wasm")
        .output()
        .expect("expected patch-gc `aver run --wasm` to execute");

    assert!(
        output.status.success(),
        "patch-gc WASM run failed:\n{}",
        format_output(&output)
    );
    // Last write is `Vector.set(_, 0, 1499)`, so final read must be 1499.
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        "1499",
        "patch-gc WASM survived compaction with wrong value:\n{}",
        format_output(&output)
    );

    let _ = fs::remove_dir_all(module_path.parent().expect("temp module dir"));
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
        .arg("--optimize")
        .arg("size")
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
fn wasm_compile_validates_int_map_keys_successfully() {
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
        output.status.success(),
        "expected Map<Int, V> WASM compile to pass validation:\n{}",
        format_output(&output)
    );
    assert!(
        output_dir.join("main.wasm").exists(),
        "expected main.wasm to exist when validation passes"
    );

    let _ = fs::remove_dir_all(module_path.parent().expect("temp module dir"));
    let _ = fs::remove_dir_all(&output_dir);
}

#[test]
fn wasm_compile_validates_float_map_keys_successfully() {
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
        output.status.success(),
        "expected Map<Float, V> WASM compile to pass validation:\n{}",
        format_output(&output)
    );
    assert!(
        output_dir.join("main.wasm").exists(),
        "expected main.wasm to exist when validation passes"
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
        .arg("wasm")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver compile --target wasm` to run");

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

#[test]
fn wasi_bundled_runs_under_wasmtime_standalone() {
    if !wasmtime_available() {
        eprintln!("skipping WASI bundled smoke test: wasmtime not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let module_path = write_temp_module(
        "aver-wasi-bundled",
        r#"module Tmp

fn main()
    ! [Console.print]
    n = 42
    Console.print("Number: {n}")
"#,
    );
    let output_dir = temp_output_dir("aver-wasi-bundled-out");

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&module_path)
        .arg("--target")
        .arg("wasm")
        .arg("--bridge")
        .arg("wasip1")
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("expected `aver compile --target wasm --bridge wasip1` to run");
    assert!(
        compile.status.success(),
        "WASI bundled compile failed:\n{}",
        format_output(&compile)
    );
    let wasm_file = output_dir.join("main.wasm");
    assert!(wasm_file.exists(), "expected main.wasm to be emitted");

    let run = Command::new("wasmtime")
        .arg(&wasm_file)
        .output()
        .expect("expected wasmtime to run");
    assert!(
        run.status.success(),
        "wasmtime run failed:\n{}",
        format_output(&run)
    );
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout.trim(),
        "Number: 42",
        "wasi-bundled stdout mismatch:\n{}",
        format_output(&run)
    );
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        !stderr.contains("--invoke"),
        "wasmtime should treat _start as a wasi command, got stderr:\n{}",
        stderr
    );

    let _ = fs::remove_dir_all(module_path.parent().expect("temp module dir"));
    let _ = fs::remove_dir_all(&output_dir);
}
