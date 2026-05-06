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
        false -> build(n - 1, Map.set(acc, String.fromInt(n), n))

fn main()
    ! [Console.print]
    m = build(12000, {})
    Console.print("Entries: {String.fromInt(List.len(Map.entries(m)))}")
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
    m = Map.set({}, 1, "one")
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
    m = Map.set({}, 1.5, "one-and-a-half")
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
    m = Map.set({}, "k", 1)
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

// ─── Memory & GC stress suite ──────────────────────────────────────────
//
// Each of these compiles a small Aver program, runs it under
// `aver run --wasm` (built-in wasmtime host), and checks stdout.
// The shape is "do enough work that the boundary GC fires + the
// runtime allocates+rebases survivors", then assert the program
// produces the *exact* expected output. A corrupted heap or
// dropped survivor will diverge silently otherwise.

fn run_wasm_and_collect(prefix: &str, source: &str) -> String {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let module_path = write_temp_module(prefix, source);
    let output = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&module_path)
        .arg("--wasm")
        .output()
        .expect("expected `aver run --wasm` to execute");
    let _ = fs::remove_dir_all(module_path.parent().expect("temp module dir"));
    assert!(
        output.status.success(),
        "{} WASM run failed:\n{}",
        prefix,
        format_output(&output)
    );
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

/// Map.set on the same key 1000 times with distinct values must leave
/// the map at length 1 carrying the last write. A broken probe / wrong
/// owned-mutate / GC tombstone bug would either grow the count or lose
/// the latest value.
#[test]
fn wasm_run_map_overwrite_same_key_keeps_count_one() {
    let stdout = run_wasm_and_collect(
        "aver-wasm-stress-map-overwrite",
        r#"module Tmp

fn loop(m: Map<String, Int>, i: Int, n: Int) -> Map<String, Int>
    match i >= n
        true  -> m
        false -> loop(Map.set(m, "k", i), i + 1, n)

fn main()
    ! [Console.print]
    m = loop({}, 0, 1000)
    Console.print(String.fromInt(List.len(Map.entries(m))))
    Console.print(String.fromInt(Option.withDefault(Map.get(m, "k"), -1)))
"#,
    );
    assert_eq!(stdout, "1\n999", "overwrite stress diverged:\n{}", stdout);
}

/// Build a 20 000-entry Map<String, Int> via TCO, then read every
/// single key back. Forces ~12 capacity doublings (8 → 16384) and
/// many GC compactions because the heap grows past 16 KiB watermark.
/// Asserts: count == 20000 and the sum-of-values is the expected
/// closed-form result, so any silently-dropped entry shifts the sum.
#[test]
fn wasm_run_map_twenty_thousand_unique_keys_round_trip() {
    let stdout = run_wasm_and_collect(
        "aver-wasm-stress-map-20k",
        r#"module Tmp

fn build(m: Map<String, Int>, i: Int, n: Int) -> Map<String, Int>
    match i >= n
        true  -> m
        false -> build(Map.set(m, String.fromInt(i), i), i + 1, n)

fn sum(m: Map<String, Int>, i: Int, n: Int, acc: Int) -> Int
    match i >= n
        true  -> acc
        false -> sum(m, i + 1, n, acc + Option.withDefault(Map.get(m, String.fromInt(i)), -1))

fn main()
    ! [Console.print]
    m = build({}, 0, 20000)
    Console.print(String.fromInt(List.len(Map.entries(m))))
    Console.print(String.fromInt(sum(m, 0, 20000, 0)))
"#,
    );
    // sum 0..19999 = 19999 * 20000 / 2 = 199990000
    assert_eq!(
        stdout, "20000\n199990000",
        "20k round-trip diverged:\n{}",
        stdout
    );
}

/// Vector.set in a 10 000-iteration TCO loop with a 10 000-element
/// flat vector. Owned-mutate fast path emits inline `i64.store`; the
/// boundary GC must not be confused by the large flat allocation
/// living across many iterations. Asserts that every cell holds the
/// value we wrote.
#[test]
fn wasm_run_vector_ten_thousand_in_place_writes_round_trip() {
    let stdout = run_wasm_and_collect(
        "aver-wasm-stress-vec-10k",
        r#"module Tmp

fn fill(v: Vector<Int>, i: Int, n: Int) -> Vector<Int>
    match i >= n
        true  -> v
        false -> fill(Option.withDefault(Vector.set(v, i, i + 1), v), i + 1, n)

fn sum(v: Vector<Int>, i: Int, n: Int, acc: Int) -> Int
    match i >= n
        true  -> acc
        false -> sum(v, i + 1, n, acc + Option.withDefault(Vector.get(v, i), 0))

fn main()
    ! [Console.print]
    v = fill(Vector.new(10000, 0), 0, 10000)
    Console.print(String.fromInt(sum(v, 0, 10000, 0)))
"#,
    );
    // sum 1..10000 = 10000 * 10001 / 2 = 50005000
    assert_eq!(stdout, "50005000", "vec stress diverged:\n{}", stdout);
}

/// Two collections mutated in lockstep inside a single TCO frame —
/// catches GC bugs where compaction handles one kind correctly and
/// the other not. Both Map and Vector see owned-mutate dispatch.
#[test]
fn wasm_run_mixed_map_vector_lockstep_in_tco() {
    let stdout = run_wasm_and_collect(
        "aver-wasm-stress-mixed-mv",
        r#"module Tmp

record Acc
    m: Map<Int, Int>
    v: Vector<Int>

fn step(a: Acc, i: Int, n: Int) -> Acc
    match i >= n
        true  -> a
        false -> step(
            Acc(
                m = Map.set(a.m, i, i * 2),
                v = Option.withDefault(Vector.set(a.v, i, i * 3), a.v),
            ),
            i + 1,
            n,
        )

fn main()
    ! [Console.print]
    a = step(Acc(m = {}, v = Vector.new(2000, 0)), 0, 2000)
    Console.print(String.fromInt(List.len(Map.entries(a.m))))
    Console.print(String.fromInt(Option.withDefault(Map.get(a.m, 1500), -1)))
    Console.print(String.fromInt(Option.withDefault(Vector.get(a.v, 1500), -1)))
"#,
    );
    assert_eq!(
        stdout, "2000\n3000\n4500",
        "mixed M+V stress diverged:\n{}",
        stdout
    );
}

/// String keys carried across many compactions. Because strings live
/// on the heap, GC must rebase each key cell when it walks an
/// occupied bucket — wrong key_kind detection, missing meta byte, or
/// a stale offset would corrupt the key and the lookup would miss.
#[test]
fn wasm_run_string_key_map_survives_compaction() {
    let stdout = run_wasm_and_collect(
        "aver-wasm-stress-strkey-gc",
        r#"module Tmp

fn build(m: Map<String, Int>, i: Int, n: Int) -> Map<String, Int>
    match i >= n
        true  -> m
        false -> build(Map.set(m, "key-" + String.fromInt(i), i), i + 1, n)

fn main()
    ! [Console.print]
    m = build({}, 0, 5000)
    Console.print(String.fromInt(Option.withDefault(Map.get(m, "key-0"), -1)))
    Console.print(String.fromInt(Option.withDefault(Map.get(m, "key-2500"), -1)))
    Console.print(String.fromInt(Option.withDefault(Map.get(m, "key-4999"), -1)))
    Console.print(String.fromInt(Option.withDefault(Map.get(m, "missing"), -1)))
"#,
    );
    assert_eq!(
        stdout, "0\n2500\n4999\n-1",
        "string-key stress diverged:\n{}",
        stdout
    );
}

/// Many short-lived string allocations inside a TCO loop while a
/// long-lived string is preserved across iterations. The short-lived
/// ones become garbage every iter; compaction must not mistakenly
/// reclaim the long-lived one. We read the long string back at the
/// end to confirm it's still intact.
#[test]
fn wasm_run_garbage_strings_do_not_corrupt_survivors() {
    let stdout = run_wasm_and_collect(
        "aver-wasm-stress-gc-strings",
        r#"module Tmp

fn churn(keep: String, i: Int, n: Int) -> String
    match i >= n
        true  -> keep
        false -> churn(keep, i + 1, n)

fn main()
    ! [Console.print]
    survivor = "I should still be here at the end"
    final = churn(survivor, 0, 50000)
    Console.print(final)
"#,
    );
    assert_eq!(
        stdout, "I should still be here at the end",
        "long-lived string corrupted:\n{}",
        stdout
    );
}

/// A nested data structure (record holds Map holds list of records)
/// reaches every kind of pointer-ish heap object the runtime
/// supports. After a forced compaction the structure must read back
/// identically — any wrong pointer rebase blows up either a load or
/// a length lookup.
#[test]
fn wasm_run_deeply_nested_record_map_list_round_trips() {
    let stdout = run_wasm_and_collect(
        "aver-wasm-stress-nested",
        r#"module Tmp

record Pair
    a: String
    b: Int

fn buildList(acc: List<Pair>, i: Int, n: Int) -> List<Pair>
    match i >= n
        true  -> acc
        false -> buildList(List.prepend(Pair(a = String.fromInt(i), b = i), acc), i + 1, n)

fn buildMap(m: Map<Int, List<Pair>>, i: Int, n: Int) -> Map<Int, List<Pair>>
    match i >= n
        true  -> m
        false -> buildMap(Map.set(m, i, buildList([], 0, 10)), i + 1, n)

fn main()
    ! [Console.print]
    m = buildMap({}, 0, 500)
    Console.print(String.fromInt(List.len(Map.entries(m))))
    Console.print(String.fromInt(List.len(Option.withDefault(Map.get(m, 250), []))))
"#,
    );
    assert_eq!(
        stdout, "500\n10",
        "nested-record stress diverged:\n{}",
        stdout
    );
}

/// Force at least 13 capacity doublings (8 → 16 → … → 32768) by
/// inserting a tight power-of-two boundary. This drives the resize
/// path repeatedly, so the rehash + bucket rebuild must not lose
/// any entry. Tombstones never come into play — we only insert.
#[test]
fn wasm_run_map_resize_through_thirteen_doublings() {
    let stdout = run_wasm_and_collect(
        "aver-wasm-stress-resize",
        r#"module Tmp

fn fill(m: Map<Int, Int>, i: Int, n: Int) -> Map<Int, Int>
    match i >= n
        true  -> m
        false -> fill(Map.set(m, i, i), i + 1, n)

fn check(m: Map<Int, Int>, i: Int, n: Int, acc: Int) -> Int
    match i >= n
        true  -> acc
        false -> check(m, i + 1, n, acc + Option.withDefault(Map.get(m, i), -1))

fn main()
    ! [Console.print]
    m = fill({}, 0, 25000)
    Console.print(String.fromInt(List.len(Map.entries(m))))
    Console.print(String.fromInt(check(m, 0, 25000, 0)))
"#,
    );
    // Sum 0..24999 = 24999*25000/2 = 312487500
    assert_eq!(
        stdout, "25000\n312487500",
        "resize stress diverged:\n{}",
        stdout
    );
}
