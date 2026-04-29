//! Cross-backend memory + GC stress harness.
//!
//! Each stress program is run on every production backend — VM,
//! WASM (built-in wasmtime), and self-host (Aver-in-Aver-in-Rust)
//! — and every backend must produce the *exact* same stdout. A
//! bug that drops a survivor across compaction, miscounts buckets,
//! corrupts a string key, or breaks owned-mutate dispatch shows
//! up as a backend mismatch the moment it surfaces.
//!
//! Iteration counts are tuned so the slowest backend (self-host,
//! ~75-100× slower than WASM) still completes each test under a
//! few hundred milliseconds.
#![cfg(feature = "wasm")]

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("{}-{}", prefix, nanos));
    fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    fs::write(&path, source).expect("write temp module source");
    path
}

fn cleanup(path: &std::path::Path) {
    let _ = fs::remove_dir_all(path.parent().expect("temp module has parent"));
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

fn run_vm(prefix: &str, source: &str) -> String {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&path)
        .output()
        .expect("expected `aver run` (VM) to execute");
    cleanup(&path);
    assert!(
        out.status.success(),
        "{} VM run failed:\n{}",
        prefix,
        format_output(&out)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

fn run_wasm(prefix: &str, source: &str) -> String {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&path)
        .arg("--wasm")
        .output()
        .expect("expected `aver run --wasm` to execute");
    cleanup(&path);
    assert!(
        out.status.success(),
        "{} WASM run failed:\n{}",
        prefix,
        format_output(&out)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

fn run_self_host(prefix: &str, source: &str) -> String {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module(prefix, source);
    let module_root = path.parent().expect("temp module has parent");
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&path)
        .arg("--module-root")
        .arg(module_root)
        .arg("--self-host")
        .output()
        .expect("expected `aver run --self-host` to execute");
    cleanup(&path);
    assert!(
        out.status.success(),
        "{} self-host run failed:\n{}",
        prefix,
        format_output(&out)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

// ─── Stress sources ────────────────────────────────────────────────────

/// 1000 sets on the same key. Count must collapse to 1, last value
/// wins. Backend that mishandles "key already present" probe path
/// either grows the count or loses the latest write.
const OVERWRITE_SRC: &str = r#"module Tmp

fn loop(m: Map<String, Int>, i: Int, n: Int) -> Map<String, Int>
    match i >= n
        true  -> m
        false -> loop(Map.set(m, "k", i), i + 1, n)

fn main()
    ! [Console.print]
    m = loop(Map.empty(), 0, 1000)
    Console.print(Int.toString(List.len(Map.entries(m))))
    Console.print(Int.toString(Option.withDefault(Map.get(m, "k"), -1)))
"#;
const OVERWRITE_OUT: &str = "1\n999";

/// 5 000 unique String keys + sum-of-values round-trip. Drives
/// multiple capacity doublings in flat-hashtable backends and
/// many compactions across all backends.
const UNIQUE_KEYS_SRC: &str = r#"module Tmp

fn build(m: Map<String, Int>, i: Int, n: Int) -> Map<String, Int>
    match i >= n
        true  -> m
        false -> build(Map.set(m, Int.toString(i), i), i + 1, n)

fn sum(m: Map<String, Int>, i: Int, n: Int, acc: Int) -> Int
    match i >= n
        true  -> acc
        false -> sum(m, i + 1, n, acc + Option.withDefault(Map.get(m, Int.toString(i)), -1))

fn main()
    ! [Console.print]
    m = build(Map.empty(), 0, 5000)
    Console.print(Int.toString(List.len(Map.entries(m))))
    Console.print(Int.toString(sum(m, 0, 5000, 0)))
"#;
// sum 0..4999 = 4999 * 5000 / 2 = 12 497 500
const UNIQUE_KEYS_OUT: &str = "5000\n12497500";

/// 3 000-iteration TCO loop writing into a 3 000-element flat
/// Vector. All-cells round-trip — sum of i+1 for i in 0..3000.
const VECTOR_TCO_SRC: &str = r#"module Tmp

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
    v = fill(Vector.new(3000, 0), 0, 3000)
    Console.print(Int.toString(sum(v, 0, 3000, 0)))
"#;
// sum 1..3000 = 3000 * 3001 / 2 = 4 501 500
const VECTOR_TCO_OUT: &str = "4501500";

/// Map and Vector mutated in lockstep inside one TCO frame —
/// catches GC bugs that handle one collection kind correctly and
/// the other not.
const MIXED_SRC: &str = r#"module Tmp

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
    a = step(Acc(m = Map.empty(), v = Vector.new(2000, 0)), 0, 2000)
    Console.print(Int.toString(List.len(Map.entries(a.m))))
    Console.print(Int.toString(Option.withDefault(Map.get(a.m, 1500), -1)))
    Console.print(Int.toString(Option.withDefault(Vector.get(a.v, 1500), -1)))
"#;
const MIXED_OUT: &str = "2000\n3000\n4500";

/// Nested data structure: record -> Map -> List -> record. Reaches
/// every reference-bearing heap kind. After compaction every
/// pointer must still resolve.
const NESTED_SRC: &str = r#"module Tmp

record Pair
    a: String
    b: Int

fn buildList(acc: List<Pair>, i: Int, n: Int) -> List<Pair>
    match i >= n
        true  -> acc
        false -> buildList(List.prepend(Pair(a = Int.toString(i), b = i), acc), i + 1, n)

fn buildMap(m: Map<Int, List<Pair>>, i: Int, n: Int) -> Map<Int, List<Pair>>
    match i >= n
        true  -> m
        false -> buildMap(Map.set(m, i, buildList([], 0, 10)), i + 1, n)

fn main()
    ! [Console.print]
    m = buildMap(Map.empty(), 0, 500)
    Console.print(Int.toString(List.len(Map.entries(m))))
    Console.print(Int.toString(List.len(Option.withDefault(Map.get(m, 250), []))))
"#;
const NESTED_OUT: &str = "500\n10";

/// 8 000 unique Int keys. WASM hashtable hits capacity doublings
/// 8 → 16 → 32 → … → 16384 (~11 resizes). Every backend must
/// rebuild buckets across resize without losing entries.
const RESIZE_SRC: &str = r#"module Tmp

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
    m = fill(Map.empty(), 0, 8000)
    Console.print(Int.toString(List.len(Map.entries(m))))
    Console.print(Int.toString(check(m, 0, 8000, 0)))
"#;
// sum 0..7999 = 7999 * 8000 / 2 = 31 996 000
const RESIZE_OUT: &str = "8000\n31996000";

// ─── Per-backend cross-checks ──────────────────────────────────────────
//
// One #[test] fn per (source × backend). Verbose vs a paste-macro
// expansion, but keeps the test list grep-able and avoids pulling in
// a proc-macro dep just for ident concatenation.

fn assert_eq_with_label(label: &str, actual: &str, expected: &str) {
    assert_eq!(
        actual, expected,
        "{} diverged: got\n{}\nwant\n{}",
        label, actual, expected
    );
}

#[test]
fn cross_overwrite_same_key_vm() {
    assert_eq_with_label(
        "VM",
        &run_vm("aver-cross-overwrite-vm", OVERWRITE_SRC),
        OVERWRITE_OUT,
    );
}
#[test]
fn cross_overwrite_same_key_wasm() {
    assert_eq_with_label(
        "WASM",
        &run_wasm("aver-cross-overwrite-wasm", OVERWRITE_SRC),
        OVERWRITE_OUT,
    );
}
#[test]
fn cross_overwrite_same_key_self_host() {
    assert_eq_with_label(
        "self-host",
        &run_self_host("aver-cross-overwrite-sh", OVERWRITE_SRC),
        OVERWRITE_OUT,
    );
}

#[test]
fn cross_map_unique_keys_5k_vm() {
    assert_eq_with_label(
        "VM",
        &run_vm("aver-cross-uniq5k-vm", UNIQUE_KEYS_SRC),
        UNIQUE_KEYS_OUT,
    );
}
#[test]
fn cross_map_unique_keys_5k_wasm() {
    assert_eq_with_label(
        "WASM",
        &run_wasm("aver-cross-uniq5k-wasm", UNIQUE_KEYS_SRC),
        UNIQUE_KEYS_OUT,
    );
}
#[test]
fn cross_map_unique_keys_5k_self_host() {
    assert_eq_with_label(
        "self-host",
        &run_self_host("aver-cross-uniq5k-sh", UNIQUE_KEYS_SRC),
        UNIQUE_KEYS_OUT,
    );
}

#[test]
fn cross_vector_tco_3k_vm() {
    assert_eq_with_label(
        "VM",
        &run_vm("aver-cross-vec3k-vm", VECTOR_TCO_SRC),
        VECTOR_TCO_OUT,
    );
}
#[test]
fn cross_vector_tco_3k_wasm() {
    assert_eq_with_label(
        "WASM",
        &run_wasm("aver-cross-vec3k-wasm", VECTOR_TCO_SRC),
        VECTOR_TCO_OUT,
    );
}
#[test]
fn cross_vector_tco_3k_self_host() {
    assert_eq_with_label(
        "self-host",
        &run_self_host("aver-cross-vec3k-sh", VECTOR_TCO_SRC),
        VECTOR_TCO_OUT,
    );
}

#[test]
fn cross_mixed_map_vector_tco_vm() {
    assert_eq_with_label("VM", &run_vm("aver-cross-mixed-vm", MIXED_SRC), MIXED_OUT);
}
#[test]
fn cross_mixed_map_vector_tco_wasm() {
    assert_eq_with_label(
        "WASM",
        &run_wasm("aver-cross-mixed-wasm", MIXED_SRC),
        MIXED_OUT,
    );
}
#[test]
fn cross_mixed_map_vector_tco_self_host() {
    assert_eq_with_label(
        "self-host",
        &run_self_host("aver-cross-mixed-sh", MIXED_SRC),
        MIXED_OUT,
    );
}

#[test]
fn cross_nested_record_map_list_vm() {
    assert_eq_with_label(
        "VM",
        &run_vm("aver-cross-nested-vm", NESTED_SRC),
        NESTED_OUT,
    );
}
#[test]
fn cross_nested_record_map_list_wasm() {
    assert_eq_with_label(
        "WASM",
        &run_wasm("aver-cross-nested-wasm", NESTED_SRC),
        NESTED_OUT,
    );
}
#[test]
fn cross_nested_record_map_list_self_host() {
    assert_eq_with_label(
        "self-host",
        &run_self_host("aver-cross-nested-sh", NESTED_SRC),
        NESTED_OUT,
    );
}

#[test]
fn cross_map_resize_8k_vm() {
    assert_eq_with_label(
        "VM",
        &run_vm("aver-cross-resize8k-vm", RESIZE_SRC),
        RESIZE_OUT,
    );
}
#[test]
fn cross_map_resize_8k_wasm() {
    assert_eq_with_label(
        "WASM",
        &run_wasm("aver-cross-resize8k-wasm", RESIZE_SRC),
        RESIZE_OUT,
    );
}
#[test]
fn cross_map_resize_8k_self_host() {
    assert_eq_with_label(
        "self-host",
        &run_self_host("aver-cross-resize8k-sh", RESIZE_SRC),
        RESIZE_OUT,
    );
}
