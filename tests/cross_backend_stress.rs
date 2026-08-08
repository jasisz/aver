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

fn run_wasm_gc(prefix: &str, source: &str) -> String {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&path)
        .arg("--wasm-gc")
        .output()
        .expect("expected `aver run --wasm-gc` to execute");
    cleanup(&path);
    assert!(
        out.status.success(),
        "{} wasm-gc run failed:\n{}",
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
    m = loop({}, 0, 1000)
    Console.print(String.fromInt(List.len(Map.entries(m))))
    Console.print(String.fromInt(Option.withDefault(Map.get(m, "k"), -1)))
"#;
const OVERWRITE_OUT: &str = "1\n999";

/// 5 000 unique String keys + sum-of-values round-trip. Drives
/// multiple capacity doublings in flat-hashtable backends and
/// many compactions across all backends.
const UNIQUE_KEYS_SRC: &str = r#"module Tmp

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
    m = build({}, 0, 5000)
    Console.print(String.fromInt(List.len(Map.entries(m))))
    Console.print(String.fromInt(sum(m, 0, 5000, 0)))
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
    Console.print(String.fromInt(sum(v, 0, 3000, 0)))
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
    a = step(Acc(m = {}, v = Vector.new(2000, 0)), 0, 2000)
    Console.print(String.fromInt(List.len(Map.entries(a.m))))
    Console.print(String.fromInt(Option.withDefault(Map.get(a.m, 1500), -1)))
    Console.print(String.fromInt(Option.withDefault(Vector.get(a.v, 1500), -1)))
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
    m = fill({}, 0, 8000)
    Console.print(String.fromInt(List.len(Map.entries(m))))
    Console.print(String.fromInt(check(m, 0, 8000, 0)))
"#;
// sum 0..7999 = 7999 * 8000 / 2 = 31 996 000
const RESIZE_OUT: &str = "8000\n31996000";

/// Vector → List bridge via `List.fromVector`. Builds a 2 000-cell
/// Vector under TCO, converts to a List, sums via list TCO. Catches
/// backends that miscount the vector→list element copy or break the
/// resulting list's `[]` empty-cons identity.
const LIST_FROM_VECTOR_SRC: &str = r#"module Tmp

fn fill(v: Vector<Int>, i: Int, n: Int) -> Vector<Int>
    match i >= n
        true  -> v
        false -> fill(Option.withDefault(Vector.set(v, i, i + 1), v), i + 1, n)

fn sumList(xs: List<Int>, acc: Int) -> Int
    match xs
        [] -> acc
        [x, ..rest] -> sumList(rest, acc + x)

fn main()
    ! [Console.print]
    v = fill(Vector.new(2000, 0), 0, 2000)
    xs = List.fromVector(v)
    Console.print(String.fromInt(List.len(xs)))
    Console.print(String.fromInt(sumList(xs, 0)))
"#;
// len = 2000, sum 1..2000 = 2000 * 2001 / 2 = 2 001 000
const LIST_FROM_VECTOR_OUT: &str = "2000\n2001000";

/// Pin the alias-corruption invariant: `outer = Vector.new(3, inner)`
/// must NOT make the inner storage of `outer[0]` share an arena cell
/// with `inner` such that mutating one bleeds into the other. The fused
/// `Option.withDefault(Vector.set(row0, 1, 9), row0)` only touches a
/// fresh local; both `row0` (read out of `outer`) and `inner` must
/// remain `0 0 0`. A backend that takes the in-place fast path on
/// `row0` without proving uniqueness produces `0 9 0` on either or
/// both rows. See conversation 2026-05-07: three rounds of consultation
/// landed on conservative aliasing as the right 0.17.x posture.
const VECTOR_ALIASING_SRC: &str = r#"module Tmp

fn cellStr(v: Vector<Int>, i: Int) -> String
    String.fromInt(Option.withDefault(Vector.get(v, i), -1))

fn show(v: Vector<Int>) -> String
    cellStr(v, 0) + cellStr(v, 1) + cellStr(v, 2)

fn main()
    ! [Console.print]
    inner = Vector.new(3, 0)
    outer = Vector.new(3, inner)
    row0 = Option.withDefault(Vector.get(outer, 0), Vector.new(0, 0))
    _ = Option.withDefault(Vector.set(row0, 1, 9), row0)
    Console.print(show(row0))
    Console.print(show(inner))
"#;
const VECTOR_ALIASING_OUT: &str = "000\n000";

/// Literal-divisor discharge, executed end to end. `Int.div(a, K)` /
/// `Int.mod(a, K)` with a syntactic nonzero integer literal `K` types as
/// plain `Int`, so every backend must return a bare integer — including
/// the Aver-in-Aver interpreter behind `--self-host`, whose own resolver
/// has to recognise the same shape. Without that rule the self-hosted
/// pipeline keeps building a guest `Result`, and a program that is
/// well-typed on the host silently prints `Result.Ok(4)` where the VM
/// prints `4` (or dies later on an arithmetic type mismatch) — the exact
/// divergence this pin exists to catch.
///
/// The matrix covers both dividend signs and zero, a negative literal
/// divisor (`-3`), and `mixed`, which puts a discharged divisor and a
/// dynamic-divisor `Result` call in the same expression. Every value is
/// Euclidean: the remainder is always in `[0, |K|)`.
const LITERAL_DIVISOR_SRC: &str = r#"module Tmp

fn half(a: Int) -> Int
    Int.div(a, 2)

fn parity(a: Int) -> Int
    Int.mod(a, 2)

fn negDiv(a: Int) -> Int
    Int.div(a, -3)

fn negMod(a: Int) -> Int
    Int.mod(a, -3)

fn dynDiv(a: Int, b: Int) -> Int
    match Int.div(a, b)
        Result.Ok(q) -> q
        Result.Err(_) -> 0 - 424242

fn mixed(a: Int) -> Int
    Int.div(a, 4) + dynDiv(a, 4)

fn row(a: Int) -> String
    String.fromInt(half(a)) + " " + String.fromInt(parity(a)) + " " + String.fromInt(negDiv(a)) + " " + String.fromInt(negMod(a)) + " " + String.fromInt(mixed(a))

fn main()
    ! [Console.print]
    Console.print(row(9))
    Console.print(row(0 - 9))
    Console.print(row(0))
    Console.print(row(0 - 8))
    Console.print(String.fromInt(half(9) + parity(9)))
"#;
// Columns: div 2 | mod 2 | div -3 | mod -3 | div 4 + dynamic div 4.
const LITERAL_DIVISOR_OUT: &str = "4 1 -3 0 4\n-5 1 3 0 -6\n0 0 0 0 0\n-4 0 3 1 -4\n5";

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
fn cross_map_resize_8k_self_host() {
    assert_eq_with_label(
        "self-host",
        &run_self_host("aver-cross-resize8k-sh", RESIZE_SRC),
        RESIZE_OUT,
    );
}

#[test]
fn cross_list_from_vector_2k_vm() {
    assert_eq_with_label(
        "VM",
        &run_vm("aver-cross-lfv2k-vm", LIST_FROM_VECTOR_SRC),
        LIST_FROM_VECTOR_OUT,
    );
}
#[test]
fn cross_list_from_vector_2k_self_host() {
    assert_eq_with_label(
        "self-host",
        &run_self_host("aver-cross-lfv2k-sh", LIST_FROM_VECTOR_SRC),
        LIST_FROM_VECTOR_OUT,
    );
}

#[test]
fn cross_vector_aliasing_pin_vm() {
    assert_eq_with_label(
        "VM",
        &run_vm("aver-cross-alias-vm", VECTOR_ALIASING_SRC),
        VECTOR_ALIASING_OUT,
    );
}
#[test]
fn cross_vector_aliasing_pin_wasm_gc() {
    assert_eq_with_label(
        "wasm-gc",
        &run_wasm_gc("aver-cross-alias-wasmgc", VECTOR_ALIASING_SRC),
        VECTOR_ALIASING_OUT,
    );
}

#[test]
fn cross_literal_divisor_discharge_vm() {
    assert_eq_with_label(
        "VM",
        &run_vm("aver-cross-litdiv-vm", LITERAL_DIVISOR_SRC),
        LITERAL_DIVISOR_OUT,
    );
}
#[test]
fn cross_literal_divisor_discharge_wasm_gc() {
    assert_eq_with_label(
        "wasm-gc",
        &run_wasm_gc("aver-cross-litdiv-wasmgc", LITERAL_DIVISOR_SRC),
        LITERAL_DIVISOR_OUT,
    );
}
#[test]
fn cross_literal_divisor_discharge_self_host() {
    assert_eq_with_label(
        "self-host",
        &run_self_host("aver-cross-litdiv-sh", LITERAL_DIVISOR_SRC),
        LITERAL_DIVISOR_OUT,
    );
}

/// Literal smart-constructor discharge, executed end to end.
/// `Local.fromList([1, 2, 3])` types as the refined type — the elements are
/// literals inside the interval the refinement's own predicate proves — so
/// every backend must produce the carrier directly, with no `Result` to
/// unwrap. `dynamic` keeps the fallible path in the same program, so a
/// backend that discharged too much or too little diverges here.
const LITERAL_REFINEMENT_SRC: &str = r#"module Local

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")

fn count(o: Octets) -> Int
    List.len(o.values)

fn first(o: Octets) -> Int
    match o.values
        [head, .._] -> head
        [] -> 0 - 1

fn dynamic(xs: List<Int>) -> Int
    match fromList(xs)
        Result.Ok(o) -> count(o)
        Result.Err(_) -> 0 - 2

fn main()
    ! [Console.print]
    Console.print(String.fromInt(count(Local.fromList([1, 2, 3]))))
    Console.print(String.fromInt(first(Local.fromList([200, 0]))))
    Console.print(String.fromInt(count(Local.fromList([]))))
    Console.print(String.fromInt(dynamic([1, 2])))
    Console.print(String.fromInt(dynamic([256])))
"#;
const LITERAL_REFINEMENT_OUT: &str = "3\n200\n0\n2\n-2";

#[test]
fn cross_literal_refinement_discharge_vm() {
    assert_eq_with_label(
        "VM",
        &run_vm("aver-cross-litref-vm", LITERAL_REFINEMENT_SRC),
        LITERAL_REFINEMENT_OUT,
    );
}

#[test]
fn cross_literal_refinement_discharge_wasm_gc() {
    assert_eq_with_label(
        "wasm-gc",
        &run_wasm_gc("aver-cross-litref-wasmgc", LITERAL_REFINEMENT_SRC),
        LITERAL_REFINEMENT_OUT,
    );
}

#[test]
fn cross_literal_refinement_discharge_rejected_by_self_host() {
    // The self-hosted resolver has no refinement recognizer — no type
    // defs, no dependency-module ASTs, no interval derivation — so it
    // cannot mirror this rule the way it mirrors the purely syntactic
    // literal-divisor one. Doing nothing would NOT be neutral: the guest
    // would keep building a `Result` where the host-checked source expects
    // the refined type, and the program would fail far from the cause (the
    // pre-gate symptom was a bare `field access on non-record`). The
    // boundary is therefore a loud, specific refusal that names the call
    // sites, and this test is what keeps it loud.
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module("aver-cross-litref-sh", LITERAL_REFINEMENT_SRC);
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
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !out.status.success(),
        "self-host must REFUSE a discharged program, not run it:\n{combined}"
    );
    assert!(
        combined.contains("does not support the literal smart-constructor discharge"),
        "self-host refusal must name the rule:\n{combined}"
    );
    assert!(
        combined.contains("Local.fromList"),
        "self-host refusal must name the offending call sites:\n{combined}"
    );
}

// ─── Verify cross-target ────────────────────────────────────────────────
//
// `aver verify` and `aver verify --wasm-gc` evaluate the same verify
// blocks via two different backends. They must agree on pass/fail —
// codegen divergence between VM and wasm-gc would surface as one
// reporting OK and the other not. The wasm-gc executor synthesizes a
// `__verify_X_check() -> Bool` helper per case that runs `lhs == rhs`
// inside wasm; the host only decodes a single i32 per case.

const VERIFY_PASS_SRC: &str = r#"module Tmp
    effects []

fn add(a: Int, b: Int) -> Int
    a + b

verify add
    add(2, 3) => 5
    add(0, 0) => 0
    add(0 - 1, 1) => 0

fn classify(n: Int) -> String
    match n
        0 -> "zero"
        _ -> match n > 0
            true -> "positive"
            false -> "negative"

verify classify
    classify(0) => "zero"
    classify(7) => "positive"
    classify(0 - 3) => "negative"

fn main() -> Int
    add(1, 2)
"#;

const VERIFY_FAIL_SRC: &str = r#"module Tmp
    effects []

fn buggy(a: Int) -> Int
    a + 1

verify buggy
    buggy(0) => 0
    buggy(5) => 6
    buggy(10) => 100

fn main() -> Int
    buggy(0)
"#;

/// Like `run_verify_summary` but returns the entire trimmed stdout
/// (not just the Summary line) so tests can assert on rendered
/// expected/actual values inside mismatch diagnostics.
fn run_verify_full_stdout(prefix: &str, source: &str, wasm_gc: bool) -> String {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module(prefix, source);
    let module_root = path.parent().expect("temp module has parent");
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root)
        .arg("verify")
        .arg(&path)
        .arg("--module-root")
        .arg(module_root);
    if wasm_gc {
        cmd.arg("--wasm-gc");
    }
    let out = cmd.output().expect("expected `aver verify` to execute");
    cleanup(&path);
    String::from_utf8_lossy(&out.stdout).to_string()
}

fn run_verify_summary(prefix: &str, source: &str, wasm_gc: bool) -> (bool, String) {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module(prefix, source);
    let module_root = path.parent().expect("temp module has parent");
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root)
        .arg("verify")
        .arg(&path)
        .arg("--module-root")
        .arg(module_root);
    if wasm_gc {
        cmd.arg("--wasm-gc");
    }
    let out = cmd.output().expect("expected `aver verify` to execute");
    cleanup(&path);
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let summary = stdout
        .lines()
        .find(|l| l.starts_with("Summary:"))
        .unwrap_or("")
        .to_string();
    (out.status.success(), summary)
}

#[test]
fn cross_verify_pass_vm() {
    let (ok, summary) = run_verify_summary("aver-cross-vfypass-vm", VERIFY_PASS_SRC, false);
    assert!(ok, "VM verify failed:\nsummary: {}", summary);
    assert!(
        summary.contains("6/6 cases passed | 0 failed"),
        "VM summary unexpected: {}",
        summary
    );
}
#[test]
fn cross_verify_pass_wasm_gc() {
    let (ok, summary) = run_verify_summary("aver-cross-vfypass-wasmgc", VERIFY_PASS_SRC, true);
    assert!(ok, "wasm-gc verify failed:\nsummary: {}", summary);
    assert!(
        summary.contains("6/6 cases passed | 0 failed"),
        "wasm-gc summary unexpected: {}",
        summary
    );
}

#[test]
fn cross_verify_fail_vm() {
    let (ok, summary) = run_verify_summary("aver-cross-vfyfail-vm", VERIFY_FAIL_SRC, false);
    assert!(!ok, "VM verify unexpectedly passed: {}", summary);
    assert!(
        summary.contains("1/3 cases passed | 2 failed"),
        "VM summary unexpected: {}",
        summary
    );
}
#[test]
fn cross_verify_fail_wasm_gc() {
    let (ok, summary) = run_verify_summary("aver-cross-vfyfail-wasmgc", VERIFY_FAIL_SRC, true);
    assert!(!ok, "wasm-gc verify unexpectedly passed: {}", summary);
    assert!(
        summary.contains("1/3 cases passed | 2 failed"),
        "wasm-gc summary unexpected: {}",
        summary
    );
}

/// Wasm-gc verify must surface actual runtime values for primitive
/// return types — not just `<wasm-gc reports pass/fail only>`. The
/// `__verify_X_left_repr() -> String` helper synthesizes a
/// `String.fromInt(left_expr)` call (for `Type::Int`) which the
/// backend lowers to a real wasm fn; the host decodes the resulting
/// String via `__rt_string_to_lm`.
#[test]
fn cross_verify_fail_renders_actual_int_wasm_gc() {
    let stdout = run_verify_full_stdout("aver-cross-vfyactual-wasmgc", VERIFY_FAIL_SRC, true);
    // buggy(0) => 0 fails with actual=1; buggy(10) => 100 fails with actual=11.
    assert!(
        stdout.contains("expected: 0") && stdout.contains("actual: 1"),
        "wasm-gc didn't render actual Int values for the buggy(0) case:\n{}",
        stdout
    );
    assert!(
        stdout.contains("expected: 100") && stdout.contains("actual: 11"),
        "wasm-gc didn't render actual Int values for the buggy(10) case:\n{}",
        stdout
    );
}

#[test]
fn cross_vector_aliasing_pin_self_host() {
    assert_eq_with_label(
        "self-host",
        &run_self_host("aver-cross-alias-sh", VECTOR_ALIASING_SRC),
        VECTOR_ALIASING_OUT,
    );
}
