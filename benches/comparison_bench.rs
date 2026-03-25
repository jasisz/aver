#![allow(clippy::approx_constant)]
//! Benchmark comparing all four execution modes on the same programs:
//!   interpreter (tree-walk), VM (bytecode), codegen (native Rust), self-hosted (aver-self).
//!
//! Codegen and self-hosted run as external processes (`aver-life`-style compiled binaries
//! and `aver-self` binary). Pre-compile before running:
//!
//!   cargo bench --bench comparison_bench

use criterion::{
    BenchmarkGroup, BenchmarkId, Criterion, criterion_group, criterion_main, measurement::WallTime,
};
use std::io::Write;
use std::process::Command;

use aver::interpreter::Interpreter;
use aver::nan_value::Arena;
use aver::resolver;
use aver::source::parse_source;
use aver::tco;
use aver::vm;

// ── Runners ──────────────────────────────────────────────────────────────────

fn run_interpreter(source: &str) {
    let mut items = parse_source(source).expect("parse error");
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);
    let mut interp = Interpreter::new();
    interp.exec_items(&items).expect("exec error");
    let main_fn = interp.lookup("main").expect("no main");
    let _ = interp.call_value_pub(main_fn, vec![]).expect("call error");
}

fn run_vm(source: &str) {
    let mut items = parse_source(source).expect("parse error");
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);
    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program(&items, &mut arena).expect("compile error");
    let mut machine = vm::VM::new(code, globals, arena);
    let _ = machine.run().expect("VM error");
}

fn run_external(bin: &str, args: &[&str]) {
    let output = Command::new(bin)
        .args(args)
        .output()
        .unwrap_or_else(|e| panic!("failed to run {}: {}", bin, e));
    assert!(
        output.status.success(),
        "{} failed: {}",
        bin,
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Compile an Aver source to a temp binary via `aver compile` + `cargo build --release`.
/// Returns the path to the compiled binary.
fn compile_to_native(source: &str, name: &str) -> std::path::PathBuf {
    let dir = tempfile::tempdir().expect("create temp dir");
    let src_path = dir.path().join("main.av");
    std::fs::write(&src_path, source).expect("write source");

    let out_dir = dir.path().join("out");
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let rt_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("aver-rt");

    let status = Command::new(aver_bin)
        .arg("compile")
        .arg(&src_path)
        .arg("-o")
        .arg(&out_dir)
        .arg("--name")
        .arg(name)
        .env("AVER_RUNTIME_PATH", &rt_path)
        .status()
        .expect("aver compile");
    assert!(status.success(), "aver compile failed");

    let status = Command::new("cargo")
        .arg("build")
        .arg("--release")
        .current_dir(&out_dir)
        .status()
        .expect("cargo build");
    assert!(status.success(), "cargo build failed");

    let binary = out_dir.join("target/release").join(name);
    // Copy to a stable location so the tempdir can be kept alive
    let stable = std::env::temp_dir().join(format!("aver_bench_{}", name));
    std::fs::copy(&binary, &stable).expect("copy binary");
    stable
}

/// Write source to a temp file, return its path (kept alive by caller).
fn write_temp_source(source: &str) -> tempfile::NamedTempFile {
    let mut tmp = tempfile::NamedTempFile::new().expect("create temp file");
    tmp.write_all(source.as_bytes()).expect("write");
    tmp.flush().expect("flush");
    tmp
}

// ── Test programs ────────────────────────────────────────────────────────────

const FIB_SRC: &str = r#"module Bench

fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)

fn main() -> Int
    fib(25)
"#;

const COUNTDOWN_SRC: &str = r#"module Bench

fn countdown(n: Int) -> Int
    match n
        0 -> 0
        _ -> countdown(n - 1)

fn main() -> Int
    countdown(20000)
"#;

const RECORD_SRC: &str = r#"module Bench

record Point
    x: Int
    y: Int
    z: Int

fn sumFields(p: Point, n: Int) -> Int
    match n
        0 -> 0
        _ -> p.x + p.y + p.z + sumFields(p, n - 1)

fn main() -> Int
    sumFields(Point(x = 1, y = 2, z = 3), 20000)
"#;

const MAP_BUILD_SRC: &str = r#"module Bench

fn buildMap(n: Int, m: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> m
        _ -> buildMap(n - 1, Map.set(m, n, n * n))

fn main() -> Map<Int, Int>
    buildMap(5000, Map.empty())
"#;

const MATCH_SRC: &str = r#"module Bench

type Shape
    Circle(Float)
    Rect(Float, Float)
    Triangle(Float, Float, Float)

fn area(s: Shape) -> Float
    match s
        Shape.Circle(r) -> 3.14159 * r * r
        Shape.Rect(w, h) -> w * h
        Shape.Triangle(a, b, c) -> (a + b + c) / 2.0

fn pickShape(n: Int) -> Shape
    match n
        1 -> Shape.Circle(1.0)
        2 -> Shape.Rect(2.0, 3.0)
        _ -> Shape.Triangle(3.0, 4.0, 5.0)

fn sumAreas(n: Int, acc: Float, pick: Int) -> Float
    match n
        0 -> acc
        _ -> sumAreas(n - 1, acc + area(pickShape(pick)), pick)

fn main() -> Float
    sumAreas(10000, 0.0, 1) + sumAreas(10000, 0.0, 2) + sumAreas(10000, 0.0, 3)
"#;

const STRING_SRC: &str = r#"module Bench

fn buildString(n: Int, acc: String) -> String
    match n
        0 -> acc
        _ -> buildString(n - 1, "{acc}-{Int.toString(n)}")

fn main() -> String
    buildString(5000, "start")
"#;

const VECTOR_SRC: &str = r#"module Bench

fn fillVector(v: Vector<Int>, n: Int, i: Int) -> Vector<Int>
    match i == n
        true -> v
        false -> fillVector(Option.withDefault(Vector.set(v, i, i * i), v), n, i + 1)

fn sumVector(v: Vector<Int>, n: Int, i: Int, acc: Int) -> Int
    match i == n
        true -> acc
        false -> sumVector(v, n, i + 1, acc + Option.withDefault(Vector.get(v, i), 0))

fn main() -> Int
    v = fillVector(Vector.new(5000, 0), 5000, 0)
    sumVector(v, 5000, 0, 0)
"#;

// ── Benchmark groups ─────────────────────────────────────────────────────────

fn bench_all_modes(
    group: &mut BenchmarkGroup<WallTime>,
    label: &str,
    source: &str,
    native_bin: &std::path::Path,
    self_hosted_bin: &str,
    source_file: &std::path::Path,
) {
    group.bench_with_input(BenchmarkId::new("interpreter", label), source, |b, src| {
        b.iter(|| run_interpreter(src));
    });
    group.bench_with_input(BenchmarkId::new("vm", label), source, |b, src| {
        b.iter(|| run_vm(src));
    });
    group.bench_function(BenchmarkId::new("codegen", label), |b| {
        b.iter(|| run_external(native_bin.to_str().unwrap(), &[]));
    });
    group.bench_function(BenchmarkId::new("self-hosted", label), |b| {
        b.iter(|| run_external(self_hosted_bin, &[source_file.to_str().unwrap()]));
    });
}

fn comparison_benches(c: &mut Criterion) {
    let tests: &[(&str, &str, &str)] = &[
        ("fib(25)", "bench_fib", FIB_SRC),
        ("countdown(20k)", "bench_countdown", COUNTDOWN_SRC),
        ("record access 20k", "bench_record", RECORD_SRC),
        ("map build 5k", "bench_map", MAP_BUILD_SRC),
        ("pattern match 30k", "bench_match", MATCH_SRC),
        ("string interp 5k", "bench_string", STRING_SRC),
        ("vector get/set 5k", "bench_vector", VECTOR_SRC),
    ];

    // Pre-compile all native binaries
    let natives: Vec<std::path::PathBuf> = tests
        .iter()
        .map(|(label, name, src)| {
            eprintln!("Compiling {} to native...", label);
            compile_to_native(src, name)
        })
        .collect();

    // Write source files for self-hosted
    let source_files: Vec<tempfile::NamedTempFile> = tests
        .iter()
        .map(|(_, _, src)| write_temp_source(src))
        .collect();

    let self_hosted = format!("{}/.cargo/bin/aver-self", std::env::var("HOME").unwrap());

    for (i, (label, _, src)) in tests.iter().enumerate() {
        let mut group = c.benchmark_group(*label);
        bench_all_modes(
            &mut group,
            label,
            src,
            &natives[i],
            &self_hosted,
            source_files[i].path(),
        );
        group.finish();
    }
}

criterion_group!(benches, comparison_benches);
criterion_main!(benches);
