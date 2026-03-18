#![allow(clippy::approx_constant)]
use criterion::{Criterion, criterion_group, criterion_main};

use aver::interpreter::Interpreter;
use aver::resolver;
use aver::source::parse_source;
use aver::tco;

fn run_aver(source: &str) -> aver::value::Value {
    let mut items = parse_source(source).expect("parse error");
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    let mut interp = Interpreter::new();
    interp.exec_items(&items).expect("exec error");

    let main_fn = interp.lookup("main").expect("no main");
    interp.call_value_pub(main_fn, vec![]).expect("call error")
}

// ---------------------------------------------------------------------------
// Fibonacci (recursive, no memo) — tests call overhead + env management
// ---------------------------------------------------------------------------
const FIB_SRC: &str = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)

fn main() -> Int
    fib(25)
"#;

fn bench_fib(c: &mut Criterion) {
    c.bench_function("fib(25) no memo", |b| {
        b.iter(|| run_aver(FIB_SRC));
    });
}

// ---------------------------------------------------------------------------
// Deep recursion (TCO) — tests env save/restore, split_off/append
// ---------------------------------------------------------------------------
const COUNTDOWN_SRC: &str = r#"
fn countdown(n: Int) -> Int
    match n
        0 -> 0
        _ -> countdown(n - 1)

fn main() -> Int
    countdown(100000)
"#;

fn bench_countdown(c: &mut Criterion) {
    c.bench_function("countdown(100k) TCO", |b| {
        b.iter(|| run_aver(COUNTDOWN_SRC));
    });
}

// ---------------------------------------------------------------------------
// Map operations — tests Map.set clone overhead
// ---------------------------------------------------------------------------
const MAP_BUILD_SRC: &str = r#"
fn buildMap(n: Int, m: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> m
        _ -> buildMap(n - 1, Map.set(m, n, n * n))

fn main() -> Map<Int, Int>
    buildMap(500, Map.empty())
"#;

fn bench_map_build(c: &mut Criterion) {
    c.bench_function("map build 500 entries", |b| {
        b.iter(|| run_aver(MAP_BUILD_SRC));
    });
}

// ---------------------------------------------------------------------------
// Record field access — tests O(n) linear scan
// ---------------------------------------------------------------------------
const RECORD_SRC: &str = r#"
record Point
    x: Int
    y: Int
    z: Int

fn sumFields(p: Point, n: Int) -> Int
    match n
        0 -> 0
        _ -> p.x + p.y + p.z + sumFields(p, n - 1)

fn main() -> Int
    sumFields(Point(x = 1, y = 2, z = 3), 10000)
"#;

fn bench_record_access(c: &mut Criterion) {
    c.bench_function("record field access 10k", |b| {
        b.iter(|| run_aver(RECORD_SRC));
    });
}

// ---------------------------------------------------------------------------
// Pattern matching — tests match dispatch + binding on sum types
// ---------------------------------------------------------------------------
const MATCH_SRC: &str = r#"
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

fn bench_pattern_matching(c: &mut Criterion) {
    c.bench_function("pattern match 10k variants", |b| {
        b.iter(|| run_aver(MATCH_SRC));
    });
}

// ---------------------------------------------------------------------------
// String interpolation — tests string alloc + concat overhead
// ---------------------------------------------------------------------------
const STRING_SRC: &str = r#"
fn buildString(n: Int, acc: String) -> String
    match n
        0 -> acc
        _ -> buildString(n - 1, "{acc}-{Int.toString(n)}")

fn main() -> String
    buildString(1000, "start")
"#;

fn bench_string_build(c: &mut Criterion) {
    c.bench_function("string interpolation 1k", |b| {
        b.iter(|| run_aver(STRING_SRC));
    });
}

criterion_group!(
    benches,
    bench_fib,
    bench_countdown,
    bench_map_build,
    bench_record_access,
    bench_pattern_matching,
    bench_string_build,
);
criterion_main!(benches);
