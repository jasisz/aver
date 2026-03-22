#![allow(clippy::approx_constant)]
use criterion::{Criterion, criterion_group, criterion_main};

use aver::nan_value::Arena;
use aver::resolver;
use aver::source::parse_source;
use aver::tco;
use aver::vm;

fn run_vm(source: &str) -> aver::nan_value::NanValue {
    let mut items = parse_source(source).expect("parse error");
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program(&items, &mut arena).expect("compile error");
    let mut machine = vm::VM::new(code, globals, arena);
    machine.run().expect("VM execution failed")
}

// ---------------------------------------------------------------------------
// Fibonacci (recursive, no memo) — tests call overhead + match dispatch
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
    c.bench_function("vm fib(25) no memo", |b| {
        b.iter(|| run_vm(FIB_SRC));
    });
}

// ---------------------------------------------------------------------------
// Deep recursion (TCO) — tests tail-call overhead
// ---------------------------------------------------------------------------
const COUNTDOWN_SRC: &str = r#"
fn countdown(n: Int) -> Int
    match n
        0 -> 0
        _ -> countdown(n - 1)

fn main() -> Int
    countdown(1000000)
"#;

fn bench_countdown(c: &mut Criterion) {
    c.bench_function("vm countdown(1M) TCO", |b| {
        b.iter(|| run_vm(COUNTDOWN_SRC));
    });
}

// ---------------------------------------------------------------------------
// Record field access — tests O(1) field index lookup
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
    sumFields(Point(x = 1, y = 2, z = 3), 100000)
"#;

fn bench_record_access(c: &mut Criterion) {
    c.bench_function("vm record field access 100k", |b| {
        b.iter(|| run_vm(RECORD_SRC));
    });
}

// ---------------------------------------------------------------------------
// Pattern matching — tests match dispatch on sum types
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
    sumAreas(100000, 0.0, 1) + sumAreas(100000, 0.0, 2) + sumAreas(100000, 0.0, 3)
"#;

fn bench_pattern_matching(c: &mut Criterion) {
    c.bench_function("vm pattern match 100k variants", |b| {
        b.iter(|| run_vm(MATCH_SRC));
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
    buildString(10000, "start")
"#;

fn bench_string_build(c: &mut Criterion) {
    c.bench_function("vm string interpolation 10k", |b| {
        b.iter(|| run_vm(STRING_SRC));
    });
}

criterion_group!(
    benches,
    bench_fib,
    bench_countdown,
    bench_record_access,
    bench_pattern_matching,
    bench_string_build,
);
criterion_main!(benches);
