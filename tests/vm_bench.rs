/// Benchmark: interpreter vs VM on identical Aver programs.
///
/// Measures compilation time, execution time, and result correctness.
/// Both sides must call main() and produce the same result.
use std::time::Instant;

use aver::ast::TopLevel;
use aver::interpreter::{Interpreter, Value};
use aver::lexer::Lexer;
use aver::nan_value::{Arena, NanValue};
use aver::parser::Parser;
use aver::resolver;
use aver::tco;
use aver::vm;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn parse(src: &str) -> Vec<TopLevel> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser.parse().expect("parse failed")
}

/// Compare interpreter Value and VM NanValue semantically.
fn results_match(interp: &Value, vm_val: NanValue, arena: &Arena) -> bool {
    let vm_as_value = vm_val.to_value(arena);
    value_eq(interp, &vm_as_value)
}

/// Structural equality for Value (not derived because of HashMap/Fn).
fn value_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => (x - y).abs() < 1e-10,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Unit, Value::Unit) => true,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Ok(x), Value::Ok(y)) => value_eq(x, y),
        (Value::Err(x), Value::Err(y)) => value_eq(x, y),
        (Value::Some(x), Value::Some(y)) => value_eq(x, y),
        (Value::None, Value::None) => true,
        _ => false,
    }
}

const WARMUP_RUNS: usize = 2;
const BENCH_RUNS: usize = 5;

struct BenchResult {
    name: &'static str,
    interp_setup_ms: f64,
    interp_exec_ms: f64,
    vm_compile_ms: f64,
    vm_exec_ms: f64,
    match_ok: bool,
    interp_repr: String,
    vm_repr: String,
}

fn median(mut v: Vec<f64>) -> f64 {
    v.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let mid = v.len() / 2;
    if v.len().is_multiple_of(2) {
        (v[mid - 1] + v[mid]) / 2.0
    } else {
        v[mid]
    }
}

fn bench(name: &'static str, src: &str) -> BenchResult {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    // Warmup: run both sides to avoid cold-cache artifacts.
    for _ in 0..WARMUP_RUNS {
        let mut interp = Interpreter::new();
        interp.exec_items(&items).unwrap();
        let main_fn = interp.lookup("main").unwrap();
        let _ = interp.call_value_pub(main_fn, vec![]);

        let mut arena = Arena::new();
        let (code, globals) = vm::compile_program(&items, &mut arena).unwrap();
        let mut machine = vm::VM::new(code, globals, arena);
        let _ = machine.run();
    }

    // Measured runs.
    let mut interp_setup_times = Vec::new();
    let mut interp_exec_times = Vec::new();
    let mut vm_compile_times = Vec::new();
    let mut vm_exec_times = Vec::new();
    let mut last_interp_result: Option<Value> = None;
    let mut last_vm_result: Option<NanValue> = None;
    let mut last_vm_arena: Option<Arena> = None;

    for _ in 0..BENCH_RUNS {
        // Interpreter
        let t0 = Instant::now();
        let mut interp = Interpreter::new();
        interp.exec_items(&items).unwrap();
        interp_setup_times.push(t0.elapsed().as_secs_f64() * 1000.0);

        let main_fn = interp.lookup("main").unwrap();
        let t1 = Instant::now();
        let interp_result = interp.call_value_pub(main_fn, vec![]).unwrap();
        interp_exec_times.push(t1.elapsed().as_secs_f64() * 1000.0);
        last_interp_result = Some(interp_result);

        // VM
        let t2 = Instant::now();
        let mut arena = Arena::new();
        let (code, globals) = vm::compile_program(&items, &mut arena).unwrap();
        vm_compile_times.push(t2.elapsed().as_secs_f64() * 1000.0);

        let t3 = Instant::now();
        let mut machine = vm::VM::new(code, globals, arena);
        let vm_result = machine.run().unwrap();
        vm_exec_times.push(t3.elapsed().as_secs_f64() * 1000.0);
        last_vm_result = Some(vm_result);
        last_vm_arena = Some(std::mem::replace(&mut machine.arena, Arena::new()));
    }

    let interp_result = last_interp_result.unwrap();
    let vm_result = last_vm_result.unwrap();
    let vm_arena = last_vm_arena.unwrap();

    let match_ok = results_match(&interp_result, vm_result, &vm_arena);
    let interp_repr = format!("{:?}", interp_result);
    let vm_repr = vm_result.repr(&vm_arena);

    BenchResult {
        name,
        interp_setup_ms: median(interp_setup_times),
        interp_exec_ms: median(interp_exec_times),
        vm_compile_ms: median(vm_compile_times),
        vm_exec_ms: median(vm_exec_times),
        match_ok,
        interp_repr,
        vm_repr,
    }
}

// ---------------------------------------------------------------------------
// Benchmark programs (pure computation, no effects)
// ---------------------------------------------------------------------------

const FIBONACCI: &str = "\
fn fib(n: Int) -> Int
    match n < 2
        true -> n
        false -> fib(n - 1) + fib(n - 2)

fn main() -> Int
    fib(25)
";

const SUM_TCO: &str = "\
fn sum(n: Int, acc: Int) -> Int
    match n == 0
        true -> acc
        false -> sum(n - 1, acc + n)

fn main() -> Int
    sum(1000000, 0)
";

const COUNTDOWN: &str = "\
fn countdown(n: Int) -> Int
    match n == 0
        true -> 0
        false -> countdown(n - 1)

fn main() -> Int
    countdown(1000000)
";

const FACTORIAL_TCO: &str = "\
fn fact(n: Int, acc: Int) -> Int
    match n == 0
        true -> acc
        false -> fact(n - 1, acc * n)

fn main() -> Int
    fact(20, 1)
";

const NESTED_MATCH: &str = "\
fn abs(n: Int) -> Int\n    match n < 0\n        true -> 0 - n\n        false -> n\n\nfn classify(n: Int) -> Int\n    match n == 0\n        true -> 0\n        false -> abs(n) * 2\n\nfn run(n: Int, acc: Int) -> Int\n    match n == 0\n        true -> acc\n        false -> run(n - 1, acc + classify(n - 500))\n\nfn main() -> Int\n    run(1000, 0)\n";

const RESULT_CHAIN: &str = "\
fn safeDivide(a: Int, b: Int) -> Result<Int, Int>\n    match b == 0\n        true -> Result.Err(0)\n        false -> Result.Ok(a / b)\n\nfn unwrapOr(r: Result<Int, Int>, d: Int) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> d\n\nfn chain(n: Int, acc: Int) -> Int\n    match n == 0\n        true -> acc\n        false -> chain(n - 1, acc + unwrapOr(safeDivide(n * 10, n), 0))\n\nfn main() -> Int\n    chain(10000, 0)\n";

const LIST_WALK: &str = "\
fn listLen(xs: List<Int>, acc: Int) -> Int\n    match xs\n        [] -> acc\n        [h, ..t] -> listLen(t, acc + 1)\n\nfn main() -> Int\n    listLen([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 0)\n";

// ---------------------------------------------------------------------------
// Realistic benchmarks (phase 3)
// ---------------------------------------------------------------------------

/// Shapes — variant construction + match + float arithmetic.
/// Real Aver program pattern: define types, match on them, compute.
const SHAPES: &str = "\
type Shape\n    Circle(Float)\n    Rect(Float, Float)\n    Point\n\nfn area(s: Shape) -> Float\n    match s\n        Shape.Circle(r) -> r * r * 3.14159\n        Shape.Rect(w, h) -> w * h\n        Shape.Point -> 0.0\n\nfn sumAreas(n: Int, acc: Float) -> Float\n    match n == 0\n        true -> acc\n        false -> sumAreas(n - 1, acc + area(Shape.Circle(1.0)) + area(Shape.Rect(3.0, 4.0)))\n\nfn main() -> Float\n    sumAreas(10000, 0.0)\n";

/// Record-heavy — create, access, update records in a loop.
const RECORD_HEAVY: &str = "\
record Point\n    x: Int\n    y: Int\n\nfn moveRight(p: Point) -> Point\n    Point.update(p, x = p.x + 1)\n\nfn walk(n: Int, p: Point) -> Int\n    match n == 0\n        true -> p.x + p.y\n        false -> walk(n - 1, moveRight(p))\n\nfn main() -> Int\n    walk(10000, Point(x = 0, y = 0))\n";

/// String building — concat in a loop. Allocation-heavy.
const STRING_BUILD: &str = "\
fn repeat(n: Int, s: String, acc: String) -> String\n    match n == 0\n        true -> acc\n        false -> repeat(n - 1, s, acc + s)\n\nfn main() -> Int\n    r = repeat(1000, \"ab\", \"\")\n    String.len(r)\n";

/// Result chaining with ? operator — realistic error handling pattern.
const ERROR_PIPELINE: &str = "\
fn validate(n: Int) -> Result<Int, String>\n    match n < 0\n        true -> Result.Err(\"negative\")\n        false -> Result.Ok(n)\n\nfn transform(n: Int) -> Result<Int, String>\n    match n > 1000\n        true -> Result.Err(\"too large\")\n        false -> Result.Ok(n * 2)\n\nfn pipeline(n: Int) -> Result<Int, String>\n    v = validate(n)?\n    transform(v)\n\nfn run(n: Int, acc: Int) -> Int\n    match n == 0\n        true -> acc\n        false -> run(n - 1, acc + Result.withDefault(pipeline(n), 0))\n\nfn main() -> Int\n    run(10000, 0)\n";

/// List operations via builtins — List.len, List.get, List.prepend.
const LIST_BUILTINS: &str = "\
fn buildAndMeasure(n: Int, acc: List<Int>) -> Int\n    match n == 0\n        true -> List.len(acc)\n        false -> buildAndMeasure(n - 1, List.prepend(n, acc))\n\nfn main() -> Int\n    buildAndMeasure(1000, [])\n";

/// Mixed: records + variants + match + Result. Most realistic Aver pattern.
const MIXED_REAL: &str = "\
record Order\n    id: Int\n    amount: Int\n    valid: Bool\n\nfn processOrder(o: Order) -> Result<Int, String>\n    match o.valid\n        true -> Result.Ok(o.amount * 2)\n        false -> Result.Err(\"invalid\")\n\nfn processAll(n: Int, acc: Int) -> Int\n    match n == 0\n        true -> acc\n        false -> processAll(n - 1, acc + Result.withDefault(processOrder(Order(id = n, amount = n * 10, valid = true)), 0))\n\nfn main() -> Int\n    processAll(10000, 0)\n";

// ---------------------------------------------------------------------------
// Main benchmark test
// ---------------------------------------------------------------------------

#[test]
fn vm_benchmark() {
    let programs: Vec<(&str, &str)> = vec![
        // Micro benchmarks
        ("fib(25)", FIBONACCI),
        ("sum_tco(1M)", SUM_TCO),
        ("countdown(1M)", COUNTDOWN),
        ("factorial(20)", FACTORIAL_TCO),
        ("nested_match(1K)", NESTED_MATCH),
        ("result_chain(10K)", RESULT_CHAIN),
        ("list_walk(10)", LIST_WALK),
        // Realistic benchmarks
        ("shapes(10K)", SHAPES),
        ("record_heavy(10K)", RECORD_HEAVY),
        ("string_build(1K)", STRING_BUILD),
        ("error_pipeline(10K)", ERROR_PIPELINE),
        ("list_builtins(1K)", LIST_BUILTINS),
        ("mixed_real(10K)", MIXED_REAL),
    ];

    eprintln!(
        "\n  Aver VM vs Interpreter — median of {} runs, {} warmup",
        BENCH_RUNS, WARMUP_RUNS
    );
    eprintln!("{:-<108}", "");
    eprintln!(
        "{:<22} {:>12} {:>12} {:>12} {:>12} {:>12} {:>6}",
        "Benchmark", "I.setup", "I.exec", "VM compile", "VM exec", "exec ratio", "Match"
    );
    eprintln!("{:-<108}", "");

    let mut all_ok = true;

    for (name, src) in &programs {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| bench(name, src))) {
            Ok(r) => {
                let ratio = if r.vm_exec_ms > 0.001 {
                    format!("{:.1}x", r.interp_exec_ms / r.vm_exec_ms)
                } else if r.interp_exec_ms > 0.001 {
                    "VM>>".to_string()
                } else {
                    "~same".to_string()
                };
                let status = if r.match_ok { "OK" } else { "DIFF" };
                eprintln!(
                    "{:<22} {:>10.3}ms {:>10.3}ms {:>10.3}ms {:>10.3}ms {:>12} {:>5}",
                    r.name,
                    r.interp_setup_ms,
                    r.interp_exec_ms,
                    r.vm_compile_ms,
                    r.vm_exec_ms,
                    ratio,
                    status
                );
                if !r.match_ok {
                    eprintln!("  interp: {}", r.interp_repr);
                    eprintln!("  vm:     {}", r.vm_repr);
                    all_ok = false;
                }
            }
            Err(e) => {
                let msg = if let Some(s) = e.downcast_ref::<String>() {
                    s.clone()
                } else if let Some(s) = e.downcast_ref::<&str>() {
                    s.to_string()
                } else {
                    "unknown panic".into()
                };
                eprintln!("{:<22} FAILED: {}", name, msg);
                all_ok = false;
            }
        }
    }
    eprintln!("{:-<108}", "");

    assert!(
        all_ok,
        "Some benchmarks produced different results between interpreter and VM"
    );
}
