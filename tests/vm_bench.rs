/// Benchmark: interpreter vs VM on identical Aver programs.
///
/// Measures compilation time, execution time, and result correctness.
/// Both sides must call main() and produce the same result.
use std::time::Instant;

use aver::ast::TopLevel;
use aver::interpreter::{Interpreter, Value};
use aver::lexer::Lexer;
use aver::nan_value::{Arena, ArenaUsage, NanValue};
use aver::parser::Parser;
use aver::resolver;
use aver::runtime_bench_cases::CORE_BENCH_CASES;
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
    interp_peak_delta: usize,
    interp_live_delta: usize,
    vm_compile_ms: f64,
    vm_exec_ms: f64,
    vm_peak_delta: usize,
    vm_live_delta: usize,
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

fn median_usize(mut values: Vec<usize>) -> usize {
    values.sort_unstable();
    let mid = values.len() / 2;
    if values.len().is_multiple_of(2) {
        (values[mid - 1] + values[mid]) / 2
    } else {
        values[mid]
    }
}

fn total_usage(usage: ArenaUsage) -> usize {
    usage.total()
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
    let mut interp_peak_deltas = Vec::new();
    let mut interp_live_deltas = Vec::new();
    let mut vm_compile_times = Vec::new();
    let mut vm_exec_times = Vec::new();
    let mut vm_peak_deltas = Vec::new();
    let mut vm_live_deltas = Vec::new();
    let mut last_interp_result: Option<Value> = None;
    let mut last_vm_result: Option<NanValue> = None;
    let mut last_vm_arena: Option<Arena> = None;

    for _ in 0..BENCH_RUNS {
        // Interpreter
        let t0 = Instant::now();
        let mut interp = Interpreter::new();
        interp.exec_items(&items).unwrap();
        interp_setup_times.push(t0.elapsed().as_secs_f64() * 1000.0);
        let interp_baseline_peak = total_usage(interp.arena.peak_usage());
        let interp_baseline_live = total_usage(interp.arena.usage());

        let main_fn = interp.lookup("main").unwrap();
        let t1 = Instant::now();
        let interp_result = interp.call_value_pub(main_fn, vec![]).unwrap();
        interp_exec_times.push(t1.elapsed().as_secs_f64() * 1000.0);
        interp_peak_deltas
            .push(total_usage(interp.arena.peak_usage()).saturating_sub(interp_baseline_peak));
        interp_live_deltas
            .push(total_usage(interp.arena.usage()).saturating_sub(interp_baseline_live));
        last_interp_result = Some(interp_result);

        // VM
        let t2 = Instant::now();
        let mut arena = Arena::new();
        let (code, globals) = vm::compile_program(&items, &mut arena).unwrap();
        vm_compile_times.push(t2.elapsed().as_secs_f64() * 1000.0);
        let vm_baseline_peak = total_usage(arena.peak_usage());
        let vm_baseline_live = total_usage(arena.usage());

        let t3 = Instant::now();
        let mut machine = vm::VM::new(code, globals, arena);
        let vm_result = machine.run().unwrap();
        vm_exec_times.push(t3.elapsed().as_secs_f64() * 1000.0);
        vm_peak_deltas
            .push(total_usage(machine.arena.peak_usage()).saturating_sub(vm_baseline_peak));
        vm_live_deltas.push(total_usage(machine.arena.usage()).saturating_sub(vm_baseline_live));
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
        interp_peak_delta: median_usize(interp_peak_deltas),
        interp_live_delta: median_usize(interp_live_deltas),
        vm_compile_ms: median(vm_compile_times),
        vm_exec_ms: median(vm_exec_times),
        vm_peak_delta: median_usize(vm_peak_deltas),
        vm_live_delta: median_usize(vm_live_deltas),
        match_ok,
        interp_repr,
        vm_repr,
    }
}

// ---------------------------------------------------------------------------
// Main benchmark test
// ---------------------------------------------------------------------------

fn run_vm_benchmark() {
    eprintln!(
        "\n  Aver VM vs Interpreter — median of {} runs, {} warmup",
        BENCH_RUNS, WARMUP_RUNS
    );
    eprintln!("{:-<152}", "");
    eprintln!(
        "{:<22} {:>12} {:>12} {:>10} {:>10} {:>12} {:>12} {:>10} {:>10} {:>10} {:>6}",
        "Benchmark",
        "I.setup",
        "I.exec",
        "I.peak+",
        "I.live+",
        "VM compile",
        "VM exec",
        "VM.peak+",
        "VM.live+",
        "exec ratio",
        "Match"
    );
    eprintln!("{:-<152}", "");

    let mut all_ok = true;

    for case in CORE_BENCH_CASES {
        let name = case.name;
        let src = case.source;
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
                    "{:<22} {:>10.3}ms {:>10.3}ms {:>10} {:>10} {:>10.3}ms {:>10.3}ms {:>10} {:>10} {:>10} {:>5}",
                    r.name,
                    r.interp_setup_ms,
                    r.interp_exec_ms,
                    r.interp_peak_delta,
                    r.interp_live_delta,
                    r.vm_compile_ms,
                    r.vm_exec_ms,
                    r.vm_peak_delta,
                    r.vm_live_delta,
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
    eprintln!("{:-<152}", "");

    assert!(
        all_ok,
        "Some benchmarks produced different results between interpreter and VM"
    );
}

#[test]
#[ignore = "performance benchmark; run manually when profiling VM vs interpreter"]
fn vm_benchmark() {
    std::thread::Builder::new()
        .name("vm_benchmark".to_string())
        .stack_size(128 * 1024 * 1024)
        .spawn(run_vm_benchmark)
        .expect("spawn vm_benchmark")
        .join()
        .expect("vm_benchmark thread");
}
