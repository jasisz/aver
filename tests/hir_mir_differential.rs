//! Full-pipeline HIR-vs-MIR differential execution harness.
//!
//! MIR is the default VM compile path. This compiles each program
//! through BOTH the HIR walker (`compile_program`) and the MIR-default
//! walker (`compile_program_with_mir_fallback`) using the FULL pipeline
//! — crucially including `last_use`, which the in-process
//! `mir_vm_parity` harness skips — runs the entry fn on each, and
//! asserts identical VM-observable values. A divergence is a latent bug
//! on the shipped default path.
//!
//! The program set targets the shapes most likely to diverge between
//! the walkers: owned/in-place mutation (Vector/Map accumulators), the
//! leaf-classifier interaction (chains of non-tail known calls), fused
//! compound leaf-ops, deep/mutual recursion, nested matches, and
//! record/tuple construction.

use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::nan_value::{Arena, NanValue, NanValueConvert};
use aver::source::parse_source;
use aver::value::Value;
use aver::vm;

fn run_both(src: &str, entry: &str) -> (Value, Value) {
    let mut items = parse_source(src).expect("parse failed");
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck {
        assert!(tc.errors.is_empty(), "typecheck errors: {:?}", tc.errors);
    }
    let resolved = result.resolved_items;
    let symbols = result.symbol_table;
    let analysis = result.analysis;

    let hir = {
        let mut arena = Arena::new();
        vm::register_service_types(&mut arena);
        let (code, globals) =
            vm::compile_program(&resolved, &symbols, &mut arena, analysis.as_ref())
                .expect("HIR compile failed");
        run_entry(code, globals, arena, entry)
    };
    let mir = {
        let mut arena = Arena::new();
        vm::register_service_types(&mut arena);
        let (code, globals) = vm::compile_program_with_mir_fallback(
            &resolved,
            &symbols,
            &mut arena,
            analysis.as_ref(),
        )
        .expect("MIR compile failed");
        run_entry(code, globals, arena, entry)
    };
    (hir, mir)
}

fn run_entry(code: vm::CodeStore, globals: Vec<NanValue>, arena: Arena, entry: &str) -> Value {
    let mut machine = vm::VM::new(code, globals, arena);
    machine.set_silent_console(true);
    let result = machine
        .run_named_function(entry, &[])
        .expect("VM run failed");
    result.to_value(&machine.arena)
}

fn assert_parity(name: &str, src: &str, entry: &str) {
    let (hir, mir) = run_both(src, entry);
    assert_eq!(
        hir, mir,
        "[{name}] HIR vs MIR execution divergence: HIR={hir:?} MIR={mir:?}"
    );
}

const MODULE_HEADER: &str = "module D\n    intent = \"differential\"\n    depends []\n\n";

fn prog(body: &str) -> String {
    format!("{MODULE_HEADER}{body}")
}

#[test]
fn leaf_classifier_call_chain() {
    // The exact shape class that crashed: chains of single non-tail
    // known calls, mixing owned (slot==arg-index) and plain calls.
    let src = prog(
        "fn deep(y: Int) -> Int\n    y\n\n\
         fn bump2(x: Int) -> Int\n    deep(x)\n\n\
         fn bump(x: Int) -> Int\n    bump2(x)\n\n\
         fn pair(a: Int, b: Int) -> Int\n    bump(b)\n\n\
         fn run() -> Int\n    pair(2, 5) + pair(7, 9)\n",
    );
    assert_parity("leaf_chain", &src, "run");
}

#[test]
fn owned_vector_accumulator() {
    let src = prog(
        "fn fill(v: Vector<Int>, n: Int, i: Int) -> Vector<Int>\n    \
         match i == n\n        true -> v\n        \
         false -> fill(Option.withDefault(Vector.set(v, i, i * i), v), n, i + 1)\n\n\
         fn sum(v: Vector<Int>, n: Int, i: Int, acc: Int) -> Int\n    \
         match i == n\n        true -> acc\n        \
         false -> sum(v, n, i + 1, acc + Option.withDefault(Vector.get(v, i), 0))\n\n\
         fn run() -> Int\n    sum(fill(Vector.new(20, 0), 20, 0), 20, 0, 0)\n",
    );
    assert_parity("owned_vector", &src, "run");
}

#[test]
fn owned_map_accumulator() {
    let src = prog(
        "fn build(n: Int, m: Map<String, Int>) -> Map<String, Int>\n    \
         match n\n        0 -> m\n        \
         _ -> build(n - 1, Map.set(m, String.fromInt(n), n))\n\n\
         fn run() -> Int\n    Map.len(build(25, {}))\n",
    );
    assert_parity("owned_map", &src, "run");
}

#[test]
fn aliased_param_threaded_then_read() {
    // A Map param mutated inside a callee whose caller keeps its own ref
    // — exercises the owned-mask alias guard end to end.
    let src = prog(
        "fn ins(m: Map<String, Int>) -> Map<String, Int>\n    Map.set(m, \"k\", 99)\n\n\
         fn run() -> Int\n    \
         base = Map.set({}, \"k\", 7)\n    \
         other = ins(base)\n    \
         Option.withDefault(Map.get(base, \"k\"), 0) * 1000 + Option.withDefault(Map.get(other, \"k\"), 0)\n",
    );
    assert_parity("aliased_map", &src, "run");
}

#[test]
fn deep_self_recursion() {
    let src = prog(
        "fn countdown(n: Int, acc: Int) -> Int\n    \
         match n\n        0 -> acc\n        _ -> countdown(n - 1, acc + n)\n\n\
         fn run() -> Int\n    countdown(1000, 0)\n",
    );
    assert_parity("deep_recursion", &src, "run");
}

#[test]
fn mutual_recursion() {
    let src = prog(
        "fn isEven(n: Int) -> Bool\n    \
         match n\n        0 -> true\n        _ -> isOdd(n - 1)\n\n\
         fn isOdd(n: Int) -> Bool\n    \
         match n\n        0 -> false\n        _ -> isEven(n - 1)\n\n\
         fn run() -> Bool\n    isEven(100)\n",
    );
    assert_parity("mutual_recursion", &src, "run");
}

#[test]
fn nested_match_and_arithmetic() {
    let src = prog(
        "fn classify(n: Int) -> Int\n    \
         match n < 0\n        \
         true -> 0 - 1\n        \
         false -> match n == 0\n            true -> 0\n            false -> 1\n\n\
         fn run() -> Int\n    classify(0 - 5) + classify(0) + classify(5)\n",
    );
    assert_parity("nested_match", &src, "run");
}

#[test]
fn record_and_tuple_roundtrip() {
    let src = prog(
        "record P\n  x: Int\n  y: Int\n\n\
         fn mk(a: Int, b: Int) -> P\n    P(x = a, y = b)\n\n\
         fn sumP(p: P) -> Int\n    p.x + p.y\n\n\
         fn run() -> Int\n    sumP(mk(3, 4)) + sumP(P(x = 10, y = 20))\n",
    );
    assert_parity("record_tuple", &src, "run");
}

#[test]
fn fib_doubly_recursive() {
    let src = prog(
        "fn fib(n: Int) -> Int\n    \
         match n < 2\n        true -> n\n        false -> fib(n - 1) + fib(n - 2)\n\n\
         fn run() -> Int\n    fib(20)\n",
    );
    assert_parity("fib", &src, "run");
}

#[test]
fn newtype_specialization() {
    let src = prog(
        "type Wrapped\n  W(Int)\n\n\
         fn unwrap(w: Wrapped) -> Int\n    \
         match w\n        Wrapped.W(n) -> n\n\n\
         fn run() -> Int\n    unwrap(Wrapped.W(41)) + unwrap(Wrapped.W(1))\n",
    );
    assert_parity("newtype", &src, "run");
}

#[test]
fn builtin_record_construction_and_projection() {
    // `HttpResponse(...)` is a built-in product type — it carries no
    // user `TypeId`, so MIR's RecordCreate rides its canonical
    // `type_name` and the walker resolves the arena type by name. Build
    // one and read a field back; identical on both paths.
    let src = prog(
        "fn mk(code: Int) -> HttpResponse\n    \
         HttpResponse(status = code, body = \"ok\", headers = {})\n\n\
         fn run() -> Int\n    mk(200).status\n",
    );
    assert_parity("builtin_record", &src, "run");
}

#[test]
fn discard_binding_evaluates_for_effect() {
    // `_ = step(5)?` — the idiomatic "run it, drop the Ok, continue"
    // form. The resolver assigns `_` no slot; the lowerer evaluates the
    // value under a fresh synthetic slot the body never reads. The
    // discarded call must still run (and its `?` still short-circuit),
    // identically on both paths. run() = drop step(5)=Ok(6), then
    // step(10) = Ok(11).
    let src = prog(
        "fn step(n: Int) -> Result<Int, String>\n    Result.Ok(n + 1)\n\n\
         fn run() -> Result<Int, String>\n    _ = step(5)?\n    step(10)\n",
    );
    assert_parity("discard_binding", &src, "run");
}

#[test]
fn first_class_fn_call_via_call_value() {
    // `applyTwice` calls its `Fn(..)` parameter `f` — a first-class fn
    // value in a local slot. The MIR walker lowers `f(x)` to a
    // CALL_VALUE dynamic dispatch (push the slot, then the args), the
    // same shape the HIR walker's compile_call fallback emits. Result:
    // applyTwice(inc, 5) = inc(inc(5)) = 7.
    let src = prog(
        "fn applyTwice(f: Fn(Int) -> Int, x: Int) -> Int\n    f(f(x))\n\n\
         fn inc(n: Int) -> Int\n    n + 1\n\n\
         fn run() -> Int\n    applyTwice(inc, 5)\n",
    );
    assert_parity("first_class_fn", &src, "run");
}

#[test]
fn nullary_ctor_value_position() {
    // `Option.None` and a nullary user variant (`Color.Black`) used in
    // value position (as arguments / bindings), not called. The resolver
    // now lowers these to the zero-arg `Ctor` shape; both walkers must
    // emit the same thing (LOAD_CONST NONE / VARIANT_NEW no-fields).
    let src = prog(
        "type Color\n  Black\n  White\n\n\
         fn ci(c: Color) -> Int\n    \
         match c\n        Color.Black -> 10\n        Color.White -> 20\n\n\
         fn oi(o: Option<Int>) -> Int\n    \
         match o\n        Option.None -> 0\n        Option.Some(n) -> n\n\n\
         fn run() -> Int\n    \
         ci(Color.Black) + ci(Color.White) + oi(Option.None) + oi(Option.Some(5))\n",
    );
    assert_parity("nullary_ctor", &src, "run");
}

#[test]
fn string_interpolation_buffer_intrinsics() {
    // Interpolation chains lower (via `interp_lower` deforestation) to
    // the synthesized buffer intrinsics (`__buf_new` / `__buf_append` /
    // `__buf_finalize` / `__to_str`). The MIR walker emits the same
    // BUFFER_* / CONCAT opcodes the HIR walker does; this pins parity on
    // that path.
    let src = prog(
        "fn build(n: Int, acc: String) -> String\n    \
         match n\n        0 -> acc\n        \
         _ -> build(n - 1, \"{acc}-{String.fromInt(n)}\")\n\n\
         fn run() -> String\n    build(6, \"start\")\n",
    );
    assert_parity("string_interp", &src, "run");
}

#[test]
fn bench_scenario_corpus_parity() {
    // Drive the real single-module bench programs (`depends []`, entry
    // `main`) through both walkers. These exercise the production shapes
    // (typed numeric loops, owned Vector/Map accumulators, record /
    // newtype specialization, string interpolation) on actual source.
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("bench/scenarios");
    let mut checked = 0usize;
    let mut entries: Vec<_> = std::fs::read_dir(&dir)
        .expect("read bench/scenarios")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|x| x == "av"))
        .collect();
    entries.sort();
    for path in entries {
        let src = std::fs::read_to_string(&path).expect("read scenario");
        let name = path.file_stem().unwrap().to_string_lossy().to_string();
        let (hir, mir) = run_both(&src, "main");
        assert_eq!(
            hir, mir,
            "[bench:{name}] HIR vs MIR execution divergence: HIR={hir:?} MIR={mir:?}"
        );
        checked += 1;
    }
    assert!(
        checked >= 10,
        "expected to check the bench corpus, got {checked}"
    );
}
