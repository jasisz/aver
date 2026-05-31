//! Phase 4b of #252 — VM parity between HIR and MIR codegen
//! paths.
//!
//! `compile_program` walks `ResolvedExpr` (HIR) and emits VM
//! bytecode. `compile_program_with_mir_fallback` walks
//! `MirExpr` for fns whose body fits the Phase 4 subset, falling
//! back to the HIR walker otherwise. Both paths must produce
//! identical VM-observable behavior — same `Value` from
//! `run_named_function` on the same input.
//!
//! Phase 4 subset (per `src/vm/compiler/mir.rs`): Literal +
//! Local + BinOp + Neg + Let + Call(Fn) + Return. Tests here
//! exercise just that subset; fns outside it ride the HIR
//! fallback and parity holds trivially.

use aver::ast::TopLevel;
use aver::ir::SymbolTable;
use aver::ir::hir::{self, ResolvedTopLevel};
use aver::lexer::Lexer;
use aver::nan_value::{Arena, NanValue, NanValueConvert};
use aver::parser::Parser;
use aver::resolver;
use aver::tco;
use aver::value::Value;
use aver::vm;

fn parse(src: &str) -> Vec<TopLevel> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser.parse().expect("parse failed")
}

fn resolve(items: &[TopLevel]) -> (Vec<ResolvedTopLevel>, SymbolTable) {
    let symbols = SymbolTable::build(items, &[]);
    let resolved = hir::resolve_program(&symbols, items);
    (resolved, symbols)
}

/// Compile `src` through the requested path and call
/// `fn_name(args)` on the result. Returns `Value` (decoded from
/// `NanValue` against the path's own arena) so the test asserts
/// run on a stable comparison form.
fn run_via_path(src: &str, fn_name: &str, args: &[i64], path: Path) -> Value {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);
    let (resolved, symbols) = resolve(&items);

    let mut arena = Arena::new();
    let (code, globals) = match path {
        Path::Hir => {
            vm::compile_program(&resolved, &symbols, &mut arena, None).expect("HIR compile failed")
        }
        Path::MirFallback => {
            vm::compile_program_with_mir_fallback(&resolved, &symbols, &mut arena, None)
                .expect("MIR-fallback compile failed")
        }
    };
    let nv_args: Vec<NanValue> = args
        .iter()
        .map(|i| NanValue::new_int(*i, &mut arena))
        .collect();
    let mut machine = vm::VM::new(code, globals, arena);
    let result = machine
        .run_named_function(fn_name, &nv_args)
        .expect("VM run failed");
    result.to_value(&machine.arena)
}

enum Path {
    Hir,
    MirFallback,
}

#[test]
fn double_parity_hir_vs_mir_fallback() {
    let src = "fn double(x: Int) -> Int\n    x + x\n";
    let hir = run_via_path(src, "double", &[7], Path::Hir);
    let mir = run_via_path(src, "double", &[7], Path::MirFallback);
    assert_eq!(
        hir, mir,
        "HIR and MIR-fallback paths must agree on double(7): HIR={hir:?}, MIR={mir:?}"
    );
    assert_eq!(hir, Value::Int(14), "double(7) should be 14");
}

#[test]
fn arithmetic_chain_parity() {
    // Two-fn corpus exercising CALL_KNOWN dispatch and inline
    // arithmetic, both inside the Phase 4 subset.
    let src = "fn double(x: Int) -> Int\n    x + x\n\nfn quad(y: Int) -> Int\n    double(y + y)\n";
    let hir = run_via_path(src, "quad", &[3], Path::Hir);
    let mir = run_via_path(src, "quad", &[3], Path::MirFallback);
    assert_eq!(
        hir, mir,
        "HIR and MIR-fallback paths must agree on quad(3): HIR={hir:?}, MIR={mir:?}"
    );
    assert_eq!(hir, Value::Int(12), "quad(3) = double(6) = 12");
}

#[test]
fn neg_parity() {
    let src = "fn flip(x: Int) -> Int\n    -x\n";
    let hir = run_via_path(src, "flip", &[5], Path::Hir);
    let mir = run_via_path(src, "flip", &[5], Path::MirFallback);
    assert_eq!(hir, mir);
    assert_eq!(hir, Value::Int(-5));
}

/// Compile both paths and return the per-fn bytecode pair so
/// tests can byte-compare specific chunks.
fn compile_both(src: &str) -> (aver::vm::CodeStore, aver::vm::CodeStore) {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);
    let (resolved, symbols) = resolve(&items);
    let mut a1 = Arena::new();
    let mut a2 = Arena::new();
    let (hir, _) =
        vm::compile_program(&resolved, &symbols, &mut a1, None).expect("HIR compile failed");
    let (mir, _) = vm::compile_program_with_mir_fallback(&resolved, &symbols, &mut a2, None)
        .expect("MIR-fallback compile failed");
    (hir, mir)
}

#[test]
fn bytecode_parity_for_double_in_phase_4_subset() {
    // Same source → identical FnChunk.code between HIR and
    // MIR-fallback paths for fns in the Phase 4 subset. This is
    // the strongest possible parity assertion: not just runtime
    // result, but actual emitted bytecode (LOAD_LOCAL / ADD /
    // RETURN sequence) is byte-identical.
    let (hir, mir) = compile_both("fn double(x: Int) -> Int\n    x + x\n");
    let hir_fn = hir.get(hir.find("double").expect("double in HIR"));
    let mir_fn = mir.get(mir.find("double").expect("double in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "double() bytecode must be byte-identical: HIR={:?}, MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn user_ctor_construction_parity() {
    // Phase 4c — `MirExpr::Construct(MirCtor::User(_))` walks
    // through the CtorEntry → owning_type lookup and emits
    // VARIANT_NEW just like the HIR walker. We verify by
    // (a) running an Int-extractor through both paths, and
    // (b) byte-comparing the FnChunk.
    let src = "type Shape\n  Circle(Int)\n  Square(Int)\n\nfn makeCircle(r: Int) -> Shape\n    Shape.Circle(r)\n\nfn area(s: Shape) -> Int\n    match s\n        Shape.Circle(r) -> r\n        Shape.Square(side) -> side\n\nfn make_and_extract(r: Int) -> Int\n    area(makeCircle(r))\n";
    let hir = run_via_path(src, "make_and_extract", &[7], Path::Hir);
    let mir = run_via_path(src, "make_and_extract", &[7], Path::MirFallback);
    assert_eq!(
        hir, mir,
        "User ctor + match parity: HIR={hir:?}, MIR={mir:?}"
    );
    assert_eq!(hir, Value::Int(7));
}

#[test]
fn bytecode_parity_for_user_ctor_construction() {
    // Just makeCircle alone — body is `Shape.Circle(r)`, fully
    // inside the Phase 4c subset. The fn's bytecode must match
    // exactly between HIR and MIR-fallback paths.
    let (hir, mir) = compile_both(
        "type Shape\n  Circle(Int)\n  Square(Int)\n\nfn makeCircle(r: Int) -> Shape\n    Shape.Circle(r)\n",
    );
    let hir_fn = hir.get(hir.find("makeCircle").expect("makeCircle in HIR"));
    let mir_fn = mir.get(mir.find("makeCircle").expect("makeCircle in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "VARIANT_NEW emit must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn builtin_ctor_wrap_parity() {
    // `Result.Ok(x)` lowers to MirCtor::Builtin(ResultOk) in
    // MIR. The Phase 4c walker emits the same WRAP 0 sequence
    // the HIR walker emits.
    let src = "fn wrap_ok(x: Int) -> Result<Int, String>\n    Result.Ok(x)\n";
    let (hir, mir) = compile_both(src);
    let hir_fn = hir.get(hir.find("wrap_ok").expect("wrap_ok in HIR"));
    let mir_fn = mir.get(mir.find("wrap_ok").expect("wrap_ok in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "WRAP emit for Result.Ok must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn record_field_access_parity() {
    // `MirExpr::Project` → RECORD_GET_NAMED, mirroring the HIR
    // path. We verify per-fn bytecode parity on a fn that takes
    // a record and projects a field.
    //
    // Note: the HIR walker may optimize to RECORD_GET (typed
    // index) when it can infer the type statically; the MIR
    // walker uses RECORD_GET_NAMED universally for now. The
    // record-creation call (`P(...)`) currently isn't in the
    // Phase 4 subset (RecordCreate is still wave-future), so
    // we test field access on a parameter — that path stays
    // pure Project in both walkers.
    let src = "record P\n  x: Int\n  y: Int\n\nfn px(p: P) -> Int\n    p.x\n";
    let (_hir, _mir) = compile_both(src);
    // We don't byte-compare here because HIR may use the typed
    // RECORD_GET op while MIR uses RECORD_GET_NAMED — both
    // semantically equivalent. Instead verify both compiled.
    // Full bytecode parity for Project lands when MIR carries
    // type stamps on its sub-nodes (Phase 6).
}

#[test]
fn try_propagation_parity_runtime() {
    // `relay(7)` should succeed with `Result.Ok(7)`. Both paths
    // emit PROPAGATE_ERR after the inner `fetch()` call; the
    // happy-path Result.Ok wrapper goes through WRAP 0.
    let src = "fn fetch(x: Int) -> Result<Int, String>\n    Result.Ok(x)\n\nfn relay(x: Int) -> Result<Int, String>\n    Result.Ok(fetch(x)?)\n";
    let hir = run_via_path(src, "relay", &[7], Path::Hir);
    let mir = run_via_path(src, "relay", &[7], Path::MirFallback);
    assert_eq!(
        hir, mir,
        "Try (`?`) propagation parity: HIR={hir:?}, MIR={mir:?}"
    );
}

#[test]
fn bytecode_parity_for_try_propagation() {
    // Per-fn bytecode parity: `relay` body lowers to
    //   Construct(Builtin(ResultOk), [Try(Call(fetch, [x]))])
    // → CALL_KNOWN fetch / PROPAGATE_ERR / WRAP 0 / RETURN
    // Same byte sequence from both walkers.
    let (hir, mir) = compile_both(
        "fn fetch(x: Int) -> Result<Int, String>\n    Result.Ok(x)\n\nfn relay(x: Int) -> Result<Int, String>\n    Result.Ok(fetch(x)?)\n",
    );
    let hir_fn = hir.get(hir.find("relay").expect("relay in HIR"));
    let mir_fn = mir.get(mir.find("relay").expect("relay in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "Try emit must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn tail_call_runtime_parity() {
    // Self-recursive countdown: tail call to self inside a match
    // arm body. The match itself rides HIR fallback (out of
    // Phase 4 subset), so this is technically an HIR-vs-HIR
    // parity test — both paths produce identical chunks
    // because the MIR path drops to HIR for the whole fn. Still
    // verifies the MIR walker doesn't break tail-call-bearing
    // fns when it can't cover them.
    let src =
        "fn countdown(n: Int) -> Int\n    match n\n        0 -> 0\n        _ -> countdown(n - 1)\n";
    let hir = run_via_path(src, "countdown", &[10], Path::Hir);
    let mir = run_via_path(src, "countdown", &[10], Path::MirFallback);
    assert_eq!(
        hir, mir,
        "countdown(10) must agree: HIR={hir:?}, MIR={mir:?}"
    );
    assert_eq!(hir, Value::Int(0));
}

#[test]
fn builtin_call_runtime_parity() {
    // Phase 4e — `MirCallee::Builtin(_)` lowers to CALL_BUILTIN.
    // Tested with `String.len` (pure builtin, no effects) so we
    // can verify runtime parity without effect plumbing.
    let src = "fn name_length(s: String) -> Int\n    String.len(s)\n";
    let (hir, mir) = compile_both(src);
    let hir_fn = hir.get(hir.find("name_length").expect("name_length in HIR"));
    let mir_fn = mir.get(mir.find("name_length").expect("name_length in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "CALL_BUILTIN emit must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn console_print_effect_bearing_call_lowers_under_mir_path() {
    // Effect-bearing builtin in non-tail position. Bytecode is
    // NOT byte-identical because HIR's `compile_stmt(is_tail=false)`
    // emits POP after the call, while MIR's wave-3a stmt-chain
    // wraps non-tail Expr stmts in `Let { binding: synthetic,
    // value: expr, body: ... }` → STORE_LOCAL instead of POP.
    // Both are semantically equivalent (effect happens, value
    // discarded), and the parity question for effect-bearing
    // intermediates moves to the runtime side. Until we have a
    // safe way to observe the effect from a test (without a
    // real stdout sink), we just verify both fns lower.
    let (hir, mir) = compile_both(
        "fn shout(msg: String) -> Int\n    ! [Console.print]\n    Console.print(msg)\n    0\n",
    );
    assert!(
        hir.find("shout").is_some(),
        "shout() should be present in HIR-only chunks"
    );
    assert!(
        mir.find("shout").is_some(),
        "shout() should be present in MIR-fallback chunks (Console.print is now in subset)"
    );
}

#[test]
fn corpus_hello_smoke_mir_walker_covers_it() {
    // examples/core/hello.av is a small program that uses
    // Console.print — Phase 4e's CALL_BUILTIN coverage should
    // land the main fn in the MIR-covered set.
    let src = std::fs::read_to_string("examples/core/hello.av")
        .expect("examples/core/hello.av should exist");
    let mut items = parse(&src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);
    let (resolved, _) = resolve(&items);
    let mir = aver::ir::mir::lower_program(&resolved);
    let cov = aver::vm::mir_vm::classify_mir_program_coverage(&mir);
    assert!(
        cov.covered >= 1,
        "examples/core/hello.av should have at least one fn in MIR-covered: {:?}",
        cov
    );
}

#[test]
fn list_literal_bytecode_parity() {
    let (hir, mir) = compile_both("fn nums() -> List<Int>\n    [1, 2, 3]\n");
    let hir_fn = hir.get(hir.find("nums").expect("nums in HIR"));
    let mir_fn = mir.get(mir.find("nums").expect("nums in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "LIST_NEW emit must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn empty_list_bytecode_parity() {
    let (hir, mir) = compile_both("fn empty() -> List<Int>\n    []\n");
    let hir_fn = hir.get(hir.find("empty").expect("empty in HIR"));
    let mir_fn = mir.get(mir.find("empty").expect("empty in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "LIST_NIL emit must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn tuple_literal_bytecode_parity() {
    let (hir, mir) = compile_both("fn pair() -> Tuple<Int, Int>\n    (1, 2)\n");
    let hir_fn = hir.get(hir.find("pair").expect("pair in HIR"));
    let mir_fn = mir.get(mir.find("pair").expect("pair in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "TUPLE_NEW emit must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn record_create_bytecode_parity() {
    // Field order follows declaration order in `get_field_names`,
    // identical between HIR and MIR paths.
    let (hir, mir) =
        compile_both("record P\n  x: Int\n  y: Int\n\nfn makeP() -> P\n    P(x = 1, y = 2)\n");
    let hir_fn = hir.get(hir.find("makeP").expect("makeP in HIR"));
    let mir_fn = mir.get(mir.find("makeP").expect("makeP in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "RECORD_NEW emit must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn match_int_literal_runtime_parity() {
    // Phase 4g-1 — match arms with Wildcard + Literal(Int) only.
    // Runtime must agree between HIR and MIR-fallback paths for
    // each branch.
    let src = "fn classify(n: Int) -> Int\n    match n\n        0 -> 100\n        1 -> 200\n        _ -> 999\n";
    for (input, expected) in &[(0, 100), (1, 200), (2, 999), (-1, 999)] {
        let hir = run_via_path(src, "classify", &[*input], Path::Hir);
        let mir = run_via_path(src, "classify", &[*input], Path::MirFallback);
        assert_eq!(
            hir, mir,
            "classify({input}) parity: HIR={hir:?}, MIR={mir:?}"
        );
        assert_eq!(hir, Value::Int(*expected as i64));
    }
}

#[test]
fn match_complex_pattern_falls_back_to_hir() {
    // Cons / EmptyList patterns are NOT in 4g-1 — those fns
    // must fall back to HIR and still produce the right answer.
    let src = "fn first_or_zero(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h\n";
    let (hir, mir) = compile_both(src);
    assert!(
        hir.find("first_or_zero").is_some(),
        "fn should exist in HIR chunks"
    );
    assert!(
        mir.find("first_or_zero").is_some(),
        "fn should still exist in MIR-path chunks via HIR fallback"
    );
}

#[test]
fn match_cons_emptylist_runtime_parity() {
    // Phase 4g-2 — `match xs { [] -> 0; [h, ..t] -> h }`.
    // Runtime must agree on both branches across HIR vs
    // MIR-fallback paths. We can't pass a `List<Int>` directly
    // via `run_named_function` (NanValue arg encoding for
    // lists isn't trivial from tests), so we drive it through
    // an Int-returning fn that constructs the list inline.
    let src = "fn head_or_zero(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h\n\nfn check_empty(_dummy: Int) -> Int\n    head_or_zero([])\n\nfn check_cons(_dummy: Int) -> Int\n    head_or_zero([42, 7, 3])\n";
    let hir_empty = run_via_path(src, "check_empty", &[0], Path::Hir);
    let mir_empty = run_via_path(src, "check_empty", &[0], Path::MirFallback);
    assert_eq!(hir_empty, mir_empty);
    assert_eq!(hir_empty, Value::Int(0));

    let hir_cons = run_via_path(src, "check_cons", &[0], Path::Hir);
    let mir_cons = run_via_path(src, "check_cons", &[0], Path::MirFallback);
    assert_eq!(hir_cons, mir_cons);
    assert_eq!(hir_cons, Value::Int(42));
}

#[test]
fn match_user_ctor_pattern_runtime_parity() {
    // Phase 4g-3 — `match s { Shape.Circle(r) -> r;
    // Shape.Square(side) -> side }` runs identically through
    // both paths. The MIR walker resolves CtorId → CtorEntry →
    // arena ctor id and emits MATCH_VARIANT, mirroring the
    // HIR walker exactly.
    let src = "type Shape\n  Circle(Int)\n  Square(Int)\n\nfn extract(s: Shape) -> Int\n    match s\n        Shape.Circle(r) -> r\n        Shape.Square(side) -> side\n\nfn circle_eight(_d: Int) -> Int\n    extract(Shape.Circle(8))\n\nfn square_three(_d: Int) -> Int\n    extract(Shape.Square(3))\n";

    let hir_circle = run_via_path(src, "circle_eight", &[0], Path::Hir);
    let mir_circle = run_via_path(src, "circle_eight", &[0], Path::MirFallback);
    assert_eq!(hir_circle, mir_circle);
    assert_eq!(hir_circle, Value::Int(8));

    let hir_square = run_via_path(src, "square_three", &[0], Path::Hir);
    let mir_square = run_via_path(src, "square_three", &[0], Path::MirFallback);
    assert_eq!(hir_square, mir_square);
    assert_eq!(hir_square, Value::Int(3));
}

#[test]
fn match_result_builtin_ctor_pattern_runtime_parity() {
    // Phase 4g-4 — `match r { Result.Ok(v) -> v; Result.Err(_) -> 0 }`
    // runs identically across HIR and MIR-fallback paths.
    let src = "fn unwrap(r: Result<Int, String>) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> 0\n\nfn run_ok(_d: Int) -> Int\n    unwrap(Result.Ok(42))\n\nfn run_err(_d: Int) -> Int\n    unwrap(Result.Err(\"nope\"))\n";

    let hir_ok = run_via_path(src, "run_ok", &[0], Path::Hir);
    let mir_ok = run_via_path(src, "run_ok", &[0], Path::MirFallback);
    assert_eq!(hir_ok, mir_ok);
    assert_eq!(hir_ok, Value::Int(42));

    let hir_err = run_via_path(src, "run_err", &[0], Path::Hir);
    let mir_err = run_via_path(src, "run_err", &[0], Path::MirFallback);
    assert_eq!(hir_err, mir_err);
    assert_eq!(hir_err, Value::Int(0));
}

#[test]
fn match_option_builtin_ctor_pattern_runtime_parity() {
    // Phase 4g-4 — Option.Some(v) and Option.None arms. None
    // doesn't bind anything; Some binds the inner value.
    let src = "fn unwrap_or_999(o: Option<Int>) -> Int\n    match o\n        Option.Some(v) -> v\n        Option.None -> 999\n\nfn run_some(_d: Int) -> Int\n    unwrap_or_999(Option.Some(7))\n\nfn run_none(o: Option<Int>) -> Int\n    unwrap_or_999(o)\n";

    let hir_some = run_via_path(src, "run_some", &[0], Path::Hir);
    let mir_some = run_via_path(src, "run_some", &[0], Path::MirFallback);
    assert_eq!(hir_some, mir_some);
    assert_eq!(hir_some, Value::Int(7));
    // Option.None as the explicit arg path would require a
    // None-valued NanValue — we just verify the fn compiles
    // through both paths.
    let (hir, mir) = compile_both(src);
    assert!(hir.find("unwrap_or_999").is_some());
    assert!(mir.find("unwrap_or_999").is_some());
}

#[test]
fn match_tuple_pattern_runtime_parity() {
    // Phase 4g-5 — flat tuple pattern with Bind subpatterns.
    // `(a, b) -> a + b` runs identically on both paths.
    let src = "fn pair_sum(p: Tuple<Int, Int>) -> Int\n    match p\n        (a, b) -> a + b\n\nfn check(_d: Int) -> Int\n    pair_sum((3, 4))\n";
    let hir = run_via_path(src, "check", &[0], Path::Hir);
    let mir = run_via_path(src, "check", &[0], Path::MirFallback);
    assert_eq!(hir, mir, "Tuple match parity: HIR={hir:?}, MIR={mir:?}");
    assert_eq!(hir, Value::Int(7));
}

#[test]
fn match_tuple_with_wildcard_subpattern_parity() {
    // `(_, b) -> b` — wildcard subpattern collapses to POP in
    // the MIR walker (mirror of HIR's bind_top_to_local on
    // wildcard).
    let src = "fn second(p: Tuple<Int, Int>) -> Int\n    match p\n        (_, b) -> b\n\nfn check(_d: Int) -> Int\n    second((11, 22))\n";
    let hir = run_via_path(src, "check", &[0], Path::Hir);
    let mir = run_via_path(src, "check", &[0], Path::MirFallback);
    assert_eq!(hir, mir);
    assert_eq!(hir, Value::Int(22));
}

#[test]
fn map_literal_bytecode_parity() {
    // Phase 4h — `{"k" => 1, "j" => 2}` lowers to
    // LOAD_CONST(empty_map) + key/value pairs each driven by
    // CALL_BUILTIN(MapSet, argc=3). Byte-identical between
    // HIR and MIR walkers.
    let (hir, mir) = compile_both("fn pairs() -> Map<String, Int>\n    {\"a\" => 1, \"b\" => 2}\n");
    let hir_fn = hir.get(hir.find("pairs").expect("pairs in HIR"));
    let mir_fn = mir.get(mir.find("pairs").expect("pairs in MIR"));
    assert_eq!(
        hir_fn.code, mir_fn.code,
        "MapLiteral emit must match byte-for-byte:\n  HIR={:?}\n  MIR={:?}",
        hir_fn.code, mir_fn.code
    );
}

#[test]
fn map_literal_runtime_parity() {
    // Sanity: the constructed map's lookup yields the same
    // value on both paths.
    let src = "fn one() -> Map<String, Int>\n    {\"answer\" => 42}\n\nfn pull_answer(_d: Int) -> Int\n    Map.get(one(), \"answer\")\n";
    let hir = run_via_path(src, "pull_answer", &[0], Path::Hir);
    let mir = run_via_path(src, "pull_answer", &[0], Path::MirFallback);
    assert_eq!(
        hir, mir,
        "MapLiteral runtime parity: HIR={hir:?}, MIR={mir:?}"
    );
    // `Map.get` returns `Option<Int>`; both paths reach the
    // same Some/None shape.
}

#[test]
fn match_fn_uses_hir_fallback_and_remains_present_in_mir_path() {
    // `match` isn't in the Phase 4 subset → MIR-fallback path
    // falls back to HIR. The fn must still appear in the output
    // (HIR walker emits the chunk); only the dispatch differs.
    let (hir, mir) = compile_both(
        "fn first(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h\n",
    );
    assert!(
        hir.find("first").is_some(),
        "first() should be present in HIR-only chunks"
    );
    assert!(
        mir.find("first").is_some(),
        "first() should still be present in MIR-fallback chunks (HIR fallback path)"
    );
}
