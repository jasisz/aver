//! MIR-VM codegen tests — #252 Phase 6.
//!
//! These started life (Phase 4b) as HIR-vs-MIR *parity* tests: compile
//! the same source through the HIR walker and the MIR-default walker and
//! assert identical VM-observable behavior + byte-identical bytecode.
//! The migration is complete and the HIR walker is retired, so the
//! "parity" framing is gone. What remains is the half worth keeping:
//!
//!   * **runtime goldens** — a shape compiles via the MIR path and runs
//!     to the expected `Value`. Captured while the two walkers were
//!     proven equal, so they pin the MIR/VM codegen output directly.
//!   * **opcode-emission invariants** — the MIR walker emits the
//!     *specialised* opcode for a shape (typed `ADD_INT` not generic
//!     `ADD`, `MOVE_LOCAL` on a last read, `MATCH_DISPATCH_CONST` for a
//!     literal-only match, the single-opcode builtin forms). These are
//!     unique to this file; nothing else asserts VM opcode selection.
//!
//! Independent-implementation cross-validation lives in the wasm-gc
//! suites and `aver verify --wasm-gc`, not in a second VM walker.
//!
//! These drive the lighter compile path (`parse` + `tco` + `resolve`,
//! no full typecheck) so a fn's body can be exercised in isolation; the
//! full-pipeline goldens (incl. `last_use`) live in
//! `tests/hir_mir_differential.rs`.

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
use aver::vm::opcode;

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

/// Compile `src` via the MIR-default VM path and call `fn_name(args)`.
/// Returns `Value` (decoded against the run's own arena) for a stable
/// comparison form.
fn run_mir(src: &str, fn_name: &str, args: &[i64]) -> Value {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);
    let (resolved, symbols) = resolve(&items);

    let mut arena = Arena::new();
    let (code, globals) =
        vm::compile_program_with_mir_fallback(&resolved, &symbols, &mut arena, None)
            .expect("MIR compile failed");
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

/// Compile `src` via the MIR-default VM path through the FULL pipeline
/// (typecheck + analyze + `last_use`) and return the `CodeStore` so a
/// test can inspect a fn's emitted bytecode. The full pipeline is
/// required for the opcode-emission invariants: type stamps drive the
/// typed-arith / typed-compare opcodes and the `last_use` pass drives
/// `MOVE_LOCAL`. (The lighter `run_mir` path skips typecheck so loosely
/// typed runtime fixtures still execute; codegen-shape assertions need
/// the production passes.)
fn compile_mir(src: &str) -> aver::vm::CodeStore {
    use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
    let mut items = aver::source::parse_source(src).expect("parse failed");
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
    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, _) = vm::compile_program_with_mir_fallback(
        &result.resolved_items,
        &result.symbol_table,
        &mut arena,
        result.analysis.as_ref(),
    )
    .expect("MIR compile failed");
    code
}

fn fn_code<'a>(cs: &'a aver::vm::CodeStore, name: &str) -> &'a [u8] {
    let id = cs
        .find(name)
        .unwrap_or_else(|| panic!("{name} not in compiled chunks"));
    &cs.get(id).code
}

/// Assert the MIR walker selected the specialised opcode `op` for the
/// body of `fn_name`. A presence check (not byte-equality) is the right
/// granularity now that there's no second walker to compare against: it
/// pins *opcode selection* (the regression we care about — falling back
/// to a generic form) without freezing operand encoding.
fn assert_emits(src: &str, fn_name: &str, op: u8, label: &str) {
    let cs = compile_mir(src);
    let code = fn_code(&cs, fn_name);
    assert!(
        code.contains(&op),
        "{fn_name} must emit {label} (0x{op:02X}); code={code:?}"
    );
}

// ── Runtime goldens ─────────────────────────────────────────────────

#[test]
fn double_runtime() {
    assert_eq!(
        run_mir("fn double(x: Int) -> Int\n    x + x\n", "double", &[7]),
        Value::Int(14)
    );
}

#[test]
fn arithmetic_chain_runtime() {
    // Two-fn corpus exercising CALL_KNOWN dispatch + inline arithmetic.
    let src = "fn double(x: Int) -> Int\n    x + x\n\nfn quad(y: Int) -> Int\n    double(y + y)\n";
    assert_eq!(
        run_mir(src, "quad", &[3]),
        Value::Int(12),
        "quad(3) = double(6) = 12"
    );
}

#[test]
fn neg_runtime() {
    assert_eq!(
        run_mir("fn flip(x: Int) -> Int\n    -x\n", "flip", &[5]),
        Value::Int(-5)
    );
}

#[test]
fn recursive_non_tail_call_runtime() {
    // fib's recursive arms are non-tail known calls (plain CALL_KNOWN —
    // the MIR walker no longer emits CALL_KNOWN_OWNED for user-fn calls;
    // see tests/mir_call_known_owned_regression.rs).
    let src = "fn fib(n: Int) -> Int\n    match n < 2\n        true -> n\n        false -> fib(n - 1) + fib(n - 2)\n";
    assert_eq!(run_mir(src, "fib", &[10]), Value::Int(55), "fib(10) = 55");
}

#[test]
fn vector_compound_leaf_op_runtime() {
    // `Option.withDefault(Vector.set(v, i, x), v)` /
    // `Option.withDefault(Vector.get(v, i), <lit>)` fuse to the single
    // VECTOR_SET_OR_KEEP / VECTOR_GET_OR opcodes; this pins behavior.
    let src = "fn build(n: Int) -> Int\n    v = Vector.new(3, 0)\n    w = Option.withDefault(Vector.set(v, 0, n), v)\n    Option.withDefault(Vector.get(w, 0), 0)\n";
    assert_eq!(
        run_mir(src, "build", &[7]),
        Value::Int(7),
        "set slot 0 to 7, read it back"
    );
}

#[test]
fn user_ctor_construction_runtime() {
    // `MirExpr::Construct(MirCtor::User(_))` → VARIANT_NEW, then a match
    // extracts the field.
    let src = "type Shape\n  Circle(Int)\n  Square(Int)\n\nfn makeCircle(r: Int) -> Shape\n    Shape.Circle(r)\n\nfn area(s: Shape) -> Int\n    match s\n        Shape.Circle(r) -> r\n        Shape.Square(side) -> side\n\nfn make_and_extract(r: Int) -> Int\n    area(makeCircle(r))\n";
    assert_eq!(run_mir(src, "make_and_extract", &[7]), Value::Int(7));
}

#[test]
fn try_propagation_runtime() {
    // `relay(7)` succeeds: `Result.Ok(fetch(7)?)` → Ok(7). PROPAGATE_ERR
    // after the inner call, WRAP 0 on the happy path.
    let src = "fn fetch(x: Int) -> Result<Int, String>\n    Result.Ok(x)\n\nfn relay(x: Int) -> Result<Int, String>\n    Result.Ok(fetch(x)?)\n";
    assert_eq!(format!("{:?}", run_mir(src, "relay", &[7])), "Ok(Int(7))");
}

#[test]
fn tail_call_runtime() {
    let src =
        "fn countdown(n: Int) -> Int\n    match n\n        0 -> 0\n        _ -> countdown(n - 1)\n";
    assert_eq!(run_mir(src, "countdown", &[10]), Value::Int(0));
}

#[test]
fn match_int_literal_runtime() {
    let src = "fn classify(n: Int) -> Int\n    match n\n        0 -> 100\n        1 -> 200\n        _ -> 999\n";
    for (input, expected) in &[(0, 100), (1, 200), (2, 999), (-1, 999)] {
        assert_eq!(
            run_mir(src, "classify", &[*input]),
            Value::Int(*expected as i64)
        );
    }
}

#[test]
fn match_cons_emptylist_runtime() {
    // `match xs { [] -> 0; [h, ..t] -> h }` — list cons/empty patterns
    // lower natively via MIR (no HIR fallback). Driven through
    // Int-returning fns that build the list inline.
    let src = "fn head_or_zero(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h\n\nfn check_empty(_dummy: Int) -> Int\n    head_or_zero([])\n\nfn check_cons(_dummy: Int) -> Int\n    head_or_zero([42, 7, 3])\n";
    assert_eq!(run_mir(src, "check_empty", &[0]), Value::Int(0));
    assert_eq!(run_mir(src, "check_cons", &[0]), Value::Int(42));
}

#[test]
fn match_user_ctor_pattern_runtime() {
    let src = "type Shape\n  Circle(Int)\n  Square(Int)\n\nfn extract(s: Shape) -> Int\n    match s\n        Shape.Circle(r) -> r\n        Shape.Square(side) -> side\n\nfn circle_eight(_d: Int) -> Int\n    extract(Shape.Circle(8))\n\nfn square_three(_d: Int) -> Int\n    extract(Shape.Square(3))\n";
    assert_eq!(run_mir(src, "circle_eight", &[0]), Value::Int(8));
    assert_eq!(run_mir(src, "square_three", &[0]), Value::Int(3));
}

#[test]
fn match_result_builtin_ctor_pattern_runtime() {
    let src = "fn unwrap(r: Result<Int, String>) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> 0\n\nfn run_ok(_d: Int) -> Int\n    unwrap(Result.Ok(42))\n\nfn run_err(_d: Int) -> Int\n    unwrap(Result.Err(\"nope\"))\n";
    assert_eq!(run_mir(src, "run_ok", &[0]), Value::Int(42));
    assert_eq!(run_mir(src, "run_err", &[0]), Value::Int(0));
}

#[test]
fn match_option_builtin_ctor_pattern_runtime() {
    let src = "fn unwrap_or_999(o: Option<Int>) -> Int\n    match o\n        Option.Some(v) -> v\n        Option.None -> 999\n\nfn run_some(_d: Int) -> Int\n    unwrap_or_999(Option.Some(7))\n\nfn run_none(o: Option<Int>) -> Int\n    unwrap_or_999(o)\n";
    assert_eq!(run_mir(src, "run_some", &[0]), Value::Int(7));
    // None-valued arg would need a None NanValue from the test; just
    // confirm the fn compiles via the MIR path.
    assert!(compile_mir(src).find("unwrap_or_999").is_some());
}

#[test]
fn match_tuple_pattern_runtime() {
    let src = "fn pair_sum(p: Tuple<Int, Int>) -> Int\n    match p\n        (a, b) -> a + b\n\nfn check(_d: Int) -> Int\n    pair_sum((3, 4))\n";
    assert_eq!(run_mir(src, "check", &[0]), Value::Int(7));
}

#[test]
fn match_tuple_with_wildcard_subpattern_runtime() {
    // `(_, b) -> b` — wildcard subpattern collapses to POP.
    let src = "fn second(p: Tuple<Int, Int>) -> Int\n    match p\n        (_, b) -> b\n\nfn check(_d: Int) -> Int\n    second((11, 22))\n";
    assert_eq!(run_mir(src, "check", &[0]), Value::Int(22));
}

#[test]
fn map_literal_runtime() {
    // The constructed map's lookup yields the expected value.
    let src = "fn one() -> Map<String, Int>\n    {\"answer\" => 42}\n\nfn pull_answer(_d: Int) -> Int\n    Map.get(one(), \"answer\")\n";
    assert_eq!(
        format!("{:?}", run_mir(src, "pull_answer", &[0])),
        "Some(Int(42))"
    );
}

#[test]
fn match_nested_tuple_subpattern_runtime() {
    // Nested structural subpatterns inside a Tuple: EmptyList / Cons /
    // Literal subpatterns.
    let src = "fn classify(p: Tuple<List<Int>, Int>) -> Int\n    match p\n        ([], 0) -> 100\n        ([], _) -> 200\n        ([_, ..t], 0) -> 300\n        _ -> 400\n\nfn run_empty_zero(_d: Int) -> Int\n    classify(([], 0))\n\nfn run_empty_other(_d: Int) -> Int\n    classify(([], 7))\n\nfn run_cons_zero(_d: Int) -> Int\n    classify(([1, 2], 0))\n\nfn run_cons_other(_d: Int) -> Int\n    classify(([1, 2], 9))\n";
    for (fn_name, expected) in &[
        ("run_empty_zero", 100),
        ("run_empty_other", 200),
        ("run_cons_zero", 300),
        ("run_cons_other", 400),
    ] {
        assert_eq!(run_mir(src, fn_name, &[0]), Value::Int(*expected));
    }
}

#[test]
fn match_top_level_ident_bind_runtime() {
    let src = "fn shift(n: Int) -> Int\n    match n\n        x -> x + 1\n";
    assert_eq!(run_mir(src, "shift", &[7]), Value::Int(8));
}

#[test]
fn match_non_int_literal_pattern_runtime() {
    // Non-Int literal pattern (Bool): generic DUP + LOAD_CONST + EQ +
    // JUMP_IF_FALSE path.
    let src = "fn say(b: Bool) -> Int\n    match b\n        true -> 1\n        false -> 0\n\nfn t(_d: Int) -> Int\n    say(true)\n\nfn f(_d: Int) -> Int\n    say(false)\n";
    assert_eq!(run_mir(src, "t", &[0]), Value::Int(1));
    assert_eq!(run_mir(src, "f", &[0]), Value::Int(0));
}

#[test]
fn match_dispatch_const_runtime() {
    let src = "fn classify(n: Int) -> Int\n    match n\n        0 -> 100\n        1 -> 200\n        _ -> 999\n";
    for (input, expected) in &[(0, 100), (1, 200), (2, 999), (-1, 999)] {
        assert_eq!(
            run_mir(src, "classify", &[*input]),
            Value::Int(*expected as i64)
        );
    }
}

#[test]
fn match_non_dispatchable_arm_runtime() {
    // Arm body is an expression, not a const literal — the dispatch
    // fast-path aborts and the walker falls through to linear emit.
    let src = "fn double_or_zero(n: Int) -> Int\n    match n\n        0 -> 0\n        _ -> n + n\n";
    for (input, expected) in &[(0, 0), (3, 6), (7, 14)] {
        assert_eq!(
            run_mir(src, "double_or_zero", &[*input]),
            Value::Int(*expected as i64)
        );
    }
}

#[test]
fn last_use_runtime() {
    assert_eq!(
        run_mir("fn double(x: Int) -> Int\n    x + x\n", "double", &[7]),
        Value::Int(14)
    );
}

// ── Opcode-emission invariants ──────────────────────────────────────

#[test]
fn user_ctor_emits_variant_new() {
    assert_emits(
        "type Shape\n  Circle(Int)\n  Square(Int)\n\nfn makeCircle(r: Int) -> Shape\n    Shape.Circle(r)\n",
        "makeCircle",
        opcode::VARIANT_NEW,
        "VARIANT_NEW",
    );
}

#[test]
fn builtin_ctor_emits_wrap() {
    assert_emits(
        "fn wrap_ok(x: Int) -> Result<Int, String>\n    Result.Ok(x)\n",
        "wrap_ok",
        opcode::WRAP,
        "WRAP",
    );
}

#[test]
fn record_field_access_emits_record_get_named() {
    // `MirExpr::Project` → RECORD_GET_NAMED. (Field access on a param,
    // so no RecordCreate in the body.)
    assert_emits(
        "record P\n  x: Int\n  y: Int\n\nfn px(p: P) -> Int\n    p.x\n",
        "px",
        opcode::RECORD_GET_NAMED,
        "RECORD_GET_NAMED",
    );
}

#[test]
fn try_propagation_emits_propagate_err() {
    assert_emits(
        "fn fetch(x: Int) -> Result<Int, String>\n    Result.Ok(x)\n\nfn relay(x: Int) -> Result<Int, String>\n    Result.Ok(fetch(x)?)\n",
        "relay",
        opcode::PROPAGATE_ERR,
        "PROPAGATE_ERR",
    );
}

#[test]
fn builtin_call_emits_call_builtin() {
    // `String.len` is a non-specialised builtin → a CALL_BUILTIN-family
    // opcode (the `_OWNED` variant when the arg is the param's last use,
    // which the full pipeline's ownership analysis detects here), as
    // opposed to a dedicated single-opcode form like LIST_LEN.
    let cs = compile_mir("fn name_length(s: String) -> Int\n    String.len(s)\n");
    let code = fn_code(&cs, "name_length");
    assert!(
        code.contains(&opcode::CALL_BUILTIN) || code.contains(&opcode::CALL_BUILTIN_OWNED),
        "name_length must emit a CALL_BUILTIN-family opcode; code={code:?}"
    );
}

#[test]
fn console_print_effect_bearing_call_lowers_under_mir_path() {
    // Effect-bearing builtin in non-tail position lowers via MIR (the
    // non-tail Expr stmt becomes a `Let` with a synthetic binding).
    let cs = compile_mir(
        "fn shout(msg: String) -> Int\n    ! [Console.print]\n    Console.print(msg)\n    0\n",
    );
    assert!(
        cs.find("shout").is_some(),
        "shout() must lower via the MIR path"
    );
}

#[test]
fn list_literal_emits_list_new() {
    assert_emits(
        "fn nums() -> List<Int>\n    [1, 2, 3]\n",
        "nums",
        opcode::LIST_NEW,
        "LIST_NEW",
    );
}

#[test]
fn empty_list_emits_list_nil() {
    assert_emits(
        "fn empty() -> List<Int>\n    []\n",
        "empty",
        opcode::LIST_NIL,
        "LIST_NIL",
    );
}

#[test]
fn tuple_literal_emits_tuple_new() {
    assert_emits(
        "fn pair() -> Tuple<Int, Int>\n    (1, 2)\n",
        "pair",
        opcode::TUPLE_NEW,
        "TUPLE_NEW",
    );
}

#[test]
fn record_create_emits_record_new() {
    assert_emits(
        "record P\n  x: Int\n  y: Int\n\nfn makeP() -> P\n    P(x = 1, y = 2)\n",
        "makeP",
        opcode::RECORD_NEW,
        "RECORD_NEW",
    );
}

#[test]
fn map_literal_emits_call_builtin() {
    // `{"a" => 1, "b" => 2}` builds via CALL_BUILTIN(MapSet) per entry.
    assert_emits(
        "fn pairs() -> Map<String, Int>\n    {\"a\" => 1, \"b\" => 2}\n",
        "pairs",
        opcode::CALL_BUILTIN,
        "CALL_BUILTIN",
    );
}

#[test]
fn typed_int_arith_emits_typed_opcodes() {
    // Type stamps drive the BinOp branch to ADD_INT / SUB_INT / MUL_INT
    // instead of the generic ADD / SUB / MUL.
    let src = "fn arith(a: Int, b: Int) -> Int\n    a + b - a * b\n";
    let cs = compile_mir(src);
    let code = fn_code(&cs, "arith");
    for (op, label) in &[
        (opcode::ADD_INT, "ADD_INT"),
        (opcode::SUB_INT, "SUB_INT"),
        (opcode::MUL_INT, "MUL_INT"),
    ] {
        assert!(code.contains(op), "arith must emit {label}; code={code:?}");
    }
}

#[test]
fn typed_float_arith_emits_add_float() {
    assert_emits(
        "fn farith(a: Float, b: Float) -> Float\n    a + b - a * b / b\n",
        "farith",
        opcode::ADD_FLOAT,
        "ADD_FLOAT",
    );
}

#[test]
fn typed_int_compare_emits_eq_int() {
    assert_emits(
        "fn cmp(a: Int, b: Int) -> Bool\n    a == b\n",
        "cmp",
        opcode::EQ_INT,
        "EQ_INT",
    );
}

#[test]
fn typed_neg_float_emits_neg_float() {
    assert_emits(
        "fn flip(x: Float) -> Float\n    -x\n",
        "flip",
        opcode::NEG_FLOAT,
        "NEG_FLOAT",
    );
}

#[test]
fn match_dispatch_const_table_emits_dispatch() {
    assert_emits(
        "fn classify(n: Int) -> Int\n    match n\n        0 -> 100\n        1 -> 200\n        _ -> 999\n",
        "classify",
        opcode::MATCH_DISPATCH_CONST,
        "MATCH_DISPATCH_CONST",
    );
}

#[test]
fn specialised_builtin_list_len_emits_list_len() {
    assert_emits(
        "fn count(xs: List<Int>) -> Int\n    List.len(xs)\n",
        "count",
        opcode::LIST_LEN,
        "LIST_LEN",
    );
}

#[test]
fn specialised_builtin_list_prepend_emits_list_prepend() {
    assert_emits(
        "fn cons(x: Int, xs: List<Int>) -> List<Int>\n    List.prepend(x, xs)\n",
        "cons",
        opcode::LIST_PREPEND,
        "LIST_PREPEND",
    );
}

#[test]
fn specialised_builtin_option_with_default_emits_unwrap_or() {
    assert_emits(
        "fn unwrap(o: Option<Int>, fallback: Int) -> Int\n    Option.withDefault(o, fallback)\n",
        "unwrap",
        opcode::UNWRAP_OR,
        "UNWRAP_OR",
    );
}

#[test]
fn specialised_builtin_result_with_default_emits_unwrap_result_or() {
    assert_emits(
        "fn unwrap(r: Result<Int, String>, fallback: Int) -> Int\n    Result.withDefault(r, fallback)\n",
        "unwrap",
        opcode::UNWRAP_RESULT_OR,
        "UNWRAP_RESULT_OR",
    );
}

#[test]
fn last_use_emits_move_local() {
    // `x + x`: the final read of `x` emits MOVE_LOCAL (skips the
    // ref-count bump so the VM yard can reclaim the slot).
    assert_emits(
        "fn double(x: Int) -> Int\n    x + x\n",
        "double",
        opcode::MOVE_LOCAL,
        "MOVE_LOCAL",
    );
}

#[test]
fn add_int_emitted_for_double() {
    assert_emits(
        "fn double(x: Int) -> Int\n    x + x\n",
        "double",
        opcode::ADD_INT,
        "ADD_INT",
    );
}

// ── MIR-coverage smoke ──────────────────────────────────────────────

#[test]
fn corpus_hello_smoke_mir_walker_covers_it() {
    // examples/core/hello.av uses Console.print; the MIR walker's
    // CALL_BUILTIN coverage lands the main fn in the covered set.
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
