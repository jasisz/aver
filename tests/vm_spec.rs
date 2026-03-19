/// Spec tests for the Aver bytecode VM.
///
/// Tests compile Aver source to bytecode and execute via the VM,
/// verifying that the VM produces correct results for core language features.
use aver::ast::TopLevel;
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

/// Full pipeline: parse → tco → resolve → compile → VM execute.
/// Returns the result of calling main().
fn vm_run(src: &str) -> NanValue {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program(&items, &mut arena).expect("compile failed");
    let mut machine = vm::VM::new(code, globals, arena);
    machine.run().expect("VM execution failed")
}

/// Like vm_run but returns the arena too (for inspecting heap values).
fn vm_run_with_arena(src: &str) -> (NanValue, Arena) {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program(&items, &mut arena).expect("compile failed");
    let mut machine = vm::VM::new(code, globals, arena);
    let result = machine.run().expect("VM execution failed");
    let arena = std::mem::replace(&mut machine.arena, Arena::new());
    (result, arena)
}

// ---------------------------------------------------------------------------
// Integer arithmetic
// ---------------------------------------------------------------------------

#[test]
fn vm_int_add() {
    let result = vm_run("fn main() -> Int\n    2 + 3\n");
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 5);
}

#[test]
fn vm_int_sub() {
    let result = vm_run("fn main() -> Int\n    10 - 4\n");
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 6);
}

#[test]
fn vm_int_mul() {
    let result = vm_run("fn main() -> Int\n    3 * 7\n");
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 21);
}

#[test]
fn vm_int_div() {
    let result = vm_run("fn main() -> Int\n    15 / 4\n");
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 3);
}

// Note: Aver has no % operator. Modulo is via Int.mod() builtin.
// This test is deferred until CALL_BUILTIN is implemented.

#[test]
fn vm_int_neg() {
    let result = vm_run("fn main() -> Int\n    0 - 42\n");
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), -42);
}

#[test]
fn vm_complex_arith() {
    let result = vm_run("fn main() -> Int\n    (2 + 3) * (10 - 4)\n");
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 30);
}

// ---------------------------------------------------------------------------
// Boolean logic
// ---------------------------------------------------------------------------

#[test]
fn vm_bool_true() {
    let result = vm_run("fn main() -> Bool\n    true\n");
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn vm_bool_false() {
    let result = vm_run("fn main() -> Bool\n    false\n");
    assert!(result.is_bool());
    assert!(!result.as_bool());
}

#[test]
fn vm_bool_eq() {
    let result = vm_run("fn main() -> Bool\n    3 == 3\n");
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn vm_bool_neq() {
    let result = vm_run("fn main() -> Bool\n    3 != 4\n");
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn vm_bool_lt() {
    let result = vm_run("fn main() -> Bool\n    2 < 5\n");
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn vm_bool_gt() {
    let result = vm_run("fn main() -> Bool\n    5 > 2\n");
    assert!(result.is_bool());
    assert!(result.as_bool());
}

// ---------------------------------------------------------------------------
// Function calls
// ---------------------------------------------------------------------------

#[test]
fn vm_simple_call() {
    let src = "fn add(a: Int, b: Int) -> Int\n    a + b\n\nfn main() -> Int\n    add(10, 20)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 30);
}

#[test]
fn vm_nested_calls() {
    let src = "fn double(x: Int) -> Int\n    x * 2\n\nfn quadruple(x: Int) -> Int\n    double(double(x))\n\nfn main() -> Int\n    quadruple(5)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 20);
}

// ---------------------------------------------------------------------------
// Local bindings
// ---------------------------------------------------------------------------

#[test]
fn vm_local_binding() {
    let src = "fn main() -> Int\n    x = 10\n    y = 20\n    x + y\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 30);
}

#[test]
fn vm_binding_in_expression() {
    let src = "fn main() -> Int\n    x = 3\n    y = x * x\n    y + 1\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 10);
}

// ---------------------------------------------------------------------------
// Tuples
// ---------------------------------------------------------------------------

#[test]
fn vm_tuple_literal() {
    let (result, arena) = vm_run_with_arena("fn main() -> (Int, String)\n    (1, \"x\")\n");
    let value = result.to_value(&arena);
    assert_eq!(
        value,
        aver::value::Value::Tuple(vec![
            aver::value::Value::Int(1),
            aver::value::Value::Str("x".to_string())
        ])
    );
}

#[test]
fn vm_match_tuple_pattern_binds_values() {
    let src = "fn sum_pair(p: (Int, Int)) -> Int\n    match p\n        (a, b) -> a + b\n        _ -> 0\n\nfn main() -> Int\n    sum_pair((2, 5))\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 7);
}

#[test]
fn vm_match_nested_tuple_pattern() {
    let src = "fn flatten(p: ((Int, Int), Int)) -> Int\n    match p\n        ((a, b), c) -> a + b + c\n        _ -> 0\n\nfn main() -> Int\n    flatten(((1, 2), 3))\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 6);
}

#[test]
fn vm_tuple_pattern_arity_mismatch_falls_through() {
    let src = "fn test(p: (Int, Int)) -> Int\n    match p\n        (a, b, c) -> a + b + c\n        _ -> 42\n\nfn main() -> Int\n    test((1, 2))\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 42);
}

// ---------------------------------------------------------------------------
// Match (integer literals)
// ---------------------------------------------------------------------------

#[test]
fn vm_match_literal() {
    let src = "fn classify(n: Int) -> Int\n    match n\n        0 -> 100\n        1 -> 200\n        _ -> 999\n\nfn main() -> Int\n    classify(1)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 200);
}

#[test]
fn vm_match_wildcard() {
    let src = "fn classify(n: Int) -> Int\n    match n\n        0 -> 100\n        _ -> 999\n\nfn main() -> Int\n    classify(42)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 999);
}

#[test]
fn vm_match_binding() {
    let src = "fn echo(n: Int) -> Int\n    match n\n        x -> x + 1\n\nfn main() -> Int\n    echo(41)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 42);
}

// ---------------------------------------------------------------------------
// Match on bool (simulated if/else)
// ---------------------------------------------------------------------------

#[test]
fn vm_match_bool() {
    let src = "fn abs(n: Int) -> Int\n    match n < 0\n        true -> 0 - n\n        false -> n\n\nfn main() -> Int\n    abs(0 - 5)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 5);
}

// ---------------------------------------------------------------------------
// Result / Option constructors and match
// ---------------------------------------------------------------------------

#[test]
fn vm_result_ok_match() {
    let src = "fn unwrap(r: Result<Int, Int>) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(e) -> e\n\nfn main() -> Int\n    unwrap(Result.Ok(42))\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 42);
}

#[test]
fn vm_result_err_match() {
    let src = "fn unwrap(r: Result<Int, Int>) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(e) -> e\n\nfn main() -> Int\n    unwrap(Result.Err(99))\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 99);
}

// ---------------------------------------------------------------------------
// Tail-call optimization (self-recursion)
// ---------------------------------------------------------------------------

#[test]
fn vm_tco_self_recursion() {
    let src = "fn countdown(n: Int) -> Int\n    match n == 0\n        true -> 0\n        false -> countdown(n - 1)\n\nfn main() -> Int\n    countdown(100000)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 0);
}

#[test]
fn vm_tco_sum() {
    let src = "fn sum(n: Int, acc: Int) -> Int\n    match n == 0\n        true -> acc\n        false -> sum(n - 1, acc + n)\n\nfn main() -> Int\n    sum(100, 0)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 5050);
}

#[test]
fn vm_tco_deep() {
    // Test that TCO actually works — 1M iterations without stack overflow.
    let src = "fn loop(n: Int) -> Int\n    match n == 0\n        true -> 0\n        false -> loop(n - 1)\n\nfn main() -> Int\n    loop(1000000)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 0);
}

#[test]
fn vm_tco_reclaims_frame_local_allocations() {
    let src = "fn loop(n: Int, acc: Int) -> Int\n    tmp = (n, acc)\n    match n == 0\n        true -> acc\n        false -> match tmp\n            (x, y) -> loop(n - 1, y + 1)\n            _ -> acc\n\nfn main() -> Int\n    loop(5000, 0)\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_int());
    let empty = Arena::new();
    assert_eq!(result.as_int(&empty), 5000);
    assert!(
        arena.len() < 8,
        "tail-recursive temps should be reclaimed, arena.len() = {}",
        arena.len()
    );
}

#[test]
fn vm_tco_reclaims_previous_aggregate_args() {
    let src = "fn build(n: Int, acc: List<Int>) -> Int\n    match n == 0\n        true -> List.len(acc)\n        false -> build(n - 1, List.prepend(n, acc))\n\nfn main() -> Int\n    build(200, [])\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_int());
    let empty = Arena::new();
    assert_eq!(result.as_int(&empty), 200);
    assert!(
        arena.len() < 8,
        "tail-recursive aggregate args should not accumulate, arena.len() = {}",
        arena.len()
    );
}

#[test]
fn vm_helper_returns_feed_tail_loop_without_accumulating() {
    let src = "fn extend(acc: List<Int>, n: Int) -> List<Int>\n    List.prepend(n, acc)\n\nfn build(n: Int, acc: List<Int>) -> Int\n    match n == 0\n        true -> List.len(acc)\n        false -> build(n - 1, extend(acc, n))\n\nfn main() -> Int\n    build(200, [])\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_int());
    let empty = Arena::new();
    assert_eq!(result.as_int(&empty), 200);
    assert!(
        arena.len() < 12,
        "helper-return handoff values should not accumulate, arena.len() = {}",
        arena.len()
    );
}

// ---------------------------------------------------------------------------
// Lists
// ---------------------------------------------------------------------------

#[test]
fn vm_empty_list() {
    let result = vm_run("fn main() -> List<Int>\n    []\n");
    assert!(result.is_list());
}

#[test]
fn vm_list_literal() {
    let (result, arena) = vm_run_with_arena("fn main() -> List<Int>\n    [1, 2, 3]\n");
    assert!(result.is_list());
    assert_eq!(arena.list_len(result.arena_index()), 3);
    assert_eq!(
        arena
            .list_get(result.arena_index(), 0)
            .unwrap()
            .as_int(&arena),
        1
    );
    assert_eq!(
        arena
            .list_get(result.arena_index(), 1)
            .unwrap()
            .as_int(&arena),
        2
    );
    assert_eq!(
        arena
            .list_get(result.arena_index(), 2)
            .unwrap()
            .as_int(&arena),
        3
    );
}

#[test]
fn vm_list_match_empty() {
    let src = "fn isEmpty(xs: List<Int>) -> Bool\n    match xs\n        [] -> true\n        [h, ..t] -> false\n\nfn main() -> Bool\n    isEmpty([])\n";
    let result = vm_run(src);
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn vm_list_match_cons() {
    let src = "fn myHead(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h\n\nfn main() -> Int\n    myHead([42, 1, 2])\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 42);
}

// ---------------------------------------------------------------------------
// Records
// ---------------------------------------------------------------------------

#[test]
fn vm_record_create_and_field_access() {
    let src = "record Point\n    x: Int\n    y: Int\n\nfn main() -> Int\n    p = Point(x = 3, y = 4)\n    p.x + p.y\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 7);
}

#[test]
fn vm_record_pass_to_fn() {
    let src = "record Point\n    x: Int\n    y: Int\n\nfn sumPoint(p: Point) -> Int\n    p.x + p.y\n\nfn main() -> Int\n    sumPoint(Point(x = 10, y = 20))\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 30);
}

#[test]
fn vm_record_in_match() {
    let src = "record Pair\n    a: Int\n    b: Int\n\nfn swap(p: Pair) -> Pair\n    Pair(a = p.b, b = p.a)\n\nfn main() -> Int\n    r = swap(Pair(a = 1, b = 2))\n    r.a\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 2);
}

#[test]
fn vm_record_update() {
    let src = "record Point\n    x: Int\n    y: Int\n\nfn main() -> Int\n    p = Point(x = 1, y = 2)\n    q = Point.update(p, x = 10)\n    q.x + q.y\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 12);
}

#[test]
fn vm_record_update_multiple() {
    let src = "record Point\n    x: Int\n    y: Int\n\nfn main() -> Int\n    p = Point(x = 1, y = 2)\n    q = Point.update(p, x = 10, y = 20)\n    q.x + q.y\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 30);
}

// ---------------------------------------------------------------------------
// Variants (user-defined sum types)
// ---------------------------------------------------------------------------

#[test]
fn vm_variant_match() {
    let src = "type Shape\n    Circle(Float)\n    Square(Float)\n\nfn area(s: Shape) -> Float\n    match s\n        Shape.Circle(r) -> r * r * 3.14\n        Shape.Square(side) -> side * side\n\nfn main() -> Float\n    area(Shape.Square(5.0))\n";
    let result = vm_run(src);
    assert!(result.is_float());
    assert!((result.as_float() - 25.0).abs() < 0.001);
}

// ---------------------------------------------------------------------------
// Unit return
// ---------------------------------------------------------------------------

#[test]
fn vm_unit_return() {
    let src = "fn noop() -> Int\n    1\n\nfn main() -> Int\n    noop()\n    42\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 42);
}

// ---------------------------------------------------------------------------
// Float arithmetic
// ---------------------------------------------------------------------------

#[test]
fn vm_float_add() {
    let result = vm_run("fn main() -> Float\n    1.5 + 2.5\n");
    assert!(result.is_float());
    assert!((result.as_float() - 4.0).abs() < 0.001);
}

#[test]
fn vm_float_mul() {
    let result = vm_run("fn main() -> Float\n    3.0 * 2.0\n");
    assert!(result.is_float());
    assert!((result.as_float() - 6.0).abs() < 0.001);
}

// (debug test removed)

// ---------------------------------------------------------------------------
// Strings
// ---------------------------------------------------------------------------

#[test]
fn vm_string_literal() {
    let (result, arena) = vm_run_with_arena("fn main() -> String\n    \"hello\"\n");
    assert!(result.is_string());
    assert_eq!(arena.get_string(result.arena_index()), "hello");
}

#[test]
fn vm_string_eq() {
    let result = vm_run("fn main() -> Bool\n    \"abc\" == \"abc\"\n");
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn vm_string_neq() {
    let result = vm_run("fn main() -> Bool\n    \"abc\" != \"xyz\"\n");
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn vm_string_concat() {
    let (result, arena) = vm_run_with_arena(
        "fn greet(name: String) -> String\n    \"hello \" + name\n\nfn main() -> String\n    greet(\"world\")\n",
    );
    assert!(result.is_string());
    assert_eq!(arena.get_string(result.arena_index()), "hello world");
}

#[test]
fn vm_string_match() {
    let src = "fn check(s: String) -> Int\n    match s\n        \"yes\" -> 1\n        \"no\" -> 2\n        _ -> 0\n\nfn main() -> Int\n    check(\"no\")\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 2);
}

#[test]
fn vm_string_interpolation() {
    let (result, arena) =
        vm_run_with_arena("fn main() -> String\n    x = 42\n    \"value is {x}\"\n");
    assert!(result.is_string());
    assert_eq!(arena.get_string(result.arena_index()), "value is 42");
}

#[test]
fn vm_string_interpolation_single_expr_part() {
    let (result, arena) = vm_run_with_arena("fn main() -> String\n    x = 42\n    \"{x}\"\n");
    assert!(result.is_string());
    assert_eq!(arena.get_string(result.arena_index()), "42");
}

// ---------------------------------------------------------------------------
// Builtin calls (namespace services)
// ---------------------------------------------------------------------------

#[test]
fn vm_int_abs() {
    let src = "fn main() -> Int\n    Int.abs(0 - 42)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 42);
}

#[test]
fn vm_int_mod() {
    let src = "fn main() -> Result<Int, String>\n    Int.mod(17, 5)\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_ok());
    let inner = arena.get_boxed(result.wrapper_index());
    assert_eq!(inner.as_int(&arena), 2);
}

#[test]
fn vm_string_len() {
    let src = "fn main() -> Int\n    String.len(\"hello\")\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 5);
}

#[test]
fn vm_list_len() {
    let src = "fn main() -> Int\n    List.len([1, 2, 3])\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 3);
}

#[test]
fn vm_list_head() {
    let src = "fn main() -> Option<Int>\n    List.get([10, 20, 30], 0)\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_some());
    let inner = arena.get_boxed(result.wrapper_index());
    assert_eq!(inner.as_int(&arena), 10);
}

#[test]
fn vm_list_prepend() {
    let (result, arena) =
        vm_run_with_arena("fn main() -> List<Int>\n    List.prepend(0, [1, 2])\n");
    assert!(result.is_list());
    assert_eq!(arena.list_len(result.arena_index()), 3);
    assert_eq!(
        arena
            .list_get(result.arena_index(), 0)
            .unwrap()
            .as_int(&arena),
        0
    );
}

#[test]
fn vm_result_with_default() {
    let src = "fn main() -> Int\n    Result.withDefault(Result.Err(0), 42)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 42);
}

#[test]
fn vm_float_abs() {
    let src = "fn main() -> Float\n    Float.abs(0.0 - 2.71)\n";
    let result = vm_run(src);
    assert!(result.is_float());
    assert!((result.as_float() - 2.71).abs() < 0.001);
}

// ---------------------------------------------------------------------------
// Error propagation (? operator)
// ---------------------------------------------------------------------------

#[test]
fn vm_error_prop_ok() {
    let src = "fn safeDivide(a: Int, b: Int) -> Result<Int, String>\n    match b == 0\n        true -> Result.Err(\"div by zero\")\n        false -> Result.Ok(a / b)\n\nfn run() -> Result<Int, String>\n    x = safeDivide(10, 2)?\n    Result.Ok(x + 1)\n\nfn main() -> Result<Int, String>\n    run()\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_ok());
    let inner = arena.get_boxed(result.wrapper_index());
    assert_eq!(inner.as_int(&arena), 6);
}

#[test]
fn vm_error_prop_err() {
    let src = "fn safeDivide(a: Int, b: Int) -> Result<Int, String>\n    match b == 0\n        true -> Result.Err(\"div by zero\")\n        false -> Result.Ok(a / b)\n\nfn run() -> Result<Int, String>\n    x = safeDivide(10, 0)?\n    Result.Ok(x + 1)\n\nfn main() -> Result<Int, String>\n    run()\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_err());
    let inner = arena.get_boxed(result.wrapper_index());
    assert!(inner.is_string());
    assert_eq!(arena.get_string(inner.arena_index()), "div by zero");
}

#[test]
fn vm_error_prop_chain() {
    let src = "fn step1() -> Result<Int, String>\n    Result.Ok(10)\n\nfn step2(x: Int) -> Result<Int, String>\n    Result.Ok(x * 2)\n\nfn run() -> Result<Int, String>\n    a = step1()?\n    b = step2(a)?\n    Result.Ok(b + 1)\n\nfn main() -> Result<Int, String>\n    run()\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_ok());
    let inner = arena.get_boxed(result.wrapper_index());
    assert_eq!(inner.as_int(&arena), 21);
}

// ---------------------------------------------------------------------------
// Effect enforcement
// ---------------------------------------------------------------------------

#[test]
fn vm_effect_allowed() {
    // main declares ! [Console.print] — Console.print should work.
    let src = "fn main() -> Unit\n    ! [Console.print]\n    Console.print(42)\n";
    let _ = vm_run(src); // should not panic
}

#[test]
fn vm_effect_violation() {
    // main declares no effects — Console.print should be blocked.
    let src = "fn main() -> Unit\n    Console.print(42)\n";
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program(&items, &mut arena).expect("compile failed");
    let mut machine = vm::VM::new(code, globals, arena);
    let result = machine.run();
    assert!(result.is_err(), "should fail with effect violation");
    let err = result.unwrap_err();
    let msg = format!("{}", err);
    assert!(
        msg.contains("effect violation"),
        "error should mention effect violation, got: {}",
        msg
    );
}

// ---------------------------------------------------------------------------
// Map literals
// ---------------------------------------------------------------------------

#[test]
fn vm_map_literal() {
    let src = "fn main() -> Int\n    m = {1 => 10, 2 => 20}\n    Map.len(m)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 2);
}
