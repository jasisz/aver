/// Spec tests for the Aver bytecode VM.
///
/// Tests compile Aver source to bytecode and execute via the VM,
/// verifying that the VM produces correct results for core language features.
use std::path::{Path, PathBuf};

use aver::ast::TopLevel;
use aver::config::ProjectConfig;
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

fn vm_run_with_module_root_and_arena(src: &str, module_root: &Path) -> (NanValue, Arena) {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program_with_modules(
        &items,
        &mut arena,
        Some(
            module_root
                .to_str()
                .expect("module root must be valid UTF-8"),
        ),
        "",
    )
    .expect("compile failed");
    let mut machine = vm::VM::new(code, globals, arena);
    let result = machine.run().expect("VM execution failed");
    let arena = std::mem::replace(&mut machine.arena, Arena::new());
    (result, arena)
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

fn vm_machine(src: &str) -> vm::VM {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program(&items, &mut arena).expect("compile failed");
    vm::VM::new(code, globals, arena)
}

fn vm_compile(src: &str) -> vm::CodeStore {
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    let (code, _globals) = vm::compile_program(&items, &mut arena).expect("compile failed");
    code
}

fn temp_module_root(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "aver_vm_spec_{}_{}_{}",
        tag,
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time went backwards")
            .as_nanos()
    ));
    std::fs::create_dir_all(&dir).expect("create temp module root");
    dir
}

fn nv_string(machine: &mut vm::VM, s: &str) -> NanValue {
    NanValue::new_string_value(s, &mut machine.arena)
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

#[test]
fn vm_string_lt() {
    let result = vm_run("fn main() -> Bool\n    \"2025-01-01\" < \"2026-01-01\"\n");
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

#[test]
fn vm_marks_small_pure_helpers_as_thin() {
    let src = "fn inc(x: Int) -> Int\n    x + 1\n\nfn dec(x: Int) -> Int\n    x - 1\n\nfn bounce(x: Int) -> Int\n    dec(inc(x))\n\nfn main() -> Int\n    bounce(41)\n";
    let code = vm_compile(src);

    for name in ["inc", "dec", "bounce", "main"] {
        let fn_id = code.find(name).unwrap_or_else(|| panic!("missing {name}"));
        assert!(code.get(fn_id).thin, "{name} should be thin");
    }

    let result = vm_run(src);
    assert_eq!(result.as_int(&Arena::new()), 41);
}

#[test]
fn vm_marks_allocating_helper_as_non_thin() {
    let src = "record Point\n    x: Int\n    y: Int\n\nfn moveRight(p: Point) -> Point\n    Point.update(p, x = p.x + 1)\n\nfn main() -> Int\n    moveRight(Point(x = 1, y = 2)).x\n";
    let code = vm_compile(src);
    let fn_id = code.find("moveRight").expect("missing moveRight");
    assert!(
        !code.get(fn_id).thin,
        "record update helper must stay non-thin"
    );
}

#[test]
fn vm_helper_return_string_survives_ordinary_return() {
    let src = "fn suffix() -> String\n    \"b\"\n\nfn main() -> String\n    \"a\" + suffix()\n";
    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Str("ab".to_string())
    );
}

#[test]
fn vm_helper_return_err_string_survives_ordinary_return() {
    let src = "fn fail(flag: Bool) -> Result<String, String>\n    match flag\n        true -> Result.Ok(\"ok\")\n        false -> Result.Err(\"bad\")\n\nfn main() -> Result<String, String>\n    fail(false)\n";
    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Err(Box::new(aver::value::Value::Str("bad".to_string())))
    );
}

#[test]
fn vm_list_get_and_option_to_result_keep_strings_alive() {
    let src = "fn extract(parts: List<String>) -> Result<String, String>\n    v = Vector.fromList(parts)\n    section = Option.toResult(Vector.get(v, 1), \"Missing path segment\")?\n    match section == \"weather\"\n        false -> Result.Err(\"Not a weather endpoint\")\n        true  -> Option.toResult(Vector.get(v, 2), \"Missing city\")\n\nfn main() -> Result<String, String>\n    extract([\"\", \"other\", \"Warsaw\"])\n";
    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Err(Box::new(aver::value::Value::Str(
            "Not a weather endpoint".to_string()
        )))
    );
}

#[test]
fn vm_repeated_helper_calls_keep_prefix_string_roots_valid() {
    let src = "fn okCase() -> Result<String, String>\n    Result.Ok(\"Warsaw\")\n\nfn errCase() -> Result<String, String>\n    Result.Err(\"Not a weather endpoint\")\n";
    let mut machine = vm_machine(src);

    let ok = machine
        .run_named_function("okCase", &[])
        .expect("okCase failed");
    assert_eq!(
        ok.to_value(&machine.arena),
        aver::value::Value::Ok(Box::new(aver::value::Value::Str("Warsaw".to_string())))
    );

    let err = machine
        .run_named_function("errCase", &[])
        .expect("errCase failed");
    assert_eq!(
        err.to_value(&machine.arena),
        aver::value::Value::Err(Box::new(aver::value::Value::Str(
            "Not a weather endpoint".to_string()
        )))
    );
}

#[test]
fn vm_nested_result_wrappers_keep_strings_alive_across_calls() {
    let src = "fn okCase() -> Result<Result<String, String>, String>\n    Result.Ok(Result.Ok(\"Warsaw\"))\n\nfn errCase() -> Result<Result<String, String>, String>\n    Result.Ok(Result.Err(\"Not a weather endpoint\"))\n";
    let mut machine = vm_machine(src);

    let ok = machine
        .run_named_function("okCase", &[])
        .expect("okCase failed");
    assert_eq!(
        ok.to_value(&machine.arena),
        aver::value::Value::Ok(Box::new(aver::value::Value::Ok(Box::new(
            aver::value::Value::Str("Warsaw".to_string())
        ))))
    );

    let err = machine
        .run_named_function("errCase", &[])
        .expect("errCase failed");
    assert_eq!(
        err.to_value(&machine.arena),
        aver::value::Value::Ok(Box::new(aver::value::Value::Err(Box::new(
            aver::value::Value::Str("Not a weather endpoint".to_string())
        ))))
    );
}

#[test]
fn vm_repeated_extract_calls_keep_string_roots_valid() {
    let src = "fn extract(parts: List<String>) -> Result<String, String>\n    v = Vector.fromList(parts)\n    section = Option.toResult(Vector.get(v, 1), \"Missing path segment\")?\n    match section == \"weather\"\n        false -> Result.Err(\"Not a weather endpoint\")\n        true  -> Option.toResult(Vector.get(v, 2), \"Missing city\")\n\nfn okCase() -> Result<Result<String, String>, String>\n    Result.Ok(extract([\"\", \"weather\", \"Warsaw\"]))\n\nfn errCase() -> Result<Result<String, String>, String>\n    Result.Ok(extract([\"\", \"other\", \"Warsaw\"]))\n";
    let mut machine = vm_machine(src);

    let ok = machine
        .run_named_function("okCase", &[])
        .expect("okCase failed");
    assert_eq!(
        ok.to_value(&machine.arena),
        aver::value::Value::Ok(Box::new(aver::value::Value::Ok(Box::new(
            aver::value::Value::Str("Warsaw".to_string())
        ))))
    );

    let err = machine
        .run_named_function("errCase", &[])
        .expect("errCase failed");
    assert_eq!(
        err.to_value(&machine.arena),
        aver::value::Value::Ok(Box::new(aver::value::Value::Err(Box::new(
            aver::value::Value::Str("Not a weather endpoint".to_string())
        ))))
    );
}

#[test]
fn vm_verify_style_helper_sequence_keeps_string_roots_valid() {
    let src = "fn extract(parts: List<String>) -> Result<String, String>\n    v = Vector.fromList(parts)\n    section = Option.toResult(Vector.get(v, 1), \"Missing path segment\")?\n    match section == \"weather\"\n        false -> Result.Err(\"Not a weather endpoint\")\n        true  -> Option.toResult(Vector.get(v, 2), \"Missing city\")\n\nfn left0() -> Unit\n    Result.Ok(extract([\"\", \"weather\", \"Warsaw\"]))\n\nfn right0() -> Unit\n    Result.Ok(Result.Ok(\"Warsaw\"))\n\nfn left1() -> Unit\n    Result.Ok(extract([\"\", \"other\", \"Warsaw\"]))\n\nfn right1() -> Unit\n    Result.Ok(Result.Err(\"Not a weather endpoint\"))\n";
    let mut machine = vm_machine(src);

    for (name, expected) in [
        (
            "left0",
            aver::value::Value::Ok(Box::new(aver::value::Value::Ok(Box::new(
                aver::value::Value::Str("Warsaw".to_string()),
            )))),
        ),
        (
            "right0",
            aver::value::Value::Ok(Box::new(aver::value::Value::Ok(Box::new(
                aver::value::Value::Str("Warsaw".to_string()),
            )))),
        ),
        (
            "left1",
            aver::value::Value::Ok(Box::new(aver::value::Value::Err(Box::new(
                aver::value::Value::Str("Not a weather endpoint".to_string()),
            )))),
        ),
        (
            "right1",
            aver::value::Value::Ok(Box::new(aver::value::Value::Err(Box::new(
                aver::value::Value::Str("Not a weather endpoint".to_string()),
            )))),
        ),
    ] {
        let value = machine
            .run_named_function(name, &[])
            .unwrap_or_else(|e| panic!("{} failed: {}", name, e));
        assert_eq!(
            value.to_value(&machine.arena),
            expected,
            "mismatch in {}",
            name
        );
    }
}

#[test]
fn vm_variant_return_keeps_string_field_alive_for_caller() {
    let src = "type ParseResult\n    Ok(Int)\n    Err(String, Int)\n\nfn fail() -> ParseResult\n    ParseResult.Err(\"Leading zeros not allowed\", 1)\n\nfn main() -> String\n    match fail()\n        ParseResult.Err(msg, p) -> msg + \" at position \" + String.fromInt(p)\n        ParseResult.Ok(_) -> \"ok\"\n";
    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Str("Leading zeros not allowed at position 1".to_string())
    );
}

#[test]
fn vm_multi_hop_variant_return_keeps_string_field_alive_for_caller() {
    let src = "type ParseResult\n    Ok(Int)\n    Err(String, Int)\n\nfn leaf() -> ParseResult\n    ParseResult.Err(\"Leading zeros not allowed\", 2)\n\nfn step1() -> ParseResult\n    leaf()\n\nfn step2() -> ParseResult\n    step1()\n\nfn step3() -> ParseResult\n    step2()\n\nfn main() -> String\n    match step3()\n        ParseResult.Err(msg, p) -> msg + \" at position \" + String.fromInt(p)\n        ParseResult.Ok(_) -> \"ok\"\n";
    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Str("Leading zeros not allowed at position 2".to_string())
    );
}

#[test]
fn vm_repeated_variant_error_rendering_keeps_message_strings_alive() {
    let src = "type ParseResult\n    Ok(Int)\n    Err(String, Int)\n\nfn case1() -> ParseResult\n    ParseResult.Err(\"Trailing content: f\", 5)\n\nfn case2() -> ParseResult\n    ParseResult.Err(\"Trailing content: o\", 2)\n\nfn case3() -> ParseResult\n    ParseResult.Err(\"Expected digit after '-'\", 1)\n\nfn case4() -> ParseResult\n    ParseResult.Err(\"Leading zeros not allowed\", 1)\n\nfn case5() -> ParseResult\n    ParseResult.Err(\"Leading zeros not allowed\", 2)\n\nfn render(value: ParseResult) -> String\n    match value\n        ParseResult.Err(msg, p) -> msg + \" at position \" + String.fromInt(p)\n        ParseResult.Ok(_) -> \"ok\"\n";
    let mut machine = vm_machine(src);

    for (name, expected) in [
        ("case1", "Trailing content: f at position 5"),
        ("case2", "Trailing content: o at position 2"),
        ("case3", "Expected digit after '-' at position 1"),
        ("case4", "Leading zeros not allowed at position 1"),
        ("case5", "Leading zeros not allowed at position 2"),
    ] {
        let value = machine
            .run_named_function(name, &[])
            .unwrap_or_else(|e| panic!("{} failed: {}", name, e));
        let rendered = machine
            .run_named_function("render", &[value])
            .unwrap_or_else(|e| panic!("render({}) failed: {}", name, e));
        assert_eq!(
            rendered.to_value(&machine.arena),
            aver::value::Value::Str(expected.to_string()),
            "mismatch for {}",
            name
        );
    }
}

#[test]
fn vm_rebuilt_variant_error_chain_keeps_message_strings_alive() {
    let src = "type ParseResult\n    Ok(Int)\n    Err(String, Int)\n\nfn leaf() -> ParseResult\n    ParseResult.Err(\"Leading zeros not allowed\", 2)\n\nfn step1() -> ParseResult\n    match leaf()\n        ParseResult.Err(msg, p) -> ParseResult.Err(msg, p)\n        ParseResult.Ok(v) -> ParseResult.Ok(v)\n\nfn step2() -> ParseResult\n    match step1()\n        ParseResult.Err(msg, p) -> ParseResult.Err(msg, p)\n        ParseResult.Ok(v) -> ParseResult.Ok(v)\n\nfn step3() -> ParseResult\n    match step2()\n        ParseResult.Err(msg, p) -> ParseResult.Err(msg, p)\n        ParseResult.Ok(v) -> ParseResult.Ok(v)\n\nfn main() -> String\n    match step3()\n        ParseResult.Err(msg, p) -> msg + \" at position \" + String.fromInt(p)\n        ParseResult.Ok(_) -> \"ok\"\n";
    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Str("Leading zeros not allowed at position 2".to_string())
    );
}

#[test]
fn vm_real_json_from_string_error_keeps_message_alive() {
    let src = include_str!("../examples/data/json.av");
    let mut machine = vm_machine(src);
    let arg = nv_string(&mut machine, "01");
    let result = machine
        .run_named_function("fromString", &[arg])
        .expect("fromString failed");
    assert_eq!(
        result.to_value(&machine.arena),
        aver::value::Value::Err(Box::new(aver::value::Value::Str(
            "Leading zeros not allowed at position 1".to_string()
        )))
    );
}

#[test]
fn vm_real_json_negative_leading_zero_error_keeps_message_alive() {
    let src = include_str!("../examples/data/json.av");
    let mut machine = vm_machine(src);
    let arg = nv_string(&mut machine, "-01");
    let result = machine
        .run_named_function("fromString", &[arg])
        .expect("fromString failed");
    assert_eq!(
        result.to_value(&machine.arena),
        aver::value::Value::Err(Box::new(aver::value::Value::Str(
            "Leading zeros not allowed at position 2".to_string()
        )))
    );
}

#[test]
fn vm_real_json_repeated_from_string_calls_keep_error_strings_alive() {
    let src = include_str!("../examples/data/json.av");
    let mut machine = vm_machine(src);

    for (input, expected) in [
        (
            "true false",
            "Trailing content: f at position 5".to_string(),
        ),
        ("42oops", "Trailing content: o at position 2".to_string()),
        ("-", "Expected digit after '-' at position 1".to_string()),
        ("01", "Leading zeros not allowed at position 1".to_string()),
        ("-01", "Leading zeros not allowed at position 2".to_string()),
    ] {
        let arg = nv_string(&mut machine, input);
        let result = machine
            .run_named_function("fromString", &[arg])
            .unwrap_or_else(|e| panic!("fromString({input:?}) failed: {}", e));
        assert_eq!(
            result.to_value(&machine.arena),
            aver::value::Value::Err(Box::new(aver::value::Value::Str(expected))),
            "mismatch for input {:?}",
            input
        );
    }
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
    assert!(
        arena.peak_usage().stable < 16,
        "helper-return handoff should not spill into stable, peak stable = {}",
        arena.peak_usage().stable
    );
}

#[test]
fn vm_multi_hop_helper_returns_feed_tail_loop_without_stable_churn() {
    let src = "fn extend0(acc: List<Int>, n: Int) -> List<Int>\n    List.prepend(n, acc)\n\nfn extend1(acc: List<Int>, n: Int) -> List<Int>\n    extend0(acc, n)\n\nfn extend2(acc: List<Int>, n: Int) -> List<Int>\n    extend1(acc, n)\n\nfn extend3(acc: List<Int>, n: Int) -> List<Int>\n    extend2(acc, n)\n\nfn build(n: Int, acc: List<Int>) -> Int\n    match n == 0\n        true -> List.len(acc)\n        false -> build(n - 1, extend3(acc, n))\n\nfn main() -> Int\n    build(200, [])\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_int());
    let empty = Arena::new();
    assert_eq!(result.as_int(&empty), 200);
    assert!(
        arena.len() < 16,
        "multi-hop helper returns should not accumulate, arena.len() = {}",
        arena.len()
    );
    assert!(
        arena.peak_usage().stable < 16,
        "multi-hop helper returns should stay off stable, peak stable = {}",
        arena.peak_usage().stable
    );
}

#[test]
fn vm_mixed_young_handoff_helper_returns_keep_strings_alive_for_caller() {
    let src = "fn escapeField(text: String) -> String\n    p = String.replace(text, \"%\", \"%25\")\n    bar = String.replace(p, \"|\", \"%7C\")\n    String.replace(bar, \"\\n\", \"%0A\")\n\nfn wrap1(text: String) -> String\n    escapeField(text)\n\nfn wrap2(text: String) -> String\n    wrap1(text)\n\nfn render(name: String) -> String\n    escaped = wrap2(name)\n    \"project:\" + escaped\n\nfn main() -> String\n    render(\"Alpha | team\")\n";
    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Str("project:Alpha %7C team".to_string())
    );
}

#[test]
fn vm_parent_thin_string_helpers_use_parent_young_path() {
    let src = "fn escapeField(text: String) -> String\n    p = String.replace(text, \"%\", \"%25\")\n    bar = String.replace(p, \"|\", \"%7C\")\n    String.replace(bar, \"\\n\", \"%0A\")\n\nfn wrap1(text: String) -> String\n    escapeField(text)\n\nfn wrap2(text: String) -> String\n    wrap1(text)\n\nfn render(name: String) -> String\n    escaped = wrap2(name)\n    \"project:\" + escaped\n\nfn main() -> String\n    render(\"Alpha | team\")\n";
    let code = vm_compile(src);
    assert!(!code.get(code.find("escapeField").unwrap()).parent_thin);
    assert!(code.get(code.find("wrap1").unwrap()).parent_thin);
    assert!(code.get(code.find("wrap2").unwrap()).parent_thin);

    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Str("project:Alpha %7C team".to_string())
    );
    assert!(
        arena.peak_usage().handoff < 4,
        "parent-thin helper chain should avoid ordinary-return handoff churn, peak handoff = {}",
        arena.peak_usage().handoff
    );
}

#[test]
fn vm_parent_thin_allows_small_match_binding_helpers() {
    let src = "fn pick(flag: Bool, left: String, right: String) -> String\n    chosen = match flag\n        true -> left\n        false -> right\n    chosen\n\nfn wrap(flag: Bool, left: String, right: String) -> String\n    pick(flag, left, right)\n\nfn main() -> String\n    wrap(true, \"alpha\", \"beta\")\n";
    let code = vm_compile(src);
    assert!(code.get(code.find("pick").unwrap()).parent_thin);
    assert!(code.get(code.find("wrap").unwrap()).parent_thin);

    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Str("alpha".to_string())
    );
    assert!(
        arena.peak_usage().handoff < 4,
        "small match helpers should stay off ordinary-return handoff churn, peak handoff = {}",
        arena.peak_usage().handoff
    );
}

#[test]
fn vm_parent_thin_allows_nullary_variant_helpers() {
    let src = "type Status\n    Todo\n    Done\n\nfn normalize(flag: Bool) -> Status\n    match flag\n        true -> Status.Todo\n        false -> Status.Done\n\nfn passthrough(flag: Bool) -> Status\n    normalize(flag)\n\nfn main() -> Status\n    passthrough(false)\n";
    let code = vm_compile(src);
    assert!(code.get(code.find("normalize").unwrap()).thin);
    assert!(code.get(code.find("normalize").unwrap()).parent_thin);
    assert!(code.get(code.find("passthrough").unwrap()).parent_thin);

    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Variant {
            type_name: "Status".to_string(),
            variant: "Done".to_string(),
            fields: vec![].into(),
        }
    );
    assert!(
        arena.peak_usage().handoff < 2,
        "nullary variant helpers should stay off ordinary-return handoff churn, peak handoff = {}",
        arena.peak_usage().handoff
    );
}

#[test]
fn vm_parent_thin_allows_heapless_builtin_helpers() {
    let src = "fn keepCount(name: String) -> Int\n    String.len(name)\n\nfn wrap(name: String) -> Int\n    keepCount(name)\n\nfn main() -> Int\n    wrap(\"alpha\")\n";
    let code = vm_compile(src);
    assert!(code.get(code.find("keepCount").unwrap()).parent_thin);
    assert!(code.get(code.find("wrap").unwrap()).parent_thin);

    let result = vm_run(src);
    assert_eq!(result.as_int(&Arena::new()), 5);
}

#[test]
fn vm_parent_thin_allows_cheap_builtin_lookup_helpers() {
    let src = "fn lookup(values: Map<String, Int>) -> Option<Int>\n    Map.get(values, \"x\")\n\nfn wrap(values: Map<String, Int>) -> Option<Int>\n    lookup(values)\n\nfn main() -> Option<Int>\n    wrap(Map.set(Map.empty(), \"x\", 9))\n";
    let code = vm_compile(src);
    assert!(code.get(code.find("lookup").unwrap()).parent_thin);
    assert!(code.get(code.find("wrap").unwrap()).parent_thin);

    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Some(Box::new(aver::value::Value::Int(9)))
    );
}

#[test]
fn vm_parent_thin_rejects_builtin_heavy_wrapper_chain() {
    let src = "fn keepNonEmpty(lines: List<String>) -> List<String>\n    lines\n\nfn splitNonEmptyLines(content: String) -> List<String>\n    keepNonEmpty(String.split(content, \"\\n\"))\n\nfn main() -> List<String>\n    splitNonEmptyLines(\"a\\n\\nb\")\n";
    let code = vm_compile(src);
    assert!(
        !code
            .get(code.find("splitNonEmptyLines").unwrap())
            .parent_thin,
        "wrapper around String.split should stay off parent-thin so large intermediate lists do not borrow caller young"
    );
}

#[test]
fn vm_parent_thin_rejects_allocating_builtin_wrappers() {
    let src = "fn upper(name: String) -> String\n    String.toUpper(name)\n\nfn wrap(name: String) -> String\n    upper(name)\n\nfn main() -> String\n    wrap(\"alpha\")\n";
    let code = vm_compile(src);
    assert!(
        !code.get(code.find("upper").unwrap()).parent_thin,
        "allocating string builtins should still stay off parent-thin"
    );
}

#[test]
fn vm_tail_call_known_resizes_stack_before_clearing_new_locals() {
    let src = "fn stepA(n: Int) -> Int\n    match n == 0\n        true -> 0\n        false -> stepB(n - 1)\n\nfn stepB(n: Int) -> Int\n    x = n\n    y = x\n    z = y\n    match n == 0\n        true -> z\n        false -> stepA(n - 1)\n\nfn main() -> Int\n    stepA(200)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 0);
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

#[test]
fn vm_nullary_variant_match() {
    let src = "type Status\n    Todo\n    Done\n\nfn score(s: Status) -> Int\n    match s\n        Status.Todo -> 1\n        Status.Done -> 2\n\nfn main() -> Int\n    score(Status.Todo)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 1);
}

#[test]
fn vm_standalone_builtin_function_value() {
    let src = "fn apply(f: Fn(String) -> String, value: String) -> String\n    f(value)\n\nfn main() -> String\n    upper = String.toUpper\n    apply(upper, \"alpha\")\n";
    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Str("ALPHA".to_string())
    );
}

#[test]
fn vm_wrapper_constructor_value() {
    let src = "fn apply(f: Fn(Int) -> Result<Int, String>, value: Int) -> Result<Int, String>\n    f(value)\n\nfn main() -> Result<Int, String>\n    wrap = Result.Ok\n    apply(wrap, 7)\n";
    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Ok(Box::new(aver::value::Value::Int(7)))
    );
}

#[test]
fn vm_independent_product_allows_builtin_branches() {
    let mut machine = vm_machine(
        r#"
fn main() -> (List<String>, Int)
    ! [Args.get]
    (Args.get(), String.len("abcd"))!
"#,
    );
    machine.set_cli_args(vec!["left".to_string(), "right".to_string()]);
    let result = machine.run().expect("vm run failed");
    assert_eq!(
        result.to_value(&machine.arena),
        aver::value::Value::Tuple(vec![
            aver::value::list_from_vec(vec![
                aver::value::Value::Str("left".to_string()),
                aver::value::Value::Str("right".to_string()),
            ]),
            aver::value::Value::Int(4),
        ])
    );
}

#[test]
fn vm_independent_product_allows_wrapper_branches() {
    let (result, arena) = vm_run_with_arena(
        r#"
fn main() -> (Result<Int, String>, Result<Int, String>)
    (Result.Ok(1), Result.Err("x"))!
"#,
    );
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Tuple(vec![
            aver::value::Value::Ok(Box::new(aver::value::Value::Int(1))),
            aver::value::Value::Err(Box::new(aver::value::Value::Str("x".to_string()))),
        ])
    );
}

#[test]
fn vm_independent_product_allows_dynamic_callable_and_global_branches() {
    let src = r#"
shared = ["aa", "bbb"]

fn fromGlobal() -> Int
    match shared
        [first, .._] -> String.len(first)
        _ -> 0

fn main() -> (Int, Int)
    f = String.len
    (f("abcd"), fromGlobal())!
"#;
    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Tuple(vec![aver::value::Value::Int(4), aver::value::Value::Int(2),])
    );
}

#[test]
fn vm_independent_product_allows_module_local_function_branches() {
    let root = temp_module_root("independent_product_module_local_fn");
    std::fs::write(
        root.join("ai.av"),
        r#"module Ai
    exposes [pair]
    intent =
        "Minimal module for CALL_PAR function-value regression."

fn score(x: Int) -> Int
    x + 1

fn pair(x: Int) -> (Int, Int)
    (score(x), score(x + 1))!
"#,
    )
    .expect("write ai.av");

    let src = r#"module Main
    depends [Ai]
    intent =
        "Runs module-local parallel scoring through VM."

fn main() -> (Int, Int)
    Ai.pair(1)
"#;

    let (result, arena) = vm_run_with_module_root_and_arena(src, &root);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Tuple(vec![aver::value::Value::Int(2), aver::value::Value::Int(3),])
    );
}

#[test]
fn vm_independent_product_preserves_string_key_maps_in_parallel_branches() {
    let src = r#"
fn hasKey(m: Map<String, Int>) -> Bool
    Map.has(m, "Board.isColor")

fn lenIsOne(m: Map<String, Int>) -> Bool
    Map.len(m) == 1

fn main() -> (Bool, Bool, Bool, Bool)
    m = Map.set(Map.empty(), "Board.isColor", 7)
    (hasKey(m), hasKey(m), lenIsOne(m), lenIsOne(m))!
"#;

    let (result, arena) = vm_run_with_arena(src);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Tuple(vec![
            aver::value::Value::Bool(true),
            aver::value::Value::Bool(true),
            aver::value::Value::Bool(true),
            aver::value::Value::Bool(true),
        ])
    );
}

#[test]
fn vm_independent_product_preserves_self_host_fnstore_maps() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("self_hosted");
    let src = r#"module Probe
    depends [Domain.Ast, Domain.Eval.Store]
    intent =
        "Regression for FnStore.nameToId disappearing in CALL_PAR child VMs."

fn hasBoard(fns: FnStore) -> Bool
    match Domain.Eval.Store.lookupFnOption(fns, "Board.isColor")
        Option.Some(_) -> true
        Option.None -> false

fn hasBoardId(fns: FnStore) -> Bool
    match Domain.Eval.Store.lookupFnId(fns, "Board.isColor")
        Result.Ok(_) -> true
        Result.Err(_) -> false

fn hasFnZero(fns: FnStore) -> Bool
    match Domain.Eval.Store.lookupFnById(fns, 0)
        Result.Ok(fd) -> fd.name == "Board.isColor"
        Result.Err(_) -> false

fn main() -> (Bool, Bool, Bool, Bool, Bool, Bool)
    store = Domain.Eval.Store.fnsToStore([
        FnDef(name = "Board.isColor", params = [], body = [], slotCount = 0, slotMap = Map.empty(), fastPath = FnFastPath.FastNone, tailLoop = false)
    ])
    (hasBoard(store), hasBoard(store), hasBoardId(store), hasBoardId(store), hasFnZero(store), hasFnZero(store))!
"#;

    let (result, arena) = vm_run_with_module_root_and_arena(src, &root);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Tuple(vec![
            aver::value::Value::Bool(true),
            aver::value::Value::Bool(true),
            aver::value::Value::Bool(true),
            aver::value::Value::Bool(true),
            aver::value::Value::Bool(true),
            aver::value::Value::Bool(true),
        ])
    );
}

#[test]
fn vm_option_none_namespace_member_value() {
    let src = "fn fallback(value: Option<Int>) -> Int\n    match value\n        Option.Some(v) -> v\n        Option.None -> 0\n\nfn main() -> Int\n    missing = Option.None\n    fallback(missing)\n";
    let result = vm_run(src);
    assert!(result.is_int());
    let arena = Arena::new();
    assert_eq!(result.as_int(&arena), 0);
}

#[test]
fn vm_resolves_nested_module_type_constructor_paths() {
    let root = temp_module_root("qualified_ctor");
    let domain_dir = root.join("domain");
    std::fs::create_dir_all(&domain_dir).expect("create domain dir");
    std::fs::write(
        domain_dir.join("types.av"),
        "module Types\n    exposes [TaskEvent]\n    intent =\n        \"Domain types.\"\n\ntype TaskEvent\n    TaskCreated(String)\n    TaskStarted(String)\n",
    )
    .expect("write domain/types.av");

    let src = "module App\n    depends [Domain.Types]\n    intent =\n        \"Uses qualified constructors.\"\n\nfn main() -> String\n    event = Domain.Types.TaskEvent.TaskCreated(\"now\")\n    match event\n        Domain.Types.TaskEvent.TaskCreated(at) -> at\n        Domain.Types.TaskEvent.TaskStarted(at) -> at\n";
    let (result, arena) = vm_run_with_module_root_and_arena(src, &root);
    assert_eq!(
        result.to_value(&arena),
        aver::value::Value::Str("now".into())
    );
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
    assert!(result.heap_index().is_none());
    assert_eq!(arena.get_string_value(result), "hello");
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
    assert_eq!(arena.get_string_value(result), "hello world");
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
    assert_eq!(arena.get_string_value(result), "value is 42");
}

#[test]
fn vm_string_interpolation_single_expr_part() {
    let (result, arena) = vm_run_with_arena("fn main() -> String\n    x = 42\n    \"{x}\"\n");
    assert!(result.is_string());
    assert_eq!(arena.get_string_value(result), "42");
}

#[test]
fn vm_empty_string_is_inline() {
    let (result, arena) = vm_run_with_arena("fn main() -> String\n    \"\"\n");
    assert!(result.is_string());
    assert!(result.heap_index().is_none());
    assert_eq!(arena.get_string_value(result), "");
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
    let inner = result.wrapper_inner(&arena);
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
fn vm_list_take() {
    let (result, arena) =
        vm_run_with_arena("fn main() -> List<Int>\n    List.take([1, 2, 3, 4], 2)\n");
    assert!(result.is_list());
    assert_eq!(arena.list_len(result.arena_index()), 2);
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
}

#[test]
fn vm_list_drop() {
    let (result, arena) =
        vm_run_with_arena("fn main() -> List<Int>\n    List.drop([1, 2, 3, 4], 2)\n");
    assert!(result.is_list());
    assert_eq!(arena.list_len(result.arena_index()), 2);
    assert_eq!(
        arena
            .list_get(result.arena_index(), 0)
            .unwrap()
            .as_int(&arena),
        3
    );
    assert_eq!(
        arena
            .list_get(result.arena_index(), 1)
            .unwrap()
            .as_int(&arena),
        4
    );
}

#[test]
fn vm_list_head() {
    let src = "fn main() -> Option<Int>\n    Vector.get(Vector.fromList([10, 20, 30]), 0)\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_some());
    let inner = result.wrapper_inner(&arena);
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
    let inner = result.wrapper_inner(&arena);
    assert_eq!(inner.as_int(&arena), 6);
}

#[test]
fn vm_error_prop_err() {
    let src = "fn safeDivide(a: Int, b: Int) -> Result<Int, String>\n    match b == 0\n        true -> Result.Err(\"div by zero\")\n        false -> Result.Ok(a / b)\n\nfn run() -> Result<Int, String>\n    x = safeDivide(10, 0)?\n    Result.Ok(x + 1)\n\nfn main() -> Result<Int, String>\n    run()\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_err());
    let inner = result.wrapper_inner(&arena);
    assert!(inner.is_string());
    assert_eq!(arena.get_string_value(inner), "div by zero");
}

#[test]
fn vm_error_prop_chain() {
    let src = "fn step1() -> Result<Int, String>\n    Result.Ok(10)\n\nfn step2(x: Int) -> Result<Int, String>\n    Result.Ok(x * 2)\n\nfn run() -> Result<Int, String>\n    a = step1()?\n    b = step2(a)?\n    Result.Ok(b + 1)\n\nfn main() -> Result<Int, String>\n    run()\n";
    let (result, arena) = vm_run_with_arena(src);
    assert!(result.is_ok());
    let inner = result.wrapper_inner(&arena);
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

#[test]
fn vm_respects_aver_toml_runtime_policy() {
    let module_root = temp_module_root("runtime_policy");
    std::fs::write(
        module_root.join("aver.toml"),
        "[effects.Disk]\npaths = [\"./allowed/**\"]\n",
    )
    .expect("write aver.toml");

    let src = "fn main() -> Bool\n    ! [Disk.exists]\n    Disk.exists(\"./blocked.txt\")\n";
    let mut items = parse(src);
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program_with_modules(
        &items,
        &mut arena,
        Some(
            module_root
                .to_str()
                .expect("module root must be valid UTF-8"),
        ),
        "",
    )
    .expect("compile failed");
    let mut machine = vm::VM::new(code, globals, arena);
    let config = ProjectConfig::load_from_dir(&module_root)
        .expect("load aver.toml")
        .expect("aver.toml should exist");
    machine.set_runtime_policy(config);

    let err = machine.run().expect_err("policy should deny Disk.exists");
    let msg = format!("{}", err);
    assert!(
        msg.contains("denied by aver.toml policy"),
        "error should mention aver.toml policy, got: {}",
        msg
    );
}

#[test]
fn vm_cancel_mode_stops_independent_product_sibling_early() {
    let marker_path = std::env::temp_dir().join(format!(
        "aver_vm_independence_{}_{}.txt",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("clock went backwards")
            .as_nanos()
    ));
    let src = format!(
        r#"fn failFast() -> Result<Unit, String>
    Result.Err("boom")

fn appendMore(path: String, remaining: Int) -> Result<Unit, String>
    ? "Appends one marker, then continues."
    ! [Disk.appendText]
    _ = Disk.appendText(path, "x\n")?
    appendMany(path, remaining - 1)

fn appendMany(path: String, remaining: Int) -> Result<Unit, String>
    ? "Appends one marker per recursive step."
    ! [Disk.appendText]
    match remaining == 0
        true -> Result.Ok(Unit)
        false -> appendMore(path, remaining)

fn main() -> Result<Unit, String>
    ! [Disk.appendText]
    _ = (failFast(), appendMany("{marker}", 64))?!
    Result.Ok(Unit)
"#,
        marker = marker_path.display()
    );

    let run_with_mode = |mode: &str| {
        let _ = std::fs::remove_file(&marker_path);
        let mut items = parse(&src);
        tco::transform_program(&mut items);
        resolver::resolve_program(&mut items);

        let mut arena = Arena::new();
        let (code, globals) = vm::compile_program(&items, &mut arena).expect("compile failed");
        let mut machine = vm::VM::new(code, globals, arena);
        machine.set_runtime_policy(
            ProjectConfig::parse(&format!("[independence]\nmode = \"{mode}\"\n"))
                .expect("parse policy"),
        );
        let result = machine.run().expect("VM execution failed");
        let value = result.to_value(&machine.arena);
        let count = match std::fs::read_to_string(&marker_path) {
            Ok(content) => content.lines().count(),
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => 0,
            Err(err) => panic!("failed to read {}: {}", marker_path.display(), err),
        };
        let _ = std::fs::remove_file(&marker_path);
        (value, count)
    };

    let (complete_value, complete_count) = run_with_mode("complete");
    let (cancel_value, cancel_count) = run_with_mode("cancel");

    assert_eq!(
        complete_value,
        aver::value::Value::Err(Box::new(aver::value::Value::Str("boom".to_string())))
    );
    assert_eq!(
        cancel_value,
        aver::value::Value::Err(Box::new(aver::value::Value::Str("boom".to_string())))
    );
    assert_eq!(complete_count, 64);
    assert!(
        cancel_count < complete_count,
        "cancel mode should stop sibling work early, got cancel_count={} vs complete_count={}",
        cancel_count,
        complete_count
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
