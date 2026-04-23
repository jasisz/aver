/// Spec tests for the Aver static type checker.
///
/// Tests are grouped into:
///   - valid programs that must produce zero type errors
///   - invalid programs that must produce at least one error with a
///     specific substring in the message
///
/// The type checker is run directly via `run_type_check`, bypassing the CLI.
use aver::ast::TopLevel;
use aver::lexer::Lexer;
use aver::parser::Parser;
use aver::types::checker::{run_type_check, run_type_check_with_base};
use std::time::{SystemTime, UNIX_EPOCH};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn parse(src: &str) -> Vec<TopLevel> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser.parse().expect("parse failed")
}

fn parse_error(src: &str) -> String {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser
        .parse()
        .expect_err("expected parse failure")
        .to_string()
}

fn errors(src: &str) -> Vec<String> {
    let items = parse(src);
    run_type_check(&items)
        .into_iter()
        .map(|e| e.message)
        .collect()
}

fn errors_with_base(src: &str, base_dir: &str) -> Vec<String> {
    let items = parse(src);
    run_type_check_with_base(&items, Some(base_dir))
        .into_iter()
        .map(|e| e.message)
        .collect()
}

fn temp_module_root(tag: &str) -> std::path::PathBuf {
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock went backwards")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver_typechecker_{}_{}", tag, ts));
    std::fs::create_dir_all(&dir).expect("create temp module dir failed");
    dir
}

fn assert_no_errors(src: &str) {
    let errs = errors(src);
    assert!(
        errs.is_empty(),
        "expected no type errors, got:\n  {}",
        errs.join("\n  ")
    );
}

fn assert_error_containing(src: &str, snippet: &str) {
    let errs = errors(src);
    assert!(
        errs.iter().any(|e| e.contains(snippet)),
        "expected error containing {:?}, got:\n  {}",
        snippet,
        if errs.is_empty() {
            "<no errors>".to_string()
        } else {
            errs.join("\n  ")
        }
    );
}

fn assert_parse_error_containing(src: &str, snippet: &str) {
    let msg = parse_error(src);
    assert!(
        msg.contains(snippet),
        "expected parse error containing {:?}, got: {}",
        snippet,
        msg
    );
}

// ---------------------------------------------------------------------------
// Valid programs — must pass with zero errors
// ---------------------------------------------------------------------------

#[test]
fn valid_int_function() {
    assert_no_errors("fn add(a: Int, b: Int) -> Int\n    a + b\n");
}

#[test]
fn valid_string_function() {
    assert_no_errors("fn greet(name: String) -> String\n    \"Hello\"\n");
}

#[test]
fn valid_bool_function() {
    assert_no_errors("fn negate(b: Bool) -> Bool\n    b\n");
}

#[test]
fn valid_float_function() {
    assert_no_errors("fn scale(x: Float) -> Float\n    x\n");
}

#[test]
fn valid_unit_function() {
    assert_no_errors("fn noop() -> Unit\n    ! [Console.print]\n    Console.print(\"hi\")\n");
}

#[test]
fn valid_pure_result_unit_singleton() {
    let src = "fn ok() -> Result<Unit, String>\n    Result.Ok(Unit)\n";
    assert_no_errors(src);
}

#[test]
fn valid_result_return() {
    assert_no_errors("fn safe_div(a: Int, b: Int) -> Result<Int, String>\n    Result.Ok(a)\n");
}

#[test]
fn valid_option_return() {
    assert_no_errors("fn maybe(x: Int) -> Option<Int>\n    Option.Some(x)\n");
}

#[test]
fn valid_list_return() {
    assert_no_errors("fn wrap(x: Int) -> List<Int>\n    [x]\n");
}

#[test]
fn valid_option_with_default_infers_type() {
    let src = "fn unwrap(x: Option<String>) -> String\n    Option.withDefault(x, \"fallback\")\n";
    assert_no_errors(src);
}

#[test]
fn valid_result_with_default_infers_type() {
    let src = "fn unwrap(x: Result<Int, String>) -> Int\n    Result.withDefault(x, 0)\n";
    assert_no_errors(src);
}

#[test]
fn valid_option_to_result_infers_type() {
    let src =
        "fn convert(x: Option<Int>) -> Result<Int, String>\n    Option.toResult(x, \"missing\")\n";
    assert_no_errors(src);
}

#[test]
fn valid_list_pattern_matching() {
    let src = "fn score(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h + List.len(t)\n";
    assert_no_errors(src);
}

#[test]
fn valid_list_contains_requires_same_element_type() {
    let src = "fn hasTwo(xs: List<Int>) -> Bool\n    List.contains(xs, 2)\n";
    assert_no_errors(src);
}

#[test]
fn valid_list_prepend_preserves_inner_type() {
    let src = "fn withZero(xs: List<Int>) -> List<Int>\n    List.prepend(0, xs)\n";
    assert_no_errors(src);
}

#[test]
fn valid_list_take_preserves_inner_type() {
    let src = "fn firstTwo(xs: List<Int>) -> List<Int>\n    List.take(xs, 2)\n";
    assert_no_errors(src);
}

#[test]
fn valid_list_drop_preserves_inner_type() {
    let src = "fn rest(xs: List<Int>) -> List<Int>\n    List.drop(xs, 1)\n";
    assert_no_errors(src);
}

#[test]
fn valid_list_concat_requires_compatible_lists() {
    let src = "fn extend(xs: List<Int>, ys: List<Int>) -> List<Int>\n    List.concat(xs, ys)\n";
    assert_no_errors(src);
}

#[test]
fn valid_list_reverse_preserves_inner_type() {
    let src = "fn rev(xs: List<Int>) -> List<Int>\n    List.reverse(xs)\n";
    assert_no_errors(src);
}

#[test]
fn invalid_list_take_requires_int_count() {
    let src = "fn bad(xs: List<Int>) -> List<Int>\n    List.take(xs, \"x\")\n";
    assert_error_containing(src, "Argument 2 of 'List.take': expected Int, got String");
}

#[test]
fn valid_tuple_return() {
    let src = "fn pair() -> (Int, String)\n    (1, \"x\")\n";
    assert_no_errors(src);
}

#[test]
fn valid_map_set_get_infers_types() {
    let src = concat!(
        "fn readAge() -> Option<Int>\n",
        "    m = Map.set(Map.empty(), \"x\", 0)\n",
        "    m2 = Map.set(m, \"age\", 42)\n",
        "    Map.get(m2, \"age\")\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_map_from_list_tuples() {
    let src = concat!(
        "fn build() -> Map<String, Int>\n",
        "    Map.fromList([(\"a\", 1), (\"b\", 2)])\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_char_namespace_signatures() {
    let src = concat!(
        "fn f() -> Option<String>\n",
        "    code = Char.toCode(\"A\")\n",
        "    Char.fromCode(code)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_byte_namespace_signatures() {
    let src = concat!(
        "fn f() -> Result<Int, String>\n",
        "    hex = Byte.toHex(255)\n",
        "    n = hex?\n",
        "    Byte.fromHex(n)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_char_to_code_argument_type() {
    let src = "fn f() -> Int\n    Char.toCode(1)\n";
    assert_error_containing(src, "Argument 1 of 'Char.toCode': expected String, got Int");
}

#[test]
fn error_byte_from_hex_argument_type() {
    let src = "fn f() -> Result<Int, String>\n    Byte.fromHex(42)\n";
    assert_error_containing(
        src,
        "Argument 1 of 'Byte.fromHex': expected String, got Int",
    );
}

#[test]
fn valid_map_literal_infers_types() {
    let src = concat!(
        "fn readAge() -> Option<Int>\n",
        "    m = {\"age\" => 42}\n",
        "    Map.get(m, \"age\")\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_empty_map_literal_with_annotation() {
    let src = "fn empty() -> Map<String, Int>\n    {}\n";
    assert_no_errors(src);
}

#[test]
fn valid_call_correct_args() {
    let src = "fn add(a: Int, b: Int) -> Int\n    a + b\nfn main() -> Unit\n    r = add(1, 2)\n";
    assert_no_errors(src);
}

#[test]
fn valid_call_chain() {
    let src =
        "fn double(x: Int) -> Int\n    x + x\nfn quadruple(x: Int) -> Int\n    double(double(x))\n";
    assert_no_errors(src);
}

#[test]
fn valid_higher_order_function_param_call() {
    let src = "fn applyTwice(f: Fn(Int) -> Int, x: Int) -> Int\n    f(f(x))\nfn inc(n: Int) -> Int\n    n + 1\nfn main() -> Unit\n    r = applyTwice(inc, 10)\n";
    assert_no_errors(src);
}

#[test]
fn valid_pure_callback_for_effectful_slot() {
    let src = "fn applyOnce(f: Fn(Int) -> Int ! [Console.print], x: Int) -> Int\n    ! [Console.print]\n    f(x)\nfn pureInc(n: Int) -> Int\n    n + 1\nfn main() -> Unit\n    ! [Console.print]\n    r = applyOnce(pureInc, 10)\n";
    assert_no_errors(src);
}

#[test]
fn valid_simple_binding_in_fn() {
    assert_no_errors("fn f() -> Int\n    x = 5\n    x\n");
}

#[test]
fn invalid_int_float_no_widening() {
    // Int + Float is no longer allowed (no implicit widening)
    assert_error_containing(
        "fn f(a: Int, b: Float) -> Float\n    a + b\n",
        "matching types",
    );
}

// --- Real example files ---

#[test]
fn valid_hello_av() {
    let src = std::fs::read_to_string("examples/core/hello.av")
        .expect("examples/core/hello.av not found");
    assert_no_errors(&src);
}

#[test]
fn valid_calculator_av() {
    let src = std::fs::read_to_string("examples/core/calculator.av")
        .expect("examples/core/calculator.av not found");
    assert_no_errors(&src);
}

#[test]
fn valid_shapes_av() {
    let src = std::fs::read_to_string("examples/core/shapes.av")
        .expect("examples/core/shapes.av not found");
    assert_no_errors(&src);
}

#[test]
fn valid_lists_av() {
    let src = std::fs::read_to_string("examples/core/lists.av")
        .expect("examples/core/lists.av not found");
    assert_no_errors(&src);
}

#[test]
fn valid_app_dot_av() {
    let src = std::fs::read_to_string("examples/modules/app_dot.av")
        .expect("examples/modules/app_dot.av not found");
    let items = parse(&src);
    let errs = run_type_check_with_base(&items, Some("examples"))
        .into_iter()
        .map(|e| e.message)
        .collect::<Vec<_>>();
    assert!(
        errs.is_empty(),
        "expected no type errors for app_dot.av, got:\n  {}",
        errs.join("\n  ")
    );
}

#[test]
fn valid_app_av() {
    let src = std::fs::read_to_string("examples/modules/app.av")
        .expect("examples/modules/app.av not found");
    let items = parse(&src);
    let errs = run_type_check_with_base(&items, Some("examples"))
        .into_iter()
        .map(|e| e.message)
        .collect::<Vec<_>>();
    assert!(
        errs.is_empty(),
        "expected no type errors for app.av, got:\n  {}",
        errs.join("\n  ")
    );
}

#[test]
fn valid_services_weather_av() {
    let src = std::fs::read_to_string("examples/services/weather.av")
        .expect("examples/services/weather.av not found");
    let items = parse(&src);
    let errs = run_type_check_with_base(&items, Some("examples"))
        .into_iter()
        .map(|e| e.message)
        .collect::<Vec<_>>();
    assert!(
        errs.is_empty(),
        "expected no type errors for services/weather.av, got:\n  {}",
        errs.join("\n  ")
    );
}

#[test]
fn valid_call_to_exposed_module_member() {
    let src = "module App\n    depends [Modules.Models.User]\n    intent =\n        \"Uses exported function\"\nfn main() -> Unit\n    x = Modules.Models.User.nameById(1)\n";
    let errs = errors_with_base(src, "examples");
    assert!(
        errs.is_empty(),
        "expected no type errors, got:\n  {}",
        errs.join("\n  ")
    );
}

#[test]
fn valid_unqualified_imported_sum_constructor_call() {
    let root = temp_module_root("imported_sum_ctor");
    let domain_dir = root.join("Domain");
    std::fs::create_dir_all(&domain_dir).expect("create Domain dir failed");
    std::fs::write(
        domain_dir.join("Types.av"),
        r#"module Types
    exposes [TaskEvent]
    intent =
        "Shared events."

type TaskEvent
    TaskStarted(String)
"#,
    )
    .expect("write Types.av failed");

    let src = r#"module App
    depends [Domain.Types]
    intent =
        "Constructs an imported sum directly."

fn make() -> TaskEvent
    TaskEvent.TaskStarted("now")
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.is_empty(),
        "expected no type errors, got:\n  {}",
        errs.join("\n  ")
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn imported_sum_pattern_payload_bindings_use_exported_constructor_types() {
    let root = temp_module_root("imported_sum_pattern");
    let domain_dir = root.join("Domain");
    std::fs::create_dir_all(&domain_dir).expect("create Domain dir failed");
    std::fs::write(
        domain_dir.join("Types.av"),
        r#"module Types
    exposes [TaskEvent]
    intent =
        "Shared events."

type TaskEvent
    TaskStarted(String)
"#,
    )
    .expect("write Types.av failed");

    let src = r#"module App
    depends [Domain.Types]
    intent =
        "Matches imported payloads."

fn startedAt(event: TaskEvent) -> String
    match event
        TaskEvent.TaskStarted(at) -> at
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.is_empty(),
        "expected no type errors, got:\n  {}",
        errs.join("\n  ")
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn fully_qualified_imported_constructor_patterns_are_exhaustive() {
    let root = temp_module_root("imported_sum_qualified_pattern");
    let domain_dir = root.join("Domain");
    std::fs::create_dir_all(&domain_dir).expect("create Domain dir failed");
    std::fs::write(
        domain_dir.join("Types.av"),
        r#"module Types
    exposes [TaskEvent]
    intent =
        "Shared events."

type TaskEvent
    TaskStarted(String)
"#,
    )
    .expect("write Types.av failed");

    let src = r#"module App
    depends [Domain.Types]
    intent =
        "Uses a fully qualified constructor pattern."

fn startedAt(event: TaskEvent) -> String
    match event
        Domain.Types.TaskEvent.TaskStarted(at) -> at
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.is_empty(),
        "expected no type errors, got:\n  {}",
        errs.join("\n  ")
    );

    let _ = std::fs::remove_dir_all(&root);
}

// ---------------------------------------------------------------------------
// Type errors — must produce at least one error
// ---------------------------------------------------------------------------

#[test]
fn error_wrong_arg_count_too_few() {
    let src = "fn add(a: Int, b: Int) -> Int\n    a + b\nfn main() -> Unit\n    r = add(1)\n";
    // actual: "Function 'add' expects 2 argument(s), got 1"
    assert_error_containing(src, "argument(s)");
}

#[test]
fn error_wrong_arg_count_too_many() {
    let src = "fn add(a: Int, b: Int) -> Int\n    a + b\nfn main() -> Unit\n    r = add(1, 2, 3)\n";
    // actual: "Function 'add' expects 2 argument(s), got 3"
    assert_error_containing(src, "argument(s)");
}

#[test]
fn error_zero_arg_constructor_called_like_function() {
    let src = "fn bad() -> Int\n    x = 1\n    x()\n";
    assert_error_containing(src, "expected function, got Int");
}

#[test]
fn error_arg_type_mismatch_string_for_int() {
    let src =
        "fn add(a: Int, b: Int) -> Int\n    a + b\nfn main() -> Unit\n    r = add(1, \"two\")\n";
    // actual: "Argument 2 of 'add': expected Int, got String"
    assert_error_containing(src, "got String");
}

#[test]
fn error_unknown_does_not_satisfy_declared_return_type() {
    let src = concat!(
        "fn bad() -> Int\n",
        "    match []\n",
        "        [h, ..t] -> h\n",
        "        []       -> 0\n",
    );
    assert_error_containing(src, "body returns Unknown but declared return type is Int");
}

#[test]
fn error_unknown_does_not_satisfy_call_argument_type() {
    let src = concat!(
        "fn takesInt(x: Int) -> Int\n",
        "    x + 1\n",
        "fn bad() -> Int\n",
        "    n = match []\n",
        "        [h, ..t] -> h\n",
        "        []       -> 0\n",
        "    takesInt(n)\n",
    );
    assert_error_containing(src, "Argument 1 of 'takesInt': expected Int, got Unknown");
}

#[test]
fn error_list_contains_mismatched_element_type() {
    let src = "fn bad(xs: List<Int>) -> Bool\n    List.contains(xs, \"x\")\n";
    assert_error_containing(
        src,
        "Argument 2 of 'List.contains': expected Int, got String",
    );
}

#[test]
fn error_map_set_key_type_mismatch() {
    let src = concat!(
        "fn bad() -> Map<String, Int>\n",
        "    m = Map.fromList([(\"a\", 1)])\n",
        "    Map.set(m, 1, 42)\n",
    );
    assert_error_containing(src, "Argument 2 of 'Map.set': expected String, got Int");
}

#[test]
fn error_map_key_type_must_be_hashable_scalar() {
    let src = concat!(
        "fn bad() -> Map<List<Int>, Int>\n",
        "    Map.fromList([([1], 2)])\n",
    );
    assert_error_containing(src, "map key type must be Int, Float, String, or Bool");
}

#[test]
fn error_map_from_list_requires_tuple_pairs() {
    let src = concat!(
        "fn bad() -> Map<String, Int>\n",
        "    Map.fromList([[\"a\", 1]])\n",
    );
    assert_error_containing(src, "expected List<(K, V)>");
}

#[test]
fn error_map_literal_key_must_be_hashable_scalar() {
    let src = concat!("fn bad() -> Map<String, Int>\n", "    {[1] => 2}\n",);
    assert_error_containing(
        src,
        "Map literal key type must be Int, Float, String, or Bool",
    );
}

#[test]
fn error_map_literal_incompatible_key_types() {
    let src = concat!(
        "fn bad() -> Map<String, Int>\n",
        "    {\"a\" => 1, 2 => 3}\n",
    );
    assert_error_containing(src, "Map literal contains incompatible key types");
}

#[test]
fn binding_empty_list_literal_is_error() {
    let src = "x = []\n";
    assert_error_containing(src, "Binding 'x' to empty list literal is not allowed");
}

#[test]
fn error_list_prepend_mismatched_element_type() {
    let src = "fn bad(xs: List<Int>) -> List<Int>\n    List.prepend(\"x\", xs)\n";
    assert_error_containing(
        src,
        "Argument 1 of 'List.prepend': expected Int, got String",
    );
}

#[test]
fn error_list_concat_mismatched_element_type() {
    let src = "fn bad(xs: List<Int>) -> List<Int>\n    List.concat(xs, [\"x\"])\n";
    assert_error_containing(src, "list element types differ: Int vs String");
}

#[test]
fn error_effectful_callback_passed_to_pure_slot() {
    let src = "fn applyPure(f: Fn(Int) -> Int, x: Int) -> Int\n    f(x)\nfn logInc(n: Int) -> Int\n    ! [Console.print]\n    Console.print(n)\n    n + 1\nfn main() -> Unit\n    ! [Console.print]\n    r = applyPure(logInc, 1)\n";
    assert_error_containing(src, "Fn(Int) -> Int ! [Console.print]");
}

#[test]
fn error_unknown_type_annotation() {
    // Capitalized typos are now parsed as Named types; the error surfaces as a type mismatch
    // (body returns Named("Intger") but declared return is Unit)
    let src = "fn f(x: Intger) -> Unit\n    x\n";
    assert_error_containing(src, "Intger");
}

#[test]
fn error_unknown_return_type() {
    // Capitalized typos are now parsed as Named types; the error surfaces as a type mismatch
    // (body returns String but declared return is Named("Strng"))
    let src = "fn f() -> Strng\n    \"hi\"\n";
    assert_error_containing(src, "Strng");
}

#[test]
fn error_duplicate_binding_in_fn() {
    let src = "fn f() -> Unit\n    x = 0\n    x = 1\n";
    assert_error_containing(src, "already defined");
}

#[test]
fn error_unknown_ident_inside_interpolated_string() {
    let src = "fn f() -> String\n    \"hello {bogus}\"\n";
    assert_error_containing(src, "Unknown identifier 'bogus'");
}

#[test]
fn error_binop_int_plus_string() {
    let src = "fn f(a: Int, b: String) -> Int\n    a + b\n";
    // actual: "Operator '+' requires Int/Float or String on both sides, got Int and String"
    assert_error_containing(src, "requires");
}

#[test]
fn error_undeclared_effect() {
    // Calling a function with an effect from a function without that effect declared
    let src = "fn log(msg: String) -> Unit\n    ! [Io]\n    Console.print(msg)\nfn caller(x: String) -> Unit\n    log(x)\n";
    assert_error_containing(src, "Io");
}

#[test]
fn error_main_undeclared_console_effect() {
    let src = "fn main() -> Unit\n    Console.print(\"hi\")\n";
    assert_error_containing(src, "main");
    assert_error_containing(src, "Console");
}

#[test]
fn error_top_level_undeclared_console_effect() {
    let src = "Console.print(\"hi\")\n";
    assert_error_containing(src, "<top-level>");
    assert_error_containing(src, "Console");
}

#[test]
fn error_verify_undeclared_console_effect() {
    let src =
        "fn main() -> Int\n    0\nverify main\n    Console.print(\"x\") => Console.print(\"x\")\n";
    assert_error_containing(src, "<verify:main>");
    assert_error_containing(src, "Console");
}

#[test]
fn error_undeclared_effect_from_function_typed_callback() {
    let src = "fn applyOnce(f: Fn(Int) -> Int ! [Console.print], x: Int) -> Int\n    f(x)\nfn pureInc(n: Int) -> Int\n    n + 1\n";
    assert_error_containing(src, "has effect 'Console.print");
}

#[test]
fn error_call_to_unexposed_module_member() {
    let src = "module App\n    depends [Modules.Models.User]\n    intent =\n        \"Tries to use hidden member\"\nfn main() -> Unit\n    x = Modules.Models.User.hidden()\n";
    let errs = errors_with_base(src, "examples");
    assert!(
        errs.iter()
            .any(|e| e.contains("Modules.Models.User.hidden")),
        "expected exposes error mentioning Modules.Models.User.hidden, got:\n  {}",
        if errs.is_empty() {
            "<no errors>".to_string()
        } else {
            errs.join("\n  ")
        }
    );
}

// ---------------------------------------------------------------------------
// Effect propagation
// ---------------------------------------------------------------------------

#[test]
fn valid_effect_propagated_correctly() {
    // caller declares the same effect as callee
    let src = "fn log(msg: String) -> Unit\n    ! [Console.print]\n    Console.print(msg)\nfn caller(x: String) -> Unit\n    ! [Console.print]\n    log(x)\n";
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// Error propagation operator (?)
// ---------------------------------------------------------------------------

#[test]
fn valid_error_prop_in_result_fn() {
    // ? on a Result<Int, String> inside a function returning Result<Int, String> — valid.
    let src = "fn safe(r: Result<Int, String>) -> Result<Int, String>\n    Result.Ok(r?)\n";
    assert_no_errors(src);
}

#[test]
fn error_prop_in_non_result_fn() {
    // ? used inside a function that returns Int — type error.
    let src = "fn bad(r: Result<Int, String>) -> Int\n    r?\n";
    assert_error_containing(src, "not Result");
}

#[test]
fn error_prop_on_non_result_type() {
    // ? applied to an Int — type error.
    let src = "fn bad(n: Int) -> Result<Int, String>\n    n?\n";
    assert_error_containing(src, "can only be applied to Result");
}

#[test]
fn error_prop_incompatible_err_types() {
    // Inner Err is String, outer function expects Err = Int — incompatible.
    let src = "fn inner(x: Int) -> Result<Int, String>\n    Result.Ok(x)\nfn outer(x: Int) -> Result<Int, Int>\n    Result.Ok(inner(x)?)\n";
    assert_error_containing(src, "incompatible");
}

// ---------------------------------------------------------------------------
// User-defined types — type checker integration
// ---------------------------------------------------------------------------

#[test]
fn valid_sum_type_definition() {
    let src = "type Shape\n  Circle(Float)\n  Rect(Float, Float)\n  Point\n";
    assert_no_errors(src);
}

#[test]
fn valid_record_definition() {
    let src = "record User\n  name: String\n  age: Int\n";
    assert_no_errors(src);
}

#[test]
fn valid_sum_type_constructor_call() {
    let src = "type Shape\n  Circle(Float)\n  Point\nc = Shape.Circle(3.14)\n";
    assert_no_errors(src);
}

#[test]
fn valid_record_creation() {
    let src = "record User\n  name: String\n  age: Int\nu = User(name = \"Alice\", age = 30)\n";
    assert_no_errors(src);
}

#[test]
fn error_record_creation_missing_required_field() {
    let src = "record User\n  name: String\n  age: Int\nu = User(name = \"Alice\")\n";
    assert_error_containing(src, "missing required field 'age'");
}

#[test]
fn error_record_creation_unknown_field() {
    let src = "record User\n  name: String\n  age: Int\nu = User(name = \"Alice\", age = 30, admin = true)\n";
    assert_error_containing(src, "has no field 'admin'");
}

#[test]
fn error_record_creation_duplicate_field() {
    let src =
        "record User\n  name: String\n  age: Int\nu = User(name = \"A\", name = \"B\", age = 30)\n";
    assert_error_containing(src, "field 'name' provided more than once");
}

#[test]
fn error_record_creation_field_type_mismatch() {
    let src = "record User\n  name: String\n  age: Int\nu = User(name = \"Alice\", age = \"30\")\n";
    assert_error_containing(src, "field 'age' expects Int, got String");
}

#[test]
fn named_types_are_compatible_with_same_name() {
    // Two Named("Shape") values should be compatible
    use aver::types::Type;
    let a = Type::Named("Shape".to_string());
    let b = Type::Named("Shape".to_string());
    assert!(a.compatible(&b));
}

#[test]
fn named_types_are_incompatible_with_different_names() {
    use aver::types::Type;
    let a = Type::Named("Shape".to_string());
    let b = Type::Named("User".to_string());
    assert!(!a.compatible(&b));
}

#[test]
fn named_type_compatible_with_unknown_fallback() {
    use aver::types::Type;
    let named = Type::Named("Shape".to_string());
    assert!(named.compatible(&Type::Unknown));
    assert!(Type::Unknown.compatible(&named));
}

#[test]
fn valid_function_using_user_type_parameter() {
    let src = concat!(
        "type Shape\n",
        "  Circle(Float)\n",
        "  Point\n",
        "fn area(s: Shape) -> Float\n",
        "  ? \"area\"\n",
        "  match s\n",
        "    Shape.Circle(r) -> r * r\n",
        "    Shape.Point -> 0.0\n",
    );
    assert_no_errors(src);
}

#[test]
fn effect_aliases_are_parse_errors() {
    let src = concat!(
        "effects AppIO = [Console.print]\n",
        "fn greet() -> Unit\n",
        "    ! [AppIO]\n",
        "    Console.print(\"hi\")\n"
    );
    assert_parse_error_containing(src, "Effect aliases were removed");
}

#[test]
fn exact_effects_must_be_declared_directly() {
    let src = concat!(
        "fn log(msg: String) -> Unit\n",
        "    ! [Console.print]\n",
        "    Console.print(msg)\n",
        "fn process() -> Unit\n",
        "    ! [Console.print]\n",
        "    log(\"processing\")\n",
    );
    assert_no_errors(src);
}

#[test]
fn removed_effect_aliases_fail_before_typecheck() {
    let src = concat!(
        "effects Silent = []\n",
        "fn greet() -> Unit\n",
        "    ! [Silent]\n",
        "    Console.print(\"hi\")\n",
    );
    assert_parse_error_containing(src, "Effect aliases were removed");
}

// ---------------------------------------------------------------------------
// Network effect
// ---------------------------------------------------------------------------

#[test]
fn error_network_get_without_effect() {
    let src = concat!(
        "fn fetch(url: String) -> Result<HttpResponse, String>\n",
        "    Http.get(url)\n",
    );
    assert_error_containing(src, "has effect 'Http.get'");
}

#[test]
fn error_network_post_without_effect() {
    let src = concat!(
        "fn send(url: String, body: String) -> Result<HttpResponse, String>\n",
        "    Http.post(url, body, \"application/json\", [])\n",
    );
    assert_error_containing(src, "has effect 'Http.post'");
}

#[test]
fn valid_network_get_with_effect() {
    let src = concat!(
        "fn fetch(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http.get]\n",
        "    Http.get(url)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_network_post_with_effect() {
    let src = concat!(
        "fn send(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http.post]\n",
        "    Http.post(url, \"{}\", \"application/json\", [])\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_network_post_with_typed_headers() {
    let src = concat!(
        "fn send(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http.post]\n",
        "    headers = [Header(name = \"Authorization\", value = \"Bearer token\")]\n",
        "    Http.post(url, \"{}\", \"application/json\", headers)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_network_post_headers_wrong_type() {
    let src = concat!(
        "fn send(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http.post]\n",
        "    Http.post(url, \"{}\", \"application/json\", [\"bad\"])\n",
    );
    assert_error_containing(src, "Argument 4 of 'Http.post': expected List<Header>");
}

#[test]
fn valid_network_all_methods_with_effect() {
    let src = concat!(
        "fn callAll(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http.delete]\n",
        "    Http.delete(url)\n",
    );
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// Disk service effect checking
// ---------------------------------------------------------------------------

#[test]
fn error_disk_read_without_effect() {
    let src = concat!(
        "fn loadCfg() -> Result<String, String>\n",
        "    Disk.readText(\"config.av\")\n",
    );
    assert_error_containing(src, "has effect 'Disk.readText'");
}

#[test]
fn error_disk_write_without_effect() {
    let src = concat!(
        "fn save() -> Result<Unit, String>\n",
        "    Disk.writeText(\"out.txt\", \"data\")\n",
    );
    assert_error_containing(src, "has effect 'Disk.writeText'");
}

#[test]
fn valid_disk_read_with_effect() {
    let src = concat!(
        "fn loadCfg() -> Result<String, String>\n",
        "    ! [Disk.readText]\n",
        "    Disk.readText(\"config.av\")\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_disk_all_methods_with_effect() {
    let src = concat!(
        "fn ops(p: String) -> Result<String, String>\n",
        "    ! [Disk.writeText, Disk.appendText, Disk.exists, Disk.delete, Disk.listDir, Disk.makeDir, Disk.readText]\n",
        "    Disk.writeText(p, \"x\")\n",
        "    Disk.appendText(p, \"y\")\n",
        "    Disk.exists(p)\n",
        "    Disk.delete(p)\n",
        "    Disk.listDir(p)\n",
        "    Disk.makeDir(p)\n",
        "    Disk.readText(p)\n",
    );
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// Console.error / warn / readLine
// ---------------------------------------------------------------------------

#[test]
fn error_console_error_without_effect() {
    let src = concat!(
        "fn report(msg: String) -> Unit\n",
        "    Console.error(msg)\n",
    );
    assert_error_containing(src, "has effect 'Console.error'");
}

#[test]
fn error_console_warn_without_effect() {
    let src = concat!(
        "fn report(msg: String) -> Unit\n",
        "    Console.warn(msg)\n",
    );
    assert_error_containing(src, "has effect 'Console.warn'");
}

#[test]
fn error_console_read_line_without_effect() {
    let src = concat!(
        "fn ask() -> Result<String, String>\n",
        "    Console.readLine()\n",
    );
    assert_error_containing(src, "has effect 'Console.readLine'");
}

#[test]
fn valid_console_all_methods_with_effect() {
    let src = concat!(
        "fn run(msg: String) -> Result<String, String>\n",
        "    ! [Console.print, Console.error, Console.warn, Console.readLine]\n",
        "    Console.print(msg)\n",
        "    Console.error(msg)\n",
        "    Console.warn(msg)\n",
        "    Console.readLine()\n",
    );
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// Time service effect checking
// ---------------------------------------------------------------------------

#[test]
fn error_time_now_without_effect() {
    let src = concat!("fn ts() -> String\n", "    Time.now()\n",);
    assert_error_containing(src, "has effect 'Time.now'");
}

#[test]
fn error_time_unix_ms_without_effect() {
    let src = concat!("fn ts() -> Int\n", "    Time.unixMs()\n",);
    assert_error_containing(src, "has effect 'Time.unixMs'");
}

#[test]
fn error_time_sleep_without_effect() {
    let src = concat!("fn wait() -> Unit\n", "    Time.sleep(1)\n",);
    assert_error_containing(src, "has effect 'Time.sleep'");
}

#[test]
fn valid_time_calls_with_effect() {
    let src = concat!(
        "fn run() -> Int\n",
        "    ! [Time.sleep, Time.unixMs]\n",
        "    Time.sleep(1)\n",
        "    Time.unixMs()\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_time_sleep_negative_constant() {
    let src = concat!(
        "fn wait() -> Unit\n",
        "    ! [Time.sleep]\n",
        "    Time.sleep(0 - 1)\n",
    );
    assert_error_containing(
        src,
        "Argument 1 of 'Time.sleep' must be a non-negative Int constant",
    );
}

// ---------------------------------------------------------------------------
// Env service effect checking
// ---------------------------------------------------------------------------

#[test]
fn error_env_get_without_effect() {
    let src = concat!("fn read(k: String) -> Option<String>\n", "    Env.get(k)\n",);
    assert_error_containing(src, "has effect 'Env.get'");
}

#[test]
fn error_env_set_without_effect() {
    let src = concat!("fn write() -> Unit\n", "    Env.set(\"A\", \"1\")\n",);
    assert_error_containing(src, "has effect 'Env.set'");
}

#[test]
fn valid_env_get_and_set_with_effect() {
    let src = concat!(
        "fn run() -> Option<String>\n",
        "    ! [Env.set, Env.get]\n",
        "    Env.set(\"A\", \"1\")\n",
        "    Env.get(\"A\")\n",
    );
    assert_no_errors(src);
}

#[test]
fn env_get_only_does_not_allow_env_set() {
    let src = concat!(
        "fn run() -> Unit\n",
        "    ! [Env.get]\n",
        "    Env.set(\"A\", \"1\")\n",
    );
    assert_error_containing(src, "has effect 'Env.set'");
}

// ---------------------------------------------------------------------------
// Record field access type checking
// ---------------------------------------------------------------------------

#[test]
fn valid_network_response_field_access() {
    // resp.status is Int — comparison with Int should pass
    let src = concat!(
        "fn isOk(resp: HttpResponse) -> Bool\n",
        "    resp.status < 400\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_network_response_body_field() {
    let src = concat!("fn body(resp: HttpResponse) -> String\n", "    resp.body\n",);
    assert_no_errors(src);
}

#[test]
fn valid_header_field_access() {
    let src = concat!(
        "record Header\n",
        "    name: String\n",
        "    value: String\n",
        "fn headerName(h: Header) -> String\n",
        "    h.name\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_user_record_field_access() {
    let src = concat!(
        "record User\n",
        "    name: String\n",
        "    age: Int\n",
        "fn getName(u: User) -> String\n",
        "    u.name\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_network_response_unknown_field() {
    let src = concat!("fn bad(resp: HttpResponse) -> String\n", "    resp.fooo\n",);
    assert_error_containing(src, "has no field 'fooo'");
}

#[test]
fn error_user_record_unknown_field() {
    let src = concat!(
        "record User\n",
        "    name: String\n",
        "fn bad(u: User) -> String\n",
        "    u.email\n",
    );
    assert_error_containing(src, "has no field 'email'");
}

// ---------------------------------------------------------------------------
// Tcp service
// ---------------------------------------------------------------------------

#[test]
fn valid_tcp_send_with_effect() {
    let src = concat!(
        "fn talk(host: String, port: Int, msg: String) -> Result<String, String>\n",
        "    ! [Tcp.send]\n",
        "    Tcp.send(host, port, msg)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_ping_with_effect() {
    let src = concat!(
        "fn check(host: String, port: Int) -> Result<Unit, String>\n",
        "    ! [Tcp.ping]\n",
        "    Tcp.ping(host, port)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_tcp_send_without_effect() {
    let src = concat!(
        "fn talk(host: String, port: Int, msg: String) -> Result<String, String>\n",
        "    Tcp.send(host, port, msg)\n",
    );
    assert_error_containing(src, "has effect 'Tcp.send'");
}

#[test]
fn error_tcp_ping_without_effect() {
    let src = concat!(
        "fn check(host: String, port: Int) -> Result<Unit, String>\n",
        "    Tcp.ping(host, port)\n",
    );
    assert_error_containing(src, "has effect 'Tcp.ping'");
}

#[test]
fn valid_http_server_listen_with_context() {
    let src = concat!(
        "fn handle(ctx: String, req: HttpRequest) -> HttpResponse\n",
        "    HttpResponse(status = 200, body = ctx, headers = [])\n",
        "fn main() -> Unit\n",
        "    ! [HttpServer.listenWith]\n",
        "    HttpServer.listenWith(8080, \"ok\", handle)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_http_server_listen_with_bad_handler_signature_uses_any_in_message() {
    let src = concat!(
        "fn bad(ctx: Int, req: Int) -> HttpResponse\n",
        "    HttpResponse(status = 200, body = \"ok\", headers = [])\n",
        "fn main() -> Unit\n",
        "    ! [HttpServer.listenWith]\n",
        "    HttpServer.listenWith(8080, \"ok\", bad)\n",
    );
    assert_error_containing(src, "expected Fn(Any, HttpRequest) -> HttpResponse");
}

#[test]
fn valid_tcp_connect_returns_connection() {
    let src = concat!(
        "fn open(host: String, port: Int) -> Result<Tcp.Connection, String>\n",
        "    ! [Tcp.connect]\n",
        "    Tcp.connect(host, port)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_write_line_with_connection() {
    let src = concat!(
        "fn send(conn: Tcp.Connection, msg: String) -> Result<Unit, String>\n",
        "    ! [Tcp.writeLine]\n",
        "    Tcp.writeLine(conn, msg)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_read_line_with_connection() {
    let src = concat!(
        "fn recv(conn: Tcp.Connection) -> Result<String, String>\n",
        "    ! [Tcp.readLine]\n",
        "    Tcp.readLine(conn)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_close_with_connection() {
    let src = concat!(
        "fn done(conn: Tcp.Connection) -> Result<Unit, String>\n",
        "    ! [Tcp.close]\n",
        "    Tcp.close(conn)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_connection_field_access() {
    let src = concat!(
        "fn getId(conn: Tcp.Connection) -> String\n",
        "    conn.id\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_tcp_write_line_with_string() {
    let src = concat!(
        "fn send(conn: String, msg: String) -> Result<Unit, String>\n",
        "    ! [Tcp.writeLine]\n",
        "    Tcp.writeLine(conn, msg)\n",
    );
    assert_error_containing(src, "expected Tcp.Connection");
}

// ---------------------------------------------------------------------------
// Int / Float / String namespace type checking
// ---------------------------------------------------------------------------

#[test]
fn valid_int_to_string() {
    assert_no_errors("fn f(n: Int) -> String\n    Int.toString(n)\n");
}

#[test]
fn valid_int_from_string() {
    assert_no_errors("fn f(s: String) -> Result<Int, String>\n    Int.fromString(s)\n");
}

#[test]
fn valid_int_abs() {
    assert_no_errors("fn f(n: Int) -> Int\n    Int.abs(n)\n");
}

#[test]
fn valid_int_min_max() {
    assert_no_errors("fn f(a: Int, b: Int) -> Int\n    Int.min(a, Int.max(a, b))\n");
}

#[test]
fn valid_int_mod() {
    assert_no_errors("fn f(a: Int, b: Int) -> Result<Int, String>\n    Int.mod(a, b)\n");
}

#[test]
fn valid_int_to_float() {
    assert_no_errors("fn f(n: Int) -> Float\n    Int.toFloat(n)\n");
}

#[test]
fn valid_float_abs() {
    assert_no_errors("fn f(x: Float) -> Float\n    Float.abs(x)\n");
}

#[test]
fn valid_float_floor_ceil_round() {
    assert_no_errors("fn f(x: Float) -> Int\n    Float.floor(x)\n");
    assert_no_errors("fn f(x: Float) -> Int\n    Float.ceil(x)\n");
    assert_no_errors("fn f(x: Float) -> Int\n    Float.round(x)\n");
}

#[test]
fn valid_float_from_int() {
    assert_no_errors("fn f(n: Int) -> Float\n    Float.fromInt(n)\n");
}

#[test]
fn valid_float_to_string() {
    assert_no_errors("fn f(x: Float) -> String\n    Float.toString(x)\n");
}

#[test]
fn valid_string_length() {
    assert_no_errors("fn f(s: String) -> Int\n    String.len(s)\n");
}

#[test]
fn valid_string_byte_length() {
    assert_no_errors("fn f(s: String) -> Int\n    String.byteLength(s)\n");
}

#[test]
fn valid_string_starts_with() {
    assert_no_errors("fn f(s: String, p: String) -> Bool\n    String.startsWith(s, p)\n");
}

#[test]
fn valid_string_contains() {
    assert_no_errors("fn f(s: String, sub: String) -> Bool\n    String.contains(s, sub)\n");
}

#[test]
fn valid_string_slice() {
    assert_no_errors("fn f(s: String) -> String\n    String.slice(s, 0, 3)\n");
}

#[test]
fn valid_string_trim() {
    assert_no_errors("fn f(s: String) -> String\n    String.trim(s)\n");
}

#[test]
fn valid_string_split() {
    assert_no_errors("fn f(s: String) -> List<String>\n    String.split(s, \",\")\n");
}

#[test]
fn valid_string_replace() {
    assert_no_errors("fn f(s: String) -> String\n    String.replace(s, \"a\", \"b\")\n");
}

#[test]
fn valid_string_join() {
    assert_no_errors("fn f(xs: List<String>) -> String\n    String.join(xs, \",\")\n");
}

#[test]
fn valid_string_chars() {
    assert_no_errors("fn f(s: String) -> List<String>\n    String.chars(s)\n");
}

#[test]
fn valid_string_from_int() {
    assert_no_errors("fn f(n: Int) -> String\n    String.fromInt(n)\n");
}

#[test]
fn valid_string_from_float() {
    assert_no_errors("fn f(x: Float) -> String\n    String.fromFloat(x)\n");
}

#[test]
fn valid_string_from_bool() {
    assert_no_errors("fn f(b: Bool) -> String\n    String.fromBool(b)\n");
}

#[test]
fn error_int_to_string_wrong_arg() {
    assert_error_containing(
        "fn f(s: String) -> String\n    Int.toString(s)\n",
        "expected Int, got String",
    );
}

#[test]
fn error_float_abs_wrong_arg() {
    // String is incompatible with Float (unlike Int which widens)
    assert_error_containing(
        "fn f(s: String) -> Float\n    Float.abs(s)\n",
        "expected Float, got String",
    );
}

#[test]
fn valid_no_effects_for_helpers() {
    // Int/Float/String namespace methods don't require effects
    assert_no_errors("fn f(n: Int) -> String\n    Int.toString(n)\n");
    assert_no_errors("fn f(x: Float) -> Int\n    Float.floor(x)\n");
    assert_no_errors("fn f(s: String) -> Int\n    String.len(s)\n");
}

#[test]
fn error_duplicate_top_level_binding() {
    assert_error_containing("x = 1\nx = 2\n", "'x' is already defined");
}

// ---------------------------------------------------------------------------
// Typed bindings: `name: Type = expr`
// ---------------------------------------------------------------------------

#[test]
fn valid_typed_binding_matches_inferred() {
    let src = "fn f() -> Int\n    x: Int = 5\n    x\n";
    assert_no_errors(src);
}

#[test]
fn valid_typed_binding_top_level() {
    let src = "x: Int = 42\n";
    assert_no_errors(src);
}

#[test]
fn error_typed_binding_mismatch() {
    let src = "fn f() -> Unit\n    x: Int = \"hello\"\n    x\n";
    assert_error_containing(
        src,
        "Binding 'x': expression has type String, annotation says Int",
    );
}

#[test]
fn error_typed_binding_unknown_type() {
    // Capitalized identifiers parse as Named("Foo"), producing a type mismatch
    let src = "fn f() -> Unit\n    x: Foo = 5\n    x\n";
    assert_error_containing(
        src,
        "Binding 'x': expression has type Int, annotation says Foo",
    );
}

#[test]
fn valid_typed_binding_empty_list_with_annotation() {
    // With a type annotation, empty list binding should be allowed
    let src = "fn f() -> List<Int>\n    xs: List<Int> = []\n    xs\n";
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// Record update
// ---------------------------------------------------------------------------

#[test]
fn valid_record_update() {
    let src = r#"
record User
    name: String
    age: Int

fn f(u: User) -> User
    User.update(u, age = 31)
"#;
    assert_no_errors(src);
}

#[test]
fn error_record_update_unknown_field() {
    let src = r#"
record User
    name: String
    age: Int

fn f(u: User) -> User
    User.update(u, foo = 1)
"#;
    assert_error_containing(src, "Record 'User' has no field 'foo'");
}

#[test]
fn error_record_update_field_type_mismatch() {
    let src = r#"
record User
    name: String
    age: Int

fn f(u: User) -> User
    User.update(u, age = "old")
"#;
    assert_error_containing(src, "Record 'User' field 'age' expects Int, got String");
}

#[test]
fn error_record_update_wrong_base_type() {
    let src = r#"
record User
    name: String
    age: Int

record Point
    x: Int
    y: Int

fn f(u: User) -> Point
    Point.update(u, x = 1)
"#;
    assert_error_containing(src, "Point.update: base has type User, expected Point");
}

// ---------------------------------------------------------------------------
// Match exhaustiveness checking
// ---------------------------------------------------------------------------

#[test]
fn exhaustive_bool_both_branches() {
    let src = concat!(
        "fn f(b: Bool) -> Int\n",
        "  match b\n",
        "    true -> 1\n",
        "    false -> 0\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_non_exhaustive_bool_missing_false() {
    let src = concat!("fn f(b: Bool) -> Int\n", "  match b\n", "    true -> 1\n",);
    assert_error_containing(src, "false");
}

#[test]
fn exhaustive_result_both_constructors() {
    let src = concat!(
        "fn f(r: Result<Int, String>) -> Int\n",
        "  match r\n",
        "    Result.Ok(x) -> x\n",
        "    Result.Err(e) -> 0\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_non_exhaustive_result_missing_err() {
    let src = concat!(
        "fn f(r: Result<Int, String>) -> Int\n",
        "  match r\n",
        "    Result.Ok(x) -> x\n",
    );
    assert_error_containing(src, "Result.Err");
}

#[test]
fn exhaustive_option_both_constructors() {
    let src = concat!(
        "fn f(o: Option<Int>) -> Int\n",
        "  match o\n",
        "    Option.Some(x) -> x\n",
        "    Option.None -> 0\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_non_exhaustive_option_missing_none() {
    let src = concat!(
        "fn f(o: Option<Int>) -> Int\n",
        "  match o\n",
        "    Option.Some(x) -> x\n",
    );
    assert_error_containing(src, "Option.None");
}

#[test]
fn exhaustive_user_sum_type_all_variants() {
    let src = concat!(
        "type Shape\n",
        "  Circle(Float)\n",
        "  Rect(Float, Float)\n",
        "  Point\n",
        "fn area(s: Shape) -> Float\n",
        "  match s\n",
        "    Shape.Circle(r) -> r * r\n",
        "    Shape.Rect(w, h) -> w * h\n",
        "    Shape.Point -> 0.0\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_non_exhaustive_user_sum_type_missing_variant() {
    let src = concat!(
        "type Shape\n",
        "  Circle(Float)\n",
        "  Rect(Float, Float)\n",
        "  Point\n",
        "fn area(s: Shape) -> Float\n",
        "  match s\n",
        "    Shape.Circle(r) -> r * r\n",
    );
    assert_error_containing(src, "Rect");
}

#[test]
fn exhaustive_list_both_patterns() {
    let src = concat!(
        "fn f(xs: List<Int>) -> Int\n",
        "  match xs\n",
        "    [] -> 0\n",
        "    [h, ..t] -> h\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_non_exhaustive_list_missing_cons() {
    let src = concat!(
        "fn f(xs: List<Int>) -> Int\n",
        "  match xs\n",
        "    [] -> 0\n",
    );
    assert_error_containing(src, "[h, ..t]");
}

#[test]
fn error_non_exhaustive_int_without_catch_all() {
    let src = concat!(
        "fn f(n: Int) -> Int\n",
        "  match n\n",
        "    0 -> 1\n",
        "    1 -> 2\n",
    );
    assert_error_containing(src, "catch-all");
}

#[test]
fn exhaustive_int_with_wildcard() {
    let src = concat!(
        "fn f(n: Int) -> Int\n",
        "  match n\n",
        "    0 -> 1\n",
        "    _ -> 0\n",
    );
    assert_no_errors(src);
}

#[test]
fn exhaustive_with_ident_catch_all() {
    let src = concat!(
        "fn f(n: Int) -> Int\n",
        "  match n\n",
        "    0 -> 1\n",
        "    x -> x\n",
    );
    assert_no_errors(src);
}

#[test]
fn exhaustive_tuple_with_binding_wildcards() {
    let src = concat!(
        "fn f(p: (Int, Int)) -> Int\n",
        "  match p\n",
        "    (_, x) -> x\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_non_exhaustive_tuple_with_literal_only() {
    let src = concat!(
        "fn f(p: (Int, Int)) -> Int\n",
        "  match p\n",
        "    (0, x) -> x\n",
    );
    assert_error_containing(src, "catch-all");
}

// ---------------------------------------------------------------------------
// Granular sub-effects
// ---------------------------------------------------------------------------

#[test]
fn valid_granular_effect_http_get() {
    // ! [Http.get] allows Http.get
    let src = concat!(
        "fn fetch(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http.get]\n",
        "    Http.get(url)\n",
    );
    assert_no_errors(src);
}

#[test]
fn parent_effect_covers_child() {
    // Namespace shorthand: ! [Http] covers Http.get
    let src = concat!(
        "fn fetch(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http]\n",
        "    Http.get(url)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_granular_effect_blocks_other_method() {
    // ! [Http.get] does NOT allow Http.post
    let src = concat!(
        "fn send(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http.get]\n",
        "    Http.post(url, \"{}\", \"application/json\", [])\n",
    );
    assert_error_containing(src, "has effect 'Http.post'");
}

#[test]
fn removed_effect_alias_parse_error() {
    let src = concat!(
        "effects ReadOnly = [Http.get, Disk.readText]\n",
        "fn load(url: String, path: String) -> Result<String, String>\n",
        "    ! [ReadOnly]\n",
        "    Http.get(url)\n",
        "    Disk.readText(path)\n",
    );
    assert_parse_error_containing(src, "Effect aliases were removed");
}

#[test]
fn removed_effect_alias_blocks_nothing_because_it_is_parse_error() {
    let src = concat!(
        "effects ReadOnly = [Http.get, Disk.readText]\n",
        "fn save(path: String) -> Result<Unit, String>\n",
        "    ! [ReadOnly]\n",
        "    Disk.writeText(path, \"data\")\n",
    );
    assert_parse_error_containing(src, "Effect aliases were removed");
}

#[test]
fn valid_mix_explicit_effects() {
    let src = concat!(
        "fn mixed(url: String, path: String) -> Result<String, String>\n",
        "    ! [Http.post, Disk.readText]\n",
        "    Http.post(url, \"{}\", \"application/json\", [])\n",
        "    Disk.readText(path)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_mix_explicit_effects_blocks_uncovered() {
    let src = concat!(
        "fn mixed(url: String, path: String) -> Result<Unit, String>\n",
        "    ! [Http.post, Disk.readText]\n",
        "    Disk.writeText(path, \"data\")\n",
    );
    assert_error_containing(src, "has effect 'Disk.writeText'");
}

#[test]
fn valid_multiple_granular_console_effects() {
    // ! [Console.print, Console.error] allows both
    let src = concat!(
        "fn log(msg: String) -> Unit\n",
        "    ! [Console.print, Console.error]\n",
        "    Console.print(msg)\n",
        "    Console.error(msg)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_granular_console_blocks_other_method() {
    // ! [Console.print] does NOT allow Console.readLine
    let src = concat!(
        "fn ask() -> Result<String, String>\n",
        "    ! [Console.print]\n",
        "    Console.readLine()\n",
    );
    assert_error_containing(src, "has effect 'Console.readLine'");
}

#[test]
fn error_effect_alias_syntax_is_removed_even_for_cycles() {
    let src = concat!(
        "effects A = [B]\n",
        "effects B = [A]\n",
        "fn greet() -> Unit\n",
        "    ! [A]\n",
        "    Console.print(\"hi\")\n",
    );
    assert_parse_error_containing(src, "Effect aliases were removed");
}

// ---------------------------------------------------------------------------
// Opaque types
// ---------------------------------------------------------------------------

#[test]
fn opaque_record_blocks_construction() {
    let root = temp_module_root("opaque_construct");
    let pricing_dir = root.join("Pricing");
    std::fs::create_dir_all(&pricing_dir).expect("create Pricing dir failed");
    std::fs::write(
        pricing_dir.join("Discount.av"),
        r#"module Discount
    exposes [mkDiscount]
    exposes opaque [Discount]
    intent = "Opaque discount."

record Discount
    percent: Float

fn mkDiscount(p: Float) -> Result<Discount, String>
    ? "Factory."
    Result.Ok(Discount(percent = p))

verify mkDiscount
    mkDiscount(50.0) => Result.Ok(Discount(percent = 50.0))
"#,
    )
    .expect("write Discount.av failed");

    let src = r#"module App
    depends [Pricing.Discount]
    intent = "Tries to construct opaque."

fn bad() -> Discount
    Discount(percent = 50.0)
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.iter()
            .any(|e| e.contains("Cannot construct opaque type")),
        "expected opaque construction error, got: {:?}",
        errs
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn opaque_record_blocks_field_access() {
    let root = temp_module_root("opaque_field");
    let pricing_dir = root.join("Pricing");
    std::fs::create_dir_all(&pricing_dir).expect("create Pricing dir failed");
    std::fs::write(
        pricing_dir.join("Discount.av"),
        r#"module Discount
    exposes [mkDiscount]
    exposes opaque [Discount]
    intent = "Opaque discount."

record Discount
    percent: Float

fn mkDiscount(p: Float) -> Result<Discount, String>
    ? "Factory."
    Result.Ok(Discount(percent = p))

verify mkDiscount
    mkDiscount(50.0) => Result.Ok(Discount(percent = 50.0))
"#,
    )
    .expect("write Discount.av failed");

    let src = r#"module App
    depends [Pricing.Discount]
    intent = "Tries to access opaque field."

fn bad(d: Discount) -> Float
    d.percent
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.iter()
            .any(|e| e.contains("Cannot access field") && e.contains("opaque")),
        "expected opaque field access error, got: {:?}",
        errs
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn opaque_type_usable_in_signatures() {
    let root = temp_module_root("opaque_sig");
    let pricing_dir = root.join("Pricing");
    std::fs::create_dir_all(&pricing_dir).expect("create Pricing dir failed");
    std::fs::write(
        pricing_dir.join("Discount.av"),
        r#"module Discount
    exposes [mkDiscount, percent]
    exposes opaque [Discount]
    intent = "Opaque discount."

record Discount
    percent: Float

fn mkDiscount(p: Float) -> Result<Discount, String>
    ? "Factory."
    Result.Ok(Discount(percent = p))

verify mkDiscount
    mkDiscount(50.0) => Result.Ok(Discount(percent = 50.0))

fn percent(d: Discount) -> Float
    ? "Accessor."
    d.percent

verify percent
    percent(Discount(percent = 42.0)) => 42.0
"#,
    )
    .expect("write Discount.av failed");

    let src = r#"module App
    depends [Pricing.Discount]
    intent = "Uses opaque type through API."

fn apply(d: Discount) -> Float
    Pricing.Discount.percent(d)
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.is_empty(),
        "expected no errors for opaque type in signatures, got: {:?}",
        errs
    );

    let _ = std::fs::remove_dir_all(&root);
}

// ---------------------------------------------------------------------------
// Oracle v1 — `given name: EffectRef = [stubs]` syntax
// ---------------------------------------------------------------------------

#[test]
fn given_oracle_ref_random_int_binds_branch_indexed_sig() {
    // `given rnd: Random.int = [stub]` should bind `rnd` to the oracle
    // signature `(BranchPath, Int, Int, Int) -> Int`. The stub function
    // matches that signature, and the law body uses `rnd` with args of
    // that shape on the RHS, so the law must type-check cleanly.
    let src = concat!(
        "fn stub(path: BranchPath, n: Int, min: Int, max: Int) -> Int\n",
        "    min\n",
        "fn caller() -> Int\n",
        "    ! [Random.int]\n",
        "    Random.int(0, 10)\n",
        "verify caller law consistent\n",
        "    given rnd: Random.int = [stub]\n",
        "    caller() => rnd(BranchPath.root(), 0, 0, 10)\n",
    );
    assert_no_errors(src);
}

#[test]
fn given_oracle_ref_args_get_binds_capability_reader() {
    // Args.get is snapshot → capability reader `() -> List<String>`.
    // The stub returns a List<String>, matching.
    let src = concat!(
        "fn stub() -> List<String>\n",
        "    [\"one\"]\n",
        "fn caller() -> List<String>\n",
        "    ! [Args.get]\n",
        "    Args.get()\n",
        "verify caller law consistent\n",
        "    given args: Args.get = [stub]\n",
        "    caller() => args()\n",
    );
    assert_no_errors(src);
}

#[test]
fn given_oracle_ref_output_effect_is_rejected() {
    // Console.print is output-only — no oracle. `given` for it should
    // produce a clear rejection pointing at the trace API.
    let src = concat!(
        "fn stub() -> Unit\n",
        "    Unit\n",
        "fn caller() -> Unit\n",
        "    ! [Console.print]\n",
        "    Console.print(\"hi\")\n",
        "verify caller law consistent\n",
        "    given log: Console.print = [stub]\n",
        "    caller() => caller()\n",
    );
    assert_error_containing(src, "output-only");
}

#[test]
fn given_oracle_ref_random_int_wrong_stub_sig_is_rejected() {
    // Stub missing BranchPath prefix — should mismatch oracle signature.
    let src = concat!(
        "fn stub(min: Int, max: Int) -> Int\n",
        "    min\n",
        "fn caller() -> Int\n",
        "    ! [Random.int]\n",
        "    Random.int(0, 10)\n",
        "verify caller law consistent\n",
        "    given rnd: Random.int = [stub]\n",
        "    caller() => rnd(BranchPath.root(), 0, 0, 10)\n",
    );
    let errs = errors(src);
    assert!(!errs.is_empty(), "expected a signature error, got none");
}

// ---------------------------------------------------------------------------
// Oracle v1 — effectful-recursion rejection for trace-aware laws
// ---------------------------------------------------------------------------

#[test]
fn trace_law_on_recursive_effectful_function_is_rejected() {
    // Recursive self-call through `rollN` + uses Random.int — should be rejected
    // when a trace-aware law targets it.
    let src = concat!(
        "fn rollN(n: Int) -> Int\n",
        "    ! [Random.int]\n",
        "    match n\n",
        "        0 -> 0\n",
        "        _ -> Random.int(1, 6) + rollN(n - 1)\n",
        "verify rollN trace law rollNSpec\n",
        "    given rnd: Random.int = [stub]\n",
        "    rollN(0) => 0\n",
        "fn stub(path: BranchPath, k: Int, min: Int, max: Int) -> Int\n",
        "    min\n",
    );
    assert_error_containing(src, "recursive effectful function");
}

#[test]
fn result_only_law_on_recursive_effectful_function_is_accepted() {
    // Same recursive effectful function, but without the `trace` keyword →
    // must still type-check. Result-only laws for effectful recursion stay
    // fully supported.
    let src = concat!(
        "fn rollN(n: Int) -> Int\n",
        "    ! [Random.int]\n",
        "    match n\n",
        "        0 -> 0\n",
        "        _ -> Random.int(1, 6) + rollN(n - 1)\n",
        "verify rollN law rollNSpec\n",
        "    given rnd: Random.int = [stub]\n",
        "    rollN(0) => 0\n",
        "fn stub(path: BranchPath, k: Int, min: Int, max: Int) -> Int\n",
        "    min\n",
    );
    let errs = errors(src);
    assert!(
        !errs.iter().any(|e| e.contains("recursive effectful function")),
        "result-only law should not be rejected; got: {:?}",
        errs
    );
}

#[test]
fn trace_law_on_non_recursive_effectful_function_is_accepted() {
    // Trace-aware law on a non-recursive effectful function is fine.
    let src = concat!(
        "fn pick() -> Int\n",
        "    ! [Random.int]\n",
        "    Random.int(1, 6)\n",
        "verify pick trace law pickSpec\n",
        "    given rnd: Random.int = [stub]\n",
        "    pick() => stub(BranchPath.root(), 0, 1, 6)\n",
        "fn stub(path: BranchPath, k: Int, min: Int, max: Int) -> Int\n",
        "    min\n",
    );
    let errs = errors(src);
    assert!(
        !errs.iter().any(|e| e.contains("recursive effectful function")),
        "non-recursive effectful function should not trigger the rejection; got: {:?}",
        errs
    );
}

#[test]
fn trace_law_on_recursive_pure_function_is_accepted() {
    // Recursion is fine when effects aren't classified — pure recursion has
    // no caller_fn ambiguity because there are no emissions to scope.
    let src = concat!(
        "fn sumTo(n: Int) -> Int\n",
        "    match n\n",
        "        0 -> 0\n",
        "        _ -> n + sumTo(n - 1)\n",
        "verify sumTo trace\n",
        "    sumTo(3) => 6\n",
    );
    let errs = errors(src);
    assert!(
        !errs.iter().any(|e| e.contains("recursive effectful function")),
        "pure recursive function should not trigger the rejection; got: {:?}",
        errs
    );
}

// ---------------------------------------------------------------------------
// Terminal service effect & signature checking
// ---------------------------------------------------------------------------

#[test]
#[cfg(feature = "terminal")]
fn error_terminal_clear_without_effect() {
    let src = concat!("fn wipe() -> Unit\n", "    Terminal.clear()\n",);
    assert_error_containing(src, "has effect 'Terminal.clear'");
}

#[test]
#[cfg(feature = "terminal")]
fn error_terminal_move_to_wrong_arg_count() {
    let src = concat!(
        "fn go(x: Int) -> Unit\n",
        "    ! [Terminal.moveTo]\n",
        "    Terminal.moveTo(x)\n",
    );
    assert_error_containing(src, "expects 2 argument(s)");
}

#[test]
#[cfg(feature = "terminal")]
fn terminal_read_key_returns_option_string() {
    let src = concat!(
        "fn poll() -> Option<String>\n",
        "    ! [Terminal.readKey]\n",
        "    Terminal.readKey()\n",
    );
    assert_no_errors(src);
}
