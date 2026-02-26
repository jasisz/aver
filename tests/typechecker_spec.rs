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

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn parse(src: &str) -> Vec<TopLevel> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser.parse().expect("parse failed")
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

// ---------------------------------------------------------------------------
// Valid programs — must pass with zero errors
// ---------------------------------------------------------------------------

#[test]
fn valid_int_function() {
    assert_no_errors("fn add(a: Int, b: Int) -> Int\n    = a + b\n");
}

#[test]
fn valid_string_function() {
    assert_no_errors("fn greet(name: String) -> String\n    = \"Hello\"\n");
}

#[test]
fn valid_bool_function() {
    assert_no_errors("fn negate(b: Bool) -> Bool\n    = b\n");
}

#[test]
fn valid_float_function() {
    assert_no_errors("fn scale(x: Float) -> Float\n    = x\n");
}

#[test]
fn valid_unit_function() {
    assert_no_errors("fn noop() -> Unit\n    ! [Console]\n    = Console.print(\"hi\")\n");
}

#[test]
fn valid_result_return() {
    assert_no_errors("fn safe_div(a: Int, b: Int) -> Result<Int, String>\n    = Result.Ok(a)\n");
}

#[test]
fn valid_option_return() {
    assert_no_errors("fn maybe(x: Int) -> Option<Int>\n    = Option.Some(x)\n");
}

#[test]
fn valid_list_return() {
    assert_no_errors("fn wrap(x: Int) -> List<Int>\n    = [x]\n");
}

#[test]
fn valid_list_get_preserves_inner_type() {
    let src = "fn first(xs: List<Int>) -> Result<Int, String>\n    = List.get(xs, 0)\n";
    assert_no_errors(src);
}

#[test]
fn valid_list_pattern_matching() {
    let src = "fn score(xs: List<Int>) -> Int\n    = match xs:\n        [] -> 0\n        [h, ..t] -> h + List.len(t)\n";
    assert_no_errors(src);
}

#[test]
fn valid_call_correct_args() {
    let src = "fn add(a: Int, b: Int) -> Int\n    = a + b\nfn main() -> Unit\n    r = add(1, 2)\n";
    assert_no_errors(src);
}

#[test]
fn valid_call_chain() {
    let src = "fn double(x: Int) -> Int\n    = x + x\nfn quadruple(x: Int) -> Int\n    = double(double(x))\n";
    assert_no_errors(src);
}

#[test]
fn valid_higher_order_function_param_call() {
    let src = "fn applyTwice(f: Fn(Int) -> Int, x: Int) -> Int\n    = f(f(x))\nfn inc(n: Int) -> Int\n    = n + 1\nfn main() -> Unit\n    r = applyTwice(inc, 10)\n";
    assert_no_errors(src);
}

#[test]
fn valid_pure_callback_for_effectful_slot() {
    let src = "fn applyOnce(f: Fn(Int) -> Int ! [Console], x: Int) -> Int\n    ! [Console]\n    = f(x)\nfn pureInc(n: Int) -> Int\n    = n + 1\nfn main() -> Unit\n    ! [Console]\n    r = applyOnce(pureInc, 10)\n";
    assert_no_errors(src);
}

#[test]
fn valid_simple_binding_in_fn() {
    assert_no_errors("fn f() -> Int\n    x = 5\n    x\n");
}

#[test]
fn valid_int_float_widening() {
    // Int is compatible with Float (widening)
    assert_no_errors("fn f(a: Int, b: Float) -> Float\n    = a + b\n");
}

#[test]
fn valid_pipe_operator() {
    let src = "fn double(x: Int) -> Int\n    = x + x\nfn main() -> Unit\n    r = 5 |> double\n";
    assert_no_errors(src);
}

// --- Real example files ---

#[test]
fn valid_hello_av() {
    let src = std::fs::read_to_string("examples/hello.av").expect("examples/hello.av not found");
    assert_no_errors(&src);
}

#[test]
fn valid_calculator_av() {
    let src = std::fs::read_to_string("examples/calculator.av")
        .expect("examples/calculator.av not found");
    assert_no_errors(&src);
}

#[test]
fn valid_shapes_av() {
    let src = std::fs::read_to_string("examples/shapes.av").expect("examples/shapes.av not found");
    assert_no_errors(&src);
}

#[test]
fn valid_lists_av() {
    let src = std::fs::read_to_string("examples/lists.av").expect("examples/lists.av not found");
    assert_no_errors(&src);
}

#[test]
fn valid_app_dot_av() {
    let src =
        std::fs::read_to_string("examples/app_dot.av").expect("examples/app_dot.av not found");
    let items = parse(&src);
    let errs = run_type_check_with_base(&items, Some("."))
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
    let src = std::fs::read_to_string("examples/app.av").expect("examples/app.av not found");
    let items = parse(&src);
    let errs = run_type_check_with_base(&items, Some("."))
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
    let errs = run_type_check_with_base(&items, Some("."))
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
    let src = "module App\n    depends [Secret]\n    intent:\n        \"Uses exported function\"\nfn main() -> Unit\n    x = Secret.pub()\n";
    let errs = errors_with_base(src, "examples");
    assert!(
        errs.is_empty(),
        "expected no type errors, got:\n  {}",
        errs.join("\n  ")
    );
}

// ---------------------------------------------------------------------------
// Type errors — must produce at least one error
// ---------------------------------------------------------------------------

#[test]
fn error_wrong_arg_count_too_few() {
    let src = "fn add(a: Int, b: Int) -> Int\n    = a + b\nfn main() -> Unit\n    r = add(1)\n";
    // actual: "Function 'add' expects 2 argument(s), got 1"
    assert_error_containing(src, "argument(s)");
}

#[test]
fn error_wrong_arg_count_too_many() {
    let src =
        "fn add(a: Int, b: Int) -> Int\n    = a + b\nfn main() -> Unit\n    r = add(1, 2, 3)\n";
    // actual: "Function 'add' expects 2 argument(s), got 3"
    assert_error_containing(src, "argument(s)");
}

#[test]
fn error_zero_arg_constructor_called_like_function() {
    let src = "type Shape\n  Point\nfn bad() -> Shape\n    = Shape.Point()\n";
    assert_error_containing(src, "Cannot call value of type Shape");
}

#[test]
fn error_arg_type_mismatch_string_for_int() {
    let src =
        "fn add(a: Int, b: Int) -> Int\n    = a + b\nfn main() -> Unit\n    r = add(1, \"two\")\n";
    // actual: "Argument 2 of 'add': expected Int, got String"
    assert_error_containing(src, "got String");
}

#[test]
fn error_list_get_wrong_declared_inner_type() {
    let src = "fn first(xs: List<Int>) -> Result<String, String>\n    = List.get(xs, 0)\n";
    assert_error_containing(src, "body returns Result<Int, String>");
}

#[test]
fn error_unknown_does_not_satisfy_declared_return_type() {
    let src = concat!(
        "fn bad() -> Int\n",
        "    = match []:\n",
        "        [h, ..t] -> h\n",
        "        []       -> 0\n",
    );
    assert_error_containing(src, "body returns Unknown but declared return type is Int");
}

#[test]
fn error_unknown_does_not_satisfy_call_argument_type() {
    let src = concat!(
        "fn takesInt(x: Int) -> Int\n",
        "    = x + 1\n",
        "fn bad() -> Int\n",
        "    n = match []:\n",
        "        [h, ..t] -> h\n",
        "        []       -> 0\n",
        "    takesInt(n)\n",
    );
    assert_error_containing(src, "Argument 1 of 'takesInt': expected Int, got Unknown");
}

#[test]
fn error_unknown_does_not_satisfy_type_ascription() {
    let src = concat!(
        "fn bad() -> Int\n",
        "    n = match []:\n",
        "        [h, ..t] -> h\n",
        "        []       -> 0\n",
        "    n: Int\n",
    );
    assert_error_containing(src, "Type ascription mismatch");
}

#[test]
fn error_list_push_mismatched_element_type() {
    let src = "fn bad(xs: List<Int>) -> List<Int>\n    = List.push(xs, \"x\")\n";
    assert_error_containing(src, "Argument 2 of 'List.push': expected Int, got String");
}

#[test]
fn error_list_filter_predicate_must_return_bool() {
    let src = "fn bad(xs: List<Int>) -> List<Int>\n    = List.filter(xs, Int.toString)\n";
    assert_error_containing(src, "predicate must return Bool");
}

#[test]
fn error_list_fold_item_type_mismatch() {
    let src = "fn step(acc: Int, s: String) -> Int\n    = acc + String.length(s)\nfn bad(xs: List<Int>) -> Int\n    = List.fold(xs, 0, step)\n";
    assert_error_containing(src, "fold item param expects String, list has Int");
}

#[test]
fn error_effectful_callback_passed_to_pure_slot() {
    let src = "fn applyPure(f: Fn(Int) -> Int, x: Int) -> Int\n    = f(x)\nfn logInc(n: Int) -> Int\n    ! [Console]\n    Console.print(n)\n    n + 1\nfn main() -> Unit\n    ! [Console]\n    r = applyPure(logInc, 1)\n";
    assert_error_containing(src, "Fn(Int) -> Int ! [Console]");
}

#[test]
fn error_unknown_type_annotation() {
    // Capitalized typos are now parsed as Named types; the error surfaces as a type mismatch
    // (body returns Named("Intger") but declared return is Unit)
    let src = "fn f(x: Intger) -> Unit\n    = x\n";
    assert_error_containing(src, "Intger");
}

#[test]
fn error_unknown_return_type() {
    // Capitalized typos are now parsed as Named types; the error surfaces as a type mismatch
    // (body returns String but declared return is Named("Strng"))
    let src = "fn f() -> Strng\n    = \"hi\"\n";
    assert_error_containing(src, "Strng");
}

#[test]
fn error_duplicate_binding_in_fn() {
    let src = "fn f() -> Unit\n    x = 0\n    x = 1\n";
    assert_error_containing(src, "already defined");
}

#[test]
fn error_binop_int_plus_string() {
    let src = "fn f(a: Int, b: String) -> Int\n    = a + b\n";
    // actual: "Operator '+' requires Int/Float or String on both sides, got Int and String"
    assert_error_containing(src, "requires");
}

#[test]
fn error_undeclared_effect() {
    // Calling a function with an effect from a function without that effect declared
    let src =
        "fn log(msg: String) -> Unit\n    ! [Io]\n    = Console.print(msg)\nfn caller(x: String) -> Unit\n    = log(x)\n";
    assert_error_containing(src, "Io");
}

#[test]
fn error_main_undeclared_console_effect() {
    let src = "fn main() -> Unit\n    = Console.print(\"hi\")\n";
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
    let src = "fn main() -> Int\n    = 0\nverify main:\n    Console.print(\"x\") => Console.print(\"x\")\n";
    assert_error_containing(src, "<verify:main>");
    assert_error_containing(src, "Console");
}

#[test]
fn error_undeclared_effect_from_function_typed_callback() {
    let src = "fn applyOnce(f: Fn(Int) -> Int ! [Console], x: Int) -> Int\n    = f(x)\nfn pureInc(n: Int) -> Int\n    = n + 1\n";
    assert_error_containing(src, "has effect 'Console'");
}

#[test]
fn error_call_to_unexposed_module_member() {
    let src = "module App\n    depends [Secret]\n    intent:\n        \"Tries to use hidden member\"\nfn main() -> Unit\n    x = Secret.hidden()\n";
    let errs = errors_with_base(src, "examples");
    assert!(
        errs.iter().any(|e| e.contains("Secret.hidden")),
        "expected exposes error mentioning Secret.hidden, got:\n  {}",
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
    let src = "fn log(msg: String) -> Unit\n    ! [Console]\n    = Console.print(msg)\nfn caller(x: String) -> Unit\n    ! [Console]\n    = log(x)\n";
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// Error propagation operator (?)
// ---------------------------------------------------------------------------

#[test]
fn valid_error_prop_in_result_fn() {
    // ? on a Result<Int, String> inside a function returning Result<Int, String> — valid.
    let src = "fn safe(r: Result<Int, String>) -> Result<Int, String>\n    = Result.Ok(r?)\n";
    assert_no_errors(src);
}

#[test]
fn error_prop_in_non_result_fn() {
    // ? used inside a function that returns Int — type error.
    let src = "fn bad(r: Result<Int, String>) -> Int\n    = r?\n";
    assert_error_containing(src, "not Result");
}

#[test]
fn error_prop_on_non_result_type() {
    // ? applied to an Int — type error.
    let src = "fn bad(n: Int) -> Result<Int, String>\n    = n?\n";
    assert_error_containing(src, "can only be applied to Result");
}

#[test]
fn error_prop_incompatible_err_types() {
    // Inner Err is String, outer function expects Err = Int — incompatible.
    let src =
        "fn inner(x: Int) -> Result<Int, String>\n    = Result.Ok(x)\nfn outer(x: Int) -> Result<Int, Int>\n    = Result.Ok(inner(x)?)\n";
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
    let src = "record User\n  name: String\n  age: Int\nu = User(name: \"Alice\", age: 30)\n";
    assert_no_errors(src);
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
        "  = match s:\n",
        "    Circle(r) -> r * r\n",
        "    Point -> 0.0\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_effect_set_alias_satisfies_effect() {
    // Function declares ! [AppIO] where AppIO = [Console] — should pass
    let src = concat!(
        "effects AppIO = [Console]\n",
        "fn greet() -> Unit\n",
        "    ! [AppIO]\n",
        "    Console.print(\"hi\")\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_effect_set_multi_alias() {
    // AppIO covers both Console and Disk — caller with AppIO can call both
    let src = concat!(
        "effects AppIO = [Console]\n",
        "fn log(msg: String) -> Unit\n",
        "    ! [Console]\n",
        "    Console.print(msg)\n",
        "fn process() -> Unit\n",
        "    ! [AppIO]\n",
        "    log(\"processing\")\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_effect_set_alias_insufficient() {
    // Function declares ! [Silent] where Silent = [] — calling print should fail
    let src = concat!(
        "effects Silent = []\n",
        "fn greet() -> Unit\n",
        "    ! [Silent]\n",
        "    Console.print(\"hi\")\n",
    );
    assert_error_containing(src, "has effect 'Console'");
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
    assert_error_containing(src, "has effect 'Http'");
}

#[test]
fn error_network_post_without_effect() {
    let src = concat!(
        "fn send(url: String, body: String) -> Result<HttpResponse, String>\n",
        "    Http.post(url, body, \"application/json\", [])\n",
    );
    assert_error_containing(src, "has effect 'Http'");
}

#[test]
fn valid_network_get_with_effect() {
    let src = concat!(
        "fn fetch(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http]\n",
        "    Http.get(url)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_network_post_with_effect() {
    let src = concat!(
        "fn send(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http]\n",
        "    Http.post(url, \"{}\", \"application/json\", [])\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_network_post_with_ascribed_empty_headers() {
    let src = concat!(
        "fn send(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http]\n",
        "    Http.post(url, \"{}\", \"application/json\", []: List<Header>)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_network_post_with_typed_headers() {
    let src = concat!(
        "fn send(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http]\n",
        "    headers = [Header(name: \"Authorization\", value: \"Bearer token\")]\n",
        "    Http.post(url, \"{}\", \"application/json\", headers)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_network_post_headers_wrong_type() {
    let src = concat!(
        "fn send(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http]\n",
        "    Http.post(url, \"{}\", \"application/json\", [\"bad\"])\n",
    );
    assert_error_containing(src, "Argument 4 of 'Http.post': expected List<Header>");
}

#[test]
fn error_type_ascription_mismatch() {
    let src = concat!("fn bad() -> Int\n", "    = \"x\": Int\n",);
    assert_error_containing(src, "Type ascription mismatch");
}

#[test]
fn valid_network_all_methods_with_effect() {
    let src = concat!(
        "fn callAll(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http]\n",
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
        "    = Disk.readText(\"config.av\")\n",
    );
    assert_error_containing(src, "has effect 'Disk'");
}

#[test]
fn error_disk_write_without_effect() {
    let src = concat!(
        "fn save() -> Result<Unit, String>\n",
        "    = Disk.writeText(\"out.txt\", \"data\")\n",
    );
    assert_error_containing(src, "has effect 'Disk'");
}

#[test]
fn valid_disk_read_with_effect() {
    let src = concat!(
        "fn loadCfg() -> Result<String, String>\n",
        "    ! [Disk]\n",
        "    = Disk.readText(\"config.av\")\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_disk_all_methods_with_effect() {
    let src = concat!(
        "fn ops(p: String) -> Result<String, String>\n",
        "    ! [Disk]\n",
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
        "    = Console.error(msg)\n",
    );
    assert_error_containing(src, "has effect 'Console'");
}

#[test]
fn error_console_warn_without_effect() {
    let src = concat!(
        "fn report(msg: String) -> Unit\n",
        "    = Console.warn(msg)\n",
    );
    assert_error_containing(src, "has effect 'Console'");
}

#[test]
fn error_console_read_line_without_effect() {
    let src = concat!(
        "fn ask() -> Result<String, String>\n",
        "    = Console.readLine()\n",
    );
    assert_error_containing(src, "has effect 'Console'");
}

#[test]
fn valid_console_all_methods_with_effect() {
    let src = concat!(
        "fn run(msg: String) -> Result<String, String>\n",
        "    ! [Console]\n",
        "    Console.print(msg)\n",
        "    Console.error(msg)\n",
        "    Console.warn(msg)\n",
        "    Console.readLine()\n",
    );
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// Record field access type checking
// ---------------------------------------------------------------------------

#[test]
fn valid_network_response_field_access() {
    // resp.status is Int — comparison with Int should pass
    let src = concat!(
        "fn isOk(resp: HttpResponse) -> Bool\n",
        "    = resp.status < 400\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_network_response_body_field() {
    let src = concat!(
        "fn body(resp: HttpResponse) -> String\n",
        "    = resp.body\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_header_field_access() {
    let src = concat!(
        "record Header\n",
        "    name: String\n",
        "    value: String\n",
        "fn headerName(h: Header) -> String\n",
        "    = h.name\n",
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
        "    = u.name\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_network_response_unknown_field() {
    let src = concat!(
        "fn bad(resp: HttpResponse) -> String\n",
        "    = resp.fooo\n",
    );
    assert_error_containing(src, "has no field 'fooo'");
}

#[test]
fn error_user_record_unknown_field() {
    let src = concat!(
        "record User\n",
        "    name: String\n",
        "fn bad(u: User) -> String\n",
        "    = u.email\n",
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
        "    ! [Tcp]\n",
        "    = Tcp.send(host, port, msg)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_ping_with_effect() {
    let src = concat!(
        "fn check(host: String, port: Int) -> Result<Unit, String>\n",
        "    ! [Tcp]\n",
        "    = Tcp.ping(host, port)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_tcp_send_without_effect() {
    let src = concat!(
        "fn talk(host: String, port: Int, msg: String) -> Result<String, String>\n",
        "    = Tcp.send(host, port, msg)\n",
    );
    assert_error_containing(src, "has effect 'Tcp'");
}

#[test]
fn error_tcp_ping_without_effect() {
    let src = concat!(
        "fn check(host: String, port: Int) -> Result<Unit, String>\n",
        "    = Tcp.ping(host, port)\n",
    );
    assert_error_containing(src, "has effect 'Tcp'");
}

#[test]
fn valid_http_server_listen_with_context() {
    let src = concat!(
        "fn handle(ctx: String, req: HttpRequest) -> HttpResponse\n",
        "    = HttpResponse(status: 200, body: ctx, headers: [])\n",
        "fn main() -> Unit\n",
        "    ! [HttpServer]\n",
        "    HttpServer.listenWith(8080, \"ok\", handle)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_connect_returns_connection() {
    let src = concat!(
        "fn open(host: String, port: Int) -> Result<Tcp.Connection, String>\n",
        "    ! [Tcp]\n",
        "    = Tcp.connect(host, port)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_write_line_with_connection() {
    let src = concat!(
        "fn send(conn: Tcp.Connection, msg: String) -> Result<Unit, String>\n",
        "    ! [Tcp]\n",
        "    = Tcp.writeLine(conn, msg)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_read_line_with_connection() {
    let src = concat!(
        "fn recv(conn: Tcp.Connection) -> Result<String, String>\n",
        "    ! [Tcp]\n",
        "    = Tcp.readLine(conn)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_close_with_connection() {
    let src = concat!(
        "fn done(conn: Tcp.Connection) -> Result<Unit, String>\n",
        "    ! [Tcp]\n",
        "    = Tcp.close(conn)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_connection_field_access() {
    let src = concat!(
        "fn getId(conn: Tcp.Connection) -> String\n",
        "    = conn.id\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_tcp_write_line_with_string() {
    let src = concat!(
        "fn send(conn: String, msg: String) -> Result<Unit, String>\n",
        "    ! [Tcp]\n",
        "    = Tcp.writeLine(conn, msg)\n",
    );
    assert_error_containing(src, "expected Tcp.Connection");
}

// ---------------------------------------------------------------------------
// Int / Float / String namespace type checking
// ---------------------------------------------------------------------------

#[test]
fn valid_int_to_string() {
    assert_no_errors("fn f(n: Int) -> String\n    = Int.toString(n)\n");
}

#[test]
fn valid_int_from_string() {
    assert_no_errors("fn f(s: String) -> Result<Int, String>\n    = Int.fromString(s)\n");
}

#[test]
fn valid_int_abs() {
    assert_no_errors("fn f(n: Int) -> Int\n    = Int.abs(n)\n");
}

#[test]
fn valid_int_min_max() {
    assert_no_errors("fn f(a: Int, b: Int) -> Int\n    = Int.min(a, Int.max(a, b))\n");
}

#[test]
fn valid_int_mod() {
    assert_no_errors("fn f(a: Int, b: Int) -> Result<Int, String>\n    = Int.mod(a, b)\n");
}

#[test]
fn valid_int_to_float() {
    assert_no_errors("fn f(n: Int) -> Float\n    = Int.toFloat(n)\n");
}

#[test]
fn valid_float_abs() {
    assert_no_errors("fn f(x: Float) -> Float\n    = Float.abs(x)\n");
}

#[test]
fn valid_float_floor_ceil_round() {
    assert_no_errors("fn f(x: Float) -> Int\n    = Float.floor(x)\n");
    assert_no_errors("fn f(x: Float) -> Int\n    = Float.ceil(x)\n");
    assert_no_errors("fn f(x: Float) -> Int\n    = Float.round(x)\n");
}

#[test]
fn valid_float_from_int() {
    assert_no_errors("fn f(n: Int) -> Float\n    = Float.fromInt(n)\n");
}

#[test]
fn valid_float_to_string() {
    assert_no_errors("fn f(x: Float) -> String\n    = Float.toString(x)\n");
}

#[test]
fn valid_string_length() {
    assert_no_errors("fn f(s: String) -> Int\n    = String.length(s)\n");
}

#[test]
fn valid_string_byte_length() {
    assert_no_errors("fn f(s: String) -> Int\n    = String.byteLength(s)\n");
}

#[test]
fn valid_string_starts_with() {
    assert_no_errors("fn f(s: String, p: String) -> Bool\n    = String.startsWith(s, p)\n");
}

#[test]
fn valid_string_contains() {
    assert_no_errors("fn f(s: String, sub: String) -> Bool\n    = String.contains(s, sub)\n");
}

#[test]
fn valid_string_slice() {
    assert_no_errors("fn f(s: String) -> String\n    = String.slice(s, 0, 3)\n");
}

#[test]
fn valid_string_trim() {
    assert_no_errors("fn f(s: String) -> String\n    = String.trim(s)\n");
}

#[test]
fn valid_string_split() {
    assert_no_errors("fn f(s: String) -> List<String>\n    = String.split(s, \",\")\n");
}

#[test]
fn valid_string_replace() {
    assert_no_errors("fn f(s: String) -> String\n    = String.replace(s, \"a\", \"b\")\n");
}

#[test]
fn valid_string_join() {
    assert_no_errors("fn f(xs: List<String>) -> String\n    = String.join(xs, \",\")\n");
}

#[test]
fn valid_string_chars() {
    assert_no_errors("fn f(s: String) -> List<String>\n    = String.chars(s)\n");
}

#[test]
fn valid_string_from_int() {
    assert_no_errors("fn f(n: Int) -> String\n    = String.fromInt(n)\n");
}

#[test]
fn valid_string_from_float() {
    assert_no_errors("fn f(x: Float) -> String\n    = String.fromFloat(x)\n");
}

#[test]
fn valid_string_from_bool() {
    assert_no_errors("fn f(b: Bool) -> String\n    = String.fromBool(b)\n");
}

#[test]
fn error_int_to_string_wrong_arg() {
    assert_error_containing(
        "fn f(s: String) -> String\n    = Int.toString(s)\n",
        "expected Int, got String",
    );
}

#[test]
fn error_float_abs_wrong_arg() {
    // String is incompatible with Float (unlike Int which widens)
    assert_error_containing(
        "fn f(s: String) -> Float\n    = Float.abs(s)\n",
        "expected Float, got String",
    );
}

#[test]
fn valid_no_effects_for_helpers() {
    // Int/Float/String namespace methods don't require effects
    assert_no_errors("fn f(n: Int) -> String\n    = Int.toString(n)\n");
    assert_no_errors("fn f(x: Float) -> Int\n    = Float.floor(x)\n");
    assert_no_errors("fn f(s: String) -> Int\n    = String.length(s)\n");
}

#[test]
fn error_duplicate_top_level_binding() {
    assert_error_containing("x = 1\nx = 2\n", "'x' is already defined");
}
