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
use aver::typechecker::run_type_check;

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
    assert_no_errors("fn noop() -> Unit\n    ! [Console]\n    = print(\"hi\")\n");
}

#[test]
fn valid_result_return() {
    assert_no_errors("fn safe_div(a: Int, b: Int) -> Result<Int, String>\n    = Ok(a)\n");
}

#[test]
fn valid_option_return() {
    assert_no_errors("fn maybe(x: Int) -> Option<Int>\n    = Some(x)\n");
}

#[test]
fn valid_list_return() {
    assert_no_errors("fn wrap(x: Int) -> List<Int>\n    = [x]\n");
}

#[test]
fn valid_list_pattern_matching() {
    let src = "fn score(xs: List<Int>) -> Int\n    = match xs:\n        [] -> 0\n        [h, ..t] -> h + len(t)\n";
    assert_no_errors(src);
}

#[test]
fn valid_explicit_any() {
    assert_no_errors("fn passthrough(x: Any) -> Any\n    = x\n");
}

#[test]
fn valid_call_correct_args() {
    let src =
        "fn add(a: Int, b: Int) -> Int\n    = a + b\nfn main() -> Unit\n    val r = add(1, 2)\n";
    assert_no_errors(src);
}

#[test]
fn valid_call_chain() {
    let src = "fn double(x: Int) -> Int\n    = x + x\nfn quadruple(x: Int) -> Int\n    = double(double(x))\n";
    assert_no_errors(src);
}

#[test]
fn valid_higher_order_function_param_call() {
    let src = "fn applyTwice(f: Fn(Int) -> Int, x: Int) -> Int\n    = f(f(x))\nfn inc(n: Int) -> Int\n    = n + 1\nfn main() -> Unit\n    val r = applyTwice(inc, 10)\n";
    assert_no_errors(src);
}

#[test]
fn valid_pure_callback_for_effectful_slot() {
    let src = "fn applyOnce(f: Fn(Int) -> Int ! [Console], x: Int) -> Int\n    ! [Console]\n    = f(x)\nfn pureInc(n: Int) -> Int\n    = n + 1\nfn main() -> Unit\n    ! [Console]\n    val r = applyOnce(pureInc, 10)\n";
    assert_no_errors(src);
}

#[test]
fn valid_var_reassignment() {
    assert_no_errors("fn f() -> Unit\n    var x = 0\n    x = 5\n");
}

#[test]
fn valid_var_reassignment_same_type() {
    assert_no_errors("fn f() -> Unit\n    var name = \"alice\"\n    name = \"bob\"\n");
}

#[test]
fn valid_int_float_widening() {
    // Int is compatible with Float (widening)
    assert_no_errors("fn f(a: Int, b: Float) -> Float\n    = a + b\n");
}

#[test]
fn valid_pipe_operator() {
    let src = "fn double(x: Int) -> Int\n    = x + x\nfn main() -> Unit\n    val r = 5 |> double\n";
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
fn valid_lists_av() {
    let src = std::fs::read_to_string("examples/lists.av").expect("examples/lists.av not found");
    assert_no_errors(&src);
}

// ---------------------------------------------------------------------------
// Type errors — must produce at least one error
// ---------------------------------------------------------------------------

#[test]
fn error_wrong_arg_count_too_few() {
    let src = "fn add(a: Int, b: Int) -> Int\n    = a + b\nfn main() -> Unit\n    val r = add(1)\n";
    // actual: "Function 'add' expects 2 argument(s), got 1"
    assert_error_containing(src, "argument(s)");
}

#[test]
fn error_wrong_arg_count_too_many() {
    let src =
        "fn add(a: Int, b: Int) -> Int\n    = a + b\nfn main() -> Unit\n    val r = add(1, 2, 3)\n";
    // actual: "Function 'add' expects 2 argument(s), got 3"
    assert_error_containing(src, "argument(s)");
}

#[test]
fn error_arg_type_mismatch_string_for_int() {
    let src = "fn add(a: Int, b: Int) -> Int\n    = a + b\nfn main() -> Unit\n    val r = add(1, \"two\")\n";
    // actual: "Argument 2 of 'add': expected Int, got String"
    assert_error_containing(src, "got String");
}

#[test]
fn error_effectful_callback_passed_to_pure_slot() {
    let src = "fn applyPure(f: Fn(Int) -> Int, x: Int) -> Int\n    = f(x)\nfn logInc(n: Int) -> Int\n    ! [Console]\n    print(n)\n    n + 1\nfn main() -> Unit\n    ! [Console]\n    val r = applyPure(logInc, 1)\n";
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
fn error_assign_to_val_is_immutable() {
    let src = "fn f() -> Unit\n    val x = 0\n    x = 1\n";
    // actual: "Assignment to immutable 'x' in 'f' ..."
    assert_error_containing(src, "immutable");
}

#[test]
fn error_assign_to_undeclared() {
    let src = "fn f() -> Unit\n    y = 1\n";
    // actual: "Assignment to undeclared variable 'y' in 'f'"
    assert_error_containing(src, "undeclared");
}

#[test]
fn error_assign_to_var_wrong_type() {
    let src = "fn f() -> Unit\n    var x = 0\n    x = \"hello\"\n";
    // actual: "Assignment to 'x' in 'f': expected Int, got String"
    assert_error_containing(src, "expected Int");
}

#[test]
fn error_binop_int_plus_string() {
    let src = "fn f(a: Int, b: String) -> Any\n    = a + b\n";
    // actual: "Operator '+' requires Int/Float or String on both sides, got Int and String"
    assert_error_containing(src, "requires");
}

#[test]
fn error_undeclared_effect() {
    // Calling a function with an effect from a function without that effect declared
    let src =
        "fn log(msg: String) -> Unit\n    ! [Io]\n    = print(msg)\nfn caller(x: String) -> Unit\n    = log(x)\n";
    assert_error_containing(src, "Io");
}

#[test]
fn error_undeclared_effect_from_function_typed_callback() {
    let src = "fn applyOnce(f: Fn(Int) -> Int ! [Console], x: Int) -> Int\n    = f(x)\nfn pureInc(n: Int) -> Int\n    = n + 1\n";
    assert_error_containing(src, "has effect 'Console'");
}

// ---------------------------------------------------------------------------
// Effect propagation
// ---------------------------------------------------------------------------

#[test]
fn valid_effect_propagated_correctly() {
    // caller declares the same effect as callee
    let src = "fn log(msg: String) -> Unit\n    ! [Console]\n    = print(msg)\nfn caller(x: String) -> Unit\n    ! [Console]\n    = log(x)\n";
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// Error propagation operator (?)
// ---------------------------------------------------------------------------

#[test]
fn valid_error_prop_in_result_fn() {
    // ? on a Result<Int, String> inside a function returning Result<Int, String> — valid.
    let src = "fn safe(r: Result<Int, String>) -> Result<Int, String>\n    = Ok(r?)\n";
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
        "fn inner(x: Int) -> Result<Int, String>\n    = Ok(x)\nfn outer(x: Int) -> Result<Int, Int>\n    = Ok(inner(x)?)\n";
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
    let src = "type Shape\n  Circle(Float)\n  Point\nval c = Shape.Circle(3.14)\n";
    assert_no_errors(src);
}

#[test]
fn valid_record_creation() {
    let src = "record User\n  name: String\n  age: Int\nval u = User(name: \"Alice\", age: 30)\n";
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
fn named_type_compatible_with_any() {
    use aver::types::Type;
    let named = Type::Named("Shape".to_string());
    assert!(named.compatible(&Type::Any));
    assert!(Type::Any.compatible(&named));
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
