/// Spec tests for the Aver static type checker.
///
/// Tests are grouped into:
///   - valid programs that must produce zero type errors
///   - invalid programs that must produce at least one error with a
///     specific substring in the message
///
/// The type checker is run directly via `run_type_check`, bypassing the CLI.
use aver::ast::TopLevel;
use aver::diagnostics::{AnalyzeOptions, analyze_source};
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
fn valid_result_from_option_infers_type() {
    let src = "fn convert(x: Option<Int>) -> Result<Int, String>\n    Result.fromOption(x, \"missing\")\n";
    assert_no_errors(src);
}

#[test]
fn valid_empty_list_default_through_with_default() {
    let src = "fn f(r: Result<List<Int>, String>) -> List<Int>\n    Result.withDefault(r, [])\n";
    assert_no_errors(src);
}

#[test]
fn valid_empty_map_default_through_with_default() {
    let src =
        "fn f(o: Option<Map<String, Int>>) -> Map<String, Int>\n    Option.withDefault(o, {})\n";
    assert_no_errors(src);
}

#[test]
fn valid_empty_error_value_through_result_from_option() {
    let src = "fn f(o: Option<Int>) -> Result<Int, List<String>>\n    Result.fromOption(o, [])\n";
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
    let src = "fn pair() -> Tuple<Int, String>\n    (1, \"x\")\n";
    assert_no_errors(src);
}

#[test]
fn valid_map_set_get_infers_types() {
    let src = concat!(
        "fn readAge() -> Option<Int>\n",
        "    m = Map.set({}, \"x\", 0)\n",
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
        "    code = Option.withDefault(String.firstCodePoint(\"A\"), 0)\n",
        "    String.fromCodePoint(code)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_string_first_code_point_argument_type() {
    let src = "fn f() -> Option<Int>\n    String.firstCodePoint(1)\n";
    assert_error_containing(
        src,
        "Argument 1 of 'String.firstCodePoint': expected String, got Int",
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
fn valid_forwarded_callback_effect_is_required_only_at_call_site() {
    let src = "fn applyOnce(f: Fn(Int) -> Int ! [_], x: Int) -> Int\n    f(x)\nfn noisy(n: Int) -> Int\n    ! [Console.print]\n    Console.print(\"called\")\n    n + 1\nfn main() -> Int\n    ! [Console.print]\n    applyOnce(noisy, 10)\n";
    assert_no_errors(src);
}

#[test]
fn invalid_missing_forwarded_callback_effect_at_call_site() {
    assert_error_containing(
        "fn applyOnce(f: Fn(Int) -> Int ! [_], x: Int) -> Int\n    f(x)\nfn noisy(n: Int) -> Int\n    ! [Console.print]\n    Console.print(\"called\")\n    n + 1\nfn main() -> Int\n    applyOnce(noisy, 10)\n",
        "passes callback 'noisy' with effect 'Console.print'",
    );
}

#[test]
fn invalid_forwarded_callback_marker_mixed_with_concrete_effect() {
    assert_error_containing(
        "fn applyOnce(f: Fn(Int) -> Int ! [_, Console.print], x: Int) -> Int\n    ! [Console.print]\n    f(x)\n",
        "callback effect '_' must be the sole effect",
    );
}

#[test]
fn invalid_forwarded_callback_marker_as_ordinary_function_effect() {
    assert_error_containing(
        "fn dishonest() -> Int\n    ! [_]\n    1\n",
        "Effect '_' is allowed only as the sole effect of a direct callback parameter type",
    );
}

#[test]
fn invalid_forwarded_callback_marker_as_module_effect() {
    assert_error_containing(
        "module Main\n    effects [_]\n    exposes [main]\n    intent = \"Reject an ambient callback marker.\"\n\nfn main() -> Int\n    1\n",
        "Effect '_' is allowed only as the sole effect of a direct callback parameter type",
    );
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
fn k5_fdiv_corpus_passes_check() {
    const K5_FILES: &[&str] = &[
        "projects/k5_fdiv/main.av",
        "projects/k5_fdiv/domain/estimate.av",
        "projects/k5_fdiv/domain/exponent.av",
        "projects/k5_fdiv/domain/floorlaws.av",
        "projects/k5_fdiv/domain/fprep.av",
        "projects/k5_fdiv/domain/fracround.av",
        "projects/k5_fdiv/domain/kernel.av",
        "projects/k5_fdiv/domain/rational.av",
        "projects/k5_fdiv/domain/recip.av",
        "projects/k5_fdiv/domain/remainder.av",
        "projects/k5_fdiv/domain/round.av",
        "projects/k5_fdiv/domain/sticky_int.av",
        "projects/k5_fdiv/domain/table.av",
    ];

    let mut errors = Vec::new();
    for path in K5_FILES {
        let source = std::fs::read_to_string(path)
            .unwrap_or_else(|error| panic!("failed to read {path}: {error}"));
        let options = AnalyzeOptions::new(*path).with_module_base_dir("projects/k5_fdiv");
        let report = analyze_source(&source, &options);
        errors.extend(
            report
                .diagnostics
                .into_iter()
                .filter(|diagnostic| diagnostic.is_error())
                .map(|diagnostic| {
                    format!(
                        "{}:{}: error[{}]: {}",
                        path, diagnostic.span.line, diagnostic.slug, diagnostic.summary
                    )
                }),
        );
    }

    assert!(
        errors.is_empty(),
        "K5 division corpus failed `aver check`:\n{}",
        errors.join("\n")
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
fn error_unsolved_type_var_does_not_satisfy_declared_return_type() {
    let src = concat!(
        "fn bad() -> Int\n",
        "    match []\n",
        "        [h, ..t] -> h\n",
        "        []       -> 0\n",
    );
    assert_error_containing(src, "body returns T but declared return type is Int");
}

#[test]
fn error_unsolved_type_var_does_not_satisfy_call_argument_type() {
    let src = concat!(
        "fn takesInt(x: Int) -> Int\n",
        "    x + 1\n",
        "fn bad() -> Int\n",
        "    n = match []\n",
        "        [h, ..t] -> h\n",
        "        []       -> 0\n",
        "    takesInt(n)\n",
    );
    assert_error_containing(src, "Argument 1 of 'takesInt': expected Int, got T");
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
fn map_key_type_accepts_user_defined_via_deep_hash() {
    // The Map runtime hashes any heap structure by value (rt_deep_hash),
    // so List<Int> (and other user-defined types) is a valid map key.
    let src = concat!(
        "fn ok() -> Map<List<Int>, Int>\n",
        "    Map.fromList([([1], 2)])\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_map_from_list_requires_tuple_pairs() {
    let src = concat!(
        "fn bad() -> Map<String, Int>\n",
        "    Map.fromList([[\"a\", 1]])\n",
    );
    assert_error_containing(
        src,
        "List element 1: expected Tuple<String, Int>, got List<String>",
    );
}

#[test]
fn map_literal_accepts_user_defined_keys() {
    let src = concat!("fn ok() -> Map<List<Int>, Int>\n", "    {[1] => 2}\n",);
    assert_no_errors(src);
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
fn error_prop_accepts_unconstrained_ok_constructor_err_type() {
    let src = "fn safe() -> Result<Int, String>\n    x = Result.Ok(5)?\n    Result.Ok(x)\n";
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
    let a = Type::named("Shape");
    let b = Type::named("Shape");
    assert!(a.compatible(&b));
}

#[test]
fn named_types_are_incompatible_with_different_names() {
    use aver::types::Type;
    let a = Type::named("Shape");
    let b = Type::named("User");
    assert!(!a.compatible(&b));
}

#[test]
fn named_type_is_compatible_with_invalid_recovery() {
    // Iron — A4: `Type::Invalid` is the "already-errored" sentinel;
    // it matches anything so a single source error doesn't fan out
    // into a cascade of `expected X, got Invalid` diagnostics. Pre-A4
    // this test asserted the opposite direction.
    use aver::types::Type;
    let named = Type::named("Shape");
    assert!(named.compatible(&Type::Invalid));
    assert!(Type::Invalid.compatible(&named));
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
    assert_parse_error_containing(src, "module-level declaration");
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
    assert_parse_error_containing(src, "module-level declaration");
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
        "    Http.post(url, \"{}\", \"application/json\", {})\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_network_post_with_typed_headers() {
    let src = concat!(
        "fn send(url: String) -> Result<HttpResponse, String>\n",
        "    ! [Http.post]\n",
        "    headers = {\"authorization\" => [\"Bearer token\"]}\n",
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
    assert_error_containing(
        src,
        "Argument 4 of 'Http.post': expected Map<String, List<String>>",
    );
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
// Disk capability effect checking
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
fn valid_time_sleep_negative_constant_is_catchable() {
    let src = concat!(
        "fn wait() -> Result<Unit, String>\n",
        "    ! [Time.sleep]\n",
        "    Time.sleep(0 - 1)\n",
    );
    assert_no_errors(src);
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
    let src = concat!(
        "fn write() -> Result<Unit, String>\n",
        "    Env.set(\"A\", \"1\")\n",
    );
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
fn valid_tcp_write_bytes_requires_nominal_bytes() {
    let src = concat!(
        "module M\n",
        "    intent = \"Use the standard nominal Bytes type.\"\n",
        "    depends [Bytes]\n",
        "    effects [Tcp.writeBytes]\n",
        "fn send(conn: Tcp.Connection, payload: Bytes) -> Result<Unit, String>\n",
        "    ! [Tcp.writeBytes]\n",
        "    Tcp.writeBytes(conn, payload)\n",
    );
    let errs = errors_with_base(src, env!("CARGO_MANIFEST_DIR"));
    assert!(errs.is_empty(), "expected no errors, got: {errs:?}");
}

#[test]
fn error_tcp_write_bytes_rejects_raw_list() {
    let src = concat!(
        "module M\n",
        "    intent = \"Reject raw lists at the TCP boundary.\"\n",
        "    depends [Bytes]\n",
        "    effects [Tcp.writeBytes]\n",
        "fn send(conn: Tcp.Connection) -> Result<Unit, String>\n",
        "    ! [Tcp.writeBytes]\n",
        "    Tcp.writeBytes(conn, [1, 2, 3])\n",
    );
    let errs = errors_with_base(src, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.iter().any(|error| error.contains("expected Bytes")),
        "expected nominal Bytes error, got: {errs:?}"
    );
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
fn valid_tcp_read_bytes_returns_nominal_bytes() {
    let src = concat!(
        "module M\n",
        "    depends [Bytes]\n",
        "    effects [Tcp.readBytes]\n",
        "fn recv(conn: Tcp.Connection, count: Int) -> Result<Bytes, String>\n",
        "    ! [Tcp.readBytes]\n",
        "    Tcp.readBytes(conn, count)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_poll_returns_caller_socket_ids() {
    let src = concat!(
        "fn ready(sockets: Map<Int, Tcp.Socket>, timeoutMs: Int) -> Result<List<Int>, String>\n",
        "    ! [Tcp.poll]\n",
        "    Tcp.poll(sockets, timeoutMs)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_tcp_socket_sum_dispatches_to_resource_specific_operations() {
    let src = concat!(
        "fn closeSocket(socket: Tcp.Socket) -> Result<Unit, String>\n",
        "    ! [Tcp.close, Tcp.closeDial, Tcp.closeListener]\n",
        "    match socket\n",
        "        Tcp.Socket.Listening(listener) -> Tcp.closeListener(listener)\n",
        "        Tcp.Socket.Dialing(dial) -> Tcp.closeDial(dial)\n",
        "        Tcp.Socket.Connected(connection) -> Tcp.close(connection)\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_tcp_dial_cannot_be_used_as_connected_socket() {
    let src = concat!(
        "fn sendBeforeConnected(dial: Tcp.Dial) -> Result<Unit, String>\n",
        "    ! [Tcp.writeLine]\n",
        "    Tcp.writeLine(dial, \"too early\")\n",
    );
    assert_error_containing(src, "expected Tcp.Connection");
}

#[test]
fn valid_tcp_read_some_returns_nominal_bytes() {
    let src = concat!(
        "module M\n",
        "    depends [Bytes]\n",
        "    effects [Tcp.readSome]\n",
        "fn recv(conn: Tcp.Connection, maxBytes: Int) -> Result<Bytes, String>\n",
        "    ! [Tcp.readSome]\n",
        "    Tcp.readSome(conn, maxBytes)\n",
    );
    assert_no_errors(src);
}

#[test]
fn valid_verify_trace_given_stub_for_tcp_read_bytes() {
    // Regression: the oracle signature for the byte-carrying TCP methods
    // is built from bare source names (`Type::Named { id: None, name:
    // "Bytes" }`), while a user-written stub fn gets its signature
    // canonicalized against the loaded stdlib `Bytes` module (`id:
    // Some(..)`). Pre-fix the stub-type check rejected the given with an
    // error printing two identical-looking signatures.
    let src = concat!(
        "module Prog\n",
        "    intent = \"User stub for Tcp.readBytes must typecheck.\"\n",
        "    depends [Bytes]\n",
        "    effects [Tcp]\n",
        "\n",
        "fn connectStub(path: BranchPath, n: Int, fresh: Tcp.Connection, host: String, port: Int) -> Result<Tcp.Connection, String>\n",
        "    ? \"Mint the provider-owned test connection.\"\n",
        "    Result.Ok(fresh)\n",
        "\n",
        "fn readStub(path: BranchPath, n: Int, conn: Tcp.Connection, count: Int) -> Result<Bytes, String>\n",
        "    ? \"Honest stub returning a fixed frame.\"\n",
        "    Result.Ok(Bytes.fromList([1, 2, 3, 4]))\n",
        "\n",
        "fn readFrame() -> Result<Bytes, String>\n",
        "    ? \"Read one 4-byte frame.\"\n",
        "    ! [Tcp.connect, Tcp.readBytes]\n",
        "    conn = Tcp.connect(\"host\", 1)?\n",
        "    Tcp.readBytes(conn, 4)\n",
        "\n",
        "verify readFrame trace\n",
        "    given opener: Tcp.connect = [connectStub]\n",
        "    given reader: Tcp.readBytes = [readStub]\n",
        "    readFrame() => Result.Ok(Bytes.fromList([1, 2, 3, 4]))\n",
    );
    let errs = errors_with_base(src, env!("CARGO_MANIFEST_DIR"));
    assert!(errs.is_empty(), "expected no errors, got: {errs:?}");
}

#[test]
fn valid_verify_trace_given_stub_for_tcp_write_bytes() {
    // Same regression as above, exercising nominal `Bytes` in stub
    // parameter position (`Tcp.writeBytes` consumes the payload).
    let src = concat!(
        "module Prog\n",
        "    intent = \"User stub for Tcp.writeBytes must typecheck.\"\n",
        "    depends [Bytes]\n",
        "    effects [Tcp]\n",
        "\n",
        "fn connectStub(path: BranchPath, n: Int, fresh: Tcp.Connection, host: String, port: Int) -> Result<Tcp.Connection, String>\n",
        "    ? \"Mint the provider-owned test connection.\"\n",
        "    Result.Ok(fresh)\n",
        "\n",
        "fn writeStub(path: BranchPath, n: Int, conn: Tcp.Connection, payload: Bytes) -> Result<Unit, String>\n",
        "    ? \"Honest stub accepting any frame.\"\n",
        "    Result.Ok(Unit)\n",
        "\n",
        "fn sendFrame(payload: Bytes) -> Result<Unit, String>\n",
        "    ? \"Write one frame.\"\n",
        "    ! [Tcp.connect, Tcp.writeBytes]\n",
        "    conn = Tcp.connect(\"host\", 1)?\n",
        "    Tcp.writeBytes(conn, payload)\n",
        "\n",
        "verify sendFrame trace\n",
        "    given opener: Tcp.connect = [connectStub]\n",
        "    given writer: Tcp.writeBytes = [writeStub]\n",
        "    sendFrame(Bytes.fromList([1, 2])) => Result.Ok(Unit)\n",
    );
    let errs = errors_with_base(src, env!("CARGO_MANIFEST_DIR"));
    assert!(errs.is_empty(), "expected no errors, got: {errs:?}");
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
fn error_tcp_connection_resource_has_no_fields() {
    // Phase 4.7+ fix #11 — `Tcp.Connection` is a capability resource. Field reads
    // and destructuring are rejected; the record is a stateful
    // handle, not a value with public metadata. Programs pass it
    // back to `Tcp.{close, writeLine, readLine}` and never inspect
    // its bytes.
    let src = concat!(
        "fn getId(conn: Tcp.Connection) -> String\n",
        "    conn.id\n",
    );
    assert_error_containing(src, "capability resource 'Tcp.Connection'");
}

#[test]
fn error_tcp_connection_resource_cannot_be_constructed() {
    // Same resource check, construction path. A hand-crafted record
    // with a forged `id` string would otherwise alias an unrelated
    // live pool slot at runtime (`aver-rt::tcp` keys its HashMap by
    // id; wasip2 parses the digits as the slot index).
    let src = concat!(
        "fn fake() -> Tcp.Connection\n",
        "    Tcp.Connection(id = \"tcp-0\", host = \"x\", port = 80)\n",
    );
    assert_error_containing(src, "capability resource 'Tcp.Connection'");
}

#[test]
fn verify_trace_cannot_fabricate_tcp_connection_resource() {
    // Capability resources stay provider-owned even in verify traces. A
    // `Tcp.connect` oracle receives the fresh resource token it may return;
    // arbitrary record syntax must never forge one.
    let src = concat!(
        "fn fakeWrite(p: BranchPath, n: Int, c: Tcp.Connection, line: String) -> Result<Unit, String>\n",
        "    ? \"stub\"\n",
        "    Result.Ok(Unit)\n",
        "\n",
        "fn fakeRead(p: BranchPath, n: Int, c: Tcp.Connection) -> Result<String, String>\n",
        "    ? \"stub\"\n",
        "    Result.Ok(\"+PONG\")\n",
        "\n",
        "fn ping(conn: Tcp.Connection) -> Result<String, String>\n",
        "    ? \"ping\"\n",
        "    ! [Tcp.readLine, Tcp.writeLine]\n",
        "    _ = Tcp.writeLine(conn, \"PING\")?\n",
        "    Tcp.readLine(conn)\n",
        "\n",
        "verify ping trace\n",
        "    given conn: Tcp.Connection = [Tcp.Connection(id = \"fake\", host = \"127.0.0.1\", port = 6379)]\n",
        "    given w: Tcp.writeLine = [fakeWrite]\n",
        "    given r: Tcp.readLine  = [fakeRead]\n",
        "    pinged = ping(conn)\n",
        "    pinged.trace.contains(Tcp.writeLine) => true\n",
    );
    assert_error_containing(src, "Cannot construct capability resource 'Tcp.Connection'");
}

#[test]
fn error_tcp_connection_fabrication_still_rejected_outside_verify() {
    // Regression: relaxation is scoped to verify-trace context. A
    // regular fn that constructs Tcp.Connection is still rejected.
    // (The original "fake()" test above guards the same property
    // without the verify-context flag flipped; this test makes the
    // scoping explicit by also covering a fn that DOES participate
    // in oracle stub signatures.)
    let src = concat!(
        "fn forgeOutsideVerify() -> Tcp.Connection\n",
        "    Tcp.Connection(id = \"forged\", host = \"x\", port = 80)\n",
    );
    assert_error_containing(src, "capability resource 'Tcp.Connection'");
}

#[test]
fn error_user_defined_opaque_not_fabricable_in_verify_trace() {
    // The relaxation is opt-in via `is_verify_fabricable_handle`.
    // User-defined opaque types protect domain invariants (smart
    // constructors), so verify-trace context MUST NOT erode that
    // protection — the typechecker should reject construction of an
    // imported opaque even from inside a verify block.
    let root = temp_module_root("verify_opaque_no_fabricate");
    let bounded_dir = root.join("Bounded");
    std::fs::create_dir_all(&bounded_dir).expect("create Bounded dir failed");
    std::fs::write(
        bounded_dir.join("Positive.av"),
        r#"module Positive
    exposes [fromInt]
    exposes opaque [Positive]
    intent = "Positive integer with smart-constructor invariant."

record Positive
    value: Int

fn fromInt(n: Int) -> Result<Positive, String>
    ? "Smart constructor: rejects non-positive inputs."
    match n > 0
        true  -> Result.Ok(Positive(value = n))
        false -> Result.Err("non-positive")
"#,
    )
    .expect("write Positive.av failed");

    let src = r#"module App
    depends [Bounded.Positive]
    intent = "Tries to fabricate Positive inside a verify trace."

fn echo(p: Positive) -> Positive
    ? "Identity over Positive."
    p

verify echo trace
    echoed = echo(Positive(value = -1))
    echoed => Positive(value = -1)
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.iter()
            .any(|e| e.contains("Cannot construct opaque type")),
        "expected opaque construction error inside verify trace, got: {:?}",
        errs
    );

    let _ = std::fs::remove_dir_all(&root);
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
    assert_no_errors("fn f(n: Int) -> String\n    String.fromInt(n)\n");
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
fn valid_int_div() {
    assert_no_errors("fn f(a: Int, b: Int) -> Result<Int, String>\n    Int.div(a, b)\n");
}

#[test]
fn literal_divisor_discharge_types_as_int() {
    // Literal-divisor discharge: a syntactic nonzero integer literal divisor
    // makes `Int.div` / `Int.mod` total, so the call types as plain `Int`.
    assert_no_errors("fn f(a: Int) -> Int\n    Int.div(a, 2)\n");
    assert_no_errors("fn f(a: Int) -> Int\n    Int.mod(a, 16)\n");
    // Negative literals (one unary minus over a literal) discharge too.
    assert_no_errors("fn f(a: Int) -> Int\n    Int.div(a, -3)\n");
    assert_no_errors("fn f(a: Int) -> Int\n    Int.mod(a, -1)\n");
    // A BigInt literal (magnitude beyond i64) is nonzero by construction.
    assert_no_errors("fn f(a: Int) -> Int\n    Int.div(a, 99999999999999999999)\n");
    // The discharged value is an Int, so Result consumers reject it.
    assert_error_containing(
        "fn f(a: Int) -> Int\n    Result.withDefault(Int.div(a, 2), 0)\n",
        "Argument 1 of 'Result.withDefault': expected Result<T, E>, got Int",
    );
}

#[test]
fn literal_divisor_discharge_boundary_zero_literal_stays_result() {
    // `0` is a literal but not a NONZERO one — the call keeps the
    // `Result<Int, String>` type (and errs at runtime).
    assert_no_errors("fn f(a: Int) -> Result<Int, String>\n    Int.div(a, 0)\n");
    assert_no_errors("fn f(a: Int) -> Result<Int, String>\n    Int.mod(a, 0)\n");
}

#[test]
fn literal_divisor_discharge_boundary_is_syntactic_literals_only() {
    // THE BOUNDARY: the discharge is keyed on a syntactic literal, nothing
    // wider. Widening it (constant expressions, named constants, flow
    // facts) is a deliberate design decision — if you are here to relax
    // this test, that decision needs its own review.
    // An identifier divisor stays `Result`, even when it is bound to a
    // literal in plain sight.
    assert_no_errors("fn f(a: Int) -> Result<Int, String>\n    k = 2\n    Int.div(a, k)\n");
    // A constant expression divisor stays `Result` (no folding).
    assert_no_errors("fn f(a: Int) -> Result<Int, String>\n    Int.div(a, 8 + 8)\n");
    // A doubly-negated literal is not a syntactic literal (single unary
    // minus only).
    assert_no_errors("fn f(a: Int) -> Result<Int, String>\n    Int.div(a, --5)\n");
    // Parenthesising an identifier does not turn it into a literal.
    assert_no_errors("fn f(a: Int) -> Result<Int, String>\n    k = 2\n    Int.div(a, (k))\n");
    // An interpolated string is not an integer literal. The discharge must
    // not fire on it — the call keeps the registered signature and the
    // normal argument check rejects the operand type, rather than the
    // divisor sneaking through a discharge that skips it.
    assert_error_containing(
        "fn f(a: Int) -> Int\n    Int.div(a, \"{2}\")\n",
        "Argument 2 of 'Int.div': expected Int, got String",
    );
    // And the discharged type does NOT satisfy a Result return.
    assert_error_containing(
        "fn f(a: Int) -> Result<Int, String>\n    Int.div(a, 2)\n",
        "body returns Int but declared return type is Result<Int, String>",
    );
}

#[test]
fn literal_divisor_discharge_parentheses_are_transparent() {
    // Parentheses around a single expression are ERASED by the parser, so
    // `(16)` is literally the same AST node as `16`. The discharge rule is
    // defined on the AST, so all three parenthesised spellings below are
    // syntactic literals and discharge to plain `Int`:
    //   `(16)`   — parenthesised literal
    //   `(-16)`  — parenthesised negative literal
    //   `-(16)`  — unary minus over a parenthesised literal
    // The last two both reduce to a single `Neg` over an `Int` literal,
    // which is exactly what the predicate accepts. This is consistent with
    // "syntactic literal" (same node), not a widening of the boundary —
    // there is no constant folding here, and `--5` two lines down in the
    // boundary test still stays on the `Result` path.
    assert_no_errors("fn f(a: Int) -> Int\n    Int.div(a, (16))\n");
    assert_no_errors("fn f(a: Int) -> Int\n    Int.div(a, (-16))\n");
    assert_no_errors("fn f(a: Int) -> Int\n    Int.div(a, -(16))\n");
    assert_no_errors("fn f(a: Int) -> Int\n    Int.mod(a, (16))\n");
    assert_no_errors("fn f(a: Int) -> Int\n    Int.mod(a, -(16))\n");
    // Nesting the parentheses changes nothing.
    assert_no_errors("fn f(a: Int) -> Int\n    Int.div(a, ((16)))\n");
    // Parentheses do not make a zero literal nonzero.
    assert_no_errors("fn f(a: Int) -> Result<Int, String>\n    Int.div(a, (0))\n");
    assert_no_errors("fn f(a: Int) -> Result<Int, String>\n    Int.mod(a, (0))\n");
}

#[test]
fn literal_vector_size_discharge_is_portable_and_syntactic() {
    assert_no_errors("fn f() -> Vector<Int>\n    Vector.new(3, 0)\n");
    assert_no_errors(&format!(
        "fn f() -> Vector<Int>\n    Vector.new({}, 0)\n",
        aver_rt::MAX_MATERIALIZED_VECTOR_ELEMENTS
    ));
}

#[test]
fn dynamic_vector_size_stays_result() {
    assert_no_errors("fn f(n: Int) -> Result<Vector<Int>, String>\n    Vector.new(n, 0)\n");
    assert_no_errors("fn f() -> Result<Vector<Int>, String>\n    Vector.new(0 - 1, 0)\n");
    assert_error_containing(
        "fn f(n: Int) -> Vector<Int>\n    Vector.new(n, 0)\n",
        "body returns Result<Vector<Int>, String> but declared return type is Vector<Int>",
    );
}

#[test]
fn valid_literal_random_range_discharges_but_dynamic_range_stays_result() {
    assert_no_errors("fn roll() -> Int\n    ! [Random.int]\n    Random.int(1, 6)\n");
    assert_no_errors(
        "fn roll(min: Int, max: Int) -> Result<Int, String>\n    ! [Random.int]\n    Random.int(min, max)\n",
    );
    assert_no_errors(
        "fn reversed() -> Result<Int, String>\n    ! [Random.int]\n    Random.int(6, 1)\n",
    );
    assert_no_errors(
        "fn tooWide() -> Result<Int, String>\n    ! [Random.int]\n    Random.int(1, 9223372036854775808)\n",
    );
    assert_error_containing(
        "fn roll() -> Result<Int, String>\n    ! [Random.int]\n    Random.int(1, 6)\n",
        "body returns Int but declared return type is Result<Int, String>",
    );
}

#[test]
fn valid_literal_sleep_discharges_but_dynamic_or_invalid_sleep_stays_result() {
    assert_no_errors("fn pause() -> Unit\n    ! [Time.sleep]\n    Time.sleep(1)\n");
    assert_no_errors("fn pause() -> Unit\n    ! [Time.sleep]\n    Time.sleep(0)\n");
    assert_no_errors(
        "fn pause(ms: Int) -> Result<Unit, String>\n    ! [Time.sleep]\n    Time.sleep(ms)\n",
    );
    assert_no_errors(
        "fn backwards() -> Result<Unit, String>\n    ! [Time.sleep]\n    Time.sleep(-1)\n",
    );
    assert_no_errors(
        "fn tooLong() -> Result<Unit, String>\n    ! [Time.sleep]\n    Time.sleep(9223372036854775808)\n",
    );
    assert_error_containing(
        "fn pause() -> Result<Unit, String>\n    ! [Time.sleep]\n    Time.sleep(1)\n",
        "body returns Unit but declared return type is Result<Unit, String>",
    );
}

#[test]
fn first_literal_above_the_vector_budget_stays_result() {
    assert_no_errors(&format!(
        "fn f() -> Result<Vector<Int>, String>\n    Vector.new({}, 0)\n",
        aver_rt::MAX_MATERIALIZED_VECTOR_ELEMENTS + 1
    ));
}

#[test]
fn valid_branch_path_literals_discharge_to_the_opaque_value() {
    assert_no_errors(
        "fn child() -> BranchPath\n    BranchPath.child(BranchPath.Root, 18446744073709551616)\n",
    );
    assert_no_errors("fn parsed() -> BranchPath\n    BranchPath.parse(\"2.0\")\n");
}

#[test]
fn dynamic_or_invalid_branch_path_inputs_stay_result() {
    assert_no_errors(
        "fn child(n: Int) -> Result<BranchPath, String>\n    BranchPath.child(BranchPath.Root, n)\n",
    );
    assert_no_errors(
        "fn parsed(s: String) -> Result<BranchPath, String>\n    BranchPath.parse(s)\n",
    );
    assert_no_errors(
        "fn negative() -> Result<BranchPath, String>\n    BranchPath.child(BranchPath.Root, -1)\n",
    );
    assert_no_errors(
        "fn invalid() -> Result<BranchPath, String>\n    BranchPath.parse(\"2..0\")\n",
    );
    assert_error_containing(
        "fn invalid() -> BranchPath\n    BranchPath.parse(\"2..0\")\n",
        "body returns Result<BranchPath, String> but declared return type is BranchPath",
    );
}

// ─── Literal smart-constructor discharge ────────────────────────────────
//
// `Bytes.fromList([<all int literals, each inside the interval the Bytes
// refinement itself proves>])` types as plain `Bytes`. Every other
// argument shape keeps `Result<Bytes, String>`. The tests below are named
// after the boundary they pin; the element bound is DERIVED from
// `stdlib/bytes.av`'s own `allInRange` predicate, never hardcoded, so a
// program with a different refinement discharges against a different
// range (see `src/analysis/literal_refinement.rs`).

/// Program skeleton for the discharge tests: one fn body, `depends [Bytes]`.
fn bytes_program(signature: &str, body: &str) -> String {
    format!(
        "module Prog\n    intent = \"literal Bytes discharge\"\n    depends [Bytes]\n    effects []\n\n{signature}\n    ? \"probe\"\n{body}\n"
    )
}

fn assert_bytes_program_clean(signature: &str, body: &str) {
    let src = bytes_program(signature, body);
    let errs = errors_with_base(&src, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.is_empty(),
        "expected no type errors for:\n{src}\ngot:\n  {}",
        errs.join("\n  ")
    );
}

fn assert_bytes_program_error(signature: &str, body: &str, snippet: &str) {
    let src = bytes_program(signature, body);
    let errs = errors_with_base(&src, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.iter().any(|e| e.contains(snippet)),
        "expected error containing {snippet:?} for:\n{src}\ngot:\n  {}",
        if errs.is_empty() {
            "<no errors>".to_string()
        } else {
            errs.join("\n  ")
        }
    );
}

#[test]
fn literal_bytes_discharge_types_as_the_refined_type() {
    assert_bytes_program_clean("fn f() -> Bytes", "    Bytes.fromList([0, 10, 255])");
    // The empty list satisfies the element bound vacuously.
    assert_bytes_program_clean("fn f() -> Bytes", "    Bytes.fromList([])");
    // Interval endpoints are inclusive.
    assert_bytes_program_clean("fn f() -> Bytes", "    Bytes.fromList([0])");
    assert_bytes_program_clean("fn f() -> Bytes", "    Bytes.fromList([255])");
    // The discharged value flows straight into a `Bytes` consumer.
    assert_bytes_program_clean(
        "fn f() -> String",
        "    Bytes.toHex(Bytes.fromList([0, 10, 255]))",
    );
    // …and no longer satisfies a `Result` return or the `?` operator.
    assert_bytes_program_error(
        "fn f() -> Result<Bytes, String>",
        "    Bytes.fromList([1, 2])",
        "body returns Bytes but declared return type is Result<Bytes, String>",
    );
    assert_bytes_program_error(
        "fn f() -> Bytes",
        "    Bytes.fromList([1, 2])?",
        "can only be applied to Result",
    );
}

#[test]
fn literal_bytes_discharge_boundary_out_of_interval_literal_stays_result() {
    // THE BOUNDARY, element side: a literal outside the interval the
    // refinement proves keeps the fallible signature, because the
    // constructor really can take its `Err` branch.
    assert_bytes_program_clean(
        "fn f() -> Result<Bytes, String>",
        "    Bytes.fromList([65, 256])",
    );
    assert_bytes_program_clean(
        "fn f() -> Result<Bytes, String>",
        "    Bytes.fromList([-1])",
    );
    // A magnitude beyond `i64` is declined outright by the syntactic half
    // of the predicate — no bignum comparison, fail-closed.
    assert_bytes_program_clean(
        "fn f() -> Result<Bytes, String>",
        "    Bytes.fromList([65, 1208925819614629174706176])",
    );
}

#[test]
fn literal_bytes_discharge_boundary_is_syntactic_literal_lists_only() {
    // THE BOUNDARY, argument-shape side: the discharge is keyed on a
    // syntactic list of syntactic literals, nothing wider. Widening it
    // (constant folding, flow facts, a proved-in-range variable) is a
    // deliberate design decision — if you are here to relax this test,
    // that decision needs its own review.
    //
    // An identifier stays `Result`, even when it is bound to a literal
    // list in plain sight.
    assert_bytes_program_clean(
        "fn f() -> Result<Bytes, String>",
        "    xs = [1, 2]\n    Bytes.fromList(xs)",
    );
    // A parameter stays `Result`.
    assert_bytes_program_clean(
        "fn f(xs: List<Int>) -> Result<Bytes, String>",
        "    Bytes.fromList(xs)",
    );
    // A computed list stays `Result` (no folding).
    assert_bytes_program_clean(
        "fn f() -> Result<Bytes, String>",
        "    Bytes.fromList(List.concat([1], [2]))",
    );
    // A computed ELEMENT stays `Result`, even though its value is in range.
    assert_bytes_program_clean(
        "fn f() -> Result<Bytes, String>",
        "    Bytes.fromList([1 + 1])",
    );
    // A doubly-negated literal is not a syntactic literal.
    assert_bytes_program_clean(
        "fn f() -> Result<Bytes, String>",
        "    Bytes.fromList([--5])",
    );
    // One in-range element does not carry an out-of-range sibling.
    assert_bytes_program_clean(
        "fn f() -> Result<Bytes, String>",
        "    Bytes.fromList([1, 2, 300])",
    );
    // A non-list argument does not discharge; the ordinary argument check
    // rejects it instead of a discharge skipping past it.
    assert_bytes_program_error(
        "fn f() -> Bytes",
        "    Bytes.fromList(\"0102\")",
        "Argument 1 of 'Bytes.fromList': expected List<Int>, got String",
    );
}

#[test]
fn literal_bytes_discharge_decides_the_same_for_every_callee_spelling() {
    // THE BOUNDARY, callee side: qualified and bare in-module spellings
    // must decide IDENTICALLY. This is not a convenience — the wasm-gc
    // backend flattens a dependency's constructor and all of its call
    // sites, qualified and in-module alike, into one prefixed bare name
    // before re-resolving, so after the flatten the two spellings are
    // indistinguishable. A spelling-sensitive rule would discharge before
    // the flatten and not after, forking the checked and unchecked
    // pipelines. Both fns below therefore return the refined type.
    let src = "module Local\n    intent = \"an entry-scope refinement\"\n    effects []\n\nrecord Octets\n    values: List<Int>\n\nfn allInRange(xs: List<Int>) -> Bool\n    ? \"probe\"\n    match xs\n        [] -> true\n        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)\n            true -> allInRange(tail)\n            false -> false\n\nfn fromList(xs: List<Int>) -> Result<Octets, String>\n    ? \"probe\"\n    match allInRange(xs)\n        true -> Result.Ok(Octets(values = xs))\n        false -> Result.Err(\"oob\")\n\nfn unqualified() -> Octets\n    ? \"probe\"\n    fromList([1, 2])\n\nfn qualified() -> Octets\n    ? \"probe\"\n    Local.fromList([1, 2])\n\nfn stillFallible(xs: List<Int>) -> Result<Octets, String>\n    ? \"probe\"\n    fromList(xs)\n";
    assert_no_errors(src);
}

#[test]
fn a_result_pattern_against_a_discharged_value_is_an_error() {
    // The migration must be LOUD. Before the discharge,
    // `match Bytes.fromList([1, 2])` scrutinised a `Result`; now it
    // scrutinises a `Bytes`, and the `Result.Ok` / `Result.Err` arms can
    // never be taken. Left unchecked the match just walks off the end at
    // runtime with no diagnostic, so the pattern checker rejects a
    // `Result` / `Option` constructor pattern whose subject is neither.
    assert_bytes_program_error(
        "fn f() -> String",
        "    match Bytes.fromList([1, 2])\n        Result.Ok(b) -> Bytes.toHex(b)\n        Result.Err(e) -> e",
        "Pattern 'Result.Ok' matches a Result value, but the match subject is Bytes",
    );
    // The same guard on a plainly wrong match, discharge or not.
    assert_error_containing(
        "fn f(n: Int) -> Int\n    match n\n        Result.Ok(v) -> v\n        Result.Err(_) -> 0\n",
        "but the match subject is Int",
    );
    // A genuine Result subject is untouched.
    assert_bytes_program_clean(
        "fn f(values: List<Int>) -> String",
        "    match Bytes.fromList(values)\n        Result.Ok(b) -> Bytes.toHex(b)\n        Result.Err(e) -> e",
    );
}

#[test]
fn literal_bytes_discharge_boundary_needs_a_recognized_smart_constructor() {
    // THE BOUNDARY, refinement side: the gate is derived from the
    // refinement SHAPE, not from a constructor name. A bare record with a
    // `fromList` of its own — no validating predicate, so nothing proves
    // an element interval — never discharges, and its declared
    // `Result` signature stands.
    let src = "module Local\n    intent = \"a fromList that validates nothing\"\n    effects []\n\nrecord Octets\n    values: List<Int>\n\nfn fromList(xs: List<Int>) -> Result<Octets, String>\n    ? \"probe\"\n    Result.Ok(Octets(values = xs))\n\nfn use() -> Result<Octets, String>\n    ? \"probe\"\n    Local.fromList([1, 2])\n";
    assert_no_errors(src);
}

// ─── The discharge is keyed on RESOLVED IDENTITY, not on the spelling ───
//
// Aver's shadowing rule is pinned above by
// `entry_local_fn_shadows_dep_module_bare_alias`: a bare call inside an
// entry module that declares its own `doit` means the entry's own `doit`,
// not the dependency's. The discharge must obey the SAME resolution — it
// may fire only when the callee the checker resolved IS the recognized
// smart constructor. Otherwise the checked type and the lowered IR
// disagree about which function ran, which is a miscompilation, not a
// missed optimisation. See `src/analysis/literal_refinement.rs`.

/// Entry program with `depends [Bytes]` that declares its own `fromList`
/// with the given signature and body, plus one caller fn.
fn shadowing_from_list_program(
    local_signature: &str,
    local_body: &str,
    caller_signature: &str,
    caller_body: &str,
) -> String {
    format!(
        "module Prog\n    intent = \"a local fromList shadowing the recognized constructor\"\n    depends [Bytes]\n    effects []\n\n{local_signature}\n    ? \"probe\"\n{local_body}\n\n{caller_signature}\n    ? \"probe\"\n{caller_body}\n"
    )
}

#[test]
fn a_local_from_list_returning_int_shadows_the_recognized_constructor() {
    // The local fn wins, so the call has type `Int`. A discharge keyed on
    // the spelling `fromList` fired here too — the checker kept `Int` only
    // because `Int` is not a `Result` to unwrap, while the HIR resolver
    // went ahead and rewrote the body to a `Bytes` carrier construction.
    // The runtime half of this pin lives in `tests/cross_backend_stress.rs`
    // (`cross_shadowed_smart_constructor_runs_the_local_fn_*`), which is
    // where that divergence was actually observable.
    let clean = shadowing_from_list_program(
        "fn fromList(xs: List<Int>) -> Int",
        "    List.len(xs)",
        "fn caller() -> Int",
        "    fromList([0, 10, 255])",
    );
    let errs = errors_with_base(&clean, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.is_empty(),
        "the local `fromList` must type the call as its own `Int`:\n{clean}\ngot:\n  {}",
        errs.join("\n  ")
    );

    // …and it is NOT the refined type: the call cannot satisfy a `Bytes`
    // return, which is what a discharge would have made it do.
    let wrong = shadowing_from_list_program(
        "fn fromList(xs: List<Int>) -> Int",
        "    List.len(xs)",
        "fn caller() -> Bytes",
        "    fromList([0, 10, 255])",
    );
    let errs = errors_with_base(&wrong, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.iter()
            .any(|e| e.contains("body returns Int but declared return type is Bytes")),
        "expected the local fn's `Int` to clash with a `Bytes` return:\n{wrong}\ngot:\n  {}",
        if errs.is_empty() {
            "<no errors>".to_string()
        } else {
            errs.join("\n  ")
        }
    );

    // The dependency's constructor is untouched: still recognized, still
    // discharging under its qualified spelling in the very same program.
    let qualified = shadowing_from_list_program(
        "fn fromList(xs: List<Int>) -> Int",
        "    List.len(xs)",
        "fn caller() -> Bytes",
        "    Bytes.fromList([0, 10, 255])",
    );
    let errs = errors_with_base(&qualified, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.is_empty(),
        "the shadowed dependency constructor must still discharge when named:\n{qualified}\ngot:\n  {}",
        errs.join("\n  ")
    );
}

#[test]
fn a_local_from_list_returning_a_result_keeps_its_own_result() {
    // The sharp case. The local fn returns `Result<Int, String>`, so the
    // spelling-keyed discharge really did strip the wrapper: the call typed
    // as plain `Int` and the `Result` return below was reported as an
    // error. The local fn is not a recognized refinement smart constructor
    // — it has no refined carrier and no proven interval — so nothing about
    // it may be discharged.
    let clean = shadowing_from_list_program(
        "fn fromList(xs: List<Int>) -> Result<Int, String>",
        "    Result.Ok(List.len(xs))",
        "fn caller() -> Result<Int, String>",
        "    fromList([0, 10, 255])",
    );
    let errs = errors_with_base(&clean, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.is_empty(),
        "the local `fromList` must keep its own `Result`:\n{clean}\ngot:\n  {}",
        errs.join("\n  ")
    );

    // The `?` operator still applies, which it could not if the call had
    // been discharged to the bare payload.
    let propagated = shadowing_from_list_program(
        "fn fromList(xs: List<Int>) -> Result<Int, String>",
        "    Result.Ok(List.len(xs))",
        "fn caller() -> Result<Int, String>",
        "    n = fromList([0, 10, 255])?\n    Result.Ok(n)",
    );
    let errs = errors_with_base(&propagated, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.is_empty(),
        "`?` must still apply to the local fn's `Result`:\n{propagated}\ngot:\n  {}",
        errs.join("\n  ")
    );

    // And the payload is the local fn's `Int`, not the dependency's `Bytes`.
    let wrong = shadowing_from_list_program(
        "fn fromList(xs: List<Int>) -> Result<Int, String>",
        "    Result.Ok(List.len(xs))",
        "fn caller() -> Result<Bytes, String>",
        "    fromList([0, 10, 255])",
    );
    let errs = errors_with_base(&wrong, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.iter().any(|e| e.contains("Result<Bytes, String>")),
        "expected the local fn's payload to clash with `Result<Bytes, String>`:\n{wrong}\ngot:\n  {}",
        if errs.is_empty() {
            "<no errors>".to_string()
        } else {
            errs.join("\n  ")
        }
    );
}

#[test]
fn integer_slash_operator_is_a_type_error() {
    // The bare `/` operator on two Ints is partial (a zero divisor; over ℤ
    // there is no `i64::MIN / -1` overflow) — so it must be rejected in
    // favour of `Int.div : Result<Int, String>`.
    assert_error_containing(
        "fn f(a: Int, b: Int) -> Int\n    a / b\n",
        "the '/' operator is not defined for Int",
    );
    // The diagnostic points at the total literal-divisor form.
    assert_error_containing(
        "fn f(a: Int, b: Int) -> Int\n    a / b\n",
        "plain Int when the divisor is a nonzero literal",
    );
}

#[test]
fn float_slash_operator_stays_total() {
    // Float `/` (and Int/Float mixed → Float) is total and unchanged.
    assert_no_errors("fn f(a: Float, b: Float) -> Float\n    a / b\n");
    assert_no_errors("fn f(a: Int, b: Float) -> Float\n    Float.fromInt(a) / b\n");
}

#[test]
fn valid_int_to_float() {
    assert_no_errors("fn f(n: Int) -> Float\n    Float.fromInt(n)\n");
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
    assert_no_errors("fn f(x: Float) -> String\n    String.fromFloat(x)\n");
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
        "fn f(s: String) -> String\n    String.fromInt(s)\n",
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
    assert_no_errors("fn f(n: Int) -> String\n    String.fromInt(n)\n");
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
fn error_non_exhaustive_string_without_catch_all() {
    let src = concat!(
        "fn f(s: String) -> Int\n",
        "  match s\n",
        "    \"a\" -> 1\n",
        "    \"b\" -> 2\n",
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
        "fn f(p: Tuple<Int, Int>) -> Int\n",
        "  match p\n",
        "    (_, x) -> x\n",
    );
    assert_no_errors(src);
}

#[test]
fn error_non_exhaustive_tuple_with_literal_only() {
    let src = concat!(
        "fn f(p: Tuple<Int, Int>) -> Int\n",
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
    assert_parse_error_containing(src, "module-level declaration");
}

#[test]
fn removed_effect_alias_blocks_nothing_because_it_is_parse_error() {
    let src = concat!(
        "effects ReadOnly = [Http.get, Disk.readText]\n",
        "fn save(path: String) -> Result<Unit, String>\n",
        "    ! [ReadOnly]\n",
        "    Disk.writeText(path, \"data\")\n",
    );
    assert_parse_error_containing(src, "module-level declaration");
}

#[test]
fn valid_mix_explicit_effects() {
    let src = concat!(
        "fn mixed(url: String, path: String) -> Result<String, String>\n",
        "    ! [Http.post, Disk.readText]\n",
        "    Http.post(url, \"{}\", \"application/json\", {})\n",
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
    assert_parse_error_containing(src, "module-level declaration");
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
fn forward_declared_record_field_type_is_constructible() {
    assert_no_errors(
        r#"record Holder
    item: Thing

record Thing
    value: Int

fn value() -> Int
    Holder(item = Thing(value = 1)).item.value
"#,
    );
}

#[test]
fn forward_declared_types_resolve_at_all_three_member_sites() {
    assert_no_errors(
        r#"record Outer
    items: List<Inner>

type Wrapper
    Wrap(Inner)
    Nothing

record Inner
    value: Int

fn makeOuter() -> Outer
    Outer(items = [Inner(value = 1)])

fn makeWrapper() -> Wrapper
    Wrapper.Wrap(Inner(value = 2))
"#,
    );
}

#[test]
fn type_def_order_does_not_change_the_check_result() {
    let defs = [
        "record Holder\n    item: Thing\n",
        "type Wrapper\n    Wrap(Thing)\n    Nothing\n",
        "record Thing\n    value: Int\n",
    ];
    for order in [
        [0, 1, 2],
        [0, 2, 1],
        [1, 0, 2],
        [1, 2, 0],
        [2, 0, 1],
        [2, 1, 0],
    ] {
        let src = format!(
            "{}\n{}\n{}\nfn makeHolder() -> Holder\n    Holder(item = Thing(value = 1))\n\nfn makeWrapper() -> Wrapper\n    Wrapper.Wrap(Thing(value = 2))\n",
            defs[order[0]], defs[order[1]], defs[order[2]]
        );
        let errs = errors(&src);
        assert!(errs.is_empty(), "order {order:?} failed: {errs:?}");
    }
}

#[test]
fn dependency_module_internal_forward_reference_is_constructible() {
    let root = temp_module_root("dep_forward_type");
    std::fs::write(
        root.join("Beta.av"),
        r#"module Beta
    exposes [Outer, Inner, mkOuter]
    intent = "Build a forward-referencing record."

record Outer
    item: Inner

record Inner
    value: Int

fn mkOuter(v: Int) -> Outer
    Outer(item = Inner(value = v))
"#,
    )
    .expect("write Beta.av failed");
    let src = r#"module App
    depends [Beta]
    intent = "Use the dependency record."

fn value() -> Int
    Beta.mkOuter(3).item.value
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(errs.is_empty(), "expected no errors, got: {errs:?}");
    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn opaque_dep_type_is_not_readable_through_a_forward_declared_local_record() {
    let root = temp_module_root("opaque_forward_capture");
    std::fs::write(
        root.join("Discount.av"),
        r#"module Discount
    exposes [mkDiscount]
    exposes opaque [Discount]
    intent = "Opaque discount."

record Discount
    percent: Float

fn mkDiscount(p: Float) -> Result<Discount, String>
    Result.Ok(Discount(percent = p))
"#,
    )
    .expect("write Discount.av failed");
    let src = r#"module App
    depends [Discount]
    intent = "Keep dependency values opaque."

record Holder
    item: Discount

record Discount
    percent: Float

fn bad() -> Float
    match Discount.mkDiscount(50.0)
        Result.Ok(d) -> Holder(item = d).item.percent
        Result.Err(_) -> 0.0
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.iter()
            .any(|e| e.contains("Record 'Holder' field 'item'")),
        "expected local/dependency type mismatch, got: {errs:?}"
    );
    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn embedded_stdlib_opaque_type_is_not_readable_through_a_forward_declared_local_record() {
    let src = r#"module App
    depends [Bytes]
    intent = "Keep standard-library values opaque."

record Holder
    item: Bytes

record Bytes
    values: List<Int>

fn bad() -> Int
    match Bytes.fromHex("010203")
        Result.Ok(b) -> List.len(Holder(item = b).item.values)
        Result.Err(_) -> 0
"#;
    let errs = errors_with_base(src, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.iter()
            .any(|e| e.contains("Record 'Holder' field 'item'")),
        "expected local/stdlib type mismatch, got: {errs:?}"
    );
}

#[test]
fn locally_declared_type_cannot_enter_a_dependency_slot() {
    let src = r#"module App
    depends [Bytes]
    intent = "Keep local values out of standard-library slots."

record Bytes
    values: List<Int>

fn bad() -> String
    Bytes.toHex(Bytes(values = [999, -1]))
"#;
    let errs = errors_with_base(src, env!("CARGO_MANIFEST_DIR"));
    assert!(
        errs.iter()
            .any(|e| e.contains("Argument 1 of 'Bytes.toHex'")),
        "expected dependency-slot type mismatch, got: {errs:?}"
    );
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
    // signature `(BranchPath, Int, Int, Int) -> Result<Int, String>`. The stub function
    // matches that signature, and the law body uses `rnd` with args of
    // that shape on the RHS, so the law must type-check cleanly.
    let src = concat!(
        "fn stub(path: BranchPath, n: Int, min: Int, max: Int) -> Result<Int, String>\n",
        "    Result.Ok(min)\n",
        "fn caller() -> Int\n",
        "    ! [Random.int]\n",
        "    Random.int(0, 10)\n",
        "verify caller law consistent\n",
        "    given rnd: Random.int = [stub]\n",
        "    caller() => Result.withDefault(rnd(BranchPath.Root, 0, 0, 10), 0)\n",
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
        "fn caller() -> Result<Int, String>\n",
        "    ! [Random.int]\n",
        "    Random.int(0, 10)\n",
        "verify caller law consistent\n",
        "    given rnd: Random.int = [stub]\n",
        "    caller() => rnd(BranchPath.Root, 0, 0, 10)\n",
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
        "fn rollN(n: Int) -> Result<Int, String>\n",
        "    ! [Random.int]\n",
        "    match n\n",
        "        0 -> Result.Ok(0)\n",
        "        _ -> match Random.int(1, 6)\n",
        "            Result.Err(msg) -> Result.Err(msg)\n",
        "            Result.Ok(_) -> rollN(n - 1)\n",
        "verify rollN trace law rollNSpec\n",
        "    given rnd: Random.int = [stub]\n",
        "    rollN(0) => Result.Ok(0)\n",
        "fn stub(path: BranchPath, k: Int, min: Int, max: Int) -> Result<Int, String>\n",
        "    Result.Ok(min)\n",
    );
    assert_error_containing(src, "recursive effectful function");
}

#[test]
fn result_only_law_on_recursive_effectful_function_is_accepted() {
    // Same recursive effectful function, but without the `trace` keyword →
    // must still type-check. Result-only laws for effectful recursion stay
    // fully supported.
    let src = concat!(
        "fn rollN(n: Int) -> Result<Int, String>\n",
        "    ! [Random.int]\n",
        "    match n\n",
        "        0 -> Result.Ok(0)\n",
        "        _ -> match Random.int(1, 6)\n",
        "            Result.Err(msg) -> Result.Err(msg)\n",
        "            Result.Ok(_) -> rollN(n - 1)\n",
        "verify rollN law rollNSpec\n",
        "    given rnd: Random.int = [stub]\n",
        "    rollN(0) => Result.Ok(0)\n",
        "fn stub(path: BranchPath, k: Int, min: Int, max: Int) -> Result<Int, String>\n",
        "    Result.Ok(min)\n",
    );
    let errs = errors(src);
    assert!(
        !errs
            .iter()
            .any(|e| e.contains("recursive effectful function")),
        "result-only law should not be rejected; got: {:?}",
        errs
    );
}

#[test]
fn trace_law_on_non_recursive_effectful_function_is_accepted() {
    // Trace-aware law on a non-recursive effectful function is fine.
    let src = concat!(
        "fn pick() -> Result<Int, String>\n",
        "    ! [Random.int]\n",
        "    Random.int(1, 6)\n",
        "verify pick trace law pickSpec\n",
        "    given rnd: Random.int = [stub]\n",
        "    pick() => stub(BranchPath.Root, 0, 1, 6)\n",
        "fn stub(path: BranchPath, k: Int, min: Int, max: Int) -> Result<Int, String>\n",
        "    Result.Ok(min)\n",
    );
    let errs = errors(src);
    assert!(
        !errs
            .iter()
            .any(|e| e.contains("recursive effectful function")),
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
        !errs
            .iter()
            .any(|e| e.contains("recursive effectful function")),
        "pure recursive function should not trigger the rejection; got: {:?}",
        errs
    );
}

// ---------------------------------------------------------------------------
// Oracle v1 — rejection for unclassified (stateful / interactive) effects
// ---------------------------------------------------------------------------

#[test]
fn verify_law_on_newly_classified_effects_is_accepted() {
    // Env.set is a fallible GenerativeOutput effect; Terminal.setColor stays
    // Output. Effect stubs do not imply state — `Env.set` does NOT affect
    // a later `Env.get`, `Terminal.setColor` does NOT model modal state
    // across calls. The trace records the call; that's all. Laws over
    // these now type-check; cross-call consistency, if needed, belongs
    // in pure user data, not the effect oracle.
    let src = concat!(
        "fn configure(k: String, v: String) -> Result<Unit, String>\n",
        "    ! [Env.set]\n",
        "    Env.set(k, v)\n",
        "fn setOk(path: BranchPath, n: Int, k: String, v: String) -> Result<Unit, String>\n",
        "    Result.Ok(Unit)\n",
        "verify configure law configureSpec\n",
        "    given k: String = [\"K\"]\n",
        "    given v: String = [\"V\"]\n",
        "    given env: Env.set = [setOk]\n",
        "    configure(k, v) => configure(k, v)\n",
    );
    let errs = errors(src);
    assert!(
        !errs
            .iter()
            .any(|e| e.contains("outside Oracle v1's proof subset")),
        "Env.set should be classified, got: {:?}",
        errs
    );

    let src = concat!(
        "fn paint(c: String) -> Unit\n",
        "    ! [Terminal.setColor]\n",
        "    Terminal.setColor(c)\n",
        "verify paint law paintSpec\n",
        "    given c: String = [\"red\"]\n",
        "    paint(c) => paint(c)\n",
    );
    let errs = errors(src);
    assert!(
        !errs
            .iter()
            .any(|e| e.contains("outside Oracle v1's proof subset")),
        "Terminal.setColor should be classified, got: {:?}",
        errs
    );
}

#[test]
fn verify_law_on_classified_effects_is_accepted() {
    // Random.int and Console.print are both in Oracle v1 — law should
    // type-check cleanly (no "outside proof subset" error).
    let src = concat!(
        "fn roll() -> Int\n",
        "    ! [Random.int, Console.print]\n",
        "    n = Random.int(1, 6)\n",
        "    Console.print(\"rolled\")\n",
        "    n\n",
        "verify roll law rollSpec\n",
        "    given rnd: Random.int = [stubRnd]\n",
        "    roll() => Result.withDefault(stubRnd(BranchPath.Root, 0, 1, 6), 1)\n",
        "fn stubRnd(path: BranchPath, k: Int, min: Int, max: Int) -> Result<Int, String>\n",
        "    Result.Ok(min)\n",
    );
    let errs = errors(src);
    assert!(
        !errs
            .iter()
            .any(|e| e.contains("outside Oracle v1's proof subset")),
        "classified effects should not trigger the rejection; got: {:?}",
        errs
    );
}

#[test]
fn verify_law_with_duplicate_given_for_same_effect_is_rejected() {
    // Two `given` bindings for the same effect method have no sensible
    // mapping — lifted fn has one oracle param per unique effect, so the
    // second stub has no slot to bind to. The emitted theorem quantifies
    // both stubs and asserts stubA = stubB, which is false.
    let src = concat!(
        "fn roll() -> Result<Int, String>\n",
        "    ! [Random.int]\n",
        "    Random.int(1, 6)\n",
        "verify roll law rollSpec\n",
        "    given rnd: Random.int = [stubA]\n",
        "    given rnd2: Random.int = [stubB]\n",
        "    roll() => rnd(BranchPath.Root, 0, 1, 6)\n",
        "fn stubA(path: BranchPath, k: Int, lo: Int, hi: Int) -> Result<Int, String>\n",
        "    Result.Ok(lo)\n",
        "fn stubB(path: BranchPath, k: Int, lo: Int, hi: Int) -> Result<Int, String>\n",
        "    Result.Ok(hi)\n",
    );
    assert_error_containing(src, "2 `given` bindings for the same effect 'Random.int'");
    assert_error_containing(src, "rnd, rnd2");
    assert_error_containing(src, "multi-value domain");
}

#[test]
fn verify_law_with_single_given_for_effect_is_accepted() {
    // Control — same shape as the duplicate-given test, minus the second
    // given. Confirms the rejection targets duplicates specifically.
    let src = concat!(
        "fn roll() -> Result<Int, String>\n",
        "    ! [Random.int]\n",
        "    Random.int(1, 6)\n",
        "verify roll law rollSpec\n",
        "    given rnd: Random.int = [stubA]\n",
        "    roll() => rnd(BranchPath.Root, 0, 1, 6)\n",
        "fn stubA(path: BranchPath, k: Int, lo: Int, hi: Int) -> Result<Int, String>\n",
        "    Result.Ok(lo)\n",
    );
    let errs = errors(src);
    assert!(
        !errs
            .iter()
            .any(|e| e.contains("`given` bindings for the same effect")),
        "single given should not trigger duplicate-given rejection; got: {:?}",
        errs
    );
}

#[test]
fn verify_law_multi_value_given_is_not_duplicate_given() {
    // A single `given` with a multi-value domain is the correct way to
    // test multiple stubs — must not be conflated with duplicate givens.
    let src = concat!(
        "fn roll() -> Result<Int, String>\n",
        "    ! [Random.int]\n",
        "    Random.int(1, 6)\n",
        "verify roll law rollSpec\n",
        "    given rnd: Random.int = [stubA, stubB]\n",
        "    roll() => rnd(BranchPath.Root, 0, 1, 6)\n",
        "fn stubA(path: BranchPath, k: Int, lo: Int, hi: Int) -> Result<Int, String>\n",
        "    Result.Ok(lo)\n",
        "fn stubB(path: BranchPath, k: Int, lo: Int, hi: Int) -> Result<Int, String>\n",
        "    Result.Ok(hi)\n",
    );
    let errs = errors(src);
    assert!(
        !errs
            .iter()
            .any(|e| e.contains("`given` bindings for the same effect")),
        "multi-value given must not be flagged as duplicate; got: {:?}",
        errs
    );
}

#[test]
fn verify_cases_on_stateful_effect_is_not_rejected_as_proof_subset() {
    // Cases-form (`verify fn` without `law`) is unit-test flavor, not
    // proof-subject — don't emit the "outside proof subset" diagnostic
    // there. This test only checks that *this specific* diagnostic is
    // absent; the block itself may have unrelated errors.
    let src = concat!(
        "fn saveLog(msg: String) -> Result<Unit, String>\n",
        "    ! [Disk.writeText]\n",
        "    Disk.writeText(\"/tmp/log\", msg)\n",
        "verify saveLog\n",
        "    saveLog(\"hi\") => saveLog(\"hi\")\n",
    );
    let errs = errors(src);
    assert!(
        !errs
            .iter()
            .any(|e| e.contains("outside Oracle v1's proof subset")),
        "cases-form verify should not trigger the proof-subset rejection; got: {:?}",
        errs
    );
}

// ---------------------------------------------------------------------------
// Terminal service effect & signature checking
// ---------------------------------------------------------------------------

#[test]
#[cfg(feature = "terminal")]
fn error_terminal_clear_without_effect() {
    let src = concat!(
        "fn wipe() -> Result<Unit, String>\n",
        "    Terminal.clear()\n",
    );
    assert_error_containing(src, "has effect 'Terminal.clear'");
}

#[test]
#[cfg(feature = "terminal")]
fn error_terminal_move_to_wrong_arg_count() {
    let src = concat!(
        "fn go(x: Int) -> Result<Unit, String>\n",
        "    ! [Terminal.moveTo]\n",
        "    Terminal.moveTo(x)\n",
    );
    assert_error_containing(src, "expects 2 argument(s)");
}

#[test]
#[cfg(feature = "terminal")]
fn terminal_read_key_returns_result_option_string() {
    let src = concat!(
        "fn poll() -> Result<Option<String>, String>\n",
        "    ! [Terminal.readKey]\n",
        "    Terminal.readKey()\n",
    );
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// Polymorphic recursion (0.20.1 — occurs check regression)
// ---------------------------------------------------------------------------

/// Polymorphic recursion that would need `A := List<A>` must surface as
/// a normal type-incompatibility error — not silently typecheck, not
/// loop, not panic. The matcher's occurs check is what guarantees this:
/// `bind_expected_var` refuses the circular bind, the caller emits the
/// standard "expected A, got List<A>" diagnostic.
///
/// Phase 4.7+ pass 6 — closes the theoretical gap a reviewer of Zero
/// (peer language at the time) flagged: "polymorphic recursion can make
/// type-check non-terminate". Aver never looped (the matcher terminated
/// structurally regardless), but it could populate the substitution map
/// with a circular `A → List<A>` entry. Belt + suspenders fix.
#[test]
fn polymorphic_recursion_with_t_into_list_t_is_type_error() {
    // `A` is a declared nominal type. Aver has no user-written generic
    // parameters — a bare capitalised name in type position names a type
    // and nothing else — so leaving `A` undeclared would make this an
    // undeclared-name test (#859) instead of the recursion-shape test it is.
    let src = concat!(
        "record A\n",
        "    v: Int\n",
        "fn nest(v: A) -> Unit\n",
        "    nest([v])\n",
    );
    let errs = errors(src);
    assert!(
        !errs.is_empty(),
        "expected at least one type error for `A := List<A>` recursive call shape, got none"
    );
    // The exact wording is "expected A, got List<A>" or similar; the
    // diagnostic shape isn't pinned to a specific phrasing here so a
    // future error-message polish doesn't break the regression test.
    // What we lock in is *some* error surfaces — the matcher refuses
    // the bind, the caller emits a real diagnostic.
}

/// Sanity that a similar recursive shape WITHOUT the type expansion
/// (just `A` consumed at the same level) still typechecks cleanly. Makes
/// sure the occurs check isn't over-rejecting legitimate recursive
/// generic calls.
#[test]
fn monomorphic_recursion_at_same_type_param_is_fine() {
    let src = concat!(
        "record A\n",
        "    v: Int\n",
        "fn identityChain(v: A) -> Unit\n",
        "    identityChain(v)\n",
    );
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// Duplicate function names (Iron — A2)
// ---------------------------------------------------------------------------

/// Pre-Iron, defining the same fn name twice silently dropped the first
/// definition (AGENTS.md "Known issues" §`No check for duplicate
/// function names`). The user got no signal — their program ran the
/// second definition and the first was dead code they didn't know was
/// dead. Now the second definition surfaces a real type error.
#[test]
fn duplicate_function_name_is_rejected() {
    let src = concat!(
        "fn double(x: Int) -> Int\n",
        "    x * 2\n",
        "fn double(x: Int) -> Int\n",
        "    x + x\n",
    );
    assert_error_containing(src, "Function 'double' is already defined");
}

/// Same fn name with different signatures (parameter count / type /
/// return) is still a duplicate — Aver does not have function
/// overloading, the second def is the bug shape regardless of how it
/// differs from the first.
#[test]
fn duplicate_function_name_rejected_even_with_different_signature() {
    let src = concat!(
        "fn handler() -> Unit\n",
        "    handler()\n",
        "fn handler(x: Int) -> Int\n",
        "    x\n",
    );
    assert_error_containing(src, "Function 'handler' is already defined");
}

/// Sanity: distinct fn names compile cleanly. Locks the check at
/// "duplicate" rather than "any second registration in fn_sigs".
#[test]
fn distinct_function_names_compile_cleanly() {
    let src = concat!(
        "fn double(x: Int) -> Int\n",
        "    x * 2\n",
        "fn triple(x: Int) -> Int\n",
        "    x * 3\n",
    );
    assert_no_errors(src);
}

/// Iron — A3 soundness regression: two modules each exposing a type
/// `Shape` with different variant sets used to silently merge through
/// the matcher's `ends_with(".Shape")` suffix fallback, so a function
/// declared `fn takesShape(s: A.Shape)` would accept a value of type
/// `B.Shape` at the call site and crash at runtime. After A3 the
/// matcher resolves `Type::Named` references to their canonical
/// "Module.Type" via `sig_aliases` before strict comparison; the
/// crossed call is rejected at typecheck time.
#[test]
fn cross_module_same_named_types_do_not_merge() {
    let root = temp_module_root("cross_module_shape");
    std::fs::write(
        root.join("A.av"),
        r#"module A
    exposes [Shape]
    intent = "Module A's Shape."

type Shape
    Circle(Float)
    Square(Float)
"#,
    )
    .expect("write A.av failed");
    std::fs::write(
        root.join("B.av"),
        r#"module B
    exposes [Shape]
    intent = "Module B's Shape."

type Shape
    Triangle(Float)
    Hexagon(Float)
"#,
    )
    .expect("write B.av failed");

    let src = r#"module Main
    depends [A, B]
    intent = "Crosses A.Shape and B.Shape."

fn takesShape(s: A.Shape) -> Float
    match s
        A.Shape.Circle(r) -> r
        A.Shape.Square(s) -> s

fn main() -> Float
    takesShape(B.Shape.Triangle(3.0))
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    // After A3 the matcher resolves `Type::Named` references through
    // `sig_aliases` before strict comparison; the call therefore
    // surfaces as an argument-type mismatch instead of silently
    // accepting the wrong canonical. The "got" side displays the
    // raw Type::Named string (bare `Shape`), but the mismatch with
    // the expected `A.Shape` is the load-bearing assertion.
    assert!(
        errs.iter()
            .any(|e| e.contains("Argument 1 of 'takesShape'") && e.contains("expected A.Shape")),
        "expected A.Shape mismatch on takesShape arg, got: {:?}",
        errs
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Counter-test for the soundness gate above: when only ONE module
/// exposes a `Status`, a function in a dependent module referring
/// to it by either the bare name or the qualified `Mod.Status` form
/// type-checks identically. Locks in "tighter matcher must not
/// regress the legit aliasing path" alongside the soundness fix.
#[test]
fn bare_and_qualified_name_for_same_type_match() {
    let root = temp_module_root("bare_vs_qualified");
    std::fs::write(
        root.join("Types.av"),
        r#"module Types
    exposes [Status, open]
    intent = "Single module Status."

type Status
    Open
    Closed

fn open() -> Status
    Status.Open
"#,
    )
    .expect("write Types.av failed");

    let src = r#"module App
    depends [Types]
    intent = "Mixes bare and qualified references to the same type."

record Wrapper
    status: Types.Status

fn make() -> Wrapper
    Wrapper(status = Types.open())

fn pick() -> Int
    match make().status
        Types.Status.Open -> 1
        Types.Status.Closed -> 0
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.is_empty(),
        "expected single-module qualified usage to typecheck, got:\n  {}",
        errs.join("\n  ")
    );

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn local_type_declared_after_use_still_shadows_a_dependency_bare_name() {
    let root = temp_module_root("local_type_shadows_dep");
    std::fs::write(
        root.join("Alpha.av"),
        r#"module Alpha
    exposes [Thing, mkThing]
    intent = "Expose a dependency Thing."

record Thing
    tag: String

fn mkThing(tag: String) -> Thing
    Thing(tag = tag)
"#,
    )
    .expect("write Alpha.av failed");
    let local_src = r#"module App
    depends [Alpha]
    intent = "The local Thing shadows Alpha.Thing."

record Holder
    item: Thing

record Thing
    value: Int

fn local() -> Holder
    Holder(item = Thing(value = 1))
"#;
    let local_errs = errors_with_base(local_src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        local_errs.is_empty(),
        "local construction should typecheck, got: {local_errs:?}"
    );

    let captured_src = format!(
        "{local_src}\nfn captured() -> Holder\n    Holder(item = Alpha.mkThing(\"hello\"))\n"
    );
    let captured_errs = errors_with_base(&captured_src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        captured_errs
            .iter()
            .any(|e| e.contains("Record 'Holder' field 'item'")),
        "dependency value should be rejected, got: {captured_errs:?}"
    );
    assert!(
        captured_errs
            .iter()
            .all(|e| !e.contains("Ambiguous type name 'Thing'")),
        "local shadowing must not become ambiguity: {captured_errs:?}"
    );
    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review #148): when two dep modules each
/// expose the same bare type name (`A.Shape` *and* `B.Shape` both
/// exposed as `Shape`), the bare alias resolution must NOT silently
/// pick one. The pre-review code last-write-won an arbitrary
/// `TypeId`; that's the same bug class typed identity is supposed to
/// eliminate. Verify the bare reference is rejected so callers are
/// forced to qualify.
#[test]
fn cross_module_same_bare_type_name_bare_reference_is_ambiguous() {
    let root = temp_module_root("bare_ambiguous_type");
    std::fs::write(
        root.join("A.av"),
        r#"module A
    exposes [Shape]
    intent = "Module A's Shape."

type Shape
    Circle(Float)
"#,
    )
    .expect("write A.av failed");
    std::fs::write(
        root.join("B.av"),
        r#"module B
    exposes [Shape]
    intent = "Module B's Shape."

type Shape
    Triangle(Float)
"#,
    )
    .expect("write B.av failed");

    let src = r#"module Main
    depends [A, B]
    intent = "Bare reference to Shape must be ambiguous."

fn takesShape(s: Shape) -> Float
    0.0

fn callsWithA() -> Float
    takesShape(A.Shape.Circle(1.0))

fn callsWithB() -> Float
    takesShape(B.Shape.Triangle(2.0))
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    // The param annotation `s: Shape` cannot resolve — both
    // `A.Shape` and `B.Shape` are exposed, so the bare alias is
    // `Ambiguous` and `resolve_type_id` deliberately refuses it.
    // Each call site then surfaces a typed-identity mismatch: the
    // caller's `A.Shape.Circle(...)` / `B.Shape.Triangle(...)` stamps
    // a `Type::Named { id: Some(_), name: "Shape" }` value, the
    // param is `Type::Named { id: None, name: "Shape" }`, and the
    // matcher refuses the mixed-id case for ambiguous-by-design
    // names. Without that refusal the old name-equality fallback
    // would let both calls go through and silently agree on
    // whichever `Shape` happened to win the global alias slot.
    // Both call sites are equally unsound (one passes A.Shape, the
    // other B.Shape against an ambiguous bare param), so both must
    // surface. Asserting exact count catches an asymmetric regression
    // where one side gets rejected because the bare alias secretly
    // resolved to the other module's identity.
    let call_mismatches: Vec<&String> = errs
        .iter()
        .filter(|e| e.contains("Argument 1 of 'takesShape'"))
        .collect();
    assert_eq!(
        call_mismatches.len(),
        2,
        "expected exactly 2 argument-mismatch diagnostics on `takesShape(...)` calls when `Shape` is ambiguous, got: {errs:?}"
    );
    // Explicit ambiguity diagnostic on the param annotation itself —
    // surfaces the underlying cause instead of just two
    // type-mismatch errors that read `expected Shape, got Shape`.
    assert!(
        errs.iter().any(|e| {
            e.contains("Ambiguous type name 'Shape'")
                && e.contains("A.Shape")
                && e.contains("B.Shape")
        }),
        "expected an 'Ambiguous type name Shape; use A.Shape or B.Shape' diagnostic, got: {errs:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review round 7): `canonicalize_named` must
/// treat an existing `Some(TypeId)` as sacred. Pre-fix, the
/// canonicaliser overwrote any `id` by whatever `resolve_type_id`
/// returned in the *current* checker context — so a value coming
/// back from `C.make()` correctly stamped with `Some(C.Shape)`
/// would get re-stamped to `Some(A.Shape)` when `infer_type` re-
/// canonicalised it in the importer's context (A's `Shape` is the
/// only visible bare alias). Soundness collapsed: `A.consume(C.make())`
/// silently typechecked.
///
/// Fix: canonicaliser only fills in `id: None`. Once a stamp has
/// `id: Some(_)`, no later context can override it.
#[test]
fn canonicalize_named_does_not_overwrite_existing_typeid() {
    let root = temp_module_root("canonicalize_preserves_id");
    std::fs::write(
        root.join("A.av"),
        r#"module A
    exposes [Shape, consume]
    intent = "Public A.Shape + consumer."

type Shape
    Circle(Float)

fn consume(s: Shape) -> Float
    match s
        Shape.Circle(r) -> r
"#,
    )
    .expect("write A.av failed");
    std::fs::write(
        root.join("C.av"),
        r#"module C
    exposes [make]
    intent = "Private C.Shape (only `make` exposed)."

type Shape
    Hexagon(Float)

fn make() -> Shape
    Shape.Hexagon(1.0)
"#,
    )
    .expect("write C.av failed");

    let src = r#"module Main
    depends [A, C]
    intent = "Tries to feed C.make() into A.consume — distinct types, must reject."

fn caller() -> Float
    A.consume(C.make())
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.iter()
            .any(|e| e.contains("Argument 1 of 'A.consume'")
                || e.contains("Argument 1 of 'consume'")),
        "expected an argument-type mismatch on `A.consume(C.make())` (A.Shape != C.Shape); got: {errs:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review round 6): an exported `fn` in module
/// B that references a type by bare name must resolve in B's own
/// resolver context — B's own types + B's actual `depends` transitive
/// exports — NOT against arbitrary siblings the entry module happens
/// to also pull in.
///
/// Repro: B depends on A and re-uses `A.Shape` as bare `Shape` in
/// its own signature. Main directly depends on A, B, and C (C exposes an
/// unrelated `Shape`). Pre-fix, when Main's checker canonicalised
/// B's exported sig, the bare `Shape` resolved against the
/// importer's bare alias map — which was ambiguous between
/// `A.Shape` and `C.Shape` — and the typed-id never got populated.
/// `takeA(B.pass(A.make()))` then failed with the cross-module
/// collapse. Fix: per-owner resolver knows B's own `depends [A]`,
/// resolves `Shape` to `A.Shape`, ignoring siblings.
#[test]
fn dep_module_resolver_uses_owner_depends_not_importer_siblings() {
    let root = temp_module_root("owner_depends_resolver");
    std::fs::write(
        root.join("A.av"),
        r#"module A
    exposes [Shape, make]
    intent = "A.Shape + factory."

type Shape
    Circle(Float)

fn make() -> Shape
    Shape.Circle(1.0)
"#,
    )
    .expect("write A.av failed");
    std::fs::write(
        root.join("B.av"),
        r#"module B
    depends [A]
    exposes [pass]
    intent = "Re-exports an A.Shape passthrough; uses bare `Shape`."

fn pass(s: Shape) -> Shape
    s
"#,
    )
    .expect("write B.av failed");
    std::fs::write(
        root.join("C.av"),
        r#"module C
    exposes [Shape]
    intent = "Unrelated sibling Shape that must NOT leak into B's scope."

type Shape
    Triangle(Float)
"#,
    )
    .expect("write C.av failed");

    let src = r#"module Main
    depends [A, B, C]
    intent = "Plumbs A.make through B.pass; C is just an unrelated sibling."

fn takeA(s: A.Shape) -> Float
    match s
        A.Shape.Circle(r) -> r

fn caller() -> Float
    takeA(B.pass(A.make()))
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.is_empty(),
        "expected B's bare `Shape` to resolve to A.Shape via B's own depends; got: {errs:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review round 6 part 2): a dep module B's
/// bare `Shape` must NOT fall back to a `Shape` declared in the
/// entry module. Entry-scope types don't belong to dep modules'
/// resolver contexts. `B.id(s: Shape)` with no `Shape` in B's scope
/// (and Main defining one) should fail to resolve, not silently bind
/// B's parameter to Main's type.
#[test]
fn dep_module_resolver_does_not_fall_back_to_entry_types() {
    let root = temp_module_root("dep_resolver_no_entry_fallback");
    std::fs::write(
        root.join("B.av"),
        r#"module B
    exposes [id]
    intent = "Has no `Shape`; uses bare `Shape` in its signature."

fn id(s: Shape) -> Shape
    s
"#,
    )
    .expect("write B.av failed");

    let src = r#"module Main
    depends [B]
    intent = "Defines its own Shape; B must NOT pick it up."

type Shape
    Circle(Float)

fn caller() -> Float
    match B.id(Shape.Circle(1.0))
        Shape.Circle(r) -> r
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    // B's signature contains an unresolved `Shape` — B has neither
    // its own nor any dep providing one. The pre-fix entry-fallback
    // let this silently bind to Main's `Shape`, smuggling Main's
    // type into B's signature. Either B's sig fails to typecheck
    // (B can't resolve `Shape`) or Main's call surfaces the
    // identity mismatch; both are acceptable, but a quiet `✓ types`
    // is the regression we're guarding against.
    assert!(
        !errs.is_empty(),
        "expected B's `s: Shape` to fail to silently bind to Main.Shape via entry-fallback; got no errors"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review round 5 F1): the type stamp on
/// a dep module's exported fn signature must resolve in the dep's
/// own scope, not the importer's. Pre-fix
/// `integrate_registry` called the regular `canonicalize_named`,
/// which uses the importer's `current_module_prefix` + bare alias
/// map — so `B.make() -> Shape` (where `Shape` is B's own type) lost
/// its `TypeId` whenever `Shape` was ambiguous or unresolvable in
/// `Main`. The user-visible failure was
/// `expected B.Shape, got Shape` on every cross-module call.
#[test]
fn dep_module_exported_sig_resolves_in_owner_scope() {
    let root = temp_module_root("owner_aware_canonicalize");
    std::fs::write(
        root.join("A.av"),
        r#"module A
    exposes [Shape]
    intent = "A.Shape."

type Shape
    Circle(Float)
"#,
    )
    .expect("write A.av failed");
    std::fs::write(
        root.join("B.av"),
        r#"module B
    exposes [Shape, make]
    intent = "B.Shape + factory."

type Shape
    Triangle(Float)

fn make() -> Shape
    Shape.Triangle(2.0)
"#,
    )
    .expect("write B.av failed");

    let src = r#"module Main
    depends [A, B]
    intent = "Forwards B.make() into a B.Shape consumer."

fn takeB(s: B.Shape) -> Float
    match s
        B.Shape.Triangle(r) -> r

fn caller() -> Float
    takeB(B.make())
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.is_empty(),
        "expected `takeB(B.make())` to typecheck cleanly when B.make's return is B.Shape; got: {errs:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Standard capability resources are implicit language atoms, so a regular
/// module may expose a signature containing `Tcp.Connection` without spelling
/// `depends [Tcp]`. The importer and the exported signature must receive the
/// same nominal TypeId; equal display names are not enough.
#[test]
fn standard_capability_resource_identity_crosses_imported_signatures() {
    let root = temp_module_root("standard_resource_identity");
    std::fs::write(
        root.join("Relay.av"),
        r#"module Relay
    exposes [forward]
    intent = "Threads a standard capability resource through its API."

fn forward(connection: Tcp.Connection) -> Tcp.Connection
    connection
"#,
    )
    .expect("write Relay.av failed");

    let src = r#"module Main
    depends [Relay]
    intent = "Passes one canonical Tcp.Connection across a module boundary."

fn forward(connection: Tcp.Connection) -> Tcp.Connection
    Relay.forward(connection)
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.is_empty(),
        "expected Tcp.Connection to retain one capability-owned identity across the imported signature, got: {errs:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review round 5 F1, soundness counterpart):
/// even when both dep modules hide their own `Shape` from `exposes`,
/// the typechecker must still distinguish `C.Shape` from `D.Shape`
/// at the matcher boundary so `D.consume(C.make())` is rejected.
/// Pre-fix the registry-side canonicalisation left both signatures'
/// `Shape` references unresolved (`id: None`), the matcher's
/// `(None, None)` branch compared by name, and the call silently
/// passed. With owner-aware canonicalisation each side carries its
/// own real `TypeId` (still treated as opaque to the importer
/// because neither type is exposed) and the matcher rejects.
#[test]
fn private_exported_sig_types_do_not_collapse_across_modules() {
    let root = temp_module_root("private_exported_sigs_distinct");
    std::fs::write(
        root.join("C.av"),
        r#"module C
    exposes [make]
    intent = "C's Shape is private but appears in exported make."

type Shape
    Hexagon(Float)

fn make() -> Shape
    Shape.Hexagon(1.0)
"#,
    )
    .expect("write C.av failed");
    std::fs::write(
        root.join("D.av"),
        r#"module D
    exposes [consume]
    intent = "D's Shape is private but appears in exported consume."

type Shape
    Circle(Float)

fn consume(s: Shape) -> Float
    match s
        Shape.Circle(r) -> r
"#,
    )
    .expect("write D.av failed");

    let src = r#"module Main
    depends [C, D]
    intent = "Tries to feed C.make() into D.consume — distinct private types."

fn main() -> Float
    D.consume(C.make())
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.iter()
            .any(|e| e.contains("Argument 1 of 'D.consume'")
                || e.contains("Argument 1 of 'consume'")),
        "expected an argument-type mismatch on `D.consume(C.make())` (C.Shape != D.Shape); got: {errs:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review round 4 F1): a qualified
/// `C.Shape` reference must NOT resolve when `C` doesn't expose
/// `Shape`. Pre-fix `resolve_type_id` consulted the symbol table
/// directly, which carries every dep type regardless of the
/// `exposes` contract, so the qualified import bypassed visibility.
/// Now `resolve_type_id` filters through `visible_type_ids`
/// (populated only from `SymbolRegistry::from_modules` and own-
/// module declarations); the qualified private import either fails
/// to resolve, or — when picked up at a signature boundary — the
/// explicit "private import" diagnostic surfaces.
#[test]
fn qualified_private_dep_type_does_not_resolve() {
    let root = temp_module_root("qualified_private_import");
    std::fs::write(
        root.join("C.av"),
        r#"module C
    exposes [helper]
    intent = "C.Shape declared but NOT exposed."

type Shape
    Hexagon(Float)

fn helper() -> Int
    0
"#,
    )
    .expect("write C.av failed");

    let src = r#"module Main
    depends [C]
    intent = "Tries to import C.Shape directly."

fn takesPrivate(s: C.Shape) -> Float
    0.0
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.iter().any(|e| {
            (e.contains("private") || e.contains("not exposed") || e.contains("not visible"))
                && e.contains("C.Shape")
        }),
        "expected a 'C.Shape is private / not exposed' diagnostic on `s: C.Shape`, got: {errs:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review round 3 nit #1): the ambiguity
/// diagnostic must NOT list a private (non-exposed) candidate even
/// when it shares the bare name. `Resolution::Ambiguous(Vec<TypeId>)`
/// carries the candidate IDs populated through visibility-exposed
/// aliases — types that aren't exposed never reach the alias map, so
/// `ambiguous_type_candidates` returns only the names the user can
/// actually pick from.
#[test]
fn ambiguity_diagnostic_omits_private_dep_candidates() {
    let root = temp_module_root("ambiguity_private_excluded");
    std::fs::write(
        root.join("A.av"),
        r#"module A
    exposes [Shape]
    intent = "Public A.Shape."

type Shape
    Circle(Float)
"#,
    )
    .expect("write A.av failed");
    std::fs::write(
        root.join("B.av"),
        r#"module B
    exposes [Shape]
    intent = "Public B.Shape."

type Shape
    Triangle(Float)
"#,
    )
    .expect("write B.av failed");
    // C declares a `Shape` but does NOT expose it. (Non-empty
    // `exposes [helper]` triggers the explicit-list rule, so `Shape`
    // — absent from the list — stays private.) Pre-fix
    // `ambiguous_type_candidates` would have scanned the full
    // SymbolTable and listed `C.Shape` alongside `A.Shape` / `B.Shape`
    // in the user-facing diagnostic, even though the user can't
    // actually reference `C.Shape` from `Main`.
    std::fs::write(
        root.join("C.av"),
        r#"module C
    exposes [helper]
    intent = "Private C.Shape (Shape NOT in exposes list)."

type Shape
    Hexagon(Float)

fn helper() -> Int
    0
"#,
    )
    .expect("write C.av failed");

    let src = r#"module Main
    depends [A, B, C]
    intent = "Bare Shape with one private dep candidate."

fn takesShape(s: Shape) -> Float
    0.0
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    let ambig = errs
        .iter()
        .find(|e| e.contains("Ambiguous type name 'Shape'"))
        .expect("expected an ambiguity diagnostic");
    assert!(ambig.contains("A.Shape"), "missing A.Shape: {ambig}");
    assert!(ambig.contains("B.Shape"), "missing B.Shape: {ambig}");
    assert!(
        !ambig.contains("C.Shape"),
        "private C.Shape leaked into diagnostic: {ambig}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review round 3 nit #2): the explicit
/// ambiguity diagnostic now also fires on local binding annotations
/// (`x: Shape = ...`), not just function param / return / record
/// field positions. Without this hook the matcher still rejects the
/// program — but the user got "expected Shape, got Shape" instead of
/// the actionable "Ambiguous type name; use A.Shape or B.Shape".
#[test]
fn ambiguous_bare_name_in_binding_annotation_surfaces_explicit_diagnostic() {
    let root = temp_module_root("ambiguous_binding_ann");
    std::fs::write(
        root.join("A.av"),
        r#"module A
    exposes [Shape]
    intent = "A.Shape."

type Shape
    Circle(Float)
"#,
    )
    .expect("write A.av failed");
    std::fs::write(
        root.join("B.av"),
        r#"module B
    exposes [Shape]
    intent = "B.Shape."

type Shape
    Triangle(Float)
"#,
    )
    .expect("write B.av failed");

    let src = r#"module Main
    depends [A, B]
    intent = "Bare Shape annotation in a binding."

fn pick() -> Float
    s: Shape = A.Shape.Circle(1.0)
    0.0
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        errs.iter().any(|e| {
            e.contains("Binding 's' annotation")
                && e.contains("Ambiguous type name 'Shape'")
                && e.contains("A.Shape")
                && e.contains("B.Shape")
        }),
        "expected an ambiguity diagnostic on the binding annotation, got: {errs:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review #148): the exported `TypeCheckResult.fn_sigs`
/// map must not silently bind a bare-name fn entry when two distinct
/// dep modules both expose the same bare name. Rust codegen and
/// other downstream consumers historically did
/// `ctx.fn_sigs.get(&fd.name)` against a `HashMap<String, _>` whose
/// global bare alias was last-write-wins — a real bug class where a
/// `foo` lookup could pick up the wrong module's parameter list.
/// After this PR the bare-name entry exists only when unambiguous;
/// the canonical `"Module.foo"` key is always present.
#[test]
fn cross_module_same_bare_fn_name_drops_bare_alias_in_exported_map() {
    use aver::source::parse_source;
    use aver::types::checker::run_type_check_with_base;

    let root = temp_module_root("bare_ambiguous_fn");
    std::fs::write(
        root.join("A.av"),
        r#"module A
    exposes [doit]
    intent = "Module A's doit."

fn doit(a: Int) -> Int
    a
"#,
    )
    .expect("write A.av failed");
    std::fs::write(
        root.join("B.av"),
        r#"module B
    exposes [doit]
    intent = "Module B's doit (different param)."

fn doit(b: String) -> Int
    0
"#,
    )
    .expect("write B.av failed");

    let src = r#"module Main
    depends [A, B]
    intent = "Two `doit` fns, only the qualified form may resolve."

fn caller() -> Int
    A.doit(1)
"#;
    let items = parse_source(src).expect("parse failed");
    let mut items = items;
    aver::tco::transform_program(&mut items);
    let _ = run_type_check_with_base(&items, root.to_str());
    let result = aver::types::checker::run_type_check_full(&items, root.to_str());

    // Both qualified canonicals are present.
    assert!(
        result.fn_sigs.contains_key("A.doit"),
        "expected canonical A.doit, got keys: {:?}",
        result.fn_sigs.keys().collect::<Vec<_>>()
    );
    assert!(
        result.fn_sigs.contains_key("B.doit"),
        "expected canonical B.doit, got keys: {:?}",
        result.fn_sigs.keys().collect::<Vec<_>>()
    );
    // The bare-name alias is suppressed because A.doit and B.doit
    // disagree — last-write-wins would have left whichever module
    // happened to iterate second silently winning the `doit` slot.
    assert!(
        !result.fn_sigs.contains_key("doit"),
        "bare `doit` must not appear when two dep modules both expose it; keys: {:?}",
        result.fn_sigs.keys().collect::<Vec<_>>()
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Phase B (#138, peer review round 2): when the entry module and a
/// dep module both declare `doit`, source-level `doit()` inside the
/// entry unambiguously means the entry's own fn. The exported
/// `TypeCheckResult.fn_sigs` bare alias must therefore point at the
/// entry fn rather than being suppressed as "ambiguous" — consumers
/// (`diagnostics::context::callgraph`, intent checks) still look the
/// entry's bodies up by `&fd.name` and need
/// the local signature.
#[test]
fn entry_local_fn_shadows_dep_module_bare_alias() {
    use aver::source::parse_source;

    let root = temp_module_root("entry_shadows_dep_fn");
    std::fs::write(
        root.join("Helper.av"),
        r#"module Helper
    exposes [doit]
    intent = "Dep `doit` returning a String."

fn doit(n: Int) -> String
    "from-helper"
"#,
    )
    .expect("write Helper.av failed");

    let src = r#"module Main
    depends [Helper]
    intent = "Local `doit` shadowing the imported one."

fn doit(n: Int) -> Int
    n

fn caller() -> Int
    doit(7)
"#;
    let items = parse_source(src).expect("parse failed");
    let mut items = items;
    aver::tco::transform_program(&mut items);
    let result = aver::types::checker::run_type_check_full(&items, root.to_str());

    // No errors — `doit(7)` resolves to the entry's own fn.
    assert!(
        result.errors.is_empty(),
        "expected entry-local `doit` to resolve cleanly, got: {:?}",
        result.errors
    );
    // Bare alias points at the entry's signature (returns `Int`),
    // not at Helper's (returns `String`).
    let (_, bare_ret, _) = result
        .fn_sigs
        .get("doit")
        .expect("expected bare `doit` alias to be present (entry shadowing)");
    assert!(
        matches!(bare_ret, aver::ast::Type::Int),
        "bare `doit` ret must be the entry fn's `Int`, not Helper.doit's `String`; got {:?}",
        bare_ret
    );
    // Canonical Helper.doit is still reachable for callers that want it.
    assert!(
        result.fn_sigs.contains_key("Helper.doit"),
        "expected canonical Helper.doit, got keys: {:?}",
        result.fn_sigs.keys().collect::<Vec<_>>()
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// Iron — A4: a single source error (here: an unknown function
/// call) must not fan out through downstream `compatible` checks
/// and produce a cascade of "expected X, got Invalid" diagnostics.
/// The matcher now treats `Type::Invalid` as a wildcard ("already
/// reported"), so the surface count stays at the originating error.
#[test]
fn type_invalid_does_not_cascade_through_arg_checks() {
    let src = r#"module Cascade
    intent = "Iron — A4: unknown-fn must not cascade through add() arg checks."

fn add(a: Int, b: Int) -> Int
    a + b

fn main() -> Unit
    ! [Console.print]
    x = unknownFn(1, 2)
    y = unknownFn(3, 4)
    z = add(x, y)
    Console.print("{z}")
"#;
    let errs = errors(src);
    let unknown_fn_count = errs
        .iter()
        .filter(|m| m.contains("Call to unknown function 'unknownFn'"))
        .count();
    let invalid_cascade_count = errs.iter().filter(|m| m.contains("got Invalid")).count();
    assert_eq!(
        unknown_fn_count, 2,
        "expected two unknown-fn errors, got errors: {errs:?}"
    );
    assert_eq!(
        invalid_cascade_count, 0,
        "expected no cascading 'got Invalid' errors after Iron — A4, got: {errs:?}"
    );
}

#[test]
fn independent_product_rejects_non_call_elements_even_under_expected_tuple() {
    // `(...)!` elements must be function calls. The expected-type path for
    // tuple literals (a `Tuple<...>` return annotation drives each element)
    // must not adopt independent products, or this validation is skipped.
    let src = "fn pair(n: Int) -> Tuple<Int, Int>\n    ? \"Pairs.\"\n    (1, n)!\n";
    let errs = errors(src);
    assert!(
        errs.iter()
            .any(|m| m.contains("Independent product element must be a function call")),
        "expected the non-call `(...)!` element to be rejected, got: {errs:?}"
    );
}

#[test]
fn plain_tuple_literal_accepts_expected_tuple_elements() {
    // Positive control for the test above: the same shape WITHOUT `!` is an
    // ordinary tuple literal and must type-check via the expected-type path.
    let src = "fn pair(n: Int) -> Tuple<Int, Int>\n    ? \"Pairs.\"\n    (1, n)\n";
    assert_no_errors(src);
}

// ---------------------------------------------------------------------------
// String interpolation — primitives only, conversions are named in source
// ---------------------------------------------------------------------------

/// The sanctioned set, one embed per primitive. This is exactly what the
/// `__to_str` lowering renders on every backend; if a primitive is added
/// or dropped there, this test and the rule must move together.
#[test]
fn interpolation_accepts_every_primitive() {
    assert_no_errors("fn show(n: Int) -> String\n    \"n={n}\"\n");
    assert_no_errors("fn show(x: Float) -> String\n    \"x={x}\"\n");
    assert_no_errors("fn show(b: Bool) -> String\n    \"b={b}\"\n");
    assert_no_errors("fn show(s: String) -> String\n    \"s={s}\"\n");
}

#[test]
fn interpolation_rejects_a_list_embed() {
    assert_error_containing(
        "fn show(xs: List<Int>) -> String\n    \"xs={xs}\"\n",
        "String interpolation renders primitives only (Int, Float, Bool, String); \
         this embed is List<Int>.",
    );
}

#[test]
fn interpolation_rejects_a_record_embed() {
    let src = "record User\n    name: String\n    age: Int\n\n\
               fn show(u: User) -> String\n    \"u={u}\"\n";
    assert_error_containing(
        src,
        "String interpolation renders primitives only (Int, Float, Bool, String); \
         this embed is User.",
    );
}

#[test]
fn interpolation_rejects_an_option_embed() {
    assert_error_containing(
        "fn show(o: Option<Int>) -> String\n    \"o={o}\"\n",
        "this embed is Option<Int>.",
    );
}

#[test]
fn interpolation_rejects_a_result_embed() {
    assert_error_containing(
        "fn show(r: Result<Int, String>) -> String\n    \"r={r}\"\n",
        "this embed is Result<Int, String>.",
    );
}

#[test]
fn interpolation_rejects_a_map_embed() {
    assert_error_containing(
        "fn show(m: Map<String, Int>) -> String\n    \"m={m}\"\n",
        "this embed is Map<String, Int>.",
    );
}

#[test]
fn interpolation_rejects_a_tuple_embed() {
    assert_error_containing(
        "fn show(t: Tuple<Int, Int>) -> String\n    \"t={t}\"\n",
        "this embed is Tuple<Int, Int>.",
    );
}

#[test]
fn interpolation_rejects_a_vector_embed() {
    assert_error_containing(
        "fn show(v: Vector<Int>) -> String\n    \"v={v}\"\n",
        "this embed is Vector<Int>.",
    );
}

/// The diagnostic must point at the fix the language actually offers —
/// a user-written conversion — and must NOT advertise a stdlib helper,
/// because none exists and none is planned.
#[test]
fn interpolation_rejection_asks_for_a_named_conversion() {
    let errs = errors("fn show(xs: List<Int>) -> String\n    \"xs={xs}\"\n");
    let msg = errs
        .iter()
        .find(|m| m.contains("String interpolation renders primitives only"))
        .expect("expected the interpolation diagnostic");
    assert!(
        msg.contains("named function returning String"),
        "diagnostic must ask for a named conversion: {msg}"
    );
    assert!(
        !msg.contains("String.from") && !msg.contains("List.toString"),
        "diagnostic must not suggest a stdlib helper: {msg}"
    );
}

/// Wrapping the value in a user-written display fn is the sanctioned
/// spelling and must type-check clean — the positive control for the
/// rejections above.
#[test]
fn interpolation_accepts_a_named_conversion_result() {
    let src = r#"fn joinInts(xs: List<Int>) -> String
    match xs
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            _ -> "{head}, {joinInts(tail)}"

fn show(xs: List<Int>) -> String
    "xs=[{joinInts(xs)}]"
"#;
    assert_no_errors(src);
}

/// An embed whose type the checker could not resolve already produced a
/// diagnostic of its own; the interpolation rule must not pile a second
/// one on top (same `Type::Invalid` discipline the neighbouring argument
/// and element rules follow).
#[test]
fn interpolation_does_not_double_report_an_invalid_embed() {
    let errs = errors("fn show() -> String\n    \"v={nope}\"\n");
    assert_eq!(
        errs.iter()
            .filter(|m| m.contains("Unknown identifier 'nope'"))
            .count(),
        1,
        "expected exactly one unknown-identifier error, got: {errs:?}"
    );
    assert_eq!(
        errs.iter()
            .filter(|m| m.contains("String interpolation renders primitives only"))
            .count(),
        0,
        "an already-errored embed must not draw a second diagnostic, got: {errs:?}"
    );
}

#[test]
fn interpolation_does_not_double_report_a_bad_call_embed() {
    let errs = errors("fn show() -> String\n    \"v={missingFn(1)}\"\n");
    assert_eq!(
        errs.iter()
            .filter(|m| m.contains("String interpolation renders primitives only"))
            .count(),
        0,
        "an already-errored embed must not draw a second diagnostic, got: {errs:?}"
    );
}

// --- Embeds whose type inference never pinned ------------------------------
//
// An unresolved `Type::Var` is NOT evidence of an earlier diagnostic, so it
// must not ride the `Type::Invalid` no-double-report acceptance. Each source
// below type-checks clean apart from the interpolation itself; admitting the
// embed would hand a clean-typechecked program to the backends with no
// renderable type for it.

/// The exact fail-open witness: `match Option.None` binds the arm's `x` to
/// the `T` of a bare `Option<T>` subject. Nothing else in this program is an
/// error.
#[test]
fn interpolation_rejects_an_unresolved_embed_from_a_bare_none_match() {
    let src = "fn render() -> String\n\
               \x20   match Option.None\n\
               \x20       Option.Some(x) -> \"{x}\"\n\
               \x20       Option.None -> \"none\"\n";
    let errs = errors(src);
    assert_eq!(
        errs.len(),
        1,
        "the unresolved embed must be the ONLY error — an unresolved variable \
         is not evidence of a prior diagnostic, got: {errs:?}"
    );
    assert!(
        errs[0].contains("the type of this embed could not be determined"),
        "expected the unresolved-embed diagnostic, got: {errs:?}"
    );
    assert!(
        errs[0].contains("named function returning String"),
        "the unresolved diagnostic must still ask for a named conversion: {errs:?}"
    );
}

/// Same hole reached through a local binding rather than an inline subject.
#[test]
fn interpolation_rejects_an_unresolved_embed_bound_through_a_local() {
    let src = "fn render() -> String\n\
               \x20   o = Option.None\n\
               \x20   match o\n\
               \x20       Option.Some(x) -> \"{x}\"\n\
               \x20       Option.None -> \"none\"\n";
    assert_error_containing(src, "the type of this embed could not be determined");
}

/// And through a bare empty-list subject, whose element type is equally open.
#[test]
fn interpolation_rejects_an_unresolved_embed_from_a_bare_empty_list_match() {
    let src = "fn render() -> String\n\
               \x20   match []\n\
               \x20       [] -> \"empty\"\n\
               \x20       [head, ..tail] -> \"{head}\"\n";
    assert_error_containing(src, "the type of this embed could not be determined");
}

/// Pinning the subject's type removes the diagnostic — the positive control
/// proving the rule rejects genuinely-unresolved embeds, not merely
/// pattern-bound ones.
#[test]
fn interpolation_accepts_a_match_binding_off_a_concrete_subject() {
    assert_no_errors(
        "fn render(o: Option<Int>) -> String\n\
         \x20   match o\n\
         \x20       Option.Some(x) -> \"{x}\"\n\
         \x20       Option.None -> \"none\"\n",
    );
    assert_no_errors(
        "fn render(xs: List<Int>) -> String\n\
         \x20   match xs\n\
         \x20       [] -> \"empty\"\n\
         \x20       [head, ..tail] -> \"{head}\"\n",
    );
    // Plain bottom-up local binding: the shape a naive "reject Var" would
    // break if inference deferred pinning to a later pass.
    assert_no_errors("fn render() -> String\n    x = 5\n    \"{x}\"\n");
}

/// The parts of an interpolation are parsed by a sub-parser that sees only
/// the `{...}` fragment, so an embed's own `line` is 1-based inside that
/// fragment. The diagnostic must be anchored to the OUTER interpolation node
/// instead. Both interpolation diagnostics are pinned, on a line > 1, so a
/// refactor cannot silently regress either to line 1.
#[test]
fn interpolation_errors_report_the_outer_source_line() {
    let items = parse(
        "fn pad() -> Int\n\
         \x20   1\n\
         \n\
         fn show(xs: List<Int>) -> String\n\
         \x20   \"xs={xs}\"\n",
    );
    let errs = aver::types::checker::run_type_check(&items);
    let err = errs
        .iter()
        .find(|e| e.message.contains("this embed is List<Int>"))
        .unwrap_or_else(|| panic!("expected the known-type interpolation error, got: {errs:?}"));
    assert_eq!(
        err.line, 5,
        "known-type interpolation error must report the outer line, got: {err:?}"
    );

    let items = parse(
        "fn pad() -> Int\n\
         \x20   1\n\
         \n\
         fn render() -> String\n\
         \x20   match Option.None\n\
         \x20       Option.Some(x) -> \"{x}\"\n\
         \x20       Option.None -> \"none\"\n",
    );
    let errs = aver::types::checker::run_type_check(&items);
    let err = errs
        .iter()
        .find(|e| e.message.contains("could not be determined"))
        .unwrap_or_else(|| panic!("expected the unresolved interpolation error, got: {errs:?}"));
    assert_eq!(
        err.line, 6,
        "unresolved interpolation error must report the outer line, got: {err:?}"
    );
}

// ---------------------------------------------------------------------------
// Undeclared type names (#859)
// ---------------------------------------------------------------------------

/// The exact repro from #859: a function whose parameter and return type are
/// the same name nobody declared. Nothing ever confronts the phantom type
/// with a real value, so before this check the program passed `aver check`,
/// `aver verify` and the proof export — and only the wasm-gc backend refused
/// it, with an unlocated "cannot lower type" long after the annotation.
#[test]
fn undeclared_type_name_is_reported_at_the_annotation() {
    let errs = errors(
        "fn twice(s: Wibble) -> Wibble\n\
         \x20   s\n",
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("Function 'twice', parameter 's'")
                && e.contains("Unknown type 'Wibble'")),
        "expected the parameter annotation to be reported, got: {errs:?}"
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("Function 'twice' return type")
                && e.contains("Unknown type 'Wibble'")),
        "expected the return annotation to be reported, got: {errs:?}"
    );
}

/// Every position that accepts a type annotation reports its own phantom
/// name: record fields, sum-variant fields, binding annotations, verify-law
/// `given` binders, and names nested inside a compound type.
#[test]
fn undeclared_type_name_is_reported_in_every_annotation_position() {
    let errs = errors(
        "record Holder\n\
         \x20   slot: Ghost\n\
         \n\
         type Choice\n\
         \x20   One(Phantom)\n\
         \n\
         fn nested(xs: List<Result<Apparition, String>>) -> Int\n\
         \x20   1\n\
         \n\
         fn binding(n: Int) -> Int\n\
         \x20   v: Wraith = n\n\
         \x20   n\n\
         \n\
         fn lawful(n: Int) -> Int\n\
         \x20   n\n\
         \n\
         verify lawful law identity\n\
         \x20   given k: Poltergeist = [1, 2]\n\
         \x20   lawful(k) => k\n",
    );
    for (position, name) in [
        ("Type 'Holder', field 'slot'", "Ghost"),
        ("Type 'Choice', variant 'One'", "Phantom"),
        ("Function 'nested', parameter 'xs'", "Apparition"),
        ("Binding 'v' annotation", "Wraith"),
        ("Verify law given 'k'", "Poltergeist"),
    ] {
        assert!(
            errs.iter()
                .any(|e| e.contains(position) && e.contains(&format!("Unknown type '{name}'"))),
            "expected {position} to report Unknown type '{name}', got: {errs:?}"
        );
    }
}

/// The names the compiler itself declares have no `type` declaration to
/// resolve against, yet a user may legitimately write every one of them in an
/// annotation: the host records effect signatures hand back, the opaque `Tcp`
/// handle, the Oracle nominals, and the embedded standard library's
/// refinements. `Result` and `Option` written without their parameters keep
/// working too. This is the guard against the undeclared-name rule turning
/// into a false positive on a real program.
#[test]
fn compiler_declared_type_names_stay_writable_in_annotations() {
    assert_no_errors(
        "fn takeResp(r: HttpResponse) -> Int\n\
         \x20   r.status\n\
         \n\
         fn takeReq(r: HttpRequest) -> String\n\
         \x20   r.path\n\
         \n\
         fn takeConn(c: Tcp.Connection) -> Tcp.Connection\n\
         \x20   c\n\
         \n\
         fn takeSize(s: Terminal.Size) -> Int\n\
         \x20   s.width\n\
         \n\
         fn takeTrace(t: Trace) -> List<EffectEvent>\n\
         \x20   t.events\n\
         \n\
         fn takeEvent(e: EffectEvent) -> String\n\
         \x20   e.method\n\
         \n\
         fn takePath(p: BranchPath) -> BranchPath\n\
         \x20   p\n\
         \n\
         fn takeBytes(b: Bytes) -> Digest32\n\
         \x20   Crypto.sha256(b)\n\
         \n\
         fn takeResult(r: Result) -> Result\n\
         \x20   r\n\
         \n\
         fn takeOption(o: Option) -> Option\n\
         \x20   o\n",
    );
}

/// A type declared later in the same file, and a type a dependency exposes —
/// bare or qualified — are declared names, so neither may be reported. The
/// dependency half is what separates "nothing declares this" from "the
/// declaration lives in another file".
#[test]
fn declared_type_names_are_never_reported_as_unknown() {
    let root = temp_module_root("declared_named_types");
    std::fs::write(
        root.join("Handle.av"),
        r#"module Handle
    exposes [Token, Colour]
    intent = "A dependency exposing two types."

record Token
    v: Int

type Colour
    Red
    Blue
"#,
    )
    .expect("write Handle.av failed");

    let src = r#"module Main
    depends [Handle]
    intent = "Local types declared after use, plus imported ones."

fn later(b: Box) -> Shade
    Shade.Dark

fn imported(t: Token) -> Handle.Colour
    Handle.Colour.Red

record Box
    v: Int

type Shade
    Dark
    Light
"#;
    let errs = errors_with_base(src, root.to_str().expect("utf-8 temp dir"));
    assert!(
        !errs.iter().any(|e| e.contains("Unknown type")),
        "no declared name may be reported as unknown, got: {errs:?}"
    );

    let _ = std::fs::remove_dir_all(&root);
}
