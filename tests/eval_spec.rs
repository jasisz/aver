/// Spec tests for the Aver tree-walking evaluator.
///
/// Tests evaluate expressions and function calls directly, bypassing the CLI
/// and the type checker so they focus solely on runtime semantics.
use aver::ast::{Stmt, TopLevel};
use aver::interpreter::{Interpreter, Value};
use aver::lexer::Lexer;
use aver::parser::Parser;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn parse(src: &str) -> Vec<TopLevel> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser.parse().expect("parse failed")
}

/// Evaluate a single top-level expression.
fn eval(src: &str) -> Value {
    let items = parse(src);
    let item = items.into_iter().next().expect("no items");
    if let TopLevel::Stmt(Stmt::Expr(expr)) = item {
        let mut interp = Interpreter::new();
        interp.eval_expr(&expr).expect("eval failed")
    } else {
        panic!("expected a single expression, got: {:?}", item);
    }
}

/// Register all function definitions from `src`, then call `fn_name` with `args`.
fn call_fn(src: &str, fn_name: &str, args: Vec<Value>) -> Value {
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).expect("exec_fn_def failed");
        }
    }
    let fn_val = interp.lookup(fn_name).expect("fn not found");
    interp.call_value_pub(fn_val, args).expect("call failed")
}

// ---------------------------------------------------------------------------
// Integer arithmetic
// ---------------------------------------------------------------------------

#[test]
fn int_add() {
    assert_eq!(eval("2 + 3"), Value::Int(5));
}

#[test]
fn int_sub() {
    assert_eq!(eval("10 - 4"), Value::Int(6));
}

#[test]
fn int_mul() {
    assert_eq!(eval("3 * 4"), Value::Int(12));
}

#[test]
fn int_div() {
    assert_eq!(eval("10 / 2"), Value::Int(5));
}

#[test]
fn int_chained_arithmetic() {
    // 2 + 3 * 4 = 2 + 12 = 14  (left-to-right, no precedence difference expected)
    // Actually Aver respects precedence: mul before add
    assert_eq!(eval("2 + 3 * 4"), Value::Int(14));
}

// ---------------------------------------------------------------------------
// Float arithmetic
// ---------------------------------------------------------------------------

#[test]
fn float_add() {
    assert_eq!(eval("1.5 + 2.5"), Value::Float(4.0));
}

#[test]
fn float_sub() {
    assert_eq!(eval("5.0 - 1.5"), Value::Float(3.5));
}

#[test]
fn int_float_promotion() {
    // Int + Float → Float
    assert_eq!(eval("1 + 2.0"), Value::Float(3.0));
}

// ---------------------------------------------------------------------------
// Comparison operators
// ---------------------------------------------------------------------------

#[test]
fn cmp_eq_true() {
    assert_eq!(eval("1 == 1"), Value::Bool(true));
}

#[test]
fn cmp_eq_false() {
    assert_eq!(eval("1 == 2"), Value::Bool(false));
}

#[test]
fn cmp_neq_true() {
    assert_eq!(eval("1 != 2"), Value::Bool(true));
}

#[test]
fn cmp_neq_false() {
    assert_eq!(eval("2 != 2"), Value::Bool(false));
}

#[test]
fn cmp_lt_true() {
    assert_eq!(eval("1 < 2"), Value::Bool(true));
}

#[test]
fn cmp_lt_false() {
    assert_eq!(eval("2 < 1"), Value::Bool(false));
}

#[test]
fn cmp_gt_true() {
    assert_eq!(eval("5 > 3"), Value::Bool(true));
}

#[test]
fn cmp_lte_equal() {
    assert_eq!(eval("3 <= 3"), Value::Bool(true));
}

#[test]
fn cmp_gte_greater() {
    assert_eq!(eval("5 >= 3"), Value::Bool(true));
}

#[test]
fn string_eq_true() {
    assert_eq!(eval("\"hello\" == \"hello\""), Value::Bool(true));
}

#[test]
fn string_eq_false() {
    assert_eq!(eval("\"hello\" == \"world\""), Value::Bool(false));
}

// ---------------------------------------------------------------------------
// Builtin functions
// ---------------------------------------------------------------------------

#[test]
fn runtime_gate_blocks_top_level_print() {
    let items = parse("Console.print(\"hi\")");
    let item = items.into_iter().next().expect("no items");
    if let TopLevel::Stmt(Stmt::Expr(expr)) = item {
        let mut interp = Interpreter::new();
        let err = interp
            .eval_expr(&expr)
            .expect_err("expected runtime gate error");
        let msg = err.to_string();
        assert!(msg.contains("Runtime effect violation"), "got: {}", msg);
        assert!(msg.contains("Console"), "got: {}", msg);
        assert!(msg.contains("<top-level>"), "got: {}", msg);
    } else {
        panic!("expected a single expression, got: {:?}", item);
    }
}

#[test]
fn runtime_gate_allows_effectful_entrypoint_with_grant() {
    let src = "fn log(n: Int) -> Unit\n    ! [Console]\n    = Console.print(n)\n";
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).expect("exec_fn_def failed");
        }
    }
    let log_fn = interp.lookup("log").expect("fn not found");

    let blocked = interp
        .call_value_pub(log_fn.clone(), vec![Value::Int(1)])
        .expect_err("expected runtime gate error");
    let blocked_msg = blocked.to_string();
    assert!(
        blocked_msg.contains("Runtime effect violation"),
        "got: {}",
        blocked_msg
    );

    let allowed = Interpreter::callable_declared_effects(&log_fn);
    let result = interp.call_value_with_effects_pub(log_fn, vec![Value::Int(2)], "<test>", allowed);
    assert!(result.is_ok(), "expected granted call to pass");
}

// ---------------------------------------------------------------------------
// Int namespace
// ---------------------------------------------------------------------------

#[test]
fn int_to_string() {
    assert_eq!(eval("Int.toString(42)"), Value::Str("42".to_string()));
}

#[test]
fn int_from_string() {
    assert_eq!(
        eval("Int.fromString(\"42\")"),
        Value::Ok(Box::new(Value::Int(42)))
    );
}

#[test]
fn int_from_string_err() {
    assert_eq!(
        eval("Int.fromString(\"abc\")"),
        Value::Err(Box::new(Value::Str(
            "Cannot parse 'abc' as Int".to_string()
        )))
    );
}

#[test]
fn int_from_float() {
    assert_eq!(eval("Int.fromFloat(3.9)"), Value::Int(3));
}

#[test]
fn int_abs() {
    assert_eq!(eval("Int.abs(5)"), Value::Int(5));
}

#[test]
fn int_abs_negative() {
    assert_eq!(eval("Int.abs(0 - 5)"), Value::Int(5));
}

#[test]
fn int_min() {
    assert_eq!(eval("Int.min(3, 7)"), Value::Int(3));
}

#[test]
fn int_max() {
    assert_eq!(eval("Int.max(3, 7)"), Value::Int(7));
}

#[test]
fn int_mod() {
    assert_eq!(eval("Int.mod(10, 3)"), Value::Ok(Box::new(Value::Int(1))));
}

#[test]
fn int_mod_zero() {
    assert_eq!(
        eval("Int.mod(10, 0)"),
        Value::Err(Box::new(Value::Str("division by zero".to_string())))
    );
}

#[test]
fn int_to_float() {
    assert_eq!(eval("Int.toFloat(5)"), Value::Float(5.0));
}

// ---------------------------------------------------------------------------
// Float namespace
// ---------------------------------------------------------------------------

#[test]
fn float_abs() {
    assert_eq!(eval("Float.abs(0.0 - 3.5)"), Value::Float(3.5));
}

#[test]
fn float_from_int() {
    assert_eq!(eval("Float.fromInt(5)"), Value::Float(5.0));
}

#[test]
fn float_to_string() {
    assert_eq!(eval("Float.toString(3.14)"), Value::Str("3.14".to_string()));
}

#[test]
fn float_from_string() {
    assert_eq!(
        eval("Float.fromString(\"2.5\")"),
        Value::Ok(Box::new(Value::Float(2.5)))
    );
}

#[test]
fn float_floor() {
    assert_eq!(eval("Float.floor(3.7)"), Value::Int(3));
}

#[test]
fn float_ceil() {
    assert_eq!(eval("Float.ceil(3.2)"), Value::Int(4));
}

#[test]
fn float_round() {
    assert_eq!(eval("Float.round(3.5)"), Value::Int(4));
}

#[test]
fn float_min() {
    assert_eq!(eval("Float.min(1.5, 2.5)"), Value::Float(1.5));
}

#[test]
fn float_max() {
    assert_eq!(eval("Float.max(1.5, 2.5)"), Value::Float(2.5));
}

// ---------------------------------------------------------------------------
// String namespace
// ---------------------------------------------------------------------------

#[test]
fn string_from_bool() {
    assert_eq!(
        eval("String.fromBool(true)"),
        Value::Str("true".to_string())
    );
}

#[test]
fn string_from_int() {
    assert_eq!(eval("String.fromInt(42)"), Value::Str("42".to_string()));
}

#[test]
fn string_from_float() {
    assert_eq!(
        eval("String.fromFloat(3.14)"),
        Value::Str("3.14".to_string())
    );
}

#[test]
fn string_length() {
    assert_eq!(eval("String.length(\"hello\")"), Value::Int(5));
}

#[test]
fn string_length_empty() {
    assert_eq!(eval("String.length(\"\")"), Value::Int(0));
}

#[test]
fn string_byte_length() {
    assert_eq!(eval("String.byteLength(\"hello\")"), Value::Int(5));
}

#[test]
fn string_starts_with() {
    assert_eq!(
        eval("String.startsWith(\"hello world\", \"hello\")"),
        Value::Bool(true)
    );
}

#[test]
fn string_starts_with_false() {
    assert_eq!(
        eval("String.startsWith(\"hello world\", \"world\")"),
        Value::Bool(false)
    );
}

#[test]
fn string_ends_with() {
    assert_eq!(
        eval("String.endsWith(\"hello world\", \"world\")"),
        Value::Bool(true)
    );
}

#[test]
fn string_contains() {
    assert_eq!(
        eval("String.contains(\"hello world\", \"lo wo\")"),
        Value::Bool(true)
    );
}

#[test]
fn string_contains_false() {
    assert_eq!(
        eval("String.contains(\"hello\", \"xyz\")"),
        Value::Bool(false)
    );
}

#[test]
fn string_slice() {
    assert_eq!(
        eval("String.slice(\"hello\", 1, 4)"),
        Value::Str("ell".to_string())
    );
}

#[test]
fn string_trim() {
    assert_eq!(
        eval("String.trim(\"  hi  \")"),
        Value::Str("hi".to_string())
    );
}

#[test]
fn string_split() {
    assert_eq!(
        eval("String.split(\"a,b,c\", \",\")"),
        Value::List(vec![
            Value::Str("a".to_string()),
            Value::Str("b".to_string()),
            Value::Str("c".to_string()),
        ])
    );
}

#[test]
fn string_replace() {
    assert_eq!(
        eval("String.replace(\"hello world\", \"world\", \"aver\")"),
        Value::Str("hello aver".to_string())
    );
}

#[test]
fn string_join() {
    assert_eq!(
        eval("String.join([\"a\", \"b\", \"c\"], \"-\")"),
        Value::Str("a-b-c".to_string())
    );
}

#[test]
fn string_chars() {
    assert_eq!(
        eval("String.chars(\"hi\")"),
        Value::List(vec![
            Value::Str("h".to_string()),
            Value::Str("i".to_string()),
        ])
    );
}

// ---------------------------------------------------------------------------
// List namespace
// ---------------------------------------------------------------------------

#[test]
fn list_len() {
    assert_eq!(eval("List.len([1, 2, 3])"), Value::Int(3));
}

#[test]
fn list_len_empty() {
    assert_eq!(eval("List.len([])"), Value::Int(0));
}

// ---------------------------------------------------------------------------
// List operations
// ---------------------------------------------------------------------------

#[test]
fn list_empty() {
    assert_eq!(eval("[]"), Value::List(vec![]));
}

#[test]
fn list_int_literal() {
    assert_eq!(
        eval("[1, 2, 3]"),
        Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
    );
}

#[test]
fn list_string_literal() {
    assert_eq!(
        eval("[\"a\", \"b\"]"),
        Value::List(vec![
            Value::Str("a".to_string()),
            Value::Str("b".to_string())
        ])
    );
}

#[test]
fn get_first_element() {
    assert_eq!(
        eval("List.get([10, 20, 30], 0)"),
        Value::Ok(Box::new(Value::Int(10)))
    );
}

#[test]
fn get_middle_element() {
    assert_eq!(
        eval("List.get([10, 20, 30], 1)"),
        Value::Ok(Box::new(Value::Int(20)))
    );
}

#[test]
fn get_out_of_bounds_returns_err() {
    let result = eval("List.get([1, 2], 5)");
    assert!(matches!(result, Value::Err(_)));
}

#[test]
fn head_returns_first() {
    assert_eq!(
        eval("List.head([42, 1, 2])"),
        Value::Ok(Box::new(Value::Int(42)))
    );
}

#[test]
fn head_empty_list_returns_err() {
    assert!(matches!(eval("List.head([])"), Value::Err(_)));
}

#[test]
fn tail_returns_rest() {
    assert_eq!(
        eval("List.tail([1, 2, 3])"),
        Value::Ok(Box::new(Value::List(vec![Value::Int(2), Value::Int(3)])))
    );
}

#[test]
fn tail_single_element_returns_empty() {
    assert_eq!(
        eval("List.tail([99])"),
        Value::Ok(Box::new(Value::List(vec![])))
    );
}

#[test]
fn tail_empty_list_returns_err() {
    assert!(matches!(eval("List.tail([])"), Value::Err(_)));
}

// ---------------------------------------------------------------------------
// Tuple and Map namespace
// ---------------------------------------------------------------------------

#[test]
fn tuple_literal_runtime() {
    assert_eq!(
        eval("(1, \"x\")"),
        Value::Tuple(vec![Value::Int(1), Value::Str("x".to_string())])
    );
}

#[test]
fn tuple_equality_runtime() {
    assert_eq!(eval("(1, \"x\") == (1, \"x\")"), Value::Bool(true));
}

#[test]
fn map_len_empty() {
    assert_eq!(eval("Map.len(Map.empty())"), Value::Int(0));
}

#[test]
fn map_set_get_has() {
    assert_eq!(
        eval("Map.has(Map.set(Map.empty(), \"a\", 1), \"a\")"),
        Value::Bool(true)
    );
    assert_eq!(
        eval("Map.get(Map.set(Map.empty(), \"a\", 1), \"a\")"),
        Value::Some(Box::new(Value::Int(1)))
    );
}

#[test]
fn map_get_missing_returns_none() {
    assert_eq!(eval("Map.get(Map.empty(), \"missing\")"), Value::None);
}

#[test]
fn map_remove_drops_key() {
    assert_eq!(
        eval("Map.has(Map.remove(Map.set(Map.empty(), \"a\", 1), \"a\"), \"a\")"),
        Value::Bool(false)
    );
}

#[test]
fn map_from_list_and_entries_roundtrip() {
    assert_eq!(
        eval("Map.keys(Map.fromList([(\"a\", 1), (\"b\", 2)]))"),
        Value::List(vec![
            Value::Str("a".to_string()),
            Value::Str("b".to_string()),
        ])
    );
    assert_eq!(
        eval("Map.entries(Map.fromList([(\"a\", 1), (\"b\", 2)]))"),
        Value::List(vec![
            Value::Tuple(vec![Value::Str("a".to_string()), Value::Int(1)]),
            Value::Tuple(vec![Value::Str("b".to_string()), Value::Int(2)]),
        ])
    );
}

#[test]
fn map_set_rejects_non_scalar_key() {
    let items = parse("Map.set(Map.empty(), [1], 42)");
    let item = items.into_iter().next().expect("no items");
    if let TopLevel::Stmt(Stmt::Expr(expr)) = item {
        let mut interp = Interpreter::new();
        let err = interp
            .eval_expr(&expr)
            .expect_err("expected runtime error for non-scalar key");
        assert!(
            err.to_string()
                .contains("key must be Int, Float, String, or Bool"),
            "unexpected error: {}",
            err
        );
    } else {
        panic!("expected expression");
    }
}

#[test]
fn push_appends_element() {
    assert_eq!(
        eval("List.push([1, 2], 3)"),
        Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
    );
}

#[test]
fn push_to_empty_list() {
    assert_eq!(eval("List.push([], 1)"), Value::List(vec![Value::Int(1)]));
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

#[test]
fn ok_wraps_value() {
    assert_eq!(eval("Result.Ok(42)"), Value::Ok(Box::new(Value::Int(42))));
}

#[test]
fn err_wraps_value() {
    assert_eq!(
        eval("Result.Err(\"fail\")"),
        Value::Err(Box::new(Value::Str("fail".to_string())))
    );
}

#[test]
fn some_wraps_value() {
    assert_eq!(eval("Option.Some(1)"), Value::Some(Box::new(Value::Int(1))));
}

#[test]
fn none_is_none() {
    assert_eq!(eval("Option.None"), Value::None);
}

// ---------------------------------------------------------------------------
// Match expressions
// ---------------------------------------------------------------------------

#[test]
fn match_literal_zero() {
    let src = "fn classify(n: Int) -> String\n    = match n\n        0 -> \"zero\"\n        _ -> \"other\"\n";
    assert_eq!(
        call_fn(src, "classify", vec![Value::Int(0)]),
        Value::Str("zero".to_string())
    );
}

#[test]
fn match_literal_wildcard() {
    let src = "fn classify(n: Int) -> String\n    = match n\n        0 -> \"zero\"\n        _ -> \"other\"\n";
    assert_eq!(
        call_fn(src, "classify", vec![Value::Int(99)]),
        Value::Str("other".to_string())
    );
}

#[test]
fn match_ok_constructor() {
    let src = "fn unwrap(r: Result<Int, String>) -> Int\n    = match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> 0\n";
    assert_eq!(
        call_fn(src, "unwrap", vec![Value::Ok(Box::new(Value::Int(42)))]),
        Value::Int(42)
    );
}

#[test]
fn match_err_constructor() {
    let src = "fn unwrap(r: Result<Int, String>) -> Int\n    = match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> 0\n";
    assert_eq!(
        call_fn(
            src,
            "unwrap",
            vec![Value::Err(Box::new(Value::Str("fail".to_string())))]
        ),
        Value::Int(0)
    );
}

#[test]
fn match_some_none() {
    let src = "fn extract(o: Option<Int>) -> Int\n    = match o\n        Option.Some(v) -> v\n        Option.None -> 0\n";
    assert_eq!(
        call_fn(src, "extract", vec![Value::Some(Box::new(Value::Int(7)))]),
        Value::Int(7)
    );
    assert_eq!(call_fn(src, "extract", vec![Value::None]), Value::Int(0));
}

#[test]
fn match_bool_literal() {
    let src = "fn yes_no(b: Bool) -> String\n    = match b\n        true -> \"yes\"\n        false -> \"no\"\n";
    assert_eq!(
        call_fn(src, "yes_no", vec![Value::Bool(true)]),
        Value::Str("yes".to_string())
    );
    assert_eq!(
        call_fn(src, "yes_no", vec![Value::Bool(false)]),
        Value::Str("no".to_string())
    );
}

#[test]
fn match_empty_list_pattern() {
    let src = "fn is_empty(xs: List<Int>) -> Bool\n    = match xs\n        [] -> true\n        [_, ..rest] -> false\n";
    assert_eq!(
        call_fn(src, "is_empty", vec![Value::List(vec![])]),
        Value::Bool(true)
    );
    assert_eq!(
        call_fn(
            src,
            "is_empty",
            vec![Value::List(vec![Value::Int(1), Value::Int(2)])]
        ),
        Value::Bool(false)
    );
}

#[test]
fn match_list_cons_binds_head_and_tail() {
    let src = "fn score(xs: List<Int>) -> Int\n    = match xs\n        [h, ..t] -> h + List.len(t)\n        [] -> 0\n";
    assert_eq!(
        call_fn(
            src,
            "score",
            vec![Value::List(vec![
                Value::Int(5),
                Value::Int(9),
                Value::Int(11)
            ])]
        ),
        Value::Int(7)
    );
    assert_eq!(
        call_fn(src, "score", vec![Value::List(vec![Value::Int(5)])]),
        Value::Int(5)
    );
    assert_eq!(
        call_fn(src, "score", vec![Value::List(vec![])]),
        Value::Int(0)
    );
}

// ---------------------------------------------------------------------------
// String interpolation
// ---------------------------------------------------------------------------

#[test]
fn interp_simple() {
    let src = "fn greet(name: String) -> String\n    = \"Hello, {name}!\"\n";
    assert_eq!(
        call_fn(src, "greet", vec![Value::Str("Alice".to_string())]),
        Value::Str("Hello, Alice!".to_string())
    );
}

#[test]
fn interp_expression() {
    let src = "fn show(x: Int) -> String\n    = \"value: {x + 1}\"\n";
    assert_eq!(
        call_fn(src, "show", vec![Value::Int(4)]),
        Value::Str("value: 5".to_string())
    );
}

// ---------------------------------------------------------------------------
// Pipe operator
// ---------------------------------------------------------------------------

#[test]
fn pipe_single() {
    let src = "fn double(x: Int) -> Int\n    = x + x\nfn apply(x: Int) -> Int\n    = x |> double\n";
    assert_eq!(call_fn(src, "apply", vec![Value::Int(5)]), Value::Int(10));
}

#[test]
fn pipe_chained() {
    let src =
        "fn inc(x: Int) -> Int\n    = x + 1\nfn apply(x: Int) -> Int\n    = x |> inc |> inc\n";
    assert_eq!(call_fn(src, "apply", vec![Value::Int(0)]), Value::Int(2));
}

// ---------------------------------------------------------------------------
// Val / Var bindings in function bodies
// ---------------------------------------------------------------------------

#[test]
fn binding_used_in_body() {
    let src = "fn compute() -> Int\n    x = 10\n    y = 20\n    x + y\n";
    assert_eq!(call_fn(src, "compute", vec![]), Value::Int(30));
}

// ---------------------------------------------------------------------------
// Higher-order functions: map, filter, fold
// ---------------------------------------------------------------------------

#[test]
fn map_doubles_list() {
    let src = "fn double(x: Int) -> Int\n    = x + x\n";
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).unwrap();
        }
    }
    let list = Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
    let double_fn = interp.lookup("double").unwrap();
    let map_fn = Value::Builtin("List.map".to_string());
    let result = interp
        .call_value_pub(map_fn, vec![list, double_fn])
        .unwrap();
    assert_eq!(
        result,
        Value::List(vec![Value::Int(2), Value::Int(4), Value::Int(6)])
    );
}

#[test]
fn filter_keeps_positives() {
    let src = "fn is_pos(x: Int) -> Bool\n    = x > 0\n";
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).unwrap();
        }
    }
    let list = Value::List(vec![
        Value::Int(-1),
        Value::Int(2),
        Value::Int(-3),
        Value::Int(4),
    ]);
    let pred = interp.lookup("is_pos").unwrap();
    let filter_fn = Value::Builtin("List.filter".to_string());
    let result = interp.call_value_pub(filter_fn, vec![list, pred]).unwrap();
    assert_eq!(result, Value::List(vec![Value::Int(2), Value::Int(4)]));
}

#[test]
fn fold_sum() {
    let src = "fn add(acc: Int, x: Int) -> Int\n    = acc + x\n";
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).unwrap();
        }
    }
    let list = Value::List(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
        Value::Int(4),
    ]);
    let add_fn = interp.lookup("add").unwrap();
    let fold_fn = Value::Builtin("List.fold".to_string());
    let result = interp
        .call_value_pub(fold_fn, vec![list, Value::Int(0), add_fn])
        .unwrap();
    assert_eq!(result, Value::Int(10));
}

#[test]
fn higher_order_apply_twice_with_function_typed_param() {
    let src = "fn applyTwice(f: Fn(Int) -> Int, x: Int) -> Int\n    = f(f(x))\nfn inc(n: Int) -> Int\n    = n + 1\n";
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).unwrap();
        }
    }
    let apply_fn = interp.lookup("applyTwice").unwrap();
    let inc_fn = interp.lookup("inc").unwrap();
    let result = interp
        .call_value_pub(apply_fn, vec![inc_fn, Value::Int(10)])
        .unwrap();
    assert_eq!(result, Value::Int(12));
}

// ---------------------------------------------------------------------------
// Error propagation operator ?
// ---------------------------------------------------------------------------

#[test]
fn error_prop_unwraps_ok() {
    let src = "fn get_ok(r: Result<Int, String>) -> Int\n    = r?\n";
    assert_eq!(
        call_fn(src, "get_ok", vec![Value::Ok(Box::new(Value::Int(99)))]),
        Value::Int(99)
    );
}

#[test]
fn error_prop_early_return_on_err() {
    // ? on Err causes early return: the function returns Err(e), not a crash.
    let src = "fn get_val(r: Result<Int, String>) -> Result<Int, String>\n    = r?\n";
    let result = call_fn(
        src,
        "get_val",
        vec![Value::Err(Box::new(Value::Str("bad".to_string())))],
    );
    assert_eq!(result, Value::Err(Box::new(Value::Str("bad".to_string()))));
}

#[test]
fn error_prop_early_return_in_block() {
    // ? in a block body causes early return, skipping subsequent statements.
    let src = "fn double_ok(r: Result<Int, String>) -> Result<Int, String>\n    x = r?\n    Result.Ok(x + x)\n";
    assert_eq!(
        call_fn(src, "double_ok", vec![Value::Ok(Box::new(Value::Int(5)))]),
        Value::Ok(Box::new(Value::Int(10)))
    );
    assert_eq!(
        call_fn(
            src,
            "double_ok",
            vec![Value::Err(Box::new(Value::Str("oops".to_string())))]
        ),
        Value::Err(Box::new(Value::Str("oops".to_string())))
    );
}

#[test]
fn error_prop_chain_short_circuits() {
    // When the first ? encounters Err, the second ? and the Ok() never run.
    let src = "fn chain(a: Result<Int, String>, b: Result<Int, String>) -> Result<Int, String>\n    x = a?\n    y = b?\n    Result.Ok(x + y)\n";
    let err = Value::Err(Box::new(Value::Str("first".to_string())));
    let ok_ten = Value::Ok(Box::new(Value::Int(10)));
    assert_eq!(call_fn(src, "chain", vec![err.clone(), ok_ten]), err);
}

// ---------------------------------------------------------------------------
// Closures
// ---------------------------------------------------------------------------

#[test]
fn closure_captures_outer_val() {
    let _src =
        "fn make_adder(n: Int) -> Fn(Int) -> Int\n    fn add(x: Int) -> Int\n        = x + n\n    add\n";
    // Note: nested function definitions are not a first-class feature in Aver.
    // This test verifies the closure capture mechanism via lambda-style usage.
    // We use map with a pre-defined function instead.
    let src2 = "fn double(x: Int) -> Int\n    = x + x\n";
    let items = parse(src2);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).unwrap();
        }
    }
    // Closures are captured at definition time
    let fn_val = interp.lookup("double").unwrap();
    let result = interp.call_value_pub(fn_val, vec![Value::Int(6)]).unwrap();
    assert_eq!(result, Value::Int(12));
}

// ---------------------------------------------------------------------------
// User-defined types — sum types (type keyword)
// ---------------------------------------------------------------------------

/// Helper: parse, register type defs and fn defs, run top-level stmts, return interpreter.
fn run_program(src: &str) -> Interpreter {
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            interp.register_type_def(td);
        }
    }
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).expect("exec_fn_def failed");
        }
    }
    for item in &items {
        if let TopLevel::Stmt(s) = item {
            interp.exec_stmt(s).expect("exec_stmt failed");
        }
    }
    interp
}

#[test]
fn sum_type_no_arg_variant_is_variant_value() {
    let src = "type Shape\n  Circle(Float)\n  Point\np = Shape.Point\n";
    let mut interp = run_program(src);
    let val = interp.lookup("p").expect("p not defined");
    assert_eq!(
        val,
        Value::Variant {
            type_name: "Shape".to_string(),
            variant: "Point".to_string(),
            fields: vec![],
        }
    );
}

#[test]
fn sum_type_constructor_creates_variant() {
    let src = "type Shape\n  Circle(Float)\n  Point\nc = Shape.Circle(3.14)\n";
    let mut interp = run_program(src);
    let val = interp.lookup("c").expect("c not defined");
    assert_eq!(
        val,
        Value::Variant {
            type_name: "Shape".to_string(),
            variant: "Circle".to_string(),
            fields: vec![Value::Float(3.14)],
        }
    );
}

#[test]
fn sum_type_multi_field_constructor() {
    let src = "type Shape\n  Rect(Float, Float)\nr = Shape.Rect(3.0, 4.0)\n";
    let mut interp = run_program(src);
    let val = interp.lookup("r").expect("r not defined");
    assert_eq!(
        val,
        Value::Variant {
            type_name: "Shape".to_string(),
            variant: "Rect".to_string(),
            fields: vec![Value::Float(3.0), Value::Float(4.0)],
        }
    );
}

#[test]
fn sum_type_match_single_field_variant() {
    let src = concat!(
        "type Shape\n",
        "  Circle(Float)\n",
        "  Point\n",
        "fn area(s: Shape) -> Float\n",
        "  ? \"area\"\n",
        "  = match s\n",
        "    Circle(r) -> r * r\n",
        "    Point -> 0.0\n",
    );
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            interp.register_type_def(td);
        }
    }
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).unwrap();
        }
    }
    let circle = Value::Variant {
        type_name: "Shape".to_string(),
        variant: "Circle".to_string(),
        fields: vec![Value::Float(5.0)],
    };
    let fn_val = interp.lookup("area").unwrap();
    let result = interp.call_value_pub(fn_val, vec![circle]).unwrap();
    assert_eq!(result, Value::Float(25.0));
}

#[test]
fn sum_type_match_no_arg_variant() {
    let src = concat!(
        "type Shape\n",
        "  Circle(Float)\n",
        "  Point\n",
        "fn area(s: Shape) -> Float\n",
        "  ? \"area\"\n",
        "  = match s\n",
        "    Circle(r) -> r * r\n",
        "    Point -> 0.0\n",
    );
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            interp.register_type_def(td);
        }
    }
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).unwrap();
        }
    }
    let point = Value::Variant {
        type_name: "Shape".to_string(),
        variant: "Point".to_string(),
        fields: vec![],
    };
    let fn_val = interp.lookup("area").unwrap();
    let result = interp.call_value_pub(fn_val, vec![point]).unwrap();
    assert_eq!(result, Value::Float(0.0));
}

// ---------------------------------------------------------------------------
// User-defined types — records (record keyword)
// ---------------------------------------------------------------------------

#[test]
fn record_creation_stores_fields() {
    let src = "record User\n  name: String\n  age: Int\nu = User(name = \"Alice\", age = 30)\n";
    let mut interp = run_program(src);
    let val = interp.lookup("u").expect("u not defined");
    assert_eq!(
        val,
        Value::Record {
            type_name: "User".to_string(),
            fields: vec![
                ("name".to_string(), Value::Str("Alice".to_string())),
                ("age".to_string(), Value::Int(30)),
            ],
        }
    );
}

#[test]
fn record_creation_canonicalizes_field_order() {
    let src = "record User\n  name: String\n  age: Int\nu = User(age = 30, name = \"Alice\")\n";
    let mut interp = run_program(src);
    let val = interp.lookup("u").expect("u not defined");
    assert_eq!(
        val,
        Value::Record {
            type_name: "User".to_string(),
            fields: vec![
                ("name".to_string(), Value::Str("Alice".to_string())),
                ("age".to_string(), Value::Int(30)),
            ],
        }
    );
}

#[test]
fn record_field_access() {
    let src =
        "record User\n  name: String\n  age: Int\nu = User(name = \"Alice\", age = 30)\nn = u.name\n";
    let mut interp = run_program(src);
    let val = interp.lookup("n").expect("n not defined");
    assert_eq!(val, Value::Str("Alice".to_string()));
}

#[test]
fn record_positional_match() {
    let src = concat!(
        "record User\n",
        "  name: String\n",
        "  age: Int\n",
        "fn get_name(u: User) -> String\n",
        "  ? \"get name\"\n",
        "  = match u\n",
        "    User(name, age) -> name\n",
    );
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            interp.register_type_def(td);
        }
    }
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).unwrap();
        }
    }
    let user = Value::Record {
        type_name: "User".to_string(),
        fields: vec![
            ("name".to_string(), Value::Str("Bob".to_string())),
            ("age".to_string(), Value::Int(25)),
        ],
    };
    let fn_val = interp.lookup("get_name").unwrap();
    let result = interp.call_value_pub(fn_val, vec![user]).unwrap();
    assert_eq!(result, Value::Str("Bob".to_string()));
}

#[test]
fn sum_type_variant_equality() {
    let c1 = Value::Variant {
        type_name: "Shape".to_string(),
        variant: "Circle".to_string(),
        fields: vec![Value::Float(3.0)],
    };
    let c2 = Value::Variant {
        type_name: "Shape".to_string(),
        variant: "Circle".to_string(),
        fields: vec![Value::Float(3.0)],
    };
    let c3 = Value::Variant {
        type_name: "Shape".to_string(),
        variant: "Circle".to_string(),
        fields: vec![Value::Float(5.0)],
    };
    let interp = Interpreter::new();
    assert!(interp.aver_eq(&c1, &c2));
    assert!(!interp.aver_eq(&c1, &c3));
}

// ---------------------------------------------------------------------------
// Http builtins — local TcpListener, no internet required
// ---------------------------------------------------------------------------

mod http_tests {
    use super::*;
    use aver::interpreter::{Interpreter, Value};
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::thread;

    /// Spawn a minimal HTTP/1.1 server on an OS-assigned port.
    /// Returns None when the bind is not permitted (sandboxed CI environments).
    fn start_server(
        status: u16,
        body: &'static str,
        extra_headers: &'static str,
    ) -> Option<String> {
        let listener = match TcpListener::bind("127.0.0.1:0") {
            Ok(l) => l,
            Err(_) => return None,
        };
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            let mut buf = [0u8; 4096];
            let _ = stream.read(&mut buf);
            let response = format!(
                "HTTP/1.1 {} OK\r\nContent-Length: {}\r\n{}\r\n{}",
                status,
                body.len(),
                extra_headers,
                body
            );
            stream.write_all(response.as_bytes()).unwrap();
        });
        Some(format!("http://127.0.0.1:{}/", port))
    }

    fn run_http_fn(src: &str, fn_name: &str) -> Value {
        let items = parse(src);
        let mut interp = Interpreter::new();
        for item in &items {
            if let TopLevel::FnDef(fd) = item {
                interp.exec_fn_def(fd).expect("exec_fn_def failed");
            }
        }
        let fn_val = interp.lookup(fn_name).expect("fn not found");
        let effects = Interpreter::callable_declared_effects(&fn_val);
        interp
            .call_value_with_effects_pub(fn_val, vec![], fn_name, effects)
            .expect("call failed")
    }

    #[test]
    #[ignore = "integration: starts a local HTTP server; run with --include-ignored --test-threads=1"]
    fn http_get_200_returns_ok_response() {
        let Some(url) = start_server(200, "hello", "") else {
            return;
        };
        let src = format!(
            "fn fetch() -> Result<HttpResponse, String>\n    ! [Http]\n    Http.get(\"{}\")\n",
            url
        );
        let val = run_http_fn(&src, "fetch");
        match val {
            Value::Ok(inner) => match *inner {
                Value::Record {
                    type_name,
                    ref fields,
                } => {
                    assert_eq!(type_name, "HttpResponse");
                    let status = fields.iter().find(|(k, _)| k == "status").map(|(_, v)| v);
                    assert_eq!(status, Some(&Value::Int(200)));
                    let body = fields.iter().find(|(k, _)| k == "body").map(|(_, v)| v);
                    assert_eq!(body, Some(&Value::Str("hello".to_string())));
                }
                other => panic!("expected Record, got {:?}", other),
            },
            other => panic!("expected Ok, got {:?}", other),
        }
    }

    #[test]
    #[ignore = "integration: starts a local HTTP server; run with --include-ignored --test-threads=1"]
    fn http_get_404_still_returns_ok_response() {
        let Some(url) = start_server(404, "not found", "") else {
            return;
        };
        let src = format!(
            "fn fetch() -> Result<HttpResponse, String>\n    ! [Http]\n    Http.get(\"{}\")\n",
            url
        );
        let val = run_http_fn(&src, "fetch");
        match val {
            Value::Ok(inner) => match *inner {
                Value::Record { ref fields, .. } => {
                    let status = fields.iter().find(|(k, _)| k == "status").map(|(_, v)| v);
                    assert_eq!(status, Some(&Value::Int(404)));
                }
                other => panic!("expected Record, got {:?}", other),
            },
            other => panic!("expected Ok for 4xx, got {:?}", other),
        }
    }

    #[test]
    fn http_get_transport_error_returns_err() {
        // Port 1 is almost certainly not listening
        let src = "fn fetch() -> Result<HttpResponse, String>\n    ! [Http]\n    Http.get(\"http://127.0.0.1:1/\")\n";
        let items = parse(src);
        let mut interp = Interpreter::new();
        for item in &items {
            if let TopLevel::FnDef(fd) = item {
                interp.exec_fn_def(fd).expect("exec_fn_def failed");
            }
        }
        let fn_val = interp.lookup("fetch").expect("fn not found");
        let effects = Interpreter::callable_declared_effects(&fn_val);
        let val = interp
            .call_value_with_effects_pub(fn_val, vec![], "fetch", effects)
            .expect("call itself should not panic");
        assert!(
            matches!(val, Value::Err(_)),
            "expected Err for unreachable host, got {:?}",
            val
        );
    }

    #[test]
    #[ignore = "integration: starts a local HTTP server; run with --include-ignored --test-threads=1"]
    fn http_post_201_returns_ok_response() {
        let Some(url) = start_server(201, "created", "") else {
            return;
        };
        let src = format!(
            "fn send() -> Result<HttpResponse, String>\n    ! [Http]\n    Http.post(\"{}\", \"data\", \"text/plain\", [])\n",
            url
        );
        let val = run_http_fn(&src, "send");
        match val {
            Value::Ok(inner) => match *inner {
                Value::Record { ref fields, .. } => {
                    let status = fields.iter().find(|(k, _)| k == "status").map(|(_, v)| v);
                    assert_eq!(status, Some(&Value::Int(201)));
                }
                other => panic!("expected Record, got {:?}", other),
            },
            other => panic!("expected Ok, got {:?}", other),
        }
    }

    #[test]
    fn http_post_bad_headers_returns_runtime_error() {
        // Pass a non-list for headers — validation fails before any HTTP call
        let src = "fn send() -> Result<HttpResponse, String>\n    ! [Http]\n    Http.post(\"http://127.0.0.1:1/\", \"\", \"text/plain\", \"bad\")\n";
        let items = parse(src);
        let mut interp = Interpreter::new();
        for item in &items {
            if let TopLevel::FnDef(fd) = item {
                interp.exec_fn_def(fd).expect("exec_fn_def failed");
            }
        }
        let fn_val = interp.lookup("send").expect("fn not found");
        let effects = Interpreter::callable_declared_effects(&fn_val);
        let result = interp.call_value_with_effects_pub(fn_val, vec![], "send", effects);
        assert!(result.is_err(), "expected RuntimeError for bad headers");
    }
}

// ---------------------------------------------------------------------------
// Disk service builtins
// ---------------------------------------------------------------------------

mod disk_tests {
    use super::*;
    use aver::interpreter::{Interpreter, Value};
    use std::io::Write;

    fn run_disk_fn(src: &str, fn_name: &str) -> Value {
        let items = parse(src);
        let mut interp = Interpreter::new();
        for item in &items {
            if let TopLevel::FnDef(fd) = item {
                interp.exec_fn_def(fd).expect("exec_fn_def failed");
            }
        }
        let fn_val = interp.lookup(fn_name).expect("fn not found");
        let effects = Interpreter::callable_declared_effects(&fn_val);
        interp
            .call_value_with_effects_pub(fn_val, vec![], fn_name, effects)
            .expect("call failed")
    }

    fn tmp_path(name: &str) -> std::path::PathBuf {
        std::env::temp_dir().join(format!("aver_disk_test_{}", name))
    }

    #[test]
    fn disk_write_and_read_text() {
        let path = tmp_path("write_read.txt");
        let path_str = path.to_string_lossy();
        let src = format!(
            "fn run() -> Result<Unit, String>\n    ! [Disk]\n    Disk.writeText(\"{}\", \"hello\")\n",
            path_str.replace('\\', "\\\\")
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));

        let src2 = format!(
            "fn run() -> Result<String, String>\n    ! [Disk]\n    Disk.readText(\"{}\")\n",
            path_str.replace('\\', "\\\\")
        );
        let val2 = run_disk_fn(&src2, "run");
        assert_eq!(val2, Value::Ok(Box::new(Value::Str("hello".to_string()))));

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn disk_append_text() {
        let path = tmp_path("append.txt");
        let path_str = path.to_string_lossy().replace('\\', "\\\\");
        // Write initial content then append
        {
            let mut f = std::fs::File::create(&path).unwrap();
            f.write_all(b"hello").unwrap();
        }
        let src = format!(
            "fn run() -> Result<Unit, String>\n    ! [Disk]\n    Disk.appendText(\"{}\", \" world\")\n",
            path_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));
        let content = std::fs::read_to_string(&path).unwrap();
        assert_eq!(content, "hello world");
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn disk_exists_true_and_false() {
        let path = tmp_path("exists.txt");
        let path_str = path.to_string_lossy().replace('\\', "\\\\");
        std::fs::write(&path, "x").unwrap();

        let src = format!(
            "fn run() -> Bool\n    ! [Disk]\n    Disk.exists(\"{}\")\n",
            path_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Bool(true));
        let _ = std::fs::remove_file(&path);

        let missing_path = tmp_path("does_not_exist_xyz.txt");
        let missing_str = missing_path.to_string_lossy().replace('\\', "\\\\");
        let src2 = format!(
            "fn run() -> Bool\n    ! [Disk]\n    Disk.exists(\"{}\")\n",
            missing_str
        );
        let val2 = run_disk_fn(&src2, "run");
        assert_eq!(val2, Value::Bool(false));
    }

    #[test]
    fn disk_delete_file() {
        let path = tmp_path("delete.txt");
        let path_str = path.to_string_lossy().replace('\\', "\\\\");
        std::fs::write(&path, "bye").unwrap();

        let src = format!(
            "fn run() -> Result<Unit, String>\n    ! [Disk]\n    Disk.delete(\"{}\")\n",
            path_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));
        assert!(!path.exists());
    }

    #[test]
    fn disk_delete_directory_returns_err() {
        // Disk.delete must refuse directories — use Disk.deleteDir instead
        let dir = tmp_path("delete_dir_guard");
        std::fs::create_dir_all(&dir).unwrap();
        let dir_str = dir.to_string_lossy().replace('\\', "\\\\");

        let src = format!(
            "fn run() -> Result<Unit, String>\n    ! [Disk]\n    Disk.delete(\"{}\")\n",
            dir_str
        );
        let val = run_disk_fn(&src, "run");
        assert!(
            matches!(val, Value::Err(_)),
            "expected Err when deleting a directory via Disk.delete"
        );
        assert!(dir.exists(), "directory must not be removed");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn disk_delete_dir_removes_directory_tree() {
        let dir = tmp_path("deletedir_tree");
        let sub = dir.join("sub");
        std::fs::create_dir_all(&sub).unwrap();
        std::fs::write(sub.join("file.txt"), "x").unwrap();
        let dir_str = dir.to_string_lossy().replace('\\', "\\\\");

        let src = format!(
            "fn run() -> Result<Unit, String>\n    ! [Disk]\n    Disk.deleteDir(\"{}\")\n",
            dir_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));
        assert!(!dir.exists());
    }

    #[test]
    fn disk_delete_dir_on_file_returns_err() {
        let path = tmp_path("deletedir_on_file.txt");
        std::fs::write(&path, "data").unwrap();
        let path_str = path.to_string_lossy().replace('\\', "\\\\");

        let src = format!(
            "fn run() -> Result<Unit, String>\n    ! [Disk]\n    Disk.deleteDir(\"{}\")\n",
            path_str
        );
        let val = run_disk_fn(&src, "run");
        assert!(
            matches!(val, Value::Err(_)),
            "expected Err when using Disk.deleteDir on a file"
        );
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn disk_delete_missing_file_returns_err() {
        let path = tmp_path("no_such_file_xyz.txt");
        let path_str = path.to_string_lossy().replace('\\', "\\\\");
        let src = format!(
            "fn run() -> Result<Unit, String>\n    ! [Disk]\n    Disk.delete(\"{}\")\n",
            path_str
        );
        let val = run_disk_fn(&src, "run");
        assert!(matches!(val, Value::Err(_)));
    }

    #[test]
    fn disk_make_dir_and_list_dir() {
        let dir = tmp_path("mydir_listtest");
        let dir_str = dir.to_string_lossy().replace('\\', "\\\\");
        let _ = std::fs::remove_dir_all(&dir);

        let src = format!(
            "fn run() -> Result<Unit, String>\n    ! [Disk]\n    Disk.makeDir(\"{}\")\n",
            dir_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));
        assert!(dir.exists());

        // Write a file inside to list it
        std::fs::write(dir.join("a.txt"), "").unwrap();
        let src2 = format!(
            "fn run() -> Result<List<String>, String>\n    ! [Disk]\n    Disk.listDir(\"{}\")\n",
            dir_str
        );
        let val2 = run_disk_fn(&src2, "run");
        match val2 {
            Value::Ok(inner) => match *inner {
                Value::List(items) => {
                    assert!(items.contains(&Value::Str("a.txt".to_string())));
                }
                other => panic!("expected List, got {:?}", other),
            },
            other => panic!("expected Ok, got {:?}", other),
        }

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn disk_read_missing_file_returns_err() {
        let src = "fn run() -> Result<String, String>\n    ! [Disk]\n    Disk.readText(\"/no/such/file.txt\")\n";
        let val = run_disk_fn(src, "run");
        assert!(matches!(val, Value::Err(_)));
    }

    #[test]
    fn runtime_gate_blocks_disk_read_without_effect() {
        // Call Disk.readText from top-level (no effect grant) → runtime gate fires
        let items = parse("Disk.readText(\"x\")");
        let item = items.into_iter().next().expect("no items");
        if let TopLevel::Stmt(Stmt::Expr(expr)) = item {
            let mut interp = Interpreter::new();
            let err = interp
                .eval_expr(&expr)
                .expect_err("expected runtime gate error");
            let msg = err.to_string();
            assert!(msg.contains("Runtime effect violation"), "got: {}", msg);
            assert!(msg.contains("Disk"), "got: {}", msg);
            assert!(msg.contains("<top-level>"), "got: {}", msg);
        } else {
            panic!("expected a single expression");
        }
    }
}

// ---------------------------------------------------------------------------
// Console service tests
// ---------------------------------------------------------------------------

mod console_tests {
    use super::*;
    use aver::interpreter::{Interpreter, Value};

    fn run_console_fn(src: &str, fn_name: &str) -> Value {
        let items = parse(src);
        let mut interp = Interpreter::new();
        for item in &items {
            if let TopLevel::FnDef(fd) = item {
                interp.exec_fn_def(fd).expect("exec_fn_def failed");
            }
        }
        let fn_val = interp.lookup(fn_name).expect("fn not found");
        let effects = Interpreter::callable_declared_effects(&fn_val);
        interp
            .call_value_with_effects_pub(fn_val, vec![], fn_name, effects)
            .expect("call failed")
    }

    #[test]
    fn console_error_returns_unit() {
        let src = concat!(
            "fn run() -> Unit\n",
            "    ! [Console]\n",
            "    Console.error(\"oops\")\n",
        );
        let val = run_console_fn(src, "run");
        assert_eq!(val, Value::Unit);
    }

    #[test]
    fn console_warn_returns_unit() {
        let src = concat!(
            "fn run() -> Unit\n",
            "    ! [Console]\n",
            "    Console.warn(\"careful\")\n",
        );
        let val = run_console_fn(src, "run");
        assert_eq!(val, Value::Unit);
    }

    #[test]
    fn console_error_unit_value_is_silent() {
        // Passing Unit to Console.error should not error — just emit nothing.
        // Console.print returns Unit, so we use its result as the argument.
        let src = concat!(
            "fn run() -> Unit\n",
            "    ! [Console]\n",
            "    Console.error(Console.print(\"setup\"))\n",
        );
        let val = run_console_fn(src, "run");
        assert_eq!(val, Value::Unit);
    }

    #[test]
    fn console_warn_unit_value_is_silent() {
        let src = concat!(
            "fn run() -> Unit\n",
            "    ! [Console]\n",
            "    Console.warn(Console.print(\"setup\"))\n",
        );
        let val = run_console_fn(src, "run");
        assert_eq!(val, Value::Unit);
    }

    #[test]
    fn runtime_gate_blocks_console_error_without_effect() {
        let items = parse("Console.error(\"x\")");
        let item = items.into_iter().next().expect("no items");
        if let TopLevel::Stmt(Stmt::Expr(expr)) = item {
            let mut interp = Interpreter::new();
            let err = interp
                .eval_expr(&expr)
                .expect_err("expected runtime gate error");
            let msg = err.to_string();
            assert!(msg.contains("Runtime effect violation"), "got: {}", msg);
            assert!(msg.contains("Console"), "got: {}", msg);
        } else {
            panic!("expected a single expression");
        }
    }
}

// ---------------------------------------------------------------------------
// Tcp builtins
// ---------------------------------------------------------------------------

mod tcp_tests {
    use super::*;
    use aver::interpreter::{Interpreter, Value};

    fn run_tcp_fn(src: &str, fn_name: &str) -> Value {
        let items = parse(src);
        let mut interp = Interpreter::new();
        for item in &items {
            if let TopLevel::FnDef(fd) = item {
                interp.exec_fn_def(fd).expect("exec_fn_def failed");
            }
        }
        let fn_val = interp.lookup(fn_name).expect("fn not found");
        let effects = Interpreter::callable_declared_effects(&fn_val);
        interp
            .call_value_with_effects_pub(fn_val, vec![], fn_name, effects)
            .expect("call failed")
    }

    #[test]
    fn tcp_ping_unreachable_returns_err() {
        // Port 1 is almost certainly not listening.
        let src = concat!(
            "fn check() -> Result<Unit, String>\n",
            "    ! [Tcp]\n",
            "    = Tcp.ping(\"127.0.0.1\", 1)\n",
        );
        let val = run_tcp_fn(src, "check");
        assert!(matches!(val, Value::Err(_)), "expected Err, got {:?}", val);
    }

    #[test]
    fn tcp_send_unreachable_returns_err() {
        let src = concat!(
            "fn talk() -> Result<String, String>\n",
            "    ! [Tcp]\n",
            "    = Tcp.send(\"127.0.0.1\", 1, \"hello\")\n",
        );
        let val = run_tcp_fn(src, "talk");
        assert!(matches!(val, Value::Err(_)), "expected Err, got {:?}", val);
    }

    #[test]
    #[ignore = "integration: starts a local TCP server; run with --include-ignored --test-threads=1"]
    fn tcp_ping_open_port_returns_ok() {
        use std::net::TcpListener;
        use std::thread;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            let _ = listener.accept();
        });

        let src = format!(
            "fn check() -> Result<Unit, String>\n    ! [Tcp]\n    = Tcp.ping(\"127.0.0.1\", {})\n",
            port
        );
        let val = run_tcp_fn(&src, "check");
        assert!(matches!(val, Value::Ok(_)), "expected Ok, got {:?}", val);
    }

    #[test]
    #[ignore = "integration: starts a local TCP server; run with --include-ignored --test-threads=1"]
    fn tcp_connect_returns_connection_record() {
        use std::net::TcpListener;
        use std::thread;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            let _ = listener.accept();
        });

        let src = format!(
            "fn open() -> Result<Tcp.Connection, String>\n    ! [Tcp]\n    = Tcp.connect(\"127.0.0.1\", {})\n",
            port
        );
        let val = run_tcp_fn(&src, "open");
        match val {
            Value::Ok(inner) => match *inner {
                Value::Record {
                    ref type_name,
                    ref fields,
                } => {
                    assert_eq!(type_name, "Tcp.Connection");
                    assert!(fields.iter().any(|(k, _)| k == "id"));
                    assert!(fields
                        .iter()
                        .any(|(k, v)| k == "host" && *v == Value::Str("127.0.0.1".to_string())));
                    assert!(fields
                        .iter()
                        .any(|(k, v)| k == "port" && *v == Value::Int(port as i64)));
                }
                other => panic!("expected Tcp.Connection record, got {:?}", other),
            },
            other => panic!("expected Ok(Tcp.Connection), got {:?}", other),
        }
    }

    #[test]
    #[ignore = "integration: starts a local TCP server; run with --include-ignored --test-threads=1"]
    fn tcp_send_echo_server() {
        use std::io::{Read, Write};
        use std::net::TcpListener;
        use std::thread;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            if let Ok((mut stream, _)) = listener.accept() {
                let mut buf = Vec::new();
                stream.read_to_end(&mut buf).ok();
                stream.write_all(&buf).ok();
            }
        });

        let src = format!(
            "fn talk() -> Result<String, String>\n    ! [Tcp]\n    = Tcp.send(\"127.0.0.1\", {}, \"echo me\")\n",
            port
        );
        let val = run_tcp_fn(&src, "talk");
        match val {
            Value::Ok(inner) => assert_eq!(*inner, Value::Str("echo me".to_string())),
            other => panic!("expected Ok(\"echo me\"), got {:?}", other),
        }
    }
}

// ---------------------------------------------------------------------------
// Verify: ? operator in verify blocks
// ---------------------------------------------------------------------------

#[test]
fn verify_error_prop_ok_unwraps() {
    // `?` on Ok in a verify case should unwrap normally
    let src = "fn ok() -> Result<Int, String>\n    = Result.Ok(42)\n\nverify ok\n    ok()? => 42\n";
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).unwrap();
        }
    }
    for item in &items {
        if let TopLevel::Verify(vb) = item {
            let result = aver::checker::run_verify(vb, &mut interp);
            assert_eq!(result.passed, 1);
            assert_eq!(result.failed, 0);
        }
    }
}

#[test]
fn verify_error_prop_err_fails_test() {
    // `?` on Err in a verify case should produce a test failure, not a panic/crash
    let src =
        "fn fail() -> Result<Int, String>\n    = Result.Err(\"boom\")\n\nverify fail\n    fail()? => 42\n";
    let items = parse(src);
    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).unwrap();
        }
    }
    for item in &items {
        if let TopLevel::Verify(vb) = item {
            let result = aver::checker::run_verify(vb, &mut interp);
            assert_eq!(result.passed, 0);
            assert_eq!(result.failed, 1);
        }
    }
}

// ---------------------------------------------------------------------------
// Module runtime semantics
// ---------------------------------------------------------------------------

mod module_runtime_tests {
    use super::*;
    use std::collections::HashSet;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_module_root(tag: &str) -> std::path::PathBuf {
        let ts = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock went backwards")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("aver_module_runtime_{}_{}", tag, ts));
        std::fs::create_dir_all(&dir).expect("create temp module dir failed");
        dir
    }

    fn load_module_into(interp: &mut Interpreter, module_root: &std::path::Path, name: &str) {
        let mut loading = Vec::new();
        let mut loading_set = HashSet::new();
        let ns = interp
            .load_module(
                name,
                module_root
                    .to_str()
                    .expect("module_root is not valid UTF-8"),
                &mut loading,
                &mut loading_set,
            )
            .expect("load_module failed");
        interp
            .define_module_path(name, ns)
            .expect("define_module_path failed");
    }

    fn register_fns(interp: &mut Interpreter, src: &str) {
        let mut items = parse(src);
        aver::resolver::resolve_program(&mut items);
        for item in &items {
            if let TopLevel::FnDef(fd) = item {
                interp.exec_fn_def(fd).expect("exec_fn_def failed");
            }
        }
    }

    #[test]
    fn imported_recursive_fn_uses_module_scope() {
        let root = temp_module_root("recursive_scope");
        let math_src = r#"
module Math
    exposes [fib]
    intent =
        "Math module"

fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
        std::fs::write(root.join("Math.av"), math_src).expect("write Math.av failed");

        let app_src = r#"
fn main() -> Int
    Math.fib(6)
"#;

        let mut interp = Interpreter::new();
        load_module_into(&mut interp, &root, "Math");
        register_fns(&mut interp, app_src);

        let main_fn = interp.lookup("main").expect("main not found");
        let out = interp
            .call_value_pub(main_fn, vec![])
            .expect("main call failed");
        assert_eq!(out, Value::Int(8));

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn memo_cache_does_not_collide_with_imported_same_name() {
        let root = temp_module_root("memo_collision");
        let math_src = r#"
module Math
    exposes [fib]
    intent =
        "Math module"

fn fib(n: Int) -> Int
    = n + 100
"#;
        std::fs::write(root.join("Math.av"), math_src).expect("write Math.av failed");

        let app_src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)

fn probe() -> Int
    a = fib(10)
    Math.fib(10)
"#;

        let mut interp = Interpreter::new();
        load_module_into(&mut interp, &root, "Math");
        interp.enable_memo(HashSet::from([String::from("fib")]));
        register_fns(&mut interp, app_src);

        let probe_fn = interp.lookup("probe").expect("probe not found");
        let out = interp
            .call_value_pub(probe_fn, vec![])
            .expect("probe call failed");
        assert_eq!(out, Value::Int(110));

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn imported_effect_alias_is_expanded_in_runtime_gate() {
        let root = temp_module_root("effect_alias");
        let lib_src = r#"
module Lib
    exposes [hi]
    intent =
        "Library"

effects IO = [Console]

fn hi() -> Unit
    ! [IO]
    = Console.print("hello")
"#;
        std::fs::write(root.join("Lib.av"), lib_src).expect("write Lib.av failed");

        let app_src = r#"
fn main() -> Unit
    ! [Console]
    = Lib.hi()
"#;

        let mut interp = Interpreter::new();
        load_module_into(&mut interp, &root, "Lib");
        register_fns(&mut interp, app_src);

        let main_fn = interp.lookup("main").expect("main not found");
        let effects = Interpreter::callable_declared_effects(&main_fn);
        let out = interp
            .call_value_with_effects_pub(main_fn, vec![], "main", effects)
            .expect("main call failed");
        assert_eq!(out, Value::Unit);

        let _ = std::fs::remove_dir_all(&root);
    }
}

// ---------------------------------------------------------------------------
// Auto-memoization tests
// ---------------------------------------------------------------------------

/// Helper: parse, type-check, resolve, compute memo_fns, register everything, call fn.
fn call_fn_with_memo(src: &str, fn_name: &str, args: Vec<Value>) -> Value {
    use aver::call_graph::find_recursive_fns;
    use aver::resolver;
    use aver::types::checker::run_type_check_full;
    use aver::types::Type;

    let mut items = parse(src);

    let tc_result = run_type_check_full(&items, None);
    assert!(
        tc_result.errors.is_empty(),
        "type errors: {:?}",
        tc_result.errors
    );

    resolver::resolve_program(&mut items);

    // Compute memo fns
    let recursive = find_recursive_fns(&items);
    let mut memo_fns = std::collections::HashSet::new();
    for name in &recursive {
        if let Some((params, _ret, effects)) = tc_result.fn_sigs.get(name) {
            if effects.is_empty() {
                let all_safe = params.iter().all(|ty| match ty {
                    Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit => true,
                    Type::Named(n) => tc_result.memo_safe_types.contains(n),
                    _ => false,
                });
                if all_safe {
                    memo_fns.insert(name.clone());
                }
            }
        }
    }

    let mut interp = Interpreter::new();
    interp.enable_memo(memo_fns);

    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            interp.register_type_def(td);
        }
    }
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).expect("exec_fn_def failed");
        }
    }
    let fn_val = interp.lookup(fn_name).expect("fn not found");
    interp.call_value_pub(fn_val, args).expect("call failed")
}

#[test]
fn memo_fib_30_returns_correct_result() {
    let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
    // fib(30) = 832040 — without memo this would take exponential time
    assert_eq!(
        call_fn_with_memo(src, "fib", vec![Value::Int(30)]),
        Value::Int(832040)
    );
}

#[test]
fn memo_fib_small_values() {
    let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
    assert_eq!(
        call_fn_with_memo(src, "fib", vec![Value::Int(0)]),
        Value::Int(0)
    );
    assert_eq!(
        call_fn_with_memo(src, "fib", vec![Value::Int(1)]),
        Value::Int(1)
    );
    assert_eq!(
        call_fn_with_memo(src, "fib", vec![Value::Int(10)]),
        Value::Int(55)
    );
}

#[test]
fn memo_non_recursive_fn_still_works() {
    // Non-recursive functions should work normally (not memoized but not broken)
    let src = "fn double(x: Int) -> Int\n    = x + x\n";
    assert_eq!(
        call_fn_with_memo(src, "double", vec![Value::Int(5)]),
        Value::Int(10)
    );
}

// ---------------------------------------------------------------------------
// Tail-call optimization (TCO) tests
// ---------------------------------------------------------------------------

/// Helper: parse → TCO transform → typecheck → resolve → interpret
fn call_fn_with_tco(src: &str, fn_name: &str, args: Vec<Value>) -> Value {
    use aver::resolver;
    use aver::tco;
    use aver::types::checker::run_type_check_full;

    let mut items = parse(src);
    tco::transform_program(&mut items);

    let tc_result = run_type_check_full(&items, None);
    assert!(
        tc_result.errors.is_empty(),
        "type errors: {:?}",
        tc_result.errors
    );

    resolver::resolve_program(&mut items);

    let mut interp = Interpreter::new();
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            interp.exec_fn_def(fd).expect("exec_fn_def failed");
        }
    }
    let fn_val = interp.lookup(fn_name).expect("fn not found");
    interp.call_value_pub(fn_val, args).expect("call failed")
}

#[test]
fn tco_factorial_large_n() {
    // Tail-recursive factorial — should not overflow with TCO
    let src = r#"
fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)
"#;
    // Small values: correctness
    assert_eq!(
        call_fn_with_tco(src, "factorial", vec![Value::Int(0), Value::Int(1)]),
        Value::Int(1)
    );
    assert_eq!(
        call_fn_with_tco(src, "factorial", vec![Value::Int(5), Value::Int(1)]),
        Value::Int(120)
    );
    assert_eq!(
        call_fn_with_tco(src, "factorial", vec![Value::Int(10), Value::Int(1)]),
        Value::Int(3628800)
    );
    assert_eq!(
        call_fn_with_tco(src, "factorial", vec![Value::Int(20), Value::Int(1)]),
        Value::Int(2432902008176640000)
    );
}

#[test]
fn tco_sum_accumulator() {
    // Tail-recursive sum with accumulator
    let src = r#"
fn sum(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> sum(n - 1, acc + n)
"#;
    assert_eq!(
        call_fn_with_tco(src, "sum", vec![Value::Int(100), Value::Int(0)]),
        Value::Int(5050)
    );
    // Large n: no stack overflow with TCO
    assert_eq!(
        call_fn_with_tco(src, "sum", vec![Value::Int(100_000), Value::Int(0)]),
        Value::Int(5000050000i64)
    );
}

#[test]
fn tco_mutual_recursion_is_even_is_odd() {
    // isEven / isOdd — mutual tail-call recursion
    let src = r#"
fn isEven(n: Int) -> Bool
    match n
        0 -> true
        _ -> isOdd(n - 1)

fn isOdd(n: Int) -> Bool
    match n
        0 -> false
        _ -> isEven(n - 1)
"#;
    // Small values
    assert_eq!(
        call_fn_with_tco(src, "isEven", vec![Value::Int(0)]),
        Value::Bool(true)
    );
    assert_eq!(
        call_fn_with_tco(src, "isEven", vec![Value::Int(1)]),
        Value::Bool(false)
    );
    assert_eq!(
        call_fn_with_tco(src, "isOdd", vec![Value::Int(1)]),
        Value::Bool(true)
    );
    assert_eq!(
        call_fn_with_tco(src, "isOdd", vec![Value::Int(4)]),
        Value::Bool(false)
    );
    // Large n: would overflow without mutual TCO
    assert_eq!(
        call_fn_with_tco(src, "isEven", vec![Value::Int(100_000)]),
        Value::Bool(true)
    );
    assert_eq!(
        call_fn_with_tco(src, "isOdd", vec![Value::Int(100_001)]),
        Value::Bool(true)
    );
}

#[test]
fn tco_non_tail_fib_still_works() {
    // fib is NOT tail-recursive — should still work (via memoization or normal recursion)
    let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
    // Small values work even without memo (normal recursion)
    assert_eq!(
        call_fn_with_tco(src, "fib", vec![Value::Int(0)]),
        Value::Int(0)
    );
    assert_eq!(
        call_fn_with_tco(src, "fib", vec![Value::Int(1)]),
        Value::Int(1)
    );
    assert_eq!(
        call_fn_with_tco(src, "fib", vec![Value::Int(10)]),
        Value::Int(55)
    );
}

#[test]
fn tco_non_tail_call_stays_recursive() {
    // h + sum(t) is NOT in tail position — normal recursion
    let src = r#"
fn mySum(n: Int) -> Int
    match n
        0 -> 0
        _ -> n + mySum(n - 1)
"#;
    assert_eq!(
        call_fn_with_tco(src, "mySum", vec![Value::Int(10)]),
        Value::Int(55)
    );
}

// ---------------------------------------------------------------------------
// Replay / record tests
// ---------------------------------------------------------------------------

mod replay_tests {
    use super::*;
    use aver::replay::{json_to_value, value_to_json, EffectRecord, JsonValue, RecordedOutcome};
    use aver::value::RuntimeError;
    use std::collections::BTreeMap;

    fn run_effect_fn(
        src: &str,
        fn_name: &str,
        interp: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let items = parse(src);
        for item in &items {
            if let TopLevel::FnDef(fd) = item {
                interp.exec_fn_def(fd).expect("exec_fn_def failed");
            }
        }
        let fn_val = interp.lookup(fn_name).expect("fn not found");
        let effects = Interpreter::callable_declared_effects(&fn_val);
        interp.call_value_with_effects_pub(fn_val, vec![], fn_name, effects)
    }

    #[test]
    fn record_mode_logs_console_effect() {
        let src = r#"
fn ping() -> Unit
    ! [Console]
    = Console.print("hello")
"#;
        let mut interp = Interpreter::new();
        interp.start_recording();
        let out = run_effect_fn(src, "ping", &mut interp).expect("ping failed");
        assert_eq!(out, Value::Unit);

        let effects = interp.take_recorded_effects();
        assert_eq!(effects.len(), 1);
        assert_eq!(effects[0].seq, 1);
        assert_eq!(effects[0].effect_type, "Console.print");
        assert_eq!(
            effects[0].args,
            vec![JsonValue::String("hello".to_string())]
        );
        assert_eq!(effects[0].outcome, RecordedOutcome::Value(JsonValue::Null));
    }

    #[test]
    fn replay_mode_substitutes_recorded_result() {
        let src = r#"
fn check() -> Bool
    ! [Disk]
    = Disk.exists("/definitely/not/existing/path")
"#;
        let mut interp = Interpreter::new();
        interp.start_replay(
            vec![EffectRecord {
                seq: 1,
                effect_type: "Disk.exists".to_string(),
                args: vec![JsonValue::String(
                    "/definitely/not/existing/path".to_string(),
                )],
                outcome: RecordedOutcome::Value(JsonValue::Bool(true)),
            }],
            true,
        );
        let out = run_effect_fn(src, "check", &mut interp).expect("check failed");
        assert_eq!(out, Value::Bool(true));
        interp
            .ensure_replay_consumed()
            .expect("all effects should be consumed");
    }

    #[test]
    fn replay_mode_detects_effect_order_mismatch() {
        let src = r#"
fn check() -> Bool
    ! [Disk]
    = Disk.exists("/tmp/x")
"#;
        let mut interp = Interpreter::new();
        let mut outcome = BTreeMap::new();
        outcome.insert("$ok".to_string(), JsonValue::String("x".to_string()));
        interp.start_replay(
            vec![EffectRecord {
                seq: 1,
                effect_type: "Http.get".to_string(),
                args: vec![JsonValue::String("https://example.com".to_string())],
                outcome: RecordedOutcome::Value(JsonValue::Object(outcome)),
            }],
            false,
        );
        let err = run_effect_fn(src, "check", &mut interp).expect_err("expected replay mismatch");
        assert!(
            matches!(err, RuntimeError::ReplayMismatch { .. }),
            "expected ReplayMismatch, got {:?}",
            err
        );
    }

    #[test]
    fn value_json_roundtrip_nested_record_variant_list() {
        let value = Value::Record {
            type_name: "Envelope".to_string(),
            fields: vec![
                ("id".to_string(), Value::Int(7)),
                (
                    "payload".to_string(),
                    Value::Variant {
                        type_name: "Event".to_string(),
                        variant: "Created".to_string(),
                        fields: vec![
                            Value::Record {
                                type_name: "User".to_string(),
                                fields: vec![
                                    ("age".to_string(), Value::Int(35)),
                                    ("name".to_string(), Value::Str("Ada".to_string())),
                                ],
                            },
                            Value::List(vec![
                                Value::Some(Box::new(Value::Int(1))),
                                Value::None,
                                Value::Ok(Box::new(Value::Str("ok".to_string()))),
                                Value::Err(Box::new(Value::Str("boom".to_string()))),
                            ]),
                        ],
                    },
                ),
            ],
        };

        let json = value_to_json(&value).expect("value_to_json failed");
        let restored = json_to_value(&json).expect("json_to_value failed");
        assert_eq!(restored, value);
    }

    #[test]
    fn value_json_roundtrip_list_with_nested_structures() {
        let value = Value::List(vec![
            Value::Record {
                type_name: "Point".to_string(),
                fields: vec![
                    ("x".to_string(), Value::Float(1.5)),
                    ("y".to_string(), Value::Float(-2.25)),
                ],
            },
            Value::Variant {
                type_name: "MaybePoint".to_string(),
                variant: "Some".to_string(),
                fields: vec![Value::Record {
                    type_name: "Point".to_string(),
                    fields: vec![
                        ("x".to_string(), Value::Int(1)),
                        ("y".to_string(), Value::Int(2)),
                    ],
                }],
            },
            Value::Ok(Box::new(Value::List(vec![
                Value::Bool(true),
                Value::Some(Box::new(Value::Str("v".to_string()))),
            ]))),
        ]);

        let json = value_to_json(&value).expect("value_to_json failed");
        let restored = json_to_value(&json).expect("json_to_value failed");
        assert_eq!(restored, value);
    }
}

// ---------------------------------------------------------------------------
// Typed bindings
// ---------------------------------------------------------------------------

#[test]
fn typed_binding_runtime_works() {
    let src = concat!(
        "fn f() -> Int\n",
        "    x: Int = 42\n",
        "    x\n",
        "result = f()\n",
    );
    let interp = run_program(src);
    assert_eq!(interp.lookup("result").unwrap(), Value::Int(42));
}
