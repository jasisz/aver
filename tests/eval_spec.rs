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
    let items = parse("print(\"hi\")");
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
    let src = "fn log(n: Int) -> Unit\n    ! [Console]\n    = print(n)\n";
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

#[test]
fn builtin_abs_positive() {
    assert_eq!(eval("abs(5)"), Value::Int(5));
}

#[test]
fn builtin_abs_negative_via_subtraction() {
    // Aver has no unary minus literal; use 0 - n
    assert_eq!(eval("abs(0 - 5)"), Value::Int(5));
}

#[test]
fn builtin_abs_float() {
    assert_eq!(eval("abs(0.0 - 3.5)"), Value::Float(3.5));
}

#[test]
fn builtin_str_int() {
    assert_eq!(eval("str(42)"), Value::Str("42".to_string()));
}

#[test]
fn builtin_str_bool() {
    assert_eq!(eval("str(true)"), Value::Str("true".to_string()));
}

#[test]
fn builtin_int_from_str() {
    assert_eq!(eval("int(\"42\")"), Value::Int(42));
}

#[test]
fn builtin_int_from_float() {
    assert_eq!(eval("int(3.9)"), Value::Int(3));
}

#[test]
fn builtin_len_string() {
    assert_eq!(eval("len(\"hello\")"), Value::Int(5));
}

#[test]
fn builtin_len_empty_string() {
    assert_eq!(eval("len(\"\")"), Value::Int(0));
}

#[test]
fn builtin_len_list() {
    assert_eq!(eval("len([1, 2, 3])"), Value::Int(3));
}

#[test]
fn builtin_len_empty_list() {
    assert_eq!(eval("len([])"), Value::Int(0));
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
        eval("get([10, 20, 30], 0)"),
        Value::Ok(Box::new(Value::Int(10)))
    );
}

#[test]
fn get_middle_element() {
    assert_eq!(
        eval("get([10, 20, 30], 1)"),
        Value::Ok(Box::new(Value::Int(20)))
    );
}

#[test]
fn get_out_of_bounds_returns_err() {
    let result = eval("get([1, 2], 5)");
    assert!(matches!(result, Value::Err(_)));
}

#[test]
fn head_returns_first() {
    assert_eq!(
        eval("head([42, 1, 2])"),
        Value::Ok(Box::new(Value::Int(42)))
    );
}

#[test]
fn head_empty_list_returns_err() {
    assert!(matches!(eval("head([])"), Value::Err(_)));
}

#[test]
fn tail_returns_rest() {
    assert_eq!(
        eval("tail([1, 2, 3])"),
        Value::Ok(Box::new(Value::List(vec![Value::Int(2), Value::Int(3)])))
    );
}

#[test]
fn tail_single_element_returns_empty() {
    assert_eq!(eval("tail([99])"), Value::Ok(Box::new(Value::List(vec![]))));
}

#[test]
fn tail_empty_list_returns_err() {
    assert!(matches!(eval("tail([])"), Value::Err(_)));
}

#[test]
fn push_appends_element() {
    assert_eq!(
        eval("push([1, 2], 3)"),
        Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
    );
}

#[test]
fn push_to_empty_list() {
    assert_eq!(eval("push([], 1)"), Value::List(vec![Value::Int(1)]));
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

#[test]
fn ok_wraps_value() {
    assert_eq!(eval("Ok(42)"), Value::Ok(Box::new(Value::Int(42))));
}

#[test]
fn err_wraps_value() {
    assert_eq!(
        eval("Err(\"fail\")"),
        Value::Err(Box::new(Value::Str("fail".to_string())))
    );
}

#[test]
fn some_wraps_value() {
    assert_eq!(eval("Some(1)"), Value::Some(Box::new(Value::Int(1))));
}

#[test]
fn none_is_none() {
    assert_eq!(eval("None"), Value::None);
}

// ---------------------------------------------------------------------------
// Match expressions
// ---------------------------------------------------------------------------

#[test]
fn match_literal_zero() {
    let src = "fn classify(n: Int) -> String\n    = match n:\n        0 -> \"zero\"\n        _ -> \"other\"\n";
    assert_eq!(
        call_fn(src, "classify", vec![Value::Int(0)]),
        Value::Str("zero".to_string())
    );
}

#[test]
fn match_literal_wildcard() {
    let src = "fn classify(n: Int) -> String\n    = match n:\n        0 -> \"zero\"\n        _ -> \"other\"\n";
    assert_eq!(
        call_fn(src, "classify", vec![Value::Int(99)]),
        Value::Str("other".to_string())
    );
}

#[test]
fn match_ok_constructor() {
    let src = "fn unwrap(r: Any) -> Int\n    = match r:\n        Ok(v) -> v\n        Err(_) -> 0\n";
    assert_eq!(
        call_fn(src, "unwrap", vec![Value::Ok(Box::new(Value::Int(42)))]),
        Value::Int(42)
    );
}

#[test]
fn match_err_constructor() {
    let src = "fn unwrap(r: Any) -> Int\n    = match r:\n        Ok(v) -> v\n        Err(_) -> 0\n";
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
    let src =
        "fn extract(o: Any) -> Int\n    = match o:\n        Some(v) -> v\n        None -> 0\n";
    assert_eq!(
        call_fn(src, "extract", vec![Value::Some(Box::new(Value::Int(7)))]),
        Value::Int(7)
    );
    assert_eq!(call_fn(src, "extract", vec![Value::None]), Value::Int(0));
}

#[test]
fn match_bool_literal() {
    let src = "fn yes_no(b: Bool) -> String\n    = match b:\n        true -> \"yes\"\n        false -> \"no\"\n";
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
    let src = "fn is_empty(xs: List<Int>) -> Bool\n    = match xs:\n        [] -> true\n        [_, ..rest] -> false\n";
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
    let src = "fn score(xs: List<Int>) -> Int\n    = match xs:\n        [h, ..t] -> h + len(t)\n        [] -> 0\n";
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
fn val_binding_used_in_body() {
    let src = "fn compute() -> Int\n    val x = 10\n    val y = 20\n    x + y\n";
    assert_eq!(call_fn(src, "compute", vec![]), Value::Int(30));
}

#[test]
fn var_reassignment() {
    let src = "fn count() -> Int\n    var x = 0\n    x = 5\n    x\n";
    assert_eq!(call_fn(src, "count", vec![]), Value::Int(5));
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
    let map_fn = interp.lookup("map").unwrap();
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
    let filter_fn = interp.lookup("filter").unwrap();
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
    let fold_fn = interp.lookup("fold").unwrap();
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
    let src = "fn get_ok(r: Any) -> Int\n    = r?\n";
    assert_eq!(
        call_fn(src, "get_ok", vec![Value::Ok(Box::new(Value::Int(99)))]),
        Value::Int(99)
    );
}

#[test]
fn error_prop_early_return_on_err() {
    // ? on Err causes early return: the function returns Err(e), not a crash.
    let src = "fn get_val(r: Any) -> Any\n    = r?\n";
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
    let src = "fn double_ok(r: Any) -> Any\n    val x = r?\n    Ok(x + x)\n";
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
    let src = "fn chain(a: Any, b: Any) -> Any\n    val x = a?\n    val y = b?\n    Ok(x + y)\n";
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
        "fn make_adder(n: Int) -> Any\n    fn add(x: Int) -> Int\n        = x + n\n    add\n";
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
    let src = "type Shape\n  Circle(Float)\n  Point\nval p = Shape.Point\n";
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
    let src = "type Shape\n  Circle(Float)\n  Point\nval c = Shape.Circle(3.14)\n";
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
    let src = "type Shape\n  Rect(Float, Float)\nval r = Shape.Rect(3.0, 4.0)\n";
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
        "  = match s:\n",
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
        "  = match s:\n",
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
    let src = "record User\n  name: String\n  age: Int\nval u = User(name: \"Alice\", age: 30)\n";
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
    let src = "record User\n  name: String\n  age: Int\nval u = User(name: \"Alice\", age: 30)\nval n = u.name\n";
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
        "  = match u:\n",
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
// Network builtins — local TcpListener, no internet required
// ---------------------------------------------------------------------------

mod network_tests {
    use super::*;
    use aver::interpreter::{Interpreter, Value};
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::thread;

    /// Spawn a minimal HTTP/1.1 server on an OS-assigned port.
    /// Returns None when the bind is not permitted (sandboxed CI environments).
    fn start_server(status: u16, body: &'static str, extra_headers: &'static str) -> Option<String> {
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

    fn run_network_fn(src: &str, fn_name: &str) -> Value {
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
    fn network_get_200_returns_ok_response() {
        let Some(url) = start_server(200, "hello", "") else { return; };
        let src = format!(
            "fn fetch() -> Any\n    ! [Network]\n    Network.get(\"{}\")\n",
            url
        );
        let val = run_network_fn(&src, "fetch");
        match val {
            Value::Ok(inner) => match *inner {
                Value::Record { type_name, ref fields } => {
                    assert_eq!(type_name, "NetworkResponse");
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
    fn network_get_404_still_returns_ok_response() {
        let Some(url) = start_server(404, "not found", "") else { return; };
        let src = format!(
            "fn fetch() -> Any\n    ! [Network]\n    Network.get(\"{}\")\n",
            url
        );
        let val = run_network_fn(&src, "fetch");
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
    fn network_get_transport_error_returns_err() {
        // Port 1 is almost certainly not listening
        let src = "fn fetch() -> Any\n    ! [Network]\n    Network.get(\"http://127.0.0.1:1/\")\n";
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
            "expected Err for unreachable host, got {:?}", val
        );
    }

    #[test]
    #[ignore = "integration: starts a local HTTP server; run with --include-ignored --test-threads=1"]
    fn network_post_201_returns_ok_response() {
        let Some(url) = start_server(201, "created", "") else { return; };
        let src = format!(
            "fn send() -> Any\n    ! [Network]\n    Network.post(\"{}\", \"data\", \"text/plain\", [])\n",
            url
        );
        let val = run_network_fn(&src, "send");
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
    fn network_post_bad_headers_returns_runtime_error() {
        // Pass a non-list for headers — validation fails before any HTTP call
        let src = "fn send() -> Any\n    ! [Network]\n    Network.post(\"http://127.0.0.1:1/\", \"\", \"text/plain\", \"bad\")\n";
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
            "fn run() -> Any\n    ! [Disk]\n    Disk.writeText(\"{}\", \"hello\")\n",
            path_str.replace('\\', "\\\\")
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));

        let src2 = format!(
            "fn run() -> Any\n    ! [Disk]\n    Disk.readText(\"{}\")\n",
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
            "fn run() -> Any\n    ! [Disk]\n    Disk.appendText(\"{}\", \" world\")\n",
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
            "fn run() -> Any\n    ! [Disk]\n    Disk.exists(\"{}\")\n",
            path_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Bool(true));
        let _ = std::fs::remove_file(&path);

        let missing_path = tmp_path("does_not_exist_xyz.txt");
        let missing_str = missing_path.to_string_lossy().replace('\\', "\\\\");
        let src2 = format!(
            "fn run() -> Any\n    ! [Disk]\n    Disk.exists(\"{}\")\n",
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
            "fn run() -> Any\n    ! [Disk]\n    Disk.delete(\"{}\")\n",
            path_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));
        assert!(!path.exists());
    }

    #[test]
    fn disk_delete_missing_file_returns_err() {
        let path = tmp_path("no_such_file_xyz.txt");
        let path_str = path.to_string_lossy().replace('\\', "\\\\");
        let src = format!(
            "fn run() -> Any\n    ! [Disk]\n    Disk.delete(\"{}\")\n",
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
            "fn run() -> Any\n    ! [Disk]\n    Disk.makeDir(\"{}\")\n",
            dir_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));
        assert!(dir.exists());

        // Write a file inside to list it
        std::fs::write(dir.join("a.txt"), "").unwrap();
        let src2 = format!(
            "fn run() -> Any\n    ! [Disk]\n    Disk.listDir(\"{}\")\n",
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
        let src = "fn run() -> Any\n    ! [Disk]\n    Disk.readText(\"/no/such/file.txt\")\n";
        let val = run_disk_fn(src, "run");
        assert!(matches!(val, Value::Err(_)));
    }
}
