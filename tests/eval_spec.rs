/// Spec tests for the Aver VM runtime.
///
/// Tests evaluate expressions and function calls via the bytecode VM,
/// bypassing the CLI and type checker so they focus solely on runtime semantics.
use std::str::FromStr;
use std::sync::Arc as Rc;

use aver::ast::{FnBody, FnDef, Stmt, TopLevel};
use aver::codegen::ModuleInfo;
use aver::ir::SymbolTable;
use aver::ir::hir::{self, ResolvedTopLevel};
use aver::lexer::Lexer;
use aver::nan_value::{Arena, NanValue, NanValueConvert};
use aver::parser::Parser;
use aver::resolver::resolve_program;
use aver::tco;
use aver::value::{Value, list_from_vec, list_to_vec};
use aver::vm;

fn resolve_for_vm(
    items: &[TopLevel],
    dep_modules: &[ModuleInfo],
) -> (Vec<ResolvedTopLevel>, SymbolTable) {
    let symbols = SymbolTable::build(items, dep_modules);
    let resolved = hir::resolve_program(&symbols, items);
    (resolved, symbols)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn parse(src: &str) -> Vec<TopLevel> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser.parse().expect("parse failed")
}

/// Compile items to VM bytecode.
fn vm_compile(items: &[TopLevel]) -> vm::VM {
    let mut items = items.to_vec();
    tco::transform_program(&mut items);
    resolve_program(&mut items);
    let mut dependencies = items
        .iter()
        .find_map(|item| match item {
            TopLevel::Module(module) => Some(module.depends.clone()),
            _ => None,
        })
        .unwrap_or_default();
    for dependency in aver::stdlib::implicit_stdlib_deps(&items) {
        if !dependencies.contains(&dependency) {
            dependencies.push(dependency);
        }
    }
    let loaded = aver::source::load_module_tree(&dependencies, ".")
        .expect("load explicit and implicit VM dependencies");
    let dep_modules = loaded
        .iter()
        .map(ModuleInfo::from_loaded)
        .collect::<Vec<_>>();
    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (resolved, symbols) = resolve_for_vm(&items, &dep_modules);
    let (code, globals) = vm::compile_program_with_loaded_modules(
        &resolved, &symbols, &mut arena, loaded, "<test>", None,
    )
    .expect("VM compile failed");
    vm::VM::new(code, globals, arena)
}

/// Compile and run top-level statements, return ready VM.
#[allow(dead_code)]
fn vm_build(src: &str) -> vm::VM {
    let items = parse(src);
    let mut machine = vm_compile(&items);
    machine.run_top_level().expect("top-level failed");
    machine
}

/// Evaluate a single top-level expression via VM.
fn eval(src: &str) -> Value {
    let items = parse(src);
    let item = items.into_iter().next().expect("no items");
    if let TopLevel::Stmt(Stmt::Expr(expr)) = item {
        let wrapper = TopLevel::FnDef(FnDef {
            name: "__eval".to_string(),
            params: vec![],
            line: 0,
            return_type: "Unit".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(expr)),
            resolution: None,
        });
        let mut machine = vm_compile(&[wrapper]);
        let result = machine
            .run_named_function("__eval", &[])
            .expect("eval failed");
        result.to_value(&machine.arena)
    } else {
        panic!("expected a single expression, got: {:?}", item);
    }
}

/// Try to evaluate a single expression, returning Err on failure.
fn try_eval(src: &str) -> Result<Value, String> {
    let items = parse(src);
    let item = items.into_iter().next().expect("no items");
    if let TopLevel::Stmt(Stmt::Expr(expr)) = item {
        let wrapper = TopLevel::FnDef(FnDef {
            name: "__eval".to_string(),
            params: vec![],
            line: 0,
            return_type: "Unit".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(expr)),
            resolution: None,
        });
        let mut machine = vm_compile(&[wrapper]);
        match machine.run_named_function("__eval", &[]) {
            Ok(v) => Ok(v.to_value(&machine.arena)),
            Err(e) => Err(format!("{}", e)),
        }
    } else {
        panic!("expected a single expression, got: {:?}", item);
    }
}

/// Register all function definitions from `src`, then call `fn_name` with `args`.
fn call_fn(src: &str, fn_name: &str, args: Vec<Value>) -> Value {
    let items = parse(src);
    let mut machine = vm_compile(&items);
    machine.run_top_level().expect("top-level failed");
    let nv_args: Vec<NanValue> = args
        .iter()
        .map(|v| NanValue::from_value(v, &mut machine.arena))
        .collect();
    let result = machine
        .run_named_function(fn_name, &nv_args)
        .expect("call failed");
    result.to_value(&machine.arena)
}

fn call_fn_resolved(src: &str, fn_name: &str, args: Vec<Value>) -> Value {
    call_fn(src, fn_name, args)
}

/// Parse, compile via VM, run top-level, then lookup a binding by appending
/// a getter function.
fn run_program_lookup(src: &str, var_name: &str) -> Value {
    let mut items = parse(src);
    let getter_items = parse(&format!("fn test__get()\n    {}", var_name));
    items.extend(getter_items);
    let mut machine = vm_compile(&items);
    machine.run_top_level().expect("top-level failed");
    let result = machine
        .run_named_function("test__get", &[])
        .expect("lookup failed");
    result.to_value(&machine.arena)
}

/// Call a function with allowed effects set (for effectful tests).
/// `run_named_function` automatically sets the allowed effects from the compiled
/// function's declared effects, so no manual setup is needed.
fn call_fn_with_effects(src: &str, fn_name: &str, args: Vec<Value>) -> Result<Value, String> {
    let items = parse(src);
    let mut machine = vm_compile(&items);
    machine.run_top_level().map_err(|e| format!("{}", e))?;

    let nv_args: Vec<NanValue> = args
        .iter()
        .map(|v| NanValue::from_value(v, &mut machine.arena))
        .collect();
    match machine.run_named_function(fn_name, &nv_args) {
        Ok(v) => Ok(v.to_value(&machine.arena)),
        Err(e) => Err(format!("{}", e)),
    }
}

// ---------------------------------------------------------------------------
// Integer arithmetic
// ---------------------------------------------------------------------------

#[test]
fn int_add() {
    assert_eq!(eval("2 + 3"), Value::int(5));
}

#[test]
fn int_sub() {
    assert_eq!(eval("10 - 4"), Value::int(6));
}

#[test]
fn int_mul() {
    assert_eq!(eval("3 * 4"), Value::int(12));
}

#[test]
fn int_div() {
    assert_eq!(eval("10 / 2"), Value::int(5));
}

#[test]
fn int_chained_arithmetic() {
    // 2 + 3 * 4 = 2 + 12 = 14  (left-to-right, no precedence difference expected)
    // Actually Aver respects precedence: mul before add
    assert_eq!(eval("2 + 3 * 4"), Value::int(14));
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
fn int_float_no_promotion() {
    // In the VM, Int + Float promotes to Float (type checker catches this
    // when enabled; this test bypasses the type checker).
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
    let result = try_eval("Console.print(\"hi\")");
    let msg = result.expect_err("expected runtime gate error");
    assert!(msg.contains("effect"), "got: {}", msg);
    assert!(msg.contains("Console"), "got: {}", msg);
}

#[test]
fn runtime_gate_allows_effectful_entrypoint_with_grant() {
    let src =
        "fn log(n: Int) -> Unit\n    ! [Console.print]\n    Console.print(String.fromInt(n))\n";
    // With effect grant via run_named_function (which sets effects from fn metadata), should succeed
    let result = call_fn_with_effects(src, "log", vec![Value::int(2)]);
    assert!(result.is_ok(), "expected granted call to pass");
}

// ---------------------------------------------------------------------------
// Int namespace
// ---------------------------------------------------------------------------

#[test]
fn int_to_string() {
    assert_eq!(eval("String.fromInt(42)"), Value::Str("42".to_string()));
}

#[test]
fn int_from_string() {
    assert_eq!(
        eval("Int.fromString(\"42\")"),
        Value::Ok(Box::new(Value::int(42)))
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
    assert_eq!(eval("Int.fromFloat(3.9)"), Value::int(3));
}

#[test]
fn int_endian_codecs_cover_exact_width_errors_and_unbounded_roundtrips() {
    let src = r#"module EndianVm
    intent = "Exercise exact-width integer byte codecs."
    depends [Bytes]
    effects []

fn showBig(value: Int, width: Int) -> String
    match Int.toBigEndian(value, width)
        Result.Ok(bytes) -> Bytes.toHex(bytes)
        Result.Err(error) -> error

fn showLittle(value: Int, width: Int) -> String
    match Int.toLittleEndian(value, width)
        Result.Ok(bytes) -> Bytes.toHex(bytes)
        Result.Err(error) -> error
"#;
    assert_eq!(
        call_fn_with_effects(src, "showBig", vec![Value::int(258), Value::int(2)]),
        Ok(Value::Str("0102".to_string()))
    );
    assert_eq!(
        call_fn_with_effects(src, "showLittle", vec![Value::int(258), Value::int(2)]),
        Ok(Value::Str("0201".to_string()))
    );
    assert_eq!(
        call_fn_with_effects(src, "showBig", vec![Value::int(0), Value::int(0)]),
        Ok(Value::Str(String::new()))
    );
    assert_eq!(
        call_fn_with_effects(src, "showBig", vec![Value::int(256), Value::int(1)]),
        Ok(Value::Str(
            "Int.toBigEndian: value does not fit in the requested width".to_string()
        ))
    );

    let beyond_u64 = aver_rt::AverInt::from_str("18446744073709551616").unwrap();
    assert_eq!(
        eval("Int.fromBigEndian(Int.toBigEndian(18446744073709551616, 9))"),
        Value::Int(beyond_u64.clone())
    );
    assert_eq!(
        eval("Int.fromLittleEndian(Int.toLittleEndian(18446744073709551616, 9))"),
        Value::Int(beyond_u64)
    );
}

#[test]
fn int_abs() {
    assert_eq!(eval("Int.abs(5)"), Value::int(5));
}

#[test]
fn int_abs_negative() {
    assert_eq!(eval("Int.abs(0 - 5)"), Value::int(5));
}

#[test]
fn int_min() {
    assert_eq!(eval("Int.min(3, 7)"), Value::int(3));
}

#[test]
fn int_max() {
    assert_eq!(eval("Int.max(3, 7)"), Value::int(7));
}

#[test]
fn int_mod() {
    // Literal-divisor discharge: a syntactic nonzero literal divisor makes
    // `Int.mod` total — the resolver lowers it to the Euclidean intrinsic,
    // so the value is a bare Int, not `Result.Ok`. (This harness compiles
    // WITHOUT typechecking — proof that the discharge keys on syntax, not
    // on type stamps.)
    assert_eq!(eval("Int.mod(10, 3)"), Value::int(1));
    // A dynamic divisor keeps the `Result` path.
    assert_eq!(
        eval("Int.mod(10, Int.min(3, 4))"),
        Value::Ok(Box::new(Value::int(1)))
    );
}

#[test]
fn int_mod_zero() {
    assert_eq!(
        eval("Int.mod(10, 0)"),
        Value::Err(Box::new(Value::Str("division by zero".to_string())))
    );
}

#[test]
fn int_mod_negative_dividend_positive_divisor() {
    // Euclidean modulo always lands in [0, |b|). Diverges from Rust
    // `%` (truncated remainder) which would return -1 here.
    assert_eq!(eval("Int.mod(-7, 3)"), Value::int(2));
}

#[test]
fn int_mod_negative_divisor() {
    // Result is in [0, |b|) regardless of b's sign. 7 = (-3) * (-2) + 1.
    assert_eq!(eval("Int.mod(7, -3)"), Value::int(1));
}

#[test]
fn int_mod_both_negative() {
    // -7 = (-3) * 3 + 2.
    assert_eq!(eval("Int.mod(-7, -3)"), Value::int(2));
}

#[test]
fn int_div_builtin() {
    // Literal-divisor discharge — bare Int, no `Result.Ok` wrap.
    assert_eq!(eval("Int.div(7, 2)"), Value::int(3));
    // A dynamic divisor keeps the `Result` path.
    assert_eq!(
        eval("Int.div(7, Int.min(2, 3))"),
        Value::Ok(Box::new(Value::int(3)))
    );
}

#[test]
fn int_div_zero() {
    assert_eq!(
        eval("Int.div(7, 0)"),
        Value::Err(Box::new(Value::Str("division by zero".to_string())))
    );
}

#[test]
fn int_div_is_euclidean_floor() {
    // Euclidean (flooring) division — the exact partner of Euclidean
    // `Int.mod`, so `Int.div(a,b)*b + Int.mod(a,b) == a` for every sign.
    // `-7 / 2 == -4` (floors toward -inf), NOT -3 (truncate toward zero).
    assert_eq!(eval("Int.div(-7, 2)"), Value::int(-4));
    assert_eq!(eval("Int.div(7, -2)"), Value::int(-3));
    assert_eq!(eval("Int.div(-7, -2)"), Value::int(4));
    // The division identity holds: -7 == (-4)*2 + 1, with Int.mod(-7,2) == 1.
    assert_eq!(eval("Int.mod(-7, 2)"), Value::int(1));
}

#[test]
fn int_div_fused_withdefault() {
    // Leaf-op fusion `Result.withDefault(Int.div(a, b), default)`. The
    // divisor must be dynamic here: a literal one discharges to a bare Int
    // (making the withDefault wrapper a type error in checked pipelines).
    assert_eq!(
        eval("Result.withDefault(Int.div(7, Int.min(2, 3)), -1)"),
        Value::int(3)
    );
    assert_eq!(
        eval("Result.withDefault(Int.div(7, 0), -1)"),
        Value::int(-1)
    );
}

#[test]
fn int_div_min_by_neg_one_is_exact_over_z() {
    // `Int` is mathematical ℤ, so `i64::MIN / -1` is NOT an overflow — it is
    // exactly `i64::MAX + 1` (`9223372036854775808`) as an
    // arbitrary-precision Big. The old i64-wrap overflow `Err` is gone. The
    // literal `-1` divisor discharges, so the value is a bare Int. `i64::MIN`
    // is built by arithmetic (the literal `-9223372036854775808` doesn't
    // parse — it's negate-of-`i64::MAX + 1`).
    let expected = aver_rt::AverInt::from_str("9223372036854775808").unwrap();
    assert_eq!(
        eval("Int.div((0 - 9223372036854775807) - 1, -1)"),
        Value::Int(expected.clone())
    );
    // Dynamic `-1` divisor (`0 - 1` is a constant expression, not a
    // syntactic literal — outside the discharge boundary): the `Result`
    // path, and the MIR const-fold's lifted `-1` arm, return the same
    // exact value (no fallback — no error).
    assert_eq!(
        eval("Result.withDefault(Int.div((0 - 9223372036854775807) - 1, 0 - 1), 99)"),
        Value::Int(expected)
    );
}

// ---------------------------------------------------------------------------
// Int = ℤ — the VM no longer wraps; runtime matches the Lean/Dafny model.
// ---------------------------------------------------------------------------

/// Assert that an eval result is a strictly-positive Big integer equal to the
/// given decimal string. Guards the exact C0 law: the VM stopped wrapping.
fn assert_positive_big(src: &str, decimal: &str) {
    let expected = aver_rt::AverInt::from_str(decimal).unwrap();
    assert!(
        matches!(&expected, aver_rt::AverInt::Big(_)),
        "test value should be a Big"
    );
    assert!(expected > aver_rt::AverInt::zero());
    assert_eq!(eval(src), Value::Int(expected));
}

#[test]
fn square_is_non_negative_at_i64_max() {
    // The exact C0 law: `a*a >= 0` for `a = i64::MAX`. On wrapping i64 this is
    // false (the product wraps negative); over ℤ it is a positive Big.
    // i64::MAX = 9223372036854775807; its square is 85070591730234615847396907784232501249.
    assert_positive_big(
        "9223372036854775807 * 9223372036854775807",
        "85070591730234615847396907784232501249",
    );
}

#[test]
fn square_is_non_negative_near_three_billion() {
    // `a ~ 4e9`: `a*a` overflows i64 (wraps negative) but is a positive Big.
    // 4000000000^2 = 16000000000000000000 (> i64::MAX = ~9.22e18, a genuine
    // i64 overflow that would wrap to a negative two's-complement value).
    assert_positive_big("4000000000 * 4000000000", "16000000000000000000");
}

#[test]
fn parse_beyond_i64_multiply_and_stringify_roundtrip() {
    // Parse a value past i64::MAX, multiply, and stringify back to an exact
    // decimal — the full bignum round trip through the running program.
    assert_eq!(
        eval(r#"String.fromInt(Result.withDefault(Int.fromString("100000000000000000000"), 0))"#),
        Value::Str("100000000000000000000".to_string())
    );
    // 10^20 * 10^20 = 10^40.
    let src = r#"String.fromInt(Result.withDefault(Int.fromString("100000000000000000000"), 0) * Result.withDefault(Int.fromString("100000000000000000000"), 0))"#;
    assert_eq!(
        eval(src),
        Value::Str("10000000000000000000000000000000000000000".to_string())
    );
}

#[test]
fn equal_bigs_built_differently_key_the_same_map_entry() {
    // Two structurally-equal Big keys built different ways must collapse to a
    // single Map entry (guards the map_key_hash arena-index bug). One is built
    // by arithmetic, the other parsed; both equal 10^20.
    // 10^20 two ways: parsed, and `10^10 * 10^10`.
    let src = r#"Map.len(Map.set(Map.set({}, Result.withDefault(Int.fromString("100000000000000000000"), 0), 1), 10000000000 * 10000000000, 2))"#;
    assert_eq!(eval(src), Value::int(1));
}

#[test]
fn vector_get_with_big_index_is_none() {
    // A ℤ-overflow index can never be in range — returns `Option.None`, never
    // a panic or a truncated wrap.
    let src = r#"Vector.get(Vector.new(3, 0), Result.withDefault(Int.fromString("100000000000000000000"), 0))"#;
    assert_eq!(eval(src), Value::None);
}

#[test]
fn vector_new_oversized_errors_cleanly() {
    // A size outside the portable materialization budget is a catchable
    // value, never a runtime abort.
    let message = aver_rt::vector_size_error_message();
    let src = r#"Vector.new(Result.withDefault(Int.fromString("100000000000000000000"), 0), 0)"#;
    assert_eq!(eval(src), Value::Err(Box::new(Value::Str(message.clone()))));
    assert_eq!(
        eval("Vector.new(0 - 1, 0)"),
        Value::Err(Box::new(Value::Str(message)))
    );
}

#[test]
fn vector_new_materialization_boundary_is_inclusive() {
    let limit = aver_rt::MAX_MATERIALIZED_VECTOR_ELEMENTS;
    assert_eq!(
        eval(&format!("Vector.len(Vector.new({limit}, 0))")),
        Value::int(limit as i64)
    );
    assert_eq!(
        eval(&format!("Vector.new({}, 0)", limit + 1)),
        Value::Err(Box::new(Value::Str(aver_rt::vector_size_error_message())))
    );
}

#[test]
fn int_to_float() {
    assert_eq!(eval("Float.fromInt(5)"), Value::Float(5.0));
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
    assert_eq!(
        eval("String.fromFloat(3.14)"),
        Value::Str("3.14".to_string())
    );
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
    assert_eq!(eval("Float.floor(3.7)"), Value::int(3));
}

#[test]
fn float_ceil() {
    assert_eq!(eval("Float.ceil(3.2)"), Value::int(4));
}

#[test]
fn float_round() {
    assert_eq!(eval("Float.round(3.5)"), Value::int(4));
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
fn string_len() {
    assert_eq!(eval("String.len(\"hello\")"), Value::int(5));
}

#[test]
fn string_len_empty() {
    assert_eq!(eval("String.len(\"\")"), Value::int(0));
}

#[test]
fn string_byte_length() {
    assert_eq!(eval("String.byteLength(\"hello\")"), Value::int(5));
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
fn string_slice_clamps_negative_indices() {
    assert_eq!(
        eval("String.slice(\"hello\", -2, 2)"),
        Value::Str("he".to_string())
    );
    assert_eq!(
        eval("String.slice(\"hello\", 1, -1)"),
        Value::Str(String::new())
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
        list_from_vec(vec![
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
        list_from_vec(vec![
            Value::Str("h".to_string()),
            Value::Str("i".to_string()),
        ])
    );
}

#[test]
fn string_char_at_valid_index() {
    assert_eq!(
        eval("String.charAt(\"hello\", 1)"),
        Value::Some(Box::new(Value::Str("e".to_string())))
    );
}

#[test]
fn string_char_at_out_of_bounds() {
    assert_eq!(eval("String.charAt(\"hi\", 5)"), Value::None);
}

#[test]
fn string_char_at_first() {
    assert_eq!(
        eval("String.charAt(\"abc\", 0)"),
        Value::Some(Box::new(Value::Str("a".to_string())))
    );
}

// ---------------------------------------------------------------------------
// List namespace
// ---------------------------------------------------------------------------

#[test]
fn list_len() {
    assert_eq!(eval("List.len([1, 2, 3])"), Value::int(3));
}

#[test]
fn list_len_empty() {
    assert_eq!(eval("List.len([])"), Value::int(0));
}

// ---------------------------------------------------------------------------
// List operations
// ---------------------------------------------------------------------------

#[test]
fn list_empty() {
    assert_eq!(eval("[]"), list_from_vec(vec![]));
}

#[test]
fn list_int_literal() {
    assert_eq!(
        eval("[1, 2, 3]"),
        list_from_vec(vec![Value::int(1), Value::int(2), Value::int(3)])
    );
}

#[test]
fn list_string_literal() {
    assert_eq!(
        eval("[\"a\", \"b\"]"),
        list_from_vec(vec![
            Value::Str("a".to_string()),
            Value::Str("b".to_string())
        ])
    );
}

#[test]
fn prepend_adds_element_to_front() {
    assert_eq!(
        eval("List.prepend(1, [2, 3])"),
        list_from_vec(vec![Value::int(1), Value::int(2), Value::int(3)])
    );
}

#[test]
fn concat_concatenates_lists() {
    assert_eq!(
        eval("List.concat([1, 2], [3, 4])"),
        list_from_vec(vec![
            Value::int(1),
            Value::int(2),
            Value::int(3),
            Value::int(4)
        ])
    );
}

#[test]
fn take_returns_prefix() {
    assert_eq!(
        eval("List.take([1, 2, 3, 4], 2)"),
        list_from_vec(vec![Value::int(1), Value::int(2)])
    );
}

#[test]
fn take_with_negative_count_returns_empty_list() {
    assert_eq!(eval("List.take([1, 2, 3], -1)"), list_from_vec(vec![]));
}

#[test]
fn drop_skips_prefix() {
    assert_eq!(
        eval("List.drop([1, 2, 3, 4], 2)"),
        list_from_vec(vec![Value::int(3), Value::int(4)])
    );
}

#[test]
fn drop_with_negative_count_returns_original_list() {
    assert_eq!(
        eval("List.drop([1, 2, 3], -1)"),
        list_from_vec(vec![Value::int(1), Value::int(2), Value::int(3)])
    );
}

#[test]
fn reverse_returns_reversed_copy() {
    assert_eq!(
        eval("List.reverse([1, 2, 3])"),
        list_from_vec(vec![Value::int(3), Value::int(2), Value::int(1)])
    );
}

#[test]
fn list_contains_returns_true() {
    assert_eq!(eval("List.contains([1, 2, 3], 2)"), Value::Bool(true));
}

#[test]
fn list_contains_returns_false() {
    assert_eq!(eval("List.contains([1, 2, 3], 9)"), Value::Bool(false));
}

// ---------------------------------------------------------------------------
// Result.withDefault / Option.withDefault / Result.fromOption
// ---------------------------------------------------------------------------

#[test]
fn result_with_default_ok() {
    assert_eq!(eval("Result.withDefault(Result.Ok(42), 0)"), Value::int(42));
}

#[test]
fn result_with_default_err() {
    assert_eq!(
        eval("Result.withDefault(Result.Err(\"oops\"), 0)"),
        Value::int(0)
    );
}

#[test]
fn option_with_default_some() {
    assert_eq!(
        eval("Option.withDefault(Option.Some(42), 0)"),
        Value::int(42)
    );
}

#[test]
fn option_with_default_none() {
    assert_eq!(eval("Option.withDefault(Option.None, 0)"), Value::int(0));
}

#[test]
fn result_from_option_some() {
    assert_eq!(
        eval("Result.fromOption(Option.Some(42), \"missing\")"),
        Value::Ok(Box::new(Value::int(42)))
    );
}

#[test]
fn result_from_option_none() {
    assert_eq!(
        eval("Result.fromOption(Option.None, \"missing\")"),
        Value::Err(Box::new(Value::Str("missing".to_string())))
    );
}

// ---------------------------------------------------------------------------
// Tuple and Map namespace
// ---------------------------------------------------------------------------

#[test]
fn tuple_literal_runtime() {
    assert_eq!(
        eval("(1, \"x\")"),
        Value::Tuple(vec![Value::int(1), Value::Str("x".to_string())])
    );
}

#[test]
fn tuple_equality_runtime() {
    assert_eq!(eval("(1, \"x\") == (1, \"x\")"), Value::Bool(true));
}

#[test]
fn map_len_empty() {
    assert_eq!(eval("Map.len({})"), Value::int(0));
}

#[test]
fn map_set_get_has() {
    assert_eq!(
        eval("Map.has(Map.set({}, \"a\", 1), \"a\")"),
        Value::Bool(true)
    );
    assert_eq!(
        eval("Map.get(Map.set({}, \"a\", 1), \"a\")"),
        Value::Some(Box::new(Value::int(1)))
    );
}

#[test]
#[allow(clippy::mutable_key_type)]
fn map_literal_runtime() {
    let mut expected = std::collections::HashMap::new();
    expected.insert(Value::Str("a".to_string()), Value::int(1));
    expected.insert(Value::Str("b".to_string()), Value::int(2));
    assert_eq!(eval("{\"a\" => 1, \"b\" => 2}"), Value::Map(expected));
}

#[test]
fn map_get_missing_returns_none() {
    assert_eq!(eval("Map.get({}, \"missing\")"), Value::None);
}

#[test]
fn map_remove_drops_key() {
    assert_eq!(
        eval("Map.has(Map.remove(Map.set({}, \"a\", 1), \"a\"), \"a\")"),
        Value::Bool(false)
    );
}

#[test]
fn map_from_list_and_entries_roundtrip() {
    assert_eq!(
        eval("Map.keys(Map.fromList([(\"a\", 1), (\"b\", 2)]))"),
        list_from_vec(vec![
            Value::Str("a".to_string()),
            Value::Str("b".to_string()),
        ])
    );
    assert_eq!(
        eval("Map.entries(Map.fromList([(\"a\", 1), (\"b\", 2)]))"),
        list_from_vec(vec![
            Value::Tuple(vec![Value::Str("a".to_string()), Value::int(1)]),
            Value::Tuple(vec![Value::Str("b".to_string()), Value::int(2)]),
        ])
    );
}

#[test]
fn map_accepts_list_key_with_structural_hash() {
    // Aver maps now hash by value across every shape, so List<Int>
    // (and any other heap structure) participates as a key.
    assert_eq!(
        eval("Map.get(Map.set({}, [1, 2], 42), [1, 2])"),
        Value::Some(Box::new(Value::int(42)))
    );
}

#[test]
fn map_literal_accepts_list_key() {
    assert_eq!(
        eval("Map.get({[1] => 42}, [1])"),
        Value::Some(Box::new(Value::int(42)))
    );
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

#[test]
fn ok_wraps_value() {
    assert_eq!(eval("Result.Ok(42)"), Value::Ok(Box::new(Value::int(42))));
}

#[test]
fn ok_wraps_unit_singleton() {
    assert_eq!(eval("Result.Ok(Unit)"), Value::Ok(Box::new(Value::Unit)));
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
    assert_eq!(eval("Option.Some(1)"), Value::Some(Box::new(Value::int(1))));
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
    let src = "fn classify(n: Int) -> String\n    match n\n        0 -> \"zero\"\n        _ -> \"other\"\n";
    assert_eq!(
        call_fn(src, "classify", vec![Value::int(0)]),
        Value::Str("zero".to_string())
    );
}

#[test]
fn match_literal_wildcard() {
    let src = "fn classify(n: Int) -> String\n    match n\n        0 -> \"zero\"\n        _ -> \"other\"\n";
    assert_eq!(
        call_fn(src, "classify", vec![Value::int(99)]),
        Value::Str("other".to_string())
    );
}

#[test]
fn match_string_literal_dispatch() {
    let src = "fn handle(cmd: String) -> Int\n    match cmd\n        \"verack\" -> 1\n        \"tx\" -> 4\n        _ -> 0\n";
    assert_eq!(
        call_fn(src, "handle", vec![Value::Str("verack".to_string())]),
        Value::int(1)
    );
    assert_eq!(
        call_fn(src, "handle", vec![Value::Str("tx".to_string())]),
        Value::int(4)
    );
    assert_eq!(
        call_fn(src, "handle", vec![Value::Str("nope".to_string())]),
        Value::int(0)
    );
}

#[test]
fn match_ok_constructor() {
    let src = "fn unwrap(r: Result<Int, String>) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> 0\n";
    assert_eq!(
        call_fn(src, "unwrap", vec![Value::Ok(Box::new(Value::int(42)))]),
        Value::int(42)
    );
}

#[test]
fn match_err_constructor() {
    let src = "fn unwrap(r: Result<Int, String>) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> 0\n";
    assert_eq!(
        call_fn(
            src,
            "unwrap",
            vec![Value::Err(Box::new(Value::Str("fail".to_string())))]
        ),
        Value::int(0)
    );
}

#[test]
fn match_some_none() {
    let src = "fn extract(o: Option<Int>) -> Int\n    match o\n        Option.Some(v) -> v\n        Option.None -> 0\n";
    assert_eq!(
        call_fn(src, "extract", vec![Value::Some(Box::new(Value::int(7)))]),
        Value::int(7)
    );
    assert_eq!(call_fn(src, "extract", vec![Value::None]), Value::int(0));
}

#[test]
fn match_bool_literal() {
    let src = "fn yes_no(b: Bool) -> String\n    match b\n        true -> \"yes\"\n        false -> \"no\"\n";
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
    let src = "fn is_empty(xs: List<Int>) -> Bool\n    match xs\n        [] -> true\n        [_, ..rest] -> false\n";
    assert_eq!(
        call_fn(src, "is_empty", vec![list_from_vec(vec![])]),
        Value::Bool(true)
    );
    assert_eq!(
        call_fn(
            src,
            "is_empty",
            vec![list_from_vec(vec![Value::int(1), Value::int(2)])]
        ),
        Value::Bool(false)
    );
}

#[test]
fn match_list_cons_binds_head_and_tail() {
    let src = "fn score(xs: List<Int>) -> Int\n    match xs\n        [h, ..t] -> h + List.len(t)\n        [] -> 0\n";
    assert_eq!(
        call_fn(
            src,
            "score",
            vec![list_from_vec(vec![
                Value::int(5),
                Value::int(9),
                Value::int(11)
            ])]
        ),
        Value::int(7)
    );
    assert_eq!(
        call_fn(src, "score", vec![list_from_vec(vec![Value::int(5)])]),
        Value::int(5)
    );
    assert_eq!(
        call_fn(src, "score", vec![list_from_vec(vec![])]),
        Value::int(0)
    );
}

#[test]
fn match_tuple_pattern_binds_values() {
    let src = "fn sum_pair(p: Tuple<Int, Int>) -> Int\n    match p\n        (a, b) -> a + b\n        _ -> 0\n";
    assert_eq!(
        call_fn(
            src,
            "sum_pair",
            vec![Value::Tuple(vec![Value::int(2), Value::int(5)])]
        ),
        Value::int(7)
    );
}

#[test]
fn match_tuple_pattern_with_wildcard() {
    let src =
        "fn first(p: Tuple<Int, Int>) -> Int\n    match p\n        (x, _) -> x\n        _ -> 0\n";
    assert_eq!(
        call_fn(
            src,
            "first",
            vec![Value::Tuple(vec![Value::int(9), Value::int(123)])]
        ),
        Value::int(9)
    );
}

#[test]
fn match_nested_tuple_pattern() {
    let src = "fn flatten(p: Tuple<Tuple<Int, Int>, Int>) -> Int\n    match p\n        ((a, b), c) -> a + b + c\n        _ -> 0\n";
    assert_eq!(
        call_fn(
            src,
            "flatten",
            vec![Value::Tuple(vec![
                Value::Tuple(vec![Value::int(1), Value::int(2)]),
                Value::int(3)
            ])]
        ),
        Value::int(6)
    );
}

#[test]
fn tuple_pattern_arity_mismatch_falls_through() {
    let src = "fn test(p: Tuple<Int, Int>) -> Int\n    match p\n        (a, b, c) -> a + b + c\n        _ -> 42\n";
    assert_eq!(
        call_fn(
            src,
            "test",
            vec![Value::Tuple(vec![Value::int(1), Value::int(2)])]
        ),
        Value::int(42)
    );
}

// ---------------------------------------------------------------------------
// String interpolation
// ---------------------------------------------------------------------------

#[test]
fn interp_simple() {
    let src = "fn greet(name: String) -> String\n    \"Hello, {name}!\"\n";
    assert_eq!(
        call_fn(src, "greet", vec![Value::Str("Alice".to_string())]),
        Value::Str("Hello, Alice!".to_string())
    );
}

#[test]
fn interp_expression() {
    let src = "fn show(x: Int) -> String\n    \"value: {x + 1}\"\n";
    assert_eq!(
        call_fn(src, "show", vec![Value::int(4)]),
        Value::Str("value: 5".to_string())
    );
}

// ---------------------------------------------------------------------------
// Val / Var bindings in function bodies
// ---------------------------------------------------------------------------

#[test]
fn binding_used_in_body() {
    let src = "fn compute() -> Int\n    x = 10\n    y = 20\n    x + y\n";
    assert_eq!(call_fn(src, "compute", vec![]), Value::int(30));
}

// ---------------------------------------------------------------------------
// Higher-order functions: map, filter, fold
// ---------------------------------------------------------------------------

#[test]
fn prepend_builtin_adds_front() {
    assert_eq!(
        eval("List.prepend(1, [2, 3])"),
        list_from_vec(vec![Value::int(1), Value::int(2), Value::int(3)])
    );
}

#[test]
fn concat_builtin_concatenates_lists() {
    assert_eq!(
        eval("List.concat([1, 2], [3, 4])"),
        list_from_vec(vec![
            Value::int(1),
            Value::int(2),
            Value::int(3),
            Value::int(4)
        ])
    );
}

#[test]
fn reverse_builtin_flips_order() {
    assert_eq!(
        eval("List.reverse([1, 2, 3])"),
        list_from_vec(vec![Value::int(3), Value::int(2), Value::int(1)])
    );
}

#[test]
fn higher_order_apply_twice_with_function_typed_param() {
    let src = "fn applyTwice(f: Fn(Int) -> Int, x: Int) -> Int\n    f(f(x))\nfn inc(n: Int) -> Int\n    n + 1\nfn test() -> Int\n    applyTwice(inc, 10)\n";
    assert_eq!(call_fn(src, "test", vec![]), Value::int(12));
}

// ---------------------------------------------------------------------------
// Error propagation operator ?
// ---------------------------------------------------------------------------

#[test]
fn error_prop_unwraps_ok() {
    let src = "fn get_ok(r: Result<Int, String>) -> Int\n    r?\n";
    assert_eq!(
        call_fn(src, "get_ok", vec![Value::Ok(Box::new(Value::int(99)))]),
        Value::int(99)
    );
}

#[test]
fn error_prop_early_return_on_err() {
    // ? on Err causes early return: the function returns Err(e), not a crash.
    let src = "fn get_val(r: Result<Int, String>) -> Result<Int, String>\n    r?\n";
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
        call_fn(src, "double_ok", vec![Value::Ok(Box::new(Value::int(5)))]),
        Value::Ok(Box::new(Value::int(10)))
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
    let ok_ten = Value::Ok(Box::new(Value::int(10)));
    assert_eq!(call_fn(src, "chain", vec![err.clone(), ok_ten]), err);
}

// ---------------------------------------------------------------------------
// Closures
// ---------------------------------------------------------------------------

#[test]
fn closure_captures_outer_val() {
    // Note: nested function definitions are not a first-class feature in Aver.
    // This test verifies the closure capture mechanism via lambda-style usage.
    // We use map with a pre-defined function instead.
    let src = "fn double(x: Int) -> Int\n    x + x\n";
    assert_eq!(call_fn(src, "double", vec![Value::int(6)]), Value::int(12));
}

// ---------------------------------------------------------------------------
// User-defined types — sum types (type keyword)
// ---------------------------------------------------------------------------

// `run_program` is replaced by `run_program_lookup` and `vm_build` helpers.

#[test]
fn sum_type_no_arg_variant_is_variant_value() {
    let src = "type Shape\n  Circle(Float)\n  Point\np = Shape.Point\n";
    let val = run_program_lookup(src, "p");
    assert_eq!(
        val,
        Value::Variant {
            type_name: "Shape".to_string(),
            variant: "Point".to_string(),
            fields: vec![].into(),
        }
    );
}

#[test]
fn sum_type_constructor_creates_variant() {
    let src = "type Shape\n  Circle(Float)\n  Point\nc = Shape.Circle(3.25)\n";
    let val = run_program_lookup(src, "c");
    assert_eq!(
        val,
        Value::Variant {
            type_name: "Shape".to_string(),
            variant: "Circle".to_string(),
            fields: vec![Value::Float(3.25)].into(),
        }
    );
}

#[test]
fn sum_type_multi_field_constructor() {
    let src = "type Shape\n  Rect(Float, Float)\nr = Shape.Rect(3.0, 4.0)\n";
    let val = run_program_lookup(src, "r");
    assert_eq!(
        val,
        Value::Variant {
            type_name: "Shape".to_string(),
            variant: "Rect".to_string(),
            fields: vec![Value::Float(3.0), Value::Float(4.0)].into(),
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
        "  match s\n",
        "    Shape.Circle(r) -> r * r\n",
        "    Shape.Point -> 0.0\n",
    );
    let circle = Value::Variant {
        type_name: "Shape".to_string(),
        variant: "Circle".to_string(),
        fields: vec![Value::Float(5.0)].into(),
    };
    let result = call_fn(src, "area", vec![circle]);
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
        "  match s\n",
        "    Shape.Circle(r) -> r * r\n",
        "    Shape.Point -> 0.0\n",
    );
    let point = Value::Variant {
        type_name: "Shape".to_string(),
        variant: "Point".to_string(),
        fields: vec![].into(),
    };
    let result = call_fn(src, "area", vec![point]);
    assert_eq!(result, Value::Float(0.0));
}

// ---------------------------------------------------------------------------
// User-defined types — records (record keyword)
// ---------------------------------------------------------------------------

#[test]
fn record_creation_stores_fields() {
    let src = "record User\n  name: String\n  age: Int\nu = User(name = \"Alice\", age = 30)\n";
    let val = run_program_lookup(src, "u");
    assert_eq!(
        val,
        Value::Record {
            type_name: "User".to_string(),
            fields: vec![
                ("name".to_string(), Value::Str("Alice".to_string())),
                ("age".to_string(), Value::int(30)),
            ]
            .into(),
        }
    );
}

#[test]
fn record_creation_canonicalizes_field_order() {
    let src = "record User\n  name: String\n  age: Int\nu = User(age = 30, name = \"Alice\")\n";
    let val = run_program_lookup(src, "u");
    assert_eq!(
        val,
        Value::Record {
            type_name: "User".to_string(),
            fields: vec![
                ("name".to_string(), Value::Str("Alice".to_string())),
                ("age".to_string(), Value::int(30)),
            ]
            .into(),
        }
    );
}

#[test]
fn record_field_access() {
    let src = "record User\n  name: String\n  age: Int\nu = User(name = \"Alice\", age = 30)\nn = u.name\n";
    let val = run_program_lookup(src, "n");
    assert_eq!(val, Value::Str("Alice".to_string()));
}

#[test]
fn record_match_binding_preserves_field_access() {
    let src = concat!(
        "record User\n",
        "  name: String\n",
        "  age: Int\n",
        "fn get_name(u: User) -> String\n",
        "  ? \"get name\"\n",
        "  match u\n",
        "    user -> user.name\n",
    );
    let user = Value::Record {
        type_name: "User".to_string(),
        fields: vec![
            ("name".to_string(), Value::Str("Bob".to_string())),
            ("age".to_string(), Value::int(25)),
        ]
        .into(),
    };
    let result = call_fn(src, "get_name", vec![user]);
    assert_eq!(result, Value::Str("Bob".to_string()));
}

#[test]
fn sum_type_variant_equality() {
    let c1 = Value::Variant {
        type_name: "Shape".to_string(),
        variant: "Circle".to_string(),
        fields: vec![Value::Float(3.0)].into(),
    };
    let c2 = Value::Variant {
        type_name: "Shape".to_string(),
        variant: "Circle".to_string(),
        fields: vec![Value::Float(3.0)].into(),
    };
    let c3 = Value::Variant {
        type_name: "Shape".to_string(),
        variant: "Circle".to_string(),
        fields: vec![Value::Float(5.0)].into(),
    };
    assert!(c1 == c2);
    assert!(c1 != c3);
}

// ---------------------------------------------------------------------------
// Http builtins — local TcpListener, no internet required
// ---------------------------------------------------------------------------

mod http_tests {
    use super::*;
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
        call_fn_with_effects(src, fn_name, vec![]).expect("call failed")
    }

    #[test]
    #[ignore = "integration: starts a local HTTP server; run with --include-ignored --test-threads=1"]
    fn http_get_200_returns_ok_response() {
        let Some(url) = start_server(200, "hello", "") else {
            return;
        };
        let src = format!(
            "fn fetch() -> Result<Http.Response, String>\n    ! [Http.get]\n    Http.get(\"{}\")\n",
            url
        );
        let val = run_http_fn(&src, "fetch");
        match val {
            Value::Ok(inner) => match *inner {
                Value::Record {
                    type_name,
                    ref fields,
                } => {
                    assert_eq!(type_name, "Http.Response");
                    let status = fields.iter().find(|(k, _)| k == "status").map(|(_, v)| v);
                    assert_eq!(status, Some(&Value::int(200)));
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
            "fn fetch() -> Result<Http.Response, String>\n    ! [Http.get]\n    Http.get(\"{}\")\n",
            url
        );
        let val = run_http_fn(&src, "fetch");
        match val {
            Value::Ok(inner) => match *inner {
                Value::Record { ref fields, .. } => {
                    let status = fields.iter().find(|(k, _)| k == "status").map(|(_, v)| v);
                    assert_eq!(status, Some(&Value::int(404)));
                }
                other => panic!("expected Record, got {:?}", other),
            },
            other => panic!("expected Ok for 4xx, got {:?}", other),
        }
    }

    #[test]
    fn http_get_transport_error_returns_err() {
        // Port 1 is almost certainly not listening
        let src = "fn fetch() -> Result<Http.Response, String>\n    ! [Http.get]\n    Http.get(\"http://127.0.0.1:1/\")\n";
        let val = call_fn_with_effects(src, "fetch", vec![]).expect("call itself should not panic");
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
            "fn send() -> Result<Http.Response, String>\n    ! [Http.post]\n    Http.post(\"{}\", \"data\", \"text/plain\", [])\n",
            url
        );
        let val = run_http_fn(&src, "send");
        match val {
            Value::Ok(inner) => match *inner {
                Value::Record { ref fields, .. } => {
                    let status = fields.iter().find(|(k, _)| k == "status").map(|(_, v)| v);
                    assert_eq!(status, Some(&Value::int(201)));
                }
                other => panic!("expected Record, got {:?}", other),
            },
            other => panic!("expected Ok, got {:?}", other),
        }
    }

    #[test]
    fn http_post_bad_headers_returns_runtime_error() {
        // Pass a non-list for headers — validation fails before any HTTP call
        let src = "fn send() -> Result<Http.Response, String>\n    ! [Http.post]\n    Http.post(\"http://127.0.0.1:1/\", \"\", \"text/plain\", \"bad\")\n";
        let result = call_fn_with_effects(src, "send", vec![]);
        assert!(result.is_err(), "expected RuntimeError for bad headers");
    }
}

// ---------------------------------------------------------------------------
// Disk capability operations
// ---------------------------------------------------------------------------

mod disk_tests {
    use super::*;
    use std::io::Write;

    fn run_disk_fn(src: &str, fn_name: &str) -> Value {
        call_fn_with_effects(src, fn_name, vec![]).expect("call failed")
    }

    fn tmp_path(name: &str) -> std::path::PathBuf {
        std::env::temp_dir().join(format!("aver_disk_test_{}", name))
    }

    #[test]
    fn disk_write_and_read_text() {
        let path = tmp_path("write_read.txt");
        let path_str = path.to_string_lossy();
        let src = format!(
            "fn run() -> Result<Unit, String>\n    ! [Disk.writeText]\n    Disk.writeText(\"{}\", \"hello\")\n",
            path_str.replace('\\', "\\\\")
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));

        let src2 = format!(
            "fn run() -> Result<String, String>\n    ! [Disk.readText]\n    Disk.readText(\"{}\")\n",
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
            "fn run() -> Result<Unit, String>\n    ! [Disk.appendText]\n    Disk.appendText(\"{}\", \" world\")\n",
            path_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));
        let content = std::fs::read_to_string(&path).unwrap();
        assert_eq!(content, "hello world");
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn disk_byte_operations_preserve_non_utf8_octets_exactly() {
        let path = tmp_path("bytes.bin");
        let path_str = path.to_string_lossy().replace('\\', "\\\\");

        let write = format!(
            "record Bytes\n    values: List<Int>\n\nfn run() -> Result<Unit, String>\n    ! [Disk.writeBytes]\n    Disk.writeBytes(\"{}\", Bytes(values = [0, 127, 128, 255]))\n",
            path_str
        );
        assert_eq!(run_disk_fn(&write, "run"), Value::Ok(Box::new(Value::Unit)));
        assert_eq!(std::fs::read(&path).unwrap(), vec![0, 127, 128, 255]);

        let append = format!(
            "record Bytes\n    values: List<Int>\n\nfn run() -> Result<Unit, String>\n    ! [Disk.appendBytes]\n    Disk.appendBytes(\"{}\", Bytes(values = [1, 2]))\n",
            path_str
        );
        assert_eq!(
            run_disk_fn(&append, "run"),
            Value::Ok(Box::new(Value::Unit))
        );
        assert_eq!(std::fs::read(&path).unwrap(), vec![0, 127, 128, 255, 1, 2]);

        let read = format!(
            "record Bytes\n    values: List<Int>\n\nfn run() -> Result<Bytes, String>\n    ! [Disk.readBytes]\n    Disk.readBytes(\"{}\")\n",
            path_str
        );
        let Value::Ok(payload) = run_disk_fn(&read, "run") else {
            panic!("expected successful byte read");
        };
        let Value::Record { fields, .. } = *payload else {
            panic!("expected Bytes record");
        };
        let values = fields
            .iter()
            .find_map(|(name, value)| (name == "values").then_some(value))
            .and_then(list_to_vec)
            .expect("Bytes.values list");
        assert_eq!(
            values,
            vec![
                Value::int(0),
                Value::int(127),
                Value::int(128),
                Value::int(255),
                Value::int(1),
                Value::int(2),
            ]
        );

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn disk_exists_true_and_false() {
        let path = tmp_path("exists.txt");
        let path_str = path.to_string_lossy().replace('\\', "\\\\");
        std::fs::write(&path, "x").unwrap();

        let src = format!(
            "fn run() -> Bool\n    ! [Disk.exists]\n    Disk.exists(\"{}\")\n",
            path_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Bool(true));
        let _ = std::fs::remove_file(&path);

        let missing_path = tmp_path("does_not_exist_xyz.txt");
        let missing_str = missing_path.to_string_lossy().replace('\\', "\\\\");
        let src2 = format!(
            "fn run() -> Bool\n    ! [Disk.exists]\n    Disk.exists(\"{}\")\n",
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
            "fn run() -> Result<Unit, String>\n    ! [Disk.delete]\n    Disk.delete(\"{}\")\n",
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
            "fn run() -> Result<Unit, String>\n    ! [Disk.delete]\n    Disk.delete(\"{}\")\n",
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
            "fn run() -> Result<Unit, String>\n    ! [Disk.deleteDir]\n    Disk.deleteDir(\"{}\")\n",
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
            "fn run() -> Result<Unit, String>\n    ! [Disk.deleteDir]\n    Disk.deleteDir(\"{}\")\n",
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
            "fn run() -> Result<Unit, String>\n    ! [Disk.delete]\n    Disk.delete(\"{}\")\n",
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
            "fn run() -> Result<Unit, String>\n    ! [Disk.makeDir]\n    Disk.makeDir(\"{}\")\n",
            dir_str
        );
        let val = run_disk_fn(&src, "run");
        assert_eq!(val, Value::Ok(Box::new(Value::Unit)));
        assert!(dir.exists());

        // Write a file inside to list it
        std::fs::write(dir.join("a.txt"), "").unwrap();
        let src2 = format!(
            "fn run() -> Result<List<String>, String>\n    ! [Disk.listDir]\n    Disk.listDir(\"{}\")\n",
            dir_str
        );
        let val2 = run_disk_fn(&src2, "run");
        match val2 {
            Value::Ok(inner) => match *inner {
                list if list_to_vec(&list).is_some() => {
                    let items = list_to_vec(&list).expect("checked above");
                    assert!(items.contains(&Value::Str("a.txt".to_string())));
                }
                other => panic!("expected List, got {:?}", other),
            },
            other => panic!("expected Ok, got {:?}", other),
        }

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn disk_sync_accepts_a_file_and_a_directory_and_refuses_an_absent_path() {
        let dir = tmp_path("sync_dir");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("create sync fixture dir");
        let file = dir.join("payload.bin");
        std::fs::write(&file, [0, 127, 128, 255]).expect("write sync fixture");

        let sync_of = |path: &std::path::Path| {
            let src = format!(
                "fn run() -> Result<Unit, String>\n    ! [Disk.sync]\n    Disk.sync(\"{}\")\n",
                path.to_string_lossy().replace('\\', "\\\\")
            );
            run_disk_fn(&src, "run")
        };

        // The file half flushes the bytes; the directory half is what makes
        // a newly created file's own name durable on POSIX. A read-only open
        // is what lets the same operation accept both.
        assert_eq!(sync_of(&file), Value::Ok(Box::new(Value::Unit)));
        assert_eq!(sync_of(&dir), Value::Ok(Box::new(Value::Unit)));
        assert!(matches!(sync_of(&dir.join("absent.bin")), Value::Err(_)));
        assert_eq!(
            std::fs::read(&file).expect("read synced payload"),
            vec![0, 127, 128, 255],
            "Disk.sync must not rewrite the file it flushes"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn disk_read_missing_file_returns_err() {
        let src = "fn run() -> Result<String, String>\n    ! [Disk.readText]\n    Disk.readText(\"/no/such/file.txt\")\n";
        let val = run_disk_fn(src, "run");
        assert!(matches!(val, Value::Err(_)));
    }

    #[test]
    fn runtime_gate_blocks_disk_read_without_effect() {
        // Call Disk.readText from top-level (no effect grant) → runtime gate fires
        let result = try_eval("Disk.readText(\"x\")");
        let msg = result.expect_err("expected runtime gate error");
        assert!(msg.contains("effect"), "got: {}", msg);
        assert!(msg.contains("Disk"), "got: {}", msg);
    }
}

// ---------------------------------------------------------------------------
// Console service tests
// ---------------------------------------------------------------------------

mod console_tests {
    use super::*;

    fn run_console_fn(src: &str, fn_name: &str) -> Value {
        call_fn_with_effects(src, fn_name, vec![]).expect("call failed")
    }

    #[test]
    fn console_error_returns_unit() {
        let src = concat!(
            "fn run() -> Unit\n",
            "    ! [Console.error]\n",
            "    Console.error(\"oops\")\n",
        );
        let val = run_console_fn(src, "run");
        assert_eq!(val, Value::Unit);
    }

    #[test]
    fn console_warn_returns_unit() {
        let src = concat!(
            "fn run() -> Unit\n",
            "    ! [Console.warn]\n",
            "    Console.warn(\"careful\")\n",
        );
        let val = run_console_fn(src, "run");
        assert_eq!(val, Value::Unit);
    }

    #[test]
    fn console_error_rejects_unit_at_the_provider_boundary() {
        // This source deliberately bypasses typechecking. The provider boundary
        // must still enforce Console.error(String) instead of retaining the old
        // service adapter's silent Unit coercion.
        let src = concat!(
            "fn run() -> Unit\n",
            "    ! [Console.error, Console.print]\n",
            "    Console.error(Console.print(\"setup\"))\n",
        );
        let err = call_fn_with_effects(src, "run", vec![]).expect_err("Unit must be rejected");
        assert!(err.contains("expected String, got Unit"), "got: {err}");
    }

    #[test]
    fn console_warn_rejects_unit_at_the_provider_boundary() {
        let src = concat!(
            "fn run() -> Unit\n",
            "    ! [Console.warn, Console.print]\n",
            "    Console.warn(Console.print(\"setup\"))\n",
        );
        let err = call_fn_with_effects(src, "run", vec![]).expect_err("Unit must be rejected");
        assert!(err.contains("expected String, got Unit"), "got: {err}");
    }

    #[test]
    fn runtime_gate_blocks_console_error_without_effect() {
        let result = try_eval("Console.error(\"x\")");
        let msg = result.expect_err("expected runtime gate error");
        assert!(msg.contains("effect"), "got: {}", msg);
        assert!(msg.contains("Console"), "got: {}", msg);
    }
}

// ---------------------------------------------------------------------------
// Time service tests
// ---------------------------------------------------------------------------

mod time_tests {
    use super::*;

    fn run_time_fn(src: &str, fn_name: &str) -> Value {
        call_fn_with_effects(src, fn_name, vec![]).expect("call failed")
    }

    #[test]
    fn time_now_returns_string() {
        let src = concat!(
            "fn now() -> String\n",
            "    ! [Time.now]\n",
            "    Time.now()\n",
        );
        let val = run_time_fn(src, "now");
        match val {
            Value::Str(s) => {
                assert!(!s.is_empty(), "expected non-empty timestamp");
                assert!(
                    s.contains('T') && s.ends_with('Z'),
                    "unexpected format: {}",
                    s
                );
            }
            other => panic!("expected String, got {:?}", other),
        }
    }

    #[test]
    fn time_unix_ms_returns_int() {
        let src = concat!(
            "fn nowMs() -> Int\n",
            "    ! [Time.unixMs]\n",
            "    Time.unixMs()\n",
        );
        let val = run_time_fn(src, "nowMs");
        match val {
            Value::Int(ms) => assert!(
                ms > aver_rt::AverInt::zero(),
                "expected positive unix ms, got {}",
                ms
            ),
            other => panic!("expected Int, got {:?}", other),
        }
    }

    #[test]
    fn time_sleep_negative_returns_catchable_error() {
        let src = concat!(
            "fn wait() -> Result<Unit, String>\n",
            "    ! [Time.sleep]\n",
            "    Time.sleep(0 - 1)\n",
        );
        let value = call_fn_with_effects(src, "wait", vec![]).expect("call should return Result");
        assert_eq!(
            value,
            Value::Err(Box::new(Value::Str(
                "Time.sleep: ms must be non-negative".to_string()
            )))
        );
    }

    #[test]
    fn time_sleep_valid_literal_runs_without_result_ceremony() {
        let src = concat!(
            "fn wait() -> Unit\n",
            "    ! [Time.sleep]\n",
            "    Time.sleep(0)\n",
        );
        let value = call_fn_with_effects(src, "wait", vec![]).expect("literal sleep should run");
        assert_eq!(value, Value::Unit);
    }

    #[test]
    fn runtime_gate_blocks_time_now_without_effect() {
        let result = try_eval("Time.now()");
        let msg = result.expect_err("expected runtime gate error");
        assert!(msg.contains("effect"), "got: {}", msg);
        assert!(msg.contains("Time.now"), "got: {}", msg);
    }
}

// ---------------------------------------------------------------------------
// Env service tests
// ---------------------------------------------------------------------------

mod env_tests {
    use super::*;
    use aver::config::ProjectConfig;

    fn unique_key(prefix: &str) -> String {
        let ts = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        format!("AVER_TEST_{}_{}_{}", prefix, std::process::id(), ts)
    }

    fn run_env_fn(src: &str, fn_name: &str) -> Value {
        call_fn_with_effects(src, fn_name, vec![]).expect("call failed")
    }

    #[test]
    fn env_get_missing_returns_none() {
        let key = unique_key("MISSING");
        let src = format!(
            "fn read() -> Option<String>\n    ! [Env.get]\n    Env.get(\"{}\")\n",
            key
        );
        let val = run_env_fn(&src, "read");
        assert_eq!(val, Value::None);
    }

    #[test]
    fn env_set_then_get_returns_some() {
        let key = unique_key("SET");
        let src = format!(
            concat!(
                "fn run() -> Result<Option<String>, String>\n",
                "    ! [Env.set, Env.get]\n",
                "    Env.set(\"{k}\", \"ok\")?\n",
                "    Result.Ok(Env.get(\"{k}\"))\n"
            ),
            k = key
        );
        let val = run_env_fn(&src, "run");
        assert_eq!(
            val,
            Value::Ok(Box::new(Value::Some(Box::new(Value::Str(
                "ok".to_string()
            )))))
        );
    }

    #[test]
    fn runtime_gate_blocks_env_get_without_effect() {
        let result = try_eval("Env.get(\"HOME\")");
        let msg = result.expect_err("expected runtime gate error");
        assert!(msg.contains("effect"), "got: {}", msg);
        assert!(msg.contains("Env.get"), "got: {}", msg);
    }

    #[test]
    fn runtime_policy_blocks_env_key() {
        let key = unique_key("DENY");
        let src = format!(
            concat!(
                "fn run() -> Result<Unit, String>\n",
                "    ! [Env.set]\n",
                "    Env.set(\"{k}\", \"ok\")\n"
            ),
            k = key
        );
        let items = parse(&src);
        let mut machine = vm_compile(&items);
        machine.set_runtime_policy(
            ProjectConfig::parse(
                r#"
[effects.Env]
keys = ["SAFE_*"]
"#,
            )
            .expect("parse policy"),
        );
        machine.run_top_level().expect("top-level failed");

        // run_named_function auto-sets allowed effects from the function's metadata
        let err = machine
            .run_named_function("run", &[])
            .expect_err("expected policy denial");
        let msg = err.to_string();
        assert!(msg.contains("denied by aver.toml policy"), "got: {}", msg);
        assert!(msg.contains("Env.set"), "got: {}", msg);
    }

    #[test]
    fn runtime_policy_blocks_disk_path_through_the_capability_door() {
        // Disk moved onto a provider-backed standard capability; the
        // aver.toml sandbox must survive that move. The write below
        // targets a path outside the allow-list, so the provider must
        // never see it.
        let src = "fn run() -> Result<Unit, String>\n    ! [Disk.writeText]\n    Disk.writeText(\"/definitely/outside/allow.txt\", \"x\")\n";
        let items = parse(src);
        let mut machine = vm_compile(&items);
        machine.set_runtime_policy(
            ProjectConfig::parse(
                r#"
[effects.Disk]
paths = ["./data/**"]
"#,
            )
            .expect("parse policy"),
        );
        machine.run_top_level().expect("top-level failed");

        let err = machine
            .run_named_function("run", &[])
            .expect_err("expected policy denial");
        let msg = err.to_string();
        assert!(msg.contains("denied by aver.toml policy"), "got: {}", msg);
        assert!(msg.contains("Disk.writeText"), "got: {}", msg);
    }

    #[test]
    fn runtime_policy_blocks_disk_byte_path_through_the_capability_door() {
        let src = "record Bytes\n    values: List<Int>\n\nfn run() -> Result<Unit, String>\n    ! [Disk.writeBytes]\n    Disk.writeBytes(\"/definitely/outside/allow.bin\", Bytes(values = [0, 255]))\n";
        let items = parse(src);
        let mut machine = vm_compile(&items);
        machine.set_runtime_policy(
            ProjectConfig::parse(
                r#"
[effects.Disk]
paths = ["./data/**"]
"#,
            )
            .expect("parse policy"),
        );
        machine.run_top_level().expect("top-level failed");

        let err = machine
            .run_named_function("run", &[])
            .expect_err("expected policy denial");
        let msg = err.to_string();
        assert!(msg.contains("denied by aver.toml policy"), "got: {}", msg);
        assert!(msg.contains("Disk.writeBytes"), "got: {}", msg);
    }

    #[test]
    fn runtime_policy_blocks_disk_sync_through_the_capability_door() {
        let src = "fn run() -> Result<Unit, String>\n    ! [Disk.sync]\n    Disk.sync(\"/definitely/outside/allow.bin\")\n";
        let items = parse(src);
        let mut machine = vm_compile(&items);
        machine.set_runtime_policy(
            ProjectConfig::parse(
                r#"
[effects.Disk]
paths = ["./data/**"]
"#,
            )
            .expect("parse policy"),
        );
        machine.run_top_level().expect("top-level failed");

        let err = machine
            .run_named_function("run", &[])
            .expect_err("expected policy denial");
        let msg = err.to_string();
        assert!(msg.contains("denied by aver.toml policy"), "got: {}", msg);
        assert!(msg.contains("Disk.sync"), "got: {}", msg);
    }
}

// ---------------------------------------------------------------------------
// Independent product runtime policy
// ---------------------------------------------------------------------------

mod independence_runtime_tests {
    use super::*;
    use aver::config::ProjectConfig;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_file_path(tag: &str) -> PathBuf {
        let ts = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock went backwards")
            .as_nanos();
        std::env::temp_dir().join(format!(
            "aver_eval_independence_{}_{}_{}.txt",
            tag,
            std::process::id(),
            ts
        ))
    }

    fn marker_count(path: &Path) -> usize {
        match std::fs::read_to_string(path) {
            Ok(content) => content.lines().count(),
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => 0,
            Err(err) => panic!("failed to read {}: {}", path.display(), err),
        }
    }

    fn run_with_mode(mode: &str, marker_path: &Path) -> (Value, usize) {
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
        let _ = std::fs::remove_file(marker_path);

        let items = parse(&src);
        let mut machine = vm_compile(&items);
        machine.set_runtime_policy(
            ProjectConfig::parse(&format!("[independence]\nmode = \"{mode}\"\n"))
                .expect("parse policy"),
        );
        machine.run_top_level().expect("top-level failed");

        // run_named_function auto-sets allowed effects from the function's metadata
        let value = machine
            .run_named_function("main", &[])
            .expect("call failed")
            .to_value(&machine.arena);
        let count = marker_count(marker_path);
        let _ = std::fs::remove_file(marker_path);
        (value, count)
    }

    #[test]
    fn vm_cancel_mode_for_independent_products() {
        let marker_path = temp_file_path("cancel_policy");

        let (complete_value, complete_count) = run_with_mode("complete", &marker_path);
        let (cancel_value, cancel_count) = run_with_mode("cancel", &marker_path);

        assert_eq!(
            complete_value,
            Value::Err(Box::new(Value::Str("boom".to_string())))
        );
        assert_eq!(
            cancel_value,
            Value::Err(Box::new(Value::Str("boom".to_string())))
        );
        assert_eq!(complete_count, 64);
        // VM dispatches independent products with CALL_PAR; cancel mode
        // may or may not shorten sibling work depending on scheduling.
        assert!(
            cancel_count <= 64,
            "expected cancel_count <= 64, got {}",
            cancel_count
        );
    }
}

// ---------------------------------------------------------------------------
// Tcp builtins
// ---------------------------------------------------------------------------

mod tcp_tests {
    use super::*;

    fn run_tcp_fn(src: &str, fn_name: &str) -> Value {
        call_fn_with_effects(src, fn_name, vec![]).expect("call failed")
    }

    #[test]
    fn tcp_ping_unreachable_returns_err() {
        // Port 1 is almost certainly not listening.
        let src = concat!(
            "fn check() -> Result<Unit, String>\n",
            "    ! [Tcp.ping]\n",
            "    Tcp.ping(\"127.0.0.1\", 1)\n",
        );
        let val = run_tcp_fn(src, "check");
        assert!(matches!(val, Value::Err(_)), "expected Err, got {:?}", val);
    }

    #[test]
    fn tcp_send_unreachable_returns_err() {
        let src = concat!(
            "fn talk() -> Result<String, String>\n",
            "    ! [Tcp.send]\n",
            "    Tcp.send(\"127.0.0.1\", 1, \"hello\")\n",
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
            "fn check() -> Result<Unit, String>\n    ! [Tcp.ping]\n    Tcp.ping(\"127.0.0.1\", {})\n",
            port
        );
        let val = run_tcp_fn(&src, "check");
        assert!(matches!(val, Value::Ok(_)), "expected Ok, got {:?}", val);
    }

    #[test]
    #[ignore = "integration: starts a local TCP server; run with --include-ignored --test-threads=1"]
    fn tcp_connect_returns_provider_resource() {
        use std::net::TcpListener;
        use std::thread;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            let _ = listener.accept();
        });

        let src = format!(
            "fn open() -> Result<Tcp.Connection, String>\n    ! [Tcp.connect]\n    Tcp.connect(\"127.0.0.1\", {})\n",
            port
        );
        let val = run_tcp_fn(&src, "open");
        match val {
            Value::Ok(inner) => assert!(matches!(*inner, Value::CapabilityResource(_))),
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
            "fn talk() -> Result<String, String>\n    ! [Tcp.send]\n    Tcp.send(\"127.0.0.1\", {}, \"echo me\")\n",
            port
        );
        let val = run_tcp_fn(&src, "talk");
        match val {
            Value::Ok(inner) => assert_eq!(*inner, Value::Str("echo me".to_string())),
            other => panic!("expected Ok(\"echo me\"), got {:?}", other),
        }
    }

    /// Regression: `Tcp.send` decodes the response with
    /// `String::from_utf8_lossy`, so every non-UTF-8 byte comes back as U+FFFD
    /// and the original is unrecoverable. `Tcp.sendBytes` must round-trip the
    /// bytes untouched. The payload here is the Bitcoin mainnet magic
    /// (`F9 BE B4 D9`) — four bytes that are each invalid UTF-8 for a different
    /// reason, so a regression on any decode path shows up here.
    #[test]
    #[ignore = "integration: starts a local TCP server; run with --include-ignored --test-threads=1"]
    fn tcp_send_bytes_round_trips_non_utf8() {
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
            "record Bytes\n    values: List<Int>\n\nfn talk() -> Result<Bytes, String>\n    ! [Tcp.sendBytes]\n    Tcp.sendBytes(\"127.0.0.1\", {}, Bytes(values = [249, 190, 180, 217]))\n",
            port
        );
        match run_tcp_fn(&src, "talk") {
            Value::Ok(inner) => match *inner {
                Value::Record { type_name, fields } => {
                    assert_eq!(type_name, "Bytes");
                    let items = match fields.iter().find(|(name, _)| name == "values") {
                        Some((_, Value::List(items))) => items,
                        other => panic!("expected Bytes.values list, got {:?}", other),
                    };
                    let got: Vec<i64> = items
                        .iter()
                        .map(|v| match v {
                            Value::Int(n) => n.to_i64().unwrap(),
                            other => panic!("expected Int element, got {:?}", other),
                        })
                        .collect();
                    assert_eq!(got, vec![249, 190, 180, 217]);
                }
                other => panic!("expected Bytes, got {:?}", other),
            },
            other => panic!("expected Ok(Bytes), got {:?}", other),
        }
    }

    /// Public code cannot construct this value: `Bytes.fromList` rejects it.
    /// If an unchecked VM caller forges one anyway, the provider codec fails
    /// before dispatch. That is a runtime boundary error, not a
    /// `Tcp.sendBytes` operation-level `Result.Err`.
    #[test]
    fn tcp_send_bytes_defensively_rejects_out_of_range_carrier() {
        let src = concat!(
            "record Bytes\n    values: List<Int>\n\n",
            "fn talk() -> Result<Bytes, String>\n",
            "    ! [Tcp.sendBytes]\n",
            "    Tcp.sendBytes(\"127.0.0.1\", 1, Bytes(values = [65, 256]))\n",
        );
        let error = call_fn_with_effects(src, "talk", vec![])
            .expect_err("a malformed Bytes carrier must fail at the provider boundary");
        assert!(
            error.contains("capability provider boundary")
                && error.contains("256")
                && error.contains("index 1"),
            "error should name the boundary, offending byte, and index, got: {error}"
        );
    }

    /// Regression: `Tcp.writeLine` appends `\r\n` and UTF-8-encodes its
    /// `String`, so `0xF9` reaches the wire as `C3 B9` followed by two bytes
    /// nobody asked for. `Tcp.writeBytes` must put exactly the given bytes on
    /// the socket. The payload here is the Bitcoin mainnet magic plus a length
    /// prefix and an embedded `0x0A`.
    #[test]
    #[ignore = "integration: starts a local TCP server; run with --include-ignored --test-threads=1"]
    fn tcp_write_bytes_puts_exact_bytes_on_the_wire() {
        use std::io::Read;
        use std::net::TcpListener;
        use std::sync::{Arc, Mutex};
        use std::thread;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let seen = Arc::new(Mutex::new(Vec::new()));
        let seen_w = Arc::clone(&seen);
        let handle = thread::spawn(move || {
            if let Ok((mut stream, _)) = listener.accept() {
                let mut buf = Vec::new();
                stream.read_to_end(&mut buf).ok();
                *seen_w.lock().unwrap() = buf;
            }
        });

        let src = format!(
            "record Bytes\n    values: List<Int>\n\nfn talk() -> Result<Unit, String>\n    ! [Tcp.connect, Tcp.writeBytes, Tcp.close]\n    conn = Tcp.connect(\"127.0.0.1\", {})?\n    payload = Bytes(values = [249, 190, 180, 217, 5, 0, 0, 0, 1, 10, 255])\n    _w = Tcp.writeBytes(conn, payload)?\n    Tcp.close(conn)\n",
            port
        );
        match run_tcp_fn(&src, "talk") {
            Value::Ok(_) => {}
            other => panic!("expected Ok, got {:?}", other),
        }
        handle.join().unwrap();
        assert_eq!(
            *seen.lock().unwrap(),
            vec![249, 190, 180, 217, 5, 0, 0, 0, 1, 10, 255],
            "writeBytes must append nothing and encode nothing"
        );
    }

    /// Out-of-range bytes are rejected before any wire I/O, so a bad payload
    /// never half-writes. Uses a real connection because `Tcp.Connection` is
    /// opaque — Aver source cannot construct one.
    #[test]
    #[ignore = "integration: starts a local TCP server; run with --include-ignored --test-threads=1"]
    fn tcp_write_bytes_rejects_out_of_range_byte() {
        use std::io::Read;
        use std::net::TcpListener;
        use std::sync::{Arc, Mutex};
        use std::thread;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let seen = Arc::new(Mutex::new(Vec::new()));
        let seen_w = Arc::clone(&seen);
        let handle = thread::spawn(move || {
            if let Ok((mut stream, _)) = listener.accept() {
                let mut buf = Vec::new();
                stream.read_to_end(&mut buf).ok();
                *seen_w.lock().unwrap() = buf;
            }
        });

        let src = format!(
            "record Bytes\n    values: List<Int>\n\nfn talk() -> Result<Unit, String>\n    ! [Tcp.connect, Tcp.writeBytes, Tcp.close]\n    conn = Tcp.connect(\"127.0.0.1\", {})?\n    r = Tcp.writeBytes(conn, Bytes(values = [65, 256]))\n    _c = Tcp.close(conn)?\n    r\n",
            port
        );
        match run_tcp_fn(&src, "talk") {
            Value::Err(inner) => match *inner {
                Value::Str(msg) => assert!(
                    msg.contains("256") && msg.contains("index 1"),
                    "error should name the offending byte and its index, got: {msg}"
                ),
                other => panic!("expected Str error, got {:?}", other),
            },
            other => panic!("expected Err, got {:?}", other),
        }
        handle.join().unwrap();
        assert!(
            seen.lock().unwrap().is_empty(),
            "a rejected payload must not put any bytes on the wire"
        );
    }

    /// Regression: `Tcp.readLine` frames on `\n` and rejects non-UTF-8, so
    /// neither half of a length-prefixed binary frame survives it.
    /// `Tcp.readBytes` must return exactly the bytes asked for. The payload
    /// carries `0x0A` twice (which `readLine` would split on) and `0xFF`
    /// (which it would reject outright).
    #[test]
    #[ignore = "integration: starts a local TCP server; run with --include-ignored --test-threads=1"]
    fn tcp_read_bytes_round_trips_binary_frame() {
        use std::io::Write;
        use std::net::TcpListener;
        use std::thread;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            if let Ok((mut stream, _)) = listener.accept() {
                stream
                    .write_all(&[0xF9, 0xBE, 0xB4, 0xD9, 0x01, 0x0A, 0xFF, 0x0A, 0x03])
                    .ok();
                thread::sleep(std::time::Duration::from_millis(500));
            }
        });

        let src = format!(
            "record Bytes\n    values: List<Int>\n\nfn talk() -> Result<Bytes, String>\n    ! [Tcp.connect, Tcp.readBytes]\n    conn = Tcp.connect(\"127.0.0.1\", {})?\n    header = Tcp.readBytes(conn, 4)?\n    payload = Tcp.readBytes(conn, 5)?\n    Result.Ok(Bytes(values = List.concat(header.values, payload.values)))\n",
            port
        );
        match run_tcp_fn(&src, "talk") {
            Value::Ok(inner) => match *inner {
                Value::Record { type_name, fields } => {
                    assert_eq!(type_name, "Bytes");
                    let items = match fields.iter().find(|(name, _)| name == "values") {
                        Some((_, Value::List(items))) => items,
                        other => panic!("expected Bytes.values list, got {:?}", other),
                    };
                    let got: Vec<i64> = items
                        .iter()
                        .map(|v| match v {
                            Value::Int(n) => n.to_i64().unwrap(),
                            other => panic!("expected Int element, got {:?}", other),
                        })
                        .collect();
                    assert_eq!(got, vec![249, 190, 180, 217, 1, 10, 255, 10, 3]);
                }
                other => panic!("expected Bytes, got {:?}", other),
            },
            other => panic!("expected Ok(Bytes), got {:?}", other),
        }
    }

    /// A short read is an error, not a truncated success: fewer bytes than the
    /// length prefix promised means the peer went away mid-frame, and returning
    /// a partial frame would desynchronise the caller's parser.
    #[test]
    #[ignore = "integration: starts a local TCP server; run with --include-ignored --test-threads=1"]
    fn tcp_read_bytes_short_read_is_an_error() {
        use std::io::Write;
        use std::net::TcpListener;
        use std::thread;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            if let Ok((mut stream, _)) = listener.accept() {
                stream.write_all(&[1, 2, 3]).ok();
            }
        });

        let src = format!(
            "record Bytes\n    values: List<Int>\n\nfn talk() -> Result<Bytes, String>\n    ! [Tcp.connect, Tcp.readBytes]\n    conn = Tcp.connect(\"127.0.0.1\", {})?\n    Tcp.readBytes(conn, 10)\n",
            port
        );
        match run_tcp_fn(&src, "talk") {
            Value::Err(_) => {}
            other => panic!("expected Err on a short read, got {:?}", other),
        }
    }

    /// A negative count, one past the read cap, and one too large for `i64`
    /// must all be catchable `Result.Err` rather than traps.
    ///
    /// Uses a real listener rather than a constructed `Tcp.Connection`:
    /// construction is rejected by the type checker (`Cannot construct opaque
    /// type 'Tcp.Connection'`), and only slips through here because
    /// `call_fn_with_effects` VM-compiles without type checking. A test that
    /// relied on that would be exercising a program Aver source cannot express.
    #[test]
    #[ignore = "integration: starts a local TCP server; run with --include-ignored --test-threads=1"]
    fn tcp_read_bytes_rejects_out_of_range_counts() {
        use std::net::TcpListener;
        use std::thread;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        thread::spawn(move || {
            while let Ok((stream, _)) = listener.accept() {
                // Hold the peer open; these cases must fail on the count
                // before any read is attempted.
                thread::sleep(std::time::Duration::from_millis(200));
                drop(stream);
            }
        });

        for (count, expect) in [
            ("-1", "negative"),
            ("20000000", "limit"),
            ("1208925819614629174706176", "read limit"),
        ] {
            let src = format!(
                "record Bytes\n    values: List<Int>\n\nfn talk() -> Result<Bytes, String>\n    ! [Tcp.connect, Tcp.readBytes]\n    conn = Tcp.connect(\"127.0.0.1\", {})?\n    Tcp.readBytes(conn, {})\n",
                port, count
            );
            match run_tcp_fn(&src, "talk") {
                Value::Err(inner) => match *inner {
                    Value::Str(msg) => assert!(
                        msg.contains(expect),
                        "count {count}: expected message mentioning {expect:?}, got: {msg}"
                    ),
                    other => panic!("expected Str error, got {:?}", other),
                },
                other => panic!("count {count}: expected Err, got {:?}", other),
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Verify: ? operator in verify blocks
// ---------------------------------------------------------------------------

#[test]
fn verify_error_prop_ok_unwraps() {
    // `?` on Ok in a verify case should unwrap normally:  ok()? == 42
    let src = "fn ok() -> Result<Int, String>\n    Result.Ok(42)\nfn test__entry() -> Bool\n    ok()? == 42\n";
    // `?` on Ok should unwrap and the value should equal 42
    // We use call_fn to evaluate the test helper
    assert_eq!(
        call_fn(src, "ok", vec![]),
        Value::Ok(Box::new(Value::int(42)))
    );
}

#[test]
fn verify_error_prop_err_propagates() {
    // `?` on Err should propagate the error (not panic)
    let src = "fn fail() -> Result<Int, String>\n    Result.Err(\"boom\")\n";
    assert_eq!(
        call_fn(src, "fail", vec![]),
        Value::Err(Box::new(Value::Str("boom".to_string())))
    );
}

#[test]
fn verify_match_does_not_require_all_arms_covered() {
    let src = "fn classify(n: Int) -> String\n    match n\n        0 -> \"zero\"\n        1 -> \"one\"\n        _ -> \"many\"\n";
    assert_eq!(
        call_fn(src, "classify", vec![Value::int(0)]),
        Value::Str("zero".to_string())
    );
}

#[test]
fn verify_match_passes_when_all_arms_covered() {
    let src = "fn classify(n: Int) -> String\n    match n\n        0 -> \"zero\"\n        1 -> \"one\"\n        _ -> \"many\"\n";
    assert_eq!(
        call_fn(src, "classify", vec![Value::int(0)]),
        Value::Str("zero".to_string())
    );
    assert_eq!(
        call_fn(src, "classify", vec![Value::int(1)]),
        Value::Str("one".to_string())
    );
    assert_eq!(
        call_fn(src, "classify", vec![Value::int(2)]),
        Value::Str("many".to_string())
    );
}

#[test]
fn verify_output_shape_does_not_require_unreachable_shapes() {
    let src = "fn onlyOk(n: Int) -> Result<Int, String>\n    Result.Ok(n)\n";
    assert_eq!(
        call_fn(src, "onlyOk", vec![Value::int(7)]),
        Value::Ok(Box::new(Value::int(7)))
    );
}

#[test]
fn verify_does_not_require_option_none_shape_coverage() {
    let src = "fn maybe(n: Int) -> Option<Int>\n    match n\n        0 -> Option.None\n        _ -> Option.Some(n)\n";
    assert_eq!(
        call_fn(src, "maybe", vec![Value::int(1)]),
        Value::Some(Box::new(Value::int(1)))
    );
}

#[test]
fn verify_does_not_require_result_err_shape_coverage() {
    let src = "fn mayFail(n: Int) -> Result<Int, String>\n    match n\n        0 -> Result.Err(\"zero\")\n        _ -> Result.Ok(n)\n";
    assert_eq!(
        call_fn(src, "mayFail", vec![Value::int(1)]),
        Value::Ok(Box::new(Value::int(1)))
    );
}

#[test]
fn verify_does_not_require_bool_shape_coverage() {
    let src = "fn sign(n: Int) -> Bool\n    match n\n        0 -> true\n        _ -> false\n";
    assert_eq!(call_fn(src, "sign", vec![Value::int(0)]), Value::Bool(true));
}

#[test]
fn verify_does_not_require_named_sum_shape_coverage() {
    let src = "type Mode\n    Fast\n    Safe\nfn chooseMode(n: Int) -> Mode\n    match n\n        0 -> Mode.Fast\n        _ -> Mode.Safe\n";
    assert_eq!(
        call_fn(src, "chooseMode", vec![Value::int(0)]),
        Value::Variant {
            type_name: "Mode".to_string(),
            variant: "Fast".to_string(),
            fields: vec![].into(),
        }
    );
}

// ---------------------------------------------------------------------------
// Module runtime semantics
// ---------------------------------------------------------------------------

mod module_runtime_tests {
    use super::*;
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

    /// Build a VM from source with module_root for `depends` resolution.
    ///
    /// Mirrors `cmd_run_vm`: preload dep modules so the entry's
    /// `SymbolTable` knows about every cross-module call before the
    /// VM compiler resolves dep bodies against it.
    fn vm_build_with_modules(src: &str, module_root: &std::path::Path) -> vm::VM {
        use aver::codegen::ModuleInfo;
        let mut items = parse(src);
        tco::transform_program(&mut items);
        resolve_program(&mut items);
        let mut arena = Arena::new();
        vm::register_service_types(&mut arena);
        let root_str = module_root
            .to_str()
            .expect("module_root is not valid UTF-8");

        // Preload deps so the unified SymbolTable covers them.
        let depends = items
            .iter()
            .find_map(|i| match i {
                aver::ast::TopLevel::Module(m) => Some(m.depends.clone()),
                _ => None,
            })
            .unwrap_or_default();
        let loaded = aver::source::load_module_tree(&depends, root_str).expect("load dep tree");
        let dep_modules: Vec<ModuleInfo> = loaded.iter().map(ModuleInfo::from_loaded).collect();

        let symbols = SymbolTable::build(&items, &dep_modules);
        let resolved = hir::resolve_program(&symbols, &items);
        let (code, globals) = vm::compile_program_with_modules(
            &resolved,
            &symbols,
            &mut arena,
            Some(root_str),
            "<test>",
            None,
        )
        .expect("VM compile failed");
        let mut machine = vm::VM::new(code, globals, arena);
        machine.run_top_level().expect("top-level failed");
        machine
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
module App
    depends [Math]
    intent = "test"

fn main() -> Int
    Math.fib(6)
"#;

        let mut machine = vm_build_with_modules(app_src, &root);
        let out = machine
            .run_named_function("main", &[])
            .expect("main call failed")
            .to_value(&machine.arena);
        assert_eq!(out, Value::int(8));

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn local_fn_does_not_collide_with_imported_same_name() {
        let root = temp_module_root("fn_name_collision");
        let math_src = r#"
module Math
    exposes [fib]
    intent =
        "Math module"

fn fib(n: Int) -> Int
    n + 100
"#;
        std::fs::write(root.join("Math.av"), math_src).expect("write Math.av failed");

        let app_src = r#"
module App
    depends [Math]
    intent = "test"

fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)

fn probe() -> Int
    a = fib(10)
    Math.fib(10)
"#;

        let mut machine = vm_build_with_modules(app_src, &root);
        let out = machine
            .run_named_function("probe", &[])
            .expect("probe call failed")
            .to_value(&machine.arena);
        assert_eq!(out, Value::int(110));

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn imported_exact_effect_passes_runtime_gate() {
        let root = temp_module_root("effect_exact");
        let lib_src = r#"
module Lib
    exposes [hi]
    intent =
        "Library"

fn hi() -> Unit
    ! [Console.print]
    Console.print("hello")
"#;
        std::fs::write(root.join("Lib.av"), lib_src).expect("write Lib.av failed");

        let app_src = r#"
module App
    depends [Lib]
    intent = "test"

fn main() -> Unit
    ! [Console.print]
    Lib.hi()
"#;

        let mut machine = vm_build_with_modules(app_src, &root);
        let out = machine
            .run_named_function("main", &[])
            .expect("main call failed")
            .to_value(&machine.arena);
        assert_eq!(out, Value::Unit);

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn fully_qualified_imported_constructor_pattern_matches_at_runtime() {
        let root = temp_module_root("qualified_ctor_pattern");
        let domain_dir = root.join("Domain");
        std::fs::create_dir_all(&domain_dir).expect("create Domain dir failed");

        let types_src = r#"
module Types
    exposes [TaskEvent]
    intent =
        "Shared events"

type TaskEvent
    TaskStarted(String)
"#;
        std::fs::write(domain_dir.join("Types.av"), types_src).expect("write Types.av failed");

        let app_src = r#"
module App
    depends [Domain.Types]
    exposes [startedAt]
    intent =
        "Matches a fully-qualified imported constructor."

fn startedAt() -> String
    event = Domain.Types.TaskEvent.TaskStarted("2026-03-08T12:00:00Z")
    match event
        Domain.Types.TaskEvent.TaskStarted(at) -> at
"#;
        std::fs::write(root.join("App.av"), app_src).expect("write App.av failed");

        // Build via the dep-preloading helper so the unified `SymbolTable`
        // covers `Domain.Types`' constructors. The fully-qualified ctor in
        // the pattern then resolves to a `CtorId` — the same shape
        // production's typechecked path produces. (A bare
        // `SymbolTable::build(items, &[])` leaves the cross-module ctor
        // unresolved, which the MIR lowerer can't carry; resolving it up
        // front is what production always does.)
        let mut machine = vm_build_with_modules(app_src, &root);
        let out = machine
            .run_named_function("startedAt", &[])
            .expect("call failed")
            .to_value(&machine.arena);
        assert_eq!(out, Value::Str("2026-03-08T12:00:00Z".to_string()));

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn imported_function_matches_on_imported_sum_type_argument() {
        let root = temp_module_root("imported_sum_match");
        let ast_src = r#"
module Ast
    exposes [Expr]
    intent =
        "Shared imported sum type."

type Expr
    Int(Int)
    Text(String)
"#;
        std::fs::write(root.join("Ast.av"), ast_src).expect("write Ast.av failed");

        let helpers_src = r#"
module Helpers
    depends [Ast]
    exposes [inspectExpr]
    intent =
        "Imported matcher helper."

fn inspectExpr(expr: Expr) -> Int
    match expr
        Expr.Int(n) -> n
        _ -> 0 - 1
"#;
        std::fs::write(root.join("Helpers.av"), helpers_src).expect("write Helpers.av failed");

        let app_src = r#"
module App
    depends [Ast, Helpers]
    intent = "test"

fn main() -> Int
    Helpers.inspectExpr(Ast.Expr.Int(7))
"#;

        let mut machine = vm_build_with_modules(app_src, &root);
        let out = machine
            .run_named_function("main", &[])
            .expect("main call failed")
            .to_value(&machine.arena);
        assert_eq!(out, Value::int(7));

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn imported_function_matches_on_imported_sum_type_returned_by_another_module() {
        let root = temp_module_root("imported_sum_return");
        let ast_src = r#"
module Ast
    exposes [Expr]
    intent =
        "Shared imported sum type."

type Expr
    Int(Int)
    Text(String)
"#;
        std::fs::write(root.join("Ast.av"), ast_src).expect("write Ast.av failed");

        let builder_src = r#"
module Builder
    depends [Ast]
    exposes [makeExpr]
    intent =
        "Construct imported values."

fn makeExpr() -> Expr
    Expr.Int(7)
"#;
        std::fs::write(root.join("Builder.av"), builder_src).expect("write Builder.av failed");

        let helpers_src = r#"
module Helpers
    depends [Ast]
    exposes [inspectExpr]
    intent =
        "Imported matcher helper."

fn inspectExpr(expr: Expr) -> Int
    match expr
        Expr.Int(n) -> n
        _ -> 0 - 1
"#;
        std::fs::write(root.join("Helpers.av"), helpers_src).expect("write Helpers.av failed");

        let app_src = r#"
module App
    depends [Ast, Builder, Helpers]
    intent = "test"

fn main() -> Int
    Helpers.inspectExpr(Builder.makeExpr())
"#;

        let mut machine = vm_build_with_modules(app_src, &root);
        let out = machine
            .run_named_function("main", &[])
            .expect("main call failed")
            .to_value(&machine.arena);
        assert_eq!(out, Value::int(7));

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn resolved_slot_lookup_survives_owned_match_binding_frames() {
        let src = r#"
type Expr
    Int(Int)
    Text(String)

fn inspect(expr: Expr, fallback: Int) -> Int
    match expr
        Expr.Int(n) -> n + fallback
        _ -> fallback
"#;

        let out = call_fn_resolved(
            src,
            "inspect",
            vec![
                Value::Variant {
                    type_name: "Expr".to_string(),
                    variant: "Int".to_string(),
                    fields: vec![Value::int(7)].into(),
                },
                Value::int(5),
            ],
        );
        assert_eq!(out, Value::int(12));
    }
}

// ---------------------------------------------------------------------------
// Recursive function correctness tests
// ---------------------------------------------------------------------------

#[test]
fn naive_fib_30_returns_correct_result() {
    let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
    assert_eq!(
        call_fn(src, "fib", vec![Value::int(30)]),
        Value::int(832040)
    );
}

#[test]
fn naive_fib_small_values() {
    let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
    assert_eq!(call_fn(src, "fib", vec![Value::int(0)]), Value::int(0));
    assert_eq!(call_fn(src, "fib", vec![Value::int(1)]), Value::int(1));
    assert_eq!(call_fn(src, "fib", vec![Value::int(10)]), Value::int(55));
}

#[test]
fn non_recursive_fn_still_works() {
    let src = "fn double(x: Int) -> Int\n    x + x\n";
    assert_eq!(call_fn(src, "double", vec![Value::int(5)]), Value::int(10));
}

#[test]
fn tuple_args_do_not_collide() {
    let src = r#"
fn pick(p: Tuple<Int, Int>) -> Int
    match p == (1, 2)
        true -> 12
        false -> 99
"#;
    let out_a = call_fn(
        src,
        "pick",
        vec![Value::Tuple(vec![Value::int(1), Value::int(2)])],
    );
    assert_eq!(out_a, Value::int(12));

    let out_b = call_fn(
        src,
        "pick",
        vec![Value::Tuple(vec![Value::int(9), Value::int(9)])],
    );
    assert_eq!(out_b, Value::int(99));
}

// ---------------------------------------------------------------------------
// Tail-call optimization (TCO) tests
// ---------------------------------------------------------------------------

/// Helper: parse -> TCO transform -> resolve -> compile to VM -> call fn.
/// VM already applies TCO during compile, so this is equivalent to call_fn.
fn call_fn_with_tco(src: &str, fn_name: &str, args: Vec<Value>) -> Value {
    call_fn(src, fn_name, args)
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
        call_fn_with_tco(src, "factorial", vec![Value::int(0), Value::int(1)]),
        Value::int(1)
    );
    assert_eq!(
        call_fn_with_tco(src, "factorial", vec![Value::int(5), Value::int(1)]),
        Value::int(120)
    );
    assert_eq!(
        call_fn_with_tco(src, "factorial", vec![Value::int(10), Value::int(1)]),
        Value::int(3628800)
    );
    assert_eq!(
        call_fn_with_tco(src, "factorial", vec![Value::int(20), Value::int(1)]),
        Value::int(2432902008176640000)
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
        call_fn_with_tco(src, "sum", vec![Value::int(100), Value::int(0)]),
        Value::int(5050)
    );
    // Large n: no stack overflow with TCO
    assert_eq!(
        call_fn_with_tco(src, "sum", vec![Value::int(100_000), Value::int(0)]),
        Value::int(5000050000i64)
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
        call_fn_with_tco(src, "isEven", vec![Value::int(0)]),
        Value::Bool(true)
    );
    assert_eq!(
        call_fn_with_tco(src, "isEven", vec![Value::int(1)]),
        Value::Bool(false)
    );
    assert_eq!(
        call_fn_with_tco(src, "isOdd", vec![Value::int(1)]),
        Value::Bool(true)
    );
    assert_eq!(
        call_fn_with_tco(src, "isOdd", vec![Value::int(4)]),
        Value::Bool(false)
    );
    // Large n: would overflow without mutual TCO
    assert_eq!(
        call_fn_with_tco(src, "isEven", vec![Value::int(100_000)]),
        Value::Bool(true)
    );
    assert_eq!(
        call_fn_with_tco(src, "isOdd", vec![Value::int(100_001)]),
        Value::Bool(true)
    );
}

#[test]
fn tco_non_tail_fib_still_works() {
    // fib is NOT tail-recursive — should still work via normal recursion
    let src = r#"
fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;
    assert_eq!(
        call_fn_with_tco(src, "fib", vec![Value::int(0)]),
        Value::int(0)
    );
    assert_eq!(
        call_fn_with_tco(src, "fib", vec![Value::int(1)]),
        Value::int(1)
    );
    assert_eq!(
        call_fn_with_tco(src, "fib", vec![Value::int(10)]),
        Value::int(55)
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
        call_fn_with_tco(src, "mySum", vec![Value::int(10)]),
        Value::int(55)
    );
}

#[test]
fn non_tail_linear_recursion_large_n_no_longer_overflows() {
    let src = r#"
fn mySum(n: Int) -> Int
    match n
        0 -> 0
        _ -> n + mySum(n - 1)
"#;
    assert_eq!(
        call_fn_with_tco(src, "mySum", vec![Value::int(100_000)]),
        Value::int(5000050000i64)
    );
}

// ---------------------------------------------------------------------------
// Replay / record tests
// ---------------------------------------------------------------------------

mod replay_tests {
    use super::*;
    use aver::replay::{EffectRecord, JsonValue, RecordedOutcome, json_to_value, value_to_json};
    use std::collections::BTreeMap;

    /// Build a VM from source — run_named_function auto-sets allowed effects.
    fn vm_build_with_effects(src: &str) -> vm::VM {
        let items = parse(src);
        let mut machine = vm_compile(&items);
        machine.run_top_level().expect("top-level failed");
        machine
    }

    #[test]
    fn record_mode_logs_console_effect() {
        let src = r#"
fn ping() -> Unit
    ! [Console.print]
    Console.print("hello")
"#;
        let mut machine = vm_build_with_effects(src);
        machine.start_recording();
        let out = machine
            .run_named_function("ping", &[])
            .expect("ping failed")
            .to_value(&machine.arena);
        assert_eq!(out, Value::Unit);

        let effects = machine.recorded_effects();
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
    ! [Disk.exists]
    Disk.exists("/definitely/not/existing/path")
"#;
        let mut machine = vm_build_with_effects(src);
        machine.start_replay(
            vec![EffectRecord {
                seq: 1,
                effect_type: "Disk.exists".to_string(),
                args: vec![JsonValue::String(
                    "/definitely/not/existing/path".to_string(),
                )],
                outcome: RecordedOutcome::Value(JsonValue::Bool(true)),
                caller_fn: String::new(),
                source_line: 0,
                group_id: None,
                branch_path: None,
                effect_occurrence: None,
            }],
            true,
        );
        let out = machine
            .run_named_function("check", &[])
            .expect("check failed")
            .to_value(&machine.arena);
        assert_eq!(out, Value::Bool(true));
        machine
            .ensure_replay_consumed()
            .expect("all effects should be consumed");
    }

    /// A recorded `Http.get` returning `Ok(Http.Response(status = 418, body =
    /// "teapot", headers = {}))`, ready to hand to `start_replay`.
    fn recorded_teapot_response() -> EffectRecord {
        // One header, not none: an empty map is indistinguishable from
        // several wrong answers when a test reads `.headers` back.
        let mut headers = std::collections::HashMap::new();
        headers.insert(
            Value::Str("content-type".to_string()),
            Value::Str("text/plain".to_string()),
        );
        let outcome = value_to_json(&Value::Ok(Box::new(Value::Record {
            type_name: "Http.Response".to_string(),
            fields: vec![
                ("status".to_string(), Value::int(418)),
                ("body".to_string(), Value::Str("teapot".to_string())),
                ("headers".to_string(), Value::Map(headers)),
            ]
            .into(),
        })))
        .expect("an Http.Response must be representable as a recorded outcome");

        EffectRecord {
            seq: 1,
            effect_type: "Http.get".to_string(),
            args: vec![JsonValue::String("https://example.com/thing".to_string())],
            outcome: RecordedOutcome::Value(outcome),
            caller_fn: String::new(),
            source_line: 0,
            group_id: None,
            branch_path: None,
            effect_occurrence: None,
        }
    }

    /// `Http.get` used to be reported as effect-free in builds without the
    /// networking implementation, which routed it straight to the plain call
    /// path and skipped Record/Replay entirely. Now that it is effectful in
    /// every build, Replay has to be able to serve it.
    #[test]
    fn replay_mode_serves_a_recorded_http_call_without_the_network() {
        let src = r#"
fn reachedServer() -> Bool
    ! [Http.get]
    result = Http.get("https://example.com/thing")
    match result
        Result.Ok(resp) -> true
        Result.Err(msg) -> false
"#;
        let mut machine = vm_build_with_effects(src);
        machine.start_replay(vec![recorded_teapot_response()], true);
        let out = machine
            .run_named_function("reachedServer", &[])
            .expect("replaying Http.get must not reach the network path")
            .to_value(&machine.arena);
        assert_eq!(
            out,
            Value::Bool(true),
            "Replay must substitute the recorded Ok(Http.Response) instead of calling out"
        );
        machine
            .ensure_replay_consumed()
            .expect("the recorded Http.get should have been consumed");
    }

    /// A replayed record's values sit in the slots its type declares.
    ///
    /// A recording writes a record's fields into a JSON object keyed by name,
    /// so they come back name-sorted, and rebuilding filled the type's slots
    /// by position. `Http.Response` is declared `status, body, headers` and
    /// sorts `body, headers, status`, so replay handed the body to every
    /// reader of `.status`.
    ///
    /// All three fields are read here on purpose. Sorted order is a ROTATION
    /// of the declared order, not a reversal, so a fix that merely undid the
    /// sort in the other direction would land `status` correctly and still
    /// swap `body` with `headers`. Only a rebuild keyed on the names gets all
    /// three.
    #[test]
    fn replay_mode_preserves_record_field_identity() {
        let src = r#"
fn status() -> Int
    ! [Http.get]
    result = Http.get("https://example.com/thing")
    match result
        Result.Ok(resp) -> resp.status
        Result.Err(msg) -> 0

fn body() -> String
    ! [Http.get]
    result = Http.get("https://example.com/thing")
    match result
        Result.Ok(resp) -> resp.body
        Result.Err(msg) -> "unreached"

fn headerCount() -> Int
    ! [Http.get]
    result = Http.get("https://example.com/thing")
    match result
        Result.Ok(resp) -> Map.len(resp.headers)
        Result.Err(msg) -> -1
"#;
        for (fn_name, expected) in [
            ("status", Value::int(418)),
            ("body", Value::Str("teapot".to_string())),
            ("headerCount", Value::int(1)),
        ] {
            let mut machine = vm_build_with_effects(src);
            machine.start_replay(vec![recorded_teapot_response()], true);
            let out = machine
                .run_named_function(fn_name, &[])
                .expect("replaying Http.get must not reach the network path")
                .to_value(&machine.arena);
            assert_eq!(
                out, expected,
                "`.{fn_name}` must read the field the recording named, not whichever \
                 one sorted into its slot"
            );
        }
    }

    /// Reading a replayed record's field returns the declared TYPE, not just
    /// the wrong value.
    ///
    /// This is the sharper half of the same defect. With the values rotated,
    /// `.status` — which the typechecker proved is an `Int` — held the body
    /// string, so arithmetic on it failed outright (`cannot add String and
    /// Namespace` here; an `Arena: expected an integer at 2 but found
    /// String("teapot")` abort in the shapes that reach the arena first) while
    /// replay itself still reported `1 replayed (1 matched)`. A record whose
    /// fields all share a type (`Terminal.Size`, `Tcp.Connection`) has no such
    /// alarm at all, which is why this one is worth pinning separately.
    #[test]
    fn replay_mode_keeps_a_record_field_at_its_declared_type() {
        let src = r#"
fn statusPlusOne() -> Int
    ! [Http.get]
    result = Http.get("https://example.com/thing")
    match result
        Result.Ok(resp) -> resp.status + 1
        Result.Err(msg) -> 0
"#;
        let mut machine = vm_build_with_effects(src);
        machine.start_replay(vec![recorded_teapot_response()], true);
        let out = machine
            .run_named_function("statusPlusOne", &[])
            .expect("arithmetic on a replayed Int field must not fail on its type")
            .to_value(&machine.arena);
        assert_eq!(
            out,
            Value::int(419),
            "`.status` must still be the Int the typechecker proved it is"
        );
    }

    /// The silent case: a record whose fields all share a type.
    ///
    /// `Terminal.Size` is declared `width, height` and sorts `height, width`,
    /// so replay swapped them and nothing anywhere complained — a program
    /// asking for the width of an 80x24 terminal got 24, with the run
    /// reporting success. Nothing about the two `Int`s can catch this except
    /// carrying the names through, which is why it is pinned next to the
    /// `Http.Response` probes rather than trusted to them.
    #[test]
    fn replay_mode_preserves_same_typed_record_fields() {
        let src = r#"
fn width() -> Result<Int, String>
    ! [Terminal.size]
    size = Terminal.size()?
    Result.Ok(size.width)
"#;
        let outcome = value_to_json(&Value::Ok(Box::new(Value::Record {
            type_name: "Terminal.Size".to_string(),
            fields: vec![
                ("width".to_string(), Value::int(80)),
                ("height".to_string(), Value::int(24)),
            ]
            .into(),
        })))
        .expect("a Terminal.Size must be representable as a recorded outcome");
        let record = EffectRecord {
            seq: 1,
            effect_type: "Terminal.size".to_string(),
            args: vec![],
            outcome: RecordedOutcome::Value(outcome),
            caller_fn: String::new(),
            source_line: 0,
            group_id: None,
            branch_path: None,
            effect_occurrence: None,
        };

        let mut machine = vm_build_with_effects(src);
        machine.start_replay(vec![record], true);
        let out = machine
            .run_named_function("width", &[])
            .expect("replaying Terminal.size must not read the real terminal")
            .to_value(&machine.arena);
        assert_eq!(
            out,
            Value::Ok(Box::new(Value::int(80))),
            "`.width` must be the recorded width, not the height it sorts behind"
        );
    }

    /// A recording carrying a field the type does not declare is a replay
    /// failure, not a slot filled with whatever was closest.
    ///
    /// This is what stops the rebuild from being a re-sort: matching on names
    /// only means something if a name that matches nothing is refused. The
    /// alternative — drop it, or slot it positionally — is the original bug
    /// with an extra step.
    #[test]
    fn replay_mode_rejects_a_recorded_field_the_type_does_not_declare() {
        let src = r#"
fn status() -> Int
    ! [Http.get]
    result = Http.get("https://example.com/thing")
    match result
        Result.Ok(resp) -> resp.status
        Result.Err(msg) -> 0
"#;
        let outcome = value_to_json(&Value::Ok(Box::new(Value::Record {
            type_name: "Http.Response".to_string(),
            fields: vec![
                ("status".to_string(), Value::int(418)),
                ("body".to_string(), Value::Str("teapot".to_string())),
                ("headers".to_string(), Value::Map(Default::default())),
                ("trailers".to_string(), Value::Str("nope".to_string())),
            ]
            .into(),
        })))
        .expect("the malformed response must still be representable as JSON");
        let mut record = recorded_teapot_response();
        record.outcome = RecordedOutcome::Value(outcome);

        let mut machine = vm_build_with_effects(src);
        machine.start_replay(vec![record], true);
        let err = machine
            .run_named_function("status", &[])
            .expect_err("a recording that does not match the type must fail the replay");
        let text = format!("{:?}", err);
        assert!(
            text.contains("trailers") && text.contains("Http.Response"),
            "the failure must name the field and the type it does not belong to, \
             got: {text}"
        );
    }

    #[test]
    fn replay_mode_detects_effect_order_mismatch() {
        let src = r#"
fn check() -> Bool
    ! [Disk.exists]
    Disk.exists("/tmp/x")
"#;
        let mut machine = vm_build_with_effects(src);
        let mut outcome = BTreeMap::new();
        outcome.insert("$ok".to_string(), JsonValue::String("x".to_string()));
        machine.start_replay(
            vec![EffectRecord {
                seq: 1,
                effect_type: "Http.get".to_string(),
                args: vec![JsonValue::String("https://example.com".to_string())],
                outcome: RecordedOutcome::Value(JsonValue::Object(outcome)),
                caller_fn: String::new(),
                source_line: 0,
                group_id: None,
                branch_path: None,
                effect_occurrence: None,
            }],
            false,
        );
        let err = machine
            .run_named_function("check", &[])
            .expect_err("expected replay mismatch");
        let err_str = err.to_string();
        assert!(
            err_str.contains("replay")
                || err_str.contains("mismatch")
                || err_str.contains("Replay"),
            "expected ReplayMismatch, got: {}",
            err_str
        );
    }

    #[test]
    fn value_json_roundtrip_nested_record_variant_list() {
        let value = Value::Record {
            type_name: "Envelope".to_string(),
            fields: vec![
                ("id".to_string(), Value::int(7)),
                (
                    "payload".to_string(),
                    Value::Variant {
                        type_name: "Event".to_string(),
                        variant: "Created".to_string(),
                        fields: vec![
                            Value::Record {
                                type_name: "User".to_string(),
                                fields: vec![
                                    ("age".to_string(), Value::int(35)),
                                    ("name".to_string(), Value::Str("Ada".to_string())),
                                ]
                                .into(),
                            },
                            list_from_vec(vec![
                                Value::Some(Box::new(Value::int(1))),
                                Value::None,
                                Value::Ok(Box::new(Value::Str("ok".to_string()))),
                                Value::Err(Box::new(Value::Str("boom".to_string()))),
                            ]),
                        ]
                        .into(),
                    },
                ),
            ]
            .into(),
        };

        let json = value_to_json(&value).expect("value_to_json failed");
        let restored = json_to_value(&json).expect("json_to_value failed");
        assert_eq!(restored, value);
    }

    #[test]
    fn value_json_roundtrip_list_with_nested_structures() {
        let value = list_from_vec(vec![
            Value::Record {
                type_name: "Point".to_string(),
                fields: vec![
                    ("x".to_string(), Value::Float(1.5)),
                    ("y".to_string(), Value::Float(-2.25)),
                ]
                .into(),
            },
            Value::Variant {
                type_name: "MaybePoint".to_string(),
                variant: "Some".to_string(),
                fields: vec![Value::Record {
                    type_name: "Point".to_string(),
                    fields: vec![
                        ("x".to_string(), Value::int(1)),
                        ("y".to_string(), Value::int(2)),
                    ]
                    .into(),
                }]
                .into(),
            },
            Value::Ok(Box::new(list_from_vec(vec![
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
    let val = run_program_lookup(src, "result");
    assert_eq!(val, Value::int(42));
}

// ---------------------------------------------------------------------------
// Char namespace
// ---------------------------------------------------------------------------

#[test]
fn string_first_code_point_ascii() {
    assert_eq!(
        eval("String.firstCodePoint(\"A\")"),
        Value::Some(Box::new(Value::int(65)))
    );
}

#[test]
fn string_first_code_point_unicode() {
    assert_eq!(
        eval("String.firstCodePoint(\"π\")"),
        Value::Some(Box::new(Value::int(960)))
    );
}

#[test]
fn string_first_code_point_emoji() {
    // 🎉 = U+1F389
    assert_eq!(
        eval("String.firstCodePoint(\"🎉\")"),
        Value::Some(Box::new(Value::int(0x1F389)))
    );
}

#[test]
fn string_first_code_point_empty_returns_none() {
    assert_eq!(eval("String.firstCodePoint(\"\")"), Value::None);
}

#[test]
fn string_from_code_point_valid() {
    assert_eq!(
        eval("String.fromCodePoint(65)"),
        Value::Some(Box::new(Value::Str("A".to_string())))
    );
}

#[test]
fn string_from_code_point_unicode() {
    assert_eq!(
        eval("String.fromCodePoint(960)"),
        Value::Some(Box::new(Value::Str("π".to_string())))
    );
}

#[test]
fn string_from_code_point_surrogate_returns_none() {
    // U+D800 is a surrogate — not a valid scalar value
    assert_eq!(eval("String.fromCodePoint(55296)"), Value::None);
}

#[test]
fn string_from_code_point_negative_returns_none() {
    assert_eq!(eval("String.fromCodePoint(0 - 1)"), Value::None);
}

#[test]
fn string_from_code_point_too_large_returns_none() {
    // > U+10FFFF
    assert_eq!(eval("String.fromCodePoint(1114112)"), Value::None);
}

#[test]
fn string_from_code_point_u32_overflow_returns_none() {
    // > u32::MAX should not wrap around to NUL
    assert_eq!(eval("String.fromCodePoint(4294967296)"), Value::None);
}

// ---------------------------------------------------------------------------
// Record update
// ---------------------------------------------------------------------------

#[test]
fn record_update_single_field() {
    let src = r#"
record User
    name: String
    age: Int

u = User(name = "Alice", age = 30)
updated = User.update(u, age = 31)
"#;
    let updated = run_program_lookup(src, "updated");
    match &updated {
        Value::Record { type_name, fields } => {
            assert_eq!(type_name, "User");
            assert_eq!(
                fields.as_ref(),
                &[
                    ("name".to_string(), Value::Str("Alice".to_string())),
                    ("age".to_string(), Value::int(31)),
                ]
            );
        }
        other => panic!("expected Record, got {:?}", other),
    }
}

#[test]
fn record_update_multiple_fields() {
    let src = r#"
record User
    name: String
    age: Int

u = User(name = "Alice", age = 30)
updated = User.update(u, name = "Bob", age = 31)
"#;
    let updated = run_program_lookup(src, "updated");
    match &updated {
        Value::Record { type_name, fields } => {
            assert_eq!(type_name, "User");
            assert_eq!(
                fields.as_ref(),
                &[
                    ("name".to_string(), Value::Str("Bob".to_string())),
                    ("age".to_string(), Value::int(31)),
                ]
            );
        }
        other => panic!("expected Record, got {:?}", other),
    }
}

#[test]
fn record_update_preserves_unmodified() {
    let src = r#"
record User
    name: String
    age: Int

u = User(name = "Alice", age = 30)
updated = User.update(u, age = 99)
"#;
    let updated = run_program_lookup(src, "updated");
    match &updated {
        Value::Record { fields, .. } => {
            // name should be unchanged
            assert_eq!(
                fields[0],
                ("name".to_string(), Value::Str("Alice".to_string()))
            );
            // age should be updated
            assert_eq!(fields[1], ("age".to_string(), Value::int(99)));
        }
        other => panic!("expected Record, got {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Oracle v1 — trace collection during stubbed verify-law eval
// ---------------------------------------------------------------------------

#[test]
fn vm_records_classified_effect_emissions_when_trace_collecting() {
    // Wire a stub for Random.int, enable trace collection, run an
    // effectful function, and read the collected events back. Verifies
    // that both stubbed (Random.int) and unstubbed (Console.print) calls
    // land in the buffer when collection is active.
    let src = concat!(
        "fn stubConst(path: BranchPath, n: Int, min: Int, max: Int) -> Result<Int, String>\n",
        "    ? \"always min\"\n",
        "    Result.Ok(min)\n",
        "fn hello() -> Int\n",
        "    ? \"one draw + print\"\n",
        "    ! [Random.int, Console.print]\n",
        "    x = Random.int(1, 6)\n",
        "    Console.print(\"hi\")\n",
        "    x\n",
    );
    let items = parse(src);
    let mut machine = vm_compile(&items);
    machine.run_top_level().expect("top-level");
    machine.set_silent_console(true);

    // Install Random.int stub → stubConst.
    let stub_fn_id = machine
        .find_fn_id("stubConst")
        .expect("stubConst must resolve");
    let mut stubs = std::collections::HashMap::new();
    stubs.insert("Random.int".to_string(), stub_fn_id);
    machine.install_oracle_stubs(stubs);

    machine.start_trace_collection();
    let result_nv = machine
        .run_named_function("hello", &[])
        .expect("hello must run");
    let events = machine.take_trace_events();
    machine.clear_oracle_stubs();

    // Stub returns min = 1; Aver Random.int(1, 6) under stubConst → 1.
    let result = result_nv.to_value(&machine.arena);
    assert_eq!(result, Value::int(1));

    // Two classified effects emitted: Random.int then Console.print.
    assert_eq!(events.len(), 2, "expected 2 events, got {:?}", events);
    match &events[0] {
        Value::Record { type_name, fields } => {
            assert_eq!(type_name, "EffectEvent");
            let method = fields
                .iter()
                .find(|(n, _)| n == "method")
                .map(|(_, v)| v)
                .expect("method field");
            assert!(matches!(method, Value::Str(s) if s == "Random.int"));
        }
        other => panic!("event[0] not a record: {:?}", other),
    }
    match &events[1] {
        Value::Record { type_name, fields } => {
            assert_eq!(type_name, "EffectEvent");
            let method = fields
                .iter()
                .find(|(n, _)| n == "method")
                .map(|(_, v)| v)
                .expect("method field");
            assert!(matches!(method, Value::Str(s) if s == "Console.print"));
        }
        other => panic!("event[1] not a record: {:?}", other),
    }
}

#[test]
fn literal_random_discharge_faults_when_an_oracle_returns_err() {
    let src = concat!(
        "fn rejectRoll(path: BranchPath, n: Int, min: Int, max: Int) -> Result<Int, String>\n",
        "    ? \"violates the Random.int contract for valid bounds\"\n",
        "    Result.Err(\"broken random provider\")\n",
        "fn rollPair() -> Tuple<Int, Int>\n",
        "    ? \"Two literal rolls exercise discharge inside an independent product.\"\n",
        "    ! [Random.int]\n",
        "    (Random.int(1, 6), Random.int(1, 6))!\n",
    );
    let items = parse(src);
    let mut machine = vm_compile(&items);
    machine.run_top_level().expect("top-level");

    let stub_fn_id = machine
        .find_fn_id("rejectRoll")
        .expect("rejectRoll must resolve");
    let mut stubs = std::collections::HashMap::new();
    stubs.insert("Random.int".to_string(), stub_fn_id);
    machine.install_oracle_stubs(stubs);

    let error = machine
        .run_named_function("rollPair", &[])
        .expect_err("a discharged Err must fault instead of fabricating a random sample");
    let message = error.to_string();
    assert!(message.contains("provider contract violated"), "{message}");
    assert!(message.contains("broken random provider"), "{message}");
}

// ---------------------------------------------------------------------------
// BranchPath — opaque builtin for Oracle-proof specs
// ---------------------------------------------------------------------------

fn assert_branch_path(value: &Value, expected_dewey: &str) {
    match value {
        Value::Record { type_name, fields } => {
            assert_eq!(
                type_name, "BranchPath",
                "expected BranchPath, got {}",
                type_name
            );
            let dewey = fields
                .iter()
                .find(|(n, _)| n == "dewey")
                .map(|(_, v)| v)
                .expect("BranchPath should carry a `dewey` field");
            match dewey {
                Value::Str(s) => assert_eq!(s, expected_dewey),
                other => panic!("BranchPath.dewey should be a String, got {:?}", other),
            }
        }
        other => panic!("expected Record(BranchPath), got {:?}", other),
    }
}

#[test]
fn branch_path_root_is_empty_dewey() {
    let v = eval("BranchPath.Root");
    assert_branch_path(&v, "");
}

#[test]
fn branch_path_child_of_root_is_single_index() {
    let v = eval("BranchPath.child(BranchPath.Root, 3)");
    assert_branch_path(&v, "3");
}

#[test]
fn branch_path_child_nests_with_dot() {
    let v = eval("BranchPath.child(BranchPath.child(BranchPath.Root, 2), 0)");
    assert_branch_path(&v, "2.0");
}

#[test]
fn branch_path_parse_accepts_root() {
    let v = eval("BranchPath.parse(\"\")");
    assert_branch_path(&v, "");
}

#[test]
fn branch_path_parse_roundtrips_dewey() {
    let v = eval("BranchPath.parse(\"2.0\")");
    assert_branch_path(&v, "2.0");
}

#[test]
fn branch_path_parse_rejects_garbage() {
    assert_eq!(
        eval("BranchPath.parse(\"not.a.path\")"),
        Value::Err(Box::new(Value::Str(
            "BranchPath.parse: invalid dewey-decimal path: `not.a.path`".to_string()
        )))
    );
}

#[test]
fn branch_path_child_rejects_negative_index() {
    assert_eq!(
        eval("BranchPath.child(BranchPath.Root, 0 - 1)"),
        Value::Err(Box::new(Value::Str(
            "BranchPath.child: `idx` must be non-negative".to_string()
        )))
    );
}

#[test]
fn branch_path_dynamic_valid_inputs_return_ok() {
    let parsed = call_fn(
        "fn parsed(raw: String) -> Result<BranchPath, String>\n    BranchPath.parse(raw)\n",
        "parsed",
        vec![Value::Str("2.0".to_string())],
    );
    let Value::Ok(parsed) = parsed else {
        panic!("dynamic valid parse should be Ok")
    };
    assert_branch_path(&parsed, "2.0");

    let child = call_fn(
        "fn child(idx: Int) -> Result<BranchPath, String>\n    BranchPath.child(BranchPath.Root, idx)\n",
        "child",
        vec![Value::Int(aver_rt::AverInt::from_i64(4_294_967_296))],
    );
    let Value::Ok(child) = child else {
        panic!("arbitrary-precision non-negative child index should be Ok")
    };
    assert_branch_path(&child, "4294967296");
}

// ---------------------------------------------------------------------------
// Terminal service runtime
// ---------------------------------------------------------------------------

#[test]
#[cfg(feature = "terminal")]
#[ignore] // requires TTY — fails on CI (EAGAIN)
fn terminal_size_returns_record_with_width_and_height() {
    let src = "fn getSize() -> Result<Terminal.Size, String>\n    ? \"get size\"\n    ! [Terminal.size]\n    Terminal.size()\n";
    let size = call_fn_with_effects(src, "getSize", vec![]).expect("call failed");
    let Value::Ok(size) = &size else {
        panic!("expected Result.Ok, got {:?}", size)
    };
    match size.as_ref() {
        Value::Record {
            type_name, fields, ..
        } => {
            assert_eq!(type_name, "Terminal.Size");
            let field_names: Vec<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
            assert!(field_names.contains(&"width"), "missing 'width' field");
            assert!(field_names.contains(&"height"), "missing 'height' field");
            // Both fields should be non-negative integers
            for (name, val) in fields.iter() {
                match val {
                    Value::Int(n) => assert!(
                        *n >= aver_rt::AverInt::zero(),
                        "field '{}' should be >= 0, got {}",
                        name,
                        n
                    ),
                    other => panic!("field '{}' should be Int, got {:?}", name, other),
                }
            }
        }
        other => panic!("expected Record, got {:?}", other),
    }
}
