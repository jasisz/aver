/// Spec tests for the Aver parser.
///
/// Each test verifies that a specific source snippet produces the expected
/// AST structure.  Tests are intentionally narrow: they assert the relevant
/// parts of the AST and ignore surrounding structure where possible.

use aver::ast::*;
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

fn parse_fails(src: &str) -> bool {
    let mut lexer = Lexer::new(src);
    let Ok(tokens) = lexer.tokenize() else { return true };
    let mut parser = Parser::new(tokens);
    parser.parse().is_err()
}

// ---------------------------------------------------------------------------
// Bindings
// ---------------------------------------------------------------------------

#[test]
fn val_int() {
    let items = parse("val x = 42");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Val(
            "x".to_string(),
            Expr::Literal(Literal::Int(42))
        ))]
    );
}

#[test]
fn val_string() {
    let items = parse("val greeting = \"hello\"");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Val(
            "greeting".to_string(),
            Expr::Literal(Literal::Str("hello".to_string()))
        ))]
    );
}

#[test]
fn val_bool_true() {
    let items = parse("val flag = true");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Val(
            "flag".to_string(),
            Expr::Literal(Literal::Bool(true))
        ))]
    );
}

#[test]
fn var_binding_no_reason() {
    let items = parse("var count = 0");
    if let TopLevel::Stmt(Stmt::Var(name, Expr::Literal(Literal::Int(0)), None)) = &items[0] {
        assert_eq!(name, "count");
    } else {
        panic!("unexpected: {:?}", items[0]);
    }
}

#[test]
fn assign_stmt_inside_fn() {
    let src = "fn f() -> Unit\n    var x = 0\n    x = 1\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let FnBody::Block(stmts) = &fd.body {
            assert!(
                matches!(stmts[0], Stmt::Var(_, _, _)),
                "first stmt should be Var"
            );
            assert!(
                matches!(stmts[1], Stmt::Assign(_, _)),
                "second stmt should be Assign"
            );
        } else {
            panic!("expected block body");
        }
    } else {
        panic!("expected FnDef");
    }
}

// ---------------------------------------------------------------------------
// Function definitions
// ---------------------------------------------------------------------------

#[test]
fn fn_single_expr_body() {
    let src = "fn double(x: Int) -> Int\n    = x + x\n";
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(fd.name, "double");
        assert_eq!(fd.params, vec![("x".to_string(), "Int".to_string())]);
        assert_eq!(fd.return_type, "Int");
        assert!(matches!(fd.body, FnBody::Expr(_)), "expected Expr body");
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_block_body() {
    let src = "fn greet(name: String) -> String\n    val msg = \"Hello\"\n    msg\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert!(matches!(fd.body, FnBody::Block(_)), "expected Block body");
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_with_desc() {
    let src = "fn add(a: Int, b: Int) -> Int\n    ? \"Adds two numbers.\"\n    = a + b\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(fd.desc, Some("Adds two numbers.".to_string()));
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_with_effects() {
    let src = "fn log(msg: String) -> Unit\n    ! [Io]\n    = print(msg)\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(fd.effects, vec!["Io".to_string()]);
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_multiple_params() {
    let src = "fn add(a: Int, b: Int) -> Int\n    = a + b\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(
            fd.params,
            vec![
                ("a".to_string(), "Int".to_string()),
                ("b".to_string(), "Int".to_string()),
            ]
        );
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_no_params() {
    let src = "fn unit() -> Unit\n    = print(\"hi\")\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert!(fd.params.is_empty());
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_result_return_type() {
    let src = "fn safe_div(a: Int, b: Int) -> Result<Int, String>\n    = Ok(a)\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(fd.return_type, "Result<Int, String>");
    } else {
        panic!("expected FnDef");
    }
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

#[test]
fn expr_arithmetic_add() {
    let items = parse("1 + 2");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Expr::BinOp(BinOp::Add, _, _)))
    ));
}

#[test]
fn expr_arithmetic_mul() {
    let items = parse("3 * 4");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Expr::BinOp(BinOp::Mul, _, _)))
    ));
}

#[test]
fn expr_comparison_eq() {
    let items = parse("a == b");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Expr::BinOp(BinOp::Eq, _, _)))
    ));
}

#[test]
fn expr_comparison_lt() {
    let items = parse("x < 10");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Expr::BinOp(BinOp::Lt, _, _)))
    ));
}

#[test]
fn expr_pipe() {
    let items = parse("x |> f");
    if let TopLevel::Stmt(Stmt::Expr(Expr::Pipe(left, right))) = &items[0] {
        assert!(matches!(**left, Expr::Ident(_)));
        assert!(matches!(**right, Expr::Ident(_)));
    } else {
        panic!("expected Pipe");
    }
}

#[test]
fn expr_fn_call() {
    let items = parse("add(1, 2)");
    if let TopLevel::Stmt(Stmt::Expr(Expr::FnCall(fn_expr, args))) = &items[0] {
        assert!(matches!(**fn_expr, Expr::Ident(_)));
        assert_eq!(args.len(), 2);
    } else {
        panic!("expected FnCall");
    }
}

#[test]
fn expr_constructor_ok() {
    let items = parse("Ok(42)");
    if let TopLevel::Stmt(Stmt::Expr(Expr::Constructor(name, Some(inner)))) = &items[0] {
        assert_eq!(name, "Ok");
        assert!(matches!(**inner, Expr::Literal(Literal::Int(42))));
    } else {
        panic!("expected Ok constructor");
    }
}

#[test]
fn expr_constructor_err() {
    let items = parse("Err(\"fail\")");
    if let TopLevel::Stmt(Stmt::Expr(Expr::Constructor(name, _))) = &items[0] {
        assert_eq!(name, "Err");
    } else {
        panic!("expected Err constructor");
    }
}

#[test]
fn expr_constructor_some() {
    let items = parse("Some(1)");
    if let TopLevel::Stmt(Stmt::Expr(Expr::Constructor(name, _))) = &items[0] {
        assert_eq!(name, "Some");
    } else {
        panic!("expected Some constructor");
    }
}

#[test]
fn expr_constructor_none() {
    let items = parse("None");
    if let TopLevel::Stmt(Stmt::Expr(Expr::Constructor(name, None))) = &items[0] {
        assert_eq!(name, "None");
    } else {
        panic!("expected None constructor");
    }
}

#[test]
fn expr_list_empty() {
    let items = parse("[]");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Expr::List(_)))
    ));
    if let TopLevel::Stmt(Stmt::Expr(Expr::List(elems))) = &items[0] {
        assert!(elems.is_empty());
    }
}

#[test]
fn expr_list_int() {
    let items = parse("[1, 2, 3]");
    if let TopLevel::Stmt(Stmt::Expr(Expr::List(elems))) = &items[0] {
        assert_eq!(elems.len(), 3);
        assert_eq!(elems[0], Expr::Literal(Literal::Int(1)));
        assert_eq!(elems[2], Expr::Literal(Literal::Int(3)));
    } else {
        panic!("expected List");
    }
}

#[test]
fn expr_error_propagation() {
    let items = parse("result?");
    if let TopLevel::Stmt(Stmt::Expr(Expr::ErrorProp(_))) = &items[0] {
        // pass
    } else {
        panic!("expected ErrorProp");
    }
}

// ---------------------------------------------------------------------------
// Match expressions
// ---------------------------------------------------------------------------

#[test]
fn match_with_wildcard() {
    let src = "fn f(n: Int) -> String\n    = match n:\n        0 -> \"zero\"\n        _ -> \"other\"\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let FnBody::Expr(Expr::Match(_, arms)) = &fd.body {
            assert_eq!(arms.len(), 2);
            assert!(matches!(arms[0].pattern, Pattern::Literal(Literal::Int(0))));
            assert!(matches!(arms[1].pattern, Pattern::Wildcard));
        } else {
            panic!("expected match body");
        }
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn match_constructor_patterns() {
    let src = "fn f(r: Any) -> Int\n    = match r:\n        Ok(v) -> v\n        Err(_) -> 0\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let FnBody::Expr(Expr::Match(_, arms)) = &fd.body {
            assert_eq!(arms.len(), 2);
            assert!(matches!(
                &arms[0].pattern,
                Pattern::Constructor(name, bindings) if name == "Ok" && !bindings.is_empty()
            ));
            assert!(matches!(
                &arms[1].pattern,
                Pattern::Constructor(name, _) if name == "Err"
            ));
        } else {
            panic!("expected match body");
        }
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn match_ident_binding() {
    let src = "fn f(x: Any) -> Any\n    = match x:\n        n -> n\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let FnBody::Expr(Expr::Match(_, arms)) = &fd.body {
            assert!(matches!(&arms[0].pattern, Pattern::Ident(s) if s == "n"));
        } else {
            panic!("expected match");
        }
    } else {
        panic!("expected FnDef");
    }
}

// ---------------------------------------------------------------------------
// Module blocks
// ---------------------------------------------------------------------------

#[test]
fn module_with_intent() {
    let src = "module Calc\n    intent:\n        \"A calculator module.\"\n";
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::Module(m) = &items[0] {
        assert_eq!(m.name, "Calc");
        assert!(m.intent.contains("calculator"));
    } else {
        panic!("expected Module");
    }
}

#[test]
fn module_with_depends() {
    // depends uses no colon: `depends [Core]`
    let src = "module App\n    depends [Core]\n    intent:\n        \"App module.\"\n";
    let items = parse(src);
    if let TopLevel::Module(m) = &items[0] {
        assert!(m.depends.contains(&"Core".to_string()));
    } else {
        panic!("expected Module");
    }
}

// ---------------------------------------------------------------------------
// Verify blocks
// ---------------------------------------------------------------------------

#[test]
fn verify_two_cases() {
    let src = "verify add:\n    add(1, 2) =>3\n    add(0, 0) =>0\n";
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::Verify(vb) = &items[0] {
        assert_eq!(vb.fn_name, "add");
        assert_eq!(vb.cases.len(), 2);
    } else {
        panic!("expected Verify");
    }
}

#[test]
fn verify_case_lhs_is_fn_call() {
    let src = "verify greet:\n    greet(\"Alice\") =>\"Hello, Alice\"\n";
    let items = parse(src);
    if let TopLevel::Verify(vb) = &items[0] {
        let (lhs, _) = &vb.cases[0];
        assert!(matches!(lhs, Expr::FnCall(_, _)));
    } else {
        panic!("expected Verify");
    }
}

#[test]
fn verify_case_rhs_is_literal() {
    let src = "verify double:\n    double(3) =>6\n";
    let items = parse(src);
    if let TopLevel::Verify(vb) = &items[0] {
        let (_, rhs) = &vb.cases[0];
        assert!(matches!(rhs, Expr::Literal(Literal::Int(6))));
    } else {
        panic!("expected Verify");
    }
}

// Verify that comparison operators work inside verify cases (not consumed as separator)
#[test]
fn verify_case_with_comparison_inside() {
    let src = "verify is_pos:\n    is_pos(1) =>true\n    is_pos(0) =>false\n";
    let items = parse(src);
    if let TopLevel::Verify(vb) = &items[0] {
        assert_eq!(vb.cases.len(), 2);
    } else {
        panic!("expected Verify");
    }
}

// ---------------------------------------------------------------------------
// Decision blocks
// ---------------------------------------------------------------------------

#[test]
fn decision_basic() {
    let src = "decision UseResult:\n    date: \"2024-01-01\"\n    reason:\n        \"Explicit error handling.\"\n    chosen: Result\n    rejected: [Exceptions]\n    impacts: [AllModules]\n";
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::Decision(d) = &items[0] {
        assert_eq!(d.name, "UseResult");
        assert_eq!(d.date, "2024-01-01");
        assert_eq!(d.chosen, "Result");
        assert_eq!(d.rejected, vec!["Exceptions".to_string()]);
        assert_eq!(d.impacts, vec!["AllModules".to_string()]);
    } else {
        panic!("expected Decision");
    }
}

#[test]
fn decision_with_author() {
    let src = "decision UseResult:\n    date: \"2024-01-01\"\n    author: \"Alice\"\n    reason:\n        \"Good choice.\"\n    chosen: Result\n    rejected: []\n    impacts: []\n";
    let items = parse(src);
    if let TopLevel::Decision(d) = &items[0] {
        assert_eq!(d.author, Some("Alice".to_string()));
    } else {
        panic!("expected Decision");
    }
}

#[test]
fn decision_multiple_rejected() {
    let src = "decision NullChoice:\n    date: \"2024-01-01\"\n    reason:\n        \"No null.\"\n    chosen: Option\n    rejected: [Null, Nil, Nothing]\n    impacts: []\n";
    let items = parse(src);
    if let TopLevel::Decision(d) = &items[0] {
        assert_eq!(d.rejected.len(), 3);
    } else {
        panic!("expected Decision");
    }
}

// ---------------------------------------------------------------------------
// Multiple top-level items
// ---------------------------------------------------------------------------

#[test]
fn multiple_fn_defs() {
    let src = "fn a() -> Unit\n    = print(\"a\")\nfn b() -> Unit\n    = print(\"b\")\n";
    let items = parse(src);
    let fns: Vec<_> = items
        .iter()
        .filter(|i| matches!(i, TopLevel::FnDef(_)))
        .collect();
    assert_eq!(fns.len(), 2);
}

#[test]
fn module_then_fn() {
    let src = "module M\n    intent:\n        \"M.\"\nfn f() -> Unit\n    = print(\"f\")\n";
    let items = parse(src);
    assert!(matches!(items[0], TopLevel::Module(_)));
    assert!(matches!(items[1], TopLevel::FnDef(_)));
}

// ---------------------------------------------------------------------------
// Parse error cases
// ---------------------------------------------------------------------------

#[test]
fn val_missing_name_is_error() {
    assert!(parse_fails("val = 42"), "missing name should fail");
}

#[test]
fn fn_missing_return_arrow_is_ok() {
    // -> return type is optional; missing it defaults to Unit
    let src = "fn noop()\n    = print(\"ok\")\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(fd.return_type, "Unit");
    } else {
        panic!("expected FnDef");
    }
}

// ---------------------------------------------------------------------------
// User-defined types
// ---------------------------------------------------------------------------

#[test]
fn parse_sum_type_with_variants() {
    let src = "type Shape\n  Circle(Float)\n  Rect(Float, Float)\n  Point\n";
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::TypeDef(TypeDef::Sum { name, variants }) = &items[0] {
        assert_eq!(name, "Shape");
        assert_eq!(variants.len(), 3);
        assert_eq!(variants[0].name, "Circle");
        assert_eq!(variants[0].fields, vec!["Float"]);
        assert_eq!(variants[1].name, "Rect");
        assert_eq!(variants[1].fields, vec!["Float", "Float"]);
        assert_eq!(variants[2].name, "Point");
        assert!(variants[2].fields.is_empty());
    } else {
        panic!("expected TopLevel::TypeDef(Sum), got: {:?}", items[0]);
    }
}

#[test]
fn parse_record_type_with_fields() {
    let src = "record User\n  name: String\n  age: Int\n";
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::TypeDef(TypeDef::Product { name, fields }) = &items[0] {
        assert_eq!(name, "User");
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0], ("name".to_string(), "String".to_string()));
        assert_eq!(fields[1], ("age".to_string(), "Int".to_string()));
    } else {
        panic!("expected TopLevel::TypeDef(Product), got: {:?}", items[0]);
    }
}

#[test]
fn parse_sum_type_constructor_call() {
    let src = "Circle(3.14)\n";
    let items = parse(src);
    if let TopLevel::Stmt(Stmt::Expr(Expr::FnCall(fn_expr, args))) = &items[0] {
        assert!(matches!(fn_expr.as_ref(), Expr::Ident(n) if n == "Circle"));
        assert_eq!(args.len(), 1);
    } else {
        panic!("expected FnCall, got: {:?}", items[0]);
    }
}

#[test]
fn parse_record_create_expression() {
    let src = "User(name: \"Alice\", age: 30)\n";
    let items = parse(src);
    if let TopLevel::Stmt(Stmt::Expr(Expr::RecordCreate { type_name, fields })) = &items[0] {
        assert_eq!(type_name, "User");
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].0, "name");
        assert_eq!(fields[1].0, "age");
    } else {
        panic!("expected RecordCreate, got: {:?}", items[0]);
    }
}

#[test]
fn parse_user_defined_constructor_pattern() {
    let src = "fn classify(s: Any) -> Any\n  = match s:\n    Circle(r) -> r\n    Rect(w, h) -> w\n    Point -> 0.0\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let FnBody::Expr(Expr::Match(_, arms)) = &fd.body {
            assert_eq!(arms.len(), 3);
            assert!(matches!(
                &arms[0].pattern,
                Pattern::Constructor(name, bindings) if name == "Circle" && bindings == &["r"]
            ));
            assert!(matches!(
                &arms[1].pattern,
                Pattern::Constructor(name, bindings) if name == "Rect" && bindings == &["w", "h"]
            ));
            assert!(matches!(
                &arms[2].pattern,
                Pattern::Constructor(name, bindings) if name == "Point" && bindings.is_empty()
            ));
        } else {
            panic!("expected match body");
        }
    } else {
        panic!("expected FnDef");
    }
}
