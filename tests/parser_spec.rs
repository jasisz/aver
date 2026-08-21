/// Spec tests for the Aver parser.
///
/// Each test verifies that a specific source snippet produces the expected
/// AST structure.  Tests are intentionally narrow: they assert the relevant
/// parts of the AST and ignore surrounding structure where possible.
use aver::ast::*;
use aver::lexer::Lexer;
use aver::parser::Parser;
use proptest::prelude::*;

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
    let Ok(tokens) = lexer.tokenize() else {
        return true;
    };
    let mut parser = Parser::new(tokens);
    parser.parse().is_err()
}

fn parse_error(src: &str) -> String {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    parser
        .parse()
        .expect_err("expected parse error")
        .to_string()
}

fn single_expr_body(fd: &FnDef) -> &Expr {
    match fd.body.as_ref() {
        FnBody::Block(stmts) if stmts.len() == 1 => match &stmts[0] {
            Stmt::Expr(expr) => &expr.node,
            other => panic!("expected single expression body, got {:?}", other),
        },
        other => panic!("expected single expression block body, got {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Bindings
// ---------------------------------------------------------------------------

#[test]
fn binding_int() {
    let items = parse("x = 42");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Binding(
            "x".to_string(),
            None,
            Spanned::bare(Expr::Literal(Literal::Int(42)))
        ))]
    );
}

#[test]
fn binding_string() {
    let items = parse("greeting = \"hello\"");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Binding(
            "greeting".to_string(),
            None,
            Spanned::bare(Expr::Literal(Literal::Str("hello".to_string())))
        ))]
    );
}

#[test]
fn binding_string_supports_backspace_and_formfeed_escapes() {
    let items = parse("x = \"\\b\\f\"");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Binding(
            "x".to_string(),
            None,
            Spanned::bare(Expr::Literal(Literal::Str("\u{0008}\u{000C}".to_string())))
        ))]
    );
}

#[test]
fn binding_bool_true() {
    let items = parse("flag = true");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Binding(
            "flag".to_string(),
            None,
            Spanned::bare(Expr::Literal(Literal::Bool(true)))
        ))]
    );
}

#[test]
fn binding_unit_singleton() {
    let items = parse("done = Unit");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Binding(
            "done".to_string(),
            None,
            Spanned::bare(Expr::Literal(Literal::Unit))
        ))]
    );
}

#[test]
fn binding_in_fn_body() {
    let src = "fn f() -> Int\n    x = 10\n    x\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        let stmts = fd.body.stmts();
        assert!(
            matches!(stmts[0], Stmt::Binding(_, _, _)),
            "first stmt should be Binding"
        );
        assert!(
            matches!(stmts[1], Stmt::Expr(_)),
            "second stmt should be Expr"
        );
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn typed_binding_parses_annotation() {
    let items = parse("x: Int = 5");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Binding(
            "x".to_string(),
            Some("Int".to_string()),
            Spanned::bare(Expr::Literal(Literal::Int(5)))
        ))]
    );
}

#[test]
fn typed_binding_generic_type() {
    let items = parse("m: Map<String, Int> = {}");
    if let TopLevel::Stmt(Stmt::Binding(name, type_ann, _)) = &items[0] {
        assert_eq!(name, "m");
        assert_eq!(type_ann.as_deref(), Some("Map<String, Int>"));
    } else {
        panic!("expected typed binding, got: {:?}", items[0]);
    }
}

#[test]
fn typed_binding_in_fn_body() {
    let src = "fn f() -> Int\n    x: Int = 10\n    x\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        let stmts = fd.body.stmts();
        if let Stmt::Binding(name, type_ann, _) = &stmts[0] {
            assert_eq!(name, "x");
            assert_eq!(type_ann.as_deref(), Some("Int"));
        } else {
            panic!("expected typed binding");
        }
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn val_keyword_is_parse_error() {
    assert!(parse_fails("val x = 42"), "'val' should be rejected");
}

#[test]
fn var_keyword_is_parse_error() {
    assert!(parse_fails("var x = 42"), "'var' should be rejected");
}

#[test]
fn leading_double_underscore_is_reserved_for_compiler_identifiers() {
    let cases = [
        (
            "intrinsic-shaped function",
            "fn __str_cursor_head(text: String, index: Int) -> Int\n    index\n",
        ),
        (
            "interpolation-capture parameter",
            "fn value(__to_str: Int) -> Int\n    __to_str\n",
        ),
        ("buffer-capture binding", "__buf_append = 1\n"),
        (
            "cursor identifier pattern",
            "fn value(x: Int) -> Int\n    match x\n        __cur_i -> __cur_i\n",
        ),
        (
            "list-builder head pattern",
            "fn value(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [__lst_push, ..tail] -> __lst_push\n",
        ),
        (
            "byte-builder tail pattern",
            "fn value(xs: List<Int>) -> List<Int>\n    match xs\n        [] -> []\n        [head, ..__byt_tail] -> __byt_tail\n",
        ),
        (
            "string-index constructor pattern",
            "fn value(x: Option<Int>) -> Int\n    match x\n        Option.None -> 0\n        Option.Some(__str_index) -> __str_index\n",
        ),
    ];

    for (role, source) in cases {
        let error = parse_error(source);
        assert!(
            error.contains("reserved for the compiler") && error.contains("cannot begin with '__'"),
            "{role} should report the reserved namespace, got: {error}"
        );
    }
}

#[test]
fn double_underscore_inside_a_user_identifier_remains_legal() {
    let items = parse(
        "fn walk__cursor(value__source: Int) -> Int\n    result__value = value__source\n    match result__value\n        bound__value -> bound__value\n",
    );
    let TopLevel::FnDef(fd) = &items[0] else {
        panic!("expected function")
    };
    assert_eq!(fd.name, "walk__cursor");
    assert_eq!(fd.params[0].0, "value__source");
}

#[test]
fn capability_block_is_parse_error() {
    // A capability is a KIND OF MODULE, declared solely by
    // `kind = capability` in the module header. There is exactly one
    // way to declare one, and a `capability Foo` block is not it — it
    // would duplicate exposes, depends, visibility and name resolution
    // the module already supplies. Before the capability grammar this
    // fell through to expression parsing and misparsed in silence.
    // Full coverage lives in `capability_grammar_spec.rs`.
    assert!(
        parse_fails("capability Tcp\n"),
        "'capability Foo' should be rejected"
    );
}

// ---------------------------------------------------------------------------
// Function definitions
// ---------------------------------------------------------------------------

#[test]
fn fn_single_expr_body() {
    let src = "fn double(x: Int) -> Int\n    x + x\n";
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(fd.name, "double");
        assert_eq!(fd.params, vec![("x".to_string(), "Int".to_string())]);
        assert_eq!(fd.return_type, "Int");
        assert!(matches!(*fd.body, FnBody::Block(_)), "expected Block body");
        assert!(matches!(single_expr_body(fd), Expr::BinOp(_, _, _)));
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_block_body() {
    let src = "fn greet(name: String) -> String\n    msg = \"Hello\"\n    msg\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert!(matches!(*fd.body, FnBody::Block(_)), "expected Block body");
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_with_desc() {
    let src = "fn add(a: Int, b: Int) -> Int\n    ? \"Adds two numbers.\"\n    a + b\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(fd.desc, Some("Adds two numbers.".to_string()));
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_with_multiline_desc() {
    let src = "fn add(a: Int, b: Int) -> Int\n    ? \"Adds two numbers.\"\n      \"Returns their sum.\"\n    a + b\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(
            fd.desc,
            Some("Adds two numbers. Returns their sum.".to_string())
        );
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_desc_requires_inline_first_string() {
    let src = "fn add(a: Int, b: Int) -> Int\n    ?\n        \"Adds two numbers.\"\n    a + b\n";
    let msg = parse_error(src);
    assert!(
        msg.contains("Expected string after '?'"),
        "expected helpful error, got: {msg}"
    );
}

#[test]
fn fn_with_effects() {
    let src = "fn log(msg: String) -> Unit\n    ! [Io]\n    print(msg)\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        let effect_names: Vec<&str> = fd.effects.iter().map(|e| e.node.as_str()).collect();
        assert_eq!(effect_names, vec!["Io"]);
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_with_multiline_effects() {
    let src = "fn log(msg: String) -> Unit\n    ! [\n        Args.get,\n        Console.print, Console.warn,\n        Time.now,\n        Disk.readText, Disk.writeText,\n    ]\n    print(msg)\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        let effect_names: Vec<&str> = fd.effects.iter().map(|e| e.node.as_str()).collect();
        assert_eq!(
            effect_names,
            vec![
                "Args.get",
                "Console.print",
                "Console.warn",
                "Time.now",
                "Disk.readText",
                "Disk.writeText",
            ]
        );
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_multiple_params() {
    let src = "fn add(a: Int, b: Int) -> Int\n    a + b\n";
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
    let src = "fn unit() -> Unit\n    print(\"hi\")\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert!(fd.params.is_empty());
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_result_return_type() {
    let src = "fn safe_div(a: Int, b: Int) -> Result<Int, String>\n    Result.Ok(a)\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(fd.return_type, "Result<Int, String>");
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_with_function_typed_param() {
    let src = "fn applyTwice(f: Fn(Int) -> Int, x: Int) -> Int\n    f(f(x))\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(
            fd.params,
            vec![
                ("f".to_string(), "Fn(Int) -> Int".to_string()),
                ("x".to_string(), "Int".to_string())
            ]
        );
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn fn_with_effectful_function_typed_param() {
    let src = "fn apply(f: Fn(Int) -> Int ! [Console], x: Int) -> Int\n    ! [Console]\n    f(x)\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(
            fd.params,
            vec![
                ("f".to_string(), "Fn(Int) -> Int ! [Console]".to_string()),
                ("x".to_string(), "Int".to_string())
            ]
        );
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
        TopLevel::Stmt(Stmt::Expr(Spanned {
            node: Expr::BinOp(BinOp::Add, _, _),
            ..
        }))
    ));
}

#[test]
fn expr_arithmetic_mul() {
    let items = parse("3 * 4");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Spanned {
            node: Expr::BinOp(BinOp::Mul, _, _),
            ..
        }))
    ));
}

#[test]
fn expr_comparison_eq() {
    let items = parse("a == b");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Spanned {
            node: Expr::BinOp(BinOp::Eq, _, _),
            ..
        }))
    ));
}

#[test]
fn expr_comparison_lt() {
    let items = parse("x < 10");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Spanned {
            node: Expr::BinOp(BinOp::Lt, _, _),
            ..
        }))
    ));
}

#[test]
fn expr_pipe_is_rejected() {
    assert!(parse_fails("x |> f"));
}

#[test]
fn expr_fn_call() {
    let items = parse("add(1, 2)");
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::FnCall(fn_expr, args) = &spanned.node
    {
        assert!(matches!(fn_expr.node, Expr::Ident(_)));
        assert_eq!(args.len(), 2);
    } else {
        panic!("expected FnCall");
    }
}

#[test]
fn expr_constructor_ok() {
    let items = parse("Result.Ok(42)");
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::FnCall(fn_expr, args) = &spanned.node
    {
        if let Expr::Attr(obj, field) = &fn_expr.node {
            assert!(matches!(&obj.node, Expr::Ident(n) if n == "Result"));
            assert_eq!(field, "Ok");
        } else {
            panic!("expected Attr, got: {:?}", fn_expr);
        }
        assert_eq!(args.len(), 1);
        assert!(matches!(&args[0].node, Expr::Literal(Literal::Int(42))));
    } else {
        panic!("expected Result.Ok call, got: {:?}", items[0]);
    }
}

#[test]
fn expr_constructor_err() {
    let items = parse("Result.Err(\"fail\")");
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::FnCall(fn_expr, _) = &spanned.node
    {
        if let Expr::Attr(obj, field) = &fn_expr.node {
            assert!(matches!(&obj.node, Expr::Ident(n) if n == "Result"));
            assert_eq!(field, "Err");
        } else {
            panic!("expected Attr, got: {:?}", fn_expr);
        }
    } else {
        panic!("expected Result.Err call, got: {:?}", items[0]);
    }
}

#[test]
fn expr_constructor_some() {
    let items = parse("Option.Some(1)");
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::FnCall(fn_expr, _) = &spanned.node
    {
        if let Expr::Attr(obj, field) = &fn_expr.node {
            assert!(matches!(&obj.node, Expr::Ident(n) if n == "Option"));
            assert_eq!(field, "Some");
        } else {
            panic!("expected Attr, got: {:?}", fn_expr);
        }
    } else {
        panic!("expected Option.Some call, got: {:?}", items[0]);
    }
}

#[test]
fn expr_constructor_none() {
    let items = parse("Option.None");
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::Attr(obj, field) = &spanned.node
    {
        assert!(matches!(&obj.node, Expr::Ident(n) if n == "Option"));
        assert_eq!(field, "None");
    } else {
        panic!("expected Option.None attr, got: {:?}", items[0]);
    }
}

#[test]
fn expr_constructor_none_with_parens_is_parse_error() {
    let msg = parse_error("Option.None()");
    assert!(
        msg.contains("Zero-argument constructor call 'Option.None()' is not allowed"),
        "unexpected parse error: {}",
        msg
    );
}

#[test]
fn expr_list_empty() {
    let items = parse("[]");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Spanned {
            node: Expr::List(_),
            ..
        }))
    ));
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::List(elems) = &spanned.node
    {
        assert!(elems.is_empty());
    }
}

#[test]
fn expr_list_int() {
    let items = parse("[1, 2, 3]");
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::List(elems) = &spanned.node
    {
        assert_eq!(elems.len(), 3);
        assert_eq!(elems[0].node, Expr::Literal(Literal::Int(1)));
        assert_eq!(elems[2].node, Expr::Literal(Literal::Int(3)));
    } else {
        panic!("expected List");
    }
}

#[test]
fn expr_tuple_literal() {
    let items = parse("(1, \"x\")");
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::Tuple(elems) = &spanned.node
    {
        assert_eq!(elems.len(), 2);
        assert_eq!(elems[0].node, Expr::Literal(Literal::Int(1)));
        assert_eq!(elems[1].node, Expr::Literal(Literal::Str("x".to_string())));
    } else {
        panic!("expected Tuple");
    }
}

#[test]
fn expr_map_literal_empty() {
    let items = parse("{}");
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::MapLiteral(entries) = &spanned.node
    {
        assert!(entries.is_empty());
    } else {
        panic!("expected MapLiteral, got: {:?}", items[0]);
    }
}

#[test]
fn expr_map_literal_entries() {
    let items = parse("{\"a\" => 1, \"b\" => 2}");
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::MapLiteral(entries) = &spanned.node
    {
        assert_eq!(entries.len(), 2);
        assert_eq!(
            entries[0].0.node,
            Expr::Literal(Literal::Str("a".to_string()))
        );
        assert_eq!(entries[0].1.node, Expr::Literal(Literal::Int(1)));
        assert_eq!(
            entries[1].0.node,
            Expr::Literal(Literal::Str("b".to_string()))
        );
        assert_eq!(entries[1].1.node, Expr::Literal(Literal::Int(2)));
    } else {
        panic!("expected MapLiteral, got: {:?}", items[0]);
    }
}

#[test]
fn expr_map_literal_missing_fat_arrow_is_parse_error() {
    let msg = parse_error("{\"a\": 1}");
    assert!(
        msg.contains("Expected '=>' between key and value in map literal"),
        "unexpected parse error: {}",
        msg
    );
}

#[test]
fn expr_parenthesized_group_not_tuple() {
    let items = parse("(1 + 2)");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Spanned {
            node: Expr::BinOp(BinOp::Add, _, _),
            ..
        }))
    ));
}

#[test]
fn expr_error_propagation() {
    let items = parse("result?");
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::ErrorProp(_) = &spanned.node
    {
        // pass
    } else {
        panic!("expected ErrorProp");
    }
}

#[test]
fn fn_with_tuple_type_annotation() {
    let src = "fn pair() -> Tuple<Int, String>\n    (1, \"x\")\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        assert_eq!(fd.return_type, "Tuple<Int, String>");
        assert!(matches!(single_expr_body(fd), Expr::Tuple(_)));
    } else {
        panic!("expected FnDef");
    }
}

// ---------------------------------------------------------------------------
// Match expressions
// ---------------------------------------------------------------------------

#[test]
fn match_with_wildcard() {
    let src =
        "fn f(n: Int) -> String\n    match n\n        0 -> \"zero\"\n        _ -> \"other\"\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let Expr::Match { arms, .. } = single_expr_body(fd) {
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
fn match_string_literal_pattern() {
    let src = "fn f(cmd: String) -> Int\n    match cmd\n        \"verack\" -> 1\n        _ -> 0\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let Expr::Match { arms, .. } = single_expr_body(fd) {
            assert_eq!(arms.len(), 2);
            assert!(matches!(&arms[0].pattern, Pattern::Literal(Literal::Str(s)) if s == "verack"));
            assert!(matches!(arms[1].pattern, Pattern::Wildcard));
        } else {
            panic!("expected match body");
        }
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn negative_int_literal_pattern_is_rejected() {
    let src =
        "fn f(n: Int) -> String\n    match n\n        -1 -> \"neg\"\n        _ -> \"other\"\n";
    let err = parse_error(src);
    assert!(
        err.contains("Expected match pattern"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn match_subject_colon_is_not_type_ascription() {
    let items = parse("match xs\n    [] -> 0\n    _ -> 1\n");
    assert!(matches!(
        items[0],
        TopLevel::Stmt(Stmt::Expr(Spanned {
            node: Expr::Match { .. },
            ..
        }))
    ));
}

#[test]
fn match_constructor_patterns() {
    let src = "fn f(r: Result<Int, String>) -> Int\n    match r\n        Result.Ok(v) -> v\n        Result.Err(_) -> 0\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let Expr::Match { arms, .. } = single_expr_body(fd) {
            assert_eq!(arms.len(), 2);
            assert!(matches!(
                &arms[0].pattern,
                Pattern::Constructor(name, bindings) if name == "Result.Ok" && !bindings.is_empty()
            ));
            assert!(matches!(
                &arms[1].pattern,
                Pattern::Constructor(name, _) if name == "Result.Err"
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
    let src = "fn f(x: Int) -> Int\n    match x\n        n -> n\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let Expr::Match { arms, .. } = single_expr_body(fd) {
            assert!(matches!(&arms[0].pattern, Pattern::Ident(s) if s == "n"));
        } else {
            panic!("expected match");
        }
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn match_list_empty_pattern() {
    let src = "fn f(xs: List<Int>) -> Int\n    match xs\n        [] -> 0\n        [h, ..t] -> h\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let Expr::Match { arms, .. } = single_expr_body(fd) {
            assert_eq!(arms.len(), 2);
            assert!(matches!(&arms[0].pattern, Pattern::EmptyList));
            assert!(matches!(
                &arms[1].pattern,
                Pattern::Cons(head, tail) if head == "h" && tail == "t"
            ));
        } else {
            panic!("expected match");
        }
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn match_list_cons_pattern_with_underscore() {
    let src = "fn f(xs: List<Int>) -> Int\n    match xs\n        [_, ..rest] -> len(rest)\n        [] -> 0\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let Expr::Match { arms, .. } = single_expr_body(fd) {
            assert!(matches!(
                &arms[0].pattern,
                Pattern::Cons(head, tail) if head == "_" && tail == "rest"
            ));
        } else {
            panic!("expected match");
        }
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn match_tuple_pattern_binds_items() {
    let src = "fn f(p: Tuple<Int, Int>) -> Int\n    match p\n        (a, b) -> a\n        _ -> 0\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let Expr::Match { arms, .. } = single_expr_body(fd) {
            assert_eq!(arms.len(), 2);
            assert!(matches!(
                &arms[0].pattern,
                Pattern::Tuple(items)
                if items.len() == 2
                    && matches!(&items[0], Pattern::Ident(a) if a == "a")
                    && matches!(&items[1], Pattern::Ident(b) if b == "b")
            ));
            assert!(matches!(arms[1].pattern, Pattern::Wildcard));
        } else {
            panic!("expected match");
        }
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn match_nested_tuple_pattern_parses() {
    let src = "fn f(p: Tuple<Tuple<Int, Int>, Int>) -> Int\n    match p\n        ((x, y), z) -> x\n        _ -> 0\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let Expr::Match { arms, .. } = single_expr_body(fd) {
            assert!(matches!(
                &arms[0].pattern,
                Pattern::Tuple(items)
                if items.len() == 2
                    && matches!(&items[0], Pattern::Tuple(inner) if inner.len() == 2)
                    && matches!(&items[1], Pattern::Ident(z) if z == "z")
            ));
        } else {
            panic!("expected match");
        }
    } else {
        panic!("expected FnDef");
    }
}

// ---------------------------------------------------------------------------
// Match arm body must be on the same line as ->
// ---------------------------------------------------------------------------

#[test]
fn match_arm_body_after_newline_is_error() {
    let src = "fn f(x: Int) -> Int\n    match x\n        1 ->\n            42\n        _ -> 0\n";
    let msg = parse_error(src);
    assert!(
        msg.contains("same line"),
        "expected 'same line' hint, got: {msg}"
    );
}

// ---------------------------------------------------------------------------
// Module blocks
// ---------------------------------------------------------------------------

#[test]
fn module_with_intent() {
    let src = "module Calc\n    intent =\n        \"A calculator module.\"\n";
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
fn module_with_inline_intent() {
    let src = "module Calc\n    intent = \"A calculator module.\"\n";
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::Module(m) = &items[0] {
        assert_eq!(m.name, "Calc");
        assert_eq!(m.intent, "A calculator module.");
    } else {
        panic!("expected Module");
    }
}

#[test]
fn module_with_inline_multiline_intent() {
    let src = "module Calc\n    intent = \"A calculator module.\"\n        \"Keeps arithmetic helpers together.\"\n";
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::Module(m) = &items[0] {
        assert_eq!(
            m.intent,
            "A calculator module. Keeps arithmetic helpers together."
        );
    } else {
        panic!("expected Module");
    }
}

#[test]
fn module_with_depends() {
    // depends uses no colon: `depends [Core]`
    let src = "module App\n    depends [Core]\n    intent =\n        \"App module.\"\n";
    let items = parse(src);
    if let TopLevel::Module(m) = &items[0] {
        assert!(m.depends.contains(&"Core".to_string()));
    } else {
        panic!("expected Module");
    }
}

#[test]
fn module_with_dotted_depends() {
    let src = "module App\n    depends [Models.User, Services.Auth]\n    intent =\n        \"App module.\"\n";
    let items = parse(src);
    if let TopLevel::Module(m) = &items[0] {
        assert!(m.depends.contains(&"Models.User".to_string()));
        assert!(m.depends.contains(&"Services.Auth".to_string()));
    } else {
        panic!("expected Module");
    }
}

// ---------------------------------------------------------------------------
// Verify blocks
// ---------------------------------------------------------------------------

#[test]
fn verify_two_cases() {
    let src = "verify add\n    add(1, 2) =>3\n    add(0, 0) =>0\n";
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
    let src = "verify greet\n    greet(\"Alice\") =>\"Hello, Alice\"\n";
    let items = parse(src);
    if let TopLevel::Verify(vb) = &items[0] {
        let (lhs, _) = &vb.cases[0];
        assert!(matches!(lhs.node, Expr::FnCall(_, _)));
    } else {
        panic!("expected Verify");
    }
}

#[test]
fn verify_case_rhs_is_literal() {
    let src = "verify double\n    double(3) =>6\n";
    let items = parse(src);
    if let TopLevel::Verify(vb) = &items[0] {
        let (_, rhs) = &vb.cases[0];
        assert!(matches!(rhs.node, Expr::Literal(Literal::Int(6))));
    } else {
        panic!("expected Verify");
    }
}

// Verify that comparison operators work inside verify cases (not consumed as separator)
#[test]
fn verify_case_with_comparison_inside() {
    let src = "verify is_pos\n    is_pos(1) =>true\n    is_pos(0) =>false\n";
    let items = parse(src);
    if let TopLevel::Verify(vb) = &items[0] {
        assert_eq!(vb.cases.len(), 2);
    } else {
        panic!("expected Verify");
    }
}

#[test]
fn verify_law_parses_given_domains_and_expands_cases() {
    let src = r#"
verify add law commutative
    given a: Int = 1..3
    given b: Int = [10, 20]
    add(a, b) => add(b, a)
"#;
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::Verify(vb) = &items[0] {
        assert!(matches!(vb.kind, VerifyKind::Law(_)));
        assert_eq!(vb.cases.len(), 6);
    } else {
        panic!("expected Verify");
    }
}

#[test]
fn verify_law_parses_optional_when_condition() {
    let src = r#"
verify max law ordered
    given a: Int = [1, 2]
    given b: Int = [1, 2]
    when a > b
    max(a, b) => a
"#;
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::Verify(vb) = &items[0] {
        let VerifyKind::Law(law) = &vb.kind else {
            panic!("expected law verify");
        };
        assert!(matches!(
            law.when.as_ref().map(|s| &s.node),
            Some(Expr::BinOp(BinOp::Gt, _, _))
        ));
        assert_eq!(vb.cases.len(), 4);
        assert_eq!(law.sample_guards.len(), 4);
        assert!(matches!(
            &law.sample_guards[0].node,
            Expr::BinOp(BinOp::Gt, left, right)
                if matches!(left.node, Expr::Literal(Literal::Int(1)))
                    && matches!(right.node, Expr::Literal(Literal::Int(1)))
        ));
        assert!(matches!(
            &law.sample_guards[3].node,
            Expr::BinOp(BinOp::Gt, left, right)
                if matches!(left.node, Expr::Literal(Literal::Int(2)))
                    && matches!(right.node, Expr::Literal(Literal::Int(2)))
        ));
    } else {
        panic!("expected Verify");
    }
}

#[test]
fn verify_law_parses_holds_claim_as_predicate_true() {
    // `P holds` is sugar for `P => true`: the rhs lowers to the Bool literal
    // `true` and the lhs is the predicate, so downstream case-expansion and
    // codegen see the same `lhs`/`rhs` pair as the explicit `=> true` form.
    let src = r#"
verify max law sane
    given a: Int = [1, 2]
    given b: Int = [1, 2]
    max(a, b) >= b holds
"#;
    let items = parse(src);
    let TopLevel::Verify(vb) = &items[0] else {
        panic!("expected Verify");
    };
    let VerifyKind::Law(law) = &vb.kind else {
        panic!("expected law verify");
    };
    assert!(
        matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true))),
        "`holds` rhs must be the implicit `true`: {:?}",
        law.rhs.node
    );
    assert!(
        matches!(&law.lhs.node, Expr::BinOp(BinOp::Gte, _, _)),
        "`holds` lhs is the predicate: {:?}",
        law.lhs.node
    );
}

#[test]
fn verify_law_requires_given_lines() {
    let src = r#"
verify add law commutative
    add(1, 2) => add(2, 1)
"#;
    let msg = parse_error(src);
    assert!(
        msg.contains("at least one 'given'"),
        "unexpected parse error: {}",
        msg
    );
}

#[test]
fn verify_law_requires_single_assertion_line() {
    let src = r#"
verify add law commutative
    given a: Int = [1]
    add(a, 2) => add(2, a)
    add(a, 3) => add(3, a)
"#;
    let msg = parse_error(src);
    assert!(
        msg.contains("exactly one assertion"),
        "unexpected parse error: {}",
        msg
    );
}

#[test]
fn verify_law_given_accepts_oracle_effect_ref_random_int() {
    // Oracle v1: `given <name>: <Effect.method> = [<stubs>]` — the type
    // position holds an effect-method reference (lowercase suffix after dot).
    // The type checker (not the parser) resolves it to the oracle signature.
    let src = r#"
verify f law law1
    given rnd: Random.int = [stubA]
    f() => stubA
"#;
    let items = parse(src);
    assert_eq!(items.len(), 1);
    let TopLevel::Verify(vb) = &items[0] else {
        panic!("expected Verify");
    };
    let VerifyKind::Law(law) = &vb.kind else {
        panic!("expected law verify");
    };
    assert_eq!(law.givens.len(), 1);
    assert_eq!(law.givens[0].name, "rnd");
    assert_eq!(law.givens[0].type_name, "Random.int");
}

#[test]
fn verify_law_given_accepts_oracle_effect_ref_http_get() {
    let src = r#"
verify f law law1
    given h: Http.get = [stubHttp]
    f() => stubHttp
"#;
    let items = parse(src);
    let TopLevel::Verify(vb) = &items[0] else {
        panic!("expected Verify");
    };
    let VerifyKind::Law(law) = &vb.kind else {
        panic!("expected law verify");
    };
    assert_eq!(law.givens[0].type_name, "Http.get");
}

#[test]
fn verify_cases_accepts_operation_given_without_trace() {
    let src = r#"
verify hashed
    given hash: Hash160.digest = [fixtureHash]
    hashed([0]) => [1]
"#;
    let items = parse(src);
    let TopLevel::Verify(vb) = &items[0] else {
        panic!("expected Verify");
    };
    assert!(matches!(vb.kind, VerifyKind::Cases));
    assert!(!vb.trace);
    assert_eq!(vb.cases_givens.len(), 1);
    assert_eq!(vb.cases_givens[0].name, "hash");
    assert_eq!(vb.cases_givens[0].type_name, "Hash160.digest");
    assert_eq!(vb.cases.len(), 1);
    assert_eq!(vb.case_givens.len(), 1);

    let rendered = aver::ast::unparse::unparse(&items).expect("unparse cases-form given");
    let reparsed = parse(&rendered);
    let TopLevel::Verify(round_trip) = &reparsed[0] else {
        panic!("expected Verify after round trip");
    };
    assert_eq!(round_trip.cases_givens[0].type_name, "Hash160.digest");
}

#[test]
fn verify_cases_accepts_a_canonical_namespaced_operation_given() {
    let src = r#"
verify doubled
    given probe: Sub.Probe.answer = [fixtureAnswer]
    doubled(2) => 204
"#;
    let items = parse(src);
    let TopLevel::Verify(vb) = &items[0] else {
        panic!("expected Verify");
    };
    assert_eq!(vb.cases_givens[0].type_name, "Sub.Probe.answer");

    let rendered = aver::ast::unparse::unparse(&items).expect("unparse namespaced given");
    let reparsed = parse(&rendered);
    let TopLevel::Verify(round_trip) = &reparsed[0] else {
        panic!("expected Verify after round trip");
    };
    assert_eq!(round_trip.cases_givens[0].type_name, "Sub.Probe.answer");
}

#[test]
fn verify_cases_parses_trace_keyword() {
    // `verify fn trace` — cases form with trace-awareness enabled.
    let src = r#"
verify f trace
    f(1) => 1
"#;
    let items = parse(src);
    let TopLevel::Verify(vb) = &items[0] else {
        panic!("expected Verify");
    };
    assert!(vb.trace, "expected trace=true");
    assert!(matches!(vb.kind, VerifyKind::Cases));
}

#[test]
fn verify_law_parses_trace_keyword_before_law() {
    // `verify fn trace law name` — law form with trace-awareness.
    let src = r#"
verify f trace law consistent
    given x: Int = [1]
    f(x) => f(x)
"#;
    let items = parse(src);
    let TopLevel::Verify(vb) = &items[0] else {
        panic!("expected Verify");
    };
    assert!(vb.trace, "expected trace=true");
    assert!(matches!(vb.kind, VerifyKind::Law(_)));
}

#[test]
fn verify_law_without_trace_keeps_trace_false() {
    let src = r#"
verify f law consistent
    given x: Int = [1]
    f(x) => f(x)
"#;
    let items = parse(src);
    let TopLevel::Verify(vb) = &items[0] else {
        panic!("expected Verify");
    };
    assert!(!vb.trace, "expected trace=false");
}

#[test]
fn verify_law_given_preserves_regular_type_annotations() {
    // Sanity: namespaced Upper.Upper paths remain types, while a lowercase
    // final segment is reserved for canonical operation references.
    let src = r#"
verify f law law1
    given conn: Domain.Tcp.Connection = [stub]
    given s: String = ["hi"]
    f(conn, s) => f(conn, s)
"#;
    let items = parse(src);
    let TopLevel::Verify(vb) = &items[0] else {
        panic!("expected Verify");
    };
    let VerifyKind::Law(law) = &vb.kind else {
        panic!("expected law verify");
    };
    assert_eq!(law.givens[0].type_name, "Domain.Tcp.Connection");
    assert_eq!(law.givens[1].type_name, "String");
}

// ---------------------------------------------------------------------------
// Decision blocks
// ---------------------------------------------------------------------------

#[test]
fn decision_basic() {
    let src = "decision UseResult\n    date = \"2024-01-01\"\n    reason =\n        \"Explicit error handling.\"\n    chosen = Result\n    rejected = [Exceptions]\n    impacts = [AllModules]\n";
    let items = parse(src);
    assert_eq!(items.len(), 1);
    if let TopLevel::Decision(d) = &items[0] {
        assert_eq!(d.name, "UseResult");
        assert_eq!(d.date, "2024-01-01");
        assert_eq!(d.chosen.node, DecisionImpact::Symbol("Result".to_string()));
        assert_eq!(
            d.rejected.iter().map(|s| &s.node).collect::<Vec<_>>(),
            vec![&DecisionImpact::Symbol("Exceptions".to_string())]
        );
        assert_eq!(
            d.impacts.iter().map(|s| &s.node).collect::<Vec<_>>(),
            vec![&DecisionImpact::Symbol("AllModules".to_string())]
        );
    } else {
        panic!("expected Decision");
    }
}

#[test]
fn decision_with_author() {
    let src = "decision UseResult\n    date = \"2024-01-01\"\n    author = \"Alice\"\n    reason =\n        \"Good choice.\"\n    chosen = Result\n    rejected = []\n    impacts = []\n";
    let items = parse(src);
    if let TopLevel::Decision(d) = &items[0] {
        assert_eq!(d.author, Some("Alice".to_string()));
    } else {
        panic!("expected Decision");
    }
}

#[test]
fn decision_multiple_rejected() {
    let src = "decision NullChoice\n    date = \"2024-01-01\"\n    reason =\n        \"No null.\"\n    chosen = Option\n    rejected = [Null, Nil, Nothing]\n    impacts = []\n";
    let items = parse(src);
    if let TopLevel::Decision(d) = &items[0] {
        assert_eq!(d.rejected.len(), 3);
        // Verify line info is preserved
        assert!(d.rejected.iter().all(|s| s.line > 0));
    } else {
        panic!("expected Decision");
    }
}

#[test]
fn decision_impacts_accepts_mixed_symbol_and_semantic_entries() {
    let src = "decision D\n    date = \"2026-03-05\"\n    reason =\n        \"R.\"\n    chosen = C\n    rejected = []\n    impacts = [doThing, \"error handling strategy\"]\n";
    let items = parse(src);
    if let TopLevel::Decision(d) = &items[0] {
        assert_eq!(
            d.impacts.iter().map(|s| &s.node).collect::<Vec<_>>(),
            vec![
                &DecisionImpact::Symbol("doThing".to_string()),
                &DecisionImpact::Semantic("error handling strategy".to_string())
            ]
        );
    } else {
        panic!("expected Decision");
    }
}

#[test]
fn decision_chosen_and_rejected_accept_mixed_symbol_and_semantic_entries() {
    let src = "decision D\n    date = \"2026-03-05\"\n    reason =\n        \"R.\"\n    chosen = \"Keep\"\n    rejected = [OldPath, \"global mutable state\"]\n    impacts = []\n";
    let items = parse(src);
    if let TopLevel::Decision(d) = &items[0] {
        assert_eq!(d.chosen.node, DecisionImpact::Semantic("Keep".to_string()));
        assert_eq!(
            d.rejected.iter().map(|s| &s.node).collect::<Vec<_>>(),
            vec![
                &DecisionImpact::Symbol("OldPath".to_string()),
                &DecisionImpact::Semantic("global mutable state".to_string()),
            ]
        );
    } else {
        panic!("expected Decision");
    }
}

#[test]
fn decision_field_names_are_allowed_as_bindings() {
    let src = "date = 1\nauthor = 2\nreason = 3\nchosen = 4\nrejected = 5\nimpacts = 6\n";
    let items = parse(src);
    let stmt_count = items
        .iter()
        .filter(|item| matches!(item, TopLevel::Stmt(_)))
        .count();
    assert_eq!(stmt_count, 6);
}

// ---------------------------------------------------------------------------
// Multiple top-level items
// ---------------------------------------------------------------------------

#[test]
fn multiple_fn_defs() {
    let src = "fn a() -> Unit\n    print(\"a\")\nfn b() -> Unit\n    print(\"b\")\n";
    let items = parse(src);
    let fns: Vec<_> = items
        .iter()
        .filter(|i| matches!(i, TopLevel::FnDef(_)))
        .collect();
    assert_eq!(fns.len(), 2);
}

#[test]
fn module_then_fn() {
    let src = "module M\n    intent =\n        \"M.\"\nfn f() -> Unit\n    print(\"f\")\n";
    let items = parse(src);
    assert!(matches!(items[0], TopLevel::Module(_)));
    assert!(matches!(items[1], TopLevel::FnDef(_)));
}

// ---------------------------------------------------------------------------
// Parse error cases
// ---------------------------------------------------------------------------

#[test]
fn val_keyword_error_in_fn_body() {
    let src = "fn f() -> Int\n    val x = 1\n    x\n";
    assert!(parse_fails(src), "'val' inside fn body should fail");
}

#[test]
fn eq_shorthand_after_binding_is_parse_error() {
    let src = "fn f(x: Int) -> String\n    items = Int.abs(x)\n    = \"result\"\n";
    let msg = parse_error(src);
    assert!(
        msg.contains("no longer use '= expr'"),
        "unexpected parse error: {}",
        msg
    );
}

#[test]
fn eq_shorthand_on_fn_header_is_parse_error() {
    let src = "fn f(x: Int) -> String = \"result\"\n";
    let msg = parse_error(src);
    assert!(
        msg.contains("no longer use '= expr'"),
        "unexpected parse error: {}",
        msg
    );
}

#[test]
fn lambda_syntax_shows_actionable_error() {
    let src = "fn apply(f: Fn(Int) -> Bool, x: Int) -> Bool\n    f(x)\nfn main() -> Bool\n    apply(fn(x: Int) -> Bool\n        x > 1, 1)\n";
    let msg = parse_error(src);
    assert!(
        msg.contains("Anonymous functions are not supported"),
        "unexpected parse error: {}",
        msg
    );
}

#[test]
fn fn_missing_return_arrow_is_ok() {
    // -> return type is optional; missing it defaults to Unit
    let src = "fn noop()\n    print(\"ok\")\n";
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
    if let TopLevel::TypeDef(TypeDef::Sum { name, variants, .. }) = &items[0] {
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
    if let TopLevel::TypeDef(TypeDef::Product { name, fields, .. }) = &items[0] {
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
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::FnCall(fn_expr, args) = &spanned.node
    {
        assert!(matches!(&fn_expr.node, Expr::Ident(n) if n == "Circle"));
        assert_eq!(args.len(), 1);
    } else {
        panic!("expected FnCall, got: {:?}", items[0]);
    }
}

#[test]
fn parse_record_create_expression() {
    let src = "User(name = \"Alice\", age = 30)\n";
    let items = parse(src);
    if let TopLevel::Stmt(Stmt::Expr(ref spanned)) = items[0]
        && let Expr::RecordCreate { type_name, fields } = &spanned.node
    {
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
    let src = "fn classify(s: Shape) -> Float\n  match s\n    Shape.Circle(r) -> r\n    Shape.Rect(w, h) -> w\n    Shape.Point -> 0.0\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[0] {
        if let Expr::Match { arms, .. } = single_expr_body(fd) {
            assert_eq!(arms.len(), 3);
            assert!(matches!(
                &arms[0].pattern,
                Pattern::Constructor(name, bindings) if name == "Shape.Circle" && bindings == &["r"]
            ));
            assert!(matches!(
                &arms[1].pattern,
                Pattern::Constructor(name, bindings) if name == "Shape.Rect" && bindings == &["w", "h"]
            ));
            assert!(matches!(
                &arms[2].pattern,
                Pattern::Constructor(name, bindings) if name == "Shape.Point" && bindings.is_empty()
            ));
        } else {
            panic!("expected match body");
        }
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn parse_fails_on_any_type_annotation() {
    let src = "fn f(x: Any) -> Int\n    x\n";
    assert!(parse_fails(src));
}

#[test]
fn parse_tcp_connection_record_constructor_is_syntactic() {
    // Phase 4.7+ fix #11 — `Tcp.Connection` is a capability resource at the
    // typechecker's side, but the *parser* still recognises the
    // dotted record-constructor shape so the typecheck diagnostic
    // can point at the offending call site. The reject lives in
    // `typechecker_spec::error_tcp_connection_manual_construction_is_opaque`.
    let src = "c = Tcp.Connection(id = \"x\", host = \"127.0.0.1\", port = 6379)\n";
    let items = parse(src);
    assert!(!items.is_empty());
}

#[test]
fn parse_singleton_variant_with_parens_is_parse_error() {
    let src = "type Shape\n  Point\np = Shape.Point()\n";
    let msg = parse_error(src);
    assert!(
        msg.contains("Zero-argument constructor call 'Shape.Point()' is not allowed"),
        "unexpected parse error: {}",
        msg
    );
}

#[test]
fn effect_set_single_is_parse_error() {
    let msg = parse_error("effects AppIO = [Console.print]");
    assert!(
        msg.contains("module-level declaration"),
        "unexpected parse error: {}",
        msg
    );
}

#[test]
fn effect_set_multiple_is_parse_error() {
    let msg = parse_error("effects AppIO = [Console.print, Disk.readText, Http.get]");
    assert!(
        msg.contains("module-level declaration"),
        "unexpected parse error: {}",
        msg
    );
}

// ---------------------------------------------------------------------------
// Record update
// ---------------------------------------------------------------------------

#[test]
fn record_update_parses() {
    let src = "record User\n    name: String\n    age: Int\n\nfn f(u: User) -> User\n    User.update(u, age = 31)\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[1] {
        match single_expr_body(fd) {
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => {
                assert_eq!(type_name, "User");
                assert!(matches!(&base.as_ref().node, Expr::Ident(n) if n == "u"));
                assert_eq!(updates.len(), 1);
                assert_eq!(updates[0].0, "age");
            }
            other => panic!("expected RecordUpdate, got {:?}", other),
        }
    } else {
        panic!("expected FnDef");
    }
}

#[test]
fn record_update_multiple_fields() {
    let src = "record User\n    name: String\n    age: Int\n\nfn f(u: User) -> User\n    User.update(u, name = \"Bob\", age = 31)\n";
    let items = parse(src);
    if let TopLevel::FnDef(fd) = &items[1] {
        match single_expr_body(fd) {
            Expr::RecordUpdate {
                type_name, updates, ..
            } => {
                assert_eq!(type_name, "User");
                assert_eq!(updates.len(), 2);
                assert_eq!(updates[0].0, "name");
                assert_eq!(updates[1].0, "age");
            }
            other => panic!("expected RecordUpdate, got {:?}", other),
        }
    } else {
        panic!("expected FnDef");
    }
}

// ---------------------------------------------------------------------------
// Opaque types
// ---------------------------------------------------------------------------

#[test]
fn module_with_exposes_opaque() {
    let src = "module Pricing\n    exposes [mkDiscount]\n    exposes opaque [Discount]\n    intent =\n        \"Opaque demo.\"\n";
    let items = parse(src);
    if let TopLevel::Module(m) = &items[0] {
        assert_eq!(m.exposes, vec!["mkDiscount".to_string()]);
        assert_eq!(m.exposes_opaque, vec!["Discount".to_string()]);
    } else {
        panic!("expected Module");
    }
}

#[test]
fn module_exposes_opaque_multiple() {
    let src = "module Pricing\n    exposes opaque [Discount, TaxRate]\n    intent =\n        \"Two opaque types.\"\n";
    let items = parse(src);
    if let TopLevel::Module(m) = &items[0] {
        assert_eq!(
            m.exposes_opaque,
            vec!["Discount".to_string(), "TaxRate".to_string()]
        );
    } else {
        panic!("expected Module");
    }
}

// ---------------------------------------------------------------------------
// Unary minus → Expr::Neg (Iron — A1)
// ---------------------------------------------------------------------------
//
// The parser used to desugar `-x` into `BinOp(Sub, Literal(Int(0)), x)`,
// producing an `Int`/`Float` mixed binop and clobbering the IEEE sign
// bit on `-0.0`. The Iron rework gives unary minus a first-class
// `Expr::Neg` node. These specs lock the new shape down.

#[test]
fn unary_minus_int_literal_emits_neg() {
    let items = parse("x = -5");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Binding(
            "x".to_string(),
            None,
            Spanned::bare(Expr::Neg(Box::new(Spanned::bare(Expr::Literal(
                Literal::Int(5)
            )))))
        ))]
    );
}

#[test]
fn unary_minus_float_literal_emits_neg() {
    let items = parse("y = -1.5");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Binding(
            "y".to_string(),
            None,
            Spanned::bare(Expr::Neg(Box::new(Spanned::bare(Expr::Literal(
                Literal::Float(1.5)
            )))))
        ))]
    );
}

#[test]
fn unary_minus_ident_emits_neg() {
    let items = parse("fn negate(x: Int) -> Int\n    -x");
    let fd = match &items[0] {
        TopLevel::FnDef(fd) => fd,
        other => panic!("expected FnDef, got {:?}", other),
    };
    match single_expr_body(fd) {
        Expr::Neg(inner) => match &inner.node {
            Expr::Ident(name) => assert_eq!(name, "x"),
            other => panic!("expected Ident, got {:?}", other),
        },
        other => panic!("expected Neg, got {:?}", other),
    }
}

#[test]
fn nested_unary_minus_emits_nested_neg() {
    let items = parse("z = -(-1.0)");
    assert_eq!(
        items,
        vec![TopLevel::Stmt(Stmt::Binding(
            "z".to_string(),
            None,
            Spanned::bare(Expr::Neg(Box::new(Spanned::bare(Expr::Neg(Box::new(
                Spanned::bare(Expr::Literal(Literal::Float(1.0)))
            ))))))
        ))]
    );
}

#[test]
fn explicit_zero_minus_stays_binop_sub() {
    // `0 - x` is an explicit subtraction, not a unary-minus desugar,
    // and must keep its `BinOp(Sub, 0, x)` shape so the typechecker /
    // backends route it through the binary numeric path.
    let items = parse("fn f(x: Int) -> Int\n    0 - x");
    let fd = match &items[0] {
        TopLevel::FnDef(fd) => fd,
        other => panic!("expected FnDef, got {:?}", other),
    };
    match single_expr_body(fd) {
        Expr::BinOp(BinOp::Sub, left, right) => {
            assert!(matches!(&left.node, Expr::Literal(Literal::Int(0))));
            assert!(matches!(&right.node, Expr::Ident(name) if name == "x"));
        }
        other => panic!("expected BinOp(Sub, 0, x), got {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Crash-resistance (Iron — C1)
// ---------------------------------------------------------------------------
//
// Mirror of the lexer crash-resistance property in lexer_spec.rs:
// the parser must always return `Result<…, ParseError>` for any
// token stream the lexer was able to produce — never panic, never
// loop. End-to-end pipeline test asserts the combined frontend
// reports errors via the Result chain rather than crashing the
// process.
//
// Why: the parser is recursive-descent over a hand-written state
// machine with many `expect_*` invariants. Most invariants are
// reached only on well-formed input; malformed input should funnel
// through error recovery instead of triggering an `expect` that
// has no graceful fallback. Fuzzing surfaces exactly that class of
// bug.

proptest! {
    #[test]
    fn parser_does_not_panic_on_arbitrary_utf8(src in "[\\PC\\s]{0,512}") {
        let mut lexer = Lexer::new(&src);
        if let Ok(tokens) = lexer.tokenize() {
            let mut parser = Parser::new(tokens);
            let _ = parser.parse();
        }
    }

    #[test]
    fn parser_does_not_panic_on_arbitrary_bytes(
        bytes in prop::collection::vec(prop::num::u8::ANY, 0..512),
    ) {
        let src = String::from_utf8_lossy(&bytes);
        let mut lexer = Lexer::new(&src);
        if let Ok(tokens) = lexer.tokenize() {
            let mut parser = Parser::new(tokens);
            let _ = parser.parse();
        }
    }
}

// ---------------------------------------------------------------------------
// Effect lists: the separator is required
// ---------------------------------------------------------------------------

/// Two effect names in a row used to be read as two effects, so a dropped
/// comma declared both and said nothing — and `aver format` reprinted the
/// source as written, so the typo survived check, verify, format and compile.
#[test]
fn an_effect_list_without_a_separator_is_a_parse_error() {
    let src = concat!(
        "fn main() -> Unit\n",
        "    ? \"Print a line.\"\n",
        "    ! [Console.error Console.print]\n",
        "    Console.print(\"hello\")\n",
    );
    let err = parse_error(src);
    assert!(
        err.contains("Expected ',' or ']' after effect `Console.error`"),
        "the refusal must name the effect it stopped after: {err}"
    );
    assert!(
        err.contains("separated by commas"),
        "the refusal must say what was missing: {err}"
    );
}

/// The same rule on the module header, which parses its effect list through
/// the same function.
#[test]
fn a_module_effect_list_without_a_separator_is_a_parse_error() {
    let src = concat!(
        "module Main\n",
        "    intent = \"Probe the separator rule.\"\n",
        "    effects [Console.error Console.print]\n",
    );
    assert!(parse_fails(src), "a module header must carry the same rule");
}

/// And in type position, where an effect list rides on an `Fn` type.
#[test]
fn an_fn_type_effect_list_without_a_separator_is_a_parse_error() {
    let src = concat!(
        "fn apply(f: Fn(String) -> Unit ! [Console.print Console.error], s: String) -> Unit\n",
        "    ? \"Applies a printer.\"\n",
        "    ! [Console.print]\n",
        "    f(s)\n",
    );
    assert!(parse_fails(src), "an Fn type must carry the same rule");
}

/// Everything a well-formed list is allowed to look like keeps parsing: one
/// entry, several on a line, several across lines, a trailing comma, and none
/// at all.
#[test]
fn well_formed_effect_lists_still_parse() {
    let shapes = [
        "! [Console.print]",
        "! [Console.print, Console.error]",
        "! [\n        Console.print,\n        Console.error,\n    ]",
        "! []",
    ];
    for shape in shapes {
        let src = format!(
            "fn main() -> Unit\n    ? \"Print a line.\"\n    {shape}\n    Console.print(\"hello\")\n"
        );
        let items = parse(&src);
        assert_eq!(items.len(), 1, "expected one item from: {shape}");
    }
}
