/// Spec tests for the Aver lexer.
///
/// Each test verifies that specific source text tokenises to the expected
/// sequence of token kinds.  Structural tokens (Newline, Eof) are filtered out
/// unless the test is specifically about structure.
use aver::lexer::{Lexer, TokenKind};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn lex_all(src: &str) -> Vec<TokenKind> {
    let mut lexer = Lexer::new(src);
    lexer
        .tokenize()
        .unwrap()
        .into_iter()
        .map(|t| t.kind)
        .collect()
}

/// Filter out Newline and Eof tokens — tests focus on meaningful tokens only.
fn lex(src: &str) -> Vec<TokenKind> {
    lex_all(src)
        .into_iter()
        .filter(|k| !matches!(k, TokenKind::Newline | TokenKind::Eof))
        .collect()
}

fn lex_err(src: &str) -> bool {
    Lexer::new(src).tokenize().is_err()
}

// ---------------------------------------------------------------------------
// Literals
// ---------------------------------------------------------------------------

#[test]
fn int_positive() {
    assert_eq!(lex("42"), vec![TokenKind::Int(42)]);
}

#[test]
fn int_zero() {
    assert_eq!(lex("0"), vec![TokenKind::Int(0)]);
}

#[test]
fn int_large() {
    assert_eq!(lex("1000000"), vec![TokenKind::Int(1_000_000)]);
}

#[test]
fn float_basic() {
    assert_eq!(lex("3.14"), vec![TokenKind::Float(3.14)]);
}

#[test]
fn float_zero() {
    assert_eq!(lex("0.0"), vec![TokenKind::Float(0.0)]);
}

#[test]
fn string_empty() {
    assert_eq!(lex("\"\""), vec![TokenKind::Str("".to_string())]);
}

#[test]
fn string_basic() {
    assert_eq!(lex("\"hello\""), vec![TokenKind::Str("hello".to_string())]);
}

#[test]
fn string_with_spaces() {
    assert_eq!(
        lex("\"hello world\""),
        vec![TokenKind::Str("hello world".to_string())]
    );
}

#[test]
fn bool_true() {
    assert_eq!(lex("true"), vec![TokenKind::Bool(true)]);
}

#[test]
fn bool_false() {
    assert_eq!(lex("false"), vec![TokenKind::Bool(false)]);
}

// ---------------------------------------------------------------------------
// Identifiers vs keywords
// ---------------------------------------------------------------------------

#[test]
fn ident_simple() {
    assert_eq!(lex("myVar"), vec![TokenKind::Ident("myVar".to_string())]);
}

#[test]
fn ident_camel_case() {
    assert_eq!(
        lex("camelCase"),
        vec![TokenKind::Ident("camelCase".to_string())]
    );
}

#[test]
fn ident_with_numbers() {
    assert_eq!(lex("x1"), vec![TokenKind::Ident("x1".to_string())]);
}

#[test]
fn keyword_val() {
    assert_eq!(lex("val"), vec![TokenKind::Val]);
}

#[test]
fn keyword_var() {
    assert_eq!(lex("var"), vec![TokenKind::Var]);
}

#[test]
fn keyword_fn() {
    assert_eq!(lex("fn"), vec![TokenKind::Fn]);
}

#[test]
fn keyword_match() {
    assert_eq!(lex("match"), vec![TokenKind::Match]);
}

#[test]
fn keyword_verify() {
    assert_eq!(lex("verify"), vec![TokenKind::Verify]);
}

#[test]
fn keyword_decision() {
    assert_eq!(lex("decision"), vec![TokenKind::Decision]);
}

#[test]
fn keyword_module() {
    assert_eq!(lex("module"), vec![TokenKind::Module]);
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

#[test]
fn constructor_ok() {
    assert_eq!(lex("Ok"), vec![TokenKind::Ok]);
}

#[test]
fn constructor_err() {
    assert_eq!(lex("Err"), vec![TokenKind::Err]);
}

#[test]
fn constructor_some() {
    assert_eq!(lex("Some"), vec![TokenKind::Some]);
}

#[test]
fn constructor_none() {
    assert_eq!(lex("None"), vec![TokenKind::None]);
}

// ---------------------------------------------------------------------------
// Operators
// ---------------------------------------------------------------------------

#[test]
fn op_plus() {
    assert_eq!(lex("+"), vec![TokenKind::Plus]);
}

#[test]
fn op_minus() {
    assert_eq!(lex("-"), vec![TokenKind::Minus]);
}

#[test]
fn op_star() {
    assert_eq!(lex("*"), vec![TokenKind::Star]);
}

#[test]
fn op_slash() {
    assert_eq!(lex("/"), vec![TokenKind::Slash]);
}

#[test]
fn op_eq() {
    assert_eq!(lex("=="), vec![TokenKind::Eq]);
}

#[test]
fn op_neq() {
    assert_eq!(lex("!="), vec![TokenKind::Neq]);
}

#[test]
fn op_lte() {
    assert_eq!(lex("<="), vec![TokenKind::Lte]);
}

#[test]
fn op_gte() {
    assert_eq!(lex(">="), vec![TokenKind::Gte]);
}

#[test]
fn op_lt() {
    assert_eq!(lex("<"), vec![TokenKind::Lt]);
}

#[test]
fn op_gt() {
    assert_eq!(lex(">"), vec![TokenKind::Gt]);
}

#[test]
fn op_pipe() {
    assert_eq!(lex("|>"), vec![TokenKind::Pipe]);
}

#[test]
fn op_arrow() {
    assert_eq!(lex("->"), vec![TokenKind::Arrow]);
}

#[test]
fn op_assign() {
    assert_eq!(lex("="), vec![TokenKind::Assign]);
}

#[test]
fn op_question() {
    assert_eq!(lex("?"), vec![TokenKind::Question]);
}

#[test]
fn op_bang() {
    assert_eq!(lex("!"), vec![TokenKind::Bang]);
}

#[test]
fn op_colon() {
    assert_eq!(lex(":"), vec![TokenKind::Colon]);
}

#[test]
fn op_comma() {
    assert_eq!(lex(","), vec![TokenKind::Comma]);
}

#[test]
fn op_dot() {
    assert_eq!(lex("."), vec![TokenKind::Dot]);
}

// ---------------------------------------------------------------------------
// String interpolation
// ---------------------------------------------------------------------------

#[test]
fn interp_plain_string_is_str_not_interp() {
    // A string with no {} should be Str, not InterpStr
    let kinds = lex("\"hello world\"");
    assert_eq!(kinds, vec![TokenKind::Str("hello world".to_string())]);
}

#[test]
fn interp_string_produces_interp_str() {
    let kinds = lex("\"Hello, {name}!\"");
    assert_eq!(kinds.len(), 1);
    assert!(matches!(&kinds[0], TokenKind::InterpStr(_)));
}

#[test]
fn interp_string_parts() {
    let kinds = lex("\"Hello, {name}!\"");
    if let TokenKind::InterpStr(parts) = &kinds[0] {
        assert_eq!(parts.len(), 3);
        assert_eq!(parts[0], (false, "Hello, ".to_string()));
        assert_eq!(parts[1], (true, "name".to_string()));
        assert_eq!(parts[2], (false, "!".to_string()));
    } else {
        panic!("expected InterpStr");
    }
}

#[test]
fn interp_string_expression_part() {
    let kinds = lex("\"{x + 1}\"");
    if let TokenKind::InterpStr(parts) = &kinds[0] {
        assert_eq!(parts.len(), 1);
        assert_eq!(parts[0].0, true); // is_expr
        assert_eq!(parts[0].1, "x + 1");
    } else {
        panic!("expected InterpStr");
    }
}

#[test]
fn interp_only_expression() {
    let kinds = lex("\"{value}\"");
    if let TokenKind::InterpStr(parts) = &kinds[0] {
        assert_eq!(parts.len(), 1);
        assert_eq!(parts[0], (true, "value".to_string()));
    } else {
        panic!("expected InterpStr");
    }
}

// ---------------------------------------------------------------------------
// Significant indentation (INDENT / DEDENT)
// ---------------------------------------------------------------------------

#[test]
fn indent_emitted_on_deeper_line() {
    let kinds = lex_all("fn:\n    x\n");
    assert!(
        kinds.contains(&TokenKind::Indent),
        "expected INDENT in: {:?}",
        kinds
    );
}

#[test]
fn dedent_emitted_on_shallower_line() {
    let kinds = lex_all("fn:\n    x\ny\n");
    assert!(
        kinds.contains(&TokenKind::Dedent),
        "expected DEDENT in: {:?}",
        kinds
    );
}

#[test]
fn nested_indentation_has_two_dedents() {
    let src = "a:\n    b:\n        c\n    d\ne\n";
    let kinds = lex_all(src);
    let dedent_count = kinds.iter().filter(|k| **k == TokenKind::Dedent).count();
    assert_eq!(dedent_count, 2, "expected 2 DEDENTs, got {}", dedent_count);
}

// ---------------------------------------------------------------------------
// Multi-token sequences
// ---------------------------------------------------------------------------

#[test]
fn negative_is_minus_plus_int() {
    // Unary minus is not a single token — lexer emits Minus then Int
    assert_eq!(lex("-5"), vec![TokenKind::Minus, TokenKind::Int(5)]);
}

#[test]
fn list_literal_tokens() {
    let kinds = lex("[1, 2, 3]");
    assert!(kinds.contains(&TokenKind::LBracket));
    assert!(kinds.contains(&TokenKind::RBracket));
    assert_eq!(
        kinds,
        vec![
            TokenKind::LBracket,
            TokenKind::Int(1),
            TokenKind::Comma,
            TokenKind::Int(2),
            TokenKind::Comma,
            TokenKind::Int(3),
            TokenKind::RBracket,
        ]
    );
}

#[test]
fn fn_call_tokens() {
    let kinds = lex("add(a, b)");
    assert_eq!(
        kinds,
        vec![
            TokenKind::Ident("add".to_string()),
            TokenKind::LParen,
            TokenKind::Ident("a".to_string()),
            TokenKind::Comma,
            TokenKind::Ident("b".to_string()),
            TokenKind::RParen,
        ]
    );
}

#[test]
fn arrow_not_confused_with_gt_minus() {
    // -> is Arrow, not Gt + Minus
    assert_eq!(lex("->"), vec![TokenKind::Arrow]);
}

#[test]
fn pipe_not_confused_with_bar_gt() {
    // |> is Pipe
    assert_eq!(lex("|>"), vec![TokenKind::Pipe]);
}

// ---------------------------------------------------------------------------
// Comments (should be skipped)
// ---------------------------------------------------------------------------

#[test]
fn line_comment_is_skipped() {
    // Aver uses // for line comments
    let kinds = lex("// this is a comment\n42");
    assert_eq!(kinds, vec![TokenKind::Int(42)]);
}

#[test]
fn inline_comment_is_skipped() {
    let kinds = lex("42 // trailing comment");
    assert_eq!(kinds, vec![TokenKind::Int(42)]);
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

#[test]
fn unterminated_string_is_error() {
    assert!(
        lex_err("\"hello"),
        "unterminated string should be a lex error"
    );
}

// ---------------------------------------------------------------------------
// Brace escape sequences: {{ → { and }} → }
// ---------------------------------------------------------------------------

#[test]
fn double_brace_open_is_literal() {
    // "{{" produces a plain Str containing "{"
    let kinds = lex("\"{{\"");
    assert_eq!(kinds, vec![TokenKind::Str("{".to_string())]);
}

#[test]
fn double_brace_close_is_literal() {
    let kinds = lex("\"}}\"");
    assert_eq!(kinds, vec![TokenKind::Str("}".to_string())]);
}

#[test]
fn double_braces_produce_empty_json() {
    let kinds = lex("\"{{}}\"");
    assert_eq!(kinds, vec![TokenKind::Str("{}".to_string())]);
}

#[test]
fn double_brace_mixed_with_interpolation() {
    // "{{key}}: {name}" → literal "{key}: " + expr "name"
    let kinds = lex("\"{{key}}: {name}\"");
    if let TokenKind::InterpStr(parts) = &kinds[0] {
        assert_eq!(parts[0], (false, "{key}: ".to_string()));
        assert_eq!(parts[1], (true, "name".to_string()));
    } else {
        panic!("expected InterpStr");
    }
}
