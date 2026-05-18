use super::{TypeChecker, run_type_check};
use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, TopLevel, Type};
use std::collections::HashMap;

fn errors(items: Vec<TopLevel>) -> Vec<String> {
    run_type_check(&items)
        .into_iter()
        .map(|e| e.message)
        .collect()
}

fn type_errors(items: Vec<TopLevel>) -> Vec<super::TypeError> {
    run_type_check(&items)
}

#[test]
fn top_level_statements_are_typechecked() {
    let items = vec![TopLevel::Stmt(Stmt::Binding(
        "x".to_string(),
        None,
        Spanned::bare(Expr::BinOp(
            BinOp::Add,
            Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
            Box::new(Spanned::bare(Expr::Literal(Literal::Str("a".to_string())))),
        )),
    ))];
    let errs = errors(items);
    assert!(
        errs.iter().any(|e| e.contains("Operator '+' requires")),
        "expected top-level BinOp type error, got: {:?}",
        errs
    );
}

#[test]
fn unknown_function_calls_are_errors() {
    let main_fn = FnDef {
        name: "main".to_string(),
        line: 1,
        params: vec![],
        return_type: "Unit".to_string(),
        effects: vec![],
        desc: None,
        body: std::sync::Arc::new(FnBody::Block(vec![Stmt::Expr(Spanned::bare(
            Expr::FnCall(
                Box::new(Spanned::bare(Expr::Ident("nosuch".to_string()))),
                vec![Spanned::bare(Expr::Literal(Literal::Int(1)))],
            ),
        ))])),
        resolution: None,
    };

    let errs = errors(vec![TopLevel::FnDef(main_fn)]);
    assert!(
        errs.iter()
            .any(|e| e.contains("Call to unknown function 'nosuch'")),
        "expected unknown function error, got: {:?}",
        errs
    );
}

#[test]
fn duplicate_binding_is_rejected() {
    let items = vec![
        TopLevel::Stmt(Stmt::Binding(
            "x".to_string(),
            None,
            Spanned::bare(Expr::Literal(Literal::Int(1))),
        )),
        TopLevel::Stmt(Stmt::Binding(
            "x".to_string(),
            None,
            Spanned::bare(Expr::Literal(Literal::Int(2))),
        )),
    ];
    let errs = errors(items);
    assert!(
        errs.iter().any(|e| e.contains("'x' is already defined")),
        "expected duplicate binding error, got: {:?}",
        errs
    );
}

#[test]
fn nested_attr_callee_key() {
    let expr = Expr::Attr(
        Box::new(Spanned::bare(Expr::Attr(
            Box::new(Spanned::bare(Expr::Ident("Models".to_string()))),
            "User".to_string(),
        ))),
        "findById".to_string(),
    );
    assert_eq!(
        TypeChecker::callee_key(&expr),
        Some("Models.User.findById".to_string())
    );
}

#[test]
fn non_exhaustive_match_reports_match_line() {
    let f = FnDef {
        name: "f".to_string(),
        line: 1,
        params: vec![("b".to_string(), "Bool".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: std::sync::Arc::new(FnBody::from_expr(Spanned::new(
            Expr::Match {
                subject: Box::new(Spanned::bare(Expr::Ident("b".to_string()))),
                arms: vec![MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(true)),
                    body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
                    binding_slots: std::sync::OnceLock::new(),
                }],
            },
            7,
        ))),
        resolution: None,
    };

    let errs = type_errors(vec![TopLevel::FnDef(f)]);
    let hit = errs
        .iter()
        .find(|e| e.message.contains("Non-exhaustive match"));
    assert!(
        hit.is_some(),
        "expected non-exhaustive match error: {errs:?}"
    );
    assert_eq!(hit.expect("checked above").line, 7);
}

#[test]
fn tuple_union_patterns_can_be_exhaustive_without_single_total_arm() {
    let f = FnDef {
        name: "f".to_string(),
        line: 1,
        params: vec![("t".to_string(), "Tuple<Bool, Bool>".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: std::sync::Arc::new(FnBody::from_expr(Spanned::new(
            Expr::Match {
                subject: Box::new(Spanned::bare(Expr::Ident("t".to_string()))),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Tuple(vec![
                            Pattern::Literal(Literal::Bool(true)),
                            Pattern::Wildcard,
                        ]),
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                    MatchArm {
                        pattern: Pattern::Tuple(vec![
                            Pattern::Literal(Literal::Bool(false)),
                            Pattern::Wildcard,
                        ]),
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(0)))),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                ],
            },
            9,
        ))),
        resolution: None,
    };

    let errs = type_errors(vec![TopLevel::FnDef(f)]);
    assert!(
        !errs
            .iter()
            .any(|e| e.message.contains("Non-exhaustive match")),
        "did not expect non-exhaustive error, got: {errs:?}"
    );
}

#[test]
fn nested_tuple_union_still_reports_missing_case() {
    let f = FnDef {
        name: "f".to_string(),
        line: 1,
        params: vec![("t".to_string(), "Tuple<Bool, Bool>".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: std::sync::Arc::new(FnBody::from_expr(Spanned::new(
            Expr::Match {
                subject: Box::new(Spanned::bare(Expr::Ident("t".to_string()))),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Tuple(vec![
                            Pattern::Literal(Literal::Bool(true)),
                            Pattern::Wildcard,
                        ]),
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                    MatchArm {
                        pattern: Pattern::Tuple(vec![
                            Pattern::Wildcard,
                            Pattern::Literal(Literal::Bool(true)),
                        ]),
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(2)))),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                ],
            },
            13,
        ))),
        resolution: None,
    };

    let errs = type_errors(vec![TopLevel::FnDef(f)]);
    let hit = errs
        .iter()
        .find(|e| e.message.contains("Non-exhaustive match"));
    assert!(
        hit.is_some(),
        "expected non-exhaustive match error, got: {errs:?}"
    );
    assert_eq!(hit.expect("checked above").line, 13);
}

fn parse_items(src: &str) -> Vec<TopLevel> {
    let mut lexer = crate::lexer::Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = crate::parser::Parser::new(tokens);
    parser.parse().expect("parse failed")
}

#[test]
fn result_with_default_rejects_option_argument() {
    // Regression: `Result.withDefault(Vector.get(...), 0)` is a foot-gun —
    // `Vector.get` returns `Option<T>` but `Result.withDefault` silently
    // accepted it through unresolved generic placeholders, passed `aver check`,
    // and at runtime returned the default for every lookup. Now caught at
    // type-check time.
    let items = parse_items(
        r#"
fn main() -> Int
    Result.withDefault(Vector.get(Vector.fromList([1, 2, 3]), 0), 99)
"#,
    );
    let errs = errors(items);
    assert!(
        errs.iter()
            .any(|e| e
                .contains("Argument 1 of 'Result.withDefault': expected Result<T, E>, got Option")),
        "expected Result.withDefault type error on Option arg, got: {errs:?}"
    );
}

#[test]
fn option_with_default_rejects_result_argument() {
    let items = parse_items(
        r#"
fn main() -> Int
    Option.withDefault(Int.mod(7, 3), 0)
"#,
    );
    let errs = errors(items);
    assert!(
        errs.iter()
            .any(|e| e
                .contains("Argument 1 of 'Option.withDefault': expected Option<T>, got Result")),
        "expected Option.withDefault type error on Result arg, got: {errs:?}"
    );
}

#[test]
fn result_with_default_accepts_result_argument() {
    let items = parse_items(
        r#"
fn main() -> Int
    Result.withDefault(Int.mod(7, 3), 0)
"#,
    );
    let errs = errors(items);
    assert!(
        !errs.iter().any(|e| e.contains("Result.withDefault")),
        "did not expect Result.withDefault error on Result arg, got: {errs:?}"
    );
}

#[test]
fn option_with_default_accepts_option_argument() {
    let items = parse_items(
        r#"
fn main() -> Int
    Option.withDefault(Vector.get(Vector.fromList([1, 2, 3]), 0), 99)
"#,
    );
    let errs = errors(items);
    assert!(
        !errs.iter().any(|e| e.contains("Option.withDefault")),
        "did not expect Option.withDefault error on Option arg, got: {errs:?}"
    );
}

#[test]
fn http_server_listen_with_rejects_mismatched_context() {
    // listenWith carries a user-defined context type; the second arg's
    // type must match the handler's first parameter type. Builtin sigs
    // can't express this with the current type system (no parametric
    // polymorphism), so it's enforced as a cross-arg check.
    let items = parse_items(
        r#"
record AppCtx
    config: String

fn handler(ctx: AppCtx, req: HttpRequest) -> HttpResponse
    HttpResponse(status = 200, body = "ok", headers = {})

fn main() -> Unit
    ! [HttpServer.listenWith]
    HttpServer.listenWith(8080, "wrong_kind_of_context", handler)
"#,
    );
    let errs = errors(items);
    assert!(
        errs.iter().any(|e| e.contains(
            "Argument 2 of 'HttpServer.listenWith': context type String must match handler's first parameter type AppCtx"
        )),
        "expected listenWith context-handler mismatch error, got: {errs:?}"
    );
}

#[test]
fn http_server_listen_with_accepts_matched_context() {
    let items = parse_items(
        r#"
record AppCtx
    config: String

fn handler(ctx: AppCtx, req: HttpRequest) -> HttpResponse
    HttpResponse(status = 200, body = ctx.config, headers = {})

fn main(ctx: AppCtx) -> Unit
    ! [HttpServer.listenWith]
    HttpServer.listenWith(8080, ctx, handler)
"#,
    );
    let errs = errors(items);
    assert!(
        !errs
            .iter()
            .any(|e| e.contains("HttpServer.listenWith") && e.contains("context type")),
        "did not expect listenWith context-handler error, got: {errs:?}"
    );
}

#[test]
fn self_host_runtime_listen_accepts_opaque_handler() {
    // `SelfHostRuntime.httpServerListen` is the self-host bridge
    // call — generated Rust passes a `Val` sumtype carrying the
    // evaluated guest fn (`Val::ValFn`), not a typed `Fn(...)`.
    // The checker keeps an open second arg (`Type::Var("Handler")`)
    // for this builtin so the bridge signature matches reality;
    // user-facing `HttpServer.listen` retains the strict
    // `Fn(HttpRequest) -> HttpResponse` shape.
    let items = parse_items(
        r#"
fn main(handler: Val) -> Result<Unit, String>
    ! [HttpServer.listen]
    SelfHostRuntime.httpServerListen(8080, handler)

type Val
    ValFn(Int)
"#,
    );
    let errs = errors(items);
    assert!(
        errs.is_empty(),
        "expected no errors for opaque self-host handler, got: {errs:?}"
    );
}

// ── Constraint-substitution edge cases ─────────────────────────────────
//
// Exercising `match_expected_type` / `bind_expected_var` directly so the
// matcher's contract — directional binding (expected.Var consumes
// actual; actual.Var stays opaque), occurs-check refusal, and
// per-binding consistency — is locked in. None of these tests reach
// through the full `run_type_check` driver; they live one layer below
// at the unification primitive itself.

#[test]
fn match_binds_expected_var_to_concrete_actual() {
    // `T` in expected, `Int` in actual → bind T := Int.
    let mut subst = HashMap::new();
    let ok =
        TypeChecker::match_expected_type(&Type::Int, &Type::Var("T".to_string()), &mut subst);
    assert!(ok, "expected match to succeed");
    assert_eq!(subst.get("T"), Some(&Type::Int));
}

#[test]
fn match_rejects_var_in_actual_against_concrete_expected() {
    // Asymmetric matching contract: `Var("T")` in the actual position
    // never satisfies a concrete expected type.
    let mut subst = HashMap::new();
    let ok =
        TypeChecker::match_expected_type(&Type::Var("T".to_string()), &Type::Int, &mut subst);
    assert!(
        !ok,
        "actual-side Var must not match a concrete expected; matcher returned true"
    );
}

#[test]
fn match_var_to_self_is_noop_true() {
    // `T` in expected, `T` in actual → trivially satisfied, no binding.
    let mut subst = HashMap::new();
    let ok = TypeChecker::match_expected_type(
        &Type::Var("T".to_string()),
        &Type::Var("T".to_string()),
        &mut subst,
    );
    assert!(ok, "expected `T == T` to match");
    assert!(subst.is_empty(), "self-bind should not populate subst");
}

#[test]
fn second_bind_must_match_first() {
    // Caller binds `T := Int` then asks for `T := Str` — second bind
    // surfaces as incompatible.
    let mut subst = HashMap::new();
    assert!(TypeChecker::match_expected_type(
        &Type::Int,
        &Type::Var("T".to_string()),
        &mut subst,
    ));
    let ok =
        TypeChecker::match_expected_type(&Type::Str, &Type::Var("T".to_string()), &mut subst);
    assert!(
        !ok,
        "second bind with conflicting type must fail; got success with subst={subst:?}"
    );
    assert_eq!(subst.get("T"), Some(&Type::Int));
}

#[test]
fn occurs_check_rejects_t_bound_to_list_of_t() {
    // Phase 4.7+ pass 6 (0.20.1) — bind `T := List<T>` is refused at
    // the source of the substitution map. The matcher signals failure
    // via the boolean result; callers translate that into a normal
    // "expected T, got List<T>" error in user-visible diagnostics.
    let mut subst = HashMap::new();
    let actual = Type::List(Box::new(Type::Var("T".to_string())));
    let ok =
        TypeChecker::match_expected_type(&actual, &Type::Var("T".to_string()), &mut subst);
    assert!(
        !ok,
        "occurs-check failure expected; matcher accepted `T := List<T>` and subst={subst:?}"
    );
    assert!(
        subst.is_empty(),
        "rejected bind must not pollute subst, got {subst:?}"
    );
}

#[test]
fn occurs_check_walks_nested_structures() {
    // The check has to recurse through every Type variant. Exercise
    // each compound shape so a future refactor that forgets a branch
    // (Map / Tuple / Fn / Result / Option / Vector) gets caught.
    for actual in [
        Type::Option(Box::new(Type::Var("T".to_string()))),
        Type::Vector(Box::new(Type::Var("T".to_string()))),
        Type::Result(Box::new(Type::Var("T".to_string())), Box::new(Type::Str)),
        Type::Result(Box::new(Type::Int), Box::new(Type::Var("T".to_string()))),
        Type::Map(Box::new(Type::Var("T".to_string())), Box::new(Type::Int)),
        Type::Map(Box::new(Type::Int), Box::new(Type::Var("T".to_string()))),
        Type::Tuple(vec![Type::Int, Type::Var("T".to_string()), Type::Str]),
        Type::Fn(
            vec![Type::Var("T".to_string())],
            Box::new(Type::Unit),
            vec![],
        ),
        Type::Fn(
            vec![Type::Int],
            Box::new(Type::Var("T".to_string())),
            vec![],
        ),
        // Nested two-deep — the rarer real-world shape.
        Type::List(Box::new(Type::List(Box::new(Type::Var("T".to_string()))))),
    ] {
        let mut subst = HashMap::new();
        let ok = TypeChecker::match_expected_type(
            &actual,
            &Type::Var("T".to_string()),
            &mut subst,
        );
        assert!(
            !ok,
            "occurs check missed `T` inside {actual:?}; matcher returned true with subst={subst:?}"
        );
    }
}

#[test]
fn unrelated_var_binds_normally_even_when_actual_mentions_other_var() {
    // Binding `U := List<T>` is fine — only `T := …T…` is the
    // occurs-failure shape. Makes sure the check is name-scoped, not
    // "reject any actual containing any Var".
    let mut subst = HashMap::new();
    let actual = Type::List(Box::new(Type::Var("T".to_string())));
    let ok =
        TypeChecker::match_expected_type(&actual, &Type::Var("U".to_string()), &mut subst);
    assert!(
        ok,
        "binding U to List<T> should succeed (no occurs of U), got false; subst={subst:?}"
    );
    assert_eq!(subst.get("U"), Some(&actual));
}
