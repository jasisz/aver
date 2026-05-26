use super::{TypeChecker, run_type_check};
use crate::ast::{
    BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, TopLevel, Type,
};
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
fn duplicate_wildcard_arms_are_unreachable() {
    let f = FnDef {
        name: "f".to_string(),
        line: 1,
        params: vec![("n".to_string(), "Int".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: std::sync::Arc::new(FnBody::from_expr(Spanned::new(
            Expr::Match {
                subject: Box::new(Spanned::bare(Expr::Ident("n".to_string()))),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Spanned::new(Expr::Literal(Literal::Int(1)), 3)),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Spanned::new(Expr::Literal(Literal::Int(2)), 4)),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                ],
            },
            2,
        ))),
        resolution: None,
    };
    let errs = type_errors(vec![TopLevel::FnDef(f)]);
    let hit = errs
        .iter()
        .find(|e| e.message.contains("Unreachable match arm"));
    assert!(hit.is_some(), "expected unreachable arm error: {errs:?}");
    assert_eq!(hit.expect("checked above").line, 4);
    assert!(
        hit.expect("checked above").message.contains("line 3"),
        "earlier arm line should be cited: {:?}",
        hit
    );
}

#[test]
fn duplicate_literal_int_arms_are_unreachable() {
    let f = FnDef {
        name: "f".to_string(),
        line: 1,
        params: vec![("n".to_string(), "Int".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: std::sync::Arc::new(FnBody::from_expr(Spanned::new(
            Expr::Match {
                subject: Box::new(Spanned::bare(Expr::Ident("n".to_string()))),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Int(0)),
                        body: Box::new(Spanned::new(Expr::Literal(Literal::Int(1)), 3)),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Int(0)),
                        body: Box::new(Spanned::new(Expr::Literal(Literal::Int(2)), 4)),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Spanned::new(Expr::Literal(Literal::Int(3)), 5)),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                ],
            },
            2,
        ))),
        resolution: None,
    };
    let errs = type_errors(vec![TopLevel::FnDef(f)]);
    let hit = errs
        .iter()
        .find(|e| e.message.contains("Unreachable match arm"));
    assert!(hit.is_some(), "expected unreachable arm error: {errs:?}");
    assert_eq!(hit.expect("checked above").line, 4);
}

#[test]
fn distinct_literal_int_arms_are_ok() {
    let f = FnDef {
        name: "f".to_string(),
        line: 1,
        params: vec![("n".to_string(), "Int".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: std::sync::Arc::new(FnBody::from_expr(Spanned::new(
            Expr::Match {
                subject: Box::new(Spanned::bare(Expr::Ident("n".to_string()))),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Int(0)),
                        body: Box::new(Spanned::new(Expr::Literal(Literal::Int(1)), 3)),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Int(1)),
                        body: Box::new(Spanned::new(Expr::Literal(Literal::Int(2)), 4)),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Spanned::new(Expr::Literal(Literal::Int(3)), 5)),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                ],
            },
            2,
        ))),
        resolution: None,
    };
    let errs = type_errors(vec![TopLevel::FnDef(f)]);
    assert!(
        !errs
            .iter()
            .any(|e| e.message.contains("Unreachable match arm")),
        "distinct literals must not trigger redundancy: {errs:?}"
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
    let ok = TypeChecker::match_expected_type(&Type::Int, &Type::Var("T".to_string()), &mut subst);
    assert!(ok, "expected match to succeed");
    assert_eq!(subst.get("T"), Some(&Type::Int));
}

#[test]
fn match_rejects_var_in_actual_against_concrete_expected() {
    // Asymmetric matching contract: `Var("T")` in the actual position
    // never satisfies a concrete expected type.
    let mut subst = HashMap::new();
    let ok = TypeChecker::match_expected_type(&Type::Var("T".to_string()), &Type::Int, &mut subst);
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
    let ok = TypeChecker::match_expected_type(&Type::Str, &Type::Var("T".to_string()), &mut subst);
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
    let ok = TypeChecker::match_expected_type(&actual, &Type::Var("T".to_string()), &mut subst);
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
        let ok = TypeChecker::match_expected_type(&actual, &Type::Var("T".to_string()), &mut subst);
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
    let ok = TypeChecker::match_expected_type(&actual, &Type::Var("U".to_string()), &mut subst);
    assert!(
        ok,
        "binding U to List<T> should succeed (no occurs of U), got false; subst={subst:?}"
    );
    assert_eq!(subst.get("U"), Some(&actual));
}

// ── Property tests (Iron — C2) ─────────────────────────────────────────
//
// Locks the matcher's contract as a set of invariants instead of
// hand-picked cases:
//   - `type_contains_var` always terminates on every Type AST shape
//     (no recursion into circular structures because the AST is
//     finite — but a future variant addition that forgets to recurse
//     into a sub-Type would also surface here)
//   - `match_expected_type` is deterministic: same (actual, expected)
//     produces the same return value and the same final substitution,
//     regardless of how many times you run it from a fresh subst
//   - The constraint `match(a, b)` is independent of the prior subst
//     state when both inputs are ground (contain no `Var`)
//
// proptest defaults to 256 cases per property. CI gate bumps that via
// `PROPTEST_CASES=10000`; local iteration can shrink via the same env
// var or per-`proptest!`-block config.

mod proptest_strategies {
    use super::Type;
    use proptest::prelude::*;

    /// Generator for arbitrary `Type` ASTs. Recursive, depth-capped so
    /// generation always terminates. Includes every variant the
    /// matcher dispatches on so a new branch addition that forgets
    /// either side fails fast under property runs.
    pub(super) fn arb_type() -> impl Strategy<Value = Type> {
        // Leaves — every non-recursive `Type` variant. Named + Var
        // carry a short identifier; the alphabet is intentionally
        // tight so the property runner exercises name collisions
        // (e.g. two `Var("A")` in different positions) at high rate.
        let leaf = prop_oneof![
            Just(Type::Int),
            Just(Type::Float),
            Just(Type::Str),
            Just(Type::Bool),
            Just(Type::Unit),
            Just(Type::Invalid),
            "[A-Z][a-zA-Z]{0,4}".prop_map(Type::named),
            "[A-Z]".prop_map(Type::Var),
        ];

        // Recursive part — every compound `Type` variant. The
        // depth/breadth caps below keep generation bounded:
        //   depth ≤ 4 levels of nesting
        //   total node budget 16 (prevents combinatorial blowup)
        //   collection size ≤ 4 elements
        leaf.prop_recursive(4, 16, 4, |inner| {
            prop_oneof![
                inner.clone().prop_map(|t| Type::Option(Box::new(t))),
                inner.clone().prop_map(|t| Type::List(Box::new(t))),
                inner.clone().prop_map(|t| Type::Vector(Box::new(t))),
                (inner.clone(), inner.clone())
                    .prop_map(|(a, b)| Type::Result(Box::new(a), Box::new(b))),
                (inner.clone(), inner.clone())
                    .prop_map(|(k, v)| Type::Map(Box::new(k), Box::new(v))),
                prop::collection::vec(inner.clone(), 1..4).prop_map(Type::Tuple),
                (
                    prop::collection::vec(inner.clone(), 0..3),
                    inner.clone(),
                    prop::collection::vec("[A-Z][a-z]{0,4}", 0..2),
                )
                    .prop_map(|(params, ret, effects)| {
                        Type::Fn(params, Box::new(ret), effects)
                    }),
            ]
        })
    }

    /// Arbitrary variable name — same alphabet as the `Var` leaf so
    /// `arb_var_name()` and a `Var(...)` inside `arb_type()` clash at
    /// a meaningful rate.
    pub(super) fn arb_var_name() -> impl Strategy<Value = String> {
        "[A-Z]".prop_map(|s| s.to_string())
    }

    /// Build a `Type` that is guaranteed to contain `Var(name)`
    /// inside at least one compound layer. Used to exercise the
    /// occurs check without relying on `prop_assume` filtering on
    /// arbitrary random Types (most don't mention any given name).
    ///
    /// The base layer enumerates every compound-Type variant the
    /// matcher dispatches on, wrapping `Var(name)` in exactly one
    /// constructor — so the bare `Var(name)` case (which is a
    /// legitimate self-bind, not an occurs violation) is never
    /// produced. The recursive layer adds further nesting on top
    /// for depth coverage.
    ///
    /// Sibling positions in binary constructors (`Result.Err`,
    /// `Map.K`) get a fresh ground type so the wrapper chain has the
    /// target Var on a single, predictable path — guarantees the
    /// test exercises both the "left arm has var" and "right arm has
    /// var" shapes symmetrically.
    pub(super) fn arb_wrapped_type(name: String) -> impl Strategy<Value = Type> {
        let var = Type::Var(name);
        // Exactly-one-wrap base shapes. Every compound variant + both
        // arms of binary constructors are represented so a future
        // occurs-check regression on a single missed branch surfaces.
        let one_wrap = prop::sample::select(vec![
            Type::Option(Box::new(var.clone())),
            Type::List(Box::new(var.clone())),
            Type::Vector(Box::new(var.clone())),
            Type::Result(Box::new(var.clone()), Box::new(Type::Int)),
            Type::Result(Box::new(Type::Int), Box::new(var.clone())),
            Type::Map(Box::new(var.clone()), Box::new(Type::Int)),
            Type::Map(Box::new(Type::Int), Box::new(var.clone())),
            Type::Tuple(vec![Type::Int, var.clone(), Type::Bool]),
            Type::Fn(vec![var.clone()], Box::new(Type::Unit), vec![]),
            Type::Fn(vec![Type::Int], Box::new(var.clone()), vec![]),
        ]);
        one_wrap.prop_recursive(3, 12, 2, |inner| {
            prop_oneof![
                inner.clone().prop_map(|t| Type::Option(Box::new(t))),
                inner.clone().prop_map(|t| Type::List(Box::new(t))),
                inner.clone().prop_map(|t| Type::Vector(Box::new(t))),
                inner
                    .clone()
                    .prop_map(|t| Type::Result(Box::new(t), Box::new(Type::Int))),
                inner
                    .clone()
                    .prop_map(|t| Type::Result(Box::new(Type::Int), Box::new(t))),
                inner.prop_map(|t| Type::Tuple(vec![Type::Int, t])),
            ]
        })
    }
}

proptest::proptest! {
    /// `type_contains_var` must return a bool on every input shape,
    /// never panic, never loop. The Type AST is finite (no cycles),
    /// so termination follows trivially from structural recursion —
    /// the property guards against a future variant addition that
    /// forgets to recurse, leaving an `_` arm that quietly returns
    /// false or a missed branch that panics.
    #[test]
    fn occurs_check_terminates_on_any_type(
        ty in proptest_strategies::arb_type(),
        name in proptest_strategies::arb_var_name(),
    ) {
        // The fact that this returns at all is the property. The
        // result itself is checked structurally below.
        let _ = TypeChecker::type_contains_var(&ty, &name);
    }

    /// Matcher determinism: same `(actual, expected)` pair always
    /// produces the same return + same final substitution map,
    /// starting from a fresh empty subst. Catches any implicit
    /// dependency on global state, RNG, hash iteration order, or
    /// non-deterministic short-circuit ordering.
    #[test]
    fn match_expected_type_is_deterministic(
        actual in proptest_strategies::arb_type(),
        expected in proptest_strategies::arb_type(),
    ) {
        let mut s1 = HashMap::new();
        let mut s2 = HashMap::new();
        let r1 = TypeChecker::match_expected_type(&actual, &expected, &mut s1);
        let r2 = TypeChecker::match_expected_type(&actual, &expected, &mut s2);
        proptest::prop_assert_eq!(r1, r2);
        proptest::prop_assert_eq!(s1, s2);
    }

    /// Occurs-check soundness: if the matcher tries to bind a name
    /// to a Type that contains the same name, the result must be
    /// `false` AND the subst must stay empty. This is the property
    /// the hand-picked `occurs_check_rejects_t_bound_to_list_of_t`
    /// test asserts on one shape — here we sweep every shape the
    /// `arb_wrapped_type` generator can produce.
    ///
    /// `arb_wrapped_type` builds the Type by wrapping `Var(name)`
    /// inside a stack of compound-Type constructors picked at random.
    /// This avoids the prop_assume-rejection problem we'd hit by
    /// filtering random Types for ones that happen to contain the
    /// chosen Var: by construction, every generated Type contains
    /// the Var.
    #[test]
    fn occurs_violation_never_populates_subst(
        (name, ty) in proptest::prelude::Strategy::prop_flat_map(
            proptest_strategies::arb_var_name(),
            |n| {
                proptest::prelude::Strategy::prop_map(
                    proptest_strategies::arb_wrapped_type(n.clone()),
                    move |t| (n.clone(), t),
                )
            },
        ),
    ) {
        let mut subst = HashMap::new();
        let r = TypeChecker::match_expected_type(
            &ty,
            &Type::Var(name.clone()),
            &mut subst,
        );
        proptest::prop_assert!(
            !r,
            "occurs check should reject `{name} := {ty:?}`, but matcher returned true"
        );
        proptest::prop_assert!(
            subst.is_empty(),
            "rejected occurs bind must not populate subst, got {subst:?}"
        );
    }
}
