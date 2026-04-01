use super::{TypeChecker, run_type_check};
use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, TopLevel};

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
        params: vec![("t".to_string(), "(Bool, Bool)".to_string())],
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
                    },
                    MatchArm {
                        pattern: Pattern::Tuple(vec![
                            Pattern::Literal(Literal::Bool(false)),
                            Pattern::Wildcard,
                        ]),
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(0)))),
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
        params: vec![("t".to_string(), "(Bool, Bool)".to_string())],
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
                    },
                    MatchArm {
                        pattern: Pattern::Tuple(vec![
                            Pattern::Wildcard,
                            Pattern::Literal(Literal::Bool(true)),
                        ]),
                        body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(2)))),
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
