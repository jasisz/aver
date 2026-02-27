use super::{run_type_check, TypeChecker};
use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Stmt, TopLevel};

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
        Expr::BinOp(
            BinOp::Add,
            Box::new(Expr::Literal(Literal::Int(1))),
            Box::new(Expr::Literal(Literal::Str("a".to_string()))),
        ),
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
        body: std::rc::Rc::new(FnBody::Block(vec![Stmt::Expr(Expr::FnCall(
            Box::new(Expr::Ident("nosuch".to_string())),
            vec![Expr::Literal(Literal::Int(1))],
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
            Expr::Literal(Literal::Int(1)),
        )),
        TopLevel::Stmt(Stmt::Binding(
            "x".to_string(),
            None,
            Expr::Literal(Literal::Int(2)),
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
        Box::new(Expr::Attr(
            Box::new(Expr::Ident("Models".to_string())),
            "User".to_string(),
        )),
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
        body: std::rc::Rc::new(FnBody::Expr(Expr::Match {
            subject: Box::new(Expr::Ident("b".to_string())),
            arms: vec![MatchArm {
                pattern: Pattern::Literal(Literal::Bool(true)),
                body: Box::new(Expr::Literal(Literal::Int(1))),
            }],
            line: 7,
        })),
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
