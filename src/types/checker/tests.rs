use super::{run_type_check, TypeChecker};
use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, Stmt, TopLevel};

fn errors(items: Vec<TopLevel>) -> Vec<String> {
    run_type_check(&items)
        .into_iter()
        .map(|e| e.message)
        .collect()
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
