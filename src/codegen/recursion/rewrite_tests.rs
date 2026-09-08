use super::*;
use crate::types::Type;

#[test]
fn fuel_rewrite_preserves_the_primitive_type_of_an_interpolated_record_field() {
    let field = Spanned::new(
        Expr::Attr(
            Box::new(Spanned::bare(Expr::Ident("item".into()))),
            "value".into(),
        ),
        17,
    );
    field.set_ty(Type::Int);
    let text = Spanned::new(
        Expr::InterpolatedStr(vec![
            StrPart::Literal("Witness item of ".into()),
            StrPart::Parsed(Box::new(field)),
        ]),
        17,
    );
    text.set_ty(Type::Str);
    let body = FnBody::from_expr(text);
    let rewritten = rewrite_recursive_calls_body(&body, &HashSet::from(["walk".into()]), "fuel'");
    let text = rewritten.tail_expr().unwrap();
    assert_eq!(text.ty(), Some(&Type::Str));
    let Expr::InterpolatedStr(parts) = &text.node else {
        panic!("interpolation expected")
    };
    let StrPart::Parsed(field) = &parts[1] else {
        panic!("field segment expected")
    };
    assert_eq!(field.ty(), Some(&Type::Int));
    assert_eq!(field.line, 17);
}

#[test]
fn fuel_calls_keep_their_result_type_without_copying_the_old_callee_signature() {
    let result_type = Type::Result(Box::new(Type::named("Domain.Item")), Box::new(Type::Str));
    let argument = Spanned::bare(Expr::Ident("item".into()));
    argument.set_ty(Type::named("Domain.Item"));
    let callee = Spanned::bare(Expr::Ident("walk".into()));
    callee.set_ty(Type::Fn(
        vec![Type::named("Domain.Item")],
        Box::new(result_type.clone()),
        vec![],
    ));
    for node in [
        Expr::FnCall(Box::new(callee), vec![argument.clone()]),
        Expr::TailCall(Box::new(TailCallData::new("walk".into(), vec![argument]))),
    ] {
        let call = Spanned::new(node, 19);
        call.set_ty(result_type.clone());
        let rewritten =
            rewrite_recursive_calls_expr(&call, &HashSet::from(["walk".into()]), "fuel'");
        assert_eq!(rewritten.ty(), Some(&result_type));
        let Expr::FnCall(helper, args) = &rewritten.node else {
            panic!("fuel call expected")
        };
        assert!(matches!(&helper.node, Expr::Ident(name) if name == "walk__fuel"));
        assert!(helper.ty().is_none(), "the helper has an extra parameter");
        assert_eq!(args.len(), 2);
        assert_eq!(args[1].ty(), Some(&Type::named("Domain.Item")));
    }
}

#[test]
fn guarded_hir_rewrite_preserves_interpolated_field_and_recursive_call_types() {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedStrPart};

    let target = crate::ir::FnId(7);
    let field = Spanned::new(
        ResolvedExpr::Attr(
            Box::new(Spanned::bare(ResolvedExpr::Ident("item".into()))),
            "value".into(),
        ),
        23,
    );
    field.set_ty(Type::Int);
    for call_node in [
        ResolvedExpr::Call(ResolvedCallee::Fn(target), vec![field.clone()]),
        ResolvedExpr::TailCall {
            target,
            args: vec![field.clone()],
        },
    ] {
        let call = Spanned::new(call_node, 23);
        call.set_ty(Type::Int);
        let text = Spanned::new(
            ResolvedExpr::InterpolatedStr(vec![
                ResolvedStrPart::Parsed(Box::new(field.clone())),
                ResolvedStrPart::Parsed(Box::new(call)),
            ]),
            23,
        );
        text.set_ty(Type::Str);
        let rewritten = rewrite_native_guarded_calls_resolved_expr(&text, target, "walk_aux");
        assert_eq!(rewritten.ty(), Some(&Type::Str));
        let ResolvedExpr::InterpolatedStr(parts) = &rewritten.node else {
            panic!("interpolation expected")
        };
        for part in parts {
            let ResolvedStrPart::Parsed(expr) = part else {
                panic!("expression expected")
            };
            assert_eq!(expr.ty(), Some(&Type::Int));
        }
        let ResolvedStrPart::Parsed(call) = &parts[1] else {
            unreachable!()
        };
        let ResolvedExpr::Call(ResolvedCallee::Unresolved { callee }, args) = &call.node else {
            panic!("auxiliary call expected")
        };
        assert!(matches!(&callee.node, ResolvedExpr::Ident(name) if name == "walk_aux"));
        assert!(callee.ty().is_none());
        assert_eq!(args[0].ty(), Some(&Type::Int));
        assert!(args[1].ty().is_none());
    }
}
