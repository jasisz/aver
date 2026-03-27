use super::{
    BoolCompareOp, BoolMatchShape, BoolSubjectPlan, CallLowerCtx, CallPlan, DispatchArmPlan,
    DispatchBindingPlan, DispatchDefaultPlan, DispatchLiteral, DispatchTableShape, LeafOp,
    ListMatchShape, MatchDispatchPlan, SemanticConstructor, SemanticDispatchPattern, TailCallPlan,
    WrapperKind, classify_bool_match_shape, classify_bool_subject_plan, classify_call_plan,
    classify_constructor_name, classify_dispatch_pattern, classify_leaf_op,
    classify_list_match_shape, classify_match_dispatch_plan, classify_tail_call_plan,
    expr_to_dotted_name,
};
use crate::ast::{BinOp, Expr, Literal, MatchArm, Pattern};

#[derive(Default)]
struct DummyCtx;

impl CallLowerCtx for DummyCtx {
    fn is_local_value(&self, name: &str) -> bool {
        name == "handler"
    }

    fn is_user_type(&self, name: &str) -> bool {
        matches!(name, "Shape" | "User")
    }

    fn resolve_module_call<'a>(&self, dotted: &'a str) -> Option<(&'a str, &'a str)> {
        match dotted {
            "Data.Fib.fib" => Some(("Data.Fib", "fib")),
            "Models.Shape.Circle" => Some(("Models", "Shape.Circle")),
            "Map.generateMap" => Some(("Map", "generateMap")),
            _ => None,
        }
    }
}

#[test]
fn dotted_name_flattens_attr_paths() {
    let expr = Expr::Attr(
        Box::new(Expr::Attr(
            Box::new(Expr::Ident("Data".to_string())),
            "Fib".to_string(),
        )),
        "fib".to_string(),
    );
    assert_eq!(expr_to_dotted_name(&expr).as_deref(), Some("Data.Fib.fib"));
}

#[test]
fn classify_builtin_wrapper_and_none() {
    let ctx = DummyCtx;
    assert_eq!(
        classify_call_plan(
            &Expr::Attr(
                Box::new(Expr::Ident("Result".to_string())),
                "Ok".to_string(),
            ),
            &ctx
        ),
        CallPlan::Wrapper(WrapperKind::ResultOk)
    );
    assert_eq!(
        classify_call_plan(
            &Expr::Attr(
                Box::new(Expr::Ident("Option".to_string())),
                "None".to_string(),
            ),
            &ctx
        ),
        CallPlan::NoneValue
    );
    assert_eq!(
        classify_call_plan(
            &Expr::Attr(Box::new(Expr::Ident("List".to_string())), "len".to_string()),
            &ctx
        ),
        CallPlan::Builtin("List.len".to_string())
    );
}

#[test]
fn classify_module_function_and_ctor() {
    let ctx = DummyCtx;
    let module_fn = Expr::Attr(
        Box::new(Expr::Attr(
            Box::new(Expr::Ident("Data".to_string())),
            "Fib".to_string(),
        )),
        "fib".to_string(),
    );
    assert_eq!(
        classify_call_plan(&module_fn, &ctx),
        CallPlan::Function("Data.Fib.fib".to_string())
    );

    let ctor = Expr::Attr(
        Box::new(Expr::Attr(
            Box::new(Expr::Ident("Models".to_string())),
            "Shape".to_string(),
        )),
        "Circle".to_string(),
    );
    assert_eq!(
        classify_call_plan(&ctor, &ctx),
        CallPlan::TypeConstructor {
            qualified_type_name: "Models.Shape".to_string(),
            variant_name: "Circle".to_string(),
        }
    );
    assert_eq!(
        classify_constructor_name("Models.Shape.Circle", &ctx),
        SemanticConstructor::TypeConstructor {
            qualified_type_name: "Models.Shape".to_string(),
            variant_name: "Circle".to_string(),
        }
    );
}

#[test]
fn lowercase_attr_and_local_ident_remain_dynamic() {
    let ctx = DummyCtx;
    let attr = Expr::Attr(
        Box::new(Expr::Ident("user".to_string())),
        "handler".to_string(),
    );
    assert_eq!(classify_call_plan(&attr, &ctx), CallPlan::Dynamic);
    assert_eq!(
        classify_call_plan(&Expr::Ident("handler".to_string()), &ctx),
        CallPlan::Dynamic
    );
}

#[test]
fn classify_constructor_wrapper_none_and_unknown() {
    let ctx = DummyCtx;
    assert_eq!(
        classify_constructor_name("Result.Ok", &ctx),
        SemanticConstructor::Wrapper(WrapperKind::ResultOk)
    );
    assert_eq!(
        classify_constructor_name("Option.None", &ctx),
        SemanticConstructor::NoneValue
    );
    assert_eq!(
        classify_constructor_name("Tcp.Connection", &ctx),
        SemanticConstructor::Unknown("Tcp.Connection".to_string())
    );
}

#[test]
fn classify_bool_and_list_match_shapes() {
    let bool_arms = vec![
        MatchArm {
            pattern: Pattern::Literal(Literal::Bool(false)),
            body: Box::new(Expr::Literal(Literal::Int(0))),
        },
        MatchArm {
            pattern: Pattern::Literal(Literal::Bool(true)),
            body: Box::new(Expr::Literal(Literal::Int(1))),
        },
    ];
    assert_eq!(
        classify_bool_match_shape(&bool_arms),
        Some(BoolMatchShape {
            true_arm_index: 1,
            false_arm_index: 0,
        })
    );

    let list_arms = vec![
        MatchArm {
            pattern: Pattern::Cons("h".to_string(), "t".to_string()),
            body: Box::new(Expr::Literal(Literal::Int(1))),
        },
        MatchArm {
            pattern: Pattern::EmptyList,
            body: Box::new(Expr::Literal(Literal::Int(0))),
        },
    ];
    assert_eq!(
        classify_list_match_shape(&list_arms),
        Some(ListMatchShape {
            empty_arm_index: 1,
            cons_arm_index: 0,
        })
    );
}

#[test]
fn classify_dispatchable_patterns() {
    let ctx = DummyCtx;
    assert_eq!(
        classify_dispatch_pattern(&Pattern::Literal(Literal::Int(42)), &ctx),
        Some(SemanticDispatchPattern::Literal(DispatchLiteral::Int(42)))
    );
    assert_eq!(
        classify_dispatch_pattern(
            &Pattern::Constructor("Option.None".to_string(), vec![]),
            &ctx
        ),
        Some(SemanticDispatchPattern::NoneValue)
    );
    assert_eq!(
        classify_dispatch_pattern(
            &Pattern::Constructor("Result.Ok".to_string(), vec!["x".to_string()]),
            &ctx
        ),
        Some(SemanticDispatchPattern::WrapperTag(WrapperKind::ResultOk))
    );
}

#[test]
fn classify_dispatch_table_shape_with_default_arm() {
    let ctx = DummyCtx;
    let arms = vec![
        MatchArm {
            pattern: Pattern::Constructor("Option.None".to_string(), vec![]),
            body: Box::new(Expr::Literal(Literal::Int(0))),
        },
        MatchArm {
            pattern: Pattern::Constructor("Option.Some".to_string(), vec!["x".to_string()]),
            body: Box::new(Expr::Literal(Literal::Int(1))),
        },
        MatchArm {
            pattern: Pattern::Wildcard,
            body: Box::new(Expr::Literal(Literal::Int(2))),
        },
    ];

    assert_eq!(
        classify_match_dispatch_plan(&arms, &ctx),
        Some(MatchDispatchPlan::Table(DispatchTableShape {
            entries: vec![
                DispatchArmPlan {
                    pattern: SemanticDispatchPattern::NoneValue,
                    arm_index: 0,
                    binding: DispatchBindingPlan::None,
                },
                DispatchArmPlan {
                    pattern: SemanticDispatchPattern::WrapperTag(WrapperKind::OptionSome),
                    arm_index: 1,
                    binding: DispatchBindingPlan::WrapperPayload("x".to_string()),
                },
            ],
            default_arm: Some(DispatchDefaultPlan {
                arm_index: 2,
                binding_name: None,
            }),
        }))
    );
}

#[test]
fn classify_tail_call_self_known_and_unknown() {
    let ctx = DummyCtx;
    assert_eq!(
        classify_tail_call_plan("loop", "loop", &ctx),
        TailCallPlan::SelfCall
    );
    assert_eq!(
        classify_tail_call_plan("Data.Fib.fib", "loop", &ctx),
        TailCallPlan::KnownFunction("Data.Fib.fib".to_string())
    );
    assert_eq!(
        classify_tail_call_plan("Result.Ok", "loop", &ctx),
        TailCallPlan::Unknown("Result.Ok".to_string())
    );
}

#[test]
fn classify_leaf_field_access_and_builtin_shapes() {
    let ctx = DummyCtx;
    let user_name = Expr::Attr(
        Box::new(Expr::Ident("user".to_string())),
        "name".to_string(),
    );
    assert!(matches!(
        classify_leaf_op(&user_name, &ctx),
        Some(LeafOp::FieldAccess {
            field_name: "name",
            ..
        })
    ));

    let map_get = Expr::FnCall(
        Box::new(Expr::Attr(
            Box::new(Expr::Ident("Map".to_string())),
            "get".to_string(),
        )),
        vec![
            Expr::Ident("m".to_string()),
            Expr::Literal(Literal::Str("k".to_string())),
        ],
    );
    assert!(matches!(
        classify_leaf_op(&map_get, &ctx),
        Some(LeafOp::MapGet { .. })
    ));
}

#[test]
fn classify_vector_get_with_literal_default_leaf() {
    let ctx = DummyCtx;
    let expr = Expr::FnCall(
        Box::new(Expr::Attr(
            Box::new(Expr::Ident("Option".to_string())),
            "withDefault".to_string(),
        )),
        vec![
            Expr::FnCall(
                Box::new(Expr::Attr(
                    Box::new(Expr::Ident("Vector".to_string())),
                    "get".to_string(),
                )),
                vec![
                    Expr::Ident("vec".to_string()),
                    Expr::Ident("idx".to_string()),
                ],
            ),
            Expr::Literal(Literal::Int(42)),
        ],
    );

    assert!(matches!(
        classify_leaf_op(&expr, &ctx),
        Some(LeafOp::VectorGetOrDefaultLiteral {
            default_literal,
            ..
        }) if matches!(default_literal, Literal::Int(42))
    ));
}

#[test]
fn uppercase_module_paths_are_not_field_leafs() {
    let ctx = DummyCtx;
    let expr = Expr::Attr(
        Box::new(Expr::Attr(
            Box::new(Expr::Ident("Data".to_string())),
            "Fib".to_string(),
        )),
        "fib".to_string(),
    );
    assert_eq!(classify_leaf_op(&expr, &ctx), None);
}

#[test]
fn classify_bool_subject_normalizes_negated_comparisons() {
    let subject = Expr::BinOp(
        BinOp::Gte,
        Box::new(Expr::Ident("n".to_string())),
        Box::new(Expr::Literal(Literal::Int(10))),
    );

    assert!(matches!(
        classify_bool_subject_plan(&subject),
        BoolSubjectPlan::Compare {
            lhs: Expr::Ident(name),
            rhs: Expr::Literal(Literal::Int(10)),
            op: BoolCompareOp::Lt,
            invert: true,
        } if name == "n"
    ));
}
