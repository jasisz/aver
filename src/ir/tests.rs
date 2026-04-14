use super::{
    BodyBindingPlan, BodyExprPlan, BodyPlan, BoolCompareOp, BoolMatchShape, BoolSubjectPlan,
    CallLowerCtx, CallPlan, DispatchArmPlan, DispatchBindingPlan, DispatchDefaultPlan,
    DispatchLiteral, DispatchTableShape, ForwardArg, ForwardCallPlan, LeafOp, ListMatchShape,
    MatchDispatchPlan, SemanticConstructor, SemanticDispatchPattern, TailCallPlan, ThinBodyCtx,
    ThinBodyPlan, ThinKind, WrapperKind, classify_body_plan, classify_bool_match_shape,
    classify_bool_subject_plan, classify_call_plan, classify_constructor_name,
    classify_dispatch_pattern, classify_forward_call_plan, classify_forward_fn_body,
    classify_leaf_op, classify_list_match_shape, classify_match_dispatch_plan,
    classify_tail_call_plan, classify_thin_body_plan, expr_to_dotted_name,
};
use crate::ast::{AnnotBool, BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt};

/// Shorthand: wrap an Expr in Spanned::bare (line=0).
fn sb(expr: Expr) -> Spanned<Expr> {
    Spanned::bare(expr)
}

/// Shorthand: wrap an Expr in Box<Spanned::bare(...)>.
fn sbb(expr: Expr) -> Box<Spanned<Expr>> {
    Box::new(Spanned::bare(expr))
}

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
        sbb(Expr::Attr(
            sbb(Expr::Ident("Data".to_string())),
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
            &Expr::Attr(sbb(Expr::Ident("Result".to_string())), "Ok".to_string(),),
            &ctx
        ),
        CallPlan::Wrapper(WrapperKind::ResultOk)
    );
    assert_eq!(
        classify_call_plan(
            &Expr::Attr(sbb(Expr::Ident("Option".to_string())), "None".to_string(),),
            &ctx
        ),
        CallPlan::NoneValue
    );
    assert_eq!(
        classify_call_plan(
            &Expr::Attr(sbb(Expr::Ident("List".to_string())), "len".to_string(),),
            &ctx
        ),
        CallPlan::Builtin("List.len".to_string())
    );
}

#[test]
fn classify_module_function_and_ctor() {
    let ctx = DummyCtx;
    let module_fn = Expr::Attr(
        sbb(Expr::Attr(
            sbb(Expr::Ident("Data".to_string())),
            "Fib".to_string(),
        )),
        "fib".to_string(),
    );
    assert_eq!(
        classify_call_plan(&module_fn, &ctx),
        CallPlan::Function("Data.Fib.fib".to_string())
    );

    let ctor = Expr::Attr(
        sbb(Expr::Attr(
            sbb(Expr::Ident("Models".to_string())),
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
    let attr = Expr::Attr(sbb(Expr::Ident("user".to_string())), "handler".to_string());
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
        classify_constructor_name("Ok", &ctx),
        SemanticConstructor::Wrapper(WrapperKind::ResultOk)
    );
    assert_eq!(
        classify_constructor_name("None", &ctx),
        SemanticConstructor::NoneValue
    );
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
            body: sbb(Expr::Literal(Literal::Int(0))),
        },
        MatchArm {
            pattern: Pattern::Literal(Literal::Bool(true)),
            body: sbb(Expr::Literal(Literal::Int(1))),
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
            body: sbb(Expr::Literal(Literal::Int(1))),
        },
        MatchArm {
            pattern: Pattern::EmptyList,
            body: sbb(Expr::Literal(Literal::Int(0))),
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
            body: sbb(Expr::Literal(Literal::Int(0))),
        },
        MatchArm {
            pattern: Pattern::Constructor("Option.Some".to_string(), vec!["x".to_string()]),
            body: sbb(Expr::Literal(Literal::Int(1))),
        },
        MatchArm {
            pattern: Pattern::Wildcard,
            body: sbb(Expr::Literal(Literal::Int(2))),
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
fn classify_forward_call_for_known_target_and_forwarded_locals() {
    let expr = Expr::FnCall(
        sbb(Expr::Attr(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Data".to_string())),
                "Fib".to_string(),
            )),
            "fib".to_string(),
        )),
        vec![sb(Expr::Resolved { slot: 1, name: "slot1".to_string(), last_use: AnnotBool(false) }), sb(Expr::Ident("slot0".to_string()))],
    );

    struct ForwardCtx;
    impl CallLowerCtx for ForwardCtx {
        fn is_local_value(&self, name: &str) -> bool {
            name == "slot0"
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

    assert_eq!(
        classify_forward_call_plan(&expr, &ForwardCtx),
        Some(ForwardCallPlan {
            target: CallPlan::Function("Data.Fib.fib".to_string()),
            args: vec![ForwardArg::Slot(1), ForwardArg::Local("slot0".to_string())],
        })
    );
}

#[test]
fn classify_forward_fn_body_requires_single_expr_wrapper() {
    struct ForwardCtx;
    impl CallLowerCtx for ForwardCtx {
        fn is_local_value(&self, name: &str) -> bool {
            matches!(name, "a" | "b")
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

    let forward_body = FnBody::from_expr(sb(Expr::FnCall(
        sbb(Expr::Ident("target".to_string())),
        vec![
            sb(Expr::Ident("b".to_string())),
            sb(Expr::Ident("a".to_string())),
        ],
    )));
    assert_eq!(
        classify_forward_fn_body(&forward_body, &ForwardCtx),
        Some(ForwardCallPlan {
            target: CallPlan::Function("target".to_string()),
            args: vec![
                ForwardArg::Local("b".to_string()),
                ForwardArg::Local("a".to_string()),
            ],
        })
    );

    let non_wrapper_body = FnBody::Block(vec![
        Stmt::Binding("x".to_string(), None, sb(Expr::Literal(Literal::Int(1)))),
        Stmt::Expr(sb(Expr::FnCall(
            sbb(Expr::Ident("target".to_string())),
            vec![sb(Expr::Ident("a".to_string()))],
        ))),
    ]);
    assert_eq!(
        classify_forward_fn_body(&non_wrapper_body, &ForwardCtx),
        None
    );
}

#[test]
fn classify_body_plan_for_single_expr_leaf_and_rejects_blocks() {
    struct LocalCtx;
    impl CallLowerCtx for LocalCtx {
        fn is_local_value(&self, name: &str) -> bool {
            matches!(name, "vec" | "idx" | "x")
        }

        fn is_user_type(&self, name: &str) -> bool {
            matches!(name, "Shape" | "User")
        }

        fn resolve_module_call<'a>(&self, dotted: &'a str) -> Option<(&'a str, &'a str)> {
            match dotted {
                "Data.Fib.fib" => Some(("Data.Fib", "fib")),
                _ => None,
            }
        }
    }

    let leaf_body = FnBody::from_expr(sb(Expr::FnCall(
        sbb(Expr::Attr(
            sbb(Expr::Ident("Option".to_string())),
            "withDefault".to_string(),
        )),
        vec![
            sb(Expr::FnCall(
                sbb(Expr::Attr(
                    sbb(Expr::Ident("Vector".to_string())),
                    "get".to_string(),
                )),
                vec![
                    sb(Expr::Ident("vec".to_string())),
                    sb(Expr::Ident("idx".to_string())),
                ],
            )),
            sb(Expr::Literal(Literal::Int(0))),
        ],
    )));
    assert!(matches!(
        classify_body_plan(&leaf_body, &LocalCtx),
        Some(BodyPlan::SingleExpr(BodyExprPlan::Leaf(
            LeafOp::VectorGetOrDefaultLiteral { .. }
        )))
    ));

    let structured = FnBody::Block(vec![
        Stmt::Binding("x".to_string(), None, sb(Expr::Literal(Literal::Int(1)))),
        Stmt::Expr(sb(Expr::Ident("x".to_string()))),
    ]);
    assert!(matches!(
        classify_body_plan(&structured, &LocalCtx),
        Some(BodyPlan::Block {
            bindings,
            tail: BodyExprPlan::Expr(Expr::Ident(name)),
            ..
        }) if bindings.len() == 1
            && matches!(
                &bindings[0],
                BodyBindingPlan {
                    name: "x",
                    expr: BodyExprPlan::Expr(Expr::Literal(Literal::Int(1))),
                }
            )
            && name == "x"
    ));

    let invalid = FnBody::Block(vec![
        Stmt::Expr(sb(Expr::Literal(Literal::Int(1)))),
        Stmt::Expr(sb(Expr::Literal(Literal::Int(2)))),
    ]);
    assert_eq!(classify_body_plan(&invalid, &LocalCtx), None);
}

#[test]
fn classify_thin_body_plan_for_known_thin_function() {
    struct ThinCtx {
        defs: Vec<FnDef>,
    }

    impl CallLowerCtx for ThinCtx {
        fn is_local_value(&self, name: &str) -> bool {
            matches!(name, "grid" | "idx")
        }

        fn is_user_type(&self, _name: &str) -> bool {
            false
        }

        fn resolve_module_call<'a>(&self, _dotted: &'a str) -> Option<(&'a str, &'a str)> {
            None
        }
    }

    impl ThinBodyCtx for ThinCtx {
        fn find_fn_def<'a>(&'a self, name: &str) -> Option<&'a FnDef> {
            self.defs.iter().find(|fd| fd.name == name)
        }
    }

    let ctx = ThinCtx {
        defs: vec![FnDef {
            name: "cellAt".to_string(),
            line: 0,
            params: vec![
                ("grid".to_string(), "Vector<Int>".to_string()),
                ("idx".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: FnBody::from_expr(sb(Expr::FnCall(
                sbb(Expr::Attr(
                    sbb(Expr::Ident("Option".to_string())),
                    "withDefault".to_string(),
                )),
                vec![
                    sb(Expr::FnCall(
                        sbb(Expr::Attr(
                            sbb(Expr::Ident("Vector".to_string())),
                            "get".to_string(),
                        )),
                        vec![
                            sb(Expr::Ident("grid".to_string())),
                            sb(Expr::Ident("idx".to_string())),
                        ],
                    )),
                    sb(Expr::Literal(Literal::Int(0))),
                ],
            )))
            .into(),
            resolution: None,
        }],
    };

    assert!(matches!(
        classify_thin_body_plan("cellAt", &ctx),
        Some(ThinBodyPlan {
            params,
            kind: ThinKind::Leaf,
            body: BodyPlan::SingleExpr(BodyExprPlan::Leaf(
                LeafOp::VectorGetOrDefaultLiteral { .. }
            )),
        }) if params.len() == 2 && params[0].0 == "grid" && params[1].0 == "idx"
    ));
}

#[test]
fn classify_thin_body_plan_for_dispatch_function() {
    struct ThinCtx {
        defs: Vec<FnDef>,
    }

    impl CallLowerCtx for ThinCtx {
        fn is_local_value(&self, name: &str) -> bool {
            name == "r"
        }

        fn is_user_type(&self, _name: &str) -> bool {
            false
        }

        fn resolve_module_call<'a>(&self, _dotted: &'a str) -> Option<(&'a str, &'a str)> {
            None
        }
    }

    impl ThinBodyCtx for ThinCtx {
        fn find_fn_def<'a>(&'a self, name: &str) -> Option<&'a FnDef> {
            self.defs.iter().find(|fd| fd.name == name)
        }
    }

    let ctx = ThinCtx {
        defs: vec![FnDef {
            name: "unwrapOrZero".to_string(),
            line: 0,
            params: vec![("r".to_string(), "Result<Int, String>".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: FnBody::from_expr(sb(Expr::Match {
                subject: sbb(Expr::Ident("r".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor(
                            "Result.Ok".to_string(),
                            vec!["n".to_string()],
                        ),
                        body: sbb(Expr::Ident("n".to_string())),
                    },
                    MatchArm {
                        pattern: Pattern::Constructor(
                            "Result.Err".to_string(),
                            vec!["_".to_string()],
                        ),
                        body: sbb(Expr::Literal(Literal::Int(0))),
                    },
                ],
            }))
            .into(),
            resolution: None,
        }],
    };

    assert!(matches!(
        classify_thin_body_plan("unwrapOrZero", &ctx),
        Some(ThinBodyPlan {
            kind: ThinKind::Dispatch,
            body: BodyPlan::SingleExpr(BodyExprPlan::Expr(Expr::Match { .. })),
            ..
        })
    ));
}

#[test]
fn classify_leaf_field_access_and_builtin_shapes() {
    let ctx = DummyCtx;
    let user_name = Expr::Attr(sbb(Expr::Ident("user".to_string())), "name".to_string());
    assert!(matches!(
        classify_leaf_op(&user_name, &ctx),
        Some(LeafOp::FieldAccess {
            field_name: "name",
            ..
        })
    ));

    let map_get = Expr::FnCall(
        sbb(Expr::Attr(
            sbb(Expr::Ident("Map".to_string())),
            "get".to_string(),
        )),
        vec![
            sb(Expr::Ident("m".to_string())),
            sb(Expr::Literal(Literal::Str("k".to_string()))),
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
        sbb(Expr::Attr(
            sbb(Expr::Ident("Option".to_string())),
            "withDefault".to_string(),
        )),
        vec![
            sb(Expr::FnCall(
                sbb(Expr::Attr(
                    sbb(Expr::Ident("Vector".to_string())),
                    "get".to_string(),
                )),
                vec![
                    sb(Expr::Ident("vec".to_string())),
                    sb(Expr::Ident("idx".to_string())),
                ],
            )),
            sb(Expr::Literal(Literal::Int(42))),
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
fn classify_int_mod_with_literal_default_leaf() {
    let ctx = DummyCtx;
    let expr = Expr::FnCall(
        sbb(Expr::Attr(
            sbb(Expr::Ident("Result".to_string())),
            "withDefault".to_string(),
        )),
        vec![
            sb(Expr::FnCall(
                sbb(Expr::Attr(
                    sbb(Expr::Ident("Int".to_string())),
                    "mod".to_string(),
                )),
                vec![
                    sb(Expr::Ident("a".to_string())),
                    sb(Expr::Ident("b".to_string())),
                ],
            )),
            sb(Expr::Literal(Literal::Int(0))),
        ],
    );

    assert!(matches!(
        classify_leaf_op(&expr, &ctx),
        Some(LeafOp::IntModOrDefaultLiteral {
            default_literal,
            ..
        }) if matches!(default_literal, Literal::Int(0))
    ));
}

#[test]
fn uppercase_module_paths_are_not_field_leafs() {
    let ctx = DummyCtx;
    let expr = Expr::Attr(
        sbb(Expr::Attr(
            sbb(Expr::Ident("Data".to_string())),
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
        sbb(Expr::Ident("n".to_string())),
        sbb(Expr::Literal(Literal::Int(10))),
    );

    assert!(matches!(
        classify_bool_subject_plan(&subject),
        BoolSubjectPlan::Compare {
            lhs,
            rhs,
            op: BoolCompareOp::Lt,
            invert: true,
        } if matches!(lhs.node, Expr::Ident(ref name) if name == "n")
          && matches!(rhs.node, Expr::Literal(Literal::Int(10)))
    ));
}
