#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    MaxSlotInExpr(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt),
    MaxSlotInExprComposite(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt),
    MaxSlotInExprAggregate(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt),
    MaxSlotInExprPair(
        crate::aver_generated::domain::ast::Expr,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverInt,
    ),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> aver_rt::AverInt {
    loop {
        __state = match __state {
            __MutualTco1::MaxSlotInExpr(mut expr @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprBoolBranch(
                            __cond_fields,
                        ) => {
                            let cond = &__cond_fields.0;
                            let thenExpr = &__cond_fields.1;
                            let elseExpr = &__cond_fields.2;
                            __MutualTco1::MaxSlotInExpr(elseExpr.clone(), {
                                let __arg12 @ _ =
                                    crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                                        cond.clone(),
                                        acc,
                                    );
                                crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                                    thenExpr.clone(),
                                    __arg12,
                                )
                            })
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                            let slot = __slot_fields.0.clone();
                            return crate::aver_generated::domain::resolver::core::maxInt(
                                slot, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprBinopSlotInt(
                            __slot_fields,
                        ) => {
                            let slot = __slot_fields.1.clone();
                            return crate::aver_generated::domain::resolver::core::maxInt(
                                slot, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprBinopSlots(
                            __lhs_fields,
                        ) => {
                            let lhs = __lhs_fields.1.clone();
                            let rhs = __lhs_fields.2.clone();
                            {
                                let __arg13 @ _ =
                                    crate::aver_generated::domain::resolver::core::maxInt(lhs, acc);
                                return crate::aver_generated::domain::resolver::core::maxInt(
                                    rhs, __arg13,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCmpSlotInt(
                            __slot_fields,
                        ) => {
                            let slot = __slot_fields.1.clone();
                            return crate::aver_generated::domain::resolver::core::maxInt(
                                slot, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCmpSlots(
                            __lhs_fields,
                        ) => {
                            let lhs = __lhs_fields.1.clone();
                            let rhs = __lhs_fields.2.clone();
                            {
                                let __arg14 @ _ =
                                    crate::aver_generated::domain::resolver::core::maxInt(lhs, acc);
                                return crate::aver_generated::domain::resolver::core::maxInt(
                                    rhs, __arg14,
                                );
                            }
                        }
                        _ => __MutualTco1::MaxSlotInExprComposite(expr, acc),
                    }
                }
            }
            __MutualTco1::MaxSlotInExprComposite(mut expr @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = &expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprVectorGetOrInt(
                            __vecExpr_fields,
                        ) => {
                            let vecExpr = &__vecExpr_fields.0;
                            let idxExpr = &__vecExpr_fields.1;
                            {
                                let __arg23 @ _ =
                                    crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                                        vecExpr.clone(),
                                        acc,
                                    );
                                __MutualTco1::MaxSlotInExpr(idxExpr.clone(), __arg23)
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprIntModOrInt(
                            __a_fields,
                        ) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            {
                                let __arg24 @ _ =
                                    crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                                        a.clone(),
                                        acc,
                                    );
                                __MutualTco1::MaxSlotInExpr(b.clone(), __arg24)
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprAdd(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            __MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprSub(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            __MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprMul(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            __MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprDiv(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            __MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprNeg(__inner_fields) => {
                            let inner = &__inner_fields.0;
                            __MutualTco1::MaxSlotInExpr(inner.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprEq(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            __MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprNeq(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            __MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            __MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            __MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc)
                        }
                        _ => __MutualTco1::MaxSlotInExprAggregate(expr, acc),
                    }
                }
            }
            __MutualTco1::MaxSlotInExprAggregate(mut expr @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                {
                    let __t = expr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprLte(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            __MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprGte(__a_fields) => {
                            let a = &__a_fields.0;
                            let b = &__a_fields.1;
                            __MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCall(__args_fields) => {
                            let args = &__args_fields.1;
                            return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                                args, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(
                            __args_fields,
                        ) => {
                            let args = &__args_fields.1;
                            return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                                args, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(
                            __args_fields,
                        ) => {
                            let args = &__args_fields.1;
                            return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                                args, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltinId(
                            __args_fields,
                        ) => {
                            let args = &__args_fields.1;
                            return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                                args, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprMatch(
                            __subj_fields,
                        ) => {
                            let subj = &__subj_fields.0;
                            let arms = &__subj_fields.1;
                            {
                                let __arg19 @ _ =
                                    crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                                        subj.clone(),
                                        acc,
                                    );
                                return crate::aver_generated::domain::resolver::core::maxSlotInArms(
                                    arms, __arg19,
                                );
                            }
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprPropagate(
                            __inner_fields,
                        ) => {
                            let inner = &__inner_fields.0;
                            __MutualTco1::MaxSlotInExpr(inner.clone(), acc)
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprConcat(
                            __parts_fields,
                        ) => {
                            let parts = &__parts_fields.0;
                            return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                                parts, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprTuple(
                            __exprs_fields,
                        ) => {
                            let exprs = &__exprs_fields.0;
                            return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                                exprs, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprIndependentProduct(
                            __exprs_fields,
                        ) => {
                            let exprs = &__exprs_fields.0;
                            return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                                exprs, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprList(
                            __exprs_fields,
                        ) => {
                            let exprs = &__exprs_fields.0;
                            return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                                exprs, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprRecord(
                            __fields_fields,
                        ) => {
                            let fields = &__fields_fields.1;
                            return crate::aver_generated::domain::resolver::core::maxSlotInFields(
                                fields, acc,
                            );
                        }
                        crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(
                            __obj_fields,
                        ) => {
                            let obj = &__obj_fields.0;
                            __MutualTco1::MaxSlotInExpr(obj.clone(), acc)
                        }
                        _ => return acc,
                    }
                }
            }
            __MutualTco1::MaxSlotInExprPair(mut a @ _, mut b @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let __arg3 @ _ =
                    crate::aver_generated::domain::resolver::core::maxSlotInExpr(a, acc);
                __MutualTco1::MaxSlotInExpr(b, __arg3)
            }
        };
    }
}

/// Find highest slot index in an expression tree.
pub fn maxSlotInExpr(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprBoolBranch(__cond_fields) => {
                let cond = &__cond_fields.0;
                let thenExpr = &__cond_fields.1;
                let elseExpr = &__cond_fields.2;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExpr(elseExpr.clone(), {
                    let __arg12 @ _ = crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                        cond.clone(),
                        acc,
                    );
                    crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                        thenExpr.clone(),
                        __arg12,
                    )
                }));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                return crate::aver_generated::domain::resolver::core::maxInt(slot, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprBinopSlotInt(__slot_fields) => {
                let slot = __slot_fields.1.clone();
                return crate::aver_generated::domain::resolver::core::maxInt(slot, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprBinopSlots(__lhs_fields) => {
                let lhs = __lhs_fields.1.clone();
                let rhs = __lhs_fields.2.clone();
                {
                    let __arg13 @ _ =
                        crate::aver_generated::domain::resolver::core::maxInt(lhs, acc);
                    return crate::aver_generated::domain::resolver::core::maxInt(rhs, __arg13);
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCmpSlotInt(__slot_fields) => {
                let slot = __slot_fields.1.clone();
                return crate::aver_generated::domain::resolver::core::maxInt(slot, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCmpSlots(__lhs_fields) => {
                let lhs = __lhs_fields.1.clone();
                let rhs = __lhs_fields.2.clone();
                {
                    let __arg14 @ _ =
                        crate::aver_generated::domain::resolver::core::maxInt(lhs, acc);
                    return crate::aver_generated::domain::resolver::core::maxInt(rhs, __arg14);
                }
            }
            _ => return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprComposite(expr, acc)),
        }
    }
}

/// Continue max-slot traversal for composite expression forms.
pub fn maxSlotInExprComposite(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprVectorGetOrInt(
                __vecExpr_fields,
            ) => {
                let vecExpr = &__vecExpr_fields.0;
                let idxExpr = &__vecExpr_fields.1;
                {
                    let __arg23 @ _ = crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                        vecExpr.clone(),
                        acc,
                    );
                    return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExpr(
                        idxExpr.clone(),
                        __arg23,
                    ));
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIntModOrInt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg24 @ _ = crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                        a.clone(),
                        acc,
                    );
                    return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExpr(
                        b.clone(),
                        __arg24,
                    ));
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprAdd(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(
                    a.clone(),
                    b.clone(),
                    acc,
                ));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSub(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(
                    a.clone(),
                    b.clone(),
                    acc,
                ));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMul(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(
                    a.clone(),
                    b.clone(),
                    acc,
                ));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprDiv(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(
                    a.clone(),
                    b.clone(),
                    acc,
                ));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeg(__inner_fields) => {
                let inner = &__inner_fields.0;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExpr(inner.clone(), acc));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprEq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(
                    a.clone(),
                    b.clone(),
                    acc,
                ));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(
                    a.clone(),
                    b.clone(),
                    acc,
                ));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(
                    a.clone(),
                    b.clone(),
                    acc,
                ));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(
                    a.clone(),
                    b.clone(),
                    acc,
                ));
            }
            _ => return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprAggregate(expr, acc)),
        }
    }
}

/// Finish max-slot traversal for aggregate and call expression forms.
pub fn maxSlotInExprAggregate(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    {
        let __t = expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprLte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(
                    a.clone(),
                    b.clone(),
                    acc,
                ));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(
                    a.clone(),
                    b.clone(),
                    acc,
                ));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCall(__args_fields) => {
                let args = &__args_fields.1;
                return crate::aver_generated::domain::resolver::core::maxSlotInExprs(args, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(__args_fields) => {
                let args = &__args_fields.1;
                return crate::aver_generated::domain::resolver::core::maxSlotInExprs(args, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(__args_fields) => {
                let args = &__args_fields.1;
                return crate::aver_generated::domain::resolver::core::maxSlotInExprs(args, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltinId(__args_fields) => {
                let args = &__args_fields.1;
                return crate::aver_generated::domain::resolver::core::maxSlotInExprs(args, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMatch(__subj_fields) => {
                let subj = &__subj_fields.0;
                let arms = &__subj_fields.1;
                {
                    let __arg19 @ _ = crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                        subj.clone(),
                        acc,
                    );
                    return crate::aver_generated::domain::resolver::core::maxSlotInArms(
                        arms, __arg19,
                    );
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprPropagate(__inner_fields) => {
                let inner = &__inner_fields.0;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExpr(inner.clone(), acc));
            }
            crate::aver_generated::domain::ast::Expr__view::ExprConcat(__parts_fields) => {
                let parts = &__parts_fields.0;
                return crate::aver_generated::domain::resolver::core::maxSlotInExprs(parts, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprTuple(__exprs_fields) => {
                let exprs = &__exprs_fields.0;
                return crate::aver_generated::domain::resolver::core::maxSlotInExprs(exprs, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIndependentProduct(
                __exprs_fields,
            ) => {
                let exprs = &__exprs_fields.0;
                return crate::aver_generated::domain::resolver::core::maxSlotInExprs(exprs, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprList(__exprs_fields) => {
                let exprs = &__exprs_fields.0;
                return crate::aver_generated::domain::resolver::core::maxSlotInExprs(exprs, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprRecord(__fields_fields) => {
                let fields = &__fields_fields.1;
                return crate::aver_generated::domain::resolver::core::maxSlotInFields(fields, acc);
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(__obj_fields) => {
                let obj = &__obj_fields.0;
                return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExpr(obj.clone(), acc));
            }
            _ => return acc,
        }
    }
}

/// Visit both sides of a binary expression.
pub fn maxSlotInExprPair(
    mut a @ _: crate::aver_generated::domain::ast::Expr,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    let __arg3 @ _ = crate::aver_generated::domain::resolver::core::maxSlotInExpr(a, acc);
    return __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExpr(b, __arg3));
}

/// Resolve each function definition.
#[inline(always)]
pub fn resolveFns(
    fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>;
    let mut fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> = fns;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fns;
            if __list_subject.is_empty() {
                return acc.reverse();
            } else {
                if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::AverList::prepend_owned(
                            crate::aver_generated::domain::resolver::core::resolveFn(f),
                            acc,
                        );
                        __hold0 = __tco0;
                        fns = &__hold0;
                        acc = __tco1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Resolve one function: assign slots to params and locals.
pub fn resolveFn(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    let paramResult @ _ = {
        let __arg4 @ _ = <aver_rt::AverMap<AverStr, aver_rt::AverInt>>::new();
        crate::aver_generated::domain::resolver::core::buildParamSlots(
            &fd.params,
            __arg4,
            aver_rt::AverInt::from_i64(0),
        )
    };
    {
        let (slotMap, nextSlot) = paramResult;
        crate::aver_generated::domain::resolver::core::resolveBody(fd, slotMap, nextSlot)
    }
}

/// Assign slot indices 0..n-1 to parameters.
#[inline(always)]
pub fn buildParamSlots(
    params @ _: &aver_rt::AverList<AverStr>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut params @ _: &aver_rt::AverList<AverStr> = params;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = params;
            if __list_subject.is_empty() {
                return (slots, next);
            } else {
                if let Some((p, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    {
                        let __tco0 = rest;
                        let __tco1 = slots.insert_owned(p, next.clone());
                        let __tco2 = next.add(&aver_rt::AverInt::from_i64(1));
                        __hold0 = __tco0;
                        params = &__hold0;
                        slots = __tco1;
                        next = __tco2;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Walk body stmts, resolve vars, track new bindings. slotCount = max slot + 1 from body scan.
pub fn resolveBody(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    nextSlot @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    let resolved @ _ = {
        let __arg7 @ _ =
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
        crate::aver_generated::domain::resolver::core::resolveStmts__collected(
            &fd.body, slots, nextSlot, __arg7,
        )
    };
    {
        let (newBody, finalSlot, finalSlotMap) = resolved;
        crate::aver_generated::domain::resolver::core::computeSlotCount(
            fd,
            &newBody,
            finalSlot,
            finalSlotMap,
        )
    }
}

/// Compute slotCount as max slot index found in body + 1.
pub fn computeSlotCount(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    body @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    baseSlot @ _: aver_rt::AverInt,
    mut slotMap @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    let maxFromBody @ _ = crate::aver_generated::domain::resolver::core::maxSlotInStmts(
        body,
        baseSlot.sub(&aver_rt::AverInt::from_i64(1)),
    );
    Box::new(crate::aver_generated::domain::ast::FnDef__Fields {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: body.clone(),
        slotCount: maxFromBody.add(&aver_rt::AverInt::from_i64(1)),
        slotMap: slotMap,
        fastPath: crate::aver_generated::domain::ast::FnFastPath::FastNone,
        tailLoop: false,
    })
}

/// Find highest slot index in a list of statements.
#[inline(always)]
pub fn maxSlotInStmts(
    stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = stmts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = stmts;
            if __list_subject.is_empty() {
                return acc;
            } else {
                if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 =
                            crate::aver_generated::domain::resolver::core::maxSlotInStmt(s, acc);
                        __hold0 = __tco0;
                        stmts = &__hold0;
                        acc = __tco1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Find highest slot index in a statement.
pub fn maxSlotInStmt(
    s @ _: &crate::aver_generated::domain::ast::Stmt,
    acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    match s {
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, expr) => {
            let slot = slot.clone();
            {
                let __arg6 @ _ = crate::aver_generated::domain::resolver::core::maxInt(slot, acc);
                crate::aver_generated::domain::resolver::core::maxSlotInExpr(expr.clone(), __arg6)
            }
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::resolver::core::maxSlotInExpr(expr.clone(), acc)
        }
        crate::aver_generated::domain::ast::Stmt::StmtBind(_, expr) => {
            crate::aver_generated::domain::resolver::core::maxSlotInExpr(expr.clone(), acc)
        }
    }
}

/// Find highest slot index in a list of expressions.
#[inline(always)]
pub fn maxSlotInExprs(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return acc;
            } else {
                if let (Some(e), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                            e.clone(),
                            acc,
                        );
                        __hold0 = __tco0;
                        exprs = &__hold0;
                        acc = __tco1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Find highest slot index in record fields.
#[inline(always)]
pub fn maxSlotInFields(
    fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let mut __hold0: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>;
    let mut fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> =
        fields;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fields;
            if __list_subject.is_empty() {
                return acc;
            } else {
                if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (_, expr) = pair;
                        {
                            let __tco0 = rest;
                            let __tco1 =
                                crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                                    expr.clone(),
                                    acc,
                                );
                            __hold0 = __tco0;
                            fields = &__hold0;
                            acc = __tco1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Find highest slot index in match arms.
#[inline(always)]
pub fn maxSlotInArms(
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>;
    let mut arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> = arms;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = arms;
            if __list_subject.is_empty() {
                return acc;
            } else {
                if let (Some(arm), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                            arm.body.clone(),
                            acc,
                        );
                        __hold0 = __tco0;
                        arms = &__hold0;
                        acc = __tco1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Return the larger of two integers.
#[inline(always)]
pub fn maxInt(a @ _: aver_rt::AverInt, b @ _: aver_rt::AverInt) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    if (a > b) { a } else { b }
}

/// Resolve statements, threading slot assignments.
#[inline(always)]
pub fn resolveStmts(
    stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    aver_rt::AverInt,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = stmts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = stmts;
            if __list_subject.is_empty() {
                return (acc.reverse(), next, slots);
            } else {
                if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match s {
                        crate::aver_generated::domain::ast::Stmt::StmtBind(__stp1, __stp0) => {
                            let __stp1 = __stp1.clone();
                            {
                                let (__stp3, __stp2) =
                                    crate::aver_generated::domain::resolver::core::resolveExpr(
                                        __stp0.clone(),
                                        slots,
                                    );
                                {
                                    let __stp4 = __stp2.insert_owned(__stp1, next.clone());
                                    {
                                        let __tco0 = rest;
                                        let __tco1 = __stp4;
                                        let __tco2 = next.add(&aver_rt::AverInt::from_i64(1));
                                        let __tco3 = aver_rt::AverList::prepend_owned(
                                            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(
                                                next, __stp3,
                                            ),
                                            acc,
                                        );
                                        __hold0 = __tco0;
                                        stmts = &__hold0;
                                        slots = __tco1;
                                        next = __tco2;
                                        acc = __tco3;
                                        continue;
                                    }
                                }
                            }
                        }
                        crate::aver_generated::domain::ast::Stmt::StmtExpr(__stp0) => {
                            let (__stp6, __stp5) =
                                crate::aver_generated::domain::resolver::core::resolveExpr(
                                    __stp0.clone(),
                                    slots,
                                );
                            {
                                let __tco0 = rest;
                                let __tco1 = __stp5.clone();
                                let __tco2 = {
                                    let __arg14 @ _ =
                                        crate::aver_generated::domain::resolver::core::mapMaxVal(
                                            &__stp5,
                                        )
                                        .add(&aver_rt::AverInt::from_i64(1));
                                    crate::aver_generated::domain::resolver::core::maxInt(
                                        next, __arg14,
                                    )
                                };
                                let __tco3 = aver_rt::AverList::prepend_owned(
                                    crate::aver_generated::domain::ast::Stmt::StmtExpr(__stp6),
                                    acc,
                                );
                                __hold0 = __tco0;
                                stmts = &__hold0;
                                slots = __tco1;
                                next = __tco2;
                                acc = __tco3;
                                continue;
                            }
                        }
                        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(_, _) => {
                            let __tco0 = rest;
                            let __tco3 = aver_rt::AverList::prepend_owned(s.clone(), acc);
                            __hold0 = __tco0;
                            stmts = &__hold0;
                            acc = __tco3;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Resolve a single statement and continue.
pub fn resolveOneStmt(
    s @ _: &crate::aver_generated::domain::ast::Stmt,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    aver_rt::AverInt,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    match s {
        crate::aver_generated::domain::ast::Stmt::StmtBind(name, expr) => {
            let name = name.clone();
            crate::aver_generated::domain::resolver::core::resolveStmtBind(
                name,
                expr.clone(),
                rest,
                slots,
                next,
                acc,
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::resolver::core::resolveStmtExpr(
                expr.clone(),
                rest,
                slots,
                next,
                acc,
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(_, _) => {
            let __arg8 @ _ = aver_rt::AverList::prepend(s.clone(), acc);
            crate::aver_generated::domain::resolver::core::resolveStmts(rest, slots, next, __arg8)
        }
    }
}

/// Resolve an expression statement, collecting pattern slots.
pub fn resolveStmtExpr(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    aver_rt::AverInt,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    {
        let (re, newSlots) =
            crate::aver_generated::domain::resolver::core::resolveExpr(expr, slots);
        crate::aver_generated::domain::resolver::core::resolveStmts(
            rest,
            newSlots.clone(),
            {
                let __arg7 @ _ =
                    crate::aver_generated::domain::resolver::core::mapMaxVal(&newSlots)
                        .add(&aver_rt::AverInt::from_i64(1));
                crate::aver_generated::domain::resolver::core::maxInt(next, __arg7)
            },
            aver_rt::AverList::prepend(crate::aver_generated::domain::ast::Stmt::StmtExpr(re), acc),
        )
    }
}

/// Resolve a binding: assign a new slot, replace with StmtBindSlot.
pub fn resolveStmtBind(
    name @ _: AverStr,
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    aver_rt::AverInt,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    {
        let (newExpr, exprSlots) =
            crate::aver_generated::domain::resolver::core::resolveExpr(expr, slots);
        crate::aver_generated::domain::resolver::core::resolveStmtBindFinish(
            name, newExpr, rest, exprSlots, next, acc,
        )
    }
}

/// Finish resolving a binding after expr is resolved.
pub fn resolveStmtBindFinish(
    name @ _: AverStr,
    mut newExpr @ _: crate::aver_generated::domain::ast::Expr,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    aver_rt::AverInt,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    let newSlots @ _ = slots.insert_owned(name, next.clone());
    crate::aver_generated::domain::resolver::core::resolveStmts(
        rest,
        newSlots,
        next.add(&aver_rt::AverInt::from_i64(1)),
        aver_rt::AverList::prepend(
            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(next, newExpr),
            acc,
        ),
    )
}

/// Resolve an expression. Returns (resolved expr, slots with any new pattern bindings).
pub fn resolveExpr(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprBoolBranch(__cond_fields) => {
                let cond = &__cond_fields.0;
                let thenExpr = &__cond_fields.1;
                let elseExpr = &__cond_fields.2;
                (
                    crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                        crate::aver_generated::domain::ast::Expr__ExprBoolBranch(
                            crate::aver_generated::domain::resolver::core::resolveExprSimple(
                                cond.clone(),
                                slots.clone(),
                            ),
                            crate::aver_generated::domain::resolver::core::resolveExprSimple(
                                thenExpr.clone(),
                                slots.clone(),
                            ),
                            crate::aver_generated::domain::resolver::core::resolveExprSimple(
                                elseExpr.clone(),
                                slots.clone(),
                            ),
                        ),
                    ),
                    slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprVar(__name_fields) => {
                let name = __name_fields.0.clone();
                (
                    crate::aver_generated::domain::resolver::core::resolveVar(name, &slots),
                    slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprInt(_) => (expr, slots),
            crate::aver_generated::domain::ast::Expr__view::ExprFloat(_) => (expr, slots),
            crate::aver_generated::domain::ast::Expr__view::ExprStr(_) => (expr, slots),
            crate::aver_generated::domain::ast::Expr__view::ExprBool(_) => (expr, slots),
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(_) => (expr, slots),
            _ => crate::aver_generated::domain::resolver::core::resolveExprAfterLeaf(expr, slots),
        }
    }
}

/// Continue expression resolution after simple leaf forms.
pub fn resolveExprAfterLeaf(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprBinopSlotInt(_) => (expr, slots),
            crate::aver_generated::domain::ast::Expr__view::ExprBinopSlots(_) => (expr, slots),
            crate::aver_generated::domain::ast::Expr__view::ExprCmpSlotInt(_) => (expr, slots),
            crate::aver_generated::domain::ast::Expr__view::ExprCmpSlots(_) => (expr, slots),
            crate::aver_generated::domain::ast::Expr__view::ExprVectorGetOrInt(
                __vecExpr_fields,
            ) => {
                let vecExpr = &__vecExpr_fields.0;
                let idxExpr = &__vecExpr_fields.1;
                let defaultValue = __vecExpr_fields.2.clone();
                (
                    crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                        crate::aver_generated::domain::ast::Expr__ExprVectorGetOrInt(
                            crate::aver_generated::domain::resolver::core::resolveExprSimple(
                                vecExpr.clone(),
                                slots.clone(),
                            ),
                            crate::aver_generated::domain::resolver::core::resolveExprSimple(
                                idxExpr.clone(),
                                slots.clone(),
                            ),
                            defaultValue,
                        ),
                    ),
                    slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIntModOrInt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                let defaultValue = __a_fields.2.clone();
                (
                    crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                        crate::aver_generated::domain::ast::Expr__ExprIntModOrInt(
                            crate::aver_generated::domain::resolver::core::resolveExprSimple(
                                a.clone(),
                                slots.clone(),
                            ),
                            crate::aver_generated::domain::resolver::core::resolveExprSimple(
                                b.clone(),
                                slots.clone(),
                            ),
                            defaultValue,
                        ),
                    ),
                    slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprAdd(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::core::resolveBinExpr(
                    a.clone(),
                    b.clone(),
                    slots,
                    ::aver_rt::str_lit!("add"),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSub(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::core::resolveBinExpr(
                    a.clone(),
                    b.clone(),
                    slots,
                    ::aver_rt::str_lit!("sub"),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMul(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::core::resolveBinExpr(
                    a.clone(),
                    b.clone(),
                    slots,
                    ::aver_rt::str_lit!("mul"),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprDiv(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::core::resolveBinExpr(
                    a.clone(),
                    b.clone(),
                    slots,
                    ::aver_rt::str_lit!("div"),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeg(__inner_fields) => {
                let inner = &__inner_fields.0;
                (
                    crate::aver_generated::domain::ast::Expr::ExprNeg(
                        crate::aver_generated::domain::ast::Expr__ExprNeg(
                            crate::aver_generated::domain::resolver::core::resolveExprSimple(
                                inner.clone(),
                                slots.clone(),
                            ),
                        ),
                    ),
                    slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprEq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::core::resolveBinExpr(
                    a.clone(),
                    b.clone(),
                    slots,
                    ::aver_rt::str_lit!("eq"),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::core::resolveBinExpr(
                    a.clone(),
                    b.clone(),
                    slots,
                    ::aver_rt::str_lit!("neq"),
                )
            }
            _ => crate::aver_generated::domain::resolver::core::resolveExprAfterArith(expr, slots),
        }
    }
}

/// Continue expression resolution after arithmetic forms.
pub fn resolveExprAfterArith(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::core::resolveBinExpr(
                    a.clone(),
                    b.clone(),
                    slots,
                    ::aver_rt::str_lit!("lt"),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::core::resolveBinExpr(
                    a.clone(),
                    b.clone(),
                    slots,
                    ::aver_rt::str_lit!("gt"),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::core::resolveBinExpr(
                    a.clone(),
                    b.clone(),
                    slots,
                    ::aver_rt::str_lit!("lte"),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::core::resolveBinExpr(
                    a.clone(),
                    b.clone(),
                    slots,
                    ::aver_rt::str_lit!("gte"),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCall(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = &__name_fields.1;
                crate::aver_generated::domain::resolver::core::resolveCallExpr(name, args, slots)
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(__fnId_fields) => {
                let fnId = __fnId_fields.0.clone();
                let args = &__fnId_fields.1;
                (
                    crate::aver_generated::domain::ast::Expr::ExprCallDirect(
                        crate::aver_generated::domain::ast::Expr__ExprCallDirect(fnId, {
                            let __arg23 @ _ = aver_rt::list_builder_new(
                                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                            );
                            crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                                args,
                                slots.clone(),
                                __arg23,
                            )
                        }),
                    ),
                    slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = &__name_fields.1;
                (
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(name, {
                            let __arg24 @ _ = aver_rt::list_builder_new(
                                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                            );
                            crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                                args,
                                slots.clone(),
                                __arg24,
                            )
                        }),
                    ),
                    slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltinId(__id_fields) => {
                let id = __id_fields.0.clone();
                let args = &__id_fields.1;
                (
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltinId(id, {
                            let __arg25 @ _ = aver_rt::list_builder_new(
                                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                            );
                            crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                                args,
                                slots.clone(),
                                __arg25,
                            )
                        }),
                    ),
                    slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMatch(__subj_fields) => {
                let subj = &__subj_fields.0;
                let arms = &__subj_fields.1;
                crate::aver_generated::domain::resolver::core::resolveMatchExpr(
                    subj.clone(),
                    arms,
                    slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprPropagate(__inner_fields) => {
                let inner = &__inner_fields.0;
                crate::aver_generated::domain::resolver::core::resolvePropExpr(inner.clone(), slots)
            }
            crate::aver_generated::domain::ast::Expr__view::ExprConcat(__parts_fields) => {
                let parts = &__parts_fields.0;
                crate::aver_generated::domain::resolver::core::resolveConcatExpr(parts, slots)
            }
            crate::aver_generated::domain::ast::Expr__view::ExprTuple(__exprs_fields) => {
                let exprs = &__exprs_fields.0;
                crate::aver_generated::domain::resolver::core::resolveTupleExpr(exprs, slots)
            }
            _ => crate::aver_generated::domain::resolver::core::resolveExprAfterAggregate(
                expr, slots,
            ),
        }
    }
}

/// Finish expression resolution for aggregate forms.
pub fn resolveExprAfterAggregate(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprIndependentProduct(
                __exprs_fields,
            ) => {
                let exprs = &__exprs_fields.0;
                let unwrap = __exprs_fields.1.clone();
                crate::aver_generated::domain::resolver::core::resolveIndependentProductExpr(
                    exprs, unwrap, slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprList(__exprs_fields) => {
                let exprs = &__exprs_fields.0;
                crate::aver_generated::domain::resolver::core::resolveListExpr(exprs, slots)
            }
            crate::aver_generated::domain::ast::Expr__view::ExprRecord(__name_fields) => {
                let name = __name_fields.0.clone();
                let fields = &__name_fields.1;
                crate::aver_generated::domain::resolver::core::resolveRecordExpr(
                    name, fields, slots,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(__obj_fields) => {
                let obj = &__obj_fields.0;
                let field = __obj_fields.1.clone();
                crate::aver_generated::domain::resolver::core::resolveFieldExpr(
                    obj.clone(),
                    field,
                    slots,
                )
            }
            _ => (expr, slots),
        }
    }
}

/// Resolve binary expression and rebuild.
#[inline(always)]
pub fn resolveBinExpr(
    mut a @ _: crate::aver_generated::domain::ast::Expr,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    op @ _: AverStr,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    let ra @ _ = crate::aver_generated::domain::resolver::core::resolveExprSimple(a, slots.clone());
    let rb @ _ = crate::aver_generated::domain::resolver::core::resolveExprSimple(b, slots.clone());
    {
        let __dispatch_subject = op;
        if &*__dispatch_subject == "add" {
            (
                crate::aver_generated::domain::ast::Expr::ExprAdd(
                    crate::aver_generated::domain::ast::Expr__ExprAdd(ra, rb),
                ),
                slots,
            )
        } else {
            if &*__dispatch_subject == "sub" {
                (
                    crate::aver_generated::domain::ast::Expr::ExprSub(
                        crate::aver_generated::domain::ast::Expr__ExprSub(ra, rb),
                    ),
                    slots,
                )
            } else {
                if &*__dispatch_subject == "mul" {
                    (
                        crate::aver_generated::domain::ast::Expr::ExprMul(
                            crate::aver_generated::domain::ast::Expr__ExprMul(ra, rb),
                        ),
                        slots,
                    )
                } else {
                    if &*__dispatch_subject == "div" {
                        (
                            crate::aver_generated::domain::ast::Expr::ExprDiv(
                                crate::aver_generated::domain::ast::Expr__ExprDiv(ra, rb),
                            ),
                            slots,
                        )
                    } else {
                        if &*__dispatch_subject == "eq" {
                            (
                                crate::aver_generated::domain::ast::Expr::ExprEq(
                                    crate::aver_generated::domain::ast::Expr__ExprEq(ra, rb),
                                ),
                                slots,
                            )
                        } else {
                            if &*__dispatch_subject == "neq" {
                                (
                                    crate::aver_generated::domain::ast::Expr::ExprNeq(
                                        crate::aver_generated::domain::ast::Expr__ExprNeq(ra, rb),
                                    ),
                                    slots,
                                )
                            } else {
                                if &*__dispatch_subject == "lt" {
                                    (
                                        crate::aver_generated::domain::ast::Expr::ExprLt(
                                            crate::aver_generated::domain::ast::Expr__ExprLt(
                                                ra, rb,
                                            ),
                                        ),
                                        slots,
                                    )
                                } else {
                                    if &*__dispatch_subject == "gt" {
                                        (
                                            crate::aver_generated::domain::ast::Expr::ExprGt(
                                                crate::aver_generated::domain::ast::Expr__ExprGt(
                                                    ra, rb,
                                                ),
                                            ),
                                            slots,
                                        )
                                    } else {
                                        if &*__dispatch_subject == "lte" {
                                            (crate::aver_generated::domain::ast::Expr::ExprLte(crate::aver_generated::domain::ast::Expr__ExprLte(ra, rb)), slots)
                                        } else {
                                            if &*__dispatch_subject == "gte" {
                                                (crate::aver_generated::domain::ast::Expr::ExprGte(crate::aver_generated::domain::ast::Expr__ExprGte(ra, rb)), slots)
                                            } else {
                                                (crate::aver_generated::domain::ast::Expr::ExprAdd(crate::aver_generated::domain::ast::Expr__ExprAdd(ra, rb)), slots)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Resolve expression, discarding slots update (for non-match contexts).
pub fn resolveExprSimple(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let (e, _) = crate::aver_generated::domain::resolver::core::resolveExpr(expr, slots);
        e
    }
}

/// Resolve function call arguments.
pub fn resolveCallExpr(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprCall(
            crate::aver_generated::domain::ast::Expr__ExprCall(name, {
                let __arg3 @ _ = aver_rt::list_builder_new(
                    (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                );
                crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                    args,
                    slots.clone(),
                    __arg3,
                )
            }),
        ),
        slots,
    )
}

/// Resolve match expression, collecting pattern slots.
pub fn resolveMatchExpr(
    mut subj @ _: crate::aver_generated::domain::ast::Expr,
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    let rs @ _ =
        crate::aver_generated::domain::resolver::core::resolveExprSimple(subj, slots.clone());
    let armsResult @ _ = {
        let __arg7 @ _ =
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
        crate::aver_generated::domain::resolver::core::resolveArms__collected(arms, slots, __arg7)
    };
    {
        let (resolvedArms, mergedSlots) = armsResult;
        (
            crate::aver_generated::domain::ast::Expr::ExprMatch(
                crate::aver_generated::domain::ast::Expr__ExprMatch(rs, resolvedArms),
            ),
            mergedSlots,
        )
    }
}

/// Resolve ? propagation.
pub fn resolvePropExpr(
    mut inner @ _: crate::aver_generated::domain::ast::Expr,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprPropagate(
            crate::aver_generated::domain::ast::Expr__ExprPropagate(
                crate::aver_generated::domain::resolver::core::resolveExprSimple(
                    inner,
                    slots.clone(),
                ),
            ),
        ),
        slots,
    )
}

/// Resolve string concatenation parts.
pub fn resolveConcatExpr(
    parts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprConcat(
            crate::aver_generated::domain::ast::Expr__ExprConcat({
                let __arg2 @ _ = aver_rt::list_builder_new(
                    (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                );
                crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                    parts,
                    slots.clone(),
                    __arg2,
                )
            }),
        ),
        slots,
    )
}

/// Resolve tuple expressions.
pub fn resolveTupleExpr(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprTuple(
            crate::aver_generated::domain::ast::Expr__ExprTuple({
                let __arg2 @ _ = aver_rt::list_builder_new(
                    (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                );
                crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                    exprs,
                    slots.clone(),
                    __arg2,
                )
            }),
        ),
        slots,
    )
}

/// Resolve independent product expressions.
pub fn resolveIndependentProductExpr(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    unwrap @ _: bool,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
            crate::aver_generated::domain::ast::Expr__ExprIndependentProduct(
                {
                    let __arg3 @ _ = aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    );
                    crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                        exprs,
                        slots.clone(),
                        __arg3,
                    )
                },
                unwrap,
            ),
        ),
        slots,
    )
}

/// Resolve list expressions.
pub fn resolveListExpr(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprList(
            crate::aver_generated::domain::ast::Expr__ExprList({
                let __arg2 @ _ = aver_rt::list_builder_new(
                    (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                );
                crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                    exprs,
                    slots.clone(),
                    __arg2,
                )
            }),
        ),
        slots,
    )
}

/// Resolve record expressions.
pub fn resolveRecordExpr(
    name @ _: AverStr,
    fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprRecord(
            crate::aver_generated::domain::ast::Expr__ExprRecord(name, {
                let __arg3 @ _ = aver_rt::list_builder_new(
                    (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                );
                crate::aver_generated::domain::resolver::core::resolveFields__collected(
                    fields,
                    slots.clone(),
                    __arg3,
                )
            }),
        ),
        slots,
    )
}

/// Resolve field access.
pub fn resolveFieldExpr(
    mut obj @ _: crate::aver_generated::domain::ast::Expr,
    field @ _: AverStr,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
            crate::aver_generated::domain::ast::Expr__ExprFieldAccess(
                crate::aver_generated::domain::resolver::core::resolveExprSimple(
                    obj,
                    slots.clone(),
                ),
                field,
            ),
        ),
        slots,
    )
}

/// Resolve a variable: local vars become ExprSlot, others stay ExprVar.
#[inline(always)]
pub fn resolveVar(
    name @ _: AverStr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match (slots).get(&name) {
        Some(slot @ _) => {
            let slot = slot.clone();
            crate::aver_generated::domain::ast::Expr::ExprSlot(
                crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
            )
        }
        None => crate::aver_generated::domain::ast::Expr::ExprVar(
            crate::aver_generated::domain::ast::Expr__ExprVar(name),
        ),
    }
}

/// Resolve a list of expressions.
#[inline(always)]
pub fn resolveExprs(
    mut exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    loop {
        crate::cancel_checkpoint();
        ::aver_rt::aver_list_match_reuse!(exprs, [] => { return acc.reverse(); }, [e, rest, __list_slot] => { {
            let __tco0 = rest;
            let __tco2 = __list_slot.prepend(crate::aver_generated::domain::resolver::core::resolveExprSimple(e, slots.clone()), acc);
            exprs = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Resolve record field expressions.
#[inline(always)]
pub fn resolveFields(
    mut fields @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    loop {
        crate::cancel_checkpoint();
        ::aver_rt::aver_list_match_reuse!(fields, [] => { return acc.reverse(); }, [pair, rest, __list_slot] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco2 = __list_slot.prepend((name, crate::aver_generated::domain::resolver::core::resolveExprSimple(expr, slots.clone())), acc);
            fields = __tco0;
            acc = __tco2;
            continue;
        } } })
    }
}

/// Resolve match arms, collecting all pattern slots.
#[inline(always)]
pub fn resolveArms(
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>;
    let mut arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> = arms;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = arms;
            if __list_subject.is_empty() {
                return (acc.reverse(), slots);
            } else {
                if let (Some(arm), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (__stp0, _) = crate::aver_generated::domain::resolver::core::resolveArm(
                            arm.clone(),
                            &slots,
                        );
                        {
                            let __tco0 = rest;
                            let __tco2 = aver_rt::AverList::prepend_owned(__stp0, acc);
                            __hold0 = __tco0;
                            arms = &__hold0;
                            acc = __tco2;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Resolve one match arm; continue with the ORIGINAL slots, since an arm's pattern binders are scoped to its own body. Threading the arm-extended map onward leaked binder slots into later arms and past the match, where a wrapping binding aliased them and shadowed names resolved wrong.
pub fn resolveOneArm(
    mut arm @ _: crate::aver_generated::domain::ast::MatchArm,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    {
        let (resolvedArm, _) =
            crate::aver_generated::domain::resolver::core::resolveArm(arm, &slots);
        {
            let __arg5 @ _ = aver_rt::AverList::prepend(resolvedArm, acc);
            crate::aver_generated::domain::resolver::core::resolveArms(rest, slots, __arg5)
        }
    }
}

/// Resolve a match arm: extend slots with pattern bindings, then resolve body.
pub fn resolveArm(
    mut arm @ _: crate::aver_generated::domain::ast::MatchArm,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::MatchArm,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    let armSlotResult @ _ =
        crate::aver_generated::domain::resolver::core::addPatternSlots(&arm.pattern, slots.clone());
    {
        let (newSlots, _) = armSlotResult;
        {
            let __arg4 @ _ = crate::aver_generated::domain::resolver::core::patternBindingSlots(
                &arm.pattern,
                slots,
            );
            crate::aver_generated::domain::resolver::core::resolveArmBody(arm, newSlots, __arg4)
        }
    }
}

/// Resolve arm body with extended slots.
pub fn resolveArmBody(
    mut arm @ _: crate::aver_generated::domain::ast::MatchArm,
    mut newSlots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut bindingSlots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::MatchArm,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    {
        let (re, finalSlots) =
            crate::aver_generated::domain::resolver::core::resolveExpr(arm.body, newSlots);
        (
            crate::aver_generated::domain::ast::MatchArm {
                pattern: arm.pattern,
                body: re,
                bindingSlots: bindingSlots,
            },
            finalSlots,
        )
    }
}

/// Compute only the slots introduced by one pattern, using the same numbering as addPatternSlots.
pub fn patternBindingSlots(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> aver_rt::AverMap<AverStr, aver_rt::AverInt> {
    crate::cancel_checkpoint();
    let nextSlot @ _ = crate::aver_generated::domain::resolver::core::mapMaxVal(slots)
        .add(&aver_rt::AverInt::from_i64(1));
    {
        let (bindingSlots, _) = {
            let __arg4 @ _ = <aver_rt::AverMap<AverStr, aver_rt::AverInt>>::new();
            crate::aver_generated::domain::resolver::core::patternBindingSlotsInner(
                pat, __arg4, nextSlot,
            )
        };
        bindingSlots
    }
}

/// Assign slots to pattern binders without carrying the outer slot map.
pub fn patternBindingSlotsInner(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
    mut bindingSlots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    crate::cancel_checkpoint();
    match pat {
        crate::aver_generated::domain::ast::Pattern::PatVar(name) => {
            let name = name.clone();
            (
                bindingSlots.insert_owned(name, next.clone()),
                next.add(&aver_rt::AverInt::from_i64(1)),
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            crate::aver_generated::domain::resolver::core::patternBindingSlotsCons(
                h,
                t,
                bindingSlots,
                next,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructor(_, bindings) => {
            crate::aver_generated::domain::resolver::core::patternBindingSlotsConstructor(
                bindings,
                bindingSlots,
                next,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructorId(_, _, bindings) => {
            crate::aver_generated::domain::resolver::core::patternBindingSlotsConstructor(
                bindings,
                bindingSlots,
                next,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatTuple(pats) => {
            crate::aver_generated::domain::resolver::core::patternBindingSlotsTuple(
                pats,
                bindingSlots,
                next,
            )
        }
        _ => (bindingSlots, next),
    }
}

/// Assign slots for cons-pattern binders.
pub fn patternBindingSlotsCons(
    h @ _: AverStr,
    t @ _: AverStr,
    mut bindingSlots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    crate::cancel_checkpoint();
    let bindingSlots2 @ _ = bindingSlots.insert_owned(h, next.clone());
    (
        bindingSlots2.insert_owned(t, next.add(&aver_rt::AverInt::from_i64(1))),
        next.add(&aver_rt::AverInt::from_i64(2)),
    )
}

/// Assign slots for constructor-pattern binders.
#[inline(always)]
pub fn patternBindingSlotsConstructor(
    bindings @ _: &aver_rt::AverList<AverStr>,
    mut bindingSlots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut bindings @ _: &aver_rt::AverList<AverStr> = bindings;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = bindings;
            if __list_subject.is_empty() {
                return (bindingSlots, next);
            } else {
                if let Some((name, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    {
                        let __tco0 = rest;
                        let __tco1 = bindingSlots.insert_owned(name, next.clone());
                        let __tco2 = next.add(&aver_rt::AverInt::from_i64(1));
                        __hold0 = __tco0;
                        bindings = &__hold0;
                        bindingSlots = __tco1;
                        next = __tco2;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Assign slots for tuple sub-patterns.
#[inline(always)]
pub fn patternBindingSlotsTuple(
    pats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    mut bindingSlots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>;
    let mut pats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern> = pats;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = pats;
            if __list_subject.is_empty() {
                return (bindingSlots, next);
            } else {
                if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (newBindingSlots, newNext) =
                            crate::aver_generated::domain::resolver::core::patternBindingSlotsInner(
                                p,
                                bindingSlots,
                                next,
                            );
                        {
                            let __tco0 = rest;
                            let __tco1 = newBindingSlots;
                            let __tco2 = newNext;
                            __hold0 = __tco0;
                            pats = &__hold0;
                            bindingSlots = __tco1;
                            next = __tco2;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Find the next available slot and add pattern bindings.
pub fn addPatternSlots(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    crate::cancel_checkpoint();
    let nextSlot @ _ = crate::aver_generated::domain::resolver::core::mapMaxVal(&slots)
        .add(&aver_rt::AverInt::from_i64(1));
    crate::aver_generated::domain::resolver::core::addPatternSlotsInner(pat, slots, nextSlot)
}

/// Get maximum value in a map, or -1 if empty.
#[inline(always)]
pub fn mapMaxVal(m @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    let vals @ _ = {
        let mut es: Vec<_> = m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
        es.sort_by(|a, b| a.0.cmp(&b.0));
        aver_rt::AverIntList::from_vec(es.into_iter().map(|(_, v)| v).collect::<Vec<_>>())
    };
    crate::aver_generated::domain::resolver::core::maxInList(&vals, aver_rt::AverInt::from_i64(-1))
}

/// Find maximum value in a list.
#[inline(always)]
pub fn maxInList(
    vals @ _: &aver_rt::AverIntList,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let mut __hold0: aver_rt::AverIntList;
    let mut vals @ _: &aver_rt::AverIntList = vals;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = vals;
            if __list_subject.is_empty() {
                return acc;
            } else {
                if let Some((v, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    if (v > acc) {
                        {
                            let __tco0 = rest;
                            let __tco1 = v;
                            __hold0 = __tco0;
                            vals = &__hold0;
                            acc = __tco1;
                            continue;
                        }
                    } else {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            vals = &__hold0;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Add slots for pattern bindings.
pub fn addPatternSlotsInner(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    crate::cancel_checkpoint();
    match pat {
        crate::aver_generated::domain::ast::Pattern::PatVar(name) => {
            let name = name.clone();
            (
                slots.insert_owned(name, next.clone()),
                next.add(&aver_rt::AverInt::from_i64(1)),
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatCons(h, t) => {
            let h = h.clone();
            let t = t.clone();
            crate::aver_generated::domain::resolver::core::addConsSlots(h, t, slots, next)
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructor(_, bindings) => {
            crate::aver_generated::domain::resolver::core::addConstructorSlots(
                bindings, slots, next,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructorId(_, _, bindings) => {
            crate::aver_generated::domain::resolver::core::addConstructorSlots(
                bindings, slots, next,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatTuple(pats) => {
            crate::aver_generated::domain::resolver::core::addTuplePatternSlots(pats, slots, next)
        }
        _ => (slots, next),
    }
}

/// Add slots for cons pattern [h, ..t].
pub fn addConsSlots(
    h @ _: AverStr,
    t @ _: AverStr,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    crate::cancel_checkpoint();
    let slots2 @ _ = slots.insert_owned(h, next.clone());
    (
        slots2.insert_owned(t, next.add(&aver_rt::AverInt::from_i64(1))),
        next.add(&aver_rt::AverInt::from_i64(2)),
    )
}

/// Add slots for constructor pattern bindings.
#[inline(always)]
pub fn addConstructorSlots(
    bindings @ _: &aver_rt::AverList<AverStr>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut bindings @ _: &aver_rt::AverList<AverStr> = bindings;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = bindings;
            if __list_subject.is_empty() {
                return (slots, next);
            } else {
                if let Some((name, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    {
                        let __tco0 = rest;
                        let __tco1 = slots.insert_owned(name, next.clone());
                        let __tco2 = next.add(&aver_rt::AverInt::from_i64(1));
                        __hold0 = __tco0;
                        bindings = &__hold0;
                        slots = __tco1;
                        next = __tco2;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Add slots for tuple sub-patterns.
#[inline(always)]
pub fn addTuplePatternSlots(
    pats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>;
    let mut pats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern> = pats;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = pats;
            if __list_subject.is_empty() {
                return (slots, next);
            } else {
                if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (newSlots, newNext) =
                            crate::aver_generated::domain::resolver::core::addPatternSlotsInner(
                                p, slots, next,
                            );
                        {
                            let __tco0 = rest;
                            let __tco1 = newSlots;
                            let __tco2 = newNext;
                            __hold0 = __tco0;
                            pats = &__hold0;
                            slots = __tco1;
                            next = __tco2;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Synthesized collecting variant of `resolveStmts`. Appends to a builder where `resolveStmts` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveStmts__collected(
    stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    aver_rt::AverInt,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = stmts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = stmts;
            if __list_subject.is_empty() {
                return (aver_rt::list_builder_finalize(acc), next, slots);
            } else {
                if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match s {
                        crate::aver_generated::domain::ast::Stmt::StmtBind(__stp1, __stp0) => {
                            let __stp1 = __stp1.clone();
                            {
                                let (__stp3, __stp2) =
                                    crate::aver_generated::domain::resolver::core::resolveExpr(
                                        __stp0.clone(),
                                        slots,
                                    );
                                {
                                    let __stp4 = __stp2.insert_owned(__stp1, next.clone());
                                    {
                                        let __tco0 = rest;
                                        let __tco1 = __stp4;
                                        let __tco2 = next.add(&aver_rt::AverInt::from_i64(1));
                                        let __tco3 = aver_rt::list_builder_push(
                                            acc,
                                            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(
                                                next, __stp3,
                                            ),
                                        );
                                        __hold0 = __tco0;
                                        stmts = &__hold0;
                                        slots = __tco1;
                                        next = __tco2;
                                        acc = __tco3;
                                        continue;
                                    }
                                }
                            }
                        }
                        crate::aver_generated::domain::ast::Stmt::StmtExpr(__stp0) => {
                            let (__stp6, __stp5) =
                                crate::aver_generated::domain::resolver::core::resolveExpr(
                                    __stp0.clone(),
                                    slots,
                                );
                            {
                                let __tco0 = rest;
                                let __tco1 = __stp5.clone();
                                let __tco2 = {
                                    let __arg14 @ _ =
                                        crate::aver_generated::domain::resolver::core::mapMaxVal(
                                            &__stp5,
                                        )
                                        .add(&aver_rt::AverInt::from_i64(1));
                                    crate::aver_generated::domain::resolver::core::maxInt(
                                        next, __arg14,
                                    )
                                };
                                let __tco3 = aver_rt::list_builder_push(
                                    acc,
                                    crate::aver_generated::domain::ast::Stmt::StmtExpr(__stp6),
                                );
                                __hold0 = __tco0;
                                stmts = &__hold0;
                                slots = __tco1;
                                next = __tco2;
                                acc = __tco3;
                                continue;
                            }
                        }
                        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(_, _) => {
                            let __tco0 = rest;
                            let __tco3 = aver_rt::list_builder_push(acc, s.clone());
                            __hold0 = __tco0;
                            stmts = &__hold0;
                            acc = __tco3;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Synthesized collecting variant of `resolveExprs`. Appends to a builder where `resolveExprs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveExprs__collected(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let (Some(e), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco2 = aver_rt::list_builder_push(
                            acc,
                            crate::aver_generated::domain::resolver::core::resolveExprSimple(
                                e.clone(),
                                slots.clone(),
                            ),
                        );
                        __hold0 = __tco0;
                        exprs = &__hold0;
                        acc = __tco2;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Synthesized collecting variant of `resolveFields`. Appends to a builder where `resolveFields` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveFields__collected(
    fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    let mut __hold0: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>;
    let mut fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> =
        fields;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fields;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (name, expr) = pair.clone();
                        {
                            let __tco0 = rest;
                            let __tco2 = aver_rt::list_builder_push(acc, (name, crate::aver_generated::domain::resolver::core::resolveExprSimple(expr, slots.clone())));
                            __hold0 = __tco0;
                            fields = &__hold0;
                            acc = __tco2;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Synthesized collecting variant of `resolveArms`. Appends to a builder where `resolveArms` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveArms__collected(
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>;
    let mut arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> = arms;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = arms;
            if __list_subject.is_empty() {
                return (aver_rt::list_builder_finalize(acc), slots);
            } else {
                if let (Some(arm), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (__stp0, _) = crate::aver_generated::domain::resolver::core::resolveArm(
                            arm.clone(),
                            &slots,
                        );
                        {
                            let __tco0 = rest;
                            let __tco2 = aver_rt::list_builder_push(acc, __stp0);
                            __hold0 = __tco0;
                            arms = &__hold0;
                            acc = __tco2;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}
