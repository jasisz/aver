#[allow(unused_imports)]
use crate::*;

/// Build name→fnId map from resolved function list.
#[inline(always)]
pub fn buildFnMap(
    fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut acc @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut idx @ _: aver_rt::AverInt,
) -> aver_rt::AverMap<AverStr, aver_rt::AverInt> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>;
    let mut fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> = fns;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fns;
            match aver_rt::list_uncons(__list_subject) {
                Some((f, rest)) => {
                    let __tco0 = rest;
                    let __tco1 = acc.insert_owned(f.name.clone(), idx.clone());
                    let __tco2 = idx.add(&aver_rt::AverInt::from_i64(1));
                    __hold0 = __tco0;
                    fns = &__hold0;
                    acc = __tco1;
                    idx = __tco2;
                    continue;
                }
                None => {
                    return acc;
                }
            }
        }
    }
}

/// Transform ExprCall→ExprCallDirect for known functions in all fn bodies.
#[inline(always)]
pub fn resolveCallsInFns(
    fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>;
    let mut fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> = fns;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fns;
            match aver_rt::list_uncons(__list_subject) {
                Some((f, rest)) => {
                    let __tco0 = rest;
                    let __tco2 = aver_rt::AverList::prepend_owned(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInFn(
                            f.clone(),
                            fnMap,
                        ),
                        acc,
                    );
                    __hold0 = __tco0;
                    fns = &__hold0;
                    acc = __tco2;
                    continue;
                }
                None => {
                    return (acc).reverse_owned();
                }
            }
        }
    }
}

/// Transform calls in one function body.
pub fn resolveCallsInFn(
    mut fd @ _: crate::aver_generated::domain::ast::FnDef,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    Box::new(crate::aver_generated::domain::ast::FnDef__Fields {
        name: fd.name,
        params: fd.params,
        body: {
            let __arg2 @ _ = aver_rt::list_builder_new(0usize);
            crate::aver_generated::domain::resolver::calls::resolveCallsInStmts__collected(
                &fd.body, fnMap, __arg2,
            )
        },
        slotCount: fd.slotCount,
        slotMap: fd.slotMap,
        fastPath: fd.fastPath,
        tailLoop: fd.tailLoop,
    })
}

/// Resolve calls in a list of statements.
#[inline(always)]
pub fn resolveCallsInStmts(
    mut stmts @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    loop {
        crate::cancel_checkpoint();
        ::aver_rt::aver_list_match_reuse!(stmts, [] => { return (acc).reverse_owned(); }, [s, rest, __list_slot] => { {
            let __tco0 = rest;
            let __tco2 = __list_slot.prepend(crate::aver_generated::domain::resolver::calls::resolveCallsInStmt(&s, fnMap), acc);
            stmts = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Resolve calls in a single statement.
pub fn resolveCallsInStmt(
    s @ _: &crate::aver_generated::domain::ast::Stmt,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::Stmt {
    crate::cancel_checkpoint();
    match s {
        crate::aver_generated::domain::ast::Stmt::StmtBind(name, expr) => {
            let name = name.clone();
            crate::aver_generated::domain::ast::Stmt::StmtBind(
                name,
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                    expr.clone(),
                    fnMap,
                ),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, expr) => {
            let slot = slot.clone();
            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(
                slot,
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                    expr.clone(),
                    fnMap,
                ),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtExpr(
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                    expr.clone(),
                    fnMap,
                ),
            )
        }
    }
}

/// Replace ExprCall with ExprCallDirect for known user functions.
pub fn resolveCallsInExpr(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprBoolBranch(__cond_fields) => {
                let cond = &__cond_fields.0;
                let thenExpr = &__cond_fields.1;
                let elseExpr = &__cond_fields.2;
                crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                    crate::aver_generated::domain::ast::Expr__ExprBoolBranch(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            cond.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            thenExpr.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            elseExpr.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCall(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = &__name_fields.1;
                crate::aver_generated::domain::resolver::calls::resolveOneCall(name, args, fnMap)
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(__fnId_fields) => {
                let fnId = __fnId_fields.0.clone();
                let args = &__fnId_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprCallDirect(
                    crate::aver_generated::domain::ast::Expr__ExprCallDirect(fnId, {
                        let __arg14 @ _ = aver_rt::list_builder_new(0usize);
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(args, fnMap, __arg14)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = &__name_fields.1;
                match crate::aver_generated::domain::ast::builtinNameToId(name.clone()) {
                    Some(id @ _) => crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltinId(id, {
                            let __arg15 @ _ = aver_rt::list_builder_new(0usize);
                            crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(args, fnMap, __arg15)
                        }),
                    ),
                    None => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(name, {
                            let __arg16 @ _ = aver_rt::list_builder_new(0usize);
                            crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(args, fnMap, __arg16)
                        }),
                    ),
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltinId(__id_fields) => {
                let id = __id_fields.0.clone();
                let args = &__id_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltinId(id, {
                        let __arg17 @ _ = aver_rt::list_builder_new(0usize);
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(args, fnMap, __arg17)
                    }),
                )
            }
            _ => crate::aver_generated::domain::resolver::calls::resolveCallsInExprInternal(
                expr, fnMap,
            ),
        }
    }
}

/// Continue direct-call linking for specialized internal and arithmetic forms.
pub fn resolveCallsInExprInternal(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprBinopSlotInt(_) => expr,
            crate::aver_generated::domain::ast::Expr__view::ExprBinopSlots(_) => expr,
            crate::aver_generated::domain::ast::Expr__view::ExprCmpSlotInt(_) => expr,
            crate::aver_generated::domain::ast::Expr__view::ExprCmpSlots(_) => expr,
            crate::aver_generated::domain::ast::Expr__view::ExprVectorGetOrInt(
                __vecExpr_fields,
            ) => {
                let vecExpr = &__vecExpr_fields.0;
                let idxExpr = &__vecExpr_fields.1;
                let defaultValue = __vecExpr_fields.2.clone();
                crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                    crate::aver_generated::domain::ast::Expr__ExprVectorGetOrInt(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            vecExpr.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            idxExpr.clone(),
                            fnMap,
                        ),
                        defaultValue,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIntModOrInt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                let defaultValue = __a_fields.2.clone();
                crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                    crate::aver_generated::domain::ast::Expr__ExprIntModOrInt(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                        defaultValue,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprAdd(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprAdd(
                    crate::aver_generated::domain::ast::Expr__ExprAdd(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSub(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprSub(
                    crate::aver_generated::domain::ast::Expr__ExprSub(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMul(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprMul(
                    crate::aver_generated::domain::ast::Expr__ExprMul(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprDiv(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprDiv(
                    crate::aver_generated::domain::ast::Expr__ExprDiv(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeg(__inner_fields) => {
                let inner = &__inner_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprNeg(
                    crate::aver_generated::domain::ast::Expr__ExprNeg(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            inner.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            _ => {
                crate::aver_generated::domain::resolver::calls::resolveCallsInExprTail(expr, fnMap)
            }
        }
    }
}

/// Finish direct-call linking for comparisons, aggregates, and product forms.
pub fn resolveCallsInExprTail(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprEq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprEq(
                    crate::aver_generated::domain::ast::Expr__ExprEq(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprNeq(
                    crate::aver_generated::domain::ast::Expr__ExprNeq(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprLt(
                    crate::aver_generated::domain::ast::Expr__ExprLt(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprGt(
                    crate::aver_generated::domain::ast::Expr__ExprGt(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprLte(
                    crate::aver_generated::domain::ast::Expr__ExprLte(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprGte(
                    crate::aver_generated::domain::ast::Expr__ExprGte(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a.clone(),
                            fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMatch(__subj_fields) => {
                let subj = &__subj_fields.0;
                let arms = &__subj_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprMatch(
                    crate::aver_generated::domain::ast::Expr__ExprMatch(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            subj.clone(),
                            fnMap,
                        ),
                        {
                            let __arg26 @ _ = aver_rt::list_builder_new(0usize);
                            crate::aver_generated::domain::resolver::calls::resolveCallsInArms__collected(arms, fnMap, __arg26)
                        },
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprPropagate(__inner_fields) => {
                let inner = &__inner_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprPropagate(
                    crate::aver_generated::domain::ast::Expr__ExprPropagate(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            inner.clone(),
                            fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprConcat(__parts_fields) => {
                let parts = &__parts_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprConcat(
                    crate::aver_generated::domain::ast::Expr__ExprConcat({
                        let __arg27 @ _ = aver_rt::list_builder_new(0usize);
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(parts, fnMap, __arg27)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprTuple(__exprs_fields) => {
                let exprs = &__exprs_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprTuple(
                    crate::aver_generated::domain::ast::Expr__ExprTuple({
                        let __arg28 @ _ = aver_rt::list_builder_new(0usize);
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(exprs, fnMap, __arg28)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIndependentProduct(
                __exprs_fields,
            ) => {
                let exprs = &__exprs_fields.0;
                let unwrap = __exprs_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
                    crate::aver_generated::domain::ast::Expr__ExprIndependentProduct(
                        {
                            let __arg29 @ _ = aver_rt::list_builder_new(0usize);
                            crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(exprs, fnMap, __arg29)
                        },
                        unwrap,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprList(__exprs_fields) => {
                let exprs = &__exprs_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprList(
                    crate::aver_generated::domain::ast::Expr__ExprList({
                        let __arg30 @ _ = aver_rt::list_builder_new(0usize);
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(exprs, fnMap, __arg30)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprRecord(__name_fields) => {
                let name = __name_fields.0.clone();
                let fields = &__name_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprRecord(
                    crate::aver_generated::domain::ast::Expr__ExprRecord(name, {
                        let __arg31 @ _ = aver_rt::list_builder_new(0usize);
                        crate::aver_generated::domain::resolver::calls::resolveCallsInFields__collected(fields, fnMap, __arg31)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(__obj_fields) => {
                let obj = &__obj_fields.0;
                let field = __obj_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
                    crate::aver_generated::domain::ast::Expr__ExprFieldAccess(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            obj.clone(),
                            fnMap,
                        ),
                        field,
                    ),
                )
            }
            _ => expr,
        }
    }
}

/// User fn → ExprCallDirect, builtin → ExprCallBuiltin, record update → ExprCall.
#[inline(always)]
pub fn resolveOneCall(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    let resolvedArgs @ _ = {
        let __arg5 @ _ = aver_rt::list_builder_new(0usize);
        crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(
            args, fnMap, __arg5,
        )
    };
    match (fnMap).get(&name) {
        Some(fnId @ _) => {
            let fnId = fnId.clone();
            crate::aver_generated::domain::ast::Expr::ExprCallDirect(
                crate::aver_generated::domain::ast::Expr__ExprCallDirect(fnId, resolvedArgs),
            )
        }
        None => {
            if name.ends_with(".update") {
                crate::aver_generated::domain::ast::Expr::ExprCall(
                    crate::aver_generated::domain::ast::Expr__ExprCall(name, resolvedArgs),
                )
            } else {
                if crate::aver_generated::domain::resolver::calls::isBuiltinCallName(name.clone()) {
                    crate::aver_generated::domain::resolver::calls::resolveBuiltinCall(
                        name,
                        &resolvedArgs,
                    )
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCall(
                        crate::aver_generated::domain::ast::Expr__ExprCall(name, resolvedArgs),
                    )
                }
            }
        }
    }
}

/// Link a builtin call, discharging a literal-divisor Int.div/Int.mod to a plain Int.
#[inline(always)]
pub fn resolveBuiltinCall(
    name @ _: AverStr,
    resolvedArgs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::resolver::calls::isLiteralDivisorCall(&name, resolvedArgs) {
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                ::aver_rt::str_lit!("Result.withDefault"),
                aver_rt::AverList::from_vec(vec![
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                            name,
                            resolvedArgs.clone(),
                        ),
                    ),
                    crate::aver_generated::domain::ast::Expr::ExprInt(
                        crate::aver_generated::domain::ast::Expr__ExprInt(
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                ]),
            ),
        )
    } else {
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(name, resolvedArgs.clone()),
        )
    }
}

/// Whether this is Int.div/Int.mod over a syntactic nonzero integer literal divisor.
#[inline(always)]
pub fn isLiteralDivisorCall(
    name @ _: &AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> bool {
    crate::cancel_checkpoint();
    if (((&**name == "Int.div") || (&**name == "Int.mod"))
        && (aver_rt::AverInt::from_i64(args.len() as i64) == aver_rt::AverInt::from_i64(2)))
    {
        crate::aver_generated::domain::resolver::calls::isNonzeroIntLiteralDivisor(args)
    } else {
        false
    }
}

/// Whether the second of exactly two arguments is a nonzero integer literal.
pub fn isNonzeroIntLiteralDivisor(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> bool {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        match aver_rt::list_uncons_cloned(__list_subject) {
            Some((_, rest)) => {
                let __list_subject = rest;
                match aver_rt::list_uncons(&__list_subject) {
                    Some((divisor, ignored)) => {
                        crate::aver_generated::domain::resolver::calls::isNonzeroIntLiteral(divisor)
                    }
                    None => false,
                }
            }
            None => false,
        }
    }
}

/// Whether an expression is a nonzero integer literal under at most one unary minus.
pub fn isNonzeroIntLiteral(expr @ _: &crate::aver_generated::domain::ast::Expr) -> bool {
    crate::cancel_checkpoint();
    {
        let __t = expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__k_fields) => {
                let k = __k_fields.0.clone();
                (k != aver_rt::AverInt::from_i64(0))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeg(__inner_fields) => {
                let inner = &__inner_fields.0;
                crate::aver_generated::domain::resolver::calls::isBareNonzeroIntLiteral(inner)
            }
            _ => false,
        }
    }
}

/// Whether an expression is a bare nonzero integer literal, with no unary minus of its own.
pub fn isBareNonzeroIntLiteral(expr @ _: &crate::aver_generated::domain::ast::Expr) -> bool {
    crate::cancel_checkpoint();
    {
        let __t = expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__k_fields) => {
                let k = __k_fields.0.clone();
                (k != aver_rt::AverInt::from_i64(0))
            }
            _ => false,
        }
    }
}

/// Return whether a call name is one of the exact builtin/service entrypoints.
#[inline(always)]
pub fn isBuiltinCallName(name @ _: AverStr) -> bool {
    crate::cancel_checkpoint();
    let __arg1 @ _ = aver_rt::AverList::from_vec(vec![
        ::aver_rt::str_lit!("Args.get"),
        ::aver_rt::str_lit!("Bool.and"),
        ::aver_rt::str_lit!("Bool.not"),
        ::aver_rt::str_lit!("Bool.or"),
        ::aver_rt::str_lit!("String.fromCodePoint"),
        ::aver_rt::str_lit!("String.firstCodePoint"),
        ::aver_rt::str_lit!("Console.error"),
        ::aver_rt::str_lit!("Console.print"),
        ::aver_rt::str_lit!("Console.readLine"),
        ::aver_rt::str_lit!("Console.warn"),
        ::aver_rt::str_lit!("Disk.appendText"),
        ::aver_rt::str_lit!("Disk.delete"),
        ::aver_rt::str_lit!("Disk.deleteDir"),
        ::aver_rt::str_lit!("Disk.exists"),
        ::aver_rt::str_lit!("Disk.listDir"),
        ::aver_rt::str_lit!("Disk.makeDir"),
        ::aver_rt::str_lit!("Disk.readText"),
        ::aver_rt::str_lit!("Disk.writeText"),
        ::aver_rt::str_lit!("Env.get"),
        ::aver_rt::str_lit!("Env.set"),
        ::aver_rt::str_lit!("Float.abs"),
        ::aver_rt::str_lit!("Float.atan2"),
        ::aver_rt::str_lit!("Float.ceil"),
        ::aver_rt::str_lit!("Float.cos"),
        ::aver_rt::str_lit!("Float.floor"),
        ::aver_rt::str_lit!("Float.fromInt"),
        ::aver_rt::str_lit!("Float.fromString"),
        ::aver_rt::str_lit!("Float.max"),
        ::aver_rt::str_lit!("Float.min"),
        ::aver_rt::str_lit!("Float.pi"),
        ::aver_rt::str_lit!("Float.pow"),
        ::aver_rt::str_lit!("Float.round"),
        ::aver_rt::str_lit!("Float.sin"),
        ::aver_rt::str_lit!("Float.sqrt"),
        ::aver_rt::str_lit!("String.fromFloat"),
        ::aver_rt::str_lit!("Http.delete"),
        ::aver_rt::str_lit!("Http.get"),
        ::aver_rt::str_lit!("Http.head"),
        ::aver_rt::str_lit!("Http.patch"),
        ::aver_rt::str_lit!("Http.post"),
        ::aver_rt::str_lit!("Http.put"),
        ::aver_rt::str_lit!("Int.abs"),
        ::aver_rt::str_lit!("Int.div"),
        ::aver_rt::str_lit!("Int.fromString"),
        ::aver_rt::str_lit!("Int.toBigEndian"),
        ::aver_rt::str_lit!("Int.toLittleEndian"),
        ::aver_rt::str_lit!("Int.fromBigEndian"),
        ::aver_rt::str_lit!("Int.fromLittleEndian"),
        ::aver_rt::str_lit!("Int.max"),
        ::aver_rt::str_lit!("Int.min"),
        ::aver_rt::str_lit!("Int.mod"),
        ::aver_rt::str_lit!("Float.fromInt"),
        ::aver_rt::str_lit!("String.fromInt"),
        ::aver_rt::str_lit!("List.concat"),
        ::aver_rt::str_lit!("List.contains"),
        ::aver_rt::str_lit!("List.drop"),
        ::aver_rt::str_lit!("List.head"),
        ::aver_rt::str_lit!("List.len"),
        ::aver_rt::str_lit!("List.prepend"),
        ::aver_rt::str_lit!("List.reverse"),
        ::aver_rt::str_lit!("List.take"),
        ::aver_rt::str_lit!("List.tail"),
        ::aver_rt::str_lit!("List.zip"),
        ::aver_rt::str_lit!("Map.entries"),
        ::aver_rt::str_lit!("Map.fromList"),
        ::aver_rt::str_lit!("Map.get"),
        ::aver_rt::str_lit!("Map.has"),
        ::aver_rt::str_lit!("Map.keys"),
        ::aver_rt::str_lit!("Map.remove"),
        ::aver_rt::str_lit!("Map.set"),
        ::aver_rt::str_lit!("Map.size"),
        ::aver_rt::str_lit!("Map.values"),
        ::aver_rt::str_lit!("Option.None"),
        ::aver_rt::str_lit!("Option.Some"),
        ::aver_rt::str_lit!("Result.fromOption"),
        ::aver_rt::str_lit!("Option.withDefault"),
        ::aver_rt::str_lit!("Random.int"),
        ::aver_rt::str_lit!("Result.Err"),
        ::aver_rt::str_lit!("Result.Ok"),
        ::aver_rt::str_lit!("Result.withDefault"),
        ::aver_rt::str_lit!("String.charAt"),
        ::aver_rt::str_lit!("String.chars"),
        ::aver_rt::str_lit!("String.contains"),
        ::aver_rt::str_lit!("String.endsWith"),
        ::aver_rt::str_lit!("String.fromBool"),
        ::aver_rt::str_lit!("String.fromFloat"),
        ::aver_rt::str_lit!("String.fromInt"),
        ::aver_rt::str_lit!("String.join"),
        ::aver_rt::str_lit!("String.len"),
        ::aver_rt::str_lit!("String.repeat"),
        ::aver_rt::str_lit!("String.replace"),
        ::aver_rt::str_lit!("String.slice"),
        ::aver_rt::str_lit!("String.split"),
        ::aver_rt::str_lit!("String.startsWith"),
        ::aver_rt::str_lit!("String.toLower"),
        ::aver_rt::str_lit!("String.toUpper"),
        ::aver_rt::str_lit!("String.trim"),
        ::aver_rt::str_lit!("Tcp.close"),
        ::aver_rt::str_lit!("Tcp.closeDial"),
        ::aver_rt::str_lit!("Tcp.closeListener"),
        ::aver_rt::str_lit!("Tcp.connect"),
        ::aver_rt::str_lit!("Tcp.beginConnect"),
        ::aver_rt::str_lit!("Tcp.dialled"),
        ::aver_rt::str_lit!("Tcp.listen"),
        ::aver_rt::str_lit!("Tcp.accept"),
        ::aver_rt::str_lit!("Tcp.peerAddress"),
        ::aver_rt::str_lit!("Tcp.ping"),
        ::aver_rt::str_lit!("Tcp.poll"),
        ::aver_rt::str_lit!("Tcp.readLine"),
        ::aver_rt::str_lit!("Tcp.send"),
        ::aver_rt::str_lit!("Tcp.writeLine"),
        ::aver_rt::str_lit!("Terminal.clear"),
        ::aver_rt::str_lit!("Terminal.disableRawMode"),
        ::aver_rt::str_lit!("Terminal.enableRawMode"),
        ::aver_rt::str_lit!("Terminal.flush"),
        ::aver_rt::str_lit!("Terminal.hideCursor"),
        ::aver_rt::str_lit!("Terminal.moveTo"),
        ::aver_rt::str_lit!("Terminal.print"),
        ::aver_rt::str_lit!("Terminal.readKey"),
        ::aver_rt::str_lit!("Terminal.resetColor"),
        ::aver_rt::str_lit!("Terminal.setColor"),
        ::aver_rt::str_lit!("Terminal.showCursor"),
        ::aver_rt::str_lit!("Terminal.size"),
        ::aver_rt::str_lit!("Time.now"),
        ::aver_rt::str_lit!("Time.sleep"),
        ::aver_rt::str_lit!("Time.unixMs"),
        ::aver_rt::str_lit!("Vector.fromList"),
        ::aver_rt::str_lit!("Vector.get"),
        ::aver_rt::str_lit!("Vector.len"),
        ::aver_rt::str_lit!("Vector.new"),
        ::aver_rt::str_lit!("Vector.set"),
        ::aver_rt::str_lit!("List.fromVector"),
    ]);
    crate::aver_generated::domain::resolver::calls::isBuiltinCallNameFrom(name, &__arg1)
}

/// Check builtin/service call names recursively by exact match.
#[inline(always)]
pub fn isBuiltinCallNameFrom(
    mut name @ _: AverStr,
    knownNames @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    let mut __hold1: aver_rt::AverList<AverStr>;
    let mut knownNames @ _: &aver_rt::AverList<AverStr> = knownNames;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = knownNames;
            match aver_rt::list_uncons_cloned(__list_subject) {
                Some((knownName, rest)) => {
                    if (name == knownName) {
                        return true;
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            knownNames = &__hold1;
                            continue;
                        }
                    }
                }
                None => {
                    return false;
                }
            }
        }
    }
}

/// Resolve calls in a list of expressions.
#[inline(always)]
pub fn resolveCallsInExprs(
    mut exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    loop {
        crate::cancel_checkpoint();
        ::aver_rt::aver_list_match_reuse!(exprs, [] => { return (acc).reverse_owned(); }, [e, rest, __list_slot] => { {
            let __tco0 = rest;
            let __tco2 = __list_slot.prepend(crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(e, fnMap), acc);
            exprs = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Resolve calls in match arm bodies.
#[inline(always)]
pub fn resolveCallsInArms(
    mut arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    loop {
        crate::cancel_checkpoint();
        ::aver_rt::aver_list_match_reuse!(arms, [] => { return (acc).reverse_owned(); }, [arm, rest, __list_slot] => { {
            let __tco0 = rest;
            let __tco2 = __list_slot.prepend(crate::aver_generated::domain::ast::MatchArm { pattern: arm.pattern, body: crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(arm.body, fnMap), bindingSlots: arm.bindingSlots }, acc);
            arms = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Resolve calls in record field expressions.
#[inline(always)]
pub fn resolveCallsInFields(
    mut fields @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    loop {
        crate::cancel_checkpoint();
        ::aver_rt::aver_list_match_reuse!(fields, [] => { return (acc).reverse_owned(); }, [pair, rest, __list_slot] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco2 = __list_slot.prepend((name, crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(expr, fnMap)), acc);
            fields = __tco0;
            acc = __tco2;
            continue;
        } } })
    }
}

/// Synthesized collecting variant of `resolveCallsInStmts`. Appends to a builder where `resolveCallsInStmts` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveCallsInStmts__collected(
    stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = stmts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = stmts;
            match aver_rt::list_uncons(__list_subject) {
                Some((s, rest)) => {
                    let __tco0 = rest;
                    let __tco2 = aver_rt::list_builder_push(
                        acc,
                        crate::aver_generated::domain::resolver::calls::resolveCallsInStmt(
                            s, fnMap,
                        ),
                    );
                    __hold0 = __tco0;
                    stmts = &__hold0;
                    acc = __tco2;
                    continue;
                }
                None => {
                    return aver_rt::list_builder_finalize(acc);
                }
            }
        }
    }
}

/// Synthesized collecting variant of `resolveCallsInExprs`. Appends to a builder where `resolveCallsInExprs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveCallsInExprs__collected(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            match aver_rt::list_uncons(__list_subject) {
                Some((e, rest)) => {
                    let __tco0 = rest;
                    let __tco2 = aver_rt::list_builder_push(
                        acc,
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            e.clone(),
                            fnMap,
                        ),
                    );
                    __hold0 = __tco0;
                    exprs = &__hold0;
                    acc = __tco2;
                    continue;
                }
                None => {
                    return aver_rt::list_builder_finalize(acc);
                }
            }
        }
    }
}

/// Synthesized collecting variant of `resolveCallsInArms`. Appends to a builder where `resolveCallsInArms` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveCallsInArms__collected(
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>;
    let mut arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> = arms;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = arms;
            match aver_rt::list_uncons(__list_subject) {
                Some((arm, rest)) => {
                    let __tco0 = rest;
                    let __tco2 = aver_rt::list_builder_push(
                        acc,
                        crate::aver_generated::domain::ast::MatchArm {
                            pattern: arm.pattern.clone(),
                            body:
                                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                                    arm.body.clone(),
                                    fnMap,
                                ),
                            bindingSlots: arm.bindingSlots.clone(),
                        },
                    );
                    __hold0 = __tco0;
                    arms = &__hold0;
                    acc = __tco2;
                    continue;
                }
                None => {
                    return aver_rt::list_builder_finalize(acc);
                }
            }
        }
    }
}

/// Synthesized collecting variant of `resolveCallsInFields`. Appends to a builder where `resolveCallsInFields` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveCallsInFields__collected(
    fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    let mut __hold0: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>;
    let mut fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> =
        fields;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fields;
            match aver_rt::list_uncons(__list_subject) {
                Some((pair, rest)) => {
                    let (name, expr) = pair.clone();
                    {
                        let __tco0 = rest;
                        let __tco2 = aver_rt::list_builder_push(
                            acc,
                            (
                                name,
                                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                                    expr, fnMap,
                                ),
                            ),
                        );
                        __hold0 = __tco0;
                        fields = &__hold0;
                        acc = __tco2;
                        continue;
                    }
                }
                None => {
                    return aver_rt::list_builder_finalize(acc);
                }
            }
        }
    }
}
