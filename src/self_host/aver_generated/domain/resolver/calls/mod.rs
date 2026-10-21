#[allow(unused_imports)]
use crate::*;

/// Build name→fnId map from resolved function list.
#[inline(always)]
pub fn buildFnMap(
    mut fns @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut acc @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut idx @ _: aver_rt::AverInt,
) -> aver_rt::AverMap<AverStr, aver_rt::AverInt> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fns, [] => { return acc; }, [f, rest] => { {
            let __tco0 = rest;
            let __tco1 = acc.insert_owned(f.name, idx.clone());
            let __tco2 = idx.add(&aver_rt::AverInt::from_i64(1));
            fns = __tco0;
            acc = __tco1;
            idx = __tco2;
            continue;
        } })
    }
}

/// Transform ExprCall→ExprCallDirect for known functions in all fn bodies.
#[inline(always)]
pub fn resolveCallsInFns(
    mut fns @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    fnMap @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    let fnMap @ _ = aver_rt::Shared::new(fnMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fns, [] => { return acc.reverse(); }, [f, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::calls::resolveCallsInFn(&f, &*fnMap), &acc);
            fns = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Transform calls in one function body.
pub fn resolveCallsInFn(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::ast::FnDef {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: crate::aver_generated::domain::resolver::calls::resolveCallsInStmts__collected(
            fd.body.clone(),
            fnMap.clone(),
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
        ),
        slotCount: fd.slotCount.clone(),
        slotMap: fd.slotMap.clone(),
        fastPath: fd.fastPath.clone(),
        tailLoop: fd.tailLoop,
    }
}

/// Resolve calls in a list of statements.
#[inline(always)]
pub fn resolveCallsInStmts(
    mut stmts @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    fnMap @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    let fnMap @ _ = aver_rt::Shared::new(fnMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return acc.reverse(); }, [s, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::calls::resolveCallsInStmt(&s, &*fnMap), &acc);
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
            let expr = expr.clone();
            crate::aver_generated::domain::ast::Stmt::StmtBind(
                name,
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(expr, fnMap),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, expr) => {
            let slot = slot.clone();
            let expr = expr.clone();
            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(
                slot,
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(expr, fnMap),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            let expr = expr.clone();
            crate::aver_generated::domain::ast::Stmt::StmtExpr(
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(expr, fnMap),
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
                let cond = __cond_fields.0.clone();
                let thenExpr = __cond_fields.1.clone();
                let elseExpr = __cond_fields.2.clone();
                crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                    crate::aver_generated::domain::ast::Expr__ExprBoolBranch(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            cond, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            thenExpr, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            elseExpr, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCall(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = __name_fields.1.clone();
                crate::aver_generated::domain::resolver::calls::resolveOneCall(name, &args, fnMap)
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(__fnId_fields) => {
                let fnId = __fnId_fields.0.clone();
                let args = __fnId_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprCallDirect(crate::aver_generated::domain::ast::Expr__ExprCallDirect(fnId, crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(args, fnMap.clone(), aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)))))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = __name_fields.1.clone();
                match crate::aver_generated::domain::ast::builtinNameToId(name.clone()) { Some(id @ _) => { crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(crate::aver_generated::domain::ast::Expr__ExprCallBuiltinId(id, crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(args, fnMap.clone(), aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))))) }, None => { crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(name, crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(args, fnMap.clone(), aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))))) } }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltinId(__id_fields) => {
                let id = __id_fields.0.clone();
                let args = __id_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(crate::aver_generated::domain::ast::Expr__ExprCallBuiltinId(id, crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(args, fnMap.clone(), aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)))))
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
                let vecExpr = __vecExpr_fields.0.clone();
                let idxExpr = __vecExpr_fields.1.clone();
                let defaultValue = __vecExpr_fields.2.clone();
                crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                    crate::aver_generated::domain::ast::Expr__ExprVectorGetOrInt(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            vecExpr, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            idxExpr, fnMap,
                        ),
                        defaultValue,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIntModOrInt(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                let defaultValue = __a_fields.2.clone();
                crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                    crate::aver_generated::domain::ast::Expr__ExprIntModOrInt(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                        defaultValue,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprAdd(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprAdd(
                    crate::aver_generated::domain::ast::Expr__ExprAdd(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSub(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprSub(
                    crate::aver_generated::domain::ast::Expr__ExprSub(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMul(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprMul(
                    crate::aver_generated::domain::ast::Expr__ExprMul(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprDiv(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprDiv(
                    crate::aver_generated::domain::ast::Expr__ExprDiv(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeg(__inner_fields) => {
                let inner = __inner_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprNeg(
                    crate::aver_generated::domain::ast::Expr__ExprNeg(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            inner, fnMap,
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
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprEq(
                    crate::aver_generated::domain::ast::Expr__ExprEq(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeq(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprNeq(
                    crate::aver_generated::domain::ast::Expr__ExprNeq(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprLt(
                    crate::aver_generated::domain::ast::Expr__ExprLt(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprGt(
                    crate::aver_generated::domain::ast::Expr__ExprGt(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLte(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprLte(
                    crate::aver_generated::domain::ast::Expr__ExprLte(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGte(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprGte(
                    crate::aver_generated::domain::ast::Expr__ExprGte(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            a, fnMap,
                        ),
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            b, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMatch(__subj_fields) => {
                let subj = __subj_fields.0.clone();
                let arms = __subj_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprMatch(crate::aver_generated::domain::ast::Expr__ExprMatch(crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(subj, fnMap), crate::aver_generated::domain::resolver::calls::resolveCallsInArms__collected(arms, fnMap.clone(), aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)))))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprPropagate(__inner_fields) => {
                let inner = __inner_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprPropagate(
                    crate::aver_generated::domain::ast::Expr__ExprPropagate(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            inner, fnMap,
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprConcat(__parts_fields) => {
                let parts = __parts_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprConcat(crate::aver_generated::domain::ast::Expr__ExprConcat(crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(parts, fnMap.clone(), aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)))))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprTuple(__exprs_fields) => {
                let exprs = __exprs_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprTuple(crate::aver_generated::domain::ast::Expr__ExprTuple(crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(exprs, fnMap.clone(), aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)))))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIndependentProduct(
                __exprs_fields,
            ) => {
                let exprs = __exprs_fields.0.clone();
                let unwrap = __exprs_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(crate::aver_generated::domain::ast::Expr__ExprIndependentProduct(crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(exprs, fnMap.clone(), aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))), unwrap))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprList(__exprs_fields) => {
                let exprs = __exprs_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprList(crate::aver_generated::domain::ast::Expr__ExprList(crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(exprs, fnMap.clone(), aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)))))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprRecord(__name_fields) => {
                let name = __name_fields.0.clone();
                let fields = __name_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprRecord(crate::aver_generated::domain::ast::Expr__ExprRecord(name, crate::aver_generated::domain::resolver::calls::resolveCallsInFields__collected(fields, fnMap.clone(), aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)))))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(__obj_fields) => {
                let obj = __obj_fields.0.clone();
                let field = __obj_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
                    crate::aver_generated::domain::ast::Expr__ExprFieldAccess(
                        crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                            obj, fnMap,
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
    let resolvedArgs @ _ =
        crate::aver_generated::domain::resolver::calls::resolveCallsInExprs__collected(
            args.clone(),
            fnMap.clone(),
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
        );
    match fnMap.get(&name).cloned() {
        Some(fnId @ _) => crate::aver_generated::domain::ast::Expr::ExprCallDirect(
            crate::aver_generated::domain::ast::Expr__ExprCallDirect(fnId, resolvedArgs),
        ),
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
    if crate::aver_generated::domain::resolver::calls::isLiteralDivisorCall(
        name.clone(),
        resolvedArgs,
    ) {
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                AverStr::from("Result.withDefault"),
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
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> bool {
    crate::cancel_checkpoint();
    if (((&*name == "Int.div") || (&*name == "Int.mod"))
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
        let __list_subject = args.clone();
        if let Some((_, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((divisor, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::calls::isNonzeroIntLiteral(&divisor)
                } else {
                    false
                }
            }
        } else {
            false
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
                let inner = __inner_fields.0.clone();
                crate::aver_generated::domain::resolver::calls::isBareNonzeroIntLiteral(&inner)
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
    crate::aver_generated::domain::resolver::calls::isBuiltinCallNameFrom(
        name,
        aver_rt::AverList::from_vec(vec![
            AverStr::from("Args.get"),
            AverStr::from("Bool.and"),
            AverStr::from("Bool.not"),
            AverStr::from("Bool.or"),
            AverStr::from("String.fromCodePoint"),
            AverStr::from("String.firstCodePoint"),
            AverStr::from("Console.error"),
            AverStr::from("Console.print"),
            AverStr::from("Console.readLine"),
            AverStr::from("Console.warn"),
            AverStr::from("Disk.appendText"),
            AverStr::from("Disk.delete"),
            AverStr::from("Disk.deleteDir"),
            AverStr::from("Disk.exists"),
            AverStr::from("Disk.listDir"),
            AverStr::from("Disk.makeDir"),
            AverStr::from("Disk.readText"),
            AverStr::from("Disk.writeText"),
            AverStr::from("Env.get"),
            AverStr::from("Env.set"),
            AverStr::from("Float.abs"),
            AverStr::from("Float.atan2"),
            AverStr::from("Float.ceil"),
            AverStr::from("Float.cos"),
            AverStr::from("Float.floor"),
            AverStr::from("Float.fromInt"),
            AverStr::from("Float.fromString"),
            AverStr::from("Float.max"),
            AverStr::from("Float.min"),
            AverStr::from("Float.pi"),
            AverStr::from("Float.pow"),
            AverStr::from("Float.round"),
            AverStr::from("Float.sin"),
            AverStr::from("Float.sqrt"),
            AverStr::from("String.fromFloat"),
            AverStr::from("Http.delete"),
            AverStr::from("Http.get"),
            AverStr::from("Http.head"),
            AverStr::from("Http.patch"),
            AverStr::from("Http.post"),
            AverStr::from("Http.put"),
            AverStr::from("Int.abs"),
            AverStr::from("Int.div"),
            AverStr::from("Int.fromString"),
            AverStr::from("Int.toBigEndian"),
            AverStr::from("Int.toLittleEndian"),
            AverStr::from("Int.fromBigEndian"),
            AverStr::from("Int.fromLittleEndian"),
            AverStr::from("Int.max"),
            AverStr::from("Int.min"),
            AverStr::from("Int.mod"),
            AverStr::from("Float.fromInt"),
            AverStr::from("String.fromInt"),
            AverStr::from("List.concat"),
            AverStr::from("List.contains"),
            AverStr::from("List.drop"),
            AverStr::from("List.head"),
            AverStr::from("List.len"),
            AverStr::from("List.prepend"),
            AverStr::from("List.reverse"),
            AverStr::from("List.take"),
            AverStr::from("List.tail"),
            AverStr::from("List.zip"),
            AverStr::from("Map.entries"),
            AverStr::from("Map.fromList"),
            AverStr::from("Map.get"),
            AverStr::from("Map.has"),
            AverStr::from("Map.keys"),
            AverStr::from("Map.remove"),
            AverStr::from("Map.set"),
            AverStr::from("Map.size"),
            AverStr::from("Map.values"),
            AverStr::from("Option.None"),
            AverStr::from("Option.Some"),
            AverStr::from("Result.fromOption"),
            AverStr::from("Option.withDefault"),
            AverStr::from("Random.int"),
            AverStr::from("Result.Err"),
            AverStr::from("Result.Ok"),
            AverStr::from("Result.withDefault"),
            AverStr::from("String.charAt"),
            AverStr::from("String.chars"),
            AverStr::from("String.contains"),
            AverStr::from("String.endsWith"),
            AverStr::from("String.fromBool"),
            AverStr::from("String.fromFloat"),
            AverStr::from("String.fromInt"),
            AverStr::from("String.join"),
            AverStr::from("String.len"),
            AverStr::from("String.repeat"),
            AverStr::from("String.replace"),
            AverStr::from("String.slice"),
            AverStr::from("String.split"),
            AverStr::from("String.startsWith"),
            AverStr::from("String.toLower"),
            AverStr::from("String.toUpper"),
            AverStr::from("String.trim"),
            AverStr::from("Tcp.close"),
            AverStr::from("Tcp.closeDial"),
            AverStr::from("Tcp.closeListener"),
            AverStr::from("Tcp.connect"),
            AverStr::from("Tcp.beginConnect"),
            AverStr::from("Tcp.dialled"),
            AverStr::from("Tcp.listen"),
            AverStr::from("Tcp.accept"),
            AverStr::from("Tcp.peerAddress"),
            AverStr::from("Tcp.ping"),
            AverStr::from("Tcp.poll"),
            AverStr::from("Tcp.readLine"),
            AverStr::from("Tcp.send"),
            AverStr::from("Tcp.writeLine"),
            AverStr::from("Terminal.clear"),
            AverStr::from("Terminal.disableRawMode"),
            AverStr::from("Terminal.enableRawMode"),
            AverStr::from("Terminal.flush"),
            AverStr::from("Terminal.hideCursor"),
            AverStr::from("Terminal.moveTo"),
            AverStr::from("Terminal.print"),
            AverStr::from("Terminal.readKey"),
            AverStr::from("Terminal.resetColor"),
            AverStr::from("Terminal.setColor"),
            AverStr::from("Terminal.showCursor"),
            AverStr::from("Terminal.size"),
            AverStr::from("Time.now"),
            AverStr::from("Time.sleep"),
            AverStr::from("Time.unixMs"),
            AverStr::from("Vector.fromList"),
            AverStr::from("Vector.get"),
            AverStr::from("Vector.len"),
            AverStr::from("Vector.new"),
            AverStr::from("Vector.set"),
            AverStr::from("List.fromVector"),
        ]),
    )
}

/// Check builtin/service call names recursively by exact match.
#[inline(always)]
pub fn isBuiltinCallNameFrom(
    mut name @ _: AverStr,
    mut knownNames @ _: aver_rt::AverList<AverStr>,
) -> bool {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(knownNames, [] => { return false; }, [knownName, rest] => { if (name == knownName) { return true; } else { {
            let __tco1 = rest;
            knownNames = __tco1;
            continue;
        } } })
    }
}

/// Resolve calls in a list of expressions.
#[inline(always)]
pub fn resolveCallsInExprs(
    mut exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    fnMap @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    let fnMap @ _ = aver_rt::Shared::new(fnMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return acc.reverse(); }, [e, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(e, &*fnMap), &acc);
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
    fnMap @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    let fnMap @ _ = aver_rt::Shared::new(fnMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(arms, [] => { return acc.reverse(); }, [arm, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::ast::MatchArm { pattern: arm.pattern, body: crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(arm.body, &*fnMap), bindingSlots: arm.bindingSlots }, &acc);
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
    fnMap @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    let fnMap @ _ = aver_rt::Shared::new(fnMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return acc.reverse(); }, [pair, rest] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend((name, crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(expr, &*fnMap)), &acc);
            fields = __tco0;
            acc = __tco2;
            continue;
        } } })
    }
}

/// Synthesized collecting variant of `resolveCallsInStmts`. Appends to a builder where `resolveCallsInStmts` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveCallsInStmts__collected(
    mut stmts @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    fnMap @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    let fnMap @ _ = aver_rt::Shared::new(fnMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return aver_rt::list_builder_finalize(acc); }, [s, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::resolver::calls::resolveCallsInStmt(&s, &*fnMap));
            stmts = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `resolveCallsInExprs`. Appends to a builder where `resolveCallsInExprs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveCallsInExprs__collected(
    mut exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    fnMap @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    let fnMap @ _ = aver_rt::Shared::new(fnMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return aver_rt::list_builder_finalize(acc); }, [e, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(e, &*fnMap));
            exprs = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `resolveCallsInArms`. Appends to a builder where `resolveCallsInArms` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveCallsInArms__collected(
    mut arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    fnMap @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    let fnMap @ _ = aver_rt::Shared::new(fnMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(arms, [] => { return aver_rt::list_builder_finalize(acc); }, [arm, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::ast::MatchArm { pattern: arm.pattern, body: crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(arm.body, &*fnMap), bindingSlots: arm.bindingSlots });
            arms = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `resolveCallsInFields`. Appends to a builder where `resolveCallsInFields` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveCallsInFields__collected(
    mut fields @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    fnMap @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    let fnMap @ _ = aver_rt::Shared::new(fnMap);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return aver_rt::list_builder_finalize(acc); }, [pair, rest] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, (name, crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(expr, &*fnMap)));
            fields = __tco0;
            acc = __tco2;
            continue;
        } } })
    }
}
