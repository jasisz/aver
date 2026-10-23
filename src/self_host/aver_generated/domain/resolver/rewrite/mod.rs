#[allow(unused_imports)]
use crate::*;

/// Rewrite resolved AST into lighter internal expression shapes without changing semantics.
#[inline(always)]
pub fn rewriteInternalFns(
    mut fns @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    loop {
        crate::cancel_checkpoint();
        aver_rt::aver_list_match_reuse!(fns, [] => { return acc.reverse(); }, [f, rest, __list_slot] => { {
            let __tco0 = rest;
            let __tco1 = __list_slot.prepend(crate::aver_generated::domain::resolver::rewrite::rewriteInternalFn(f), acc);
            fns = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Rewrite the body of one resolved function into lighter internal expression shapes.
pub fn rewriteInternalFn(
    mut fd @ _: crate::aver_generated::domain::ast::FnDef,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    Box::new(crate::aver_generated::domain::ast::FnDef__Fields {
        name: fd.name,
        params: fd.params,
        body: {
            let __arg1 @ _ =
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalStmts__collected(
                &fd.body, __arg1,
            )
        },
        slotCount: fd.slotCount,
        slotMap: fd.slotMap,
        fastPath: fd.fastPath,
        tailLoop: fd.tailLoop,
    })
}

/// Rewrite statements recursively into lighter internal expression shapes.
#[inline(always)]
pub fn rewriteInternalStmts(
    stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = stmts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = stmts;
            if __list_subject.is_empty() {
                return acc.reverse();
            } else {
                if let (Some(stmt), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::AverList::prepend_owned(
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalStmt(
                                stmt,
                            ),
                            acc,
                        );
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

/// Rewrite one statement recursively into lighter internal expression shapes.
pub fn rewriteInternalStmt(
    stmt @ _: &crate::aver_generated::domain::ast::Stmt,
) -> crate::aver_generated::domain::ast::Stmt {
    crate::cancel_checkpoint();
    match stmt {
        crate::aver_generated::domain::ast::Stmt::StmtBind(name, expr) => {
            let name = name.clone();
            crate::aver_generated::domain::ast::Stmt::StmtBind(
                name,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr.clone()),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, expr) => {
            let slot = slot.clone();
            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(
                slot,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr.clone()),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtExpr(
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr.clone()),
            )
        }
    }
}

/// Rewrite one expression into lighter internal forms after resolve and direct-call linking.
pub fn rewriteInternalExpr(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
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
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            cond.clone(),
                        ),
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            thenExpr.clone(),
                        ),
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            elseExpr.clone(),
                        ),
                    ),
                )
            }
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
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            vecExpr.clone(),
                        ),
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            idxExpr.clone(),
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
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            a.clone(),
                        ),
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            b.clone(),
                        ),
                        defaultValue,
                    ),
                )
            }
            _ => {
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprAfterLeaf(expr)
            }
        }
    }
}

/// Continue internal rewrite for arithmetic and comparison forms.
pub fn rewriteInternalExprAfterLeaf(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprAdd(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg22 @ _ = crate::aver_generated::domain::ast::BinOp::OpAdd;
                    {
                        let __arg23 @ _ =
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                a.clone(),
                            );
                        {
                            let __arg24 @ _ = crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b.clone());
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                                __arg22, __arg23, __arg24,
                            )
                        }
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSub(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg25 @ _ = crate::aver_generated::domain::ast::BinOp::OpSub;
                    {
                        let __arg26 @ _ =
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                a.clone(),
                            );
                        {
                            let __arg27 @ _ = crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b.clone());
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                                __arg25, __arg26, __arg27,
                            )
                        }
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMul(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg28 @ _ = crate::aver_generated::domain::ast::BinOp::OpMul;
                    {
                        let __arg29 @ _ =
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                a.clone(),
                            );
                        {
                            let __arg30 @ _ = crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b.clone());
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                                __arg28, __arg29, __arg30,
                            )
                        }
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprDiv(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg31 @ _ = crate::aver_generated::domain::ast::BinOp::OpDiv;
                    {
                        let __arg32 @ _ =
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                a.clone(),
                            );
                        {
                            let __arg33 @ _ = crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b.clone());
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                                __arg31, __arg32, __arg33,
                            )
                        }
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeg(__inner_fields) => {
                let inner = &__inner_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprNeg(
                    crate::aver_generated::domain::ast::Expr__ExprNeg(
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            inner.clone(),
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprEq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg34 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpEq;
                    {
                        let __arg35 @ _ =
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                a.clone(),
                            );
                        {
                            let __arg36 @ _ = crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b.clone());
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                                __arg34, __arg35, __arg36,
                            )
                        }
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg37 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpNeq;
                    {
                        let __arg38 @ _ =
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                a.clone(),
                            );
                        {
                            let __arg39 @ _ = crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b.clone());
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                                __arg37, __arg38, __arg39,
                            )
                        }
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg40 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpLt;
                    {
                        let __arg41 @ _ =
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                a.clone(),
                            );
                        {
                            let __arg42 @ _ = crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b.clone());
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                                __arg40, __arg41, __arg42,
                            )
                        }
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg43 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpGt;
                    {
                        let __arg44 @ _ =
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                a.clone(),
                            );
                        {
                            let __arg45 @ _ = crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b.clone());
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                                __arg43, __arg44, __arg45,
                            )
                        }
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg46 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpLte;
                    {
                        let __arg47 @ _ =
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                a.clone(),
                            );
                        {
                            let __arg48 @ _ = crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b.clone());
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                                __arg46, __arg47, __arg48,
                            )
                        }
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg49 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpGte;
                    {
                        let __arg50 @ _ =
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                a.clone(),
                            );
                        {
                            let __arg51 @ _ = crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b.clone());
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                                __arg49, __arg50, __arg51,
                            )
                        }
                    }
                }
            }
            _ => crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprAfterArith(
                expr,
            ),
        }
    }
}

/// Finish internal rewrite for aggregates, calls, and products.
pub fn rewriteInternalExprAfterArith(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprMatch(__scrutinee_fields) => {
                let scrutinee = &__scrutinee_fields.0;
                let arms = &__scrutinee_fields.1;
                {
                    let __arg23 @ _ =
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            scrutinee.clone(),
                        );
                    {
                        let __arg24 @ _ = {
                            let __arg22 @ _ = aver_rt::list_builder_new(
                                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                            );
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalArms__collected(arms, __arg22)
                        };
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalMatch(
                            __arg23, __arg24,
                        )
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprPropagate(__inner_fields) => {
                let inner = &__inner_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprPropagate(
                    crate::aver_generated::domain::ast::Expr__ExprPropagate(
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            inner.clone(),
                        ),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprConcat(__parts_fields) => {
                let parts = &__parts_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprConcat(
                    crate::aver_generated::domain::ast::Expr__ExprConcat({
                        let __arg25 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(parts, __arg25)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprTuple(__exprs_fields) => {
                let exprs = &__exprs_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprTuple(
                    crate::aver_generated::domain::ast::Expr__ExprTuple({
                        let __arg26 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(exprs, __arg26)
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
                            let __arg27 @ _ = aver_rt::list_builder_new(
                                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                            );
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(exprs, __arg27)
                        },
                        unwrap,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprList(__exprs_fields) => {
                let exprs = &__exprs_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprList(
                    crate::aver_generated::domain::ast::Expr__ExprList({
                        let __arg28 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(exprs, __arg28)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprRecord(__name_fields) => {
                let name = __name_fields.0.clone();
                let fields = &__name_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprRecord(
                    crate::aver_generated::domain::ast::Expr__ExprRecord(name, {
                        let __arg29 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalFields__collected(fields, __arg29)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(__obj_fields) => {
                let obj = &__obj_fields.0;
                let field = __obj_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
                    crate::aver_generated::domain::ast::Expr__ExprFieldAccess(
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                            obj.clone(),
                        ),
                        field,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprVar(__name_fields) => {
                let name = __name_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprVar(
                    crate::aver_generated::domain::ast::Expr__ExprVar(
                        crate::aver_generated::domain::ast::canonicalCtorName(name),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCall(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = &__name_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprCall(
                    crate::aver_generated::domain::ast::Expr__ExprCall(
                        crate::aver_generated::domain::ast::canonicalCtorName(name),
                        {
                            let __arg30 @ _ = aver_rt::list_builder_new(
                                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                            );
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(args, __arg30)
                        },
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(__fnId_fields) => {
                let fnId = __fnId_fields.0.clone();
                let args = &__fnId_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprCallDirect(
                    crate::aver_generated::domain::ast::Expr__ExprCallDirect(fnId, {
                        let __arg31 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(args, __arg31)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = &__name_fields.1;
                {
                    let __arg33 @ _ = {
                        let __arg32 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(args, __arg32)
                    };
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalBuiltin(
                        name, __arg33,
                    )
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltinId(__id_fields) => {
                let id = __id_fields.0.clone();
                let args = &__id_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltinId(id, {
                        let __arg34 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(args, __arg34)
                    }),
                )
            }
            _ => expr,
        }
    }
}

/// Rewrite a list of expressions recursively.
#[inline(always)]
pub fn rewriteInternalExprs(
    mut exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    loop {
        crate::cancel_checkpoint();
        aver_rt::aver_list_match_reuse!(exprs, [] => { return acc.reverse(); }, [expr, rest, __list_slot] => { {
            let __tco0 = rest;
            let __tco1 = __list_slot.prepend(crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr), acc);
            exprs = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Rewrite record field expressions recursively.
#[inline(always)]
pub fn rewriteInternalFields(
    mut fields @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    loop {
        crate::cancel_checkpoint();
        aver_rt::aver_list_match_reuse!(fields, [] => { return acc.reverse(); }, [pair, rest, __list_slot] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco1 = __list_slot.prepend((name, crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr)), acc);
            fields = __tco0;
            acc = __tco1;
            continue;
        } } })
    }
}

/// Rewrite match arm bodies and resolve PatConstructor to PatConstructorId using constructor tags.
#[inline(always)]
pub fn rewriteInternalArms(
    mut arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    loop {
        crate::cancel_checkpoint();
        aver_rt::aver_list_match_reuse!(arms, [] => { return acc.reverse(); }, [arm, rest, __list_slot] => { {
            let __tco0 = rest;
            let __tco1 = __list_slot.prepend(crate::aver_generated::domain::ast::MatchArm { pattern: crate::aver_generated::domain::resolver::rewrite::rewritePattern(arm.pattern), body: crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(arm.body), bindingSlots: arm.bindingSlots }, acc);
            arms = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Rewrite PatConstructor to PatConstructorId with both tag and full constructor name.
pub fn rewritePattern(
    mut pat @ _: crate::aver_generated::domain::ast::Pattern,
) -> crate::aver_generated::domain::ast::Pattern {
    crate::cancel_checkpoint();
    match &pat {
        crate::aver_generated::domain::ast::Pattern::PatConstructor(name, bindings) => {
            let name = name.clone();
            crate::aver_generated::domain::resolver::rewrite::rewritePatConstructor(name, bindings)
        }
        crate::aver_generated::domain::ast::Pattern::PatTuple(pats) => {
            crate::aver_generated::domain::ast::Pattern::PatTuple({
                let __arg4 @ _ = aver_rt::list_builder_new(
                    (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                );
                crate::aver_generated::domain::resolver::rewrite::rewritePatterns__collected(
                    pats, __arg4,
                )
            })
        }
        _ => pat,
    }
}

/// Convert PatConstructor to PatConstructorId with stable tag and canonical constructor name, so a pattern that names the declaring module matches values the module itself built.
pub fn rewritePatConstructor(
    name @ _: AverStr,
    bindings @ _: &aver_rt::AverList<AverStr>,
) -> crate::aver_generated::domain::ast::Pattern {
    crate::cancel_checkpoint();
    let canonical @ _ = crate::aver_generated::domain::ast::canonicalCtorName(name);
    let tag @ _ = crate::aver_generated::domain::ast::ctorNameToTag(canonical.clone());
    crate::aver_generated::domain::ast::Pattern::PatConstructorId(tag, canonical, bindings.clone())
}

/// Rewrite a list of patterns.
#[inline(always)]
pub fn rewritePatterns(
    mut pats @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Pattern> {
    loop {
        crate::cancel_checkpoint();
        aver_rt::aver_list_match_reuse!(pats, [] => { return acc.reverse(); }, [p, rest, __list_slot] => { {
            let __tco0 = rest;
            let __tco1 = __list_slot.prepend(crate::aver_generated::domain::resolver::rewrite::rewritePattern(p), acc);
            pats = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Compress simple resolved arithmetic into slot-based internal nodes.
pub fn rewriteInternalBinop(
    mut op @ _: crate::aver_generated::domain::ast::BinOp,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    mut right @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &left;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinopSlotLeft(
                    op, slot, right,
                )
            }
            _ => {
                let __t = &right;
                match __t.view() {
                    crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                        let slot = __slot_fields.0.clone();
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinopSlotRight(op, left, slot)
                    }
                    _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
                        &op, left, right,
                    ),
                }
            }
        }
    }
}

/// Rewrite arithmetic with a slot on the left when the right side is simple.
pub fn rewriteInternalBinopSlotLeft(
    mut op @ _: crate::aver_generated::domain::ast::BinOp,
    slot @ _: aver_rt::AverInt,
    mut right @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &right;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__n_fields) => {
                let n = __n_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(
                    crate::aver_generated::domain::ast::Expr__ExprBinopSlotInt(op, slot, n),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__rhs_fields) => {
                let rhs = __rhs_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprBinopSlots(
                    crate::aver_generated::domain::ast::Expr__ExprBinopSlots(op, slot, rhs),
                )
            }
            _ => {
                let __arg5 @ _ = crate::aver_generated::domain::ast::Expr::ExprSlot(
                    crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
                );
                crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
                    &op, __arg5, right,
                )
            }
        }
    }
}

/// Rewrite commutative arithmetic when the slot appears on the right.
pub fn rewriteInternalBinopSlotRight(
    mut op @ _: crate::aver_generated::domain::ast::BinOp,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    slot @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &left;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__lhs_fields) => {
                let lhs = __lhs_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprBinopSlots(
                    crate::aver_generated::domain::ast::Expr__ExprBinopSlots(op, lhs, slot),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__n_fields) => {
                let n = __n_fields.0.clone();
                if crate::aver_generated::domain::resolver::rewrite::binopCanFlip(&op) {
                    crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(
                        crate::aver_generated::domain::ast::Expr__ExprBinopSlotInt(op, slot, n),
                    )
                } else {
                    {
                        let __arg5 @ _ = crate::aver_generated::domain::ast::Expr::ExprSlot(
                            crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
                        );
                        crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
                            &op, left, __arg5,
                        )
                    }
                }
            }
            _ => {
                let __arg6 @ _ = crate::aver_generated::domain::ast::Expr::ExprSlot(
                    crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
                );
                crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
                    &op, left, __arg6,
                )
            }
        }
    }
}

/// Rebuild a generic arithmetic expression when no narrower internal form applies.
pub fn rebuildInternalBinop(
    op @ _: &crate::aver_generated::domain::ast::BinOp,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    mut right @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::BinOp::OpAdd => {
            crate::aver_generated::domain::ast::Expr::ExprAdd(
                crate::aver_generated::domain::ast::Expr__ExprAdd(left, right),
            )
        }
        crate::aver_generated::domain::ast::BinOp::OpSub => {
            crate::aver_generated::domain::ast::Expr::ExprSub(
                crate::aver_generated::domain::ast::Expr__ExprSub(left, right),
            )
        }
        crate::aver_generated::domain::ast::BinOp::OpMul => {
            crate::aver_generated::domain::ast::Expr::ExprMul(
                crate::aver_generated::domain::ast::Expr__ExprMul(left, right),
            )
        }
        crate::aver_generated::domain::ast::BinOp::OpDiv => {
            crate::aver_generated::domain::ast::Expr::ExprDiv(
                crate::aver_generated::domain::ast::Expr__ExprDiv(left, right),
            )
        }
    }
}

/// Return whether swapping operand order preserves arithmetic semantics.
pub fn binopCanFlip(op @ _: &crate::aver_generated::domain::ast::BinOp) -> bool {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::BinOp::OpAdd => true,
        crate::aver_generated::domain::ast::BinOp::OpMul => true,
        _ => false,
    }
}

/// Recognize fused builtin wrapper patterns after children have already been rewritten.
#[inline(always)]
pub fn rewriteInternalBuiltin(
    name @ _: AverStr,
    mut args @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Option.withDefault" {
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalOptionWithDefault(args)
        } else {
            if &*__dispatch_subject == "Result.withDefault" {
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalResultWithDefault(
                    args,
                )
            } else {
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(name, args),
                )
            }
        }
    }
}

/// Rewrite Option.withDefault(Vector.get(...), int) into one internal node.
pub fn rewriteInternalOptionWithDefault(
    mut args @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((optionExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let (Some(defaultExpr), Some(ignored)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalOptionWithDefaultArgs(optionExpr, defaultExpr.clone())
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                            ::aver_rt::str_lit!("Option.withDefault"),
                            args,
                        ),
                    )
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    ::aver_rt::str_lit!("Option.withDefault"),
                    args,
                ),
            )
        }
    }
}

/// Recognize a fused Vector.get-with-default shape.
pub fn rewriteInternalOptionWithDefaultArgs(
    mut optionExpr @ _: crate::aver_generated::domain::ast::Expr,
    mut defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &defaultExpr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__defaultValue_fields) => {
                let defaultValue = __defaultValue_fields.0.clone();
                {
                    let __t = &optionExpr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(
                            __name_fields,
                        ) => {
                            let name = __name_fields.0.clone();
                            let innerArgs = __name_fields.1.clone();
                            if (&*name == "Vector.get") {
                                crate::aver_generated::domain::resolver::rewrite::rewriteInternalVectorGetOrInt(&innerArgs, defaultValue)
                            } else {
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                        ::aver_rt::str_lit!("Option.withDefault"),
                                        aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]),
                                    ),
                                )
                            }
                        }
                        _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                ::aver_rt::str_lit!("Option.withDefault"),
                                aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]),
                            ),
                        ),
                    }
                }
            }
            _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    ::aver_rt::str_lit!("Option.withDefault"),
                    aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]),
                ),
            ),
        }
    }
}

/// Fuse Vector.get(vec, idx) with an integer fallback.
pub fn rewriteInternalVectorGetOrInt(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultValue @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let Some((vecExpr, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
            {
                let __list_subject = rest;
                if let (Some(idxExpr), Some(ignored)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalVectorGetOrIntArgs(vecExpr, idxExpr.clone(), defaultValue)
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                            ::aver_rt::str_lit!("Option.withDefault"),
                            aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                        ::aver_rt::str_lit!("Vector.get"),
                                        args.clone(),
                                    ),
                                ),
                                crate::aver_generated::domain::ast::Expr::ExprInt(
                                    crate::aver_generated::domain::ast::Expr__ExprInt(defaultValue),
                                ),
                            ]),
                        ),
                    )
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    ::aver_rt::str_lit!("Option.withDefault"),
                    aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                ::aver_rt::str_lit!("Vector.get"),
                                args.clone(),
                            ),
                        ),
                        crate::aver_generated::domain::ast::Expr::ExprInt(
                            crate::aver_generated::domain::ast::Expr__ExprInt(defaultValue),
                        ),
                    ]),
                ),
            )
        }
    }
}

/// Only fuse Vector.get-with-default when at least one operand is already slot-shaped.
#[inline(always)]
pub fn rewriteInternalVectorGetOrIntArgs(
    mut vecExpr @ _: crate::aver_generated::domain::ast::Expr,
    mut idxExpr @ _: crate::aver_generated::domain::ast::Expr,
    defaultValue @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::resolver::rewrite::exprHasSlotShape(&vecExpr) {
        crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
            crate::aver_generated::domain::ast::Expr__ExprVectorGetOrInt(
                vecExpr,
                idxExpr,
                defaultValue,
            ),
        )
    } else {
        if crate::aver_generated::domain::resolver::rewrite::exprHasSlotShape(&idxExpr) {
            crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                crate::aver_generated::domain::ast::Expr__ExprVectorGetOrInt(
                    vecExpr,
                    idxExpr,
                    defaultValue,
                ),
            )
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    ::aver_rt::str_lit!("Option.withDefault"),
                    aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                ::aver_rt::str_lit!("Vector.get"),
                                aver_rt::AverList::from_vec(vec![vecExpr, idxExpr]),
                            ),
                        ),
                        crate::aver_generated::domain::ast::Expr::ExprInt(
                            crate::aver_generated::domain::ast::Expr__ExprInt(defaultValue),
                        ),
                    ]),
                ),
            )
        }
    }
}

/// Rewrite Result.withDefault(Int.mod(...), int) into one internal node.
pub fn rewriteInternalResultWithDefault(
    mut args @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((resultExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let (Some(defaultExpr), Some(ignored)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalResultWithDefaultArgs(resultExpr, defaultExpr.clone())
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                            ::aver_rt::str_lit!("Result.withDefault"),
                            args,
                        ),
                    )
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    ::aver_rt::str_lit!("Result.withDefault"),
                    args,
                ),
            )
        }
    }
}

/// Recognize a fused Int.mod-with-default shape.
pub fn rewriteInternalResultWithDefaultArgs(
    mut resultExpr @ _: crate::aver_generated::domain::ast::Expr,
    mut defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &defaultExpr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__defaultValue_fields) => {
                let defaultValue = __defaultValue_fields.0.clone();
                {
                    let __t = &resultExpr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(
                            __name_fields,
                        ) => {
                            let name = __name_fields.0.clone();
                            let innerArgs = __name_fields.1.clone();
                            if (&*name == "Int.mod") {
                                crate::aver_generated::domain::resolver::rewrite::rewriteInternalIntModOrInt(&innerArgs, defaultValue)
                            } else {
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                        ::aver_rt::str_lit!("Result.withDefault"),
                                        aver_rt::AverList::from_vec(vec![resultExpr, defaultExpr]),
                                    ),
                                )
                            }
                        }
                        _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                ::aver_rt::str_lit!("Result.withDefault"),
                                aver_rt::AverList::from_vec(vec![resultExpr, defaultExpr]),
                            ),
                        ),
                    }
                }
            }
            _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    ::aver_rt::str_lit!("Result.withDefault"),
                    aver_rt::AverList::from_vec(vec![resultExpr, defaultExpr]),
                ),
            ),
        }
    }
}

/// Fuse Int.mod(a, b) with an integer fallback when the operands are slot-shaped.
pub fn rewriteInternalIntModOrInt(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultValue @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let Some((a, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
            {
                let __list_subject = rest;
                if let (Some(b), Some(ignored)) = (__list_subject.first(), __list_subject.tail()) {
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalIntModOrIntArgs(
                        a,
                        b.clone(),
                        defaultValue,
                    )
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                            ::aver_rt::str_lit!("Result.withDefault"),
                            aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                        ::aver_rt::str_lit!("Int.mod"),
                                        args.clone(),
                                    ),
                                ),
                                crate::aver_generated::domain::ast::Expr::ExprInt(
                                    crate::aver_generated::domain::ast::Expr__ExprInt(defaultValue),
                                ),
                            ]),
                        ),
                    )
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    ::aver_rt::str_lit!("Result.withDefault"),
                    aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                ::aver_rt::str_lit!("Int.mod"),
                                args.clone(),
                            ),
                        ),
                        crate::aver_generated::domain::ast::Expr::ExprInt(
                            crate::aver_generated::domain::ast::Expr__ExprInt(defaultValue),
                        ),
                    ]),
                ),
            )
        }
    }
}

/// Only fuse Int.mod-with-default when at least one operand is already slot-shaped.
#[inline(always)]
pub fn rewriteInternalIntModOrIntArgs(
    mut a @ _: crate::aver_generated::domain::ast::Expr,
    mut b @ _: crate::aver_generated::domain::ast::Expr,
    defaultValue @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::resolver::rewrite::exprHasSlotShape(&a) {
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
            crate::aver_generated::domain::ast::Expr__ExprIntModOrInt(a, b, defaultValue),
        )
    } else {
        if crate::aver_generated::domain::resolver::rewrite::exprHasSlotShape(&b) {
            crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                crate::aver_generated::domain::ast::Expr__ExprIntModOrInt(a, b, defaultValue),
            )
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    ::aver_rt::str_lit!("Result.withDefault"),
                    aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                ::aver_rt::str_lit!("Int.mod"),
                                aver_rt::AverList::from_vec(vec![a, b]),
                            ),
                        ),
                        crate::aver_generated::domain::ast::Expr::ExprInt(
                            crate::aver_generated::domain::ast::Expr__ExprInt(defaultValue),
                        ),
                    ]),
                ),
            )
        }
    }
}

/// Return whether an expression is already in a slot-based internal shape worth fusing around.
pub fn exprHasSlotShape(expr @ _: &crate::aver_generated::domain::ast::Expr) -> bool {
    crate::cancel_checkpoint();
    {
        let __t = expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(_) => true,
            crate::aver_generated::domain::ast::Expr__view::ExprBinopSlotInt(_) => true,
            crate::aver_generated::domain::ast::Expr__view::ExprBinopSlots(_) => true,
            crate::aver_generated::domain::ast::Expr__view::ExprCmpSlotInt(_) => true,
            crate::aver_generated::domain::ast::Expr__view::ExprCmpSlots(_) => true,
            crate::aver_generated::domain::ast::Expr__view::ExprVectorGetOrInt(_) => true,
            crate::aver_generated::domain::ast::Expr__view::ExprIntModOrInt(_) => true,
            _ => false,
        }
    }
}

/// Compress simple resolved comparisons into slot-based internal nodes.
pub fn rewriteInternalCmp(
    mut op @ _: crate::aver_generated::domain::ast::CmpOp,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    mut right @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &left;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmpSlotLeft(
                    op, slot, right,
                )
            }
            _ => {
                let __t = &right;
                match __t.view() {
                    crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                        let slot = __slot_fields.0.clone();
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmpSlotRight(op, left, slot)
                    }
                    _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalCmp(
                        &op, left, right,
                    ),
                }
            }
        }
    }
}

/// Rewrite comparisons with a slot on the left when the right side is simple.
pub fn rewriteInternalCmpSlotLeft(
    mut op @ _: crate::aver_generated::domain::ast::CmpOp,
    slot @ _: aver_rt::AverInt,
    mut right @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &right;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__n_fields) => {
                let n = __n_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(
                    crate::aver_generated::domain::ast::Expr__ExprCmpSlotInt(op, slot, n),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__rhs_fields) => {
                let rhs = __rhs_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprCmpSlots(
                    crate::aver_generated::domain::ast::Expr__ExprCmpSlots(op, slot, rhs),
                )
            }
            _ => {
                let __arg5 @ _ = crate::aver_generated::domain::ast::Expr::ExprSlot(
                    crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
                );
                crate::aver_generated::domain::resolver::rewrite::rebuildInternalCmp(
                    &op, __arg5, right,
                )
            }
        }
    }
}

/// Rewrite comparisons with a slot on the right when the left side is a simple integer.
pub fn rewriteInternalCmpSlotRight(
    mut op @ _: crate::aver_generated::domain::ast::CmpOp,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    slot @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &left;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__n_fields) => {
                let n = __n_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(
                    crate::aver_generated::domain::ast::Expr__ExprCmpSlotInt(
                        crate::aver_generated::domain::resolver::rewrite::flipCmp(&op),
                        slot,
                        n,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__lhs_fields) => {
                let lhs = __lhs_fields.0.clone();
                crate::aver_generated::domain::ast::Expr::ExprCmpSlots(
                    crate::aver_generated::domain::ast::Expr__ExprCmpSlots(op, lhs, slot),
                )
            }
            _ => {
                let __arg5 @ _ = crate::aver_generated::domain::ast::Expr::ExprSlot(
                    crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
                );
                crate::aver_generated::domain::resolver::rewrite::rebuildInternalCmp(
                    &op, left, __arg5,
                )
            }
        }
    }
}

/// Rebuild a generic comparison expression when no narrower internal form applies.
pub fn rebuildInternalCmp(
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    mut right @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            crate::aver_generated::domain::ast::Expr::ExprEq(
                crate::aver_generated::domain::ast::Expr__ExprEq(left, right),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            crate::aver_generated::domain::ast::Expr::ExprNeq(
                crate::aver_generated::domain::ast::Expr__ExprNeq(left, right),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            crate::aver_generated::domain::ast::Expr::ExprLt(
                crate::aver_generated::domain::ast::Expr__ExprLt(left, right),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            crate::aver_generated::domain::ast::Expr::ExprGt(
                crate::aver_generated::domain::ast::Expr__ExprGt(left, right),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            crate::aver_generated::domain::ast::Expr::ExprLte(
                crate::aver_generated::domain::ast::Expr__ExprLte(left, right),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            crate::aver_generated::domain::ast::Expr::ExprGte(
                crate::aver_generated::domain::ast::Expr__ExprGte(left, right),
            )
        }
    }
}

/// Flip a comparison when swapping operand order.
pub fn flipCmp(
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
) -> crate::aver_generated::domain::ast::CmpOp {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            crate::aver_generated::domain::ast::CmpOp::CmpEq
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            crate::aver_generated::domain::ast::CmpOp::CmpNeq
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            crate::aver_generated::domain::ast::CmpOp::CmpGt
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            crate::aver_generated::domain::ast::CmpOp::CmpLt
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            crate::aver_generated::domain::ast::CmpOp::CmpGte
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            crate::aver_generated::domain::ast::CmpOp::CmpLte
        }
    }
}

/// Rewrite simple bool matches into a direct branch node.
#[inline(always)]
pub fn rewriteInternalMatch(
    mut scrutinee @ _: crate::aver_generated::domain::ast::Expr,
    mut arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::resolver::rewrite::rewriteBoolMatchArms(&arms) {
        Some(pair @ _) => {
            let (thenExpr, elseExpr) = pair;
            crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                crate::aver_generated::domain::ast::Expr__ExprBoolBranch(
                    scrutinee, thenExpr, elseExpr,
                ),
            )
        }
        None => crate::aver_generated::domain::ast::Expr::ExprMatch(
            crate::aver_generated::domain::ast::Expr__ExprMatch(scrutinee, arms),
        ),
    }
}

/// Extract (thenExpr, elseExpr) from a two-arm bool match regardless of arm order.
pub fn rewriteBoolMatchArms(
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> Option<(
    crate::aver_generated::domain::ast::Expr,
    crate::aver_generated::domain::ast::Expr,
)> {
    crate::cancel_checkpoint();
    {
        let __list_subject = arms;
        if let (Some(arm1), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(arm2), Some(tail)) = (__list_subject.first(), __list_subject.tail()) {
                    if (tail == aver_rt::AverList::empty()) {
                        crate::aver_generated::domain::resolver::rewrite::rewriteBoolMatchArmPair(
                            arm1.clone(),
                            arm2.clone(),
                        )
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        } else {
            None
        }
    }
}

/// Order two complementary bool arms into (trueExpr, falseExpr).
pub fn rewriteBoolMatchArmPair(
    mut arm1 @ _: crate::aver_generated::domain::ast::MatchArm,
    mut arm2 @ _: crate::aver_generated::domain::ast::MatchArm,
) -> Option<(
    crate::aver_generated::domain::ast::Expr,
    crate::aver_generated::domain::ast::Expr,
)> {
    crate::cancel_checkpoint();
    match arm1.pattern {
        crate::aver_generated::domain::ast::Pattern::PatBool(b1) => {
            crate::aver_generated::domain::resolver::rewrite::rewriteBoolMatchArmPairInner(
                b1,
                arm1.body,
                &arm2.pattern,
                arm2.body,
            )
        }
        _ => None,
    }
}

/// Finish ordering bool arms once the first pattern bool is known.
pub fn rewriteBoolMatchArmPairInner(
    b1 @ _: bool,
    mut body1 @ _: crate::aver_generated::domain::ast::Expr,
    p2 @ _: &crate::aver_generated::domain::ast::Pattern,
    mut body2 @ _: crate::aver_generated::domain::ast::Expr,
) -> Option<(
    crate::aver_generated::domain::ast::Expr,
    crate::aver_generated::domain::ast::Expr,
)> {
    crate::cancel_checkpoint();
    match p2 {
        crate::aver_generated::domain::ast::Pattern::PatBool(b2) => {
            let b2 = b2.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteBoolMatchArmPairBools(
                b1, body1, b2, body2,
            )
        }
        _ => None,
    }
}

/// Return ordered bool branch bodies when the pair is exactly true/false or false/true.
pub fn rewriteBoolMatchArmPairBools(
    b1 @ _: bool,
    mut body1 @ _: crate::aver_generated::domain::ast::Expr,
    b2 @ _: bool,
    mut body2 @ _: crate::aver_generated::domain::ast::Expr,
) -> Option<(
    crate::aver_generated::domain::ast::Expr,
    crate::aver_generated::domain::ast::Expr,
)> {
    crate::cancel_checkpoint();
    match (b1, b2) {
        (true, false) => Some((body1, body2)),
        (false, true) => Some((body2, body1)),
        _ => None,
    }
}

/// Synthesized collecting variant of `rewriteInternalStmts`. Appends to a builder where `rewriteInternalStmts` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn rewriteInternalStmts__collected(
    stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = stmts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = stmts;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let (Some(stmt), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::list_builder_push(
                            acc,
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalStmt(
                                stmt,
                            ),
                        );
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

/// Synthesized collecting variant of `rewriteInternalExprs`. Appends to a builder where `rewriteInternalExprs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn rewriteInternalExprs__collected(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
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
                if let (Some(expr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::list_builder_push(
                            acc,
                            crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                                expr.clone(),
                            ),
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

/// Synthesized collecting variant of `rewriteInternalFields`. Appends to a builder where `rewriteInternalFields` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn rewriteInternalFields__collected(
    fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
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
                            let __tco1 = aver_rt::list_builder_push(acc, (name, crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr)));
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

/// Synthesized collecting variant of `rewriteInternalArms`. Appends to a builder where `rewriteInternalArms` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn rewriteInternalArms__collected(
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>;
    let mut arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> = arms;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = arms;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let (Some(arm), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::ast::MatchArm { pattern: crate::aver_generated::domain::resolver::rewrite::rewritePattern(arm.pattern.clone()), body: crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(arm.body.clone()), bindingSlots: arm.bindingSlots.clone() });
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

/// Synthesized collecting variant of `rewritePatterns`. Appends to a builder where `rewritePatterns` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn rewritePatterns__collected(
    pats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Pattern> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>;
    let mut pats @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern> = pats;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = pats;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 = aver_rt::list_builder_push(
                            acc,
                            crate::aver_generated::domain::resolver::rewrite::rewritePattern(
                                p.clone(),
                            ),
                        );
                        __hold0 = __tco0;
                        pats = &__hold0;
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
