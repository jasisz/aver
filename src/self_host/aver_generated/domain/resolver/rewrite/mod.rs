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
        aver_list_match!(fns, [] => { return acc.reverse(); }, [f, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::rewrite::rewriteInternalFn(&f), &acc);
            fns = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Rewrite the body of one resolved function into lighter internal expression shapes.
pub fn rewriteInternalFn(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::ast::FnDef {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: crate::aver_generated::domain::resolver::rewrite::rewriteInternalStmts__collected(
            fd.body.clone(),
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
        ),
        slotCount: fd.slotCount.clone(),
        slotMap: fd.slotMap.clone(),
        fastPath: fd.fastPath.clone(),
        tailLoop: fd.tailLoop,
    }
}

/// Rewrite statements recursively into lighter internal expression shapes.
#[inline(always)]
pub fn rewriteInternalStmts(
    mut stmts @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return acc.reverse(); }, [stmt, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::rewrite::rewriteInternalStmt(&stmt), &acc);
            stmts = __tco0;
            acc = __tco1;
            continue;
        } })
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
            let expr = expr.clone();
            crate::aver_generated::domain::ast::Stmt::StmtBind(
                name,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, expr) => {
            let slot = slot.clone();
            let expr = expr.clone();
            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(
                slot,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            let expr = expr.clone();
            crate::aver_generated::domain::ast::Stmt::StmtExpr(
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr),
            )
        }
    }
}

/// Rewrite one expression into lighter internal forms after resolve and direct-call linking.
pub fn rewriteInternalExpr(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match &expr {
        crate::aver_generated::domain::ast::Expr::ExprBoolBranch(__cond_fields) => {
            let cond = __cond_fields.0.clone();
            let thenExpr = __cond_fields.1.clone();
            let elseExpr = __cond_fields.2.clone();
            crate::aver_generated::domain::ast::Expr::ExprBoolBranch(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprBoolBranch(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(cond),
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(thenExpr),
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(elseExpr),
                ),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(_) => expr,
        crate::aver_generated::domain::ast::Expr::ExprBinopSlots(_) => expr,
        crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(_) => expr,
        crate::aver_generated::domain::ast::Expr::ExprCmpSlots(_) => expr,
        crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(__vecExpr_fields) => {
            let vecExpr = __vecExpr_fields.0.clone();
            let idxExpr = __vecExpr_fields.1.clone();
            let defaultValue = __vecExpr_fields.2.clone();
            crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprVectorGetOrInt(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(vecExpr),
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(idxExpr),
                    defaultValue,
                ),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            let defaultValue = __a_fields.2.clone();
            crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprIntModOrInt(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
                    defaultValue,
                ),
            ))
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprAfterLeaf(expr),
    }
}

/// Continue internal rewrite for arithmetic and comparison forms.
pub fn rewriteInternalExprAfterLeaf(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match &expr {
        crate::aver_generated::domain::ast::Expr::ExprAdd(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                crate::aver_generated::domain::ast::BinOp::OpAdd,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprSub(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                crate::aver_generated::domain::ast::BinOp::OpSub,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprMul(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                crate::aver_generated::domain::ast::BinOp::OpMul,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprDiv(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                crate::aver_generated::domain::ast::BinOp::OpDiv,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprNeg(__inner_fields) => {
            let inner = __inner_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprNeg(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprNeg(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(inner),
                ),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprEq(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                crate::aver_generated::domain::ast::CmpOp::CmpEq,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprNeq(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                crate::aver_generated::domain::ast::CmpOp::CmpNeq,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLt(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                crate::aver_generated::domain::ast::CmpOp::CmpLt,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGt(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                crate::aver_generated::domain::ast::CmpOp::CmpGt,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLte(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                crate::aver_generated::domain::ast::CmpOp::CmpLte,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGte(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                crate::aver_generated::domain::ast::CmpOp::CmpGte,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(a),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(b),
            )
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprAfterArith(expr),
    }
}

/// Finish internal rewrite for aggregates, calls, and products.
pub fn rewriteInternalExprAfterArith(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match &expr {
        crate::aver_generated::domain::ast::Expr::ExprMatch(__scrutinee_fields) => {
            let scrutinee = __scrutinee_fields.0.clone();
            let arms = __scrutinee_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalMatch(
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(scrutinee),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalArms__collected(
                    arms,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprPropagate(__inner_fields) => {
            let inner = __inner_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprPropagate(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprPropagate(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(inner),
                ),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprConcat(__parts_fields) => {
            let parts = __parts_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprConcat(std::sync::Arc::new(crate::aver_generated::domain::ast::Expr__ExprConcat(crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(parts, aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))))))
        }
        crate::aver_generated::domain::ast::Expr::ExprTuple(__exprs_fields) => {
            let exprs = __exprs_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprTuple(std::sync::Arc::new(crate::aver_generated::domain::ast::Expr__ExprTuple(crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(exprs, aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))))))
        }
        crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(__exprs_fields) => {
            let exprs = __exprs_fields.0.clone();
            let unwrap = __exprs_fields.1.clone();
            crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(std::sync::Arc::new(crate::aver_generated::domain::ast::Expr__ExprIndependentProduct(crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(exprs, aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))), unwrap)))
        }
        crate::aver_generated::domain::ast::Expr::ExprList(__exprs_fields) => {
            let exprs = __exprs_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprList(std::sync::Arc::new(crate::aver_generated::domain::ast::Expr__ExprList(crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(exprs, aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))))))
        }
        crate::aver_generated::domain::ast::Expr::ExprRecord(__name_fields) => {
            let name = __name_fields.0.clone();
            let fields = __name_fields.1.clone();
            crate::aver_generated::domain::ast::Expr::ExprRecord(std::sync::Arc::new(crate::aver_generated::domain::ast::Expr__ExprRecord(name, crate::aver_generated::domain::resolver::rewrite::rewriteInternalFields__collected(fields, aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))))))
        }
        crate::aver_generated::domain::ast::Expr::ExprFieldAccess(__obj_fields) => {
            let obj = __obj_fields.0.clone();
            let field = __obj_fields.1.clone();
            crate::aver_generated::domain::ast::Expr::ExprFieldAccess(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprFieldAccess(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(obj),
                    field,
                ),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprVar(__name_fields) => {
            let name = __name_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprVar(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprVar(
                    crate::aver_generated::domain::ast::canonicalCtorName(name),
                ),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprCall(__name_fields) => {
            let name = __name_fields.0.clone();
            let args = __name_fields.1.clone();
            crate::aver_generated::domain::ast::Expr::ExprCall(std::sync::Arc::new(crate::aver_generated::domain::ast::Expr__ExprCall(crate::aver_generated::domain::ast::canonicalCtorName(name), crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(args, aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))))))
        }
        crate::aver_generated::domain::ast::Expr::ExprCallDirect(__fnId_fields) => {
            let fnId = __fnId_fields.0.clone();
            let args = __fnId_fields.1.clone();
            crate::aver_generated::domain::ast::Expr::ExprCallDirect(std::sync::Arc::new(crate::aver_generated::domain::ast::Expr__ExprCallDirect(fnId, crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(args, aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))))))
        }
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(__name_fields) => {
            let name = __name_fields.0.clone();
            let args = __name_fields.1.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBuiltin(
                name,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(
                    args,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(__id_fields) => {
            let id = __id_fields.0.clone();
            let args = __id_fields.1.clone();
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(std::sync::Arc::new(crate::aver_generated::domain::ast::Expr__ExprCallBuiltinId(id, crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(args, aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0))))))
        }
        _ => expr,
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
        aver_list_match!(exprs, [] => { return acc.reverse(); }, [expr, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr), &acc);
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
        aver_list_match!(fields, [] => { return acc.reverse(); }, [pair, rest] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend((name, crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr)), &acc);
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
        aver_list_match!(arms, [] => { return acc.reverse(); }, [arm, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::ast::MatchArm { pattern: crate::aver_generated::domain::resolver::rewrite::rewritePattern(arm.pattern), body: crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(arm.body), bindingSlots: arm.bindingSlots }, &acc);
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
            let bindings = bindings.clone();
            crate::aver_generated::domain::resolver::rewrite::rewritePatConstructor(name, &bindings)
        }
        crate::aver_generated::domain::ast::Pattern::PatTuple(pats) => {
            let pats = pats.clone();
            crate::aver_generated::domain::ast::Pattern::PatTuple(
                crate::aver_generated::domain::resolver::rewrite::rewritePatterns__collected(
                    pats,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
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
        aver_list_match!(pats, [] => { return acc.reverse(); }, [p, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::rewrite::rewritePattern(p), &acc);
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
    match &left {
        crate::aver_generated::domain::ast::Expr::ExprSlot(__slot_fields) => {
            let slot = __slot_fields.0.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinopSlotLeft(
                op, slot, right,
            )
        }
        _ => match &right {
            crate::aver_generated::domain::ast::Expr::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinopSlotRight(
                    op, left, slot,
                )
            }
            _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
                &op, left, right,
            ),
        },
    }
}

/// Rewrite arithmetic with a slot on the left when the right side is simple.
pub fn rewriteInternalBinopSlotLeft(
    mut op @ _: crate::aver_generated::domain::ast::BinOp,
    slot @ _: aver_rt::AverInt,
    mut right @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match &right {
        crate::aver_generated::domain::ast::Expr::ExprInt(__n_fields) => {
            let n = __n_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprBinopSlotInt(op, slot, n),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprSlot(__rhs_fields) => {
            let rhs = __rhs_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprBinopSlots(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprBinopSlots(op, slot, rhs),
            ))
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
            &op,
            crate::aver_generated::domain::ast::Expr::ExprSlot(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
            )),
            right,
        ),
    }
}

/// Rewrite commutative arithmetic when the slot appears on the right.
pub fn rewriteInternalBinopSlotRight(
    mut op @ _: crate::aver_generated::domain::ast::BinOp,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    slot @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match &left {
        crate::aver_generated::domain::ast::Expr::ExprSlot(__lhs_fields) => {
            let lhs = __lhs_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprBinopSlots(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprBinopSlots(op, lhs, slot),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprInt(__n_fields) => {
            let n = __n_fields.0.clone();
            if crate::aver_generated::domain::resolver::rewrite::binopCanFlip(&op) {
                crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(std::sync::Arc::new(
                    crate::aver_generated::domain::ast::Expr__ExprBinopSlotInt(op, slot, n),
                ))
            } else {
                crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
                    &op,
                    left,
                    crate::aver_generated::domain::ast::Expr::ExprSlot(std::sync::Arc::new(
                        crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
                    )),
                )
            }
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
            &op,
            left,
            crate::aver_generated::domain::ast::Expr::ExprSlot(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
            )),
        ),
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
            crate::aver_generated::domain::ast::Expr::ExprAdd(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprAdd(left, right),
            ))
        }
        crate::aver_generated::domain::ast::BinOp::OpSub => {
            crate::aver_generated::domain::ast::Expr::ExprSub(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprSub(left, right),
            ))
        }
        crate::aver_generated::domain::ast::BinOp::OpMul => {
            crate::aver_generated::domain::ast::Expr::ExprMul(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprMul(left, right),
            ))
        }
        crate::aver_generated::domain::ast::BinOp::OpDiv => {
            crate::aver_generated::domain::ast::Expr::ExprDiv(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprDiv(left, right),
            ))
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
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
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
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(name, args.clone()),
                ))
            }
        }
    }
}

/// Rewrite Option.withDefault(Vector.get(...), int) into one internal node.
pub fn rewriteInternalOptionWithDefault(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((optionExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((defaultExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalOptionWithDefaultArgs(optionExpr, defaultExpr)
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                            AverStr::from("Option.withDefault"),
                            args.clone(),
                        ),
                    ))
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    AverStr::from("Option.withDefault"),
                    args.clone(),
                ),
            ))
        }
    }
}

/// Recognize a fused Vector.get-with-default shape.
pub fn rewriteInternalOptionWithDefaultArgs(
    mut optionExpr @ _: crate::aver_generated::domain::ast::Expr,
    mut defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match &defaultExpr {
        crate::aver_generated::domain::ast::Expr::ExprInt(__defaultValue_fields) => {
            let defaultValue = __defaultValue_fields.0.clone();
            match &optionExpr {
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(__name_fields) => {
                    let name = __name_fields.0.clone();
                    let innerArgs = __name_fields.1.clone();
                    if (&*name == "Vector.get") {
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalVectorGetOrInt(&innerArgs, defaultValue)
                    } else {
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            std::sync::Arc::new(
                                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                    AverStr::from("Option.withDefault"),
                                    aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]),
                                ),
                            ),
                        )
                    }
                }
                _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                    std::sync::Arc::new(crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                        AverStr::from("Option.withDefault"),
                        aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]),
                    )),
                ),
            }
        }
        _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                AverStr::from("Option.withDefault"),
                aver_rt::AverList::from_vec(vec![optionExpr, defaultExpr]),
            ),
        )),
    }
}

/// Fuse Vector.get(vec, idx) with an integer fallback.
pub fn rewriteInternalVectorGetOrInt(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultValue @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((vecExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((idxExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalVectorGetOrIntArgs(vecExpr, idxExpr, defaultValue)
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                            AverStr::from("Option.withDefault"),
                            aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    std::sync::Arc::new(
                                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                            AverStr::from("Vector.get"),
                                            args.clone(),
                                        ),
                                    ),
                                ),
                                crate::aver_generated::domain::ast::Expr::ExprInt(
                                    std::sync::Arc::new(
                                        crate::aver_generated::domain::ast::Expr__ExprInt(
                                            defaultValue,
                                        ),
                                    ),
                                ),
                            ]),
                        ),
                    ))
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    AverStr::from("Option.withDefault"),
                    aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            std::sync::Arc::new(
                                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                    AverStr::from("Vector.get"),
                                    args.clone(),
                                ),
                            ),
                        ),
                        crate::aver_generated::domain::ast::Expr::ExprInt(std::sync::Arc::new(
                            crate::aver_generated::domain::ast::Expr__ExprInt(defaultValue),
                        )),
                    ]),
                ),
            ))
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
        crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(std::sync::Arc::new(
            crate::aver_generated::domain::ast::Expr__ExprVectorGetOrInt(
                vecExpr,
                idxExpr,
                defaultValue,
            ),
        ))
    } else {
        if crate::aver_generated::domain::resolver::rewrite::exprHasSlotShape(&idxExpr) {
            crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprVectorGetOrInt(
                    vecExpr,
                    idxExpr,
                    defaultValue,
                ),
            ))
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    AverStr::from("Option.withDefault"),
                    aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            std::sync::Arc::new(
                                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                    AverStr::from("Vector.get"),
                                    aver_rt::AverList::from_vec(vec![vecExpr, idxExpr]),
                                ),
                            ),
                        ),
                        crate::aver_generated::domain::ast::Expr::ExprInt(std::sync::Arc::new(
                            crate::aver_generated::domain::ast::Expr__ExprInt(defaultValue),
                        )),
                    ]),
                ),
            ))
        }
    }
}

/// Rewrite Result.withDefault(Int.mod(...), int) into one internal node.
pub fn rewriteInternalResultWithDefault(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((resultExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((defaultExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalResultWithDefaultArgs(resultExpr, defaultExpr)
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                            AverStr::from("Result.withDefault"),
                            args.clone(),
                        ),
                    ))
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    AverStr::from("Result.withDefault"),
                    args.clone(),
                ),
            ))
        }
    }
}

/// Recognize a fused Int.mod-with-default shape.
pub fn rewriteInternalResultWithDefaultArgs(
    mut resultExpr @ _: crate::aver_generated::domain::ast::Expr,
    mut defaultExpr @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match &defaultExpr {
        crate::aver_generated::domain::ast::Expr::ExprInt(__defaultValue_fields) => {
            let defaultValue = __defaultValue_fields.0.clone();
            match &resultExpr {
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(__name_fields) => {
                    let name = __name_fields.0.clone();
                    let innerArgs = __name_fields.1.clone();
                    if (&*name == "Int.mod") {
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalIntModOrInt(
                            &innerArgs,
                            defaultValue,
                        )
                    } else {
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            std::sync::Arc::new(
                                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                    AverStr::from("Result.withDefault"),
                                    aver_rt::AverList::from_vec(vec![resultExpr, defaultExpr]),
                                ),
                            ),
                        )
                    }
                }
                _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                    std::sync::Arc::new(crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                        AverStr::from("Result.withDefault"),
                        aver_rt::AverList::from_vec(vec![resultExpr, defaultExpr]),
                    )),
                ),
            }
        }
        _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
            crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                AverStr::from("Result.withDefault"),
                aver_rt::AverList::from_vec(vec![resultExpr, defaultExpr]),
            ),
        )),
    }
}

/// Fuse Int.mod(a, b) with an integer fallback when the operands are slot-shaped.
pub fn rewriteInternalIntModOrInt(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultValue @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((a, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((b, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalIntModOrIntArgs(
                        a,
                        b,
                        defaultValue,
                    )
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                            AverStr::from("Result.withDefault"),
                            aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                    std::sync::Arc::new(
                                        crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                            AverStr::from("Int.mod"),
                                            args.clone(),
                                        ),
                                    ),
                                ),
                                crate::aver_generated::domain::ast::Expr::ExprInt(
                                    std::sync::Arc::new(
                                        crate::aver_generated::domain::ast::Expr__ExprInt(
                                            defaultValue,
                                        ),
                                    ),
                                ),
                            ]),
                        ),
                    ))
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    AverStr::from("Result.withDefault"),
                    aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            std::sync::Arc::new(
                                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                    AverStr::from("Int.mod"),
                                    args.clone(),
                                ),
                            ),
                        ),
                        crate::aver_generated::domain::ast::Expr::ExprInt(std::sync::Arc::new(
                            crate::aver_generated::domain::ast::Expr__ExprInt(defaultValue),
                        )),
                    ]),
                ),
            ))
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
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(std::sync::Arc::new(
            crate::aver_generated::domain::ast::Expr__ExprIntModOrInt(a, b, defaultValue),
        ))
    } else {
        if crate::aver_generated::domain::resolver::rewrite::exprHasSlotShape(&b) {
            crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprIntModOrInt(a, b, defaultValue),
            ))
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                    AverStr::from("Result.withDefault"),
                    aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            std::sync::Arc::new(
                                crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(
                                    AverStr::from("Int.mod"),
                                    aver_rt::AverList::from_vec(vec![a, b]),
                                ),
                            ),
                        ),
                        crate::aver_generated::domain::ast::Expr::ExprInt(std::sync::Arc::new(
                            crate::aver_generated::domain::ast::Expr__ExprInt(defaultValue),
                        )),
                    ]),
                ),
            ))
        }
    }
}

/// Return whether an expression is already in a slot-based internal shape worth fusing around.
pub fn exprHasSlotShape(expr @ _: &crate::aver_generated::domain::ast::Expr) -> bool {
    crate::cancel_checkpoint();
    match expr {
        crate::aver_generated::domain::ast::Expr::ExprSlot(_) => true,
        crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(_) => true,
        crate::aver_generated::domain::ast::Expr::ExprBinopSlots(_) => true,
        crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(_) => true,
        crate::aver_generated::domain::ast::Expr::ExprCmpSlots(_) => true,
        crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(_) => true,
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(_) => true,
        _ => false,
    }
}

/// Compress simple resolved comparisons into slot-based internal nodes.
pub fn rewriteInternalCmp(
    mut op @ _: crate::aver_generated::domain::ast::CmpOp,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    mut right @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match &left {
        crate::aver_generated::domain::ast::Expr::ExprSlot(__slot_fields) => {
            let slot = __slot_fields.0.clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmpSlotLeft(
                op, slot, right,
            )
        }
        _ => match &right {
            crate::aver_generated::domain::ast::Expr::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmpSlotRight(
                    op, left, slot,
                )
            }
            _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalCmp(
                &op, left, right,
            ),
        },
    }
}

/// Rewrite comparisons with a slot on the left when the right side is simple.
pub fn rewriteInternalCmpSlotLeft(
    mut op @ _: crate::aver_generated::domain::ast::CmpOp,
    slot @ _: aver_rt::AverInt,
    mut right @ _: crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match &right {
        crate::aver_generated::domain::ast::Expr::ExprInt(__n_fields) => {
            let n = __n_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprCmpSlotInt(op, slot, n),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprSlot(__rhs_fields) => {
            let rhs = __rhs_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprCmpSlots(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprCmpSlots(op, slot, rhs),
            ))
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalCmp(
            &op,
            crate::aver_generated::domain::ast::Expr::ExprSlot(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
            )),
            right,
        ),
    }
}

/// Rewrite comparisons with a slot on the right when the left side is a simple integer.
pub fn rewriteInternalCmpSlotRight(
    mut op @ _: crate::aver_generated::domain::ast::CmpOp,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    slot @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match &left {
        crate::aver_generated::domain::ast::Expr::ExprInt(__n_fields) => {
            let n = __n_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprCmpSlotInt(
                    crate::aver_generated::domain::resolver::rewrite::flipCmp(&op),
                    slot,
                    n,
                ),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprSlot(__lhs_fields) => {
            let lhs = __lhs_fields.0.clone();
            crate::aver_generated::domain::ast::Expr::ExprCmpSlots(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprCmpSlots(op, lhs, slot),
            ))
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalCmp(
            &op,
            left,
            crate::aver_generated::domain::ast::Expr::ExprSlot(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprSlot(slot),
            )),
        ),
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
            crate::aver_generated::domain::ast::Expr::ExprEq(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprEq(left, right),
            ))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            crate::aver_generated::domain::ast::Expr::ExprNeq(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprNeq(left, right),
            ))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            crate::aver_generated::domain::ast::Expr::ExprLt(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprLt(left, right),
            ))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            crate::aver_generated::domain::ast::Expr::ExprGt(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprGt(left, right),
            ))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            crate::aver_generated::domain::ast::Expr::ExprLte(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprLte(left, right),
            ))
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            crate::aver_generated::domain::ast::Expr::ExprGte(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprGte(left, right),
            ))
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
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::resolver::rewrite::rewriteBoolMatchArms(arms) {
        Some(pair @ _) => {
            let (thenExpr, elseExpr) = pair;
            crate::aver_generated::domain::ast::Expr::ExprBoolBranch(std::sync::Arc::new(
                crate::aver_generated::domain::ast::Expr__ExprBoolBranch(
                    scrutinee, thenExpr, elseExpr,
                ),
            ))
        }
        None => crate::aver_generated::domain::ast::Expr::ExprMatch(std::sync::Arc::new(
            crate::aver_generated::domain::ast::Expr__ExprMatch(scrutinee, arms.clone()),
        )),
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
        let __list_subject = arms.clone();
        if let Some((arm1, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((arm2, tail)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    if (tail == aver_rt::AverList::empty()) {
                        crate::aver_generated::domain::resolver::rewrite::rewriteBoolMatchArmPair(
                            arm1, arm2,
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
    mut stmts @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return aver_rt::list_builder_finalize(acc); }, [stmt, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::resolver::rewrite::rewriteInternalStmt(&stmt));
            stmts = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `rewriteInternalExprs`. Appends to a builder where `rewriteInternalExprs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn rewriteInternalExprs__collected(
    mut exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return aver_rt::list_builder_finalize(acc); }, [expr, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr));
            exprs = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `rewriteInternalFields`. Appends to a builder where `rewriteInternalFields` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn rewriteInternalFields__collected(
    mut fields @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return aver_rt::list_builder_finalize(acc); }, [pair, rest] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, (name, crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(expr)));
            fields = __tco0;
            acc = __tco1;
            continue;
        } } })
    }
}

/// Synthesized collecting variant of `rewriteInternalArms`. Appends to a builder where `rewriteInternalArms` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn rewriteInternalArms__collected(
    mut arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(arms, [] => { return aver_rt::list_builder_finalize(acc); }, [arm, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::ast::MatchArm { pattern: crate::aver_generated::domain::resolver::rewrite::rewritePattern(arm.pattern), body: crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(arm.body), bindingSlots: arm.bindingSlots });
            arms = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `rewritePatterns`. Appends to a builder where `rewritePatterns` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn rewritePatterns__collected(
    mut pats @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Pattern> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(pats, [] => { return aver_rt::list_builder_finalize(acc); }, [p, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::resolver::rewrite::rewritePattern(p));
            pats = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}
