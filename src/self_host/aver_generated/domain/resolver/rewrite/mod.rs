#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
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
    match stmt.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtBind(name, expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtBind(
                name,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&expr),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(
                slot,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&expr),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtExpr(
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&expr),
            )
        }
    }
}

/// Rewrite one expression into lighter internal forms after resolve and direct-call linking.
pub fn rewriteInternalExpr(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprBoolBranch(cond, thenExpr, elseExpr) => {
            let cond = (*cond).clone();
            let thenExpr = (*thenExpr).clone();
            let elseExpr = (*elseExpr).clone();
            crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&cond),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                        &thenExpr,
                    ),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(
                        &elseExpr,
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(_, _, _) => expr.clone(),
        crate::aver_generated::domain::ast::Expr::ExprBinopSlots(_, _, _) => expr.clone(),
        crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(_, _, _) => expr.clone(),
        crate::aver_generated::domain::ast::Expr::ExprCmpSlots(_, _, _) => expr.clone(),
        crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
            vecExpr,
            idxExpr,
            defaultValue,
        ) => {
            let vecExpr = (*vecExpr).clone();
            let idxExpr = (*idxExpr).clone();
            crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&vecExpr),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&idxExpr),
                ),
                defaultValue,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(a, b, defaultValue) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
                ),
                defaultValue,
            )
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprAfterLeaf(expr),
    }
}

/// Continue internal rewrite for arithmetic and comparison forms.
pub fn rewriteInternalExprAfterLeaf(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprAdd(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                &crate::aver_generated::domain::ast::BinOp::OpAdd,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprSub(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                &crate::aver_generated::domain::ast::BinOp::OpSub,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprMul(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                &crate::aver_generated::domain::ast::BinOp::OpMul,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprDiv(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinop(
                &crate::aver_generated::domain::ast::BinOp::OpDiv,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprNeg(inner) => {
            let inner = (*inner).clone();
            crate::aver_generated::domain::ast::Expr::ExprNeg(std::sync::Arc::new(
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&inner),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                &crate::aver_generated::domain::ast::CmpOp::CmpEq,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprNeq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                &crate::aver_generated::domain::ast::CmpOp::CmpNeq,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                &crate::aver_generated::domain::ast::CmpOp::CmpLt,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                &crate::aver_generated::domain::ast::CmpOp::CmpGt,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                &crate::aver_generated::domain::ast::CmpOp::CmpLte,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmp(
                &crate::aver_generated::domain::ast::CmpOp::CmpGte,
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&a),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&b),
            )
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprAfterArith(expr),
    }
}

/// Finish internal rewrite for aggregates, calls, and products.
pub fn rewriteInternalExprAfterArith(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprMatch(scrutinee, arms) => {
            let scrutinee = (*scrutinee).clone();
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalMatch(
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&scrutinee),
                &crate::aver_generated::domain::resolver::rewrite::rewriteInternalArms__collected(
                    arms,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprPropagate(inner) => {
            let inner = (*inner).clone();
            crate::aver_generated::domain::ast::Expr::ExprPropagate(std::sync::Arc::new(
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&inner),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprConcat(parts) => {
            crate::aver_generated::domain::ast::Expr::ExprConcat(
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(
                    parts,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprTuple(exprs) => {
            crate::aver_generated::domain::ast::Expr::ExprTuple(
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(
                    exprs,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(exprs, unwrap) => {
            crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(
                    exprs,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
                unwrap,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprList(exprs) => {
            crate::aver_generated::domain::ast::Expr::ExprList(
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(
                    exprs,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprRecord(name, fields) => {
            crate::aver_generated::domain::ast::Expr::ExprRecord(
                name,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalFields__collected(
                    fields,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprFieldAccess(obj, field) => {
            let obj = (*obj).clone();
            crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&obj),
                ),
                field,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprVar(name) => {
            crate::aver_generated::domain::ast::Expr::ExprVar(
                crate::aver_generated::domain::ast::canonicalCtorName(name),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCall(name, args) => {
            crate::aver_generated::domain::ast::Expr::ExprCall(
                crate::aver_generated::domain::ast::canonicalCtorName(name),
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(
                    args,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCallDirect(fnId, args) => {
            crate::aver_generated::domain::ast::Expr::ExprCallDirect(
                fnId,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(
                    args,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, args) => {
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
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(id, args) => {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                id,
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalExprs__collected(
                    args,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        _ => expr.clone(),
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
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&expr), &acc);
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
            let __tco1 = aver_rt::AverList::prepend((name, crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&expr)), &acc);
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
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::ast::MatchArm { pattern: crate::aver_generated::domain::resolver::rewrite::rewritePattern(&arm.pattern), body: crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&arm.body), bindingSlots: arm.bindingSlots.clone() }, &acc);
            arms = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Rewrite PatConstructor to PatConstructorId with both tag and full constructor name.
pub fn rewritePattern(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
) -> crate::aver_generated::domain::ast::Pattern {
    crate::cancel_checkpoint();
    match pat.clone() {
        crate::aver_generated::domain::ast::Pattern::PatConstructor(name, bindings) => {
            crate::aver_generated::domain::resolver::rewrite::rewritePatConstructor(name, &bindings)
        }
        crate::aver_generated::domain::ast::Pattern::PatTuple(pats) => {
            crate::aver_generated::domain::ast::Pattern::PatTuple(
                crate::aver_generated::domain::resolver::rewrite::rewritePatterns__collected(
                    pats,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        _ => pat.clone(),
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
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::rewrite::rewritePattern(&p), &acc);
            pats = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Compress simple resolved arithmetic into slot-based internal nodes.
pub fn rewriteInternalBinop(
    op @ _: &crate::aver_generated::domain::ast::BinOp,
    left @ _: &crate::aver_generated::domain::ast::Expr,
    right @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match left.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinopSlotLeft(
                op, slot, right,
            )
        }
        _ => match right.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalBinopSlotRight(
                    op, left, slot,
                )
            }
            _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
                op, left, right,
            ),
        },
    }
}

/// Rewrite arithmetic with a slot on the left when the right side is simple.
pub fn rewriteInternalBinopSlotLeft(
    op @ _: &crate::aver_generated::domain::ast::BinOp,
    slot @ _: aver_rt::AverInt,
    right @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match right.clone() {
        crate::aver_generated::domain::ast::Expr::ExprInt(n) => {
            crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(op.clone(), slot, n)
        }
        crate::aver_generated::domain::ast::Expr::ExprSlot(rhs) => {
            crate::aver_generated::domain::ast::Expr::ExprBinopSlots(op.clone(), slot, rhs)
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
            op,
            &crate::aver_generated::domain::ast::Expr::ExprSlot(slot),
            right,
        ),
    }
}

/// Rewrite commutative arithmetic when the slot appears on the right.
pub fn rewriteInternalBinopSlotRight(
    op @ _: &crate::aver_generated::domain::ast::BinOp,
    left @ _: &crate::aver_generated::domain::ast::Expr,
    slot @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match left.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(lhs) => {
            crate::aver_generated::domain::ast::Expr::ExprBinopSlots(op.clone(), lhs, slot)
        }
        crate::aver_generated::domain::ast::Expr::ExprInt(n) => {
            if crate::aver_generated::domain::resolver::rewrite::binopCanFlip(op) {
                crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(op.clone(), slot, n)
            } else {
                crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
                    op,
                    left,
                    &crate::aver_generated::domain::ast::Expr::ExprSlot(slot),
                )
            }
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalBinop(
            op,
            left,
            &crate::aver_generated::domain::ast::Expr::ExprSlot(slot),
        ),
    }
}

/// Rebuild a generic arithmetic expression when no narrower internal form applies.
pub fn rebuildInternalBinop(
    op @ _: &crate::aver_generated::domain::ast::BinOp,
    left @ _: &crate::aver_generated::domain::ast::Expr,
    right @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::BinOp::OpAdd => {
            crate::aver_generated::domain::ast::Expr::ExprAdd(
                std::sync::Arc::new(left.clone()),
                std::sync::Arc::new(right.clone()),
            )
        }
        crate::aver_generated::domain::ast::BinOp::OpSub => {
            crate::aver_generated::domain::ast::Expr::ExprSub(
                std::sync::Arc::new(left.clone()),
                std::sync::Arc::new(right.clone()),
            )
        }
        crate::aver_generated::domain::ast::BinOp::OpMul => {
            crate::aver_generated::domain::ast::Expr::ExprMul(
                std::sync::Arc::new(left.clone()),
                std::sync::Arc::new(right.clone()),
            )
        }
        crate::aver_generated::domain::ast::BinOp::OpDiv => {
            crate::aver_generated::domain::ast::Expr::ExprDiv(
                std::sync::Arc::new(left.clone()),
                std::sync::Arc::new(right.clone()),
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
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, args.clone())
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
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalOptionWithDefaultArgs(&optionExpr, &defaultExpr)
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        AverStr::from("Option.withDefault"),
                        args.clone(),
                    )
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                AverStr::from("Option.withDefault"),
                args.clone(),
            )
        }
    }
}

/// Recognize a fused Vector.get-with-default shape.
pub fn rewriteInternalOptionWithDefaultArgs(
    optionExpr @ _: &crate::aver_generated::domain::ast::Expr,
    defaultExpr @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match defaultExpr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprInt(defaultValue) => {
            match optionExpr.clone() {
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, innerArgs) => {
                    if (&*name == "Vector.get") {
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalVectorGetOrInt(&innerArgs, defaultValue)
                    } else {
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            AverStr::from("Option.withDefault"),
                            aver_rt::AverList::from_vec(vec![
                                optionExpr.clone(),
                                defaultExpr.clone(),
                            ]),
                        )
                    }
                }
                _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                    AverStr::from("Option.withDefault"),
                    aver_rt::AverList::from_vec(vec![optionExpr.clone(), defaultExpr.clone()]),
                ),
            }
        }
        _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
            AverStr::from("Option.withDefault"),
            aver_rt::AverList::from_vec(vec![optionExpr.clone(), defaultExpr.clone()]),
        ),
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
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalVectorGetOrIntArgs(&vecExpr, &idxExpr, defaultValue)
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        AverStr::from("Option.withDefault"),
                        aver_rt::AverList::from_vec(vec![
                            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                AverStr::from("Vector.get"),
                                args.clone(),
                            ),
                            crate::aver_generated::domain::ast::Expr::ExprInt(defaultValue),
                        ]),
                    )
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                AverStr::from("Option.withDefault"),
                aver_rt::AverList::from_vec(vec![
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        AverStr::from("Vector.get"),
                        args.clone(),
                    ),
                    crate::aver_generated::domain::ast::Expr::ExprInt(defaultValue),
                ]),
            )
        }
    }
}

/// Only fuse Vector.get-with-default when at least one operand is already slot-shaped.
#[inline(always)]
pub fn rewriteInternalVectorGetOrIntArgs(
    vecExpr @ _: &crate::aver_generated::domain::ast::Expr,
    idxExpr @ _: &crate::aver_generated::domain::ast::Expr,
    defaultValue @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::resolver::rewrite::exprHasSlotShape(vecExpr) {
        crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
            std::sync::Arc::new(vecExpr.clone()),
            std::sync::Arc::new(idxExpr.clone()),
            defaultValue,
        )
    } else {
        if crate::aver_generated::domain::resolver::rewrite::exprHasSlotShape(idxExpr) {
            crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                std::sync::Arc::new(vecExpr.clone()),
                std::sync::Arc::new(idxExpr.clone()),
                defaultValue,
            )
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                AverStr::from("Option.withDefault"),
                aver_rt::AverList::from_vec(vec![
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        AverStr::from("Vector.get"),
                        aver_rt::AverList::from_vec(vec![vecExpr.clone(), idxExpr.clone()]),
                    ),
                    crate::aver_generated::domain::ast::Expr::ExprInt(defaultValue),
                ]),
            )
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
                    crate::aver_generated::domain::resolver::rewrite::rewriteInternalResultWithDefaultArgs(&resultExpr, &defaultExpr)
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        AverStr::from("Result.withDefault"),
                        args.clone(),
                    )
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                AverStr::from("Result.withDefault"),
                args.clone(),
            )
        }
    }
}

/// Recognize a fused Int.mod-with-default shape.
pub fn rewriteInternalResultWithDefaultArgs(
    resultExpr @ _: &crate::aver_generated::domain::ast::Expr,
    defaultExpr @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match defaultExpr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprInt(defaultValue) => {
            match resultExpr.clone() {
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, innerArgs) => {
                    if (&*name == "Int.mod") {
                        crate::aver_generated::domain::resolver::rewrite::rewriteInternalIntModOrInt(
                            &innerArgs,
                            defaultValue,
                        )
                    } else {
                        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                            AverStr::from("Result.withDefault"),
                            aver_rt::AverList::from_vec(vec![
                                resultExpr.clone(),
                                defaultExpr.clone(),
                            ]),
                        )
                    }
                }
                _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                    AverStr::from("Result.withDefault"),
                    aver_rt::AverList::from_vec(vec![resultExpr.clone(), defaultExpr.clone()]),
                ),
            }
        }
        _ => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
            AverStr::from("Result.withDefault"),
            aver_rt::AverList::from_vec(vec![resultExpr.clone(), defaultExpr.clone()]),
        ),
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
                        &a,
                        &b,
                        defaultValue,
                    )
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        AverStr::from("Result.withDefault"),
                        aver_rt::AverList::from_vec(vec![
                            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                                AverStr::from("Int.mod"),
                                args.clone(),
                            ),
                            crate::aver_generated::domain::ast::Expr::ExprInt(defaultValue),
                        ]),
                    )
                }
            }
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                AverStr::from("Result.withDefault"),
                aver_rt::AverList::from_vec(vec![
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        AverStr::from("Int.mod"),
                        args.clone(),
                    ),
                    crate::aver_generated::domain::ast::Expr::ExprInt(defaultValue),
                ]),
            )
        }
    }
}

/// Only fuse Int.mod-with-default when at least one operand is already slot-shaped.
#[inline(always)]
pub fn rewriteInternalIntModOrIntArgs(
    a @ _: &crate::aver_generated::domain::ast::Expr,
    b @ _: &crate::aver_generated::domain::ast::Expr,
    defaultValue @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::resolver::rewrite::exprHasSlotShape(a) {
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
            std::sync::Arc::new(a.clone()),
            std::sync::Arc::new(b.clone()),
            defaultValue,
        )
    } else {
        if crate::aver_generated::domain::resolver::rewrite::exprHasSlotShape(b) {
            crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                std::sync::Arc::new(a.clone()),
                std::sync::Arc::new(b.clone()),
                defaultValue,
            )
        } else {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                AverStr::from("Result.withDefault"),
                aver_rt::AverList::from_vec(vec![
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                        AverStr::from("Int.mod"),
                        aver_rt::AverList::from_vec(vec![a.clone(), b.clone()]),
                    ),
                    crate::aver_generated::domain::ast::Expr::ExprInt(defaultValue),
                ]),
            )
        }
    }
}

/// Return whether an expression is already in a slot-based internal shape worth fusing around.
pub fn exprHasSlotShape(expr @ _: &crate::aver_generated::domain::ast::Expr) -> bool {
    crate::cancel_checkpoint();
    match expr {
        crate::aver_generated::domain::ast::Expr::ExprSlot(_) => true,
        crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(_, _, _) => true,
        crate::aver_generated::domain::ast::Expr::ExprBinopSlots(_, _, _) => true,
        crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(_, _, _) => true,
        crate::aver_generated::domain::ast::Expr::ExprCmpSlots(_, _, _) => true,
        crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(_, _, _) => true,
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(_, _, _) => true,
        _ => false,
    }
}

/// Compress simple resolved comparisons into slot-based internal nodes.
pub fn rewriteInternalCmp(
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
    left @ _: &crate::aver_generated::domain::ast::Expr,
    right @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match left.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
            crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmpSlotLeft(
                op, slot, right,
            )
        }
        _ => match right.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
                crate::aver_generated::domain::resolver::rewrite::rewriteInternalCmpSlotRight(
                    op, left, slot,
                )
            }
            _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalCmp(
                op, left, right,
            ),
        },
    }
}

/// Rewrite comparisons with a slot on the left when the right side is simple.
pub fn rewriteInternalCmpSlotLeft(
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
    slot @ _: aver_rt::AverInt,
    right @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match right.clone() {
        crate::aver_generated::domain::ast::Expr::ExprInt(n) => {
            crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(op.clone(), slot, n)
        }
        crate::aver_generated::domain::ast::Expr::ExprSlot(rhs) => {
            crate::aver_generated::domain::ast::Expr::ExprCmpSlots(op.clone(), slot, rhs)
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalCmp(
            op,
            &crate::aver_generated::domain::ast::Expr::ExprSlot(slot),
            right,
        ),
    }
}

/// Rewrite comparisons with a slot on the right when the left side is a simple integer.
pub fn rewriteInternalCmpSlotRight(
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
    left @ _: &crate::aver_generated::domain::ast::Expr,
    slot @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match left.clone() {
        crate::aver_generated::domain::ast::Expr::ExprInt(n) => {
            crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(
                crate::aver_generated::domain::resolver::rewrite::flipCmp(op),
                slot,
                n,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprSlot(lhs) => {
            crate::aver_generated::domain::ast::Expr::ExprCmpSlots(op.clone(), lhs, slot)
        }
        _ => crate::aver_generated::domain::resolver::rewrite::rebuildInternalCmp(
            op,
            left,
            &crate::aver_generated::domain::ast::Expr::ExprSlot(slot),
        ),
    }
}

/// Rebuild a generic comparison expression when no narrower internal form applies.
pub fn rebuildInternalCmp(
    op @ _: &crate::aver_generated::domain::ast::CmpOp,
    left @ _: &crate::aver_generated::domain::ast::Expr,
    right @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match op {
        crate::aver_generated::domain::ast::CmpOp::CmpEq => {
            crate::aver_generated::domain::ast::Expr::ExprEq(
                std::sync::Arc::new(left.clone()),
                std::sync::Arc::new(right.clone()),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpNeq => {
            crate::aver_generated::domain::ast::Expr::ExprNeq(
                std::sync::Arc::new(left.clone()),
                std::sync::Arc::new(right.clone()),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLt => {
            crate::aver_generated::domain::ast::Expr::ExprLt(
                std::sync::Arc::new(left.clone()),
                std::sync::Arc::new(right.clone()),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGt => {
            crate::aver_generated::domain::ast::Expr::ExprGt(
                std::sync::Arc::new(left.clone()),
                std::sync::Arc::new(right.clone()),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpLte => {
            crate::aver_generated::domain::ast::Expr::ExprLte(
                std::sync::Arc::new(left.clone()),
                std::sync::Arc::new(right.clone()),
            )
        }
        crate::aver_generated::domain::ast::CmpOp::CmpGte => {
            crate::aver_generated::domain::ast::Expr::ExprGte(
                std::sync::Arc::new(left.clone()),
                std::sync::Arc::new(right.clone()),
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
    scrutinee @ _: &crate::aver_generated::domain::ast::Expr,
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::resolver::rewrite::rewriteBoolMatchArms(arms) {
        Some(pair @ _) => {
            let (thenExpr, elseExpr) = pair;
            crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                std::sync::Arc::new(scrutinee.clone()),
                std::sync::Arc::new(thenExpr),
                std::sync::Arc::new(elseExpr),
            )
        }
        None => crate::aver_generated::domain::ast::Expr::ExprMatch(
            std::sync::Arc::new(scrutinee.clone()),
            arms.clone(),
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
        let __list_subject = arms.clone();
        if let Some((arm1, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((arm2, tail)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    if (tail == aver_rt::AverList::empty()) {
                        crate::aver_generated::domain::resolver::rewrite::rewriteBoolMatchArmPair(
                            &arm1, &arm2,
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
    arm1 @ _: &crate::aver_generated::domain::ast::MatchArm,
    arm2 @ _: &crate::aver_generated::domain::ast::MatchArm,
) -> Option<(
    crate::aver_generated::domain::ast::Expr,
    crate::aver_generated::domain::ast::Expr,
)> {
    crate::cancel_checkpoint();
    match arm1.pattern.clone() {
        crate::aver_generated::domain::ast::Pattern::PatBool(b1) => {
            crate::aver_generated::domain::resolver::rewrite::rewriteBoolMatchArmPairInner(
                b1,
                &arm1.body,
                &arm2.pattern,
                &arm2.body,
            )
        }
        _ => None,
    }
}

/// Finish ordering bool arms once the first pattern bool is known.
pub fn rewriteBoolMatchArmPairInner(
    b1 @ _: bool,
    body1 @ _: &crate::aver_generated::domain::ast::Expr,
    p2 @ _: &crate::aver_generated::domain::ast::Pattern,
    body2 @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<(
    crate::aver_generated::domain::ast::Expr,
    crate::aver_generated::domain::ast::Expr,
)> {
    crate::cancel_checkpoint();
    match p2.clone() {
        crate::aver_generated::domain::ast::Pattern::PatBool(b2) => {
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
    body1 @ _: &crate::aver_generated::domain::ast::Expr,
    b2 @ _: bool,
    body2 @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<(
    crate::aver_generated::domain::ast::Expr,
    crate::aver_generated::domain::ast::Expr,
)> {
    crate::cancel_checkpoint();
    match (b1, b2) {
        (true, false) => Some((body1.clone(), body2.clone())),
        (false, true) => Some((body2.clone(), body1.clone())),
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
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&expr));
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
            let __tco1 = aver_rt::list_builder_push(acc, (name, crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&expr)));
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
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::ast::MatchArm { pattern: crate::aver_generated::domain::resolver::rewrite::rewritePattern(&arm.pattern), body: crate::aver_generated::domain::resolver::rewrite::rewriteInternalExpr(&arm.body), bindingSlots: arm.bindingSlots.clone() });
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
            let __tco1 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::resolver::rewrite::rewritePattern(&p));
            pats = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}
