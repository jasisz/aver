#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
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
                match expr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                        cond,
                        thenExpr,
                        elseExpr,
                    ) => {
                        let cond = (*cond).clone();
                        let thenExpr = (*thenExpr).clone();
                        let elseExpr = (*elseExpr).clone();
                        __MutualTco1::MaxSlotInExpr(
                            elseExpr,
                            crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                                &thenExpr,
                                crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                                    &cond, acc,
                                ),
                            ),
                        )
                    }
                    crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
                        return crate::aver_generated::domain::resolver::core::maxInt(slot, acc);
                    }
                    crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(_, slot, _) => {
                        return crate::aver_generated::domain::resolver::core::maxInt(slot, acc);
                    }
                    crate::aver_generated::domain::ast::Expr::ExprBinopSlots(_, lhs, rhs) => {
                        return crate::aver_generated::domain::resolver::core::maxInt(
                            rhs,
                            crate::aver_generated::domain::resolver::core::maxInt(lhs, acc),
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(_, slot, _) => {
                        return crate::aver_generated::domain::resolver::core::maxInt(slot, acc);
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCmpSlots(_, lhs, rhs) => {
                        return crate::aver_generated::domain::resolver::core::maxInt(
                            rhs,
                            crate::aver_generated::domain::resolver::core::maxInt(lhs, acc),
                        );
                    }
                    _ => __MutualTco1::MaxSlotInExprComposite(expr, acc),
                }
            }
            __MutualTco1::MaxSlotInExprComposite(mut expr @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                match expr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                        vecExpr,
                        idxExpr,
                        _,
                    ) => {
                        let vecExpr = (*vecExpr).clone();
                        let idxExpr = (*idxExpr).clone();
                        __MutualTco1::MaxSlotInExpr(
                            idxExpr,
                            crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                                &vecExpr, acc,
                            ),
                        )
                    }
                    crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(a, b, _) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExpr(
                            b,
                            crate::aver_generated::domain::resolver::core::maxSlotInExpr(&a, acc),
                        )
                    }
                    crate::aver_generated::domain::ast::Expr::ExprAdd(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExprPair(a, b, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprSub(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExprPair(a, b, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprMul(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExprPair(a, b, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprDiv(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExprPair(a, b, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprNeg(inner) => {
                        let inner = (*inner).clone();
                        __MutualTco1::MaxSlotInExpr(inner, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprEq(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExprPair(a, b, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprNeq(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExprPair(a, b, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprLt(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExprPair(a, b, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprGt(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExprPair(a, b, acc)
                    }
                    _ => __MutualTco1::MaxSlotInExprAggregate(expr, acc),
                }
            }
            __MutualTco1::MaxSlotInExprAggregate(mut expr @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                match expr {
                    crate::aver_generated::domain::ast::Expr::ExprLte(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExprPair(a, b, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprGte(a, b) => {
                        let a = (*a).clone();
                        let b = (*b).clone();
                        __MutualTco1::MaxSlotInExprPair(a, b, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCall(_, args) => {
                        return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                            args, acc,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCallDirect(_, args) => {
                        return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                            args, acc,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(_, args) => {
                        return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                            args, acc,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(_, args) => {
                        return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                            args, acc,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprMatch(subj, arms) => {
                        let subj = (*subj).clone();
                        return crate::aver_generated::domain::resolver::core::maxSlotInArms(
                            arms,
                            crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                                &subj, acc,
                            ),
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprPropagate(inner) => {
                        let inner = (*inner).clone();
                        __MutualTco1::MaxSlotInExpr(inner, acc)
                    }
                    crate::aver_generated::domain::ast::Expr::ExprConcat(parts) => {
                        return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                            parts, acc,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprTuple(exprs) => {
                        return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                            exprs, acc,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(exprs, _) => {
                        return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                            exprs, acc,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprList(exprs) => {
                        return crate::aver_generated::domain::resolver::core::maxSlotInExprs(
                            exprs, acc,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprRecord(_, fields) => {
                        return crate::aver_generated::domain::resolver::core::maxSlotInFields(
                            fields, acc,
                        );
                    }
                    crate::aver_generated::domain::ast::Expr::ExprFieldAccess(obj, _) => {
                        let obj = (*obj).clone();
                        __MutualTco1::MaxSlotInExpr(obj, acc)
                    }
                    _ => return acc,
                }
            }
            __MutualTco1::MaxSlotInExprPair(mut a @ _, mut b @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                __MutualTco1::MaxSlotInExpr(
                    b,
                    crate::aver_generated::domain::resolver::core::maxSlotInExpr(&a, acc),
                )
            }
        };
    }
}

/// Find highest slot index in an expression tree.
pub fn maxSlotInExpr(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExpr(expr.clone(), acc))
}

/// Continue max-slot traversal for composite expression forms.
pub fn maxSlotInExprComposite(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprComposite(expr.clone(), acc))
}

/// Finish max-slot traversal for aggregate and call expression forms.
pub fn maxSlotInExprAggregate(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprAggregate(expr.clone(), acc))
}

/// Visit both sides of a binary expression.
pub fn maxSlotInExprPair(
    a @ _: &crate::aver_generated::domain::ast::Expr,
    b @ _: &crate::aver_generated::domain::ast::Expr,
    acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc))
}

/// Resolve each function definition.
#[inline(always)]
pub fn resolveFns(
    mut fns @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fns, [] => { return acc.reverse(); }, [f, rest] => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::core::resolveFn(&f), &acc);
            fns = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Resolve one function: assign slots to params and locals.
pub fn resolveFn(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    let paramResult @ _ = crate::aver_generated::domain::resolver::core::buildParamSlots(
        fd.params.clone(),
        HashMap::new(),
        aver_rt::AverInt::from_i64(0),
    );
    {
        let (slotMap, nextSlot) = paramResult;
        crate::aver_generated::domain::resolver::core::resolveBody(fd, &slotMap, nextSlot)
    }
}

/// Assign slot indices 0..n-1 to parameters.
#[inline(always)]
pub fn buildParamSlots(
    mut params @ _: aver_rt::AverList<AverStr>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(params, [] => { return (slots, next); }, [p, rest] => { {
            let __tco0 = rest;
            let __tco1 = slots.insert_owned(p, next.clone());
            let __tco2 = next.add(&aver_rt::AverInt::from_i64(1));
            params = __tco0;
            slots = __tco1;
            next = __tco2;
            continue;
        } })
    }
}

/// Walk body stmts, resolve vars, track new bindings. slotCount = max slot + 1 from body scan.
pub fn resolveBody(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    nextSlot @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    let resolved @ _ = crate::aver_generated::domain::resolver::core::resolveStmts__collected(
        fd.body.clone(),
        slots.clone(),
        nextSlot,
        aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
    );
    {
        let (newBody, finalSlot, finalSlotMap) = resolved;
        crate::aver_generated::domain::resolver::core::computeSlotCount(
            fd,
            &newBody,
            finalSlot,
            &finalSlotMap,
        )
    }
}

/// Compute slotCount as max slot index found in body + 1.
pub fn computeSlotCount(
    fd @ _: &crate::aver_generated::domain::ast::FnDef,
    body @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    baseSlot @ _: aver_rt::AverInt,
    slotMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    let maxFromBody @ _ = crate::aver_generated::domain::resolver::core::maxSlotInStmts(
        body.clone(),
        baseSlot.sub(&aver_rt::AverInt::from_i64(1)),
    );
    crate::aver_generated::domain::ast::FnDef {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: body.clone(),
        slotCount: maxFromBody.add(&aver_rt::AverInt::from_i64(1)),
        slotMap: slotMap.clone(),
        fastPath: crate::aver_generated::domain::ast::FnFastPath::FastNone,
        tailLoop: false,
    }
}

/// Find highest slot index in a list of statements.
#[inline(always)]
pub fn maxSlotInStmts(
    mut stmts @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return acc; }, [s, rest] => { {
            let __tco0 = rest;
            let __tco1 = crate::aver_generated::domain::resolver::core::maxSlotInStmt(&s, acc);
            stmts = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Find highest slot index in a statement.
pub fn maxSlotInStmt(
    s @ _: &crate::aver_generated::domain::ast::Stmt,
    acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    match s.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, expr) => {
            crate::aver_generated::domain::resolver::core::maxSlotInExpr(
                &expr,
                crate::aver_generated::domain::resolver::core::maxInt(slot, acc),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::resolver::core::maxSlotInExpr(&expr, acc)
        }
        crate::aver_generated::domain::ast::Stmt::StmtBind(_, expr) => {
            crate::aver_generated::domain::resolver::core::maxSlotInExpr(&expr, acc)
        }
    }
}

/// Find highest slot index in a list of expressions.
#[inline(always)]
pub fn maxSlotInExprs(
    mut exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return acc; }, [e, rest] => { {
            let __tco0 = rest;
            let __tco1 = crate::aver_generated::domain::resolver::core::maxSlotInExpr(&e, acc);
            exprs = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Find highest slot index in record fields.
#[inline(always)]
pub fn maxSlotInFields(
    mut fields @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return acc; }, [pair, rest] => { { let (_, expr) = pair; {
            let __tco0 = rest;
            let __tco1 = crate::aver_generated::domain::resolver::core::maxSlotInExpr(&expr, acc);
            fields = __tco0;
            acc = __tco1;
            continue;
        } } })
    }
}

/// Find highest slot index in match arms.
#[inline(always)]
pub fn maxSlotInArms(
    mut arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(arms, [] => { return acc; }, [arm, rest] => { {
            let __tco0 = rest;
            let __tco1 = crate::aver_generated::domain::resolver::core::maxSlotInExpr(&arm.body, acc);
            arms = __tco0;
            acc = __tco1;
            continue;
        } })
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
    mut stmts @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    aver_rt::AverInt,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return (acc.reverse(), next, slots); }, [s, rest] => { match s.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtBind(__stp1, __stp0) => {
            { let (__stp3, __stp2) = crate::aver_generated::domain::resolver::core::resolveExpr(&__stp0, &slots); { let __stp4 = __stp2.insert_owned(__stp1, next.clone()); {
            let __tco0 = rest;
            let __tco1 = __stp4;
            let __tco2 = next.add(&aver_rt::AverInt::from_i64(1));
            let __tco3 = aver_rt::AverList::prepend(crate::aver_generated::domain::ast::Stmt::StmtBindSlot(next, __stp3), &acc);
            stmts = __tco0;
            slots = __tco1;
            next = __tco2;
            acc = __tco3;
            continue;
        } } }
        },
        crate::aver_generated::domain::ast::Stmt::StmtExpr(__stp0) => {
            { let (__stp6, __stp5) = crate::aver_generated::domain::resolver::core::resolveExpr(&__stp0, &slots); {
            let __tco0 = rest;
            let __tco1 = __stp5.clone();
            let __tco2 = crate::aver_generated::domain::resolver::core::maxInt(next, crate::aver_generated::domain::resolver::core::mapMaxVal(&__stp5).add(&aver_rt::AverInt::from_i64(1)));
            let __tco3 = aver_rt::AverList::prepend(crate::aver_generated::domain::ast::Stmt::StmtExpr(__stp6), &acc);
            stmts = __tco0;
            slots = __tco1;
            next = __tco2;
            acc = __tco3;
            continue;
        } }
        },
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(_, _) => {
            {
            let __tco0 = rest;
            let __tco3 = aver_rt::AverList::prepend(s, &acc);
            stmts = __tco0;
            acc = __tco3;
            continue;
        }
        }
    } })
    }
}

/// Resolve a single statement and continue.
pub fn resolveOneStmt(
    s @ _: &crate::aver_generated::domain::ast::Stmt,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    aver_rt::AverInt,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    match s.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtBind(name, expr) => {
            crate::aver_generated::domain::resolver::core::resolveStmtBind(
                name, &expr, rest, slots, next, acc,
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::resolver::core::resolveStmtExpr(
                &expr, rest, slots, next, acc,
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(_, _) => {
            crate::aver_generated::domain::resolver::core::resolveStmts(
                rest.clone(),
                slots.clone(),
                next,
                aver_rt::AverList::prepend(s.clone(), &acc.clone()),
            )
        }
    }
}

/// Resolve an expression statement, collecting pattern slots.
pub fn resolveStmtExpr(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
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
            rest.clone(),
            newSlots.clone(),
            crate::aver_generated::domain::resolver::core::maxInt(
                next,
                crate::aver_generated::domain::resolver::core::mapMaxVal(&newSlots)
                    .add(&aver_rt::AverInt::from_i64(1)),
            ),
            aver_rt::AverList::prepend(
                crate::aver_generated::domain::ast::Stmt::StmtExpr(re),
                &acc.clone(),
            ),
        )
    }
}

/// Resolve a binding: assign a new slot, replace with StmtBindSlot.
pub fn resolveStmtBind(
    name @ _: AverStr,
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
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
            name, &newExpr, rest, &exprSlots, next, acc,
        )
    }
}

/// Finish resolving a binding after expr is resolved.
pub fn resolveStmtBindFinish(
    name @ _: AverStr,
    newExpr @ _: &crate::aver_generated::domain::ast::Expr,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    aver_rt::AverInt,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    let newSlots @ _ = slots.clone().insert_owned(name, next.clone());
    crate::aver_generated::domain::resolver::core::resolveStmts(
        rest.clone(),
        newSlots,
        next.add(&aver_rt::AverInt::from_i64(1)),
        aver_rt::AverList::prepend(
            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(next, newExpr.clone()),
            &acc.clone(),
        ),
    )
}

/// Resolve an expression. Returns (resolved expr, slots with any new pattern bindings).
pub fn resolveExpr(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprBoolBranch(cond, thenExpr, elseExpr) => {
            let cond = (*cond).clone();
            let thenExpr = (*thenExpr).clone();
            let elseExpr = (*elseExpr).clone();
            (
                crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                    std::sync::Arc::new(
                        crate::aver_generated::domain::resolver::core::resolveExprSimple(
                            &cond, slots,
                        ),
                    ),
                    std::sync::Arc::new(
                        crate::aver_generated::domain::resolver::core::resolveExprSimple(
                            &thenExpr, slots,
                        ),
                    ),
                    std::sync::Arc::new(
                        crate::aver_generated::domain::resolver::core::resolveExprSimple(
                            &elseExpr, slots,
                        ),
                    ),
                ),
                slots.clone(),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprVar(name) => (
            crate::aver_generated::domain::resolver::core::resolveVar(name, slots),
            slots.clone(),
        ),
        crate::aver_generated::domain::ast::Expr::ExprInt(_) => (expr.clone(), slots.clone()),
        crate::aver_generated::domain::ast::Expr::ExprFloat(_) => (expr.clone(), slots.clone()),
        crate::aver_generated::domain::ast::Expr::ExprStr(_) => (expr.clone(), slots.clone()),
        crate::aver_generated::domain::ast::Expr::ExprBool(_) => (expr.clone(), slots.clone()),
        crate::aver_generated::domain::ast::Expr::ExprSlot(_) => (expr.clone(), slots.clone()),
        _ => crate::aver_generated::domain::resolver::core::resolveExprAfterLeaf(expr, slots),
    }
}

/// Continue expression resolution after simple leaf forms.
pub fn resolveExprAfterLeaf(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprBinopSlotInt(_, _, _) => {
            (expr.clone(), slots.clone())
        }
        crate::aver_generated::domain::ast::Expr::ExprBinopSlots(_, _, _) => {
            (expr.clone(), slots.clone())
        }
        crate::aver_generated::domain::ast::Expr::ExprCmpSlotInt(_, _, _) => {
            (expr.clone(), slots.clone())
        }
        crate::aver_generated::domain::ast::Expr::ExprCmpSlots(_, _, _) => {
            (expr.clone(), slots.clone())
        }
        crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
            vecExpr,
            idxExpr,
            defaultValue,
        ) => {
            let vecExpr = (*vecExpr).clone();
            let idxExpr = (*idxExpr).clone();
            (
                crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                    std::sync::Arc::new(
                        crate::aver_generated::domain::resolver::core::resolveExprSimple(
                            &vecExpr, slots,
                        ),
                    ),
                    std::sync::Arc::new(
                        crate::aver_generated::domain::resolver::core::resolveExprSimple(
                            &idxExpr, slots,
                        ),
                    ),
                    defaultValue,
                ),
                slots.clone(),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(a, b, defaultValue) => {
            let a = (*a).clone();
            let b = (*b).clone();
            (
                crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                    std::sync::Arc::new(
                        crate::aver_generated::domain::resolver::core::resolveExprSimple(&a, slots),
                    ),
                    std::sync::Arc::new(
                        crate::aver_generated::domain::resolver::core::resolveExprSimple(&b, slots),
                    ),
                    defaultValue,
                ),
                slots.clone(),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprAdd(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::core::resolveBinExpr(
                &a,
                &b,
                slots,
                AverStr::from("add"),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprSub(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::core::resolveBinExpr(
                &a,
                &b,
                slots,
                AverStr::from("sub"),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprMul(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::core::resolveBinExpr(
                &a,
                &b,
                slots,
                AverStr::from("mul"),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprDiv(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::core::resolveBinExpr(
                &a,
                &b,
                slots,
                AverStr::from("div"),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprNeg(inner) => {
            let inner = (*inner).clone();
            (
                crate::aver_generated::domain::ast::Expr::ExprNeg(std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::core::resolveExprSimple(&inner, slots),
                )),
                slots.clone(),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::core::resolveBinExpr(
                &a,
                &b,
                slots,
                AverStr::from("eq"),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprNeq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::core::resolveBinExpr(
                &a,
                &b,
                slots,
                AverStr::from("neq"),
            )
        }
        _ => crate::aver_generated::domain::resolver::core::resolveExprAfterArith(expr, slots),
    }
}

/// Continue expression resolution after arithmetic forms.
pub fn resolveExprAfterArith(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::core::resolveBinExpr(
                &a,
                &b,
                slots,
                AverStr::from("lt"),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::core::resolveBinExpr(
                &a,
                &b,
                slots,
                AverStr::from("gt"),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::core::resolveBinExpr(
                &a,
                &b,
                slots,
                AverStr::from("lte"),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::core::resolveBinExpr(
                &a,
                &b,
                slots,
                AverStr::from("gte"),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCall(name, args) => {
            crate::aver_generated::domain::resolver::core::resolveCallExpr(name, &args, slots)
        }
        crate::aver_generated::domain::ast::Expr::ExprCallDirect(fnId, args) => (
            crate::aver_generated::domain::ast::Expr::ExprCallDirect(
                fnId,
                crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                    args,
                    slots.clone(),
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            ),
            slots.clone(),
        ),
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, args) => (
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                name,
                crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                    args,
                    slots.clone(),
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            ),
            slots.clone(),
        ),
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(id, args) => (
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                id,
                crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                    args,
                    slots.clone(),
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            ),
            slots.clone(),
        ),
        crate::aver_generated::domain::ast::Expr::ExprMatch(subj, arms) => {
            let subj = (*subj).clone();
            crate::aver_generated::domain::resolver::core::resolveMatchExpr(&subj, &arms, slots)
        }
        crate::aver_generated::domain::ast::Expr::ExprPropagate(inner) => {
            let inner = (*inner).clone();
            crate::aver_generated::domain::resolver::core::resolvePropExpr(&inner, slots)
        }
        crate::aver_generated::domain::ast::Expr::ExprConcat(parts) => {
            crate::aver_generated::domain::resolver::core::resolveConcatExpr(&parts, slots)
        }
        crate::aver_generated::domain::ast::Expr::ExprTuple(exprs) => {
            crate::aver_generated::domain::resolver::core::resolveTupleExpr(&exprs, slots)
        }
        _ => crate::aver_generated::domain::resolver::core::resolveExprAfterAggregate(expr, slots),
    }
}

/// Finish expression resolution for aggregate forms.
pub fn resolveExprAfterAggregate(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(exprs, unwrap) => {
            crate::aver_generated::domain::resolver::core::resolveIndependentProductExpr(
                &exprs, unwrap, slots,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprList(exprs) => {
            crate::aver_generated::domain::resolver::core::resolveListExpr(&exprs, slots)
        }
        crate::aver_generated::domain::ast::Expr::ExprRecord(name, fields) => {
            crate::aver_generated::domain::resolver::core::resolveRecordExpr(name, &fields, slots)
        }
        crate::aver_generated::domain::ast::Expr::ExprFieldAccess(obj, field) => {
            let obj = (*obj).clone();
            crate::aver_generated::domain::resolver::core::resolveFieldExpr(&obj, field, slots)
        }
        _ => (expr.clone(), slots.clone()),
    }
}

/// Resolve binary expression and rebuild.
#[inline(always)]
pub fn resolveBinExpr(
    a @ _: &crate::aver_generated::domain::ast::Expr,
    b @ _: &crate::aver_generated::domain::ast::Expr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    op @ _: AverStr,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    let ra @ _ = crate::aver_generated::domain::resolver::core::resolveExprSimple(a, slots);
    let rb @ _ = crate::aver_generated::domain::resolver::core::resolveExprSimple(b, slots);
    {
        let __dispatch_subject = op;
        if &*__dispatch_subject == "add" {
            (
                crate::aver_generated::domain::ast::Expr::ExprAdd(
                    std::sync::Arc::new(ra),
                    std::sync::Arc::new(rb),
                ),
                slots.clone(),
            )
        } else {
            if &*__dispatch_subject == "sub" {
                (
                    crate::aver_generated::domain::ast::Expr::ExprSub(
                        std::sync::Arc::new(ra),
                        std::sync::Arc::new(rb),
                    ),
                    slots.clone(),
                )
            } else {
                if &*__dispatch_subject == "mul" {
                    (
                        crate::aver_generated::domain::ast::Expr::ExprMul(
                            std::sync::Arc::new(ra),
                            std::sync::Arc::new(rb),
                        ),
                        slots.clone(),
                    )
                } else {
                    if &*__dispatch_subject == "div" {
                        (
                            crate::aver_generated::domain::ast::Expr::ExprDiv(
                                std::sync::Arc::new(ra),
                                std::sync::Arc::new(rb),
                            ),
                            slots.clone(),
                        )
                    } else {
                        if &*__dispatch_subject == "eq" {
                            (
                                crate::aver_generated::domain::ast::Expr::ExprEq(
                                    std::sync::Arc::new(ra),
                                    std::sync::Arc::new(rb),
                                ),
                                slots.clone(),
                            )
                        } else {
                            if &*__dispatch_subject == "neq" {
                                (
                                    crate::aver_generated::domain::ast::Expr::ExprNeq(
                                        std::sync::Arc::new(ra),
                                        std::sync::Arc::new(rb),
                                    ),
                                    slots.clone(),
                                )
                            } else {
                                if &*__dispatch_subject == "lt" {
                                    (
                                        crate::aver_generated::domain::ast::Expr::ExprLt(
                                            std::sync::Arc::new(ra),
                                            std::sync::Arc::new(rb),
                                        ),
                                        slots.clone(),
                                    )
                                } else {
                                    if &*__dispatch_subject == "gt" {
                                        (
                                            crate::aver_generated::domain::ast::Expr::ExprGt(
                                                std::sync::Arc::new(ra),
                                                std::sync::Arc::new(rb),
                                            ),
                                            slots.clone(),
                                        )
                                    } else {
                                        if &*__dispatch_subject == "lte" {
                                            (
                                                crate::aver_generated::domain::ast::Expr::ExprLte(
                                                    std::sync::Arc::new(ra),
                                                    std::sync::Arc::new(rb),
                                                ),
                                                slots.clone(),
                                            )
                                        } else {
                                            if &*__dispatch_subject == "gte" {
                                                (crate::aver_generated::domain::ast::Expr::ExprGte(std::sync::Arc::new(ra), std::sync::Arc::new(rb)), slots.clone())
                                            } else {
                                                (crate::aver_generated::domain::ast::Expr::ExprAdd(std::sync::Arc::new(ra), std::sync::Arc::new(rb)), slots.clone())
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
    expr @ _: &crate::aver_generated::domain::ast::Expr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
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
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprCall(
            name,
            crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                args.clone(),
                slots.clone(),
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
            ),
        ),
        slots.clone(),
    )
}

/// Resolve match expression, collecting pattern slots.
pub fn resolveMatchExpr(
    subj @ _: &crate::aver_generated::domain::ast::Expr,
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    let rs @ _ = crate::aver_generated::domain::resolver::core::resolveExprSimple(subj, slots);
    let armsResult @ _ = crate::aver_generated::domain::resolver::core::resolveArms__collected(
        arms.clone(),
        slots.clone(),
        aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
    );
    {
        let (resolvedArms, mergedSlots) = armsResult;
        (
            crate::aver_generated::domain::ast::Expr::ExprMatch(
                std::sync::Arc::new(rs),
                resolvedArms,
            ),
            mergedSlots,
        )
    }
}

/// Resolve ? propagation.
pub fn resolvePropExpr(
    inner @ _: &crate::aver_generated::domain::ast::Expr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprPropagate(std::sync::Arc::new(
            crate::aver_generated::domain::resolver::core::resolveExprSimple(inner, slots),
        )),
        slots.clone(),
    )
}

/// Resolve string concatenation parts.
pub fn resolveConcatExpr(
    parts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprConcat(
            crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                parts.clone(),
                slots.clone(),
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
            ),
        ),
        slots.clone(),
    )
}

/// Resolve tuple expressions.
pub fn resolveTupleExpr(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprTuple(
            crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                exprs.clone(),
                slots.clone(),
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
            ),
        ),
        slots.clone(),
    )
}

/// Resolve independent product expressions.
pub fn resolveIndependentProductExpr(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    unwrap @ _: bool,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
            crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                exprs.clone(),
                slots.clone(),
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
            ),
            unwrap,
        ),
        slots.clone(),
    )
}

/// Resolve list expressions.
pub fn resolveListExpr(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprList(
            crate::aver_generated::domain::resolver::core::resolveExprs__collected(
                exprs.clone(),
                slots.clone(),
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
            ),
        ),
        slots.clone(),
    )
}

/// Resolve record expressions.
pub fn resolveRecordExpr(
    name @ _: AverStr,
    fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprRecord(
            name,
            crate::aver_generated::domain::resolver::core::resolveFields__collected(
                fields.clone(),
                slots.clone(),
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
            ),
        ),
        slots.clone(),
    )
}

/// Resolve field access.
pub fn resolveFieldExpr(
    obj @ _: &crate::aver_generated::domain::ast::Expr,
    field @ _: AverStr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::Expr,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
            std::sync::Arc::new(
                crate::aver_generated::domain::resolver::core::resolveExprSimple(obj, slots),
            ),
            field,
        ),
        slots.clone(),
    )
}

/// Resolve a variable: local vars become ExprSlot, others stay ExprVar.
#[inline(always)]
pub fn resolveVar(
    name @ _: AverStr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match slots.get(&name).cloned() {
        Some(slot @ _) => crate::aver_generated::domain::ast::Expr::ExprSlot(slot),
        None => crate::aver_generated::domain::ast::Expr::ExprVar(name),
    }
}

/// Resolve a list of expressions.
#[inline(always)]
pub fn resolveExprs(
    mut exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    let slots @ _ = std::sync::Arc::new(slots);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return acc.reverse(); }, [e, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::core::resolveExprSimple(&e, &*slots), &acc);
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
    slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    let slots @ _ = std::sync::Arc::new(slots);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return acc.reverse(); }, [pair, rest] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend((name, crate::aver_generated::domain::resolver::core::resolveExprSimple(&expr, &*slots)), &acc);
            fields = __tco0;
            acc = __tco2;
            continue;
        } } })
    }
}

/// Resolve match arms, collecting all pattern slots.
#[inline(always)]
pub fn resolveArms(
    mut arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    let slots @ _ = std::sync::Arc::new(slots);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(arms, [] => { return (acc.reverse(), (*slots).clone()); }, [arm, rest] => { { let (__stp0, _) = crate::aver_generated::domain::resolver::core::resolveArm(&arm, &*slots); {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(__stp0, &acc);
            arms = __tco0;
            acc = __tco2;
            continue;
        } } })
    }
}

/// Resolve one match arm; continue with the ORIGINAL slots, since an arm's pattern binders are scoped to its own body. Threading the arm-extended map onward leaked binder slots into later arms and past the match, where a wrapping binding aliased them and shadowed names resolved wrong.
pub fn resolveOneArm(
    arm @ _: &crate::aver_generated::domain::ast::MatchArm,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    {
        let (resolvedArm, _) =
            crate::aver_generated::domain::resolver::core::resolveArm(arm, slots);
        crate::aver_generated::domain::resolver::core::resolveArms(
            rest.clone(),
            slots.clone(),
            aver_rt::AverList::prepend(resolvedArm, &acc.clone()),
        )
    }
}

/// Resolve a match arm: extend slots with pattern bindings, then resolve body.
pub fn resolveArm(
    arm @ _: &crate::aver_generated::domain::ast::MatchArm,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::MatchArm,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    let armSlotResult @ _ =
        crate::aver_generated::domain::resolver::core::addPatternSlots(&arm.pattern, slots);
    {
        let (newSlots, _) = armSlotResult;
        crate::aver_generated::domain::resolver::core::resolveArmBody(
            arm,
            &newSlots,
            &crate::aver_generated::domain::resolver::core::patternBindingSlots(
                &arm.pattern,
                slots,
            ),
        )
    }
}

/// Resolve arm body with extended slots.
pub fn resolveArmBody(
    arm @ _: &crate::aver_generated::domain::ast::MatchArm,
    newSlots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    bindingSlots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    crate::aver_generated::domain::ast::MatchArm,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    crate::cancel_checkpoint();
    {
        let (re, finalSlots) =
            crate::aver_generated::domain::resolver::core::resolveExpr(&arm.body, newSlots);
        (
            crate::aver_generated::domain::ast::MatchArm {
                pattern: arm.pattern.clone(),
                body: re,
                bindingSlots: bindingSlots.clone(),
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
        let (bindingSlots, _) =
            crate::aver_generated::domain::resolver::core::patternBindingSlotsInner(
                pat,
                &HashMap::new(),
                nextSlot,
            );
        bindingSlots
    }
}

/// Assign slots to pattern binders without carrying the outer slot map.
pub fn patternBindingSlotsInner(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
    bindingSlots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    crate::cancel_checkpoint();
    match pat.clone() {
        crate::aver_generated::domain::ast::Pattern::PatVar(name) => (
            bindingSlots.clone().insert_owned(name, next.clone()),
            next.add(&aver_rt::AverInt::from_i64(1)),
        ),
        crate::aver_generated::domain::ast::Pattern::PatCons(h, t) => {
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
                bindingSlots.clone(),
                next,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructorId(_, _, bindings) => {
            crate::aver_generated::domain::resolver::core::patternBindingSlotsConstructor(
                bindings,
                bindingSlots.clone(),
                next,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatTuple(pats) => {
            crate::aver_generated::domain::resolver::core::patternBindingSlotsTuple(
                pats,
                bindingSlots.clone(),
                next,
            )
        }
        _ => (bindingSlots.clone(), next),
    }
}

/// Assign slots for cons-pattern binders.
pub fn patternBindingSlotsCons(
    h @ _: AverStr,
    t @ _: AverStr,
    bindingSlots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    crate::cancel_checkpoint();
    let bindingSlots2 @ _ = bindingSlots.clone().insert_owned(h, next.clone());
    (
        bindingSlots2.insert_owned(t, next.add(&aver_rt::AverInt::from_i64(1))),
        next.add(&aver_rt::AverInt::from_i64(2)),
    )
}

/// Assign slots for constructor-pattern binders.
#[inline(always)]
pub fn patternBindingSlotsConstructor(
    mut bindings @ _: aver_rt::AverList<AverStr>,
    mut bindingSlots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(bindings, [] => { return (bindingSlots, next); }, [name, rest] => { {
            let __tco0 = rest;
            let __tco1 = bindingSlots.insert_owned(name, next.clone());
            let __tco2 = next.add(&aver_rt::AverInt::from_i64(1));
            bindings = __tco0;
            bindingSlots = __tco1;
            next = __tco2;
            continue;
        } })
    }
}

/// Assign slots for tuple sub-patterns.
#[inline(always)]
pub fn patternBindingSlotsTuple(
    mut pats @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    mut bindingSlots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(pats, [] => { return (bindingSlots, next); }, [p, rest] => { { let (newBindingSlots, newNext) = crate::aver_generated::domain::resolver::core::patternBindingSlotsInner(&p, &bindingSlots, next); {
            let __tco0 = rest;
            let __tco1 = newBindingSlots;
            let __tco2 = newNext;
            pats = __tco0;
            bindingSlots = __tco1;
            next = __tco2;
            continue;
        } } })
    }
}

/// Find the next available slot and add pattern bindings.
pub fn addPatternSlots(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    crate::cancel_checkpoint();
    let nextSlot @ _ = crate::aver_generated::domain::resolver::core::mapMaxVal(slots)
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
    crate::aver_generated::domain::resolver::core::maxInList(vals, aver_rt::AverInt::from_i64(-1))
}

/// Find maximum value in a list.
#[inline(always)]
pub fn maxInList(
    mut vals @ _: aver_rt::AverIntList,
    mut acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(vals, [] => { return acc; }, [v, rest] => { if (v > acc) { {
            let __tco0 = rest;
            let __tco1 = v;
            vals = __tco0;
            acc = __tco1;
            continue;
        } } else { {
            let __tco0 = rest;
            vals = __tco0;
            continue;
        } } })
    }
}

/// Add slots for pattern bindings.
pub fn addPatternSlotsInner(
    pat @ _: &crate::aver_generated::domain::ast::Pattern,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    crate::cancel_checkpoint();
    match pat.clone() {
        crate::aver_generated::domain::ast::Pattern::PatVar(name) => (
            slots.clone().insert_owned(name, next.clone()),
            next.add(&aver_rt::AverInt::from_i64(1)),
        ),
        crate::aver_generated::domain::ast::Pattern::PatCons(h, t) => {
            crate::aver_generated::domain::resolver::core::addConsSlots(h, t, slots, next)
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructor(_, bindings) => {
            crate::aver_generated::domain::resolver::core::addConstructorSlots(
                bindings,
                slots.clone(),
                next,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatConstructorId(_, _, bindings) => {
            crate::aver_generated::domain::resolver::core::addConstructorSlots(
                bindings,
                slots.clone(),
                next,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatTuple(pats) => {
            crate::aver_generated::domain::resolver::core::addTuplePatternSlots(
                pats,
                slots.clone(),
                next,
            )
        }
        _ => (slots.clone(), next),
    }
}

/// Add slots for cons pattern [h, ..t].
pub fn addConsSlots(
    h @ _: AverStr,
    t @ _: AverStr,
    slots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    crate::cancel_checkpoint();
    let slots2 @ _ = slots.clone().insert_owned(h, next.clone());
    (
        slots2.insert_owned(t, next.add(&aver_rt::AverInt::from_i64(1))),
        next.add(&aver_rt::AverInt::from_i64(2)),
    )
}

/// Add slots for constructor pattern bindings.
#[inline(always)]
pub fn addConstructorSlots(
    mut bindings @ _: aver_rt::AverList<AverStr>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(bindings, [] => { return (slots, next); }, [name, rest] => { {
            let __tco0 = rest;
            let __tco1 = slots.insert_owned(name, next.clone());
            let __tco2 = next.add(&aver_rt::AverInt::from_i64(1));
            bindings = __tco0;
            slots = __tco1;
            next = __tco2;
            continue;
        } })
    }
}

/// Add slots for tuple sub-patterns.
#[inline(always)]
pub fn addTuplePatternSlots(
    mut pats @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
) -> (
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    aver_rt::AverInt,
) {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(pats, [] => { return (slots, next); }, [p, rest] => { { let (newSlots, newNext) = crate::aver_generated::domain::resolver::core::addPatternSlotsInner(&p, &slots, next); {
            let __tco0 = rest;
            let __tco1 = newSlots;
            let __tco2 = newNext;
            pats = __tco0;
            slots = __tco1;
            next = __tco2;
            continue;
        } } })
    }
}

/// Synthesized collecting variant of `resolveStmts`. Appends to a builder where `resolveStmts` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveStmts__collected(
    mut stmts @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut next @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    aver_rt::AverInt,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return (aver_rt::list_builder_finalize(acc), next, slots); }, [s, rest] => { match s.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtBind(__stp1, __stp0) => {
            { let (__stp3, __stp2) = crate::aver_generated::domain::resolver::core::resolveExpr(&__stp0, &slots); { let __stp4 = __stp2.insert_owned(__stp1, next.clone()); {
            let __tco0 = rest;
            let __tco1 = __stp4;
            let __tco2 = next.add(&aver_rt::AverInt::from_i64(1));
            let __tco3 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::ast::Stmt::StmtBindSlot(next, __stp3));
            stmts = __tco0;
            slots = __tco1;
            next = __tco2;
            acc = __tco3;
            continue;
        } } }
        },
        crate::aver_generated::domain::ast::Stmt::StmtExpr(__stp0) => {
            { let (__stp6, __stp5) = crate::aver_generated::domain::resolver::core::resolveExpr(&__stp0, &slots); {
            let __tco0 = rest;
            let __tco1 = __stp5.clone();
            let __tco2 = crate::aver_generated::domain::resolver::core::maxInt(next, crate::aver_generated::domain::resolver::core::mapMaxVal(&__stp5).add(&aver_rt::AverInt::from_i64(1)));
            let __tco3 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::ast::Stmt::StmtExpr(__stp6));
            stmts = __tco0;
            slots = __tco1;
            next = __tco2;
            acc = __tco3;
            continue;
        } }
        },
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(_, _) => {
            {
            let __tco0 = rest;
            let __tco3 = aver_rt::list_builder_push(acc, s);
            stmts = __tco0;
            acc = __tco3;
            continue;
        }
        }
    } })
    }
}

/// Synthesized collecting variant of `resolveExprs`. Appends to a builder where `resolveExprs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveExprs__collected(
    mut exprs @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    let slots @ _ = std::sync::Arc::new(slots);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return aver_rt::list_builder_finalize(acc); }, [e, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::resolver::core::resolveExprSimple(&e, &*slots));
            exprs = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `resolveFields`. Appends to a builder where `resolveFields` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveFields__collected(
    mut fields @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    let slots @ _ = std::sync::Arc::new(slots);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return aver_rt::list_builder_finalize(acc); }, [pair, rest] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, (name, crate::aver_generated::domain::resolver::core::resolveExprSimple(&expr, &*slots)));
            fields = __tco0;
            acc = __tco2;
            continue;
        } } })
    }
}

/// Synthesized collecting variant of `resolveArms`. Appends to a builder where `resolveArms` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn resolveArms__collected(
    mut arms @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    slots @ _: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> (
    aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) {
    let slots @ _ = std::sync::Arc::new(slots);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(arms, [] => { return (aver_rt::list_builder_finalize(acc), (*slots).clone()); }, [arm, rest] => { { let (__stp0, _) = crate::aver_generated::domain::resolver::core::resolveArm(&arm, &*slots); {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, __stp0);
            arms = __tco0;
            acc = __tco2;
            continue;
        } } })
    }
}
