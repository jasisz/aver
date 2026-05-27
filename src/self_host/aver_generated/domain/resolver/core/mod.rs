#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    MaxSlotInExpr(Expr, i64),
    MaxSlotInExprComposite(Expr, i64),
    MaxSlotInExprAggregate(Expr, i64),
    MaxSlotInExprPair(Expr, Expr, i64),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> i64 {
    loop {
        __state = match __state {
            __MutualTco1::MaxSlotInExpr(mut expr, mut acc) => {
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
            __MutualTco1::MaxSlotInExprComposite(mut expr, mut acc) => {
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
            __MutualTco1::MaxSlotInExprAggregate(mut expr, mut acc) => {
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
            __MutualTco1::MaxSlotInExprPair(mut a, mut b, mut acc) => {
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
pub fn maxSlotInExpr(expr: &Expr, acc: i64) -> i64 {
    __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExpr(expr.clone(), acc))
}

/// Continue max-slot traversal for composite expression forms.
pub fn maxSlotInExprComposite(expr: &Expr, acc: i64) -> i64 {
    __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprComposite(expr.clone(), acc))
}

/// Finish max-slot traversal for aggregate and call expression forms.
pub fn maxSlotInExprAggregate(expr: &Expr, acc: i64) -> i64 {
    __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprAggregate(expr.clone(), acc))
}

/// Visit both sides of a binary expression.
pub fn maxSlotInExprPair(a: &Expr, b: &Expr, acc: i64) -> i64 {
    __mutual_tco_trampoline_1(__MutualTco1::MaxSlotInExprPair(a.clone(), b.clone(), acc))
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    ResolveArms(
        aver_rt::AverList<MatchArm>,
        aver_rt::AverMap<AverStr, i64>,
        aver_rt::AverList<MatchArm>,
    ),
    ResolveOneArm(
        MatchArm,
        aver_rt::AverList<MatchArm>,
        aver_rt::AverMap<AverStr, i64>,
        aver_rt::AverList<MatchArm>,
    ),
}

fn __mutual_tco_trampoline_2(
    mut __state: __MutualTco2,
) -> (aver_rt::AverList<MatchArm>, aver_rt::AverMap<AverStr, i64>) {
    loop {
        __state = match __state {
            __MutualTco2::ResolveArms(mut arms, mut slots, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(arms, [] => { return (acc.reverse(), slots) }, [arm, rest] => __MutualTco2::ResolveOneArm(arm, rest, slots, acc))
            }
            __MutualTco2::ResolveOneArm(mut arm, mut rest, mut slots, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::resolver::core::resolveArm(&arm, &slots) {
                    (resolvedArm, mergedSlots) => __MutualTco2::ResolveArms(
                        rest,
                        mergedSlots,
                        aver_rt::AverList::prepend(resolvedArm, &acc),
                    ),
                }
            }
        };
    }
}

/// Resolve match arms, collecting all pattern slots.
pub fn resolveArms(
    arms: &aver_rt::AverList<MatchArm>,
    slots: &aver_rt::AverMap<AverStr, i64>,
    acc: &aver_rt::AverList<MatchArm>,
) -> (aver_rt::AverList<MatchArm>, aver_rt::AverMap<AverStr, i64>) {
    __mutual_tco_trampoline_2(__MutualTco2::ResolveArms(
        arms.clone(),
        slots.clone(),
        acc.clone(),
    ))
}

/// Resolve one match arm and continue.
pub fn resolveOneArm(
    arm: &MatchArm,
    rest: &aver_rt::AverList<MatchArm>,
    slots: &aver_rt::AverMap<AverStr, i64>,
    acc: &aver_rt::AverList<MatchArm>,
) -> (aver_rt::AverList<MatchArm>, aver_rt::AverMap<AverStr, i64>) {
    __mutual_tco_trampoline_2(__MutualTco2::ResolveOneArm(
        arm.clone(),
        rest.clone(),
        slots.clone(),
        acc.clone(),
    ))
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    ResolveStmts(
        aver_rt::AverList<Stmt>,
        aver_rt::AverMap<AverStr, i64>,
        i64,
        aver_rt::AverList<Stmt>,
    ),
    ResolveOneStmt(
        Stmt,
        aver_rt::AverList<Stmt>,
        aver_rt::AverMap<AverStr, i64>,
        i64,
        aver_rt::AverList<Stmt>,
    ),
    ResolveStmtExpr(
        Expr,
        aver_rt::AverList<Stmt>,
        aver_rt::AverMap<AverStr, i64>,
        i64,
        aver_rt::AverList<Stmt>,
    ),
    ResolveStmtBind(
        AverStr,
        Expr,
        aver_rt::AverList<Stmt>,
        aver_rt::AverMap<AverStr, i64>,
        i64,
        aver_rt::AverList<Stmt>,
    ),
    ResolveStmtBindFinish(
        AverStr,
        Expr,
        aver_rt::AverList<Stmt>,
        aver_rt::AverMap<AverStr, i64>,
        i64,
        aver_rt::AverList<Stmt>,
    ),
}

fn __mutual_tco_trampoline_3(
    mut __state: __MutualTco3,
) -> (aver_rt::AverList<Stmt>, i64, aver_rt::AverMap<AverStr, i64>) {
    loop {
        __state = match __state {
            __MutualTco3::ResolveStmts(mut stmts, mut slots, mut next, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(stmts, [] => { return (acc.reverse(), next, slots) }, [s, rest] => __MutualTco3::ResolveOneStmt(s, rest, slots, next, acc))
            }
            __MutualTco3::ResolveOneStmt(mut s, mut rest, mut slots, mut next, mut acc) => {
                crate::cancel_checkpoint();
                match s.clone() {
                    crate::aver_generated::domain::ast::Stmt::StmtBind(name, expr) => {
                        __MutualTco3::ResolveStmtBind(name, expr, rest, slots, next, acc)
                    }
                    crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
                        __MutualTco3::ResolveStmtExpr(expr, rest, slots, next, acc)
                    }
                    crate::aver_generated::domain::ast::Stmt::StmtBindSlot(_, _) => {
                        __MutualTco3::ResolveStmts(
                            rest,
                            slots,
                            next,
                            aver_rt::AverList::prepend(s, &acc),
                        )
                    }
                }
            }
            __MutualTco3::ResolveStmtExpr(mut expr, mut rest, mut slots, mut next, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::resolver::core::resolveExpr(&expr, &slots) {
                    (re, newSlots) => __MutualTco3::ResolveStmts(
                        rest,
                        newSlots.clone(),
                        crate::aver_generated::domain::resolver::core::maxInt(
                            next,
                            (crate::aver_generated::domain::resolver::core::mapMaxVal(&newSlots)
                                + 1i64),
                        ),
                        aver_rt::AverList::prepend(
                            crate::aver_generated::domain::ast::Stmt::StmtExpr(re),
                            &acc,
                        ),
                    ),
                }
            }
            __MutualTco3::ResolveStmtBind(
                mut name,
                mut expr,
                mut rest,
                mut slots,
                mut next,
                mut acc,
            ) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::resolver::core::resolveExpr(&expr, &slots) {
                    (newExpr, exprSlots) => __MutualTco3::ResolveStmtBindFinish(
                        name, newExpr, rest, exprSlots, next, acc,
                    ),
                }
            }
            __MutualTco3::ResolveStmtBindFinish(
                mut name,
                mut newExpr,
                mut rest,
                mut slots,
                mut next,
                mut acc,
            ) => {
                crate::cancel_checkpoint();
                let newSlots = slots.insert_owned(name, next);
                __MutualTco3::ResolveStmts(
                    rest,
                    newSlots,
                    (next + 1i64),
                    aver_rt::AverList::prepend(
                        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(next, newExpr),
                        &acc,
                    ),
                )
            }
        };
    }
}

/// Resolve statements, threading slot assignments.
pub fn resolveStmts(
    stmts: &aver_rt::AverList<Stmt>,
    slots: &aver_rt::AverMap<AverStr, i64>,
    next: i64,
    acc: &aver_rt::AverList<Stmt>,
) -> (aver_rt::AverList<Stmt>, i64, aver_rt::AverMap<AverStr, i64>) {
    __mutual_tco_trampoline_3(__MutualTco3::ResolveStmts(
        stmts.clone(),
        slots.clone(),
        next,
        acc.clone(),
    ))
}

/// Resolve a single statement and continue.
pub fn resolveOneStmt(
    s: &Stmt,
    rest: &aver_rt::AverList<Stmt>,
    slots: &aver_rt::AverMap<AverStr, i64>,
    next: i64,
    acc: &aver_rt::AverList<Stmt>,
) -> (aver_rt::AverList<Stmt>, i64, aver_rt::AverMap<AverStr, i64>) {
    __mutual_tco_trampoline_3(__MutualTco3::ResolveOneStmt(
        s.clone(),
        rest.clone(),
        slots.clone(),
        next,
        acc.clone(),
    ))
}

/// Resolve an expression statement, collecting pattern slots.
pub fn resolveStmtExpr(
    expr: &Expr,
    rest: &aver_rt::AverList<Stmt>,
    slots: &aver_rt::AverMap<AverStr, i64>,
    next: i64,
    acc: &aver_rt::AverList<Stmt>,
) -> (aver_rt::AverList<Stmt>, i64, aver_rt::AverMap<AverStr, i64>) {
    __mutual_tco_trampoline_3(__MutualTco3::ResolveStmtExpr(
        expr.clone(),
        rest.clone(),
        slots.clone(),
        next,
        acc.clone(),
    ))
}

/// Resolve a binding: assign a new slot, replace with StmtBindSlot.
pub fn resolveStmtBind(
    name: AverStr,
    expr: &Expr,
    rest: &aver_rt::AverList<Stmt>,
    slots: &aver_rt::AverMap<AverStr, i64>,
    next: i64,
    acc: &aver_rt::AverList<Stmt>,
) -> (aver_rt::AverList<Stmt>, i64, aver_rt::AverMap<AverStr, i64>) {
    __mutual_tco_trampoline_3(__MutualTco3::ResolveStmtBind(
        name,
        expr.clone(),
        rest.clone(),
        slots.clone(),
        next,
        acc.clone(),
    ))
}

/// Finish resolving a binding after expr is resolved.
pub fn resolveStmtBindFinish(
    name: AverStr,
    newExpr: &Expr,
    rest: &aver_rt::AverList<Stmt>,
    slots: &aver_rt::AverMap<AverStr, i64>,
    next: i64,
    acc: &aver_rt::AverList<Stmt>,
) -> (aver_rt::AverList<Stmt>, i64, aver_rt::AverMap<AverStr, i64>) {
    __mutual_tco_trampoline_3(__MutualTco3::ResolveStmtBindFinish(
        name,
        newExpr.clone(),
        rest.clone(),
        slots.clone(),
        next,
        acc.clone(),
    ))
}

/// Resolve each function definition.
#[inline(always)]
pub fn resolveFns(
    mut fns: aver_rt::AverList<FnDef>,
    mut acc: aver_rt::AverList<FnDef>,
) -> aver_rt::AverList<FnDef> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fns, [] => acc.reverse(), [f, rest] => { {
            let __tmp1 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::core::resolveFn(&f), &acc);
            fns = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Resolve one function: assign slots to params and locals.
pub fn resolveFn(fd: &FnDef) -> FnDef {
    crate::cancel_checkpoint();
    let paramResult = crate::aver_generated::domain::resolver::core::buildParamSlots(
        fd.params.clone(),
        HashMap::new(),
        0i64,
    );
    {
        let (slotMap, nextSlot) = paramResult;
        crate::aver_generated::domain::resolver::core::resolveBody(fd, &slotMap, nextSlot)
    }
}

/// Assign slot indices 0..n-1 to parameters.
#[inline(always)]
pub fn buildParamSlots(
    mut params: aver_rt::AverList<AverStr>,
    mut slots: aver_rt::AverMap<AverStr, i64>,
    mut next: i64,
) -> (aver_rt::AverMap<AverStr, i64>, i64) {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(params, [] => (slots, next), [p, rest] => { {
            let __tmp1 = slots.insert_owned(p, next);
            let __tmp2 = (next + 1i64);
            params = rest;
            slots = __tmp1;
            next = __tmp2;
            continue;
        } });
    }
}

/// Walk body stmts, resolve vars, track new bindings. slotCount = max slot + 1 from body scan.
pub fn resolveBody(fd: &FnDef, slots: &aver_rt::AverMap<AverStr, i64>, nextSlot: i64) -> FnDef {
    crate::cancel_checkpoint();
    let resolved = crate::aver_generated::domain::resolver::core::resolveStmts(
        &fd.body,
        slots,
        nextSlot,
        &aver_rt::AverList::empty(),
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
    fd: &FnDef,
    body: &aver_rt::AverList<Stmt>,
    baseSlot: i64,
    slotMap: &aver_rt::AverMap<AverStr, i64>,
) -> FnDef {
    crate::cancel_checkpoint();
    let maxFromBody = crate::aver_generated::domain::resolver::core::maxSlotInStmts(
        body.clone(),
        (baseSlot - 1i64),
    );
    FnDef {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: body.clone(),
        slotCount: (maxFromBody + 1i64),
        slotMap: slotMap.clone(),
        fastPath: FnFastPath::FastNone.clone(),
        tailLoop: false,
    }
}

/// Find highest slot index in a list of statements.
#[inline(always)]
pub fn maxSlotInStmts(mut stmts: aver_rt::AverList<Stmt>, mut acc: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(stmts, [] => acc, [s, rest] => { {
            let __tmp1 = crate::aver_generated::domain::resolver::core::maxSlotInStmt(&s, acc);
            stmts = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Find highest slot index in a statement.
pub fn maxSlotInStmt(s: &Stmt, acc: i64) -> i64 {
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
pub fn maxSlotInExprs(mut exprs: aver_rt::AverList<Expr>, mut acc: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => acc, [e, rest] => { {
            let __tmp1 = crate::aver_generated::domain::resolver::core::maxSlotInExpr(&e, acc);
            exprs = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Find highest slot index in record fields.
#[inline(always)]
pub fn maxSlotInFields(mut fields: aver_rt::AverList<(AverStr, Expr)>, mut acc: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fields, [] => acc, [pair, rest] => { match pair {
            (_, expr) => {
            let __tmp1 = crate::aver_generated::domain::resolver::core::maxSlotInExpr(&expr, acc);
            fields = rest;
            acc = __tmp1;
            continue;
        }
        } });
    }
}

/// Find highest slot index in match arms.
#[inline(always)]
pub fn maxSlotInArms(mut arms: aver_rt::AverList<MatchArm>, mut acc: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(arms, [] => acc, [arm, rest] => { {
            let __tmp1 = crate::aver_generated::domain::resolver::core::maxSlotInExpr(&arm.body, acc);
            arms = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Return the larger of two integers.
#[inline(always)]
pub fn maxInt(a: i64, b: i64) -> i64 {
    crate::cancel_checkpoint();
    if (a > b) { a } else { b }
}

/// Resolve an expression. Returns (resolved expr, slots with any new pattern bindings).
pub fn resolveExpr(
    expr: &Expr,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
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
    expr: &Expr,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
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
    expr: &Expr,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
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
                crate::aver_generated::domain::resolver::core::resolveExprs(
                    args,
                    slots.clone(),
                    aver_rt::AverList::empty(),
                ),
            ),
            slots.clone(),
        ),
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, args) => (
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                name,
                crate::aver_generated::domain::resolver::core::resolveExprs(
                    args,
                    slots.clone(),
                    aver_rt::AverList::empty(),
                ),
            ),
            slots.clone(),
        ),
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(id, args) => (
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                id,
                crate::aver_generated::domain::resolver::core::resolveExprs(
                    args,
                    slots.clone(),
                    aver_rt::AverList::empty(),
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
    expr: &Expr,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
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
    a: &Expr,
    b: &Expr,
    slots: &aver_rt::AverMap<AverStr, i64>,
    op: AverStr,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
    crate::cancel_checkpoint();
    let ra = crate::aver_generated::domain::resolver::core::resolveExprSimple(a, slots);
    let rb = crate::aver_generated::domain::resolver::core::resolveExprSimple(b, slots);
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
pub fn resolveExprSimple(expr: &Expr, slots: &aver_rt::AverMap<AverStr, i64>) -> Expr {
    crate::cancel_checkpoint();
    {
        let (e, _) = crate::aver_generated::domain::resolver::core::resolveExpr(expr, slots);
        e
    }
}

/// Resolve function call arguments.
pub fn resolveCallExpr(
    name: AverStr,
    args: &aver_rt::AverList<Expr>,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprCall(
            name,
            crate::aver_generated::domain::resolver::core::resolveExprs(
                args.clone(),
                slots.clone(),
                aver_rt::AverList::empty(),
            ),
        ),
        slots.clone(),
    )
}

/// Resolve match expression, collecting pattern slots.
pub fn resolveMatchExpr(
    subj: &Expr,
    arms: &aver_rt::AverList<MatchArm>,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
    crate::cancel_checkpoint();
    let rs = crate::aver_generated::domain::resolver::core::resolveExprSimple(subj, slots);
    let armsResult = crate::aver_generated::domain::resolver::core::resolveArms(
        arms,
        slots,
        &aver_rt::AverList::empty(),
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
    inner: &Expr,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
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
    parts: &aver_rt::AverList<Expr>,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprConcat(
            crate::aver_generated::domain::resolver::core::resolveExprs(
                parts.clone(),
                slots.clone(),
                aver_rt::AverList::empty(),
            ),
        ),
        slots.clone(),
    )
}

/// Resolve tuple expressions.
pub fn resolveTupleExpr(
    exprs: &aver_rt::AverList<Expr>,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprTuple(
            crate::aver_generated::domain::resolver::core::resolveExprs(
                exprs.clone(),
                slots.clone(),
                aver_rt::AverList::empty(),
            ),
        ),
        slots.clone(),
    )
}

/// Resolve independent product expressions.
pub fn resolveIndependentProductExpr(
    exprs: &aver_rt::AverList<Expr>,
    unwrap: bool,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
            crate::aver_generated::domain::resolver::core::resolveExprs(
                exprs.clone(),
                slots.clone(),
                aver_rt::AverList::empty(),
            ),
            unwrap,
        ),
        slots.clone(),
    )
}

/// Resolve list expressions.
pub fn resolveListExpr(
    exprs: &aver_rt::AverList<Expr>,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprList(
            crate::aver_generated::domain::resolver::core::resolveExprs(
                exprs.clone(),
                slots.clone(),
                aver_rt::AverList::empty(),
            ),
        ),
        slots.clone(),
    )
}

/// Resolve record expressions.
pub fn resolveRecordExpr(
    name: AverStr,
    fields: &aver_rt::AverList<(AverStr, Expr)>,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
    crate::cancel_checkpoint();
    (
        crate::aver_generated::domain::ast::Expr::ExprRecord(
            name,
            crate::aver_generated::domain::resolver::core::resolveFields(
                fields.clone(),
                slots.clone(),
                aver_rt::AverList::empty(),
            ),
        ),
        slots.clone(),
    )
}

/// Resolve field access.
pub fn resolveFieldExpr(
    obj: &Expr,
    field: AverStr,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (Expr, aver_rt::AverMap<AverStr, i64>) {
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
pub fn resolveVar(name: AverStr, slots: &aver_rt::AverMap<AverStr, i64>) -> Expr {
    crate::cancel_checkpoint();
    match slots.get(&name).cloned() {
        Some(slot) => crate::aver_generated::domain::ast::Expr::ExprSlot(slot),
        None => crate::aver_generated::domain::ast::Expr::ExprVar(name),
    }
}

/// Resolve a list of expressions.
#[inline(always)]
pub fn resolveExprs(
    mut exprs: aver_rt::AverList<Expr>,
    mut slots: aver_rt::AverMap<AverStr, i64>,
    mut acc: aver_rt::AverList<Expr>,
) -> aver_rt::AverList<Expr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => acc.reverse(), [e, rest] => { {
            let __tmp1 = slots.clone();
            let __tmp2 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::core::resolveExprSimple(&e, &slots), &acc);
            exprs = rest;
            slots = __tmp1;
            acc = __tmp2;
            continue;
        } });
    }
}

/// Resolve record field expressions.
#[inline(always)]
pub fn resolveFields(
    mut fields: aver_rt::AverList<(AverStr, Expr)>,
    mut slots: aver_rt::AverMap<AverStr, i64>,
    mut acc: aver_rt::AverList<(AverStr, Expr)>,
) -> aver_rt::AverList<(AverStr, Expr)> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fields, [] => acc.reverse(), [pair, rest] => { match pair {
            (name, expr) => {
            let __tmp1 = slots.clone();
            let __tmp2 = aver_rt::AverList::prepend((name, crate::aver_generated::domain::resolver::core::resolveExprSimple(&expr, &slots)), &acc);
            fields = rest;
            slots = __tmp1;
            acc = __tmp2;
            continue;
        }
        } });
    }
}

/// Resolve a match arm: extend slots with pattern bindings, then resolve body.
pub fn resolveArm(
    arm: &MatchArm,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (MatchArm, aver_rt::AverMap<AverStr, i64>) {
    crate::cancel_checkpoint();
    let armSlotResult =
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
    arm: &MatchArm,
    newSlots: &aver_rt::AverMap<AverStr, i64>,
    bindingSlots: &aver_rt::AverMap<AverStr, i64>,
) -> (MatchArm, aver_rt::AverMap<AverStr, i64>) {
    crate::cancel_checkpoint();
    {
        let (re, finalSlots) =
            crate::aver_generated::domain::resolver::core::resolveExpr(&arm.body, newSlots);
        (
            MatchArm {
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
    pat: &Pattern,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> aver_rt::AverMap<AverStr, i64> {
    crate::cancel_checkpoint();
    let nextSlot = (crate::aver_generated::domain::resolver::core::mapMaxVal(slots) + 1i64);
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
    pat: &Pattern,
    bindingSlots: &aver_rt::AverMap<AverStr, i64>,
    next: i64,
) -> (aver_rt::AverMap<AverStr, i64>, i64) {
    crate::cancel_checkpoint();
    match pat.clone() {
        crate::aver_generated::domain::ast::Pattern::PatVar(name) => {
            (bindingSlots.clone().insert_owned(name, next), (next + 1i64))
        }
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
    h: AverStr,
    t: AverStr,
    bindingSlots: &aver_rt::AverMap<AverStr, i64>,
    next: i64,
) -> (aver_rt::AverMap<AverStr, i64>, i64) {
    crate::cancel_checkpoint();
    let bindingSlots2 = bindingSlots.clone().insert_owned(h, next);
    (bindingSlots2.insert_owned(t, (next + 1i64)), (next + 2i64))
}

/// Assign slots for constructor-pattern binders.
#[inline(always)]
pub fn patternBindingSlotsConstructor(
    mut bindings: aver_rt::AverList<AverStr>,
    mut bindingSlots: aver_rt::AverMap<AverStr, i64>,
    mut next: i64,
) -> (aver_rt::AverMap<AverStr, i64>, i64) {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(bindings, [] => (bindingSlots, next), [name, rest] => { {
            let __tmp1 = bindingSlots.insert_owned(name, next);
            let __tmp2 = (next + 1i64);
            bindings = rest;
            bindingSlots = __tmp1;
            next = __tmp2;
            continue;
        } });
    }
}

/// Assign slots for tuple sub-patterns.
#[inline(always)]
pub fn patternBindingSlotsTuple(
    mut pats: aver_rt::AverList<Pattern>,
    mut bindingSlots: aver_rt::AverMap<AverStr, i64>,
    mut next: i64,
) -> (aver_rt::AverMap<AverStr, i64>, i64) {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(pats, [] => (bindingSlots, next), [p, rest] => { match crate::aver_generated::domain::resolver::core::patternBindingSlotsInner(&p, &bindingSlots, next) {
            (newBindingSlots, newNext) => {
            pats = rest;
            bindingSlots = newBindingSlots;
            next = newNext;
            continue;
        }
        } });
    }
}

/// Find the next available slot and add pattern bindings.
pub fn addPatternSlots(
    pat: &Pattern,
    slots: &aver_rt::AverMap<AverStr, i64>,
) -> (aver_rt::AverMap<AverStr, i64>, i64) {
    crate::cancel_checkpoint();
    let nextSlot = (crate::aver_generated::domain::resolver::core::mapMaxVal(slots) + 1i64);
    crate::aver_generated::domain::resolver::core::addPatternSlotsInner(pat, slots, nextSlot)
}

/// Get maximum value in a map, or -1 if empty.
#[inline(always)]
pub fn mapMaxVal(m: &aver_rt::AverMap<AverStr, i64>) -> i64 {
    crate::cancel_checkpoint();
    let vals = aver_rt::AverList::from_vec(m.values().cloned().collect::<Vec<_>>());
    crate::aver_generated::domain::resolver::core::maxInList(vals, (0i64 - 1i64))
}

/// Find maximum value in a list.
#[inline(always)]
pub fn maxInList(mut vals: aver_rt::AverList<i64>, mut acc: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(vals, [] => acc, [v, rest] => { if (v > acc) { {
            vals = rest;
            acc = v;
            continue;
        } } else { {
            vals = rest;
            continue;
        } } });
    }
}

/// Add slots for pattern bindings.
pub fn addPatternSlotsInner(
    pat: &Pattern,
    slots: &aver_rt::AverMap<AverStr, i64>,
    next: i64,
) -> (aver_rt::AverMap<AverStr, i64>, i64) {
    crate::cancel_checkpoint();
    match pat.clone() {
        crate::aver_generated::domain::ast::Pattern::PatVar(name) => {
            (slots.clone().insert_owned(name, next), (next + 1i64))
        }
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
    h: AverStr,
    t: AverStr,
    slots: &aver_rt::AverMap<AverStr, i64>,
    next: i64,
) -> (aver_rt::AverMap<AverStr, i64>, i64) {
    crate::cancel_checkpoint();
    let slots2 = slots.clone().insert_owned(h, next);
    (slots2.insert_owned(t, (next + 1i64)), (next + 2i64))
}

/// Add slots for constructor pattern bindings.
#[inline(always)]
pub fn addConstructorSlots(
    mut bindings: aver_rt::AverList<AverStr>,
    mut slots: aver_rt::AverMap<AverStr, i64>,
    mut next: i64,
) -> (aver_rt::AverMap<AverStr, i64>, i64) {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(bindings, [] => (slots, next), [name, rest] => { {
            let __tmp1 = slots.insert_owned(name, next);
            let __tmp2 = (next + 1i64);
            bindings = rest;
            slots = __tmp1;
            next = __tmp2;
            continue;
        } });
    }
}

/// Add slots for tuple sub-patterns.
#[inline(always)]
pub fn addTuplePatternSlots(
    mut pats: aver_rt::AverList<Pattern>,
    mut slots: aver_rt::AverMap<AverStr, i64>,
    mut next: i64,
) -> (aver_rt::AverMap<AverStr, i64>, i64) {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(pats, [] => (slots, next), [p, rest] => { match crate::aver_generated::domain::resolver::core::addPatternSlotsInner(&p, &slots, next) {
            (newSlots, newNext) => {
            pats = rest;
            slots = newSlots;
            next = newNext;
            continue;
        }
        } });
    }
}
