#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::*;

/// Tag simple single-expression functions so eval can skip the stmt walker.
#[inline(always)]
pub fn annotateFastFns(
    mut fns: aver_rt::AverList<FnDef>,
    mut fnMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc: aver_rt::AverList<FnDef>,
) -> aver_rt::AverList<FnDef> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fns, [] => { return acc.reverse(); }, [f, rest] => { {
            let __tco0 = rest;
            let __tco1 = fnMap.clone();
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::fast::annotateFastFn(&f, &fnMap), &acc);
            fns = __tco0;
            fnMap = __tco1;
            acc = __tco2;
            continue;
        } })
    }
}

/// Attach a narrow fast-path tag to a function definition.
pub fn annotateFastFn(fd: &FnDef, fnMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>) -> FnDef {
    crate::cancel_checkpoint();
    let selfId = fnMap
        .get(&fd.name)
        .cloned()
        .unwrap_or(aver_rt::AverInt::from_i64(-1));
    crate::aver_generated::domain::ast::FnDef {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: fd.body.clone(),
        slotCount: fd.slotCount.clone(),
        slotMap: fd.slotMap.clone(),
        fastPath: crate::aver_generated::domain::resolver::fast::classifyFastPath(&fd.body),
        tailLoop: crate::aver_generated::domain::resolver::fast::classifyTailLoop(
            selfId,
            fd.body.clone(),
        ),
    }
}

/// Precompute whether the final expression position can self-tail-call directly.
#[inline(always)]
pub fn classifyTailLoop(mut selfId: aver_rt::AverInt, mut body: aver_rt::AverList<Stmt>) -> bool {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(body, [] => { return false; }, [stmt, rest] => { { let __list_subject = rest.clone(); if __list_subject.is_empty() { return crate::aver_generated::domain::resolver::fast::stmtNeedsTailLoop(selfId, &stmt); } else { {
            let __tco1 = rest;
            body = __tco1;
            continue;
        } } } })
    }
}

/// Only the final expression statement can trigger the tail-loop slot evaluator.
pub fn stmtNeedsTailLoop(selfId: aver_rt::AverInt, stmt: &Stmt) -> bool {
    crate::cancel_checkpoint();
    match stmt.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::resolver::fast::exprNeedsTailLoop(selfId, expr)
        }
        _ => false,
    }
}

/// Recognize direct self-calls in tail position, including bool branches and matches.
pub fn exprNeedsTailLoop(mut selfId: aver_rt::AverInt, mut expr: Expr) -> bool {
    loop {
        crate::cancel_checkpoint();
        match expr {
            crate::aver_generated::domain::ast::Expr::ExprCallDirect(fnId, _) => {
                return (fnId == selfId);
            }
            crate::aver_generated::domain::ast::Expr::ExprBoolBranch(_, thenExpr, elseExpr) => {
                let thenExpr = (*thenExpr).clone();
                let elseExpr = (*elseExpr).clone();
                if crate::aver_generated::domain::resolver::fast::exprNeedsTailLoop(
                    selfId.clone(),
                    thenExpr,
                ) {
                    return true;
                } else {
                    {
                        let __tco1 = elseExpr;
                        expr = __tco1;
                        continue;
                    }
                }
            }
            crate::aver_generated::domain::ast::Expr::ExprMatch(_, arms) => {
                return crate::aver_generated::domain::resolver::fast::armsNeedTailLoop(
                    selfId, arms,
                );
            }
            _ => {
                return false;
            }
        }
    }
}

/// Return true when any match arm ends in a direct self-tail-call.
#[inline(always)]
pub fn armsNeedTailLoop(
    mut selfId: aver_rt::AverInt,
    mut arms: aver_rt::AverList<MatchArm>,
) -> bool {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(arms, [] => { return false; }, [arm, rest] => { if crate::aver_generated::domain::resolver::fast::exprNeedsTailLoop(selfId.clone(), arm.body.clone()) { return true; } else { {
            let __tco1 = rest;
            arms = __tco1;
            continue;
        } } })
    }
}

/// Single expression bodies can bypass evalStmts* and evaluate the expr directly.
pub fn classifyFastPath(body: &aver_rt::AverList<Stmt>) -> FnFastPath {
    crate::cancel_checkpoint();
    {
        let __list_subject = body.clone();
        if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            if (rest == aver_rt::AverList::empty()) {
                crate::aver_generated::domain::resolver::fast::classifyFastStmt(&stmt)
            } else {
                crate::aver_generated::domain::ast::FnFastPath::FastNone
            }
        } else {
            crate::aver_generated::domain::ast::FnFastPath::FastNone
        }
    }
}

/// Only plain expression bodies get a fast expr tag.
pub fn classifyFastStmt(stmt: &Stmt) -> FnFastPath {
    crate::cancel_checkpoint();
    match stmt.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::resolver::fast::classifyFastExpr(&expr)
        }
        _ => crate::aver_generated::domain::ast::FnFastPath::FastNone,
    }
}

/// Recognize a few leaf-like and branch-like expr shapes worth running without the full stmt walker.
#[inline(always)]
pub fn classifyFastExpr(expr: &Expr) -> FnFastPath {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::resolver::fast::classifyFastLeafExpr(expr) {
        Some(leaf) => crate::aver_generated::domain::ast::FnFastPath::FastLeaf(leaf),
        None => match expr.clone() {
            crate::aver_generated::domain::ast::Expr::ExprCallDirect(fnId, args) => {
                crate::aver_generated::domain::resolver::fast::classifyFastForwardCall(fnId, &args)
            }
            crate::aver_generated::domain::ast::Expr::ExprMatch(scrutinee, arms) => {
                let scrutinee = (*scrutinee).clone();
                crate::aver_generated::domain::resolver::fast::classifyFastMatch(&scrutinee, &arms)
            }
            _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        },
    }
}

/// Recognize a leaf-like expr that can be executed without descending the AST.
pub fn classifyFastLeafExpr(expr: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprInt(n) => Some(
            crate::aver_generated::domain::ast::FastLeaf::LeafConstInt(n),
        ),
        crate::aver_generated::domain::ast::Expr::ExprFloat(f) => {
            Some(crate::aver_generated::domain::ast::FastLeaf::LeafConstFloat(f))
        }
        crate::aver_generated::domain::ast::Expr::ExprStr(s) => Some(
            crate::aver_generated::domain::ast::FastLeaf::LeafConstStr(s),
        ),
        crate::aver_generated::domain::ast::Expr::ExprBool(b) => Some(
            crate::aver_generated::domain::ast::FastLeaf::LeafConstBool(b),
        ),
        crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
            Some(crate::aver_generated::domain::ast::FastLeaf::LeafSlot(slot))
        }
        crate::aver_generated::domain::ast::Expr::ExprFieldAccess(obj, field) => {
            let obj = (*obj).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastFieldAccess(&obj, field)
        }
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, args) => {
            crate::aver_generated::domain::resolver::fast::classifyFastBuiltinLeaf(name, &args)
        }
        crate::aver_generated::domain::ast::Expr::ExprAdd(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastBinopSlots(
                &crate::aver_generated::domain::ast::BinOp::OpAdd,
                &a,
                &b,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprSub(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastBinopSlots(
                &crate::aver_generated::domain::ast::BinOp::OpSub,
                &a,
                &b,
            )
        }
        _ => crate::aver_generated::domain::resolver::fast::classifyFastLeafExprTail(expr),
    }
}

/// Finish fast-leaf classification for arithmetic and comparison forms.
pub fn classifyFastLeafExprTail(expr: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprMul(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastBinopSlots(
                &crate::aver_generated::domain::ast::BinOp::OpMul,
                &a,
                &b,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprDiv(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastBinopSlots(
                &crate::aver_generated::domain::ast::BinOp::OpDiv,
                &a,
                &b,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                &crate::aver_generated::domain::ast::CmpOp::CmpEq,
                &a,
                &b,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprNeq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                &crate::aver_generated::domain::ast::CmpOp::CmpNeq,
                &a,
                &b,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                &crate::aver_generated::domain::ast::CmpOp::CmpLt,
                &a,
                &b,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                &crate::aver_generated::domain::ast::CmpOp::CmpGt,
                &a,
                &b,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                &crate::aver_generated::domain::ast::CmpOp::CmpLte,
                &a,
                &b,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                &crate::aver_generated::domain::ast::CmpOp::CmpGte,
                &a,
                &b,
            )
        }
        _ => None,
    }
}

/// Recognize obj.field when obj is already a resolved slot.
pub fn classifyFastFieldAccess(obj: &Expr, field: AverStr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match obj.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
            Some(crate::aver_generated::domain::ast::FastLeaf::LeafFieldAccess(slot, field))
        }
        _ => None,
    }
}

/// Recognize slot op slot for arithmetic.
pub fn classifyFastBinopSlots(op: &BinOp, a: &Expr, b: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match a.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(sa) => match b.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(sb) => Some(
                crate::aver_generated::domain::ast::FastLeaf::LeafBinopSlots(op.clone(), sa, sb),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// Recognize slot cmp slot for comparisons.
pub fn classifyFastCmpSlots(op: &CmpOp, a: &Expr, b: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match a.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(sa) => match b.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(sb) => Some(
                crate::aver_generated::domain::ast::FastLeaf::LeafCmpSlots(op.clone(), sa, sb),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// Recognize tiny builtin wrappers that only shuffle slots and integer constants.
#[inline(always)]
pub fn classifyFastBuiltinLeaf(name: AverStr, args: &aver_rt::AverList<Expr>) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name;
        if &*__dispatch_subject == "Vector.new" {
            crate::aver_generated::domain::resolver::fast::classifyFastVectorNew(args)
        } else {
            if &*__dispatch_subject == "Vector.len" {
                crate::aver_generated::domain::resolver::fast::classifyFastVectorLen(args)
            } else {
                if &*__dispatch_subject == "Option.withDefault" {
                    crate::aver_generated::domain::resolver::fast::classifyFastOptionWithDefault(
                        args,
                    )
                } else {
                    if &*__dispatch_subject == "Map.get" {
                        crate::aver_generated::domain::resolver::fast::classifyFastMapGet(args)
                    } else {
                        if &*__dispatch_subject == "Map.set" {
                            crate::aver_generated::domain::resolver::fast::classifyFastMapSet(args)
                        } else {
                            if &*__dispatch_subject == "Map.has" {
                                crate::aver_generated::domain::resolver::fast::classifyFastMapHas(
                                    args,
                                )
                            } else {
                                if &*__dispatch_subject == "Map.remove" {
                                    crate::aver_generated::domain::resolver::fast::classifyFastMapRemove(args)
                                } else {
                                    None
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Recognize Map.get(slotMap, slotKey).
pub fn classifyFastMapGet(args: &aver_rt::AverList<Expr>) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((mapExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((keyExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::fast::classifyFastMapGetArgs(
                        &mapExpr, &keyExpr,
                    )
                } else {
                    None
                }
            }
        } else {
            None
        }
    }
}

/// Encode a direct map lookup when both operands are resolved slots.
pub fn classifyFastMapGetArgs(mapExpr: &Expr, keyExpr: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match mapExpr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(mapSlot) => match keyExpr.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(keySlot) => Some(
                crate::aver_generated::domain::ast::FastLeaf::LeafMapGet(mapSlot, keySlot),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// Recognize Map.set(slotMap, slotKey, slotValue).
pub fn classifyFastMapSet(args: &aver_rt::AverList<Expr>) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((mapExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((keyExpr, rest2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = rest2;
                        if let Some((valueExpr, ignored)) =
                            aver_rt::list_uncons_cloned(&__list_subject)
                        {
                            crate::aver_generated::domain::resolver::fast::classifyFastMapSetArgs(
                                &mapExpr, &keyExpr, &valueExpr,
                            )
                        } else {
                            None
                        }
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

/// Encode a direct map update when all operands are resolved slots.
pub fn classifyFastMapSetArgs(
    mapExpr: &Expr,
    keyExpr: &Expr,
    valueExpr: &Expr,
) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match mapExpr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(mapSlot) => match keyExpr.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(keySlot) => {
                match valueExpr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprSlot(valueSlot) => {
                        Some(crate::aver_generated::domain::ast::FastLeaf::LeafMapSet(
                            mapSlot, keySlot, valueSlot,
                        ))
                    }
                    _ => None,
                }
            }
            _ => None,
        },
        _ => None,
    }
}

/// Recognize Map.has(slotMap, slotKey).
pub fn classifyFastMapHas(args: &aver_rt::AverList<Expr>) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((mapExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((keyExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::fast::classifyFastMapHasArgs(
                        &mapExpr, &keyExpr,
                    )
                } else {
                    None
                }
            }
        } else {
            None
        }
    }
}

/// Encode a direct map membership test when both operands are resolved slots.
pub fn classifyFastMapHasArgs(mapExpr: &Expr, keyExpr: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match mapExpr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(mapSlot) => match keyExpr.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(keySlot) => Some(
                crate::aver_generated::domain::ast::FastLeaf::LeafMapHas(mapSlot, keySlot),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// Recognize Map.remove(slotMap, slotKey).
pub fn classifyFastMapRemove(args: &aver_rt::AverList<Expr>) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((mapExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((keyExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::fast::classifyFastMapRemoveArgs(
                        &mapExpr, &keyExpr,
                    )
                } else {
                    None
                }
            }
        } else {
            None
        }
    }
}

/// Encode a direct map key removal when both operands are resolved slots.
pub fn classifyFastMapRemoveArgs(mapExpr: &Expr, keyExpr: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match mapExpr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(mapSlot) => match keyExpr.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(keySlot) => Some(
                crate::aver_generated::domain::ast::FastLeaf::LeafMapRemove(mapSlot, keySlot),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// Recognize Vector.len(slotVec).
pub fn classifyFastVectorLen(args: &aver_rt::AverList<Expr>) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((vecExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match vecExpr {
                crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => Some(
                    crate::aver_generated::domain::ast::FastLeaf::LeafVectorLen(slot),
                ),
                _ => None,
            }
        } else {
            None
        }
    }
}

/// Recognize Vector.new(slot, int) wrappers.
pub fn classifyFastVectorNew(args: &aver_rt::AverList<Expr>) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((sizeExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((fillExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::fast::classifyFastVectorNewArgs(
                        &sizeExpr, &fillExpr,
                    )
                } else {
                    None
                }
            }
        } else {
            None
        }
    }
}

/// Encode Vector.new(slot, int) without keeping the whole AST around.
pub fn classifyFastVectorNewArgs(sizeExpr: &Expr, fillExpr: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match sizeExpr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(sizeSlot) => match fillExpr.clone() {
            crate::aver_generated::domain::ast::Expr::ExprInt(fill) => Some(
                crate::aver_generated::domain::ast::FastLeaf::LeafVectorNew(sizeSlot, fill),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// Recognize Option.withDefault(Vector.get(slot, slot), int).
pub fn classifyFastOptionWithDefault(args: &aver_rt::AverList<Expr>) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((optionExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((defaultExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::fast::classifyFastOptionWithDefaultArgs(
                        &optionExpr,
                        &defaultExpr,
                    )
                } else {
                    None
                }
            }
        } else {
            None
        }
    }
}

/// Encode the common Vector.get-with-default wrapper used in tiny helpers like cellAt.
pub fn classifyFastOptionWithDefaultArgs(
    optionExpr: &Expr,
    defaultExpr: &Expr,
) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match optionExpr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, innerArgs) => {
            if (name == AverStr::from("Vector.get")) {
                crate::aver_generated::domain::resolver::fast::classifyFastVectorGet(
                    &innerArgs,
                    defaultExpr,
                )
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Recognize Vector.get(slotVec, slotIdx) with an integer default.
pub fn classifyFastVectorGet(
    args: &aver_rt::AverList<Expr>,
    defaultExpr: &Expr,
) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((vecExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((idxExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    crate::aver_generated::domain::resolver::fast::classifyFastVectorGetArgs(
                        &vecExpr,
                        &idxExpr,
                        defaultExpr,
                    )
                } else {
                    None
                }
            }
        } else {
            None
        }
    }
}

/// Encode Vector.get(slotVec, slotIdx) with an integer fallback.
pub fn classifyFastVectorGetArgs(
    vecExpr: &Expr,
    idxExpr: &Expr,
    defaultExpr: &Expr,
) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match vecExpr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(vecSlot) => match idxExpr.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(idxSlot) => {
                match defaultExpr.clone() {
                    crate::aver_generated::domain::ast::Expr::ExprInt(defaultValue) => Some(
                        crate::aver_generated::domain::ast::FastLeaf::LeafVectorGetOrInt(
                            vecSlot,
                            idxSlot,
                            defaultValue,
                        ),
                    ),
                    _ => None,
                }
            }
            _ => None,
        },
        _ => None,
    }
}

/// Recognize a small fixed-shape branch over bools or slot-based comparisons.
#[inline(always)]
pub fn classifyFastMatch(scrutinee: &Expr, arms: &aver_rt::AverList<MatchArm>) -> FnFastPath {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::resolver::fast::classifyBoolArms(arms) {
        Some(pair) => {
            let (thenLeaf, elseLeaf) = pair;
            crate::aver_generated::domain::resolver::fast::classifyFastMatchScrutinee(
                scrutinee, &thenLeaf, &elseLeaf,
            )
        }
        None => {
            crate::aver_generated::domain::resolver::fast::classifyFastListMatch(scrutinee, arms)
        }
    }
}

/// Recognize a two-arm list match with [] and [h, ..t] leaf bodies.
pub fn classifyFastListMatch(scrutinee: &Expr, arms: &aver_rt::AverList<MatchArm>) -> FnFastPath {
    crate::cancel_checkpoint();
    match scrutinee.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
            crate::aver_generated::domain::resolver::fast::classifyFastListArms(slot, arms)
        }
        _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Extract fixed empty/cons list arms regardless of order.
pub fn classifyFastListArms(
    slot: aver_rt::AverInt,
    arms: &aver_rt::AverList<MatchArm>,
) -> FnFastPath {
    crate::cancel_checkpoint();
    {
        let __list_subject = arms.clone();
        if let Some((arm1, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((arm2, tail)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    if (tail == aver_rt::AverList::empty()) {
                        crate::aver_generated::domain::resolver::fast::classifyFastListArmPair(
                            slot, &arm1, &arm2,
                        )
                    } else {
                        crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr
                    }
                } else {
                    crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr
                }
            }
        } else {
            crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr
        }
    }
}

/// Convert two list-pattern arms into a direct slot branch.
#[inline(always)]
pub fn classifyFastListArmPair(
    slot: aver_rt::AverInt,
    arm1: &MatchArm,
    arm2: &MatchArm,
) -> FnFastPath {
    crate::cancel_checkpoint();
    let leaf1 = crate::aver_generated::domain::resolver::fast::classifyFastLeafExpr(&arm1.body);
    let leaf2 = crate::aver_generated::domain::resolver::fast::classifyFastLeafExpr(&arm2.body);
    match leaf1 {
        Some(v1) => match leaf2 {
            Some(v2) => crate::aver_generated::domain::resolver::fast::classifyFastListPatterns(
                slot,
                &arm1.pattern,
                &arm1.bindingSlots,
                &v1,
                &arm2.pattern,
                &arm2.bindingSlots,
                &v2,
            ),
            None => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        },
        None => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Order empty/cons arms into a direct list branch.
pub fn classifyFastListPatterns(
    slot: aver_rt::AverInt,
    p1: &Pattern,
    bindingSlots1: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    leaf1: &FastLeaf,
    p2: &Pattern,
    bindingSlots2: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    leaf2: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match p1.clone() {
        crate::aver_generated::domain::ast::Pattern::PatEmpty => {
            crate::aver_generated::domain::resolver::fast::classifyFastListOther(
                slot,
                leaf1,
                p2,
                bindingSlots2,
                leaf2,
            )
        }
        crate::aver_generated::domain::ast::Pattern::PatCons(head, tail) => {
            crate::aver_generated::domain::resolver::fast::classifyFastListConsFirst(
                slot,
                head,
                tail,
                bindingSlots1,
                leaf1,
                p2,
                leaf2,
            )
        }
        _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Finish list fast-path classification when the empty arm is known.
pub fn classifyFastListOther(
    slot: aver_rt::AverInt,
    emptyLeaf: &FastLeaf,
    other: &Pattern,
    bindingSlots: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    otherLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match other.clone() {
        crate::aver_generated::domain::ast::Pattern::PatCons(head, tail) => {
            crate::aver_generated::domain::resolver::fast::classifyFastListCons(
                slot,
                emptyLeaf,
                head,
                tail,
                bindingSlots,
                otherLeaf,
            )
        }
        _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Handle reversed list arms where the cons case appears first.
pub fn classifyFastListConsFirst(
    slot: aver_rt::AverInt,
    head: AverStr,
    tail: AverStr,
    bindingSlots: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    consLeaf: &FastLeaf,
    other: &Pattern,
    otherLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match other {
        crate::aver_generated::domain::ast::Pattern::PatEmpty => {
            crate::aver_generated::domain::resolver::fast::classifyFastListCons(
                slot,
                otherLeaf,
                head,
                tail,
                bindingSlots,
                consLeaf,
            )
        }
        _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Build the list-slot branch once head/tail binding slots are known.
#[inline(always)]
pub fn classifyFastListCons(
    slot: aver_rt::AverInt,
    emptyLeaf: &FastLeaf,
    head: AverStr,
    tail: AverStr,
    bindingSlots: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    consLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match bindingSlots.get(&head).cloned() {
        Some(headSlot) => match bindingSlots.get(&tail).cloned() {
            Some(tailSlot) => crate::aver_generated::domain::ast::FnFastPath::FastListSlotBranch(
                slot,
                emptyLeaf.clone(),
                headSlot,
                tailSlot,
                consLeaf.clone(),
            ),
            None => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        },
        None => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Recognize tiny direct-call wrappers that only forward slot arguments.
#[inline(always)]
pub fn classifyFastForwardCall(
    fnId: aver_rt::AverInt,
    args: &aver_rt::AverList<Expr>,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::resolver::fast::classifyFastForwardSlots(
        args.clone(),
        aver_rt::AverList::empty(),
    ) {
        Some(slotArgs) => {
            crate::aver_generated::domain::ast::FnFastPath::FastForwardCall(fnId, slotArgs)
        }
        None => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Extract slot numbers from a direct call argument list.
#[inline(always)]
pub fn classifyFastForwardSlots(
    mut args: aver_rt::AverList<Expr>,
    mut acc: aver_rt::AverList<aver_rt::AverInt>,
) -> Option<aver_rt::AverList<aver_rt::AverInt>> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(args, [] => { return Some(acc.reverse()); }, [arg, rest] => { match arg {
        crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
            {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(slot, &acc);
            args = __tco0;
            acc = __tco1;
            continue;
        }
        },
        _ => {
            return None;
        }
    } })
    }
}

/// Extract (then, else) leaves from a two-arm bool match, regardless of arm order.
pub fn classifyBoolArms(arms: &aver_rt::AverList<MatchArm>) -> Option<(FastLeaf, FastLeaf)> {
    crate::cancel_checkpoint();
    {
        let __list_subject = arms.clone();
        if let Some((arm1, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((arm2, tail)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    if (tail == aver_rt::AverList::empty()) {
                        crate::aver_generated::domain::resolver::fast::classifyBoolArmPair(
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

/// Convert two bool-pattern arms into ordered leaves.
#[inline(always)]
pub fn classifyBoolArmPair(arm1: &MatchArm, arm2: &MatchArm) -> Option<(FastLeaf, FastLeaf)> {
    crate::cancel_checkpoint();
    let leaf1 = crate::aver_generated::domain::resolver::fast::classifyFastLeafExpr(&arm1.body);
    let leaf2 = crate::aver_generated::domain::resolver::fast::classifyFastLeafExpr(&arm2.body);
    match leaf1 {
        Some(v1) => match leaf2 {
            Some(v2) => crate::aver_generated::domain::resolver::fast::classifyBoolArmPatterns(
                &arm1.pattern,
                &v1,
                &arm2.pattern,
                &v2,
            ),
            None => None,
        },
        None => None,
    }
}

/// Order bool match arms into (trueLeaf, falseLeaf).
pub fn classifyBoolArmPatterns(
    p1: &Pattern,
    leaf1: &FastLeaf,
    p2: &Pattern,
    leaf2: &FastLeaf,
) -> Option<(FastLeaf, FastLeaf)> {
    crate::cancel_checkpoint();
    match p1.clone() {
        crate::aver_generated::domain::ast::Pattern::PatBool(b1) => {
            crate::aver_generated::domain::resolver::fast::classifyBoolArmPatternsInner(
                b1, leaf1, p2, leaf2,
            )
        }
        _ => None,
    }
}

/// Finish ordering bool match arms once the first arm bool is extracted.
pub fn classifyBoolArmPatternsInner(
    b1: bool,
    leaf1: &FastLeaf,
    p2: &Pattern,
    leaf2: &FastLeaf,
) -> Option<(FastLeaf, FastLeaf)> {
    crate::cancel_checkpoint();
    match p2.clone() {
        crate::aver_generated::domain::ast::Pattern::PatBool(b2) => {
            crate::aver_generated::domain::resolver::fast::classifyBoolArmPatternsPair(
                b1, leaf1, b2, leaf2,
            )
        }
        _ => None,
    }
}

/// Return (trueLeaf, falseLeaf) when the two bool arms are complementary.
pub fn classifyBoolArmPatternsPair(
    b1: bool,
    leaf1: &FastLeaf,
    b2: bool,
    leaf2: &FastLeaf,
) -> Option<(FastLeaf, FastLeaf)> {
    crate::cancel_checkpoint();
    match (b1, b2) {
        (true, false) => Some((leaf1.clone(), leaf2.clone())),
        (false, true) => Some((leaf2.clone(), leaf1.clone())),
        _ => None,
    }
}

/// Encode a recognized branch scrutinee with preclassified leaves.
pub fn classifyFastMatchScrutinee(
    scrutinee: &Expr,
    thenLeaf: &FastLeaf,
    elseLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match scrutinee.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
            crate::aver_generated::domain::ast::FnFastPath::FastBoolSlotBranch(
                slot,
                thenLeaf.clone(),
                elseLeaf.clone(),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastEqScrutinee(
                &a, &b, thenLeaf, elseLeaf,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastLtScrutinee(
                &a, &b, thenLeaf, elseLeaf,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::resolver::fast::classifyFastLtScrutinee(
                &b, &a, thenLeaf, elseLeaf,
            )
        }
        _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Recognize slot == int/string in either operand order.
pub fn classifyFastEqScrutinee(
    a: &Expr,
    b: &Expr,
    thenLeaf: &FastLeaf,
    elseLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match a.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
            crate::aver_generated::domain::resolver::fast::classifyFastEqOther(
                slot, b, thenLeaf, elseLeaf,
            )
        }
        _ => match b.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(slot) => {
                crate::aver_generated::domain::resolver::fast::classifyFastEqOther(
                    slot, a, thenLeaf, elseLeaf,
                )
            }
            _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        },
    }
}

/// Encode equality against a constant once the slot side is known.
pub fn classifyFastEqOther(
    slot: aver_rt::AverInt,
    other: &Expr,
    thenLeaf: &FastLeaf,
    elseLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match other.clone() {
        crate::aver_generated::domain::ast::Expr::ExprInt(n) => {
            crate::aver_generated::domain::ast::FnFastPath::FastEqIntBranch(
                slot,
                n,
                thenLeaf.clone(),
                elseLeaf.clone(),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprStr(s) => {
            crate::aver_generated::domain::ast::FnFastPath::FastEqStringBranch(
                slot,
                s,
                thenLeaf.clone(),
                elseLeaf.clone(),
            )
        }
        _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Recognize slot < slot branches like minInt.
pub fn classifyFastLtScrutinee(
    a: &Expr,
    b: &Expr,
    thenLeaf: &FastLeaf,
    elseLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match a.clone() {
        crate::aver_generated::domain::ast::Expr::ExprSlot(lhs) => match b.clone() {
            crate::aver_generated::domain::ast::Expr::ExprSlot(rhs) => {
                crate::aver_generated::domain::ast::FnFastPath::FastLtIntSlotsBranch(
                    lhs,
                    rhs,
                    thenLeaf.clone(),
                    elseLeaf.clone(),
                )
            }
            _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        },
        _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}
