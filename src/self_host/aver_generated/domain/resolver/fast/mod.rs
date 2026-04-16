#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::*;

/// Tag simple single-expression functions so eval can skip the stmt walker.
#[inline(always)]
pub fn annotateFastFns(
    mut fns: aver_rt::AverList<FnDef>,
    mut fnMap: aver_rt::AverMap<AverStr, i64>,
    mut acc: aver_rt::AverList<FnDef>,
) -> aver_rt::AverList<FnDef> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fns, [] => acc.reverse(), [f, rest] => { {
            let __tmp1 = fnMap.clone();
            let __tmp2 = aver_rt::AverList::prepend(annotateFastFn(&f, &fnMap), &acc);
            fns = rest;
            fnMap = __tmp1;
            acc = __tmp2;
            continue;
        } });
    }
}

/// Attach a narrow fast-path tag to a function definition.
pub fn annotateFastFn(fd: &FnDef, fnMap: &aver_rt::AverMap<AverStr, i64>) -> FnDef {
    crate::cancel_checkpoint();
    let selfId = fnMap.get(&fd.name).cloned().unwrap_or((-1i64));
    FnDef {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: fd.body.clone(),
        slotCount: fd.slotCount,
        slotMap: fd.slotMap.clone(),
        fastPath: classifyFastPath(&fd.body),
        tailLoop: classifyTailLoop(selfId, fd.body.clone()),
    }
}

/// Precompute whether the final expression position can self-tail-call directly.
#[inline(always)]
pub fn classifyTailLoop(mut selfId: i64, mut body: aver_rt::AverList<Stmt>) -> bool {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(body, [] => false, [stmt, rest] => { { let __list_subject = rest.clone(); if __list_subject.is_empty() { stmtNeedsTailLoop(selfId, &stmt) } else { {
            body = rest;
            continue;
        } } } });
    }
}

/// Only the final expression statement can trigger the tail-loop slot evaluator.
pub fn stmtNeedsTailLoop(selfId: i64, stmt: &Stmt) -> bool {
    crate::cancel_checkpoint();
    match stmt.clone() {
        Stmt::StmtExpr(expr) => exprNeedsTailLoop(selfId, expr),
        _ => false,
    }
}

/// Recognize direct self-calls in tail position, including bool branches and matches.
pub fn exprNeedsTailLoop(mut selfId: i64, mut expr: Expr) -> bool {
    loop {
        crate::cancel_checkpoint();
        return match expr {
            Expr::ExprCallDirect(fnId, _) => (fnId == selfId),
            Expr::ExprBoolBranch(_, thenExpr, elseExpr) => {
                let thenExpr = (*thenExpr).clone();
                let elseExpr = (*elseExpr).clone();
                if exprNeedsTailLoop(selfId, thenExpr) {
                    true
                } else {
                    {
                        expr = elseExpr;
                        continue;
                    }
                }
            }
            Expr::ExprMatch(_, arms) => armsNeedTailLoop(selfId, arms),
            _ => false,
        };
    }
}

/// Return true when any match arm ends in a direct self-tail-call.
#[inline(always)]
pub fn armsNeedTailLoop(mut selfId: i64, mut arms: aver_rt::AverList<MatchArm>) -> bool {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(arms, [] => false, [arm, rest] => { if exprNeedsTailLoop(selfId, arm.body.clone()) { true } else { {
            arms = rest;
            continue;
        } } });
    }
}

/// Single expression bodies can bypass evalStmts* and evaluate the expr directly.
pub fn classifyFastPath(body: &aver_rt::AverList<Stmt>) -> FnFastPath {
    crate::cancel_checkpoint();
    {
        let __list_subject = body.clone();
        if let Some((stmt, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            if (rest == aver_rt::AverList::empty()) {
                classifyFastStmt(&stmt)
            } else {
                FnFastPath::FastNone.clone()
            }
        } else {
            FnFastPath::FastNone.clone()
        }
    }
}

/// Only plain expression bodies get a fast expr tag.
pub fn classifyFastStmt(stmt: &Stmt) -> FnFastPath {
    crate::cancel_checkpoint();
    match stmt.clone() {
        Stmt::StmtExpr(expr) => classifyFastExpr(&expr),
        _ => FnFastPath::FastNone.clone(),
    }
}

/// Recognize a few leaf-like and branch-like expr shapes worth running without the full stmt walker.
#[inline(always)]
pub fn classifyFastExpr(expr: &Expr) -> FnFastPath {
    crate::cancel_checkpoint();
    match classifyFastLeafExpr(expr) {
        Some(leaf) => FnFastPath::FastLeaf(leaf),
        None => match expr.clone() {
            Expr::ExprCallDirect(fnId, args) => classifyFastForwardCall(fnId, &args),
            Expr::ExprMatch(scrutinee, arms) => {
                let scrutinee = (*scrutinee).clone();
                classifyFastMatch(&scrutinee, &arms)
            }
            _ => FnFastPath::FastSingleExpr.clone(),
        },
    }
}

/// Recognize a leaf-like expr that can be executed without descending the AST.
pub fn classifyFastLeafExpr(expr: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprInt(n) => Some(FastLeaf::LeafConstInt(n)),
        Expr::ExprFloat(f) => Some(FastLeaf::LeafConstFloat(f)),
        Expr::ExprStr(s) => Some(FastLeaf::LeafConstStr(s)),
        Expr::ExprBool(b) => Some(FastLeaf::LeafConstBool(b)),
        Expr::ExprSlot(slot) => Some(FastLeaf::LeafSlot(slot)),
        Expr::ExprFieldAccess(obj, field) => {
            let obj = (*obj).clone();
            classifyFastFieldAccess(&obj, field)
        }
        Expr::ExprCallBuiltin(name, args) => classifyFastBuiltinLeaf(name, &args),
        Expr::ExprAdd(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastBinopSlots(&BinOp::OpAdd, &a, &b)
        }
        Expr::ExprSub(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastBinopSlots(&BinOp::OpSub, &a, &b)
        }
        _ => classifyFastLeafExprTail(expr),
    }
}

/// Finish fast-leaf classification for arithmetic and comparison forms.
pub fn classifyFastLeafExprTail(expr: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprMul(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastBinopSlots(&BinOp::OpMul, &a, &b)
        }
        Expr::ExprDiv(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastBinopSlots(&BinOp::OpDiv, &a, &b)
        }
        Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastCmpSlots(&CmpOp::CmpEq, &a, &b)
        }
        Expr::ExprNeq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastCmpSlots(&CmpOp::CmpNeq, &a, &b)
        }
        Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastCmpSlots(&CmpOp::CmpLt, &a, &b)
        }
        Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastCmpSlots(&CmpOp::CmpGt, &a, &b)
        }
        Expr::ExprLte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastCmpSlots(&CmpOp::CmpLte, &a, &b)
        }
        Expr::ExprGte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastCmpSlots(&CmpOp::CmpGte, &a, &b)
        }
        _ => None,
    }
}

/// Recognize obj.field when obj is already a resolved slot.
pub fn classifyFastFieldAccess(obj: &Expr, field: AverStr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match obj.clone() {
        Expr::ExprSlot(slot) => Some(FastLeaf::LeafFieldAccess(slot, field)),
        _ => None,
    }
}

/// Recognize slot op slot for arithmetic.
pub fn classifyFastBinopSlots(op: &BinOp, a: &Expr, b: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match a.clone() {
        Expr::ExprSlot(sa) => match b.clone() {
            Expr::ExprSlot(sb) => Some(FastLeaf::LeafBinopSlots(op.clone(), sa, sb)),
            _ => None,
        },
        _ => None,
    }
}

/// Recognize slot cmp slot for comparisons.
pub fn classifyFastCmpSlots(op: &CmpOp, a: &Expr, b: &Expr) -> Option<FastLeaf> {
    crate::cancel_checkpoint();
    match a.clone() {
        Expr::ExprSlot(sa) => match b.clone() {
            Expr::ExprSlot(sb) => Some(FastLeaf::LeafCmpSlots(op.clone(), sa, sb)),
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
            classifyFastVectorNew(args)
        } else {
            if &*__dispatch_subject == "Vector.len" {
                classifyFastVectorLen(args)
            } else {
                if &*__dispatch_subject == "Option.withDefault" {
                    classifyFastOptionWithDefault(args)
                } else {
                    if &*__dispatch_subject == "Map.get" {
                        classifyFastMapGet(args)
                    } else {
                        if &*__dispatch_subject == "Map.set" {
                            classifyFastMapSet(args)
                        } else {
                            if &*__dispatch_subject == "Map.has" {
                                classifyFastMapHas(args)
                            } else {
                                if &*__dispatch_subject == "Map.remove" {
                                    classifyFastMapRemove(args)
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
                    classifyFastMapGetArgs(&mapExpr, &keyExpr)
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
        Expr::ExprSlot(mapSlot) => match keyExpr.clone() {
            Expr::ExprSlot(keySlot) => Some(FastLeaf::LeafMapGet(mapSlot, keySlot)),
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
                            classifyFastMapSetArgs(&mapExpr, &keyExpr, &valueExpr)
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
        Expr::ExprSlot(mapSlot) => match keyExpr.clone() {
            Expr::ExprSlot(keySlot) => match valueExpr.clone() {
                Expr::ExprSlot(valueSlot) => {
                    Some(FastLeaf::LeafMapSet(mapSlot, keySlot, valueSlot))
                }
                _ => None,
            },
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
                    classifyFastMapHasArgs(&mapExpr, &keyExpr)
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
        Expr::ExprSlot(mapSlot) => match keyExpr.clone() {
            Expr::ExprSlot(keySlot) => Some(FastLeaf::LeafMapHas(mapSlot, keySlot)),
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
                    classifyFastMapRemoveArgs(&mapExpr, &keyExpr)
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
        Expr::ExprSlot(mapSlot) => match keyExpr.clone() {
            Expr::ExprSlot(keySlot) => Some(FastLeaf::LeafMapRemove(mapSlot, keySlot)),
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
                Expr::ExprSlot(slot) => Some(FastLeaf::LeafVectorLen(slot)),
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
                    classifyFastVectorNewArgs(&sizeExpr, &fillExpr)
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
        Expr::ExprSlot(sizeSlot) => match fillExpr.clone() {
            Expr::ExprInt(fill) => Some(FastLeaf::LeafVectorNew(sizeSlot, fill)),
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
                    classifyFastOptionWithDefaultArgs(&optionExpr, &defaultExpr)
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
        Expr::ExprCallBuiltin(name, innerArgs) => {
            if (name == AverStr::from("Vector.get")) {
                classifyFastVectorGet(&innerArgs, defaultExpr)
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
                    classifyFastVectorGetArgs(&vecExpr, &idxExpr, defaultExpr)
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
        Expr::ExprSlot(vecSlot) => match idxExpr.clone() {
            Expr::ExprSlot(idxSlot) => match defaultExpr.clone() {
                Expr::ExprInt(defaultValue) => {
                    Some(FastLeaf::LeafVectorGetOrInt(vecSlot, idxSlot, defaultValue))
                }
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

/// Recognize a small fixed-shape branch over bools or slot-based comparisons.
#[inline(always)]
pub fn classifyFastMatch(scrutinee: &Expr, arms: &aver_rt::AverList<MatchArm>) -> FnFastPath {
    crate::cancel_checkpoint();
    match classifyBoolArms(arms) {
        Some(pair) => {
            let (thenLeaf, elseLeaf) = pair;
            classifyFastMatchScrutinee(scrutinee, &thenLeaf, &elseLeaf)
        }
        None => classifyFastListMatch(scrutinee, arms),
    }
}

/// Recognize a two-arm list match with [] and [h, ..t] leaf bodies.
pub fn classifyFastListMatch(scrutinee: &Expr, arms: &aver_rt::AverList<MatchArm>) -> FnFastPath {
    crate::cancel_checkpoint();
    match scrutinee.clone() {
        Expr::ExprSlot(slot) => classifyFastListArms(slot, arms),
        _ => FnFastPath::FastSingleExpr.clone(),
    }
}

/// Extract fixed empty/cons list arms regardless of order.
pub fn classifyFastListArms(slot: i64, arms: &aver_rt::AverList<MatchArm>) -> FnFastPath {
    crate::cancel_checkpoint();
    {
        let __list_subject = arms.clone();
        if let Some((arm1, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((arm2, tail)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    if (tail == aver_rt::AverList::empty()) {
                        classifyFastListArmPair(slot, &arm1, &arm2)
                    } else {
                        FnFastPath::FastSingleExpr.clone()
                    }
                } else {
                    FnFastPath::FastSingleExpr.clone()
                }
            }
        } else {
            FnFastPath::FastSingleExpr.clone()
        }
    }
}

/// Convert two list-pattern arms into a direct slot branch.
#[inline(always)]
pub fn classifyFastListArmPair(slot: i64, arm1: &MatchArm, arm2: &MatchArm) -> FnFastPath {
    crate::cancel_checkpoint();
    let leaf1 = classifyFastLeafExpr(&arm1.body);
    let leaf2 = classifyFastLeafExpr(&arm2.body);
    match leaf1 {
        Some(v1) => match leaf2 {
            Some(v2) => classifyFastListPatterns(
                slot,
                &arm1.pattern,
                &arm1.bindingSlots,
                &v1,
                &arm2.pattern,
                &arm2.bindingSlots,
                &v2,
            ),
            None => FnFastPath::FastSingleExpr.clone(),
        },
        None => FnFastPath::FastSingleExpr.clone(),
    }
}

/// Order empty/cons arms into a direct list branch.
pub fn classifyFastListPatterns(
    slot: i64,
    p1: &Pattern,
    bindingSlots1: &aver_rt::AverMap<AverStr, i64>,
    leaf1: &FastLeaf,
    p2: &Pattern,
    bindingSlots2: &aver_rt::AverMap<AverStr, i64>,
    leaf2: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match p1.clone() {
        Pattern::PatEmpty => classifyFastListOther(slot, leaf1, p2, bindingSlots2, leaf2),
        Pattern::PatCons(head, tail) => {
            classifyFastListConsFirst(slot, head, tail, bindingSlots1, leaf1, p2, leaf2)
        }
        _ => FnFastPath::FastSingleExpr.clone(),
    }
}

/// Finish list fast-path classification when the empty arm is known.
pub fn classifyFastListOther(
    slot: i64,
    emptyLeaf: &FastLeaf,
    other: &Pattern,
    bindingSlots: &aver_rt::AverMap<AverStr, i64>,
    otherLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match other.clone() {
        Pattern::PatCons(head, tail) => {
            classifyFastListCons(slot, emptyLeaf, head, tail, bindingSlots, otherLeaf)
        }
        _ => FnFastPath::FastSingleExpr.clone(),
    }
}

/// Handle reversed list arms where the cons case appears first.
pub fn classifyFastListConsFirst(
    slot: i64,
    head: AverStr,
    tail: AverStr,
    bindingSlots: &aver_rt::AverMap<AverStr, i64>,
    consLeaf: &FastLeaf,
    other: &Pattern,
    otherLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match other {
        Pattern::PatEmpty => {
            classifyFastListCons(slot, otherLeaf, head, tail, bindingSlots, consLeaf)
        }
        _ => FnFastPath::FastSingleExpr.clone(),
    }
}

/// Build the list-slot branch once head/tail binding slots are known.
#[inline(always)]
pub fn classifyFastListCons(
    slot: i64,
    emptyLeaf: &FastLeaf,
    head: AverStr,
    tail: AverStr,
    bindingSlots: &aver_rt::AverMap<AverStr, i64>,
    consLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match bindingSlots.get(&head).cloned() {
        Some(headSlot) => match bindingSlots.get(&tail).cloned() {
            Some(tailSlot) => FnFastPath::FastListSlotBranch(
                slot,
                emptyLeaf.clone(),
                headSlot,
                tailSlot,
                consLeaf.clone(),
            ),
            None => FnFastPath::FastSingleExpr.clone(),
        },
        None => FnFastPath::FastSingleExpr.clone(),
    }
}

/// Recognize tiny direct-call wrappers that only forward slot arguments.
#[inline(always)]
pub fn classifyFastForwardCall(fnId: i64, args: &aver_rt::AverList<Expr>) -> FnFastPath {
    crate::cancel_checkpoint();
    match classifyFastForwardSlots(args.clone(), aver_rt::AverList::empty()) {
        Some(slotArgs) => FnFastPath::FastForwardCall(fnId, slotArgs),
        None => FnFastPath::FastSingleExpr.clone(),
    }
}

/// Extract slot numbers from a direct call argument list.
#[inline(always)]
pub fn classifyFastForwardSlots(
    mut args: aver_rt::AverList<Expr>,
    mut acc: aver_rt::AverList<i64>,
) -> Option<aver_rt::AverList<i64>> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(args, [] => Some(acc.reverse()), [arg, rest] => { match arg {
            Expr::ExprSlot(slot) => {
            let __tmp1 = aver_rt::AverList::prepend(slot, &acc);
            args = rest;
            acc = __tmp1;
            continue;
        },
            _ => None
        } });
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
                        classifyBoolArmPair(&arm1, &arm2)
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
    let leaf1 = classifyFastLeafExpr(&arm1.body);
    let leaf2 = classifyFastLeafExpr(&arm2.body);
    match leaf1 {
        Some(v1) => match leaf2 {
            Some(v2) => classifyBoolArmPatterns(&arm1.pattern, &v1, &arm2.pattern, &v2),
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
        Pattern::PatBool(b1) => classifyBoolArmPatternsInner(b1, leaf1, p2, leaf2),
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
        Pattern::PatBool(b2) => classifyBoolArmPatternsPair(b1, leaf1, b2, leaf2),
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
        Expr::ExprSlot(slot) => {
            FnFastPath::FastBoolSlotBranch(slot, thenLeaf.clone(), elseLeaf.clone())
        }
        Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastEqScrutinee(&a, &b, thenLeaf, elseLeaf)
        }
        Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastLtScrutinee(&a, &b, thenLeaf, elseLeaf)
        }
        Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            classifyFastLtScrutinee(&b, &a, thenLeaf, elseLeaf)
        }
        _ => FnFastPath::FastSingleExpr.clone(),
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
        Expr::ExprSlot(slot) => classifyFastEqOther(slot, b, thenLeaf, elseLeaf),
        _ => match b.clone() {
            Expr::ExprSlot(slot) => classifyFastEqOther(slot, a, thenLeaf, elseLeaf),
            _ => FnFastPath::FastSingleExpr.clone(),
        },
    }
}

/// Encode equality against a constant once the slot side is known.
pub fn classifyFastEqOther(
    slot: i64,
    other: &Expr,
    thenLeaf: &FastLeaf,
    elseLeaf: &FastLeaf,
) -> FnFastPath {
    crate::cancel_checkpoint();
    match other.clone() {
        Expr::ExprInt(n) => {
            FnFastPath::FastEqIntBranch(slot, n, thenLeaf.clone(), elseLeaf.clone())
        }
        Expr::ExprStr(s) => {
            FnFastPath::FastEqStringBranch(slot, s, thenLeaf.clone(), elseLeaf.clone())
        }
        _ => FnFastPath::FastSingleExpr.clone(),
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
        Expr::ExprSlot(lhs) => match b.clone() {
            Expr::ExprSlot(rhs) => {
                FnFastPath::FastLtIntSlotsBranch(lhs, rhs, thenLeaf.clone(), elseLeaf.clone())
            }
            _ => FnFastPath::FastSingleExpr.clone(),
        },
        _ => FnFastPath::FastSingleExpr.clone(),
    }
}
