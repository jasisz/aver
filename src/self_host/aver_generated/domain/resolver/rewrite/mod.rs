#[allow(unused_imports)]
use crate::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;

/// Rewrite resolved AST into lighter internal expression shapes without changing semantics.
#[inline(always)]
pub fn rewriteInternalFns(mut fns: aver_rt::AverList<FnDef>, mut acc: aver_rt::AverList<FnDef>) -> aver_rt::AverList<FnDef> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fns, [] => acc.reverse(), [f, rest] => { {
            let __tmp1 = aver_rt::AverList::prepend(rewriteInternalFn(&f), &acc);
            fns = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Rewrite the body of one resolved function into lighter internal expression shapes.
pub fn rewriteInternalFn(fd: &FnDef) -> FnDef {
    crate::cancel_checkpoint();
    FnDef { name: fd.name.clone(), params: fd.params.clone(), body: rewriteInternalStmts(fd.body.clone(), aver_rt::AverList::empty()), slotCount: fd.slotCount, slotMap: fd.slotMap.clone(), fastPath: fd.fastPath.clone(), tailLoop: fd.tailLoop }
}

/// Rewrite statements recursively into lighter internal expression shapes.
#[inline(always)]
pub fn rewriteInternalStmts(mut stmts: aver_rt::AverList<Stmt>, mut acc: aver_rt::AverList<Stmt>) -> aver_rt::AverList<Stmt> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(stmts, [] => acc.reverse(), [stmt, rest] => { {
            let __tmp1 = aver_rt::AverList::prepend(rewriteInternalStmt(&stmt), &acc);
            stmts = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Rewrite one statement recursively into lighter internal expression shapes.
pub fn rewriteInternalStmt(stmt: &Stmt) -> Stmt {
    crate::cancel_checkpoint();
    match stmt.clone() {
        Stmt::StmtBind(name, expr) => {
            Stmt::StmtBind(name, rewriteInternalExpr(&expr))
        },
        Stmt::StmtBindSlot(slot, expr) => {
            Stmt::StmtBindSlot(slot, rewriteInternalExpr(&expr))
        },
        Stmt::StmtExpr(expr) => {
            Stmt::StmtExpr(rewriteInternalExpr(&expr))
        }
    }
}

/// Rewrite one expression into lighter internal forms after resolve and direct-call linking.
pub fn rewriteInternalExpr(expr: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprBoolBranch(cond, thenExpr, elseExpr) => {
            let cond = (*cond).clone();
            let thenExpr = (*thenExpr).clone();
            let elseExpr = (*elseExpr).clone();
            Expr::ExprBoolBranch(std::sync::Arc::new(rewriteInternalExpr(&cond)), std::sync::Arc::new(rewriteInternalExpr(&thenExpr)), std::sync::Arc::new(rewriteInternalExpr(&elseExpr)))
        },
        Expr::ExprBinopSlotInt(_, _, _) => {
            expr.clone()
        },
        Expr::ExprBinopSlots(_, _, _) => {
            expr.clone()
        },
        Expr::ExprCmpSlotInt(_, _, _) => {
            expr.clone()
        },
        Expr::ExprCmpSlots(_, _, _) => {
            expr.clone()
        },
        Expr::ExprVectorGetOrInt(vecExpr, idxExpr, defaultValue) => {
            let vecExpr = (*vecExpr).clone();
            let idxExpr = (*idxExpr).clone();
            Expr::ExprVectorGetOrInt(std::sync::Arc::new(rewriteInternalExpr(&vecExpr)), std::sync::Arc::new(rewriteInternalExpr(&idxExpr)), defaultValue)
        },
        Expr::ExprIntModOrInt(a, b, defaultValue) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprIntModOrInt(std::sync::Arc::new(rewriteInternalExpr(&a)), std::sync::Arc::new(rewriteInternalExpr(&b)), defaultValue)
        },
        _ => {
            rewriteInternalExprAfterLeaf(expr)
        }
    }
}

/// Continue internal rewrite for arithmetic and comparison forms.
pub fn rewriteInternalExprAfterLeaf(expr: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprAdd(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            rewriteInternalBinop(&BinOp::OpAdd, &rewriteInternalExpr(&a), &rewriteInternalExpr(&b))
        },
        Expr::ExprSub(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            rewriteInternalBinop(&BinOp::OpSub, &rewriteInternalExpr(&a), &rewriteInternalExpr(&b))
        },
        Expr::ExprMul(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            rewriteInternalBinop(&BinOp::OpMul, &rewriteInternalExpr(&a), &rewriteInternalExpr(&b))
        },
        Expr::ExprDiv(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            rewriteInternalBinop(&BinOp::OpDiv, &rewriteInternalExpr(&a), &rewriteInternalExpr(&b))
        },
        Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            rewriteInternalCmp(&CmpOp::CmpEq, &rewriteInternalExpr(&a), &rewriteInternalExpr(&b))
        },
        Expr::ExprNeq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            rewriteInternalCmp(&CmpOp::CmpNeq, &rewriteInternalExpr(&a), &rewriteInternalExpr(&b))
        },
        Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            rewriteInternalCmp(&CmpOp::CmpLt, &rewriteInternalExpr(&a), &rewriteInternalExpr(&b))
        },
        Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            rewriteInternalCmp(&CmpOp::CmpGt, &rewriteInternalExpr(&a), &rewriteInternalExpr(&b))
        },
        Expr::ExprLte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            rewriteInternalCmp(&CmpOp::CmpLte, &rewriteInternalExpr(&a), &rewriteInternalExpr(&b))
        },
        Expr::ExprGte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            rewriteInternalCmp(&CmpOp::CmpGte, &rewriteInternalExpr(&a), &rewriteInternalExpr(&b))
        },
        _ => {
            rewriteInternalExprAfterArith(expr)
        }
    }
}

/// Finish internal rewrite for aggregates, calls, and products.
pub fn rewriteInternalExprAfterArith(expr: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprMatch(scrutinee, arms) => {
            let scrutinee = (*scrutinee).clone();
            rewriteInternalMatch(&rewriteInternalExpr(&scrutinee), &rewriteInternalArms(arms, aver_rt::AverList::empty()))
        },
        Expr::ExprPropagate(inner) => {
            let inner = (*inner).clone();
            Expr::ExprPropagate(std::sync::Arc::new(rewriteInternalExpr(&inner)))
        },
        Expr::ExprConcat(parts) => {
            Expr::ExprConcat(rewriteInternalExprs(parts, aver_rt::AverList::empty()))
        },
        Expr::ExprTuple(exprs) => {
            Expr::ExprTuple(rewriteInternalExprs(exprs, aver_rt::AverList::empty()))
        },
        Expr::ExprIndependentProduct(exprs, unwrap) => {
            Expr::ExprIndependentProduct(rewriteInternalExprs(exprs, aver_rt::AverList::empty()), unwrap)
        },
        Expr::ExprList(exprs) => {
            Expr::ExprList(rewriteInternalExprs(exprs, aver_rt::AverList::empty()))
        },
        Expr::ExprRecord(name, fields) => {
            Expr::ExprRecord(name, rewriteInternalFields(fields, aver_rt::AverList::empty()))
        },
        Expr::ExprFieldAccess(obj, field) => {
            let obj = (*obj).clone();
            Expr::ExprFieldAccess(std::sync::Arc::new(rewriteInternalExpr(&obj)), field)
        },
        Expr::ExprCall(name, args) => {
            Expr::ExprCall(name, rewriteInternalExprs(args, aver_rt::AverList::empty()))
        },
        Expr::ExprCallDirect(fnId, args) => {
            Expr::ExprCallDirect(fnId, rewriteInternalExprs(args, aver_rt::AverList::empty()))
        },
        Expr::ExprCallBuiltin(name, args) => {
            rewriteInternalBuiltin(name, &rewriteInternalExprs(args, aver_rt::AverList::empty()))
        },
        Expr::ExprCallBuiltinId(id, args) => {
            Expr::ExprCallBuiltinId(id, rewriteInternalExprs(args, aver_rt::AverList::empty()))
        },
        _ => {
            expr.clone()
        }
    }
}

/// Rewrite a list of expressions recursively.
#[inline(always)]
pub fn rewriteInternalExprs(mut exprs: aver_rt::AverList<Expr>, mut acc: aver_rt::AverList<Expr>) -> aver_rt::AverList<Expr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => acc.reverse(), [expr, rest] => { {
            let __tmp1 = aver_rt::AverList::prepend(rewriteInternalExpr(&expr), &acc);
            exprs = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Rewrite record field expressions recursively.
#[inline(always)]
pub fn rewriteInternalFields(mut fields: aver_rt::AverList<(AverStr, Expr)>, mut acc: aver_rt::AverList<(AverStr, Expr)>) -> aver_rt::AverList<(AverStr, Expr)> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fields, [] => acc.reverse(), [pair, rest] => { match pair {
            (name, expr) => {
            let __tmp1 = aver_rt::AverList::prepend((name, rewriteInternalExpr(&expr)), &acc);
            fields = rest;
            acc = __tmp1;
            continue;
        }
        } });
    }
}

/// Rewrite match arm bodies and resolve PatConstructor to PatConstructorId using constructor tags.
#[inline(always)]
pub fn rewriteInternalArms(mut arms: aver_rt::AverList<MatchArm>, mut acc: aver_rt::AverList<MatchArm>) -> aver_rt::AverList<MatchArm> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(arms, [] => acc.reverse(), [arm, rest] => { {
            let __tmp1 = aver_rt::AverList::prepend(MatchArm { pattern: rewritePattern(&arm.pattern), body: rewriteInternalExpr(&arm.body), bindingSlots: arm.bindingSlots.clone() }, &acc);
            arms = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Rewrite PatConstructor to PatConstructorId with both tag and full constructor name.
pub fn rewritePattern(pat: &Pattern) -> Pattern {
    crate::cancel_checkpoint();
    match pat.clone() {
        Pattern::PatConstructor(name, bindings) => {
            rewritePatConstructor(name, &bindings)
        },
        Pattern::PatTuple(pats) => {
            Pattern::PatTuple(rewritePatterns(pats, aver_rt::AverList::empty()))
        },
        _ => {
            pat.clone()
        }
    }
}

/// Convert PatConstructor to PatConstructorId with stable tag and full constructor name.
#[inline(always)]
pub fn rewritePatConstructor(name: AverStr, bindings: &aver_rt::AverList<AverStr>) -> Pattern {
    crate::cancel_checkpoint();
    let tag = crate::aver_generated::domain::ast::ctorNameToTag(name.clone());
    Pattern::PatConstructorId(tag, name.clone(), bindings.clone())
}

/// Rewrite a list of patterns.
#[inline(always)]
pub fn rewritePatterns(mut pats: aver_rt::AverList<Pattern>, mut acc: aver_rt::AverList<Pattern>) -> aver_rt::AverList<Pattern> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(pats, [] => acc.reverse(), [p, rest] => { {
            let __tmp1 = aver_rt::AverList::prepend(rewritePattern(&p), &acc);
            pats = rest;
            acc = __tmp1;
            continue;
        } });
    }
}

/// Compress simple resolved arithmetic into slot-based internal nodes.
pub fn rewriteInternalBinop(op: &BinOp, left: &Expr, right: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match left.clone() {
        Expr::ExprSlot(slot) => {
            rewriteInternalBinopSlotLeft(op, slot, right)
        },
        _ => {
            match right.clone() {
        Expr::ExprSlot(slot) => {
            rewriteInternalBinopSlotRight(op, left, slot)
        },
        _ => {
            rebuildInternalBinop(op, left, right)
        }
    }
        }
    }
}

/// Rewrite arithmetic with a slot on the left when the right side is simple.
pub fn rewriteInternalBinopSlotLeft(op: &BinOp, slot: i64, right: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match right.clone() {
        Expr::ExprInt(n) => {
            Expr::ExprBinopSlotInt(op.clone(), slot, n)
        },
        Expr::ExprSlot(rhs) => {
            Expr::ExprBinopSlots(op.clone(), slot, rhs)
        },
        _ => {
            rebuildInternalBinop(op, &Expr::ExprSlot(slot), right)
        }
    }
}

/// Rewrite commutative arithmetic when the slot appears on the right.
pub fn rewriteInternalBinopSlotRight(op: &BinOp, left: &Expr, slot: i64) -> Expr {
    crate::cancel_checkpoint();
    match left.clone() {
        Expr::ExprSlot(lhs) => {
            Expr::ExprBinopSlots(op.clone(), lhs, slot)
        },
        Expr::ExprInt(n) => {
            if binopCanFlip(op) { Expr::ExprBinopSlotInt(op.clone(), slot, n) } else { rebuildInternalBinop(op, left, &Expr::ExprSlot(slot)) }
        },
        _ => {
            rebuildInternalBinop(op, left, &Expr::ExprSlot(slot))
        }
    }
}

/// Rebuild a generic arithmetic expression when no narrower internal form applies.
pub fn rebuildInternalBinop(op: &BinOp, left: &Expr, right: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match op {
        BinOp::OpAdd => {
            Expr::ExprAdd(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone()))
        },
        BinOp::OpSub => {
            Expr::ExprSub(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone()))
        },
        BinOp::OpMul => {
            Expr::ExprMul(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone()))
        },
        BinOp::OpDiv => {
            Expr::ExprDiv(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone()))
        }
    }
}

/// Return whether swapping operand order preserves arithmetic semantics.
pub fn binopCanFlip(op: &BinOp) -> bool {
    crate::cancel_checkpoint();
    match op {
        BinOp::OpAdd => {
            true
        },
        BinOp::OpMul => {
            true
        },
        _ => {
            false
        }
    }
}

/// Recognize fused builtin wrapper patterns after children have already been rewritten.
#[inline(always)]
pub fn rewriteInternalBuiltin(name: AverStr, args: &aver_rt::AverList<Expr>) -> Expr {
    crate::cancel_checkpoint();
    { let __dispatch_subject = name.clone(); if &*__dispatch_subject == "Option.withDefault" { rewriteInternalOptionWithDefault(args) } else { if &*__dispatch_subject == "Result.withDefault" { rewriteInternalResultWithDefault(args) } else { Expr::ExprCallBuiltin(name, args.clone()) } } }
}

/// Rewrite Option.withDefault(Vector.get(...), int) into one internal node.
pub fn rewriteInternalOptionWithDefault(args: &aver_rt::AverList<Expr>) -> Expr {
    crate::cancel_checkpoint();
    { let __list_subject = args.clone(); if let Some((optionExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = rest; if let Some((defaultExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) { rewriteInternalOptionWithDefaultArgs(&optionExpr, &defaultExpr) } else { Expr::ExprCallBuiltin(AverStr::from("Option.withDefault"), args.clone()) } } } else { Expr::ExprCallBuiltin(AverStr::from("Option.withDefault"), args.clone()) } }
}

/// Recognize a fused Vector.get-with-default shape.
pub fn rewriteInternalOptionWithDefaultArgs(optionExpr: &Expr, defaultExpr: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match defaultExpr.clone() {
        Expr::ExprInt(defaultValue) => {
            match optionExpr.clone() {
        Expr::ExprCallBuiltin(name, innerArgs) => {
            if (name == AverStr::from("Vector.get")) { rewriteInternalVectorGetOrInt(&innerArgs, defaultValue) } else { Expr::ExprCallBuiltin(AverStr::from("Option.withDefault"), aver_rt::AverList::from_vec(vec![optionExpr.clone(), defaultExpr.clone()])) }
        },
        _ => {
            Expr::ExprCallBuiltin(AverStr::from("Option.withDefault"), aver_rt::AverList::from_vec(vec![optionExpr.clone(), defaultExpr.clone()]))
        }
    }
        },
        _ => {
            Expr::ExprCallBuiltin(AverStr::from("Option.withDefault"), aver_rt::AverList::from_vec(vec![optionExpr.clone(), defaultExpr.clone()]))
        }
    }
}

/// Fuse Vector.get(vec, idx) with an integer fallback.
pub fn rewriteInternalVectorGetOrInt(args: &aver_rt::AverList<Expr>, defaultValue: i64) -> Expr {
    crate::cancel_checkpoint();
    { let __list_subject = args.clone(); if let Some((vecExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = rest; if let Some((idxExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) { rewriteInternalVectorGetOrIntArgs(&vecExpr, &idxExpr, defaultValue) } else { Expr::ExprCallBuiltin(AverStr::from("Option.withDefault"), aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.get"), args.clone()), Expr::ExprInt(defaultValue)])) } } } else { Expr::ExprCallBuiltin(AverStr::from("Option.withDefault"), aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.get"), args.clone()), Expr::ExprInt(defaultValue)])) } }
}

/// Only fuse Vector.get-with-default when at least one operand is already slot-shaped.
#[inline(always)]
pub fn rewriteInternalVectorGetOrIntArgs(vecExpr: &Expr, idxExpr: &Expr, defaultValue: i64) -> Expr {
    crate::cancel_checkpoint();
    if exprHasSlotShape(vecExpr) { Expr::ExprVectorGetOrInt(std::sync::Arc::new(vecExpr.clone()), std::sync::Arc::new(idxExpr.clone()), defaultValue) } else { if exprHasSlotShape(idxExpr) { Expr::ExprVectorGetOrInt(std::sync::Arc::new(vecExpr.clone()), std::sync::Arc::new(idxExpr.clone()), defaultValue) } else { Expr::ExprCallBuiltin(AverStr::from("Option.withDefault"), aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Vector.get"), aver_rt::AverList::from_vec(vec![vecExpr.clone(), idxExpr.clone()])), Expr::ExprInt(defaultValue)])) } }
}

/// Rewrite Result.withDefault(Int.mod(...), int) into one internal node.
pub fn rewriteInternalResultWithDefault(args: &aver_rt::AverList<Expr>) -> Expr {
    crate::cancel_checkpoint();
    { let __list_subject = args.clone(); if let Some((resultExpr, rest)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = rest; if let Some((defaultExpr, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) { rewriteInternalResultWithDefaultArgs(&resultExpr, &defaultExpr) } else { Expr::ExprCallBuiltin(AverStr::from("Result.withDefault"), args.clone()) } } } else { Expr::ExprCallBuiltin(AverStr::from("Result.withDefault"), args.clone()) } }
}

/// Recognize a fused Int.mod-with-default shape.
pub fn rewriteInternalResultWithDefaultArgs(resultExpr: &Expr, defaultExpr: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match defaultExpr.clone() {
        Expr::ExprInt(defaultValue) => {
            match resultExpr.clone() {
        Expr::ExprCallBuiltin(name, innerArgs) => {
            if (name == AverStr::from("Int.mod")) { rewriteInternalIntModOrInt(&innerArgs, defaultValue) } else { Expr::ExprCallBuiltin(AverStr::from("Result.withDefault"), aver_rt::AverList::from_vec(vec![resultExpr.clone(), defaultExpr.clone()])) }
        },
        _ => {
            Expr::ExprCallBuiltin(AverStr::from("Result.withDefault"), aver_rt::AverList::from_vec(vec![resultExpr.clone(), defaultExpr.clone()]))
        }
    }
        },
        _ => {
            Expr::ExprCallBuiltin(AverStr::from("Result.withDefault"), aver_rt::AverList::from_vec(vec![resultExpr.clone(), defaultExpr.clone()]))
        }
    }
}

/// Fuse Int.mod(a, b) with an integer fallback when the operands are slot-shaped.
pub fn rewriteInternalIntModOrInt(args: &aver_rt::AverList<Expr>, defaultValue: i64) -> Expr {
    crate::cancel_checkpoint();
    { let __list_subject = args.clone(); if let Some((a, rest)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = rest; if let Some((b, ignored)) = aver_rt::list_uncons_cloned(&__list_subject) { rewriteInternalIntModOrIntArgs(&a, &b, defaultValue) } else { Expr::ExprCallBuiltin(AverStr::from("Result.withDefault"), aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Int.mod"), args.clone()), Expr::ExprInt(defaultValue)])) } } } else { Expr::ExprCallBuiltin(AverStr::from("Result.withDefault"), aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Int.mod"), args.clone()), Expr::ExprInt(defaultValue)])) } }
}

/// Only fuse Int.mod-with-default when at least one operand is already slot-shaped.
#[inline(always)]
pub fn rewriteInternalIntModOrIntArgs(a: &Expr, b: &Expr, defaultValue: i64) -> Expr {
    crate::cancel_checkpoint();
    if exprHasSlotShape(a) { Expr::ExprIntModOrInt(std::sync::Arc::new(a.clone()), std::sync::Arc::new(b.clone()), defaultValue) } else { if exprHasSlotShape(b) { Expr::ExprIntModOrInt(std::sync::Arc::new(a.clone()), std::sync::Arc::new(b.clone()), defaultValue) } else { Expr::ExprCallBuiltin(AverStr::from("Result.withDefault"), aver_rt::AverList::from_vec(vec![Expr::ExprCallBuiltin(AverStr::from("Int.mod"), aver_rt::AverList::from_vec(vec![a.clone(), b.clone()])), Expr::ExprInt(defaultValue)])) } }
}

/// Return whether an expression is already in a slot-based internal shape worth fusing around.
pub fn exprHasSlotShape(expr: &Expr) -> bool {
    crate::cancel_checkpoint();
    match expr {
        Expr::ExprSlot(_) => {
            true
        },
        Expr::ExprBinopSlotInt(_, _, _) => {
            true
        },
        Expr::ExprBinopSlots(_, _, _) => {
            true
        },
        Expr::ExprCmpSlotInt(_, _, _) => {
            true
        },
        Expr::ExprCmpSlots(_, _, _) => {
            true
        },
        Expr::ExprVectorGetOrInt(_, _, _) => {
            true
        },
        Expr::ExprIntModOrInt(_, _, _) => {
            true
        },
        _ => {
            false
        }
    }
}

/// Compress simple resolved comparisons into slot-based internal nodes.
pub fn rewriteInternalCmp(op: &CmpOp, left: &Expr, right: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match left.clone() {
        Expr::ExprSlot(slot) => {
            rewriteInternalCmpSlotLeft(op, slot, right)
        },
        _ => {
            match right.clone() {
        Expr::ExprSlot(slot) => {
            rewriteInternalCmpSlotRight(op, left, slot)
        },
        _ => {
            rebuildInternalCmp(op, left, right)
        }
    }
        }
    }
}

/// Rewrite comparisons with a slot on the left when the right side is simple.
pub fn rewriteInternalCmpSlotLeft(op: &CmpOp, slot: i64, right: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match right.clone() {
        Expr::ExprInt(n) => {
            Expr::ExprCmpSlotInt(op.clone(), slot, n)
        },
        Expr::ExprSlot(rhs) => {
            Expr::ExprCmpSlots(op.clone(), slot, rhs)
        },
        _ => {
            rebuildInternalCmp(op, &Expr::ExprSlot(slot), right)
        }
    }
}

/// Rewrite comparisons with a slot on the right when the left side is a simple integer.
pub fn rewriteInternalCmpSlotRight(op: &CmpOp, left: &Expr, slot: i64) -> Expr {
    crate::cancel_checkpoint();
    match left.clone() {
        Expr::ExprInt(n) => {
            Expr::ExprCmpSlotInt(flipCmp(op), slot, n)
        },
        Expr::ExprSlot(lhs) => {
            Expr::ExprCmpSlots(op.clone(), lhs, slot)
        },
        _ => {
            rebuildInternalCmp(op, left, &Expr::ExprSlot(slot))
        }
    }
}

/// Rebuild a generic comparison expression when no narrower internal form applies.
pub fn rebuildInternalCmp(op: &CmpOp, left: &Expr, right: &Expr) -> Expr {
    crate::cancel_checkpoint();
    match op {
        CmpOp::CmpEq => {
            Expr::ExprEq(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone()))
        },
        CmpOp::CmpNeq => {
            Expr::ExprNeq(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone()))
        },
        CmpOp::CmpLt => {
            Expr::ExprLt(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone()))
        },
        CmpOp::CmpGt => {
            Expr::ExprGt(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone()))
        },
        CmpOp::CmpLte => {
            Expr::ExprLte(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone()))
        },
        CmpOp::CmpGte => {
            Expr::ExprGte(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone()))
        }
    }
}

/// Flip a comparison when swapping operand order.
pub fn flipCmp(op: &CmpOp) -> CmpOp {
    crate::cancel_checkpoint();
    match op {
        CmpOp::CmpEq => {
            CmpOp::CmpEq.clone()
        },
        CmpOp::CmpNeq => {
            CmpOp::CmpNeq.clone()
        },
        CmpOp::CmpLt => {
            CmpOp::CmpGt.clone()
        },
        CmpOp::CmpGt => {
            CmpOp::CmpLt.clone()
        },
        CmpOp::CmpLte => {
            CmpOp::CmpGte.clone()
        },
        CmpOp::CmpGte => {
            CmpOp::CmpLte.clone()
        }
    }
}

/// Rewrite simple bool matches into a direct branch node.
#[inline(always)]
pub fn rewriteInternalMatch(scrutinee: &Expr, arms: &aver_rt::AverList<MatchArm>) -> Expr {
    crate::cancel_checkpoint();
    match rewriteBoolMatchArms(arms) { Some(pair) => { { let (thenExpr, elseExpr) = pair; Expr::ExprBoolBranch(std::sync::Arc::new(scrutinee.clone()), std::sync::Arc::new(thenExpr), std::sync::Arc::new(elseExpr)) } }, None => { Expr::ExprMatch(std::sync::Arc::new(scrutinee.clone()), arms.clone()) } }
}

/// Extract (thenExpr, elseExpr) from a two-arm bool match regardless of arm order.
pub fn rewriteBoolMatchArms(arms: &aver_rt::AverList<MatchArm>) -> Option<(Expr, Expr)> {
    crate::cancel_checkpoint();
    { let __list_subject = arms.clone(); if let Some((arm1, rest)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = rest; if let Some((arm2, tail)) = aver_rt::list_uncons_cloned(&__list_subject) { if (tail == aver_rt::AverList::empty()) { rewriteBoolMatchArmPair(&arm1, &arm2) } else { None } } else { None } } } else { None } }
}

/// Order two complementary bool arms into (trueExpr, falseExpr).
pub fn rewriteBoolMatchArmPair(arm1: &MatchArm, arm2: &MatchArm) -> Option<(Expr, Expr)> {
    crate::cancel_checkpoint();
    match arm1.pattern.clone() {
        Pattern::PatBool(b1) => {
            rewriteBoolMatchArmPairInner(b1, &arm1.body, &arm2.pattern, &arm2.body)
        },
        _ => {
            None
        }
    }
}

/// Finish ordering bool arms once the first pattern bool is known.
pub fn rewriteBoolMatchArmPairInner(b1: bool, body1: &Expr, p2: &Pattern, body2: &Expr) -> Option<(Expr, Expr)> {
    crate::cancel_checkpoint();
    match p2.clone() {
        Pattern::PatBool(b2) => {
            rewriteBoolMatchArmPairBools(b1, body1, b2, body2)
        },
        _ => {
            None
        }
    }
}

/// Return ordered bool branch bodies when the pair is exactly true/false or false/true.
pub fn rewriteBoolMatchArmPairBools(b1: bool, body1: &Expr, b2: bool, body2: &Expr) -> Option<(Expr, Expr)> {
    crate::cancel_checkpoint();
    match (b1, b2) {
        (true, false) => {
            Some((body1.clone(), body2.clone()))
        },
        (false, true) => {
            Some((body2.clone(), body1.clone()))
        },
        _ => {
            None
        }
    }
}
