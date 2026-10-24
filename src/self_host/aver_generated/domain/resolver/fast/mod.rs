#[allow(unused_imports)]
use crate::*;

/// Tag simple single-expression functions so eval can skip the stmt walker.
#[inline(always)]
pub fn annotateFastFns(
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
            if __list_subject.is_empty() {
                return acc.reverse();
            } else {
                if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco2 = aver_rt::AverList::prepend_owned(
                            crate::aver_generated::domain::resolver::fast::annotateFastFn(
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
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Attach a narrow fast-path tag to a function definition.
pub fn annotateFastFn(
    mut fd @ _: crate::aver_generated::domain::ast::FnDef,
    fnMap @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    let selfId @ _ = fnMap
        .get(&fd.name)
        .cloned()
        .unwrap_or(aver_rt::AverInt::from_i64(-1));
    Box::new(crate::aver_generated::domain::ast::FnDef__Fields {
        name: fd.name,
        params: fd.params,
        body: fd.body.clone(),
        slotCount: fd.slotCount,
        slotMap: fd.slotMap,
        fastPath: crate::aver_generated::domain::resolver::fast::classifyFastPath(&fd.body),
        tailLoop: crate::aver_generated::domain::resolver::fast::classifyTailLoop(selfId, &fd.body),
    })
}

/// Precompute whether the final expression position can self-tail-call directly.
#[inline(always)]
pub fn classifyTailLoop(
    mut selfId @ _: aver_rt::AverInt,
    body @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> bool {
    let mut __hold1: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut body @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = body;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = body;
            if __list_subject.is_empty() {
                return false;
            } else {
                if let (Some(stmt), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = rest.clone();
                        if __list_subject.is_empty() {
                            return crate::aver_generated::domain::resolver::fast::stmtNeedsTailLoop(selfId, stmt);
                        } else {
                            {
                                let __tco1 = rest;
                                __hold1 = __tco1;
                                body = &__hold1;
                                continue;
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Only the final expression statement can trigger the tail-loop slot evaluator.
pub fn stmtNeedsTailLoop(
    selfId @ _: aver_rt::AverInt,
    stmt @ _: &crate::aver_generated::domain::ast::Stmt,
) -> bool {
    crate::cancel_checkpoint();
    match stmt {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::resolver::fast::exprNeedsTailLoop(selfId, expr)
        }
        _ => false,
    }
}

/// Recognize direct self-calls in tail position, including bool branches and matches.
pub fn exprNeedsTailLoop(
    mut selfId @ _: aver_rt::AverInt,
    expr @ _: &crate::aver_generated::domain::ast::Expr,
) -> bool {
    let mut __hold1: crate::aver_generated::domain::ast::Expr;
    let mut expr @ _: &crate::aver_generated::domain::ast::Expr = expr;
    loop {
        crate::cancel_checkpoint();
        {
            let __t = expr;
            match __t.view() {
                crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(__fnId_fields) => {
                    let fnId = __fnId_fields.0.clone();
                    return (fnId == selfId);
                }
                crate::aver_generated::domain::ast::Expr__view::ExprBoolBranch(
                    __thenExpr_fields,
                ) => {
                    let thenExpr = &__thenExpr_fields.1;
                    let elseExpr = &__thenExpr_fields.2;
                    if crate::aver_generated::domain::resolver::fast::exprNeedsTailLoop(
                        selfId.clone(),
                        thenExpr,
                    ) {
                        return true;
                    } else {
                        {
                            let __tco1 = elseExpr.clone();
                            __hold1 = __tco1;
                            expr = &__hold1;
                            continue;
                        }
                    }
                }
                crate::aver_generated::domain::ast::Expr__view::ExprMatch(__arms_fields) => {
                    let arms = &__arms_fields.1;
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
}

/// Return true when any match arm ends in a direct self-tail-call.
#[inline(always)]
pub fn armsNeedTailLoop(
    mut selfId @ _: aver_rt::AverInt,
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> bool {
    let mut __hold1: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>;
    let mut arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> = arms;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = arms;
            if __list_subject.is_empty() {
                return false;
            } else {
                if let (Some(arm), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if crate::aver_generated::domain::resolver::fast::exprNeedsTailLoop(
                        selfId.clone(),
                        &arm.body,
                    ) {
                        return true;
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            arms = &__hold1;
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

/// Single expression bodies can bypass evalStmts* and evaluate the expr directly.
pub fn classifyFastPath(
    body @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    {
        let __list_subject = body;
        if let (Some(stmt), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            if (rest == aver_rt::AverList::empty()) {
                crate::aver_generated::domain::resolver::fast::classifyFastStmt(stmt)
            } else {
                crate::aver_generated::domain::ast::FnFastPath::FastNone
            }
        } else {
            crate::aver_generated::domain::ast::FnFastPath::FastNone
        }
    }
}

/// Only plain expression bodies get a fast expr tag.
pub fn classifyFastStmt(
    stmt @ _: &crate::aver_generated::domain::ast::Stmt,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    match stmt {
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::resolver::fast::classifyFastExpr(expr)
        }
        _ => crate::aver_generated::domain::ast::FnFastPath::FastNone,
    }
}

/// Recognize a few leaf-like and branch-like expr shapes worth running without the full stmt walker.
#[inline(always)]
pub fn classifyFastExpr(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::resolver::fast::classifyFastLeafExpr(expr) {
        Some(leaf @ _) => crate::aver_generated::domain::ast::FnFastPath::FastLeaf(leaf),
        None => {
            let __t = expr;
            match __t.view() {
                crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(__fnId_fields) => {
                    let fnId = __fnId_fields.0.clone();
                    let args = &__fnId_fields.1;
                    crate::aver_generated::domain::resolver::fast::classifyFastForwardCall(
                        fnId, args,
                    )
                }
                crate::aver_generated::domain::ast::Expr__view::ExprMatch(__scrutinee_fields) => {
                    let scrutinee = &__scrutinee_fields.0;
                    let arms = &__scrutinee_fields.1;
                    crate::aver_generated::domain::resolver::fast::classifyFastMatch(
                        scrutinee, arms,
                    )
                }
                _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
            }
        }
    }
}

/// Recognize a leaf-like expr that can be executed without descending the AST.
pub fn classifyFastLeafExpr(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__n_fields) => {
                let n = __n_fields.0.clone();
                Some(crate::aver_generated::domain::ast::FastLeaf::LeafConstInt(
                    n,
                ))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFloat(__f_fields) => {
                let f = __f_fields.0.clone();
                Some(crate::aver_generated::domain::ast::FastLeaf::LeafConstFloat(f))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprStr(__s_fields) => {
                let s = __s_fields.0.clone();
                Some(crate::aver_generated::domain::ast::FastLeaf::LeafConstStr(
                    s,
                ))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprBool(__b_fields) => {
                let b = __b_fields.0.clone();
                Some(crate::aver_generated::domain::ast::FastLeaf::LeafConstBool(
                    b,
                ))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                Some(crate::aver_generated::domain::ast::FastLeaf::LeafSlot(slot))
            }
            crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(__obj_fields) => {
                let obj = &__obj_fields.0;
                let field = __obj_fields.1.clone();
                crate::aver_generated::domain::resolver::fast::classifyFastFieldAccess(obj, field)
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = &__name_fields.1;
                crate::aver_generated::domain::resolver::fast::classifyFastBuiltinLeaf(name, args)
            }
            crate::aver_generated::domain::ast::Expr__view::ExprAdd(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg14 @ _ = crate::aver_generated::domain::ast::BinOp::OpAdd;
                    crate::aver_generated::domain::resolver::fast::classifyFastBinopSlots(
                        __arg14, a, b,
                    )
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSub(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg15 @ _ = crate::aver_generated::domain::ast::BinOp::OpSub;
                    crate::aver_generated::domain::resolver::fast::classifyFastBinopSlots(
                        __arg15, a, b,
                    )
                }
            }
            _ => crate::aver_generated::domain::resolver::fast::classifyFastLeafExprTail(expr),
        }
    }
}

/// Finish fast-leaf classification for arithmetic and comparison forms.
pub fn classifyFastLeafExprTail(
    expr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprMul(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg17 @ _ = crate::aver_generated::domain::ast::BinOp::OpMul;
                    crate::aver_generated::domain::resolver::fast::classifyFastBinopSlots(
                        __arg17, a, b,
                    )
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprDiv(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg18 @ _ = crate::aver_generated::domain::ast::BinOp::OpDiv;
                    crate::aver_generated::domain::resolver::fast::classifyFastBinopSlots(
                        __arg18, a, b,
                    )
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprEq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg19 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpEq;
                    crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                        __arg19, a, b,
                    )
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg20 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpNeq;
                    crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                        __arg20, a, b,
                    )
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg21 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpLt;
                    crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                        __arg21, a, b,
                    )
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg22 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpGt;
                    crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                        __arg22, a, b,
                    )
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg23 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpLte;
                    crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                        __arg23, a, b,
                    )
                }
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                {
                    let __arg24 @ _ = crate::aver_generated::domain::ast::CmpOp::CmpGte;
                    crate::aver_generated::domain::resolver::fast::classifyFastCmpSlots(
                        __arg24, a, b,
                    )
                }
            }
            _ => None,
        }
    }
}

/// Recognize obj.field when obj is already a resolved slot.
pub fn classifyFastFieldAccess(
    obj @ _: &crate::aver_generated::domain::ast::Expr,
    field @ _: AverStr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = obj;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                Some(crate::aver_generated::domain::ast::FastLeaf::LeafFieldAccess(slot, field))
            }
            _ => None,
        }
    }
}

/// Recognize slot op slot for arithmetic.
pub fn classifyFastBinopSlots(
    mut op @ _: crate::aver_generated::domain::ast::BinOp,
    a @ _: &crate::aver_generated::domain::ast::Expr,
    b @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = a;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__sa_fields) => {
                let sa = __sa_fields.0.clone();
                {
                    let __t = b;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprSlot(__sb_fields) => {
                            let sb = __sb_fields.0.clone();
                            Some(
                                crate::aver_generated::domain::ast::FastLeaf::LeafBinopSlots(
                                    op, sa, sb,
                                ),
                            )
                        }
                        _ => None,
                    }
                }
            }
            _ => None,
        }
    }
}

/// Recognize slot cmp slot for comparisons.
pub fn classifyFastCmpSlots(
    mut op @ _: crate::aver_generated::domain::ast::CmpOp,
    a @ _: &crate::aver_generated::domain::ast::Expr,
    b @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = a;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__sa_fields) => {
                let sa = __sa_fields.0.clone();
                {
                    let __t = b;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprSlot(__sb_fields) => {
                            let sb = __sb_fields.0.clone();
                            Some(crate::aver_generated::domain::ast::FastLeaf::LeafCmpSlots(
                                op, sa, sb,
                            ))
                        }
                        _ => None,
                    }
                }
            }
            _ => None,
        }
    }
}

/// Recognize tiny builtin wrappers that only shuffle slots and integer constants.
#[inline(always)]
pub fn classifyFastBuiltinLeaf(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
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
pub fn classifyFastMapGet(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(mapExpr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(keyExpr), Some(ignored)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    crate::aver_generated::domain::resolver::fast::classifyFastMapGetArgs(
                        mapExpr, keyExpr,
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
pub fn classifyFastMapGetArgs(
    mapExpr @ _: &crate::aver_generated::domain::ast::Expr,
    keyExpr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = mapExpr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__mapSlot_fields) => {
                let mapSlot = __mapSlot_fields.0.clone();
                {
                    let __t = keyExpr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprSlot(
                            __keySlot_fields,
                        ) => {
                            let keySlot = __keySlot_fields.0.clone();
                            Some(crate::aver_generated::domain::ast::FastLeaf::LeafMapGet(
                                mapSlot, keySlot,
                            ))
                        }
                        _ => None,
                    }
                }
            }
            _ => None,
        }
    }
}

/// Recognize Map.set(slotMap, slotKey, slotValue).
pub fn classifyFastMapSet(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(mapExpr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(keyExpr), Some(rest2)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    {
                        let __list_subject = rest2;
                        if let (Some(valueExpr), Some(ignored)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            crate::aver_generated::domain::resolver::fast::classifyFastMapSetArgs(
                                mapExpr, keyExpr, valueExpr,
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
    mapExpr @ _: &crate::aver_generated::domain::ast::Expr,
    keyExpr @ _: &crate::aver_generated::domain::ast::Expr,
    valueExpr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = mapExpr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__mapSlot_fields) => {
                let mapSlot = __mapSlot_fields.0.clone();
                {
                    let __t = keyExpr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprSlot(
                            __keySlot_fields,
                        ) => {
                            let keySlot = __keySlot_fields.0.clone();
                            {
                                let __t = valueExpr;
                                match __t.view() {
                                    crate::aver_generated::domain::ast::Expr__view::ExprSlot(
                                        __valueSlot_fields,
                                    ) => {
                                        let valueSlot = __valueSlot_fields.0.clone();
                                        Some(crate::aver_generated::domain::ast::FastLeaf::LeafMapSet(mapSlot, keySlot, valueSlot))
                                    }
                                    _ => None,
                                }
                            }
                        }
                        _ => None,
                    }
                }
            }
            _ => None,
        }
    }
}

/// Recognize Map.has(slotMap, slotKey).
pub fn classifyFastMapHas(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(mapExpr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(keyExpr), Some(ignored)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    crate::aver_generated::domain::resolver::fast::classifyFastMapHasArgs(
                        mapExpr, keyExpr,
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
pub fn classifyFastMapHasArgs(
    mapExpr @ _: &crate::aver_generated::domain::ast::Expr,
    keyExpr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = mapExpr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__mapSlot_fields) => {
                let mapSlot = __mapSlot_fields.0.clone();
                {
                    let __t = keyExpr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprSlot(
                            __keySlot_fields,
                        ) => {
                            let keySlot = __keySlot_fields.0.clone();
                            Some(crate::aver_generated::domain::ast::FastLeaf::LeafMapHas(
                                mapSlot, keySlot,
                            ))
                        }
                        _ => None,
                    }
                }
            }
            _ => None,
        }
    }
}

/// Recognize Map.remove(slotMap, slotKey).
pub fn classifyFastMapRemove(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(mapExpr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(keyExpr), Some(ignored)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    crate::aver_generated::domain::resolver::fast::classifyFastMapRemoveArgs(
                        mapExpr, keyExpr,
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
pub fn classifyFastMapRemoveArgs(
    mapExpr @ _: &crate::aver_generated::domain::ast::Expr,
    keyExpr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = mapExpr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__mapSlot_fields) => {
                let mapSlot = __mapSlot_fields.0.clone();
                {
                    let __t = keyExpr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprSlot(
                            __keySlot_fields,
                        ) => {
                            let keySlot = __keySlot_fields.0.clone();
                            Some(crate::aver_generated::domain::ast::FastLeaf::LeafMapRemove(
                                mapSlot, keySlot,
                            ))
                        }
                        _ => None,
                    }
                }
            }
            _ => None,
        }
    }
}

/// Recognize Vector.len(slotVec).
pub fn classifyFastVectorLen(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(vecExpr), Some(ignored)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __t = vecExpr;
                match __t.view() {
                    crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                        let slot = __slot_fields.0.clone();
                        Some(crate::aver_generated::domain::ast::FastLeaf::LeafVectorLen(
                            slot,
                        ))
                    }
                    _ => None,
                }
            }
        } else {
            None
        }
    }
}

/// Recognize Vector.new(slot, int) wrappers.
pub fn classifyFastVectorNew(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(sizeExpr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(fillExpr), Some(ignored)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    crate::aver_generated::domain::resolver::fast::classifyFastVectorNewArgs(
                        sizeExpr, fillExpr,
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
pub fn classifyFastVectorNewArgs(
    sizeExpr @ _: &crate::aver_generated::domain::ast::Expr,
    fillExpr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = sizeExpr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__sizeSlot_fields) => {
                let sizeSlot = __sizeSlot_fields.0.clone();
                {
                    let __t = fillExpr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprInt(__fill_fields) => {
                            let fill = __fill_fields.0.clone();
                            Some(crate::aver_generated::domain::ast::FastLeaf::LeafVectorNew(
                                sizeSlot, fill,
                            ))
                        }
                        _ => None,
                    }
                }
            }
            _ => None,
        }
    }
}

/// Recognize Option.withDefault(Vector.get(slot, slot), int).
pub fn classifyFastOptionWithDefault(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(optionExpr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(defaultExpr), Some(ignored)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    crate::aver_generated::domain::resolver::fast::classifyFastOptionWithDefaultArgs(
                        optionExpr,
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

/// Encode the common Vector.get-with-default wrapper used in tiny helpers like cellAt.
pub fn classifyFastOptionWithDefaultArgs(
    optionExpr @ _: &crate::aver_generated::domain::ast::Expr,
    defaultExpr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = optionExpr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(__name_fields) => {
                let name = __name_fields.0.clone();
                let innerArgs = &__name_fields.1;
                if (&*name == "Vector.get") {
                    crate::aver_generated::domain::resolver::fast::classifyFastVectorGet(
                        innerArgs,
                        defaultExpr,
                    )
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

/// Recognize Vector.get(slotVec, slotIdx) with an integer default.
pub fn classifyFastVectorGet(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    defaultExpr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(vecExpr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(idxExpr), Some(ignored)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    crate::aver_generated::domain::resolver::fast::classifyFastVectorGetArgs(
                        vecExpr,
                        idxExpr,
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
    vecExpr @ _: &crate::aver_generated::domain::ast::Expr,
    idxExpr @ _: &crate::aver_generated::domain::ast::Expr,
    defaultExpr @ _: &crate::aver_generated::domain::ast::Expr,
) -> Option<crate::aver_generated::domain::ast::FastLeaf> {
    crate::cancel_checkpoint();
    {
        let __t = vecExpr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__vecSlot_fields) => {
                let vecSlot = __vecSlot_fields.0.clone();
                {
                    let __t = idxExpr;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprSlot(
                            __idxSlot_fields,
                        ) => {
                            let idxSlot = __idxSlot_fields.0.clone();
                            {
                                let __t = defaultExpr;
                                match __t.view() {
                                    crate::aver_generated::domain::ast::Expr__view::ExprInt(
                                        __defaultValue_fields,
                                    ) => {
                                        let defaultValue = __defaultValue_fields.0.clone();
                                        Some(crate::aver_generated::domain::ast::FastLeaf::LeafVectorGetOrInt(vecSlot, idxSlot, defaultValue))
                                    }
                                    _ => None,
                                }
                            }
                        }
                        _ => None,
                    }
                }
            }
            _ => None,
        }
    }
}

/// Recognize a small fixed-shape branch over bools or slot-based comparisons.
#[inline(always)]
pub fn classifyFastMatch(
    scrutinee @ _: &crate::aver_generated::domain::ast::Expr,
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::resolver::fast::classifyBoolArms(arms) {
        Some(pair @ _) => {
            let (thenLeaf, elseLeaf) = pair;
            crate::aver_generated::domain::resolver::fast::classifyFastMatchScrutinee(
                scrutinee, thenLeaf, elseLeaf,
            )
        }
        None => {
            crate::aver_generated::domain::resolver::fast::classifyFastListMatch(scrutinee, arms)
        }
    }
}

/// Recognize a two-arm list match with [] and [h, ..t] leaf bodies.
pub fn classifyFastListMatch(
    scrutinee @ _: &crate::aver_generated::domain::ast::Expr,
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    {
        let __t = scrutinee;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                crate::aver_generated::domain::resolver::fast::classifyFastListArms(slot, arms)
            }
            _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        }
    }
}

/// Extract fixed empty/cons list arms regardless of order.
pub fn classifyFastListArms(
    slot @ _: aver_rt::AverInt,
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    {
        let __list_subject = arms;
        if let (Some(arm1), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(arm2), Some(tail)) = (__list_subject.first(), __list_subject.tail()) {
                    if (tail == aver_rt::AverList::empty()) {
                        crate::aver_generated::domain::resolver::fast::classifyFastListArmPair(
                            slot, arm1, arm2,
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
    slot @ _: aver_rt::AverInt,
    arm1 @ _: &crate::aver_generated::domain::ast::MatchArm,
    arm2 @ _: &crate::aver_generated::domain::ast::MatchArm,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    let leaf1 @ _ = crate::aver_generated::domain::resolver::fast::classifyFastLeafExpr(&arm1.body);
    let leaf2 @ _ = crate::aver_generated::domain::resolver::fast::classifyFastLeafExpr(&arm2.body);
    match leaf1 {
        Some(v1 @ _) => match leaf2 {
            Some(v2 @ _) => {
                crate::aver_generated::domain::resolver::fast::classifyFastListPatterns(
                    slot,
                    &arm1.pattern,
                    &arm1.bindingSlots,
                    v1,
                    &arm2.pattern,
                    &arm2.bindingSlots,
                    v2,
                )
            }
            None => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        },
        None => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Order empty/cons arms into a direct list branch.
pub fn classifyFastListPatterns(
    slot @ _: aver_rt::AverInt,
    p1 @ _: &crate::aver_generated::domain::ast::Pattern,
    bindingSlots1 @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut leaf1 @ _: crate::aver_generated::domain::ast::FastLeaf,
    p2 @ _: &crate::aver_generated::domain::ast::Pattern,
    bindingSlots2 @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut leaf2 @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    match p1 {
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
            let head = head.clone();
            let tail = tail.clone();
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
    slot @ _: aver_rt::AverInt,
    mut emptyLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
    other @ _: &crate::aver_generated::domain::ast::Pattern,
    bindingSlots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut otherLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    match other {
        crate::aver_generated::domain::ast::Pattern::PatCons(head, tail) => {
            let head = head.clone();
            let tail = tail.clone();
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
    slot @ _: aver_rt::AverInt,
    head @ _: AverStr,
    tail @ _: AverStr,
    bindingSlots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut consLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
    other @ _: &crate::aver_generated::domain::ast::Pattern,
    mut otherLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> crate::aver_generated::domain::ast::FnFastPath {
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
    slot @ _: aver_rt::AverInt,
    mut emptyLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
    head @ _: AverStr,
    tail @ _: AverStr,
    bindingSlots @ _: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut consLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    match (bindingSlots).get(&head) {
        Some(headSlot @ _) => {
            let headSlot = headSlot.clone();
            match (bindingSlots).get(&tail) {
                Some(tailSlot @ _) => {
                    let tailSlot = tailSlot.clone();
                    crate::aver_generated::domain::ast::FnFastPath::FastListSlotBranch(
                        slot, emptyLeaf, headSlot, tailSlot, consLeaf,
                    )
                }
                None => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
            }
        }
        None => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
    }
}

/// Recognize tiny direct-call wrappers that only forward slot arguments.
#[inline(always)]
pub fn classifyFastForwardCall(
    fnId @ _: aver_rt::AverInt,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    {
        let __subject = {
            let __arg3 @ _ = aver_rt::int_list_builder_new(
                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
            );
            crate::aver_generated::domain::resolver::fast::classifyFastForwardSlots__collected(
                args, __arg3,
            )
        };
        match __subject {
            Some(slotArgs @ _) => {
                crate::aver_generated::domain::ast::FnFastPath::FastForwardCall(fnId, slotArgs)
            }
            None => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        }
    }
}

/// Extract slot numbers from a direct call argument list.
#[inline(always)]
pub fn classifyFastForwardSlots(
    mut args @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut acc @ _: aver_rt::AverIntList,
) -> Option<aver_rt::AverIntList> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(args, [] => { return Some(acc.reverse()); }, [arg, rest] => { { let __t = arg; match __t.view() {
        crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
            let slot = __slot_fields.0.clone();
            {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverIntList::prepend_owned(slot, acc);
            args = __tco0;
            acc = __tco1;
            continue;
        }
        },
        _ => {
            return None;
        }
    } } })
    }
}

/// Extract (then, else) leaves from a two-arm bool match, regardless of arm order.
pub fn classifyBoolArms(
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> Option<(
    crate::aver_generated::domain::ast::FastLeaf,
    crate::aver_generated::domain::ast::FastLeaf,
)> {
    crate::cancel_checkpoint();
    {
        let __list_subject = arms;
        if let (Some(arm1), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(arm2), Some(tail)) = (__list_subject.first(), __list_subject.tail()) {
                    if (tail == aver_rt::AverList::empty()) {
                        crate::aver_generated::domain::resolver::fast::classifyBoolArmPair(
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

/// Convert two bool-pattern arms into ordered leaves.
#[inline(always)]
pub fn classifyBoolArmPair(
    arm1 @ _: &crate::aver_generated::domain::ast::MatchArm,
    arm2 @ _: &crate::aver_generated::domain::ast::MatchArm,
) -> Option<(
    crate::aver_generated::domain::ast::FastLeaf,
    crate::aver_generated::domain::ast::FastLeaf,
)> {
    crate::cancel_checkpoint();
    let leaf1 @ _ = crate::aver_generated::domain::resolver::fast::classifyFastLeafExpr(&arm1.body);
    let leaf2 @ _ = crate::aver_generated::domain::resolver::fast::classifyFastLeafExpr(&arm2.body);
    match leaf1 {
        Some(v1 @ _) => match leaf2 {
            Some(v2 @ _) => crate::aver_generated::domain::resolver::fast::classifyBoolArmPatterns(
                &arm1.pattern,
                v1,
                &arm2.pattern,
                v2,
            ),
            None => None,
        },
        None => None,
    }
}

/// Order bool match arms into (trueLeaf, falseLeaf).
pub fn classifyBoolArmPatterns(
    p1 @ _: &crate::aver_generated::domain::ast::Pattern,
    mut leaf1 @ _: crate::aver_generated::domain::ast::FastLeaf,
    p2 @ _: &crate::aver_generated::domain::ast::Pattern,
    mut leaf2 @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> Option<(
    crate::aver_generated::domain::ast::FastLeaf,
    crate::aver_generated::domain::ast::FastLeaf,
)> {
    crate::cancel_checkpoint();
    match p1 {
        crate::aver_generated::domain::ast::Pattern::PatBool(b1) => {
            let b1 = b1.clone();
            crate::aver_generated::domain::resolver::fast::classifyBoolArmPatternsInner(
                b1, leaf1, p2, leaf2,
            )
        }
        _ => None,
    }
}

/// Finish ordering bool match arms once the first arm bool is extracted.
pub fn classifyBoolArmPatternsInner(
    b1 @ _: bool,
    mut leaf1 @ _: crate::aver_generated::domain::ast::FastLeaf,
    p2 @ _: &crate::aver_generated::domain::ast::Pattern,
    mut leaf2 @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> Option<(
    crate::aver_generated::domain::ast::FastLeaf,
    crate::aver_generated::domain::ast::FastLeaf,
)> {
    crate::cancel_checkpoint();
    match p2 {
        crate::aver_generated::domain::ast::Pattern::PatBool(b2) => {
            let b2 = b2.clone();
            crate::aver_generated::domain::resolver::fast::classifyBoolArmPatternsPair(
                b1, leaf1, b2, leaf2,
            )
        }
        _ => None,
    }
}

/// Return (trueLeaf, falseLeaf) when the two bool arms are complementary.
pub fn classifyBoolArmPatternsPair(
    b1 @ _: bool,
    mut leaf1 @ _: crate::aver_generated::domain::ast::FastLeaf,
    b2 @ _: bool,
    mut leaf2 @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> Option<(
    crate::aver_generated::domain::ast::FastLeaf,
    crate::aver_generated::domain::ast::FastLeaf,
)> {
    crate::cancel_checkpoint();
    match (b1, b2) {
        (true, false) => Some((leaf1, leaf2)),
        (false, true) => Some((leaf2, leaf1)),
        _ => None,
    }
}

/// Encode a recognized branch scrutinee with preclassified leaves.
pub fn classifyFastMatchScrutinee(
    scrutinee @ _: &crate::aver_generated::domain::ast::Expr,
    mut thenLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
    mut elseLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    {
        let __t = scrutinee;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                crate::aver_generated::domain::ast::FnFastPath::FastBoolSlotBranch(
                    slot, thenLeaf, elseLeaf,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprEq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::fast::classifyFastEqScrutinee(
                    a, b, thenLeaf, elseLeaf,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::fast::classifyFastLtScrutinee(
                    a, b, thenLeaf, elseLeaf,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::resolver::fast::classifyFastLtScrutinee(
                    b, a, thenLeaf, elseLeaf,
                )
            }
            _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        }
    }
}

/// Recognize slot == int/string in either operand order.
pub fn classifyFastEqScrutinee(
    a @ _: &crate::aver_generated::domain::ast::Expr,
    b @ _: &crate::aver_generated::domain::ast::Expr,
    mut thenLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
    mut elseLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    {
        let __t = a;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                let slot = __slot_fields.0.clone();
                crate::aver_generated::domain::resolver::fast::classifyFastEqOther(
                    slot, b, thenLeaf, elseLeaf,
                )
            }
            _ => {
                let __t = b;
                match __t.view() {
                    crate::aver_generated::domain::ast::Expr__view::ExprSlot(__slot_fields) => {
                        let slot = __slot_fields.0.clone();
                        crate::aver_generated::domain::resolver::fast::classifyFastEqOther(
                            slot, a, thenLeaf, elseLeaf,
                        )
                    }
                    _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
                }
            }
        }
    }
}

/// Encode equality against a constant once the slot side is known.
pub fn classifyFastEqOther(
    slot @ _: aver_rt::AverInt,
    other @ _: &crate::aver_generated::domain::ast::Expr,
    mut thenLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
    mut elseLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    {
        let __t = other;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprInt(__n_fields) => {
                let n = __n_fields.0.clone();
                crate::aver_generated::domain::ast::FnFastPath::FastEqIntBranch(
                    slot, n, thenLeaf, elseLeaf,
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprStr(__s_fields) => {
                let s = __s_fields.0.clone();
                crate::aver_generated::domain::ast::FnFastPath::FastEqStringBranch(
                    slot, s, thenLeaf, elseLeaf,
                )
            }
            _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        }
    }
}

/// Recognize slot < slot branches like minInt.
pub fn classifyFastLtScrutinee(
    a @ _: &crate::aver_generated::domain::ast::Expr,
    b @ _: &crate::aver_generated::domain::ast::Expr,
    mut thenLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
    mut elseLeaf @ _: crate::aver_generated::domain::ast::FastLeaf,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    {
        let __t = a;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprSlot(__lhs_fields) => {
                let lhs = __lhs_fields.0.clone();
                {
                    let __t = b;
                    match __t.view() {
                        crate::aver_generated::domain::ast::Expr__view::ExprSlot(__rhs_fields) => {
                            let rhs = __rhs_fields.0.clone();
                            crate::aver_generated::domain::ast::FnFastPath::FastLtIntSlotsBranch(
                                lhs, rhs, thenLeaf, elseLeaf,
                            )
                        }
                        _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
                    }
                }
            }
            _ => crate::aver_generated::domain::ast::FnFastPath::FastSingleExpr,
        }
    }
}

/// Synthesized collecting variant of `classifyFastForwardSlots`. Appends to a builder where `classifyFastForwardSlots` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn classifyFastForwardSlots__collected(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut acc @ _: aver_rt::AverIntList,
) -> Option<aver_rt::AverIntList> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut args @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = args;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = args;
            if __list_subject.is_empty() {
                return Some(aver_rt::int_list_builder_finalize(acc));
            } else {
                if let (Some(arg), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __t = arg;
                        match __t.view() {
                            crate::aver_generated::domain::ast::Expr__view::ExprSlot(
                                __slot_fields,
                            ) => {
                                let slot = __slot_fields.0.clone();
                                {
                                    let __tco0 = rest;
                                    let __tco1 = aver_rt::int_list_builder_push(acc, slot);
                                    __hold0 = __tco0;
                                    args = &__hold0;
                                    acc = __tco1;
                                    continue;
                                }
                            }
                            _ => {
                                return None;
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}
