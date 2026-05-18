#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::*;

/// Build name→fnId map from resolved function list.
#[inline(always)]
pub fn buildFnMap(
    mut fns: aver_rt::AverList<FnDef>,
    mut acc: aver_rt::AverMap<AverStr, i64>,
    mut idx: i64,
) -> aver_rt::AverMap<AverStr, i64> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fns, [] => acc, [f, rest] => { {
            let __tmp1 = acc.insert_owned(f.name.clone(), idx);
            let __tmp2 = (idx + 1i64);
            fns = rest;
            acc = __tmp1;
            idx = __tmp2;
            continue;
        } });
    }
}

/// Transform ExprCall→ExprCallDirect for known functions in all fn bodies.
#[inline(always)]
pub fn resolveCallsInFns(
    mut fns: aver_rt::AverList<FnDef>,
    mut fnMap: aver_rt::AverMap<AverStr, i64>,
    mut acc: aver_rt::AverList<FnDef>,
) -> aver_rt::AverList<FnDef> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fns, [] => acc.reverse(), [f, rest] => { {
            let __tmp1 = fnMap.clone();
            let __tmp2 = aver_rt::AverList::prepend(resolveCallsInFn(&f, &fnMap), &acc);
            fns = rest;
            fnMap = __tmp1;
            acc = __tmp2;
            continue;
        } });
    }
}

/// Transform calls in one function body.
pub fn resolveCallsInFn(fd: &FnDef, fnMap: &aver_rt::AverMap<AverStr, i64>) -> FnDef {
    crate::cancel_checkpoint();
    FnDef {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: resolveCallsInStmts(fd.body.clone(), fnMap.clone(), aver_rt::AverList::empty()),
        slotCount: fd.slotCount,
        slotMap: fd.slotMap.clone(),
        fastPath: fd.fastPath.clone(),
        tailLoop: fd.tailLoop,
    }
}

/// Resolve calls in a list of statements.
#[inline(always)]
pub fn resolveCallsInStmts(
    mut stmts: aver_rt::AverList<Stmt>,
    mut fnMap: aver_rt::AverMap<AverStr, i64>,
    mut acc: aver_rt::AverList<Stmt>,
) -> aver_rt::AverList<Stmt> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(stmts, [] => acc.reverse(), [s, rest] => { {
            let __tmp1 = fnMap.clone();
            let __tmp2 = aver_rt::AverList::prepend(resolveCallsInStmt(&s, &fnMap), &acc);
            stmts = rest;
            fnMap = __tmp1;
            acc = __tmp2;
            continue;
        } });
    }
}

/// Resolve calls in a single statement.
pub fn resolveCallsInStmt(s: &Stmt, fnMap: &aver_rt::AverMap<AverStr, i64>) -> Stmt {
    crate::cancel_checkpoint();
    match s.clone() {
        Stmt::StmtBind(name, expr) => Stmt::StmtBind(name, resolveCallsInExpr(&expr, fnMap)),
        Stmt::StmtBindSlot(slot, expr) => {
            Stmt::StmtBindSlot(slot, resolveCallsInExpr(&expr, fnMap))
        }
        Stmt::StmtExpr(expr) => Stmt::StmtExpr(resolveCallsInExpr(&expr, fnMap)),
    }
}

/// Replace ExprCall with ExprCallDirect for known user functions.
pub fn resolveCallsInExpr(expr: &Expr, fnMap: &aver_rt::AverMap<AverStr, i64>) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprBoolBranch(cond, thenExpr, elseExpr) => {
            let cond = (*cond).clone();
            let thenExpr = (*thenExpr).clone();
            let elseExpr = (*elseExpr).clone();
            Expr::ExprBoolBranch(
                std::sync::Arc::new(resolveCallsInExpr(&cond, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&thenExpr, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&elseExpr, fnMap)),
            )
        }
        Expr::ExprCall(name, args) => resolveOneCall(name, &args, fnMap),
        Expr::ExprCallDirect(fnId, args) => Expr::ExprCallDirect(
            fnId,
            resolveCallsInExprs(args, fnMap.clone(), aver_rt::AverList::empty()),
        ),
        Expr::ExprCallBuiltin(name, args) => {
            match crate::aver_generated::domain::ast::builtinNameToId(name.clone()) {
                Some(id) => Expr::ExprCallBuiltinId(
                    id,
                    resolveCallsInExprs(args, fnMap.clone(), aver_rt::AverList::empty()),
                ),
                None => Expr::ExprCallBuiltin(
                    name,
                    resolveCallsInExprs(args, fnMap.clone(), aver_rt::AverList::empty()),
                ),
            }
        }
        Expr::ExprCallBuiltinId(id, args) => Expr::ExprCallBuiltinId(
            id,
            resolveCallsInExprs(args, fnMap.clone(), aver_rt::AverList::empty()),
        ),
        _ => resolveCallsInExprInternal(expr, fnMap),
    }
}

/// Continue direct-call linking for specialized internal and arithmetic forms.
pub fn resolveCallsInExprInternal(expr: &Expr, fnMap: &aver_rt::AverMap<AverStr, i64>) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprBinopSlotInt(_, _, _) => expr.clone(),
        Expr::ExprBinopSlots(_, _, _) => expr.clone(),
        Expr::ExprCmpSlotInt(_, _, _) => expr.clone(),
        Expr::ExprCmpSlots(_, _, _) => expr.clone(),
        Expr::ExprVectorGetOrInt(vecExpr, idxExpr, defaultValue) => {
            let vecExpr = (*vecExpr).clone();
            let idxExpr = (*idxExpr).clone();
            Expr::ExprVectorGetOrInt(
                std::sync::Arc::new(resolveCallsInExpr(&vecExpr, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&idxExpr, fnMap)),
                defaultValue,
            )
        }
        Expr::ExprIntModOrInt(a, b, defaultValue) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprIntModOrInt(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
                defaultValue,
            )
        }
        Expr::ExprAdd(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprAdd(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
            )
        }
        Expr::ExprSub(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprSub(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
            )
        }
        Expr::ExprMul(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprMul(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
            )
        }
        Expr::ExprDiv(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprDiv(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
            )
        }
        Expr::ExprNeg(inner) => {
            let inner = (*inner).clone();
            Expr::ExprNeg(std::sync::Arc::new(resolveCallsInExpr(&inner, fnMap)))
        }
        _ => resolveCallsInExprTail(expr, fnMap),
    }
}

/// Finish direct-call linking for comparisons, aggregates, and product forms.
pub fn resolveCallsInExprTail(expr: &Expr, fnMap: &aver_rt::AverMap<AverStr, i64>) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprEq(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
            )
        }
        Expr::ExprNeq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprNeq(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
            )
        }
        Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprLt(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
            )
        }
        Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprGt(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
            )
        }
        Expr::ExprLte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprLte(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
            )
        }
        Expr::ExprGte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprGte(
                std::sync::Arc::new(resolveCallsInExpr(&a, fnMap)),
                std::sync::Arc::new(resolveCallsInExpr(&b, fnMap)),
            )
        }
        Expr::ExprMatch(subj, arms) => {
            let subj = (*subj).clone();
            Expr::ExprMatch(
                std::sync::Arc::new(resolveCallsInExpr(&subj, fnMap)),
                resolveCallsInArms(arms, fnMap.clone(), aver_rt::AverList::empty()),
            )
        }
        Expr::ExprPropagate(inner) => {
            let inner = (*inner).clone();
            Expr::ExprPropagate(std::sync::Arc::new(resolveCallsInExpr(&inner, fnMap)))
        }
        Expr::ExprConcat(parts) => Expr::ExprConcat(resolveCallsInExprs(
            parts,
            fnMap.clone(),
            aver_rt::AverList::empty(),
        )),
        Expr::ExprTuple(exprs) => Expr::ExprTuple(resolveCallsInExprs(
            exprs,
            fnMap.clone(),
            aver_rt::AverList::empty(),
        )),
        Expr::ExprIndependentProduct(exprs, unwrap) => Expr::ExprIndependentProduct(
            resolveCallsInExprs(exprs, fnMap.clone(), aver_rt::AverList::empty()),
            unwrap,
        ),
        Expr::ExprList(exprs) => Expr::ExprList(resolveCallsInExprs(
            exprs,
            fnMap.clone(),
            aver_rt::AverList::empty(),
        )),
        Expr::ExprRecord(name, fields) => Expr::ExprRecord(
            name,
            resolveCallsInFields(fields, fnMap.clone(), aver_rt::AverList::empty()),
        ),
        Expr::ExprFieldAccess(obj, field) => {
            let obj = (*obj).clone();
            Expr::ExprFieldAccess(std::sync::Arc::new(resolveCallsInExpr(&obj, fnMap)), field)
        }
        _ => expr.clone(),
    }
}

/// User fn → ExprCallDirect, builtin → ExprCallBuiltin, record update → ExprCall.
#[inline(always)]
pub fn resolveOneCall(
    name: AverStr,
    args: &aver_rt::AverList<Expr>,
    fnMap: &aver_rt::AverMap<AverStr, i64>,
) -> Expr {
    crate::cancel_checkpoint();
    let resolvedArgs = resolveCallsInExprs(args.clone(), fnMap.clone(), aver_rt::AverList::empty());
    match fnMap.get(&name).cloned() {
        Some(fnId) => Expr::ExprCallDirect(fnId, resolvedArgs),
        None => {
            if name.ends_with(".update") {
                Expr::ExprCall(name, resolvedArgs)
            } else {
                if isBuiltinCallName(name.clone()) {
                    Expr::ExprCallBuiltin(name, resolvedArgs)
                } else {
                    Expr::ExprCall(name, resolvedArgs)
                }
            }
        }
    }
}

/// Return whether a call name is one of the exact builtin/service entrypoints.
#[inline(always)]
pub fn isBuiltinCallName(name: AverStr) -> bool {
    crate::cancel_checkpoint();
    isBuiltinCallNameFrom(
        name,
        aver_rt::AverList::from_vec(vec![
            AverStr::from("Args.get"),
            AverStr::from("Bool.and"),
            AverStr::from("Bool.not"),
            AverStr::from("Bool.or"),
            AverStr::from("Byte.fromHex"),
            AverStr::from("Byte.toHex"),
            AverStr::from("Char.fromCode"),
            AverStr::from("Char.toCode"),
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
            AverStr::from("HttpServer.listen"),
            AverStr::from("HttpServer.listenWith"),
            AverStr::from("Int.abs"),
            AverStr::from("Int.fromString"),
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
            AverStr::from("Option.toResult"),
            AverStr::from("Option.withDefault"),
            AverStr::from("Random.int"),
            AverStr::from("Result.Err"),
            AverStr::from("Result.Ok"),
            AverStr::from("Result.withDefault"),
            AverStr::from("SelfHostRuntime.httpServerListen"),
            AverStr::from("SelfHostRuntime.httpServerListenWith"),
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
            AverStr::from("Tcp.connect"),
            AverStr::from("Tcp.ping"),
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
    mut name: AverStr,
    mut knownNames: aver_rt::AverList<AverStr>,
) -> bool {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(knownNames, [] => false, [knownName, rest] => { if (name == knownName) { true } else { {
            knownNames = rest;
            continue;
        } } });
    }
}

/// Resolve calls in a list of expressions.
#[inline(always)]
pub fn resolveCallsInExprs(
    mut exprs: aver_rt::AverList<Expr>,
    mut fnMap: aver_rt::AverMap<AverStr, i64>,
    mut acc: aver_rt::AverList<Expr>,
) -> aver_rt::AverList<Expr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => acc.reverse(), [e, rest] => { {
            let __tmp1 = fnMap.clone();
            let __tmp2 = aver_rt::AverList::prepend(resolveCallsInExpr(&e, &fnMap), &acc);
            exprs = rest;
            fnMap = __tmp1;
            acc = __tmp2;
            continue;
        } });
    }
}

/// Resolve calls in match arm bodies.
#[inline(always)]
pub fn resolveCallsInArms(
    mut arms: aver_rt::AverList<MatchArm>,
    mut fnMap: aver_rt::AverMap<AverStr, i64>,
    mut acc: aver_rt::AverList<MatchArm>,
) -> aver_rt::AverList<MatchArm> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(arms, [] => acc.reverse(), [arm, rest] => { {
            let __tmp1 = fnMap.clone();
            let __tmp2 = aver_rt::AverList::prepend(MatchArm { pattern: arm.pattern.clone(), body: resolveCallsInExpr(&arm.body, &fnMap), bindingSlots: arm.bindingSlots.clone() }, &acc);
            arms = rest;
            fnMap = __tmp1;
            acc = __tmp2;
            continue;
        } });
    }
}

/// Resolve calls in record field expressions.
#[inline(always)]
pub fn resolveCallsInFields(
    mut fields: aver_rt::AverList<(AverStr, Expr)>,
    mut fnMap: aver_rt::AverMap<AverStr, i64>,
    mut acc: aver_rt::AverList<(AverStr, Expr)>,
) -> aver_rt::AverList<(AverStr, Expr)> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fields, [] => acc.reverse(), [pair, rest] => { match pair {
            (name, expr) => {
            let __tmp1 = fnMap.clone();
            let __tmp2 = aver_rt::AverList::prepend((name, resolveCallsInExpr(&expr, &fnMap)), &acc);
            fields = rest;
            fnMap = __tmp1;
            acc = __tmp2;
            continue;
        }
        } });
    }
}
