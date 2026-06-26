#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::*;

/// Build name→fnId map from resolved function list.
#[inline(always)]
pub fn buildFnMap(
    mut fns: aver_rt::AverList<FnDef>,
    mut acc: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut idx: aver_rt::AverInt,
) -> aver_rt::AverMap<AverStr, aver_rt::AverInt> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fns, [] => { return acc; }, [f, rest] => { {
            let __tco0 = rest;
            let __tco1 = acc.insert_owned(f.name.clone(), idx.clone());
            let __tco2 = idx.add(&aver_rt::AverInt::from_i64(1));
            fns = __tco0;
            acc = __tco1;
            idx = __tco2;
            continue;
        } })
    }
}

/// Transform ExprCall→ExprCallDirect for known functions in all fn bodies.
#[inline(always)]
pub fn resolveCallsInFns(
    mut fns: aver_rt::AverList<FnDef>,
    mut fnMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc: aver_rt::AverList<FnDef>,
) -> aver_rt::AverList<FnDef> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fns, [] => { return acc.reverse(); }, [f, rest] => { {
            let __tco0 = rest;
            let __tco1 = fnMap.clone();
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::calls::resolveCallsInFn(&f, &fnMap), &acc);
            fns = __tco0;
            fnMap = __tco1;
            acc = __tco2;
            continue;
        } })
    }
}

/// Transform calls in one function body.
pub fn resolveCallsInFn(fd: &FnDef, fnMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>) -> FnDef {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::ast::FnDef {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: crate::aver_generated::domain::resolver::calls::resolveCallsInStmts(
            fd.body.clone(),
            fnMap.clone(),
            aver_rt::AverList::empty(),
        ),
        slotCount: fd.slotCount.clone(),
        slotMap: fd.slotMap.clone(),
        fastPath: fd.fastPath.clone(),
        tailLoop: fd.tailLoop,
    }
}

/// Resolve calls in a list of statements.
#[inline(always)]
pub fn resolveCallsInStmts(
    mut stmts: aver_rt::AverList<Stmt>,
    mut fnMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc: aver_rt::AverList<Stmt>,
) -> aver_rt::AverList<Stmt> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return acc.reverse(); }, [s, rest] => { {
            let __tco0 = rest;
            let __tco1 = fnMap.clone();
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::calls::resolveCallsInStmt(&s, &fnMap), &acc);
            stmts = __tco0;
            fnMap = __tco1;
            acc = __tco2;
            continue;
        } })
    }
}

/// Resolve calls in a single statement.
pub fn resolveCallsInStmt(s: &Stmt, fnMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>) -> Stmt {
    crate::cancel_checkpoint();
    match s.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtBind(name, expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtBind(
                name,
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&expr, fnMap),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(
                slot,
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&expr, fnMap),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtExpr(
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&expr, fnMap),
            )
        }
    }
}

/// Replace ExprCall with ExprCallDirect for known user functions.
pub fn resolveCallsInExpr(
    expr: &Expr,
    fnMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprBoolBranch(cond, thenExpr, elseExpr) => {
            let cond = (*cond).clone();
            let thenExpr = (*thenExpr).clone();
            let elseExpr = (*elseExpr).clone();
            crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                        &cond, fnMap,
                    ),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                        &thenExpr, fnMap,
                    ),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                        &elseExpr, fnMap,
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCall(name, args) => {
            crate::aver_generated::domain::resolver::calls::resolveOneCall(name, &args, fnMap)
        }
        crate::aver_generated::domain::ast::Expr::ExprCallDirect(fnId, args) => {
            crate::aver_generated::domain::ast::Expr::ExprCallDirect(
                fnId,
                crate::aver_generated::domain::resolver::calls::resolveCallsInExprs(
                    args,
                    fnMap.clone(),
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, args) => {
            match crate::aver_generated::domain::ast::builtinNameToId(name.clone()) {
                Some(id) => crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                    id,
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExprs(
                        args,
                        fnMap.clone(),
                        aver_rt::AverList::empty(),
                    ),
                ),
                None => crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                    name,
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExprs(
                        args,
                        fnMap.clone(),
                        aver_rt::AverList::empty(),
                    ),
                ),
            }
        }
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(id, args) => {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                id,
                crate::aver_generated::domain::resolver::calls::resolveCallsInExprs(
                    args,
                    fnMap.clone(),
                    aver_rt::AverList::empty(),
                ),
            )
        }
        _ => {
            crate::aver_generated::domain::resolver::calls::resolveCallsInExprInternal(expr, fnMap)
        }
    }
}

/// Continue direct-call linking for specialized internal and arithmetic forms.
pub fn resolveCallsInExprInternal(
    expr: &Expr,
    fnMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
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
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                        &vecExpr, fnMap,
                    ),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                        &idxExpr, fnMap,
                    ),
                ),
                defaultValue,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(a, b, defaultValue) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
                defaultValue,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprAdd(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprAdd(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprSub(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprSub(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprMul(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprMul(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprDiv(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprDiv(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprNeg(inner) => {
            let inner = (*inner).clone();
            crate::aver_generated::domain::ast::Expr::ExprNeg(std::sync::Arc::new(
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&inner, fnMap),
            ))
        }
        _ => crate::aver_generated::domain::resolver::calls::resolveCallsInExprTail(expr, fnMap),
    }
}

/// Finish direct-call linking for comparisons, aggregates, and product forms.
pub fn resolveCallsInExprTail(
    expr: &Expr,
    fnMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprEq(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprNeq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprNeq(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprLt(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprGt(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprLte(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprGte(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&a, fnMap),
                ),
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&b, fnMap),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprMatch(subj, arms) => {
            let subj = (*subj).clone();
            crate::aver_generated::domain::ast::Expr::ExprMatch(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(
                        &subj, fnMap,
                    ),
                ),
                crate::aver_generated::domain::resolver::calls::resolveCallsInArms(
                    arms,
                    fnMap.clone(),
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprPropagate(inner) => {
            let inner = (*inner).clone();
            crate::aver_generated::domain::ast::Expr::ExprPropagate(std::sync::Arc::new(
                crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&inner, fnMap),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprConcat(parts) => {
            crate::aver_generated::domain::ast::Expr::ExprConcat(
                crate::aver_generated::domain::resolver::calls::resolveCallsInExprs(
                    parts,
                    fnMap.clone(),
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprTuple(exprs) => {
            crate::aver_generated::domain::ast::Expr::ExprTuple(
                crate::aver_generated::domain::resolver::calls::resolveCallsInExprs(
                    exprs,
                    fnMap.clone(),
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(exprs, unwrap) => {
            crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
                crate::aver_generated::domain::resolver::calls::resolveCallsInExprs(
                    exprs,
                    fnMap.clone(),
                    aver_rt::AverList::empty(),
                ),
                unwrap,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprList(exprs) => {
            crate::aver_generated::domain::ast::Expr::ExprList(
                crate::aver_generated::domain::resolver::calls::resolveCallsInExprs(
                    exprs,
                    fnMap.clone(),
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprRecord(name, fields) => {
            crate::aver_generated::domain::ast::Expr::ExprRecord(
                name,
                crate::aver_generated::domain::resolver::calls::resolveCallsInFields(
                    fields,
                    fnMap.clone(),
                    aver_rt::AverList::empty(),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprFieldAccess(obj, field) => {
            let obj = (*obj).clone();
            crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
                std::sync::Arc::new(
                    crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&obj, fnMap),
                ),
                field,
            )
        }
        _ => expr.clone(),
    }
}

/// User fn → ExprCallDirect, builtin → ExprCallBuiltin, record update → ExprCall.
#[inline(always)]
pub fn resolveOneCall(
    name: AverStr,
    args: &aver_rt::AverList<Expr>,
    fnMap: &aver_rt::AverMap<AverStr, aver_rt::AverInt>,
) -> Expr {
    crate::cancel_checkpoint();
    let resolvedArgs = crate::aver_generated::domain::resolver::calls::resolveCallsInExprs(
        args.clone(),
        fnMap.clone(),
        aver_rt::AverList::empty(),
    );
    match fnMap.get(&name).cloned() {
        Some(fnId) => crate::aver_generated::domain::ast::Expr::ExprCallDirect(fnId, resolvedArgs),
        None => {
            if name.ends_with(".update") {
                crate::aver_generated::domain::ast::Expr::ExprCall(name, resolvedArgs)
            } else {
                if crate::aver_generated::domain::resolver::calls::isBuiltinCallName(name.clone()) {
                    crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, resolvedArgs)
                } else {
                    crate::aver_generated::domain::ast::Expr::ExprCall(name, resolvedArgs)
                }
            }
        }
    }
}

/// Return whether a call name is one of the exact builtin/service entrypoints.
#[inline(always)]
pub fn isBuiltinCallName(name: AverStr) -> bool {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::resolver::calls::isBuiltinCallNameFrom(
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
            AverStr::from("Int.div"),
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
        aver_list_match!(knownNames, [] => { return false; }, [knownName, rest] => { if (name == knownName) { return true; } else { {
            let __tco1 = rest;
            knownNames = __tco1;
            continue;
        } } })
    }
}

/// Resolve calls in a list of expressions.
#[inline(always)]
pub fn resolveCallsInExprs(
    mut exprs: aver_rt::AverList<Expr>,
    mut fnMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc: aver_rt::AverList<Expr>,
) -> aver_rt::AverList<Expr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return acc.reverse(); }, [e, rest] => { {
            let __tco0 = rest;
            let __tco1 = fnMap.clone();
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&e, &fnMap), &acc);
            exprs = __tco0;
            fnMap = __tco1;
            acc = __tco2;
            continue;
        } })
    }
}

/// Resolve calls in match arm bodies.
#[inline(always)]
pub fn resolveCallsInArms(
    mut arms: aver_rt::AverList<MatchArm>,
    mut fnMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc: aver_rt::AverList<MatchArm>,
) -> aver_rt::AverList<MatchArm> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(arms, [] => { return acc.reverse(); }, [arm, rest] => { {
            let __tco0 = rest;
            let __tco1 = fnMap.clone();
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::ast::MatchArm { pattern: arm.pattern.clone(), body: crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&arm.body, &fnMap), bindingSlots: arm.bindingSlots.clone() }, &acc);
            arms = __tco0;
            fnMap = __tco1;
            acc = __tco2;
            continue;
        } })
    }
}

/// Resolve calls in record field expressions.
#[inline(always)]
pub fn resolveCallsInFields(
    mut fields: aver_rt::AverList<(AverStr, Expr)>,
    mut fnMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    mut acc: aver_rt::AverList<(AverStr, Expr)>,
) -> aver_rt::AverList<(AverStr, Expr)> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return acc.reverse(); }, [pair, rest] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco1 = fnMap.clone();
            let __tco2 = aver_rt::AverList::prepend((name, crate::aver_generated::domain::resolver::calls::resolveCallsInExpr(&expr, &fnMap)), &acc);
            fields = __tco0;
            fnMap = __tco1;
            acc = __tco2;
            continue;
        } } })
    }
}
