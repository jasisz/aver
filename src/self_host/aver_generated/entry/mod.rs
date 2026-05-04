#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::eval::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::lexer::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::parser::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::resolver::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    LoadModules(
        aver_rt::AverList<AverStr>,
        AverStr,
        aver_rt::AverList<FnDef>,
        aver_rt::AverMap<AverStr, bool>,
    ),
    LoadOneModule(
        AverStr,
        aver_rt::AverList<AverStr>,
        AverStr,
        aver_rt::AverList<FnDef>,
        aver_rt::AverMap<AverStr, bool>,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
) -> Result<(aver_rt::AverList<FnDef>, aver_rt::AverMap<AverStr, bool>), AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::LoadModules(mut deps, mut moduleRoot, mut acc, mut loaded) => {
                crate::cancel_checkpoint();
                aver_list_match!(deps, [] => return Ok((acc, loaded)), [dep, rest] => if loaded.contains_key(&dep) { __MutualTco1::LoadModules(rest, moduleRoot, acc, loaded) } else { __MutualTco1::LoadOneModule(dep, rest, moduleRoot, acc, loaded) })
            }
            __MutualTco1::LoadOneModule(mut dep, mut rest, mut moduleRoot, mut acc, mut loaded) => {
                crate::cancel_checkpoint();
                let path = findModulePath(dep.clone(), moduleRoot.clone(), 0i64);
                let source = {
                    let __effect_arg0 = path;
                    crate::cancel_checkpoint();
                    aver_replay::invoke_effect(
                        "Disk.readText",
                        vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
                        || (aver_rt::read_text(&__effect_arg0)).into_aver(),
                    )
                }?;
                let tokens = crate::aver_generated::domain::lexer::lex(source);
                let prog = crate::aver_generated::domain::parser::parse(&tokens)?;
                let moduleFns = resolveQualifiedModuleFns(&prog, dep.clone());
                let loaded2 = loaded.insert_owned(dep, true);
                let innerResult = loadModules(&prog.deps, moduleRoot.clone(), &acc, &loaded2)?;
                match innerResult {
                    (accWithInner, loaded3) => __MutualTco1::LoadModules(
                        rest,
                        moduleRoot,
                        aver_rt::AverList::concat(
                            &accWithInner.clone(),
                            &shiftFnIdsInFns(
                                moduleFns,
                                (accWithInner.len() as i64),
                                aver_rt::AverList::empty(),
                            ),
                        ),
                        loaded3,
                    ),
                }
            }
        };
    }
}

/// Load all module dependencies, skipping already-loaded modules.
pub fn loadModules(
    deps: &aver_rt::AverList<AverStr>,
    moduleRoot: AverStr,
    acc: &aver_rt::AverList<FnDef>,
    loaded: &aver_rt::AverMap<AverStr, bool>,
) -> Result<(aver_rt::AverList<FnDef>, aver_rt::AverMap<AverStr, bool>), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::LoadModules(
        deps.clone(),
        moduleRoot,
        acc.clone(),
        loaded.clone(),
    ))
}

/// Load one module. Tries moduleRoot, then parent dirs up to 3 levels.
pub fn loadOneModule(
    dep: AverStr,
    rest: &aver_rt::AverList<AverStr>,
    moduleRoot: AverStr,
    acc: &aver_rt::AverList<FnDef>,
    loaded: &aver_rt::AverMap<AverStr, bool>,
) -> Result<(aver_rt::AverList<FnDef>, aver_rt::AverMap<AverStr, bool>), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::LoadOneModule(
        dep,
        rest.clone(),
        moduleRoot,
        acc.clone(),
        loaded.clone(),
    ))
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    QualifyFns(aver_rt::AverList<FnDef>, AverStr, aver_rt::AverList<FnDef>),
    QualifyFnsOne(
        FnDef,
        aver_rt::AverList<FnDef>,
        AverStr,
        aver_rt::AverList<FnDef>,
    ),
}

fn __mutual_tco_trampoline_2(mut __state: __MutualTco2) -> aver_rt::AverList<FnDef> {
    loop {
        __state = match __state {
            __MutualTco2::QualifyFns(mut fns, mut prefix, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(fns, [] => return acc.reverse(), [f, rest] => __MutualTco2::QualifyFnsOne(f, rest, prefix, acc))
            }
            __MutualTco2::QualifyFnsOne(mut f, mut rest, mut prefix, mut acc) => {
                crate::cancel_checkpoint();
                let qualified = FnDef {
                    name: ((prefix.clone() + &AverStr::from(".")) + &f.name),
                    params: f.params.clone(),
                    body: f.body.clone(),
                    slotCount: f.slotCount,
                    slotMap: f.slotMap.clone(),
                    fastPath: f.fastPath.clone(),
                    tailLoop: f.tailLoop,
                };
                __MutualTco2::QualifyFns(
                    rest,
                    prefix,
                    aver_rt::AverList::prepend(f, &aver_rt::AverList::prepend(qualified, &acc)),
                )
            }
        };
    }
}

/// Register functions with both qualified (Domain.Foo.bar) and unqualified (bar) names.
pub fn qualifyFns(
    fns: &aver_rt::AverList<FnDef>,
    prefix: AverStr,
    acc: &aver_rt::AverList<FnDef>,
) -> aver_rt::AverList<FnDef> {
    __mutual_tco_trampoline_2(__MutualTco2::QualifyFns(fns.clone(), prefix, acc.clone()))
}

/// Add both qualified and unqualified versions.
pub fn qualifyFnsOne(
    f: &FnDef,
    rest: &aver_rt::AverList<FnDef>,
    prefix: AverStr,
    acc: &aver_rt::AverList<FnDef>,
) -> aver_rt::AverList<FnDef> {
    __mutual_tco_trampoline_2(__MutualTco2::QualifyFnsOne(
        f.clone(),
        rest.clone(),
        prefix,
        acc.clone(),
    ))
}

/// Lex, parse, resolve, and evaluate an Aver source string (no module loading).
pub fn run(source: AverStr) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let tokens = crate::aver_generated::domain::lexer::lex(source.clone());
    let prog = crate::aver_generated::domain::parser::parse(&tokens)?;
    let resolved = crate::aver_generated::domain::resolver::resolveProgram(&prog);
    runGuestProgram(
        &resolved,
        &aver_rt::AverList::empty(),
        &aver_rt::AverList::empty(),
    )
}

/// Lex, parse, resolve, load modules, and evaluate.
pub fn runWithModules(source: AverStr, moduleRoot: AverStr) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let prepared = prepareProgramWithModules(source, moduleRoot)?;
    {
        let (prog, moduleFns) = prepared;
        runGuestProgram(&prog, &moduleFns, &aver_rt::AverList::empty())
    }
}

/// Lex, parse, resolve, load modules, and return a guest program ready for execution.
pub fn prepareProgramWithModules(
    source: AverStr,
    moduleRoot: AverStr,
) -> Result<(Program, aver_rt::AverList<FnDef>), AverStr> {
    crate::cancel_checkpoint();
    let tokens = crate::aver_generated::domain::lexer::lex(source.clone());
    let prog = crate::aver_generated::domain::parser::parse(&tokens)?;
    let resolved = crate::aver_generated::domain::resolver::resolveProgram(&prog);
    let r = loadModules(
        &resolved.deps,
        moduleRoot,
        &aver_rt::AverList::empty(),
        &HashMap::new(),
    )?;
    {
        let (moduleFns, _) = r;
        Ok((resolved, moduleFns))
    }
}

/// Execute an already-loaded guest program. guestArgs marks the guest input boundary for scoped replay and policy in generated Rust.
pub fn runGuestProgram(
    prog: &Program,
    moduleFns: &aver_rt::AverList<FnDef>,
    guestArgs: &aver_rt::AverList<AverStr>,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::evalProgramWithFns(
        &shiftFnIdsInProgram(prog, (moduleFns.len() as i64)),
        moduleFns,
    )
}

/// Execute a loaded guest program with CLI-compatible main semantics inside the guest boundary.
pub fn runGuestCliProgram(
    prog: &Program,
    moduleFns: &aver_rt::AverList<FnDef>,
    localFns: &aver_rt::AverList<FnDef>,
    guestArgs: &aver_rt::AverList<AverStr>,
) -> Result<(), AverStr> {
    let __replay_input = aver_replay::ReplayValue::to_replay_json(guestArgs);
    aver_replay::with_guest_scope_args_result(
        "runGuestCliProgram",
        __replay_input,
        guestArgs.clone(),
        || {
            crate::self_host_support::with_program_fn_store(
                prog.fns.clone(),
                moduleFns.clone(),
                || {
                    crate::cancel_checkpoint();
                    match runGuestProgram(prog, moduleFns, guestArgs) {
                        Ok(result) => finishCliRun(localFns, &result),
                        Err(e) => Err((AverStr::from("Runtime error: ") + &e)),
                    }
                },
            )
        },
    )
}

/// Find module file, trying root then parent dirs.
pub fn findModulePath(mut dep: AverStr, mut root: AverStr, mut depth: i64) -> AverStr {
    loop {
        crate::cancel_checkpoint();
        let path = modulePathFromName(dep.clone(), root.clone());
        return if {
            let __effect_arg0 = path.clone();
            crate::cancel_checkpoint();
            aver_replay::invoke_effect(
                "Disk.exists",
                vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
                || aver_rt::path_exists(&__effect_arg0),
            )
        } {
            path
        } else {
            if (depth >= 3i64) {
                path
            } else {
                {
                    let __tmp1 = (root + &AverStr::from("/.."));
                    let __tmp2 = (depth + 1i64);
                    root = __tmp1;
                    depth = __tmp2;
                    continue;
                }
            }
        };
    }
}

/// Convert Module.Name to file path: Domain.Foo -> domain/foo.av
pub fn modulePathFromName(name: AverStr, moduleRoot: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    (((moduleRoot + &AverStr::from("/"))
        + &dotToSlash(
            name.clone(),
            0i64,
            (name.chars().count() as i64),
            AverStr::from(""),
        ))
        + &AverStr::from(".av"))
}

/// Replace dots with slashes and lowercase first char of each segment.
#[inline(always)]
pub fn dotToSlash(mut name: AverStr, mut pos: i64, mut total: i64, mut acc: AverStr) -> AverStr {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return if (pos >= total) {
            acc
        } else {
            match (name.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                Some(c) => {
                    if (&*c == ".") {
                        {
                            let __tmp3 = (acc + &AverStr::from("/"));
                            pos = nextPos;
                            acc = __tmp3;
                            continue;
                        }
                    } else {
                        {
                            let __tmp3 = (acc + &(c.to_lowercase()).into_aver());
                            pos = nextPos;
                            acc = __tmp3;
                            continue;
                        }
                    }
                }
                None => acc,
            }
        };
    }
}

/// Duplicate module function names first, then resolve locally so direct-call ids are consistent within that module.
pub fn resolveQualifiedModuleFns(prog: &Program, dep: AverStr) -> aver_rt::AverList<FnDef> {
    crate::cancel_checkpoint();
    let qualifiedProg = Program {
        deps: prog.deps.clone(),
        fns: qualifyFns(&prog.fns, dep, &aver_rt::AverList::empty()),
        stmts: prog.stmts.clone(),
    };
    crate::aver_generated::domain::resolver::resolveProgram(&qualifiedProg).fns
}

/// Shift module-local direct-call ids so they point into the final combined function store.
#[inline(always)]
pub fn shiftFnIdsInFns(
    mut fns: aver_rt::AverList<FnDef>,
    mut offset: i64,
    mut acc: aver_rt::AverList<FnDef>,
) -> aver_rt::AverList<FnDef> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fns, [] => acc.reverse(), [fd, rest] => { {
            let __tmp2 = aver_rt::AverList::prepend(shiftFnIdsInFn(&fd, offset), &acc);
            fns = rest;
            acc = __tmp2;
            continue;
        } });
    }
}

/// Shift entry-program direct-call ids before prepending loaded module functions.
pub fn shiftFnIdsInProgram(prog: &Program, offset: i64) -> Program {
    crate::cancel_checkpoint();
    Program {
        deps: prog.deps.clone(),
        fns: shiftFnIdsInFns(prog.fns.clone(), offset, aver_rt::AverList::empty()),
        stmts: shiftFnIdsInStmts(prog.stmts.clone(), offset, aver_rt::AverList::empty()),
    }
}

/// Shift all embedded direct-call ids in one function definition.
pub fn shiftFnIdsInFn(fd: &FnDef, offset: i64) -> FnDef {
    crate::cancel_checkpoint();
    FnDef {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: shiftFnIdsInStmts(fd.body.clone(), offset, aver_rt::AverList::empty()),
        slotCount: fd.slotCount,
        slotMap: fd.slotMap.clone(),
        fastPath: shiftFnIdsInFastPath(&fd.fastPath, offset),
        tailLoop: fd.tailLoop,
    }
}

/// Shift fast-path target ids that were resolved against a module-local function list.
pub fn shiftFnIdsInFastPath(path: &FnFastPath, offset: i64) -> FnFastPath {
    crate::cancel_checkpoint();
    match path.clone() {
        FnFastPath::FastForwardCall(targetId, slotArgs) => {
            FnFastPath::FastForwardCall((targetId + offset), slotArgs)
        }
        _ => path.clone(),
    }
}

/// Shift direct-call ids through a statement list.
#[inline(always)]
pub fn shiftFnIdsInStmts(
    mut stmts: aver_rt::AverList<Stmt>,
    mut offset: i64,
    mut acc: aver_rt::AverList<Stmt>,
) -> aver_rt::AverList<Stmt> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(stmts, [] => acc.reverse(), [stmt, rest] => { {
            let __tmp2 = aver_rt::AverList::prepend(shiftFnIdsInStmt(&stmt, offset), &acc);
            stmts = rest;
            acc = __tmp2;
            continue;
        } });
    }
}

/// Shift direct-call ids in one statement.
pub fn shiftFnIdsInStmt(stmt: &Stmt, offset: i64) -> Stmt {
    crate::cancel_checkpoint();
    match stmt.clone() {
        Stmt::StmtBind(name, expr) => Stmt::StmtBind(name, shiftFnIdsInExpr(&expr, offset)),
        Stmt::StmtBindSlot(slot, expr) => Stmt::StmtBindSlot(slot, shiftFnIdsInExpr(&expr, offset)),
        Stmt::StmtExpr(expr) => Stmt::StmtExpr(shiftFnIdsInExpr(&expr, offset)),
    }
}

/// Shift direct-call ids in one expression tree.
pub fn shiftFnIdsInExpr(expr: &Expr, offset: i64) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprBoolBranch(cond, thenExpr, elseExpr) => {
            let cond = (*cond).clone();
            let thenExpr = (*thenExpr).clone();
            let elseExpr = (*elseExpr).clone();
            Expr::ExprBoolBranch(
                std::sync::Arc::new(shiftFnIdsInExpr(&cond, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&thenExpr, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&elseExpr, offset)),
            )
        }
        Expr::ExprVectorGetOrInt(vecExpr, idxExpr, defaultValue) => {
            let vecExpr = (*vecExpr).clone();
            let idxExpr = (*idxExpr).clone();
            Expr::ExprVectorGetOrInt(
                std::sync::Arc::new(shiftFnIdsInExpr(&vecExpr, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&idxExpr, offset)),
                defaultValue,
            )
        }
        Expr::ExprIntModOrInt(a, b, defaultValue) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprIntModOrInt(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
                defaultValue,
            )
        }
        Expr::ExprAdd(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprAdd(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        Expr::ExprSub(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprSub(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        Expr::ExprMul(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprMul(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        Expr::ExprDiv(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprDiv(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprEq(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        Expr::ExprNeq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprNeq(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        _ => shiftFnIdsInExprTail(expr, offset),
    }
}

/// Finish shifting direct-call ids through the remaining aggregate forms.
pub fn shiftFnIdsInExprTail(expr: &Expr, offset: i64) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprLt(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprGt(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        Expr::ExprLte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprLte(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        Expr::ExprGte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            Expr::ExprGte(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset)),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        Expr::ExprConcat(parts) => {
            Expr::ExprConcat(shiftFnIdsInExprs(parts, offset, aver_rt::AverList::empty()))
        }
        Expr::ExprTuple(exprs) => {
            Expr::ExprTuple(shiftFnIdsInExprs(exprs, offset, aver_rt::AverList::empty()))
        }
        Expr::ExprList(exprs) => {
            Expr::ExprList(shiftFnIdsInExprs(exprs, offset, aver_rt::AverList::empty()))
        }
        Expr::ExprRecord(name, fields) => Expr::ExprRecord(
            name,
            shiftFnIdsInFields(fields, offset, aver_rt::AverList::empty()),
        ),
        _ => shiftFnIdsInExprCalls(expr, offset),
    }
}

/// Finish shifting direct-call ids through access, call, and match forms.
pub fn shiftFnIdsInExprCalls(expr: &Expr, offset: i64) -> Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        Expr::ExprFieldAccess(obj, field) => {
            let obj = (*obj).clone();
            Expr::ExprFieldAccess(std::sync::Arc::new(shiftFnIdsInExpr(&obj, offset)), field)
        }
        Expr::ExprCall(name, args) => Expr::ExprCall(
            name,
            shiftFnIdsInExprs(args, offset, aver_rt::AverList::empty()),
        ),
        Expr::ExprCallDirect(fnId, args) => Expr::ExprCallDirect(
            (fnId + offset),
            shiftFnIdsInExprs(args, offset, aver_rt::AverList::empty()),
        ),
        Expr::ExprCallBuiltin(name, args) => Expr::ExprCallBuiltin(
            name,
            shiftFnIdsInExprs(args, offset, aver_rt::AverList::empty()),
        ),
        Expr::ExprCallBuiltinId(id, args) => Expr::ExprCallBuiltinId(
            id,
            shiftFnIdsInExprs(args, offset, aver_rt::AverList::empty()),
        ),
        Expr::ExprMatch(scrutinee, arms) => {
            let scrutinee = (*scrutinee).clone();
            Expr::ExprMatch(
                std::sync::Arc::new(shiftFnIdsInExpr(&scrutinee, offset)),
                shiftFnIdsInArms(arms, offset, aver_rt::AverList::empty()),
            )
        }
        Expr::ExprPropagate(inner) => {
            let inner = (*inner).clone();
            Expr::ExprPropagate(std::sync::Arc::new(shiftFnIdsInExpr(&inner, offset)))
        }
        Expr::ExprIndependentProduct(exprs, unwrap) => Expr::ExprIndependentProduct(
            shiftFnIdsInExprs(exprs, offset, aver_rt::AverList::empty()),
            unwrap,
        ),
        _ => expr.clone(),
    }
}

/// Shift direct-call ids in a list of expressions.
#[inline(always)]
pub fn shiftFnIdsInExprs(
    mut exprs: aver_rt::AverList<Expr>,
    mut offset: i64,
    mut acc: aver_rt::AverList<Expr>,
) -> aver_rt::AverList<Expr> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(exprs, [] => acc.reverse(), [expr, rest] => { {
            let __tmp2 = aver_rt::AverList::prepend(shiftFnIdsInExpr(&expr, offset), &acc);
            exprs = rest;
            acc = __tmp2;
            continue;
        } });
    }
}

/// Shift direct-call ids in record field expressions.
#[inline(always)]
pub fn shiftFnIdsInFields(
    mut fields: aver_rt::AverList<(AverStr, Expr)>,
    mut offset: i64,
    mut acc: aver_rt::AverList<(AverStr, Expr)>,
) -> aver_rt::AverList<(AverStr, Expr)> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fields, [] => acc.reverse(), [pair, rest] => { match pair {
            (name, expr) => {
            let __tmp2 = aver_rt::AverList::prepend((name, shiftFnIdsInExpr(&expr, offset)), &acc);
            fields = rest;
            acc = __tmp2;
            continue;
        }
        } });
    }
}

/// Shift direct-call ids in match arm bodies.
#[inline(always)]
pub fn shiftFnIdsInArms(
    mut arms: aver_rt::AverList<MatchArm>,
    mut offset: i64,
    mut acc: aver_rt::AverList<MatchArm>,
) -> aver_rt::AverList<MatchArm> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(arms, [] => acc.reverse(), [arm, rest] => { {
            let __tmp2 = aver_rt::AverList::prepend(MatchArm { pattern: arm.pattern.clone(), body: shiftFnIdsInExpr(&arm.body, offset), bindingSlots: arm.bindingSlots.clone() }, &acc);
            arms = rest;
            acc = __tmp2;
            continue;
        } });
    }
}

/// Run source and return result representation or error.
pub fn runRepr(source: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    match run(source) {
        Ok(val) => crate::aver_generated::domain::value::valRepr(&val),
        Err(e) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity((23i64) as usize);
                __b.push_str(&AverStr::from("ERROR: "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(e))));
            __b
        }),
    }
}

/// Read a file and run it through the self-hosted pipeline with module loading.
pub fn runFile(path: AverStr, moduleRoot: AverStr) -> Result<AverStr, AverStr> {
    crate::cancel_checkpoint();
    match loadProgramFromFile(path, moduleRoot) {
        Ok(pair) => {
            let (prog, moduleFns) = pair;
            runFileLoaded(&prog, &moduleFns)
        }
        Err(e) => Err(e),
    }
}

/// Turn a loaded guest program into a representation string.
pub fn runFileLoaded(
    prog: &Program,
    moduleFns: &aver_rt::AverList<FnDef>,
) -> Result<AverStr, AverStr> {
    crate::cancel_checkpoint();
    match runGuestProgram(prog, moduleFns, &aver_rt::AverList::empty()) {
        Ok(val) => Ok(crate::aver_generated::domain::value::valRepr(&val)),
        Err(e) => Err(e),
    }
}

/// Read a file and prepare the guest program plus loaded modules.
pub fn loadProgramFromFile(
    path: AverStr,
    moduleRoot: AverStr,
) -> Result<(Program, aver_rt::AverList<FnDef>), AverStr> {
    crate::cancel_checkpoint();
    let source = {
        let __effect_arg0 = path;
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Disk.readText",
            vec![aver_replay::ReplayValue::to_replay_json(&__effect_arg0)],
            || (aver_rt::read_text(&__effect_arg0)).into_aver(),
        )
    }?;
    prepareProgramWithModules(source, moduleRoot.clone())
}

/// Dispatch normalized CLI args: path, module root, then guest args.
pub fn runFromFileWithRest(
    path: AverStr,
    rest: &aver_rt::AverList<AverStr>,
) -> Result<(), AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(rest.clone(), [] => runCliFile(path, AverStr::from("."), &aver_rt::AverList::empty()), [moduleRoot, guestArgs] => runCliFile(path, moduleRoot, &guestArgs))
}

/// Load a guest file outside scope, then execute the guest program inside the guest boundary.
pub fn runCliFile(
    path: AverStr,
    moduleRoot: AverStr,
    guestArgs: &aver_rt::AverList<AverStr>,
) -> Result<(), AverStr> {
    crate::cancel_checkpoint();
    let prepared = loadProgramFromFile(path, moduleRoot)?;
    {
        let (prog, moduleFns) = prepared;
        runGuestCliProgram(&prog, &moduleFns, &prog.fns, guestArgs)
    }
}

/// Mirror host CLI semantics: ignore successful return values, but surface main() Result.Err as process failure.
#[inline(always)]
pub fn finishCliRun(localFns: &aver_rt::AverList<FnDef>, result: &Val) -> Result<(), AverStr> {
    crate::cancel_checkpoint();
    if hasLocalMain(localFns.clone()) {
        finishCliMainResult(result)
    } else {
        Ok(())
    }
}

/// Convert a guest main() return value into CLI success or failure.
pub fn finishCliMainResult(result: &Val) -> Result<(), AverStr> {
    crate::cancel_checkpoint();
    match result.clone() {
        Val::ValErr(err) => {
            let err = (*err).clone();
            Err((AverStr::from("Main returned error: ")
                + &crate::aver_generated::domain::value::valRepr(&err)))
        }
        _ => Ok(()),
    }
}

/// Check whether the entry program defines its own main().
#[inline(always)]
pub fn hasLocalMain(mut fns: aver_rt::AverList<FnDef>) -> bool {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(fns, [] => false, [f, rest] => { if (&*f.name == "main") { true } else { {
            fns = rest;
            continue;
        } } });
    }
}

/// Print unless the result is Unit.
pub fn printIfNotUnit(s: AverStr) -> () {
    crate::cancel_checkpoint();
    if (s == AverStr::from("()")) {
        {
            let __effect_arg0 = AverStr::from("");
            crate::cancel_checkpoint();
            aver_replay::invoke_effect(
                "Console.print",
                vec![serde_json::Value::String(format!("{}", __effect_arg0))],
                || aver_rt::console_print(&__effect_arg0),
            )
        }
    } else {
        {
            let __effect_arg0 = s;
            crate::cancel_checkpoint();
            aver_replay::invoke_effect(
                "Console.print",
                vec![serde_json::Value::String(format!("{}", __effect_arg0))],
                || aver_rt::console_print(&__effect_arg0),
            )
        }
    }
}

/// Show built-in demo programs.
pub fn runDemo() -> Result<(), AverStr> {
    crate::cancel_checkpoint();
    let demoArithmetic = runRepr(AverStr::from("x = 3 + 4\nx * 2"));
    let demoDouble = runRepr(AverStr::from("fn double(n)\n    n + n\n\ndouble(21)"));
    let demoFib = runRepr(AverStr::from(
        "fn fib(n)\n    match n\n        0 -> 0\n        1 -> 1\n        _ -> fib(n - 1) + fib(n - 2)\n\nfib(10)",
    ));
    {
        let __effect_arg0 = AverStr::from("=== Self-Hosted Mini Aver ===");
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.print",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_print(&__effect_arg0),
        )
    };
    {
        let __effect_arg0 = AverStr::from("");
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.print",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_print(&__effect_arg0),
        )
    };
    {
        let __effect_arg0 = AverStr::from(
            "Usage: aver run self_hosted/main.av --module-root self_hosted -- <file.av> <module-root> [guest args...]",
        );
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.print",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_print(&__effect_arg0),
        )
    };
    {
        let __effect_arg0 = AverStr::from("");
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.print",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_print(&__effect_arg0),
        )
    };
    {
        let __effect_arg0 = AverStr::from("Built-in demos:");
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.print",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_print(&__effect_arg0),
        )
    };
    {
        let __effect_arg0 = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity((31i64) as usize);
                __b.push_str(&AverStr::from("  x=3+4; x*2 = "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(demoArithmetic),
            )));
            __b
        });
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.print",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_print(&__effect_arg0),
        )
    };
    {
        let __effect_arg0 = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity((31i64) as usize);
                __b.push_str(&AverStr::from("  double(21) = "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(demoDouble),
            )));
            __b
        });
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.print",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_print(&__effect_arg0),
        )
    };
    {
        let __effect_arg0 = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity((28i64) as usize);
                __b.push_str(&AverStr::from("  fib(10) = "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(demoFib))));
            __b
        });
        crate::cancel_checkpoint();
        aver_replay::invoke_effect(
            "Console.print",
            vec![serde_json::Value::String(format!("{}", __effect_arg0))],
            || aver_rt::console_print(&__effect_arg0),
        )
    };
    Ok(())
}

pub fn main() -> Result<(), AverStr> {
    let args = {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Args.get", vec![], || aver_replay::current_cli_args())
    };
    aver_list_match!(args, [] => runDemo(), [path, rest] => runFromFileWithRest(path, &rest))
}
