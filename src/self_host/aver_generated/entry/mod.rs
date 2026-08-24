#[allow(unused_imports)]
use crate::aver_generated::disk::*;
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
    QualifyFns__indexed(
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
        AverStr,
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    ),
    QualifyFnsOne__indexed(
        crate::aver_generated::domain::ast::FnDef,
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
        AverStr,
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    loop {
        __state = match __state {
            __MutualTco1::QualifyFns__indexed(mut fns, mut prefix, mut acc) => {
                crate::cancel_checkpoint();
                aver_list_match!(fns, [] => { return acc.reverse() }, [f, rest] => __MutualTco1::QualifyFnsOne__indexed(f, rest, prefix, acc))
            }
            __MutualTco1::QualifyFnsOne__indexed(mut f, mut rest, mut prefix, mut acc) => {
                crate::cancel_checkpoint();
                let qualified = crate::aver_generated::domain::ast::FnDef {
                    name: ((prefix.clone() + &AverStr::from(".")) + &f.name),
                    params: f.params.clone(),
                    body: f.body.clone(),
                    slotCount: f.slotCount.clone(),
                    slotMap: f.slotMap.clone(),
                    fastPath: f.fastPath.clone(),
                    tailLoop: f.tailLoop,
                };
                __MutualTco1::QualifyFns__indexed(
                    rest,
                    prefix,
                    aver_rt::AverList::prepend(f, &aver_rt::AverList::prepend(qualified, &acc)),
                )
            }
        };
    }
}

/// Synthesized indexed worker of `qualifyFns`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn qualifyFns__indexed(
    fns: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    prefix: AverStr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    __mutual_tco_trampoline_1(
        __MutualTco1::QualifyFns__indexed(fns.clone(), prefix, acc.clone()),
        &__str_index,
    )
}

/// Synthesized indexed worker of `qualifyFnsOne`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn qualifyFnsOne__indexed(
    f: &crate::aver_generated::domain::ast::FnDef,
    rest: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    prefix: AverStr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    __mutual_tco_trampoline_1(
        __MutualTco1::QualifyFnsOne__indexed(f.clone(), rest.clone(), prefix, acc.clone()),
        &__str_index,
    )
}

/// Lex, parse, resolve, and evaluate an Aver source string (no module loading).
pub fn run(source: AverStr) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let tokens = crate::aver_generated::domain::lexer::lex(source);
    let prog = crate::aver_generated::domain::parser::parse(&tokens)?;
    let resolved = crate::aver_generated::domain::resolver::resolveProgram(&prog);
    runGuestProgram(
        &resolved,
        &aver_rt::AverList::empty(),
        &aver_rt::AverList::empty(),
    )
}

/// Lex, parse, resolve, load modules, and evaluate.
pub fn runWithModules(
    source: AverStr,
    moduleRoot: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
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
) -> Result<
    (
        crate::aver_generated::domain::ast::Program,
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    let tokens = crate::aver_generated::domain::lexer::lex(source);
    let prog = crate::aver_generated::domain::parser::parse(&tokens)?;
    let resolved = crate::aver_generated::domain::resolver::resolveProgram(&prog);
    let r = loadModules(
        resolved.deps.clone(),
        moduleRoot,
        aver_rt::AverList::empty(),
        HashMap::new(),
    )?;
    {
        let (moduleFns, _) = r;
        Ok((resolved, moduleFns))
    }
}

/// Execute an already-loaded guest program. guestArgs marks the guest input boundary for scoped replay and policy in generated Rust.
pub fn runGuestProgram(
    prog: &crate::aver_generated::domain::ast::Program,
    moduleFns: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    guestArgs: &aver_rt::AverList<AverStr>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::eval::evalProgramWithFns(
        &shiftFnIdsInProgram(prog, aver_rt::AverInt::from_i64(moduleFns.len() as i64)),
        moduleFns,
    )
}

/// Execute a loaded guest program with CLI-compatible main semantics inside the guest boundary. Returns the user main()'s return Val so the replay scope can serialise it as recording.output (and replay-mode output comparison sees the live value), instead of dropping it to Unit before the wrapping aver_replay scope captures the result.
pub fn runGuestCliProgram(
    prog: &crate::aver_generated::domain::ast::Program,
    moduleFns: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    localFns: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    guestArgs: &aver_rt::AverList<AverStr>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
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
                        Ok(result @ _) => finishCliRun(localFns, &result),
                        Err(e @ _) => Err((AverStr::from("Runtime error: ") + &e)),
                    }
                },
            )
        },
    )
}

/// Load all module dependencies, skipping already-loaded modules.
pub fn loadModules(
    mut deps: aver_rt::AverList<AverStr>,
    mut moduleRoot: AverStr,
    acc: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    loaded: aver_rt::AverMap<AverStr, bool>,
) -> Result<
    (
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
        aver_rt::AverMap<AverStr, bool>,
    ),
    AverStr,
> {
    let loaded = std::sync::Arc::new(loaded);
    let acc = std::sync::Arc::new(acc);
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(deps, [] => { return Ok(((*acc).clone(), (*loaded).clone())); }, [dep, rest] => { if loaded.contains_key(&dep) { {
            let __tco0 = rest;
            deps = __tco0;
            continue;
        } } else { return loadOneModule(dep, &rest, moduleRoot, &*acc, &*loaded); } })
    }
}

/// Load one module. Tries moduleRoot, then parent dirs up to 3 levels.
pub fn loadOneModule(
    dep: AverStr,
    rest: &aver_rt::AverList<AverStr>,
    moduleRoot: AverStr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    loaded: &aver_rt::AverMap<AverStr, bool>,
) -> Result<
    (
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
        aver_rt::AverMap<AverStr, bool>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    loadOneModule__indexed(
        dep.clone(),
        rest,
        moduleRoot,
        acc,
        loaded,
        &aver_rt::string_index_build(&dep),
    )
}

/// Find module file, trying root then parent dirs.
pub fn findModulePath(dep: AverStr, root: AverStr, depth: aver_rt::AverInt) -> AverStr {
    crate::cancel_checkpoint();
    findModulePath__indexed(dep.clone(), root, depth, aver_rt::string_index_build(&dep))
}

/// Convert Module.Name to file path: Domain.Foo -> domain/foo.av
#[inline(always)]
pub fn modulePathFromName(name: AverStr, moduleRoot: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    modulePathFromName__indexed(
        name.clone(),
        moduleRoot,
        &aver_rt::string_index_build(&name),
    )
}

/// Replace dots with slashes and lowercase first char of each segment.
#[inline(always)]
pub fn dotToSlash(
    name: AverStr,
    pos: aver_rt::AverInt,
    total: aver_rt::AverInt,
    acc: AverStr,
) -> AverStr {
    crate::cancel_checkpoint();
    dotToSlash__indexed(
        name.clone(),
        pos,
        total,
        acc,
        aver_rt::string_index_build(&name),
    )
}

/// Register functions with both qualified (Domain.Foo.bar) and unqualified (bar) names.
#[inline(always)]
pub fn qualifyFns(
    fns: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    prefix: AverStr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    crate::cancel_checkpoint();
    qualifyFns__indexed(
        fns,
        prefix.clone(),
        acc,
        &aver_rt::string_index_build(&prefix),
    )
}

/// Add both qualified and unqualified versions.
#[inline(always)]
pub fn qualifyFnsOne(
    f: &crate::aver_generated::domain::ast::FnDef,
    rest: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    prefix: AverStr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    crate::cancel_checkpoint();
    qualifyFnsOne__indexed(
        f,
        rest,
        prefix.clone(),
        acc,
        &aver_rt::string_index_build(&prefix),
    )
}

/// Duplicate module function names first, then resolve locally so direct-call ids are consistent within that module.
#[inline(always)]
pub fn resolveQualifiedModuleFns(
    prog: &crate::aver_generated::domain::ast::Program,
    dep: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    crate::cancel_checkpoint();
    resolveQualifiedModuleFns__indexed(prog, dep.clone(), &aver_rt::string_index_build(&dep))
}

/// Shift module-local direct-call ids so they point into the final combined function store.
#[inline(always)]
pub fn shiftFnIdsInFns(
    mut fns: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut offset: aver_rt::AverInt,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fns, [] => { return acc.reverse(); }, [fd, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(shiftFnIdsInFn(&fd, offset.clone()), &acc);
            fns = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Shift entry-program direct-call ids before prepending loaded module functions.
pub fn shiftFnIdsInProgram(
    prog: &crate::aver_generated::domain::ast::Program,
    offset: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Program {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::ast::Program {
        deps: prog.deps.clone(),
        fns: shiftFnIdsInFns__collected(
            prog.fns.clone(),
            offset.clone(),
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
        ),
        stmts: shiftFnIdsInStmts__collected(
            prog.stmts.clone(),
            offset,
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
        ),
    }
}

/// Shift all embedded direct-call ids in one function definition.
pub fn shiftFnIdsInFn(
    fd: &crate::aver_generated::domain::ast::FnDef,
    offset: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::ast::FnDef {
        name: fd.name.clone(),
        params: fd.params.clone(),
        body: shiftFnIdsInStmts__collected(
            fd.body.clone(),
            offset.clone(),
            aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
        ),
        slotCount: fd.slotCount.clone(),
        slotMap: fd.slotMap.clone(),
        fastPath: shiftFnIdsInFastPath(&fd.fastPath, offset),
        tailLoop: fd.tailLoop,
    }
}

/// Shift fast-path target ids that were resolved against a module-local function list.
pub fn shiftFnIdsInFastPath(
    path: &crate::aver_generated::domain::ast::FnFastPath,
    offset: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    match path.clone() {
        crate::aver_generated::domain::ast::FnFastPath::FastForwardCall(targetId, slotArgs) => {
            crate::aver_generated::domain::ast::FnFastPath::FastForwardCall(
                targetId.add(&offset),
                slotArgs,
            )
        }
        _ => path.clone(),
    }
}

/// Shift direct-call ids through a statement list.
#[inline(always)]
pub fn shiftFnIdsInStmts(
    mut stmts: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut offset: aver_rt::AverInt,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return acc.reverse(); }, [stmt, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(shiftFnIdsInStmt(&stmt, offset.clone()), &acc);
            stmts = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Shift direct-call ids in one statement.
pub fn shiftFnIdsInStmt(
    stmt: &crate::aver_generated::domain::ast::Stmt,
    offset: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Stmt {
    crate::cancel_checkpoint();
    match stmt.clone() {
        crate::aver_generated::domain::ast::Stmt::StmtBind(name, expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtBind(
                name,
                shiftFnIdsInExpr(&expr, offset),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(
                slot,
                shiftFnIdsInExpr(&expr, offset),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtExpr(shiftFnIdsInExpr(&expr, offset))
        }
    }
}

/// Shift direct-call ids in one expression tree.
pub fn shiftFnIdsInExpr(
    expr: &crate::aver_generated::domain::ast::Expr,
    offset: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprBoolBranch(cond, thenExpr, elseExpr) => {
            let cond = (*cond).clone();
            let thenExpr = (*thenExpr).clone();
            let elseExpr = (*elseExpr).clone();
            crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                std::sync::Arc::new(shiftFnIdsInExpr(&cond, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&thenExpr, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&elseExpr, offset)),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
            vecExpr,
            idxExpr,
            defaultValue,
        ) => {
            let vecExpr = (*vecExpr).clone();
            let idxExpr = (*idxExpr).clone();
            crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                std::sync::Arc::new(shiftFnIdsInExpr(&vecExpr, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&idxExpr, offset)),
                defaultValue,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(a, b, defaultValue) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
                defaultValue,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprAdd(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprAdd(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprSub(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprSub(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprMul(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprMul(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprDiv(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprDiv(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprEq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprEq(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprNeq(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprNeq(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        _ => shiftFnIdsInExprTail(expr, offset),
    }
}

/// Finish shifting direct-call ids through the remaining aggregate forms.
pub fn shiftFnIdsInExprTail(
    expr: &crate::aver_generated::domain::ast::Expr,
    offset: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprLt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprLt(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGt(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprGt(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprLte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprLte(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprGte(a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::aver_generated::domain::ast::Expr::ExprGte(
                std::sync::Arc::new(shiftFnIdsInExpr(&a, offset.clone())),
                std::sync::Arc::new(shiftFnIdsInExpr(&b, offset)),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprConcat(parts) => {
            crate::aver_generated::domain::ast::Expr::ExprConcat(shiftFnIdsInExprs__collected(
                parts,
                offset,
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprTuple(exprs) => {
            crate::aver_generated::domain::ast::Expr::ExprTuple(shiftFnIdsInExprs__collected(
                exprs,
                offset,
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprList(exprs) => {
            crate::aver_generated::domain::ast::Expr::ExprList(shiftFnIdsInExprs__collected(
                exprs,
                offset,
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprRecord(name, fields) => {
            crate::aver_generated::domain::ast::Expr::ExprRecord(
                name,
                shiftFnIdsInFields__collected(
                    fields,
                    offset,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        _ => shiftFnIdsInExprCalls(expr, offset),
    }
}

/// Finish shifting direct-call ids through access, call, and match forms.
pub fn shiftFnIdsInExprCalls(
    expr: &crate::aver_generated::domain::ast::Expr,
    offset: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    match expr.clone() {
        crate::aver_generated::domain::ast::Expr::ExprFieldAccess(obj, field) => {
            let obj = (*obj).clone();
            crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
                std::sync::Arc::new(shiftFnIdsInExpr(&obj, offset)),
                field,
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCall(name, args) => {
            crate::aver_generated::domain::ast::Expr::ExprCall(
                name,
                shiftFnIdsInExprs__collected(
                    args,
                    offset,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCallDirect(fnId, args) => {
            crate::aver_generated::domain::ast::Expr::ExprCallDirect(
                fnId.add(&offset),
                shiftFnIdsInExprs__collected(
                    args,
                    offset,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(name, args) => {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                name,
                shiftFnIdsInExprs__collected(
                    args,
                    offset,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(id, args) => {
            crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                id,
                shiftFnIdsInExprs__collected(
                    args,
                    offset,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprMatch(scrutinee, arms) => {
            let scrutinee = (*scrutinee).clone();
            crate::aver_generated::domain::ast::Expr::ExprMatch(
                std::sync::Arc::new(shiftFnIdsInExpr(&scrutinee, offset.clone())),
                shiftFnIdsInArms__collected(
                    arms,
                    offset,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            )
        }
        crate::aver_generated::domain::ast::Expr::ExprPropagate(inner) => {
            let inner = (*inner).clone();
            crate::aver_generated::domain::ast::Expr::ExprPropagate(std::sync::Arc::new(
                shiftFnIdsInExpr(&inner, offset),
            ))
        }
        crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(exprs, unwrap) => {
            crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
                shiftFnIdsInExprs__collected(
                    exprs,
                    offset,
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
                unwrap,
            )
        }
        _ => expr.clone(),
    }
}

/// Shift direct-call ids in a list of expressions.
#[inline(always)]
pub fn shiftFnIdsInExprs(
    mut exprs: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut offset: aver_rt::AverInt,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return acc.reverse(); }, [expr, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(shiftFnIdsInExpr(&expr, offset.clone()), &acc);
            exprs = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Shift direct-call ids in record field expressions.
#[inline(always)]
pub fn shiftFnIdsInFields(
    mut fields: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut offset: aver_rt::AverInt,
    mut acc: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return acc.reverse(); }, [pair, rest] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend((name, shiftFnIdsInExpr(&expr, offset.clone())), &acc);
            fields = __tco0;
            acc = __tco2;
            continue;
        } } })
    }
}

/// Shift direct-call ids in match arm bodies.
#[inline(always)]
pub fn shiftFnIdsInArms(
    mut arms: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut offset: aver_rt::AverInt,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(arms, [] => { return acc.reverse(); }, [arm, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(crate::aver_generated::domain::ast::MatchArm { pattern: arm.pattern.clone(), body: shiftFnIdsInExpr(&arm.body, offset.clone()), bindingSlots: arm.bindingSlots.clone() }, &acc);
            arms = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Run source and return result representation or error.
pub fn runRepr(source: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    match run(source) {
        Ok(val @ _) => crate::aver_generated::domain::value::valRepr(&val),
        Err(e @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                );
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
        Ok(pair @ _) => {
            let (prog, moduleFns) = pair;
            runFileLoaded(&prog, &moduleFns)
        }
        Err(e @ _) => Err(e),
    }
}

/// Turn a loaded guest program into a representation string.
pub fn runFileLoaded(
    prog: &crate::aver_generated::domain::ast::Program,
    moduleFns: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> Result<AverStr, AverStr> {
    crate::cancel_checkpoint();
    match runGuestProgram(prog, moduleFns, &aver_rt::AverList::empty()) {
        Ok(val @ _) => Ok(crate::aver_generated::domain::value::valRepr(&val)),
        Err(e @ _) => Err(e),
    }
}

/// Read a file and prepare the guest program plus loaded modules.
pub fn loadProgramFromFile(
    path: AverStr,
    moduleRoot: AverStr,
) -> Result<
    (
        crate::aver_generated::domain::ast::Program,
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    let source = {
        let __provider_arg0 = path;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.readText",
            "recorded",
            vec![crate::aver_replay::ReplayValue::to_replay_json(
                &__provider_arg0,
            )],
            || {
                crate::provider_support::invoke::<Result<AverStr, AverStr>>(
                    "Disk",
                    "Disk.readText",
                    vec![crate::provider_support::encode(__provider_arg0, "Disk")],
                    None,
                    "Result<String, String>",
                )
            },
        )
    }?;
    prepareProgramWithModules(source, moduleRoot)
}

/// Dispatch normalized CLI args: path, module root, then guest args. Carries the user main()'s return Val up so the wrapping replay scope serialises it as recording.output.
pub fn runFromFileWithRest(
    path: AverStr,
    rest: &aver_rt::AverList<AverStr>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    aver_list_match!(rest.clone(), [] => runCliFile(path, AverStr::from("."), &aver_rt::AverList::empty()), [moduleRoot, guestArgs] => runCliFile(path, moduleRoot, &guestArgs))
}

/// Load a guest file outside scope, then execute the guest program inside the guest boundary. Propagates the user main()'s return Val unchanged.
pub fn runCliFile(
    path: AverStr,
    moduleRoot: AverStr,
    guestArgs: &aver_rt::AverList<AverStr>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let prepared = loadProgramFromFile(path, moduleRoot)?;
    {
        let (prog, moduleFns) = prepared;
        runGuestCliProgram(&prog, &moduleFns, &prog.fns, guestArgs)
    }
}

/// Mirror host CLI semantics: surface user main() Result.Err as process failure, but otherwise propagate the live return Val so replay-mode output comparison sees the actual value (recording.output stores the serialised Val, replay re-serialises and compares — dropping to Unit here would force every recording to claim Unit-output, masking real divergence).
#[inline(always)]
pub fn finishCliRun(
    localFns: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    result: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if hasLocalMain(localFns.clone()) {
        finishCliMainResult(result)
    } else {
        Ok(result.clone())
    }
}

/// Convert a guest main() return value into CLI success or failure. Successful returns propagate the live Val so the replay scope can serialise it as recording.output.
pub fn finishCliMainResult(
    result: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match result.clone() {
        crate::aver_generated::domain::value::Val::ValErr(err) => {
            let err = (*err).clone();
            Err((AverStr::from("Main returned error: ")
                + &crate::aver_generated::domain::value::valRepr(&err)))
        }
        _ => Ok(result.clone()),
    }
}

/// Check whether the entry program defines its own main().
#[inline(always)]
pub fn hasLocalMain(mut fns: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>) -> bool {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fns, [] => { return false; }, [f, rest] => { if (&*f.name == "main") { return true; } else { {
            let __tco0 = rest;
            fns = __tco0;
            continue;
        } } })
    }
}

/// Print unless the result is Unit.
pub fn printIfNotUnit(s: AverStr) -> () {
    crate::cancel_checkpoint();
    if (&*s == "()") {
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
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(31)).to_usize().unwrap_or(0),
                );
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
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(31)).to_usize().unwrap_or(0),
                );
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
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(28)).to_usize().unwrap_or(0),
                );
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

/// Synthesized indexed worker of `loadOneModule`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn loadOneModule__indexed(
    dep: AverStr,
    rest: &aver_rt::AverList<AverStr>,
    moduleRoot: AverStr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    loaded: &aver_rt::AverMap<AverStr, bool>,
    __str_index: &aver_rt::StringIndex,
) -> Result<
    (
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
        aver_rt::AverMap<AverStr, bool>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    let path = findModulePath__indexed(
        dep.clone(),
        moduleRoot.clone(),
        aver_rt::AverInt::from_i64(0),
        __str_index.clone(),
    );
    let source = {
        let __provider_arg0 = path;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.readText",
            "recorded",
            vec![crate::aver_replay::ReplayValue::to_replay_json(
                &__provider_arg0,
            )],
            || {
                crate::provider_support::invoke::<Result<AverStr, AverStr>>(
                    "Disk",
                    "Disk.readText",
                    vec![crate::provider_support::encode(__provider_arg0, "Disk")],
                    None,
                    "Result<String, String>",
                )
            },
        )
    }?;
    let tokens = crate::aver_generated::domain::lexer::lex(source);
    let prog = crate::aver_generated::domain::parser::parse(&tokens)?;
    let moduleFns = resolveQualifiedModuleFns__indexed(&prog, dep.clone(), __str_index);
    let loaded2 = loaded.clone().insert_owned(dep, true);
    let innerResult = loadModules(prog.deps.clone(), moduleRoot.clone(), acc.clone(), loaded2)?;
    {
        let (accWithInner, loaded3) = innerResult;
        loadModules(
            rest.clone(),
            moduleRoot,
            aver_rt::AverList::concat(
                &accWithInner.clone(),
                &shiftFnIdsInFns__collected(
                    moduleFns,
                    aver_rt::AverInt::from_i64(accWithInner.len() as i64),
                    aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    ),
                ),
            ),
            loaded3,
        )
    }
}

/// Synthesized indexed worker of `findModulePath`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn findModulePath__indexed(
    mut dep: AverStr,
    mut root: AverStr,
    mut depth: aver_rt::AverInt,
    __str_index: aver_rt::StringIndex,
) -> AverStr {
    let __str_index = std::sync::Arc::new(__str_index);
    loop {
        crate::cancel_checkpoint();
        let path = modulePathFromName__indexed(dep.clone(), root.clone(), &*__str_index);
        if {
            let __provider_arg0 = path.clone();
            crate::cancel_checkpoint();
            crate::aver_replay::invoke_capability_effect(
                "Disk.exists",
                "recorded",
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )],
                || {
                    crate::provider_support::invoke::<bool>(
                        "Disk",
                        "Disk.exists",
                        vec![crate::provider_support::encode(__provider_arg0, "Disk")],
                        None,
                        "Bool",
                    )
                },
            )
        } {
            return path;
        } else {
            if (depth < aver_rt::AverInt::from_i64(3)) {
                {
                    let __tco1 = (root + &AverStr::from("/.."));
                    let __tco2 = depth.add(&aver_rt::AverInt::from_i64(1));
                    root = __tco1;
                    depth = __tco2;
                    continue;
                }
            } else {
                return path;
            }
        }
    }
}

/// Synthesized indexed worker of `modulePathFromName`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn modulePathFromName__indexed(
    name: AverStr,
    moduleRoot: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> AverStr {
    crate::cancel_checkpoint();
    (((moduleRoot + &AverStr::from("/"))
        + &dotToSlash__indexed(
            name.clone(),
            aver_rt::AverInt::from_i64(0),
            aver_rt::AverInt::from_i64(name.chars().count() as i64),
            AverStr::from(""),
            __str_index.clone(),
        ))
        + &AverStr::from(".av"))
}

/// Synthesized indexed worker of `dotToSlash`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn dotToSlash__indexed(
    mut name: AverStr,
    mut pos: aver_rt::AverInt,
    mut total: aver_rt::AverInt,
    mut acc: AverStr,
    __str_index: aver_rt::StringIndex,
) -> AverStr {
    let __str_index = std::sync::Arc::new(__str_index);
    loop {
        crate::cancel_checkpoint();
        let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
        if (pos < total) {
            match aver_rt::string_index_char_at(&name, &__str_index, &pos) {
                Some(c @ _) => {
                    if (&*c == ".") {
                        {
                            let __tco1 = nextPos;
                            let __tco3 = (acc + &AverStr::from("/"));
                            pos = __tco1;
                            acc = __tco3;
                            continue;
                        }
                    } else {
                        {
                            let __tco1 = nextPos;
                            let __tco3 = (acc + &(c.to_lowercase()).into_aver());
                            pos = __tco1;
                            acc = __tco3;
                            continue;
                        }
                    }
                }
                None => {
                    return acc;
                }
            }
        } else {
            return acc;
        }
    }
}

/// Synthesized indexed worker of `resolveQualifiedModuleFns`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn resolveQualifiedModuleFns__indexed(
    prog: &crate::aver_generated::domain::ast::Program,
    dep: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    crate::cancel_checkpoint();
    let qualifiedProg = crate::aver_generated::domain::ast::Program {
        deps: prog.deps.clone(),
        fns: qualifyFns__indexed(&prog.fns, dep, &aver_rt::AverList::empty(), __str_index),
        stmts: prog.stmts.clone(),
    };
    crate::aver_generated::domain::resolver::resolveProgram(&qualifiedProg).fns
}

/// Synthesized collecting variant of `shiftFnIdsInFns`. Appends to a builder where `shiftFnIdsInFns` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn shiftFnIdsInFns__collected(
    mut fns: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut offset: aver_rt::AverInt,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fns, [] => { return aver_rt::list_builder_finalize(acc); }, [fd, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, shiftFnIdsInFn(&fd, offset.clone()));
            fns = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `shiftFnIdsInStmts`. Appends to a builder where `shiftFnIdsInStmts` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn shiftFnIdsInStmts__collected(
    mut stmts: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut offset: aver_rt::AverInt,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(stmts, [] => { return aver_rt::list_builder_finalize(acc); }, [stmt, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, shiftFnIdsInStmt(&stmt, offset.clone()));
            stmts = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `shiftFnIdsInExprs`. Appends to a builder where `shiftFnIdsInExprs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn shiftFnIdsInExprs__collected(
    mut exprs: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut offset: aver_rt::AverInt,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(exprs, [] => { return aver_rt::list_builder_finalize(acc); }, [expr, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, shiftFnIdsInExpr(&expr, offset.clone()));
            exprs = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

/// Synthesized collecting variant of `shiftFnIdsInFields`. Appends to a builder where `shiftFnIdsInFields` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn shiftFnIdsInFields__collected(
    mut fields: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut offset: aver_rt::AverInt,
    mut acc: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(fields, [] => { return aver_rt::list_builder_finalize(acc); }, [pair, rest] => { { let (name, expr) = pair; {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, (name, shiftFnIdsInExpr(&expr, offset.clone())));
            fields = __tco0;
            acc = __tco2;
            continue;
        } } })
    }
}

/// Synthesized collecting variant of `shiftFnIdsInArms`. Appends to a builder where `shiftFnIdsInArms` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn shiftFnIdsInArms__collected(
    mut arms: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut offset: aver_rt::AverInt,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(arms, [] => { return aver_rt::list_builder_finalize(acc); }, [arm, rest] => { {
            let __tco0 = rest;
            let __tco2 = aver_rt::list_builder_push(acc, crate::aver_generated::domain::ast::MatchArm { pattern: arm.pattern.clone(), body: shiftFnIdsInExpr(&arm.body, offset.clone()), bindingSlots: arm.bindingSlots.clone() });
            arms = __tco0;
            acc = __tco2;
            continue;
        } })
    }
}

pub fn main() -> Result<(), AverStr> {
    crate::cancel_checkpoint();
    let args = {
        crate::cancel_checkpoint();
        aver_replay::invoke_effect("Args.get", vec![], || aver_replay::current_cli_args())
    };
    aver_list_match!(args, [] => runDemo(), [path, rest] => match runFromFileWithRest(path, &rest) { Ok(_) => { Ok(()) }, Err(e @ _) => { Err(e) } })
}
