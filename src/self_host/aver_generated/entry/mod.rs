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
    __str_index @ _: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    loop {
        __state = match __state {
            __MutualTco1::QualifyFns__indexed(mut fns @ _, mut prefix @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                aver_list_match!(fns, [] => { return acc.reverse() }, [f, rest] => __MutualTco1::QualifyFnsOne__indexed(f, rest, prefix, acc))
            }
            __MutualTco1::QualifyFnsOne__indexed(
                mut f @ _,
                mut rest @ _,
                mut prefix @ _,
                mut acc @ _,
            ) => {
                crate::cancel_checkpoint();
                let qualified @ _ = Box::new(crate::aver_generated::domain::ast::FnDef__Fields {
                    name: ((prefix.clone() + &::aver_rt::str_lit!(".")) + &f.name),
                    params: f.params.clone(),
                    body: f.body.clone(),
                    slotCount: f.slotCount.clone(),
                    slotMap: f.slotMap.clone(),
                    fastPath: f.fastPath.clone(),
                    tailLoop: f.tailLoop,
                });
                let __arg6 @ _ = aver_rt::AverList::prepend_owned(
                    f,
                    aver_rt::AverList::prepend_owned(qualified, acc),
                );
                __MutualTco1::QualifyFns__indexed(rest, prefix, __arg6)
            }
        };
    }
}

/// Synthesized indexed worker of `qualifyFns`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn qualifyFns__indexed(
    fns @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    prefix @ _: AverStr,
    acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    __str_index @ _: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    __mutual_tco_trampoline_1(
        __MutualTco1::QualifyFns__indexed(fns, prefix, acc),
        &__str_index,
    )
}

/// Synthesized indexed worker of `qualifyFnsOne`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn qualifyFnsOne__indexed(
    f @ _: crate::aver_generated::domain::ast::FnDef,
    rest @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    prefix @ _: AverStr,
    acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    __str_index @ _: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    __mutual_tco_trampoline_1(
        __MutualTco1::QualifyFnsOne__indexed(f, rest, prefix, acc),
        &__str_index,
    )
}

/// Lex, parse, resolve, and evaluate an Aver source string (no module loading).
pub fn run(source @ _: AverStr) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let tokens @ _ = crate::aver_generated::domain::lexer::lex(source);
    let prog @ _ = crate::aver_generated::domain::parser::parse(&tokens)?;
    let resolved @ _ = crate::aver_generated::domain::resolver::resolveProgram(&prog);
    let __arg4 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>>::empty();
    let __arg5 @ _ = <aver_rt::AverList<AverStr>>::empty();
    runGuestProgram(&resolved, &__arg4, &__arg5)
}

/// Lex, parse, resolve, load modules, and evaluate.
pub fn runWithModules(
    source @ _: AverStr,
    moduleRoot @ _: AverStr,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let prepared @ _ = prepareProgramWithModules(source, moduleRoot)?;
    {
        let (prog, moduleFns) = prepared;
        {
            let __arg5 @ _ = <aver_rt::AverList<AverStr>>::empty();
            runGuestProgram(&prog, &moduleFns, &__arg5)
        }
    }
}

/// Lex, parse, resolve, load modules, and return a guest program ready for execution.
pub fn prepareProgramWithModules(
    source @ _: AverStr,
    moduleRoot @ _: AverStr,
) -> Result<
    (
        crate::aver_generated::domain::ast::Program,
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    let tokens @ _ = crate::aver_generated::domain::lexer::lex(source);
    let prog @ _ = crate::aver_generated::domain::parser::parse(&tokens)?;
    let resolved @ _ = crate::aver_generated::domain::resolver::resolveProgram(&prog);
    let r @ _ = {
        let __arg7 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>>::empty();
        {
            let __arg8 @ _ = <aver_rt::AverMap<AverStr, bool>>::new();
            loadModules(&resolved.deps, moduleRoot, __arg7, __arg8)
        }
    }?;
    {
        let (moduleFns, _) = r;
        Ok((resolved, moduleFns))
    }
}

/// Execute an already-loaded guest program. guestArgs marks the guest input boundary for scoped replay and policy in generated Rust.
pub fn runGuestProgram(
    prog @ _: &crate::aver_generated::domain::ast::Program,
    moduleFns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    guestArgs @ _: &aver_rt::AverList<AverStr>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let __arg4 @ _ = {
        let __arg3 @ _ = aver_rt::AverInt::from_i64(moduleFns.len() as i64);
        shiftFnIdsInProgram(prog, __arg3)
    };
    crate::aver_generated::domain::eval::evalProgramWithFns(&__arg4, moduleFns)
}

/// Execute a loaded guest program with CLI-compatible main semantics inside the guest boundary. Returns the user main()'s return Val so the replay scope can serialise it as recording.output (and replay-mode output comparison sees the live value), instead of dropping it to Unit before the wrapping aver_replay scope captures the result.
pub fn runGuestCliProgram(
    prog @ _: &crate::aver_generated::domain::ast::Program,
    moduleFns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    localFns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    guestArgs @ _: &aver_rt::AverList<AverStr>,
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
                        Ok(result @ _) => finishCliRun(localFns, result),
                        Err(e @ _) => Err((::aver_rt::str_lit!("Runtime error: ") + &e)),
                    }
                },
            )
        },
    )
}

/// Load all module dependencies, skipping already-loaded modules.
pub fn loadModules(
    deps @ _: &aver_rt::AverList<AverStr>,
    mut moduleRoot @ _: AverStr,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut loaded @ _: aver_rt::AverMap<AverStr, bool>,
) -> Result<
    (
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
        aver_rt::AverMap<AverStr, bool>,
    ),
    AverStr,
> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut deps @ _: &aver_rt::AverList<AverStr> = deps;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = deps;
            if __list_subject.is_empty() {
                return Ok((acc, loaded));
            } else {
                if let Some((dep, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    if loaded.contains_key(&dep) {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            deps = &__hold0;
                            continue;
                        }
                    } else {
                        return loadOneModule(dep, &rest, moduleRoot, &acc, loaded);
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Load one module. Tries moduleRoot, then parent dirs up to 3 levels.
pub fn loadOneModule(
    dep @ _: AverStr,
    rest @ _: &aver_rt::AverList<AverStr>,
    moduleRoot @ _: AverStr,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut loaded @ _: aver_rt::AverMap<AverStr, bool>,
) -> Result<
    (
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
        aver_rt::AverMap<AverStr, bool>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    let __arg5 @ _ = aver_rt::string_index_build(&dep);
    loadOneModule__indexed(dep, rest, moduleRoot, acc, loaded, &__arg5)
}

/// Find module file, trying root then parent dirs.
pub fn findModulePath(dep @ _: AverStr, root @ _: AverStr, depth @ _: aver_rt::AverInt) -> AverStr {
    crate::cancel_checkpoint();
    let __arg3 @ _ = aver_rt::string_index_build(&dep);
    findModulePath__indexed(dep, root, depth, &__arg3)
}

/// Convert Module.Name to file path: Domain.Foo -> domain/foo.av
#[inline(always)]
pub fn modulePathFromName(name @ _: AverStr, moduleRoot @ _: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::string_index_build(&name);
    modulePathFromName__indexed(name, moduleRoot, __arg2)
}

/// Replace dots with slashes and lowercase first char of each segment.
#[inline(always)]
pub fn dotToSlash(
    name @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    total @ _: aver_rt::AverInt,
    acc @ _: AverStr,
) -> AverStr {
    crate::cancel_checkpoint();
    let __arg4 @ _ = aver_rt::string_index_build(&name);
    dotToSlash__indexed(name, pos, total, acc, __arg4)
}

/// Register functions with both qualified (Domain.Foo.bar) and unqualified (bar) names.
#[inline(always)]
pub fn qualifyFns(
    fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    prefix @ _: AverStr,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    crate::cancel_checkpoint();
    let __arg3 @ _ = aver_rt::string_index_build(&prefix);
    qualifyFns__indexed(fns.clone(), prefix, acc.clone(), &__arg3)
}

/// Add both qualified and unqualified versions.
#[inline(always)]
pub fn qualifyFnsOne(
    mut f @ _: crate::aver_generated::domain::ast::FnDef,
    rest @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    prefix @ _: AverStr,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    crate::cancel_checkpoint();
    let __arg4 @ _ = aver_rt::string_index_build(&prefix);
    qualifyFnsOne__indexed(f, rest.clone(), prefix, acc.clone(), &__arg4)
}

/// Duplicate module function names first, then resolve locally so direct-call ids are consistent within that module.
#[inline(always)]
pub fn resolveQualifiedModuleFns(
    prog @ _: &crate::aver_generated::domain::ast::Program,
    dep @ _: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::string_index_build(&dep);
    resolveQualifiedModuleFns__indexed(prog, dep, &__arg2)
}

/// Shift module-local direct-call ids so they point into the final combined function store.
#[inline(always)]
pub fn shiftFnIdsInFns(
    fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut offset @ _: aver_rt::AverInt,
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
                if let (Some(fd), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco2 = aver_rt::AverList::prepend_owned(
                            shiftFnIdsInFn(fd.clone(), offset.clone()),
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

/// Shift entry-program direct-call ids before prepending loaded module functions.
pub fn shiftFnIdsInProgram(
    prog @ _: &crate::aver_generated::domain::ast::Program,
    offset @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Program {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::ast::Program {
        deps: prog.deps.clone(),
        fns: {
            let __arg2 @ _ =
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
            shiftFnIdsInFns__collected(&prog.fns, offset.clone(), __arg2)
        },
        stmts: {
            let __arg3 @ _ =
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
            shiftFnIdsInStmts__collected(&prog.stmts, offset, __arg3)
        },
    }
}

/// Shift all embedded direct-call ids in one function definition.
pub fn shiftFnIdsInFn(
    mut fd @ _: crate::aver_generated::domain::ast::FnDef,
    offset @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::FnDef {
    crate::cancel_checkpoint();
    Box::new(crate::aver_generated::domain::ast::FnDef__Fields {
        name: fd.name,
        params: fd.params,
        body: {
            let __arg2 @ _ =
                aver_rt::list_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0));
            shiftFnIdsInStmts__collected(&fd.body, offset.clone(), __arg2)
        },
        slotCount: fd.slotCount,
        slotMap: fd.slotMap,
        fastPath: shiftFnIdsInFastPath(fd.fastPath, offset),
        tailLoop: fd.tailLoop,
    })
}

/// Shift fast-path target ids that were resolved against a module-local function list.
pub fn shiftFnIdsInFastPath(
    mut path @ _: crate::aver_generated::domain::ast::FnFastPath,
    offset @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::FnFastPath {
    crate::cancel_checkpoint();
    match &path {
        crate::aver_generated::domain::ast::FnFastPath::FastForwardCall(targetId, slotArgs) => {
            let targetId = targetId.clone();
            crate::aver_generated::domain::ast::FnFastPath::FastForwardCall(
                targetId.add(&offset),
                slotArgs.clone(),
            )
        }
        _ => path,
    }
}

/// Shift direct-call ids through a statement list.
#[inline(always)]
pub fn shiftFnIdsInStmts(
    stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut offset @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = stmts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = stmts;
            if __list_subject.is_empty() {
                return acc.reverse();
            } else {
                if let (Some(stmt), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco2 = aver_rt::AverList::prepend_owned(
                            shiftFnIdsInStmt(stmt, offset.clone()),
                            acc,
                        );
                        __hold0 = __tco0;
                        stmts = &__hold0;
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

/// Shift direct-call ids in one statement.
pub fn shiftFnIdsInStmt(
    stmt @ _: &crate::aver_generated::domain::ast::Stmt,
    offset @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Stmt {
    crate::cancel_checkpoint();
    match stmt {
        crate::aver_generated::domain::ast::Stmt::StmtBind(name, expr) => {
            let name = name.clone();
            crate::aver_generated::domain::ast::Stmt::StmtBind(
                name,
                shiftFnIdsInExpr(expr.clone(), offset),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtBindSlot(slot, expr) => {
            let slot = slot.clone();
            crate::aver_generated::domain::ast::Stmt::StmtBindSlot(
                slot,
                shiftFnIdsInExpr(expr.clone(), offset),
            )
        }
        crate::aver_generated::domain::ast::Stmt::StmtExpr(expr) => {
            crate::aver_generated::domain::ast::Stmt::StmtExpr(shiftFnIdsInExpr(
                expr.clone(),
                offset,
            ))
        }
    }
}

/// Shift direct-call ids in one expression tree.
pub fn shiftFnIdsInExpr(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    offset @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprBoolBranch(__cond_fields) => {
                let cond = &__cond_fields.0;
                let thenExpr = &__cond_fields.1;
                let elseExpr = &__cond_fields.2;
                crate::aver_generated::domain::ast::Expr::ExprBoolBranch(
                    crate::aver_generated::domain::ast::Expr__ExprBoolBranch(
                        shiftFnIdsInExpr(cond.clone(), offset.clone()),
                        shiftFnIdsInExpr(thenExpr.clone(), offset.clone()),
                        shiftFnIdsInExpr(elseExpr.clone(), offset),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprVectorGetOrInt(
                __vecExpr_fields,
            ) => {
                let vecExpr = &__vecExpr_fields.0;
                let idxExpr = &__vecExpr_fields.1;
                let defaultValue = __vecExpr_fields.2.clone();
                crate::aver_generated::domain::ast::Expr::ExprVectorGetOrInt(
                    crate::aver_generated::domain::ast::Expr__ExprVectorGetOrInt(
                        shiftFnIdsInExpr(vecExpr.clone(), offset.clone()),
                        shiftFnIdsInExpr(idxExpr.clone(), offset),
                        defaultValue,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIntModOrInt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                let defaultValue = __a_fields.2.clone();
                crate::aver_generated::domain::ast::Expr::ExprIntModOrInt(
                    crate::aver_generated::domain::ast::Expr__ExprIntModOrInt(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                        defaultValue,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprAdd(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprAdd(
                    crate::aver_generated::domain::ast::Expr__ExprAdd(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprSub(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprSub(
                    crate::aver_generated::domain::ast::Expr__ExprSub(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMul(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprMul(
                    crate::aver_generated::domain::ast::Expr__ExprMul(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprDiv(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprDiv(
                    crate::aver_generated::domain::ast::Expr__ExprDiv(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprEq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprEq(
                    crate::aver_generated::domain::ast::Expr__ExprEq(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprNeq(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprNeq(
                    crate::aver_generated::domain::ast::Expr__ExprNeq(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                    ),
                )
            }
            _ => shiftFnIdsInExprTail(expr, offset),
        }
    }
}

/// Finish shifting direct-call ids through the remaining aggregate forms.
pub fn shiftFnIdsInExprTail(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    offset @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprLt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprLt(
                    crate::aver_generated::domain::ast::Expr__ExprLt(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGt(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprGt(
                    crate::aver_generated::domain::ast::Expr__ExprGt(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprLte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprLte(
                    crate::aver_generated::domain::ast::Expr__ExprLte(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprGte(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprGte(
                    crate::aver_generated::domain::ast::Expr__ExprGte(
                        shiftFnIdsInExpr(a.clone(), offset.clone()),
                        shiftFnIdsInExpr(b.clone(), offset),
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprConcat(__parts_fields) => {
                let parts = &__parts_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprConcat(
                    crate::aver_generated::domain::ast::Expr__ExprConcat({
                        let __arg15 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        shiftFnIdsInExprs__collected(parts, offset, __arg15)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprTuple(__exprs_fields) => {
                let exprs = &__exprs_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprTuple(
                    crate::aver_generated::domain::ast::Expr__ExprTuple({
                        let __arg16 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        shiftFnIdsInExprs__collected(exprs, offset, __arg16)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprList(__exprs_fields) => {
                let exprs = &__exprs_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprList(
                    crate::aver_generated::domain::ast::Expr__ExprList({
                        let __arg17 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        shiftFnIdsInExprs__collected(exprs, offset, __arg17)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprRecord(__name_fields) => {
                let name = __name_fields.0.clone();
                let fields = &__name_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprRecord(
                    crate::aver_generated::domain::ast::Expr__ExprRecord(name, {
                        let __arg18 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        shiftFnIdsInFields__collected(fields, offset, __arg18)
                    }),
                )
            }
            _ => shiftFnIdsInExprCalls(expr, offset),
        }
    }
}

/// Finish shifting direct-call ids through access, call, and match forms.
pub fn shiftFnIdsInExprCalls(
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
    offset @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::ast::Expr {
    crate::cancel_checkpoint();
    {
        let __t = &expr;
        match __t.view() {
            crate::aver_generated::domain::ast::Expr__view::ExprFieldAccess(__obj_fields) => {
                let obj = &__obj_fields.0;
                let field = __obj_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
                    crate::aver_generated::domain::ast::Expr__ExprFieldAccess(
                        shiftFnIdsInExpr(obj.clone(), offset),
                        field,
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCall(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = &__name_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprCall(
                    crate::aver_generated::domain::ast::Expr__ExprCall(name, {
                        let __arg17 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        shiftFnIdsInExprs__collected(args, offset, __arg17)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallDirect(__fnId_fields) => {
                let fnId = __fnId_fields.0.clone();
                let args = &__fnId_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprCallDirect(
                    crate::aver_generated::domain::ast::Expr__ExprCallDirect(fnId.add(&offset), {
                        let __arg18 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        shiftFnIdsInExprs__collected(args, offset, __arg18)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltin(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = &__name_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltin(
                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltin(name, {
                        let __arg19 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        shiftFnIdsInExprs__collected(args, offset, __arg19)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprCallBuiltinId(__id_fields) => {
                let id = __id_fields.0.clone();
                let args = &__id_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprCallBuiltinId(
                    crate::aver_generated::domain::ast::Expr__ExprCallBuiltinId(id, {
                        let __arg20 @ _ = aver_rt::list_builder_new(
                            (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                        );
                        shiftFnIdsInExprs__collected(args, offset, __arg20)
                    }),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprMatch(__scrutinee_fields) => {
                let scrutinee = &__scrutinee_fields.0;
                let arms = &__scrutinee_fields.1;
                crate::aver_generated::domain::ast::Expr::ExprMatch(
                    crate::aver_generated::domain::ast::Expr__ExprMatch(
                        shiftFnIdsInExpr(scrutinee.clone(), offset.clone()),
                        {
                            let __arg21 @ _ = aver_rt::list_builder_new(
                                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                            );
                            shiftFnIdsInArms__collected(arms, offset, __arg21)
                        },
                    ),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprPropagate(__inner_fields) => {
                let inner = &__inner_fields.0;
                crate::aver_generated::domain::ast::Expr::ExprPropagate(
                    crate::aver_generated::domain::ast::Expr__ExprPropagate(shiftFnIdsInExpr(
                        inner.clone(),
                        offset,
                    )),
                )
            }
            crate::aver_generated::domain::ast::Expr__view::ExprIndependentProduct(
                __exprs_fields,
            ) => {
                let exprs = &__exprs_fields.0;
                let unwrap = __exprs_fields.1.clone();
                crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
                    crate::aver_generated::domain::ast::Expr__ExprIndependentProduct(
                        {
                            let __arg22 @ _ = aver_rt::list_builder_new(
                                (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                            );
                            shiftFnIdsInExprs__collected(exprs, offset, __arg22)
                        },
                        unwrap,
                    ),
                )
            }
            _ => expr,
        }
    }
}

/// Shift direct-call ids in a list of expressions.
#[inline(always)]
pub fn shiftFnIdsInExprs(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut offset @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return acc.reverse();
            } else {
                if let (Some(expr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco2 = aver_rt::AverList::prepend_owned(
                            shiftFnIdsInExpr(expr.clone(), offset.clone()),
                            acc,
                        );
                        __hold0 = __tco0;
                        exprs = &__hold0;
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

/// Shift direct-call ids in record field expressions.
#[inline(always)]
pub fn shiftFnIdsInFields(
    fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut offset @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    let mut __hold0: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>;
    let mut fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> =
        fields;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fields;
            if __list_subject.is_empty() {
                return acc.reverse();
            } else {
                if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (name, expr) = pair.clone();
                        {
                            let __tco0 = rest;
                            let __tco2 = aver_rt::AverList::prepend_owned(
                                (name, shiftFnIdsInExpr(expr, offset.clone())),
                                acc,
                            );
                            __hold0 = __tco0;
                            fields = &__hold0;
                            acc = __tco2;
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

/// Shift direct-call ids in match arm bodies.
#[inline(always)]
pub fn shiftFnIdsInArms(
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut offset @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>;
    let mut arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> = arms;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = arms;
            if __list_subject.is_empty() {
                return acc.reverse();
            } else {
                if let (Some(arm), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco2 = aver_rt::AverList::prepend_owned(
                            crate::aver_generated::domain::ast::MatchArm {
                                pattern: arm.pattern.clone(),
                                body: shiftFnIdsInExpr(arm.body.clone(), offset.clone()),
                                bindingSlots: arm.bindingSlots.clone(),
                            },
                            acc,
                        );
                        __hold0 = __tco0;
                        arms = &__hold0;
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

/// Run source and return result representation or error.
pub fn runRepr(source @ _: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    match run(source) {
        Ok(val @ _) => crate::aver_generated::domain::value::valRepr(&val),
        Err(e @ _) => aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(23)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("ERROR: "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(e))));
            __b
        }),
    }
}

/// Read a file and run it through the self-hosted pipeline with module loading.
pub fn runFile(path @ _: AverStr, moduleRoot @ _: AverStr) -> Result<AverStr, AverStr> {
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
    prog @ _: &crate::aver_generated::domain::ast::Program,
    moduleFns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> Result<AverStr, AverStr> {
    crate::cancel_checkpoint();
    {
        let __subject = {
            let __arg4 @ _ = <aver_rt::AverList<AverStr>>::empty();
            runGuestProgram(prog, moduleFns, &__arg4)
        };
        match __subject {
            Ok(val @ _) => Ok(crate::aver_generated::domain::value::valRepr(&val)),
            Err(e @ _) => Err(e),
        }
    }
}

/// Read a file and prepare the guest program plus loaded modules.
pub fn loadProgramFromFile(
    path @ _: AverStr,
    moduleRoot @ _: AverStr,
) -> Result<
    (
        crate::aver_generated::domain::ast::Program,
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    let source @ _ = {
        let __provider_arg0: AverStr = path;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.readText",
            "recorded",
            crate::aver_replay::effect_args("Disk.readText", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
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
    path @ _: AverStr,
    rest @ _: &aver_rt::AverList<AverStr>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = rest;
        if let Some((moduleRoot, guestArgs)) = aver_rt::list_uncons_cloned(__list_subject) {
            runCliFile(path, moduleRoot, &guestArgs)
        } else {
            if __list_subject.is_empty() {
                {
                    let __arg4 @ _ = <aver_rt::AverList<AverStr>>::empty();
                    runCliFile(path, ::aver_rt::str_lit!("."), &__arg4)
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Load a guest file outside scope, then execute the guest program inside the guest boundary. Propagates the user main()'s return Val unchanged.
pub fn runCliFile(
    path @ _: AverStr,
    moduleRoot @ _: AverStr,
    guestArgs @ _: &aver_rt::AverList<AverStr>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let prepared @ _ = loadProgramFromFile(path, moduleRoot)?;
    {
        let (prog, moduleFns) = prepared;
        runGuestCliProgram(&prog, &moduleFns, &prog.fns, guestArgs)
    }
}

/// Mirror host CLI semantics: surface user main() Result.Err as process failure, but otherwise propagate the live return Val so replay-mode output comparison sees the actual value (recording.output stores the serialised Val, replay re-serialises and compares — dropping to Unit here would force every recording to claim Unit-output, masking real divergence).
#[inline(always)]
pub fn finishCliRun(
    localFns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut result @ _: crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    if hasLocalMain(localFns) {
        finishCliMainResult(result)
    } else {
        Ok(result)
    }
}

/// Convert a guest main() return value into CLI success or failure. Successful returns propagate the live Val so the replay scope can serialise it as recording.output.
pub fn finishCliMainResult(
    mut result @ _: crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = &result;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValErr(__err_fields) => {
                let err = &__err_fields.0;
                Err((::aver_rt::str_lit!("Main returned error: ")
                    + &crate::aver_generated::domain::value::valRepr(err)))
            }
            _ => Ok(result),
        }
    }
}

/// Check whether the entry program defines its own main().
#[inline(always)]
pub fn hasLocalMain(
    fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> bool {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>;
    let mut fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> = fns;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fns;
            if __list_subject.is_empty() {
                return false;
            } else {
                if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (&*f.name == "main") {
                        return true;
                    } else {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            fns = &__hold0;
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

/// Print unless the result is Unit.
pub fn printIfNotUnit(s @ _: AverStr) -> () {
    crate::cancel_checkpoint();
    if (&*s == "()") {
        {
            let __provider_arg0: AverStr = ::aver_rt::str_lit!("");
            crate::cancel_checkpoint();
            crate::aver_replay::invoke_capability_effect(
                "Console.print",
                "reissued",
                crate::aver_replay::effect_args("Console.print", || {
                    vec![crate::aver_replay::ReplayValue::to_replay_json(
                        &__provider_arg0,
                    )]
                }),
                || {
                    crate::provider_support::invoke::<()>(
                        "Console",
                        "Console.print",
                        vec![crate::provider_support::encode(__provider_arg0, "Console")],
                        None,
                        "Unit",
                    )
                },
            )
        }
    } else {
        {
            let __provider_arg0: AverStr = s;
            crate::cancel_checkpoint();
            crate::aver_replay::invoke_capability_effect(
                "Console.print",
                "reissued",
                crate::aver_replay::effect_args("Console.print", || {
                    vec![crate::aver_replay::ReplayValue::to_replay_json(
                        &__provider_arg0,
                    )]
                }),
                || {
                    crate::provider_support::invoke::<()>(
                        "Console",
                        "Console.print",
                        vec![crate::provider_support::encode(__provider_arg0, "Console")],
                        None,
                        "Unit",
                    )
                },
            )
        }
    }
}

/// Show built-in demo programs.
pub fn runDemo() -> Result<(), AverStr> {
    crate::cancel_checkpoint();
    let demoArithmetic @ _ = runRepr(::aver_rt::str_lit!("x = 3 + 4\nx * 2"));
    let demoDouble @ _ = runRepr(::aver_rt::str_lit!("fn double(n)\n    n + n\n\ndouble(21)"));
    let demoFib @ _ = runRepr(::aver_rt::str_lit!(
        "fn fib(n)\n    match n\n        0 -> 0\n        1 -> 1\n        _ -> fib(n - 1) + fib(n - 2)\n\nfib(10)"
    ));
    {
        let __provider_arg0: AverStr = ::aver_rt::str_lit!("=== Self-Hosted Mini Aver ===");
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.print",
            "reissued",
            crate::aver_replay::effect_args("Console.print", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.print",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    {
        let __provider_arg0: AverStr = ::aver_rt::str_lit!("");
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.print",
            "reissued",
            crate::aver_replay::effect_args("Console.print", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.print",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    {
        let __provider_arg0: AverStr = ::aver_rt::str_lit!(
            "Usage: aver run self_hosted/main.av --module-root self_hosted -- <file.av> <module-root> [guest args...]"
        );
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.print",
            "reissued",
            crate::aver_replay::effect_args("Console.print", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.print",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    {
        let __provider_arg0: AverStr = ::aver_rt::str_lit!("");
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.print",
            "reissued",
            crate::aver_replay::effect_args("Console.print", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.print",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    {
        let __provider_arg0: AverStr = ::aver_rt::str_lit!("Built-in demos:");
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.print",
            "reissued",
            crate::aver_replay::effect_args("Console.print", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.print",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    {
        let __provider_arg0: AverStr = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(31)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("  x=3+4; x*2 = "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(demoArithmetic),
            )));
            __b
        });
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.print",
            "reissued",
            crate::aver_replay::effect_args("Console.print", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.print",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    {
        let __provider_arg0: AverStr = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(31)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("  double(21) = "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(demoDouble),
            )));
            __b
        });
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.print",
            "reissued",
            crate::aver_replay::effect_args("Console.print", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.print",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    {
        let __provider_arg0: AverStr = aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(28)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("  fib(10) = "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(demoFib))));
            __b
        });
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Console.print",
            "reissued",
            crate::aver_replay::effect_args("Console.print", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
            || {
                crate::provider_support::invoke::<()>(
                    "Console",
                    "Console.print",
                    vec![crate::provider_support::encode(__provider_arg0, "Console")],
                    None,
                    "Unit",
                )
            },
        )
    };
    Ok(())
}

/// Synthesized indexed worker of `loadOneModule`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn loadOneModule__indexed(
    dep @ _: AverStr,
    rest @ _: &aver_rt::AverList<AverStr>,
    moduleRoot @ _: AverStr,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut loaded @ _: aver_rt::AverMap<AverStr, bool>,
    __str_index @ _: &aver_rt::StringIndex,
) -> Result<
    (
        aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
        aver_rt::AverMap<AverStr, bool>,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    let path @ _ = findModulePath__indexed(
        dep.clone(),
        moduleRoot.clone(),
        aver_rt::AverInt::from_i64(0),
        __str_index,
    );
    let source @ _ = {
        let __provider_arg0: AverStr = path;
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Disk.readText",
            "recorded",
            crate::aver_replay::effect_args("Disk.readText", || {
                vec![crate::aver_replay::ReplayValue::to_replay_json(
                    &__provider_arg0,
                )]
            }),
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
    let tokens @ _ = crate::aver_generated::domain::lexer::lex(source);
    let prog @ _ = crate::aver_generated::domain::parser::parse(&tokens)?;
    let moduleFns @ _ = resolveQualifiedModuleFns__indexed(&prog, dep.clone(), __str_index);
    let loaded2 @ _ = loaded.insert_owned(dep, true);
    let innerResult @ _ = loadModules(&prog.deps, moduleRoot.clone(), acc.clone(), loaded2)?;
    {
        let (accWithInner, loaded3) = innerResult;
        {
            let __arg17 @ _ = aver_rt::AverList::concat(&accWithInner.clone(), &{
                let __arg15 @ _ = aver_rt::AverInt::from_i64(accWithInner.len() as i64);
                {
                    let __arg16 @ _ = aver_rt::list_builder_new(
                        (aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0),
                    );
                    shiftFnIdsInFns__collected(&moduleFns, __arg15, __arg16)
                }
            });
            loadModules(rest, moduleRoot, __arg17, loaded3)
        }
    }
}

/// Synthesized indexed worker of `findModulePath`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn findModulePath__indexed(
    mut dep @ _: AverStr,
    mut root @ _: AverStr,
    mut depth @ _: aver_rt::AverInt,
    __str_index @ _: &aver_rt::StringIndex,
) -> AverStr {
    loop {
        crate::cancel_checkpoint();
        let path @ _ = modulePathFromName__indexed(dep.clone(), root.clone(), __str_index.clone());
        if {
            let __provider_arg0: AverStr = path.clone();
            crate::cancel_checkpoint();
            crate::aver_replay::invoke_capability_effect(
                "Disk.exists",
                "recorded",
                crate::aver_replay::effect_args("Disk.exists", || {
                    vec![crate::aver_replay::ReplayValue::to_replay_json(
                        &__provider_arg0,
                    )]
                }),
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
                    let __tco1 = (root + &::aver_rt::str_lit!("/.."));
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
    name @ _: AverStr,
    moduleRoot @ _: AverStr,
    mut __str_index @ _: aver_rt::StringIndex,
) -> AverStr {
    crate::cancel_checkpoint();
    (((moduleRoot + &::aver_rt::str_lit!("/"))
        + &{
            let __arg3 @ _ = aver_rt::AverInt::from_i64(name.chars().count() as i64);
            dotToSlash__indexed(
                name,
                aver_rt::AverInt::from_i64(0),
                __arg3,
                ::aver_rt::str_lit!(""),
                __str_index,
            )
        })
        + &::aver_rt::str_lit!(".av"))
}

/// Synthesized indexed worker of `dotToSlash`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn dotToSlash__indexed(
    mut name @ _: AverStr,
    mut pos @ _: aver_rt::AverInt,
    mut total @ _: aver_rt::AverInt,
    mut acc @ _: AverStr,
    mut __str_index @ _: aver_rt::StringIndex,
) -> AverStr {
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        if (pos < total) {
            match aver_rt::string_index_char_at(&name, &__str_index, &pos) {
                Some(c @ _) => {
                    if (&*c == ".") {
                        {
                            let __tco1 = nextPos;
                            let __tco3 = (acc + &::aver_rt::str_lit!("/"));
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
    prog @ _: &crate::aver_generated::domain::ast::Program,
    dep @ _: AverStr,
    __str_index @ _: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    crate::cancel_checkpoint();
    let qualifiedProg @ _ = crate::aver_generated::domain::ast::Program {
        deps: prog.deps.clone(),
        fns: {
            let __arg4 @ _ =
                <aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>>::empty();
            qualifyFns__indexed(prog.fns.clone(), dep, __arg4, __str_index)
        },
        stmts: prog.stmts.clone(),
    };
    crate::aver_generated::domain::resolver::resolveProgram(&qualifiedProg).fns
}

/// Synthesized collecting variant of `shiftFnIdsInFns`. Appends to a builder where `shiftFnIdsInFns` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn shiftFnIdsInFns__collected(
    fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
    mut offset @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::FnDef>;
    let mut fns @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::FnDef> = fns;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fns;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let (Some(fd), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco2 = aver_rt::list_builder_push(
                            acc,
                            shiftFnIdsInFn(fd.clone(), offset.clone()),
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

/// Synthesized collecting variant of `shiftFnIdsInStmts`. Appends to a builder where `shiftFnIdsInStmts` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn shiftFnIdsInStmts__collected(
    stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
    mut offset @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Stmt>;
    let mut stmts @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Stmt> = stmts;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = stmts;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let (Some(stmt), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco2 =
                            aver_rt::list_builder_push(acc, shiftFnIdsInStmt(stmt, offset.clone()));
                        __hold0 = __tco0;
                        stmts = &__hold0;
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

/// Synthesized collecting variant of `shiftFnIdsInExprs`. Appends to a builder where `shiftFnIdsInExprs` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn shiftFnIdsInExprs__collected(
    exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut offset @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::Expr> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>;
    let mut exprs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr> = exprs;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = exprs;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let (Some(expr), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco2 = aver_rt::list_builder_push(
                            acc,
                            shiftFnIdsInExpr(expr.clone(), offset.clone()),
                        );
                        __hold0 = __tco0;
                        exprs = &__hold0;
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

/// Synthesized collecting variant of `shiftFnIdsInFields`. Appends to a builder where `shiftFnIdsInFields` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn shiftFnIdsInFields__collected(
    fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut offset @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> {
    let mut __hold0: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>;
    let mut fields @ _: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)> =
        fields;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = fields;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let (Some(pair), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (name, expr) = pair.clone();
                        {
                            let __tco0 = rest;
                            let __tco2 = aver_rt::list_builder_push(
                                acc,
                                (name, shiftFnIdsInExpr(expr, offset.clone())),
                            );
                            __hold0 = __tco0;
                            fields = &__hold0;
                            acc = __tco2;
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

/// Synthesized collecting variant of `shiftFnIdsInArms`. Appends to a builder where `shiftFnIdsInArms` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here.
#[inline(always)]
pub fn shiftFnIdsInArms__collected(
    arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut offset @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>;
    let mut arms @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm> = arms;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = arms;
            if __list_subject.is_empty() {
                return aver_rt::list_builder_finalize(acc);
            } else {
                if let (Some(arm), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco2 = aver_rt::list_builder_push(
                            acc,
                            crate::aver_generated::domain::ast::MatchArm {
                                pattern: arm.pattern.clone(),
                                body: shiftFnIdsInExpr(arm.body.clone(), offset.clone()),
                                bindingSlots: arm.bindingSlots.clone(),
                            },
                        );
                        __hold0 = __tco0;
                        arms = &__hold0;
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

pub fn main() -> Result<(), AverStr> {
    crate::cancel_checkpoint();
    let args @ _ = {
        crate::cancel_checkpoint();
        crate::aver_replay::invoke_capability_effect(
            "Args.get",
            "recorded",
            crate::aver_replay::effect_args("Args.get", || vec![]),
            || {
                crate::provider_support::invoke::<aver_rt::AverList<AverStr>>(
                    "Args",
                    "Args.get",
                    vec![],
                    None,
                    "List<String>",
                )
            },
        )
    };
    aver_list_match!(args, [] => runDemo(), [path, rest] => match runFromFileWithRest(path, &rest) { Ok(_) => { Ok(()) }, Err(e @ _) => { Err(e) } })
}
