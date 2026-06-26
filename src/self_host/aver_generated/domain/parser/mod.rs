#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::lexer::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::parser::expr::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::parser_match::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;
#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    ParseFnBodyStmtsFlat(
        aver_rt::AverList<Token>,
        aver_rt::AverInt,
        aver_rt::AverList<Stmt>,
    ),
    ParseFnBodyOneStmtFlat(
        aver_rt::AverList<Token>,
        aver_rt::AverInt,
        aver_rt::AverList<Stmt>,
    ),
    ParseFnBodyAfterStmtFlat(
        aver_rt::AverList<Token>,
        aver_rt::AverInt,
        aver_rt::AverList<Stmt>,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
) -> Result<(aver_rt::AverList<Stmt>, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::ParseFnBodyStmtsFlat(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::parser_match::isBodyEndFlat(&tokens, pos.clone())
                {
                    return Ok((acc, pos));
                } else {
                    __MutualTco1::ParseFnBodyOneStmtFlat(tokens, pos, acc)
                }
            }
            __MutualTco1::ParseFnBodyOneStmtFlat(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let r = crate::aver_generated::domain::parser::parseStmt(&tokens, pos)?;
                {
                    let (stmt, pos2) = r;
                    __MutualTco1::ParseFnBodyAfterStmtFlat(
                        tokens,
                        pos2,
                        aver_rt::AverList::prepend(stmt, &acc),
                    )
                }
            }
            __MutualTco1::ParseFnBodyAfterStmtFlat(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkNewline => {
                        __MutualTco1::ParseFnBodyStmtsFlat(
                            tokens,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            acc,
                        )
                    }
                    _ => return Ok((acc, pos)),
                }
            }
        };
    }
}

/// Parse statements in function body (flat mode).
pub fn parseFnBodyStmtsFlat(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<Stmt>,
) -> Result<(aver_rt::AverList<Stmt>, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseFnBodyStmtsFlat(
        tokens.clone(),
        pos,
        acc.clone(),
    ))
}

/// Parse one statement in function body and continue (flat mode).
pub fn parseFnBodyOneStmtFlat(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<Stmt>,
) -> Result<(aver_rt::AverList<Stmt>, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseFnBodyOneStmtFlat(
        tokens.clone(),
        pos,
        acc.clone(),
    ))
}

/// After a body statement: single newline continues, anything else ends (flat mode).
pub fn parseFnBodyAfterStmtFlat(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<Stmt>,
) -> Result<(aver_rt::AverList<Stmt>, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseFnBodyAfterStmtFlat(
        tokens.clone(),
        pos,
        acc.clone(),
    ))
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    ParseFnBodyStmtsIndented(
        aver_rt::AverList<Token>,
        aver_rt::AverInt,
        aver_rt::AverList<Stmt>,
    ),
    ParseFnBodyOneStmtIndented(
        aver_rt::AverList<Token>,
        aver_rt::AverInt,
        aver_rt::AverList<Stmt>,
    ),
}

fn __mutual_tco_trampoline_2(
    mut __state: __MutualTco2,
) -> Result<(aver_rt::AverList<Stmt>, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::ParseFnBodyStmtsIndented(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let pos2 =
                    crate::aver_generated::domain::parser_match::skipNewlines(tokens.clone(), pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos2.clone()) {
                    crate::aver_generated::domain::token::Token::TkDedent => {
                        return Ok((acc, pos2.add(&aver_rt::AverInt::from_i64(1))));
                    }
                    crate::aver_generated::domain::token::Token::TkEof => return Ok((acc, pos2)),
                    _ => __MutualTco2::ParseFnBodyOneStmtIndented(tokens, pos2, acc),
                }
            }
            __MutualTco2::ParseFnBodyOneStmtIndented(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let r = crate::aver_generated::domain::parser::parseStmt(&tokens, pos)?;
                {
                    let (stmt, pos2) = r;
                    __MutualTco2::ParseFnBodyStmtsIndented(
                        tokens,
                        pos2,
                        aver_rt::AverList::prepend(stmt, &acc),
                    )
                }
            }
        };
    }
}

/// Parse statements until DEDENT or EOF.
pub fn parseFnBodyStmtsIndented(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<Stmt>,
) -> Result<(aver_rt::AverList<Stmt>, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseFnBodyStmtsIndented(
        tokens.clone(),
        pos,
        acc.clone(),
    ))
}

/// Parse one statement and continue with indented body.
pub fn parseFnBodyOneStmtIndented(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<Stmt>,
) -> Result<(aver_rt::AverList<Stmt>, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseFnBodyOneStmtIndented(
        tokens.clone(),
        pos,
        acc.clone(),
    ))
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    ParseProgram(
        aver_rt::AverList<Token>,
        aver_rt::AverInt,
        aver_rt::AverList<AverStr>,
        aver_rt::AverList<FnDef>,
        aver_rt::AverList<Stmt>,
    ),
    ParseProgramKeyword(
        aver_rt::AverList<Token>,
        aver_rt::AverInt,
        aver_rt::AverList<AverStr>,
        aver_rt::AverList<FnDef>,
        aver_rt::AverList<Stmt>,
        AverStr,
    ),
    ParseProgramModuleHeader(
        aver_rt::AverList<Token>,
        aver_rt::AverInt,
        aver_rt::AverList<FnDef>,
        aver_rt::AverList<Stmt>,
    ),
    ParseProgramFn(
        aver_rt::AverList<Token>,
        aver_rt::AverInt,
        aver_rt::AverList<AverStr>,
        aver_rt::AverList<FnDef>,
        aver_rt::AverList<Stmt>,
    ),
    ParseProgramStmt(
        aver_rt::AverList<Token>,
        aver_rt::AverInt,
        aver_rt::AverList<AverStr>,
        aver_rt::AverList<FnDef>,
        aver_rt::AverList<Stmt>,
    ),
}

fn __mutual_tco_trampoline_3(mut __state: __MutualTco3) -> Result<Program, AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::ParseProgram(mut tokens, mut pos, mut deps, mut fns, mut stmts) => {
                crate::cancel_checkpoint();
                let pos2 = crate::aver_generated::domain::parser_match::skipNewlinesAndDedents(
                    tokens.clone(),
                    pos,
                );
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos2.clone()) {
                    crate::aver_generated::domain::token::Token::TkEof => {
                        return Ok(crate::aver_generated::domain::ast::Program {
                            deps: deps,
                            fns: fns.reverse(),
                            stmts: stmts.reverse(),
                        });
                    }
                    crate::aver_generated::domain::token::Token::TkFn => {
                        __MutualTco3::ParseProgramFn(
                            tokens,
                            pos2.add(&aver_rt::AverInt::from_i64(1)),
                            deps,
                            fns,
                            stmts,
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkIdent(kw) => {
                        __MutualTco3::ParseProgramKeyword(tokens, pos2, deps, fns, stmts, kw)
                    }
                    _ => __MutualTco3::ParseProgramStmt(tokens, pos2, deps, fns, stmts),
                }
            }
            __MutualTco3::ParseProgramKeyword(
                mut tokens,
                mut pos,
                mut deps,
                mut fns,
                mut stmts,
                mut kw,
            ) => {
                crate::cancel_checkpoint();
                let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
                {
                    let __dispatch_subject = kw;
                    if &*__dispatch_subject == "module" {
                        __MutualTco3::ParseProgramModuleHeader(tokens, nextPos, fns, stmts)
                    } else {
                        if &*__dispatch_subject == "type" {
                            __MutualTco3::ParseProgram(
                                tokens.clone(),
                                crate::aver_generated::domain::parser_match::skipBlock(
                                    &tokens, nextPos,
                                ),
                                deps,
                                fns,
                                stmts,
                            )
                        } else {
                            if &*__dispatch_subject == "record" {
                                __MutualTco3::ParseProgram(
                                    tokens.clone(),
                                    crate::aver_generated::domain::parser_match::skipBlock(
                                        &tokens, nextPos,
                                    ),
                                    deps,
                                    fns,
                                    stmts,
                                )
                            } else {
                                if &*__dispatch_subject == "verify" {
                                    __MutualTco3::ParseProgram(
                                        tokens.clone(),
                                        crate::aver_generated::domain::parser_match::skipBlock(
                                            &tokens, nextPos,
                                        ),
                                        deps,
                                        fns,
                                        stmts,
                                    )
                                } else {
                                    if &*__dispatch_subject == "decision" {
                                        __MutualTco3::ParseProgram(
                                            tokens.clone(),
                                            crate::aver_generated::domain::parser_match::skipBlock(
                                                &tokens, nextPos,
                                            ),
                                            deps,
                                            fns,
                                            stmts,
                                        )
                                    } else {
                                        __MutualTco3::ParseProgramStmt(
                                            tokens, pos, deps, fns, stmts,
                                        )
                                    }
                                }
                            }
                        }
                    }
                }
            }
            __MutualTco3::ParseProgramModuleHeader(mut tokens, mut pos, mut fns, mut stmts) => {
                crate::cancel_checkpoint();
                let r =
                    crate::aver_generated::domain::parser_match::parseModuleHeader(&tokens, pos);
                {
                    let (depList, endPos) = r;
                    __MutualTco3::ParseProgram(tokens, endPos, depList, fns, stmts)
                }
            }
            __MutualTco3::ParseProgramFn(mut tokens, mut pos, mut deps, mut fns, mut stmts) => {
                crate::cancel_checkpoint();
                let r = crate::aver_generated::domain::parser::parseFnDef(&tokens, pos)?;
                {
                    let (fd, pos2) = r;
                    __MutualTco3::ParseProgram(
                        tokens.clone(),
                        crate::aver_generated::domain::parser_match::skipNewlines(tokens, pos2),
                        deps,
                        aver_rt::AverList::prepend(fd, &fns),
                        stmts,
                    )
                }
            }
            __MutualTco3::ParseProgramStmt(mut tokens, mut pos, mut deps, mut fns, mut stmts) => {
                crate::cancel_checkpoint();
                let r = crate::aver_generated::domain::parser::parseStmt(&tokens, pos)?;
                {
                    let (st, pos2) = r;
                    __MutualTco3::ParseProgram(
                        tokens.clone(),
                        crate::aver_generated::domain::parser_match::skipNewlines(tokens, pos2),
                        deps,
                        fns,
                        aver_rt::AverList::prepend(st, &stmts),
                    )
                }
            }
        };
    }
}

/// Parse top-level items until EOF.
pub fn parseProgram(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    deps: &aver_rt::AverList<AverStr>,
    fns: &aver_rt::AverList<FnDef>,
    stmts: &aver_rt::AverList<Stmt>,
) -> Result<Program, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseProgram(
        tokens.clone(),
        pos,
        deps.clone(),
        fns.clone(),
        stmts.clone(),
    ))
}

/// Handle keywords: module (extract depends), type/record/verify/decision (skip).
pub fn parseProgramKeyword(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    deps: &aver_rt::AverList<AverStr>,
    fns: &aver_rt::AverList<FnDef>,
    stmts: &aver_rt::AverList<Stmt>,
    kw: AverStr,
) -> Result<Program, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseProgramKeyword(
        tokens.clone(),
        pos,
        deps.clone(),
        fns.clone(),
        stmts.clone(),
        kw,
    ))
}

/// Parse module header and continue with program.
pub fn parseProgramModuleHeader(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    fns: &aver_rt::AverList<FnDef>,
    stmts: &aver_rt::AverList<Stmt>,
) -> Result<Program, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseProgramModuleHeader(
        tokens.clone(),
        pos,
        fns.clone(),
        stmts.clone(),
    ))
}

/// Parse a function definition and continue.
pub fn parseProgramFn(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    deps: &aver_rt::AverList<AverStr>,
    fns: &aver_rt::AverList<FnDef>,
    stmts: &aver_rt::AverList<Stmt>,
) -> Result<Program, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseProgramFn(
        tokens.clone(),
        pos,
        deps.clone(),
        fns.clone(),
        stmts.clone(),
    ))
}

/// Parse a statement and continue.
pub fn parseProgramStmt(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    deps: &aver_rt::AverList<AverStr>,
    fns: &aver_rt::AverList<FnDef>,
    stmts: &aver_rt::AverList<Stmt>,
) -> Result<Program, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseProgramStmt(
        tokens.clone(),
        pos,
        deps.clone(),
        fns.clone(),
        stmts.clone(),
    ))
}

/// Parse a statement: binding (name = expr) or expression.
pub fn parseStmt(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
) -> Result<(Stmt, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let t = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone());
    match t {
        crate::aver_generated::domain::token::Token::TkIdent(name) => {
            crate::aver_generated::domain::parser::parseStmtAfterIdent(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
                name,
            )
        }
        _ => crate::aver_generated::domain::parser::parseStmtExpr(tokens, pos),
    }
}

/// After ident: if '=' or ':' follows it's a binding, otherwise reparse as expr.
pub fn parseStmtAfterIdent(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
) -> Result<(Stmt, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkEq => {
            crate::aver_generated::domain::parser::parseBinding(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
                name,
            )
        }
        crate::aver_generated::domain::token::Token::TkColon => {
            crate::aver_generated::domain::parser::parseBinding(
                tokens,
                crate::aver_generated::domain::parser::skipTypeAnnotationAndEq(tokens, pos),
                name,
            )
        }
        _ => crate::aver_generated::domain::parser::parseStmtExprFrom(tokens, pos, name),
    }
}

/// Skip : Type = and return position after =.
pub fn skipTypeAnnotationAndEq(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::skipTypeAnnotation(tokens, pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkEq => {
            pos2.add(&aver_rt::AverInt::from_i64(1))
        }
        _ => pos2,
    }
}

/// Parse: name = expr. Skips leading NL+INDENT and trailing DEDENT for multi-line exprs.
pub fn parseBinding(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
) -> Result<(Stmt, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r = crate::aver_generated::domain::parser::expr::parseExpr(tokens, pos.clone())?;
    {
        let (expr, pos2) = r;
        Ok((
            crate::aver_generated::domain::ast::Stmt::StmtBind(name, expr),
            crate::aver_generated::domain::parser::skipTrailingDedent(tokens, pos2, pos),
        ))
    }
}

/// If expression consumed an INDENT (multi-line), skip the trailing DEDENT.
pub fn skipTrailingDedent(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    startPos: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    {
        let __int_match_subject = crate::aver_generated::domain::parser::countIndentsInRange(
            tokens.clone(),
            startPos,
            pos.clone(),
            aver_rt::AverInt::from_i64(0),
        );
        if __int_match_subject == aver_rt::AverInt::from_i64(0) {
            pos
        } else {
            let n = __int_match_subject.clone();
            crate::aver_generated::domain::parser::skipNDedents(tokens.clone(), pos, n)
        }
    }
}

/// Count net indent/dedent balance in a token range.
#[inline(always)]
pub fn countIndentsInRange(
    mut tokens: aver_rt::AverList<Token>,
    mut pos: aver_rt::AverInt,
    mut endPos: aver_rt::AverInt,
    mut count: aver_rt::AverInt,
) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
        if (pos < endPos) {
            match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                crate::aver_generated::domain::token::Token::TkIndent => {
                    let __tco1 = nextPos;
                    let __tco3 = count.add(&aver_rt::AverInt::from_i64(1));
                    pos = __tco1;
                    count = __tco3;
                    continue;
                }
                crate::aver_generated::domain::token::Token::TkDedent => {
                    let __tco1 = nextPos;
                    let __tco3 = count.sub(&aver_rt::AverInt::from_i64(1));
                    pos = __tco1;
                    count = __tco3;
                    continue;
                }
                _ => {
                    let __tco1 = nextPos;
                    pos = __tco1;
                    continue;
                }
            }
        } else {
            return count;
        }
    }
}

/// Skip n DEDENT tokens (and interleaved newlines) after multi-line expression.
#[inline(always)]
pub fn skipNDedents(
    mut tokens: aver_rt::AverList<Token>,
    mut pos: aver_rt::AverInt,
    mut n: aver_rt::AverInt,
) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
        if (n > aver_rt::AverInt::from_i64(0)) {
            match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos.clone()) {
                crate::aver_generated::domain::token::Token::TkNewline => {
                    let __tco1 = nextPos;
                    pos = __tco1;
                    continue;
                }
                crate::aver_generated::domain::token::Token::TkDedent => {
                    let __tco1 = nextPos;
                    let __tco2 = n.sub(&aver_rt::AverInt::from_i64(1));
                    pos = __tco1;
                    n = __tco2;
                    continue;
                }
                _ => {
                    return pos;
                }
            }
        } else {
            return pos;
        }
    }
}

/// Parse an expression statement.
pub fn parseStmtExpr(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
) -> Result<(Stmt, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r = crate::aver_generated::domain::parser::expr::parseExpr(tokens, pos.clone())?;
    {
        let (expr, pos2) = r;
        Ok((
            crate::aver_generated::domain::ast::Stmt::StmtExpr(expr),
            crate::aver_generated::domain::parser::skipTrailingDedent(tokens, pos2, pos),
        ))
    }
}

/// Reparse as expression starting from an identifier (might be call or var in expr).
pub fn parseStmtExprFrom(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
) -> Result<(Stmt, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let ir = crate::aver_generated::domain::parser::expr::parseIdentOrCall(tokens, pos, name)?;
    {
        let (expr, pos2) = ir;
        crate::aver_generated::domain::parser::parseStmtExprFromMul(tokens, pos2, &expr)
    }
}

/// Continue parsing expression statement from multiplicative level.
pub fn parseStmtExprFromMul(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    expr: &Expr,
) -> Result<(Stmt, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let mr = crate::aver_generated::domain::parser::expr::parseMulExprTail(tokens, pos, expr)?;
    {
        let (expr2, pos2) = mr;
        crate::aver_generated::domain::parser::parseStmtExprFromAdd(tokens, pos2, &expr2)
    }
}

/// Continue parsing expression statement from additive level.
pub fn parseStmtExprFromAdd(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    expr: &Expr,
) -> Result<(Stmt, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let ar = crate::aver_generated::domain::parser::expr::parseAddExprTail(tokens, pos, expr)?;
    {
        let (expr2, pos2) = ar;
        crate::aver_generated::domain::parser::parseStmtExprFromCmp(tokens, pos2, &expr2)
    }
}

/// Continue parsing expression statement from comparison level.
pub fn parseStmtExprFromCmp(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    expr: &Expr,
) -> Result<(Stmt, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let cr = crate::aver_generated::domain::parser::expr::parseCmpExprTail(tokens, pos, expr)?;
    {
        let (expr2, pos2) = cr;
        Ok(crate::aver_generated::domain::parser::parseStmtExprFromQ(
            tokens, pos2, &expr2,
        ))
    }
}

/// Check for postfix ? on expression statement.
pub fn parseStmtExprFromQ(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    expr: &Expr,
) -> (Stmt, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    let qr = crate::aver_generated::domain::parser::expr::parsePostfixQuestion(tokens, pos, expr);
    {
        let (expr2, pos2) = qr;
        (
            crate::aver_generated::domain::ast::Stmt::StmtExpr(expr2),
            pos2,
        )
    }
}

/// Parse: fn NAME(params) NEWLINE body
pub fn parseFnDef(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
) -> Result<(FnDef, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let t = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone());
    match t {
        crate::aver_generated::domain::token::Token::TkIdent(name) => {
            crate::aver_generated::domain::parser::parseFnDefParams(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
                name,
            )
        }
        _ => Err(AverStr::from("Expected function name after 'fn'")),
    }
}

/// Parse parameter list and body.
pub fn parseFnDefParams(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
) -> Result<(FnDef, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::expect(
        tokens,
        pos,
        &crate::aver_generated::domain::token::Token::TkLParen,
    )?;
    let pr = crate::aver_generated::domain::parser_match::parseParamList(
        tokens,
        pos2,
        &aver_rt::AverList::empty(),
    )?;
    {
        let (params, pos3) = pr;
        crate::aver_generated::domain::parser::parseFnDefBody(tokens, pos3, name, &params)
    }
}

/// Parse function body: skip return type, newline, INDENT, ?, !, then statements until DEDENT.
pub fn parseFnDefBody(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    params: &aver_rt::AverList<AverStr>,
) -> Result<(FnDef, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::skipReturnType(tokens, pos);
    let pos3 = crate::aver_generated::domain::parser_match::skipNewlines(tokens.clone(), pos2);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos3.clone()) {
        crate::aver_generated::domain::token::Token::TkIndent => {
            crate::aver_generated::domain::parser::parseFnDefBodyIndented(
                tokens,
                pos3.add(&aver_rt::AverInt::from_i64(1)),
                name,
                params,
            )
        }
        _ => crate::aver_generated::domain::parser::parseFnDefBodyFlat(tokens, pos3, name, params),
    }
}

/// Parse function body with INDENT/DEDENT.
pub fn parseFnDefBodyIndented(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    params: &aver_rt::AverList<AverStr>,
) -> Result<(FnDef, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::skipDescAndEffects(tokens.clone(), pos);
    let sr = crate::aver_generated::domain::parser::parseFnBodyStmtsIndented(
        tokens,
        pos2,
        &aver_rt::AverList::empty(),
    )?;
    {
        let (stmts, pos3) = sr;
        Ok((
            crate::aver_generated::domain::ast::FnDef {
                name: name,
                params: params.clone(),
                body: stmts.reverse(),
                slotCount: aver_rt::AverInt::from_i64(0),
                slotMap: HashMap::new(),
                fastPath: crate::aver_generated::domain::ast::FnFastPath::FastNone,
                tailLoop: false,
            },
            pos3,
        ))
    }
}

/// Parse function body without INDENT/DEDENT (fallback).
pub fn parseFnDefBodyFlat(
    tokens: &aver_rt::AverList<Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    params: &aver_rt::AverList<AverStr>,
) -> Result<(FnDef, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::skipDescAndEffects(tokens.clone(), pos);
    let sr = crate::aver_generated::domain::parser::parseFnBodyStmtsFlat(
        tokens,
        pos2,
        &aver_rt::AverList::empty(),
    )?;
    {
        let (stmts, pos3) = sr;
        Ok((
            crate::aver_generated::domain::ast::FnDef {
                name: name,
                params: params.clone(),
                body: stmts.reverse(),
                slotCount: aver_rt::AverInt::from_i64(0),
                slotMap: HashMap::new(),
                fastPath: crate::aver_generated::domain::ast::FnFastPath::FastNone,
                tailLoop: false,
            },
            pos3,
        ))
    }
}

/// Parse a token list into a Program.
#[inline(always)]
pub fn parse(tokens: &aver_rt::AverList<Token>) -> Result<Program, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::parser::parseProgram(
        tokens,
        aver_rt::AverInt::from_i64(0),
        &aver_rt::AverList::empty(),
        &aver_rt::AverList::empty(),
        &aver_rt::AverList::empty(),
    )
}

pub mod expr;
