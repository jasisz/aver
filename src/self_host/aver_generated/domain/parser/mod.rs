#[allow(unused_imports)]
use crate::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::lexer::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::parser_match::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::parser::expr::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    ParseFnBodyStmtsFlat(aver_rt::AverList<Token>, i64, aver_rt::AverList<Stmt>),
    ParseFnBodyOneStmtFlat(aver_rt::AverList<Token>, i64, aver_rt::AverList<Stmt>),
    ParseFnBodyAfterStmtFlat(aver_rt::AverList<Token>, i64, aver_rt::AverList<Stmt>),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> Result<(aver_rt::AverList<Stmt>, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::ParseFnBodyStmtsFlat(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::parser_match::isBodyEndFlat(&tokens, pos) { return Ok((acc, pos)) } else { __MutualTco1::ParseFnBodyOneStmtFlat(tokens, pos, acc) }
            }
            __MutualTco1::ParseFnBodyOneStmtFlat(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let r = parseStmt(&tokens, pos)?;
                match r {
                (stmt, pos2) => __MutualTco1::ParseFnBodyAfterStmtFlat(tokens, pos2, aver_rt::AverList::prepend(stmt, &acc))
                }
            }
            __MutualTco1::ParseFnBodyAfterStmtFlat(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkNewline => __MutualTco1::ParseFnBodyStmtsFlat(tokens, (pos + 1i64), acc),
                _ => return Ok((acc, pos))
                }
            }
        };
    }
}

/// Parse statements in function body (flat mode).
pub fn parseFnBodyStmtsFlat(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Stmt>) -> Result<(aver_rt::AverList<Stmt>, i64), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseFnBodyStmtsFlat(tokens.clone(), pos, acc.clone()))
}

/// Parse one statement in function body and continue (flat mode).
pub fn parseFnBodyOneStmtFlat(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Stmt>) -> Result<(aver_rt::AverList<Stmt>, i64), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseFnBodyOneStmtFlat(tokens.clone(), pos, acc.clone()))
}

/// After a body statement: single newline continues, anything else ends (flat mode).
pub fn parseFnBodyAfterStmtFlat(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Stmt>) -> Result<(aver_rt::AverList<Stmt>, i64), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseFnBodyAfterStmtFlat(tokens.clone(), pos, acc.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    ParseFnBodyStmtsIndented(aver_rt::AverList<Token>, i64, aver_rt::AverList<Stmt>),
    ParseFnBodyOneStmtIndented(aver_rt::AverList<Token>, i64, aver_rt::AverList<Stmt>),
}

fn __mutual_tco_trampoline_2(mut __state: __MutualTco2) -> Result<(aver_rt::AverList<Stmt>, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::ParseFnBodyStmtsIndented(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let pos2 = crate::aver_generated::domain::parser_match::skipNewlines(tokens.clone(), pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos2.clone()) {
                Token::TkDedent => return Ok((acc, (pos2 + 1i64))),
                Token::TkEof => return Ok((acc, pos2)),
                _ => __MutualTco2::ParseFnBodyOneStmtIndented(tokens, pos2, acc)
                }
            }
            __MutualTco2::ParseFnBodyOneStmtIndented(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let r = parseStmt(&tokens, pos)?;
                match r {
                (stmt, pos2) => __MutualTco2::ParseFnBodyStmtsIndented(tokens, pos2, aver_rt::AverList::prepend(stmt, &acc))
                }
            }
        };
    }
}

/// Parse statements until DEDENT or EOF.
pub fn parseFnBodyStmtsIndented(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Stmt>) -> Result<(aver_rt::AverList<Stmt>, i64), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseFnBodyStmtsIndented(tokens.clone(), pos, acc.clone()))
}

/// Parse one statement and continue with indented body.
pub fn parseFnBodyOneStmtIndented(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Stmt>) -> Result<(aver_rt::AverList<Stmt>, i64), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseFnBodyOneStmtIndented(tokens.clone(), pos, acc.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    ParseProgram(aver_rt::AverList<Token>, i64, aver_rt::AverList<AverStr>, aver_rt::AverList<FnDef>, aver_rt::AverList<Stmt>),
    ParseProgramKeyword(aver_rt::AverList<Token>, i64, aver_rt::AverList<AverStr>, aver_rt::AverList<FnDef>, aver_rt::AverList<Stmt>, AverStr),
    ParseProgramModuleHeader(aver_rt::AverList<Token>, i64, aver_rt::AverList<FnDef>, aver_rt::AverList<Stmt>),
    ParseProgramFn(aver_rt::AverList<Token>, i64, aver_rt::AverList<AverStr>, aver_rt::AverList<FnDef>, aver_rt::AverList<Stmt>),
    ParseProgramStmt(aver_rt::AverList<Token>, i64, aver_rt::AverList<AverStr>, aver_rt::AverList<FnDef>, aver_rt::AverList<Stmt>),
}

fn __mutual_tco_trampoline_3(mut __state: __MutualTco3) -> Result<Program, AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::ParseProgram(mut tokens, mut pos, mut deps, mut fns, mut stmts) => {
                crate::cancel_checkpoint();
                let pos2 = crate::aver_generated::domain::parser_match::skipNewlinesAndDedents(tokens.clone(), pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos2.clone()) {
                Token::TkEof => return Ok(Program { deps: deps, fns: fns.reverse(), stmts: stmts.reverse() }),
                Token::TkFn => __MutualTco3::ParseProgramFn(tokens, (pos2 + 1i64), deps, fns, stmts),
                Token::TkIdent(kw) => __MutualTco3::ParseProgramKeyword(tokens, pos2, deps, fns, stmts, kw),
                _ => __MutualTco3::ParseProgramStmt(tokens, pos2, deps, fns, stmts)
                }
            }
            __MutualTco3::ParseProgramKeyword(mut tokens, mut pos, mut deps, mut fns, mut stmts, mut kw) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                { let __dispatch_subject = kw; if &*__dispatch_subject == "module" { __MutualTco3::ParseProgramModuleHeader(tokens, nextPos, fns, stmts) } else { if &*__dispatch_subject == "type" { __MutualTco3::ParseProgram(tokens.clone(), crate::aver_generated::domain::parser_match::skipBlock(&tokens, nextPos), deps, fns, stmts) } else { if &*__dispatch_subject == "record" { __MutualTco3::ParseProgram(tokens.clone(), crate::aver_generated::domain::parser_match::skipBlock(&tokens, nextPos), deps, fns, stmts) } else { if &*__dispatch_subject == "verify" { __MutualTco3::ParseProgram(tokens.clone(), crate::aver_generated::domain::parser_match::skipBlock(&tokens, nextPos), deps, fns, stmts) } else { if &*__dispatch_subject == "decision" { __MutualTco3::ParseProgram(tokens.clone(), crate::aver_generated::domain::parser_match::skipBlock(&tokens, nextPos), deps, fns, stmts) } else { __MutualTco3::ParseProgramStmt(tokens, pos, deps, fns, stmts) } } } } } }
            }
            __MutualTco3::ParseProgramModuleHeader(mut tokens, mut pos, mut fns, mut stmts) => {
                crate::cancel_checkpoint();
                let r = crate::aver_generated::domain::parser_match::parseModuleHeader(&tokens, pos);
                match r {
                (depList, endPos) => __MutualTco3::ParseProgram(tokens, endPos, depList, fns, stmts)
                }
            }
            __MutualTco3::ParseProgramFn(mut tokens, mut pos, mut deps, mut fns, mut stmts) => {
                crate::cancel_checkpoint();
                let r = parseFnDef(&tokens, pos)?;
                match r {
                (fd, pos2) => __MutualTco3::ParseProgram(tokens.clone(), crate::aver_generated::domain::parser_match::skipNewlines(tokens, pos2), deps, aver_rt::AverList::prepend(fd, &fns), stmts)
                }
            }
            __MutualTco3::ParseProgramStmt(mut tokens, mut pos, mut deps, mut fns, mut stmts) => {
                crate::cancel_checkpoint();
                let r = parseStmt(&tokens, pos)?;
                match r {
                (st, pos2) => __MutualTco3::ParseProgram(tokens.clone(), crate::aver_generated::domain::parser_match::skipNewlines(tokens, pos2), deps, fns, aver_rt::AverList::prepend(st, &stmts))
                }
            }
        };
    }
}

/// Parse top-level items until EOF.
pub fn parseProgram(tokens: &aver_rt::AverList<Token>, pos: i64, deps: &aver_rt::AverList<AverStr>, fns: &aver_rt::AverList<FnDef>, stmts: &aver_rt::AverList<Stmt>) -> Result<Program, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseProgram(tokens.clone(), pos, deps.clone(), fns.clone(), stmts.clone()))
}

/// Handle keywords: module (extract depends), type/record/verify/decision (skip).
pub fn parseProgramKeyword(tokens: &aver_rt::AverList<Token>, pos: i64, deps: &aver_rt::AverList<AverStr>, fns: &aver_rt::AverList<FnDef>, stmts: &aver_rt::AverList<Stmt>, kw: AverStr) -> Result<Program, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseProgramKeyword(tokens.clone(), pos, deps.clone(), fns.clone(), stmts.clone(), kw))
}

/// Parse module header and continue with program.
pub fn parseProgramModuleHeader(tokens: &aver_rt::AverList<Token>, pos: i64, fns: &aver_rt::AverList<FnDef>, stmts: &aver_rt::AverList<Stmt>) -> Result<Program, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseProgramModuleHeader(tokens.clone(), pos, fns.clone(), stmts.clone()))
}

/// Parse a function definition and continue.
pub fn parseProgramFn(tokens: &aver_rt::AverList<Token>, pos: i64, deps: &aver_rt::AverList<AverStr>, fns: &aver_rt::AverList<FnDef>, stmts: &aver_rt::AverList<Stmt>) -> Result<Program, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseProgramFn(tokens.clone(), pos, deps.clone(), fns.clone(), stmts.clone()))
}

/// Parse a statement and continue.
pub fn parseProgramStmt(tokens: &aver_rt::AverList<Token>, pos: i64, deps: &aver_rt::AverList<AverStr>, fns: &aver_rt::AverList<FnDef>, stmts: &aver_rt::AverList<Stmt>) -> Result<Program, AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseProgramStmt(tokens.clone(), pos, deps.clone(), fns.clone(), stmts.clone()))
}

/// Parse a statement: binding (name = expr) or expression.
pub fn parseStmt(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Stmt, i64), AverStr> {
    crate::cancel_checkpoint();
    let t = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
    match t {
        Token::TkIdent(name) => {
            parseStmtAfterIdent(tokens, (pos + 1i64), name)
        },
        _ => {
            parseStmtExpr(tokens, pos)
        }
    }
}

/// After ident: if '=' or ':' follows it's a binding, otherwise reparse as expr.
pub fn parseStmtAfterIdent(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr) -> Result<(Stmt, i64), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        Token::TkEq => {
            parseBinding(tokens, (pos + 1i64), name)
        },
        Token::TkColon => {
            parseBinding(tokens, skipTypeAnnotationAndEq(tokens, pos), name)
        },
        _ => {
            parseStmtExprFrom(tokens, pos, name)
        }
    }
}

/// Skip : Type = and return position after =.
pub fn skipTypeAnnotationAndEq(tokens: &aver_rt::AverList<Token>, pos: i64) -> i64 {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::skipTypeAnnotation(tokens, pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        Token::TkEq => {
            (pos2 + 1i64)
        },
        _ => {
            pos2
        }
    }
}

/// Parse: name = expr. Skips leading NL+INDENT and trailing DEDENT for multi-line exprs.
pub fn parseBinding(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr) -> Result<(Stmt, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = crate::aver_generated::domain::parser::expr::parseExpr(tokens, pos)?;
    { let (expr, pos2) = r; Ok((Stmt::StmtBind(name, expr), skipTrailingDedent(tokens, pos2, pos))) }
}

/// If expression consumed an INDENT (multi-line), skip the trailing DEDENT.
pub fn skipTrailingDedent(tokens: &aver_rt::AverList<Token>, pos: i64, startPos: i64) -> i64 {
    crate::cancel_checkpoint();
    match countIndentsInRange(tokens.clone(), startPos, pos, 0i64) {
        0i64 => {
            pos
        },
        n => {
            skipNDedents(tokens.clone(), pos, n)
        }
    }
}

/// Count net indent/dedent balance in a token range.
#[inline(always)]
pub fn countIndentsInRange(mut tokens: aver_rt::AverList<Token>, mut pos: i64, mut endPos: i64, mut count: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return if (pos >= endPos) { count } else { match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
            Token::TkIndent => {
            let __tmp3 = (count + 1i64);
            pos = nextPos;
            count = __tmp3;
            continue;
        },
            Token::TkDedent => {
            let __tmp3 = (count - 1i64);
            pos = nextPos;
            count = __tmp3;
            continue;
        },
            _ => {
            pos = nextPos;
            continue;
        }
        } };
    }
}

/// Skip n DEDENT tokens (and interleaved newlines) after multi-line expression.
#[inline(always)]
pub fn skipNDedents(mut tokens: aver_rt::AverList<Token>, mut pos: i64, mut n: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return if (n <= 0i64) { pos } else { match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
            Token::TkNewline => {
            pos = nextPos;
            continue;
        },
            Token::TkDedent => {
            let __tmp1 = nextPos;
            let __tmp2 = (n - 1i64);
            pos = __tmp1;
            n = __tmp2;
            continue;
        },
            _ => pos
        } };
    }
}

/// Parse an expression statement.
pub fn parseStmtExpr(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Stmt, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = crate::aver_generated::domain::parser::expr::parseExpr(tokens, pos)?;
    { let (expr, pos2) = r; Ok((Stmt::StmtExpr(expr), skipTrailingDedent(tokens, pos2, pos))) }
}

/// Reparse as expression starting from an identifier (might be call or var in expr).
pub fn parseStmtExprFrom(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr) -> Result<(Stmt, i64), AverStr> {
    crate::cancel_checkpoint();
    let ir = crate::aver_generated::domain::parser::expr::parseIdentOrCall(tokens, pos, name)?;
    { let (expr, pos2) = ir; parseStmtExprFromMul(tokens, pos2, &expr) }
}

/// Continue parsing expression statement from multiplicative level.
pub fn parseStmtExprFromMul(tokens: &aver_rt::AverList<Token>, pos: i64, expr: &Expr) -> Result<(Stmt, i64), AverStr> {
    crate::cancel_checkpoint();
    let mr = crate::aver_generated::domain::parser::expr::parseMulExprTail(tokens, pos, expr)?;
    { let (expr2, pos2) = mr; parseStmtExprFromAdd(tokens, pos2, &expr2) }
}

/// Continue parsing expression statement from additive level.
pub fn parseStmtExprFromAdd(tokens: &aver_rt::AverList<Token>, pos: i64, expr: &Expr) -> Result<(Stmt, i64), AverStr> {
    crate::cancel_checkpoint();
    let ar = crate::aver_generated::domain::parser::expr::parseAddExprTail(tokens, pos, expr)?;
    { let (expr2, pos2) = ar; parseStmtExprFromCmp(tokens, pos2, &expr2) }
}

/// Continue parsing expression statement from comparison level.
pub fn parseStmtExprFromCmp(tokens: &aver_rt::AverList<Token>, pos: i64, expr: &Expr) -> Result<(Stmt, i64), AverStr> {
    crate::cancel_checkpoint();
    let cr = crate::aver_generated::domain::parser::expr::parseCmpExprTail(tokens, pos, expr)?;
    { let (expr2, pos2) = cr; Ok(parseStmtExprFromQ(tokens, pos2, &expr2)) }
}

/// Check for postfix ? on expression statement.
pub fn parseStmtExprFromQ(tokens: &aver_rt::AverList<Token>, pos: i64, expr: &Expr) -> (Stmt, i64) {
    crate::cancel_checkpoint();
    let qr = crate::aver_generated::domain::parser::expr::parsePostfixQuestion(tokens, pos, expr);
    { let (expr2, pos2) = qr; (Stmt::StmtExpr(expr2), pos2) }
}

/// Parse: fn NAME(params) NEWLINE body
pub fn parseFnDef(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(FnDef, i64), AverStr> {
    crate::cancel_checkpoint();
    let t = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
    match t {
        Token::TkIdent(name) => {
            parseFnDefParams(tokens, (pos + 1i64), name)
        },
        _ => {
            Err(AverStr::from("Expected function name after 'fn'"))
        }
    }
}

/// Parse parameter list and body.
pub fn parseFnDefParams(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr) -> Result<(FnDef, i64), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::expect(tokens, pos, &Token::TkLParen)?;
    let pr = crate::aver_generated::domain::parser_match::parseParamList(tokens, pos2, &aver_rt::AverList::empty())?;
    { let (params, pos3) = pr; parseFnDefBody(tokens, pos3, name, &params) }
}

/// Parse function body: skip return type, newline, INDENT, ?, !, then statements until DEDENT.
pub fn parseFnDefBody(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, params: &aver_rt::AverList<AverStr>) -> Result<(FnDef, i64), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::skipReturnType(tokens, pos);
    let pos3 = crate::aver_generated::domain::parser_match::skipNewlines(tokens.clone(), pos2);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos3.clone()) {
        Token::TkIndent => {
            parseFnDefBodyIndented(tokens, (pos3 + 1i64), name, params)
        },
        _ => {
            parseFnDefBodyFlat(tokens, pos3, name, params)
        }
    }
}

/// Parse function body with INDENT/DEDENT.
pub fn parseFnDefBodyIndented(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, params: &aver_rt::AverList<AverStr>) -> Result<(FnDef, i64), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::skipDescAndEffects(tokens.clone(), pos);
    let sr = parseFnBodyStmtsIndented(tokens, pos2, &aver_rt::AverList::empty())?;
    { let (stmts, pos3) = sr; Ok((FnDef { name: name, params: params.clone(), body: stmts.reverse(), slotCount: 0i64, slotMap: HashMap::new(), fastPath: FnFastPath::FastNone.clone(), tailLoop: false }, pos3)) }
}

/// Parse function body without INDENT/DEDENT (fallback).
pub fn parseFnDefBodyFlat(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, params: &aver_rt::AverList<AverStr>) -> Result<(FnDef, i64), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::skipDescAndEffects(tokens.clone(), pos);
    let sr = parseFnBodyStmtsFlat(tokens, pos2, &aver_rt::AverList::empty())?;
    { let (stmts, pos3) = sr; Ok((FnDef { name: name, params: params.clone(), body: stmts.reverse(), slotCount: 0i64, slotMap: HashMap::new(), fastPath: FnFastPath::FastNone.clone(), tailLoop: false }, pos3)) }
}

/// Parse a token list into a Program.
#[inline(always)]
pub fn parse(tokens: &aver_rt::AverList<Token>) -> Result<Program, AverStr> {
    crate::cancel_checkpoint();
    parseProgram(tokens, 0i64, &aver_rt::AverList::empty(), &aver_rt::AverList::empty(), &aver_rt::AverList::empty())
}

pub mod expr;
