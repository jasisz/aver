#[allow(unused_imports)]
use crate::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::parser_match::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    ParseAddExprTail(aver_rt::AverList<Token>, i64, Expr),
    ParseAddExprRight(aver_rt::AverList<Token>, i64, Expr, i64),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::ParseAddExprTail(mut tokens, mut pos, mut left) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                let t = crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos);
                match t {
                Token::TkPlus => __MutualTco1::ParseAddExprRight(tokens, nextPos, left, 0i64),
                Token::TkMinus => __MutualTco1::ParseAddExprRight(tokens, nextPos, left, 1i64),
                _ => return Ok((left, pos))
                }
            }
            __MutualTco1::ParseAddExprRight(mut tokens, mut pos, mut left, mut op) => {
                crate::cancel_checkpoint();
                let r = parseMulExpr(&tokens, pos)?;
                match r {
                (right, pos2) => if (op == 0i64) { __MutualTco1::ParseAddExprTail(tokens, pos2, Expr::ExprAdd(std::sync::Arc::new(left), std::sync::Arc::new(right))) } else { __MutualTco1::ParseAddExprTail(tokens, pos2, Expr::ExprSub(std::sync::Arc::new(left), std::sync::Arc::new(right))) }
                }
            }
        };
    }
}

/// Continue parsing additive operators.
pub fn parseAddExprTail(tokens: &aver_rt::AverList<Token>, pos: i64, left: &Expr) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseAddExprTail(tokens.clone(), pos, left.clone()))
}

/// Parse right side of additive op.
pub fn parseAddExprRight(tokens: &aver_rt::AverList<Token>, pos: i64, left: &Expr, op: i64) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseAddExprRight(tokens.clone(), pos, left.clone(), op))
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    ParseCallArgsList(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<Expr>),
    ParseCallArgsListTail(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<Expr>),
    ParseCallArgsAfterComma(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<Expr>),
    ParseCallArgsCheckNamed(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<Expr>, AverStr),
}

fn __mutual_tco_trampoline_2(mut __state: __MutualTco2) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::ParseCallArgsList(mut tokens, mut pos, mut name, mut acc) => {
                crate::cancel_checkpoint();
                let r = parseExpr(&tokens, pos)?;
                match r {
                (expr, pos2) => __MutualTco2::ParseCallArgsListTail(tokens, pos2, name, aver_rt::AverList::prepend(expr, &acc))
                }
            }
            __MutualTco2::ParseCallArgsListTail(mut tokens, mut pos0, mut name, mut args) => {
                crate::cancel_checkpoint();
                let pos = skipNl(tokens.clone(), pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos.clone()) {
                Token::TkComma => __MutualTco2::ParseCallArgsAfterComma(tokens.clone(), skipNl(tokens, (pos + 1i64)), name, args),
                Token::TkRParen => return Ok((Expr::ExprCall(name, args.reverse()), (pos + 1i64))),
                _ => return Err(AverStr::from("Expected ',' or ')' in argument list"))
                }
            }
            __MutualTco2::ParseCallArgsAfterComma(mut tokens, mut pos, mut name, mut args) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkRParen => return Ok((Expr::ExprCall(name, args.reverse()), (pos + 1i64))),
                Token::TkIdent(field) => __MutualTco2::ParseCallArgsCheckNamed(tokens, pos, name, args, field),
                _ => __MutualTco2::ParseCallArgsList(tokens, pos, name, args)
                }
            }
            __MutualTco2::ParseCallArgsCheckNamed(mut tokens, mut pos, mut name, mut args, mut field) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, (pos + 1i64)) {
                Token::TkEq => return parseNamedArgs(&tokens, pos, name, &args, &aver_rt::AverList::empty()),
                _ => __MutualTco2::ParseCallArgsList(tokens, pos, name, args)
                }
            }
        };
    }
}

/// Parse comma-separated argument list.
pub fn parseCallArgsList(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, acc: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseCallArgsList(tokens.clone(), pos, name, acc.clone()))
}

/// After an argument: ',' for more or ')' to end. Detects named args (field = expr).
pub fn parseCallArgsListTail(tokens: &aver_rt::AverList<Token>, pos0: i64, name: AverStr, args: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseCallArgsListTail(tokens.clone(), pos0, name, args.clone()))
}

/// After comma in call args: check if next is ident = (named arg) or regular expr.
pub fn parseCallArgsAfterComma(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, args: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseCallArgsAfterComma(tokens.clone(), pos, name, args.clone()))
}

/// Lookahead: if next token after ident is =, switch to named args mode.
pub fn parseCallArgsCheckNamed(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, args: &aver_rt::AverList<Expr>, field: AverStr) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseCallArgsCheckNamed(tokens.clone(), pos, name, args.clone(), field))
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    ParseFieldAccess(aver_rt::AverList<Token>, i64, Expr),
    ParseFieldAccessTail(aver_rt::AverList<Token>, i64, Expr),
}

fn __mutual_tco_trampoline_3(mut __state: __MutualTco3) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::ParseFieldAccess(mut tokens, mut pos, mut obj) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkIdent(field) => __MutualTco3::ParseFieldAccessTail(tokens, (pos + 1i64), Expr::ExprFieldAccess(std::sync::Arc::new(obj), field)),
                _ => return Err(AverStr::from("Expected field name after '.'"))
                }
            }
            __MutualTco3::ParseFieldAccessTail(mut tokens, mut pos, mut expr) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkDot => __MutualTco3::ParseFieldAccess(tokens, (pos + 1i64), expr),
                _ => return Ok((expr, pos))
                }
            }
        };
    }
}

/// Parse .field after an expression.
pub fn parseFieldAccess(tokens: &aver_rt::AverList<Token>, pos: i64, obj: &Expr) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseFieldAccess(tokens.clone(), pos, obj.clone()))
}

/// Check for chained field access.
pub fn parseFieldAccessTail(tokens: &aver_rt::AverList<Token>, pos: i64, expr: &Expr) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseFieldAccessTail(tokens.clone(), pos, expr.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco4 {
    ParseInterpParts(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
    ParseInterpAfterExpr(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
    ParseInterpContinue(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
    ParseInterpAfterStr(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
}

fn __mutual_tco_trampoline_4(mut __state: __MutualTco4) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco4::ParseInterpParts(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let r = parseExpr(&tokens, pos)?;
                match r {
                (expr, pos2) => __MutualTco4::ParseInterpAfterExpr(tokens, pos2, aver_rt::AverList::prepend(expr, &acc))
                }
            }
            __MutualTco4::ParseInterpAfterExpr(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkInterpEnd => __MutualTco4::ParseInterpContinue(tokens, (pos + 1i64), acc),
                _ => return Err(AverStr::from("Expected } in string interpolation"))
                }
            }
            __MutualTco4::ParseInterpContinue(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkStr(s) => __MutualTco4::ParseInterpAfterStr(tokens, nextPos, aver_rt::AverList::prepend(Expr::ExprStr(s), &acc)),
                Token::TkInterpStart => __MutualTco4::ParseInterpParts(tokens, nextPos, acc),
                _ => return Ok((Expr::ExprConcat(acc.reverse()), pos))
                }
            }
            __MutualTco4::ParseInterpAfterStr(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkInterpStart => __MutualTco4::ParseInterpParts(tokens, (pos + 1i64), acc),
                _ => return Ok((Expr::ExprConcat(acc.reverse()), pos))
                }
            }
        };
    }
}

/// Parse interpolation parts: expressions between TkInterpStart/TkInterpEnd and TkStr segments.
pub fn parseInterpParts(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::ParseInterpParts(tokens.clone(), pos, acc.clone()))
}

/// After interpolation expr: expect TkInterpEnd, then maybe more string.
pub fn parseInterpAfterExpr(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::ParseInterpAfterExpr(tokens.clone(), pos, acc.clone()))
}

/// After }: next TkStr continues, or end of string.
pub fn parseInterpContinue(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::ParseInterpContinue(tokens.clone(), pos, acc.clone()))
}

/// After string segment: more interpolation or end.
pub fn parseInterpAfterStr(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::ParseInterpAfterStr(tokens.clone(), pos, acc.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco5 {
    ParseListItems(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
    ParseListItemsTail(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
    ParseListAfterComma(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
}

fn __mutual_tco_trampoline_5(mut __state: __MutualTco5) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco5::ParseListItems(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let r = parseExpr(&tokens, pos)?;
                match r {
                (expr, pos2) => __MutualTco5::ParseListItemsTail(tokens, pos2, aver_rt::AverList::prepend(expr, &acc))
                }
            }
            __MutualTco5::ParseListItemsTail(mut tokens, mut pos0, mut items) => {
                crate::cancel_checkpoint();
                let pos = skipNl(tokens.clone(), pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos.clone()) {
                Token::TkComma => __MutualTco5::ParseListAfterComma(tokens.clone(), skipNl(tokens, (pos + 1i64)), items),
                Token::TkRBracket => return Ok((Expr::ExprList(items.reverse()), (pos + 1i64))),
                _ => return Err(AverStr::from("Expected ',' or ']' in list literal"))
                }
            }
            __MutualTco5::ParseListAfterComma(mut tokens, mut pos, mut items) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkRBracket => return Ok((Expr::ExprList(items), (pos + 1i64))),
                _ => __MutualTco5::ParseListItems(tokens, pos, items)
                }
            }
        };
    }
}

/// Parse comma-separated list items.
pub fn parseListItems(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::ParseListItems(tokens.clone(), pos, acc.clone()))
}

/// After a list item: ',' for more or ']' to end.
pub fn parseListItemsTail(tokens: &aver_rt::AverList<Token>, pos0: i64, items: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::ParseListItemsTail(tokens.clone(), pos0, items.clone()))
}

/// Allow a trailing comma before ']' in list literals.
pub fn parseListAfterComma(tokens: &aver_rt::AverList<Token>, pos: i64, items: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::ParseListAfterComma(tokens.clone(), pos, items.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco6 {
    ParseMapEntries(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
    ParseMapAfterKey(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>, Expr),
    ParseMapEntryTail(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
}

fn __mutual_tco_trampoline_6(mut __state: __MutualTco6) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco6::ParseMapEntries(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let kr = parseExpr(&tokens, pos)?;
                match kr {
                (keyExpr, pos2) => __MutualTco6::ParseMapAfterKey(tokens, pos2, acc, keyExpr)
                }
            }
            __MutualTco6::ParseMapAfterKey(mut tokens, mut pos, mut acc, mut keyExpr) => {
                crate::cancel_checkpoint();
                let pos2 = crate::aver_generated::domain::parser_match::expect(&tokens, pos, &Token::TkFatArrow)?;
                let vr = parseExpr(&tokens, pos2)?;
                match vr {
                (valExpr, pos3) => __MutualTco6::ParseMapEntryTail(tokens, pos3, aver_rt::AverList::prepend(Expr::ExprTuple(aver_rt::AverList::from_vec(vec![keyExpr, valExpr])), &acc))
                }
            }
            __MutualTco6::ParseMapEntryTail(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let pos2 = skipNl(tokens.clone(), pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos2.clone()) {
                Token::TkComma => __MutualTco6::ParseMapEntries(tokens.clone(), skipNl(tokens, (pos2 + 1i64)), acc),
                Token::TkRBrace => return Ok((Expr::ExprCall(AverStr::from("Map.fromList"), aver_rt::AverList::from_vec(vec![Expr::ExprList(acc.reverse())])), (pos2 + 1i64))),
                _ => return Err(AverStr::from("Expected ',' or '}' in map literal"))
                }
            }
        };
    }
}

/// Parse key => value pairs.
pub fn parseMapEntries(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_6(__MutualTco6::ParseMapEntries(tokens.clone(), pos, acc.clone()))
}

/// After key, expect => then value.
pub fn parseMapAfterKey(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Expr>, keyExpr: &Expr) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_6(__MutualTco6::ParseMapAfterKey(tokens.clone(), pos, acc.clone(), keyExpr.clone()))
}

/// After entry: , for more or } to end.
pub fn parseMapEntryTail(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_6(__MutualTco6::ParseMapEntryTail(tokens.clone(), pos, acc.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco7 {
    ParseMatchArms(aver_rt::AverList<Token>, i64, Expr, aver_rt::AverList<MatchArm>),
    ParseOneArm(aver_rt::AverList<Token>, i64, Expr, aver_rt::AverList<MatchArm>),
    ParseOneArmBody(aver_rt::AverList<Token>, i64, Expr, aver_rt::AverList<MatchArm>, Pattern),
}

fn __mutual_tco_trampoline_7(mut __state: __MutualTco7) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco7::ParseMatchArms(mut tokens, mut pos, mut subject, mut acc) => {
                crate::cancel_checkpoint();
                let pos2 = crate::aver_generated::domain::parser_match::skipNewlines(tokens.clone(), pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos2.clone()) {
                Token::TkDedent => { let __list_subject = acc.clone(); if __list_subject.is_empty() { return Err(AverStr::from("Expected at least one match arm")) } else { return Ok((Expr::ExprMatch(std::sync::Arc::new(subject), acc.reverse()), (pos2 + 1i64))) } },
                Token::TkEof => { let __list_subject = acc.clone(); if __list_subject.is_empty() { return Err(AverStr::from("Expected at least one match arm")) } else { return Ok((Expr::ExprMatch(std::sync::Arc::new(subject), acc.reverse()), pos2)) } },
                _ => __MutualTco7::ParseOneArm(tokens, pos2, subject, acc)
                }
            }
            __MutualTco7::ParseOneArm(mut tokens, mut pos, mut subject, mut acc) => {
                crate::cancel_checkpoint();
                let pr = crate::aver_generated::domain::parser_match::parsePattern(&tokens, pos)?;
                match pr {
                (pat, pos2) => __MutualTco7::ParseOneArmBody(tokens, pos2, subject, acc, pat)
                }
            }
            __MutualTco7::ParseOneArmBody(mut tokens, mut pos, mut subject, mut acc, mut pat) => {
                crate::cancel_checkpoint();
                let pos2 = crate::aver_generated::domain::parser_match::expect(&tokens, pos, &Token::TkArrow)?;
                let er = parseExpr(&tokens, pos2)?;
                match er {
                (body, pos3) => __MutualTco7::ParseMatchArms(tokens.clone(), crate::aver_generated::domain::parser_match::skipNewlines(tokens, pos3), subject, aver_rt::AverList::prepend(MatchArm { pattern: pat, body: body, bindingSlots: HashMap::new() }, &acc))
                }
            }
        };
    }
}

/// Parse match arms until DEDENT.
pub fn parseMatchArms(tokens: &aver_rt::AverList<Token>, pos: i64, subject: &Expr, acc: &aver_rt::AverList<MatchArm>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_7(__MutualTco7::ParseMatchArms(tokens.clone(), pos, subject.clone(), acc.clone()))
}

/// Parse one match arm: pattern '->' expr NEWLINE
pub fn parseOneArm(tokens: &aver_rt::AverList<Token>, pos: i64, subject: &Expr, acc: &aver_rt::AverList<MatchArm>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_7(__MutualTco7::ParseOneArm(tokens.clone(), pos, subject.clone(), acc.clone()))
}

/// Parse arrow and body of a match arm.
pub fn parseOneArmBody(tokens: &aver_rt::AverList<Token>, pos: i64, subject: &Expr, acc: &aver_rt::AverList<MatchArm>, pat: &Pattern) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_7(__MutualTco7::ParseOneArmBody(tokens.clone(), pos, subject.clone(), acc.clone(), pat.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco8 {
    ParseMatchArmsFlat(aver_rt::AverList<Token>, i64, Expr, aver_rt::AverList<MatchArm>),
    ParseOneArmFlat(aver_rt::AverList<Token>, i64, Expr, aver_rt::AverList<MatchArm>),
    ParseOneArmFlatBody(aver_rt::AverList<Token>, i64, Expr, aver_rt::AverList<MatchArm>, Pattern),
}

fn __mutual_tco_trampoline_8(mut __state: __MutualTco8) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco8::ParseMatchArmsFlat(mut tokens, mut pos, mut subject, mut acc) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::parser_match::isArmStart(&tokens, pos) { __MutualTco8::ParseOneArmFlat(tokens, pos, subject, acc) } else { { let __list_subject = acc.clone(); if __list_subject.is_empty() { return Err(AverStr::from("Expected at least one match arm")) } else { return Ok((Expr::ExprMatch(std::sync::Arc::new(subject), acc.reverse()), pos)) } } }
            }
            __MutualTco8::ParseOneArmFlat(mut tokens, mut pos, mut subject, mut acc) => {
                crate::cancel_checkpoint();
                let pr = crate::aver_generated::domain::parser_match::parsePattern(&tokens, pos)?;
                match pr {
                (pat, pos2) => __MutualTco8::ParseOneArmFlatBody(tokens, pos2, subject, acc, pat)
                }
            }
            __MutualTco8::ParseOneArmFlatBody(mut tokens, mut pos, mut subject, mut acc, mut pat) => {
                crate::cancel_checkpoint();
                let pos2 = crate::aver_generated::domain::parser_match::expect(&tokens, pos, &Token::TkArrow)?;
                let er = parseExpr(&tokens, pos2)?;
                match er {
                (body, pos3) => __MutualTco8::ParseMatchArmsFlat(tokens.clone(), crate::aver_generated::domain::parser_match::skipNewlines(tokens, pos3), subject, aver_rt::AverList::prepend(MatchArm { pattern: pat, body: body, bindingSlots: HashMap::new() }, &acc))
                }
            }
        };
    }
}

/// Parse match arms without INDENT/DEDENT (fallback for flat token streams).
pub fn parseMatchArmsFlat(tokens: &aver_rt::AverList<Token>, pos: i64, subject: &Expr, acc: &aver_rt::AverList<MatchArm>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_8(__MutualTco8::ParseMatchArmsFlat(tokens.clone(), pos, subject.clone(), acc.clone()))
}

/// Parse one arm and continue in flat mode.
pub fn parseOneArmFlat(tokens: &aver_rt::AverList<Token>, pos: i64, subject: &Expr, acc: &aver_rt::AverList<MatchArm>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_8(__MutualTco8::ParseOneArmFlat(tokens.clone(), pos, subject.clone(), acc.clone()))
}

/// Parse arrow and body, then continue flat.
pub fn parseOneArmFlatBody(tokens: &aver_rt::AverList<Token>, pos: i64, subject: &Expr, acc: &aver_rt::AverList<MatchArm>, pat: &Pattern) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_8(__MutualTco8::ParseOneArmFlatBody(tokens.clone(), pos, subject.clone(), acc.clone(), pat.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco9 {
    ParseMulExprTail(aver_rt::AverList<Token>, i64, Expr),
    ParseMulExprRight(aver_rt::AverList<Token>, i64, Expr, i64),
}

fn __mutual_tco_trampoline_9(mut __state: __MutualTco9) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco9::ParseMulExprTail(mut tokens, mut pos, mut left) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkStar => __MutualTco9::ParseMulExprRight(tokens, nextPos, left, 0i64),
                Token::TkSlash => __MutualTco9::ParseMulExprRight(tokens, nextPos, left, 1i64),
                _ => return Ok((left, pos))
                }
            }
            __MutualTco9::ParseMulExprRight(mut tokens, mut pos, mut left, mut op) => {
                crate::cancel_checkpoint();
                let r = parseAtom(&tokens, pos)?;
                match r {
                (right, pos2) => if (op == 0i64) { __MutualTco9::ParseMulExprTail(tokens, pos2, Expr::ExprMul(std::sync::Arc::new(left), std::sync::Arc::new(right))) } else { __MutualTco9::ParseMulExprTail(tokens, pos2, Expr::ExprDiv(std::sync::Arc::new(left), std::sync::Arc::new(right))) }
                }
            }
        };
    }
}

/// Continue parsing multiplicative operators.
pub fn parseMulExprTail(tokens: &aver_rt::AverList<Token>, pos: i64, left: &Expr) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_9(__MutualTco9::ParseMulExprTail(tokens.clone(), pos, left.clone()))
}

/// Parse right side of mul/div op.
pub fn parseMulExprRight(tokens: &aver_rt::AverList<Token>, pos: i64, left: &Expr, op: i64) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_9(__MutualTco9::ParseMulExprRight(tokens.clone(), pos, left.clone(), op))
}

#[allow(non_camel_case_types)]
enum __MutualTco10 {
    ParseNamedArgs(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<Expr>, aver_rt::AverList<(AverStr, Expr)>),
    ParseNamedArgField(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<Expr>, aver_rt::AverList<(AverStr, Expr)>, AverStr),
    ParseNamedArgsTail(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<Expr>, aver_rt::AverList<(AverStr, Expr)>),
}

fn __mutual_tco_trampoline_10(mut __state: __MutualTco10) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco10::ParseNamedArgs(mut tokens, mut pos, mut name, mut positionalArgs, mut namedAcc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkIdent(field) => __MutualTco10::ParseNamedArgField(tokens, (pos + 1i64), name, positionalArgs, namedAcc, field),
                Token::TkRParen => return Ok((Expr::ExprCall(name, aver_rt::AverList::concat(&positionalArgs.reverse(), &aver_rt::AverList::from_vec(vec![Expr::ExprRecord(AverStr::from("_named"), namedAcc.reverse())]))), (pos + 1i64))),
                _ => return Err(AverStr::from("Expected field name or ')' in named arguments"))
                }
            }
            __MutualTco10::ParseNamedArgField(mut tokens, mut pos, mut name, mut positionalArgs, mut namedAcc, mut field) => {
                crate::cancel_checkpoint();
                let pos2 = crate::aver_generated::domain::parser_match::expect(&tokens, pos, &Token::TkEq)?;
                let r = parseExpr(&tokens, pos2)?;
                match r {
                (expr, pos3) => __MutualTco10::ParseNamedArgsTail(tokens, pos3, name, positionalArgs, aver_rt::AverList::prepend((field, expr), &namedAcc))
                }
            }
            __MutualTco10::ParseNamedArgsTail(mut tokens, mut pos0, mut name, mut positionalArgs, mut namedAcc) => {
                crate::cancel_checkpoint();
                let pos = skipNl(tokens.clone(), pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos.clone()) {
                Token::TkComma => __MutualTco10::ParseNamedArgs(tokens.clone(), skipNl(tokens, (pos + 1i64)), name, positionalArgs, namedAcc),
                Token::TkRParen => return Ok((Expr::ExprCall(name, aver_rt::AverList::concat(&positionalArgs.reverse(), &aver_rt::AverList::from_vec(vec![Expr::ExprRecord(AverStr::from("_named"), namedAcc.reverse())]))), (pos + 1i64))),
                _ => return Err(AverStr::from("Expected ',' or ')' in named arguments"))
                }
            }
        };
    }
}

/// Parse field = expr pairs for named/record-update args.
pub fn parseNamedArgs(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, positionalArgs: &aver_rt::AverList<Expr>, namedAcc: &aver_rt::AverList<(AverStr, Expr)>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_10(__MutualTco10::ParseNamedArgs(tokens.clone(), pos, name, positionalArgs.clone(), namedAcc.clone()))
}

/// Parse = expr after field name in named args.
pub fn parseNamedArgField(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, positionalArgs: &aver_rt::AverList<Expr>, namedAcc: &aver_rt::AverList<(AverStr, Expr)>, field: AverStr) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_10(__MutualTco10::ParseNamedArgField(tokens.clone(), pos, name, positionalArgs.clone(), namedAcc.clone(), field))
}

/// After named arg: ',' for more or ')' to end.
pub fn parseNamedArgsTail(tokens: &aver_rt::AverList<Token>, pos0: i64, name: AverStr, positionalArgs: &aver_rt::AverList<Expr>, namedAcc: &aver_rt::AverList<(AverStr, Expr)>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_10(__MutualTco10::ParseNamedArgsTail(tokens.clone(), pos0, name, positionalArgs.clone(), namedAcc.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco11 {
    ParseRecordFields(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<(AverStr, Expr)>),
    ParseRecordField(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<(AverStr, Expr)>, AverStr),
    ParseRecordFieldsTail(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<(AverStr, Expr)>),
    ParseRecordAfterComma(aver_rt::AverList<Token>, i64, AverStr, aver_rt::AverList<(AverStr, Expr)>),
}

fn __mutual_tco_trampoline_11(mut __state: __MutualTco11) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco11::ParseRecordFields(mut tokens, mut pos, mut name, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkIdent(field) => __MutualTco11::ParseRecordField(tokens, (pos + 1i64), name, acc, field),
                _ => return Err(AverStr::from("Expected field name in record constructor"))
                }
            }
            __MutualTco11::ParseRecordField(mut tokens, mut pos, mut name, mut acc, mut field) => {
                crate::cancel_checkpoint();
                let pos2 = crate::aver_generated::domain::parser_match::expect(&tokens, pos, &Token::TkEq)?;
                let r = parseExpr(&tokens, pos2)?;
                match r {
                (expr, pos3) => __MutualTco11::ParseRecordFieldsTail(tokens, pos3, name, aver_rt::AverList::prepend((field, expr), &acc))
                }
            }
            __MutualTco11::ParseRecordFieldsTail(mut tokens, mut pos, mut name, mut fields) => {
                crate::cancel_checkpoint();
                let pos2 = skipNl(tokens.clone(), pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos2.clone()) {
                Token::TkComma => __MutualTco11::ParseRecordAfterComma(tokens.clone(), skipNl(tokens, (pos2 + 1i64)), name, fields),
                Token::TkRParen => return Ok((Expr::ExprRecord(name, fields.reverse()), (pos2 + 1i64))),
                _ => return Err(AverStr::from("Expected ',' or ')' in record constructor"))
                }
            }
            __MutualTco11::ParseRecordAfterComma(mut tokens, mut pos, mut name, mut fields) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkRParen => return Ok((Expr::ExprRecord(name, fields), (pos + 1i64))),
                _ => __MutualTco11::ParseRecordFields(tokens, pos, name, fields)
                }
            }
        };
    }
}

/// Parse record fields: field = expr, ...
pub fn parseRecordFields(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, acc: &aver_rt::AverList<(AverStr, Expr)>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_11(__MutualTco11::ParseRecordFields(tokens.clone(), pos, name, acc.clone()))
}

/// Parse = expr after field name.
pub fn parseRecordField(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, acc: &aver_rt::AverList<(AverStr, Expr)>, field: AverStr) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_11(__MutualTco11::ParseRecordField(tokens.clone(), pos, name, acc.clone(), field))
}

/// After a field: ',' for more or ')' to end.
pub fn parseRecordFieldsTail(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, fields: &aver_rt::AverList<(AverStr, Expr)>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_11(__MutualTco11::ParseRecordFieldsTail(tokens.clone(), pos, name, fields.clone()))
}

/// Allow a trailing comma before ')' in record constructors.
pub fn parseRecordAfterComma(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, fields: &aver_rt::AverList<(AverStr, Expr)>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_11(__MutualTco11::ParseRecordAfterComma(tokens.clone(), pos, name, fields.clone()))
}

#[allow(non_camel_case_types)]
enum __MutualTco12 {
    ParseTupleRest(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
    ParseTupleRestTail(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
    ParseTupleAfterComma(aver_rt::AverList<Token>, i64, aver_rt::AverList<Expr>),
}

fn __mutual_tco_trampoline_12(mut __state: __MutualTco12) -> Result<(Expr, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco12::ParseTupleRest(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let r = parseExpr(&tokens, pos)?;
                match r {
                (expr, pos2) => __MutualTco12::ParseTupleRestTail(tokens, pos2, aver_rt::AverList::prepend(expr, &acc))
                }
            }
            __MutualTco12::ParseTupleRestTail(mut tokens, mut pos0, mut items) => {
                crate::cancel_checkpoint();
                let pos = skipNl(tokens.clone(), pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos.clone()) {
                Token::TkComma => __MutualTco12::ParseTupleAfterComma(tokens.clone(), skipNl(tokens, (pos + 1i64)), items),
                Token::TkRParen => return finishTupleOrProduct(&tokens, (pos + 1i64), &items.reverse()),
                _ => return Err(AverStr::from("Expected ')' or ',' in tuple"))
                }
            }
            __MutualTco12::ParseTupleAfterComma(mut tokens, mut pos, mut items) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                Token::TkRParen => return finishTupleOrProduct(&tokens, (pos + 1i64), &items),
                _ => __MutualTco12::ParseTupleRest(tokens, pos, items)
                }
            }
        };
    }
}

/// Parse remaining tuple elements after first comma.
pub fn parseTupleRest(tokens: &aver_rt::AverList<Token>, pos: i64, acc: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_12(__MutualTco12::ParseTupleRest(tokens.clone(), pos, acc.clone()))
}

/// After tuple element: ',' for more or ')' to end.
pub fn parseTupleRestTail(tokens: &aver_rt::AverList<Token>, pos0: i64, items: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_12(__MutualTco12::ParseTupleRestTail(tokens.clone(), pos0, items.clone()))
}

/// Allow a trailing comma before ')' in tuples.
pub fn parseTupleAfterComma(tokens: &aver_rt::AverList<Token>, pos: i64, items: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    __mutual_tco_trampoline_12(__MutualTco12::ParseTupleAfterComma(tokens.clone(), pos, items.clone()))
}

/// Parse an expression: comparison level, then optional ? postfix.
pub fn parseExpr(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = parseCmpExpr(tokens, pos)?;
    { let (expr, pos2) = r; Ok(parsePostfixQuestion(tokens, pos2, &expr)) }
}

/// Check for postfix ? (error propagation).
pub fn parsePostfixQuestion(tokens: &aver_rt::AverList<Token>, pos: i64, expr: &Expr) -> (Expr, i64) {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        Token::TkQuestion => {
            (Expr::ExprPropagate(std::sync::Arc::new(expr.clone())), (pos + 1i64))
        },
        _ => {
            (expr.clone(), pos)
        }
    }
}

/// Parse comparison: addExpr (('==' | '<' | '>') addExpr)?
pub fn parseCmpExpr(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = parseAddExpr(tokens, pos)?;
    { let (left, pos2) = r; parseCmpExprTail(tokens, pos2, &left) }
}

/// Check for comparison operator.
pub fn parseCmpExprTail(tokens: &aver_rt::AverList<Token>, pos: i64, left: &Expr) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        Token::TkEqEq => {
            parseCmpExprRight(tokens, nextPos, left, 0i64)
        },
        Token::TkNeq => {
            parseCmpExprRight(tokens, nextPos, left, 1i64)
        },
        Token::TkLt => {
            parseCmpExprRight(tokens, nextPos, left, 2i64)
        },
        Token::TkGt => {
            parseCmpExprRight(tokens, nextPos, left, 3i64)
        },
        Token::TkLte => {
            parseCmpExprRight(tokens, nextPos, left, 4i64)
        },
        Token::TkGte => {
            parseCmpExprRight(tokens, nextPos, left, 5i64)
        },
        _ => {
            Ok((left.clone(), pos))
        }
    }
}

/// Parse right side of comparison.
pub fn parseCmpExprRight(tokens: &aver_rt::AverList<Token>, pos: i64, left: &Expr, op: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = parseAddExpr(tokens, pos)?;
    { let (right, pos2) = r; buildCmpExpr(left, &right, pos2, op) }
}

/// Build comparison AST node from op code.
#[inline(always)]
pub fn buildCmpExpr(left: &Expr, right: &Expr, pos: i64, op: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    { let __dispatch_subject = op; if __dispatch_subject == 0i64 { Ok((Expr::ExprEq(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone())), pos)) } else { if __dispatch_subject == 1i64 { Ok((Expr::ExprNeq(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone())), pos)) } else { if __dispatch_subject == 2i64 { Ok((Expr::ExprLt(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone())), pos)) } else { if __dispatch_subject == 3i64 { Ok((Expr::ExprGt(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone())), pos)) } else { if __dispatch_subject == 4i64 { Ok((Expr::ExprLte(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone())), pos)) } else { if __dispatch_subject == 5i64 { Ok((Expr::ExprGte(std::sync::Arc::new(left.clone()), std::sync::Arc::new(right.clone())), pos)) } else { Err(AverStr::from("unknown cmp operator")) } } } } } } }
}

/// Parse additive: mulExpr (('+' | '-') mulExpr)*
pub fn parseAddExpr(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = parseMulExpr(tokens, pos)?;
    { let (expr, pos2) = r; parseAddExprTail(tokens, pos2, &expr) }
}

/// Parse multiplicative: atom ('*' atom)*
pub fn parseMulExpr(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = parseAtom(tokens, pos)?;
    { let (expr, pos2) = r; parseMulExprTail(tokens, pos2, &expr) }
}

/// Parse atomic expression: literal, variable, call, match, or parenthesized.
pub fn parseAtom(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    let t = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
    match t.clone() {
        Token::TkMinus => {
            parseNegAtom(tokens, nextPos)
        },
        Token::TkInt(n) => {
            Ok((Expr::ExprInt(n), nextPos))
        },
        Token::TkFloat(f) => {
            Ok((Expr::ExprFloat(f), nextPos))
        },
        Token::TkStr(s) => {
            parseStringOrInterp(tokens, nextPos, s)
        },
        Token::TkTrue => {
            Ok((Expr::ExprBool(true), nextPos))
        },
        Token::TkFalse => {
            Ok((Expr::ExprBool(false), nextPos))
        },
        Token::TkLBracket => {
            parseListExpr(tokens, nextPos)
        },
        Token::TkLBrace => {
            parseMapLiteral(tokens, nextPos)
        },
        Token::TkLParen => {
            parseParenExpr(tokens, nextPos)
        },
        Token::TkMatch => {
            parseMatchExpr(tokens, nextPos)
        },
        Token::TkIdent(name) => {
            parseIdentOrCall(tokens, nextPos, name)
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((41i64) as usize); __b.push_str(&AverStr::from("Expected expression, got ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::aver_generated::domain::token::tokenRepr(&t))))); __b }))
        }
    }
}

/// Parse map literal into Map.fromList call.
pub fn parseMapLiteral(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = skipNl(tokens.clone(), pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        Token::TkRBrace => {
            Ok((Expr::ExprCall(AverStr::from("Map.fromList"), aver_rt::AverList::from_vec(vec![Expr::ExprList(aver_rt::AverList::empty())])), (pos2 + 1i64)))
        },
        _ => {
            parseMapEntries(tokens, pos2, &aver_rt::AverList::empty())
        }
    }
}

/// Parse unary minus: -expr -> 0 - expr.
pub fn parseNegAtom(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = parseAtom(tokens, pos)?;
    { let (expr, pos2) = r; Ok((Expr::ExprSub(std::sync::Arc::new(Expr::ExprInt(0i64)), std::sync::Arc::new(expr)), pos2)) }
}

/// After TkStr: check for interpolation (TkInterpStart) or plain string.
pub fn parseStringOrInterp(tokens: &aver_rt::AverList<Token>, pos: i64, prefix: AverStr) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        Token::TkInterpStart => {
            parseInterpParts(tokens, (pos + 1i64), &aver_rt::AverList::from_vec(vec![Expr::ExprStr(prefix)]))
        },
        _ => {
            Ok((Expr::ExprStr(prefix), pos))
        }
    }
}

/// Parse list literal: [expr, expr, ...] or [].
pub fn parseListExpr(tokens: &aver_rt::AverList<Token>, pos0: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let pos = skipNl(tokens.clone(), pos0);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        Token::TkRBracket => {
            Ok((Expr::ExprList(aver_rt::AverList::empty()), (pos + 1i64)))
        },
        _ => {
            parseListItems(tokens, pos, &aver_rt::AverList::empty())
        }
    }
}

/// Parse parenthesized expression or tuple.
pub fn parseParenExpr(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = parseExpr(tokens, pos)?;
    { let (expr, pos2) = r; match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        Token::TkComma => {
            parseTupleRest(tokens, (pos2 + 1i64), &aver_rt::AverList::from_vec(vec![expr]))
        },
        Token::TkRParen => {
            Ok((expr, (pos2 + 1i64)))
        },
        _ => {
            Err(AverStr::from("Expected ')' or ',' after expression"))
        }
    } }
}

/// After tuple closing ')': check for ?! or ! postfix to form independent product.
pub fn finishTupleOrProduct(tokens: &aver_rt::AverList<Token>, pos: i64, items: &aver_rt::AverList<Expr>) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        Token::TkQuestion => {
            match crate::aver_generated::domain::parser_match::tokenAt(tokens, (pos + 1i64)) {
        Token::TkBang => {
            Ok((Expr::ExprIndependentProduct(items.clone(), true), (pos + 2i64)))
        },
        _ => {
            Ok((Expr::ExprTuple(items.clone()), pos))
        }
    }
        },
        Token::TkBang => {
            match crate::aver_generated::domain::parser_match::tokenAt(tokens, (pos + 1i64)) {
        Token::TkLBracket => {
            Ok((Expr::ExprTuple(items.clone()), pos))
        },
        _ => {
            Ok((Expr::ExprIndependentProduct(items.clone(), false), (pos + 1i64)))
        }
    }
        },
        _ => {
            Ok((Expr::ExprTuple(items.clone()), pos))
        }
    }
}

/// After reading an identifier: call, record constructor, field access, or variable?
pub fn parseIdentOrCall(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        Token::TkLParen => {
            chainFieldAccess(tokens, nextPos, name)
        },
        Token::TkDot => {
            parseFieldAccess(tokens, nextPos, &Expr::ExprVar(name))
        },
        _ => {
            Ok((Expr::ExprVar(name), pos))
        }
    }
}

/// Parse call/record then chain .field access if present.
pub fn chainFieldAccess(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = parseCallOrRecord(tokens, pos, name)?;
    { let (expr, pos2) = r; parseFieldAccessTail(tokens, pos2, &expr) }
}

/// Skip newlines and indents inside parenthesized expressions.
pub fn skipNl(mut tokens: aver_rt::AverList<Token>, mut pos: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
            Token::TkNewline => {
            pos = nextPos;
            continue;
        },
            Token::TkIndent => {
            pos = nextPos;
            continue;
        },
            Token::TkDedent => {
            pos = nextPos;
            continue;
        },
            _ => pos
        };
    }
}

/// After '(': is this a function call f(args) or record Foo(field = val)?
pub fn parseCallOrRecord(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = skipNl(tokens.clone(), pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        Token::TkRParen => {
            Ok((Expr::ExprCall(name, aver_rt::AverList::empty()), (pos2 + 1i64)))
        },
        Token::TkIdent(first) => {
            parseCallOrRecordLookahead(tokens, pos2, name, first)
        },
        _ => {
            parseCallArgsList(tokens, pos2, name, &aver_rt::AverList::empty())
        }
    }
}

/// Lookahead after first ident in parens: '=' means record, otherwise call.
pub fn parseCallOrRecordLookahead(tokens: &aver_rt::AverList<Token>, pos: i64, name: AverStr, first: AverStr) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, (pos + 1i64)) {
        Token::TkEq => {
            parseRecordFields(tokens, pos, name, &aver_rt::AverList::empty())
        },
        _ => {
            parseCallArgsList(tokens, pos, name, &aver_rt::AverList::empty())
        }
    }
}

/// Parse: match expr NEWLINE INDENT arms DEDENT. Called after TkMatch is consumed.
pub fn parseMatchExpr(tokens: &aver_rt::AverList<Token>, pos: i64) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let r = parseExpr(tokens, pos)?;
    { let (subject, pos2) = r; parseMatchAfterSubject(tokens, pos2, &subject) }
}

/// Skip newlines, consume INDENT, then parse arms.
pub fn parseMatchAfterSubject(tokens: &aver_rt::AverList<Token>, pos: i64, subject: &Expr) -> Result<(Expr, i64), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::skipNewlines(tokens.clone(), pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        Token::TkIndent => {
            parseMatchArms(tokens, (pos2 + 1i64), subject, &aver_rt::AverList::empty())
        },
        _ => {
            parseMatchArmsFlat(tokens, pos2, subject, &aver_rt::AverList::empty())
        }
    }
}
