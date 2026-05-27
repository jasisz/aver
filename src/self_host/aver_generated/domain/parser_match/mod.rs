#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;
#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    ParseConstructorPatternBindings(
        aver_rt::AverList<Token>,
        i64,
        AverStr,
        aver_rt::AverList<AverStr>,
    ),
    ParseConstructorPatternTail(
        aver_rt::AverList<Token>,
        i64,
        AverStr,
        aver_rt::AverList<AverStr>,
    ),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> Result<(Pattern, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::ParseConstructorPatternBindings(
                mut tokens,
                mut pos,
                mut name,
                mut acc,
            ) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Pattern::PatConstructor(
                                name,
                                acc.reverse(),
                            ),
                            (pos + 1i64),
                        ));
                    }
                    crate::aver_generated::domain::token::Token::TkIdent(binding) => {
                        __MutualTco1::ParseConstructorPatternTail(
                            tokens,
                            (pos + 1i64),
                            name,
                            aver_rt::AverList::prepend(binding, &acc),
                        )
                    }
                    _ => {
                        return Err(AverStr::from(
                            "Expected binding or ')' in constructor pattern",
                        ));
                    }
                }
            }
            __MutualTco1::ParseConstructorPatternTail(mut tokens, mut pos, mut name, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco1::ParseConstructorPatternBindings(
                            tokens,
                            (pos + 1i64),
                            name,
                            acc,
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Pattern::PatConstructor(
                                name,
                                acc.reverse(),
                            ),
                            (pos + 1i64),
                        ));
                    }
                    _ => return Err(AverStr::from("Expected ',' or ')' in constructor pattern")),
                }
            }
        };
    }
}

/// Parse bindings inside constructor pattern: Ctor(a, b) or Ctor().
pub fn parseConstructorPatternBindings(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    name: AverStr,
    acc: &aver_rt::AverList<AverStr>,
) -> Result<(Pattern, i64), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseConstructorPatternBindings(
        tokens.clone(),
        pos,
        name,
        acc.clone(),
    ))
}

/// After binding: ',' for more or ')' to end.
pub fn parseConstructorPatternTail(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    name: AverStr,
    acc: &aver_rt::AverList<AverStr>,
) -> Result<(Pattern, i64), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseConstructorPatternTail(
        tokens.clone(),
        pos,
        name,
        acc.clone(),
    ))
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    ParseIdentPatternMaybeConstructor(aver_rt::AverList<Token>, i64, AverStr),
    ParseIdentPatternDotted(aver_rt::AverList<Token>, i64, AverStr),
}

fn __mutual_tco_trampoline_2(mut __state: __MutualTco2) -> Result<(Pattern, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::ParseIdentPatternMaybeConstructor(mut tokens, mut pos, mut name) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                crate::aver_generated::domain::token::Token::TkDot => __MutualTco2::ParseIdentPatternDotted(tokens, nextPos, name),
                crate::aver_generated::domain::token::Token::TkLParen => return crate::aver_generated::domain::parser_match::parseConstructorPatternBindings(&tokens, nextPos, name, &aver_rt::AverList::empty()),
                _ => if name.contains(".") { return Ok((crate::aver_generated::domain::ast::Pattern::PatConstructor(name, aver_rt::AverList::empty()), pos)) } else { return Ok((crate::aver_generated::domain::ast::Pattern::PatVar(name), pos)) }
                }
            }
            __MutualTco2::ParseIdentPatternDotted(mut tokens, mut pos, mut prefix) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                    crate::aver_generated::domain::token::Token::TkIdent(s) => {
                        __MutualTco2::ParseIdentPatternMaybeConstructor(
                            tokens,
                            (pos + 1i64),
                            ((prefix + &AverStr::from(".")) + &s),
                        )
                    }
                    _ => return Err(AverStr::from("Expected identifier after '.' in pattern")),
                }
            }
        };
    }
}

/// Check for dot-qualified name, constructor pattern, or plain variable.
pub fn parseIdentPatternMaybeConstructor(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    name: AverStr,
) -> Result<(Pattern, i64), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseIdentPatternMaybeConstructor(
        tokens.clone(),
        pos,
        name,
    ))
}

/// After dot: read next ident and check for ( or another dot.
pub fn parseIdentPatternDotted(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    prefix: AverStr,
) -> Result<(Pattern, i64), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseIdentPatternDotted(
        tokens.clone(),
        pos,
        prefix,
    ))
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    ParseParamList(aver_rt::AverList<Token>, i64, aver_rt::AverList<AverStr>),
    ParseParamListTail(aver_rt::AverList<Token>, i64, aver_rt::AverList<AverStr>),
}

fn __mutual_tco_trampoline_3(
    mut __state: __MutualTco3,
) -> Result<(aver_rt::AverList<AverStr>, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::ParseParamList(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((acc.reverse(), (pos + 1i64)));
                    }
                    crate::aver_generated::domain::token::Token::TkIdent(name) => {
                        __MutualTco3::ParseParamListTail(
                            tokens.clone(),
                            crate::aver_generated::domain::parser_match::skipTypeAnnotation(
                                &tokens,
                                (pos + 1i64),
                            ),
                            aver_rt::AverList::prepend(name, &acc),
                        )
                    }
                    _ => return Err(AverStr::from("Expected parameter name or ')'")),
                }
            }
            __MutualTco3::ParseParamListTail(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((acc.reverse(), (pos + 1i64)));
                    }
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco3::ParseParamList(tokens, (pos + 1i64), acc)
                    }
                    _ => return Err(AverStr::from("Expected ',' or ')' in parameter list")),
                }
            }
        };
    }
}

/// Parse comma-separated parameter names until ')'.
pub fn parseParamList(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    acc: &aver_rt::AverList<AverStr>,
) -> Result<(aver_rt::AverList<AverStr>, i64), AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseParamList(
        tokens.clone(),
        pos,
        acc.clone(),
    ))
}

/// After a param name: ',' for more or ')' to end.
pub fn parseParamListTail(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    acc: &aver_rt::AverList<AverStr>,
) -> Result<(aver_rt::AverList<AverStr>, i64), AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseParamListTail(
        tokens.clone(),
        pos,
        acc.clone(),
    ))
}

#[allow(non_camel_case_types)]
enum __MutualTco4 {
    ParseTuplePatternElements(aver_rt::AverList<Token>, i64, aver_rt::AverList<Pattern>),
    ParseTuplePatternElement(aver_rt::AverList<Token>, i64, aver_rt::AverList<Pattern>),
    ParseTuplePatternElementTail(aver_rt::AverList<Token>, i64, aver_rt::AverList<Pattern>),
}

fn __mutual_tco_trampoline_4(mut __state: __MutualTco4) -> Result<(Pattern, i64), AverStr> {
    loop {
        __state = match __state {
            __MutualTco4::ParseTuplePatternElements(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Pattern::PatTuple(acc.reverse()),
                            (pos + 1i64),
                        ));
                    }
                    _ => __MutualTco4::ParseTuplePatternElement(tokens, pos, acc),
                }
            }
            __MutualTco4::ParseTuplePatternElement(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let pr = crate::aver_generated::domain::parser_match::parsePattern(&tokens, pos)?;
                match pr {
                    (pat, pos2) => __MutualTco4::ParseTuplePatternElementTail(
                        tokens,
                        pos2,
                        aver_rt::AverList::prepend(pat, &acc),
                    ),
                }
            }
            __MutualTco4::ParseTuplePatternElementTail(mut tokens, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco4::ParseTuplePatternElements(tokens, (pos + 1i64), acc)
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Pattern::PatTuple(acc.reverse()),
                            (pos + 1i64),
                        ));
                    }
                    _ => return Err(AverStr::from("Expected ',' or ')' in tuple pattern")),
                }
            }
        };
    }
}

/// Parse elements of a tuple pattern.
pub fn parseTuplePatternElements(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    acc: &aver_rt::AverList<Pattern>,
) -> Result<(Pattern, i64), AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::ParseTuplePatternElements(
        tokens.clone(),
        pos,
        acc.clone(),
    ))
}

/// Parse one pattern element and continue.
pub fn parseTuplePatternElement(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    acc: &aver_rt::AverList<Pattern>,
) -> Result<(Pattern, i64), AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::ParseTuplePatternElement(
        tokens.clone(),
        pos,
        acc.clone(),
    ))
}

/// After element: ',' for more or ')' to end.
pub fn parseTuplePatternElementTail(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    acc: &aver_rt::AverList<Pattern>,
) -> Result<(Pattern, i64), AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::ParseTuplePatternElementTail(
        tokens.clone(),
        pos,
        acc.clone(),
    ))
}

#[allow(non_camel_case_types)]
enum __MutualTco5 {
    SkipBlockFlat(aver_rt::AverList<Token>, i64),
    SkipBlockFlatAfterNewline(aver_rt::AverList<Token>, i64),
}

fn __mutual_tco_trampoline_5(mut __state: __MutualTco5) -> i64 {
    loop {
        __state = match __state {
            __MutualTco5::SkipBlockFlat(mut tokens, mut pos) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                    crate::aver_generated::domain::token::Token::TkEof => return pos,
                    crate::aver_generated::domain::token::Token::TkNewline => {
                        __MutualTco5::SkipBlockFlatAfterNewline(tokens, nextPos)
                    }
                    _ => __MutualTco5::SkipBlockFlat(tokens, nextPos),
                }
            }
            __MutualTco5::SkipBlockFlatAfterNewline(mut tokens, mut pos) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                    crate::aver_generated::domain::token::Token::TkEof => return pos,
                    crate::aver_generated::domain::token::Token::TkNewline => return pos,
                    crate::aver_generated::domain::token::Token::TkFn => return pos,
                    crate::aver_generated::domain::token::Token::TkIdent(kw) => {
                        let __dispatch_subject = kw;
                        if &*__dispatch_subject == "module" {
                            return pos;
                        } else {
                            if &*__dispatch_subject == "type" {
                                return pos;
                            } else {
                                if &*__dispatch_subject == "record" {
                                    return pos;
                                } else {
                                    if &*__dispatch_subject == "verify" {
                                        return pos;
                                    } else {
                                        if &*__dispatch_subject == "decision" {
                                            return pos;
                                        } else {
                                            __MutualTco5::SkipBlockFlat(tokens, pos)
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => __MutualTco5::SkipBlockFlat(tokens, pos),
                }
            }
        };
    }
}

/// Fallback: skip until blank line, fn, or EOF.
pub fn skipBlockFlat(tokens: &aver_rt::AverList<Token>, pos: i64) -> i64 {
    __mutual_tco_trampoline_5(__MutualTco5::SkipBlockFlat(tokens.clone(), pos))
}

/// After newline: another newline or fn/type/record/verify/EOF = end of block.
pub fn skipBlockFlatAfterNewline(tokens: &aver_rt::AverList<Token>, pos: i64) -> i64 {
    __mutual_tco_trampoline_5(__MutualTco5::SkipBlockFlatAfterNewline(tokens.clone(), pos))
}

/// Token at position, or TkEof if past end.
#[inline(always)]
pub fn tokenAt(tokens: &aver_rt::AverList<Token>, pos: i64) -> Token {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::parser_match::tokenAtWalk(tokens.clone(), pos, 0i64)
}

/// Walk the list to find token at target index.
#[inline(always)]
pub fn tokenAtWalk(mut tokens: aver_rt::AverList<Token>, mut target: i64, mut idx: i64) -> Token {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(tokens, [] => Token::TkEof, [t, rest] => { if (idx == target) { t } else { {
            let __tmp2 = (idx + 1i64);
            tokens = rest;
            idx = __tmp2;
            continue;
        } } });
    }
}

/// Check if token matches expected (structural equality via repr).
pub fn isToken(t: &Token, expected: &Token) -> bool {
    crate::cancel_checkpoint();
    (crate::aver_generated::domain::token::tokenRepr(t)
        == crate::aver_generated::domain::token::tokenRepr(expected))
}

/// Consume expected token, return new position.
#[inline(always)]
pub fn expect(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    expected: &Token,
) -> Result<i64, AverStr> {
    crate::cancel_checkpoint();
    let t = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
    if crate::aver_generated::domain::parser_match::isToken(&t, expected) {
        Ok((pos + 1i64))
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity((47i64) as usize);
                        __b.push_str(&AverStr::from("Expected "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::aver_generated::domain::token::tokenRepr(expected)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(", got "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::aver_generated::domain::token::tokenRepr(&t)),
            )));
            __b
        }))
    }
}

/// Skip past any newline tokens.
pub fn skipNewlines(mut tokens: aver_rt::AverList<Token>, mut pos: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        return match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
            crate::aver_generated::domain::token::Token::TkNewline => {
                let __tmp1 = (pos + 1i64);
                pos = __tmp1;
                continue;
            }
            _ => pos,
        };
    }
}

/// Skip newlines and stray DEDENT tokens at top level.
pub fn skipNewlinesAndDedents(mut tokens: aver_rt::AverList<Token>, mut pos: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
            crate::aver_generated::domain::token::Token::TkNewline => {
                pos = nextPos;
                continue;
            }
            crate::aver_generated::domain::token::Token::TkDedent => {
                pos = nextPos;
                continue;
            }
            _ => pos,
        };
    }
}

/// Parse a match pattern: INT literal, _ (wildcard), or variable.
pub fn parsePattern(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
) -> Result<(Pattern, i64), AverStr> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    let t = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
    match t.clone() {
        crate::aver_generated::domain::token::Token::TkInt(n) => Ok((
            crate::aver_generated::domain::ast::Pattern::PatInt(n),
            nextPos,
        )),
        crate::aver_generated::domain::token::Token::TkFloat(f) => Ok((
            crate::aver_generated::domain::ast::Pattern::PatFloat(f),
            nextPos,
        )),
        crate::aver_generated::domain::token::Token::TkStr(s) => Ok((
            crate::aver_generated::domain::ast::Pattern::PatStr(s),
            nextPos,
        )),
        crate::aver_generated::domain::token::Token::TkTrue => Ok((
            crate::aver_generated::domain::ast::Pattern::PatBool(true),
            nextPos,
        )),
        crate::aver_generated::domain::token::Token::TkFalse => Ok((
            crate::aver_generated::domain::ast::Pattern::PatBool(false),
            nextPos,
        )),
        crate::aver_generated::domain::token::Token::TkLBracket => {
            crate::aver_generated::domain::parser_match::parseListPattern(tokens, nextPos)
        }
        crate::aver_generated::domain::token::Token::TkLParen => {
            crate::aver_generated::domain::parser_match::parseTuplePattern(tokens, nextPos)
        }
        crate::aver_generated::domain::token::Token::TkIdent(s) => {
            crate::aver_generated::domain::parser_match::parseIdentPattern(tokens, nextPos, s)
        }
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity((38i64) as usize);
                __b.push_str(&AverStr::from("Expected pattern, got "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::aver_generated::domain::token::tokenRepr(&t)),
            )));
            __b
        })),
    }
}

/// Parse ident pattern: wildcard _, variable, or constructor Foo.Bar(bindings).
#[inline(always)]
pub fn parseIdentPattern(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    name: AverStr,
) -> Result<(Pattern, i64), AverStr> {
    crate::cancel_checkpoint();
    if (name == AverStr::from("_")) {
        Ok((Pattern::PatWild.clone(), pos))
    } else {
        crate::aver_generated::domain::parser_match::parseIdentPatternMaybeConstructor(
            tokens, pos, name,
        )
    }
}

/// Parse tuple pattern: (pat, pat, ...).
pub fn parseTuplePattern(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
) -> Result<(Pattern, i64), AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::parser_match::parseTuplePatternElements(
        tokens,
        pos,
        &aver_rt::AverList::empty(),
    )
}

/// Parse [] or [h, ..t] list pattern.
pub fn parseListPattern(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
) -> Result<(Pattern, i64), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        crate::aver_generated::domain::token::Token::TkRBracket => {
            Ok((Pattern::PatEmpty.clone(), (pos + 1i64)))
        }
        crate::aver_generated::domain::token::Token::TkIdent(head) => {
            crate::aver_generated::domain::parser_match::parseConsPattern(
                tokens,
                (pos + 1i64),
                head,
            )
        }
        _ => Err(AverStr::from("Expected identifier or ']' in list pattern")),
    }
}

/// Parse [h, ..t] after reading head ident.
pub fn parseConsPattern(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    head: AverStr,
) -> Result<(Pattern, i64), AverStr> {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::expect(tokens, pos, &Token::TkComma)?;
    let pos3 = crate::aver_generated::domain::parser_match::expect(tokens, pos2, &Token::TkDotDot)?;
    let t = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos3.clone());
    match t {
        crate::aver_generated::domain::token::Token::TkIdent(tail) => Ok((
            crate::aver_generated::domain::ast::Pattern::PatCons(head, tail),
            crate::aver_generated::domain::parser_match::expect(
                tokens,
                (pos3 + 1i64),
                &Token::TkRBracket,
            )?,
        )),
        _ => Err(AverStr::from("Expected tail identifier in cons pattern")),
    }
}

/// Skip optional : Type annotation.
pub fn skipTypeAnnotation(tokens: &aver_rt::AverList<Token>, pos: i64) -> i64 {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        crate::aver_generated::domain::token::Token::TkColon => {
            crate::aver_generated::domain::parser_match::skipTypeExpr(tokens, (pos + 1i64))
        }
        _ => pos,
    }
}

/// Skip a type expression (ident, possibly with generics).
pub fn skipTypeExpr(tokens: &aver_rt::AverList<Token>, pos: i64) -> i64 {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        crate::aver_generated::domain::token::Token::TkIdent(_) => {
            crate::aver_generated::domain::parser_match::skipTypeExprTail(tokens.clone(), nextPos)
        }
        crate::aver_generated::domain::token::Token::TkLParen => {
            crate::aver_generated::domain::parser_match::skipUntilClose(
                tokens.clone(),
                nextPos,
                1i64,
            )
        }
        _ => pos,
    }
}

/// After type ident: possibly <...> generics or .Qualified.
pub fn skipTypeExprTail(mut tokens: aver_rt::AverList<Token>, mut pos: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
            crate::aver_generated::domain::token::Token::TkLt => {
                crate::aver_generated::domain::parser_match::skipUntilGt(tokens, nextPos, 1i64)
            }
            crate::aver_generated::domain::token::Token::TkDot => {
                match crate::aver_generated::domain::parser_match::tokenAt(&tokens, nextPos) {
                    crate::aver_generated::domain::token::Token::TkIdent(_) => {
                        let __tmp1 = (pos + 2i64);
                        pos = __tmp1;
                        continue;
                    }
                    _ => pos,
                }
            }
            _ => pos,
        };
    }
}

/// Skip until matching > for generic type params.
#[inline(always)]
pub fn skipUntilGt(mut tokens: aver_rt::AverList<Token>, mut pos: i64, mut depth: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return if (depth == 0i64) {
            pos
        } else {
            match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                crate::aver_generated::domain::token::Token::TkEof => pos,
                crate::aver_generated::domain::token::Token::TkLt => {
                    let __tmp2 = (depth + 1i64);
                    pos = nextPos;
                    depth = __tmp2;
                    continue;
                }
                crate::aver_generated::domain::token::Token::TkGt => {
                    let __tmp2 = (depth - 1i64);
                    pos = nextPos;
                    depth = __tmp2;
                    continue;
                }
                _ => {
                    pos = nextPos;
                    continue;
                }
            }
        };
    }
}

/// Skip until matching ) for tuple types.
#[inline(always)]
pub fn skipUntilClose(mut tokens: aver_rt::AverList<Token>, mut pos: i64, mut depth: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return if (depth == 0i64) {
            pos
        } else {
            match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                crate::aver_generated::domain::token::Token::TkEof => pos,
                crate::aver_generated::domain::token::Token::TkLParen => {
                    let __tmp2 = (depth + 1i64);
                    pos = nextPos;
                    depth = __tmp2;
                    continue;
                }
                crate::aver_generated::domain::token::Token::TkRParen => {
                    let __tmp2 = (depth - 1i64);
                    pos = nextPos;
                    depth = __tmp2;
                    continue;
                }
                _ => {
                    pos = nextPos;
                    continue;
                }
            }
        };
    }
}

/// Skip optional -> ReturnType annotation.
pub fn skipReturnType(tokens: &aver_rt::AverList<Token>, pos: i64) -> i64 {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        crate::aver_generated::domain::token::Token::TkArrow => {
            crate::aver_generated::domain::parser_match::skipTypeExpr(tokens, (pos + 1i64))
        }
        _ => pos,
    }
}

/// Skip optional ? description and ! [effects] lines (including multi-line effects).
pub fn skipDescAndEffects(mut tokens: aver_rt::AverList<Token>, mut pos: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
            crate::aver_generated::domain::token::Token::TkQuestion => {
                let __tmp0 = tokens.clone();
                let __tmp1 =
                    crate::aver_generated::domain::parser_match::skipToNextLine(tokens, nextPos);
                tokens = __tmp0;
                pos = __tmp1;
                continue;
            }
            crate::aver_generated::domain::token::Token::TkBang => {
                let __tmp0 = tokens.clone();
                let __tmp1 =
                    crate::aver_generated::domain::parser_match::skipEffectsBlock(&tokens, nextPos);
                tokens = __tmp0;
                pos = __tmp1;
                continue;
            }
            _ => pos,
        };
    }
}

/// Skip ! [effects] block — may span multiple lines with INDENT/DEDENT.
pub fn skipEffectsBlock(tokens: &aver_rt::AverList<Token>, pos: i64) -> i64 {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        crate::aver_generated::domain::token::Token::TkLBracket => {
            crate::aver_generated::domain::parser_match::skipUntilRBracket(
                tokens.clone(),
                (pos + 1i64),
            )
        }
        _ => crate::aver_generated::domain::parser_match::skipToNextLine(tokens.clone(), pos),
    }
}

/// Skip tokens until ] then past any trailing newline.
pub fn skipUntilRBracket(mut tokens: aver_rt::AverList<Token>, mut pos: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
            crate::aver_generated::domain::token::Token::TkEof => pos,
            crate::aver_generated::domain::token::Token::TkRBracket => {
                crate::aver_generated::domain::parser_match::skipNewlines(tokens, nextPos)
            }
            _ => {
                pos = nextPos;
                continue;
            }
        };
    }
}

/// Skip tokens until newline, then past it.
pub fn skipToNextLine(mut tokens: aver_rt::AverList<Token>, mut pos: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
            crate::aver_generated::domain::token::Token::TkEof => pos,
            crate::aver_generated::domain::token::Token::TkNewline => nextPos,
            _ => {
                pos = nextPos;
                continue;
            }
        };
    }
}

/// Skip tokens until matching DEDENT, or fall back to heuristic for flat streams.
pub fn skipBlock(tokens: &aver_rt::AverList<Token>, pos: i64) -> i64 {
    crate::cancel_checkpoint();
    let pos2 = crate::aver_generated::domain::parser_match::skipNewlines(tokens.clone(), pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkIndent => {
            crate::aver_generated::domain::parser_match::skipIndentBlock(
                tokens.clone(),
                (pos2 + 1i64),
                1i64,
            )
        }
        crate::aver_generated::domain::token::Token::TkEof => pos2,
        _ => crate::aver_generated::domain::parser_match::skipBlockFlat(tokens, pos2),
    }
}

/// Skip until matching DEDENT.
pub fn skipIndentBlock(mut tokens: aver_rt::AverList<Token>, mut pos: i64, mut depth: i64) -> i64 {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
            crate::aver_generated::domain::token::Token::TkEof => pos,
            crate::aver_generated::domain::token::Token::TkIndent => {
                let __tmp2 = (depth + 1i64);
                pos = nextPos;
                depth = __tmp2;
                continue;
            }
            crate::aver_generated::domain::token::Token::TkDedent => {
                if (depth <= 1i64) {
                    nextPos
                } else {
                    {
                        let __tmp2 = (depth - 1i64);
                        pos = nextPos;
                        depth = __tmp2;
                        continue;
                    }
                }
            }
            _ => {
                pos = nextPos;
                continue;
            }
        };
    }
}

/// Parse module header: find depends list, skip everything else. Returns (deps, endPos).
pub fn parseModuleHeader(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
) -> (aver_rt::AverList<AverStr>, i64) {
    crate::cancel_checkpoint();
    let endPos = crate::aver_generated::domain::parser_match::skipBlock(tokens, pos);
    let depList = crate::aver_generated::domain::parser_match::findDependsInRange(
        tokens.clone(),
        pos,
        endPos.clone(),
    );
    (depList, endPos)
}

/// Scan token range for depends [...] and extract module names.
#[inline(always)]
pub fn findDependsInRange(
    mut tokens: aver_rt::AverList<Token>,
    mut pos: i64,
    mut endPos: i64,
) -> aver_rt::AverList<AverStr> {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return if (pos >= endPos) {
            aver_rt::AverList::empty()
        } else {
            match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                crate::aver_generated::domain::token::Token::TkIdent(kw) => {
                    if (&*kw == "depends") {
                        crate::aver_generated::domain::parser_match::findDependsListAt(
                            &tokens, nextPos, endPos,
                        )
                    } else {
                        {
                            pos = nextPos;
                            continue;
                        }
                    }
                }
                _ => {
                    pos = nextPos;
                    continue;
                }
            }
        };
    }
}

/// Parse [Name, Name] after depends keyword.
pub fn findDependsListAt(
    tokens: &aver_rt::AverList<Token>,
    pos: i64,
    endPos: i64,
) -> aver_rt::AverList<AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos) {
        crate::aver_generated::domain::token::Token::TkLBracket => {
            crate::aver_generated::domain::parser_match::collectDependsNames(
                tokens.clone(),
                (pos + 1i64),
                endPos,
                aver_rt::AverList::empty(),
            )
        }
        _ => aver_rt::AverList::empty(),
    }
}

/// Collect module names until ].
#[inline(always)]
pub fn collectDependsNames(
    mut tokens: aver_rt::AverList<Token>,
    mut pos: i64,
    mut endPos: i64,
    mut acc: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<AverStr> {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return if (pos >= endPos) {
            acc.reverse()
        } else {
            match crate::aver_generated::domain::parser_match::tokenAt(&tokens, pos) {
                crate::aver_generated::domain::token::Token::TkRBracket => acc.reverse(),
                crate::aver_generated::domain::token::Token::TkIdent(name) => {
                    let __tmp3 = aver_rt::AverList::prepend(name, &acc);
                    pos = nextPos;
                    acc = __tmp3;
                    continue;
                }
                _ => {
                    pos = nextPos;
                    continue;
                }
            }
        };
    }
}

/// Check if we've reached the end of a function body (flat mode).
pub fn isBodyEndFlat(tokens: &aver_rt::AverList<Token>, pos: i64) -> bool {
    crate::cancel_checkpoint();
    let t = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
    match t {
        crate::aver_generated::domain::token::Token::TkEof => true,
        crate::aver_generated::domain::token::Token::TkFn => true,
        crate::aver_generated::domain::token::Token::TkNewline => true,
        crate::aver_generated::domain::token::Token::TkDedent => true,
        _ => false,
    }
}

/// Check if position looks like a match arm start.
pub fn isArmStart(tokens: &aver_rt::AverList<Token>, pos: i64) -> bool {
    crate::cancel_checkpoint();
    let t = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
    match t {
        crate::aver_generated::domain::token::Token::TkInt(_) => true,
        crate::aver_generated::domain::token::Token::TkFloat(_) => true,
        crate::aver_generated::domain::token::Token::TkStr(_) => true,
        crate::aver_generated::domain::token::Token::TkTrue => true,
        crate::aver_generated::domain::token::Token::TkFalse => true,
        crate::aver_generated::domain::token::Token::TkLBracket => true,
        crate::aver_generated::domain::token::Token::TkLParen => true,
        crate::aver_generated::domain::token::Token::TkIdent(_) => true,
        _ => false,
    }
}
