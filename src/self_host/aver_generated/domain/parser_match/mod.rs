#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;
#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    ParseIdentPatternMaybeConstructor(aver_rt::AverInt, AverStr),
    ParseIdentPatternDotted(aver_rt::AverInt, AverStr),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    loop {
        __state = match __state {
            __MutualTco1::ParseIdentPatternMaybeConstructor(mut pos @ _, mut name @ _) => {
                crate::cancel_checkpoint();
                let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkDot => {
            __MutualTco1::ParseIdentPatternDotted(nextPos, name)
        },
        crate::aver_generated::domain::token::Token::TkLParen => {
            return crate::aver_generated::domain::parser_match::parseConstructorPatternBindings(&*tokens, nextPos, name, &aver_rt::AverList::empty())
        },
        _ => {
            if name.contains(".") { return Ok((crate::aver_generated::domain::ast::Pattern::PatConstructor(name, aver_rt::AverList::empty()), pos)) } else { return Ok((crate::aver_generated::domain::ast::Pattern::PatVar(name), pos)) }
        }
    }
            }
            __MutualTco1::ParseIdentPatternDotted(mut pos @ _, mut prefix @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkIdent(s) => {
                        __MutualTco1::ParseIdentPatternMaybeConstructor(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
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
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    name @ _: AverStr,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    __mutual_tco_trampoline_1(
        __MutualTco1::ParseIdentPatternMaybeConstructor(pos, name),
        &tokens,
    )
}

/// After dot: read next ident and check for ( or another dot.
pub fn parseIdentPatternDotted(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    prefix @ _: AverStr,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseIdentPatternDotted(pos, prefix), &tokens)
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    ParseConstructorPatternBindings(aver_rt::AverInt, AverStr, aver_rt::AverList<AverStr>),
    ParseConstructorPatternTail(aver_rt::AverInt, AverStr, aver_rt::AverList<AverStr>),
}

fn __mutual_tco_trampoline_2(
    mut __state: __MutualTco2,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    loop {
        __state = match __state {
            __MutualTco2::ParseConstructorPatternBindings(
                mut pos @ _,
                mut name @ _,
                mut acc @ _,
            ) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Pattern::PatConstructor(
                                name,
                                acc.reverse(),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    crate::aver_generated::domain::token::Token::TkIdent(binding) => {
                        __MutualTco2::ParseConstructorPatternTail(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
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
            __MutualTco2::ParseConstructorPatternTail(mut pos @ _, mut name @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco2::ParseConstructorPatternBindings(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
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
                            pos.add(&aver_rt::AverInt::from_i64(1)),
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
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    name @ _: AverStr,
    acc @ _: &aver_rt::AverList<AverStr>,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    __mutual_tco_trampoline_2(
        __MutualTco2::ParseConstructorPatternBindings(pos, name, acc.clone()),
        &tokens,
    )
}

/// After binding: ',' for more or ')' to end.
pub fn parseConstructorPatternTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    name @ _: AverStr,
    acc @ _: &aver_rt::AverList<AverStr>,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    __mutual_tco_trampoline_2(
        __MutualTco2::ParseConstructorPatternTail(pos, name, acc.clone()),
        &tokens,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    ParseTuplePatternElements(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    ),
    ParseTuplePatternElement(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    ),
    ParseTuplePatternElementTail(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
    ),
}

fn __mutual_tco_trampoline_3(
    mut __state: __MutualTco3,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    loop {
        __state = match __state {
            __MutualTco3::ParseTuplePatternElements(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Pattern::PatTuple(acc.reverse()),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => __MutualTco3::ParseTuplePatternElement(pos, acc),
                }
            }
            __MutualTco3::ParseTuplePatternElement(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let pr @ _ =
                    crate::aver_generated::domain::parser_match::parsePattern(&*tokens, pos)?;
                {
                    let (pat, pos2) = pr;
                    __MutualTco3::ParseTuplePatternElementTail(
                        pos2,
                        aver_rt::AverList::prepend(pat, &acc),
                    )
                }
            }
            __MutualTco3::ParseTuplePatternElementTail(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco3::ParseTuplePatternElements(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            acc,
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Pattern::PatTuple(acc.reverse()),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
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
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    __mutual_tco_trampoline_3(
        __MutualTco3::ParseTuplePatternElements(pos, acc.clone()),
        &tokens,
    )
}

/// Parse one pattern element and continue.
pub fn parseTuplePatternElement(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    __mutual_tco_trampoline_3(
        __MutualTco3::ParseTuplePatternElement(pos, acc.clone()),
        &tokens,
    )
}

/// After element: ',' for more or ')' to end.
pub fn parseTuplePatternElementTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Pattern>,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    __mutual_tco_trampoline_3(
        __MutualTco3::ParseTuplePatternElementTail(pos, acc.clone()),
        &tokens,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco4 {
    SkipBlockFlat(aver_rt::AverInt),
    SkipBlockFlatAfterNewline(aver_rt::AverInt),
}

fn __mutual_tco_trampoline_4(
    mut __state: __MutualTco4,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> aver_rt::AverInt {
    loop {
        __state = match __state {
            __MutualTco4::SkipBlockFlat(mut pos @ _) => {
                crate::cancel_checkpoint();
                let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkEof => return pos,
                    crate::aver_generated::domain::token::Token::TkNewline => {
                        __MutualTco4::SkipBlockFlatAfterNewline(nextPos)
                    }
                    _ => __MutualTco4::SkipBlockFlat(nextPos),
                }
            }
            __MutualTco4::SkipBlockFlatAfterNewline(mut pos @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
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
                                            __MutualTco4::SkipBlockFlat(pos)
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => __MutualTco4::SkipBlockFlat(pos),
                }
            }
        };
    }
}

/// Fallback: skip until blank line, fn, or EOF.
pub fn skipBlockFlat(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    __mutual_tco_trampoline_4(__MutualTco4::SkipBlockFlat(pos), &tokens)
}

/// After newline: another newline or fn/type/record/verify/EOF = end of block.
pub fn skipBlockFlatAfterNewline(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    __mutual_tco_trampoline_4(__MutualTco4::SkipBlockFlatAfterNewline(pos), &tokens)
}

#[allow(non_camel_case_types)]
enum __MutualTco5 {
    ParseParamList(aver_rt::AverInt, aver_rt::AverList<AverStr>),
    ParseParamListTail(aver_rt::AverInt, aver_rt::AverList<AverStr>),
}

fn __mutual_tco_trampoline_5(
    mut __state: __MutualTco5,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(aver_rt::AverList<AverStr>, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco5::ParseParamList(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((acc.reverse(), pos.add(&aver_rt::AverInt::from_i64(1))));
                    }
                    crate::aver_generated::domain::token::Token::TkIdent(name) => {
                        __MutualTco5::ParseParamListTail(
                            crate::aver_generated::domain::parser_match::skipTypeAnnotation(
                                &*tokens,
                                pos.add(&aver_rt::AverInt::from_i64(1)),
                            ),
                            aver_rt::AverList::prepend(name, &acc),
                        )
                    }
                    _ => return Err(AverStr::from("Expected parameter name or ')'")),
                }
            }
            __MutualTco5::ParseParamListTail(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((acc.reverse(), pos.add(&aver_rt::AverInt::from_i64(1))));
                    }
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco5::ParseParamList(pos.add(&aver_rt::AverInt::from_i64(1)), acc)
                    }
                    _ => return Err(AverStr::from("Expected ',' or ')' in parameter list")),
                }
            }
        };
    }
}

/// Parse comma-separated parameter names until ')'.
pub fn parseParamList(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<AverStr>,
) -> Result<(aver_rt::AverList<AverStr>, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::ParseParamList(pos, acc.clone()), &tokens)
}

/// After a param name: ',' for more or ')' to end.
pub fn parseParamListTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    acc @ _: &aver_rt::AverList<AverStr>,
) -> Result<(aver_rt::AverList<AverStr>, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::ParseParamListTail(pos, acc.clone()), &tokens)
}

/// Token at position, or TkEof if past end.
#[inline(always)]
pub fn tokenAt(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::token::Token {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::parser_match::tokenAtWalk(
        tokens.clone(),
        pos,
        aver_rt::AverInt::from_i64(0),
    )
}

/// Walk the list to find token at target index.
#[inline(always)]
pub fn tokenAtWalk(
    mut tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut target @ _: aver_rt::AverInt,
    mut idx @ _: aver_rt::AverInt,
) -> crate::aver_generated::domain::token::Token {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(tokens, [] => { return crate::aver_generated::domain::token::Token::TkEof; }, [t, rest] => { if (idx == target) { return t; } else { {
            let __tco0 = rest;
            let __tco2 = idx.add(&aver_rt::AverInt::from_i64(1));
            tokens = __tco0;
            idx = __tco2;
            continue;
        } } })
    }
}

/// Check if token matches expected (structural equality via repr).
pub fn isToken(
    t @ _: &crate::aver_generated::domain::token::Token,
    expected @ _: &crate::aver_generated::domain::token::Token,
) -> bool {
    crate::cancel_checkpoint();
    (crate::aver_generated::domain::token::tokenRepr(t)
        == crate::aver_generated::domain::token::tokenRepr(expected))
}

/// Consume expected token, return new position.
#[inline(always)]
pub fn expect(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    expected @ _: &crate::aver_generated::domain::token::Token,
) -> Result<aver_rt::AverInt, AverStr> {
    crate::cancel_checkpoint();
    let t @ _ = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone());
    if crate::aver_generated::domain::parser_match::isToken(&t, expected) {
        Ok(pos.add(&aver_rt::AverInt::from_i64(1)))
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                        );
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
pub fn skipNewlines(
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
            crate::aver_generated::domain::token::Token::TkNewline => {
                let __tco1 = pos.add(&aver_rt::AverInt::from_i64(1));
                pos = __tco1;
                continue;
            }
            _ => {
                return pos;
            }
        }
    }
}

/// Skip newlines and stray DEDENT tokens at top level.
pub fn skipNewlinesAndDedents(
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
            crate::aver_generated::domain::token::Token::TkNewline => {
                let __tco1 = nextPos;
                pos = __tco1;
                continue;
            }
            crate::aver_generated::domain::token::Token::TkDedent => {
                let __tco1 = nextPos;
                pos = __tco1;
                continue;
            }
            _ => {
                return pos;
            }
        }
    }
}

/// Parse a match pattern: INT literal, _ (wildcard), or variable.
pub fn parsePattern(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    let t @ _ = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
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
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(38)).to_usize().unwrap_or(0),
                );
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
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    name @ _: AverStr,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    if (&*name == "_") {
        Ok((crate::aver_generated::domain::ast::Pattern::PatWild, pos))
    } else {
        crate::aver_generated::domain::parser_match::parseIdentPatternMaybeConstructor(
            tokens, pos, name,
        )
    }
}

/// Parse tuple pattern: (pat, pat, ...).
pub fn parseTuplePattern(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::parser_match::parseTuplePatternElements(
        tokens,
        pos,
        &aver_rt::AverList::empty(),
    )
}

/// Parse [] or [h, ..t] list pattern.
pub fn parseListPattern(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkRBracket => Ok((
            crate::aver_generated::domain::ast::Pattern::PatEmpty,
            pos.add(&aver_rt::AverInt::from_i64(1)),
        )),
        crate::aver_generated::domain::token::Token::TkIdent(head) => {
            crate::aver_generated::domain::parser_match::parseConsPattern(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
                head,
            )
        }
        _ => Err(AverStr::from("Expected identifier or ']' in list pattern")),
    }
}

/// Parse [h, ..t] after reading head ident.
pub fn parseConsPattern(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    head @ _: AverStr,
) -> Result<
    (
        crate::aver_generated::domain::ast::Pattern,
        aver_rt::AverInt,
    ),
    AverStr,
> {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser_match::expect(
        tokens,
        pos,
        &crate::aver_generated::domain::token::Token::TkComma,
    )?;
    let pos3 @ _ = crate::aver_generated::domain::parser_match::expect(
        tokens,
        pos2,
        &crate::aver_generated::domain::token::Token::TkDotDot,
    )?;
    let t @ _ = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos3.clone());
    match t {
        crate::aver_generated::domain::token::Token::TkIdent(tail) => Ok((
            crate::aver_generated::domain::ast::Pattern::PatCons(head, tail),
            crate::aver_generated::domain::parser_match::expect(
                tokens,
                pos3.add(&aver_rt::AverInt::from_i64(1)),
                &crate::aver_generated::domain::token::Token::TkRBracket,
            )?,
        )),
        _ => Err(AverStr::from("Expected tail identifier in cons pattern")),
    }
}

/// Skip optional : Type annotation.
pub fn skipTypeAnnotation(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkColon => {
            crate::aver_generated::domain::parser_match::skipTypeExpr(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
            )
        }
        _ => pos,
    }
}

/// Skip a type expression (ident, possibly with generics).
pub fn skipTypeExpr(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkIdent(_) => {
            crate::aver_generated::domain::parser_match::skipTypeExprTail(tokens.clone(), nextPos)
        }
        crate::aver_generated::domain::token::Token::TkLParen => {
            crate::aver_generated::domain::parser_match::skipUntilClose(
                tokens.clone(),
                nextPos,
                aver_rt::AverInt::from_i64(1),
            )
        }
        _ => pos,
    }
}

/// After type ident: possibly <...> generics or .Qualified.
pub fn skipTypeExprTail(
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
            crate::aver_generated::domain::token::Token::TkLt => {
                return crate::aver_generated::domain::parser_match::skipUntilGt(
                    (*tokens).clone(),
                    nextPos,
                    aver_rt::AverInt::from_i64(1),
                );
            }
            crate::aver_generated::domain::token::Token::TkDot => {
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, nextPos) {
                    crate::aver_generated::domain::token::Token::TkIdent(_) => {
                        let __tco1 = pos.add(&aver_rt::AverInt::from_i64(2));
                        pos = __tco1;
                        continue;
                    }
                    _ => {
                        return pos;
                    }
                }
            }
            _ => {
                return pos;
            }
        }
    }
}

/// Skip until matching > for generic type params.
#[inline(always)]
pub fn skipUntilGt(
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut depth @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        if (depth == aver_rt::AverInt::from_i64(0)) {
            return pos;
        } else {
            match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                crate::aver_generated::domain::token::Token::TkEof => {
                    return pos;
                }
                crate::aver_generated::domain::token::Token::TkLt => {
                    let __tco1 = nextPos;
                    let __tco2 = depth.add(&aver_rt::AverInt::from_i64(1));
                    pos = __tco1;
                    depth = __tco2;
                    continue;
                }
                crate::aver_generated::domain::token::Token::TkGt => {
                    let __tco1 = nextPos;
                    let __tco2 = depth.sub(&aver_rt::AverInt::from_i64(1));
                    pos = __tco1;
                    depth = __tco2;
                    continue;
                }
                _ => {
                    let __tco1 = nextPos;
                    pos = __tco1;
                    continue;
                }
            }
        }
    }
}

/// Skip until matching ) for tuple types.
#[inline(always)]
pub fn skipUntilClose(
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut depth @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        if (depth == aver_rt::AverInt::from_i64(0)) {
            return pos;
        } else {
            match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                crate::aver_generated::domain::token::Token::TkEof => {
                    return pos;
                }
                crate::aver_generated::domain::token::Token::TkLParen => {
                    let __tco1 = nextPos;
                    let __tco2 = depth.add(&aver_rt::AverInt::from_i64(1));
                    pos = __tco1;
                    depth = __tco2;
                    continue;
                }
                crate::aver_generated::domain::token::Token::TkRParen => {
                    let __tco1 = nextPos;
                    let __tco2 = depth.sub(&aver_rt::AverInt::from_i64(1));
                    pos = __tco1;
                    depth = __tco2;
                    continue;
                }
                _ => {
                    let __tco1 = nextPos;
                    pos = __tco1;
                    continue;
                }
            }
        }
    }
}

/// Skip optional -> ReturnType annotation.
pub fn skipReturnType(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkArrow => {
            crate::aver_generated::domain::parser_match::skipTypeExpr(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
            )
        }
        _ => pos,
    }
}

/// Skip optional ? description and ! [effects] lines (including multi-line effects).
pub fn skipDescAndEffects(
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
            crate::aver_generated::domain::token::Token::TkQuestion => {
                let __tco1 = crate::aver_generated::domain::parser_match::skipToNextLine(
                    (*tokens).clone(),
                    nextPos,
                );
                pos = __tco1;
                continue;
            }
            crate::aver_generated::domain::token::Token::TkBang => {
                let __tco1 = crate::aver_generated::domain::parser_match::skipEffectsBlock(
                    &*tokens, nextPos,
                );
                pos = __tco1;
                continue;
            }
            _ => {
                return pos;
            }
        }
    }
}

/// Skip ! [effects] block — may span multiple lines with INDENT/DEDENT.
pub fn skipEffectsBlock(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkLBracket => {
            crate::aver_generated::domain::parser_match::skipUntilRBracket(
                tokens.clone(),
                pos.add(&aver_rt::AverInt::from_i64(1)),
            )
        }
        _ => crate::aver_generated::domain::parser_match::skipToNextLine(tokens.clone(), pos),
    }
}

/// Skip tokens until ] then past any trailing newline.
pub fn skipUntilRBracket(
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
            crate::aver_generated::domain::token::Token::TkEof => {
                return pos;
            }
            crate::aver_generated::domain::token::Token::TkRBracket => {
                return crate::aver_generated::domain::parser_match::skipNewlines(
                    (*tokens).clone(),
                    nextPos,
                );
            }
            _ => {
                let __tco1 = nextPos;
                pos = __tco1;
                continue;
            }
        }
    }
}

/// Skip tokens until newline, then past it.
pub fn skipToNextLine(
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
            crate::aver_generated::domain::token::Token::TkEof => {
                return pos;
            }
            crate::aver_generated::domain::token::Token::TkNewline => {
                return nextPos;
            }
            _ => {
                let __tco1 = nextPos;
                pos = __tco1;
                continue;
            }
        }
    }
}

/// Skip tokens until matching DEDENT, or fall back to heuristic for flat streams.
pub fn skipBlock(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser_match::skipNewlines(tokens.clone(), pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkIndent => {
            crate::aver_generated::domain::parser_match::skipIndentBlock(
                tokens.clone(),
                pos2.add(&aver_rt::AverInt::from_i64(1)),
                aver_rt::AverInt::from_i64(1),
            )
        }
        crate::aver_generated::domain::token::Token::TkEof => pos2,
        _ => crate::aver_generated::domain::parser_match::skipBlockFlat(tokens, pos2),
    }
}

/// Skip until matching DEDENT.
pub fn skipIndentBlock(
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut depth @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
            crate::aver_generated::domain::token::Token::TkEof => {
                return pos;
            }
            crate::aver_generated::domain::token::Token::TkIndent => {
                let __tco1 = nextPos;
                let __tco2 = depth.add(&aver_rt::AverInt::from_i64(1));
                pos = __tco1;
                depth = __tco2;
                continue;
            }
            crate::aver_generated::domain::token::Token::TkDedent => {
                if (depth > aver_rt::AverInt::from_i64(1)) {
                    {
                        let __tco1 = nextPos;
                        let __tco2 = depth.sub(&aver_rt::AverInt::from_i64(1));
                        pos = __tco1;
                        depth = __tco2;
                        continue;
                    }
                } else {
                    return nextPos;
                }
            }
            _ => {
                let __tco1 = nextPos;
                pos = __tco1;
                continue;
            }
        }
    }
}

/// Parse module header: find depends list, skip everything else. Returns (deps, endPos).
pub fn parseModuleHeader(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> (aver_rt::AverList<AverStr>, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    let endPos @ _ = crate::aver_generated::domain::parser_match::skipBlock(tokens, pos.clone());
    let depList @ _ = crate::aver_generated::domain::parser_match::findDependsInRange(
        tokens.clone(),
        pos,
        endPos.clone(),
    );
    (depList, endPos)
}

/// Scan token range for depends [...] and extract module names.
#[inline(always)]
pub fn findDependsInRange(
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut endPos @ _: aver_rt::AverInt,
) -> aver_rt::AverList<AverStr> {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        if (pos < endPos) {
            match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos) {
                crate::aver_generated::domain::token::Token::TkIdent(kw) => {
                    if (&*kw == "depends") {
                        return crate::aver_generated::domain::parser_match::findDependsListAt(
                            &*tokens, nextPos, endPos,
                        );
                    } else {
                        {
                            let __tco1 = nextPos;
                            pos = __tco1;
                            continue;
                        }
                    }
                }
                _ => {
                    let __tco1 = nextPos;
                    pos = __tco1;
                    continue;
                }
            }
        } else {
            return aver_rt::AverList::empty();
        }
    }
}

/// Parse [Name, Name] after depends keyword.
pub fn findDependsListAt(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    endPos @ _: aver_rt::AverInt,
) -> aver_rt::AverList<AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkLBracket => {
            crate::aver_generated::domain::parser_match::collectDependsNames(
                tokens.clone(),
                pos.add(&aver_rt::AverInt::from_i64(1)),
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
    tokens @ _: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut endPos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<AverStr> {
    let tokens @ _ = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        if (pos < endPos) {
            match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos) {
                crate::aver_generated::domain::token::Token::TkRBracket => {
                    return acc.reverse();
                }
                crate::aver_generated::domain::token::Token::TkIdent(name) => {
                    let __tco1 = nextPos;
                    let __tco3 = aver_rt::AverList::prepend(name, &acc);
                    pos = __tco1;
                    acc = __tco3;
                    continue;
                }
                _ => {
                    let __tco1 = nextPos;
                    pos = __tco1;
                    continue;
                }
            }
        } else {
            return acc.reverse();
        }
    }
}

/// Check if we've reached the end of a function body (flat mode).
pub fn isBodyEndFlat(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> bool {
    crate::cancel_checkpoint();
    let t @ _ = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
    match t {
        crate::aver_generated::domain::token::Token::TkEof => true,
        crate::aver_generated::domain::token::Token::TkFn => true,
        crate::aver_generated::domain::token::Token::TkNewline => true,
        crate::aver_generated::domain::token::Token::TkDedent => true,
        _ => false,
    }
}

/// Check if position looks like a match arm start.
pub fn isArmStart(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> bool {
    crate::cancel_checkpoint();
    let t @ _ = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
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
