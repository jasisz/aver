#[allow(unused_imports)]
use crate::aver_generated::domain::lexer::chars::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;
#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    CountIndent(AverStr, i64, i64),
    CountIndentChar(AverStr, i64, i64),
}

fn __mutual_tco_trampoline_1(mut __state: __MutualTco1) -> (i64, i64) {
    loop {
        __state = match __state {
            __MutualTco1::CountIndent(mut src, mut pos, mut spaces) => {
                crate::cancel_checkpoint();
                if (pos < (src.chars().count() as i64)) {
                    __MutualTco1::CountIndentChar(src, pos, spaces)
                } else {
                    return (spaces, pos);
                }
            }
            __MutualTco1::CountIndentChar(mut src, mut pos, mut spaces) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                    Some(c) => {
                        let __dispatch_subject = c;
                        if &*__dispatch_subject == " " {
                            __MutualTco1::CountIndent(src, nextPos, (spaces + 1i64))
                        } else {
                            if &*__dispatch_subject == "\n" {
                                __MutualTco1::CountIndent(src, nextPos, 0i64)
                            } else {
                                return (spaces, pos);
                            }
                        }
                    }
                    None => return (spaces, pos),
                }
            }
        };
    }
}

/// Count leading spaces after newline. Skip blank lines (reset on another newline).
pub fn countIndent(src: AverStr, pos: i64, spaces: i64) -> (i64, i64) {
    __mutual_tco_trampoline_1(__MutualTco1::CountIndent(src, pos, spaces))
}

/// Check one character for indent counting.
pub fn countIndentChar(src: AverStr, pos: i64, spaces: i64) -> (i64, i64) {
    __mutual_tco_trampoline_1(__MutualTco1::CountIndentChar(src, pos, spaces))
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    TokenizeDefault(AverStr, AverStr, i64),
    TokenizeBraceOrSkip(AverStr, AverStr, i64),
    TokenizeChar(AverStr, AverStr, i64),
    TokenizeSome(AverStr, AverStr, i64),
    TokenizeAtPos(AverStr, i64),
    Tokenize(AverStr, i64),
}

fn __mutual_tco_trampoline_2(mut __state: __MutualTco2) -> aver_rt::AverList<Token> {
    loop {
        __state = match __state {
            __MutualTco2::TokenizeDefault(mut c, mut src, mut pos) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::lexer::chars::isDigit(c.clone()) {
                    return crate::aver_generated::domain::lexer::tokenizeDigit(src, pos);
                } else {
                    if crate::aver_generated::domain::lexer::chars::isAlpha(c.clone()) {
                        return crate::aver_generated::domain::lexer::tokenizeAlpha(src, pos);
                    } else {
                        __MutualTco2::TokenizeBraceOrSkip(c, src, pos)
                    }
                }
            }
            __MutualTco2::TokenizeBraceOrSkip(mut c, mut src, mut pos) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                if (c == crate::aver_generated::domain::lexer::openBrace()) {
                    return aver_rt::AverList::prepend(
                        crate::aver_generated::domain::token::Token::TkLBrace,
                        &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
                    );
                } else {
                    if (c == crate::aver_generated::domain::lexer::closeBrace()) {
                        return aver_rt::AverList::prepend(
                            crate::aver_generated::domain::token::Token::TkRBrace,
                            &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
                        );
                    } else {
                        __MutualTco2::Tokenize(src, nextPos)
                    }
                }
            }
            __MutualTco2::TokenizeChar(mut c, mut src, mut pos) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                {
                    let __dispatch_subject = c.clone();
                    if &*__dispatch_subject == " " {
                        __MutualTco2::Tokenize(src, nextPos)
                    } else {
                        if &*__dispatch_subject == "\n" {
                            return crate::aver_generated::domain::lexer::tokenizeNewline(
                                src, nextPos,
                            );
                        } else {
                            if &*__dispatch_subject == "/" {
                                return crate::aver_generated::domain::lexer::tokenizeSlashOrComment(
                                    src, pos,
                                );
                            } else {
                                if &*__dispatch_subject == "+" {
                                    return aver_rt::AverList::prepend(
                                        crate::aver_generated::domain::token::Token::TkPlus,
                                        &crate::aver_generated::domain::lexer::tokenize(
                                            src, nextPos,
                                        ),
                                    );
                                } else {
                                    if &*__dispatch_subject == "*" {
                                        return aver_rt::AverList::prepend(
                                            crate::aver_generated::domain::token::Token::TkStar,
                                            &crate::aver_generated::domain::lexer::tokenize(
                                                src, nextPos,
                                            ),
                                        );
                                    } else {
                                        if &*__dispatch_subject == "<" {
                                            return crate::aver_generated::domain::lexer::tokenizeLt(
                                                src, pos,
                                            );
                                        } else {
                                            if &*__dispatch_subject == ">" {
                                                return crate::aver_generated::domain::lexer::tokenizeGt(src, pos);
                                            } else {
                                                if &*__dispatch_subject == "!" {
                                                    return crate::aver_generated::domain::lexer::tokenizeBang(src, pos);
                                                } else {
                                                    if &*__dispatch_subject == "?" {
                                                        return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkQuestion, &crate::aver_generated::domain::lexer::tokenize(src, nextPos));
                                                    } else {
                                                        if &*__dispatch_subject == "\"" {
                                                            return crate::aver_generated::domain::lexer::tokenizeString(src, nextPos, AverStr::from(""));
                                                        } else {
                                                            if &*__dispatch_subject == "(" {
                                                                return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkLParen, &crate::aver_generated::domain::lexer::tokenize(src, nextPos));
                                                            } else {
                                                                if &*__dispatch_subject == ")" {
                                                                    return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkRParen, &crate::aver_generated::domain::lexer::tokenize(src, nextPos));
                                                                } else {
                                                                    if &*__dispatch_subject == "[" {
                                                                        return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkLBracket, &crate::aver_generated::domain::lexer::tokenize(src, nextPos));
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "]"
                                                                        {
                                                                            return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkRBracket, &crate::aver_generated::domain::lexer::tokenize(src, nextPos));
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "."
                                                                            {
                                                                                return crate::aver_generated::domain::lexer::tokenizeDot(src, pos);
                                                                            } else {
                                                                                if &*__dispatch_subject == "," { return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkComma, &crate::aver_generated::domain::lexer::tokenize(src, nextPos)) } else { if &*__dispatch_subject == ":" { return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkColon, &crate::aver_generated::domain::lexer::tokenize(src, nextPos)) } else { if &*__dispatch_subject == "=" { return crate::aver_generated::domain::lexer::tokenizeEq(src, pos) } else { if &*__dispatch_subject == "-" { return crate::aver_generated::domain::lexer::tokenizeMinus(src, pos) } else { __MutualTco2::TokenizeDefault(c, src, pos) } } } }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            __MutualTco2::TokenizeSome(mut c, mut src, mut pos) => {
                crate::cancel_checkpoint();
                __MutualTco2::TokenizeChar(c, src, pos)
            }
            __MutualTco2::TokenizeAtPos(mut src, mut pos) => {
                crate::cancel_checkpoint();
                match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                    None => {
                        return aver_rt::AverList::from_vec(vec![
                            crate::aver_generated::domain::token::Token::TkEof,
                        ]);
                    }
                    Some(c) => __MutualTco2::TokenizeSome(c, src, pos),
                }
            }
            __MutualTco2::Tokenize(mut src, mut pos) => {
                crate::cancel_checkpoint();
                if (pos < (src.chars().count() as i64)) {
                    __MutualTco2::TokenizeAtPos(src, pos)
                } else {
                    return aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::token::Token::TkEof,
                    ]);
                }
            }
        };
    }
}

/// Tokenize a character that is not a known single-char token.
pub fn tokenizeDefault(c: AverStr, src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_2(__MutualTco2::TokenizeDefault(c, src, pos))
}

/// Handle brace tokens or skip unknown chars.
pub fn tokenizeBraceOrSkip(c: AverStr, src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_2(__MutualTco2::TokenizeBraceOrSkip(c, src, pos))
}

/// Tokenize based on the current character.
pub fn tokenizeChar(c: AverStr, src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_2(__MutualTco2::TokenizeChar(c, src, pos))
}

/// Tokenize when charAt returned Some.
pub fn tokenizeSome(c: AverStr, src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_2(__MutualTco2::TokenizeSome(c, src, pos))
}

/// Tokenize at given position after bounds check.
pub fn tokenizeAtPos(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_2(__MutualTco2::TokenizeAtPos(src, pos))
}

/// Tokenize source string starting from pos.
pub fn tokenize(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_2(__MutualTco2::Tokenize(src, pos))
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    TokenizeInterpExpr(AverStr, i64),
    TokenizeInterpExprAt(AverStr, i64),
    TokenizeInterpExprC(AverStr, i64, AverStr),
    TokenizeInterpExprChar(AverStr, i64, AverStr),
    TokenizeInterpNonDigit(AverStr, i64, AverStr),
    TokenizeInterpPunct(AverStr, i64, AverStr),
    TokenizeInterpAlpha(AverStr, i64),
}

fn __mutual_tco_trampoline_3(mut __state: __MutualTco3) -> aver_rt::AverList<Token> {
    loop {
        __state = match __state {
            __MutualTco3::TokenizeInterpExpr(mut src, mut pos) => {
                crate::cancel_checkpoint();
                if (pos < (src.chars().count() as i64)) {
                    __MutualTco3::TokenizeInterpExprAt(src, pos)
                } else {
                    return aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::token::Token::TkInterpEnd,
                        crate::aver_generated::domain::token::Token::TkEof,
                    ]);
                }
            }
            __MutualTco3::TokenizeInterpExprAt(mut src, mut pos) => {
                crate::cancel_checkpoint();
                match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                    Some(c) => __MutualTco3::TokenizeInterpExprC(src, pos, c),
                    None => {
                        return aver_rt::AverList::from_vec(vec![
                            crate::aver_generated::domain::token::Token::TkInterpEnd,
                            crate::aver_generated::domain::token::Token::TkEof,
                        ]);
                    }
                }
            }
            __MutualTco3::TokenizeInterpExprC(mut src, mut pos, mut c) => {
                crate::cancel_checkpoint();
                if (c == crate::aver_generated::domain::lexer::closeBrace()) {
                    return aver_rt::AverList::prepend(
                        crate::aver_generated::domain::token::Token::TkInterpEnd,
                        &crate::aver_generated::domain::lexer::tokenizeString(
                            src,
                            (pos + 1i64),
                            AverStr::from(""),
                        ),
                    );
                } else {
                    __MutualTco3::TokenizeInterpExprChar(src, pos, c)
                }
            }
            __MutualTco3::TokenizeInterpExprChar(mut src, mut pos, mut c) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::lexer::chars::isDigit(c.clone()) {
                    return crate::aver_generated::domain::lexer::tokenizeInterpDigit(src, pos);
                } else {
                    __MutualTco3::TokenizeInterpNonDigit(src, pos, c)
                }
            }
            __MutualTco3::TokenizeInterpNonDigit(mut src, mut pos, mut c) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::lexer::chars::isAlpha(c.clone()) {
                    __MutualTco3::TokenizeInterpAlpha(src, pos)
                } else {
                    __MutualTco3::TokenizeInterpPunct(src, pos, c)
                }
            }
            __MutualTco3::TokenizeInterpPunct(mut src, mut pos, mut c) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                {
                    let __dispatch_subject = c;
                    if &*__dispatch_subject == " " {
                        __MutualTco3::TokenizeInterpExpr(src, nextPos)
                    } else {
                        if &*__dispatch_subject == "(" {
                            return aver_rt::AverList::prepend(
                                crate::aver_generated::domain::token::Token::TkLParen,
                                &crate::aver_generated::domain::lexer::tokenizeInterpExpr(
                                    src, nextPos,
                                ),
                            );
                        } else {
                            if &*__dispatch_subject == ")" {
                                return aver_rt::AverList::prepend(
                                    crate::aver_generated::domain::token::Token::TkRParen,
                                    &crate::aver_generated::domain::lexer::tokenizeInterpExpr(
                                        src, nextPos,
                                    ),
                                );
                            } else {
                                if &*__dispatch_subject == "+" {
                                    return aver_rt::AverList::prepend(
                                        crate::aver_generated::domain::token::Token::TkPlus,
                                        &crate::aver_generated::domain::lexer::tokenizeInterpExpr(
                                            src, nextPos,
                                        ),
                                    );
                                } else {
                                    if &*__dispatch_subject == "-" {
                                        return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkMinus, &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, nextPos));
                                    } else {
                                        if &*__dispatch_subject == "*" {
                                            return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkStar, &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, nextPos));
                                        } else {
                                            if &*__dispatch_subject == "," {
                                                return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkComma, &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, nextPos));
                                            } else {
                                                if &*__dispatch_subject == "." {
                                                    return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkDot, &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, nextPos));
                                                } else {
                                                    if &*__dispatch_subject == "[" {
                                                        return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkLBracket, &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, nextPos));
                                                    } else {
                                                        if &*__dispatch_subject == "]" {
                                                            return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkRBracket, &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, nextPos));
                                                        } else {
                                                            if &*__dispatch_subject == "\"" {
                                                                return crate::aver_generated::domain::lexer::tokenizeInterpString(src, nextPos, AverStr::from(""));
                                                            } else {
                                                                __MutualTco3::TokenizeInterpExpr(
                                                                    src, nextPos,
                                                                )
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            __MutualTco3::TokenizeInterpAlpha(mut src, mut pos) => {
                crate::cancel_checkpoint();
                match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                    Some(c) => {
                        let (word, newPos) = crate::aver_generated::domain::lexer::chars::readIdent(
                            src.clone(),
                            pos,
                            AverStr::from(""),
                            crate::aver_generated::domain::lexer::chars::isUpper(c),
                        );
                        return aver_rt::AverList::prepend(
                            crate::aver_generated::domain::lexer::chars::keywordOrIdent(word),
                            &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, newPos),
                        );
                    }
                    None => __MutualTco3::TokenizeInterpExpr(src, pos),
                }
            }
        };
    }
}

/// Tokenize expression inside interpolation braces.
pub fn tokenizeInterpExpr(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_3(__MutualTco3::TokenizeInterpExpr(src, pos))
}

/// Read one token of interpolation expression.
pub fn tokenizeInterpExprAt(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_3(__MutualTco3::TokenizeInterpExprAt(src, pos))
}

/// Dispatch interpolation char.
pub fn tokenizeInterpExprC(src: AverStr, pos: i64, c: AverStr) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_3(__MutualTco3::TokenizeInterpExprC(src, pos, c))
}

/// Tokenize one char of interpolation expression.
pub fn tokenizeInterpExprChar(src: AverStr, pos: i64, c: AverStr) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_3(__MutualTco3::TokenizeInterpExprChar(src, pos, c))
}

/// Handle non-digit char in interpolation.
pub fn tokenizeInterpNonDigit(src: AverStr, pos: i64, c: AverStr) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_3(__MutualTco3::TokenizeInterpNonDigit(src, pos, c))
}

/// Handle punctuation in interpolation.
pub fn tokenizeInterpPunct(src: AverStr, pos: i64, c: AverStr) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_3(__MutualTco3::TokenizeInterpPunct(src, pos, c))
}

/// Read identifier inside interpolation.
pub fn tokenizeInterpAlpha(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_3(__MutualTco3::TokenizeInterpAlpha(src, pos))
}

#[allow(non_camel_case_types)]
enum __MutualTco4 {
    TokenizeString(AverStr, i64, AverStr),
    TokenizeStringAt(AverStr, i64, AverStr),
    TokenizeStringEscape(AverStr, i64, AverStr),
    TokenizeStringChar(AverStr, i64, AverStr, AverStr),
    TokenizeStringCharInner(AverStr, i64, AverStr, AverStr),
    TokenizeStringMaybeEscapedBrace(AverStr, i64, AverStr),
    TokenizeStringMaybeEscapedClose(AverStr, i64, AverStr),
}

fn __mutual_tco_trampoline_4(mut __state: __MutualTco4) -> aver_rt::AverList<Token> {
    loop {
        __state = match __state {
            __MutualTco4::TokenizeString(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                if (pos < (src.chars().count() as i64)) {
                    __MutualTco4::TokenizeStringAt(src, pos, acc)
                } else {
                    return aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::token::Token::TkStr(acc),
                        crate::aver_generated::domain::token::Token::TkEof,
                    ]);
                }
            }
            __MutualTco4::TokenizeStringAt(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                    Some(c) => {
                        if (c == AverStr::from("\\")) {
                            __MutualTco4::TokenizeStringEscape(src, (pos + 1i64), acc)
                        } else {
                            __MutualTco4::TokenizeStringChar(src, pos, acc, c)
                        }
                    }
                    None => {
                        return aver_rt::AverList::from_vec(vec![
                            crate::aver_generated::domain::token::Token::TkStr(acc),
                            crate::aver_generated::domain::token::Token::TkEof,
                        ]);
                    }
                }
            }
            __MutualTco4::TokenizeStringEscape(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                    Some(c) => {
                        let __dispatch_subject = c.clone();
                        if &*__dispatch_subject == "n" {
                            __MutualTco4::TokenizeString(src, nextPos, (acc + &AverStr::from("\n")))
                        } else {
                            if &*__dispatch_subject == "t" {
                                __MutualTco4::TokenizeString(
                                    src,
                                    nextPos,
                                    (acc + &AverStr::from("\t")),
                                )
                            } else {
                                if &*__dispatch_subject == "r" {
                                    __MutualTco4::TokenizeString(
                                        src,
                                        nextPos,
                                        (acc + &AverStr::from("\r")),
                                    )
                                } else {
                                    if &*__dispatch_subject == "b" {
                                        __MutualTco4::TokenizeString(
                                            src,
                                            nextPos,
                                            (acc + &AverStr::from("\u{8}")),
                                        )
                                    } else {
                                        if &*__dispatch_subject == "f" {
                                            __MutualTco4::TokenizeString(
                                                src,
                                                nextPos,
                                                (acc + &AverStr::from("\u{c}")),
                                            )
                                        } else {
                                            if &*__dispatch_subject == "\"" {
                                                __MutualTco4::TokenizeString(
                                                    src,
                                                    nextPos,
                                                    (acc + &AverStr::from("\"")),
                                                )
                                            } else {
                                                if &*__dispatch_subject == "\\" {
                                                    __MutualTco4::TokenizeString(
                                                        src,
                                                        nextPos,
                                                        (acc + &AverStr::from("\\")),
                                                    )
                                                } else {
                                                    __MutualTco4::TokenizeString(
                                                        src,
                                                        nextPos,
                                                        (acc + &c),
                                                    )
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    None => {
                        return aver_rt::AverList::from_vec(vec![
                            crate::aver_generated::domain::token::Token::TkStr(acc),
                            crate::aver_generated::domain::token::Token::TkEof,
                        ]);
                    }
                }
            }
            __MutualTco4::TokenizeStringChar(mut src, mut pos, mut acc, mut c) => {
                crate::cancel_checkpoint();
                if (c == AverStr::from("\"")) {
                    return aver_rt::AverList::prepend(
                        crate::aver_generated::domain::token::Token::TkStr(acc),
                        &crate::aver_generated::domain::lexer::tokenize(src, (pos + 1i64)),
                    );
                } else {
                    __MutualTco4::TokenizeStringCharInner(src, pos, acc, c)
                }
            }
            __MutualTco4::TokenizeStringCharInner(mut src, mut pos, mut acc, mut c) => {
                crate::cancel_checkpoint();
                if (c == crate::aver_generated::domain::lexer::openBrace()) {
                    __MutualTco4::TokenizeStringMaybeEscapedBrace(src, pos, acc)
                } else {
                    if (c == crate::aver_generated::domain::lexer::closeBrace()) {
                        __MutualTco4::TokenizeStringMaybeEscapedClose(src, pos, acc)
                    } else {
                        __MutualTco4::TokenizeString(src, (pos + 1i64), (acc + &c))
                    }
                }
            }
            __MutualTco4::TokenizeStringMaybeEscapedBrace(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
                    Some(next) => {
                        if (next == crate::aver_generated::domain::lexer::openBrace()) {
                            __MutualTco4::TokenizeString(
                                src,
                                (pos + 2i64),
                                (acc + &crate::aver_generated::domain::lexer::openBrace()),
                            )
                        } else {
                            return crate::aver_generated::domain::lexer::tokenizeInterp(
                                src, pos, acc,
                            );
                        }
                    }
                    None => {
                        return crate::aver_generated::domain::lexer::tokenizeInterp(src, pos, acc);
                    }
                }
            }
            __MutualTco4::TokenizeStringMaybeEscapedClose(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                let accBrace = (acc + &crate::aver_generated::domain::lexer::closeBrace());
                match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
                    Some(next) => {
                        if (next == crate::aver_generated::domain::lexer::closeBrace()) {
                            __MutualTco4::TokenizeString(src, (pos + 2i64), accBrace)
                        } else {
                            __MutualTco4::TokenizeString(src, nextPos, accBrace)
                        }
                    }
                    None => __MutualTco4::TokenizeString(src, nextPos, accBrace),
                }
            }
        };
    }
}

/// Read string literal with interpolation and escape sequences.
pub fn tokenizeString(src: AverStr, pos: i64, acc: AverStr) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_4(__MutualTco4::TokenizeString(src, pos, acc))
}

/// Read one character of string.
pub fn tokenizeStringAt(src: AverStr, pos: i64, acc: AverStr) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_4(__MutualTco4::TokenizeStringAt(src, pos, acc))
}

/// Handle escape sequence in string: \n -> newline, \t -> tab, etc.
pub fn tokenizeStringEscape(src: AverStr, pos: i64, acc: AverStr) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_4(__MutualTco4::TokenizeStringEscape(src, pos, acc))
}

/// Handle one character inside a string literal.
pub fn tokenizeStringChar(
    src: AverStr,
    pos: i64,
    acc: AverStr,
    c: AverStr,
) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_4(__MutualTco4::TokenizeStringChar(src, pos, acc, c))
}

/// Check for interpolation start, { escape, or continue string.
pub fn tokenizeStringCharInner(
    src: AverStr,
    pos: i64,
    acc: AverStr,
    c: AverStr,
) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_4(__MutualTco4::TokenizeStringCharInner(src, pos, acc, c))
}

/// Check for { (escaped brace) or start interpolation.
pub fn tokenizeStringMaybeEscapedBrace(
    src: AverStr,
    pos: i64,
    acc: AverStr,
) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_4(__MutualTco4::TokenizeStringMaybeEscapedBrace(src, pos, acc))
}

/// Check for } (escaped close brace) or continue.
pub fn tokenizeStringMaybeEscapedClose(
    src: AverStr,
    pos: i64,
    acc: AverStr,
) -> aver_rt::AverList<Token> {
    __mutual_tco_trampoline_4(__MutualTco4::TokenizeStringMaybeEscapedClose(src, pos, acc))
}

/// Tokenize starting from a digit character.
pub fn tokenizeDigit(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    {
        let (n, newPos) =
            crate::aver_generated::domain::lexer::chars::readNumber(src.clone(), pos, 0i64);
        crate::aver_generated::domain::lexer::tokenizeAfterInt(src, newPos, n)
    }
}

/// After reading integer part, check for decimal point to form a float.
#[inline(always)]
pub fn tokenizeAfterInt(src: AverStr, pos: i64, n: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            if (c == AverStr::from(".")) {
                crate::aver_generated::domain::lexer::tokenizeAfterIntDot(src, pos, n)
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkInt(n),
                    &crate::aver_generated::domain::lexer::tokenize(src, pos),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkInt(n),
            &crate::aver_generated::domain::lexer::tokenize(src, pos),
        ),
    }
}

/// After integer and dot, check if next char is digit (float) or not (int + dot).
#[inline(always)]
pub fn tokenizeAfterIntDot(src: AverStr, pos: i64, n: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
        Some(d) => {
            if crate::aver_generated::domain::lexer::chars::isDigit(d) {
                crate::aver_generated::domain::lexer::tokenizeFloat(src, nextPos, n)
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkInt(n),
                    &crate::aver_generated::domain::lexer::tokenize(src, pos),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkInt(n),
            &crate::aver_generated::domain::lexer::tokenize(src, pos),
        ),
    }
}

/// Read decimal digits and build float token.
pub fn tokenizeFloat(src: AverStr, pos: i64, intPart: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    {
        let (decPart, newPos) =
            crate::aver_generated::domain::lexer::chars::readNumber(src.clone(), pos, 0i64);
        crate::aver_generated::domain::lexer::buildFloat(
            src,
            newPos.clone(),
            intPart,
            decPart,
            (newPos - pos),
        )
    }
}

/// Construct float from integer and decimal parts.
pub fn buildFloat(
    src: AverStr,
    pos: i64,
    intPart: i64,
    decPart: i64,
    decDigits: i64,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let f = (intPart as f64
        + (decPart as f64 / crate::aver_generated::domain::lexer::pow10(decDigits)));
    aver_rt::AverList::prepend(
        crate::aver_generated::domain::token::Token::TkFloat(f),
        &crate::aver_generated::domain::lexer::tokenize(src, pos),
    )
}

/// Compute 10^n as Float.
#[inline(always)]
pub fn pow10(n: i64) -> f64 {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::pow10Acc(n, 1.0f64)
}

/// Accumulate 10^n as Float.
#[inline(always)]
pub fn pow10Acc(mut n: i64, mut acc: f64) -> f64 {
    loop {
        crate::cancel_checkpoint();
        if (n > 0i64) {
            {
                let __tco0 = (n - 1i64);
                let __tco1 = (acc * 10.0f64);
                n = __tco0;
                acc = __tco1;
                continue;
            }
        } else {
            return acc;
        }
    }
}

/// Tokenize starting from an alpha character.
#[inline(always)]
pub fn tokenizeAlpha(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => crate::aver_generated::domain::lexer::tokenizeAlphaWith(
            src,
            pos,
            crate::aver_generated::domain::lexer::chars::isUpper(c),
        ),
        None => {
            aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::token::Token::TkEof])
        }
    }
}

/// Tokenize identifier with known dotted mode.
pub fn tokenizeAlphaWith(src: AverStr, pos: i64, dotted: bool) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    {
        let (word, newPos) = crate::aver_generated::domain::lexer::chars::readIdent(
            src.clone(),
            pos,
            AverStr::from(""),
            dotted,
        );
        aver_rt::AverList::prepend(
            crate::aver_generated::domain::lexer::chars::keywordOrIdent(word),
            &crate::aver_generated::domain::lexer::tokenize(src, newPos),
        )
    }
}

/// Check if a character is the greater-than sign.
pub fn isGreaterThan(c: AverStr) -> bool {
    crate::cancel_checkpoint();
    (&*c == ">")
}

/// Tokenize a minus or arrow token.
#[inline(always)]
pub fn tokenizeMinus(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            if crate::aver_generated::domain::lexer::isGreaterThan(c) {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkArrow,
                    &crate::aver_generated::domain::lexer::tokenize(src, (pos + 2i64)),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkMinus,
                    &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkMinus,
            &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
        ),
    }
}

/// Return the opening brace character.
#[inline(always)]
pub fn openBrace() -> AverStr {
    crate::cancel_checkpoint();
    (char::from_u32(123i64 as u32).map(|c| c.to_string()))
        .into_aver()
        .unwrap_or(AverStr::from("x"))
}

/// Start interpolation: emit accumulated string, TkInterpStart, expr tokens, TkInterpEnd.
#[inline(always)]
pub fn tokenizeInterp(src: AverStr, pos: i64, acc: AverStr) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    aver_rt::AverList::prepend(
        crate::aver_generated::domain::token::Token::TkStr(acc),
        &aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkInterpStart,
            &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, (pos + 1i64)),
        ),
    )
}

/// Return the closing brace character.
#[inline(always)]
pub fn closeBrace() -> AverStr {
    crate::cancel_checkpoint();
    (char::from_u32(125i64 as u32).map(|c| c.to_string()))
        .into_aver()
        .unwrap_or(AverStr::from("x"))
}

/// Read string literal inside interpolation braces.
#[inline(always)]
pub fn tokenizeInterpString(
    mut src: AverStr,
    mut pos: i64,
    mut acc: AverStr,
) -> aver_rt::AverList<Token> {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        if (pos < (src.chars().count() as i64)) {
            match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                Some(c) => {
                    if (c == AverStr::from("\"")) {
                        return aver_rt::AverList::prepend(
                            crate::aver_generated::domain::token::Token::TkStr(acc),
                            &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, nextPos),
                        );
                    } else {
                        {
                            let __tco1 = nextPos;
                            let __tco2 = (acc + &c);
                            pos = __tco1;
                            acc = __tco2;
                            continue;
                        }
                    }
                }
                None => {
                    return aver_rt::AverList::prepend(
                        crate::aver_generated::domain::token::Token::TkStr(acc),
                        &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, pos),
                    );
                }
            }
        } else {
            return aver_rt::AverList::prepend(
                crate::aver_generated::domain::token::Token::TkStr(acc),
                &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, pos),
            );
        }
    }
}

/// Read number inside interpolation; may be an int or a float literal.
pub fn tokenizeInterpDigit(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    {
        let (n, newPos) =
            crate::aver_generated::domain::lexer::chars::readNumber(src.clone(), pos, 0i64);
        crate::aver_generated::domain::lexer::tokenizeInterpAfterInt(src, newPos, n)
    }
}

/// After integer part inside interpolation, check for a decimal point.
#[inline(always)]
pub fn tokenizeInterpAfterInt(src: AverStr, pos: i64, n: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            if (c == AverStr::from(".")) {
                crate::aver_generated::domain::lexer::tokenizeInterpAfterIntDot(src, pos, n)
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkInt(n),
                    &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, pos),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkInt(n),
            &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, pos),
        ),
    }
}

/// After integer and dot inside interpolation: digit -> float, else int + dot.
#[inline(always)]
pub fn tokenizeInterpAfterIntDot(src: AverStr, pos: i64, n: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
        Some(d) => {
            if crate::aver_generated::domain::lexer::chars::isDigit(d) {
                crate::aver_generated::domain::lexer::tokenizeInterpFloat(src, nextPos, n)
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkInt(n),
                    &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, pos),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkInt(n),
            &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, pos),
        ),
    }
}

/// Read decimal digits and build a float token inside interpolation.
pub fn tokenizeInterpFloat(src: AverStr, pos: i64, intPart: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    {
        let (decPart, newPos) =
            crate::aver_generated::domain::lexer::chars::readNumber(src.clone(), pos, 0i64);
        crate::aver_generated::domain::lexer::tokenizeInterpBuildFloat(
            src,
            newPos.clone(),
            intPart,
            decPart,
            (newPos - pos),
        )
    }
}

/// Construct a float from integer and decimal parts inside interpolation.
pub fn tokenizeInterpBuildFloat(
    src: AverStr,
    pos: i64,
    intPart: i64,
    decPart: i64,
    decDigits: i64,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let f = (intPart as f64
        + (decPart as f64 / crate::aver_generated::domain::lexer::pow10(decDigits)));
    aver_rt::AverList::prepend(
        crate::aver_generated::domain::token::Token::TkFloat(f),
        &crate::aver_generated::domain::lexer::tokenizeInterpExpr(src, pos),
    )
}

/// Tokenize / (division) or // (line comment).
#[inline(always)]
pub fn tokenizeSlashOrComment(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            if (c == AverStr::from("/")) {
                crate::aver_generated::domain::lexer::skipLineComment(src, (pos + 2i64))
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkSlash,
                    &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkSlash,
            &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
        ),
    }
}

/// Skip characters until newline or EOF. Newline goes through indent handling.
#[inline(always)]
pub fn skipLineComment(mut src: AverStr, mut pos: i64) -> aver_rt::AverList<Token> {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        if (pos < (src.chars().count() as i64)) {
            match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                Some(c) => {
                    if (c == AverStr::from("\n")) {
                        return crate::aver_generated::domain::lexer::tokenizeNewline(src, nextPos);
                    } else {
                        {
                            let __tco1 = nextPos;
                            pos = __tco1;
                            continue;
                        }
                    }
                }
                None => {
                    return aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::token::Token::TkEof,
                    ]);
                }
            }
        } else {
            return aver_rt::AverList::from_vec(vec![
                crate::aver_generated::domain::token::Token::TkEof,
            ]);
        }
    }
}

/// Tokenize . (field access) or .. (rest pattern).
#[inline(always)]
pub fn tokenizeDot(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            if (c == AverStr::from(".")) {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkDotDot,
                    &crate::aver_generated::domain::lexer::tokenize(src, (pos + 2i64)),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkDot,
                    &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkDot,
            &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
        ),
    }
}

/// Tokenize < or <=.
#[inline(always)]
pub fn tokenizeLt(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            if (c == AverStr::from("=")) {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkLte,
                    &crate::aver_generated::domain::lexer::tokenize(src, (pos + 2i64)),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkLt,
                    &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkLt,
            &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
        ),
    }
}

/// Tokenize > or >=.
#[inline(always)]
pub fn tokenizeGt(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            if (c == AverStr::from("=")) {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkGte,
                    &crate::aver_generated::domain::lexer::tokenize(src, (pos + 2i64)),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkGt,
                    &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkGt,
            &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
        ),
    }
}

/// Tokenize ! or !=.
#[inline(always)]
pub fn tokenizeBang(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            if (c == AverStr::from("=")) {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkNeq,
                    &crate::aver_generated::domain::lexer::tokenize(src, (pos + 2i64)),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkBang,
                    &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkBang,
            &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
        ),
    }
}

/// Tokenize =, ==, or =>.
#[inline(always)]
pub fn tokenizeEq(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    let pos2 = (pos + 2i64);
    match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            let __dispatch_subject = c;
            if &*__dispatch_subject == "=" {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkEqEq,
                    &crate::aver_generated::domain::lexer::tokenize(src, pos2),
                )
            } else {
                if &*__dispatch_subject == ">" {
                    aver_rt::AverList::prepend(
                        crate::aver_generated::domain::token::Token::TkFatArrow,
                        &crate::aver_generated::domain::lexer::tokenize(src, pos2),
                    )
                } else {
                    aver_rt::AverList::prepend(
                        crate::aver_generated::domain::token::Token::TkEq,
                        &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
                    )
                }
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkEq,
            &crate::aver_generated::domain::lexer::tokenize(src, nextPos),
        ),
    }
}

/// Handle newline: count indent of next line, emit NEWLINE + raw indent marker.
pub fn tokenizeNewline(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let r = crate::aver_generated::domain::lexer::countIndent(src.clone(), pos, 0i64);
    {
        let (indent, newPos) = r;
        aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkNewline,
            &aver_rt::AverList::prepend(
                crate::aver_generated::domain::token::Token::TkInt(((0i64 - indent) - 1i64)),
                &crate::aver_generated::domain::lexer::tokenize(src, newPos),
            ),
        )
    }
}

/// Tokenize a complete source string with INDENT/DEDENT.
pub fn lex(src: AverStr) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let raw = crate::aver_generated::domain::lexer::tokenize(src, 0i64);
    let processed = crate::aver_generated::domain::lexer::processIndentation(
        &raw,
        &aver_rt::AverList::from_vec(vec![0i64]),
    );
    processed
}

/// Convert raw indent markers (negative TkInt after TkNewline) into INDENT/DEDENT tokens.
#[inline(always)]
pub fn processIndentation(
    tokens: &aver_rt::AverList<Token>,
    stack: &aver_rt::AverList<i64>,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    aver_list_match!(tokens.clone(), [] => crate::aver_generated::domain::lexer::emitFinalDedents(stack), [t, rest] => crate::aver_generated::domain::lexer::processIndentToken(&t, &rest, stack))
}

/// Process one token in the indentation pass.
pub fn processIndentToken(
    t: &Token,
    rest: &aver_rt::AverList<Token>,
    stack: &aver_rt::AverList<i64>,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    match t {
        crate::aver_generated::domain::token::Token::TkNewline => {
            crate::aver_generated::domain::lexer::processAfterNewline(rest, stack)
        }
        crate::aver_generated::domain::token::Token::TkEof => aver_rt::AverList::concat(
            &crate::aver_generated::domain::lexer::emitFinalDedents(stack),
            &aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::token::Token::TkEof]),
        ),
        _ => aver_rt::AverList::prepend(
            t.clone(),
            &crate::aver_generated::domain::lexer::processIndentation(rest, stack),
        ),
    }
}

/// After TkNewline, check for raw indent marker (negative TkInt).
#[inline(always)]
pub fn processAfterNewline(
    tokens: &aver_rt::AverList<Token>,
    stack: &aver_rt::AverList<i64>,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    aver_list_match!(tokens.clone(), [] => aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkNewline, &crate::aver_generated::domain::lexer::emitFinalDedents(stack)), [t, rest] => crate::aver_generated::domain::lexer::processAfterNewlineToken(&t, &rest, stack))
}

/// Check if token after newline is a raw indent marker.
pub fn processAfterNewlineToken(
    t: &Token,
    rest: &aver_rt::AverList<Token>,
    stack: &aver_rt::AverList<i64>,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    match t.clone() {
        crate::aver_generated::domain::token::Token::TkInt(n) => {
            if (n < 0i64) {
                crate::aver_generated::domain::lexer::emitIndentChange(
                    ((0i64 - n) - 1i64),
                    rest,
                    stack,
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkNewline,
                    &aver_rt::AverList::prepend(
                        t.clone(),
                        &crate::aver_generated::domain::lexer::processIndentation(rest, stack),
                    ),
                )
            }
        }
        _ => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkNewline,
            &aver_rt::AverList::prepend(
                t.clone(),
                &crate::aver_generated::domain::lexer::processIndentation(rest, stack),
            ),
        ),
    }
}

/// Compare indent level to stack top and emit INDENT, DEDENT, or NEWLINE.
#[inline(always)]
pub fn emitIndentChange(
    indent: i64,
    rest: &aver_rt::AverList<Token>,
    stack: &aver_rt::AverList<i64>,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let currentIndent = crate::aver_generated::domain::lexer::stackTop(stack);
    if (indent > currentIndent) {
        aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkNewline,
            &aver_rt::AverList::prepend(
                crate::aver_generated::domain::token::Token::TkIndent,
                &crate::aver_generated::domain::lexer::processIndentation(
                    rest,
                    &aver_rt::AverList::prepend(indent, &stack.clone()),
                ),
            ),
        )
    } else {
        if (indent < currentIndent) {
            crate::aver_generated::domain::lexer::emitDedents(indent, rest, stack)
        } else {
            aver_rt::AverList::prepend(
                crate::aver_generated::domain::token::Token::TkNewline,
                &crate::aver_generated::domain::lexer::processIndentation(rest, stack),
            )
        }
    }
}

/// Emit DEDENT tokens until stack matches target indent.
pub fn emitDedents(
    targetIndent: i64,
    rest: &aver_rt::AverList<Token>,
    stack: &aver_rt::AverList<i64>,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::emitDedentsAcc(
        targetIndent,
        rest.clone(),
        stack.clone(),
        aver_rt::AverList::empty(),
    )
}

/// Accumulate DEDENT tokens until stack matches target indent.
#[inline(always)]
pub fn emitDedentsAcc(
    mut targetIndent: i64,
    mut rest: aver_rt::AverList<Token>,
    mut stack: aver_rt::AverList<i64>,
    mut acc: aver_rt::AverList<Token>,
) -> aver_rt::AverList<Token> {
    loop {
        crate::cancel_checkpoint();
        let reversed = acc.reverse();
        aver_list_match!(stack.clone(), [] => { return aver_rt::AverList::concat(&reversed, &crate::aver_generated::domain::lexer::processIndentation(&rest, &aver_rt::AverList::from_vec(vec![0i64]))); }, [top, below] => { if (top > targetIndent) { {
            let __tco2 = below;
            let __tco3 = aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkDedent, &acc);
            stack = __tco2;
            acc = __tco3;
            continue;
        } } else { return aver_rt::AverList::concat(&reversed, &aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkNewline, &crate::aver_generated::domain::lexer::processIndentation(&rest, &stack))); } })
    }
}

/// At EOF, emit DEDENT for each indent level above 0.
#[inline(always)]
pub fn emitFinalDedents(stack: &aver_rt::AverList<i64>) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::emitFinalDedentsAcc(
        stack.clone(),
        aver_rt::AverList::empty(),
    )
}

/// Accumulate DEDENT tokens for each indent level above 0.
#[inline(always)]
pub fn emitFinalDedentsAcc(
    mut stack: aver_rt::AverList<i64>,
    mut acc: aver_rt::AverList<Token>,
) -> aver_rt::AverList<Token> {
    loop {
        crate::cancel_checkpoint();
        let reversed = acc.reverse();
        aver_list_match!(stack, [] => { return reversed; }, [top, rest] => { if (top > 0i64) { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkDedent, &acc);
            stack = __tco0;
            acc = __tco1;
            continue;
        } } else { return reversed; } })
    }
}

/// Return top of indent stack, or 0 if empty.
#[inline(always)]
pub fn stackTop(stack: &aver_rt::AverList<i64>) -> i64 {
    crate::cancel_checkpoint();
    aver_list_match!(stack.clone(), [] => 0i64, [top, rest] => top)
}

pub mod chars;
