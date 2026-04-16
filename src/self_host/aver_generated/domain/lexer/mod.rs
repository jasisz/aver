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
                if (pos >= (src.chars().count() as i64)) {
                    return (spaces, pos);
                } else {
                    __MutualTco1::CountIndentChar(src, pos, spaces)
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
                    return tokenizeDigit(src, pos);
                } else {
                    if crate::aver_generated::domain::lexer::chars::isAlpha(c.clone()) {
                        return tokenizeAlpha(src, pos);
                    } else {
                        __MutualTco2::TokenizeBraceOrSkip(c, src, pos)
                    }
                }
            }
            __MutualTco2::TokenizeBraceOrSkip(mut c, mut src, mut pos) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                if (c == openBrace()) {
                    return aver_rt::AverList::prepend(
                        Token::TkLBrace.clone(),
                        &tokenize(src, nextPos),
                    );
                } else {
                    if (c == closeBrace()) {
                        return aver_rt::AverList::prepend(
                            Token::TkRBrace.clone(),
                            &tokenize(src, nextPos),
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
                            return tokenizeNewline(src, nextPos);
                        } else {
                            if &*__dispatch_subject == "/" {
                                return tokenizeSlashOrComment(src, pos);
                            } else {
                                if &*__dispatch_subject == "+" {
                                    return aver_rt::AverList::prepend(
                                        Token::TkPlus.clone(),
                                        &tokenize(src, nextPos),
                                    );
                                } else {
                                    if &*__dispatch_subject == "*" {
                                        return aver_rt::AverList::prepend(
                                            Token::TkStar.clone(),
                                            &tokenize(src, nextPos),
                                        );
                                    } else {
                                        if &*__dispatch_subject == "<" {
                                            return tokenizeLt(src, pos);
                                        } else {
                                            if &*__dispatch_subject == ">" {
                                                return tokenizeGt(src, pos);
                                            } else {
                                                if &*__dispatch_subject == "!" {
                                                    return tokenizeBang(src, pos);
                                                } else {
                                                    if &*__dispatch_subject == "?" {
                                                        return aver_rt::AverList::prepend(
                                                            Token::TkQuestion.clone(),
                                                            &tokenize(src, nextPos),
                                                        );
                                                    } else {
                                                        if &*__dispatch_subject == "\"" {
                                                            return tokenizeString(
                                                                src,
                                                                nextPos,
                                                                AverStr::from(""),
                                                            );
                                                        } else {
                                                            if &*__dispatch_subject == "(" {
                                                                return aver_rt::AverList::prepend(
                                                                    Token::TkLParen.clone(),
                                                                    &tokenize(src, nextPos),
                                                                );
                                                            } else {
                                                                if &*__dispatch_subject == ")" {
                                                                    return aver_rt::AverList::prepend(Token::TkRParen.clone(), &tokenize(src, nextPos));
                                                                } else {
                                                                    if &*__dispatch_subject == "[" {
                                                                        return aver_rt::AverList::prepend(Token::TkLBracket.clone(), &tokenize(src, nextPos));
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "]"
                                                                        {
                                                                            return aver_rt::AverList::prepend(Token::TkRBracket.clone(), &tokenize(src, nextPos));
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "."
                                                                            {
                                                                                return tokenizeDot(
                                                                                    src, pos,
                                                                                );
                                                                            } else {
                                                                                if &*__dispatch_subject == "," { return aver_rt::AverList::prepend(Token::TkComma.clone(), &tokenize(src, nextPos)) } else { if &*__dispatch_subject == ":" { return aver_rt::AverList::prepend(Token::TkColon.clone(), &tokenize(src, nextPos)) } else { if &*__dispatch_subject == "=" { return tokenizeEq(src, pos) } else { if &*__dispatch_subject == "-" { return tokenizeMinus(src, pos) } else { __MutualTco2::TokenizeDefault(c, src, pos) } } } }
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
                    None => return aver_rt::AverList::from_vec(vec![Token::TkEof.clone()]),
                    Some(c) => __MutualTco2::TokenizeSome(c, src, pos),
                }
            }
            __MutualTco2::Tokenize(mut src, mut pos) => {
                crate::cancel_checkpoint();
                if (pos >= (src.chars().count() as i64)) {
                    return aver_rt::AverList::from_vec(vec![Token::TkEof.clone()]);
                } else {
                    __MutualTco2::TokenizeAtPos(src, pos)
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
                if (pos >= (src.chars().count() as i64)) {
                    return aver_rt::AverList::from_vec(vec![
                        Token::TkInterpEnd.clone(),
                        Token::TkEof.clone(),
                    ]);
                } else {
                    __MutualTco3::TokenizeInterpExprAt(src, pos)
                }
            }
            __MutualTco3::TokenizeInterpExprAt(mut src, mut pos) => {
                crate::cancel_checkpoint();
                match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                    Some(c) => __MutualTco3::TokenizeInterpExprC(src, pos, c),
                    None => {
                        return aver_rt::AverList::from_vec(vec![
                            Token::TkInterpEnd.clone(),
                            Token::TkEof.clone(),
                        ]);
                    }
                }
            }
            __MutualTco3::TokenizeInterpExprC(mut src, mut pos, mut c) => {
                crate::cancel_checkpoint();
                if (c == closeBrace()) {
                    return aver_rt::AverList::prepend(
                        Token::TkInterpEnd.clone(),
                        &tokenizeString(src, (pos + 1i64), AverStr::from("")),
                    );
                } else {
                    __MutualTco3::TokenizeInterpExprChar(src, pos, c)
                }
            }
            __MutualTco3::TokenizeInterpExprChar(mut src, mut pos, mut c) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::lexer::chars::isDigit(c.clone()) {
                    return tokenizeInterpDigit(src, pos);
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
                                Token::TkLParen.clone(),
                                &tokenizeInterpExpr(src, nextPos),
                            );
                        } else {
                            if &*__dispatch_subject == ")" {
                                return aver_rt::AverList::prepend(
                                    Token::TkRParen.clone(),
                                    &tokenizeInterpExpr(src, nextPos),
                                );
                            } else {
                                if &*__dispatch_subject == "+" {
                                    return aver_rt::AverList::prepend(
                                        Token::TkPlus.clone(),
                                        &tokenizeInterpExpr(src, nextPos),
                                    );
                                } else {
                                    if &*__dispatch_subject == "-" {
                                        return aver_rt::AverList::prepend(
                                            Token::TkMinus.clone(),
                                            &tokenizeInterpExpr(src, nextPos),
                                        );
                                    } else {
                                        if &*__dispatch_subject == "*" {
                                            return aver_rt::AverList::prepend(
                                                Token::TkStar.clone(),
                                                &tokenizeInterpExpr(src, nextPos),
                                            );
                                        } else {
                                            if &*__dispatch_subject == "," {
                                                return aver_rt::AverList::prepend(
                                                    Token::TkComma.clone(),
                                                    &tokenizeInterpExpr(src, nextPos),
                                                );
                                            } else {
                                                if &*__dispatch_subject == "." {
                                                    return aver_rt::AverList::prepend(
                                                        Token::TkDot.clone(),
                                                        &tokenizeInterpExpr(src, nextPos),
                                                    );
                                                } else {
                                                    if &*__dispatch_subject == "[" {
                                                        return aver_rt::AverList::prepend(
                                                            Token::TkLBracket.clone(),
                                                            &tokenizeInterpExpr(src, nextPos),
                                                        );
                                                    } else {
                                                        if &*__dispatch_subject == "]" {
                                                            return aver_rt::AverList::prepend(
                                                                Token::TkRBracket.clone(),
                                                                &tokenizeInterpExpr(src, nextPos),
                                                            );
                                                        } else {
                                                            if &*__dispatch_subject == "\"" {
                                                                return tokenizeInterpString(
                                                                    src,
                                                                    nextPos,
                                                                    AverStr::from(""),
                                                                );
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
                        match crate::aver_generated::domain::lexer::chars::readIdent(
                            src.clone(),
                            pos,
                            AverStr::from(""),
                            crate::aver_generated::domain::lexer::chars::isUpper(c),
                        ) {
                            (word, newPos) => {
                                return aver_rt::AverList::prepend(
                                    crate::aver_generated::domain::lexer::chars::keywordOrIdent(
                                        word,
                                    ),
                                    &tokenizeInterpExpr(src, newPos),
                                );
                            }
                        }
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
                if (pos >= (src.chars().count() as i64)) {
                    return aver_rt::AverList::from_vec(vec![
                        Token::TkStr(acc),
                        Token::TkEof.clone(),
                    ]);
                } else {
                    __MutualTco4::TokenizeStringAt(src, pos, acc)
                }
            }
            __MutualTco4::TokenizeStringAt(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                    Some(c) => {
                        if (&*c == "\\") {
                            __MutualTco4::TokenizeStringEscape(src, (pos + 1i64), acc)
                        } else {
                            __MutualTco4::TokenizeStringChar(src, pos, acc, c)
                        }
                    }
                    None => {
                        return aver_rt::AverList::from_vec(vec![
                            Token::TkStr(acc),
                            Token::TkEof.clone(),
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
                            Token::TkStr(acc),
                            Token::TkEof.clone(),
                        ]);
                    }
                }
            }
            __MutualTco4::TokenizeStringChar(mut src, mut pos, mut acc, mut c) => {
                crate::cancel_checkpoint();
                if (&*c == "\"") {
                    return aver_rt::AverList::prepend(
                        Token::TkStr(acc),
                        &tokenize(src, (pos + 1i64)),
                    );
                } else {
                    __MutualTco4::TokenizeStringCharInner(src, pos, acc, c)
                }
            }
            __MutualTco4::TokenizeStringCharInner(mut src, mut pos, mut acc, mut c) => {
                crate::cancel_checkpoint();
                if (c == openBrace()) {
                    __MutualTco4::TokenizeStringMaybeEscapedBrace(src, pos, acc)
                } else {
                    if (c == closeBrace()) {
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
                        if (next == openBrace()) {
                            __MutualTco4::TokenizeString(src, (pos + 2i64), (acc + &openBrace()))
                        } else {
                            return tokenizeInterp(src, pos, acc);
                        }
                    }
                    None => return tokenizeInterp(src, pos, acc),
                }
            }
            __MutualTco4::TokenizeStringMaybeEscapedClose(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let nextPos = (pos + 1i64);
                let accBrace = (acc + &closeBrace());
                match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
                    Some(next) => {
                        if (next == closeBrace()) {
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
        tokenizeAfterInt(src, newPos, n)
    }
}

/// After reading integer part, check for decimal point to form a float.
#[inline(always)]
pub fn tokenizeAfterInt(src: AverStr, pos: i64, n: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            if (c == AverStr::from(".")) {
                tokenizeAfterIntDot(src, pos, n)
            } else {
                aver_rt::AverList::prepend(Token::TkInt(n), &tokenize(src, pos))
            }
        }
        None => aver_rt::AverList::prepend(Token::TkInt(n), &tokenize(src, pos)),
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
                tokenizeFloat(src, nextPos, n)
            } else {
                aver_rt::AverList::prepend(Token::TkInt(n), &tokenize(src, pos))
            }
        }
        None => aver_rt::AverList::prepend(Token::TkInt(n), &tokenize(src, pos)),
    }
}

/// Read decimal digits and build float token.
pub fn tokenizeFloat(src: AverStr, pos: i64, intPart: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    {
        let (decPart, newPos) =
            crate::aver_generated::domain::lexer::chars::readNumber(src.clone(), pos, 0i64);
        buildFloat(src, newPos.clone(), intPart, decPart, (newPos - pos))
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
    let f = ((intPart as f64) + ((decPart as f64) / pow10(decDigits)));
    aver_rt::AverList::prepend(Token::TkFloat(f), &tokenize(src, pos))
}

/// Compute 10^n as Float.
#[inline(always)]
pub fn pow10(n: i64) -> f64 {
    crate::cancel_checkpoint();
    pow10Acc(n, 1.0f64)
}

/// Accumulate 10^n as Float.
#[inline(always)]
pub fn pow10Acc(mut n: i64, mut acc: f64) -> f64 {
    loop {
        crate::cancel_checkpoint();
        return if (n <= 0i64) {
            acc
        } else {
            {
                let __tmp0 = (n - 1i64);
                let __tmp1 = (acc * 10.0f64);
                n = __tmp0;
                acc = __tmp1;
                continue;
            }
        };
    }
}

/// Tokenize starting from an alpha character.
#[inline(always)]
pub fn tokenizeAlpha(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => tokenizeAlphaWith(
            src,
            pos,
            crate::aver_generated::domain::lexer::chars::isUpper(c),
        ),
        None => aver_rt::AverList::from_vec(vec![Token::TkEof.clone()]),
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
            &tokenize(src, newPos),
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
            if isGreaterThan(c) {
                aver_rt::AverList::prepend(Token::TkArrow.clone(), &tokenize(src, (pos + 2i64)))
            } else {
                aver_rt::AverList::prepend(Token::TkMinus.clone(), &tokenize(src, nextPos))
            }
        }
        None => aver_rt::AverList::prepend(Token::TkMinus.clone(), &tokenize(src, nextPos)),
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
        Token::TkStr(acc),
        &aver_rt::AverList::prepend(
            Token::TkInterpStart.clone(),
            &tokenizeInterpExpr(src, (pos + 1i64)),
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
        return if (pos >= (src.chars().count() as i64)) {
            aver_rt::AverList::prepend(Token::TkStr(acc), &tokenizeInterpExpr(src, pos))
        } else {
            match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                Some(c) => {
                    if (&*c == "\"") {
                        aver_rt::AverList::prepend(
                            Token::TkStr(acc),
                            &tokenizeInterpExpr(src, nextPos),
                        )
                    } else {
                        {
                            let __tmp2 = (acc + &c);
                            pos = nextPos;
                            acc = __tmp2;
                            continue;
                        }
                    }
                }
                None => {
                    aver_rt::AverList::prepend(Token::TkStr(acc), &tokenizeInterpExpr(src, pos))
                }
            }
        };
    }
}

/// Read number inside interpolation.
pub fn tokenizeInterpDigit(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    {
        let (n, newPos) =
            crate::aver_generated::domain::lexer::chars::readNumber(src.clone(), pos, 0i64);
        aver_rt::AverList::prepend(Token::TkInt(n), &tokenizeInterpExpr(src, newPos))
    }
}

/// Tokenize / (division) or // (line comment).
#[inline(always)]
pub fn tokenizeSlashOrComment(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let nextPos = (pos + 1i64);
    match (src.chars().nth(nextPos as usize).map(|c| c.to_string())).into_aver() {
        Some(c) => {
            if (c == AverStr::from("/")) {
                skipLineComment(src, (pos + 2i64))
            } else {
                aver_rt::AverList::prepend(Token::TkSlash.clone(), &tokenize(src, nextPos))
            }
        }
        None => aver_rt::AverList::prepend(Token::TkSlash.clone(), &tokenize(src, nextPos)),
    }
}

/// Skip characters until newline or EOF. Newline goes through indent handling.
#[inline(always)]
pub fn skipLineComment(mut src: AverStr, mut pos: i64) -> aver_rt::AverList<Token> {
    loop {
        crate::cancel_checkpoint();
        let nextPos = (pos + 1i64);
        return if (pos >= (src.chars().count() as i64)) {
            aver_rt::AverList::from_vec(vec![Token::TkEof.clone()])
        } else {
            match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() {
                Some(c) => {
                    if (&*c == "\n") {
                        tokenizeNewline(src, nextPos)
                    } else {
                        {
                            pos = nextPos;
                            continue;
                        }
                    }
                }
                None => aver_rt::AverList::from_vec(vec![Token::TkEof.clone()]),
            }
        };
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
                aver_rt::AverList::prepend(Token::TkDotDot.clone(), &tokenize(src, (pos + 2i64)))
            } else {
                aver_rt::AverList::prepend(Token::TkDot.clone(), &tokenize(src, nextPos))
            }
        }
        None => aver_rt::AverList::prepend(Token::TkDot.clone(), &tokenize(src, nextPos)),
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
                aver_rt::AverList::prepend(Token::TkLte.clone(), &tokenize(src, (pos + 2i64)))
            } else {
                aver_rt::AverList::prepend(Token::TkLt.clone(), &tokenize(src, nextPos))
            }
        }
        None => aver_rt::AverList::prepend(Token::TkLt.clone(), &tokenize(src, nextPos)),
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
                aver_rt::AverList::prepend(Token::TkGte.clone(), &tokenize(src, (pos + 2i64)))
            } else {
                aver_rt::AverList::prepend(Token::TkGt.clone(), &tokenize(src, nextPos))
            }
        }
        None => aver_rt::AverList::prepend(Token::TkGt.clone(), &tokenize(src, nextPos)),
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
                aver_rt::AverList::prepend(Token::TkNeq.clone(), &tokenize(src, (pos + 2i64)))
            } else {
                aver_rt::AverList::prepend(Token::TkBang.clone(), &tokenize(src, nextPos))
            }
        }
        None => aver_rt::AverList::prepend(Token::TkBang.clone(), &tokenize(src, nextPos)),
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
                aver_rt::AverList::prepend(Token::TkEqEq.clone(), &tokenize(src, pos2))
            } else {
                if &*__dispatch_subject == ">" {
                    aver_rt::AverList::prepend(Token::TkFatArrow.clone(), &tokenize(src, pos2))
                } else {
                    aver_rt::AverList::prepend(Token::TkEq.clone(), &tokenize(src, nextPos))
                }
            }
        }
        None => aver_rt::AverList::prepend(Token::TkEq.clone(), &tokenize(src, nextPos)),
    }
}

/// Handle newline: count indent of next line, emit NEWLINE + raw indent marker.
pub fn tokenizeNewline(src: AverStr, pos: i64) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let r = countIndent(src.clone(), pos, 0i64);
    {
        let (indent, newPos) = r;
        aver_rt::AverList::prepend(
            Token::TkNewline.clone(),
            &aver_rt::AverList::prepend(Token::TkInt(((-indent) - 1i64)), &tokenize(src, newPos)),
        )
    }
}

/// Tokenize a complete source string with INDENT/DEDENT.
pub fn lex(src: AverStr) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    let raw = tokenize(src, 0i64);
    let processed = processIndentation(&raw, &aver_rt::AverList::from_vec(vec![0i64]));
    processed
}

/// Convert raw indent markers (negative TkInt after TkNewline) into INDENT/DEDENT tokens.
#[inline(always)]
pub fn processIndentation(
    tokens: &aver_rt::AverList<Token>,
    stack: &aver_rt::AverList<i64>,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    aver_list_match!(tokens.clone(), [] => emitFinalDedents(stack), [t, rest] => processIndentToken(&t, &rest, stack))
}

/// Process one token in the indentation pass.
pub fn processIndentToken(
    t: &Token,
    rest: &aver_rt::AverList<Token>,
    stack: &aver_rt::AverList<i64>,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    match t {
        Token::TkNewline => processAfterNewline(rest, stack),
        Token::TkEof => aver_rt::AverList::concat(
            &emitFinalDedents(stack),
            &aver_rt::AverList::from_vec(vec![Token::TkEof.clone()]),
        ),
        _ => aver_rt::AverList::prepend(t.clone(), &processIndentation(rest, stack)),
    }
}

/// After TkNewline, check for raw indent marker (negative TkInt).
#[inline(always)]
pub fn processAfterNewline(
    tokens: &aver_rt::AverList<Token>,
    stack: &aver_rt::AverList<i64>,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    aver_list_match!(tokens.clone(), [] => aver_rt::AverList::prepend(Token::TkNewline.clone(), &emitFinalDedents(stack)), [t, rest] => processAfterNewlineToken(&t, &rest, stack))
}

/// Check if token after newline is a raw indent marker.
pub fn processAfterNewlineToken(
    t: &Token,
    rest: &aver_rt::AverList<Token>,
    stack: &aver_rt::AverList<i64>,
) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    match t.clone() {
        Token::TkInt(n) => {
            if (n < 0i64) {
                emitIndentChange(((-n) - 1i64), rest, stack)
            } else {
                aver_rt::AverList::prepend(
                    Token::TkNewline.clone(),
                    &aver_rt::AverList::prepend(t.clone(), &processIndentation(rest, stack)),
                )
            }
        }
        _ => aver_rt::AverList::prepend(
            Token::TkNewline.clone(),
            &aver_rt::AverList::prepend(t.clone(), &processIndentation(rest, stack)),
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
    let currentIndent = stackTop(stack);
    if (indent > currentIndent) {
        aver_rt::AverList::prepend(
            Token::TkNewline.clone(),
            &aver_rt::AverList::prepend(
                Token::TkIndent.clone(),
                &processIndentation(rest, &aver_rt::AverList::prepend(indent, &stack.clone())),
            ),
        )
    } else {
        if (indent < currentIndent) {
            emitDedents(indent, rest, stack)
        } else {
            aver_rt::AverList::prepend(Token::TkNewline.clone(), &processIndentation(rest, stack))
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
    emitDedentsAcc(
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
        return aver_list_match!(stack.clone(), [] => aver_rt::AverList::concat(&reversed, &processIndentation(&rest, &aver_rt::AverList::from_vec(vec![0i64]))), [top, below] => { if (top <= targetIndent) { aver_rt::AverList::concat(&reversed, &aver_rt::AverList::prepend(Token::TkNewline.clone(), &processIndentation(&rest, &stack))) } else { {
            let __tmp3 = aver_rt::AverList::prepend(Token::TkDedent.clone(), &acc);
            stack = below;
            acc = __tmp3;
            continue;
        } } });
    }
}

/// At EOF, emit DEDENT for each indent level above 0.
#[inline(always)]
pub fn emitFinalDedents(stack: &aver_rt::AverList<i64>) -> aver_rt::AverList<Token> {
    crate::cancel_checkpoint();
    emitFinalDedentsAcc(stack.clone(), aver_rt::AverList::empty())
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
        return aver_list_match!(stack, [] => reversed, [top, rest] => { if (top > 0i64) { {
            let __tmp1 = aver_rt::AverList::prepend(Token::TkDedent.clone(), &acc);
            stack = rest;
            acc = __tmp1;
            continue;
        } } else { reversed } });
    }
}

/// Return top of indent stack, or 0 if empty.
#[inline(always)]
pub fn stackTop(stack: &aver_rt::AverList<i64>) -> i64 {
    crate::cancel_checkpoint();
    aver_list_match!(stack.clone(), [] => 0i64, [top, rest] => top)
}

pub mod chars;
