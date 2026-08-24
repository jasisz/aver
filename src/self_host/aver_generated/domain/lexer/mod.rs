#[allow(unused_imports)]
use crate::aver_generated::domain::lexer::chars::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;
#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    TokenizeDefault__indexed(AverStr, AverStr, aver_rt::AverInt),
    TokenizeBraceOrSkip__indexed(AverStr, AverStr, aver_rt::AverInt),
    TokenizeChar__indexed(AverStr, AverStr, aver_rt::AverInt),
    TokenizeSome__indexed(AverStr, AverStr, aver_rt::AverInt),
    TokenizeAtPos__indexed(AverStr, aver_rt::AverInt),
    Tokenize__indexed(AverStr, aver_rt::AverInt),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    loop {
        __state = match __state {
            __MutualTco1::TokenizeDefault__indexed(mut c, mut src, mut pos) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::lexer::chars::isDigit(c.clone()) {
                    return crate::aver_generated::domain::lexer::tokenizeDigit__indexed(
                        src,
                        pos,
                        &*__str_index,
                    );
                } else {
                    if crate::aver_generated::domain::lexer::chars::isAlpha(c.clone()) {
                        return crate::aver_generated::domain::lexer::tokenizeAlpha__indexed(
                            src,
                            pos,
                            &*__str_index,
                        );
                    } else {
                        __MutualTco1::TokenizeBraceOrSkip__indexed(c, src, pos)
                    }
                }
            }
            __MutualTco1::TokenizeBraceOrSkip__indexed(mut c, mut src, mut pos) => {
                crate::cancel_checkpoint();
                let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
                if (c == crate::aver_generated::domain::lexer::openBrace()) {
                    return aver_rt::AverList::prepend(
                        crate::aver_generated::domain::token::Token::TkLBrace,
                        &crate::aver_generated::domain::lexer::tokenize__indexed(
                            src,
                            nextPos,
                            &*__str_index,
                        ),
                    );
                } else {
                    if (c == crate::aver_generated::domain::lexer::closeBrace()) {
                        return aver_rt::AverList::prepend(
                            crate::aver_generated::domain::token::Token::TkRBrace,
                            &crate::aver_generated::domain::lexer::tokenize__indexed(
                                src,
                                nextPos,
                                &*__str_index,
                            ),
                        );
                    } else {
                        __MutualTco1::Tokenize__indexed(src, nextPos)
                    }
                }
            }
            __MutualTco1::TokenizeChar__indexed(mut c, mut src, mut pos) => {
                crate::cancel_checkpoint();
                let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
                {
                    let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                    if __dispatch_subject == aver_rt::AverInt::from_i64(32) {
                        __MutualTco1::Tokenize__indexed(src, nextPos)
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(10) {
                            return crate::aver_generated::domain::lexer::tokenizeNewline__indexed(
                                src,
                                nextPos,
                                &*__str_index,
                            );
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(47) {
                                return crate::aver_generated::domain::lexer::tokenizeSlashOrComment__indexed(src, pos, &*__str_index);
                            } else {
                                if __dispatch_subject == aver_rt::AverInt::from_i64(43) {
                                    return aver_rt::AverList::prepend(
                                        crate::aver_generated::domain::token::Token::TkPlus,
                                        &crate::aver_generated::domain::lexer::tokenize__indexed(
                                            src,
                                            nextPos,
                                            &*__str_index,
                                        ),
                                    );
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(42) {
                                        return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkStar, &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, &*__str_index));
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(60) {
                                            return crate::aver_generated::domain::lexer::tokenizeLt__indexed(src, pos, &*__str_index);
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(62)
                                            {
                                                return crate::aver_generated::domain::lexer::tokenizeGt__indexed(src, pos, &*__str_index);
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(33)
                                                {
                                                    return crate::aver_generated::domain::lexer::tokenizeBang__indexed(src, pos, &*__str_index);
                                                } else {
                                                    if __dispatch_subject
                                                        == aver_rt::AverInt::from_i64(63)
                                                    {
                                                        return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkQuestion, &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, &*__str_index));
                                                    } else {
                                                        if __dispatch_subject
                                                            == aver_rt::AverInt::from_i64(34)
                                                        {
                                                            return crate::aver_generated::domain::lexer::tokenizeString__indexed(src, nextPos, AverStr::from(""), &*__str_index);
                                                        } else {
                                                            if __dispatch_subject
                                                                == aver_rt::AverInt::from_i64(40)
                                                            {
                                                                return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkLParen, &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, &*__str_index));
                                                            } else {
                                                                if __dispatch_subject
                                                                    == aver_rt::AverInt::from_i64(
                                                                        41,
                                                                    )
                                                                {
                                                                    return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkRParen, &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, &*__str_index));
                                                                } else {
                                                                    if __dispatch_subject == aver_rt::AverInt::from_i64(91) { return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkLBracket, &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, &*__str_index)) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(93) { return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkRBracket, &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, &*__str_index)) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(46) { return crate::aver_generated::domain::lexer::tokenizeDot__indexed(src, pos, &*__str_index) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(44) { return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkComma, &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, &*__str_index)) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(58) { return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkColon, &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, &*__str_index)) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(61) { return crate::aver_generated::domain::lexer::tokenizeEq__indexed(src, pos, &*__str_index) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(45) { return crate::aver_generated::domain::lexer::tokenizeMinus__indexed(src, pos, &*__str_index) } else { __MutualTco1::TokenizeDefault__indexed(c, src, pos) } } } } } } }
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
            __MutualTco1::TokenizeSome__indexed(mut c, mut src, mut pos) => {
                crate::cancel_checkpoint();
                __MutualTco1::TokenizeChar__indexed(c, src, pos)
            }
            __MutualTco1::TokenizeAtPos__indexed(mut src, mut pos) => {
                crate::cancel_checkpoint();
                match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
                    None => {
                        return aver_rt::AverList::from_vec(vec![
                            crate::aver_generated::domain::token::Token::TkEof,
                        ]);
                    }
                    Some(c @ _) => __MutualTco1::TokenizeSome__indexed(c, src, pos),
                }
            }
            __MutualTco1::Tokenize__indexed(mut src, mut pos) => {
                crate::cancel_checkpoint();
                if (pos < aver_rt::AverInt::from_i64(src.chars().count() as i64)) {
                    __MutualTco1::TokenizeAtPos__indexed(src, pos)
                } else {
                    return aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::token::Token::TkEof,
                    ]);
                }
            }
        };
    }
}

/// Synthesized indexed worker of `tokenizeDefault`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeDefault__indexed(
    c: AverStr,
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_1(
        __MutualTco1::TokenizeDefault__indexed(c, src, pos),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeBraceOrSkip`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeBraceOrSkip__indexed(
    c: AverStr,
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_1(
        __MutualTco1::TokenizeBraceOrSkip__indexed(c, src, pos),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeChar`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeChar__indexed(
    c: AverStr,
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_1(
        __MutualTco1::TokenizeChar__indexed(c, src, pos),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeSome`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeSome__indexed(
    c: AverStr,
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_1(
        __MutualTco1::TokenizeSome__indexed(c, src, pos),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeAtPos`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeAtPos__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_1(__MutualTco1::TokenizeAtPos__indexed(src, pos), &__str_index)
}

/// Synthesized indexed worker of `tokenize`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenize__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_1(__MutualTco1::Tokenize__indexed(src, pos), &__str_index)
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    TokenizeString__indexed(AverStr, aver_rt::AverInt, AverStr),
    TokenizeStringAt__indexed(AverStr, aver_rt::AverInt, AverStr),
    TokenizeStringEscape__indexed(AverStr, aver_rt::AverInt, AverStr),
    TokenizeStringChar__indexed(AverStr, aver_rt::AverInt, AverStr, AverStr),
    TokenizeStringCharInner__indexed(AverStr, aver_rt::AverInt, AverStr, AverStr),
    TokenizeStringMaybeEscapedBrace__indexed(AverStr, aver_rt::AverInt, AverStr),
    TokenizeStringMaybeEscapedClose__indexed(AverStr, aver_rt::AverInt, AverStr),
}

fn __mutual_tco_trampoline_2(
    mut __state: __MutualTco2,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    loop {
        __state = match __state {
            __MutualTco2::TokenizeString__indexed(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                if (pos < aver_rt::AverInt::from_i64(src.chars().count() as i64)) {
                    __MutualTco2::TokenizeStringAt__indexed(src, pos, acc)
                } else {
                    return aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::token::Token::TkStr(acc),
                        crate::aver_generated::domain::token::Token::TkEof,
                    ]);
                }
            }
            __MutualTco2::TokenizeStringAt__indexed(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
                    Some(c @ _) => {
                        if (&*c == "\\") {
                            __MutualTco2::TokenizeStringEscape__indexed(
                                src,
                                pos.add(&aver_rt::AverInt::from_i64(1)),
                                acc,
                            )
                        } else {
                            __MutualTco2::TokenizeStringChar__indexed(src, pos, acc, c)
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
            __MutualTco2::TokenizeStringEscape__indexed(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
                match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
                    Some(c @ _) => {
                        let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                        if __dispatch_subject == aver_rt::AverInt::from_i64(110) {
                            __MutualTco2::TokenizeString__indexed(
                                src,
                                nextPos,
                                (acc + &AverStr::from("\n")),
                            )
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(116) {
                                __MutualTco2::TokenizeString__indexed(
                                    src,
                                    nextPos,
                                    (acc + &AverStr::from("\t")),
                                )
                            } else {
                                if __dispatch_subject == aver_rt::AverInt::from_i64(114) {
                                    __MutualTco2::TokenizeString__indexed(
                                        src,
                                        nextPos,
                                        (acc + &AverStr::from("\r")),
                                    )
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(98) {
                                        __MutualTco2::TokenizeString__indexed(
                                            src,
                                            nextPos,
                                            (acc + &AverStr::from("\u{8}")),
                                        )
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(102) {
                                            __MutualTco2::TokenizeString__indexed(
                                                src,
                                                nextPos,
                                                (acc + &AverStr::from("\u{c}")),
                                            )
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(34)
                                            {
                                                __MutualTco2::TokenizeString__indexed(
                                                    src,
                                                    nextPos,
                                                    (acc + &AverStr::from("\"")),
                                                )
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(92)
                                                {
                                                    __MutualTco2::TokenizeString__indexed(
                                                        src,
                                                        nextPos,
                                                        (acc + &AverStr::from("\\")),
                                                    )
                                                } else {
                                                    __MutualTco2::TokenizeString__indexed(
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
            __MutualTco2::TokenizeStringChar__indexed(mut src, mut pos, mut acc, mut c) => {
                crate::cancel_checkpoint();
                if (&*c == "\"") {
                    return aver_rt::AverList::prepend(
                        crate::aver_generated::domain::token::Token::TkStr(acc),
                        &crate::aver_generated::domain::lexer::tokenize__indexed(
                            src,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            &*__str_index,
                        ),
                    );
                } else {
                    __MutualTco2::TokenizeStringCharInner__indexed(src, pos, acc, c)
                }
            }
            __MutualTco2::TokenizeStringCharInner__indexed(mut src, mut pos, mut acc, mut c) => {
                crate::cancel_checkpoint();
                if (c == crate::aver_generated::domain::lexer::openBrace()) {
                    __MutualTco2::TokenizeStringMaybeEscapedBrace__indexed(src, pos, acc)
                } else {
                    if (c == crate::aver_generated::domain::lexer::closeBrace()) {
                        __MutualTco2::TokenizeStringMaybeEscapedClose__indexed(src, pos, acc)
                    } else {
                        __MutualTco2::TokenizeString__indexed(
                            src,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            (acc + &c),
                        )
                    }
                }
            }
            __MutualTco2::TokenizeStringMaybeEscapedBrace__indexed(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
                match aver_rt::string_index_char_at(&src, &__str_index, &nextPos) {
                    Some(next @ _) => {
                        if (next == crate::aver_generated::domain::lexer::openBrace()) {
                            __MutualTco2::TokenizeString__indexed(
                                src,
                                pos.add(&aver_rt::AverInt::from_i64(2)),
                                (acc + &crate::aver_generated::domain::lexer::openBrace()),
                            )
                        } else {
                            return crate::aver_generated::domain::lexer::tokenizeInterp__indexed(
                                src,
                                pos,
                                acc,
                                &*__str_index,
                            );
                        }
                    }
                    None => {
                        return crate::aver_generated::domain::lexer::tokenizeInterp__indexed(
                            src,
                            pos,
                            acc,
                            &*__str_index,
                        );
                    }
                }
            }
            __MutualTco2::TokenizeStringMaybeEscapedClose__indexed(mut src, mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
                let accBrace = (acc + &crate::aver_generated::domain::lexer::closeBrace());
                match aver_rt::string_index_char_at(&src, &__str_index, &nextPos) {
                    Some(next @ _) => {
                        if (next == crate::aver_generated::domain::lexer::closeBrace()) {
                            __MutualTco2::TokenizeString__indexed(
                                src,
                                pos.add(&aver_rt::AverInt::from_i64(2)),
                                accBrace,
                            )
                        } else {
                            __MutualTco2::TokenizeString__indexed(src, nextPos, accBrace)
                        }
                    }
                    None => __MutualTco2::TokenizeString__indexed(src, nextPos, accBrace),
                }
            }
        };
    }
}

/// Synthesized indexed worker of `tokenizeString`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeString__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_2(
        __MutualTco2::TokenizeString__indexed(src, pos, acc),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeStringAt`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeStringAt__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_2(
        __MutualTco2::TokenizeStringAt__indexed(src, pos, acc),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeStringEscape`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeStringEscape__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_2(
        __MutualTco2::TokenizeStringEscape__indexed(src, pos, acc),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeStringChar`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeStringChar__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
    c: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_2(
        __MutualTco2::TokenizeStringChar__indexed(src, pos, acc, c),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeStringCharInner`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeStringCharInner__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
    c: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_2(
        __MutualTco2::TokenizeStringCharInner__indexed(src, pos, acc, c),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeStringMaybeEscapedBrace`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeStringMaybeEscapedBrace__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_2(
        __MutualTco2::TokenizeStringMaybeEscapedBrace__indexed(src, pos, acc),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeStringMaybeEscapedClose`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeStringMaybeEscapedClose__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_2(
        __MutualTco2::TokenizeStringMaybeEscapedClose__indexed(src, pos, acc),
        &__str_index,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    TokenizeInterpExpr__indexed(AverStr, aver_rt::AverInt),
    TokenizeInterpExprAt__indexed(AverStr, aver_rt::AverInt),
    TokenizeInterpExprC__indexed(AverStr, aver_rt::AverInt, AverStr),
    TokenizeInterpExprChar__indexed(AverStr, aver_rt::AverInt, AverStr),
    TokenizeInterpNonDigit__indexed(AverStr, aver_rt::AverInt, AverStr),
    TokenizeInterpPunct__indexed(AverStr, aver_rt::AverInt, AverStr),
    TokenizeInterpAlpha__indexed(AverStr, aver_rt::AverInt),
}

fn __mutual_tco_trampoline_3(
    mut __state: __MutualTco3,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    loop {
        __state = match __state {
            __MutualTco3::TokenizeInterpExpr__indexed(mut src, mut pos) => {
                crate::cancel_checkpoint();
                if (pos < aver_rt::AverInt::from_i64(src.chars().count() as i64)) {
                    __MutualTco3::TokenizeInterpExprAt__indexed(src, pos)
                } else {
                    return aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::token::Token::TkInterpEnd,
                        crate::aver_generated::domain::token::Token::TkEof,
                    ]);
                }
            }
            __MutualTco3::TokenizeInterpExprAt__indexed(mut src, mut pos) => {
                crate::cancel_checkpoint();
                match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
                    Some(c @ _) => __MutualTco3::TokenizeInterpExprC__indexed(src, pos, c),
                    None => {
                        return aver_rt::AverList::from_vec(vec![
                            crate::aver_generated::domain::token::Token::TkInterpEnd,
                            crate::aver_generated::domain::token::Token::TkEof,
                        ]);
                    }
                }
            }
            __MutualTco3::TokenizeInterpExprC__indexed(mut src, mut pos, mut c) => {
                crate::cancel_checkpoint();
                if (c == crate::aver_generated::domain::lexer::closeBrace()) {
                    return aver_rt::AverList::prepend(
                        crate::aver_generated::domain::token::Token::TkInterpEnd,
                        &crate::aver_generated::domain::lexer::tokenizeString__indexed(
                            src,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            AverStr::from(""),
                            &*__str_index,
                        ),
                    );
                } else {
                    __MutualTco3::TokenizeInterpExprChar__indexed(src, pos, c)
                }
            }
            __MutualTco3::TokenizeInterpExprChar__indexed(mut src, mut pos, mut c) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::lexer::chars::isDigit(c.clone()) {
                    return crate::aver_generated::domain::lexer::tokenizeInterpDigit__indexed(
                        src,
                        pos,
                        &*__str_index,
                    );
                } else {
                    __MutualTco3::TokenizeInterpNonDigit__indexed(src, pos, c)
                }
            }
            __MutualTco3::TokenizeInterpNonDigit__indexed(mut src, mut pos, mut c) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::lexer::chars::isAlpha(c.clone()) {
                    __MutualTco3::TokenizeInterpAlpha__indexed(src, pos)
                } else {
                    __MutualTco3::TokenizeInterpPunct__indexed(src, pos, c)
                }
            }
            __MutualTco3::TokenizeInterpPunct__indexed(mut src, mut pos, mut c) => {
                crate::cancel_checkpoint();
                let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
                {
                    let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                    if __dispatch_subject == aver_rt::AverInt::from_i64(32) {
                        __MutualTco3::TokenizeInterpExpr__indexed(src, nextPos)
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(40) {
                            return aver_rt::AverList::prepend(
                                crate::aver_generated::domain::token::Token::TkLParen,
                                &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
                                    src,
                                    nextPos,
                                    &*__str_index,
                                ),
                            );
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(41) {
                                return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkRParen, &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(src, nextPos, &*__str_index));
                            } else {
                                if __dispatch_subject == aver_rt::AverInt::from_i64(43) {
                                    return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkPlus, &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(src, nextPos, &*__str_index));
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(45) {
                                        return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkMinus, &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(src, nextPos, &*__str_index));
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(42) {
                                            return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkStar, &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(src, nextPos, &*__str_index));
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(44)
                                            {
                                                return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkComma, &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(src, nextPos, &*__str_index));
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(46)
                                                {
                                                    return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkDot, &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(src, nextPos, &*__str_index));
                                                } else {
                                                    if __dispatch_subject
                                                        == aver_rt::AverInt::from_i64(91)
                                                    {
                                                        return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkLBracket, &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(src, nextPos, &*__str_index));
                                                    } else {
                                                        if __dispatch_subject
                                                            == aver_rt::AverInt::from_i64(93)
                                                        {
                                                            return aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkRBracket, &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(src, nextPos, &*__str_index));
                                                        } else {
                                                            if __dispatch_subject
                                                                == aver_rt::AverInt::from_i64(34)
                                                            {
                                                                return crate::aver_generated::domain::lexer::tokenizeInterpString__indexed(src, nextPos, AverStr::from(""), (*__str_index).clone());
                                                            } else {
                                                                __MutualTco3::TokenizeInterpExpr__indexed(src, nextPos)
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
            __MutualTco3::TokenizeInterpAlpha__indexed(mut src, mut pos) => {
                crate::cancel_checkpoint();
                match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
                    Some(c @ _) => {
                        let (word, newPos) = crate::aver_generated::domain::lexer::chars::readIdent(
                            src.clone(),
                            pos,
                            AverStr::from(""),
                            crate::aver_generated::domain::lexer::chars::isUpper(c),
                        );
                        return aver_rt::AverList::prepend(
                            crate::aver_generated::domain::lexer::chars::keywordOrIdent(word),
                            &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
                                src,
                                newPos,
                                &*__str_index,
                            ),
                        );
                    }
                    None => __MutualTco3::TokenizeInterpExpr__indexed(src, pos),
                }
            }
        };
    }
}

/// Synthesized indexed worker of `tokenizeInterpExpr`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeInterpExpr__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_3(
        __MutualTco3::TokenizeInterpExpr__indexed(src, pos),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeInterpExprAt`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeInterpExprAt__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_3(
        __MutualTco3::TokenizeInterpExprAt__indexed(src, pos),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeInterpExprC`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeInterpExprC__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    c: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_3(
        __MutualTco3::TokenizeInterpExprC__indexed(src, pos, c),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeInterpExprChar`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeInterpExprChar__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    c: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_3(
        __MutualTco3::TokenizeInterpExprChar__indexed(src, pos, c),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeInterpNonDigit`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeInterpNonDigit__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    c: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_3(
        __MutualTco3::TokenizeInterpNonDigit__indexed(src, pos, c),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeInterpPunct`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeInterpPunct__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    c: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_3(
        __MutualTco3::TokenizeInterpPunct__indexed(src, pos, c),
        &__str_index,
    )
}

/// Synthesized indexed worker of `tokenizeInterpAlpha`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeInterpAlpha__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    __mutual_tco_trampoline_3(
        __MutualTco3::TokenizeInterpAlpha__indexed(src, pos),
        &__str_index,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco4 {
    CountIndent__indexed(AverStr, aver_rt::AverInt, aver_rt::AverInt),
    CountIndentChar__indexed(AverStr, aver_rt::AverInt, aver_rt::AverInt),
}

fn __mutual_tco_trampoline_4(
    mut __state: __MutualTco4,
    __str_index: &aver_rt::StringIndex,
) -> (aver_rt::AverInt, aver_rt::AverInt) {
    loop {
        __state = match __state {
            __MutualTco4::CountIndent__indexed(mut src, mut pos, mut spaces) => {
                crate::cancel_checkpoint();
                if (pos < aver_rt::AverInt::from_i64(src.chars().count() as i64)) {
                    __MutualTco4::CountIndentChar__indexed(src, pos, spaces)
                } else {
                    return (spaces, pos);
                }
            }
            __MutualTco4::CountIndentChar__indexed(mut src, mut pos, mut spaces) => {
                crate::cancel_checkpoint();
                let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
                {
                    let __int_match_subject =
                        aver_rt::string_index_code_at(&src, &__str_index, &pos);
                    if __int_match_subject == -1i64 {
                        return (spaces, pos);
                    } else {
                        let __str_ix_c1 = __int_match_subject;
                        {
                            let __dispatch_subject = __str_ix_c1;
                            if __dispatch_subject == 32i64 {
                                __MutualTco4::CountIndent__indexed(
                                    src,
                                    nextPos,
                                    spaces.add(&aver_rt::AverInt::from_i64(1)),
                                )
                            } else {
                                if __dispatch_subject == 10i64 {
                                    __MutualTco4::CountIndent__indexed(
                                        src,
                                        nextPos,
                                        aver_rt::AverInt::from_i64(0),
                                    )
                                } else {
                                    return (spaces, pos);
                                }
                            }
                        }
                    }
                }
            }
        };
    }
}

/// Synthesized indexed worker of `countIndent`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn countIndent__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    spaces: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> (aver_rt::AverInt, aver_rt::AverInt) {
    __mutual_tco_trampoline_4(
        __MutualTco4::CountIndent__indexed(src, pos, spaces),
        &__str_index,
    )
}

/// Synthesized indexed worker of `countIndentChar`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn countIndentChar__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    spaces: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> (aver_rt::AverInt, aver_rt::AverInt) {
    __mutual_tco_trampoline_4(
        __MutualTco4::CountIndentChar__indexed(src, pos, spaces),
        &__str_index,
    )
}

/// Tokenize starting from a digit character.
#[inline(always)]
pub fn tokenizeDigit(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeDigit__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// After reading integer part, check for decimal point to form a float.
#[inline(always)]
pub fn tokenizeAfterInt(
    src: AverStr,
    pos: aver_rt::AverInt,
    n: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeAfterInt__indexed(
        src.clone(),
        pos,
        n,
        &aver_rt::string_index_build(&src),
    )
}

/// After integer and dot, check if next char is digit (float) or not (int + dot).
#[inline(always)]
pub fn tokenizeAfterIntDot(
    src: AverStr,
    pos: aver_rt::AverInt,
    n: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeAfterIntDot__indexed(
        src.clone(),
        pos,
        n,
        &aver_rt::string_index_build(&src),
    )
}

/// Read decimal digits and build float token.
#[inline(always)]
pub fn tokenizeFloat(
    src: AverStr,
    pos: aver_rt::AverInt,
    intPart: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeFloat__indexed(
        src.clone(),
        pos,
        intPart,
        &aver_rt::string_index_build(&src),
    )
}

/// Construct float from integer and decimal parts.
#[inline(always)]
pub fn buildFloat(
    src: AverStr,
    pos: aver_rt::AverInt,
    intPart: aver_rt::AverInt,
    decPart: aver_rt::AverInt,
    decDigits: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::buildFloat__indexed(
        src.clone(),
        pos,
        intPart,
        decPart,
        decDigits,
        &aver_rt::string_index_build(&src),
    )
}

/// Compute 10^n as Float.
#[inline(always)]
pub fn pow10(n: aver_rt::AverInt) -> f64 {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::pow10Acc(n, 1.0f64)
}

/// Accumulate 10^n as Float.
#[inline(always)]
pub fn pow10Acc(mut n: aver_rt::AverInt, mut acc: f64) -> f64 {
    loop {
        crate::cancel_checkpoint();
        if (n > aver_rt::AverInt::from_i64(0)) {
            {
                let __tco0 = n.sub(&aver_rt::AverInt::from_i64(1));
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
pub fn tokenizeAlpha(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeAlpha__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize identifier with known dotted mode.
#[inline(always)]
pub fn tokenizeAlphaWith(
    src: AverStr,
    pos: aver_rt::AverInt,
    dotted: bool,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeAlphaWith__indexed(
        src.clone(),
        pos,
        dotted,
        &aver_rt::string_index_build(&src),
    )
}

/// Check if a character is the greater-than sign.
pub fn isGreaterThan(c: AverStr) -> bool {
    crate::cancel_checkpoint();
    (&*c == ">")
}

/// Tokenize a minus or arrow token.
#[inline(always)]
pub fn tokenizeMinus(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeMinus__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize a character that is not a known single-char token.
#[inline(always)]
pub fn tokenizeDefault(
    c: AverStr,
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeDefault__indexed(
        c,
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Handle brace tokens or skip unknown chars.
#[inline(always)]
pub fn tokenizeBraceOrSkip(
    c: AverStr,
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeBraceOrSkip__indexed(
        c,
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Read string literal with interpolation and escape sequences.
#[inline(always)]
pub fn tokenizeString(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeString__indexed(
        src.clone(),
        pos,
        acc,
        &aver_rt::string_index_build(&src),
    )
}

/// Read one character of string.
#[inline(always)]
pub fn tokenizeStringAt(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeStringAt__indexed(
        src.clone(),
        pos,
        acc,
        &aver_rt::string_index_build(&src),
    )
}

/// Handle escape sequence in string: \n -> newline, \t -> tab, etc.
#[inline(always)]
pub fn tokenizeStringEscape(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeStringEscape__indexed(
        src.clone(),
        pos,
        acc,
        &aver_rt::string_index_build(&src),
    )
}

/// Return the opening brace character.
#[inline(always)]
pub fn openBrace() -> AverStr {
    crate::cancel_checkpoint();
    ((aver_rt::AverInt::from_i64(123))
        .to_u32()
        .and_then(char::from_u32)
        .map(|c| c.to_string()))
    .into_aver()
    .unwrap_or(AverStr::from("x"))
}

/// Handle one character inside a string literal.
#[inline(always)]
pub fn tokenizeStringChar(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
    c: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeStringChar__indexed(
        src.clone(),
        pos,
        acc,
        c,
        &aver_rt::string_index_build(&src),
    )
}

/// Check for interpolation start, { escape, or continue string.
#[inline(always)]
pub fn tokenizeStringCharInner(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
    c: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeStringCharInner__indexed(
        src.clone(),
        pos,
        acc,
        c,
        &aver_rt::string_index_build(&src),
    )
}

/// Check for { (escaped brace) or start interpolation.
#[inline(always)]
pub fn tokenizeStringMaybeEscapedBrace(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeStringMaybeEscapedBrace__indexed(
        src.clone(),
        pos,
        acc,
        &aver_rt::string_index_build(&src),
    )
}

/// Check for } (escaped close brace) or continue.
#[inline(always)]
pub fn tokenizeStringMaybeEscapedClose(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeStringMaybeEscapedClose__indexed(
        src.clone(),
        pos,
        acc,
        &aver_rt::string_index_build(&src),
    )
}

/// Start interpolation: emit accumulated string, TkInterpStart, expr tokens, TkInterpEnd.
#[inline(always)]
pub fn tokenizeInterp(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterp__indexed(
        src.clone(),
        pos,
        acc,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize expression inside interpolation braces.
#[inline(always)]
pub fn tokenizeInterpExpr(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Read one token of interpolation expression.
#[inline(always)]
pub fn tokenizeInterpExprAt(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpExprAt__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Return the closing brace character.
#[inline(always)]
pub fn closeBrace() -> AverStr {
    crate::cancel_checkpoint();
    ((aver_rt::AverInt::from_i64(125))
        .to_u32()
        .and_then(char::from_u32)
        .map(|c| c.to_string()))
    .into_aver()
    .unwrap_or(AverStr::from("x"))
}

/// Dispatch interpolation char.
#[inline(always)]
pub fn tokenizeInterpExprC(
    src: AverStr,
    pos: aver_rt::AverInt,
    c: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpExprC__indexed(
        src.clone(),
        pos,
        c,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize one char of interpolation expression.
#[inline(always)]
pub fn tokenizeInterpExprChar(
    src: AverStr,
    pos: aver_rt::AverInt,
    c: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpExprChar__indexed(
        src.clone(),
        pos,
        c,
        &aver_rt::string_index_build(&src),
    )
}

/// Handle non-digit char in interpolation.
#[inline(always)]
pub fn tokenizeInterpNonDigit(
    src: AverStr,
    pos: aver_rt::AverInt,
    c: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpNonDigit__indexed(
        src.clone(),
        pos,
        c,
        &aver_rt::string_index_build(&src),
    )
}

/// Handle punctuation in interpolation.
#[inline(always)]
pub fn tokenizeInterpPunct(
    src: AverStr,
    pos: aver_rt::AverInt,
    c: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpPunct__indexed(
        src.clone(),
        pos,
        c,
        &aver_rt::string_index_build(&src),
    )
}

/// Read string literal inside interpolation braces.
#[inline(always)]
pub fn tokenizeInterpString(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpString__indexed(
        src.clone(),
        pos,
        acc,
        aver_rt::string_index_build(&src),
    )
}

/// Read number inside interpolation; may be an int or a float literal.
#[inline(always)]
pub fn tokenizeInterpDigit(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpDigit__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// After integer part inside interpolation, check for a decimal point.
#[inline(always)]
pub fn tokenizeInterpAfterInt(
    src: AverStr,
    pos: aver_rt::AverInt,
    n: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpAfterInt__indexed(
        src.clone(),
        pos,
        n,
        &aver_rt::string_index_build(&src),
    )
}

/// After integer and dot inside interpolation: digit -> float, else int + dot.
#[inline(always)]
pub fn tokenizeInterpAfterIntDot(
    src: AverStr,
    pos: aver_rt::AverInt,
    n: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpAfterIntDot__indexed(
        src.clone(),
        pos,
        n,
        &aver_rt::string_index_build(&src),
    )
}

/// Read decimal digits and build a float token inside interpolation.
#[inline(always)]
pub fn tokenizeInterpFloat(
    src: AverStr,
    pos: aver_rt::AverInt,
    intPart: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpFloat__indexed(
        src.clone(),
        pos,
        intPart,
        &aver_rt::string_index_build(&src),
    )
}

/// Construct a float from integer and decimal parts inside interpolation.
#[inline(always)]
pub fn tokenizeInterpBuildFloat(
    src: AverStr,
    pos: aver_rt::AverInt,
    intPart: aver_rt::AverInt,
    decPart: aver_rt::AverInt,
    decDigits: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpBuildFloat__indexed(
        src.clone(),
        pos,
        intPart,
        decPart,
        decDigits,
        &aver_rt::string_index_build(&src),
    )
}

/// Read identifier inside interpolation.
#[inline(always)]
pub fn tokenizeInterpAlpha(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeInterpAlpha__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize / (division) or // (line comment).
#[inline(always)]
pub fn tokenizeSlashOrComment(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeSlashOrComment__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Skip characters until newline or EOF. Newline goes through indent handling.
#[inline(always)]
pub fn skipLineComment(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::skipLineComment__indexed(
        src.clone(),
        pos,
        aver_rt::string_index_build(&src),
    )
}

/// Tokenize . (field access) or .. (rest pattern).
#[inline(always)]
pub fn tokenizeDot(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeDot__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize < or <=.
#[inline(always)]
pub fn tokenizeLt(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeLt__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize > or >=.
#[inline(always)]
pub fn tokenizeGt(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeGt__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize ! or !=.
#[inline(always)]
pub fn tokenizeBang(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeBang__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize =, ==, or =>.
#[inline(always)]
pub fn tokenizeEq(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeEq__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize based on the current character.
#[inline(always)]
pub fn tokenizeChar(
    c: AverStr,
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeChar__indexed(
        c,
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize when charAt returned Some.
#[inline(always)]
pub fn tokenizeSome(
    c: AverStr,
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeSome__indexed(
        c,
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize at given position after bounds check.
#[inline(always)]
pub fn tokenizeAtPos(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeAtPos__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize source string starting from pos.
#[inline(always)]
pub fn tokenize(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenize__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Handle newline: count indent of next line, emit NEWLINE + raw indent marker.
#[inline(always)]
pub fn tokenizeNewline(
    src: AverStr,
    pos: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::tokenizeNewline__indexed(
        src.clone(),
        pos,
        &aver_rt::string_index_build(&src),
    )
}

/// Count leading spaces after newline. Skip blank lines (reset on another newline).
#[inline(always)]
pub fn countIndent(
    src: AverStr,
    pos: aver_rt::AverInt,
    spaces: aver_rt::AverInt,
) -> (aver_rt::AverInt, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::countIndent__indexed(
        src.clone(),
        pos,
        spaces,
        &aver_rt::string_index_build(&src),
    )
}

/// Check one character for indent counting.
#[inline(always)]
pub fn countIndentChar(
    src: AverStr,
    pos: aver_rt::AverInt,
    spaces: aver_rt::AverInt,
) -> (aver_rt::AverInt, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::countIndentChar__indexed(
        src.clone(),
        pos,
        spaces,
        &aver_rt::string_index_build(&src),
    )
}

/// Tokenize a complete source string with INDENT/DEDENT.
#[inline(always)]
pub fn lex(src: AverStr) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::lex__indexed(
        src.clone(),
        &aver_rt::string_index_build(&src),
    )
}

/// Convert raw indent markers (negative TkInt after TkNewline) into INDENT/DEDENT tokens.
#[inline(always)]
pub fn processIndentation(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    stack: &aver_rt::AverIntList,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    aver_list_match!(tokens.clone(), [] => crate::aver_generated::domain::lexer::emitFinalDedents(stack), [t, rest] => crate::aver_generated::domain::lexer::processIndentToken(&t, &rest, stack))
}

/// Process one token in the indentation pass.
pub fn processIndentToken(
    t: &crate::aver_generated::domain::token::Token,
    rest: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    stack: &aver_rt::AverIntList,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    stack: &aver_rt::AverIntList,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    aver_list_match!(tokens.clone(), [] => aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkNewline, &crate::aver_generated::domain::lexer::emitFinalDedents(stack)), [t, rest] => crate::aver_generated::domain::lexer::processAfterNewlineToken(&t, &rest, stack))
}

/// Check if token after newline is a raw indent marker.
pub fn processAfterNewlineToken(
    t: &crate::aver_generated::domain::token::Token,
    rest: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    stack: &aver_rt::AverIntList,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    match t.clone() {
        crate::aver_generated::domain::token::Token::TkInt(n) => {
            if (n < aver_rt::AverInt::from_i64(0)) {
                crate::aver_generated::domain::lexer::emitIndentChange(
                    aver_rt::AverInt::from_i64(0)
                        .sub(&n)
                        .sub(&aver_rt::AverInt::from_i64(1)),
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
    indent: aver_rt::AverInt,
    rest: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    stack: &aver_rt::AverIntList,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let currentIndent = crate::aver_generated::domain::lexer::stackTop(stack);
    if (indent > currentIndent) {
        aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkNewline,
            &aver_rt::AverList::prepend(
                crate::aver_generated::domain::token::Token::TkIndent,
                &crate::aver_generated::domain::lexer::processIndentation(
                    rest,
                    &aver_rt::AverIntList::prepend(indent, &stack.clone()),
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
    targetIndent: aver_rt::AverInt,
    rest: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    stack: &aver_rt::AverIntList,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
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
    mut targetIndent: aver_rt::AverInt,
    rest: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut stack: aver_rt::AverIntList,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    let rest = std::sync::Arc::new(rest);
    loop {
        crate::cancel_checkpoint();
        let reversed = acc.reverse();
        aver_list_match!(stack.clone(), [] => { return aver_rt::AverList::concat(&reversed, &crate::aver_generated::domain::lexer::processIndentation(&*rest, &aver_rt::AverIntList::from_vec(vec![aver_rt::AverInt::from_i64(0)]))); }, [top, below] => { if (top > targetIndent) { {
            let __tco2 = below;
            let __tco3 = aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkDedent, &acc);
            stack = __tco2;
            acc = __tco3;
            continue;
        } } else { return aver_rt::AverList::concat(&reversed, &aver_rt::AverList::prepend(crate::aver_generated::domain::token::Token::TkNewline, &crate::aver_generated::domain::lexer::processIndentation(&*rest, &stack))); } })
    }
}

/// At EOF, emit DEDENT for each indent level above 0.
#[inline(always)]
pub fn emitFinalDedents(
    stack: &aver_rt::AverIntList,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::emitFinalDedentsAcc(
        stack.clone(),
        aver_rt::AverList::empty(),
    )
}

/// Accumulate DEDENT tokens for each indent level above 0.
#[inline(always)]
pub fn emitFinalDedentsAcc(
    mut stack: aver_rt::AverIntList,
    mut acc: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    loop {
        crate::cancel_checkpoint();
        let reversed = acc.reverse();
        aver_list_match!(stack, [] => { return reversed; }, [top, rest] => { if (top > aver_rt::AverInt::from_i64(0)) { {
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
pub fn stackTop(stack: &aver_rt::AverIntList) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    aver_list_match!(stack.clone(), [] => aver_rt::AverInt::from_i64(0), [top, rest] => top)
}

/// Synthesized indexed worker of `tokenizeDigit`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeDigit__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    {
        let (n, newPos) = crate::aver_generated::domain::lexer::chars::readNumber(
            src.clone(),
            pos,
            aver_rt::AverInt::from_i64(0),
        );
        crate::aver_generated::domain::lexer::tokenizeAfterInt__indexed(src, newPos, n, __str_index)
    }
}

/// Synthesized indexed worker of `tokenizeAfterInt`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeAfterInt__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    n: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
        Some(c @ _) => {
            if (&*c == ".") {
                crate::aver_generated::domain::lexer::tokenizeAfterIntDot__indexed(
                    src,
                    pos,
                    n,
                    __str_index,
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkInt(n),
                    &crate::aver_generated::domain::lexer::tokenize__indexed(src, pos, __str_index),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkInt(n),
            &crate::aver_generated::domain::lexer::tokenize__indexed(src, pos, __str_index),
        ),
    }
}

/// Synthesized indexed worker of `tokenizeAfterIntDot`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeAfterIntDot__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    n: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
    match aver_rt::string_index_char_at(&src, &__str_index, &nextPos) {
        Some(d @ _) => {
            if crate::aver_generated::domain::lexer::chars::isDigit(d) {
                crate::aver_generated::domain::lexer::tokenizeFloat__indexed(
                    src,
                    nextPos,
                    n,
                    __str_index,
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkInt(n),
                    &crate::aver_generated::domain::lexer::tokenize__indexed(src, pos, __str_index),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkInt(n),
            &crate::aver_generated::domain::lexer::tokenize__indexed(src, pos, __str_index),
        ),
    }
}

/// Synthesized indexed worker of `tokenizeFloat`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeFloat__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    intPart: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    {
        let (decPart, newPos) = crate::aver_generated::domain::lexer::chars::readNumber(
            src.clone(),
            pos.clone(),
            aver_rt::AverInt::from_i64(0),
        );
        crate::aver_generated::domain::lexer::buildFloat__indexed(
            src,
            newPos.clone(),
            intPart,
            decPart,
            newPos.sub(&pos),
            __str_index,
        )
    }
}

/// Synthesized indexed worker of `buildFloat`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn buildFloat__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    intPart: aver_rt::AverInt,
    decPart: aver_rt::AverInt,
    decDigits: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let f = (intPart.to_f64()
        + (decPart.to_f64() / crate::aver_generated::domain::lexer::pow10(decDigits)));
    aver_rt::AverList::prepend(
        crate::aver_generated::domain::token::Token::TkFloat(f),
        &crate::aver_generated::domain::lexer::tokenize__indexed(src, pos, __str_index),
    )
}

/// Synthesized indexed worker of `tokenizeAlpha`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeAlpha__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
        Some(c @ _) => crate::aver_generated::domain::lexer::tokenizeAlphaWith__indexed(
            src,
            pos,
            crate::aver_generated::domain::lexer::chars::isUpper(c),
            __str_index,
        ),
        None => {
            aver_rt::AverList::from_vec(vec![crate::aver_generated::domain::token::Token::TkEof])
        }
    }
}

/// Synthesized indexed worker of `tokenizeAlphaWith`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeAlphaWith__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    dotted: bool,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
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
            &crate::aver_generated::domain::lexer::tokenize__indexed(src, newPos, __str_index),
        )
    }
}

/// Synthesized indexed worker of `tokenizeMinus`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeMinus__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
    match aver_rt::string_index_char_at(&src, &__str_index, &nextPos) {
        Some(c @ _) => {
            if crate::aver_generated::domain::lexer::isGreaterThan(c) {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkArrow,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        pos.add(&aver_rt::AverInt::from_i64(2)),
                        __str_index,
                    ),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkMinus,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        nextPos,
                        __str_index,
                    ),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkMinus,
            &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, __str_index),
        ),
    }
}

/// Synthesized indexed worker of `tokenizeInterp`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeInterp__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    acc: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    aver_rt::AverList::prepend(
        crate::aver_generated::domain::token::Token::TkStr(acc),
        &aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkInterpStart,
            &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
                src,
                pos.add(&aver_rt::AverInt::from_i64(1)),
                __str_index,
            ),
        ),
    )
}

/// Synthesized indexed worker of `tokenizeInterpString`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeInterpString__indexed(
    mut src: AverStr,
    mut pos: aver_rt::AverInt,
    mut acc: AverStr,
    __str_index: aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    let __str_index = std::sync::Arc::new(__str_index);
    loop {
        crate::cancel_checkpoint();
        let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
        if (pos < aver_rt::AverInt::from_i64(src.chars().count() as i64)) {
            match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
                Some(c @ _) => {
                    if (&*c == "\"") {
                        return aver_rt::AverList::prepend(
                            crate::aver_generated::domain::token::Token::TkStr(acc),
                            &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
                                src,
                                nextPos,
                                &*__str_index,
                            ),
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
                        &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
                            src,
                            pos,
                            &*__str_index,
                        ),
                    );
                }
            }
        } else {
            return aver_rt::AverList::prepend(
                crate::aver_generated::domain::token::Token::TkStr(acc),
                &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
                    src,
                    pos,
                    &*__str_index,
                ),
            );
        }
    }
}

/// Synthesized indexed worker of `tokenizeInterpDigit`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeInterpDigit__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    {
        let (n, newPos) = crate::aver_generated::domain::lexer::chars::readNumber(
            src.clone(),
            pos,
            aver_rt::AverInt::from_i64(0),
        );
        crate::aver_generated::domain::lexer::tokenizeInterpAfterInt__indexed(
            src,
            newPos,
            n,
            __str_index,
        )
    }
}

/// Synthesized indexed worker of `tokenizeInterpAfterInt`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeInterpAfterInt__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    n: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
        Some(c @ _) => {
            if (&*c == ".") {
                crate::aver_generated::domain::lexer::tokenizeInterpAfterIntDot__indexed(
                    src,
                    pos,
                    n,
                    __str_index,
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkInt(n),
                    &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
                        src,
                        pos,
                        __str_index,
                    ),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkInt(n),
            &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
                src,
                pos,
                __str_index,
            ),
        ),
    }
}

/// Synthesized indexed worker of `tokenizeInterpAfterIntDot`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeInterpAfterIntDot__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    n: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
    match aver_rt::string_index_char_at(&src, &__str_index, &nextPos) {
        Some(d @ _) => {
            if crate::aver_generated::domain::lexer::chars::isDigit(d) {
                crate::aver_generated::domain::lexer::tokenizeInterpFloat__indexed(
                    src,
                    nextPos,
                    n,
                    __str_index,
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkInt(n),
                    &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
                        src,
                        pos,
                        __str_index,
                    ),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkInt(n),
            &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(
                src,
                pos,
                __str_index,
            ),
        ),
    }
}

/// Synthesized indexed worker of `tokenizeInterpFloat`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeInterpFloat__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    intPart: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    {
        let (decPart, newPos) = crate::aver_generated::domain::lexer::chars::readNumber(
            src.clone(),
            pos.clone(),
            aver_rt::AverInt::from_i64(0),
        );
        crate::aver_generated::domain::lexer::tokenizeInterpBuildFloat__indexed(
            src,
            newPos.clone(),
            intPart,
            decPart,
            newPos.sub(&pos),
            __str_index,
        )
    }
}

/// Synthesized indexed worker of `tokenizeInterpBuildFloat`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeInterpBuildFloat__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    intPart: aver_rt::AverInt,
    decPart: aver_rt::AverInt,
    decDigits: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let f = (intPart.to_f64()
        + (decPart.to_f64() / crate::aver_generated::domain::lexer::pow10(decDigits)));
    aver_rt::AverList::prepend(
        crate::aver_generated::domain::token::Token::TkFloat(f),
        &crate::aver_generated::domain::lexer::tokenizeInterpExpr__indexed(src, pos, __str_index),
    )
}

/// Synthesized indexed worker of `tokenizeSlashOrComment`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeSlashOrComment__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
    match aver_rt::string_index_char_at(&src, &__str_index, &nextPos) {
        Some(c @ _) => {
            if (&*c == "/") {
                crate::aver_generated::domain::lexer::skipLineComment__indexed(
                    src,
                    pos.add(&aver_rt::AverInt::from_i64(2)),
                    __str_index.clone(),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkSlash,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        nextPos,
                        __str_index,
                    ),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkSlash,
            &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, __str_index),
        ),
    }
}

/// Synthesized indexed worker of `skipLineComment`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn skipLineComment__indexed(
    mut src: AverStr,
    mut pos: aver_rt::AverInt,
    __str_index: aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    let __str_index = std::sync::Arc::new(__str_index);
    loop {
        crate::cancel_checkpoint();
        let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
        if (pos < aver_rt::AverInt::from_i64(src.chars().count() as i64)) {
            match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
                Some(c @ _) => {
                    if (&*c == "\n") {
                        return crate::aver_generated::domain::lexer::tokenizeNewline__indexed(
                            src,
                            nextPos,
                            &*__str_index,
                        );
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

/// Synthesized indexed worker of `tokenizeDot`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeDot__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
    match aver_rt::string_index_char_at(&src, &__str_index, &nextPos) {
        Some(c @ _) => {
            if (&*c == ".") {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkDotDot,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        pos.add(&aver_rt::AverInt::from_i64(2)),
                        __str_index,
                    ),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkDot,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        nextPos,
                        __str_index,
                    ),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkDot,
            &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, __str_index),
        ),
    }
}

/// Synthesized indexed worker of `tokenizeLt`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeLt__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
    match aver_rt::string_index_char_at(&src, &__str_index, &nextPos) {
        Some(c @ _) => {
            if (&*c == "=") {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkLte,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        pos.add(&aver_rt::AverInt::from_i64(2)),
                        __str_index,
                    ),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkLt,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        nextPos,
                        __str_index,
                    ),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkLt,
            &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, __str_index),
        ),
    }
}

/// Synthesized indexed worker of `tokenizeGt`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeGt__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
    match aver_rt::string_index_char_at(&src, &__str_index, &nextPos) {
        Some(c @ _) => {
            if (&*c == "=") {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkGte,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        pos.add(&aver_rt::AverInt::from_i64(2)),
                        __str_index,
                    ),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkGt,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        nextPos,
                        __str_index,
                    ),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkGt,
            &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, __str_index),
        ),
    }
}

/// Synthesized indexed worker of `tokenizeBang`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn tokenizeBang__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
    match aver_rt::string_index_char_at(&src, &__str_index, &nextPos) {
        Some(c @ _) => {
            if (&*c == "=") {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkNeq,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        pos.add(&aver_rt::AverInt::from_i64(2)),
                        __str_index,
                    ),
                )
            } else {
                aver_rt::AverList::prepend(
                    crate::aver_generated::domain::token::Token::TkBang,
                    &crate::aver_generated::domain::lexer::tokenize__indexed(
                        src,
                        nextPos,
                        __str_index,
                    ),
                )
            }
        }
        None => aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkBang,
            &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, __str_index),
        ),
    }
}

/// Synthesized indexed worker of `tokenizeEq`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeEq__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let nextPos = pos.add(&aver_rt::AverInt::from_i64(1));
    let pos2 = pos.add(&aver_rt::AverInt::from_i64(2));
    {
        let __int_match_subject = aver_rt::string_index_code_at(&src, &__str_index, &nextPos);
        if __int_match_subject == -1i64 {
            aver_rt::AverList::prepend(
                crate::aver_generated::domain::token::Token::TkEq,
                &crate::aver_generated::domain::lexer::tokenize__indexed(src, nextPos, __str_index),
            )
        } else {
            let __str_ix_c1 = __int_match_subject;
            {
                let __dispatch_subject = __str_ix_c1;
                if __dispatch_subject == 61i64 {
                    aver_rt::AverList::prepend(
                        crate::aver_generated::domain::token::Token::TkEqEq,
                        &crate::aver_generated::domain::lexer::tokenize__indexed(
                            src,
                            pos2,
                            __str_index,
                        ),
                    )
                } else {
                    if __dispatch_subject == 62i64 {
                        aver_rt::AverList::prepend(
                            crate::aver_generated::domain::token::Token::TkFatArrow,
                            &crate::aver_generated::domain::lexer::tokenize__indexed(
                                src,
                                pos2,
                                __str_index,
                            ),
                        )
                    } else {
                        aver_rt::AverList::prepend(
                            crate::aver_generated::domain::token::Token::TkEq,
                            &crate::aver_generated::domain::lexer::tokenize__indexed(
                                src,
                                nextPos,
                                __str_index,
                            ),
                        )
                    }
                }
            }
        }
    }
}

/// Synthesized indexed worker of `tokenizeNewline`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn tokenizeNewline__indexed(
    src: AverStr,
    pos: aver_rt::AverInt,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let r = crate::aver_generated::domain::lexer::countIndent__indexed(
        src.clone(),
        pos,
        aver_rt::AverInt::from_i64(0),
        __str_index,
    );
    {
        let (indent, newPos) = r;
        aver_rt::AverList::prepend(
            crate::aver_generated::domain::token::Token::TkNewline,
            &aver_rt::AverList::prepend(
                crate::aver_generated::domain::token::Token::TkInt(
                    aver_rt::AverInt::from_i64(0)
                        .sub(&indent)
                        .sub(&aver_rt::AverInt::from_i64(1)),
                ),
                &crate::aver_generated::domain::lexer::tokenize__indexed(src, newPos, __str_index),
            ),
        )
    }
}

/// Synthesized indexed worker of `lex`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
pub fn lex__indexed(
    src: AverStr,
    __str_index: &aver_rt::StringIndex,
) -> aver_rt::AverList<crate::aver_generated::domain::token::Token> {
    crate::cancel_checkpoint();
    let raw = crate::aver_generated::domain::lexer::tokenize__indexed(
        src,
        aver_rt::AverInt::from_i64(0),
        __str_index,
    );
    let processed = crate::aver_generated::domain::lexer::processIndentation(
        &raw,
        &aver_rt::AverIntList::from_vec(vec![aver_rt::AverInt::from_i64(0)]),
    );
    processed
}

pub mod chars;
