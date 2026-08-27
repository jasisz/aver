#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;
#[allow(unused_imports)]
use crate::*;

/// Check if a single character is a digit.
#[inline(always)]
pub fn isDigit(c @ _: AverStr) -> bool {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
        if __dispatch_subject == aver_rt::AverInt::from_i64(48) {
            true
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(49) {
                true
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(50) {
                    true
                } else {
                    if __dispatch_subject == aver_rt::AverInt::from_i64(51) {
                        true
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(52) {
                            true
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(53) {
                                true
                            } else {
                                if __dispatch_subject == aver_rt::AverInt::from_i64(54) {
                                    true
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(55) {
                                        true
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(56) {
                                            true
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(57)
                                            {
                                                true
                                            } else {
                                                false
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

/// Check if a character is a lowercase letter.
#[inline(always)]
pub fn isLower(c @ _: AverStr) -> bool {
    crate::cancel_checkpoint();
    if (c < AverStr::from("a")) {
        false
    } else {
        (c <= AverStr::from("z"))
    }
}

/// Check if a character is an uppercase letter.
#[inline(always)]
pub fn isUpper(c @ _: AverStr) -> bool {
    crate::cancel_checkpoint();
    if (c < AverStr::from("A")) {
        false
    } else {
        (c <= AverStr::from("Z"))
    }
}

/// Check if a character is a letter or underscore.
#[inline(always)]
pub fn isLetterOrUnderscore(c @ _: AverStr) -> bool {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::lexer::chars::isLower(c.clone()) {
        true
    } else {
        if crate::aver_generated::domain::lexer::chars::isUpper(c.clone()) {
            true
        } else {
            (&*c == "_")
        }
    }
}

/// Check if a single character is a letter or underscore.
#[inline(always)]
pub fn isAlpha(c @ _: AverStr) -> bool {
    crate::cancel_checkpoint();
    if (aver_rt::AverInt::from_i64(c.chars().count() as i64) == aver_rt::AverInt::from_i64(1)) {
        crate::aver_generated::domain::lexer::chars::isLetterOrUnderscore(c)
    } else {
        false
    }
}

/// Check if a character is alphanumeric or underscore.
#[inline(always)]
pub fn isAlphaNum(c @ _: AverStr) -> bool {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::lexer::chars::isAlpha(c.clone()) {
        true
    } else {
        crate::aver_generated::domain::lexer::chars::isDigit(c)
    }
}

/// Convert a digit character to its integer value.
#[inline(always)]
pub fn digitVal(c @ _: AverStr) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
        if __dispatch_subject == aver_rt::AverInt::from_i64(48) {
            aver_rt::AverInt::from_i64(0)
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(49) {
                aver_rt::AverInt::from_i64(1)
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(50) {
                    aver_rt::AverInt::from_i64(2)
                } else {
                    if __dispatch_subject == aver_rt::AverInt::from_i64(51) {
                        aver_rt::AverInt::from_i64(3)
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(52) {
                            aver_rt::AverInt::from_i64(4)
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(53) {
                                aver_rt::AverInt::from_i64(5)
                            } else {
                                if __dispatch_subject == aver_rt::AverInt::from_i64(54) {
                                    aver_rt::AverInt::from_i64(6)
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(55) {
                                        aver_rt::AverInt::from_i64(7)
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(56) {
                                            aver_rt::AverInt::from_i64(8)
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(57)
                                            {
                                                aver_rt::AverInt::from_i64(9)
                                            } else {
                                                aver_rt::AverInt::from_i64(0)
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

/// Read consecutive digits from pos, return (number, newPos).
#[inline(always)]
pub fn readNumberLoop(
    src @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    acc @ _: aver_rt::AverInt,
) -> (aver_rt::AverInt, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::chars::readNumberLoop__indexed(
        src.clone(),
        pos,
        acc,
        aver_rt::string_index_build(&src),
    )
}

/// Read consecutive digits from pos, return (number, newPos).
#[inline(always)]
pub fn readNumber(
    src @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    acc @ _: aver_rt::AverInt,
) -> (aver_rt::AverInt, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::chars::readNumber__indexed(
        src.clone(),
        pos,
        acc,
        &aver_rt::string_index_build(&src),
    )
}

/// Check if character can continue a dotted identifier (alphanumeric, underscore, or dot).
#[inline(always)]
pub fn isIdentCharDotted(c @ _: AverStr) -> bool {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::lexer::chars::isAlphaNum(c.clone()) {
        true
    } else {
        (&*c == ".")
    }
}

/// Check if character can continue a plain identifier (alphanumeric, underscore only).
#[inline(always)]
pub fn isIdentCharPlain(c @ _: AverStr) -> bool {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::chars::isAlphaNum(c)
}

/// Read identifier including dots (for qualified names like List.prepend).
#[inline(always)]
pub fn readIdentLoopDotted(
    src @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    acc @ _: AverStr,
) -> (AverStr, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::chars::readIdentLoopDotted__indexed(
        src.clone(),
        pos,
        acc,
        aver_rt::string_index_build(&src),
    )
}

/// Read identifier without dots (for local variables).
#[inline(always)]
pub fn readIdentLoopPlain(
    src @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    acc @ _: AverStr,
) -> (AverStr, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::chars::readIdentLoopPlain__indexed(
        src.clone(),
        pos,
        acc,
        aver_rt::string_index_build(&src),
    )
}

/// Read identifier, dotted if starts with uppercase.
#[inline(always)]
pub fn readIdent(
    src @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    acc @ _: AverStr,
    dotted @ _: bool,
) -> (AverStr, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::chars::readIdent__indexed(
        src.clone(),
        pos,
        acc,
        dotted,
        &aver_rt::string_index_build(&src),
    )
}

/// Classify an identifier as keyword or plain ident.
#[inline(always)]
pub fn keywordOrIdent(s @ _: AverStr) -> crate::aver_generated::domain::token::Token {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = s.clone();
        if &*__dispatch_subject == "fn" {
            crate::aver_generated::domain::token::Token::TkFn
        } else {
            if &*__dispatch_subject == "match" {
                crate::aver_generated::domain::token::Token::TkMatch
            } else {
                if &*__dispatch_subject == "true" {
                    crate::aver_generated::domain::token::Token::TkTrue
                } else {
                    if &*__dispatch_subject == "false" {
                        crate::aver_generated::domain::token::Token::TkFalse
                    } else {
                        if &*__dispatch_subject == "module" {
                            crate::aver_generated::domain::token::Token::TkIdent(AverStr::from(
                                "module",
                            ))
                        } else {
                            if &*__dispatch_subject == "type" {
                                crate::aver_generated::domain::token::Token::TkIdent(AverStr::from(
                                    "type",
                                ))
                            } else {
                                if &*__dispatch_subject == "record" {
                                    crate::aver_generated::domain::token::Token::TkIdent(
                                        AverStr::from("record"),
                                    )
                                } else {
                                    if &*__dispatch_subject == "verify" {
                                        crate::aver_generated::domain::token::Token::TkIdent(
                                            AverStr::from("verify"),
                                        )
                                    } else {
                                        if &*__dispatch_subject == "depends" {
                                            crate::aver_generated::domain::token::Token::TkIdent(
                                                AverStr::from("depends"),
                                            )
                                        } else {
                                            if &*__dispatch_subject == "exposes" {
                                                crate::aver_generated::domain::token::Token::TkIdent(
                                                    AverStr::from("exposes"),
                                                )
                                            } else {
                                                if &*__dispatch_subject == "intent" {
                                                    crate::aver_generated::domain::token::Token::TkIdent(AverStr::from("intent"))
                                                } else {
                                                    if &*__dispatch_subject == "decision" {
                                                        crate::aver_generated::domain::token::Token::TkIdent(AverStr::from("decision"))
                                                    } else {
                                                        crate::aver_generated::domain::token::Token::TkIdent(s)
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

/// Synthesized indexed worker of `readNumberLoop`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn readNumberLoop__indexed(
    mut src @ _: AverStr,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverInt,
    __str_index @ _: aver_rt::StringIndex,
) -> (aver_rt::AverInt, aver_rt::AverInt) {
    let __str_index @ _ = std::sync::Arc::new(__str_index);
    loop {
        crate::cancel_checkpoint();
        if (pos < aver_rt::AverInt::from_i64(src.chars().count() as i64)) {
            {
                let __int_match_subject = aver_rt::string_index_code_at(&src, &__str_index, &pos);
                if __int_match_subject == -1i64 {
                    return (acc, pos);
                } else {
                    let __str_ix_c1 = __int_match_subject;
                    if crate::aver_generated::domain::lexer::chars::isDigit__code(__str_ix_c1) {
                        {
                            let __tco1 = pos.add(&aver_rt::AverInt::from_i64(1));
                            let __tco2 = acc.mul(&aver_rt::AverInt::from_i64(10)).add(
                                &crate::aver_generated::domain::lexer::chars::digitVal__code(
                                    __str_ix_c1,
                                ),
                            );
                            pos = __tco1;
                            acc = __tco2;
                            continue;
                        }
                    } else {
                        return (acc, pos);
                    }
                }
            }
        } else {
            return (acc, pos);
        }
    }
}

/// Synthesized indexed worker of `readNumber`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn readNumber__indexed(
    src @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    acc @ _: aver_rt::AverInt,
    __str_index @ _: &aver_rt::StringIndex,
) -> (aver_rt::AverInt, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::lexer::chars::readNumberLoop__indexed(
        src,
        pos,
        acc,
        __str_index.clone(),
    )
}

/// Synthesized indexed worker of `readIdentLoopDotted`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn readIdentLoopDotted__indexed(
    mut src @ _: AverStr,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: AverStr,
    __str_index @ _: aver_rt::StringIndex,
) -> (AverStr, aver_rt::AverInt) {
    let __str_index @ _ = std::sync::Arc::new(__str_index);
    loop {
        crate::cancel_checkpoint();
        if (pos < aver_rt::AverInt::from_i64(src.chars().count() as i64)) {
            match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
                Some(c @ _) => {
                    if crate::aver_generated::domain::lexer::chars::isIdentCharDotted(c.clone()) {
                        {
                            let __tco1 = pos.add(&aver_rt::AverInt::from_i64(1));
                            let __tco2 = (acc + &c);
                            pos = __tco1;
                            acc = __tco2;
                            continue;
                        }
                    } else {
                        return (acc, pos);
                    }
                }
                None => {
                    return (acc, pos);
                }
            }
        } else {
            return (acc, pos);
        }
    }
}

/// Synthesized indexed worker of `readIdentLoopPlain`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn readIdentLoopPlain__indexed(
    mut src @ _: AverStr,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: AverStr,
    __str_index @ _: aver_rt::StringIndex,
) -> (AverStr, aver_rt::AverInt) {
    let __str_index @ _ = std::sync::Arc::new(__str_index);
    loop {
        crate::cancel_checkpoint();
        if (pos < aver_rt::AverInt::from_i64(src.chars().count() as i64)) {
            match aver_rt::string_index_char_at(&src, &__str_index, &pos) {
                Some(c @ _) => {
                    if crate::aver_generated::domain::lexer::chars::isIdentCharPlain(c.clone()) {
                        {
                            let __tco1 = pos.add(&aver_rt::AverInt::from_i64(1));
                            let __tco2 = (acc + &c);
                            pos = __tco1;
                            acc = __tco2;
                            continue;
                        }
                    } else {
                        return (acc, pos);
                    }
                }
                None => {
                    return (acc, pos);
                }
            }
        } else {
            return (acc, pos);
        }
    }
}

/// Synthesized indexed worker of `readIdent`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn readIdent__indexed(
    src @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    acc @ _: AverStr,
    dotted @ _: bool,
    __str_index @ _: &aver_rt::StringIndex,
) -> (AverStr, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    if dotted {
        crate::aver_generated::domain::lexer::chars::readIdentLoopDotted__indexed(
            src,
            pos,
            acc,
            __str_index.clone(),
        )
    } else {
        crate::aver_generated::domain::lexer::chars::readIdentLoopPlain__indexed(
            src,
            pos,
            acc,
            __str_index.clone(),
        )
    }
}

/// Synthesized codepoint variant of `digitVal` for indexed character dispatch. Parameter `c` is the Unicode scalar of the one-character String accepted by the source function.
#[inline(always)]
pub fn digitVal__code(__str_index_code @ _: i64) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = __str_index_code;
        if __dispatch_subject == 48i64 {
            aver_rt::AverInt::from_i64(0)
        } else {
            if __dispatch_subject == 49i64 {
                aver_rt::AverInt::from_i64(1)
            } else {
                if __dispatch_subject == 50i64 {
                    aver_rt::AverInt::from_i64(2)
                } else {
                    if __dispatch_subject == 51i64 {
                        aver_rt::AverInt::from_i64(3)
                    } else {
                        if __dispatch_subject == 52i64 {
                            aver_rt::AverInt::from_i64(4)
                        } else {
                            if __dispatch_subject == 53i64 {
                                aver_rt::AverInt::from_i64(5)
                            } else {
                                if __dispatch_subject == 54i64 {
                                    aver_rt::AverInt::from_i64(6)
                                } else {
                                    if __dispatch_subject == 55i64 {
                                        aver_rt::AverInt::from_i64(7)
                                    } else {
                                        if __dispatch_subject == 56i64 {
                                            aver_rt::AverInt::from_i64(8)
                                        } else {
                                            if __dispatch_subject == 57i64 {
                                                aver_rt::AverInt::from_i64(9)
                                            } else {
                                                aver_rt::AverInt::from_i64(0)
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

/// Synthesized codepoint variant of `isDigit` for indexed character dispatch. Parameter `c` is the Unicode scalar of the one-character String accepted by the source function.
#[inline(always)]
pub fn isDigit__code(__str_index_code @ _: i64) -> bool {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = __str_index_code;
        if __dispatch_subject == 48i64 {
            true
        } else {
            if __dispatch_subject == 49i64 {
                true
            } else {
                if __dispatch_subject == 50i64 {
                    true
                } else {
                    if __dispatch_subject == 51i64 {
                        true
                    } else {
                        if __dispatch_subject == 52i64 {
                            true
                        } else {
                            if __dispatch_subject == 53i64 {
                                true
                            } else {
                                if __dispatch_subject == 54i64 {
                                    true
                                } else {
                                    if __dispatch_subject == 55i64 {
                                        true
                                    } else {
                                        if __dispatch_subject == 56i64 {
                                            true
                                        } else {
                                            if __dispatch_subject == 57i64 {
                                                true
                                            } else {
                                                false
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
