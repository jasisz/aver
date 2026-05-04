#[allow(unused_imports)]
use crate::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;

/// Check if a single character is a digit.
#[inline(always)]
pub fn isDigit(c: AverStr) -> bool {
    crate::cancel_checkpoint();
    { let __dispatch_subject = c; if &*__dispatch_subject == "0" { true } else { if &*__dispatch_subject == "1" { true } else { if &*__dispatch_subject == "2" { true } else { if &*__dispatch_subject == "3" { true } else { if &*__dispatch_subject == "4" { true } else { if &*__dispatch_subject == "5" { true } else { if &*__dispatch_subject == "6" { true } else { if &*__dispatch_subject == "7" { true } else { if &*__dispatch_subject == "8" { true } else { if &*__dispatch_subject == "9" { true } else { false } } } } } } } } } } }
}

/// Check if a character is a lowercase letter.
#[inline(always)]
pub fn isLower(c: AverStr) -> bool {
    crate::cancel_checkpoint();
    if (c < AverStr::from("a")) { false } else { (c <= AverStr::from("z")) }
}

/// Check if a character is an uppercase letter.
#[inline(always)]
pub fn isUpper(c: AverStr) -> bool {
    crate::cancel_checkpoint();
    if (c < AverStr::from("A")) { false } else { (c <= AverStr::from("Z")) }
}

/// Check if a character is a letter or underscore.
#[inline(always)]
pub fn isLetterOrUnderscore(c: AverStr) -> bool {
    crate::cancel_checkpoint();
    if isLower(c.clone()) { true } else { if isUpper(c.clone()) { true } else { (&*c == "_") } }
}

/// Check if a single character is a letter or underscore.
#[inline(always)]
pub fn isAlpha(c: AverStr) -> bool {
    crate::cancel_checkpoint();
    if ((c.chars().count() as i64) == 1i64) { isLetterOrUnderscore(c) } else { false }
}

/// Check if a character is alphanumeric or underscore.
#[inline(always)]
pub fn isAlphaNum(c: AverStr) -> bool {
    crate::cancel_checkpoint();
    if isAlpha(c.clone()) { true } else { isDigit(c) }
}

/// Convert a digit character to its integer value.
#[inline(always)]
pub fn digitVal(c: AverStr) -> i64 {
    crate::cancel_checkpoint();
    { let __dispatch_subject = c; if &*__dispatch_subject == "0" { 0i64 } else { if &*__dispatch_subject == "1" { 1i64 } else { if &*__dispatch_subject == "2" { 2i64 } else { if &*__dispatch_subject == "3" { 3i64 } else { if &*__dispatch_subject == "4" { 4i64 } else { if &*__dispatch_subject == "5" { 5i64 } else { if &*__dispatch_subject == "6" { 6i64 } else { if &*__dispatch_subject == "7" { 7i64 } else { if &*__dispatch_subject == "8" { 8i64 } else { if &*__dispatch_subject == "9" { 9i64 } else { 0i64 } } } } } } } } } } }
}

/// Read consecutive digits from pos, return (number, newPos).
#[inline(always)]
pub fn readNumberLoop(mut src: AverStr, mut pos: i64, mut acc: i64) -> (i64, i64) {
    loop {
        crate::cancel_checkpoint();
        return if (pos < (src.chars().count() as i64)) { match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() { Some(c) => { if isDigit(c.clone()) { {
            let __tmp1 = (pos + 1i64);
            let __tmp2 = ((acc * 10i64) + digitVal(c));
            pos = __tmp1;
            acc = __tmp2;
            continue;
        } } else { (acc, pos) } }, None => { (acc, pos) } } } else { (acc, pos) };
    }
}

/// Read consecutive digits from pos, return (number, newPos).
#[inline(always)]
pub fn readNumber(src: AverStr, pos: i64, acc: i64) -> (i64, i64) {
    crate::cancel_checkpoint();
    readNumberLoop(src.clone(), pos, acc)
}

/// Check if character can continue a dotted identifier (alphanumeric, underscore, or dot).
#[inline(always)]
pub fn isIdentCharDotted(c: AverStr) -> bool {
    crate::cancel_checkpoint();
    if isAlphaNum(c.clone()) { true } else { (&*c == ".") }
}

/// Check if character can continue a plain identifier (alphanumeric, underscore only).
#[inline(always)]
pub fn isIdentCharPlain(c: AverStr) -> bool {
    crate::cancel_checkpoint();
    isAlphaNum(c.clone())
}

/// Read identifier including dots (for qualified names like List.prepend).
#[inline(always)]
pub fn readIdentLoopDotted(mut src: AverStr, mut pos: i64, mut acc: AverStr) -> (AverStr, i64) {
    loop {
        crate::cancel_checkpoint();
        return if (pos < (src.chars().count() as i64)) { match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() { Some(c) => { if isIdentCharDotted(c.clone()) { {
            let __tmp1 = (pos + 1i64);
            let __tmp2 = (acc + &c);
            pos = __tmp1;
            acc = __tmp2;
            continue;
        } } else { (acc, pos) } }, None => { (acc, pos) } } } else { (acc, pos) };
    }
}

/// Read identifier without dots (for local variables).
#[inline(always)]
pub fn readIdentLoopPlain(mut src: AverStr, mut pos: i64, mut acc: AverStr) -> (AverStr, i64) {
    loop {
        crate::cancel_checkpoint();
        return if (pos < (src.chars().count() as i64)) { match (src.chars().nth(pos as usize).map(|c| c.to_string())).into_aver() { Some(c) => { if isIdentCharPlain(c.clone()) { {
            let __tmp1 = (pos + 1i64);
            let __tmp2 = (acc + &c);
            pos = __tmp1;
            acc = __tmp2;
            continue;
        } } else { (acc, pos) } }, None => { (acc, pos) } } } else { (acc, pos) };
    }
}

/// Read identifier, dotted if starts with uppercase.
#[inline(always)]
pub fn readIdent(src: AverStr, pos: i64, acc: AverStr, dotted: bool) -> (AverStr, i64) {
    crate::cancel_checkpoint();
    if dotted { readIdentLoopDotted(src, pos, acc) } else { readIdentLoopPlain(src, pos, acc) }
}

/// Classify an identifier as keyword or plain ident.
#[inline(always)]
pub fn keywordOrIdent(s: AverStr) -> Token {
    crate::cancel_checkpoint();
    { let __dispatch_subject = s.clone(); if &*__dispatch_subject == "fn" { Token::TkFn.clone() } else { if &*__dispatch_subject == "match" { Token::TkMatch.clone() } else { if &*__dispatch_subject == "true" { Token::TkTrue.clone() } else { if &*__dispatch_subject == "false" { Token::TkFalse.clone() } else { if &*__dispatch_subject == "module" { Token::TkIdent(AverStr::from("module")) } else { if &*__dispatch_subject == "type" { Token::TkIdent(AverStr::from("type")) } else { if &*__dispatch_subject == "record" { Token::TkIdent(AverStr::from("record")) } else { if &*__dispatch_subject == "verify" { Token::TkIdent(AverStr::from("verify")) } else { if &*__dispatch_subject == "depends" { Token::TkIdent(AverStr::from("depends")) } else { if &*__dispatch_subject == "exposes" { Token::TkIdent(AverStr::from("exposes")) } else { if &*__dispatch_subject == "intent" { Token::TkIdent(AverStr::from("intent")) } else { if &*__dispatch_subject == "decision" { Token::TkIdent(AverStr::from("decision")) } else { Token::TkIdent(s) } } } } } } } } } } } } }
}
