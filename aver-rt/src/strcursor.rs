//! Codepoint cursor over a `String`, and single-codepoint decoding.
//!
//! Backs the six intrinsics the chars-fusion pass emits. They exist
//! here, on plain `&str`, for one reason: the VM and compiled Rust must
//! agree to the codepoint, and the cheapest way to guarantee that is to
//! have one implementation and two thin call sites.
//!
//! The cursor is a BYTE offset that only ever lands on a character
//! boundary, because the only way to move it is [`str_cursor_next`],
//! which steps by the UTF-8 length of the character it is looking at.
//! `String.chars` iterates Unicode scalar values (`str::chars`), so
//! stepping this way visits exactly the same elements in the same
//! order — which a byte-per-step cursor would not: an even-length check
//! over `"é7"` counts two characters and three bytes.

/// True when the cursor has run off the end of `s` — the fused
/// spelling of `match <the rest of the list> { [] -> … }`.
#[inline]
pub fn str_cursor_end(s: &str, i: usize) -> bool {
    i >= s.len()
}

/// The one-character string at byte offset `i` — the fused spelling of
/// the head binding in `[head, ..tail]`. Empty when the cursor is at or
/// past the end, which the emitted code never asks for: every read is
/// guarded by a [`str_cursor_end`] test on the same offset.
#[inline]
pub fn str_cursor_head(s: &str, i: usize) -> &str {
    match char_at(s, i) {
        Some((c, start)) => &s[start..start + c.len_utf8()],
        None => "",
    }
}

/// The cursor one character on. Saturates at the end of the string, so
/// a cursor can never step past `s.len()` and the end test stays
/// monotone.
#[inline]
pub fn str_cursor_next(s: &str, i: usize) -> usize {
    match char_at(s, i) {
        Some((c, start)) => start + c.len_utf8(),
        None => s.len(),
    }
}

/// The codepoint of exactly the character [`str_cursor_head`] would
/// return at byte offset `i`, or `-1` past the end (where the head is
/// the empty string, whose [`str_code1`] is `-1`). One read instead of
/// building the one-character string and decoding it again.
#[inline]
pub fn str_cursor_code(s: &str, i: usize) -> i64 {
    match char_at(s, i) {
        Some((c, _)) => i64::from(u32::from(c)),
        None => -1,
    }
}

/// [`str_code1_lower`] of the one-character string of codepoint `c`,
/// without building the string: the full Unicode lowercase of the
/// character, or `-1` when `c` is not a Unicode scalar value or its
/// lowercase is not exactly one character (`U+0130` expands to two).
///
/// The general arm iterates `char::to_lowercase` — for a
/// single-character string that is the same answer `str::to_lowercase`
/// gives, which the exhaustive test below checks over every scalar, so
/// the codepoint route cannot drift from the string route it stands in
/// for.
#[inline]
pub fn str_fold_lower(c: i64) -> i64 {
    let Some(ch) = u32::try_from(c).ok().and_then(char::from_u32) else {
        return -1;
    };
    if ch.is_ascii() {
        return i64::from((ch as u8).to_ascii_lowercase());
    }
    let mut it = ch.to_lowercase();
    match (it.next(), it.next()) {
        (Some(l), None) => i64::from(u32::from(l)),
        _ => -1,
    }
}

/// [`str_code1_upper`] of the one-character string of codepoint `c` —
/// mirror of [`str_fold_lower`] (`U+00DF` uppercases to `"SS"`, two
/// characters, so `-1`).
#[inline]
pub fn str_fold_upper(c: i64) -> i64 {
    let Some(ch) = u32::try_from(c).ok().and_then(char::from_u32) else {
        return -1;
    };
    if ch.is_ascii() {
        return i64::from((ch as u8).to_ascii_uppercase());
    }
    let mut it = ch.to_uppercase();
    match (it.next(), it.next()) {
        (Some(u), None) => i64::from(u32::from(u)),
        _ => -1,
    }
}

/// The single codepoint of `s`, or `-1` when `s` is not exactly one
/// character long.
///
/// `-1` is what makes the match rewrite exact rather than approximate:
/// every literal the pass rewrites is a single ASCII codepoint, so a
/// negative sentinel equals none of them and the wildcard arm fires for
/// precisely the strings that matched no literal before.
#[inline]
pub fn str_code1(s: &str) -> i64 {
    let mut it = s.chars();
    match (it.next(), it.next()) {
        (Some(c), None) => i64::from(u32::from(c)),
        _ => -1,
    }
}

/// [`str_code1`] of `String.toLower(s)`, without building the lowered
/// string when it can be avoided.
///
/// The general arm calls `str::to_lowercase` — the same routine
/// `String.toLower` itself calls on every backend that has this pass —
/// so the rewrite cannot drift from the builtin it replaces, not even
/// on `U+0130`, which lowercases to TWO characters and therefore
/// matches no single-character literal (`-1`, wildcard) either way.
///
/// The fast arm is exact for the same reason it is fast: the lowercase
/// of an ASCII character is that ASCII character lowercased, one
/// character wide.
#[inline]
pub fn str_code1_lower(s: &str) -> i64 {
    let mut it = s.chars();
    match (it.next(), it.next()) {
        (Some(c), None) if c.is_ascii() => i64::from((c as u8).to_ascii_lowercase()),
        _ => str_code1(&s.to_lowercase()),
    }
}

/// [`str_code1`] of `String.toUpper(s)` — mirror of
/// [`str_code1_lower`], with `str::to_uppercase` as the general arm
/// (`U+00DF` uppercases to `"SS"`, two characters, so `-1`).
#[inline]
pub fn str_code1_upper(s: &str) -> i64 {
    let mut it = s.chars();
    match (it.next(), it.next()) {
        (Some(c), None) if c.is_ascii() => i64::from((c as u8).to_ascii_uppercase()),
        _ => str_code1(&s.to_uppercase()),
    }
}

/// The character the cursor points at, plus the byte offset it starts
/// at. `None` past the end. An offset that is not a character boundary
/// cannot arise from the emitted code (the only step is
/// [`str_cursor_next`]); should one arrive anyway, it reads the
/// character the offset falls inside rather than panicking on a slice.
#[inline]
fn char_at(s: &str, i: usize) -> Option<(char, usize)> {
    if i >= s.len() {
        return None;
    }
    let mut start = i;
    while !s.is_char_boundary(start) {
        start -= 1;
    }
    s[start..].chars().next().map(|c| (c, start))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The cursor visits what `String.chars` visits, in order — the
    /// whole reason the pass is allowed to replace one with the other.
    #[test]
    fn the_cursor_walks_the_same_characters_as_chars() {
        for s in ["", "abc", "é7", "🦀x🦀", "İi", "aé🦀z"] {
            let mut seen = Vec::new();
            let mut i = 0;
            while !str_cursor_end(s, i) {
                seen.push(str_cursor_head(s, i).to_string());
                i = str_cursor_next(s, i);
            }
            let expected: Vec<String> = s.chars().map(|c| c.to_string()).collect();
            assert_eq!(seen, expected, "cursor over {s:?}");
        }
    }

    /// A byte cursor would report three elements for `"é7"`. The probe
    /// that motivated this pass found exactly that: an even-length
    /// check counts characters, so a byte step is not a rewrite of the
    /// same program.
    #[test]
    fn a_multibyte_character_is_one_step_not_two() {
        assert_eq!(str_cursor_next("é7", 0), 2);
        assert_eq!(str_cursor_head("é7", 0), "é");
        assert_eq!(str_cursor_head("é7", 2), "7");
        assert!(str_cursor_end("é7", 3));
    }

    #[test]
    fn code1_is_the_codepoint_only_for_one_character_strings() {
        assert_eq!(str_code1("a"), 97);
        assert_eq!(str_code1("é"), 233);
        assert_eq!(str_code1(""), -1);
        assert_eq!(str_code1("ab"), -1);
    }

    /// The folding forms answer what `str_code1(&s.to_lower/upper())`
    /// answers, for every Unicode scalar value. The probe checked this
    /// call site over all 1_114_112 scalars; here it is as a test, so
    /// the ASCII fast path cannot silently stop agreeing with the
    /// builtin it stands in for.
    #[test]
    fn folding_agrees_with_the_builtin_on_every_scalar() {
        for cp in 0u32..=0x10_FFFF {
            let Some(c) = char::from_u32(cp) else {
                continue;
            };
            let s = c.to_string();
            assert_eq!(
                str_code1_lower(&s),
                str_code1(&s.to_lowercase()),
                "lower {cp:#x}"
            );
            assert_eq!(
                str_code1_upper(&s),
                str_code1(&s.to_uppercase()),
                "upper {cp:#x}"
            );
        }
    }

    /// The cursor's code read answers what decoding the head would
    /// answer, at every position including past the end — the whole
    /// license for binding the code instead of the head.
    #[test]
    fn the_code_at_the_cursor_is_the_code_of_the_head() {
        for s in ["", "abc", "é7", "🦀x🦀", "İi", "aé🦀z"] {
            let mut i = 0;
            while !str_cursor_end(s, i) {
                assert_eq!(
                    str_cursor_code(s, i),
                    str_code1(str_cursor_head(s, i)),
                    "cursor over {s:?} at {i}"
                );
                i = str_cursor_next(s, i);
            }
            assert_eq!(str_cursor_code(s, s.len()), -1, "past the end of {s:?}");
        }
    }

    /// The codepoint-level folds answer what the string-level folds
    /// answer, for every Unicode scalar value — the soundness receipt
    /// for handing a classifier the code instead of the string. Runs in
    /// seconds; do not sample it down.
    #[test]
    fn codepoint_folding_agrees_with_the_string_folding_on_every_scalar() {
        for cp in 0u32..=0x10_FFFF {
            let Some(c) = char::from_u32(cp) else {
                continue;
            };
            let s = c.to_string();
            let code = i64::from(cp);
            assert_eq!(str_fold_lower(code), str_code1_lower(&s), "lower {cp:#x}");
            assert_eq!(str_fold_upper(code), str_code1_upper(&s), "upper {cp:#x}");
        }
        // And off the scalar range there is no character to fold.
        assert_eq!(str_fold_lower(-1), -1);
        assert_eq!(str_fold_upper(-1), -1);
        assert_eq!(str_fold_lower(0xD800), -1, "surrogates are not scalars");
        assert_eq!(str_fold_lower(0x11_0000), -1, "past the last scalar");
    }

    /// `U+212A KELVIN SIGN` lowercases to `"k"`. A rewrite that folded
    /// case by ASCII arithmetic on the codepoint would answer `-1` here
    /// and take the wildcard, which is a different answer than the
    /// program gave. It is why the general arm calls `to_lowercase`.
    #[test]
    fn a_non_ascii_character_that_lowercases_into_ascii_keeps_its_arm() {
        assert_eq!(str_code1_lower("\u{212A}"), i64::from(b'k'));
        assert_eq!(str_code1_lower("\u{0130}"), -1);
        assert_eq!(str_code1_upper("\u{00DF}"), -1);
    }
}
