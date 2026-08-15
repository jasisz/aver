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
