//! Loop-scoped codepoint index for repeated indexed string access.
//!
//! This is compiler plumbing, not an Aver surface type. The string-index
//! pass builds one value at the boundary of an indexed-access call cone and
//! threads it through synthesized workers. Public positions stay Unicode
//! scalar indices; the index only translates them to UTF-8 byte boundaries.

use std::sync::Arc;

use crate::{AverInt, AverStr, StaticStr};

/// UTF-8 byte boundaries for one immutable string.
///
/// The common representation is packed `u32`: Rust strings larger than 4 GiB
/// graduate to `usize` rather than making a valid Aver value unindexable. The
/// final boundary is always the source byte length, so a string containing
/// `n` Unicode scalar values has `n + 1` entries.
#[derive(Clone, Debug)]
pub struct StringIndex {
    boundaries: Boundaries,
}

#[derive(Clone, Debug)]
enum Boundaries {
    /// Every scalar value is one byte, so position `i` is byte `i`.
    Ascii(usize),
    U32(Arc<[u32]>),
    Usize(Arc<[usize]>),
}

impl StringIndex {
    /// Build the codepoint-to-byte table in one UTF-8 pass; an ASCII string
    /// needs no table.
    pub fn new(text: &str) -> Self {
        if text.is_ascii() {
            Self {
                boundaries: Boundaries::Ascii(text.len()),
            }
        } else if u32::try_from(text.len()).is_ok() {
            let mut boundaries = Vec::with_capacity(text.len() + 1);
            boundaries.extend(text.char_indices().map(|(byte, _)| {
                u32::try_from(byte).expect("byte offset is bounded by the checked string length")
            }));
            boundaries.push(
                u32::try_from(text.len()).expect("string length was checked before indexing"),
            );
            Self {
                boundaries: Boundaries::U32(boundaries.into()),
            }
        } else {
            let mut boundaries = Vec::with_capacity(text.len() + 1);
            boundaries.extend(text.char_indices().map(|(byte, _)| byte));
            boundaries.push(text.len());
            Self {
                boundaries: Boundaries::Usize(boundaries.into()),
            }
        }
    }

    /// Number of Unicode scalar values in the indexed string.
    pub fn char_len(&self) -> usize {
        match &self.boundaries {
            Boundaries::Ascii(len) => *len,
            Boundaries::U32(items) => items.len().saturating_sub(1),
            Boundaries::Usize(items) => items.len().saturating_sub(1),
        }
    }

    /// UTF-8 byte boundary for a clamped codepoint position.
    fn boundary(&self, index: usize) -> usize {
        match &self.boundaries {
            Boundaries::Ascii(_) => index,
            Boundaries::U32(items) => items[index] as usize,
            Boundaries::Usize(items) => items[index],
        }
    }
}

/// Build the hidden index threaded through a synthesized call cone.
#[inline]
pub fn string_index_build(text: &AverStr) -> StringIndex {
    StringIndex::new(text)
}

/// Indexed equivalent of `String.charAt`.
pub fn string_index_char_at(
    text: &AverStr,
    index: &StringIndex,
    position: &AverInt,
) -> Option<AverStr> {
    let position = position.to_usize()?;
    if position >= index.char_len() {
        return None;
    }
    if let Boundaries::Ascii(_) = index.boundaries {
        return Some(AverStr::from_static(
            &ASCII_CHARS[usize::from(text.as_bytes()[position])],
        ));
    }
    let from = index.boundary(position);
    let to = index.boundary(position + 1);
    Some(AverStr::from(&text[from..to]))
}

/// Codepoint-only indexed `String.charAt`: the scalar value, or `-1` for
/// `Option.None`. This is the allocation-free lowering for a `charAt` whose
/// `Some` payload is consumed only by character dispatch.
pub fn string_index_code_at(text: &AverStr, index: &StringIndex, position: &AverInt) -> i64 {
    let Some(position) = position.to_usize() else {
        return -1;
    };
    if position >= index.char_len() {
        return -1;
    }
    if let Boundaries::Ascii(_) = index.boundaries {
        return i64::from(text.as_bytes()[position]);
    }
    let from = index.boundary(position);
    let to = index.boundary(position + 1);
    text[from..to]
        .chars()
        .next()
        .map_or(-1, |ch| i64::from(u32::from(ch)))
}

/// The one-character strings of ASCII, in statics: `charAt` on an ASCII
/// string neither slices nor allocates.
static ASCII_CHARS: [StaticStr<1>; 128] = {
    let mut chars = [const { StaticStr::new("\0") }; 128];
    let mut byte = 0;
    while byte < 128 {
        let text = [byte as u8];
        let Ok(text) = std::str::from_utf8(&text) else {
            panic!("an ASCII byte is UTF-8");
        };
        chars[byte] = StaticStr::new(text);
        byte += 1;
    }
    chars
};

/// Indexed equivalent of `String.slice`, including its clamping semantics.
pub fn string_index_slice(
    text: &AverStr,
    index: &StringIndex,
    from: &AverInt,
    to: &AverInt,
) -> AverStr {
    let len = index.char_len();
    let from = clamp_position(from, len);
    let to = clamp_position(to, len);
    if from >= to {
        return AverStr::from("");
    }
    AverStr::from(&text[index.boundary(from)..index.boundary(to)])
}

fn clamp_position(position: &AverInt, len: usize) -> usize {
    match position.to_usize() {
        Some(position) => position.min(len),
        None if position < &AverInt::zero() => 0,
        None => len,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unicode_positions_are_codepoints_and_slice_uses_the_same_boundaries() {
        let text = AverStr::from("aą😀z");
        let index = string_index_build(&text);

        assert_eq!(index.char_len(), 4);
        assert_eq!(
            string_index_char_at(&text, &index, &AverInt::from_i64(1)),
            Some(AverStr::from("ą"))
        );
        assert_eq!(
            string_index_char_at(&text, &index, &AverInt::from_i64(2)),
            Some(AverStr::from("😀"))
        );
        assert_eq!(
            string_index_code_at(&text, &index, &AverInt::from_i64(1)),
            i64::from(u32::from('ą'))
        );
        assert_eq!(
            string_index_code_at(&text, &index, &AverInt::from_i64(2)),
            i64::from(u32::from('😀'))
        );
        assert_eq!(
            string_index_slice(&text, &index, &AverInt::from_i64(1), &AverInt::from_i64(3)),
            AverStr::from("ą😀")
        );
    }

    /// The table of ASCII boundaries gives what the UTF-8 one gives.
    fn utf8_index(text: &str) -> StringIndex {
        let mut boundaries: Vec<u32> = text.char_indices().map(|(byte, _)| byte as u32).collect();
        boundaries.push(text.len() as u32);
        StringIndex {
            boundaries: Boundaries::U32(boundaries.into()),
        }
    }

    #[test]
    fn ascii_fast_path_matches_utf8_boundaries() {
        let ascii: String = (0u8..128).map(char::from).collect();
        for text in [ascii.as_str(), "", "a(b) x\n", "aą😀z"] {
            let text = AverStr::from(text);
            let fast = string_index_build(&text);
            let slow = utf8_index(&text);
            for position in -3..=(text.len() as i64 + 3) {
                let position = AverInt::from_i64(position);
                assert_eq!(
                    string_index_char_at(&text, &fast, &position),
                    string_index_char_at(&text, &slow, &position)
                );
                assert_eq!(
                    string_index_code_at(&text, &fast, &position),
                    string_index_code_at(&text, &slow, &position)
                );
            }
        }
    }

    #[test]
    fn out_of_range_positions_match_surface_clamping() {
        let text = AverStr::from("abc");
        let index = string_index_build(&text);

        assert_eq!(
            string_index_char_at(&text, &index, &AverInt::from_i64(-1)),
            None
        );
        assert_eq!(
            string_index_char_at(&text, &index, &AverInt::from_i64(3)),
            None
        );
        assert_eq!(
            string_index_code_at(&text, &index, &AverInt::from_i64(-1)),
            -1
        );
        assert_eq!(
            string_index_code_at(&text, &index, &AverInt::from_i64(3)),
            -1
        );
        assert_eq!(
            string_index_slice(
                &text,
                &index,
                &AverInt::from_i64(-9),
                &AverInt::from_i64(99)
            ),
            text
        );
    }
}
