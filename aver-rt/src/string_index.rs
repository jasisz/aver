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
/// A string containing `n` Unicode scalar values has `n + 1` boundaries,
/// the last one its byte length.
#[derive(Clone, Debug)]
pub struct StringIndex {
    boundaries: Boundaries,
}

#[derive(Clone, Debug)]
enum Boundaries {
    /// Every scalar value is one byte, so position `i` is byte `i`.
    Ascii(usize),
    Blocks(Arc<Blocks>),
}

/// Scalars per block of [`Blocks`]. Sixty-four of them are at most 256
/// bytes, so a boundary's distance from the start of its block fits a byte.
const BLOCK: usize = 64;

/// The boundaries in two levels: the byte offset of every [`BLOCK`]-th
/// scalar, and every boundary's distance from the start of its block. About
/// one byte per scalar, a quarter of a full table of `u32` offsets, and a
/// read is still two loads.
#[derive(Debug)]
struct Blocks {
    starts: Vec<usize>,
    within: Vec<u8>,
}

impl StringIndex {
    /// Build the boundary table in one UTF-8 pass; an ASCII string needs no
    /// table.
    pub fn new(text: &str) -> Self {
        if text.is_ascii() {
            return Self {
                boundaries: Boundaries::Ascii(text.len()),
            };
        }
        let chars = text.chars().count();
        let mut starts = Vec::with_capacity(chars / BLOCK + 1);
        let mut within = Vec::with_capacity(chars + 1);
        let bytes = text.char_indices().map(|(byte, _)| byte);
        let mut start = 0;
        for (position, byte) in bytes.chain(std::iter::once(text.len())).enumerate() {
            if position % BLOCK == 0 {
                start = byte;
                starts.push(byte);
            }
            // At most `BLOCK - 1` scalars of four bytes since `start`.
            within.push((byte - start) as u8);
        }
        Self {
            boundaries: Boundaries::Blocks(Arc::new(Blocks { starts, within })),
        }
    }

    /// Number of Unicode scalar values in the indexed string.
    pub fn char_len(&self) -> usize {
        match &self.boundaries {
            Boundaries::Ascii(len) => *len,
            Boundaries::Blocks(blocks) => blocks.within.len() - 1,
        }
    }

    /// UTF-8 byte boundary for a clamped codepoint position.
    fn boundary(&self, index: usize) -> usize {
        match &self.boundaries {
            Boundaries::Ascii(_) => index,
            Boundaries::Blocks(blocks) => {
                blocks.starts[index / BLOCK] + usize::from(blocks.within[index])
            }
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
    text[index.boundary(position)..]
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

    /// The two indexes agree with the boundaries `char_indices` gives, at
    /// every position, on ASCII, on text with multibyte scalars, on blocks
    /// of four-byte scalars, and at block edges.
    #[test]
    fn every_index_reads_the_boundaries_of_the_text() {
        let ascii: String = (0u8..128).map(char::from).collect();
        let mixed: String = (0..300).map(|i| ['a', 'ą', '→', '😀'][i % 4]).collect();
        let wide: String = std::iter::repeat_n('😀', BLOCK * 2).collect();
        let edge: String = "a".repeat(BLOCK - 1) + "é" + &"b".repeat(BLOCK);
        for text in [
            ascii.as_str(),
            "",
            "a(b) x\n",
            "aą😀z",
            "é",
            &mixed,
            &wide,
            &edge,
        ] {
            let index = StringIndex::new(text);
            let expected: Vec<usize> = text
                .char_indices()
                .map(|(byte, _)| byte)
                .chain(std::iter::once(text.len()))
                .collect();
            assert_eq!(index.char_len() + 1, expected.len(), "{text:?}");
            for (position, byte) in expected.iter().enumerate() {
                assert_eq!(index.boundary(position), *byte, "{text:?} at {position}");
            }
            let text = AverStr::from(text);
            for position in -3..=(expected.len() as i64 + 3) {
                let at = AverInt::from_i64(position);
                let expected_char = usize::try_from(position)
                    .ok()
                    .and_then(|p| text.chars().nth(p));
                assert_eq!(
                    string_index_char_at(&text, &index, &at),
                    expected_char.map(|c| AverStr::from(c.to_string().as_str()))
                );
                assert_eq!(
                    string_index_code_at(&text, &index, &at),
                    expected_char.map_or(-1, |c| i64::from(u32::from(c)))
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
