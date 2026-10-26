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
    chars: usize,
    /// None when every scalar value is one byte, so position `i` is byte
    /// `i`.
    blocks: Option<Arc<Blocks>>,
}

/// Scalars per block of [`Blocks`]. Sixty-four of them are at most 256
/// bytes, so a boundary's distance from the start of its block fits a byte.
const BLOCK: usize = 64;

/// The boundaries in two levels: the byte offset of every [`BLOCK`]-th
/// scalar, and every boundary's distance from the start of its block. About
/// one byte per scalar, a quarter of a full table of `u32` offsets, and a
/// read is still two loads. A block of ASCII scalars reads in one: there
/// the distance is the position within the block.
#[derive(Debug)]
struct Blocks {
    /// The start of each block shifted left by one, its low bit set when
    /// every scalar of the block is one byte.
    starts: Vec<usize>,
    within: Vec<u8>,
}

impl StringIndex {
    /// Build the boundary table; an ASCII string needs no table, and a
    /// block of ASCII scalars is checked a word at a time.
    pub fn new(text: &str) -> Self {
        if text.is_ascii() {
            return Self {
                chars: text.len(),
                blocks: None,
            };
        }
        let chars = text.chars().count();
        let mut starts = Vec::with_capacity(chars / BLOCK + 1);
        let mut within = Vec::with_capacity(chars + 1);
        let mut start = 0;
        for block in 0..=chars / BLOCK {
            let scalars = (chars - block * BLOCK).min(BLOCK);
            let ascii = text.as_bytes()[start..start + scalars].is_ascii();
            starts.push(start << 1 | usize::from(ascii));
            let span = match ascii {
                true => {
                    within.extend_from_slice(&IOTA[..scalars]);
                    scalars
                }
                // At most `BLOCK - 1` scalars of four bytes since `start`.
                false => text[start..]
                    .char_indices()
                    .take(scalars)
                    .fold(0, |_, (offset, c)| {
                        within.push(offset as u8);
                        offset + c.len_utf8()
                    }),
            };
            start += span;
        }
        // The end of the text, in the last block.
        within.push((text.len() - (starts[chars / BLOCK] >> 1)) as u8);
        Self {
            chars,
            blocks: Some(Arc::new(Blocks { starts, within })),
        }
    }

    /// Number of Unicode scalar values in the indexed string.
    pub fn char_len(&self) -> usize {
        self.chars
    }

    /// UTF-8 byte boundary for a clamped codepoint position.
    fn boundary(&self, index: usize) -> usize {
        let Some(blocks) = &self.blocks else {
            return index;
        };
        let start = blocks.starts[index / BLOCK];
        (start >> 1)
            + match start & 1 {
                1 => index % BLOCK,
                _ => usize::from(blocks.within[index]),
            }
    }
}

/// Build the hidden index threaded through a synthesized call cone.
#[inline]
pub fn string_index_build(text: &AverStr) -> StringIndex {
    StringIndex::new(text)
}

/// A string position as the raw `i64` the indexed reads take. A position
/// outside `i64` reads like one past the end (or before the start), so
/// saturating it keeps every answer of `charAt` / `slice`.
#[inline]
pub fn string_position(position: &AverInt) -> i64 {
    position.to_i64().unwrap_or(if position < &AverInt::zero() {
        i64::MIN
    } else {
        i64::MAX
    })
}

/// The start of a string walk the compiler checks once on entry: the
/// position as an `i64` when it lies in `[lo, hi]`, the range a successful
/// read can be at, else `None` (the walk's first read fails there).
#[inline]
pub fn string_position_within(position: &AverInt, lo: i64, hi: i64) -> Option<i64> {
    position.to_i64().filter(|p| (lo..=hi).contains(p))
}

/// Indexed equivalent of `String.charAt`.
pub fn string_index_char_at(
    text: &AverStr,
    index: &StringIndex,
    position: &AverInt,
) -> Option<AverStr> {
    string_index_char_at_i64(text, index, string_position(position))
}

/// [`string_index_char_at`] over a raw position (a loop position the
/// compiler proved to fit `i64`).
pub fn string_index_char_at_i64(
    text: &AverStr,
    index: &StringIndex,
    position: i64,
) -> Option<AverStr> {
    let position = usize::try_from(position).ok()?;
    if index.blocks.is_none() {
        // An ASCII text has a byte per scalar, so the read's own bounds
        // check is the check against the length.
        let byte = *text.as_bytes().get(position)?;
        return Some(AverStr::from_static(&ASCII_CHARS[usize::from(byte)]));
    }
    if position >= index.char_len() {
        return None;
    }
    let from = index.boundary(position);
    let lead = text.as_bytes()[from];
    if lead.is_ascii() {
        return Some(AverStr::from_static(&ASCII_CHARS[usize::from(lead)]));
    }
    let to = index.boundary(position + 1);
    Some(AverStr::from(&text[from..to]))
}

/// Codepoint-only indexed `String.charAt`: the scalar value, or `-1` for
/// `Option.None`. This is the allocation-free lowering for a `charAt` whose
/// `Some` payload is consumed only by character dispatch.
pub fn string_index_code_at(text: &AverStr, index: &StringIndex, position: &AverInt) -> i64 {
    string_index_code_at_i64(text, index, string_position(position))
}

/// [`string_index_code_at`] over a raw position.
pub fn string_index_code_at_i64(text: &AverStr, index: &StringIndex, position: i64) -> i64 {
    let Ok(position) = usize::try_from(position) else {
        return -1;
    };
    if index.blocks.is_none() {
        // As in `string_index_char_at_i64`.
        return text.as_bytes().get(position).map_or(-1, |&byte| i64::from(byte));
    }
    if position >= index.char_len() {
        return -1;
    }
    let byte = index.boundary(position);
    let lead = text.as_bytes()[byte];
    if lead.is_ascii() {
        return i64::from(lead);
    }
    text[byte..]
        .chars()
        .next()
        .map_or(-1, |ch| i64::from(u32::from(ch)))
}

/// The distances of the boundaries of a block of ASCII scalars.
static IOTA: [u8; BLOCK] = {
    let mut iota = [0; BLOCK];
    let mut i = 0;
    while i < BLOCK {
        iota[i] = i as u8;
        i += 1;
    }
    iota
};

/// The one-character strings of ASCII, in statics: `charAt` on an ASCII
/// string neither slices nor allocates.
pub(crate) static ASCII_CHARS: [StaticStr<1>; 128] = {
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
    string_index_slice_i64(text, index, string_position(from), string_position(to))
}

/// [`string_index_slice`] over raw positions.
pub fn string_index_slice_i64(text: &AverStr, index: &StringIndex, from: i64, to: i64) -> AverStr {
    crate::string_part(text, string_index_slice_str(text, index, from, to))
}

/// The text [`string_index_slice_i64`] cuts out, read in place: a slice that
/// is only compared builds no string.
pub fn string_index_slice_str<'a>(
    text: &'a str,
    index: &StringIndex,
    from: i64,
    to: i64,
) -> &'a str {
    let len = index.char_len();
    let from = clamp_position(from, len);
    let to = clamp_position(to, len);
    if from >= to {
        return "";
    }
    &text[index.boundary(from)..index.boundary(to)]
}

fn clamp_position(position: i64, len: usize) -> usize {
    usize::try_from(position).map_or(0, |position| position.min(len))
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
        // Sparse: ASCII blocks around one multibyte scalar at each edge of a
        // block, and a last block that is not full.
        let sparse: String = "a".repeat(BLOCK)
            + "😀"
            + &"b".repeat(2 * BLOCK - 2)
            + "é"
            + &"c".repeat(BLOCK + 5)
            + "→";
        let block_end: String = "x".repeat(2 * BLOCK);
        for text in [
            ascii.as_str(),
            "",
            "a(b) x\n",
            "aą😀z",
            "é",
            &mixed,
            &wide,
            &edge,
            &sparse,
            &sparse[..sparse.len() - "→".len()],
            &block_end,
            &(block_end.clone() + "é"),
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
                for to in [
                    position - 1,
                    position,
                    position + 2,
                    position + 65,
                    i64::MAX,
                ] {
                    assert_eq!(
                        string_index_slice_str(&text, &index, position, to),
                        crate::string_slice(&text, position, to),
                        "{text:?} at {position}..{to}"
                    );
                }
            }
        }
    }

    #[test]
    fn a_block_of_ascii_scalars_is_marked() {
        let text = "a".repeat(BLOCK)
            + "😀"
            + &"b".repeat(2 * BLOCK - 2)
            + "é"
            + &"c".repeat(BLOCK + 5)
            + "→";
        let blocks = StringIndex::new(&text).blocks.expect("not ASCII");
        let marks: Vec<usize> = blocks.starts.iter().map(|start| start & 1).collect();
        assert_eq!(marks, [1, 0, 0, 1, 0]);
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

    #[test]
    fn a_start_is_let_into_the_loop_only_within_its_range() {
        let top = isize::MAX as i64 - 9;
        let within = |p: &AverInt| string_position_within(p, 0, top);
        assert_eq!(within(&AverInt::from_i64(0)), Some(0));
        assert_eq!(within(&AverInt::from_i64(top)), Some(top));
        assert_eq!(within(&AverInt::from_i64(top + 1)), None);
        assert_eq!(within(&AverInt::from_i64(-1)), None);
        assert_eq!(within(&AverInt::from_i64(i64::MIN)), None);
        let past = AverInt::from_i64(i64::MAX).add(&AverInt::from_i64(1));
        assert_eq!(within(&past), None);
    }
}
