//! Loop-scoped codepoint index for repeated indexed string access.
//!
//! This is compiler plumbing, not an Aver surface type. The string-index
//! pass builds one value at the boundary of an indexed-access call cone and
//! threads it through synthesized workers. Public positions stay Unicode
//! scalar indices; the index only translates them to UTF-8 byte boundaries.

use std::sync::Arc;

use crate::{AverInt, AverStr};

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
    U32(Arc<[u32]>),
    Usize(Arc<[usize]>),
}

impl StringIndex {
    /// Build the codepoint-to-byte table in one UTF-8 pass.
    pub fn new(text: &str) -> Self {
        if u32::try_from(text.len()).is_ok() {
            let mut boundaries = Vec::new();
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
            let mut boundaries = Vec::new();
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
            Boundaries::U32(items) => items.len().saturating_sub(1),
            Boundaries::Usize(items) => items.len().saturating_sub(1),
        }
    }

    /// UTF-8 byte boundary for a clamped codepoint position.
    fn boundary(&self, index: usize) -> usize {
        match &self.boundaries {
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
    let from = index.boundary(position);
    let to = index.boundary(position + 1);
    Some(AverStr::from(&text[from..to]))
}

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
            string_index_slice(&text, &index, &AverInt::from_i64(1), &AverInt::from_i64(3)),
            AverStr::from("ą😀")
        );
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
