/// Convert an LSP UTF-16 character offset to a byte index in a UTF-8 Rust string.
///
/// The returned index is always a valid UTF-8 boundary and is clamped to the line end.
pub fn utf16_col_to_byte_idx(line: &str, utf16_col: u32) -> usize {
    let target = utf16_col as usize;
    let mut utf16_seen = 0usize;

    for (byte_idx, ch) in line.char_indices() {
        if utf16_seen >= target {
            return byte_idx;
        }

        let next = utf16_seen + ch.len_utf16();
        if next > target {
            // Cursor points inside a multi-unit scalar; snap to char start.
            return byte_idx;
        }
        utf16_seen = next;
    }

    line.len()
}

#[cfg(test)]
mod tests {
    use super::utf16_col_to_byte_idx;

    #[test]
    fn utf16_ascii_offsets_match_bytes() {
        let s = "abc";
        assert_eq!(utf16_col_to_byte_idx(s, 0), 0);
        assert_eq!(utf16_col_to_byte_idx(s, 1), 1);
        assert_eq!(utf16_col_to_byte_idx(s, 2), 2);
        assert_eq!(utf16_col_to_byte_idx(s, 3), 3);
        assert_eq!(utf16_col_to_byte_idx(s, 99), 3);
    }

    #[test]
    fn utf16_handles_surrogate_pairs() {
        let s = "a😀b";
        assert_eq!(utf16_col_to_byte_idx(s, 0), 0);
        assert_eq!(utf16_col_to_byte_idx(s, 1), 1);
        assert_eq!(utf16_col_to_byte_idx(s, 2), 1);
        assert_eq!(utf16_col_to_byte_idx(s, 3), 5);
        assert_eq!(utf16_col_to_byte_idx(s, 4), 6);
    }
}
