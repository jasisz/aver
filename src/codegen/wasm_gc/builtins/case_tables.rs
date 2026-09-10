//! Pack the shared Unicode case tables into a wasm passive data segment.

use crate::codegen::unicode_case::{
    CASE_IGNORABLE, CASED, Expansion, LOWER_EXPAND, LOWER_SIMPLE, Range, SimpleRun, UPPER_EXPAND,
    UPPER_SIMPLE,
};
pub(in crate::codegen::wasm_gc) use crate::codegen::unicode_case::{
    MAX_LOWER_GROWTH, MAX_UPPER_GROWTH,
};

/// Bytes per record in the emitted blob. Every field is a
/// little-endian `u24`, so one read routine in WAT serves all four
/// tables and every record length is a multiple of 3.
const SIMPLE_RECORD: u32 = 9;
const EXPAND_RECORD: u32 = 12;
const RANGE_RECORD: u32 = 6;

/// Where one table sits inside the blob: byte offset of its first
/// record and how many records it has. Both are compile-time constants
/// of the emitted module, substituted straight into the helper WAT.
#[derive(Clone, Copy, Debug)]
pub(in crate::codegen::wasm_gc) struct TableSpan {
    pub off: u32,
    pub count: u32,
}

/// The one passive data segment a module that calls `String.toLower` /
/// `String.toUpper` carries, and where each table sits inside it. Only
/// the tables the module actually needs are packed: an upper-only
/// module pays nothing for the final-sigma context tables.
pub(in crate::codegen::wasm_gc) struct CaseBlob {
    pub bytes: Vec<u8>,
    pub lower_simple: Option<TableSpan>,
    pub lower_expand: Option<TableSpan>,
    pub upper_simple: Option<TableSpan>,
    pub upper_expand: Option<TableSpan>,
    pub cased: Option<TableSpan>,
    pub ignorable: Option<TableSpan>,
}

/// Everything the case helper bodies need to reach the blob: the blob
/// itself, the passive data segment index it was emitted at, and the
/// wasm global that caches the decoded array.
pub(in crate::codegen::wasm_gc) struct CaseWiring<'a> {
    pub blob: &'a CaseBlob,
    pub data_segment_idx: u32,
    pub global_idx: u32,
}

fn push_u24(bytes: &mut Vec<u8>, value: u32) {
    let v = value & 0x00FF_FFFF;
    bytes.push((v & 0xFF) as u8);
    bytes.push(((v >> 8) & 0xFF) as u8);
    bytes.push(((v >> 16) & 0xFF) as u8);
}

fn push_simple(bytes: &mut Vec<u8>, runs: &[SimpleRun]) -> TableSpan {
    let off = bytes.len() as u32;
    for (first, last, delta) in runs {
        push_u24(bytes, *first);
        push_u24(bytes, *last);
        push_u24(bytes, *delta as u32);
    }
    debug_assert_eq!(bytes.len() as u32 - off, runs.len() as u32 * SIMPLE_RECORD);
    TableSpan {
        off,
        count: runs.len() as u32,
    }
}

fn push_expand(bytes: &mut Vec<u8>, entries: &[Expansion]) -> TableSpan {
    let off = bytes.len() as u32;
    for (from, to) in entries {
        push_u24(bytes, *from);
        push_u24(bytes, to[0]);
        push_u24(bytes, to[1]);
        push_u24(bytes, to[2]);
    }
    debug_assert_eq!(
        bytes.len() as u32 - off,
        entries.len() as u32 * EXPAND_RECORD
    );
    TableSpan {
        off,
        count: entries.len() as u32,
    }
}

fn push_ranges(bytes: &mut Vec<u8>, ranges: &[Range]) -> TableSpan {
    let off = bytes.len() as u32;
    for (first, last) in ranges {
        push_u24(bytes, *first);
        push_u24(bytes, *last);
    }
    debug_assert_eq!(bytes.len() as u32 - off, ranges.len() as u32 * RANGE_RECORD);
    TableSpan {
        off,
        count: ranges.len() as u32,
    }
}

/// Pack the tables the module needs into one blob.
pub(in crate::codegen::wasm_gc) fn build_blob(need_lower: bool, need_upper: bool) -> CaseBlob {
    let mut bytes = Vec::new();
    let (mut lower_simple, mut lower_expand) = (None, None);
    let (mut upper_simple, mut upper_expand) = (None, None);
    let (mut cased, mut ignorable) = (None, None);
    if need_lower {
        lower_simple = Some(push_simple(&mut bytes, LOWER_SIMPLE));
        lower_expand = Some(push_expand(&mut bytes, LOWER_EXPAND));
    }
    if need_upper {
        upper_simple = Some(push_simple(&mut bytes, UPPER_SIMPLE));
        upper_expand = Some(push_expand(&mut bytes, UPPER_EXPAND));
    }
    if need_lower {
        cased = Some(push_ranges(&mut bytes, CASED));
        ignorable = Some(push_ranges(&mut bytes, CASE_IGNORABLE));
    }
    CaseBlob {
        bytes,
        lower_simple,
        lower_expand,
        upper_simple,
        upper_expand,
        cased,
        ignorable,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    /// The blob layout the helper WAT is generated against.
    #[test]
    fn blob_spans_cover_the_bytes() {
        let both = build_blob(true, true);
        assert_eq!(
            both.bytes.len() as u32,
            LOWER_SIMPLE.len() as u32 * SIMPLE_RECORD
                + LOWER_EXPAND.len() as u32 * EXPAND_RECORD
                + UPPER_SIMPLE.len() as u32 * SIMPLE_RECORD
                + UPPER_EXPAND.len() as u32 * EXPAND_RECORD
                + CASED.len() as u32 * RANGE_RECORD
                + CASE_IGNORABLE.len() as u32 * RANGE_RECORD
        );
        let upper_only = build_blob(false, true);
        assert!(upper_only.cased.is_none());
        assert!(upper_only.lower_simple.is_none());
        assert_eq!(upper_only.upper_simple.map(|s| s.off), Some(0));
        let lower_only = build_blob(true, false);
        assert!(lower_only.upper_expand.is_none());
        assert!(lower_only.ignorable.is_some());
    }
}
