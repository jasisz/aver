/// WASM Value layout: uniform tagged i64.
///
/// ```text
/// 63    60 59                              0
/// +--------+-------------------------------+
/// | tag    |           payload              |
/// | 4 bits |           60 bits              |
/// +--------+-------------------------------+
/// ```
///
/// Tag 0: Int (60-bit signed immediate)
/// Tag 1: Const (0=false, 1=true, 2=unit, 3=none, 4=empty_list)
/// Tag 2: HeapRef (handle to object in linear memory)
/// Tag 3+: reserved

// ---------------------------------------------------------------------------
// Value tags
// ---------------------------------------------------------------------------

pub const TAG_INT: u64 = 0;
pub const TAG_CONST: u64 = 1;
pub const TAG_HEAP: u64 = 2;
pub const TAG_SHIFT: u32 = 60;
pub const PAYLOAD_MASK: u64 = (1u64 << 60) - 1;

// ---------------------------------------------------------------------------
// Const payloads
// ---------------------------------------------------------------------------

pub const CONST_FALSE: u64 = (TAG_CONST << TAG_SHIFT) | 0;
pub const CONST_TRUE: u64 = (TAG_CONST << TAG_SHIFT) | 1;
pub const CONST_UNIT: u64 = (TAG_CONST << TAG_SHIFT) | 2;
pub const CONST_NONE: u64 = (TAG_CONST << TAG_SHIFT) | 3;
pub const CONST_EMPTY_LIST: u64 = (TAG_CONST << TAG_SHIFT) | 4;

// ---------------------------------------------------------------------------
// Heap object kinds (shared semantics with VM — see aver-memory object model)
// ---------------------------------------------------------------------------

pub const OBJ_STRING: u64 = 0;
pub const OBJ_RECORD: u64 = 1;
pub const OBJ_VARIANT: u64 = 2;
pub const OBJ_WRAPPER: u64 = 3;
pub const OBJ_LIST_CONS: u64 = 4;
pub const OBJ_TUPLE: u64 = 5;
pub const OBJ_FLOAT: u64 = 6;

// ---------------------------------------------------------------------------
// Wrapper tags (Ok/Err/Some)
// ---------------------------------------------------------------------------

pub const WRAP_OK: u64 = 0;
pub const WRAP_ERR: u64 = 1;
pub const WRAP_SOME: u64 = 2;

// ---------------------------------------------------------------------------
// Heap object header layout
// ---------------------------------------------------------------------------
// [63:56] = object kind (8 bits)
// [55:48] = variant/wrapper tag (8 bits)
// [47:32] = type_id (16 bits)
// [31:0]  = field_count / length (32 bits)

pub const HDR_KIND_SHIFT: u32 = 56;
pub const HDR_TAG_SHIFT: u32 = 48;
pub const HDR_TYPE_SHIFT: u32 = 32;

/// Build a heap object header.
pub fn make_header(kind: u64, variant_tag: u64, type_id: u64, field_count: u64) -> u64 {
    (kind << HDR_KIND_SHIFT)
        | (variant_tag << HDR_TAG_SHIFT)
        | (type_id << HDR_TYPE_SHIFT)
        | field_count
}

// ---------------------------------------------------------------------------
// Value encoding helpers (compile-time, used by codegen)
// ---------------------------------------------------------------------------

/// Encode an integer literal as a tagged Value (compile-time).
pub fn encode_int(n: i64) -> u64 {
    // Tag 0, payload = lower 60 bits of n (sign-extended)
    (TAG_INT << TAG_SHIFT) | ((n as u64) & PAYLOAD_MASK)
}

/// Encode a HeapRef Value from a handle.
pub fn encode_heap_ref(handle: u64) -> u64 {
    (TAG_HEAP << TAG_SHIFT) | (handle & PAYLOAD_MASK)
}
