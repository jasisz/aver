// WASM Value layout: typed ABI.
//
// Native scalar types map directly to WASM types:
//   Int   → i64 (plain value)
//   Float → f64 (plain value)
//   Bool  → i32 (0 = false, 1 = true)
//   Unit  → i32 (0)
//
// Heap-allocated types are i32 pointers:
//   String → i32 (pointer to string object)
//   Result/Option → i32 (pointer to wrapper object)
//   List   → i32 (pointer to cons cell, 0 = empty)
//   Record → i32 (pointer to record object)
//   Tuple  → i32 (pointer to tuple object)
//
// Heap objects have an 8-byte i64 header followed by 8-byte fields.

// ---------------------------------------------------------------------------
// Heap object kinds
// ---------------------------------------------------------------------------

pub const OBJ_STRING: u64 = 0;
pub const OBJ_RECORD: u64 = 1;
#[allow(dead_code)]
pub const OBJ_VARIANT: u64 = 2;
pub const OBJ_WRAPPER: u64 = 3; // inner field is i64
pub const OBJ_LIST_CONS: u64 = 4; // head is i64, tail is i32 (stored as i64)
pub const OBJ_TUPLE: u64 = 5;
// 6 reserved
pub const OBJ_WRAPPER_F64: u64 = 7; // inner field is f64
pub const OBJ_WRAPPER_I32: u64 = 8; // inner field is i32 (stored as i64)
pub const OBJ_LIST_CONS_F64: u64 = 9; // head is f64, tail is i32 (stored as i64)
pub const OBJ_VECTOR: u64 = 10; // flat array of i64 elements
pub const OBJ_MAP_ENTRY: u64 = 11; // cons cell in association list (same layout as LIST_CONS)

// ---------------------------------------------------------------------------
// Wrapper tags (Ok/Err/Some)
// ---------------------------------------------------------------------------

pub const WRAP_OK: u64 = 0;
pub const WRAP_ERR: u64 = 1;
pub const WRAP_SOME: u64 = 2;

// ---------------------------------------------------------------------------
// Sentinels (i32 values for non-heap constants)
// ---------------------------------------------------------------------------

/// Empty list sentinel (i32 pointer = 0, null).
pub const EMPTY_LIST: i32 = 0;

/// Option.None sentinel (i32 = -1, distinguishable from null pointer).
pub const NONE_SENTINEL: i32 = -1;

/// First dynamic sentinel for the symbol table.
/// Nullary variants, wrapper(bool/unit) combos start here and go more negative.
pub const SYMBOL_SENTINEL_BASE: i32 = -2;

// Fixed wrapper+literal sentinels (assigned before user variants).
pub const SENTINEL_OK_TRUE: i32 = -2;
pub const SENTINEL_OK_FALSE: i32 = -3;
pub const SENTINEL_OK_UNIT: i32 = -4;
pub const SENTINEL_ERR_TRUE: i32 = -5;
pub const SENTINEL_ERR_FALSE: i32 = -6;
pub const SENTINEL_ERR_UNIT: i32 = -7;
pub const SENTINEL_SOME_TRUE: i32 = -8;
pub const SENTINEL_SOME_FALSE: i32 = -9;
pub const SENTINEL_SOME_UNIT: i32 = -10;

/// First sentinel ID available for user-defined nullary variants.
pub const USER_SENTINEL_BASE: i32 = -11;

// ---------------------------------------------------------------------------
// Heap object header layout
// ---------------------------------------------------------------------------
// [63:56] = object kind (8 bits)
// [55:48] = variant/wrapper tag (8 bits)
// [47:32] = metadata / pointer mask (16 bits)
// [31:0]  = field_count / length (32 bits)

pub const HDR_KIND_SHIFT: u32 = 56;
pub const HDR_TAG_SHIFT: u32 = 48;
#[allow(dead_code)]
pub const HDR_TYPE_SHIFT: u32 = 32;
pub const HDR_META_SHIFT: u32 = HDR_TYPE_SHIFT;
pub const HDR_META_MASK: u64 = 0xFFFF;
pub const OBJ_FORWARD: u64 = 0xFF;

/// Build a heap object header.
///
/// The third field is now runtime metadata:
/// - fixed-size objects use it as a pointer-field bitmask
/// - wrappers/vectors/lists use low bits as per-kind flags
pub fn make_header(kind: u64, variant_tag: u64, type_id: u64, field_count: u64) -> u64 {
    (kind << HDR_KIND_SHIFT)
        | (variant_tag << HDR_TAG_SHIFT)
        | (type_id << HDR_TYPE_SHIFT)
        | field_count
}
