//! NaN-boxed compact Value representation (8 bytes per value).
//!
//! Layout: every value is a `u64` interpreted as an IEEE 754 `f64`.
//!
//! - **Float**: any f64 that is NOT a quiet NaN with our marker -> stored directly.
//! - **Everything else**: encoded as a quiet NaN with tag + payload in the mantissa.
//!
//! IEEE 754 quiet NaN: exponent=0x7FF (all 1s), quiet bit=1, plus our marker bit.
//! We use `0x7FFC` as 14-bit prefix (bits 63-50), leaving bits 49-0 free.
//!
//! ```text
//! 63      50 49  46 45                    0
//! ┌────────┬──────┬────────────────────────┐
//! │ 0x7FFC │ tag  │       payload          │
//! │ 14 bits│ 4 bit│       46 bits          │
//! └────────┴──────┴────────────────────────┘
//! ```
//!
//! Tag map:
//!   0  = Int        payload bit45: 0=inline(45-bit signed), 1=arena index
//!   1  = Immediate  payload bits 0-1: 00=false, 01=true, 10=unit, 11=none
//!   2  = Wrapper    payload bits 0-1: 00=some, 01=ok, 10=err; rest=arena index
//!   3  = String     payload = arena index
//!   4  = List       payload = arena index
//!   5  = Tuple      payload = arena index
//!   6  = Map        payload = arena index
//!   7  = Record     payload = arena index
//!   8  = Variant    payload = arena index
//!   9  = Fn         payload = arena index
//!   10 = Builtin    payload = arena index
//!   11 = Namespace  payload = arena index
//!   12-15 = (reserved)

use std::rc::Rc;

use crate::value::FunctionValue;

/// Persistent immutable map -> O(1) clone via structural sharing.
pub type PersistentMap = im::HashMap<u64, (NanValue, NanValue)>;

// ---------------------------------------------------------------------------
// Bit layout constants
// ---------------------------------------------------------------------------

const QNAN: u64 = 0x7FFC_0000_0000_0000;
const QNAN_MASK: u64 = 0xFFFC_0000_0000_0000;
const TAG_SHIFT: u32 = 46;
const TAG_MASK: u64 = 0xF;
const PAYLOAD_MASK: u64 = (1u64 << 46) - 1;

const TAG_INT: u64 = 0;
const TAG_IMMEDIATE: u64 = 1;
const TAG_WRAPPER: u64 = 2;
const TAG_STRING: u64 = 3;
const TAG_LIST: u64 = 4;
const TAG_TUPLE: u64 = 5;
const TAG_MAP: u64 = 6;
const TAG_RECORD: u64 = 7;
const TAG_VARIANT: u64 = 8;
const TAG_FN: u64 = 9;
const TAG_BUILTIN: u64 = 10;
const TAG_NAMESPACE: u64 = 11;

const IMM_FALSE: u64 = 0;
const IMM_TRUE: u64 = 1;
const IMM_UNIT: u64 = 2;
const IMM_NONE: u64 = 3;

const WRAP_SOME: u64 = 0;
const WRAP_OK: u64 = 1;
const WRAP_ERR: u64 = 2;

const INT_BIG_BIT: u64 = 1u64 << 45;
const INT_INLINE_MASK: u64 = (1u64 << 45) - 1;
const INT_INLINE_MAX: i64 = (1i64 << 44) - 1;
const INT_INLINE_MIN: i64 = -(1i64 << 44);

// ---------------------------------------------------------------------------
// NanValue - the 8-byte compact value
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
pub struct NanValue(u64);

// -- Encoding / decoding ---------------------------------------------------

impl NanValue {
    #[inline]
    fn encode(tag: u64, payload: u64) -> Self {
        debug_assert!(tag <= TAG_MASK);
        debug_assert!(payload <= PAYLOAD_MASK);
        NanValue(QNAN | (tag << TAG_SHIFT) | payload)
    }

    #[inline]
    fn is_nan_boxed(self) -> bool {
        (self.0 & QNAN_MASK) == QNAN
    }

    #[inline]
    fn tag(self) -> u64 {
        (self.0 >> TAG_SHIFT) & TAG_MASK
    }

    #[inline]
    fn payload(self) -> u64 {
        self.0 & PAYLOAD_MASK
    }

    // -- Constructors ------------------------------------------------------

    #[inline]
    pub fn new_float(f: f64) -> Self {
        let bits = f.to_bits();
        if (bits & QNAN_MASK) == QNAN {
            NanValue(bits ^ 1)
        } else {
            NanValue(bits)
        }
    }

    #[inline]
    pub fn as_float(self) -> f64 {
        f64::from_bits(self.0)
    }

    #[inline]
    pub fn new_int_inline(i: i64) -> Self {
        debug_assert!((INT_INLINE_MIN..=INT_INLINE_MAX).contains(&i));
        let payload = (i as u64) & INT_INLINE_MASK;
        Self::encode(TAG_INT, payload)
    }

    #[inline]
    pub fn new_int_arena(arena_index: u32) -> Self {
        Self::encode(TAG_INT, INT_BIG_BIT | (arena_index as u64))
    }

    #[inline]
    pub fn new_int(i: i64, arena: &mut Arena) -> Self {
        if (INT_INLINE_MIN..=INT_INLINE_MAX).contains(&i) {
            Self::new_int_inline(i)
        } else {
            let idx = arena.push_i64(i);
            Self::new_int_arena(idx)
        }
    }

    #[inline]
    pub fn as_int(self, arena: &Arena) -> i64 {
        let p = self.payload();
        if p & INT_BIG_BIT != 0 {
            let idx = (p & !INT_BIG_BIT) as u32;
            arena.get_i64(idx)
        } else {
            let raw = p & INT_INLINE_MASK;
            if raw & (1u64 << 44) != 0 {
                (raw | !INT_INLINE_MASK) as i64
            } else {
                raw as i64
            }
        }
    }

    // -- Immediates --------------------------------------------------------

    pub const FALSE: NanValue = NanValue(QNAN | (TAG_IMMEDIATE << TAG_SHIFT) | IMM_FALSE);
    pub const TRUE: NanValue = NanValue(QNAN | (TAG_IMMEDIATE << TAG_SHIFT) | IMM_TRUE);
    pub const UNIT: NanValue = NanValue(QNAN | (TAG_IMMEDIATE << TAG_SHIFT) | IMM_UNIT);
    pub const NONE: NanValue = NanValue(QNAN | (TAG_IMMEDIATE << TAG_SHIFT) | IMM_NONE);

    #[inline]
    pub fn new_bool(b: bool) -> Self {
        if b { Self::TRUE } else { Self::FALSE }
    }

    #[inline]
    pub fn as_bool(self) -> bool {
        self.0 == Self::TRUE.0
    }

    // -- Wrappers (Some/Ok/Err) -------------------------------------------

    #[inline]
    pub fn new_some(inner_index: u32) -> Self {
        Self::encode(TAG_WRAPPER, WRAP_SOME | ((inner_index as u64) << 2))
    }

    #[inline]
    pub fn new_ok(inner_index: u32) -> Self {
        Self::encode(TAG_WRAPPER, WRAP_OK | ((inner_index as u64) << 2))
    }

    #[inline]
    pub fn new_err(inner_index: u32) -> Self {
        Self::encode(TAG_WRAPPER, WRAP_ERR | ((inner_index as u64) << 2))
    }

    #[inline]
    pub fn wrapper_kind(self) -> u64 {
        self.payload() & 3
    }

    #[inline]
    pub fn wrapper_index(self) -> u32 {
        (self.payload() >> 2) as u32
    }

    // -- Arena-backed constructors -----------------------------------------

    #[inline]
    pub fn new_string(arena_index: u32) -> Self {
        Self::encode(TAG_STRING, arena_index as u64)
    }

    #[inline]
    pub fn new_list(arena_index: u32) -> Self {
        Self::encode(TAG_LIST, arena_index as u64)
    }

    #[inline]
    pub fn new_tuple(arena_index: u32) -> Self {
        Self::encode(TAG_TUPLE, arena_index as u64)
    }

    #[inline]
    pub fn new_map(arena_index: u32) -> Self {
        Self::encode(TAG_MAP, arena_index as u64)
    }

    #[inline]
    pub fn new_record(arena_index: u32) -> Self {
        Self::encode(TAG_RECORD, arena_index as u64)
    }

    #[inline]
    pub fn new_variant(arena_index: u32) -> Self {
        Self::encode(TAG_VARIANT, arena_index as u64)
    }

    #[inline]
    pub fn new_fn(arena_index: u32) -> Self {
        Self::encode(TAG_FN, arena_index as u64)
    }

    #[inline]
    pub fn new_builtin(arena_index: u32) -> Self {
        Self::encode(TAG_BUILTIN, arena_index as u64)
    }

    #[inline]
    pub fn new_namespace(arena_index: u32) -> Self {
        Self::encode(TAG_NAMESPACE, arena_index as u64)
    }

    #[inline]
    pub fn arena_index(self) -> u32 {
        self.payload() as u32
    }

    #[inline]
    pub fn heap_index(self) -> Option<u32> {
        if !self.is_nan_boxed() {
            return None;
        }
        match self.tag() {
            TAG_INT => {
                let p = self.payload();
                if p & INT_BIG_BIT != 0 {
                    Some((p & !INT_BIG_BIT) as u32)
                } else {
                    None
                }
            }
            TAG_WRAPPER => Some(self.wrapper_index()),
            TAG_STRING | TAG_LIST | TAG_TUPLE | TAG_MAP | TAG_RECORD | TAG_VARIANT | TAG_FN
            | TAG_BUILTIN | TAG_NAMESPACE => Some(self.arena_index()),
            _ => None,
        }
    }

    #[inline]
    pub fn with_heap_index(self, index: u32) -> Self {
        if !self.is_nan_boxed() {
            return self;
        }
        match self.tag() {
            TAG_INT => {
                debug_assert!(self.payload() & INT_BIG_BIT != 0);
                Self::new_int_arena(index)
            }
            TAG_WRAPPER => match self.wrapper_kind() {
                WRAP_SOME => Self::new_some(index),
                WRAP_OK => Self::new_ok(index),
                WRAP_ERR => Self::new_err(index),
                _ => self,
            },
            TAG_STRING => Self::new_string(index),
            TAG_LIST => Self::new_list(index),
            TAG_TUPLE => Self::new_tuple(index),
            TAG_MAP => Self::new_map(index),
            TAG_RECORD => Self::new_record(index),
            TAG_VARIANT => Self::new_variant(index),
            TAG_FN => Self::new_fn(index),
            TAG_BUILTIN => Self::new_builtin(index),
            TAG_NAMESPACE => Self::new_namespace(index),
            _ => self,
        }
    }

    // -- Type checks -------------------------------------------------------

    #[inline]
    pub fn is_float(self) -> bool {
        !self.is_nan_boxed()
    }

    #[inline]
    pub fn is_int(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_INT
    }

    #[inline]
    pub fn is_bool(self) -> bool {
        self.is_nan_boxed()
            && self.tag() == TAG_IMMEDIATE
            && (self.payload() == IMM_TRUE || self.payload() == IMM_FALSE)
    }

    #[inline]
    pub fn is_unit(self) -> bool {
        self.0 == Self::UNIT.0
    }

    #[inline]
    pub fn is_none(self) -> bool {
        self.0 == Self::NONE.0
    }

    #[inline]
    pub fn is_some(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_WRAPPER && self.wrapper_kind() == WRAP_SOME
    }

    #[inline]
    pub fn is_ok(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_WRAPPER && self.wrapper_kind() == WRAP_OK
    }

    #[inline]
    pub fn is_err(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_WRAPPER && self.wrapper_kind() == WRAP_ERR
    }

    #[inline]
    pub fn is_string(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_STRING
    }

    #[inline]
    pub fn is_list(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_LIST
    }

    #[inline]
    pub fn is_record(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_RECORD
    }

    #[inline]
    pub fn is_fn(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_FN
    }

    #[inline]
    pub fn is_variant(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_VARIANT
    }

    #[inline]
    pub fn is_map(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_MAP
    }

    #[inline]
    pub fn is_tuple(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_TUPLE
    }

    #[inline]
    pub fn is_builtin(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_BUILTIN
    }

    #[inline]
    pub fn is_namespace(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_NAMESPACE
    }

    pub fn type_name(self) -> &'static str {
        if self.is_float() {
            return "Float";
        }
        match self.tag() {
            TAG_INT => "Int",
            TAG_IMMEDIATE => match self.payload() {
                IMM_FALSE | IMM_TRUE => "Bool",
                IMM_UNIT => "Unit",
                IMM_NONE => "Option.None",
                _ => "Unknown",
            },
            TAG_WRAPPER => match self.wrapper_kind() {
                WRAP_SOME => "Option.Some",
                WRAP_OK => "Result.Ok",
                WRAP_ERR => "Result.Err",
                _ => "Unknown",
            },
            TAG_STRING => "String",
            TAG_LIST => "List",
            TAG_TUPLE => "Tuple",
            TAG_MAP => "Map",
            TAG_RECORD => "Record",
            TAG_VARIANT => "Variant",
            TAG_FN => "Fn",
            TAG_BUILTIN => "Builtin",
            TAG_NAMESPACE => "Namespace",
            _ => "Unknown",
        }
    }

    /// Raw bits - useful for using as HashMap key (inline values only).
    #[inline]
    pub fn bits(self) -> u64 {
        self.0
    }

    /// Content-based hash for use as map key. For inline values (int, float, bool),
    /// uses bits(). For arena-backed strings, hashes the string content so that
    /// two NanValues for the same string content produce the same key regardless
    /// of arena index.
    pub fn map_key_hash(self, arena: &Arena) -> u64 {
        if self.is_string() {
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            3u8.hash(&mut hasher);
            arena.get_string(self.arena_index()).hash(&mut hasher);
            hasher.finish()
        } else {
            self.bits()
        }
    }
}

// -- Debug -----------------------------------------------------------------

impl std::fmt::Debug for NanValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_float() {
            return write!(f, "Float({})", self.as_float());
        }
        match self.tag() {
            TAG_INT => {
                if self.payload() & INT_BIG_BIT != 0 {
                    write!(f, "Int(arena:{})", (self.payload() & !INT_BIG_BIT) as u32)
                } else {
                    let raw = self.payload() & INT_INLINE_MASK;
                    let val = if raw & (1u64 << 44) != 0 {
                        (raw | !INT_INLINE_MASK) as i64
                    } else {
                        raw as i64
                    };
                    write!(f, "Int({})", val)
                }
            }
            TAG_IMMEDIATE => match self.payload() {
                IMM_FALSE => write!(f, "False"),
                IMM_TRUE => write!(f, "True"),
                IMM_UNIT => write!(f, "Unit"),
                IMM_NONE => write!(f, "None"),
                _ => write!(f, "Immediate({})", self.payload()),
            },
            TAG_WRAPPER => {
                let kind = match self.wrapper_kind() {
                    WRAP_SOME => "Some",
                    WRAP_OK => "Ok",
                    WRAP_ERR => "Err",
                    _ => "?",
                };
                write!(f, "{}(arena:{})", kind, self.wrapper_index())
            }
            _ => write!(f, "{}(arena:{})", self.type_name(), self.arena_index()),
        }
    }
}

// ---------------------------------------------------------------------------
// Arena
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Arena {
    young_entries: Vec<ArenaEntry>,
    yard_entries: Vec<ArenaEntry>,
    handoff_entries: Vec<ArenaEntry>,
    stable_entries: Vec<ArenaEntry>,
    scratch_young: Vec<u32>,
    scratch_yard: Vec<u32>,
    scratch_handoff: Vec<u32>,
    scratch_stable: Vec<u32>,
    peak_usage: ArenaUsage,
    alloc_space: AllocSpace,
    pub(crate) type_names: Vec<String>,
    pub(crate) type_field_names: Vec<Vec<String>>,
    pub(crate) type_variant_names: Vec<Vec<String>>,
}

#[derive(Debug, Clone)]
pub enum ArenaEntry {
    Int(i64),
    String(Rc<str>),
    List(ArenaList),
    Tuple(Vec<NanValue>),
    Map(PersistentMap),
    Record {
        type_id: u32,
        fields: Vec<NanValue>,
    },
    Variant {
        type_id: u32,
        variant_id: u16,
        fields: Vec<NanValue>,
    },
    Fn(Rc<FunctionValue>),
    Builtin(Rc<str>),
    Namespace {
        name: Rc<str>,
        members: Vec<(Rc<str>, NanValue)>,
    },
    Boxed(NanValue),
}

#[derive(Debug, Clone)]
pub enum ArenaList {
    Flat {
        items: Rc<Vec<NanValue>>,
        start: usize,
    },
    Prepend {
        head: NanValue,
        tail: NanValue,
        len: usize,
    },
    Concat {
        left: NanValue,
        right: NanValue,
        len: usize,
    },
    Segments {
        current: NanValue,
        rest: Rc<Vec<NanValue>>,
        start: usize,
        len: usize,
    },
}

const LIST_APPEND_CHUNK_LIMIT: usize = 128;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HeapSpace {
    Young = 0,
    Yard = 1,
    Handoff = 2,
    Stable = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocSpace {
    Young,
    Yard,
    Handoff,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ArenaUsage {
    pub young: usize,
    pub yard: usize,
    pub handoff: usize,
    pub stable: usize,
}

impl ArenaUsage {
    pub fn total(self) -> usize {
        self.young + self.yard + self.handoff + self.stable
    }
}

const HEAP_SPACE_SHIFT: u32 = 30;
const HEAP_SPACE_MASK_U32: u32 = 0b11 << HEAP_SPACE_SHIFT;
const HEAP_INDEX_MASK_U32: u32 = (1 << HEAP_SPACE_SHIFT) - 1;

mod arena;
mod compare;
mod convert;
mod lists;
mod memory;

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests;
