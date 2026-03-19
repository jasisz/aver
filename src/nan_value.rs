//! NaN-boxed compact Value representation (8 bytes per value).
//!
//! Layout: every value is a `u64` interpreted as an IEEE 754 `f64`.
//!
//! - **Float**: any f64 that is NOT a quiet NaN with our marker → stored directly.
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

use std::collections::HashMap;
use std::rc::Rc;

/// Persistent immutable map — O(1) clone via structural sharing.
pub type PersistentMap = im::HashMap<u64, (NanValue, NanValue)>;

use crate::value::FunctionValue;

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
// NanValue — the 8-byte compact value
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

    /// Raw bits — useful for using as HashMap key (inline values only).
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
            3u8.hash(&mut hasher); // tag discriminant to avoid collisions with ints
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
}

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

const HEAP_SPACE_SHIFT: u32 = 30;
const HEAP_SPACE_MASK_U32: u32 = 0b11 << HEAP_SPACE_SHIFT;
const HEAP_INDEX_MASK_U32: u32 = (1 << HEAP_SPACE_SHIFT) - 1;

impl Arena {
    pub fn new() -> Self {
        Arena {
            young_entries: Vec::with_capacity(256),
            yard_entries: Vec::with_capacity(64),
            handoff_entries: Vec::with_capacity(64),
            stable_entries: Vec::with_capacity(64),
            alloc_space: AllocSpace::Young,
            type_names: Vec::new(),
            type_field_names: Vec::new(),
            type_variant_names: Vec::new(),
        }
    }

    #[inline]
    pub fn push(&mut self, entry: ArenaEntry) -> u32 {
        match self.alloc_space {
            AllocSpace::Young => {
                let idx = self.young_entries.len() as u32;
                self.young_entries.push(entry);
                Self::encode_index(HeapSpace::Young, idx)
            }
            AllocSpace::Yard => {
                let idx = self.yard_entries.len() as u32;
                self.yard_entries.push(entry);
                Self::encode_index(HeapSpace::Yard, idx)
            }
            AllocSpace::Handoff => {
                let idx = self.handoff_entries.len() as u32;
                self.handoff_entries.push(entry);
                Self::encode_index(HeapSpace::Handoff, idx)
            }
        }
    }

    #[inline]
    pub fn get(&self, index: u32) -> &ArenaEntry {
        let (space, raw_index) = Self::decode_index(index);
        match space {
            HeapSpace::Young => &self.young_entries[raw_index as usize],
            HeapSpace::Yard => &self.yard_entries[raw_index as usize],
            HeapSpace::Handoff => &self.handoff_entries[raw_index as usize],
            HeapSpace::Stable => &self.stable_entries[raw_index as usize],
        }
    }

    #[inline]
    fn encode_index(space: HeapSpace, index: u32) -> u32 {
        ((space as u32) << HEAP_SPACE_SHIFT) | index
    }

    #[inline]
    fn encode_yard_index(index: u32) -> u32 {
        Self::encode_index(HeapSpace::Yard, index)
    }

    #[inline]
    fn encode_stable_index(index: u32) -> u32 {
        Self::encode_index(HeapSpace::Stable, index)
    }

    #[inline]
    fn encode_handoff_index(index: u32) -> u32 {
        Self::encode_index(HeapSpace::Handoff, index)
    }

    #[inline]
    fn decode_index(index: u32) -> (HeapSpace, u32) {
        let space = match (index & HEAP_SPACE_MASK_U32) >> HEAP_SPACE_SHIFT {
            0 => HeapSpace::Young,
            1 => HeapSpace::Yard,
            2 => HeapSpace::Handoff,
            3 => HeapSpace::Stable,
            _ => unreachable!("invalid heap space bits"),
        };
        (space, index & HEAP_INDEX_MASK_U32)
    }

    #[inline]
    pub fn is_stable_index(index: u32) -> bool {
        matches!(Self::decode_index(index).0, HeapSpace::Stable)
    }

    #[inline]
    pub fn is_yard_index_in_region(&self, index: u32, mark: u32) -> bool {
        let (space, raw_index) = Self::decode_index(index);
        matches!(space, HeapSpace::Yard)
            && raw_index >= mark
            && raw_index < self.yard_entries.len() as u32
    }

    #[inline]
    pub fn is_handoff_index_in_region(&self, index: u32, mark: u32) -> bool {
        let (space, raw_index) = Self::decode_index(index);
        matches!(space, HeapSpace::Handoff)
            && raw_index >= mark
            && raw_index < self.handoff_entries.len() as u32
    }

    #[inline]
    pub fn is_young_index_in_region(&self, index: u32, mark: u32) -> bool {
        let (space, raw_index) = Self::decode_index(index);
        matches!(space, HeapSpace::Young)
            && raw_index >= mark
            && raw_index < self.young_entries.len() as u32
    }

    #[inline]
    pub fn young_len(&self) -> usize {
        self.young_entries.len()
    }

    #[inline]
    pub fn yard_len(&self) -> usize {
        self.yard_entries.len()
    }

    #[inline]
    pub fn handoff_len(&self) -> usize {
        self.handoff_entries.len()
    }

    #[inline]
    pub fn is_frame_local_index(
        &self,
        index: u32,
        arena_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
    ) -> bool {
        self.is_young_index_in_region(index, arena_mark)
            || self.is_yard_index_in_region(index, yard_mark)
            || self.is_handoff_index_in_region(index, handoff_mark)
    }

    pub fn with_alloc_space<T>(&mut self, space: AllocSpace, f: impl FnOnce(&mut Arena) -> T) -> T {
        let prev = self.alloc_space;
        self.alloc_space = space;
        let out = f(self);
        self.alloc_space = prev;
        out
    }

    // -- Typed push helpers ------------------------------------------------

    pub fn push_i64(&mut self, val: i64) -> u32 {
        self.push(ArenaEntry::Int(val))
    }
    pub fn push_string(&mut self, s: &str) -> u32 {
        self.push(ArenaEntry::String(Rc::from(s)))
    }
    pub fn push_boxed(&mut self, val: NanValue) -> u32 {
        self.push(ArenaEntry::Boxed(val))
    }
    pub fn push_record(&mut self, type_id: u32, fields: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::Record { type_id, fields })
    }
    pub fn push_variant(&mut self, type_id: u32, variant_id: u16, fields: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::Variant {
            type_id,
            variant_id,
            fields,
        })
    }
    pub fn push_list(&mut self, items: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::List(ArenaList::Flat {
            items: Rc::new(items),
            start: 0,
        }))
    }
    pub fn push_map(&mut self, map: PersistentMap) -> u32 {
        self.push(ArenaEntry::Map(map))
    }
    pub fn push_tuple(&mut self, items: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::Tuple(items))
    }
    pub fn push_fn(&mut self, f: Rc<FunctionValue>) -> u32 {
        self.push(ArenaEntry::Fn(f))
    }
    pub fn push_builtin(&mut self, name: &str) -> u32 {
        self.push(ArenaEntry::Builtin(Rc::from(name)))
    }

    // -- Typed getters -----------------------------------------------------

    pub fn get_i64(&self, index: u32) -> i64 {
        match self.get(index) {
            ArenaEntry::Int(i) => *i,
            _ => panic!("Arena: expected Int at {}", index),
        }
    }
    pub fn get_string(&self, index: u32) -> &str {
        match self.get(index) {
            ArenaEntry::String(s) => s,
            _ => panic!("Arena: expected String at {}", index),
        }
    }
    pub fn get_boxed(&self, index: u32) -> NanValue {
        match self.get(index) {
            ArenaEntry::Boxed(v) => *v,
            _ => panic!("Arena: expected Boxed at {}", index),
        }
    }
    pub fn get_record(&self, index: u32) -> (u32, &[NanValue]) {
        match self.get(index) {
            ArenaEntry::Record { type_id, fields } => (*type_id, fields),
            _ => panic!("Arena: expected Record at {}", index),
        }
    }
    pub fn get_variant(&self, index: u32) -> (u32, u16, &[NanValue]) {
        match self.get(index) {
            ArenaEntry::Variant {
                type_id,
                variant_id,
                fields,
            } => (*type_id, *variant_id, fields),
            other => panic!("Arena: expected Variant at {} but found {:?}", index, other),
        }
    }
    pub fn get_list(&self, index: u32) -> &ArenaList {
        match self.get(index) {
            ArenaEntry::List(items) => items,
            _ => panic!("Arena: expected List at {}", index),
        }
    }
    pub fn get_tuple(&self, index: u32) -> &[NanValue] {
        match self.get(index) {
            ArenaEntry::Tuple(items) => items,
            _ => panic!("Arena: expected Tuple at {}", index),
        }
    }
    pub fn get_map(&self, index: u32) -> &PersistentMap {
        match self.get(index) {
            ArenaEntry::Map(map) => map,
            _ => panic!("Arena: expected Map at {}", index),
        }
    }
    pub fn get_fn(&self, index: u32) -> &FunctionValue {
        match self.get(index) {
            ArenaEntry::Fn(f) => f,
            _ => panic!("Arena: expected Fn at {}", index),
        }
    }
    pub fn get_fn_rc(&self, index: u32) -> &Rc<FunctionValue> {
        match self.get(index) {
            ArenaEntry::Fn(f) => f,
            _ => panic!("Arena: expected Fn at {}", index),
        }
    }
    pub fn get_builtin(&self, index: u32) -> &str {
        match self.get(index) {
            ArenaEntry::Builtin(s) => s,
            _ => panic!("Arena: expected Builtin at {}", index),
        }
    }
    pub fn get_namespace(&self, index: u32) -> (&str, &[(Rc<str>, NanValue)]) {
        match self.get(index) {
            ArenaEntry::Namespace { name, members } => (name, members),
            _ => panic!("Arena: expected Namespace at {}", index),
        }
    }

    // -- Type registry -----------------------------------------------------

    pub fn register_record_type(&mut self, name: &str, field_names: Vec<String>) -> u32 {
        let id = self.type_names.len() as u32;
        self.type_names.push(name.to_string());
        self.type_field_names.push(field_names);
        self.type_variant_names.push(Vec::new());
        id
    }

    pub fn register_sum_type(&mut self, name: &str, variant_names: Vec<String>) -> u32 {
        let id = self.type_names.len() as u32;
        self.type_names.push(name.to_string());
        self.type_field_names.push(Vec::new());
        self.type_variant_names.push(variant_names);
        id
    }

    pub fn get_type_name(&self, type_id: u32) -> &str {
        &self.type_names[type_id as usize]
    }
    pub fn get_field_names(&self, type_id: u32) -> &[String] {
        &self.type_field_names[type_id as usize]
    }
    pub fn get_variant_name(&self, type_id: u32, variant_id: u16) -> &str {
        &self.type_variant_names[type_id as usize][variant_id as usize]
    }
    pub fn find_type_id(&self, name: &str) -> Option<u32> {
        self.type_names
            .iter()
            .position(|n| n == name)
            .map(|i| i as u32)
    }
    pub fn find_variant_id(&self, type_id: u32, variant_name: &str) -> Option<u16> {
        self.type_variant_names
            .get(type_id as usize)?
            .iter()
            .position(|n| n == variant_name)
            .map(|i| i as u16)
    }

    pub fn len(&self) -> usize {
        self.young_entries.len()
            + self.yard_entries.len()
            + self.handoff_entries.len()
            + self.stable_entries.len()
    }
    pub fn is_empty(&self) -> bool {
        self.young_entries.is_empty()
            && self.yard_entries.is_empty()
            && self.handoff_entries.is_empty()
            && self.stable_entries.is_empty()
    }

    pub fn push_list_prepend(&mut self, head: NanValue, tail: NanValue) -> u32 {
        debug_assert!(tail.is_list());
        let len = self.list_len(tail.arena_index()) + 1;
        self.push(ArenaEntry::List(ArenaList::Prepend { head, tail, len }))
    }

    pub fn push_list_concat(&mut self, left: NanValue, right: NanValue) -> u32 {
        debug_assert!(left.is_list());
        debug_assert!(right.is_list());
        let len = self.list_len(left.arena_index()) + self.list_len(right.arena_index());
        self.push(ArenaEntry::List(ArenaList::Concat { left, right, len }))
    }

    pub fn list_len(&self, index: u32) -> usize {
        match self.get_list(index) {
            ArenaList::Flat { items, start } => items.len().saturating_sub(*start),
            ArenaList::Prepend { len, .. } | ArenaList::Concat { len, .. } => *len,
        }
    }

    pub fn list_is_empty(&self, index: u32) -> bool {
        self.list_len(index) == 0
    }

    pub fn list_get(&self, index: u32, position: usize) -> Option<NanValue> {
        let mut current = NanValue::new_list(index);
        let mut remaining = position;

        loop {
            match self.get_list(current.arena_index()) {
                ArenaList::Flat { items, start } => {
                    return items.get(start.saturating_add(remaining)).copied();
                }
                ArenaList::Prepend { head, tail, .. } => {
                    if remaining == 0 {
                        return Some(*head);
                    }
                    remaining -= 1;
                    current = *tail;
                }
                ArenaList::Concat { left, right, .. } => {
                    let left_len = self.list_len(left.arena_index());
                    if remaining < left_len {
                        current = *left;
                    } else {
                        remaining -= left_len;
                        current = *right;
                    }
                }
            }
        }
    }

    pub fn list_to_vec(&self, index: u32) -> Vec<NanValue> {
        let mut out = Vec::with_capacity(self.list_len(index));
        let mut stack = vec![NanValue::new_list(index)];

        while let Some(list) = stack.pop() {
            match self.get_list(list.arena_index()) {
                ArenaList::Flat { items, start } => {
                    out.extend(items[*start..].iter().copied());
                }
                ArenaList::Prepend { head, tail, .. } => {
                    out.push(*head);
                    stack.push(*tail);
                }
                ArenaList::Concat { left, right, .. } => {
                    stack.push(*right);
                    stack.push(*left);
                }
            }
        }

        out
    }

    pub fn list_uncons(&mut self, list: NanValue) -> Option<(NanValue, NanValue)> {
        debug_assert!(list.is_list());
        let mut rights = Vec::new();
        let mut current = list;

        loop {
            match self.get_list(current.arena_index()).clone() {
                ArenaList::Flat { items, start } => {
                    let head = *items.get(start)?;
                    let tail = if start + 1 >= items.len() {
                        NanValue::new_list(self.push_list(Vec::new()))
                    } else {
                        let tail_idx = self.push(ArenaEntry::List(ArenaList::Flat {
                            items: Rc::clone(&items),
                            start: start + 1,
                        }));
                        NanValue::new_list(tail_idx)
                    };
                    return Some((head, self.rebuild_list_from_rights(tail, rights)));
                }
                ArenaList::Prepend { head, tail, .. } => {
                    return Some((head, self.rebuild_list_from_rights(tail, rights)));
                }
                ArenaList::Concat { left, right, .. } => {
                    if self.list_is_empty(left.arena_index()) {
                        current = right;
                    } else {
                        rights.push(right);
                        current = left;
                    }
                }
            }
        }
    }

    fn rebuild_list_from_rights(
        &mut self,
        mut base: NanValue,
        mut rights: Vec<NanValue>,
    ) -> NanValue {
        while let Some(right) = rights.pop() {
            if self.list_is_empty(base.arena_index()) {
                base = right;
            } else if self.list_is_empty(right.arena_index()) {
                continue;
            } else {
                base = NanValue::new_list(self.push_list_concat(base, right));
            }
        }
        base
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::new()
    }
}

impl Arena {
    pub fn truncate_to(&mut self, mark: u32) {
        self.young_entries.truncate(mark as usize);
    }

    pub fn collect_young_from_roots(&mut self, mark: u32, roots: &mut [NanValue]) {
        if self.young_entries.len() <= mark as usize {
            return;
        }

        let mut relocated = vec![u32::MAX; self.young_entries.len()];
        let mut compacted = Vec::with_capacity(self.young_entries.len() - mark as usize);

        for root in roots {
            *root = self.relocate_young_root(*root, mark, &mut relocated, &mut compacted);
        }

        self.young_entries.truncate(mark as usize);
        self.young_entries.extend(compacted);
    }

    pub fn truncate_yard_to(&mut self, mark: u32) {
        self.yard_entries.truncate(mark as usize);
    }

    pub fn truncate_handoff_to(&mut self, mark: u32) {
        self.handoff_entries.truncate(mark as usize);
    }

    pub fn evacuate_frame_to_yard(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        roots: &mut [NanValue],
    ) -> (bool, bool) {
        self.evacuate_frame_locals(young_mark, yard_mark, handoff_mark, roots, AllocSpace::Yard)
    }

    pub fn evacuate_frame_to_handoff(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        roots: &mut [NanValue],
    ) -> (bool, bool) {
        self.evacuate_frame_locals(
            young_mark,
            yard_mark,
            handoff_mark,
            roots,
            AllocSpace::Handoff,
        )
    }

    fn evacuate_frame_locals(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        roots: &mut [NanValue],
        young_target: AllocSpace,
    ) -> (bool, bool) {
        let mut relocated_young = vec![u32::MAX; self.young_entries.len()];
        let mut relocated_yard = vec![u32::MAX; self.yard_entries.len()];
        let mut relocated_handoff = vec![u32::MAX; self.handoff_entries.len()];
        let mut compacted_yard =
            Vec::with_capacity(self.yard_entries.len().saturating_sub(yard_mark as usize));
        let mut compacted_handoff = Vec::with_capacity(
            self.handoff_entries
                .len()
                .saturating_sub(handoff_mark as usize),
        );

        for root in roots {
            *root = self.evacuate_local_root(
                *root,
                young_mark,
                yard_mark,
                handoff_mark,
                young_target,
                &mut relocated_young,
                &mut relocated_yard,
                &mut relocated_handoff,
                &mut compacted_yard,
                &mut compacted_handoff,
            );
        }

        self.young_entries.truncate(young_mark as usize);
        self.yard_entries.truncate(yard_mark as usize);
        self.yard_entries.extend(compacted_yard);
        self.handoff_entries.truncate(handoff_mark as usize);
        self.handoff_entries.extend(compacted_handoff);

        (
            self.yard_entries.len() > yard_mark as usize,
            self.handoff_entries.len() > handoff_mark as usize,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_local_root(
        &mut self,
        value: NanValue,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        match Self::decode_index(index).0 {
            HeapSpace::Young if self.is_young_index_in_region(index, young_mark) => self
                .evacuate_young_value(
                    value,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                ),
            HeapSpace::Yard if self.is_yard_index_in_region(index, yard_mark) => self
                .evacuate_yard_value(
                    value,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                ),
            HeapSpace::Handoff if self.is_handoff_index_in_region(index, handoff_mark) => self
                .evacuate_handoff_value(
                    value,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                ),
            _ => value,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_young_value(
        &mut self,
        value: NanValue,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let index = value.heap_index().expect("young value must be heap-backed");
        let (_, raw_index) = Self::decode_index(index);
        let relocation_slot = raw_index as usize;
        let relocated_index = relocated_young[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let (new_index, compacted_pos) = match young_target {
            AllocSpace::Yard => {
                let pos = compacted_yard.len() as u32;
                let idx = Self::encode_yard_index(yard_mark + pos);
                compacted_yard.push(ArenaEntry::Int(0));
                (idx, pos)
            }
            AllocSpace::Handoff => {
                let pos = compacted_handoff.len() as u32;
                let idx = Self::encode_handoff_index(handoff_mark + pos);
                compacted_handoff.push(ArenaEntry::Int(0));
                (idx, pos)
            }
            AllocSpace::Young => unreachable!("young evacuation must target yard or handoff"),
        };
        relocated_young[relocation_slot] = new_index;

        let entry = std::mem::replace(
            &mut self.young_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.evacuate_local_entry(
            entry,
            young_mark,
            yard_mark,
            handoff_mark,
            young_target,
            relocated_young,
            relocated_yard,
            relocated_handoff,
            compacted_yard,
            compacted_handoff,
        );
        match young_target {
            AllocSpace::Yard => compacted_yard[compacted_pos as usize] = new_entry,
            AllocSpace::Handoff => compacted_handoff[compacted_pos as usize] = new_entry,
            AllocSpace::Young => unreachable!(),
        }
        value.with_heap_index(new_index)
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_yard_value(
        &mut self,
        value: NanValue,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let index = value.heap_index().expect("yard value must be heap-backed");
        let (_, raw_index) = Self::decode_index(index);
        let relocation_slot = raw_index as usize;
        let relocated_index = relocated_yard[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let compacted_pos = compacted_yard.len() as u32;
        let new_index = Self::encode_yard_index(yard_mark + compacted_pos);
        relocated_yard[relocation_slot] = new_index;
        compacted_yard.push(ArenaEntry::Int(0));

        let entry = std::mem::replace(
            &mut self.yard_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.evacuate_local_entry(
            entry,
            young_mark,
            yard_mark,
            handoff_mark,
            young_target,
            relocated_young,
            relocated_yard,
            relocated_handoff,
            compacted_yard,
            compacted_handoff,
        );
        compacted_yard[compacted_pos as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_handoff_value(
        &mut self,
        value: NanValue,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let index = value
            .heap_index()
            .expect("handoff value must be heap-backed");
        let (_, raw_index) = Self::decode_index(index);
        let relocation_slot = raw_index as usize;
        let relocated_index = relocated_handoff[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let compacted_pos = compacted_handoff.len() as u32;
        let new_index = Self::encode_handoff_index(handoff_mark + compacted_pos);
        relocated_handoff[relocation_slot] = new_index;
        compacted_handoff.push(ArenaEntry::Int(0));

        let entry = std::mem::replace(
            &mut self.handoff_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.evacuate_local_entry(
            entry,
            young_mark,
            yard_mark,
            handoff_mark,
            young_target,
            relocated_young,
            relocated_yard,
            relocated_handoff,
            compacted_yard,
            compacted_handoff,
        );
        compacted_handoff[compacted_pos as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_local_entry(
        &mut self,
        entry: ArenaEntry,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        match entry {
            ArenaEntry::Int(i) => ArenaEntry::Int(i),
            ArenaEntry::String(s) => ArenaEntry::String(s),
            ArenaEntry::Builtin(name) => ArenaEntry::Builtin(name),
            ArenaEntry::Fn(f) => ArenaEntry::Fn(f),
            ArenaEntry::Boxed(inner) => ArenaEntry::Boxed(self.evacuate_local_root(
                inner,
                young_mark,
                yard_mark,
                handoff_mark,
                young_target,
                relocated_young,
                relocated_yard,
                relocated_handoff,
                compacted_yard,
                compacted_handoff,
            )),
            ArenaEntry::List(list) => ArenaEntry::List(self.evacuate_local_list(
                list,
                young_mark,
                yard_mark,
                handoff_mark,
                young_target,
                relocated_young,
                relocated_yard,
                relocated_handoff,
                compacted_yard,
                compacted_handoff,
            )),
            ArenaEntry::Tuple(mut items) => {
                for value in &mut items {
                    *value = self.evacuate_local_root(
                        *value,
                        young_mark,
                        yard_mark,
                        handoff_mark,
                        young_target,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                        compacted_yard,
                        compacted_handoff,
                    );
                }
                ArenaEntry::Tuple(items)
            }
            ArenaEntry::Map(map) => {
                let mut out = PersistentMap::new();
                for (hash, (key, value)) in map {
                    let new_key = self.evacuate_local_root(
                        key,
                        young_mark,
                        yard_mark,
                        handoff_mark,
                        young_target,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                        compacted_yard,
                        compacted_handoff,
                    );
                    let new_value = self.evacuate_local_root(
                        value,
                        young_mark,
                        yard_mark,
                        handoff_mark,
                        young_target,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                        compacted_yard,
                        compacted_handoff,
                    );
                    out.insert(hash, (new_key, new_value));
                }
                ArenaEntry::Map(out)
            }
            ArenaEntry::Record {
                type_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.evacuate_local_root(
                        *value,
                        young_mark,
                        yard_mark,
                        handoff_mark,
                        young_target,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                        compacted_yard,
                        compacted_handoff,
                    );
                }
                ArenaEntry::Record { type_id, fields }
            }
            ArenaEntry::Variant {
                type_id,
                variant_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.evacuate_local_root(
                        *value,
                        young_mark,
                        yard_mark,
                        handoff_mark,
                        young_target,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                        compacted_yard,
                        compacted_handoff,
                    );
                }
                ArenaEntry::Variant {
                    type_id,
                    variant_id,
                    fields,
                }
            }
            ArenaEntry::Namespace { name, mut members } => {
                for (_, value) in &mut members {
                    *value = self.evacuate_local_root(
                        *value,
                        young_mark,
                        yard_mark,
                        handoff_mark,
                        young_target,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                        compacted_yard,
                        compacted_handoff,
                    );
                }
                ArenaEntry::Namespace { name, members }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_local_list(
        &mut self,
        list: ArenaList,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
    ) -> ArenaList {
        match list {
            ArenaList::Flat { items, start } => ArenaList::Flat {
                items: Rc::new(
                    items[start..]
                        .iter()
                        .map(|value| {
                            self.evacuate_local_root(
                                *value,
                                young_mark,
                                yard_mark,
                                handoff_mark,
                                young_target,
                                relocated_young,
                                relocated_yard,
                                relocated_handoff,
                                compacted_yard,
                                compacted_handoff,
                            )
                        })
                        .collect(),
                ),
                start: 0,
            },
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: self.evacuate_local_root(
                    head,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                ),
                tail: self.evacuate_local_root(
                    tail,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                ),
                len,
            },
            ArenaList::Concat { left, right, len } => ArenaList::Concat {
                left: self.evacuate_local_root(
                    left,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                ),
                right: self.evacuate_local_root(
                    right,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                ),
                len,
            },
        }
    }

    fn relocate_young_root(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if matches!(space, HeapSpace::Young)
            && raw_index >= mark
            && raw_index < self.young_entries.len() as u32
        {
            return self.relocate_young_value(value, mark, relocated, compacted);
        }
        self.rewrite_young_refs_in_place(space, raw_index, mark, relocated, compacted);
        value
    }

    fn relocate_young_value(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if !matches!(space, HeapSpace::Young) || raw_index < mark {
            return value;
        }

        let relocation_slot = raw_index as usize;
        let relocated_index = relocated[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let compacted_pos = compacted.len() as u32;
        let new_index = Self::encode_index(HeapSpace::Young, mark + compacted_pos);
        relocated[relocation_slot] = new_index;
        compacted.push(ArenaEntry::Int(0));

        let entry = std::mem::replace(
            &mut self.young_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.relocate_young_entry(entry, mark, relocated, compacted);
        compacted[compacted_pos as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn relocate_young_entry(
        &mut self,
        entry: ArenaEntry,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        match entry {
            ArenaEntry::Int(i) => ArenaEntry::Int(i),
            ArenaEntry::String(s) => ArenaEntry::String(s),
            ArenaEntry::Builtin(name) => ArenaEntry::Builtin(name),
            ArenaEntry::Fn(f) => ArenaEntry::Fn(f),
            ArenaEntry::Boxed(inner) => {
                ArenaEntry::Boxed(self.relocate_young_value(inner, mark, relocated, compacted))
            }
            ArenaEntry::List(list) => {
                ArenaEntry::List(self.relocate_young_list(list, mark, relocated, compacted))
            }
            ArenaEntry::Tuple(mut items) => {
                for value in &mut items {
                    *value = self.relocate_young_value(*value, mark, relocated, compacted);
                }
                ArenaEntry::Tuple(items)
            }
            ArenaEntry::Map(map) => {
                let mut out = PersistentMap::new();
                for (hash, (key, value)) in map {
                    let relocated_key = self.relocate_young_value(key, mark, relocated, compacted);
                    let relocated_value =
                        self.relocate_young_value(value, mark, relocated, compacted);
                    out.insert(hash, (relocated_key, relocated_value));
                }
                ArenaEntry::Map(out)
            }
            ArenaEntry::Record {
                type_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.relocate_young_value(*value, mark, relocated, compacted);
                }
                ArenaEntry::Record { type_id, fields }
            }
            ArenaEntry::Variant {
                type_id,
                variant_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.relocate_young_value(*value, mark, relocated, compacted);
                }
                ArenaEntry::Variant {
                    type_id,
                    variant_id,
                    fields,
                }
            }
            ArenaEntry::Namespace { name, mut members } => {
                for (_, value) in &mut members {
                    *value = self.relocate_young_value(*value, mark, relocated, compacted);
                }
                ArenaEntry::Namespace { name, members }
            }
        }
    }

    fn relocate_young_list(
        &mut self,
        list: ArenaList,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaList {
        match list {
            ArenaList::Flat { items, start } => ArenaList::Flat {
                items: Rc::new(
                    items[start..]
                        .iter()
                        .map(|value| self.relocate_young_value(*value, mark, relocated, compacted))
                        .collect(),
                ),
                start: 0,
            },
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: self.relocate_young_value(head, mark, relocated, compacted),
                tail: self.relocate_young_value(tail, mark, relocated, compacted),
                len,
            },
            ArenaList::Concat { left, right, len } => ArenaList::Concat {
                left: self.relocate_young_value(left, mark, relocated, compacted),
                right: self.relocate_young_value(right, mark, relocated, compacted),
                len,
            },
        }
    }

    fn rewrite_young_refs_in_place(
        &mut self,
        space: HeapSpace,
        raw_index: u32,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) {
        let raw_index = raw_index as usize;
        match space {
            HeapSpace::Young => {
                if raw_index >= self.young_entries.len() || raw_index >= mark as usize {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.young_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.young_entries[raw_index] = new_entry;
            }
            HeapSpace::Yard => {
                if raw_index >= self.yard_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.yard_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.yard_entries[raw_index] = new_entry;
            }
            HeapSpace::Handoff => {
                if raw_index >= self.handoff_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.handoff_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.handoff_entries[raw_index] = new_entry;
            }
            HeapSpace::Stable => {
                if raw_index >= self.stable_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.stable_entries[raw_index] = new_entry;
            }
        }
    }

    fn rewrite_young_entry(
        &mut self,
        entry: ArenaEntry,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        match entry {
            ArenaEntry::Int(i) => ArenaEntry::Int(i),
            ArenaEntry::String(s) => ArenaEntry::String(s),
            ArenaEntry::Builtin(name) => ArenaEntry::Builtin(name),
            ArenaEntry::Fn(f) => ArenaEntry::Fn(f),
            ArenaEntry::Boxed(inner) => {
                ArenaEntry::Boxed(self.relocate_young_root(inner, mark, relocated, compacted))
            }
            ArenaEntry::List(list) => {
                ArenaEntry::List(self.rewrite_young_list(list, mark, relocated, compacted))
            }
            ArenaEntry::Tuple(mut items) => {
                for value in &mut items {
                    *value = self.relocate_young_root(*value, mark, relocated, compacted);
                }
                ArenaEntry::Tuple(items)
            }
            ArenaEntry::Map(map) => {
                let mut out = PersistentMap::new();
                for (hash, (key, value)) in map {
                    let relocated_key = self.relocate_young_root(key, mark, relocated, compacted);
                    let relocated_value =
                        self.relocate_young_root(value, mark, relocated, compacted);
                    out.insert(hash, (relocated_key, relocated_value));
                }
                ArenaEntry::Map(out)
            }
            ArenaEntry::Record {
                type_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.relocate_young_root(*value, mark, relocated, compacted);
                }
                ArenaEntry::Record { type_id, fields }
            }
            ArenaEntry::Variant {
                type_id,
                variant_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.relocate_young_root(*value, mark, relocated, compacted);
                }
                ArenaEntry::Variant {
                    type_id,
                    variant_id,
                    fields,
                }
            }
            ArenaEntry::Namespace { name, mut members } => {
                for (_, value) in &mut members {
                    *value = self.relocate_young_root(*value, mark, relocated, compacted);
                }
                ArenaEntry::Namespace { name, members }
            }
        }
    }

    fn rewrite_young_list(
        &mut self,
        list: ArenaList,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaList {
        match list {
            ArenaList::Flat { items, start } => ArenaList::Flat {
                items: Rc::new(
                    items[start..]
                        .iter()
                        .map(|value| self.relocate_young_root(*value, mark, relocated, compacted))
                        .collect(),
                ),
                start: 0,
            },
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: self.relocate_young_root(head, mark, relocated, compacted),
                tail: self.relocate_young_root(tail, mark, relocated, compacted),
                len,
            },
            ArenaList::Concat { left, right, len } => ArenaList::Concat {
                left: self.relocate_young_root(left, mark, relocated, compacted),
                right: self.relocate_young_root(right, mark, relocated, compacted),
                len,
            },
        }
    }

    pub fn promote_young_roots_to_yard(&mut self, mark: u32, roots: &mut [NanValue]) {
        if self.young_entries.len() <= mark as usize {
            return;
        }

        let mut relocated = vec![u32::MAX; self.young_entries.len()];

        for root in roots {
            *root = self.promote_region_root_to_yard(*root, mark, &mut relocated);
        }

        self.young_entries.truncate(mark as usize);
    }

    pub fn promote_young_roots_to_handoff(&mut self, mark: u32, roots: &mut [NanValue]) {
        if self.young_entries.len() <= mark as usize {
            return;
        }

        let mut relocated = vec![u32::MAX; self.young_entries.len()];

        for root in roots {
            *root = self.promote_region_root_to_handoff(*root, mark, &mut relocated);
        }

        self.young_entries.truncate(mark as usize);
    }

    pub fn promote_roots_to_stable(&mut self, roots: &mut [NanValue]) {
        let mut relocated_young = vec![u32::MAX; self.young_entries.len()];
        let mut relocated_yard = vec![u32::MAX; self.yard_entries.len()];
        let mut relocated_handoff = vec![u32::MAX; self.handoff_entries.len()];

        for root in roots {
            *root = self.promote_value_to_stable(
                *root,
                &mut relocated_young,
                &mut relocated_yard,
                &mut relocated_handoff,
            );
        }
    }

    pub fn collect_yard_from_roots(&mut self, mark: u32, roots: &mut [NanValue]) {
        if self.yard_entries.len() <= mark as usize {
            return;
        }

        let mut relocated = vec![u32::MAX; self.yard_entries.len()];
        let mut compacted = Vec::with_capacity(self.yard_entries.len() - mark as usize);

        for root in roots {
            *root = self.relocate_yard_root(*root, mark, &mut relocated, &mut compacted);
        }

        self.yard_entries.truncate(mark as usize);
        self.yard_entries.extend(compacted);
    }

    pub fn collect_stable_from_roots(&mut self, roots: &mut [NanValue]) {
        if self.stable_entries.is_empty() {
            return;
        }

        let mut relocated = vec![u32::MAX; self.stable_entries.len()];
        let mut compacted = Vec::with_capacity(self.stable_entries.len());

        for root in roots {
            *root = self.relocate_stable_root(*root, &mut relocated, &mut compacted);
        }

        self.stable_entries = compacted;
    }

    fn promote_region_root_to_yard(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        if !self.is_young_index_in_region(index, mark) {
            return value;
        }
        self.promote_value_to_yard(value, relocated)
    }

    fn promote_region_root_to_handoff(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        if !self.is_young_index_in_region(index, mark) {
            return value;
        }
        self.promote_value_to_handoff(value, relocated)
    }

    fn promote_value_to_yard(&mut self, value: NanValue, relocated: &mut [u32]) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if !matches!(space, HeapSpace::Young) {
            return value;
        }

        let relocation_slot = raw_index as usize;
        let relocated_index = relocated[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let new_index = Self::encode_yard_index(self.yard_entries.len() as u32);
        relocated[relocation_slot] = new_index;
        self.yard_entries.push(ArenaEntry::Int(0));

        let entry = std::mem::replace(
            &mut self.young_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.promote_entry_to_yard(entry, relocated);
        self.yard_entries[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn promote_entry_to_yard(&mut self, entry: ArenaEntry, relocated: &mut [u32]) -> ArenaEntry {
        match entry {
            ArenaEntry::Int(i) => ArenaEntry::Int(i),
            ArenaEntry::String(s) => ArenaEntry::String(s),
            ArenaEntry::Builtin(name) => ArenaEntry::Builtin(name),
            ArenaEntry::Fn(f) => ArenaEntry::Fn(f),
            ArenaEntry::Boxed(inner) => {
                ArenaEntry::Boxed(self.promote_value_to_yard(inner, relocated))
            }
            ArenaEntry::List(list) => ArenaEntry::List(self.promote_list_to_yard(list, relocated)),
            ArenaEntry::Tuple(mut items) => {
                for value in &mut items {
                    *value = self.promote_value_to_yard(*value, relocated);
                }
                ArenaEntry::Tuple(items)
            }
            ArenaEntry::Map(map) => {
                let mut out = PersistentMap::new();
                for (hash, (key, value)) in map {
                    let relocated_key = self.promote_value_to_yard(key, relocated);
                    let relocated_value = self.promote_value_to_yard(value, relocated);
                    out.insert(hash, (relocated_key, relocated_value));
                }
                ArenaEntry::Map(out)
            }
            ArenaEntry::Record {
                type_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.promote_value_to_yard(*value, relocated);
                }
                ArenaEntry::Record { type_id, fields }
            }
            ArenaEntry::Variant {
                type_id,
                variant_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.promote_value_to_yard(*value, relocated);
                }
                ArenaEntry::Variant {
                    type_id,
                    variant_id,
                    fields,
                }
            }
            ArenaEntry::Namespace { name, mut members } => {
                for (_, value) in &mut members {
                    *value = self.promote_value_to_yard(*value, relocated);
                }
                ArenaEntry::Namespace { name, members }
            }
        }
    }

    fn promote_value_to_handoff(&mut self, value: NanValue, relocated: &mut [u32]) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if !matches!(space, HeapSpace::Young) {
            return value;
        }

        let relocation_slot = raw_index as usize;
        let relocated_index = relocated[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let new_index = Self::encode_handoff_index(self.handoff_entries.len() as u32);
        relocated[relocation_slot] = new_index;
        self.handoff_entries.push(ArenaEntry::Int(0));

        let entry = std::mem::replace(
            &mut self.young_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.promote_entry_to_handoff(entry, relocated);
        self.handoff_entries[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn promote_entry_to_handoff(&mut self, entry: ArenaEntry, relocated: &mut [u32]) -> ArenaEntry {
        match entry {
            ArenaEntry::Int(i) => ArenaEntry::Int(i),
            ArenaEntry::String(s) => ArenaEntry::String(s),
            ArenaEntry::Builtin(name) => ArenaEntry::Builtin(name),
            ArenaEntry::Fn(f) => ArenaEntry::Fn(f),
            ArenaEntry::Boxed(inner) => {
                ArenaEntry::Boxed(self.promote_value_to_handoff(inner, relocated))
            }
            ArenaEntry::List(list) => {
                ArenaEntry::List(self.promote_list_to_handoff(list, relocated))
            }
            ArenaEntry::Tuple(mut items) => {
                for value in &mut items {
                    *value = self.promote_value_to_handoff(*value, relocated);
                }
                ArenaEntry::Tuple(items)
            }
            ArenaEntry::Map(map) => {
                let mut out = PersistentMap::new();
                for (hash, (key, value)) in map {
                    let relocated_key = self.promote_value_to_handoff(key, relocated);
                    let relocated_value = self.promote_value_to_handoff(value, relocated);
                    out.insert(hash, (relocated_key, relocated_value));
                }
                ArenaEntry::Map(out)
            }
            ArenaEntry::Record {
                type_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.promote_value_to_handoff(*value, relocated);
                }
                ArenaEntry::Record { type_id, fields }
            }
            ArenaEntry::Variant {
                type_id,
                variant_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.promote_value_to_handoff(*value, relocated);
                }
                ArenaEntry::Variant {
                    type_id,
                    variant_id,
                    fields,
                }
            }
            ArenaEntry::Namespace { name, mut members } => {
                for (_, value) in &mut members {
                    *value = self.promote_value_to_handoff(*value, relocated);
                }
                ArenaEntry::Namespace { name, members }
            }
        }
    }

    fn promote_list_to_yard(&mut self, list: ArenaList, relocated: &mut [u32]) -> ArenaList {
        match list {
            ArenaList::Flat { items, start } => ArenaList::Flat {
                items: Rc::new(
                    items[start..]
                        .iter()
                        .map(|value| self.promote_value_to_yard(*value, relocated))
                        .collect(),
                ),
                start: 0,
            },
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: self.promote_value_to_yard(head, relocated),
                tail: self.promote_value_to_yard(tail, relocated),
                len,
            },
            ArenaList::Concat { left, right, len } => ArenaList::Concat {
                left: self.promote_value_to_yard(left, relocated),
                right: self.promote_value_to_yard(right, relocated),
                len,
            },
        }
    }

    fn promote_list_to_handoff(&mut self, list: ArenaList, relocated: &mut [u32]) -> ArenaList {
        match list {
            ArenaList::Flat { items, start } => ArenaList::Flat {
                items: Rc::new(
                    items[start..]
                        .iter()
                        .map(|value| self.promote_value_to_handoff(*value, relocated))
                        .collect(),
                ),
                start: 0,
            },
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: self.promote_value_to_handoff(head, relocated),
                tail: self.promote_value_to_handoff(tail, relocated),
                len,
            },
            ArenaList::Concat { left, right, len } => ArenaList::Concat {
                left: self.promote_value_to_handoff(left, relocated),
                right: self.promote_value_to_handoff(right, relocated),
                len,
            },
        }
    }

    fn promote_value_to_stable(
        &mut self,
        value: NanValue,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        match space {
            HeapSpace::Young => {
                let relocation_slot = raw_index as usize;
                let relocated_index = relocated_young[relocation_slot];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }

                let new_index = Self::encode_stable_index(self.stable_entries.len() as u32);
                relocated_young[relocation_slot] = new_index;
                self.stable_entries.push(ArenaEntry::Int(0));

                let entry = self.young_entries[raw_index as usize].clone();
                let new_entry = self.promote_entry_to_stable(
                    entry,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                );
                self.stable_entries[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
                value.with_heap_index(new_index)
            }
            HeapSpace::Yard => {
                let relocation_slot = raw_index as usize;
                let relocated_index = relocated_yard[relocation_slot];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }

                let new_index = Self::encode_stable_index(self.stable_entries.len() as u32);
                relocated_yard[relocation_slot] = new_index;
                self.stable_entries.push(ArenaEntry::Int(0));

                let entry = self.yard_entries[raw_index as usize].clone();
                let new_entry = self.promote_entry_to_stable(
                    entry,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                );
                self.stable_entries[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
                value.with_heap_index(new_index)
            }
            HeapSpace::Handoff => {
                let relocation_slot = raw_index as usize;
                let relocated_index = relocated_handoff[relocation_slot];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }

                let new_index = Self::encode_stable_index(self.stable_entries.len() as u32);
                relocated_handoff[relocation_slot] = new_index;
                self.stable_entries.push(ArenaEntry::Int(0));

                let entry = self.handoff_entries[raw_index as usize].clone();
                let new_entry = self.promote_entry_to_stable(
                    entry,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                );
                self.stable_entries[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
                value.with_heap_index(new_index)
            }
            HeapSpace::Stable => value,
        }
    }

    fn promote_entry_to_stable(
        &mut self,
        entry: ArenaEntry,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
    ) -> ArenaEntry {
        match entry {
            ArenaEntry::Int(i) => ArenaEntry::Int(i),
            ArenaEntry::String(s) => ArenaEntry::String(s),
            ArenaEntry::Builtin(name) => ArenaEntry::Builtin(name),
            ArenaEntry::Fn(f) => ArenaEntry::Fn(f),
            ArenaEntry::Boxed(inner) => ArenaEntry::Boxed(self.promote_value_to_stable(
                inner,
                relocated_young,
                relocated_yard,
                relocated_handoff,
            )),
            ArenaEntry::List(list) => ArenaEntry::List(self.promote_list_to_stable(
                list,
                relocated_young,
                relocated_yard,
                relocated_handoff,
            )),
            ArenaEntry::Tuple(mut items) => {
                for value in &mut items {
                    *value = self.promote_value_to_stable(
                        *value,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                    );
                }
                ArenaEntry::Tuple(items)
            }
            ArenaEntry::Map(map) => {
                let mut out = PersistentMap::new();
                for (hash, (key, value)) in map {
                    let relocated_key = self.promote_value_to_stable(
                        key,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                    );
                    let relocated_value = self.promote_value_to_stable(
                        value,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                    );
                    out.insert(hash, (relocated_key, relocated_value));
                }
                ArenaEntry::Map(out)
            }
            ArenaEntry::Record {
                type_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.promote_value_to_stable(
                        *value,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                    );
                }
                ArenaEntry::Record { type_id, fields }
            }
            ArenaEntry::Variant {
                type_id,
                variant_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.promote_value_to_stable(
                        *value,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                    );
                }
                ArenaEntry::Variant {
                    type_id,
                    variant_id,
                    fields,
                }
            }
            ArenaEntry::Namespace { name, mut members } => {
                for (_, value) in &mut members {
                    *value = self.promote_value_to_stable(
                        *value,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                    );
                }
                ArenaEntry::Namespace { name, members }
            }
        }
    }

    fn promote_list_to_stable(
        &mut self,
        list: ArenaList,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
    ) -> ArenaList {
        match list {
            ArenaList::Flat { items, start } => ArenaList::Flat {
                items: Rc::new(
                    items[start..]
                        .iter()
                        .map(|value| {
                            self.promote_value_to_stable(
                                *value,
                                relocated_young,
                                relocated_yard,
                                relocated_handoff,
                            )
                        })
                        .collect(),
                ),
                start: 0,
            },
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: self.promote_value_to_stable(
                    head,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                ),
                tail: self.promote_value_to_stable(
                    tail,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                ),
                len,
            },
            ArenaList::Concat { left, right, len } => ArenaList::Concat {
                left: self.promote_value_to_stable(
                    left,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                ),
                right: self.promote_value_to_stable(
                    right,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                ),
                len,
            },
        }
    }

    fn relocate_yard_root(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        if !self.is_yard_index_in_region(index, mark) {
            return value;
        }
        self.relocate_yard_value(value, mark, relocated, compacted)
    }

    fn relocate_yard_value(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if !matches!(space, HeapSpace::Yard) || raw_index < mark {
            return value;
        }

        let relocation_slot = raw_index as usize;
        let relocated_index = relocated[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let compacted_pos = compacted.len() as u32;
        let new_index = Self::encode_yard_index(mark + compacted_pos);
        relocated[relocation_slot] = new_index;
        compacted.push(ArenaEntry::Int(0));

        let entry = std::mem::replace(
            &mut self.yard_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.relocate_yard_entry(entry, mark, relocated, compacted);
        compacted[compacted_pos as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn relocate_yard_entry(
        &mut self,
        entry: ArenaEntry,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        match entry {
            ArenaEntry::Int(i) => ArenaEntry::Int(i),
            ArenaEntry::String(s) => ArenaEntry::String(s),
            ArenaEntry::Builtin(name) => ArenaEntry::Builtin(name),
            ArenaEntry::Fn(f) => ArenaEntry::Fn(f),
            ArenaEntry::Boxed(inner) => {
                ArenaEntry::Boxed(self.relocate_yard_value(inner, mark, relocated, compacted))
            }
            ArenaEntry::List(list) => {
                ArenaEntry::List(self.relocate_yard_list(list, mark, relocated, compacted))
            }
            ArenaEntry::Tuple(mut items) => {
                for value in &mut items {
                    *value = self.relocate_yard_value(*value, mark, relocated, compacted);
                }
                ArenaEntry::Tuple(items)
            }
            ArenaEntry::Map(map) => {
                let mut out = PersistentMap::new();
                for (hash, (key, value)) in map {
                    let relocated_key = self.relocate_yard_value(key, mark, relocated, compacted);
                    let relocated_value =
                        self.relocate_yard_value(value, mark, relocated, compacted);
                    out.insert(hash, (relocated_key, relocated_value));
                }
                ArenaEntry::Map(out)
            }
            ArenaEntry::Record {
                type_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.relocate_yard_value(*value, mark, relocated, compacted);
                }
                ArenaEntry::Record { type_id, fields }
            }
            ArenaEntry::Variant {
                type_id,
                variant_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.relocate_yard_value(*value, mark, relocated, compacted);
                }
                ArenaEntry::Variant {
                    type_id,
                    variant_id,
                    fields,
                }
            }
            ArenaEntry::Namespace { name, mut members } => {
                for (_, value) in &mut members {
                    *value = self.relocate_yard_value(*value, mark, relocated, compacted);
                }
                ArenaEntry::Namespace { name, members }
            }
        }
    }

    fn relocate_yard_list(
        &mut self,
        list: ArenaList,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaList {
        match list {
            ArenaList::Flat { items, start } => ArenaList::Flat {
                items: Rc::new(
                    items[start..]
                        .iter()
                        .map(|value| self.relocate_yard_value(*value, mark, relocated, compacted))
                        .collect(),
                ),
                start: 0,
            },
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: self.relocate_yard_value(head, mark, relocated, compacted),
                tail: self.relocate_yard_value(tail, mark, relocated, compacted),
                len,
            },
            ArenaList::Concat { left, right, len } => ArenaList::Concat {
                left: self.relocate_yard_value(left, mark, relocated, compacted),
                right: self.relocate_yard_value(right, mark, relocated, compacted),
                len,
            },
        }
    }

    fn relocate_stable_root(
        &mut self,
        value: NanValue,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        if !matches!(Self::decode_index(index).0, HeapSpace::Stable) {
            return value;
        }
        self.relocate_stable_value(value, relocated, compacted)
    }

    fn relocate_stable_value(
        &mut self,
        value: NanValue,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if !matches!(space, HeapSpace::Stable) {
            return value;
        }

        let relocation_slot = raw_index as usize;
        let relocated_index = relocated[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let new_index = Self::encode_stable_index(compacted.len() as u32);
        relocated[relocation_slot] = new_index;
        compacted.push(ArenaEntry::Int(0));

        let entry = std::mem::replace(
            &mut self.stable_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.relocate_stable_entry(entry, relocated, compacted);
        compacted[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn relocate_stable_entry(
        &mut self,
        entry: ArenaEntry,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        match entry {
            ArenaEntry::Int(i) => ArenaEntry::Int(i),
            ArenaEntry::String(s) => ArenaEntry::String(s),
            ArenaEntry::Builtin(name) => ArenaEntry::Builtin(name),
            ArenaEntry::Fn(f) => ArenaEntry::Fn(f),
            ArenaEntry::Boxed(inner) => {
                ArenaEntry::Boxed(self.relocate_stable_value(inner, relocated, compacted))
            }
            ArenaEntry::List(list) => {
                ArenaEntry::List(self.relocate_stable_list(list, relocated, compacted))
            }
            ArenaEntry::Tuple(mut items) => {
                for value in &mut items {
                    *value = self.relocate_stable_value(*value, relocated, compacted);
                }
                ArenaEntry::Tuple(items)
            }
            ArenaEntry::Map(map) => {
                let mut out = PersistentMap::new();
                for (hash, (key, value)) in map {
                    let relocated_key = self.relocate_stable_value(key, relocated, compacted);
                    let relocated_value = self.relocate_stable_value(value, relocated, compacted);
                    out.insert(hash, (relocated_key, relocated_value));
                }
                ArenaEntry::Map(out)
            }
            ArenaEntry::Record {
                type_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.relocate_stable_value(*value, relocated, compacted);
                }
                ArenaEntry::Record { type_id, fields }
            }
            ArenaEntry::Variant {
                type_id,
                variant_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = self.relocate_stable_value(*value, relocated, compacted);
                }
                ArenaEntry::Variant {
                    type_id,
                    variant_id,
                    fields,
                }
            }
            ArenaEntry::Namespace { name, mut members } => {
                for (_, value) in &mut members {
                    *value = self.relocate_stable_value(*value, relocated, compacted);
                }
                ArenaEntry::Namespace { name, members }
            }
        }
    }

    fn relocate_stable_list(
        &mut self,
        list: ArenaList,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaList {
        match list {
            ArenaList::Flat { items, start } => ArenaList::Flat {
                items: Rc::new(
                    items[start..]
                        .iter()
                        .map(|value| self.relocate_stable_value(*value, relocated, compacted))
                        .collect(),
                ),
                start: 0,
            },
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: self.relocate_stable_value(head, relocated, compacted),
                tail: self.relocate_stable_value(tail, relocated, compacted),
                len,
            },
            ArenaList::Concat { left, right, len } => ArenaList::Concat {
                left: self.relocate_stable_value(left, relocated, compacted),
                right: self.relocate_stable_value(right, relocated, compacted),
                len,
            },
        }
    }
}

// ---------------------------------------------------------------------------
// Arena-aware methods on NanValue
// ---------------------------------------------------------------------------

impl NanValue {
    pub fn eq_in(self, other: Self, arena: &Arena) -> bool {
        if self.0 == other.0 {
            return true;
        }
        if self.is_float() != other.is_float() {
            return false;
        }
        if self.is_float() {
            return self.as_float() == other.as_float();
        }
        if self.tag() != other.tag() {
            return false;
        }
        match self.tag() {
            TAG_INT => self.as_int(arena) == other.as_int(arena),
            TAG_IMMEDIATE => false,
            TAG_WRAPPER => {
                self.wrapper_kind() == other.wrapper_kind() && {
                    let a = arena.get_boxed(self.wrapper_index());
                    let b = arena.get_boxed(other.wrapper_index());
                    a.eq_in(b, arena)
                }
            }
            TAG_STRING => {
                arena.get_string(self.arena_index()) == arena.get_string(other.arena_index())
            }
            TAG_LIST => {
                let a_idx = self.arena_index();
                let b_idx = other.arena_index();
                arena.list_len(a_idx) == arena.list_len(b_idx)
                    && (0..arena.list_len(a_idx)).all(|i| {
                        arena
                            .list_get(a_idx, i)
                            .zip(arena.list_get(b_idx, i))
                            .is_some_and(|(x, y)| x.eq_in(y, arena))
                    })
            }
            TAG_TUPLE => {
                let a = arena.get_tuple(self.arena_index());
                let b = arena.get_tuple(other.arena_index());
                a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.eq_in(*y, arena))
            }
            TAG_MAP => {
                let a = arena.get_map(self.arena_index());
                let b = arena.get_map(other.arena_index());
                a.len() == b.len()
                    && a.iter()
                        .all(|(k, (_, v1))| b.get(k).is_some_and(|(_, v2)| v1.eq_in(*v2, arena)))
            }
            TAG_RECORD => {
                let (ta, fa) = arena.get_record(self.arena_index());
                let (tb, fb) = arena.get_record(other.arena_index());
                ta == tb
                    && fa.len() == fb.len()
                    && fa.iter().zip(fb).all(|(a, b)| a.eq_in(*b, arena))
            }
            TAG_VARIANT => {
                let (ta, va, fa) = arena.get_variant(self.arena_index());
                let (tb, vb, fb) = arena.get_variant(other.arena_index());
                ta == tb
                    && va == vb
                    && fa.len() == fb.len()
                    && fa.iter().zip(fb).all(|(a, b)| a.eq_in(*b, arena))
            }
            TAG_FN => self.arena_index() == other.arena_index(),
            _ => false,
        }
    }

    pub fn hash_in<H: std::hash::Hasher>(self, state: &mut H, arena: &Arena) {
        use std::hash::Hash;
        if self.is_float() {
            1u8.hash(state);
            let f = self.as_float();
            let bits = if f == 0.0 {
                0.0f64.to_bits()
            } else {
                f.to_bits()
            };
            bits.hash(state);
            return;
        }
        let tag = self.tag();
        (tag as u8).hash(state);
        match tag {
            TAG_INT => self.as_int(arena).hash(state),
            TAG_IMMEDIATE => self.payload().hash(state),
            TAG_WRAPPER => {
                self.wrapper_kind().hash(state);
                arena.get_boxed(self.wrapper_index()).hash_in(state, arena);
            }
            TAG_STRING => arena.get_string(self.arena_index()).hash(state),
            TAG_LIST => {
                let list_idx = self.arena_index();
                arena.list_len(list_idx).hash(state);
                for item in arena.list_to_vec(list_idx) {
                    item.hash_in(state, arena);
                }
            }
            TAG_TUPLE => {
                let items = arena.get_tuple(self.arena_index());
                items.len().hash(state);
                for item in items {
                    item.hash_in(state, arena);
                }
            }
            TAG_RECORD => {
                let (tid, fields) = arena.get_record(self.arena_index());
                tid.hash(state);
                for f in fields {
                    f.hash_in(state, arena);
                }
            }
            TAG_VARIANT => {
                let (tid, vid, fields) = arena.get_variant(self.arena_index());
                tid.hash(state);
                vid.hash(state);
                for f in fields {
                    f.hash_in(state, arena);
                }
            }
            _ => self.0.hash(state),
        }
    }

    pub fn repr(self, arena: &Arena) -> String {
        if self.is_float() {
            return self.as_float().to_string();
        }
        match self.tag() {
            TAG_INT => self.as_int(arena).to_string(),
            TAG_IMMEDIATE => match self.payload() {
                IMM_FALSE => "false".into(),
                IMM_TRUE => "true".into(),
                IMM_UNIT => "Unit".into(),
                IMM_NONE => "Option.None".into(),
                _ => "??".into(),
            },
            TAG_WRAPPER => {
                let inner = arena.get_boxed(self.wrapper_index());
                let ir = inner.repr_inner(arena);
                match self.wrapper_kind() {
                    WRAP_SOME => format!("Option.Some({})", ir),
                    WRAP_OK => format!("Result.Ok({})", ir),
                    WRAP_ERR => format!("Result.Err({})", ir),
                    _ => "??".into(),
                }
            }
            TAG_STRING => arena.get_string(self.arena_index()).to_string(),
            TAG_LIST => {
                let parts: Vec<_> = arena
                    .list_to_vec(self.arena_index())
                    .into_iter()
                    .map(|v| v.repr_inner(arena))
                    .collect();
                format!("[{}]", parts.join(", "))
            }
            TAG_TUPLE => {
                let items = arena.get_tuple(self.arena_index());
                let parts: Vec<_> = items.iter().map(|v| v.repr_inner(arena)).collect();
                format!("({})", parts.join(", "))
            }
            TAG_MAP => {
                let map = arena.get_map(self.arena_index());
                let mut pairs: Vec<_> = map
                    .values()
                    .map(|(k, v)| (k.repr_inner(arena), v.repr_inner(arena)))
                    .collect();
                pairs.sort_by(|(a, _), (b, _)| a.cmp(b));
                let parts: Vec<_> = pairs
                    .into_iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                format!("{{{}}}", parts.join(", "))
            }
            TAG_RECORD => {
                let (tid, fields) = arena.get_record(self.arena_index());
                let name = arena.get_type_name(tid);
                let fnames = arena.get_field_names(tid);
                let parts: Vec<_> = fnames
                    .iter()
                    .zip(fields)
                    .map(|(n, v)| format!("{}: {}", n, v.repr_inner(arena)))
                    .collect();
                format!("{}({})", name, parts.join(", "))
            }
            TAG_VARIANT => {
                let (tid, vid, fields) = arena.get_variant(self.arena_index());
                let vname = arena.get_variant_name(tid, vid);
                if fields.is_empty() {
                    vname.to_string()
                } else {
                    let parts: Vec<_> = fields.iter().map(|v| v.repr_inner(arena)).collect();
                    format!("{}({})", vname, parts.join(", "))
                }
            }
            TAG_FN => format!("<fn {}>", arena.get_fn(self.arena_index()).name),
            TAG_BUILTIN => format!("<builtin {}>", arena.get_builtin(self.arena_index())),
            TAG_NAMESPACE => {
                let (name, _) = arena.get_namespace(self.arena_index());
                format!("<type {}>", name)
            }
            _ => "??".into(),
        }
    }

    fn repr_inner(self, arena: &Arena) -> String {
        if self.is_string() {
            return format!("\"{}\"", arena.get_string(self.arena_index()));
        }
        self.repr(arena)
    }

    pub fn display(self, arena: &Arena) -> Option<String> {
        if self.is_unit() {
            None
        } else {
            Some(self.repr(arena))
        }
    }
}

// ---------------------------------------------------------------------------
// Value <-> NanValue conversion
// ---------------------------------------------------------------------------

use crate::value::Value;

impl NanValue {
    /// Convert old Value to NanValue, storing heap data in arena.
    pub fn from_value(val: &Value, arena: &mut Arena) -> Self {
        match val {
            Value::Int(i) => NanValue::new_int(*i, arena),
            Value::Float(f) => NanValue::new_float(*f),
            Value::Bool(b) => NanValue::new_bool(*b),
            Value::Unit => NanValue::UNIT,
            Value::None => NanValue::NONE,
            Value::Str(s) => NanValue::new_string(arena.push_string(s)),
            Value::Ok(inner) => {
                let inner_nv = NanValue::from_value(inner, arena);
                let idx = arena.push_boxed(inner_nv);
                NanValue::new_ok(idx)
            }
            Value::Err(inner) => {
                let inner_nv = NanValue::from_value(inner, arena);
                let idx = arena.push_boxed(inner_nv);
                NanValue::new_err(idx)
            }
            Value::Some(inner) => {
                let inner_nv = NanValue::from_value(inner, arena);
                let idx = arena.push_boxed(inner_nv);
                NanValue::new_some(idx)
            }
            Value::Tuple(items) => {
                let nv_items: Vec<_> = items
                    .iter()
                    .map(|v| NanValue::from_value(v, arena))
                    .collect();
                NanValue::new_tuple(arena.push_tuple(nv_items))
            }
            Value::List(aver_list) => {
                let items: Vec<_> = aver_list
                    .to_vec()
                    .iter()
                    .map(|v| NanValue::from_value(v, arena))
                    .collect();
                NanValue::new_list(arena.push_list(items))
            }
            Value::Map(map) => {
                let mut nv_map = PersistentMap::new();
                for (k, v) in map {
                    let nk = NanValue::from_value(k, arena);
                    let nv = NanValue::from_value(v, arena);
                    nv_map.insert(nk.map_key_hash(arena), (nk, nv));
                }
                let idx = arena.push(ArenaEntry::Map(nv_map));
                NanValue::new_map(idx)
            }
            Value::Fn(f) => NanValue::new_fn(arena.push_fn(Rc::clone(f))),
            Value::Builtin(name) => NanValue::new_builtin(arena.push_builtin(name)),
            Value::Record { type_name, fields } => {
                let type_id = arena.find_type_id(type_name).unwrap_or_else(|| {
                    let field_names: Vec<String> = fields.iter().map(|(n, _)| n.clone()).collect();
                    arena.register_record_type(type_name, field_names)
                });
                let nv_fields: Vec<_> = fields
                    .iter()
                    .map(|(_, v)| NanValue::from_value(v, arena))
                    .collect();
                NanValue::new_record(arena.push_record(type_id, nv_fields))
            }
            Value::Variant {
                type_name,
                variant,
                fields,
            } => {
                let type_id = arena
                    .find_type_id(type_name)
                    .unwrap_or_else(|| arena.register_sum_type(type_name, vec![variant.clone()]));
                let variant_id = arena.find_variant_id(type_id, variant).unwrap_or_else(|| {
                    // Register new variant dynamically
                    let variants = &mut arena.type_variant_names[type_id as usize];
                    let id = variants.len() as u16;
                    variants.push(variant.clone());
                    id
                });
                let nv_fields: Vec<_> = fields
                    .iter()
                    .map(|v| NanValue::from_value(v, arena))
                    .collect();
                NanValue::new_variant(arena.push_variant(type_id, variant_id, nv_fields))
            }
            Value::Namespace { name, members } => {
                let nv_members: Vec<_> = members
                    .iter()
                    .map(|(k, v)| (Rc::from(k.as_str()), NanValue::from_value(v, arena)))
                    .collect();
                let idx = arena.push(ArenaEntry::Namespace {
                    name: Rc::from(name.as_str()),
                    members: nv_members,
                });
                NanValue::new_namespace(idx)
            }
        }
    }

    /// Convert NanValue back to old Value (for interop during migration).
    pub fn to_value(self, arena: &Arena) -> Value {
        if self.is_float() {
            return Value::Float(self.as_float());
        }
        match self.tag() {
            TAG_INT => Value::Int(self.as_int(arena)),
            TAG_IMMEDIATE => match self.payload() {
                IMM_FALSE => Value::Bool(false),
                IMM_TRUE => Value::Bool(true),
                IMM_UNIT => Value::Unit,
                IMM_NONE => Value::None,
                _ => Value::Unit,
            },
            TAG_WRAPPER => {
                let inner = arena.get_boxed(self.wrapper_index()).to_value(arena);
                match self.wrapper_kind() {
                    WRAP_SOME => Value::Some(Box::new(inner)),
                    WRAP_OK => Value::Ok(Box::new(inner)),
                    WRAP_ERR => Value::Err(Box::new(inner)),
                    _ => Value::Unit,
                }
            }
            TAG_STRING => Value::Str(arena.get_string(self.arena_index()).to_string()),
            TAG_LIST => {
                let vals: Vec<Value> = arena
                    .list_to_vec(self.arena_index())
                    .into_iter()
                    .map(|v| v.to_value(arena))
                    .collect();
                Value::List(aver_rt::AverList::from_vec(vals))
            }
            TAG_TUPLE => {
                let items = arena.get_tuple(self.arena_index());
                Value::Tuple(items.iter().map(|v| v.to_value(arena)).collect())
            }
            TAG_MAP => {
                let map = arena.get_map(self.arena_index());
                let mut hm = HashMap::new();
                for (k, v) in map.values() {
                    hm.insert(k.to_value(arena), v.to_value(arena));
                }
                Value::Map(hm)
            }
            TAG_RECORD => {
                let (type_id, fields) = arena.get_record(self.arena_index());
                let type_name = arena.get_type_name(type_id).to_string();
                let field_names = arena.get_field_names(type_id);
                let pairs: Vec<(String, Value)> = field_names
                    .iter()
                    .zip(fields)
                    .map(|(n, v)| (n.clone(), v.to_value(arena)))
                    .collect();
                Value::Record {
                    type_name,
                    fields: pairs.into(),
                }
            }
            TAG_VARIANT => {
                let (type_id, variant_id, fields) = arena.get_variant(self.arena_index());
                let type_name = arena.get_type_name(type_id).to_string();
                let variant = arena.get_variant_name(type_id, variant_id).to_string();
                let vals: Vec<Value> = fields.iter().map(|v| v.to_value(arena)).collect();
                Value::Variant {
                    type_name,
                    variant,
                    fields: vals.into(),
                }
            }
            TAG_FN => Value::Fn(Rc::clone(arena.get_fn_rc(self.arena_index()))),
            TAG_BUILTIN => Value::Builtin(arena.get_builtin(self.arena_index()).to_string()),
            TAG_NAMESPACE => {
                let (name, members) = arena.get_namespace(self.arena_index());
                let mut hm = HashMap::new();
                for (k, v) in members {
                    hm.insert(k.to_string(), v.to_value(arena));
                }
                Value::Namespace {
                    name: name.to_string(),
                    members: hm,
                }
            }
            _ => Value::Unit,
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use super::*;

    #[test]
    fn size_is_8_bytes() {
        assert_eq!(std::mem::size_of::<NanValue>(), 8);
    }

    #[test]
    fn float_roundtrip() {
        for &f in &[0.0, -0.0, 1.0, -1.0, 3.14, f64::INFINITY, f64::NEG_INFINITY] {
            let v = NanValue::new_float(f);
            assert!(v.is_float());
            assert_eq!(v.as_float().to_bits(), f.to_bits());
        }
    }

    #[test]
    fn float_nan_roundtrip() {
        let v = NanValue::new_float(f64::NAN);
        assert!(v.as_float().is_nan());
    }

    #[test]
    fn int_inline_roundtrip() {
        let mut arena = Arena::new();
        for i in [
            0,
            1,
            -1,
            42,
            -42,
            1_000_000,
            -1_000_000,
            INT_INLINE_MAX,
            INT_INLINE_MIN,
        ] {
            let v = NanValue::new_int(i, &mut arena);
            assert!(v.is_int());
            assert_eq!(v.as_int(&arena), i);
        }
        assert_eq!(arena.len(), 0);
    }

    #[test]
    fn int_big_roundtrip() {
        let mut arena = Arena::new();
        for i in [i64::MAX, i64::MIN, INT_INLINE_MAX + 1, INT_INLINE_MIN - 1] {
            let v = NanValue::new_int(i, &mut arena);
            assert!(v.is_int());
            assert_eq!(v.as_int(&arena), i);
        }
        assert_eq!(arena.len(), 4);
    }

    #[test]
    fn immediates() {
        assert!(NanValue::TRUE.is_bool());
        assert!(NanValue::FALSE.is_bool());
        assert!(NanValue::UNIT.is_unit());
        assert!(NanValue::NONE.is_none());
        assert!(!NanValue::TRUE.is_float());
        assert!(!NanValue::UNIT.is_int());
    }

    #[test]
    fn wrapper_some_roundtrip() {
        let mut arena = Arena::new();
        let inner = NanValue::new_int_inline(42);
        let idx = arena.push_boxed(inner);
        let v = NanValue::new_some(idx);
        assert!(v.is_some());
        assert_eq!(arena.get_boxed(v.wrapper_index()).as_int(&arena), 42);
    }

    #[test]
    fn wrapper_ok_err_roundtrip() {
        let mut arena = Arena::new();
        let ok_idx = arena.push_boxed(NanValue::new_int_inline(100));
        let ok_val = NanValue::new_ok(ok_idx);
        assert!(ok_val.is_ok());

        let err_str_idx = arena.push_string("error");
        let err_idx = arena.push_boxed(NanValue::new_string(err_str_idx));
        let err_val = NanValue::new_err(err_idx);
        assert!(err_val.is_err());

        assert_eq!(arena.get_boxed(ok_val.wrapper_index()).as_int(&arena), 100);
        let inner = arena.get_boxed(err_val.wrapper_index());
        assert_eq!(arena.get_string(inner.arena_index()), "error");
    }

    #[test]
    fn string_roundtrip() {
        let mut arena = Arena::new();
        let idx = arena.push_string("hello");
        let v = NanValue::new_string(idx);
        assert!(v.is_string());
        assert_eq!(arena.get_string(v.arena_index()), "hello");
    }

    #[test]
    fn record_roundtrip() {
        let mut arena = Arena::new();
        let fields = vec![NanValue::new_int_inline(1), NanValue::new_int_inline(2)];
        let idx = arena.push_record(0, fields);
        let v = NanValue::new_record(idx);
        assert!(v.is_record());
        let (tid, fields) = arena.get_record(v.arena_index());
        assert_eq!(tid, 0);
        assert_eq!(fields.len(), 2);
    }

    #[test]
    fn list_roundtrip() {
        let mut arena = Arena::new();
        let items = vec![NanValue::new_int_inline(10), NanValue::new_int_inline(20)];
        let idx = arena.push_list(items);
        let v = NanValue::new_list(idx);
        assert!(v.is_list());
        assert_eq!(arena.list_len(v.arena_index()), 2);
    }

    #[test]
    fn types_dont_collide() {
        let mut arena = Arena::new();
        let f = NanValue::new_float(3.14);
        let i = NanValue::new_int(42, &mut arena);
        let b = NanValue::TRUE;
        let u = NanValue::UNIT;
        let n = NanValue::NONE;

        assert!(f.is_float() && !f.is_int() && !f.is_bool());
        assert!(i.is_int() && !i.is_float() && !i.is_bool());
        assert!(b.is_bool() && !b.is_int() && !b.is_float());
        assert!(u.is_unit() && !u.is_bool() && !u.is_none());
        assert!(n.is_none() && !n.is_unit() && !n.is_bool());
    }

    #[test]
    fn nested_record_in_list() {
        let mut arena = Arena::new();
        let p1 = arena.push_record(
            0,
            vec![NanValue::new_int_inline(1), NanValue::new_int_inline(2)],
        );
        let p2 = arena.push_record(
            0,
            vec![NanValue::new_int_inline(3), NanValue::new_int_inline(4)],
        );
        let list_idx = arena.push_list(vec![NanValue::new_record(p1), NanValue::new_record(p2)]);
        let list = NanValue::new_list(list_idx);

        let second = arena.list_get(list.arena_index(), 1).unwrap();
        let (_, fields) = arena.get_record(second.arena_index());
        assert_eq!(fields[1].as_int(&arena), 4);
    }

    #[test]
    fn eq_inline_values() {
        let arena = Arena::new();
        assert!(NanValue::new_int_inline(42).eq_in(NanValue::new_int_inline(42), &arena));
        assert!(!NanValue::new_int_inline(42).eq_in(NanValue::new_int_inline(43), &arena));
        assert!(NanValue::TRUE.eq_in(NanValue::TRUE, &arena));
        assert!(!NanValue::TRUE.eq_in(NanValue::FALSE, &arena));
        assert!(NanValue::UNIT.eq_in(NanValue::UNIT, &arena));
        assert!(NanValue::new_float(3.14).eq_in(NanValue::new_float(3.14), &arena));
    }

    #[test]
    fn eq_string_by_content() {
        let mut arena = Arena::new();
        let a = NanValue::new_string(arena.push_string("hello"));
        let b = NanValue::new_string(arena.push_string("hello"));
        let c = NanValue::new_string(arena.push_string("world"));
        assert!(a.eq_in(b, &arena));
        assert!(!a.eq_in(c, &arena));
    }

    #[test]
    fn repr_basics() {
        let mut arena = Arena::new();
        assert_eq!(NanValue::new_int_inline(42).repr(&arena), "42");
        assert_eq!(NanValue::new_float(3.14).repr(&arena), "3.14");
        assert_eq!(NanValue::TRUE.repr(&arena), "true");
        assert_eq!(NanValue::UNIT.repr(&arena), "Unit");
        assert_eq!(NanValue::NONE.repr(&arena), "Option.None");

        let s = NanValue::new_string(arena.push_string("hi"));
        assert_eq!(s.repr(&arena), "hi");

        let ok_idx = arena.push_boxed(NanValue::new_int_inline(1));
        let ok = NanValue::new_ok(ok_idx);
        assert_eq!(ok.repr(&arena), "Result.Ok(1)");
    }

    #[test]
    fn value_roundtrip_primitives() {
        use crate::value::Value;
        let mut arena = Arena::new();

        let cases: Vec<Value> = vec![
            Value::Int(42),
            Value::Int(-1),
            Value::Int(i64::MAX),
            Value::Float(3.14),
            Value::Bool(true),
            Value::Bool(false),
            Value::Unit,
            Value::None,
            Value::Str("hello".to_string()),
            Value::Ok(Box::new(Value::Int(1))),
            Value::Err(Box::new(Value::Str("bad".to_string()))),
            Value::Some(Box::new(Value::Bool(true))),
        ];

        for val in &cases {
            let nv = NanValue::from_value(val, &mut arena);
            let back = nv.to_value(&arena);
            assert_eq!(
                format!("{:?}", val),
                format!("{:?}", back),
                "roundtrip failed for {:?}",
                val
            );
        }
    }
}
