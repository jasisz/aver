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
//!   0  = Immediate       payload 0-2: false/true/unit
//!   1  = Symbol          payload bits 0-1: fn/builtin/namespace/nullary-variant; rest=symbol index
//!   2  = Int             payload bit45: 0=inline(45-bit signed), 1=arena index
//!   3  = String          payload bit45: 0=inline small string (len + 5 bytes), 1=arena index
//!   4  = Some            payload bit45: 0=inline inner, 1=arena index
//!   5  = None            singleton
//!   6  = Ok              payload bit45: 0=inline inner, 1=arena index
//!   7  = Err             payload bit45: 0=inline inner, 1=arena index
//!   8  = List            payload bit45: 0=empty list, 1=arena index
//!   9  = Tuple           payload bit45: 1=arena index
//!   10 = Map             payload bit45: 0=empty map, 1=arena index
//!   11 = Record          payload bit45: 1=arena index
//!   12 = Variant         payload bit45: 1=arena index
//!   13-15 = (reserved)

use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
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

const TAG_IMMEDIATE: u64 = 0;
const TAG_SYMBOL: u64 = 1;
const TAG_INT: u64 = 2;
const TAG_STRING: u64 = 3;
const TAG_SOME: u64 = 4;
const TAG_NONE: u64 = 5;
const TAG_OK: u64 = 6;
const TAG_ERR: u64 = 7;
const TAG_LIST: u64 = 8;
const TAG_TUPLE: u64 = 9;
const TAG_MAP: u64 = 10;
const TAG_RECORD: u64 = 11;
const TAG_VARIANT: u64 = 12;

const SYMBOL_FN: u64 = 0;
const SYMBOL_BUILTIN: u64 = 1;
const SYMBOL_NAMESPACE: u64 = 2;
const SYMBOL_NULLARY_VARIANT: u64 = 3;
const SYMBOL_KIND_MASK: u64 = 0b11;

const IMM_FALSE: u64 = 0;
const IMM_TRUE: u64 = 1;
const IMM_UNIT: u64 = 2;

const WRAP_SOME: u64 = 0;
const WRAP_OK: u64 = 1;
const WRAP_ERR: u64 = 2;
const WRAPPER_INLINE_KIND_SHIFT: u32 = 43;
const WRAPPER_INLINE_KIND_MASK: u64 = 0b11 << WRAPPER_INLINE_KIND_SHIFT;
const WRAPPER_INLINE_PAYLOAD_MASK: u64 = (1u64 << WRAPPER_INLINE_KIND_SHIFT) - 1;
const WRAPPER_INLINE_IMMEDIATE: u64 = 0;
const WRAPPER_INLINE_INT: u64 = 1;
const WRAPPER_INLINE_NONE: u64 = 2;
const WRAPPER_INT_INLINE_MASK: u64 = WRAPPER_INLINE_PAYLOAD_MASK;
const WRAPPER_INT_INLINE_MAX: i64 = (1i64 << 42) - 1;
const WRAPPER_INT_INLINE_MIN: i64 = -(1i64 << 42);

const ARENA_REF_BIT: u64 = 1u64 << 45;
const INT_BIG_BIT: u64 = ARENA_REF_BIT;
const INT_INLINE_MASK: u64 = (1u64 << 45) - 1;
const INT_INLINE_MAX: i64 = (1i64 << 44) - 1;
const INT_INLINE_MIN: i64 = -(1i64 << 44);

const STRING_ARENA_BIT: u64 = ARENA_REF_BIT;
const STRING_INLINE_LEN_SHIFT: u32 = 40;
const STRING_INLINE_LEN_MASK: u64 = 0b111 << STRING_INLINE_LEN_SHIFT;
const STRING_INLINE_MAX_BYTES: usize = 5;

// ---------------------------------------------------------------------------
// NanValue - the 8-byte compact value
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
pub struct NanValue(u64);

#[derive(Clone, Copy, Debug)]
pub enum NanString<'a> {
    Borrowed(&'a str),
    Inline {
        len: u8,
        bytes: [u8; STRING_INLINE_MAX_BYTES],
    },
}

impl<'a> NanString<'a> {
    #[inline]
    pub fn as_str(&self) -> &str {
        match self {
            NanString::Borrowed(s) => s,
            NanString::Inline { len, bytes } => std::str::from_utf8(&bytes[..*len as usize])
                .expect("NanString inline payload must be valid UTF-8"),
        }
    }
}

impl Deref for NanString<'_> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl PartialEq for NanString<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for NanString<'_> {}

impl PartialEq<&str> for NanString<'_> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<NanString<'_>> for &str {
    #[inline]
    fn eq(&self, other: &NanString<'_>) -> bool {
        *self == other.as_str()
    }
}

impl PartialOrd for NanString<'_> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NanString<'_> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Hash for NanString<'_> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl std::fmt::Display for NanString<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

// -- Encoding / decoding ---------------------------------------------------

impl NanValue {
    #[inline]
    fn decode_inline_int_payload(payload: u64) -> i64 {
        debug_assert!(payload & INT_BIG_BIT == 0);
        let raw = payload & INT_INLINE_MASK;
        if raw & (1u64 << 44) != 0 {
            (raw | !INT_INLINE_MASK) as i64
        } else {
            raw as i64
        }
    }

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
            Self::decode_inline_int_payload(p)
        }
    }

    #[inline]
    fn inline_int_payload(self) -> Option<u64> {
        (self.is_nan_boxed() && self.tag() == TAG_INT && self.payload() & INT_BIG_BIT == 0)
            .then_some(self.payload())
    }

    #[inline]
    pub fn inline_int_value(self) -> Option<i64> {
        self.inline_int_payload()
            .map(Self::decode_inline_int_payload)
    }

    // -- Immediates --------------------------------------------------------

    pub const FALSE: NanValue = NanValue(QNAN | (TAG_IMMEDIATE << TAG_SHIFT) | IMM_FALSE);
    pub const TRUE: NanValue = NanValue(QNAN | (TAG_IMMEDIATE << TAG_SHIFT) | IMM_TRUE);
    pub const UNIT: NanValue = NanValue(QNAN | (TAG_IMMEDIATE << TAG_SHIFT) | IMM_UNIT);
    pub const NONE: NanValue = NanValue(QNAN | (TAG_NONE << TAG_SHIFT));
    pub const EMPTY_LIST: NanValue = NanValue(QNAN | (TAG_LIST << TAG_SHIFT));
    pub const EMPTY_MAP: NanValue = NanValue(QNAN | (TAG_MAP << TAG_SHIFT));
    pub const EMPTY_STRING: NanValue = NanValue(QNAN | (TAG_STRING << TAG_SHIFT));

    #[inline]
    pub fn new_bool(b: bool) -> Self {
        if b { Self::TRUE } else { Self::FALSE }
    }

    #[inline]
    pub fn as_bool(self) -> bool {
        self.0 == Self::TRUE.0
    }

    #[inline]
    fn plain_immediate_payload(self) -> Option<u64> {
        (self.is_nan_boxed() && self.tag() == TAG_IMMEDIATE && self.payload() <= IMM_UNIT)
            .then_some(self.payload())
    }

    #[inline]
    fn wrapper_kind(self) -> u64 {
        match self.tag() {
            TAG_SOME => WRAP_SOME,
            TAG_OK => WRAP_OK,
            TAG_ERR => WRAP_ERR,
            _ => panic!("wrapper_kind() called on non-wrapper"),
        }
    }

    #[inline]
    fn wrapper_inline_kind(self) -> Option<u64> {
        if !self.is_nan_boxed() {
            return None;
        }
        match self.tag() {
            TAG_SOME | TAG_OK | TAG_ERR if self.payload() & ARENA_REF_BIT == 0 => {
                Some((self.payload() & WRAPPER_INLINE_KIND_MASK) >> WRAPPER_INLINE_KIND_SHIFT)
            }
            _ => None,
        }
    }

    #[inline]
    fn decode_wrapper_inline_int_payload(payload: u64) -> i64 {
        let raw = payload & WRAPPER_INT_INLINE_MASK;
        if raw & (1u64 << 42) != 0 {
            (raw | !WRAPPER_INT_INLINE_MASK) as i64
        } else {
            raw as i64
        }
    }

    #[inline]
    fn encode_wrapper_inline_int(i: i64) -> u64 {
        debug_assert!((WRAPPER_INT_INLINE_MIN..=WRAPPER_INT_INLINE_MAX).contains(&i));
        (i as u64) & WRAPPER_INT_INLINE_MASK
    }

    #[inline]
    fn wrapper_inline_inner(self) -> Option<NanValue> {
        let kind = self.wrapper_inline_kind()?;
        let payload = self.payload() & WRAPPER_INLINE_PAYLOAD_MASK;
        match kind {
            WRAPPER_INLINE_IMMEDIATE => Some(Self::encode(TAG_IMMEDIATE, payload)),
            WRAPPER_INLINE_INT => Some(Self::new_int_inline(
                Self::decode_wrapper_inline_int_payload(payload),
            )),
            WRAPPER_INLINE_NONE => Some(Self::NONE),
            _ => None,
        }
    }

    #[inline]
    fn new_inline_wrapper(tag: u64, inline_kind: u64, payload: u64) -> Self {
        debug_assert!(matches!(tag, TAG_SOME | TAG_OK | TAG_ERR));
        debug_assert!(inline_kind <= WRAPPER_INLINE_NONE);
        debug_assert!(payload <= WRAPPER_INLINE_PAYLOAD_MASK);
        Self::encode(tag, (inline_kind << WRAPPER_INLINE_KIND_SHIFT) | payload)
    }

    #[inline]
    fn wrapper_parts(self, arena: &Arena) -> Option<(u64, NanValue)> {
        if !self.is_nan_boxed() {
            return None;
        }
        match self.tag() {
            TAG_SOME | TAG_OK | TAG_ERR if self.payload() & ARENA_REF_BIT != 0 => {
                Some((self.wrapper_kind(), arena.get_boxed(self.arena_index())))
            }
            TAG_SOME | TAG_OK | TAG_ERR => self
                .wrapper_inline_inner()
                .map(|inner| (self.wrapper_kind(), inner)),
            _ => None,
        }
    }

    // -- Wrappers (Some/Ok/Err) -------------------------------------------

    #[inline]
    pub fn new_some(inner_index: u32) -> Self {
        Self::encode(TAG_SOME, ARENA_REF_BIT | (inner_index as u64))
    }

    #[inline]
    pub fn new_ok(inner_index: u32) -> Self {
        Self::encode(TAG_OK, ARENA_REF_BIT | (inner_index as u64))
    }

    #[inline]
    pub fn new_err(inner_index: u32) -> Self {
        Self::encode(TAG_ERR, ARENA_REF_BIT | (inner_index as u64))
    }

    #[inline]
    pub fn wrapper_index(self) -> u32 {
        debug_assert!(
            self.is_nan_boxed()
                && matches!(self.tag(), TAG_SOME | TAG_OK | TAG_ERR)
                && self.payload() & ARENA_REF_BIT != 0
        );
        self.arena_index()
    }

    #[inline]
    pub fn wrapper_inner(self, arena: &Arena) -> NanValue {
        self.wrapper_parts(arena)
            .map(|(_, inner)| inner)
            .expect("wrapper_inner() called on non-wrapper")
    }

    #[inline]
    fn wrap_value(kind: u64, inner: NanValue, arena: &mut Arena) -> Self {
        if let Some(payload) = inner.plain_immediate_payload() {
            let tag = match kind {
                WRAP_SOME => TAG_SOME,
                WRAP_OK => TAG_OK,
                WRAP_ERR => TAG_ERR,
                _ => unreachable!("invalid wrapper kind"),
            };
            Self::new_inline_wrapper(tag, WRAPPER_INLINE_IMMEDIATE, payload)
        } else if inner.is_none() {
            let tag = match kind {
                WRAP_SOME => TAG_SOME,
                WRAP_OK => TAG_OK,
                WRAP_ERR => TAG_ERR,
                _ => unreachable!("invalid wrapper kind"),
            };
            Self::new_inline_wrapper(tag, WRAPPER_INLINE_NONE, 0)
        } else if let Some(value) = inner.inline_int_value() {
            if (WRAPPER_INT_INLINE_MIN..=WRAPPER_INT_INLINE_MAX).contains(&value) {
                let tag = match kind {
                    WRAP_SOME => TAG_SOME,
                    WRAP_OK => TAG_OK,
                    WRAP_ERR => TAG_ERR,
                    _ => unreachable!("invalid wrapper kind"),
                };
                return Self::new_inline_wrapper(
                    tag,
                    WRAPPER_INLINE_INT,
                    Self::encode_wrapper_inline_int(value),
                );
            }
            let idx = arena.push_boxed(inner);
            match kind {
                WRAP_SOME => Self::new_some(idx),
                WRAP_OK => Self::new_ok(idx),
                WRAP_ERR => Self::new_err(idx),
                _ => unreachable!("invalid wrapper kind"),
            }
        } else {
            let idx = arena.push_boxed(inner);
            match kind {
                WRAP_SOME => Self::new_some(idx),
                WRAP_OK => Self::new_ok(idx),
                WRAP_ERR => Self::new_err(idx),
                _ => unreachable!("invalid wrapper kind"),
            }
        }
    }

    #[inline]
    pub fn new_some_value(inner: NanValue, arena: &mut Arena) -> Self {
        Self::wrap_value(WRAP_SOME, inner, arena)
    }

    #[inline]
    pub fn new_ok_value(inner: NanValue, arena: &mut Arena) -> Self {
        Self::wrap_value(WRAP_OK, inner, arena)
    }

    #[inline]
    pub fn new_err_value(inner: NanValue, arena: &mut Arena) -> Self {
        Self::wrap_value(WRAP_ERR, inner, arena)
    }

    // -- Arena-backed constructors -----------------------------------------

    #[inline]
    pub fn new_string(arena_index: u32) -> Self {
        Self::encode(TAG_STRING, STRING_ARENA_BIT | (arena_index as u64))
    }

    #[inline]
    fn new_small_string_bytes(bytes: &[u8]) -> Self {
        debug_assert!(bytes.len() <= STRING_INLINE_MAX_BYTES);
        let mut payload = (bytes.len() as u64) << STRING_INLINE_LEN_SHIFT;
        for (idx, byte) in bytes.iter().enumerate() {
            payload |= (*byte as u64) << (idx * 8);
        }
        Self::encode(TAG_STRING, payload)
    }

    #[inline]
    pub(crate) fn small_string(self) -> Option<NanString<'static>> {
        if !self.is_nan_boxed()
            || self.tag() != TAG_STRING
            || self.payload() & STRING_ARENA_BIT != 0
        {
            return None;
        }
        let payload = self.payload();
        let len = ((payload & STRING_INLINE_LEN_MASK) >> STRING_INLINE_LEN_SHIFT) as u8;
        if len as usize > STRING_INLINE_MAX_BYTES {
            return None;
        }
        let mut bytes = [0u8; STRING_INLINE_MAX_BYTES];
        for (idx, slot) in bytes.iter_mut().take(len as usize).enumerate() {
            *slot = ((payload >> (idx * 8)) & 0xFF) as u8;
        }
        Some(NanString::Inline { len, bytes })
    }

    #[inline]
    pub fn new_string_value(s: &str, arena: &mut Arena) -> Self {
        if s.len() <= STRING_INLINE_MAX_BYTES {
            Self::new_small_string_bytes(s.as_bytes())
        } else {
            Self::new_string(arena.push_string(s))
        }
    }

    #[inline]
    pub fn new_list(arena_index: u32) -> Self {
        Self::encode(TAG_LIST, ARENA_REF_BIT | (arena_index as u64))
    }

    #[inline]
    pub fn new_tuple(arena_index: u32) -> Self {
        Self::encode(TAG_TUPLE, ARENA_REF_BIT | (arena_index as u64))
    }

    #[inline]
    pub fn new_map(arena_index: u32) -> Self {
        Self::encode(TAG_MAP, ARENA_REF_BIT | (arena_index as u64))
    }

    #[inline]
    pub fn new_record(arena_index: u32) -> Self {
        Self::encode(TAG_RECORD, ARENA_REF_BIT | (arena_index as u64))
    }

    #[inline]
    pub fn new_variant(arena_index: u32) -> Self {
        Self::encode(TAG_VARIANT, ARENA_REF_BIT | (arena_index as u64))
    }

    #[inline]
    fn new_symbol(symbol_kind: u64, symbol_index: u32) -> Self {
        Self::encode(TAG_SYMBOL, symbol_kind | ((symbol_index as u64) << 2))
    }

    #[inline]
    pub(crate) fn symbol_kind(self) -> u64 {
        debug_assert!(self.is_nan_boxed() && self.tag() == TAG_SYMBOL);
        self.payload() & SYMBOL_KIND_MASK
    }

    #[inline]
    pub(crate) fn symbol_index(self) -> u32 {
        debug_assert!(self.is_nan_boxed() && self.tag() == TAG_SYMBOL);
        (self.payload() >> 2) as u32
    }

    #[inline]
    pub fn new_nullary_variant(symbol_index: u32) -> Self {
        Self::new_symbol(SYMBOL_NULLARY_VARIANT, symbol_index)
    }

    #[inline]
    pub fn new_fn(arena_index: u32) -> Self {
        Self::new_symbol(SYMBOL_FN, arena_index)
    }

    #[inline]
    pub fn new_builtin(arena_index: u32) -> Self {
        Self::new_symbol(SYMBOL_BUILTIN, arena_index)
    }

    #[inline]
    pub fn new_namespace(arena_index: u32) -> Self {
        Self::new_symbol(SYMBOL_NAMESPACE, arena_index)
    }

    #[inline]
    pub fn arena_index(self) -> u32 {
        (self.payload() & !ARENA_REF_BIT) as u32
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
            TAG_STRING | TAG_SOME | TAG_OK | TAG_ERR | TAG_LIST | TAG_TUPLE | TAG_MAP
            | TAG_RECORD | TAG_VARIANT => {
                (self.payload() & ARENA_REF_BIT != 0).then_some(self.arena_index())
            }
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
            TAG_SOME => Self::new_some(index),
            TAG_OK => Self::new_ok(index),
            TAG_ERR => Self::new_err(index),
            TAG_STRING => Self::new_string(index),
            TAG_LIST => Self::new_list(index),
            TAG_TUPLE => Self::new_tuple(index),
            TAG_MAP => Self::new_map(index),
            TAG_RECORD => Self::new_record(index),
            TAG_VARIANT => Self::new_variant(index),
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
        self.is_nan_boxed() && self.tag() == TAG_SOME
    }

    #[inline]
    pub fn is_ok(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_OK
    }

    #[inline]
    pub fn is_err(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_ERR
    }

    #[inline]
    pub fn is_string(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_STRING
    }

    /// Deep string equality: compare actual string content, not NanValue bits.
    /// Handles both inline short strings and arena-allocated strings.
    pub fn string_eq(self, other: NanValue, arena: &Arena) -> bool {
        if self.bits() == other.bits() {
            return true; // fast path: same bits (inline or same arena entry)
        }
        if !self.is_string() || !other.is_string() {
            return false;
        }
        arena.get_string_value(self).as_str() == arena.get_string_value(other).as_str()
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
        self.is_nan_boxed() && self.tag() == TAG_SYMBOL && self.symbol_kind() == SYMBOL_FN
    }

    #[inline]
    pub fn is_variant(self) -> bool {
        self.is_nan_boxed()
            && (self.tag() == TAG_VARIANT
                || (self.tag() == TAG_SYMBOL && self.symbol_kind() == SYMBOL_NULLARY_VARIANT))
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
        self.is_nan_boxed() && self.tag() == TAG_SYMBOL && self.symbol_kind() == SYMBOL_BUILTIN
    }

    #[inline]
    pub fn is_namespace(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_SYMBOL && self.symbol_kind() == SYMBOL_NAMESPACE
    }

    #[inline]
    pub fn is_empty_list_immediate(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_LIST && self.payload() & ARENA_REF_BIT == 0
    }

    #[inline]
    pub fn is_empty_map_immediate(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_MAP && self.payload() & ARENA_REF_BIT == 0
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
                _ => "Unknown",
            },
            TAG_SOME => "Option.Some",
            TAG_NONE => "Option.None",
            TAG_OK => "Result.Ok",
            TAG_ERR => "Result.Err",
            TAG_STRING => "String",
            TAG_LIST => "List",
            TAG_TUPLE => "Tuple",
            TAG_MAP => "Map",
            TAG_RECORD => "Record",
            TAG_VARIANT => "Variant",
            TAG_SYMBOL => match self.symbol_kind() {
                SYMBOL_FN => "Fn",
                SYMBOL_BUILTIN => "Builtin",
                SYMBOL_NAMESPACE => "Namespace",
                SYMBOL_NULLARY_VARIANT => "Variant",
                _ => "Unknown",
            },
            _ => "Unknown",
        }
    }

    #[inline]
    pub fn variant_ctor_id(self, arena: &Arena) -> Option<u32> {
        if !self.is_nan_boxed() {
            return None;
        }
        match self.tag() {
            TAG_VARIANT => {
                let (type_id, variant_id, _) = arena.get_variant(self.arena_index());
                arena.find_ctor_id(type_id, variant_id)
            }
            TAG_SYMBOL if self.symbol_kind() == SYMBOL_NULLARY_VARIANT => {
                Some(arena.get_nullary_variant_ctor(self.symbol_index()))
            }
            _ => None,
        }
    }

    #[inline]
    pub fn variant_parts(self, arena: &Arena) -> Option<(u32, u16, &[NanValue])> {
        if !self.is_nan_boxed() {
            return None;
        }
        match self.tag() {
            TAG_VARIANT => {
                let (type_id, variant_id, fields) = arena.get_variant(self.arena_index());
                Some((type_id, variant_id, fields))
            }
            TAG_SYMBOL if self.symbol_kind() == SYMBOL_NULLARY_VARIANT => {
                let (type_id, variant_id) =
                    arena.get_ctor_parts(arena.get_nullary_variant_ctor(self.symbol_index()));
                Some((type_id, variant_id, &[]))
            }
            _ => None,
        }
    }

    /// Raw bits - useful for using as HashMap key (inline values only).
    #[inline]
    pub fn bits(self) -> u64 {
        self.0
    }

    #[inline]
    pub fn from_bits(bits: u64) -> Self {
        NanValue(bits)
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
            arena.get_string_value(self).hash(&mut hasher);
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
                    write!(
                        f,
                        "Int({})",
                        Self::decode_inline_int_payload(self.payload())
                    )
                }
            }
            TAG_IMMEDIATE => match self.payload() {
                IMM_FALSE => write!(f, "False"),
                IMM_TRUE => write!(f, "True"),
                IMM_UNIT => write!(f, "Unit"),
                _ => write!(f, "Immediate({})", self.payload()),
            },
            TAG_NONE => write!(f, "None"),
            TAG_SOME | TAG_OK | TAG_ERR => {
                let kind = match self.tag() {
                    TAG_SOME => "Some",
                    TAG_OK => "Ok",
                    TAG_ERR => "Err",
                    _ => "?",
                };
                if self.payload() & ARENA_REF_BIT != 0 {
                    write!(f, "{}(arena:{})", kind, self.arena_index())
                } else if let Some(inner) = self.wrapper_inline_inner() {
                    write!(f, "{}({:?})", kind, inner)
                } else {
                    write!(f, "{}(?)", kind)
                }
            }
            TAG_SYMBOL => match self.symbol_kind() {
                SYMBOL_FN => write!(f, "Fn(symbol:{})", self.symbol_index()),
                SYMBOL_BUILTIN => write!(f, "Builtin(symbol:{})", self.symbol_index()),
                SYMBOL_NAMESPACE => write!(f, "Namespace(symbol:{})", self.symbol_index()),
                SYMBOL_NULLARY_VARIANT => {
                    write!(f, "NullaryVariant(symbol:{})", self.symbol_index())
                }
                _ => write!(f, "Symbol({})", self.payload()),
            },
            TAG_STRING => {
                if let Some(s) = self.small_string() {
                    write!(f, "String({:?})", s.as_str())
                } else {
                    write!(f, "String(arena:{})", self.arena_index())
                }
            }
            TAG_LIST if self.is_empty_list_immediate() => write!(f, "EmptyList"),
            TAG_MAP if self.is_empty_map_immediate() => write!(f, "EmptyMap"),
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
    pub(crate) type_variant_ctor_ids: Vec<Vec<u32>>,
    pub(crate) ctor_to_type_variant: Vec<(u32, u16)>,
    pub(crate) symbol_entries: Vec<ArenaSymbol>,
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
pub enum ArenaSymbol {
    Fn(Rc<FunctionValue>),
    Builtin(Rc<str>),
    Namespace {
        name: Rc<str>,
        members: Vec<(Rc<str>, NanValue)>,
    },
    NullaryVariant {
        ctor_id: u32,
    },
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
