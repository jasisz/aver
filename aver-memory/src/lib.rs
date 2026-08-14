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
//! +--------+------+------------------------+
//! | 0x7FFC | tag  |       payload          |
//! | 14 bits| 4 bit|       46 bits          |
//! +--------+------+------------------------+
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
//!   9  = Vector          payload bit45: 0=empty vector, 1=arena index
//!   10 = Map             payload bit45: 0=empty map, 1=arena index
//!   11 = Record          payload bit45: 1=arena index
//!   12 = Variant         payload bit45: 1=arena index
//!   13 = Tuple           payload bit45: 1=arena index
//!   14 = InlineVariant  [45:30]=ctor_id, [29]=kind(0=int,1=imm), [28:0]=value
//!   15 = (reserved)

#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use alloc::format;
use alloc::string::String;
use alloc::sync::Arc as Rc;
use alloc::vec::Vec;
use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use core::ops::Deref;

// ---------------------------------------------------------------------------
// ArenaTypes trait — parameterises the arena over consumer-specific types
// ---------------------------------------------------------------------------

/// Trait that defines the function and map types used by the arena.
///
/// The arena stores function values and persistent maps, but their concrete
/// types depend on the consumer (VM, WASM runtime, codegen, etc.).
pub trait ArenaTypes: Clone + core::fmt::Debug + 'static {
    /// The function value type (e.g. `FunctionValue` in the VM).
    type Fn: Clone + core::fmt::Debug + FnValueName;
    /// The persistent map type (e.g. `AverMap<u64, (NanValue, NanValue)>`).
    type Map: Clone + core::fmt::Debug + MapLike;
}

/// Trait for extracting a display name from a function value.
pub trait FnValueName {
    fn name(&self) -> &str;
}

/// Trait abstracting the persistent-map operations needed by the arena.
///
/// Implementors provide a hash-keyed map from `u64` to `(NanValue, NanValue)`.
pub trait MapLike: Sized {
    fn new() -> Self;
    fn get(&self, key: &u64) -> Option<&(NanValue, NanValue)>;
    fn insert(&self, key: u64, value: (NanValue, NanValue)) -> Self;
    /// Insert with owned self — avoids clone when sole owner.
    fn insert_owned(self, key: u64, value: (NanValue, NanValue)) -> Self {
        self.insert(key, value) // default: fall back to &self version
    }
    /// Rewrite NanValue pairs in place — avoids rebuilding the hash table.
    /// Uses copy-on-write: O(1) when sole owner, O(n) clone when shared.
    fn rewrite_values_mut(&mut self, f: impl FnMut(&mut (NanValue, NanValue)));
    /// Identity of the backing table, for callers that need to tell an in-place
    /// update from a copy-on-write duplication: a value that changes across an
    /// update means the whole table was rebuilt. The default answers "cannot
    /// tell", which is what an implementation without a shared table has to
    /// say; such a build has no VM to count duplication for.
    fn table_id(&self) -> usize {
        0
    }
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    fn iter(&self) -> impl Iterator<Item = (&u64, &(NanValue, NanValue))>;
    fn values(&self) -> impl Iterator<Item = &(NanValue, NanValue)>;
}

/// Whether every key and every value in `map` is [`NanValue::is_immediate`] —
/// the proof behind the `all_immediate` flag on [`ArenaEntry::Map`].
///
/// This reads the whole table, so it belongs to builders that have no
/// predecessor flag to derive from. A builder that grows a map it already has
/// the flag for must not call this: doing that once per insert would put back
/// the per-step walk the flag exists to remove.
pub fn map_all_immediate<M: MapLike>(map: &M) -> bool {
    map.values()
        .all(|(key, value)| key.is_immediate() && value.is_immediate())
}

// ---------------------------------------------------------------------------
// Bit layout constants
// ---------------------------------------------------------------------------

const QNAN: u64 = 0x7FFC_0000_0000_0000;
const QNAN_MASK: u64 = 0xFFFC_0000_0000_0000;
const QNAN_MARKER_BIT: u64 = 1u64 << 50;
const TAG_SHIFT: u32 = 46;
const TAG_MASK: u64 = 0xF;
const PAYLOAD_MASK: u64 = (1u64 << 46) - 1;

pub const TAG_IMMEDIATE: u64 = 0;
pub const TAG_SYMBOL: u64 = 1;
pub const TAG_INT: u64 = 2;
pub const TAG_STRING: u64 = 3;
pub const TAG_SOME: u64 = 4;
pub const TAG_NONE: u64 = 5;
pub const TAG_OK: u64 = 6;
pub const TAG_ERR: u64 = 7;
pub const TAG_LIST: u64 = 8;
pub const TAG_VECTOR: u64 = 9;
pub const TAG_MAP: u64 = 10;
pub const TAG_RECORD: u64 = 11;
pub const TAG_VARIANT: u64 = 12;
pub const TAG_TUPLE: u64 = 13;
pub const TAG_INLINE_VARIANT: u64 = 14;

pub const SYMBOL_FN: u64 = 0;
pub const SYMBOL_BUILTIN: u64 = 1;
pub const SYMBOL_NAMESPACE: u64 = 2;
pub const SYMBOL_NULLARY_VARIANT: u64 = 3;
const SYMBOL_KIND_MASK: u64 = 0b11;

pub const IMM_FALSE: u64 = 0;
pub const IMM_TRUE: u64 = 1;
pub const IMM_UNIT: u64 = 2;

pub const WRAP_SOME: u64 = 0;
pub const WRAP_OK: u64 = 1;
pub const WRAP_ERR: u64 = 2;
const WRAPPER_INLINE_KIND_SHIFT: u32 = 43;
const WRAPPER_INLINE_KIND_MASK: u64 = 0b11 << WRAPPER_INLINE_KIND_SHIFT;
const WRAPPER_INLINE_PAYLOAD_MASK: u64 = (1u64 << WRAPPER_INLINE_KIND_SHIFT) - 1;
const WRAPPER_INLINE_IMMEDIATE: u64 = 0;
const WRAPPER_INLINE_INT: u64 = 1;
const WRAPPER_INLINE_NONE: u64 = 2;
const WRAPPER_INT_INLINE_MASK: u64 = WRAPPER_INLINE_PAYLOAD_MASK;
const WRAPPER_INT_INLINE_MAX: i64 = (1i64 << 42) - 1;
const WRAPPER_INT_INLINE_MIN: i64 = -(1i64 << 42);

pub const ARENA_REF_BIT: u64 = 1u64 << 45;
const INT_BIG_BIT: u64 = ARENA_REF_BIT;
const INT_INLINE_MASK: u64 = (1u64 << 45) - 1;
pub const INT_INLINE_MAX: i64 = (1i64 << 44) - 1;
pub const INT_INLINE_MIN: i64 = -(1i64 << 44);

const STRING_ARENA_BIT: u64 = ARENA_REF_BIT;
const STRING_INLINE_LEN_SHIFT: u32 = 40;
const STRING_INLINE_LEN_MASK: u64 = 0b111 << STRING_INLINE_LEN_SHIFT;
const STRING_INLINE_MAX_BYTES: usize = 5;

// -- Inline variant layout --------------------------------------------------
const IV_CTOR_SHIFT: u32 = 30;
const IV_CTOR_MASK: u64 = 0xFFFF;
const IV_KIND_BIT: u64 = 1 << 29;
const IV_INT_MASK: u64 = (1u64 << 29) - 1;
const IV_INT_SIGN_BIT: u64 = 1u64 << 28;
const IV_INT_MAX: i64 = (1i64 << 28) - 1;
const IV_INT_MIN: i64 = -(1i64 << 28);
const IV_IMM_SHIFT: u32 = 27;
const IV_IMM_FALSE: u64 = 0;
const IV_IMM_TRUE: u64 = 1;
const IV_IMM_UNIT: u64 = 2;
const IV_IMM_NONE: u64 = 3;

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
            NanString::Inline { len, bytes } => core::str::from_utf8(&bytes[..*len as usize])
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

impl core::fmt::Display for NanString<'_> {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
    pub fn encode(tag: u64, payload: u64) -> Self {
        debug_assert!(tag <= TAG_MASK);
        debug_assert!(payload <= PAYLOAD_MASK);
        NanValue(QNAN | (tag << TAG_SHIFT) | payload)
    }

    #[inline]
    pub fn is_nan_boxed(self) -> bool {
        (self.0 & QNAN_MASK) == QNAN
    }

    #[inline]
    pub fn tag(self) -> u64 {
        (self.0 >> TAG_SHIFT) & TAG_MASK
    }

    #[inline]
    pub fn payload(self) -> u64 {
        self.0 & PAYLOAD_MASK
    }

    // -- Constructors ------------------------------------------------------

    #[inline]
    pub fn new_float(f: f64) -> Self {
        let bits = f.to_bits();
        if (bits & QNAN_MASK) == QNAN {
            // A colliding pattern is already a quiet NaN. Clear only Aver's
            // marker bit: the result remains a quiet NaN, retains its sign and
            // remaining payload, and can no longer be mistaken for a box.
            NanValue(bits & !QNAN_MARKER_BIT)
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
    pub fn new_int<T: ArenaTypes>(i: i64, arena: &mut Arena<T>) -> Self {
        if (INT_INLINE_MIN..=INT_INLINE_MAX).contains(&i) {
            Self::new_int_inline(i)
        } else {
            let idx = arena.push_i64(i);
            Self::new_int_arena(idx)
        }
    }

    /// Store an arbitrary-precision integer and return its NaN-box. This is a
    /// canonical-form boundary: a payload that fits `i64` is demoted to the
    /// inline / `i64`-overflow representation rather than boxed, so a
    /// numerically-`Small` value can never enter the value space wearing a
    /// `BigInt` slot (which would break `Eq`/`Ord`/`Hash` and Map/Set keying).
    /// Only a genuinely out-of-`i64`-range value allocates a `BigInt` slot.
    #[inline]
    pub fn new_big_int<T: ArenaTypes>(value: num_bigint::BigInt, arena: &mut Arena<T>) -> Self {
        match i64::try_from(&value) {
            Ok(n) => Self::new_int(n, arena),
            Err(_) => Self::new_int_arena(arena.push_bigint(value)),
        }
    }

    /// Materialize the stored value as `i64`. Valid only for inline ints and
    /// the `i64`-overflow arena slot; panics on a ℤ-overflow (`BigInt`) slot,
    /// which cannot be represented. Use `int_ref` when the value may be Big.
    #[inline]
    pub fn as_int<T: ArenaTypes>(self, arena: &Arena<T>) -> i64 {
        let p = self.payload();
        if p & INT_BIG_BIT != 0 {
            let idx = (p & !INT_BIG_BIT) as u32;
            arena.get_i64(idx)
        } else {
            Self::decode_inline_int_payload(p)
        }
    }

    /// Borrow the stored integer, discriminating inline / `i64`-overflow /
    /// ℤ-overflow without losing precision. The runtime crate maps this to a
    /// canonical `AverInt`.
    #[inline]
    pub fn int_ref<T: ArenaTypes>(self, arena: &Arena<T>) -> ArenaIntRef<'_> {
        let p = self.payload();
        if p & INT_BIG_BIT != 0 {
            arena.int_ref_at((p & !INT_BIG_BIT) as u32)
        } else {
            ArenaIntRef::Small(Self::decode_inline_int_payload(p))
        }
    }

    #[inline]
    pub fn inline_int_payload(self) -> Option<u64> {
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
    pub const EMPTY_VECTOR: NanValue = NanValue(QNAN | (TAG_VECTOR << TAG_SHIFT));
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
    pub fn plain_immediate_payload(self) -> Option<u64> {
        (self.is_nan_boxed() && self.tag() == TAG_IMMEDIATE && self.payload() <= IMM_UNIT)
            .then_some(self.payload())
    }

    #[inline]
    pub fn wrapper_kind(self) -> u64 {
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
    pub fn wrapper_inline_inner(self) -> Option<NanValue> {
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
    pub fn wrapper_parts<T: ArenaTypes>(self, arena: &Arena<T>) -> Option<(u64, NanValue)> {
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
    pub fn wrapper_inner<T: ArenaTypes>(self, arena: &Arena<T>) -> NanValue {
        self.wrapper_parts(arena)
            .map(|(_, inner)| inner)
            .expect("wrapper_inner() called on non-wrapper")
    }

    #[inline]
    fn wrap_value<T: ArenaTypes>(kind: u64, inner: NanValue, arena: &mut Arena<T>) -> Self {
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
    pub fn new_some_value<T: ArenaTypes>(inner: NanValue, arena: &mut Arena<T>) -> Self {
        Self::wrap_value(WRAP_SOME, inner, arena)
    }

    #[inline]
    pub fn new_ok_value<T: ArenaTypes>(inner: NanValue, arena: &mut Arena<T>) -> Self {
        Self::wrap_value(WRAP_OK, inner, arena)
    }

    #[inline]
    pub fn new_err_value<T: ArenaTypes>(inner: NanValue, arena: &mut Arena<T>) -> Self {
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
    pub fn small_string(self) -> Option<NanString<'static>> {
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
    pub fn new_string_value<T: ArenaTypes>(s: &str, arena: &mut Arena<T>) -> Self {
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
    pub fn new_vector(arena_index: u32) -> Self {
        Self::encode(TAG_VECTOR, ARENA_REF_BIT | (arena_index as u64))
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
    pub fn symbol_kind(self) -> u64 {
        debug_assert!(self.is_nan_boxed() && self.tag() == TAG_SYMBOL);
        self.payload() & SYMBOL_KIND_MASK
    }

    #[inline]
    pub fn symbol_index(self) -> u32 {
        debug_assert!(self.is_nan_boxed() && self.tag() == TAG_SYMBOL);
        (self.payload() >> 2) as u32
    }

    #[inline]
    pub fn new_nullary_variant(symbol_index: u32) -> Self {
        Self::new_symbol(SYMBOL_NULLARY_VARIANT, symbol_index)
    }

    #[inline]
    pub fn try_new_inline_variant(ctor_id: u32, inner: NanValue) -> Option<Self> {
        if ctor_id > IV_CTOR_MASK as u32 {
            return None;
        }
        let ctor_bits = (ctor_id as u64) << IV_CTOR_SHIFT;

        if inner.is_nan_boxed() {
            match inner.tag() {
                TAG_INT if inner.payload() & INT_BIG_BIT == 0 => {
                    let i = Self::decode_inline_int_payload(inner.payload());
                    if (IV_INT_MIN..=IV_INT_MAX).contains(&i) {
                        let int_bits = (i as u64) & IV_INT_MASK;
                        return Some(Self::encode(TAG_INLINE_VARIANT, ctor_bits | int_bits));
                    }
                }
                TAG_IMMEDIATE => {
                    let imm = match inner.payload() {
                        IMM_FALSE => IV_IMM_FALSE,
                        IMM_TRUE => IV_IMM_TRUE,
                        IMM_UNIT => IV_IMM_UNIT,
                        _ => return None,
                    };
                    return Some(Self::encode(
                        TAG_INLINE_VARIANT,
                        ctor_bits | IV_KIND_BIT | (imm << IV_IMM_SHIFT),
                    ));
                }
                TAG_NONE => {
                    return Some(Self::encode(
                        TAG_INLINE_VARIANT,
                        ctor_bits | IV_KIND_BIT | (IV_IMM_NONE << IV_IMM_SHIFT),
                    ));
                }
                _ => {}
            }
        }
        None
    }

    #[inline]
    pub fn inline_variant_ctor_id(self) -> u32 {
        debug_assert!(self.is_nan_boxed() && self.tag() == TAG_INLINE_VARIANT);
        ((self.payload() >> IV_CTOR_SHIFT) & IV_CTOR_MASK) as u32
    }

    #[inline]
    pub fn inline_variant_inner(self) -> NanValue {
        debug_assert!(self.is_nan_boxed() && self.tag() == TAG_INLINE_VARIANT);
        let payload = self.payload();
        if payload & IV_KIND_BIT == 0 {
            let raw = payload & IV_INT_MASK;
            let i = if raw & IV_INT_SIGN_BIT != 0 {
                (raw | !IV_INT_MASK) as i64
            } else {
                raw as i64
            };
            Self::new_int_inline(i)
        } else {
            let imm = (payload >> IV_IMM_SHIFT) & 0b11;
            match imm {
                IV_IMM_FALSE => Self::FALSE,
                IV_IMM_TRUE => Self::TRUE,
                IV_IMM_UNIT => Self::UNIT,
                IV_IMM_NONE => Self::NONE,
                _ => unreachable!(),
            }
        }
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

    /// Whether the value carries no arena index at all, so no collection can
    /// ever relocate it and rewriting it is provably the identity.
    ///
    /// This is the single definition of "immediate" behind every escape the
    /// collector takes without reading what it is skipping —
    /// [`ListBody::all_immediate`] and the `all_immediate` flag on
    /// [`ArenaEntry::Map`] are both decided by it. A second definition that
    /// drifted from this one would let the collector skip something that does
    /// move, which is silent: the skipped value keeps pointing at where its
    /// contents used to be.
    #[inline]
    pub fn is_immediate(self) -> bool {
        self.heap_index().is_none()
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
            | TAG_RECORD | TAG_VARIANT | TAG_VECTOR => {
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
            TAG_VECTOR => Self::new_vector(index),
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

    pub fn string_eq<T: ArenaTypes>(self, other: NanValue, arena: &Arena<T>) -> bool {
        if self.bits() == other.bits() {
            return true;
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
                || self.tag() == TAG_INLINE_VARIANT
                || (self.tag() == TAG_SYMBOL && self.symbol_kind() == SYMBOL_NULLARY_VARIANT))
    }

    #[inline]
    pub fn is_inline_variant(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_INLINE_VARIANT
    }

    #[inline]
    pub fn is_map(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_MAP
    }

    #[inline]
    pub fn is_vector(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_VECTOR
    }

    #[inline]
    pub fn is_empty_vector_immediate(self) -> bool {
        self.is_nan_boxed() && self.tag() == TAG_VECTOR && self.payload() & ARENA_REF_BIT == 0
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
            TAG_VECTOR => "Vector",
            TAG_RECORD => "Record",
            TAG_VARIANT | TAG_INLINE_VARIANT => "Variant",
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
    pub fn variant_ctor_id<T: ArenaTypes>(self, arena: &Arena<T>) -> Option<u32> {
        if !self.is_nan_boxed() {
            return None;
        }
        match self.tag() {
            TAG_INLINE_VARIANT => Some(self.inline_variant_ctor_id()),
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
    pub fn variant_parts<T: ArenaTypes>(self, arena: &Arena<T>) -> Option<(u32, u16, &[NanValue])> {
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

    #[inline]
    pub fn variant_single_field<T: ArenaTypes>(self, arena: &Arena<T>) -> NanValue {
        if self.tag() == TAG_INLINE_VARIANT {
            self.inline_variant_inner()
        } else {
            let (_, _, fields) = arena.get_variant(self.arena_index());
            debug_assert_eq!(fields.len(), 1);
            fields[0]
        }
    }

    #[inline]
    pub fn inline_variant_info<T: ArenaTypes>(
        self,
        arena: &Arena<T>,
    ) -> Option<(u32, u16, NanValue)> {
        if !self.is_nan_boxed() || self.tag() != TAG_INLINE_VARIANT {
            return None;
        }
        let ctor_id = self.inline_variant_ctor_id();
        let (type_id, variant_id) = arena.get_ctor_parts(ctor_id);
        Some((type_id, variant_id, self.inline_variant_inner()))
    }

    #[inline]
    pub fn bits(self) -> u64 {
        self.0
    }

    #[inline]
    pub fn from_bits(bits: u64) -> Self {
        NanValue(bits)
    }

    pub fn map_key_hash<T: ArenaTypes>(self, arena: &Arena<T>) -> u64 {
        if self.is_string() {
            use core::hash::{Hash, Hasher};
            let mut hasher = DefaultHasher::new();
            3u8.hash(&mut hasher);
            arena.get_string_value(self).hash(&mut hasher);
            hasher.finish()
        } else if self.is_int() && self.heap_index().is_some() {
            // An arena-backed int (i64-overflow or ℤ-overflow) carries the
            // arena INDEX in its bits, not the value — two equal ints at
            // different slots would mis-key. Hash the value structurally.
            // Inline ints encode the value directly, so they skip this.
            self.map_key_hash_deep(arena)
        } else {
            self.bits()
        }
    }

    /// Structural hash that respects `eq_in` for every value shape:
    /// two equal heap values (variants/tuples/records/lists/etc.) always
    /// produce the same u64 regardless of arena layout. Used by Map
    /// when the key type is anything beyond inline scalars.
    pub fn map_key_hash_deep<T: ArenaTypes>(self, arena: &Arena<T>) -> u64 {
        use core::hash::Hasher;
        let mut hasher = DefaultHasher::new();
        self.hash_in(&mut hasher, arena);
        hasher.finish()
    }
}

// -- Debug -----------------------------------------------------------------

impl core::fmt::Debug for NanValue {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
            TAG_INLINE_VARIANT => {
                let ctor = self.inline_variant_ctor_id();
                let inner = self.inline_variant_inner();
                write!(f, "InlineVariant(ctor:{}, {:?})", ctor, inner)
            }
            TAG_LIST if self.is_empty_list_immediate() => write!(f, "EmptyList"),
            TAG_MAP if self.is_empty_map_immediate() => write!(f, "EmptyMap"),
            TAG_VECTOR if self.is_empty_vector_immediate() => write!(f, "EmptyVector"),
            _ => write!(f, "{}(arena:{})", self.type_name(), self.arena_index()),
        }
    }
}

// ---------------------------------------------------------------------------
// A simple DefaultHasher for no_std — mirrors std::collections::hash_map::DefaultHasher
// ---------------------------------------------------------------------------

/// SipHasher-like hasher for use in no_std contexts.
/// When std is available, delegates to `std::collections::hash_map::DefaultHasher`.
struct DefaultHasher {
    #[cfg(feature = "std")]
    inner: std::collections::hash_map::DefaultHasher,
    #[cfg(not(feature = "std"))]
    state: u64,
}

impl DefaultHasher {
    fn new() -> Self {
        #[cfg(feature = "std")]
        {
            Self {
                inner: std::collections::hash_map::DefaultHasher::new(),
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Self {
                state: 0xcbf29ce484222325,
            }
        }
    }
}

impl Hasher for DefaultHasher {
    #[cfg(feature = "std")]
    fn finish(&self) -> u64 {
        self.inner.finish()
    }
    #[cfg(feature = "std")]
    fn write(&mut self, bytes: &[u8]) {
        self.inner.write(bytes)
    }

    #[cfg(not(feature = "std"))]
    fn finish(&self) -> u64 {
        self.state
    }
    #[cfg(not(feature = "std"))]
    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.state ^= b as u64;
            self.state = self.state.wrapping_mul(0x100000001b3);
        }
    }
}

// ---------------------------------------------------------------------------
// Arena
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Arena<T: ArenaTypes> {
    young_entries: Vec<ArenaEntry<T>>,
    yard_entries: Vec<ArenaEntry<T>>,
    handoff_entries: Vec<ArenaEntry<T>>,
    stable_entries: Vec<ArenaEntry<T>>,
    scratch_young: Vec<u32>,
    scratch_yard: Vec<u32>,
    scratch_handoff: Vec<u32>,
    scratch_stable: Vec<u32>,
    peak_usage: ArenaUsage,
    alloc_space: AllocSpace,
    /// Total list elements the collector has written into a fresh shared body.
    /// A collector that keeps sharing intact leaves this proportional to the
    /// number of elements that actually moved, so a quadratic copy shows up
    /// here without any wall-clock measurement.
    list_elements_copied: u64,
    /// Total list elements the collector has *read* while deciding whether a
    /// shared body needs rebuilding. This is the other half of the cost, and it
    /// is not implied by the one above: a body whose elements all relocate to
    /// themselves is scanned in full and copied not at all. Only a body of
    /// immediates escapes the read, so this is the counter that says which
    /// element types the traversal cost is actually linear in.
    list_elements_scanned: u64,
    /// Total map entries duplicated because a table was rebuilt instead of
    /// written into — `Map.set` and `Map.remove` on a target the ownership
    /// analysis could not prove unshared, any map builder that rebuilds its
    /// table per entry, and [`Arena::deep_import`] carrying a map into another
    /// arena. A map threaded linearly through a fold leaves this proportional
    /// to the number of inserts; one the analysis gave up on makes it
    /// quadratic, which is visible here without any wall-clock measurement.
    ///
    /// One copy is deliberately outside it: the collector duplicates map
    /// storage of its own accord on the stable-promotion path, and that is not
    /// in this number — see [`Arena::map_entries_scanned`].
    ///
    /// The count is per-arena. [`Arena::clone_static`] starts a child at zero,
    /// and [`Arena::absorb_copy_counters`] folds a child's total back into its
    /// parent when the branch rejoins.
    map_entries_copied: u64,
    /// Total map entries the collector has *read* while deciding whether a live
    /// map needs rewriting. Unlike a list body, a map carries no
    /// all-immediate flag, so this grows on every collection that sees a live
    /// map whatever the map holds. It is the counter the residual time of a
    /// map-building program follows.
    map_entries_scanned: u64,
    /// Canonical lookup keys consulted by `find_type_id`: bare for entry and
    /// builtin types, and module-qualified for dependency types. Kept in
    /// lockstep with `type_names`.
    pub type_keys: Vec<String>,
    /// Source-facing display spellings used by value conversion and rendering.
    pub type_names: Vec<String>,
    pub type_field_names: Vec<Vec<String>>,
    pub type_variant_names: Vec<Vec<String>>,
    pub type_variant_ctor_ids: Vec<Vec<u32>>,
    pub ctor_to_type_variant: Vec<(u32, u16)>,
    pub symbol_entries: Vec<ArenaSymbol<T>>,
    /// Fallback aliases for types (e.g. bare "Shape" → type_id for "Data.Shape").
    pub type_aliases: Vec<(String, u32)>,
}

#[derive(Debug, Clone)]
pub enum ArenaEntry<T: ArenaTypes> {
    /// An `i64`-fitting integer that overflowed the 45-bit inline NaN-box.
    /// Also the cheap zero-filler used by the GC during compaction.
    Int(i64),
    /// An integer outside the `i64` range (mathematical ℤ overflow slot).
    /// Always holds a value not representable as `i64`, so the runtime's
    /// canonical-form invariant holds across inline / arena-i64 / arena-big.
    BigInt(Box<num_bigint::BigInt>),
    String(Rc<str>),
    List(ArenaList),
    Tuple(Vec<NanValue>),
    /// A map, plus the same claim [`ListBody::all_immediate`] makes about a
    /// list body: `all_immediate` is `true` only when every key and every value
    /// in `map` is [`NanValue::is_immediate`], which makes relocating the table
    /// provably the identity and lets the collector skip it without reading it.
    ///
    /// The flag describes the shared content, so it is right to clone with the
    /// entry — a second entry over the same table makes the same claim about
    /// the same values.
    ///
    /// `false` is always safe: it only costs the scan. `true` when something in
    /// the table does carry an arena index is not safe and does not fail
    /// loudly — the collector would move that value and leave the map pointing
    /// at where it used to be. Every site that builds one of these therefore
    /// either proves the flag by [`map_all_immediate`] or derives it from the
    /// flag of the map it was built from; [`Arena::push_map`] is the choke
    /// point that proves it for the builders with nothing to inherit.
    Map {
        map: T::Map,
        all_immediate: bool,
    },
    Vector(Vec<NanValue>),
    Record {
        type_id: u32,
        fields: Vec<NanValue>,
    },
    Variant {
        type_id: u32,
        variant_id: u16,
        fields: Vec<NanValue>,
    },
    Fn(Rc<T::Fn>),
    Builtin(Rc<str>),
    Namespace {
        name: Rc<str>,
        members: Vec<(Rc<str>, NanValue)>,
    },
    Boxed(NanValue),
}

/// A borrowed view of an arena-stored integer, discriminating the
/// `i64`-overflow slot from the ℤ-overflow slot without materializing.
/// The runtime crate reconstructs a canonical `AverInt` from this.
#[derive(Debug, Clone, Copy)]
pub enum ArenaIntRef<'a> {
    Small(i64),
    Big(&'a num_bigint::BigInt),
}

#[derive(Debug, Clone)]
pub enum ArenaSymbol<T: ArenaTypes> {
    Fn(Rc<T::Fn>),
    Builtin(Rc<str>),
    Namespace {
        name: Rc<str>,
        members: Vec<(Rc<str>, NanValue)>,
    },
    NullaryVariant {
        ctor_id: u32,
    },
}

/// Element storage shared by every list view that points at it.
///
/// A body is immutable once built — nothing in the arena hands out a mutable
/// reference to one — so `all_immediate` can be decided at construction and
/// trusted forever after. It is `true` only when no element carries a heap
/// index, which makes relocating the elements provably the identity and lets
/// the collector skip the body without reading it. `false` is always safe: it
/// only costs the scan.
#[derive(Debug)]
pub struct ListBody {
    items: Vec<NanValue>,
    all_immediate: bool,
}

impl ListBody {
    pub fn new(items: Vec<NanValue>) -> Self {
        let all_immediate = items.iter().all(|value| value.is_immediate());
        Self {
            items,
            all_immediate,
        }
    }

    /// Whether no element carries a heap index, so no element can ever be
    /// relocated by a collection.
    #[inline]
    pub fn all_immediate(&self) -> bool {
        self.all_immediate
    }
}

impl Deref for ListBody {
    type Target = [NanValue];

    #[inline]
    fn deref(&self) -> &[NanValue] {
        &self.items
    }
}

#[derive(Debug, Clone)]
pub enum ArenaList {
    Flat {
        items: Rc<ListBody>,
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
        rest: Rc<ListBody>,
        start: usize,
        len: usize,
    },
}

const LIST_APPEND_CHUNK_LIMIT: usize = 128;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HeapSpace {
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

pub(crate) const HEAP_SPACE_SHIFT: u32 = 30;
pub(crate) const HEAP_SPACE_MASK_U32: u32 = 0b11 << HEAP_SPACE_SHIFT;
pub(crate) const HEAP_INDEX_MASK_U32: u32 = (1 << HEAP_SPACE_SHIFT) - 1;

mod arena;
mod compare;
mod lists;
mod memory;

// ---------------------------------------------------------------------------
// Feature-gated MapLike impl for aver_rt::AverMap
// ---------------------------------------------------------------------------

#[cfg(feature = "runtime")]
/// `PersistentMap` type alias used by the VM arena.
pub type PersistentMap = aver_rt::AverMap<u64, (NanValue, NanValue)>;

#[cfg(feature = "runtime")]
impl MapLike for aver_rt::AverMap<u64, (NanValue, NanValue)> {
    fn new() -> Self {
        aver_rt::AverMap::new()
    }

    fn get(&self, key: &u64) -> Option<&(NanValue, NanValue)> {
        aver_rt::AverMap::get(self, key)
    }

    fn insert(&self, key: u64, value: (NanValue, NanValue)) -> Self {
        aver_rt::AverMap::insert(self, key, value)
    }

    fn insert_owned(self, key: u64, value: (NanValue, NanValue)) -> Self {
        aver_rt::AverMap::insert_owned(self, key, value)
    }

    fn rewrite_values_mut(&mut self, f: impl FnMut(&mut (NanValue, NanValue))) {
        self.rewrite_values_in_place(f)
    }

    fn table_id(&self) -> usize {
        aver_rt::AverMap::table_id(self)
    }

    fn len(&self) -> usize {
        aver_rt::AverMap::len(self)
    }

    fn is_empty(&self) -> bool {
        aver_rt::AverMap::is_empty(self)
    }

    fn iter(&self) -> impl Iterator<Item = (&u64, &(NanValue, NanValue))> {
        aver_rt::AverMap::iter(self)
    }

    fn values(&self) -> impl Iterator<Item = &(NanValue, NanValue)> {
        aver_rt::AverMap::values(self)
    }
}

// ---------------------------------------------------------------------------
// Stub PersistentMap when runtime is off (BTreeMap-based)
// ---------------------------------------------------------------------------

#[cfg(not(feature = "runtime"))]
/// Stub `PersistentMap` for non-runtime builds (e.g. wasm-compile only).
#[derive(Clone, Debug)]
pub struct PersistentMap(alloc::collections::BTreeMap<u64, (NanValue, NanValue)>);

#[cfg(not(feature = "runtime"))]
impl MapLike for PersistentMap {
    fn new() -> Self {
        PersistentMap(alloc::collections::BTreeMap::new())
    }

    fn get(&self, key: &u64) -> Option<&(NanValue, NanValue)> {
        self.0.get(key)
    }

    fn insert(&self, key: u64, value: (NanValue, NanValue)) -> Self {
        let mut m = self.0.clone();
        m.insert(key, value);
        PersistentMap(m)
    }

    fn rewrite_values_mut(&mut self, mut f: impl FnMut(&mut (NanValue, NanValue))) {
        for value in self.0.values_mut() {
            f(value);
        }
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn iter(&self) -> impl Iterator<Item = (&u64, &(NanValue, NanValue))> {
        self.0.iter()
    }

    fn values(&self) -> impl Iterator<Item = &(NanValue, NanValue)> {
        self.0.values()
    }
}
