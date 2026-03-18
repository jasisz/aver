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
        debug_assert!(i >= INT_INLINE_MIN && i <= INT_INLINE_MAX);
        let payload = (i as u64) & INT_INLINE_MASK;
        Self::encode(TAG_INT, payload)
    }

    #[inline]
    pub fn new_int_arena(arena_index: u32) -> Self {
        Self::encode(TAG_INT, INT_BIG_BIT | (arena_index as u64))
    }

    #[inline]
    pub fn new_int(i: i64, arena: &mut Arena) -> Self {
        if i >= INT_INLINE_MIN && i <= INT_INLINE_MAX {
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
    entries: Vec<ArenaEntry>,
    pub(crate) type_names: Vec<String>,
    pub(crate) type_field_names: Vec<Vec<String>>,
    pub(crate) type_variant_names: Vec<Vec<String>>,
}

#[derive(Debug, Clone)]
pub enum ArenaEntry {
    Int(i64),
    String(Rc<str>),
    List(Vec<NanValue>),
    Tuple(Vec<NanValue>),
    Map(HashMap<u64, (NanValue, NanValue)>),
    Record { type_id: u32, fields: Vec<NanValue> },
    Variant { type_id: u32, variant_id: u16, fields: Vec<NanValue> },
    Fn(Rc<FunctionValue>),
    Builtin(Rc<str>),
    Namespace { name: Rc<str>, members: Vec<(Rc<str>, NanValue)> },
    Boxed(NanValue),
}

impl Arena {
    pub fn new() -> Self {
        Arena {
            entries: Vec::with_capacity(256),
            type_names: Vec::new(),
            type_field_names: Vec::new(),
            type_variant_names: Vec::new(),
        }
    }

    #[inline]
    pub fn push(&mut self, entry: ArenaEntry) -> u32 {
        let idx = self.entries.len() as u32;
        self.entries.push(entry);
        idx
    }

    #[inline]
    pub fn get(&self, index: u32) -> &ArenaEntry {
        &self.entries[index as usize]
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
        self.push(ArenaEntry::Variant { type_id, variant_id, fields })
    }
    pub fn push_list(&mut self, items: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::List(items))
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
            ArenaEntry::Variant { type_id, variant_id, fields } => (*type_id, *variant_id, fields),
            _ => panic!("Arena: expected Variant at {}", index),
        }
    }
    pub fn get_list(&self, index: u32) -> &[NanValue] {
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
    pub fn get_map(&self, index: u32) -> &HashMap<u64, (NanValue, NanValue)> {
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
        self.type_names.iter().position(|n| n == name).map(|i| i as u32)
    }
    pub fn find_variant_id(&self, type_id: u32, variant_name: &str) -> Option<u16> {
        self.type_variant_names
            .get(type_id as usize)?
            .iter()
            .position(|n| n == variant_name)
            .map(|i| i as u16)
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::new()
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
                let a = arena.get_list(self.arena_index());
                let b = arena.get_list(other.arena_index());
                a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.eq_in(*y, arena))
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
                ta == tb && fa.len() == fb.len() && fa.iter().zip(fb).all(|(a, b)| a.eq_in(*b, arena))
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
            let bits = if f == 0.0 { 0.0f64.to_bits() } else { f.to_bits() };
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
                let items = arena.get_list(self.arena_index());
                items.len().hash(state);
                for item in items {
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
                let items = arena.get_list(self.arena_index());
                let parts: Vec<_> = items.iter().map(|v| v.repr_inner(arena)).collect();
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
                let parts: Vec<_> = pairs.into_iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
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
        if self.is_unit() { None } else { Some(self.repr(arena)) }
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
                let nv_items: Vec<_> = items.iter().map(|v| NanValue::from_value(v, arena)).collect();
                NanValue::new_tuple(arena.push_tuple(nv_items))
            }
            Value::List(aver_list) => {
                let items: Vec<_> = aver_list.to_vec().iter().map(|v| NanValue::from_value(v, arena)).collect();
                NanValue::new_list(arena.push_list(items))
            }
            Value::Map(map) => {
                let mut nv_map = HashMap::new();
                for (k, v) in map {
                    let nk = NanValue::from_value(k, arena);
                    let nv = NanValue::from_value(v, arena);
                    nv_map.insert(nk.bits(), (nk, nv));
                }
                let idx = arena.push(ArenaEntry::Map(nv_map));
                NanValue::new_map(idx)
            }
            Value::Fn(f) => NanValue::new_fn(arena.push_fn(Rc::clone(f))),
            Value::Builtin(name) => NanValue::new_builtin(arena.push_builtin(name)),
            Value::Record { type_name, fields } => {
                let type_id = arena
                    .find_type_id(type_name)
                    .unwrap_or_else(|| {
                        let field_names: Vec<String> = fields.iter().map(|(n, _)| n.clone()).collect();
                        arena.register_record_type(type_name, field_names)
                    });
                let nv_fields: Vec<_> = fields.iter().map(|(_, v)| NanValue::from_value(v, arena)).collect();
                NanValue::new_record(arena.push_record(type_id, nv_fields))
            }
            Value::Variant { type_name, variant, fields } => {
                let type_id = arena
                    .find_type_id(type_name)
                    .unwrap_or_else(|| arena.register_sum_type(type_name, vec![variant.clone()]));
                let variant_id = arena
                    .find_variant_id(type_id, variant)
                    .unwrap_or_else(|| {
                        // Register new variant dynamically
                        let variants = &mut arena.type_variant_names[type_id as usize];
                        let id = variants.len() as u16;
                        variants.push(variant.clone());
                        id
                    });
                let nv_fields: Vec<_> = fields.iter().map(|v| NanValue::from_value(v, arena)).collect();
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
                let items = arena.get_list(self.arena_index());
                let vals: Vec<Value> = items.iter().map(|v| v.to_value(arena)).collect();
                Value::List(aver_rt::AverList::from_vec(vals))
            }
            TAG_TUPLE => {
                let items = arena.get_tuple(self.arena_index());
                Value::Tuple(items.iter().map(|v| v.to_value(arena)).collect())
            }
            TAG_MAP => {
                let map = arena.get_map(self.arena_index());
                let mut hm = HashMap::new();
                for (_, (k, v)) in map {
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
        for i in [0, 1, -1, 42, -42, 1_000_000, -1_000_000, INT_INLINE_MAX, INT_INLINE_MIN] {
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
        assert_eq!(arena.get_list(v.arena_index()).len(), 2);
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
        let p1 = arena.push_record(0, vec![NanValue::new_int_inline(1), NanValue::new_int_inline(2)]);
        let p2 = arena.push_record(0, vec![NanValue::new_int_inline(3), NanValue::new_int_inline(4)]);
        let list_idx = arena.push_list(vec![NanValue::new_record(p1), NanValue::new_record(p2)]);
        let list = NanValue::new_list(list_idx);

        let items = arena.get_list(list.arena_index());
        let (_, fields) = arena.get_record(items[1].arena_index());
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
