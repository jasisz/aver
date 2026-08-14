//! NaN-boxed compact Value representation — thin re-export layer.
//!
//! The core NanValue, Arena, and supporting types now live in the
//! `aver_memory` crate.  This module defines `AverTypes` that plugs
//! `FunctionValue` and `PersistentMap` into the generic arena,
//! provides type aliases so existing
//! `use crate::nan_value::{Arena, NanValue, ...}` continues to work,
//! and keeps the `convert` (Value <-> NanValue) and `tests` submodules
//! that depend on runtime-specific types.

use std::sync::Arc as Rc;

use crate::value::FunctionValue;

// ---------------------------------------------------------------------------
// Re-exports from aver_memory (everything the rest of aver-lang needs)
// ---------------------------------------------------------------------------

pub use aver_memory::{
    // Bit-layout constants needed by convert.rs and downstream code
    ARENA_REF_BIT,
    AllocSpace,
    ArenaIntRef,
    ArenaList,
    ArenaTypes,
    ArenaUsage,
    FnValueName,
    HEAP_SPACE_COUNT,
    IMM_FALSE,
    IMM_TRUE,
    IMM_UNIT,
    INT_INLINE_MAX,
    INT_INLINE_MIN,
    ListBody,
    MapLike,
    NanString,
    NanValue,
    PersistentMap,
    SYMBOL_BUILTIN,
    SYMBOL_FN,
    SYMBOL_NAMESPACE,
    SYMBOL_NULLARY_VARIANT,
    TAG_ERR,
    TAG_IMMEDIATE,
    TAG_INLINE_VARIANT,
    TAG_INT,
    TAG_LIST,
    TAG_MAP,
    TAG_NONE,
    TAG_OK,
    TAG_RECORD,
    TAG_SOME,
    TAG_STRING,
    TAG_SYMBOL,
    TAG_TUPLE,
    TAG_VARIANT,
    TAG_VECTOR,
    WRAP_ERR,
    WRAP_OK,
    WRAP_SOME,
    split_heap_index,
};

// ---------------------------------------------------------------------------
// Aver-specific concrete types
// ---------------------------------------------------------------------------

/// Concrete arena types for the Aver runtime.
#[derive(Clone, Debug)]
pub struct AverTypes;

impl ArenaTypes for AverTypes {
    type Fn = FunctionValue;
    type Map = PersistentMap;
}

// -- FnValueName for FunctionValue -----------------------------------------

impl FnValueName for FunctionValue {
    fn name(&self) -> &str {
        &self.name
    }
}

// ---------------------------------------------------------------------------
// Type aliases — these make existing code work unchanged
// ---------------------------------------------------------------------------

pub type Arena = aver_memory::Arena<AverTypes>;
pub type ArenaEntry = aver_memory::ArenaEntry<AverTypes>;
pub type ArenaSymbol = aver_memory::ArenaSymbol<AverTypes>;

/// Every arena slot is one of these, so the widest variant sets what a slot
/// costs whatever it holds. `Namespace` is the widest at 40 bytes, which leaves
/// the `all_immediate` flag beside the 8-byte map handle riding in padding that
/// was already there — the flag that lets the collector skip a map of
/// immediates costs nothing per slot. A variant that grew past 40 would put
/// that back, on every entry in the arena, which is why this is pinned.
const _: () = assert!(core::mem::size_of::<ArenaEntry>() == 48);

// ---------------------------------------------------------------------------
// Extension trait for Value <-> NanValue conversion
// ---------------------------------------------------------------------------

#[cfg(feature = "runtime")]
pub use convert::NanValueConvert;

#[cfg(feature = "runtime")]
mod convert;

#[cfg(feature = "runtime")]
pub use int_ext::NanIntExt;

#[cfg(feature = "runtime")]
mod int_ext;

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests;
