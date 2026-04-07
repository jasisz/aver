//! NaN-boxed compact Value representation — thin re-export layer.
//!
//! The core NanValue, Arena, and supporting types now live in the
//! `aver_memory` crate.  This module defines the interpreter-specific
//! `InterpreterTypes` that plugs `FunctionValue` and `PersistentMap`
//! into the generic arena, provides type aliases so existing
//! `use crate::nan_value::{Arena, NanValue, ...}` continues to work,
//! and keeps the `convert` (Value <-> NanValue) and `tests` submodules
//! that depend on interpreter-specific types.

use std::sync::Arc as Rc;

use crate::value::FunctionValue;

// ---------------------------------------------------------------------------
// Re-exports from aver_memory (everything the rest of aver-lang needs)
// ---------------------------------------------------------------------------

pub use aver_memory::{
    // Bit-layout constants needed by convert.rs and downstream code
    ARENA_REF_BIT,
    AllocSpace,
    ArenaList,
    ArenaTypes,
    ArenaUsage,
    FnValueName,
    IMM_FALSE,
    IMM_TRUE,
    IMM_UNIT,
    INT_INLINE_MAX,
    INT_INLINE_MIN,
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
};

// ---------------------------------------------------------------------------
// Interpreter-specific concrete types
// ---------------------------------------------------------------------------

/// The interpreter's concrete arena types.
#[derive(Clone, Debug)]
pub struct InterpreterTypes;

impl ArenaTypes for InterpreterTypes {
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

pub type Arena = aver_memory::Arena<InterpreterTypes>;
pub type ArenaEntry = aver_memory::ArenaEntry<InterpreterTypes>;
pub type ArenaSymbol = aver_memory::ArenaSymbol<InterpreterTypes>;

// ---------------------------------------------------------------------------
// Extension trait for Value <-> NanValue conversion
// ---------------------------------------------------------------------------

pub use convert::NanValueConvert;

mod convert;

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests;
