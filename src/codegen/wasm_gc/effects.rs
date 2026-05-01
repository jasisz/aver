//! Effectful builtin tracking — `Console.print`, `Http.get`, etc.
//!
//! These don't get bodies in the user module. Instead, the codegen
//! emits `(import "aver" "<name>" (func ...))` and the host (browser
//! / workerd / wasmtime+wasi) supplies the implementation. Same shape
//! the legacy backend uses for effects, just without the
//! `aver_runtime.wasm` middleman.
//!
//! Imports take the lowest fn indices in wasm — `0..K` for K
//! registered effects. User fn indices and builtin helper fn
//! indices shift up by K.

use std::collections::HashMap;

use wasm_encoder::ValType;

use super::WasmGcError;
use super::types::TypeRegistry;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum EffectName {
    /// `Console.print(String) -> Unit`. Imported as `aver.console_print`
    /// — the host writes the string to its stdout (or equivalent).
    ConsolePrint,
    ConsoleError,
    ConsoleWarn,
    /// `Time.unixMs() -> Int`. Imported as `aver.time_unix_ms` — host
    /// supplies the current unix timestamp in milliseconds.
    TimeUnixMs,
}

impl EffectName {
    pub(super) fn from_dotted(s: &str) -> Option<Self> {
        match s {
            "Console.print" => Some(Self::ConsolePrint),
            "Console.error" => Some(Self::ConsoleError),
            "Console.warn" => Some(Self::ConsoleWarn),
            "Time.unixMs" => Some(Self::TimeUnixMs),
            _ => None,
        }
    }

    pub(super) fn canonical(self) -> &'static str {
        match self {
            Self::ConsolePrint => "Console.print",
            Self::ConsoleError => "Console.error",
            Self::ConsoleWarn => "Console.warn",
            Self::TimeUnixMs => "Time.unixMs",
        }
    }

    /// Wasm import (module, field) pair. Module is always `aver` for
    /// our effects — host supplies a single namespace.
    pub(super) fn import_pair(self) -> (&'static str, &'static str) {
        match self {
            Self::ConsolePrint => ("aver", "console_print"),
            Self::ConsoleError => ("aver", "console_error"),
            Self::ConsoleWarn => ("aver", "console_warn"),
            Self::TimeUnixMs => ("aver", "time_unix_ms"),
        }
    }

    /// Param types declared in the wasm import. We use `(ref null any)`
    /// for String-typed args — see ConsolePrint comment above for the
    /// subtyping rationale.
    pub(super) fn params(self, _registry: &TypeRegistry) -> Result<Vec<ValType>, WasmGcError> {
        match self {
            Self::ConsolePrint | Self::ConsoleError | Self::ConsoleWarn => {
                Ok(vec![any_ref_ty()])
            }
            Self::TimeUnixMs => Ok(vec![]),
        }
    }

    pub(super) fn results(self, _registry: &TypeRegistry) -> Result<Vec<ValType>, WasmGcError> {
        match self {
            Self::ConsolePrint | Self::ConsoleError | Self::ConsoleWarn => Ok(vec![]),
            Self::TimeUnixMs => Ok(vec![ValType::I64]),
        }
    }
}

fn any_ref_ty() -> ValType {
    ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Abstract {
            shared: false,
            ty: wasm_encoder::AbstractHeapType::Any,
        },
    })
}

/// Per-module registry of used effects. Allocates wasm fn indices
/// starting at 0 (imports come first); the offset returned via
/// `import_count` is the value that gets added to every other fn
/// index in the module.
#[derive(Default)]
pub(super) struct EffectRegistry {
    order: Vec<EffectName>,
    wasm_fn_idx: HashMap<EffectName, u32>,
    wasm_type_idx: HashMap<EffectName, u32>,
}

impl EffectRegistry {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn register(&mut self, name: EffectName) {
        if !self.order.contains(&name) {
            self.order.push(name);
        }
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = EffectName> + '_ {
        self.order.iter().copied()
    }

    pub(super) fn import_count(&self) -> u32 {
        self.order.len() as u32
    }

    /// Reserve type and fn-idx slots for each registered effect.
    /// Imports occupy fn-idx 0..K; type indices come from the same
    /// counter the user-fn types use, deferred by the caller.
    pub(super) fn assign_slots(&mut self, next_type_idx: &mut u32) {
        for (i, name) in self.order.iter().copied().enumerate() {
            self.wasm_fn_idx.insert(name, i as u32);
            self.wasm_type_idx.insert(name, *next_type_idx);
            *next_type_idx += 1;
        }
    }

    pub(super) fn lookup_wasm_fn_idx(&self, name: EffectName) -> Option<u32> {
        self.wasm_fn_idx.get(&name).copied()
    }

    pub(super) fn lookup_wasm_type_idx(&self, name: EffectName) -> Option<u32> {
        self.wasm_type_idx.get(&name).copied()
    }
}

fn string_ref_ty(registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    let idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "effect requires String repr but no string type slot was allocated".into(),
        ))?;
    Ok(ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(idx),
    }))
}
