//! Canonical-ABI import registry for `--target wasip2`.
//!
//! Mirrors the shape of `EffectRegistry` but speaks Component-Model
//! canonical-ABI names instead of `aver/*` host-bridge names. The
//! two registries coexist: when `target == TargetMode::Wasip2`,
//! the wasm-gc emitter populates this registry from the discovered
//! `EffectName`s (via `EffectName::lowers_on_wasip2`) and the
//! import-section emit branch in `module.rs` reads from THIS
//! registry instead of the `EffectRegistry`'s `import_pair()`.
//!
//! Why a separate registry: one Aver effect (`Console.print`) lowers
//! to MULTIPLE wasip2 imports (cache-stdout-handle + write-bytes),
//! so the existing 1-effect → 1-import shape in `EffectName`
//! cannot retrofit. See the plan in
//! `~/.claude-personal/plans/zaplanujmy-sobie-adnie-to-snug-rabin.md`.
//!
//! Phase 1.2b1.2 wires the registry skeleton + the import-section
//! branch. The slots themselves get exercised in Phase 1.2b1.5
//! when the call-site lowering for Console.print/error/warn lands.
//! Until then, programs that touch any wasip2-relevant effect are
//! still rejected upstream by `wasip2::effect_check`.

use std::collections::HashMap;

use wasm_encoder::ValType;

/// One canonical-ABI import the Phase 1.2b1 wasip2 path may need.
///
/// Canonical core wasm import names (validated against
/// `wasip2-1.0.1+wasi-0.2.4` bindgen output and
/// `wit-component-0.248.0/tests/components/`):
///
/// - module = the WIT interface qualified name including version
///   (`"wasi:cli/stdout@0.2.4"`, `"wasi:io/streams@0.2.4"`);
/// - field for free fns = kebab-case WIT name (`"get-stdout"`);
/// - field for resource methods = `"[method]<resource>.<method>"`
///   (`"[method]output-stream.blocking-write-and-flush"`);
/// - field for resource drops = `"[resource-drop]<resource>"`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum Wasip2ImportSlot {
    /// `wasi:cli/stdout.get-stdout: func() -> output-stream`.
    /// Canonical-ABI signature: `() -> i32` (the resource handle).
    CliGetStdout,
    /// `wasi:cli/stderr.get-stderr: func() -> output-stream`.
    /// Canonical-ABI signature: `() -> i32`.
    CliGetStderr,
    /// `wasi:io/streams.[method]output-stream.blocking-write-and-flush:
    /// func(contents: list<u8>) -> result<_, stream-error>`.
    ///
    /// Canonical-ABI signature with the `result<_, stream-error>` lowered
    /// via retptr (host writes 12 bytes at retptr):
    ///   `(handle: i32, buf_ptr: i32, buf_len: i32, retptr: i32)`.
    /// Phase 1.2b1 ignores the retptr contents — Aver `Console.print`
    /// is `Unit`, matching the wasm-gc target's fire-and-forget
    /// semantics.
    OutputStreamBlockingWriteAndFlush,
}

impl Wasip2ImportSlot {
    /// Canonical core wasm `(module, field)` pair this slot imports.
    /// `wit_component::ComponentEncoder` matches against these names
    /// when binding the component's WIT-typed imports to the core
    /// module's plain wasm imports.
    pub(super) fn module_field_pair(self) -> (&'static str, &'static str) {
        match self {
            Wasip2ImportSlot::CliGetStdout => ("wasi:cli/stdout@0.2.4", "get-stdout"),
            Wasip2ImportSlot::CliGetStderr => ("wasi:cli/stderr@0.2.4", "get-stderr"),
            Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush => (
                "wasi:io/streams@0.2.4",
                "[method]output-stream.blocking-write-and-flush",
            ),
        }
    }

    pub(super) fn params(self) -> Vec<ValType> {
        match self {
            Wasip2ImportSlot::CliGetStdout | Wasip2ImportSlot::CliGetStderr => Vec::new(),
            Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush => {
                vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32]
            }
        }
    }

    pub(super) fn results(self) -> Vec<ValType> {
        match self {
            // Resource handle — i32 ID owned by the host.
            Wasip2ImportSlot::CliGetStdout | Wasip2ImportSlot::CliGetStderr => vec![ValType::I32],
            // Result lowered via retptr — no inline return.
            Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush => Vec::new(),
        }
    }
}

/// Per-program registry of canonical-ABI imports the wasip2 emit
/// path declares. Mirrors `EffectRegistry`'s shape:
/// - `order` is the deterministic insertion sequence (also the
///   wasm fn idx assignment order — slots take fn idx `0..K`),
/// - `wasm_fn_idx` / `wasm_type_idx` are populated by `assign_slots`
///   once the type-section has run far enough to allocate slots.
#[derive(Default)]
pub(super) struct Wasip2ImportRegistry {
    order: Vec<Wasip2ImportSlot>,
    wasm_fn_idx: HashMap<Wasip2ImportSlot, u32>,
    wasm_type_idx: HashMap<Wasip2ImportSlot, u32>,
}

impl Wasip2ImportRegistry {
    pub(super) fn new() -> Self {
        Self::default()
    }

    /// Idempotent. Order of first registration is preserved.
    pub(super) fn register(&mut self, slot: Wasip2ImportSlot) {
        if !self.order.contains(&slot) {
            self.order.push(slot);
        }
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = Wasip2ImportSlot> + '_ {
        self.order.iter().copied()
    }

    pub(super) fn import_count(&self) -> u32 {
        self.order.len() as u32
    }

    /// Reserve type and fn-idx slots for each registered import.
    /// Called from `module.rs` once the type-section counter has
    /// advanced past user types but BEFORE user-fn types are
    /// allocated — wasip2 imports occupy fn idx `0..K`, exactly
    /// where `EffectRegistry` would have allocated `aver/*` imports
    /// on the AverBridge target.
    pub(super) fn assign_slots(&mut self, next_type_idx: &mut u32) {
        for (i, slot) in self.order.iter().copied().enumerate() {
            self.wasm_fn_idx.insert(slot, i as u32);
            self.wasm_type_idx.insert(slot, *next_type_idx);
            *next_type_idx += 1;
        }
    }

    /// Used by Phase 1.2b1.5 call-site lowering — kept ahead of the
    /// commit that consumes it so the registry shape is complete.
    #[allow(dead_code)]
    pub(super) fn lookup_wasm_fn_idx(&self, slot: Wasip2ImportSlot) -> Option<u32> {
        self.wasm_fn_idx.get(&slot).copied()
    }

    pub(super) fn lookup_wasm_type_idx(&self, slot: Wasip2ImportSlot) -> Option<u32> {
        self.wasm_type_idx.get(&slot).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn module_field_pair_matches_canonical_abi_names() {
        // Validated against
        //   ~/.cargo/registry/src/.../wasip2-1.0.1+wasi-0.2.4/src/imports.rs
        // and
        //   ~/.cargo/registry/src/.../wit-component-0.248.0/tests/
        //     components/adapt-stub-wasip2/module.wat
        // — these names are what `wit_component::ComponentEncoder`
        // matches against at component-build time.
        assert_eq!(
            Wasip2ImportSlot::CliGetStdout.module_field_pair(),
            ("wasi:cli/stdout@0.2.4", "get-stdout"),
        );
        assert_eq!(
            Wasip2ImportSlot::CliGetStderr.module_field_pair(),
            ("wasi:cli/stderr@0.2.4", "get-stderr"),
        );
        assert_eq!(
            Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush.module_field_pair(),
            (
                "wasi:io/streams@0.2.4",
                "[method]output-stream.blocking-write-and-flush",
            ),
        );
    }

    #[test]
    fn registry_assigns_slots_in_order() {
        let mut r = Wasip2ImportRegistry::new();
        r.register(Wasip2ImportSlot::CliGetStdout);
        r.register(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush);
        // Idempotent — second register of the same slot is a no-op.
        r.register(Wasip2ImportSlot::CliGetStdout);
        assert_eq!(r.import_count(), 2);

        let mut next_type_idx: u32 = 100;
        r.assign_slots(&mut next_type_idx);
        assert_eq!(next_type_idx, 102);

        assert_eq!(
            r.lookup_wasm_fn_idx(Wasip2ImportSlot::CliGetStdout),
            Some(0)
        );
        assert_eq!(
            r.lookup_wasm_fn_idx(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush),
            Some(1)
        );
        assert_eq!(
            r.lookup_wasm_type_idx(Wasip2ImportSlot::CliGetStdout),
            Some(100)
        );
        assert_eq!(
            r.lookup_wasm_type_idx(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush),
            Some(101)
        );
    }
}
