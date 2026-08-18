//! Wasm-gc backend-link view — the resolved-HIR slice the rest of
//! this backend reads.
//!
//! ## Why this exists as a named view, not a transitional hack
//!
//! The pipeline (`parse → tco → typecheck → resolve → CodegenContext`)
//! is module-aware: `CodegenContext.resolved_fn_defs` holds the
//! entry's fns and `resolved_module_fn_defs` holds each dep's fns
//! separately, both keyed against the pre-flatten `SymbolTable`.
//! That structure is correct for backends that emit per-module
//! artifacts (Rust → one Rust crate per Aver module, VM → per-
//! module bytecode, Lean → namespaces, Dafny → modules).
//!
//! The wasm-gc backend emits a single .wasm file by design — that's
//! a *linking* decision, not a semantic one. The pipeline shouldn't
//! know about it. So this backend runs its own legal link stage:
//! [`flatten_multimodule`] (in `crate::codegen::wasm_gc::flatten`)
//! merges dep fns into the entry's items with module-prefixed names
//! (`Fractal.render` → `Fractal_render`), then this view runs a
//! fresh resolver pass against the flattened items.
//!
//! Calling this a "link" stage names what it actually is: the
//! backend collapses N modules into 1 emit unit, and identity
//! lookups in the post-link world use [`WasmGcLinkedView`] as the
//! canonical resolver substrate. Mirrors how a native linker has
//! its own symbol view distinct from the per-translation-unit views
//! the compiler produced.
//!
//! ## Invariant
//!
//! - **Input:** post-typecheck, post-resolve, post-flatten items
//!   slice. Every reachable `FnDef` has been monomorphised and
//!   prefixed.
//! - **Output:** [`WasmGcLinkedView`] holding the flattened-scope
//!   [`SymbolTable`] plus one [`ResolvedFnDef`] per source `FnDef`,
//!   indexable by stable [`FnId`].
//!
//! ## Why FnId lookup, not bare name
//!
//! Pre-PR-9.3a the codegen `CodegenContext::resolve_fn_def` flat-
//! searched resolved tables by `rfd.name == fd.name`. Post-flatten
//! that happens to work in wasm-gc because every name is prefixed.
//! [`fn_def_by_id`] keys by `FnId` instead so the pattern doesn't
//! depend on flatten's name-uniqueness side effect — the link stage
//! still rewrites names, but the identity layer stays robust.
//!
//! [`flatten_multimodule`]: super::flatten::flatten_multimodule
//! [`SymbolTable`]: crate::ir::SymbolTable
//! [`ResolvedFnDef`]: crate::ir::hir::ResolvedFnDef
//! [`FnId`]: crate::ir::FnId

use std::collections::HashMap;

use crate::ast::{FnDef, TopLevel};
use crate::ir::FnId;
use crate::ir::SymbolTable;
use crate::ir::hir::{ResolvedFnDef, resolve_fn_def_external};

use super::WasmGcError;

/// Resolved-HIR view of the wasm-gc backend's post-link compile
/// unit. See module doc for the invariant.
pub(super) struct WasmGcLinkedView {
    /// Resolver SymbolTable built against the flattened items. Use
    /// `fn_id_of(&FnKey)` against this table to translate a source
    /// name to a stable identity.
    pub(super) symbol_table: SymbolTable,
    /// Resolved fn defs, one per source `FnDef` in the flattened
    /// compile unit. Source-position order matches `module.rs`'s
    /// wasm fn idx allocation, so positional indexing
    /// (`resolved_fn_defs[i]`) lines up with the `i`-th
    /// `&FnDef` the caller iterates.
    pub(super) resolved_fn_defs: Vec<ResolvedFnDef>,
    /// `FnId → resolved_fn_defs index` for O(1) lookup by stable
    /// identity. Built once during [`Self::build`]; mirrors the
    /// FnKey/FnId-keyed pattern in
    /// [`crate::codegen::CodegenContext::resolve_fn_def`] (PR 9.3a).
    fn_id_to_idx: HashMap<FnId, usize>,
}

impl WasmGcLinkedView {
    /// Build the view from post-flatten items + the `&FnDef` list
    /// the caller already extracted from them.
    ///
    /// Wasm-gc emits a single flattened module — `dep_modules` is
    /// empty by construction in [`SymbolTable::build`]; the
    /// flattened items carry every dep fn under prefixed names.
    pub(super) fn build(
        items: &[TopLevel],
        fn_defs: &[&FnDef],
        capability_wit_plan: Option<&crate::codegen::wasip2::CapabilityWitPlan>,
    ) -> Result<Self, WasmGcError> {
        let mut symbol_table = SymbolTable::build(items, &[]);
        if let Some(plan) = capability_wit_plan {
            symbol_table.merge_capability_wit_plan(plan);
        }
        let mut resolve_ctx = crate::ir::hir::ResolveCtx::new(&symbol_table);
        // Same current-module context every other resolution site uses
        // (`resolve_program`, the Rust and Dafny lifters, `CodegenContext`):
        // a program that declares `module Local` may spell its own members
        // qualified, and `ResolveCtx::resolve_fn_id` only probes the entry
        // scope for `Local.f` when it knows `Local` IS the current module.
        // Without this, a self-qualified call resolved to `Unresolved` here
        // and lowered to `unreachable` — while every other backend called
        // it fine.
        resolve_ctx.current_module = items.iter().find_map(|i| match i {
            TopLevel::Module(m) => Some(m.name.clone()),
            _ => None,
        });
        let resolved_fn_defs: Vec<ResolvedFnDef> = fn_defs
            .iter()
            .filter_map(|fd| resolve_fn_def_external(&resolve_ctx, fd))
            .collect();
        if resolved_fn_defs.len() != fn_defs.len() {
            return Err(WasmGcError::Validation(format!(
                "wasm-gc resolver dropped {} of {} fn defs — typecheck must run before codegen",
                fn_defs.len() - resolved_fn_defs.len(),
                fn_defs.len()
            )));
        }
        let fn_id_to_idx: HashMap<FnId, usize> = resolved_fn_defs
            .iter()
            .enumerate()
            .map(|(i, rfd)| (rfd.fn_id, i))
            .collect();
        Ok(Self {
            symbol_table,
            resolved_fn_defs,
            fn_id_to_idx,
        })
    }

    /// Stable identity lookup — preferred over the linear
    /// `resolved_fn_defs.iter().find(|rfd| rfd.name == ...)` shape.
    /// Returns `None` for FnIds the resolver pass didn't lift,
    /// which today only happens on typecheck-rejected programs.
    #[allow(dead_code)]
    pub(super) fn fn_def_by_id(&self, fn_id: FnId) -> Option<&ResolvedFnDef> {
        self.fn_id_to_idx
            .get(&fn_id)
            .map(|&i| &self.resolved_fn_defs[i])
    }
}
