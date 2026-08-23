//! Canonical resolved view of the program for codegen consumers.
//!
//! Single source of truth for "what does the program look like, after
//! name resolution, with module/entry scope preserved". Backends that
//! used to walk `CodegenContext.{items, fn_defs, type_defs}` (raw
//! `ast::*`) should iterate this view's resolved fn defs instead. The
//! AST-shape fields on `CodegenContext` remain available as source
//! metadata (spans, diagnostics, syntax-discovery), not as the
//! authoritative backend input.
//!
//! # Current Typed HIR boundary (post epic #180)
//!
//! Aver's **Typed HIR** is a query layer, not a duplicate IR enum. Its
//! two structural primitives — what backends mean when they reach for
//! "the typed program" — are:
//!
//! 1. [`ResolvedProgramView`] (this struct). The canonical post-name-
//!    resolution program: every `FnDef` projected through
//!    [`crate::ir::hir::ResolvedFnDef`] with typed
//!    `params: Vec<(String, Type)>` and `return_type: Type`, indexed
//!    by opaque [`crate::ir::FnId`]. Backends ask
//!    "what does this fn look like?" through `fn_by_id` or
//!    `entry_fns()` / `module_fns(prefix)` and get typed answers.
//! 2. Stamped [`crate::ast::Spanned`]`<`[`crate::ir::hir::ResolvedExpr`]`>`.
//!    Every reachable body expression in a well-typed program has
//!    `.ty().is_some()` after the typechecker stamps and the
//!    resolver propagates. Backends read inferred types per node
//!    without re-running inference.
//!
//! Together they answer "what name does this refer to?" (resolved
//! HIR via `FnId` / `TypeId` / `CtorId`) and "what type does this
//! expression have?" (typed slot via `Spanned::ty()`).
//!
//! Conscious exceptions still walking `ast::*` directly (each
//! documented at the call site):
//!
//! - **`verify_law` helper walkers** ([`crate::verify_law`]). The
//!   helper-law / contextual-helper hint walkers operate on
//!   entry-only AST `FnDef`s through [`crate::verify_law::EntryFnIndex`].
//!   Module-scoped verify export now ships, so this is active migration
//!   debt tracked by #1087 rather than a deferred boundary.
//! - **Lean / Dafny proof + law spec emitters**. Several recognisers
//!   (`emit_*_spec_equivalence_law`, `direct_call` AST shape match)
//!   keep walking `Spanned<Expr>` source-shape because the proof IR
//!   is keyed on syntactic surfaces the user wrote, not the resolver-
//!   canonicalised form. `emit_expr_legacy` is the explicit adapter.
//! - **wasm-gc post-link view** ([`crate::codegen::wasm_gc::view`]).
//!   The wasm-gc backend collapses N modules into one emit unit via
//!   `flatten_multimodule`, then rebuilds its own
//!   [`crate::ir::SymbolTable`] + resolver pass over the flattened
//!   slice. The post-link namespace is the identity layer in that
//!   backend; the documented `backend-link-stage` category covers it.
//!
//! Trigger-driven follow-up (NOT scoped to #180):
//!
//! - **MIR / Core IR**. Per-expression effects, SSA / CFG / explicit
//!   ownership / effect tracking per instruction belong to a separate
//!   epic. Typed HIR is the substrate they lower from; build the IR
//!   when an inliner / monomorphiser / cross-scope optimiser needs it.
//! - **`TypedProgramView` wrapper**. Combining `resolved_program`'s
//!   `FnId` index with the typed expression slot into one query
//!   surface (`view.fn_by_id(id).body_expr_type(span)` or similar)
//!   would tighten the contract but adds no semantic value today —
//!   wait for a real consumer.
//! - **FnId-keyed `verify_law` helpers**. Dependency-owned verify blocks
//!   are now exported under module scope; remove `EntryFnIndex` and resolve
//!   helper-hint subjects through the canonical program view (#1087).
//!
//! ## Construction
//!
//! Built once at the codegen boundary:
//! - **Entry slice** projects from `PipelineResult.resolved_items` —
//!   the resolver pass populates this unconditionally, so we just
//!   pick out the `ResolvedTopLevel::FnDef` variants and clone the
//!   resolved fn defs out of them. No re-resolve.
//! - **Module slice** resolves each dep module's `&[FnDef]` through
//!   `resolve_fn_def_external` under a `ResolveCtx` pinned to that
//!   module's prefix. This is the only producer of resolved module
//!   bodies — `CodegenContext.resolved_module_fn_defs` is a
//!   projection / cache of this view, not an independent source.
//!
//! ## Lookups
//!
//! The view exposes `fn_by_id(FnId)` so callsites that have an opaque
//! `FnId` in hand (typical for identity-sensitive emit) can recover
//! the resolved body without round-tripping through bare names.
//! Iteration is provided per-scope: `entry_fns()` walks the entry
//! slice; `module_fns(prefix)` walks one dep module's slice.
//!
//! Epic #170 Phase 1: this is the foundation every later phase
//! builds on. Backends migrate to consuming the view as primary
//! input in subsequent PRs; this PR only consolidates the producer
//! side without touching backend signatures.

use std::collections::HashMap;

use crate::codegen::ModuleInfo;
use crate::ir::SymbolTable;
use crate::ir::hir::{ResolveCtx, ResolvedFnDef, ResolvedTopLevel, resolve_fn_def_external};

/// Resolved-form mirror of one dep module's fn defs, with the
/// module's prefix preserved so per-scope iteration is cheap and the
/// view can build an `FnId`-keyed lookup without losing scope.
#[derive(Debug, Clone, Default)]
pub struct ResolvedModuleFns {
    /// Module prefix (e.g. `"Models.User"`). Same value as
    /// `ModuleInfo.prefix` on the AST side.
    pub prefix: String,
    /// Resolved fn defs in module source order — position-aligned
    /// with `ModuleInfo.fn_defs` for the rare consumer that needs
    /// to pair AST and resolved forms side-by-side.
    pub fn_defs: Vec<ResolvedFnDef>,
}

/// Canonical resolved-program view consumed by codegen.
///
/// Holds the entry-scope items (post-pipeline, post-NameResolve) plus
/// each dep module's resolved fn defs, with an `FnId` index so
/// identity-keyed lookups don't walk linearly.
#[derive(Debug, Clone, Default)]
pub struct ResolvedProgramView {
    /// Entry-scope resolved items, exactly as the pipeline produced
    /// them. Includes `FnDef`, `Module` markers, and `Passthrough`
    /// variants (verify/decision/typedef nodes that haven't been
    /// promoted to resolved form yet — see [`ResolvedTopLevel`]).
    pub entry_items: Vec<ResolvedTopLevel>,
    /// Per-dep-module resolved fn defs. Order matches the input
    /// `Vec<ModuleInfo>` so existing position-keyed consumers still
    /// work during migration.
    pub modules: Vec<ResolvedModuleFns>,
    /// `FnId → (scope, ResolvedFnDef index)` index. The opaque
    /// `FnId` keys (built by `SymbolTable` at pipeline head) are the
    /// canonical identity primitive — every consumer that has an
    /// `FnId` in hand should reach this map instead of bare-name
    /// matching against `entry_items`.
    fn_index: HashMap<crate::ir::FnId, FnIndexEntry>,
}

#[derive(Debug, Clone, Copy)]
struct FnIndexEntry {
    /// `None` = entry scope; `Some(i)` = `modules[i]`.
    module: Option<usize>,
    /// Position inside the owning slice's `fn_defs`.
    pos: usize,
}

impl ResolvedProgramView {
    /// Build the view from a pipeline-produced entry slice and the
    /// `Vec<ModuleInfo>` carrying dep modules' AST fn defs. The
    /// caller has already run `pipeline::run` (or equivalent) and is
    /// passing through the canonical `resolved_items` — we do not
    /// re-resolve the entry side. Module fn defs are resolved here
    /// (the pipeline doesn't walk them), pinning
    /// `ResolveCtx.current_module` to each module's prefix so a
    /// dotted reference inside one module's body resolves through
    /// that module's import set first.
    pub fn build(
        entry_items: Vec<ResolvedTopLevel>,
        modules: &[ModuleInfo],
        symbol_table: &SymbolTable,
    ) -> Self {
        let module_views: Vec<ResolvedModuleFns> = modules
            .iter()
            .map(|module| {
                let mut rctx = ResolveCtx::new(symbol_table);
                rctx.current_module = Some(module.prefix.clone());
                let fn_defs = module
                    .fn_defs
                    .iter()
                    .filter_map(|fd| resolve_fn_def_external(&rctx, fd))
                    .collect();
                ResolvedModuleFns {
                    prefix: module.prefix.clone(),
                    fn_defs,
                }
            })
            .collect();

        let mut fn_index: HashMap<crate::ir::FnId, FnIndexEntry> = HashMap::new();
        for (pos, item) in entry_items.iter().enumerate() {
            if let ResolvedTopLevel::FnDef(rfd) = item {
                fn_index.insert(rfd.fn_id, FnIndexEntry { module: None, pos });
            }
        }
        for (i, m) in module_views.iter().enumerate() {
            for (pos, rfd) in m.fn_defs.iter().enumerate() {
                fn_index.insert(
                    rfd.fn_id,
                    FnIndexEntry {
                        module: Some(i),
                        pos,
                    },
                );
            }
        }

        Self {
            entry_items,
            modules: module_views,
            fn_index,
        }
    }

    /// Resolved entry-scope fn defs in source order — the same set
    /// `CodegenContext.resolved_fn_defs` projected today, just
    /// reached through the canonical view.
    pub fn entry_fns(&self) -> impl Iterator<Item = &ResolvedFnDef> + '_ {
        self.entry_items.iter().filter_map(|item| match item {
            ResolvedTopLevel::FnDef(rfd) => Some(rfd),
            _ => None,
        })
    }

    /// Resolved fn defs from one dep module, in source order.
    /// Returns an empty iterator when no module with that prefix is
    /// present — callers that need an existence check should use
    /// [`Self::module`] instead.
    pub fn module_fns(&self, prefix: &str) -> impl Iterator<Item = &ResolvedFnDef> + '_ {
        self.modules
            .iter()
            .find(|m| m.prefix == prefix)
            .into_iter()
            .flat_map(|m| m.fn_defs.iter())
    }

    /// Whole module slice (prefix + resolved fns) for callsites that
    /// need the prefix alongside.
    pub fn module(&self, prefix: &str) -> Option<&ResolvedModuleFns> {
        self.modules.iter().find(|m| m.prefix == prefix)
    }

    /// `FnId`-keyed lookup. Returns the resolved fn def regardless
    /// of scope — identity-sensitive consumers (proof emitter,
    /// inliner, monomorph) reach this rather than walking entry +
    /// modules manually.
    pub fn fn_by_id(&self, id: crate::ir::FnId) -> Option<&ResolvedFnDef> {
        let entry = *self.fn_index.get(&id)?;
        match entry.module {
            None => {
                let item = self.entry_items.get(entry.pos)?;
                match item {
                    ResolvedTopLevel::FnDef(rfd) => Some(rfd),
                    _ => None,
                }
            }
            Some(i) => self.modules.get(i)?.fn_defs.get(entry.pos),
        }
    }

    /// First fn (entry first, then dep modules in declaration order)
    /// whose source-level name matches. Stage 5+ of #232 uses this to
    /// bridge ad-hoc detectors that historically searched by string
    /// name (`is_directly_recursive` in dafny codegen) into the typed
    /// `FnId`-keyed `ProgramShape`. Returns `None` if no such fn
    /// exists in the resolved view.
    ///
    /// Note: bare-name lookup is ambiguous when the same name lives
    /// in two dep modules. Entry-first ordering matches what the
    /// legacy string-name detectors already assumed.
    pub fn fn_by_name(&self, name: &str) -> Option<&ResolvedFnDef> {
        for fd in self.entry_fns() {
            if fd.name == name {
                return Some(fd);
            }
        }
        for m in &self.modules {
            for fd in &m.fn_defs {
                if fd.name == name {
                    return Some(fd);
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FnBody, FnDef, Module, Spanned, TopLevel};
    use crate::codegen::ModuleInfo;
    use crate::ir::SymbolTable;

    fn mk_fn(name: &str) -> FnDef {
        FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: std::sync::Arc::new(FnBody::Block(vec![crate::ast::Stmt::Expr(Spanned::bare(
                crate::ast::Expr::Literal(crate::ast::Literal::Int(0)),
            ))])),
            resolution: None,
        }
    }

    fn mk_module(prefix: &str, fn_names: &[&str]) -> ModuleInfo {
        ModuleInfo {
            prefix: prefix.to_string(),
            depends: vec![],
            exposes: vec![],
            exposes_opaque: vec![],
            type_defs: vec![],
            fn_defs: fn_names.iter().map(|n| mk_fn(n)).collect(),
            capability_items: vec![],
            capability_semantics: None,
            verify_blocks: vec![],
            verify_laws: vec![],
            analysis: None,
        }
    }

    #[test]
    fn view_indexes_entry_fns_by_fn_id() {
        let entry_items = vec![TopLevel::FnDef(mk_fn("foo")), TopLevel::FnDef(mk_fn("bar"))];
        let symbol_table = SymbolTable::build(&entry_items, &[]);
        let resolved = crate::ir::hir::resolve_program(&symbol_table, &entry_items);
        let view = ResolvedProgramView::build(resolved, &[], &symbol_table);

        assert_eq!(view.entry_fns().count(), 2);
        let foo_id = symbol_table
            .fn_id_of(&crate::ir::FnKey::entry("foo"))
            .expect("foo entry FnId");
        let bar_id = symbol_table
            .fn_id_of(&crate::ir::FnKey::entry("bar"))
            .expect("bar entry FnId");
        assert_eq!(view.fn_by_id(foo_id).map(|f| f.name.as_str()), Some("foo"));
        assert_eq!(view.fn_by_id(bar_id).map(|f| f.name.as_str()), Some("bar"));
    }

    #[test]
    fn cross_module_same_bare_name_disambiguates_by_fn_id() {
        // The whole point of this view: two modules with a `walker`
        // fn must NOT collide. The `FnId` lookup picks the right
        // resolved body for each.
        let entry_items: Vec<TopLevel> = vec![TopLevel::Module(Module {
            name: "Entry".to_string(),
            line: 1,
            depends: vec!["A".to_string(), "B".to_string()],
            exposes: vec![],
            exposes_opaque: vec![],
            exposes_line: None,
            intent: "Test".to_string(),
            effects: None,
            effects_line: None,
            kind: None,
            kind_line: None,
            semantics: None,
            semantics_line: None,
        })];
        let modules = vec![mk_module("A", &["walker"]), mk_module("B", &["walker"])];
        let symbol_table = SymbolTable::build(&entry_items, &modules);
        let resolved = crate::ir::hir::resolve_program(&symbol_table, &entry_items);
        let view = ResolvedProgramView::build(resolved, &modules, &symbol_table);

        let a_id = symbol_table
            .fn_id_of(&crate::ir::FnKey::in_module("A".to_string(), "walker"))
            .expect("A.walker FnId");
        let b_id = symbol_table
            .fn_id_of(&crate::ir::FnKey::in_module("B".to_string(), "walker"))
            .expect("B.walker FnId");
        assert_ne!(a_id, b_id, "FnIds must be distinct across modules");

        let a_walker = view.fn_by_id(a_id).expect("A.walker present");
        let b_walker = view.fn_by_id(b_id).expect("B.walker present");
        assert_eq!(a_walker.name, "walker");
        assert_eq!(b_walker.name, "walker");
        assert_eq!(a_walker.fn_id, a_id);
        assert_eq!(b_walker.fn_id, b_id);
        assert_eq!(view.module_fns("A").count(), 1);
        assert_eq!(view.module_fns("B").count(), 1);
    }
}
