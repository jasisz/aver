//! `MirProgram` + `MirFn` — the top-level shape of Core MIR.
//!
//! Phase 2a of #252. The data model is structured (not flat /
//! basic-block), expression-based (every `MirExpr` is a value), and
//! identity-typed (declaration refs go through `FnId` / `TypeId` /
//! `CtorId`). See [`super::RFC.md`] for the full rationale.

use std::collections::HashMap;

use crate::ir::{FnId, ModuleId};

use crate::ast::Spanned;

use super::expr::{MirEffectAnnotation, MirExpr};
use super::stats::LowerStats;

/// A whole compiled program in Core MIR. Keyed by `FnId` (stable
/// across compilation units; same identity layer the rest of the
/// pipeline consumes). Phase 2a builds an empty `MirProgram`
/// directly; Phase 3 grows the lowering that populates it from
/// `ResolvedProgramView`.
#[derive(Debug, Clone, Default)]
pub struct MirProgram {
    /// All compiled functions, keyed by their `FnId`.
    pub fns: HashMap<FnId, MirFn>,
    /// Optional module identity for tooling that wants to walk
    /// `MirProgram` per source module (LSP outline, future
    /// per-module IR dump). Empty when the program was assembled
    /// from a single entry file with no `depends [...]`.
    pub modules: Vec<ModuleId>,
    /// Coverage telemetry for the lowering that produced this
    /// program. `lowered + skipped.values().sum()` equals the
    /// number of `ResolvedFnDef` items the lowerer saw — see
    /// [`super::stats::LowerStats`] for the coverage gate that
    /// Phase 4 (VM slice) consumes.
    pub stats: LowerStats,
    /// Phase 6 wave 11 — interned built-in fn names. Indexed by
    /// `BuiltinId`. Lowering grows this lazily as it encounters
    /// each unique `ResolvedCallee::Builtin(name)` shape; MIR
    /// consumers (VM walker, Rust walker, optimize passes) look
    /// up the canonical string via `program.builtin_name(id)`
    /// instead of carrying the string inline on every
    /// `MirCallee::Builtin`.
    pub builtins: Vec<String>,
    /// `true` when this program is a *fragment* of a larger build whose
    /// other parts are compiled separately — specifically a dependency
    /// module under the VM's per-module compile (the VM lowers each
    /// `depends [...]` module to its own `MirProgram`). Such a fragment
    /// does NOT see the entry/sibling call sites, so whole-program
    /// analyses that rely on "every caller is visible" (notably
    /// `own_param_refine`) must bail. `false` for a whole-program compile
    /// (single entry, or the flattened wasm-gc / Rust builds), where all
    /// callers are present and graduation is sound.
    pub external_callers_possible: bool,
}

impl MirProgram {
    /// Construct an empty program. Phase 3 lowering will populate
    /// `fns` and `modules` as it walks `ResolvedProgramView`.
    pub fn empty() -> Self {
        Self::default()
    }

    /// Lookup a function by its `FnId`. Returns `None` if the id
    /// wasn't part of this compilation (e.g. a stale id surviving
    /// from a previous program view).
    pub fn fn_by_id(&self, id: FnId) -> Option<&MirFn> {
        self.fns.get(&id)
    }

    /// All functions in arbitrary (`HashMap`) order. Callers that
    /// need a stable walk (snapshot tests, dump output) should sort
    /// by `FnId` themselves.
    pub fn iter(&self) -> impl Iterator<Item = (&FnId, &MirFn)> {
        self.fns.iter()
    }

    /// Phase 6 wave 11 — look up the canonical name behind a
    /// `BuiltinId` minted by this program's lowering. Returns
    /// `""` (empty slice) if the id is out of range — a defensive
    /// fallback for diagnostic paths; consumers in the hot path
    /// should hold valid ids by construction.
    pub fn builtin_name(&self, id: crate::ir::BuiltinId) -> &str {
        self.builtins
            .get(id.0 as usize)
            .map(String::as_str)
            .unwrap_or("")
    }

    /// Phase 6 wave 11 — intern a built-in fn name into this
    /// program's table. Returns the stable `BuiltinId`, reusing
    /// the existing slot when the name has already been
    /// registered. Lowering calls this; downstream optimizer
    /// passes inherit the existing ids when they construct new
    /// `MirCallee::Builtin` instances (e.g. from inlining).
    pub fn intern_builtin(&mut self, name: &str) -> crate::ir::BuiltinId {
        for (idx, existing) in self.builtins.iter().enumerate() {
            if existing == name {
                return crate::ir::BuiltinId(idx as u32);
            }
        }
        let id = crate::ir::BuiltinId(self.builtins.len() as u32);
        self.builtins.push(name.to_string());
        id
    }
}

/// One function in Core MIR. Body is a single `MirExpr` — MIR is
/// expression-based, so the function's body is the expression that
/// produces its return value. `Let` chains within the body bind
/// intermediate results; there's no separate "block" or
/// "terminator" concept at this phase.
#[derive(Debug, Clone)]
pub struct MirFn {
    /// Stable identity for this function. Matches the `FnId` issued
    /// by `SymbolTable` during the existing pipeline; downstream
    /// consumers can cross-reference `ProofIR` / `ProgramShape` by
    /// the same id.
    pub fn_id: FnId,
    /// Source-level name. Carried for dumps + diagnostics; backends
    /// must not key off this string for identity — that's what
    /// `fn_id` is for.
    pub name: String,
    /// Parameters in declaration order. Each gets a fresh `LocalId`
    /// at lowering time so the body can refer to it.
    pub params: Vec<MirParam>,
    /// Source-level return type annotation as it appears on the fn
    /// signature. Phase 4 backends consume this for VM stack-slot
    /// layout; later phases may replace it with a richer type
    /// representation when the typechecker's `Type` enum becomes
    /// the universal vocabulary.
    pub return_type: String,
    /// Declared effects (`! [Namespace.method, ...]`). Function-
    /// level only — per-call-site effect annotation is deferred to
    /// the Phase 6 optimizer track per the RFC.
    pub effects: Vec<MirEffectAnnotation>,
    /// The single expression that, when evaluated, produces the
    /// function's return value. Wrapped in `Spanned` so the body's
    /// own source location stays available to dumps / diagnostics
    /// (the surrounding `MirFn` has its own identity layer via
    /// `fn_id`, but the body span is needed for sub-expression
    /// errors that don't carry a closer one).
    pub body: Spanned<MirExpr>,
    /// Number of frame slots the lowered body needs: the resolver's
    /// `local_count` plus any synthetic slots minted for opaque-let
    /// temps during stmt-chain lowering. The VM walker reserves this
    /// many slots; using the resolver's count alone underruns the
    /// frame when the body stores into a synthetic slot.
    pub local_count: u32,
    /// Per-slot alias-proneness, indexed by `LocalId`/slot: `true`
    /// when the resolver's alias analysis flagged the slot as possibly
    /// sharing its backing engine array/struct with another binding.
    /// Backends combine it with a node's `last_use` to gate the
    /// owned-mutate fast path (`owned = last_use && !aliased`) — the
    /// VM's owned-mask and the wasm-gc clone-on-write skip. Carried on
    /// the MIR fn so MIR consumers read this ownership fact off MIR
    /// instead of reaching back into the AST `FnResolution`
    /// side-channel. Cloned from the resolver at lowering today; a
    /// later phase recomputes it as a MIR analysis pass. An
    /// out-of-range slot reads `false` (not aliased → fast path sound),
    /// matching the resolver tables.
    pub aliased_slots: std::sync::Arc<Vec<bool>>,
}

/// One formal parameter. The `LocalId` is assigned at lowering
/// time and is the binding the function body refers to.
#[derive(Debug, Clone)]
pub struct MirParam {
    /// Local binding introduced for this parameter. The function
    /// body refers to it via `MirExpr::Local(LocalId)`.
    pub local: LocalId,
    /// Source-level parameter name. For dumps + diagnostics only.
    pub name: String,
    /// Source-level type annotation. Same caveat as `MirFn::return_type`.
    pub ty: String,
}

/// Local binding identifier. Unique per function body (not per
/// program) — backends look up the binding by walking the body's
/// `Let` chain, so collision across functions isn't a concern.
///
/// Phase 2 picks "assign at MIR construction time" over "carry the
/// HIR slot index" (the latter was the strawman alternative listed
/// in the RFC). The carry-from-HIR option would have meant MIR's
/// local space matches whatever the resolver chose — fine for the
/// VM consumer, but the future inliner / monomorphizer needs the
/// freedom to introduce fresh locals during optimization passes,
/// and inheriting HIR's numbering would silently overlap with
/// those.
///
/// **Seed during Phase 3 (waves 1-3b):** the lowerer seeds new
/// `LocalId`s from the resolver's slot indices
/// (`ResolvedExpr::Resolved { slot, .. }`, `FnResolution.local_slots`,
/// `ResolvedMatchArm.binding_slots`) and grows synthetic locals
/// from `local_count` upward for `Stmt::Expr` intermediates the
/// resolver didn't see. The slot space is unique per function so
/// reuse is safe. Later optimizer passes are still free to
/// renumber — `LocalId` is just an opaque identity per body, not
/// a promise about resolver lineage.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LocalId(pub u32);

impl std::fmt::Display for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}
