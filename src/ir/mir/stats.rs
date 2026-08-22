//! `LowerStats` + `SkipReason` — coverage telemetry for the
//! HIR → MIR lowerer.
//!
//! Phase 3 lowers `ResolvedProgramView` into `MirProgram` in
//! widening waves. Until the lowerer covers everything, fns
//! containing unsupported shapes get dropped from
//! `MirProgram.fns` — `MirProgram` is "what MIR could lower so
//! far", not "what HIR contained". Without telemetry, that drop
//! is invisible: dump renders, tests pass on the supported
//! subset, but Phase 4 (VM slice) could land thinking everything
//! is wired when in fact 60% of the corpus silently disappeared.
//!
//! `LowerStats` is the gate. It rides on `MirProgram` so any
//! consumer can ask "how complete is this lowering". Each
//! skipped fn bumps a counter keyed by `SkipReason` — the first
//! unsupported shape the lowerer hit — so widening-wave PRs can
//! prove that the reason set shrinks.
//!
//! ## Coverage contract (Phase 3 → 4 gate)
//!
//! - Every dropped fn must produce exactly one `SkipReason`. No
//!   silent drops, no double-counting.
//! - The Phase 4 (VM slice) entry gate asserts a corpus-wide
//!   coverage ratio (lowered / total ≥ target). Each wave PR
//!   nudges the target up.

use std::collections::HashMap;

/// Why a single `ResolvedFnDef` failed to lower.
///
/// One variant per source-level reason — the dominant unsupported
/// shape inside the fn body. Wave PRs sequentially remove
/// reasons from the live set; once the set is empty the lowerer
/// is corpus-complete on the shipped examples.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SkipReason {
    /// `ResolvedFnDef.body` had no statements. Frontend should
    /// have rejected this earlier; recorded for safety.
    EmptyBody,
    /// Single-stmt body whose only statement is a binding (no
    /// tail expression). Well-typed Aver can't produce this, but
    /// the lowerer guards anyway.
    BindingOnlyTail,
    /// Multi-stmt body, `FnResolution` not populated by the
    /// resolver pass — usually means the fn wasn't visited.
    MissingResolution,
    /// Binding referenced by name didn't resolve to a slot via
    /// `FnResolution.local_slots`. Indicates a resolver gap.
    BindingSlotLookupMissing,
    /// `Match` arm's `binding_slots` had fewer slots than the
    /// pattern's preorder binding count. Indicates a resolver /
    /// pattern desync.
    PatternSlotShortfall,
    /// Built-in constructor *construction* (`Result.Ok(v)`,
    /// `Option.Some(v)`, …) — wave 3c-i territory. Built-in
    /// ctors need a typed identity story beyond raw `CtorId`.
    BuiltinCtorConstruction,
    /// Built-in constructor *pattern* (`Result.Ok(v) -> ...`,
    /// `Option.Some(v) -> ...`) — same wave 3c-i territory as
    /// the construction side.
    BuiltinCtorPattern,
    /// A constructor whose `CtorId` didn't resolve. Drops the fn
    /// rather than panic on a half-resolved ctor — but never
    /// silently: the resolved body still holds the name and the line,
    /// and every backend that finds the fn missing reports them
    /// (`crate::ir::hir::collect_unresolved_in_fn`). It is NOT safe to
    /// assume the type checker already said something; it resolves a
    /// bare name through its own visibility relation and can accept
    /// what this resolver could not classify.
    UnresolvedCtor,
    /// A call target the resolver could not classify. Same family as
    /// [`Self::UnresolvedCtor`], reported the same way.
    UnsupportedCallee,
    /// `Record{...}` / `Record{base & ...}` on a built-in type
    /// (`HttpResponse`, `Header`, `Buffer`, …) with no `TypeId`.
    /// Wave 3c-i / 3c-iv territory depending on the type.
    BuiltinRecord,
    /// `Try` (the `?` propagation node) — wave 3c-ii.
    UnsupportedTry,
    /// Tail call — wave 3c-iii.
    UnsupportedTailCall,
    /// List literal — wave 3c-iv.
    UnsupportedList,
    /// Tuple literal — wave 3c-iv.
    UnsupportedTuple,
    /// Map literal — wave 3c-iv.
    UnsupportedMap,
    /// Interpolated string — wave 3c-iv.
    UnsupportedInterpolatedStr,
    /// `IndependentProduct` (`?!` / `!`) — wave 3c-v.
    UnsupportedIndependentProduct,
    /// Unresolved top-level `Ident` that survived past the
    /// resolver — usually a resolver gap. Drops the fn rather than
    /// emit a half-resolved name.
    UnresolvedIdent,
    /// A `ResolvedExpr` variant the lowerer doesn't recognise at
    /// all. Should be empty after wave 3c lands — anything
    /// here points to a missed variant.
    UnsupportedOther,
}

impl SkipReason {
    /// Short human-readable label for dumps / diagnostics.
    pub fn label(self) -> &'static str {
        match self {
            Self::EmptyBody => "empty body",
            Self::BindingOnlyTail => "binding-only tail",
            Self::MissingResolution => "missing resolution",
            Self::BindingSlotLookupMissing => "binding slot lookup missing",
            Self::PatternSlotShortfall => "pattern slot shortfall",
            Self::BuiltinCtorConstruction => "built-in ctor construction",
            Self::BuiltinCtorPattern => "built-in ctor pattern",
            Self::UnresolvedCtor => "unresolved ctor",
            Self::UnsupportedCallee => "unsupported callee",
            Self::BuiltinRecord => "built-in record type",
            Self::UnsupportedTry => "unsupported Try (`?`)",
            Self::UnsupportedTailCall => "unsupported tail call",
            Self::UnsupportedList => "unsupported list literal",
            Self::UnsupportedTuple => "unsupported tuple literal",
            Self::UnsupportedMap => "unsupported map literal",
            Self::UnsupportedInterpolatedStr => "unsupported interpolated string",
            Self::UnsupportedIndependentProduct => "unsupported IndependentProduct",
            Self::UnresolvedIdent => "unresolved Ident (resolver gap)",
            Self::UnsupportedOther => "unsupported (other)",
        }
    }
}

/// Coverage counters for a single `lower_program` call.
///
/// `lowered` = fns successfully placed into `MirProgram.fns`.
/// `skipped` = fns dropped, keyed by the first `SkipReason` the
/// lowerer hit inside that fn. Total = `lowered + skipped.values().sum()`
/// equals the number of `ResolvedFnDef` items the call saw.
#[derive(Debug, Clone, Default)]
pub struct LowerStats {
    /// Number of fns whose body lowered successfully into
    /// `MirProgram.fns`.
    pub lowered: u32,
    /// Skipped fns keyed by reason. One bump per dropped fn.
    pub skipped: HashMap<SkipReason, u32>,
}

impl LowerStats {
    /// Total fns the lowerer saw, lowered or not.
    pub fn total(&self) -> u32 {
        self.lowered + self.skipped.values().sum::<u32>()
    }

    /// Coverage ratio in `[0.0, 1.0]`. Returns `1.0` for an empty
    /// program so empty corpora don't poison the gate.
    pub fn coverage_ratio(&self) -> f64 {
        let total = self.total();
        if total == 0 {
            return 1.0;
        }
        f64::from(self.lowered) / f64::from(total)
    }

    /// Record one lowered fn.
    pub fn record_lowered(&mut self) {
        self.lowered += 1;
    }

    /// Record one dropped fn with its dominant reason.
    pub fn record_skip(&mut self, reason: SkipReason) {
        *self.skipped.entry(reason).or_insert(0) += 1;
    }

    /// Iterate `(SkipReason, count)` pairs in a stable order
    /// (variant order via `SkipReason as u8` ascending), so dumps
    /// + test assertions don't depend on `HashMap` iteration drift.
    pub fn skipped_sorted(&self) -> Vec<(SkipReason, u32)> {
        let mut entries: Vec<_> = self.skipped.iter().map(|(r, c)| (*r, *c)).collect();
        entries.sort_by_key(|(r, _)| *r as u8);
        entries
    }
}
