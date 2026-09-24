//! Subtype-encoded oracle bounds for proof export.
//!
//! Aver runtime guarantees per-classified-effect invariants — `Random.int`
//! returns `Result.Ok` in `[min, max]` for a valid host-representable range,
//! `Random.float` in `[0.0, 1.0]`, `Time.unixMs`
//! is non-negative. Earlier 0.13 attempts emitted these as standalone
//! `axiom ∀ rng, bounds` blocks, which is logically unsound: in Lean,
//! a user could pick `rng = fun _ _ _ _ => max + 1` and derive `False`
//! from the postulate.
//!
//! The sound shape is a **subtype type definition** that pairs the
//! function with its bound proof:
//!
//! ```lean
//! def RandomIntInBounds : Type :=
//!   { f : BranchPath → Int → Int → Int → Except String Int
//!     // ∀ p n min max, i64Min ≤ min ∧ max ≤ i64Max ∧ min ≤ max →
//!        ∃ value, f p n min max = Except.ok value ∧
//!          min ≤ value ∧ value ≤ max }
//! ```
//!
//! These are *types*, not postulates. Defining them adds no logical
//! claim — a value of type `RandomIntInBounds` is a *function plus a
//! proof*, and the user constructs one explicitly when they need the
//! bound in their own theorem (using `decide` for concrete stubs, or
//! the runtime trust assumption for the live oracle). Lifted fn
//! signatures in the emitted proof stay plain function types in 0.13;
//! the subtype types are available as opt-in helpers, not enforced
//! contracts. Auto-threading them through every lifted spec is the
//! 0.14 follow-up.
//!
//! Naming convention (stable across releases):
//!   `<Effect>Oracle`        — the plain function type, alias for
//!                              parser-friendly use sites.
//!   effect-specific suffix  — the constrained carrier states the law
//!                              (`InBounds`, `Nonneg`, `Monotonic`, ...).

use crate::codegen::common::DeclaredEffects;

/// Runtime invariant carried by an oracle subtype/predicate in proof export.
///
/// The older name "bounded subtype" stopped describing the set once
/// `Process.stopRequested` added a cross-call monotonicity law. This enum is
/// now the shared classification; backend helpers only select the spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OracleSubtypeKind {
    RandomIntInBounds,
    RandomFloatInUnit,
    TimeUnixMsNonneg,
    ProcessStopRequestedMonotonic,
}

impl OracleSubtypeKind {
    pub fn for_effect(effect: &str) -> Option<Self> {
        match effect {
            "Random.int" => Some(Self::RandomIntInBounds),
            "Random.float" => Some(Self::RandomFloatInUnit),
            "Time.unixMs" => Some(Self::TimeUnixMsNonneg),
            "Process.stopRequested" => Some(Self::ProcessStopRequestedMonotonic),
            _ => None,
        }
    }

    pub const fn lean_type_name(self) -> &'static str {
        match self {
            Self::RandomIntInBounds => "RandomIntInBounds",
            Self::RandomFloatInUnit => "RandomFloatInUnit",
            Self::TimeUnixMsNonneg => "TimeUnixMsNonneg",
            Self::ProcessStopRequestedMonotonic => "ProcessStopRequestedMonotonic",
        }
    }
}

/// True when the effect has any runtime-invariant oracle carrier in proof
/// export. Call sites project `.val` from these carriers.
pub fn has_oracle_subtype(effect: &str) -> bool {
    OracleSubtypeKind::for_effect(effect).is_some()
}

/// Lean 4 helper type definitions for every classified effect declared
/// in the program. Empty when the program declares no relevant effects.
pub(crate) fn lean_subtypes(declared: &DeclaredEffects) -> String {
    let mut out = String::new();
    let mut emitted_any = false;

    let mut push_block = |body: &str| {
        if !emitted_any {
            out.push_str(
                "-- Oracle-invariant helper types. These are *type definitions*,\n\
                 -- not axioms — instantiating one requires a proof of the\n\
                 -- bound. User-side theorems that need the bound on a stub\n\
                 -- can construct an instance via `⟨stub, by decide⟩` for\n\
                 -- concrete cases, or rely on the runtime trust assumption\n\
                 -- documented in the header above for the live oracle.\n\n",
            );
            emitted_any = true;
        }
        out.push_str(body);
        out.push('\n');
    };

    if declared.includes("Random.int") {
        push_block(
            "abbrev RandomIntOracle := BranchPath → Int → Int → Int → Except String Int\n\
             \n\
             def RandomIntInBounds : Type :=\n  \
               { f : RandomIntOracle //\n    \
                 ∀ (path : BranchPath) (n min max : Int),\n      \
                 (-9223372036854775808 : Int) ≤ min ∧\n      \
                 max ≤ (9223372036854775807 : Int) ∧ min ≤ max →\n      \
                 ∃ value : Int, f path n min max = Except.ok value ∧\n        \
                   min ≤ value ∧ value ≤ max }\n\
             \n\
             noncomputable def RandomIntInBounds.valueAt\n    \
                 (rnd : RandomIntInBounds) (path : BranchPath) (n min max : Int)\n    \
                 (valid : (-9223372036854775808 : Int) ≤ min ∧\n      \
                   max ≤ (9223372036854775807 : Int) ∧ min ≤ max) : Int :=\n  \
               Classical.choose (rnd.property path n min max valid)\n\
             \n\
             @[simp] theorem RandomIntInBounds.result_eq\n    \
                 (rnd : RandomIntInBounds) (path : BranchPath) (n min max : Int)\n    \
                 (valid : (-9223372036854775808 : Int) ≤ min ∧\n      \
                   max ≤ (9223372036854775807 : Int) ∧ min ≤ max) :\n    \
                 rnd.val path n min max =\n      \
                   Except.ok (rnd.valueAt path n min max valid) := by\n  \
               exact (Classical.choose_spec (rnd.property path n min max valid)).1\n",
        );
    }
    if declared.includes("Random.float") {
        push_block(
            "abbrev RandomFloatOracle := BranchPath → Int → Float\n\
             \n\
             def RandomFloatInUnit : Type :=\n  \
               { f : RandomFloatOracle //\n    \
                 ∀ (path : BranchPath) (n : Int),\n      \
                 0.0 ≤ f path n ∧ f path n ≤ 1.0 }\n",
        );
    }
    if declared.includes("Time.unixMs") {
        push_block(
            "abbrev TimeUnixMsOracle := BranchPath → Int → Int\n\
             \n\
             def TimeUnixMsNonneg : Type :=\n  \
               { f : TimeUnixMsOracle //\n    \
                 ∀ (path : BranchPath) (n : Int), 0 ≤ f path n }\n",
        );
    }
    if declared.includes("Process.stopRequested") {
        push_block(
            "abbrev ProcessStopRequestedOracle := BranchPath → Int → Bool\n\
             \n\
             def ProcessStopRequestedMonotonic : Type :=\n  \
               { f : ProcessStopRequestedOracle //\n    \
                 ∀ (path : BranchPath) (i j : Int),\n      \
                 i ≤ j → f path i = true → f path j = true }\n",
        );
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn declared(methods: &[&str]) -> DeclaredEffects {
        DeclaredEffects {
            bare_namespaces: HashSet::new(),
            methods: methods.iter().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn lean_emits_no_axiom_declaration() {
        let d = declared(&["Random.int", "Random.float", "Time.unixMs"]);
        let out = lean_subtypes(&d);
        // The whole point of this module is no axioms. Any axiom slip
        // would re-introduce the 0.13-pre soundness hole. Match a
        // line-start `axiom ` declaration, not the literal word in
        // commentary.
        let any_axiom_decl = out
            .lines()
            .any(|line| line.trim_start().starts_with("axiom "));
        assert!(
            !any_axiom_decl,
            "subtype helpers must not emit `axiom` declarations; got:\n{}",
            out
        );
    }

    #[test]
    fn lean_emits_subtype_for_random_int() {
        let d = declared(&["Random.int"]);
        let out = lean_subtypes(&d);
        assert!(out.contains("RandomIntInBounds"));
        assert!(out.contains("Except String Int"));
        assert!(out.contains("f path n min max = Except.ok value"));
    }

    #[test]
    fn lean_empty_when_no_relevant_effects() {
        let d = declared(&["Args.get"]); // Args.get has no bound to encode.
        assert!(lean_subtypes(&d).is_empty());
    }

    #[test]
    fn process_uses_a_cross_call_monotonicity_carrier() {
        let d = declared(&["Process.stopRequested"]);
        let lean = lean_subtypes(&d);
        assert!(lean.contains("ProcessStopRequestedMonotonic"));
        assert!(lean.contains("i ≤ j → f path i = true → f path j = true"));

        let kind = OracleSubtypeKind::for_effect("Process.stopRequested")
            .expect("Process invariant classification");
        assert_eq!(kind.lean_type_name(), "ProcessStopRequestedMonotonic");
        assert!(has_oracle_subtype("Process.stopRequested"));
    }
}
