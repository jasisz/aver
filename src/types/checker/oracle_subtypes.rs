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
//!   `<Effect>InBounds`      — the constrained subtype carrying the
//!                              invariant proof.

use crate::codegen::common::DeclaredEffects;

/// True when the effect has a bounded-subtype carrier in the proof
/// export (`RandomIntInBounds`, `RandomFloatInUnit`, `TimeUnixMsNonneg`).
/// Used by `rewrite_effectful_calls_in_law` to decide which oracle
/// givens need `.val` projection on call sites. Keep in sync with
/// [`lean_subtypes`] / [`dafny_subtype_predicates`] and with the
/// per-backend `bounded_oracle_subtype_for` helpers.
pub fn has_bounded_subtype(effect: &str) -> bool {
    matches!(effect, "Random.int" | "Random.float" | "Time.unixMs")
}

/// Lean 4 helper type definitions for every classified effect declared
/// in the program. Empty when the program declares no relevant effects.
pub(crate) fn lean_subtypes(declared: &DeclaredEffects) -> String {
    let mut out = String::new();
    let mut emitted_any = false;

    let mut push_block = |body: &str| {
        if !emitted_any {
            out.push_str(
                "-- Bounded-oracle helper types. These are *type definitions*,\n\
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
                   min ≤ value ∧ value ≤ max }\n",
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
    out
}

/// Dafny equivalent — subset types carrying the bound predicate.
/// Dafny's idiom for "function + invariant" is a `predicate IsXxx(f)`
/// that the user threads through their own lemmas with `requires`,
/// since Dafny doesn't have first-class subtype types over function
/// values the way Lean's `Subtype` does.
pub(crate) fn dafny_subtype_predicates(declared: &DeclaredEffects) -> String {
    let mut out = String::new();
    let mut emitted_any = false;

    let mut push_block = |body: &str| {
        if !emitted_any {
            out.push_str(
                "// Bounded-oracle predicates. These are *predicates*, not\n\
                 // axioms — a lemma that needs the bound on a stub takes\n\
                 // it as a `requires` precondition. The runtime trust\n\
                 // assumption documented in the header above discharges\n\
                 // it for the live Aver oracle; concrete stubs can\n\
                 // discharge it directly with the verifier.\n\n",
            );
            emitted_any = true;
        }
        out.push_str(body);
        out.push('\n');
    };

    if declared.includes("Random.int") {
        push_block(
            "ghost predicate IsRandomIntInBounds(\n    \
                 f: (BranchPath, int, int, int) -> Result<int, string>)\n\
             {\n  \
               forall path, n, min, max ::\n    \
                 -9223372036854775808 <= min <= max <= 9223372036854775807 ==>\n      \
                   exists value :: f(path, n, min, max) == Result.Ok(value)\n        \
                     && min <= value <= max\n\
             }\n",
        );
    }
    if declared.includes("Random.float") {
        push_block(
            "ghost predicate IsRandomFloatInUnit(\n    \
                 f: (BranchPath, int) -> real)\n\
             {\n  \
               forall path, n :: 0.0 <= f(path, n) <= 1.0\n\
             }\n",
        );
    }
    if declared.includes("Time.unixMs") {
        push_block(
            "ghost predicate IsTimeUnixMsNonneg(\n    \
                 f: (BranchPath, int) -> int)\n\
             {\n  \
               forall path, n :: 0 <= f(path, n)\n\
             }\n",
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
    fn dafny_uses_ghost_predicates_not_axioms() {
        let d = declared(&["Random.int", "Random.float", "Time.unixMs"]);
        let out = dafny_subtype_predicates(&d);
        assert!(!out.contains("{:axiom}"));
        assert!(out.contains("ghost predicate IsRandomIntInBounds"));
        assert!(out.contains("ghost predicate IsRandomFloatInUnit"));
        assert!(out.contains("ghost predicate IsTimeUnixMsNonneg"));
    }

    #[test]
    fn dafny_uses_function_arrow_syntax() {
        let d = declared(&["Random.int"]);
        let out = dafny_subtype_predicates(&d);
        assert!(out.contains("(BranchPath, int, int, int) -> Result<int, string>"));
    }
}
