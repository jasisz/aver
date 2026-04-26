//! Built-in oracle invariant axioms for proof export.
//!
//! Aver's runtime guarantees per-classified-effect invariants — `Random.int`
//! returns a value in `[min, max]`, `Random.float` in `[0.0, 1.0]`, etc.
//! Without these as importable lemmas, every user-side law about a roll
//! falling in range would have to start `sorry` against the universally
//! quantified oracle parameter the lifter introduces.
//!
//! This module emits those invariants as `axiom` blocks (Lean) and
//! `lemma {:axiom}` blocks (Dafny). They are *trust postulates* keyed
//! to the runtime contract documented in
//! [`super::proof_trust_header`]; the proof-export pipeline writes them
//! once into the entry file so any law in the project can `apply` /
//! `requires` them by name.
//!
//! Naming: `oracle_<method>_<property>` — `oracle_random_int_in_bounds`,
//! `oracle_random_float_in_unit_interval`, etc. Stable across releases
//! so user proofs can `import` them.

use crate::codegen::common::DeclaredEffects;

/// Lean 4 axiom block for every classified effect declared in the
/// program. Empty string when the program declares no classified
/// effects (no axioms needed). Output is comment-ready: starts with a
/// header comment, no trailing newline.
pub(crate) fn lean_axioms(declared: &DeclaredEffects) -> String {
    let mut out = String::new();
    let mut emitted_any = false;

    let mut push_axiom = |body: &str| {
        if !emitted_any {
            out.push_str(
                "-- Aver runtime invariants exposed as trust axioms.\n\
                 -- See the trust header above for the runtime contract\n\
                 -- these postulate. User-side laws may `apply` them by\n\
                 -- name to discharge bounds obligations on oracle args.\n\n",
            );
            emitted_any = true;
        }
        out.push_str(body);
        out.push('\n');
    };

    if declared.includes("Random.int") {
        push_axiom(
            "axiom oracle_random_int_in_bounds :\n  \
                 ∀ (rng : BranchPath → Int → Int → Int → Int)\n    \
                   (path : BranchPath) (n : Int) (min max : Int),\n  \
                 min ≤ max →\n  \
                 min ≤ rng path n min max ∧ rng path n min max ≤ max\n",
        );
    }
    if declared.includes("Random.float") {
        push_axiom(
            "axiom oracle_random_float_in_unit_interval :\n  \
                 ∀ (rng : BranchPath → Int → Float)\n    \
                   (path : BranchPath) (n : Int),\n  \
                 0.0 ≤ rng path n ∧ rng path n ≤ 1.0\n",
        );
    }
    if declared.includes("Time.unixMs") {
        push_axiom(
            "axiom oracle_time_unix_ms_nonnegative :\n  \
                 ∀ (clock : BranchPath → Int → Int)\n    \
                   (path : BranchPath) (n : Int),\n  \
                 0 ≤ clock path n\n",
        );
    }
    out
}

/// Dafny lemma-axiom block. Same invariants, Dafny syntax (`lemma
/// {:axiom}` is the canonical form for a postulate).
pub(crate) fn dafny_axioms(declared: &DeclaredEffects) -> String {
    let mut out = String::new();
    let mut emitted_any = false;

    let mut push_axiom = |body: &str| {
        if !emitted_any {
            out.push_str(
                "// Aver runtime invariants exposed as trust axioms.\n\
                 // See the trust header above for the runtime contract\n\
                 // these postulate. User-side lemmas may `requires`\n\
                 // their bound conclusions through these.\n\n",
            );
            emitted_any = true;
        }
        out.push_str(body);
        out.push('\n');
    };

    if declared.includes("Random.int") {
        push_axiom(
            "lemma {:axiom} oracle_random_int_in_bounds(\n    \
                 rng: (BranchPath, int, int, int) -> int,\n    \
                 path: BranchPath, n: int, min: int, max: int)\n  \
             requires min <= max\n  \
             ensures min <= rng(path, n, min, max) <= max\n",
        );
    }
    if declared.includes("Random.float") {
        push_axiom(
            "lemma {:axiom} oracle_random_float_in_unit_interval(\n    \
                 rng: (BranchPath, int) -> real,\n    \
                 path: BranchPath, n: int)\n  \
             ensures 0.0 <= rng(path, n) <= 1.0\n",
        );
    }
    if declared.includes("Time.unixMs") {
        push_axiom(
            "lemma {:axiom} oracle_time_unix_ms_nonnegative(\n    \
                 clock: (BranchPath, int) -> int,\n    \
                 path: BranchPath, n: int)\n  \
             ensures 0 <= clock(path, n)\n",
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
    fn lean_axioms_empty_when_no_classified_effects() {
        let d = declared(&["Args.get"]); // unclassified-for-bounds (no invariant emitted)
        let out = lean_axioms(&d);
        assert!(out.is_empty());
    }

    #[test]
    fn lean_axioms_emits_random_int_block() {
        let d = declared(&["Random.int"]);
        let out = lean_axioms(&d);
        assert!(out.contains("oracle_random_int_in_bounds"));
        assert!(out.contains("min ≤ max"));
        assert!(out.contains("axiom"));
    }

    #[test]
    fn lean_axioms_emits_all_invariants_when_all_declared() {
        let d = declared(&["Random.int", "Random.float", "Time.unixMs"]);
        let out = lean_axioms(&d);
        assert!(out.contains("oracle_random_int_in_bounds"));
        assert!(out.contains("oracle_random_float_in_unit_interval"));
        assert!(out.contains("oracle_time_unix_ms_nonnegative"));
        // Header comment should appear exactly once even with multiple
        // axioms emitted.
        assert_eq!(
            out.matches("-- Aver runtime invariants").count(),
            1,
            "header comment should appear once"
        );
    }

    #[test]
    fn dafny_axioms_emits_random_int_block() {
        let d = declared(&["Random.int"]);
        let out = dafny_axioms(&d);
        assert!(out.contains("oracle_random_int_in_bounds"));
        assert!(out.contains("requires min <= max"));
        assert!(out.contains("ensures min <= rng"));
    }

    #[test]
    fn dafny_axioms_uses_lemma_axiom_syntax() {
        let d = declared(&["Random.float"]);
        let out = dafny_axioms(&d);
        assert!(out.contains("lemma {:axiom}"));
    }
}
