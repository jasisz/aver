//! Committed-lemma consumption — the proof-side "sink" for auxiliary helper
//! lemmas.
//!
//! A normal `aver proof` parses a committed `DiscoveredLemmas.lean` (hash-gated
//! for staleness via [`discovery_surface_hash`]), re-pins each in-scope
//! `Induction` law to `ProofStrategy::SimpOverLemmas(names)`, and the Lean
//! backend embeds the lemma texts before the law theorem — re-proving them in
//! the same `lake build` — and simps over their names. So a law that needs an
//! auxiliary lemma closes universally once that lemma is committed in scope.
//! Re-verification in `lake build`, NOT the hash, is the soundness guard: a
//! stale or hand-edited file is ignored, never trusted.
//!
//! The committed lemmas are produced EXTERNALLY — by hand or by the Method
//! agent loop (the `the-method` skill: an agent proposes auxiliary laws, the
//! kernel certifies them) — not by an in-tree enumerator. The brute-force
//! equation enumerator that previously populated `DiscoveredLemmas.lean`
//! (`aver proof --discover` / `--emit-laws`) was removed; its full source is
//! archived at the git tag `archive/lemma-discovery-enumerator`.

mod committed;
pub use committed::{
    CommittedLemma, LemmaProvenance, SimpDirection, apply_simp_over_lemma_pins,
    discovery_surface_hash, forbidden_token_in_lemma, lemma_lhs_fns, mentioned_fns,
    parse_committed_lemmas, plan_simp_over_lemma_pins, simp_entries, simp_orientation,
};
