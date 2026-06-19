//! The feedback half of the discovery loop (`ProofStrategy::SimpOverLemmas`):
//! consume a previously-committed `DiscoveredLemmas.lean` so the kernel-proved
//! lemmas JOIN the normal `aver proof` run instead of only being re-verified
//! next to it.
//!
//! Flow (CLI-driven, Lean backend):
//!
//! ```text
//!   <out>/DiscoveredLemmas.lean  ─►  parse_committed_lemmas  ─►  plan_simp_over_lemma_pins
//!   (hash-gated: stale surface       (name + verbatim text        (per `verify … law`: every
//!    means IGNORE — behave exactly    per `theorem` block)          committed lemma whose program-fn
//!    like no discovery ran)                                         mentions ⊆ the law's cone)
//!                                                  │
//!                                                  ▼
//!                       apply_simp_over_lemma_pins re-pins `Induction` → `SimpOverLemmas(names)`;
//!                       the Lean backend then EMBEDS the lemma texts before the law theorem
//!                       (re-verifying them in the same `lake build` — the soundness guard)
//!                       and adds their names to the law's simp set.
//! ```
//!
//! The cone-hash gate is a staleness key ONLY (skip-feedback, like
//! skip-rediscovery on replay). Soundness never rests on it: an embedded lemma
//! is re-proved by the kernel on every build, so a lemma staled by a
//! same-signature body change fails the build loudly instead of being trusted.

use std::collections::{BTreeMap, BTreeSet};

use crate::ast::{TopLevel, VerifyKind};
use crate::codegen::proof_lower::{LawProofCone, ProofLowerInputs};
use crate::ir::proof_ir::ProofIR;

/// Who PROPOSED a lemma — the ORIGIN axis, orthogonal to the verification
/// STRENGTH axis (the sidecar's "verified (bounded), kernel proof pending" vs
/// proven header). The proof system has several lemma proposers; each lands on
/// a different point of the discovery-vs-plumbing map, so the artifacts carry
/// the origin honestly instead of letting one proposer's name (the
/// `--discover` enumerator) stand in for all of them.
///
/// - [`Conjectured`](Self::Conjectured) — an agent/LLM generalization, a LEAP
///   into the discovery half of the map.
/// - [`Enumerated`](Self::Enumerated) — the `discover` blind enumerate +
///   hostile forward-check, a search that LANDS mostly on plumbing.
/// - [`Recognized`](Self::Recognized) — shape recognizers / bridges, FORCED by
///   the program's structure → plumbing.
/// - [`Calculated`](Self::Calculated) — Lemma-Calculation (residual → lemma),
///   FORCED by the stuck goal → plumbing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LemmaProvenance {
    /// Agent/LLM generalization — a leap into discovery.
    Conjectured,
    /// The `discover` blind enumerate + hostile forward-check — a search that
    /// lands mostly on plumbing.
    Enumerated,
    /// Shape recognizers / bridges — plumbing forced by structure.
    Recognized,
    /// Lemma-Calculation (residual → lemma) — plumbing forced by the stuck goal.
    Calculated,
}

impl LemmaProvenance {
    /// Stable lowercase tag for rendering and round-trip:
    /// `"conjectured"` / `"enumerated"` / `"recognized"` / `"calculated"`.
    pub fn as_tag(&self) -> &'static str {
        match self {
            LemmaProvenance::Conjectured => "conjectured",
            LemmaProvenance::Enumerated => "enumerated",
            LemmaProvenance::Recognized => "recognized",
            LemmaProvenance::Calculated => "calculated",
        }
    }
}

impl std::fmt::Display for LemmaProvenance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_tag())
    }
}

/// A lemma available to a law's proof: its theorem name plus Lean text
/// (statement, and for embedded ones the tactic too). Two provenances flow
/// through the same orientation / loop-exclusion / simp-selection machinery:
///
/// - **embedded** (`embed = true`) — a kernel-proved lemma parsed back from a
///   committed `DiscoveredLemmas.lean`; its full text is written into the
///   generated proof project (re-proved in the same `lake build`).
/// - **reference** (`embed = false`) — an already-proved EARLIER user
///   `verify … law` in the same file (część A): its theorem is already
///   emitted, so only the NAME joins later laws' simp sets; `text` carries
///   just the synthesized `theorem <name> : <lhs> = <rhs>` statement, used
///   for orientation + loop analysis, never written out.
#[derive(Debug, Clone)]
pub struct CommittedLemma {
    pub name: String,
    pub text: String,
    /// Write `text` verbatim into the proof project (`true`), or only
    /// reference `name` in simp sets because it is already emitted (`false`).
    pub embed: bool,
    /// Which proposer ORIGINATED this lemma (orthogonal to `embed`/strength).
    /// Carried so the future Lemma-Calculation path can set it; every current
    /// producer is the discovery enumerator, so this is always
    /// [`LemmaProvenance::Enumerated`] for now.
    pub provenance: LemmaProvenance,
}

impl CommittedLemma {
    /// A reference to an already-emitted theorem (an earlier user law) — name
    /// plus synthesized statement, never written out. `text` should be a
    /// well-formed `theorem <name> : <stmt> := by` head so the shared
    /// orientation / loop analysis reads it like any other lemma.
    pub fn reference(name: String, text: String) -> Self {
        Self {
            name,
            text,
            embed: false,
            provenance: LemmaProvenance::Enumerated,
        }
    }
}

/// Parse a committed `DiscoveredLemmas.lean` into its theorem blocks. A block
/// starts at a column-0 `theorem ` line and runs until the next one (proof
/// lines are indented, so this never splits a tactic). Header comments before
/// the first theorem are dropped; comment/blank lines between theorems are
/// absorbed into the preceding block's text (harmless Lean comments).
pub fn parse_committed_lemmas(content: &str) -> Vec<CommittedLemma> {
    let mut lemmas: Vec<CommittedLemma> = Vec::new();
    let mut current: Option<CommittedLemma> = None;
    for line in content.lines() {
        if let Some(rest) = line.strip_prefix("theorem ") {
            if let Some(mut done) = current.take() {
                done.text.truncate(done.text.trim_end().len());
                lemmas.push(done);
            }
            let name = rest
                .split_whitespace()
                .next()
                .unwrap_or("")
                .trim_end_matches(':')
                .to_string();
            current = Some(CommittedLemma {
                name,
                text: line.to_string(),
                embed: true,
                provenance: LemmaProvenance::Enumerated,
            });
        } else if let Some(block) = current.as_mut() {
            block.text.push('\n');
            block.text.push_str(line);
        }
    }
    if let Some(mut done) = current.take() {
        done.text.truncate(done.text.trim_end().len());
        lemmas.push(done);
    }
    lemmas.retain(|l| !l.name.is_empty());
    lemmas
}

/// Soundness validation for a parsed committed lemma: the embed path writes
/// `text` VERBATIM into the generated entry root, where lake compiles it as
/// top-level Lean — so a block absorbing anything beyond its own
/// `theorem … := by` + tactic lines (the parser takes every non-`theorem `
/// line as-is, and Lean accepts indented top-level commands) could smuggle a
/// declaration like `axiom cheat : False` into the proof environment.
/// Returns the first forbidden declaration keyword found outside `--` line
/// comments (skipping the block's own leading `theorem`), or `None` when the
/// block is clean. The CLI rejects the WHOLE artifact on any hit — a
/// discovery-emitted file never contains these, so a hit means hand-edited
/// or corrupted content that must not join a kernel-trust pipeline. (The
/// axiom WHITELIST in the universal metric is the backstop; this check makes
/// the failure loud and early instead.)
pub fn forbidden_token_in_lemma(text: &str) -> Option<&'static str> {
    const DENY: [&str; 30] = [
        "axiom",
        "opaque",
        "unsafe",
        "macro",
        "macro_rules",
        "notation",
        "syntax",
        "elab",
        "attribute",
        "set_option",
        "instance",
        "structure",
        "inductive",
        "class",
        "def",
        "abbrev",
        "example",
        "import",
        "open",
        "namespace",
        "section",
        "end",
        "mutual",
        "initialize",
        "run_cmd",
        "partial",
        "noncomputable",
        "deriving",
        "theorem",
        "sorry",
    ];
    for (line_idx, line) in text.lines().enumerate() {
        let code = line.split("--").next().unwrap_or("");
        for (tok_idx, tok) in code
            .split(|c: char| !(c.is_alphanumeric() || c == '_' || c == '.' || c == '\''))
            .filter(|t| !t.is_empty())
            .enumerate()
        {
            // The block's own header keyword.
            if line_idx == 0 && tok_idx == 0 && tok == "theorem" {
                continue;
            }
            if let Some(hit) = DENY.iter().find(|d| **d == tok) {
                return Some(hit);
            }
        }
    }
    None
}

/// Program fns a lemma's Lean text mentions, projected through `lean_index`
/// (Lean name → caller-chosen value, e.g. the source name). Token scan over
/// identifier-shaped chunks; builtin lemma names (`List.append_assoc`, …) and
/// binder names simply miss the index.
pub fn mentioned_fns(text: &str, lean_index: &BTreeMap<String, String>) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for token in text.split(|c: char| !(c.is_alphanumeric() || c == '_' || c == '.' || c == '\'')) {
        if let Some(v) = lean_index.get(token) {
            out.insert(v.clone());
        }
    }
    out
}

/// Program fns a lemma's LEFT-HAND SIDE mentions — the rewrite rule's pattern,
/// projected through `lean_index` like [`mentioned_fns`]. A Forward lemma fires
/// against the consumer goal only through its LHS shape (`length (append x y) =
/// plus …` matches a goal containing `length (append …)`), so a sibling whose
/// LHS sits entirely inside the consumer's proof cone is RELEVANT even when its
/// RHS introduces an out-of-cone combinator (`plus`) — that combinator's
/// `= a + b` bridge is synthesized downstream, and loop safety is handled by
/// [`simp_entries`]. Falls back to the whole statement when there is no
/// top-level `=` (an invariant-shaped lemma, which is inert as a rewrite anyway).
pub fn lemma_lhs_fns(text: &str, lean_index: &BTreeMap<String, String>) -> BTreeSet<String> {
    let lhs = statement_body(text)
        .and_then(|stmt| {
            split_after_top_eq(stmt).map(|rhs| {
                let end = stmt.len() - rhs.len() - 1; // strip the `=` between lhs and rhs
                stmt[..end].trim()
            })
        })
        .unwrap_or(text);
    mentioned_fns(lhs, lean_index)
}

/// How a committed lemma may join a `simp` set. Discovery commits equations
/// in enumeration orientation, so usability as a rewrite rule is a property
/// to RECOVER, not assume.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimpDirection {
    /// LHS head is a program fn (`count x2 (x0 ++ x1) = plus …`,
    /// `decode (encode xs) = xs`): use as-is — rewrites toward
    /// decomposed/builtin normal form.
    Forward,
    /// LHS is builtin-headed but the RHS head is a program fn (the trivia
    /// `(x0 ++ x1) = append x0 x1`): use as `← name` — rewrites the opaque
    /// program fn INTO its builtin shape (an unfolding equation the fn's own
    /// def can't provide when its recursion is stuck on a symbolic arg).
    Reversed,
}

/// Classify a committed lemma as a usable `simp` rewrite rule, or `None`
/// (e.g. a `0 <= …` invariant, or an equation connecting nothing to a
/// program fn head). A `None` lemma stays EMBEDDED (other committed lemmas'
/// proofs may depend on it) but joins no simp set — a builtin-headed
/// equation used left-to-right re-folds the very structure the induction
/// ladder needs peeled, and loops against the fn's own def unfold.
pub fn simp_orientation(text: &str, program_fns: &BTreeSet<String>) -> Option<SimpDirection> {
    let stmt = statement_body(text)?;
    let rhs = split_after_top_eq(stmt);
    // A Forward rule is usable only if it does not GROW the term — if the RHS
    // textually contains the whole LHS (`dbl x = idNat (dbl x)`), rewriting
    // LHS→RHS re-exposes the LHS and `simp` never terminates (a maxHeartbeats
    // BUILD error `first` cannot catch). The `simp_entries` loop-exclusion
    // only drops forward/reversed PAIRS, not a single self-growing forward
    // rule — so reject it here. The shrinking REVERSED direction (RHS→LHS) is
    // still safe and is tried next.
    let lhs = rhs.map(|r| {
        let end = stmt.len() - r.len() - 1; // strip the `=` between lhs and rhs
        stmt[..end].trim()
    });
    let forward_grows = matches!((lhs, rhs), (Some(l), Some(r)) if !l.is_empty() && r.contains(l));
    if program_fns.contains(&head_token(stmt)) && !forward_grows {
        return Some(SimpDirection::Forward);
    }
    let rhs = rhs?;
    // Symmetric guard for the reversed direction: a self-growing reversed rule
    // (LHS contains the RHS) would loop the other way.
    let reversed_grows = matches!(lhs, Some(l) if !rhs.trim().is_empty() && l.contains(rhs.trim()));
    if program_fns.contains(&head_token(rhs)) && !reversed_grows {
        return Some(SimpDirection::Reversed);
    }
    None
}

/// Ready-to-emit `simp` set entries for a pinned lemma selection: a Forward
/// lemma joins as `name`, a Reversed one as `← name` — minus the loop-prone
/// combinations. A Forward rule whose RHS mentions a program fn that some
/// Reversed rule in the SAME set unfolds (its RHS head) would compose into a
/// rewrite cycle — e.g. `length (x0 ++ x1) = length (append x0 x1)` (forward)
/// against `← ((x0 ++ x1) = append x0 x1)` ping-pongs `++ ↔ append` under
/// `length` forever. `simp` loops are NOT a caught failure: they abort the
/// build with a deterministic maxHeartbeats ERROR that `first` cannot
/// recover from, so the exclusion is a build-safety requirement, not a
/// quality preference.
pub fn simp_entries(lemmas: &[&CommittedLemma], program_fns: &BTreeSet<String>) -> Vec<String> {
    let classified: Vec<(&CommittedLemma, SimpDirection)> = lemmas
        .iter()
        .filter_map(|l| simp_orientation(&l.text, program_fns).map(|d| (*l, d)))
        .collect();
    let reversed_heads: BTreeSet<String> = classified
        .iter()
        .filter(|(_, d)| *d == SimpDirection::Reversed)
        .filter_map(|(l, _)| {
            let rhs = split_after_top_eq(statement_body(&l.text)?)?;
            Some(head_token(rhs))
        })
        .collect();
    classified
        .into_iter()
        .filter_map(|(l, d)| match d {
            SimpDirection::Forward => {
                let rhs = split_after_top_eq(statement_body(&l.text)?)?;
                let mentions_unfolded = rhs
                    .split(|c: char| !(c.is_alphanumeric() || c == '_' || c == '.' || c == '\''))
                    .any(|tok| reversed_heads.contains(tok));
                if mentions_unfolded {
                    None
                } else {
                    Some(l.name.clone())
                }
            }
            SimpDirection::Reversed => Some(format!("← {}", l.name)),
        })
        .collect()
}

/// [`statement_of`] with the `∀ binders,` prefix stripped — the equation body
/// the orientation/loop analyses operate on.
fn statement_body(text: &str) -> Option<&str> {
    let stmt = statement_of(text)?.trim_start();
    if let Some(rest) = stmt.strip_prefix('∀') {
        split_after_depth0(rest, ',')
    } else {
        Some(stmt)
    }
}

/// First identifier-shaped token, skipping leading whitespace and `(`.
fn head_token(text: &str) -> String {
    text.chars()
        .skip_while(|c| c.is_whitespace() || *c == '(')
        .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.' || *c == '\'')
        .collect()
}

/// The slice after the top-level `=` of an equation — depth-0, not part of
/// `<=` / `>=` / `!=` / `==` (the only `=`-bearing operators the lemma
/// templates emit; `:=` was already cut off by [`statement_of`]).
fn split_after_top_eq(text: &str) -> Option<&str> {
    let mut depth = 0i32;
    let mut prev = ' ';
    let bytes = text.as_bytes();
    for (i, c) in text.char_indices() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            '=' if depth == 0 => {
                let next_eq = bytes.get(i + 1) == Some(&b'=');
                if !matches!(prev, '<' | '>' | '!' | '=') && !next_eq {
                    return Some(&text[i + 1..]);
                }
            }
            _ => {}
        }
        prev = c;
    }
    None
}

/// The statement region of a theorem text: after the first depth-0 `:`
/// (binders keep their `:`s inside parens/brackets), up to the depth-0 `:=`.
fn statement_of(text: &str) -> Option<&str> {
    let mut depth = 0i32;
    let mut start = None;
    let mut prev_colon = false;
    for (i, c) in text.char_indices() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            ':' if depth == 0 && start.is_none() => {
                start = Some(i + 1);
            }
            '=' if depth == 0 && prev_colon => {
                // `:=` — if it directly follows the colon that opened the
                // statement, the statement is empty (malformed); else end.
                let s = start?;
                if i > s {
                    return Some(&text[s..i - 1]);
                }
                return None;
            }
            _ => {}
        }
        prev_colon = c == ':' && depth == 0;
    }
    None
}

/// Byte offset just past the first depth-0 occurrence of `sep`, as a slice.
fn split_after_depth0(text: &str, sep: char) -> Option<&str> {
    let mut depth = 0i32;
    for (i, c) in text.char_indices() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            c2 if c2 == sep && depth == 0 => return Some(&text[i + c.len_utf8()..]),
            _ => {}
        }
    }
    None
}

/// A planned re-pin: `(fn_id, law_name)` goes from `Induction` to
/// `SimpOverLemmas(lemma_names)`.
pub type SimpOverLemmaPin = (crate::ir::FnId, String, Vec<String>);

/// Decide which laws get the committed lemmas. A lemma is in-scope for a law
/// when every program fn its text mentions is inside the law's proof cone
/// (plus the law's subject fn) — the same scope discovery enumerated over, so
/// the embedded text can only reference fns already emitted before the law's
/// theorem. Only laws the lowerer pinned `Induction` are re-pinned: that is
/// the strategy the discovery cluster (list/Peano homomorphisms) lands on,
/// and the Lean renderer for `SimpOverLemmas` reuses the same induction
/// ladder, so the swap can only ADD proving power.
pub fn plan_simp_over_lemma_pins(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    lemmas: &[CommittedLemma],
) -> Vec<SimpOverLemmaPin> {
    use crate::codegen::lean::aver_name_to_lean;
    if lemmas.is_empty() {
        return Vec::new();
    }
    // Lean name → Lean name over EVERY pure program fn: the universe the
    // subset test runs in. A lemma mentioning no program fn at all carries no
    // connection to the program and is never pinned.
    let all_fns: BTreeMap<String, String> = inputs
        .pure_fns()
        .iter()
        .map(|fd| {
            let lean = aver_name_to_lean(&fd.name);
            (lean.clone(), lean)
        })
        .collect();
    let all_fn_names: BTreeSet<String> = all_fns.keys().cloned().collect();
    let mentions: Vec<BTreeSet<String>> = lemmas
        .iter()
        .map(|l| mentioned_fns(&l.text, &all_fns))
        .collect();
    let oriented: Vec<bool> = lemmas
        .iter()
        .map(|l| simp_orientation(&l.text, &all_fn_names).is_some())
        .collect();

    let mut plan = Vec::new();
    for item in inputs.entry_items {
        let TopLevel::Verify(vb) = item else { continue };
        let VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        let Some(fn_id) = inputs
            .symbol_table
            .fn_id_of(&crate::ir::FnKey::entry(&vb.fn_name))
        else {
            continue;
        };
        let Some(thm) = ir
            .law_theorems
            .iter()
            .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        else {
            continue;
        };
        if !matches!(thm.strategy, crate::ir::ProofStrategy::Induction { .. }) {
            continue;
        }
        let cone = LawProofCone::compute(law, &vb.fn_name, inputs);
        let mut scope: BTreeSet<String> = cone
            .pure_fns()
            .iter()
            .map(|fd| aver_name_to_lean(&fd.name))
            .collect();
        scope.insert(aver_name_to_lean(&vb.fn_name));
        // The pin carries every in-scope lemma (the EMBED set — committed
        // lemmas may depend on each other, so dropping one could break
        // another's embedded proof), but a law is only worth pinning when at
        // least one of them is a usable simp rewrite rule — the Lean emit
        // re-derives that selection for its `simp` sets.
        let mut any_oriented = false;
        let mut selected: BTreeSet<usize> = BTreeSet::new();
        for (i, (m, o)) in mentions.iter().zip(&oriented).enumerate() {
            if !m.is_empty() && m.is_subset(&scope) {
                selected.insert(i);
                any_oriented |= *o;
            }
        }
        if !any_oriented {
            continue;
        }
        // Dependency closure: a committed lemma's PROOF may reference a
        // sibling committed theorem by name (the structural chains do —
        // e.g. a guarded `…_succ` step rewriting with its `…_natAbs_succ`
        // helper, which itself mentions no program fn and so failed the
        // in-scope gate above). Embedding one without the other is an
        // unknown-identifier BUILD error, so pull referenced siblings in
        // until fixpoint. Every program fn is emitted before the verify
        // theorems regardless of cone, so an added dependency always
        // type-checks; preserving committed-file order (the BTreeSet index
        // walk below) keeps each dependency ahead of its dependent.
        loop {
            let added: Vec<usize> = lemmas
                .iter()
                .enumerate()
                .filter(|(j, lj)| {
                    !selected.contains(j)
                        && selected.iter().any(|&i| lemmas[i].text.contains(&lj.name))
                })
                .map(|(j, _)| j)
                .collect();
            if added.is_empty() {
                break;
            }
            selected.extend(added);
        }
        let names: Vec<String> = selected.iter().map(|&i| lemmas[i].name.clone()).collect();
        plan.push((fn_id, law.name.clone(), names));
    }
    plan
}

/// Apply a [`plan_simp_over_lemma_pins`] plan to the lowered IR.
pub fn apply_simp_over_lemma_pins(ir: &mut ProofIR, plan: &[SimpOverLemmaPin]) {
    for (fn_id, law_name, names) in plan {
        if let Some(t) = ir
            .law_theorems
            .iter_mut()
            .find(|t| t.fn_id == *fn_id && t.law_name == *law_name)
        {
            t.strategy = crate::ir::ProofStrategy::SimpOverLemmas(names.clone());
        }
    }
}

/// Staleness key for a committed `DiscoveredLemmas.lean`: an FNV-1a hash over
/// the sorted signatures of the program's pure-fn proof surface. A normal
/// `aver proof` re-pins committed lemmas only when this matches the hash the
/// file was tagged with — a changed surface means the committed lemmas may be
/// stale, so they are ignored (the re-pin behaves exactly as if none existed).
/// The hash gates staleness only; re-verification in `lake build` is the
/// soundness guard, never the hash.
pub fn discovery_surface_hash(inputs: &ProofLowerInputs) -> String {
    let mut sigs: Vec<String> = inputs
        .pure_fns()
        .iter()
        .map(|fd| {
            let params: Vec<String> = fd.params.iter().map(|(n, t)| format!("{n}:{t}")).collect();
            format!("{}({})->{}", fd.name, params.join(","), fd.return_type)
        })
        .collect();
    sigs.sort();
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in sigs.join(";").bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    format!("{hash:016x}")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The count-into-plus fold family (mirrors the conjecturer fixture in
    /// the parent module), plus an `orphan` pure fn UNREACHABLE from the law —
    /// the out-of-cone case the in-scope gate must reject.
    const SRC: &str = r#"
type Nat
    Z
    S(Nat)

fn eqNat(x: Nat, y: Nat) -> Bool
    match x
        Nat.Z -> match y
            Nat.Z -> true
            Nat.S(z) -> false
        Nat.S(x2) -> match y
            Nat.Z -> false
            Nat.S(y2) -> eqNat(x2, y2)

fn count(x: Nat, y: List<Nat>) -> Nat
    match y
        [] -> Nat.Z
        [z, ..ys] -> match eqNat(x, z)
            true -> Nat.S(count(x, ys))
            false -> count(x, ys)

fn plus(x: Nat, y: Nat) -> Nat
    match x
        Nat.Z -> y
        Nat.S(z) -> Nat.S(plus(z, y))

fn appendNat(xs: List<Nat>, ys: List<Nat>) -> List<Nat>
    List.concat(xs, ys)

fn orphan(x: Nat) -> Nat
    x

verify count law countPlusConcat
    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]
    given xs: List<Nat> = [[], [Nat.Z]]
    given ys: List<Nat> = [[], [Nat.S(Nat.Z)]]
    plus(count(n, xs), count(n, ys)) => count(n, appendNat(xs, ys))
"#;

    const COMMITTED: &str = "-- Discovered lemmas for prop_02.av — `aver proof --discover`\n\
        -- cone-hash: 00deadbeef00\n\
        -- Each theorem below was discovered and kernel-proved.\n\
        \n\
        theorem aver_helper_succ (n : Int) : Int.natAbs (n + 1) = Int.natAbs n + 1 := by\n\
        \x20 omega\n\
        \n\
        theorem aver_discovered_lemma_0 (x0 : List Nat) (x1 : List Nat) (x2 : Nat) : count x2 (x0 ++ x1) = plus (count x2 x0) (count x2 x1) := by\n\
        \x20 induction x0 with\n\
        \x20 | nil => first | (simp [count]; done) | (simp [count, aver_helper_succ]; omega)\n\
        \x20 | cons head tail ih => first | (simp_all [count]; done) | (simp_all [count]; omega)\n\
        \n\
        theorem aver_discovered_lemma_1 (x0 : Nat) : orphan (plus x0 x0) = plus x0 x0 := by\n\
        \x20 simp [orphan]\n";

    fn with_inputs<R>(src: &str, f: impl FnOnce(&ProofLowerInputs) -> R) -> R {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut items = crate::parser::Parser::new(tokens).parse().expect("parse");
        crate::ir::pipeline::tco(&mut items);
        crate::ir::pipeline::resolve(&mut items);
        let symbols = crate::ir::SymbolTable::build(&items, &[]);
        let prefixes: std::collections::HashSet<String> = std::collections::HashSet::new();
        let recursive: std::collections::HashSet<crate::ir::FnId> =
            std::collections::HashSet::new();
        let no_modules: &[crate::codegen::ModuleInfo] = &[];
        let inputs = ProofLowerInputs {
            entry_items: &items,
            dep_modules: no_modules,
            module_prefixes: &prefixes,
            recursive_fns: &recursive,
            symbol_table: &symbols,
            program_shape: None,
        };
        f(&inputs)
    }

    #[test]
    fn parses_committed_theorem_blocks() {
        let lemmas = parse_committed_lemmas(COMMITTED);
        assert_eq!(lemmas.len(), 3);
        assert_eq!(lemmas[0].name, "aver_helper_succ");
        assert_eq!(lemmas[1].name, "aver_discovered_lemma_0");
        assert_eq!(lemmas[2].name, "aver_discovered_lemma_1");
        // Block boundaries: each text starts at its own `theorem` line and
        // carries its full (indented) tactic, nothing of its neighbour.
        assert!(
            lemmas[1]
                .text
                .starts_with("theorem aver_discovered_lemma_0 ")
        );
        assert!(lemmas[1].text.contains("induction x0 with"));
        assert!(!lemmas[1].text.contains("aver_discovered_lemma_1"));
        assert!(lemmas[2].text.ends_with("simp [orphan]"));
        // Header comments are not a lemma.
        assert!(lemmas.iter().all(|l| !l.text.contains("cone-hash")));
    }

    #[test]
    fn plan_pins_in_scope_lemma_and_rejects_out_of_cone() {
        with_inputs(SRC, |inputs| {
            let mut ir = ProofIR::default();
            crate::codegen::proof_lower::populate_law_theorems(inputs, &mut ir);
            assert_eq!(ir.law_theorems.len(), 1);
            assert!(matches!(
                ir.law_theorems[0].strategy,
                crate::ir::ProofStrategy::Induction { .. }
            ));

            let lemmas = parse_committed_lemmas(COMMITTED);
            let plan = plan_simp_over_lemma_pins(inputs, &ir, &lemmas);
            // Exactly one law pinned. lemma_0 mentions {count, plus} ⊆ cone ∪
            // {subject} — in. Its tactic references `aver_helper_succ` by
            // name, so the helper (no program-fn mentions — it would fail the
            // in-scope gate alone) rides in via the dependency closure, AHEAD
            // of its dependent (committed-file order). lemma_1 mentions
            // `orphan`, which the law never reaches — out-of-cone, rejected.
            assert_eq!(plan.len(), 1);
            assert_eq!(plan[0].1, "countPlusConcat");
            assert_eq!(
                plan[0].2,
                vec![
                    "aver_helper_succ".to_string(),
                    "aver_discovered_lemma_0".to_string()
                ]
            );

            apply_simp_over_lemma_pins(&mut ir, &plan);
            match &ir.law_theorems[0].strategy {
                crate::ir::ProofStrategy::SimpOverLemmas(names) => {
                    assert_eq!(names.len(), 2);
                }
                other => panic!("expected SimpOverLemmas pin, got {other:?}"),
            }
        });
    }

    #[test]
    fn simp_orientation_classifies_rewrite_direction() {
        let fns: BTreeSet<String> = ["count", "plus", "appendNat", "decode", "encode"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        // Homomorphism: program-fn-headed LHS — a forward rewrite rule.
        assert_eq!(
            simp_orientation(
                "theorem t0 (x0 : List Nat) (x2 : Nat) : (count x2 (x0 ++ x1)) = (plus (count x2 x0) (count x2 x1)) := by\n  simp",
                &fns
            ),
            Some(SimpDirection::Forward)
        );
        // Roundtrip-shaped brick: also forward.
        assert_eq!(
            simp_orientation(
                "theorem t1 (xs : List String) : decode (encode xs) = xs := by\n  simp",
                &fns
            ),
            Some(SimpDirection::Forward)
        );
        // Builtin-headed LHS with a program-fn-headed RHS: usable REVERSED
        // (`← name` unfolds the opaque wrapper into its builtin shape).
        assert_eq!(
            simp_orientation(
                "theorem t2 (x0 : List Nat) : (x0 ++ x0) = (appendNat x0 x0) := by\n  simp",
                &fns
            ),
            Some(SimpDirection::Reversed)
        );
        // ∀-quantified template: the binder list is skipped before the head.
        assert_eq!(
            simp_orientation(
                "theorem t3 : ∀ (list : List Int) (acc : Int), plus list acc = acc := by\n  simp",
                &fns
            ),
            Some(SimpDirection::Forward)
        );
        // Non-equation invariant (`0 <= …`) connecting no program-fn head on
        // either side of an `=`: no usable direction (embed-only).
        assert_eq!(
            simp_orientation(
                "theorem t4 (acc : Acc) (x : Int) : 0 <= (count acc x) := by\n  simp",
                &fns
            ),
            None
        );
        // Builtin-to-builtin associativity trivia: no direction either.
        assert_eq!(
            simp_orientation(
                "theorem t5 (x0 : List Nat) : ((x0 ++ x0) ++ x0) = (x0 ++ (x0 ++ x0)) := by\n  simp",
                &fns
            ),
            None
        );
        // SELF-GROWING forward rule (`dbl x = idNat (dbl x)`, RHS contains the
        // whole LHS): rewriting LHS→RHS never terminates, so Forward is
        // forbidden — but the shrinking REVERSED direction (RHS head `idNat` is
        // a program fn, LHS does not contain the RHS) is safe.
        let dfns: BTreeSet<String> = ["dbl", "idNat"].iter().map(|s| s.to_string()).collect();
        assert_eq!(
            simp_orientation(
                "theorem t6 (x : Nat) : dbl x = idNat (dbl x) := by\n  simp",
                &dfns
            ),
            Some(SimpDirection::Reversed)
        );
        // A reflexive equation (`loopy x = loopy x`) grows in BOTH directions
        // (each side contains the other), so neither direction is a usable
        // rewrite — dropped.
        let efns: BTreeSet<String> = ["loopy"].iter().map(|s| s.to_string()).collect();
        assert_eq!(
            simp_orientation(
                "theorem t7 (x : Nat) : loopy x = loopy x := by\n  rfl",
                &efns
            ),
            None
        );
    }

    #[test]
    fn forbidden_tokens_reject_smuggled_declarations() {
        // A genuine discovery block: clean.
        let lemmas = parse_committed_lemmas(COMMITTED);
        assert!(
            lemmas
                .iter()
                .all(|l| forbidden_token_in_lemma(&l.text).is_none()),
            "discovery-emitted blocks must validate clean"
        );
        // The smuggle vector the adversarial review found: a column-0 (or
        // indented — Lean accepts indented top-level commands) `axiom` line
        // absorbed into a block's verbatim text would join the kernel
        // environment and defeat the universal metric.
        assert_eq!(
            forbidden_token_in_lemma("theorem t : True := by\n  trivial\naxiom cheat : False"),
            Some("axiom")
        );
        assert_eq!(
            forbidden_token_in_lemma("theorem t : True := by\n  trivial\n  set_option foo true"),
            Some("set_option")
        );
        // `sorry` never appears in a committed lemma (proved-or-dropped).
        assert_eq!(
            forbidden_token_in_lemma("theorem t : P := by\n  first | simp | sorry"),
            Some("sorry")
        );
        // Words inside `--` comments don't trip the scan.
        assert_eq!(
            forbidden_token_in_lemma("theorem t : True := by\n  trivial -- no axiom here"),
            None
        );
        // A second `theorem` cannot hide inside a block either.
        assert_eq!(
            forbidden_token_in_lemma("theorem t : True := by\n  trivial\n  theorem u : True"),
            Some("theorem")
        );
    }

    #[test]
    fn plan_ignores_lemmas_with_no_program_connection() {
        with_inputs(SRC, |inputs| {
            let mut ir = ProofIR::default();
            crate::codegen::proof_lower::populate_law_theorems(inputs, &mut ir);
            // A lemma mentioning NO program fn (pure builtin algebra) carries
            // no connection to the program — never pinned.
            let lemmas = vec![CommittedLemma {
                name: "free_floating".to_string(),
                text: "theorem free_floating (a : Nat) : a + 0 = a := by simp".to_string(),
                embed: true,
                provenance: LemmaProvenance::Enumerated,
            }];
            assert!(plan_simp_over_lemma_pins(inputs, &ir, &lemmas).is_empty());
        });
    }
}
