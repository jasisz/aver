//! Lemma discovery — the "locksmith" (Phase 2 of the charter,
//! `prompts/lemma-discovery.md`).
//!
//! Where the legacy `AccumulatorRoundtrip` recognizer was a *key* cut for one
//! lock (it fires on exactly `rle.av`), this is the *locksmith*: a pass that
//! discovers the auxiliary lemmas an inductive proof needs, proves them, and
//! emits them as explicit checkable artifacts. The full pipeline is:
//!
//! ```text
//!   LawProofCone  ─►  typed-term enumerator  ─►  VM-filter  ─►  backend-prove  ─►  commit
//!   (scope: pure       (small equations over     (Aver VM as    (Lean = truth,     (named .lean/
//!    fns + ADTs)         the cone, bounded by      test oracle,   Dafny = regression) .dfy + manifest)
//!                        term SIZE) + LLM          conservative
//!                        conjecturer (guarded)     on overflow)
//! ```
//!
//! The cone (built by `LawProofCone::compute`) is the differentiator: the
//! compiler already knows a law's scope, so the enumerator gets
//! goal-direction *for free* — external tools (HipSpec/CCLemma/…) must
//! reconstruct scope at cost.
//!
//! # What's implemented here (Phase 2a → 2e)
//!
//! The type-directed term enumerator, candidate generator, VM-filter, the Lean
//! theorem rendering the CLI kernel-checks, and the discovery-surface hash that
//! keys committed-lemma replay:
//!
//! 1. A **typed variable context** — a small fixed pool of variables (up to
//!    [`MAX_VARS_PER_TYPE`] per distinct parameter type the cone fns range
//!    over). Sharing one context across both sides of an equation is what
//!    lets `decode(a ++ b)` and `decode(a) ++ decode(b)` mention the *same*
//!    `a`, `b`.
//! 2. **Bottom-up term enumeration** over {cone pure fns, the `List.concat`
//!    builtin, the variables}, bounded by term **size** (node count, not
//!    arity × depth) up to [`MAX_TERM_SIZE`], deduplicated by rendering.
//! 3. **Candidate equations** — every pair of distinct, same-type terms that
//!    share the same free-variable set (see [`conjectures_from_terms`] for
//!    why that pruning, and what it deliberately does not yet reach).
//! 4. **VM-filter** ([`vm_filter`]) — runs both sides of each candidate on the
//!    Aver VM over sample variable assignments and drops counterexamples.
//!    Conservative (an eval error / out-of-guard `Int` never refutes), so a
//!    backend-true lemma is never wrongly dropped.
//!
//! 5. **Lean theorem rendering** ([`lean_lemma_theorem`], [`rank_candidate_indices`])
//!    — a survivor with a list free variable becomes a theorem with the
//!    list-induction template; the CLI (`aver proof --discover`) appends it to
//!    the generated Lean project and `lake build`s it (proved ⟺ exit 0). This
//!    is the proved-or-dropped gate (2d).
//!
//! 6. **Discover-once / replay** ([`discovery_surface_hash`]) — proved lemmas
//!    are committed by the CLI as a reviewable `DiscoveredLemmas.lean` tagged
//!    with the surface hash; on a re-run with an unchanged surface the CLI
//!    REPLAYS (re-verifies the committed lemmas via `lake build`) instead of
//!    re-enumerating. Re-verification — not the hash — is the soundness guard.
//!
//! Still ahead: re-enter the *law's own proof* with a discovered lemma (via the
//! `ProofStrategy::SimpOverLemmas` hook) so a `verify ... law` that needs it
//! closes under normal `aver proof` — for the flagship rle roundtrip that also
//! needs the guarded accumulator-generalization family (charter layer 3), so
//! it is deferred there. Entry [`run_discovery`] (+ [`vm_filter`], + the CLI
//! prove/replay step) is invoked by `aver proof --discover`; normal
//! `aver proof` never runs this (discovery is the explicit, cached step).

use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::ast::{TopLevel, VerifyKind};
use crate::codegen::proof_lower::{LawProofCone, ProofLowerInputs};
use crate::nan_value::{NanValue, NanValueConvert};
use crate::types::Type;
use crate::value::Value;

/// Variables minted per distinct cone parameter type. Two is the smallest
/// count that makes distributivity-shaped lemmas (`f(a ++ b) = f a ++ f b`,
/// two vars of one type) reachable.
const MAX_VARS_PER_TYPE: usize = 2;
/// Largest term (node count) the enumerator builds. The `decode_append`
/// right-hand side `List.concat(decode(a), decode(b))` is size 5, so this is
/// the minimum that surfaces the Phase-2 acceptance lemma.
const MAX_TERM_SIZE: usize = 5;
/// Safety cap on total enumerated terms — discovery is the expensive step but
/// must still terminate predictably on a large cone. Hitting it is recorded
/// in [`DiscoveryStats::terms_truncated`] (charter: no silent caps).
const MAX_TERMS: usize = 20_000;
/// Safety cap on generated candidate equations. Recorded in
/// [`DiscoveryStats::conjectures_truncated`] when hit.
const MAX_CONJECTURES: usize = 20_000;
/// Work cap on the O(bucket²) candidate-pairing scan. Bounds `--discover`
/// time on large cones (e.g. json's ~68-fn cones) independently of how many
/// candidates are actually emitted — filtered (different-free-var) pairs
/// still cost a comparison, so the output cap alone doesn't bound the work.
/// Recorded in [`DiscoveryStats::conjectures_truncated`] when hit.
const MAX_PAIRS_EXAMINED: usize = 2_000_000;
/// Cap on a single n-ary application's argument cartesian product, so one
/// wide op over large term pools can't blow up before [`MAX_TERMS`] bites.
/// Hitting it is folded into [`DiscoveryStats::terms_truncated`].
const CARTESIAN_CAP: usize = 4_000;
/// Cones with more pure fns than this skip enumeration entirely. Naive
/// size-[`MAX_TERM_SIZE`] discovery over a very large cone (e.g. json's
/// 60+-fn parser cones) is both slow and low-signal — the space is
/// astronomically undersampled, and such subsystems are a separate problem
/// (charter Phase 4). Recorded in [`DiscoveryStats::skipped_large_cone`];
/// healthy cones (rle 6, quicksort ≤5, red-black-tree ≤10) stay well under it.
const MAX_CONE_FNS: usize = 24;

/// A free variable in a typed term — a source-renderable name plus its Aver
/// type. `TermNode::Var(i)` refers to the binder at index `i` in the owning
/// [`LawDiscovery`]'s shared `binders`.
#[derive(Debug, Clone)]
pub struct Binder {
    pub name: String,
    pub ty: Type,
}

/// A node in a typed term tree. `App.callee` is a cone pure-fn name or a
/// builtin (`List.concat`); rendering is uniform `callee(arg, …)`.
#[derive(Debug, Clone, PartialEq)]
pub enum TermNode {
    /// A bound variable, by index into the shared `binders`.
    Var(usize),
    /// Application of a cone fn or builtin op to typed args.
    App { callee: String, args: Vec<TermNode> },
}

impl TermNode {
    fn size(&self) -> usize {
        match self {
            TermNode::Var(_) => 1,
            TermNode::App { args, .. } => 1 + args.iter().map(TermNode::size).sum::<usize>(),
        }
    }

    fn render(&self, binders: &[Binder]) -> String {
        match self {
            TermNode::Var(i) => binders
                .get(*i)
                .map(|b| b.name.clone())
                .unwrap_or_else(|| format!("?{i}")),
            TermNode::App { callee, args } => {
                let rendered: Vec<String> = args.iter().map(|a| a.render(binders)).collect();
                format!("{callee}({})", rendered.join(", "))
            }
        }
    }

    fn free_vars(&self, out: &mut BTreeSet<usize>) {
        match self {
            TermNode::Var(i) => {
                out.insert(*i);
            }
            TermNode::App { args, .. } => {
                for a in args {
                    a.free_vars(out);
                }
            }
        }
    }
}

/// An applicable operation in the enumeration vocabulary: a cone pure fn or a
/// builtin, with its (monomorphic, instantiated) parameter and result types.
#[derive(Debug, Clone)]
struct Op {
    callee: String,
    params: Vec<Type>,
    ret: Type,
}

/// A well-typed term built during enumeration, over the law's shared binders.
#[derive(Debug, Clone)]
struct EnumTerm {
    node: TermNode,
    ty: Type,
}

/// A candidate equation `lhs == rhs` (both `ty`-typed) over the shared
/// binders. A *conjecture*, not a theorem — the charter's proved-or-dropped
/// gate (VM-filter then kernel proof, 2c–2d) is what turns survivors into
/// usable lemmas.
#[derive(Debug, Clone)]
pub struct Conjecture {
    pub lhs: TermNode,
    pub rhs: TermNode,
    pub ty: Type,
}

impl Conjecture {
    /// Source-shaped rendering, e.g.
    /// `decode(List.concat(x2, x3)) == List.concat(decode(x2), decode(x3))`.
    pub fn render(&self, binders: &[Binder]) -> String {
        format!(
            "{} == {}",
            self.lhs.render(binders),
            self.rhs.render(binders)
        )
    }
}

/// Coverage / truncation accounting for one law's discovery run.
#[derive(Debug, Clone)]
pub struct DiscoveryStats {
    pub cone_fn_count: usize,
    pub term_count: usize,
    /// Candidate equations enumerated (2b), before the VM-filter.
    pub conjecture_count: usize,
    pub terms_truncated: bool,
    pub conjectures_truncated: bool,
    /// The cone exceeded [`MAX_CONE_FNS`]; enumeration was skipped entirely.
    pub skipped_large_cone: bool,
    /// The VM-filter (2c) ran for this law. When `true`, `conjectures` holds
    /// only the survivors and `candidates_refuted` counts the rest.
    pub vm_filtered: bool,
    /// Candidates the VM-filter refuted (counterexample found on sample data).
    pub candidates_refuted: usize,
    pub max_term_size: usize,
}

/// A discovery report for one `verify ... law`: the cone summary, the shared
/// variable context, and the enumerated candidate equations. Later phases
/// extend this with VM-filter verdicts and proved lemmas.
#[derive(Debug, Clone)]
pub struct LawDiscovery {
    /// The law's subject fn (`verify <fn> law <name>`); excluded from the cone.
    pub subject_fn: String,
    /// The law's name.
    pub law_name: String,
    /// The cone vocabulary — pure fns the enumerator may apply (sorted).
    pub cone_fns: Vec<String>,
    /// The cone type alphabet — user ADTs reachable from those fns (sorted).
    pub cone_types: Vec<String>,
    /// The shared typed variable context the conjectures range over.
    pub binders: Vec<Binder>,
    /// Candidate equations (size-ascending); after the VM-filter, survivors.
    pub conjectures: Vec<Conjecture>,
    /// Rendered equations kernel-proved by a backend (2d). Filled by the prove
    /// step (which needs a `CodegenContext` + prover, so it lives in the CLI);
    /// `run_discovery` leaves it empty.
    pub proved: Vec<String>,
    /// Coverage / truncation accounting.
    pub stats: DiscoveryStats,
}

mod bricks;
mod enumerate;
mod render;
mod vm_filter;

pub use bricks::structural_lemma_groups;
pub use enumerate::run_discovery;
pub use render::{
    discovery_surface_hash, lean_lemma_theorem, rank_candidate_indices, render_report,
};
pub use vm_filter::vm_filter;

// Shared internal helper used across submodules (the report renderer in
// `render` formats binder types via the enumerator's `render_type`).
use enumerate::render_type;
// Surfaced for the encoder-role detector test in `mod tests`.
#[cfg(test)]
use bricks::detect_encoders;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::ModuleInfo;
    use std::collections::HashSet;

    /// Minimal RLE-shaped fixture: a `decode` recursor over `List<Run>` with
    /// a transitive helper chain (`decode → expandRun → repeat`) and a
    /// roundtrip law whose subject is `encode`. Exercises the cone's
    /// fn-closure, type alphabet, and the enumerator + candidate generator.
    const SRC: &str = r#"
record Run
    char: String
    count: Int

fn repeat(c: String, n: Int) -> List<String>
    [c]

fn expandRun(r: Run) -> List<String>
    repeat(r.char, r.count)

fn decode(runs: List<Run>) -> List<String>
    match runs
        [] -> []
        [run, ..rest] -> List.concat(expandRun(run), decode(rest))

fn encode(xs: List<String>) -> List<Run>
    []

verify encode law roundtrip
    given xs: List<String> = [[], ["a"]]
    decode(encode(xs)) => xs
"#;

    /// Build a `ProofLowerInputs` from source and run `f` on it. The full
    /// lex→parse→tco→resolve pipeline runs so the VM-filter / oracle can compile
    /// the cone fns; `LawProofCone::compute` works on the resolved AST (it
    /// handles both `Ident` and `Resolved`).
    fn with_inputs<R>(src: &str, f: impl FnOnce(&ProofLowerInputs) -> R) -> R {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut items = crate::parser::Parser::new(tokens).parse().expect("parse");
        crate::ir::pipeline::tco(&mut items);
        crate::ir::pipeline::resolve(&mut items);
        let symbols = crate::ir::SymbolTable::build(&items, &[]);
        // Build the program shape (WrapperOverRecursion, …) so shape-anchored
        // detection exercises the same path in tests as in the CLI (where the
        // CodegenContext populates `program_shape`).
        let resolved = crate::ir::hir::resolve_program(&symbols, &items);
        let resolved_fns: Vec<&crate::ir::hir::ResolvedFnDef> = resolved
            .iter()
            .filter_map(|t| match t {
                crate::ir::hir::ResolvedTopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        // `analyze_program_with_modules` (not `analyze_program`) is what
        // populates `patterns` (WrapperOverRecursion, …) — the entry-only
        // variant leaves them empty.
        let shape =
            crate::analysis::shape::analyze_program_with_modules(&resolved_fns, &items, &[]);
        let prefixes: HashSet<String> = HashSet::new();
        let recursive: HashSet<crate::ir::FnId> = HashSet::new();
        let no_modules: &[ModuleInfo] = &[];
        let inputs = ProofLowerInputs {
            entry_items: &items,
            dep_modules: no_modules,
            module_prefixes: &prefixes,
            recursive_fns: &recursive,
            symbol_table: &symbols,
            program_shape: Some(&shape),
        };
        f(&inputs)
    }

    /// Enumerate (2b) AND VM-filter (2c).
    fn discover(src: &str) -> Vec<LawDiscovery> {
        with_inputs(src, |inputs| {
            let mut reports = run_discovery(inputs);
            vm_filter(&mut reports, inputs);
            reports
        })
    }

    fn rle_source() -> String {
        std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/examples/data/rle.av"))
            .expect("read rle.av")
    }

    fn tally_source() -> String {
        std::fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/data/tally.av"
        ))
        .expect("read tally.av")
    }

    fn drain_source() -> String {
        std::fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/data/drain.av"
        ))
        .expect("read drain.av")
    }

    fn scale_source() -> String {
        std::fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/data/scale.av"
        ))
        .expect("read scale.av")
    }

    fn twofield_source() -> String {
        std::fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/data/twofield.av"
        ))
        .expect("read twofield.av")
    }

    fn sparse_source() -> String {
        std::fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/data/sparse.av"
        ))
        .expect("read sparse.av")
    }

    fn sum_acc_source() -> String {
        std::fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/data/sum_acc.av"
        ))
        .expect("read sum_acc.av")
    }

    fn encoder_roles(src: &str) -> Vec<String> {
        with_inputs(src, |inputs| {
            detect_encoders(inputs)
                .into_iter()
                .map(|e| {
                    format!(
                        "{}/{}/{}/{}/{}/{}/{}",
                        e.wrapper, e.inverse, e.loop_fn, e.finish, e.step, e.expand, e.var
                    )
                })
                .collect()
        })
    }

    /// Matches the clearly-FALSE `x == List.concat(x, x)` (either orientation):
    /// a candidate the VM-filter must refute (a non-empty list ≠ itself
    /// appended to itself).
    fn is_self_concat_identity(c: &Conjecture) -> bool {
        fn oriented(l: &TermNode, r: &TermNode) -> bool {
            let TermNode::Var(x) = l else { return false };
            let TermNode::App { callee, args } = r else {
                return false;
            };
            callee == "List.concat"
                && args.len() == 2
                && matches!((&args[0], &args[1]), (TermNode::Var(a), TermNode::Var(b)) if a == x && b == x)
        }
        oriented(&c.lhs, &c.rhs) || oriented(&c.rhs, &c.lhs)
    }

    /// Structural matcher for the `decode_append` shape, in either orientation:
    /// `decode(List.concat(a, b)) == List.concat(decode(a), decode(b))` with
    /// `a`, `b` distinct variables.
    fn is_decode_append(c: &Conjecture) -> bool {
        fn oriented(l: &TermNode, r: &TermNode) -> bool {
            // l = decode(List.concat(Var(a), Var(b)))
            let TermNode::App {
                callee: lc,
                args: la,
            } = l
            else {
                return false;
            };
            if lc != "decode" || la.len() != 1 {
                return false;
            }
            let TermNode::App {
                callee: cc,
                args: ca,
            } = &la[0]
            else {
                return false;
            };
            if cc != "List.concat" || ca.len() != 2 {
                return false;
            }
            let (TermNode::Var(a), TermNode::Var(b)) = (&ca[0], &ca[1]) else {
                return false;
            };
            if a == b {
                return false;
            }
            // r = List.concat(decode(Var(a)), decode(Var(b)))
            let TermNode::App {
                callee: rc,
                args: ra,
            } = r
            else {
                return false;
            };
            if rc != "List.concat" || ra.len() != 2 {
                return false;
            }
            let (
                TermNode::App {
                    callee: d1,
                    args: r1,
                },
                TermNode::App {
                    callee: d2,
                    args: r2,
                },
            ) = (&ra[0], &ra[1])
            else {
                return false;
            };
            if d1 != "decode" || d2 != "decode" || r1.len() != 1 || r2.len() != 1 {
                return false;
            }
            matches!((&r1[0], &r2[0]), (TermNode::Var(a2), TermNode::Var(b2)) if a2 == a && b2 == b)
        }
        oriented(&c.lhs, &c.rhs) || oriented(&c.rhs, &c.lhs)
    }

    #[test]
    fn cone_excludes_subject_and_closes_over_pure_helpers() {
        let reports = discover(SRC);
        assert_eq!(reports.len(), 1);
        let r = &reports[0];
        assert_eq!(r.subject_fn, "encode");
        assert_eq!(r.law_name, "roundtrip");
        // `encode` (subject) is dropped; `decode` + its transitive pure
        // helpers stay, sorted by name.
        assert_eq!(r.cone_fns, vec!["decode", "expandRun", "repeat"]);
    }

    #[test]
    fn cone_types_resolve_adts_from_signatures() {
        let r = &discover(SRC)[0];
        // `Run` is reachable from `decode`/`expandRun` signatures; builtin
        // scalars (`String`/`Int`) and collection ctors drop out.
        assert_eq!(r.cone_types, vec!["Run"]);
    }

    #[test]
    fn enumerator_rediscovers_decode_append() {
        let r = &discover(SRC)[0];
        // The Phase-2 acceptance lemma falls out of the size-bounded
        // enumeration as a candidate equation — unguarded, purely from the
        // cone vocabulary, with no RLE-specific recognizer.
        assert!(
            r.conjectures.iter().any(is_decode_append),
            "decode_append candidate not found among {} conjectures",
            r.conjectures.len()
        );
        // Sanity: enumeration stayed within the safety caps.
        assert!(!r.stats.terms_truncated, "term enumeration truncated");
        assert!(
            !r.stats.conjectures_truncated,
            "conjecture generation truncated"
        );
    }

    /// The acceptance, on the real ground-truth fixture (not just the minimal
    /// inline one): `examples/data/rle.av`'s `encode law roundtrip` cone is
    /// the full `[decode, encodeFold, encodeLoop, expandRun, flushAcc,
    /// repeat]`, yet `decode_append` still falls out of the enumeration.
    #[test]
    fn enumerator_rediscovers_decode_append_on_real_rle() {
        let src =
            std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/examples/data/rle.av"))
                .expect("read rle.av");
        let reports = discover(&src);
        let roundtrip = reports
            .iter()
            .find(|r| r.law_name == "roundtrip")
            .expect("roundtrip law");
        assert_eq!(
            roundtrip.cone_fns,
            vec![
                "decode",
                "encodeFold",
                "encodeLoop",
                "expandRun",
                "flushAcc",
                "repeat"
            ]
        );
        assert!(
            roundtrip.conjectures.iter().any(is_decode_append),
            "decode_append candidate not found among {} conjectures on real rle.av",
            roundtrip.conjectures.len()
        );
        assert!(!roundtrip.stats.terms_truncated && !roundtrip.stats.conjectures_truncated);
    }

    #[test]
    fn vm_filter_refutes_false_keeps_decode_append() {
        let r = &discover(SRC)[0];
        // The VM-filter actually ran (oracle compiled) and dropped candidates.
        assert!(r.stats.vm_filtered, "VM-filter did not run");
        assert!(
            r.stats.candidates_refuted > 0,
            "VM-filter refuted nothing — oracle likely failed to compile"
        );
        // decode_append is TRUE → survives the filter.
        assert!(
            r.conjectures.iter().any(is_decode_append),
            "decode_append did not survive the VM-filter"
        );
        // `x == List.concat(x, x)` is FALSE → refuted, not among survivors.
        assert!(
            !r.conjectures.iter().any(is_self_concat_identity),
            "false self-concat identity survived the VM-filter"
        );
    }

    #[test]
    fn lean_theorem_renders_decode_append() {
        let r = &discover(SRC)[0];
        let c = r
            .conjectures
            .iter()
            .find(|c| is_decode_append(c))
            .expect("decode_append survives");
        let thm = lean_lemma_theorem(c, &r.binders, "L").expect("template applies");
        // Statement: `theorem L (x.. : List Run) (..) : decode (.. ++ ..) = .. := by`
        assert!(thm.contains("theorem L "), "{thm}");
        assert!(thm.contains(": List Run)"), "{thm}");
        assert!(thm.contains("decode (") && thm.contains("++"), "{thm}");
        // Tactic: session-grade list-induction ladder (mirrors the user-stated-law
        // prover) — the decode unfold + append_assoc, plus the `omega` and
        // `split` branches that give discovered candidates the same reach.
        assert!(thm.contains("induction "), "{thm}");
        assert!(thm.contains("| nil => first | (simp [decode]"), "{thm}");
        assert!(thm.contains("List.append_assoc"), "{thm}");
        assert!(thm.contains("ih"), "{thm}");
        assert!(
            thm.contains("omega"),
            "ladder must include the omega branch: {thm}"
        );
        assert!(
            thm.contains("split <;>"),
            "ladder must include the inner-match split branch: {thm}"
        );
        // Discovery is proved-or-dropped: NO `sorry` fallback (a sorry builds
        // clean and would falsely mark a refuted candidate "proved").
        assert!(
            !thm.contains("sorry"),
            "discovered-lemma tactic must never carry a sorry fallback: {thm}"
        );
    }

    #[test]
    fn ranking_puts_homomorphism_first() {
        let r = &discover(SRC)[0];
        let ranked = rank_candidate_indices(r);
        // The first ranked candidate is the list-homomorphism (decode_append).
        let first = &r.conjectures[ranked[0]];
        assert!(
            is_decode_append(first),
            "expected decode_append ranked first, got {}",
            first.render(&r.binders)
        );
    }

    /// A `count`-into-`plus` fold whose `count(n, a ++ b) = plus(count n a,
    /// count n b)` monoid homomorphism is size ~7 — past the enumerator's
    /// `MAX_TERM_SIZE = 5` — so only the structure-directed conjecturer can mint it.
    const COUNT_HOMO_SRC: &str = r#"
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

verify count law countPlusConcat
    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]
    given xs: List<Nat> = [[], [Nat.Z]]
    given ys: List<Nat> = [[], [Nat.S(Nat.Z)]]
    plus(count(n, xs), count(n, ys)) => count(n, appendNat(xs, ys))
"#;

    #[test]
    fn structural_homomorphism_conjectured_for_count_fold() {
        // The conjecturer mints the count homomorphism (the subject fn `count`
        // is excluded from the cone, so the conjecturer adds it back), and the
        // VM-filter KEEPS it (it is a true homomorphism). A render like
        // `count(x2, List.concat(x0, x1)) == plus(count(x2, x0), count(x2, x1))`.
        let r = &discover(COUNT_HOMO_SRC)[0];
        let found = r.conjectures.iter().any(|c| {
            let s = c.render(&r.binders);
            s.contains("List.concat(") && s.contains("plus(count(")
        });
        assert!(
            found,
            "count→plus homomorphism not conjectured/surviving; survivors:\n{}",
            r.conjectures
                .iter()
                .map(|c| c.render(&r.binders))
                .collect::<Vec<_>>()
                .join("\n")
        );
        // It must also rank first — it is the highest-value target, so the
        // bounded prove budget reaches it.
        let ranked = rank_candidate_indices(r);
        let top = r.conjectures[ranked[0]].render(&r.binders);
        assert!(
            top.contains("List.concat(") && top.contains("plus(count("),
            "count homomorphism must rank first, got {top}"
        );
    }

    #[test]
    fn structural_conjecturer_on_real_rle() {
        // rle is a full encoder, so its counted-repeat (`repeat`) and
        // monotone-nonneg (`encodeFold.count`) bricks are SUBSUMED by the
        // relational roundtrip chain (which re-proves them internally) and are
        // NOT emitted a second time as standalone groups — dedup. So exactly one
        // group fires: the chain.
        let groups = with_inputs(&rle_source(), structural_lemma_groups);
        assert_eq!(
            groups.len(),
            1,
            "expected only the relational chain (standalone bricks deduped)"
        );
        let all: String = groups.iter().flatten().map(|(_, t)| t.as_str()).collect();
        // brick 1 (now inside the chain): guarded counted-repeat advance
        // (`repeat` escapes to `repeat'`; its param is named `char` in rle).
        assert!(
            all.contains("repeat' char (n + 1) = repeat' char n ++ [char]"),
            "{all}"
        );
        assert!(
            all.contains("(hn : 0 <= n)") && all.contains("natAbs"),
            "{all}"
        );
        // generalized brick 2 (now inside the chain): monotone-nonneg field
        // invariant, with the shape-agnostic split/omega template.
        assert!(all.contains("_count_nonneg"), "{all}");
        assert!(all.contains("0 <= (encodeFold acc char).count"), "{all}");
        assert!(
            all.contains("unfold encodeFold") && all.contains("split <;>"),
            "{all}"
        );
        // DEDUP regression guard: the count-nonneg invariant is proved EXACTLY
        // once (only the chain's copy), not also as a standalone structural group.
        assert_eq!(
            all.matches("0 <= (encodeFold acc char).count").count(),
            1,
            "count_nonneg duplicated — dedup regressed"
        );
    }

    #[test]
    fn monotone_field_generalizes_beyond_rle_shape() {
        // tally.av: a NON-rle accumulator that branches on `x > acc.last`
        // (not `count == 0`). The generalized detector must still find the
        // monotone-nonneg `seen` field and emit its invariant — proof that
        // brick 2 keys on the field arithmetic, not the RLE step shape.
        let groups = with_inputs(&tally_source(), structural_lemma_groups);
        let all: String = groups.iter().flatten().map(|(_, t)| t.as_str()).collect();
        assert!(all.contains("0 <= (tallyStep acc x).seen"), "{all}");
        assert!(all.contains("unfold tallyStep"), "{all}");
        // It must NOT key on the RLE discriminants.
        assert!(!all.contains("acc.current"), "{all}");
    }

    #[test]
    fn bounded_step_handles_decreasing_accumulator() {
        // drain.av: `tick` does `acc.n + 1` / `acc.n - 1`, so `0 <= acc.n` is
        // FALSE — monotone-nonneg must decline. The bounded-step fallback fires
        // instead, emitting the TRUE two-sided bound (delta in [-1, +1]), and
        // never the false nonneg invariant.
        let groups = with_inputs(&drain_source(), structural_lemma_groups);
        let all: String = groups.iter().flatten().map(|(_, t)| t.as_str()).collect();
        // Soundness: the false nonneg invariant is not even conjectured.
        assert!(!all.contains("0 <= (tick acc x).n"), "{all}");
        // Generalization: both sides of the bounded step, via the same template.
        assert!(all.contains("acc.n - 1 <= (tick acc x).n"), "{all}");
        assert!(all.contains("(tick acc x).n <= acc.n + 1"), "{all}");
        assert!(
            all.contains("unfold tick") && all.contains("split <;>"),
            "{all}"
        );
    }

    #[test]
    fn nonneg_covers_multiplicative_scaling() {
        // scale.av: `grow` does `acc.level * 2` / `acc.level`. The `* 2` is not a
        // `+ k` shift, but `level * 2` keeps `0 <= level` and stays linear in the
        // field, so the nonneg invariant still fires (closes the nonlinear-nonneg
        // gap) — not the bounded step.
        let groups = with_inputs(&scale_source(), structural_lemma_groups);
        let all: String = groups.iter().flatten().map(|(_, t)| t.as_str()).collect();
        assert!(all.contains("0 <= (grow acc x).level"), "{all}");
        assert!(all.contains("unfold grow"), "{all}");
    }

    #[test]
    fn detect_encoder_recognizes_rle_and_sparse() {
        // The relational-brick role detector must recover the SAME encoder
        // skeleton from two structurally different encoders — proof it keys on
        // the fold-with-inverse shape, not on rle. (Roles arrive via TailCall
        // nodes after TCO; `as_call` handles that.)
        assert!(
            encoder_roles(&rle_source())
                .contains(&"encode/decode/encodeLoop/flushAcc/encodeFold/expandRun/xs".to_string()),
            "rle roles: {:?}",
            encoder_roles(&rle_source())
        );
        assert!(
            encoder_roles(&sparse_source()).contains(
                &"encodeSparse/decodeSparse/sparseLoop/flushSparse/sparseStep/expandTok/xs"
                    .to_string()
            ),
            "sparse roles: {:?}",
            encoder_roles(&sparse_source())
        );
    }

    #[test]
    fn shape_classifies_fold_wrappers() {
        // Shape-anchoring precondition: `analysis::shape` must classify the
        // encoder/monoidal wrappers as `WrapperOverRecursion` — the principled
        // "is this a wrapper-over-recursion fold" decision the detectors gate on,
        // sourced from the shared shape vocabulary, not a bespoke AST walk.
        use crate::analysis::shape::ModulePattern;
        let wrappers = |src: &str| -> Vec<(String, String)> {
            with_inputs(src, |inputs| {
                inputs
                    .program_shape
                    .map(|s| {
                        s.patterns
                            .iter()
                            .filter_map(|p| match p {
                                ModulePattern::WrapperOverRecursion {
                                    wrapper_fn,
                                    inner_fn,
                                    ..
                                } => Some((wrapper_fn.clone(), inner_fn.clone())),
                                _ => None,
                            })
                            .collect()
                    })
                    .unwrap_or_default()
            })
        };
        assert!(
            wrappers(&rle_source()).contains(&("encode".to_string(), "encodeLoop".to_string())),
            "rle: {:?}",
            wrappers(&rle_source())
        );
        assert!(
            wrappers(&sparse_source())
                .contains(&("encodeSparse".to_string(), "sparseLoop".to_string())),
            "sparse: {:?}",
            wrappers(&sparse_source())
        );
        assert!(
            wrappers(&sum_acc_source()).contains(&("sum".to_string(), "sumTR".to_string())),
            "sum_acc: {:?}",
            wrappers(&sum_acc_source())
        );
    }

    #[test]
    fn monoidal_spec_equivalence_emitted_for_sum_acc() {
        // The MONOIDAL flavor of the same accumulator-generalization schema: the
        // detector recognizes `sum(xs) = sumDirect(xs)` (sum = sumTR(·, 0), an
        // additive fold) and emits the shared `loop_gen` skeleton closed by
        // `omega` — NOT the codec roundtrip chain. Evidence the schema is one
        // thing across codec and monoidal, not two bespoke recognizers.
        let all: String = with_inputs(&sum_acc_source(), structural_lemma_groups)
            .iter()
            .flatten()
            .map(|(_, t)| t.as_str())
            .collect();
        // The law itself + the strengthened loop invariant.
        assert!(all.contains("sum xs = sumDirect xs"), "{all}");
        assert!(
            all.contains("sumTR list acc = acc + sumDirect list"),
            "{all}"
        );
        // The shared induct-and-instantiate skeleton, additive closer.
        assert!(all.contains("induction list with"), "{all}");
        assert!(
            all.contains("rw [ih (acc + h)]") && all.contains("omega"),
            "{all}"
        );
        // It must NOT drag in the codec bricks (no inverse / counted-repeat here).
        assert!(!all.contains("flush_fold_step"), "{all}");
        assert!(!all.contains("inv_append"), "{all}");
    }

    #[test]
    fn relational_chain_emitted_for_rle_and_sparse() {
        // The relational-brick emitter must produce the FULL roundtrip chain for
        // BOTH encoders, with the IDENTICAL generic `flush_fold_step` tactic —
        // the locksmith, not a per-program key. (Text-level pin; the lake-gated
        // proof_spec tests confirm it kernel-proves.)
        let rle: String = with_inputs(&rle_source(), structural_lemma_groups)
            .iter()
            .flatten()
            .map(|(_, t)| t.as_str())
            .collect();
        // The law itself + the strengthened invariant + the crux, all present.
        assert!(rle.contains("decode (encode xs) = xs"), "{rle}");
        assert!(
            rle.contains("decode (flushAcc (encodeFold acc x)) = decode (flushAcc acc) ++ [x]"),
            "{rle}"
        );
        assert!(rle.contains("unfold encodeFold flushAcc"), "{rle}");
        // The neutral accumulator is rendered from the wrapper body.
        assert!(
            rle.contains("{ runs := [], current := \"\", count := 0 }"),
            "{rle}"
        );

        let sparse: String = with_inputs(&sparse_source(), structural_lemma_groups)
            .iter()
            .flatten()
            .map(|(_, t)| t.as_str())
            .collect();
        assert!(
            sparse.contains("decodeSparse (encodeSparse xs) = xs"),
            "{sparse}"
        );
        assert!(sparse.contains("repeat0 1 = [0]"), "{sparse}");
        assert!(sparse.contains("unfold sparseStep flushSparse"), "{sparse}");
        assert!(sparse.contains("{ out := [], pending := 0 }"), "{sparse}");
        // The crux tactic is byte-for-byte the SAME combinator on both (only the
        // role names differ) — the evidence it generalizes.
        let crux = "split <;> (try split) <;> (try split) <;>";
        assert!(rle.contains(crux) && sparse.contains(crux));
    }

    #[test]
    fn multi_int_fields_each_get_a_lemma() {
        // twofield.av: `meterStep` has a non-negative `seen` AND a strictly
        // decreasing `budget`. Discovery must emit a lemma for EACH field, not
        // just the first — `seen`'s nonneg invariant and `budget`'s bounded step.
        let groups = with_inputs(&twofield_source(), structural_lemma_groups);
        let all: String = groups.iter().flatten().map(|(_, t)| t.as_str()).collect();
        assert!(all.contains("0 <= (meterStep acc x).seen"), "{all}");
        assert!(
            all.contains("acc.budget - 1 <= (meterStep acc x).budget"),
            "{all}"
        );
        assert!(
            all.contains("(meterStep acc x).budget <= acc.budget - 1"),
            "{all}"
        );
    }
}
