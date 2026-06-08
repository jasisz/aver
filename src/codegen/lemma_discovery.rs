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

/// Run lemma discovery over every `verify ... law` in the entry module: build
/// each law's [`LawProofCone`], a typed variable context, the enumerated
/// terms, and the candidate equations. Pure analysis — no VM, no prover, no
/// file writes.
pub fn run_discovery(inputs: &ProofLowerInputs) -> Vec<LawDiscovery> {
    let mut reports = Vec::new();
    for item in inputs.entry_items {
        let TopLevel::Verify(vb) = item else {
            continue;
        };
        let VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        let cone = LawProofCone::compute(law, &vb.fn_name, inputs);
        let cone_fn_count = cone.pure_fns().len();

        // Gate very large cones out of the naive enumerator (see MAX_CONE_FNS).
        let mut binders = Vec::new();
        let mut conjectures = Vec::new();
        let mut term_count = 0;
        let mut terms_truncated = false;
        let mut conjectures_truncated = false;
        let skipped_large_cone = cone_fn_count > MAX_CONE_FNS;
        if !skipped_large_cone {
            binders = variable_context(&cone);
            let ops = operations(&cone, &binders);
            let (terms, tt) = enumerate_terms(&binders, &ops, MAX_TERM_SIZE);
            let (conj, ct) = conjectures_from_terms(&terms, &binders);
            term_count = terms.len();
            terms_truncated = tt;
            conjectures = conj;
            conjectures_truncated = ct;
        }

        reports.push(LawDiscovery {
            subject_fn: vb.fn_name.clone(),
            law_name: law.name.clone(),
            cone_fns: cone.pure_fns().iter().map(|fd| fd.name.clone()).collect(),
            cone_types: cone
                .types()
                .iter()
                .map(|td| crate::codegen::common::type_def_name(td).to_string())
                .collect(),
            binders,
            proved: Vec::new(),
            stats: DiscoveryStats {
                cone_fn_count,
                term_count,
                conjecture_count: conjectures.len(),
                terms_truncated,
                conjectures_truncated,
                skipped_large_cone,
                vm_filtered: false,
                candidates_refuted: 0,
                max_term_size: MAX_TERM_SIZE,
            },
            conjectures,
        });
    }
    reports
}

/// Mint the shared variable context: up to [`MAX_VARS_PER_TYPE`] variables for
/// each distinct type that appears as a cone fn parameter. Deterministic —
/// types are keyed and ordered by their rendered name, variables are `x0`,
/// `x1`, … in that order.
fn variable_context(cone: &LawProofCone) -> Vec<Binder> {
    let mut param_types: BTreeMap<String, Type> = BTreeMap::new();
    for fd in cone.pure_fns() {
        for (_param_name, annotation) in &fd.params {
            let ty = crate::codegen::common::parse_type_annotation(annotation);
            // Skip unparseable annotations — `Type::Invalid` spuriously
            // unifies with any other `Invalid` under `==` and would mint junk
            // variables. A typecheck-clean cone never hits this.
            if ty == Type::Invalid {
                continue;
            }
            param_types.entry(render_type(&ty)).or_insert(ty);
        }
    }
    let mut binders = Vec::new();
    for ty in param_types.values() {
        for _ in 0..MAX_VARS_PER_TYPE {
            binders.push(Binder {
                name: format!("x{}", binders.len()),
                ty: ty.clone(),
            });
        }
    }
    binders
}

/// The enumeration vocabulary: every cone pure fn as an `Op`, plus a
/// `List.concat` instance for each list element type that occurs anywhere in
/// the variable context or the fns' signatures. `List.concat` is the one
/// builtin `decode_append` needs (`a ++ b`); more builtins join here later.
fn operations(cone: &LawProofCone, binders: &[Binder]) -> Vec<Op> {
    let mut ops = Vec::new();
    for fd in cone.pure_fns() {
        let params: Vec<Type> = fd
            .params
            .iter()
            .map(|(_, ann)| crate::codegen::common::parse_type_annotation(ann))
            .collect();
        let ret = crate::codegen::common::parse_type_annotation(&fd.return_type);
        // Skip fns whose signature didn't parse cleanly — an `Invalid` param
        // or return would unify with any other `Invalid` under `==` and build
        // ill-typed junk terms. A typecheck-clean cone never hits this.
        if ret == Type::Invalid || params.contains(&Type::Invalid) {
            continue;
        }
        ops.push(Op {
            callee: fd.name.clone(),
            params,
            ret,
        });
    }

    // Collect every list element type reachable from the variable types and
    // the fns' parameter/return types, then add a `List.concat` per element.
    let mut elem_types: BTreeMap<String, Type> = BTreeMap::new();
    for b in binders {
        collect_list_elem_types(&b.ty, &mut elem_types);
    }
    for op in &ops {
        for p in &op.params {
            collect_list_elem_types(p, &mut elem_types);
        }
        collect_list_elem_types(&op.ret, &mut elem_types);
    }
    for elem in elem_types.values() {
        let list_ty = Type::List(Box::new(elem.clone()));
        ops.push(Op {
            callee: "List.concat".to_string(),
            params: vec![list_ty.clone(), list_ty.clone()],
            ret: list_ty,
        });
    }
    ops
}

/// Walk a type, recording every `List<elem>` element type by rendered name.
fn collect_list_elem_types(ty: &Type, out: &mut BTreeMap<String, Type>) {
    match ty {
        Type::List(elem) | Type::Vector(elem) => {
            out.entry(render_type(elem))
                .or_insert_with(|| (**elem).clone());
            collect_list_elem_types(elem, out);
        }
        Type::Option(inner) => collect_list_elem_types(inner, out),
        Type::Result(a, b) | Type::Map(a, b) => {
            collect_list_elem_types(a, out);
            collect_list_elem_types(b, out);
        }
        Type::Tuple(items) => {
            for t in items {
                collect_list_elem_types(t, out);
            }
        }
        Type::Fn(args, ret, _) => {
            for a in args {
                collect_list_elem_types(a, out);
            }
            collect_list_elem_types(ret, out);
        }
        _ => {}
    }
}

/// Bottom-up typed enumeration: size-1 terms are the variables; a size-`k`
/// term is an `Op` applied to already-built sub-terms whose sizes sum to
/// `k - 1` and whose types match the op's parameters. Deduplicated by
/// rendering. Returns the terms and whether [`MAX_TERMS`] truncated the run.
fn enumerate_terms(binders: &[Binder], ops: &[Op], max_size: usize) -> (Vec<EnumTerm>, bool) {
    let mut terms: Vec<EnumTerm> = Vec::new();
    let mut by_size: Vec<Vec<usize>> = vec![Vec::new(); max_size + 1];
    let mut seen: HashSet<String> = HashSet::new();
    let mut truncated = false;

    // Size 1: the variables.
    for (i, b) in binders.iter().enumerate() {
        let node = TermNode::Var(i);
        if seen.insert(node.render(binders)) {
            by_size[1].push(terms.len());
            terms.push(EnumTerm {
                node,
                ty: b.ty.clone(),
            });
        }
    }

    'sizes: for size in 2..=max_size {
        for op in ops {
            let arity = op.params.len();
            if arity == 0 || arity > size - 1 {
                continue;
            }
            for comp in compositions(size - 1, arity) {
                // Per-argument candidate pools: terms of the right size whose
                // type matches the op's parameter at that position.
                let mut pools: Vec<Vec<usize>> = Vec::with_capacity(arity);
                let mut any_empty = false;
                for (j, &part) in comp.iter().enumerate() {
                    let pool: Vec<usize> = by_size[part]
                        .iter()
                        .copied()
                        .filter(|&id| terms[id].ty == op.params[j])
                        .collect();
                    if pool.is_empty() {
                        any_empty = true;
                        break;
                    }
                    pools.push(pool);
                }
                if any_empty {
                    continue;
                }
                let (combos, combos_capped) = cartesian(&pools, CARTESIAN_CAP);
                if combos_capped {
                    truncated = true;
                }
                for combo in combos {
                    if terms.len() >= MAX_TERMS {
                        truncated = true;
                        break 'sizes;
                    }
                    let args: Vec<TermNode> =
                        combo.iter().map(|&id| terms[id].node.clone()).collect();
                    let node = TermNode::App {
                        callee: op.callee.clone(),
                        args,
                    };
                    let rendered = node.render(binders);
                    if seen.insert(rendered) {
                        by_size[size].push(terms.len());
                        terms.push(EnumTerm {
                            node,
                            ty: op.ret.clone(),
                        });
                    }
                }
            }
        }
    }

    (terms, truncated)
}

/// Pair terms into candidate equations. Two terms become a candidate iff they
/// have the **same result type** and the **same free-variable SET** (variable
/// multiplicity ignored). For a universally-quantified equational law
/// `∀ vars. L == R` both sides range over the same variables, so this is a
/// sound necessary condition and the QuickSpec-style variable-aware batching
/// that keeps the count tractable.
///
/// What it KEEPS: homomorphism / distributivity (`decode_append`) and also
/// same-set shapes like `f(x, x) == g(x)` ({x} == {x}).
/// What it DROPS, by design for now: pairs whose two sides have DIFFERENT
/// free-var sets — projections / absorptions where one side ignores a
/// variable the other mentions (`head([x, ..xs]) == x`, `xs ++ [] == xs`).
/// The constant-identity ones also need nil/literals, not yet in the
/// vocabulary; a `vars(L) ⊇ vars(R)` relaxation reaches the projection class
/// once constructors/literals land.
/// Out of scope independently of this filter: 3-variable laws (associativity)
/// — only [`MAX_VARS_PER_TYPE`] vars are minted — and cross-module type-name
/// unification (terms are typed by `==` on parsed annotations, which does not
/// collapse `Module.Bare` vs `Bare`; a completeness gap, never unsound).
///
/// Returns the candidates (size-ascending, so the simplest survive a cap) and
/// whether the run was truncated by either the output cap [`MAX_CONJECTURES`]
/// or the work cap [`MAX_PAIRS_EXAMINED`] (which bounds the O(bucket²) pair
/// scan so a large cone can't stall `--discover`).
fn conjectures_from_terms(terms: &[EnumTerm], binders: &[Binder]) -> (Vec<Conjecture>, bool) {
    // Bucket term ids by result type, sorted within each bucket by
    // (size, rendering) so smaller candidates are generated first.
    let mut buckets: BTreeMap<String, Vec<usize>> = BTreeMap::new();
    for (id, t) in terms.iter().enumerate() {
        buckets.entry(render_type(&t.ty)).or_default().push(id);
    }
    for ids in buckets.values_mut() {
        ids.sort_by_key(|&id| (terms[id].node.size(), terms[id].node.render(binders)));
    }

    let mut out = Vec::new();
    let mut seen_pairs: HashSet<(String, String)> = HashSet::new();
    let mut truncated = false;
    let mut pairs_examined = 0usize;

    'buckets: for ids in buckets.values() {
        for a in 0..ids.len() {
            for b in (a + 1)..ids.len() {
                // Work budget: bound the O(bucket²) scan up front, before the
                // free-var / render work, so filtered pairs still count toward
                // termination (the output cap alone can't bound a bucket of
                // mostly different-free-var pairs).
                pairs_examined += 1;
                if pairs_examined >= MAX_PAIRS_EXAMINED {
                    truncated = true;
                    break 'buckets;
                }

                let lt = &terms[ids[a]];
                let rt = &terms[ids[b]];

                let mut lv = BTreeSet::new();
                lt.node.free_vars(&mut lv);
                let mut rv = BTreeSet::new();
                rt.node.free_vars(&mut rv);
                if lv != rv {
                    continue;
                }

                let lr = lt.node.render(binders);
                let rr = rt.node.render(binders);
                if lr == rr {
                    continue;
                }
                let pair = if lr < rr {
                    (lr.clone(), rr.clone())
                } else {
                    (rr.clone(), lr.clone())
                };
                if !seen_pairs.insert(pair) {
                    continue;
                }

                if out.len() >= MAX_CONJECTURES {
                    truncated = true;
                    break 'buckets;
                }
                out.push(Conjecture {
                    lhs: lt.node.clone(),
                    rhs: rt.node.clone(),
                    ty: lt.ty.clone(),
                });
            }
        }
    }

    (out, truncated)
}

/// All length-`parts` compositions of `total` into positive parts (ordered).
fn compositions(total: usize, parts: usize) -> Vec<Vec<usize>> {
    if parts == 0 {
        return if total == 0 { vec![vec![]] } else { vec![] };
    }
    if parts == 1 {
        return if total >= 1 {
            vec![vec![total]]
        } else {
            vec![]
        };
    }
    let mut out = Vec::new();
    for first in 1..=total.saturating_sub(parts - 1) {
        for mut rest in compositions(total - first, parts - 1) {
            let mut v = vec![first];
            v.append(&mut rest);
            out.push(v);
        }
    }
    out
}

/// Cartesian product of the per-argument id pools, capped at `cap` tuples.
/// Returns the tuples and whether the cap truncated the product (so the
/// caller can surface a non-silent partial enumeration).
fn cartesian(pools: &[Vec<usize>], cap: usize) -> (Vec<Vec<usize>>, bool) {
    let mut capped = false;
    let mut acc: Vec<Vec<usize>> = vec![Vec::new()];
    for pool in pools {
        let mut next: Vec<Vec<usize>> = Vec::new();
        'fill: for prefix in &acc {
            for &id in pool {
                let mut v = prefix.clone();
                v.push(id);
                next.push(v);
                if next.len() >= cap {
                    capped = true;
                    break 'fill;
                }
            }
        }
        acc = next;
    }
    (acc, capped)
}

/// Render an Aver [`Type`] back to source-shaped text (`List<Run>`,
/// `Result<T, String>`, `(A, B)`, …) for keying, reporting, and dedup.
fn render_type(ty: &Type) -> String {
    match ty {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Result(a, b) => format!("Result<{}, {}>", render_type(a), render_type(b)),
        Type::Option(a) => format!("Option<{}>", render_type(a)),
        Type::List(a) => format!("List<{}>", render_type(a)),
        Type::Vector(a) => format!("Vector<{}>", render_type(a)),
        Type::Map(a, b) => format!("Map<{}, {}>", render_type(a), render_type(b)),
        Type::Tuple(items) => format!(
            "({})",
            items.iter().map(render_type).collect::<Vec<_>>().join(", ")
        ),
        Type::Fn(args, ret, _) => format!(
            "({}) -> {}",
            args.iter().map(render_type).collect::<Vec<_>>().join(", "),
            render_type(ret)
        ),
        Type::Named { name, .. } => name.clone(),
        Type::Var(n) => n.clone(),
        Type::Invalid => "<invalid>".to_string(),
    }
}

// ===========================================================================
// Phase 2c — VM-filter (Aver's VM as the test oracle).
//
// Each enumerated candidate (2b) is a *guess*. The VM-filter instantiates the
// equation's variables with concrete sample values and runs BOTH sides on the
// Aver VM — if any sample makes the sides disagree, the candidate is a
// counterexample and is dropped. Survivors are not theorems (the charter's
// proved-or-dropped gate still demands a kernel proof, 2d), but a single false
// candidate that the VM refutes never reaches the (expensive) prover.
//
// Conservative by construction (charter's semantic-model-mismatch caution): a
// VM error, an unmodeled builtin, or an out-of-guard `Int` magnitude makes a
// sample INCONCLUSIVE — it never refutes. So a backend-true lemma is never
// wrongly dropped because the bounded-`Int` VM diverged from the unbounded
// proof model; the worst case is a false candidate slipping to the prover,
// which then rejects it.
// ===========================================================================

/// Variable instantiations tried per candidate before declaring it
/// counterexample-free on samples.
const VM_FILTER_ROUNDS: usize = 6;
/// Opcode cap per `run_named_function` so a pathological term can't hang.
const VM_STEP_LIMIT: u64 = 1_000_000;
/// `Int` magnitudes at/above this in a result make the sample inconclusive —
/// near i64 range the bounded VM wraps and diverges from the proof model.
const VM_INT_MAGNITUDE_GUARD: u64 = 1 << 40;
/// Recursion bound for the sample-value generator (records → fields → …).
const SAMPLE_DEPTH: usize = 3;

/// Refute or keep every candidate of every (enumerated) law by running both
/// sides on the Aver VM over sample variable assignments. Compiles the
/// program's pure cone ONCE and reuses it across all candidates. On compile
/// failure it leaves every candidate in place (conservative — discovery stays
/// a superset, the prover is the real gate).
pub fn vm_filter(reports: &mut [LawDiscovery], inputs: &ProofLowerInputs) {
    let Some(mut vm) = compile_oracle_vm(inputs) else {
        return;
    };
    for report in reports.iter_mut() {
        if report.stats.skipped_large_cone || report.conjectures.is_empty() {
            continue;
        }
        let samples: Vec<Vec<Value>> = report
            .binders
            .iter()
            .map(|b| sample_values(&b.ty, inputs, SAMPLE_DEPTH))
            .collect();
        let mut survivors = Vec::with_capacity(report.conjectures.len());
        let mut refuted = 0usize;
        for c in &report.conjectures {
            if vm_refutes(c, &samples, &mut vm) {
                refuted += 1;
            } else {
                survivors.push(c.clone());
            }
        }
        report.stats.vm_filtered = true;
        report.stats.candidates_refuted = refuted;
        report.conjectures = survivors;
    }
}

/// `true` iff some sample assignment makes the two sides evaluate to DIFFERENT
/// values (both conclusive). Inconclusive samples (eval error / out-of-guard
/// magnitude) are skipped, never counted as a refutation.
fn vm_refutes(c: &Conjecture, samples: &[Vec<Value>], vm: &mut crate::vm::VM) -> bool {
    for r in 0..VM_FILTER_ROUNDS {
        // Offset by binder index so two same-typed variables usually differ in
        // a round (needed to refute e.g. spurious commutativity and to give
        // distributivity a non-degenerate test).
        let assignment: Vec<Option<Value>> = samples
            .iter()
            .enumerate()
            .map(|(i, s)| {
                if s.is_empty() {
                    None
                } else {
                    Some(s[(r + i) % s.len()].clone())
                }
            })
            .collect();
        let (Some(l), Some(rhs)) = (
            eval_term(&c.lhs, &assignment, vm),
            eval_term(&c.rhs, &assignment, vm),
        ) else {
            continue;
        };
        if !value_within_int_guard(&l) || !value_within_int_guard(&rhs) {
            continue;
        }
        if l != rhs {
            return true;
        }
    }
    false
}

/// Evaluate a term under a variable assignment on the VM. `None` = inconclusive
/// (unassigned variable, eval error, or an unmodeled builtin callee).
fn eval_term(
    node: &TermNode,
    assignment: &[Option<Value>],
    vm: &mut crate::vm::VM,
) -> Option<Value> {
    match node {
        TermNode::Var(i) => assignment.get(*i).cloned().flatten(),
        TermNode::App { callee, args } => {
            let argvals: Option<Vec<Value>> =
                args.iter().map(|a| eval_term(a, assignment, vm)).collect();
            let argvals = argvals?;
            // `List.concat` is the only builtin in the current vocabulary;
            // evaluate it directly on `Value` (no VM call). Other builtins are
            // left unmodeled → inconclusive.
            if callee == "List.concat" {
                if argvals.len() != 2 {
                    return None;
                }
                return crate::value::list_concat(&argvals[0], &argvals[1]);
            }
            // User cone fn: call it on the VM with the constructed arguments.
            let nanargs: Vec<NanValue> = argvals
                .iter()
                .map(|v| NanValue::from_value(v, &mut vm.arena))
                .collect();
            let result = vm.run_named_function(callee, &nanargs).ok()?;
            Some(result.to_value(&vm.arena))
        }
    }
}

/// Compile the program's pure functions into a runnable VM (the oracle).
/// Mirrors the `aver run` / `vm_verify` setup; `None` on any compile failure.
fn compile_oracle_vm(inputs: &ProofLowerInputs) -> Option<crate::vm::VM> {
    let resolved = crate::ir::hir::resolve_program(inputs.symbol_table, inputs.entry_items);
    let mut arena = crate::nan_value::Arena::new();
    let (code, globals) = crate::vm::compile_program_with_mir_fallback(
        &resolved,
        inputs.symbol_table,
        &mut arena,
        None,
    )
    .ok()?;
    let mut vm = crate::vm::VM::new(code, globals, arena);
    vm.set_step_limit(Some(VM_STEP_LIMIT));
    vm.run_top_level().ok()?;
    Some(vm)
}

/// `true` iff no `Int` anywhere in the value is near i64 range — beyond the
/// guard the bounded VM wraps, so the sample can't be trusted against the
/// unbounded proof model.
fn value_within_int_guard(v: &Value) -> bool {
    match v {
        Value::Int(i) => i.unsigned_abs() < VM_INT_MAGNITUDE_GUARD,
        Value::Ok(b) | Value::Err(b) | Value::Some(b) => value_within_int_guard(b),
        Value::Tuple(xs) => xs.iter().all(value_within_int_guard),
        Value::Record { fields, .. } => fields.iter().all(|(_, x)| value_within_int_guard(x)),
        Value::Variant { fields, .. } => fields.iter().all(value_within_int_guard),
        Value::List(_) | Value::Vector(_) => crate::value::list_to_vec(v)
            .map(|xs| xs.iter().all(value_within_int_guard))
            .unwrap_or(true),
        _ => true,
    }
}

/// Type-directed sample-value generator: a few concrete Aver [`Value`]s of a
/// type, for the VM-filter to instantiate equation variables. Kept tiny and
/// low-magnitude (`Int` in `0..2`) so bounded-`Int` wrap can't manufacture a
/// false agreement. Records / variants are built from `inputs.find_type_def`;
/// `depth` bounds recursion (recursive ADTs terminate via the empty-list base
/// or by exhausting variants that reference the type).
fn sample_values(ty: &Type, inputs: &ProofLowerInputs, depth: usize) -> Vec<Value> {
    match ty {
        Type::Int => vec![Value::Int(0), Value::Int(1), Value::Int(2)],
        Type::Float => vec![Value::Float(0.0), Value::Float(1.0)],
        Type::Str => vec![
            Value::Str(String::new()),
            Value::Str("a".to_string()),
            Value::Str("b".to_string()),
        ],
        Type::Bool => vec![Value::Bool(true), Value::Bool(false)],
        Type::Unit => vec![Value::Unit],
        Type::List(elem) => {
            let es = sample_values(elem, inputs, depth);
            let mut out = vec![crate::value::list_from_vec(Vec::new())];
            if let Some(e0) = es.first() {
                out.push(crate::value::list_from_vec(vec![e0.clone()]));
            }
            if es.len() >= 2 {
                out.push(crate::value::list_from_vec(vec![
                    es[0].clone(),
                    es[1].clone(),
                ]));
            }
            out
        }
        Type::Option(inner) => {
            let mut out = vec![Value::None];
            if let Some(v) = sample_values(inner, inputs, depth).into_iter().next() {
                out.push(Value::Some(Box::new(v)));
            }
            out
        }
        Type::Result(ok, err) => {
            let mut out = Vec::new();
            if let Some(v) = sample_values(ok, inputs, depth).into_iter().next() {
                out.push(Value::Ok(Box::new(v)));
            }
            if let Some(v) = sample_values(err, inputs, depth).into_iter().next() {
                out.push(Value::Err(Box::new(v)));
            }
            out
        }
        Type::Tuple(items) => {
            let parts: Option<Vec<Value>> = items
                .iter()
                .map(|t| sample_values(t, inputs, depth).into_iter().next())
                .collect();
            parts.map(|p| vec![Value::Tuple(p)]).unwrap_or_default()
        }
        Type::Named { name, .. } => {
            if depth == 0 {
                return Vec::new();
            }
            match inputs.find_type_def(name) {
                Some(td) => named_sample(td, inputs, depth - 1),
                None => Vec::new(),
            }
        }
        _ => Vec::new(),
    }
}

/// Build sample values for a user ADT (`Product` → records, `Sum` → variants),
/// recursing into field types at the (already-decremented) `depth`.
fn named_sample(td: &crate::ast::TypeDef, inputs: &ProofLowerInputs, depth: usize) -> Vec<Value> {
    use std::sync::Arc;
    let field_sample = |fty: &str, pick_last: bool| -> Option<Value> {
        let ty = crate::codegen::common::parse_type_annotation(fty);
        let s = sample_values(&ty, inputs, depth);
        if pick_last {
            s.into_iter().last()
        } else {
            s.into_iter().next()
        }
    };
    match td {
        crate::ast::TypeDef::Product { name, fields, .. } => {
            let build = |pick_last: bool| -> Option<Value> {
                let built: Option<Vec<(String, Value)>> = fields
                    .iter()
                    .map(|(fname, fty)| field_sample(fty, pick_last).map(|v| (fname.clone(), v)))
                    .collect();
                built.map(|f| Value::Record {
                    type_name: name.clone(),
                    fields: Arc::from(f.as_slice()),
                })
            };
            let mut out = Vec::new();
            if let Some(first) = build(false) {
                out.push(first);
            }
            if let Some(second) = build(true)
                && !out.contains(&second)
            {
                out.push(second);
            }
            out
        }
        crate::ast::TypeDef::Sum { name, variants, .. } => {
            let mut out = Vec::new();
            for v in variants {
                let built: Option<Vec<Value>> = v
                    .fields
                    .iter()
                    .map(|fty| field_sample(fty, false))
                    .collect();
                if let Some(fields) = built {
                    out.push(Value::Variant {
                        type_name: name.clone(),
                        variant: v.name.clone(),
                        fields: Arc::from(fields.as_slice()),
                    });
                }
                if out.len() >= 2 {
                    break;
                }
            }
            out
        }
    }
}

// ===========================================================================
// Phase 2d — render a candidate to a Lean theorem for kernel proof.
//
// A VM-survivor is still a guess; the proved-or-dropped gate demands a kernel
// proof. These pure helpers render a candidate to standalone Lean theorem text
// using the SAME name/type mapping the program defs are emitted with (so the
// theorem references `decode` / `Run` / `++` exactly as generated). The actual
// `lake build` (needs a CodegenContext + prover process) lives in the CLI.
// ===========================================================================

/// Render a candidate as a standalone Lean theorem with the list-induction
/// template, IF it has a list-typed free variable to induct on (else `None` —
/// the template doesn't apply). Proof:
/// `induction <v> with | nil => simp [fns] | cons .. ih => simp [fns, ih, List.append_assoc]`,
/// which closes the list-homomorphism class (`decode_append` and kin).
pub fn lean_lemma_theorem(c: &Conjecture, binders: &[Binder], name: &str) -> Option<String> {
    let mut fvs = BTreeSet::new();
    c.lhs.free_vars(&mut fvs);
    c.rhs.free_vars(&mut fvs);
    // Induction target: the first list-typed free variable.
    let induct = fvs
        .iter()
        .copied()
        .find(|&i| matches!(binders.get(i).map(|b| &b.ty), Some(Type::List(_))))?;

    // Lean binder declarations for the free variables, in index order.
    let binder_decls: Vec<String> = fvs
        .iter()
        .filter_map(|&i| binders.get(i))
        .map(|b| {
            format!(
                "({} : {})",
                b.name,
                crate::codegen::lean::type_to_lean(&b.ty)
            )
        })
        .collect();

    // User fns referenced (the simp unfold list); `List.concat` is `++`, not a
    // simp lemma, so it's excluded.
    let mut fns = BTreeSet::new();
    collect_user_fns(&c.lhs, &mut fns);
    collect_user_fns(&c.rhs, &mut fns);
    let unfolds: Vec<String> = fns
        .iter()
        .map(|f| crate::codegen::lean::aver_name_to_lean(f))
        .collect();

    let lhs = term_to_lean(&c.lhs, binders);
    let rhs = term_to_lean(&c.rhs, binders);
    let iv = &binders[induct].name;
    let nil_simp = unfolds.join(", ");
    let cons_simp = {
        let mut v = unfolds.clone();
        v.push("ih".to_string());
        v.push("List.append_assoc".to_string());
        v.join(", ")
    };
    Some(format!(
        "theorem {name} {binders} : {lhs} = {rhs} := by\n  \
         induction {iv} with\n  \
         | nil => simp [{nil_simp}]\n  \
         | cons head tail ih => simp [{cons_simp}]\n",
        binders = binder_decls.join(" "),
    ))
}

/// Render a term to a Lean expression: `Var` → binder name, `List.concat` →
/// `(a ++ b)`, any other callee → `(f arg …)` via [`aver_name_to_lean`].
fn term_to_lean(node: &TermNode, binders: &[Binder]) -> String {
    match node {
        TermNode::Var(i) => binders
            .get(*i)
            .map(|b| b.name.clone())
            .unwrap_or_else(|| format!("x{i}")),
        TermNode::App { callee, args } => {
            if callee == "List.concat" && args.len() == 2 {
                return format!(
                    "({} ++ {})",
                    term_to_lean(&args[0], binders),
                    term_to_lean(&args[1], binders)
                );
            }
            let lean_fn = crate::codegen::lean::aver_name_to_lean(callee);
            let rendered: Vec<String> = args.iter().map(|a| term_to_lean(a, binders)).collect();
            format!("({} {})", lean_fn, rendered.join(" "))
        }
    }
}

/// Collect user-fn callees in a term (everything but the `List.concat` builtin).
fn collect_user_fns(node: &TermNode, out: &mut BTreeSet<String>) {
    if let TermNode::App { callee, args } = node {
        if callee != "List.concat" {
            out.insert(callee.clone());
        }
        for a in args {
            collect_user_fns(a, out);
        }
    }
}

/// `true` iff the candidate is the list-homomorphism shape
/// `g(a ++ b) == g(a) ++ g(b)` (either orientation), `g` a user fn, `a`/`b`
/// distinct variables. These are the highest-value proof targets, ranked first.
fn is_homomorphism(c: &Conjecture) -> bool {
    fn oriented(l: &TermNode, r: &TermNode) -> bool {
        let TermNode::App {
            callee: g,
            args: la,
        } = l
        else {
            return false;
        };
        if g == "List.concat" || la.len() != 1 {
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
                callee: g1,
                args: r1,
            },
            TermNode::App {
                callee: g2,
                args: r2,
            },
        ) = (&ra[0], &ra[1])
        else {
            return false;
        };
        g1 == g
            && g2 == g
            && r1.len() == 1
            && r2.len() == 1
            && matches!((&r1[0], &r2[0]), (TermNode::Var(a2), TermNode::Var(b2)) if a2 == a && b2 == b)
    }
    oriented(&c.lhs, &c.rhs) || oriented(&c.rhs, &c.lhs)
}

/// A stable content hash of the discovery surface (every pure fn's signature),
/// used as the committed-lemma cache key: a matching hash means the committed
/// lemmas can be REPLAYED (re-verified) instead of rediscovered. FNV-1a over
/// the sorted canonical signatures — stable across runs/platforms (unlike
/// `DefaultHasher`). This is only a performance key (skip rediscovery); the
/// soundness guard is re-verification every build, never the hash (charter).
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

// ===========================================================================
// Layer 3 (first brick) — STRUCTURE-DIRECTED guarded conjectures.
//
// Some lemmas an inductive proof needs are GUARDED and arithmetic
// (`repeat(c, n+1) = repeat(c, n) ++ [c]` under `0 <= n`) — unreachable by the
// equational enumerator (no `n+1`/literals in its vocabulary). They are instead
// CONJECTURED from a recognized recursion shape and proved with a dedicated
// template (here: a fixed `Int.natAbs` bridge + a fuel-unfold). This is the
// first member of the accumulator-generalization family (the RLE retirement
// target); `count_nonneg` / `flush_fold_step` / `loop_gen` follow the same
// detect-shape → conjecture → templated-proof pattern (charter layer 3).
// ===========================================================================

/// A counted-append `repeat`: `fn f(c, n) = match n <= 0 { true -> []; false ->
/// List.concat(f(c, n - 1), [c]) }`. Detected by SHAPE (not name); `n`'s type
/// must be `Int`. Yields the successor-advance lemma `f(c, n+1) = f(c,n) ++ [c]`.
struct CountedRepeat {
    fn_name: String,
    item_type: String,
}

fn cr_ident_name(e: &crate::ast::Spanned<crate::ast::Expr>) -> Option<String> {
    match &e.node {
        crate::ast::Expr::Ident(n) => Some(n.clone()),
        crate::ast::Expr::Resolved { name, .. } => Some(name.clone()),
        _ => None,
    }
}

fn cr_is_int_lit(e: &crate::ast::Spanned<crate::ast::Expr>, n: i64) -> bool {
    matches!(&e.node, crate::ast::Expr::Literal(crate::ast::Literal::Int(k)) if *k == n)
}

fn cr_fn_call(
    e: &crate::ast::Spanned<crate::ast::Expr>,
) -> Option<(String, &[crate::ast::Spanned<crate::ast::Expr>])> {
    if let crate::ast::Expr::FnCall(callee, args) = &e.node {
        let name = crate::codegen::common::expr_to_dotted_name(&callee.node)?;
        Some((name, args.as_slice()))
    } else {
        None
    }
}

/// Recognize the counted-append `repeat` shape by structure (see [`CountedRepeat`]).
fn detect_counted_repeat(fd: &crate::ast::FnDef) -> Option<CountedRepeat> {
    use crate::ast::{BinOp, Expr, Literal, Pattern, Stmt};
    if fd.params.len() != 2 || fd.params[1].1 != "Int" {
        return None;
    }
    let item_param = fd.params[0].0.as_str();
    let count_param = fd.params[1].0.as_str();

    let Some(Stmt::Expr(term)) = fd.body.stmts().last() else {
        return None;
    };
    let Expr::Match { subject, arms } = &term.node else {
        return None;
    };
    // subject = `count <= 0`
    let Expr::BinOp(BinOp::Lte, l, r) = &subject.node else {
        return None;
    };
    if cr_ident_name(l).as_deref() != Some(count_param) || !cr_is_int_lit(r, 0) {
        return None;
    }
    if arms.len() != 2 {
        return None;
    }
    let mut true_body = None;
    let mut false_body = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => true_body = Some(&arm.body),
            Pattern::Literal(Literal::Bool(false)) => false_body = Some(&arm.body),
            _ => return None,
        }
    }
    // true arm: `[]`
    if !matches!(&true_body?.node, Expr::List(items) if items.is_empty()) {
        return None;
    }
    // false arm: `List.concat(f(c, n - 1), [c])`
    let (concat, cargs) = cr_fn_call(false_body?)?;
    if concat != "List.concat" || cargs.len() != 2 {
        return None;
    }
    let (rec, rargs) = cr_fn_call(&cargs[0])?;
    if rec != fd.name || rargs.len() != 2 || cr_ident_name(&rargs[0]).as_deref() != Some(item_param)
    {
        return None;
    }
    let Expr::BinOp(BinOp::Sub, sl, sr) = &rargs[1].node else {
        return None;
    };
    if cr_ident_name(sl).as_deref() != Some(count_param) || !cr_is_int_lit(sr, 1) {
        return None;
    }
    let Expr::List(tail) = &cargs[1].node else {
        return None;
    };
    if tail.len() != 1 || cr_ident_name(&tail[0]).as_deref() != Some(item_param) {
        return None;
    }
    Some(CountedRepeat {
        fn_name: fd.name.clone(),
        item_type: fd.params[0].1.clone(),
    })
}

/// The Lean theorems for a counted-repeat advance, in dependency order: a fixed
/// `Int.natAbs` successor bridge, then the guarded advance proved by the
/// fuel-unfold template. Names/types use the same Lean mapping the program defs
/// are emitted with, so the theorem references the generated `repeat`/fuel defs.
fn counted_repeat_lemmas(shape: &CountedRepeat, base: &str) -> Vec<(String, String)> {
    let repeat_l = crate::codegen::lean::aver_name_to_lean(&shape.fn_name);
    let fuel_l = crate::codegen::lean::aver_name_to_lean(
        &crate::codegen::recursion::fuel_helper_name(&shape.fn_name),
    );
    let item_l = crate::codegen::lean::type_to_lean(
        &crate::codegen::common::parse_type_annotation(&shape.item_type),
    );
    let natabs = format!("{base}_natAbs_succ");
    let succ = format!("{base}_repeat_succ");

    let natabs_thm = format!(
        "theorem {natabs} (n : Int) (hn : 0 <= n) : Int.natAbs (n + 1) = Int.natAbs n + 1 := by\n  \
         apply Int.ofNat_inj.mp\n  \
         change (Int.natAbs (n + 1) : Int) = (Int.natAbs n : Int) + 1\n  \
         rw [Int.natAbs_of_nonneg (by omega), Int.natAbs_of_nonneg hn]\n"
    );
    let succ_thm = format!(
        "theorem {succ} (c : {item_l}) (n : Int) (hn : 0 <= n) : \
         {repeat_l} c (n + 1) = {repeat_l} c n ++ [c] := by\n  \
         unfold {repeat_l}\n  \
         rw [{natabs} n hn]\n  \
         have hpos : ¬ n + 1 <= 0 := by omega\n  \
         simp [{fuel_l}, hpos]\n"
    );
    vec![(natabs, natabs_thm), (succ, succ_thm)]
}

/// An RLE-fold `step`: `step(acc: Acc, c: Item) -> Acc` whose body is the
/// nested bool-match `match acc.<count> == 0 { true -> Acc{..}; false -> match
/// acc.<current> == c { true -> Acc{..}; false -> Acc{..} } }`. The discriminant
/// fields (`count`/`current`) are discovered by structural role. Yields the
/// well-formedness-preservation lemma `0 <= step(acc, c).count` under
/// `0 <= acc.count` — brick 2 of the accumulator-generalization family.
struct RleStep {
    fn_name: String,
    acc_type: String,
    item_type: String,
    count_field: String,
    current_field: String,
}

/// `acc.<field>` → the field name, if `e` is an attribute access of `acc`.
fn cr_attr_of(e: &crate::ast::Spanned<crate::ast::Expr>, acc: &str) -> Option<String> {
    if let crate::ast::Expr::Attr(obj, field) = &e.node
        && cr_ident_name(obj).as_deref() == Some(acc)
    {
        return Some(field.clone());
    }
    None
}

/// A 2-arm bool `match`: returns (subject, true-arm body, false-arm body).
fn cr_bool_match(
    e: &crate::ast::Spanned<crate::ast::Expr>,
) -> Option<(
    &crate::ast::Spanned<crate::ast::Expr>,
    &crate::ast::Spanned<crate::ast::Expr>,
    &crate::ast::Spanned<crate::ast::Expr>,
)> {
    use crate::ast::{Expr, Literal, Pattern};
    let Expr::Match { subject, arms } = &e.node else {
        return None;
    };
    if arms.len() != 2 {
        return None;
    }
    let mut t = None;
    let mut f = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => t = Some(arm.body.as_ref()),
            Pattern::Literal(Literal::Bool(false)) => f = Some(arm.body.as_ref()),
            _ => return None,
        }
    }
    Some((subject.as_ref(), t?, f?))
}

fn cr_is_record_of(e: &crate::ast::Spanned<crate::ast::Expr>, ty: &str) -> bool {
    matches!(&e.node, crate::ast::Expr::RecordCreate { type_name, .. } if type_name == ty)
}

/// Recognize the RLE-fold `step` shape by structure (see [`RleStep`]).
fn detect_rle_step(fd: &crate::ast::FnDef) -> Option<RleStep> {
    use crate::ast::{BinOp, Expr, Stmt};
    if fd.params.len() != 2 || fd.params[0].1 != fd.return_type {
        return None;
    }
    let acc_param = fd.params[0].0.as_str();
    let c_param = fd.params[1].0.as_str();
    let acc_type = &fd.params[0].1;
    let item_type = &fd.params[1].1;

    let Some(Stmt::Expr(term)) = fd.body.stmts().last() else {
        return None;
    };
    // outer: `match acc.<count> == 0 { true -> Acc{..}; false -> inner }`
    let (outer_subj, outer_true, outer_false) = cr_bool_match(term)?;
    let Expr::BinOp(BinOp::Eq, ol, or_) = &outer_subj.node else {
        return None;
    };
    let count_field = cr_attr_of(ol, acc_param)?;
    if !cr_is_int_lit(or_, 0) || !cr_is_record_of(outer_true, acc_type) {
        return None;
    }
    // inner: `match acc.<current> == c { true -> Acc{..}; false -> Acc{..} }`
    let (inner_subj, inner_true, inner_false) = cr_bool_match(outer_false)?;
    let Expr::BinOp(BinOp::Eq, il, ir) = &inner_subj.node else {
        return None;
    };
    let current_field = cr_attr_of(il, acc_param)?;
    if cr_ident_name(ir).as_deref() != Some(c_param)
        || !cr_is_record_of(inner_true, acc_type)
        || !cr_is_record_of(inner_false, acc_type)
        || count_field == current_field
    {
        return None;
    }
    Some(RleStep {
        fn_name: fd.name.clone(),
        acc_type: acc_type.clone(),
        item_type: item_type.clone(),
        count_field,
        current_field,
    })
}

/// The Lean theorem for step's count-invariant preservation, proved by the
/// case-split-on-guard template (nested `by_cases` on the step's discriminant
/// fields). No dependencies.
fn count_nonneg_lemma(step: &RleStep, base: &str) -> (String, String) {
    let step_l = crate::codegen::lean::aver_name_to_lean(&step.fn_name);
    let acc_l = crate::codegen::lean::type_to_lean(&crate::codegen::common::parse_type_annotation(
        &step.acc_type,
    ));
    let item_l = crate::codegen::lean::type_to_lean(
        &crate::codegen::common::parse_type_annotation(&step.item_type),
    );
    let count = &step.count_field;
    let current = &step.current_field;
    let name = format!("{base}_count_nonneg");
    let text = format!(
        "theorem {name} (acc : {acc_l}) (c : {item_l}) (hcount : 0 <= acc.{count}) : \
         0 <= ({step_l} acc c).{count} := by\n  \
         by_cases hzero : acc.{count} = 0\n  \
         · simp [{step_l}, hzero]\n  \
         · by_cases hsame : acc.{current} = c\n    \
         · simp [{step_l}, hzero, hsame]\n      \
         omega\n    \
         · simp [{step_l}, hzero, hsame]\n"
    );
    (name, text)
}

/// Structure-directed GUARDED lemma groups (layer 3): conjectures derived from
/// a recognized recursion shape rather than blind enumeration. Each group is a
/// set of co-dependent Lean theorems proved TOGETHER (one `lake build`, in
/// dependency order). Families so far: the counted-repeat advance
/// (`repeat_succ`, brick 1) and the RLE-fold count-invariant (`count_nonneg`,
/// brick 2). Returns one group per matching pure fn.
pub fn structural_lemma_groups(inputs: &ProofLowerInputs) -> Vec<Vec<(String, String)>> {
    let mut groups: Vec<Vec<(String, String)>> = Vec::new();
    for fd in inputs.pure_fns() {
        let base = format!("aver_structural_{}", groups.len());
        if let Some(shape) = detect_counted_repeat(fd) {
            groups.push(counted_repeat_lemmas(&shape, &base));
        } else if let Some(step) = detect_rle_step(fd) {
            groups.push(vec![count_nonneg_lemma(&step, &base)]);
        }
    }
    groups
}

/// Candidate indices in proof-attempt order: list-homomorphism shapes first
/// (the highest-value, template-closable class), then smallest-first. The
/// prover (CLI) walks this and attempts the top few within a build budget.
pub fn rank_candidate_indices(report: &LawDiscovery) -> Vec<usize> {
    let mut idx: Vec<usize> = (0..report.conjectures.len()).collect();
    idx.sort_by_key(|&i| {
        let c = &report.conjectures[i];
        let homo = if is_homomorphism(c) { 0 } else { 1 };
        (homo, c.lhs.size() + c.rhs.size(), c.render(&report.binders))
    });
    idx
}

/// Human-readable multi-line report for `aver proof --discover` output. Shows
/// each law's cone, variable legend, stats, a sample of the candidate
/// equations (after the VM-filter, survivors), and any kernel-proved lemmas.
pub fn render_report(reports: &[LawDiscovery]) -> String {
    const SAMPLE: usize = 12;
    let mut out = String::new();
    if reports.is_empty() {
        out.push_str("lemma discovery: no `verify ... law` blocks found\n");
        return out;
    }
    out.push_str(&format!(
        "lemma discovery (skeleton): {} law(s)\n",
        reports.len()
    ));
    for r in reports {
        out.push_str(&format!("\n• verify {} law {}\n", r.subject_fn, r.law_name));
        out.push_str(&format!("    cone fns:   [{}]\n", r.cone_fns.join(", ")));
        out.push_str(&format!("    cone types: [{}]\n", r.cone_types.join(", ")));
        if r.stats.skipped_large_cone {
            out.push_str(&format!(
                "    cone too large ({} fns > {}) — enumeration skipped (size-{} discovery not meaningful at this scope)\n",
                r.stats.cone_fn_count, MAX_CONE_FNS, r.stats.max_term_size
            ));
            continue;
        }
        let legend: Vec<String> = r
            .binders
            .iter()
            .map(|b| format!("{}: {}", b.name, render_type(&b.ty)))
            .collect();
        out.push_str(&format!("    variables:  [{}]\n", legend.join(", ")));
        out.push_str(&format!(
            "    enumerated {} terms (size ≤ {}{}), {} candidate equations{}\n",
            r.stats.term_count,
            r.stats.max_term_size,
            if r.stats.terms_truncated {
                ", TRUNCATED"
            } else {
                ""
            },
            r.stats.conjecture_count,
            if r.stats.conjectures_truncated {
                " (TRUNCATED)"
            } else {
                ""
            },
        ));
        if r.stats.vm_filtered {
            out.push_str(&format!(
                "    VM-filter: {} survived, {} refuted on sample data\n",
                r.conjectures.len(),
                r.stats.candidates_refuted
            ));
        }
        let shown = r.conjectures.len().min(SAMPLE);
        let label = if r.stats.vm_filtered {
            "survivors"
        } else {
            "candidates"
        };
        out.push_str(&format!("    {label} (showing {shown}):\n"));
        for c in r.conjectures.iter().take(SAMPLE) {
            out.push_str(&format!("      {}\n", c.render(&r.binders)));
        }
        if r.conjectures.len() > SAMPLE {
            out.push_str(&format!(
                "      … and {} more\n",
                r.conjectures.len() - SAMPLE
            ));
        }
        if !r.proved.is_empty() {
            out.push_str(&format!(
                "    PROVED (Lean, kernel-checked): {}\n",
                r.proved.len()
            ));
            for p in &r.proved {
                out.push_str(&format!("      ✓ {p}\n"));
            }
        }
    }
    out
}

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
        let prefixes: HashSet<String> = HashSet::new();
        let recursive: HashSet<crate::ir::FnId> = HashSet::new();
        let no_modules: &[ModuleInfo] = &[];
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
        // Tactic: list-induction template with the decode unfold + append_assoc.
        assert!(thm.contains("induction "), "{thm}");
        assert!(thm.contains("| nil => simp [decode]"), "{thm}");
        assert!(thm.contains("List.append_assoc"), "{thm}");
        assert!(thm.contains("ih"), "{thm}");
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

    #[test]
    fn structural_conjecturer_on_real_rle() {
        // Two structure-directed families fire on rle: the counted-repeat
        // advance (`repeat`) and the RLE-fold count-invariant (`encodeFold`).
        let groups = with_inputs(&rle_source(), structural_lemma_groups);
        assert_eq!(groups.len(), 2, "expected counted-repeat + rle-step groups");
        let all: String = groups.iter().flatten().map(|(_, t)| t.as_str()).collect();
        // brick 1: guarded counted-repeat advance (`repeat` escapes to `repeat'`).
        assert!(
            all.contains("repeat' c (n + 1) = repeat' c n ++ [c]"),
            "{all}"
        );
        assert!(
            all.contains("(hn : 0 <= n)") && all.contains("natAbs"),
            "{all}"
        );
        // brick 2: guarded step count-invariant, by_cases-on-guard template.
        assert!(all.contains("_count_nonneg"), "{all}");
        assert!(all.contains("0 <= (encodeFold acc c).count"), "{all}");
        assert!(all.contains("by_cases hzero"), "{all}");
    }
}
