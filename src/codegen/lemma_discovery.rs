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
//! # What's implemented here (Phase 2a → 2b)
//!
//! The type-directed term enumerator and candidate-equation generator:
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
//!
//! Still ahead (2c–2e): VM-filter (Aver VM as test oracle, conservative on
//! bounded-`Int` overflow), backend-prove (Lean = truth), and commit + replay
//! via the `ProofStrategy::SimpOverLemmas` hook. No VM / prover / file writes
//! happen here yet. Entry point [`run_discovery`] is invoked by
//! `aver proof --discover`; normal `aver proof` never runs this (discovery is
//! the explicit, expensive, cached step).

use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::ast::{TopLevel, VerifyKind};
use crate::codegen::proof_lower::{LawProofCone, ProofLowerInputs};
use crate::types::Type;

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
    pub conjecture_count: usize,
    pub terms_truncated: bool,
    pub conjectures_truncated: bool,
    /// The cone exceeded [`MAX_CONE_FNS`]; enumeration was skipped entirely.
    pub skipped_large_cone: bool,
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
    /// Candidate equations (size-ascending), pre-VM-filter, pre-proof.
    pub conjectures: Vec<Conjecture>,
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
            stats: DiscoveryStats {
                cone_fn_count,
                term_count,
                conjecture_count: conjectures.len(),
                terms_truncated,
                conjectures_truncated,
                skipped_large_cone,
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

/// Human-readable multi-line report for `aver proof --discover` output. Shows
/// each law's cone, variable legend, stats, and a sample of the candidate
/// equations (the full set is pre-VM-filter, so only a sample is printed).
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
        let shown = r.conjectures.len().min(SAMPLE);
        out.push_str(&format!("    candidates (showing {shown}):\n"));
        for c in r.conjectures.iter().take(SAMPLE) {
            out.push_str(&format!("      {}\n", c.render(&r.binders)));
        }
        if r.conjectures.len() > SAMPLE {
            out.push_str(&format!(
                "      … and {} more\n",
                r.conjectures.len() - SAMPLE
            ));
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

    fn discover(src: &str) -> Vec<LawDiscovery> {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let items = crate::parser::Parser::new(tokens).parse().expect("parse");
        // `LawProofCone::compute` walks the AST directly (it does not touch
        // the symbol table), so a parse-only fixture is sufficient and
        // hermetic — no typecheck / resolve needed.
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
        run_discovery(&inputs)
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
}
