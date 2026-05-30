//! Per-fn shape recognition — typed facts over resolved HIR.
//!
//! Stage 1 of issue #232 (0.23 "Shape"). This module is the
//! *recognition-only* layer: walks `ResolvedFnDef` bodies, builds
//! `Facts` per fn, emits multi-label archetype classifications,
//! computes call-graph SCCs. **No `ModuleShape` vector / Kind /
//! Layer / renderer here** — those live one tier up in
//! `aver::diagnostics::shape` (presentation) or
//! `aver::codegen::proof_lower` (meaning), and the future Stage 6
//! patterns / relations (`WrapperOverRecursion`, `ResultPipeline`,
//! …) land here next to the primitives they're built from.

use std::collections::{HashMap, HashSet};

use crate::ir::FnId;
use crate::ir::hir::{
    ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedFnDef, ResolvedMatchArm, ResolvedPattern,
    ResolvedStmt, ResolvedStrPart,
};
use crate::types::Type;

// ─── Per-fn AST facts ────────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub struct Facts {
    pub calls_to: HashSet<FnId>,
    pub tail_calls: HashSet<FnId>,
    pub builtin_calls: Vec<String>,
    pub ctor_constructs: Vec<ResolvedCtor>,
    pub ctor_match_patterns: usize,
    pub other_match_patterns: usize,
    pub has_match: bool,
    pub has_error_prop: bool,
    pub record_creates: usize,
    pub last_stmt_is_match: bool,
    pub only_stmt_is_literal: bool,
    pub body_stmt_count: usize,
    pub has_interp_str: bool,
    pub string_builtin_calls: usize,
    pub has_match_with_err_arm: bool,
}

fn walk_expr(e: &ResolvedExpr, facts: &mut Facts) {
    match e {
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {}
        ResolvedExpr::Attr(inner, _) => walk_expr(&inner.node, facts),
        ResolvedExpr::Call(callee, args) => {
            match callee {
                ResolvedCallee::Fn(id) => {
                    facts.calls_to.insert(*id);
                }
                ResolvedCallee::Builtin(name) => {
                    if name.starts_with("String.") {
                        facts.string_builtin_calls += 1;
                    }
                    facts.builtin_calls.push(name.clone());
                }
                ResolvedCallee::Intrinsic(_) => {}
                ResolvedCallee::LocalSlot { .. } => {}
                ResolvedCallee::Unresolved { callee } => walk_expr(&callee.node, facts),
            }
            for a in args {
                walk_expr(&a.node, facts);
            }
        }
        ResolvedExpr::BinOp(_, a, b) => {
            walk_expr(&a.node, facts);
            walk_expr(&b.node, facts);
        }
        ResolvedExpr::Neg(inner) => walk_expr(&inner.node, facts),
        ResolvedExpr::Match { subject, arms } => {
            facts.has_match = true;
            walk_expr(&subject.node, facts);
            for arm in arms {
                count_arm_pattern(&arm.pattern, facts);
                walk_match_arm(arm, facts);
            }
        }
        ResolvedExpr::Ctor(c, args) => {
            facts.ctor_constructs.push(c.clone());
            for a in args {
                walk_expr(&a.node, facts);
            }
        }
        ResolvedExpr::ErrorProp(inner) => {
            facts.has_error_prop = true;
            walk_expr(&inner.node, facts);
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            facts.has_interp_str = true;
            for p in parts {
                if let ResolvedStrPart::Parsed(e) = p {
                    walk_expr(&e.node, facts);
                }
            }
        }
        ResolvedExpr::List(xs) | ResolvedExpr::Tuple(xs) => {
            for x in xs {
                walk_expr(&x.node, facts);
            }
        }
        ResolvedExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                walk_expr(&k.node, facts);
                walk_expr(&v.node, facts);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            facts.record_creates += 1;
            for (_, v) in fields {
                walk_expr(&v.node, facts);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            walk_expr(&base.node, facts);
            for (_, v) in updates {
                walk_expr(&v.node, facts);
            }
        }
        ResolvedExpr::TailCall { target, args } => {
            facts.tail_calls.insert(*target);
            facts.calls_to.insert(*target);
            for a in args {
                walk_expr(&a.node, facts);
            }
        }
        ResolvedExpr::IndependentProduct(xs, _) => {
            for x in xs {
                walk_expr(&x.node, facts);
            }
        }
    }
}

fn count_arm_pattern(p: &ResolvedPattern, facts: &mut Facts) {
    match p {
        ResolvedPattern::Ctor(ctor, _) => {
            facts.ctor_match_patterns += 1;
            if matches!(
                ctor,
                ResolvedCtor::Builtin(crate::ir::hir::BuiltinCtor::ResultErr)
            ) {
                facts.has_match_with_err_arm = true;
            }
        }
        _ => facts.other_match_patterns += 1,
    }
}

fn walk_match_arm(arm: &ResolvedMatchArm, facts: &mut Facts) {
    walk_expr(&arm.body.node, facts);
}

pub fn extract_facts(fd: &ResolvedFnDef) -> Facts {
    let mut facts = Facts::default();
    let stmts = fd.body.stmts();
    facts.body_stmt_count = stmts.len();
    for stmt in stmts {
        match stmt {
            ResolvedStmt::Binding { value, .. } => walk_expr(&value.node, &mut facts),
            ResolvedStmt::Expr(value) => walk_expr(&value.node, &mut facts),
        }
    }
    if let Some(last) = stmts.last() {
        let expr = match last {
            ResolvedStmt::Binding { value, .. } => &value.node,
            ResolvedStmt::Expr(value) => &value.node,
        };
        facts.last_stmt_is_match = matches!(expr, ResolvedExpr::Match { .. });
    }
    facts.only_stmt_is_literal = stmts.len() == 1 && {
        let expr = match &stmts[0] {
            ResolvedStmt::Binding { value, .. } => &value.node,
            ResolvedStmt::Expr(value) => &value.node,
        };
        matches!(
            expr,
            ResolvedExpr::Literal(_)
                | ResolvedExpr::List(_)
                | ResolvedExpr::Tuple(_)
                | ResolvedExpr::MapLiteral(_)
                | ResolvedExpr::RecordCreate { .. }
                | ResolvedExpr::Ctor(_, _)
        )
    };
    facts
}

// ─── Per-fn classification ───────────────────────────────────────────────────

pub fn classify(fd: &ResolvedFnDef, facts: &Facts, scc: &HashSet<FnId>) -> Vec<Archetype> {
    let mut labels = Vec::new();

    let self_call = facts.calls_to.contains(&fd.fn_id) || facts.tail_calls.contains(&fd.fn_id);
    if self_call {
        labels.push(Archetype::StructuralRecursion);
    }
    if scc.contains(&fd.fn_id) {
        labels.push(Archetype::SccMutual);
    }

    if facts.last_stmt_is_match {
        if facts.ctor_match_patterns >= facts.other_match_patterns && facts.ctor_match_patterns > 0
        {
            labels.push(Archetype::MatchDispatcher);
        } else {
            labels.push(Archetype::MatchOnValue);
        }
    }

    if !fd.effects.is_empty() {
        let total_calls = facts.calls_to.len() + facts.builtin_calls.len();
        if total_calls >= 2 {
            labels.push(Archetype::Orchestration);
        } else {
            labels.push(Archetype::EffectfulLeaf);
        }
    }

    let is_result_ret = type_is_result(&fd.return_type);
    if is_result_ret && facts.has_error_prop {
        labels.push(Archetype::PipelineResult);
    } else if is_result_ret && facts.has_match_with_err_arm {
        labels.push(Archetype::ManualResultAdapter);
    }

    let is_string_ret = matches!(&fd.return_type, Type::Named { name, .. } if name == "String");
    if is_string_ret
        && fd.effects.is_empty()
        && (facts.has_interp_str || facts.string_builtin_calls >= 1)
        && (facts.body_stmt_count >= 2 || facts.has_interp_str)
    {
        labels.push(Archetype::RendererFormatter);
    }

    if facts.body_stmt_count == 1
        && (!facts.ctor_constructs.is_empty() || facts.record_creates > 0)
        && !facts.has_match
    {
        labels.push(Archetype::ConstructorWrapper);
    }

    if fd.params.is_empty()
        && facts.calls_to.is_empty()
        && facts.builtin_calls.is_empty()
        && fd.effects.is_empty()
        && facts.only_stmt_is_literal
    {
        labels.push(Archetype::DataAsFunction);
    }

    if facts.body_stmt_count == 1
        && !facts.has_match
        && !self_call
        && fd.effects.is_empty()
        && (!facts.builtin_calls.is_empty() || !facts.calls_to.is_empty())
    {
        labels.push(Archetype::TrivialHelper);
    }

    if facts.body_stmt_count == 1
        && !facts.has_match
        && !self_call
        && fd.effects.is_empty()
        && facts.builtin_calls.is_empty()
        && facts.calls_to.is_empty()
        && facts.ctor_constructs.is_empty()
        && !fd.params.is_empty()
    {
        labels.push(Archetype::PureExpression);
    }

    if facts.body_stmt_count >= 2
        && !facts.last_stmt_is_match
        && fd.effects.is_empty()
        && (!facts.builtin_calls.is_empty() || !facts.calls_to.is_empty())
        && !self_call
    {
        labels.push(Archetype::LetPipeline);
    }

    labels
}

pub fn type_is_result(t: &Type) -> bool {
    matches!(t, Type::Named { name, .. } if name == "Result")
}

/// Per-fn archetype label. Multi-label per fn — `classify` returns
/// a `Vec<Archetype>`; `primary_label` picks one by the precedence
/// declared in [`Archetype::all`]. Stringified only at presentation
/// boundaries (renderer, JSON, LSP hover).
///
/// Stage 3 of #232 ("Shape") replaced the prior `&'static str`-keyed
/// representation with this typed enum. The string forms are still
/// the public JSON / text contract; `as_str` / `parse` translate
/// at the edges.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Archetype {
    SccMutual,
    StructuralRecursion,
    MatchDispatcher,
    PipelineResult,
    ManualResultAdapter,
    RendererFormatter,
    MatchOnValue,
    Orchestration,
    EffectfulLeaf,
    LetPipeline,
    ConstructorWrapper,
    DataAsFunction,
    TrivialHelper,
    PureExpression,
    /// Fallback for fn bodies that don't match any classifier rule.
    /// Surfaces as `"unclassified"` in JSON / text output.
    Unclassified,
}

impl Archetype {
    /// Precedence-ordered list. `primary_label` walks this in order;
    /// the first archetype that fires on a fn wins. Renderers also
    /// use this for histogram tie-breaking.
    pub fn all() -> &'static [Archetype] {
        &[
            Archetype::SccMutual,
            Archetype::StructuralRecursion,
            Archetype::MatchDispatcher,
            Archetype::PipelineResult,
            Archetype::ManualResultAdapter,
            Archetype::RendererFormatter,
            Archetype::MatchOnValue,
            Archetype::Orchestration,
            Archetype::EffectfulLeaf,
            Archetype::LetPipeline,
            Archetype::ConstructorWrapper,
            Archetype::DataAsFunction,
            Archetype::TrivialHelper,
            Archetype::PureExpression,
        ]
    }

    /// Canonical kebab-case label. Used by renderers, JSON, hover.
    pub fn as_str(&self) -> &'static str {
        match self {
            Archetype::SccMutual => "scc-mutual",
            Archetype::StructuralRecursion => "structural-recursion",
            Archetype::MatchDispatcher => "match-dispatcher",
            Archetype::PipelineResult => "pipeline-result",
            Archetype::ManualResultAdapter => "manual-result-adapter",
            Archetype::RendererFormatter => "renderer-formatter",
            Archetype::MatchOnValue => "match-on-value",
            Archetype::Orchestration => "orchestration",
            Archetype::EffectfulLeaf => "effectful-leaf",
            Archetype::LetPipeline => "let-pipeline",
            Archetype::ConstructorWrapper => "constructor-wrapper",
            Archetype::DataAsFunction => "data-as-function",
            Archetype::TrivialHelper => "trivial-helper",
            Archetype::PureExpression => "pure-expression",
            Archetype::Unclassified => "unclassified",
        }
    }

    /// Parse a string back into the typed form. Used by callers that
    /// receive the public JSON shape and want to operate on the typed
    /// enum (e.g. the research test reading per-folder histograms).
    pub fn parse(s: &str) -> Option<Archetype> {
        Some(match s {
            "scc-mutual" => Archetype::SccMutual,
            "structural-recursion" => Archetype::StructuralRecursion,
            "match-dispatcher" => Archetype::MatchDispatcher,
            "pipeline-result" => Archetype::PipelineResult,
            "manual-result-adapter" => Archetype::ManualResultAdapter,
            "renderer-formatter" => Archetype::RendererFormatter,
            "match-on-value" => Archetype::MatchOnValue,
            "orchestration" => Archetype::Orchestration,
            "effectful-leaf" => Archetype::EffectfulLeaf,
            "let-pipeline" => Archetype::LetPipeline,
            "constructor-wrapper" => Archetype::ConstructorWrapper,
            "data-as-function" => Archetype::DataAsFunction,
            "trivial-helper" => Archetype::TrivialHelper,
            "pure-expression" => Archetype::PureExpression,
            "unclassified" => Archetype::Unclassified,
            _ => return None,
        })
    }
}

pub fn primary_label(labels: &[Archetype]) -> Archetype {
    for &want in Archetype::all() {
        if labels.contains(&want) {
            return want;
        }
    }
    Archetype::Unclassified
}

// ─── Call-graph SCC (multi-node strongly connected components) ───────────────

pub fn compute_sccs(fns: &[&ResolvedFnDef], facts_by_id: &HashMap<FnId, &Facts>) -> HashSet<FnId> {
    let mut graph: HashMap<FnId, Vec<FnId>> = HashMap::new();
    let fn_ids: HashSet<FnId> = fns.iter().map(|f| f.fn_id).collect();
    for fd in fns {
        if let Some(facts) = facts_by_id.get(&fd.fn_id) {
            let edges: Vec<FnId> = facts
                .calls_to
                .iter()
                .copied()
                .filter(|c| fn_ids.contains(c) && *c != fd.fn_id)
                .collect();
            graph.insert(fd.fn_id, edges);
        }
    }
    let mut index = 0u32;
    let mut stack: Vec<FnId> = Vec::new();
    let mut on_stack: HashSet<FnId> = HashSet::new();
    let mut indices: HashMap<FnId, u32> = HashMap::new();
    let mut lowlinks: HashMap<FnId, u32> = HashMap::new();
    let mut multi_scc: HashSet<FnId> = HashSet::new();

    #[allow(clippy::too_many_arguments)]
    fn strongconnect(
        v: FnId,
        graph: &HashMap<FnId, Vec<FnId>>,
        index: &mut u32,
        stack: &mut Vec<FnId>,
        on_stack: &mut HashSet<FnId>,
        indices: &mut HashMap<FnId, u32>,
        lowlinks: &mut HashMap<FnId, u32>,
        multi_scc: &mut HashSet<FnId>,
    ) {
        indices.insert(v, *index);
        lowlinks.insert(v, *index);
        *index += 1;
        stack.push(v);
        on_stack.insert(v);
        if let Some(neighbors) = graph.get(&v).cloned() {
            for w in neighbors {
                if !indices.contains_key(&w) {
                    strongconnect(
                        w, graph, index, stack, on_stack, indices, lowlinks, multi_scc,
                    );
                    let lv = *lowlinks.get(&v).unwrap();
                    let lw = *lowlinks.get(&w).unwrap();
                    lowlinks.insert(v, lv.min(lw));
                } else if on_stack.contains(&w) {
                    let lv = *lowlinks.get(&v).unwrap();
                    let iw = *indices.get(&w).unwrap();
                    lowlinks.insert(v, lv.min(iw));
                }
            }
        }
        if lowlinks.get(&v) == indices.get(&v) {
            let mut scc: Vec<FnId> = Vec::new();
            loop {
                let w = stack.pop().unwrap();
                on_stack.remove(&w);
                scc.push(w);
                if w == v {
                    break;
                }
            }
            if scc.len() > 1 {
                multi_scc.extend(scc);
            }
        }
    }

    let nodes: Vec<FnId> = graph.keys().copied().collect();
    for v in nodes {
        if !indices.contains_key(&v) {
            strongconnect(
                v,
                &graph,
                &mut index,
                &mut stack,
                &mut on_stack,
                &mut indices,
                &mut lowlinks,
                &mut multi_scc,
            );
        }
    }
    multi_scc
}

// ─── Program-level shape (stage 4 of #232) ───────────────────────────────────

/// Per-fn recognition output: precedence-picked primary archetype +
/// the full multi-label set the classifier fired on.
///
/// Facts (the AST walker output) intentionally don't escape here —
/// they're cheap to recompute if a future consumer wants them, and
/// keeping the `ProgramShape` value small means downstream passes
/// (proof_lower, future inliner / monomorphizer) can clone it freely.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnRecognition {
    pub primary: Archetype,
    pub labels: Vec<Archetype>,
}

/// Whole-program shape facts produced by [`analyze_program`].
///
/// Stage 4 of issue #232 (0.23 "Shape"). This is the *recognition*
/// substrate every downstream consumer reads from — `aver shape` CLI,
/// proof_lower's strategy router, future inliner / monomorphizer.
/// Stage 6 will grow `patterns: Vec<ModulePattern>` and
/// `relations: Vec<FnRelation>` next to `per_fn` for the higher-arity
/// recognitions (`WrapperOverRecursion`, `RefinementSmartConstructor`,
/// `ResultPipelineChain`, …).
///
/// Computed once per compilation per the peer-review note ("compute
/// ProgramShape once per compilation / per HIR snapshot — don't
/// persistent-cache per FnId yet"). Threaded as a read-only borrow
/// from the call site; the analysis tier never mutates it.
#[derive(Debug, Clone, Default)]
pub struct ProgramShape {
    /// Per-fn recognition keyed by stable `FnId`.
    pub per_fn: std::collections::HashMap<FnId, FnRecognition>,
    /// Multi-node SCC participants in the local call graph. Kept here
    /// so consumers don't have to recompute Tarjan to ask
    /// "is this fn part of a mutual-recursion group?".
    pub sccs: HashSet<FnId>,
    /// Whole-module typed patterns (stage 6 of #232). Populated by
    /// [`analyze_program_with_modules`]; [`analyze_program`] leaves
    /// this empty since it only sees the resolved-fn snapshot, not
    /// the source items needed to detect module-level shapes like
    /// `RefinementSmartConstructor`.
    pub patterns: Vec<ModulePattern>,
    /// Source-level sum type names that are eligible as induction
    /// targets: directly self-referential in at least one variant
    /// and not indirectly recursive through nested generics the
    /// per-variant emit can't case-split (e.g. `Some(List<Self>)`).
    /// Mirrors `proof_lower::detect_induction_target`'s inline
    /// scan so the detector can read this set instead of
    /// re-walking type defs.
    pub inductable_sum_types: HashSet<String>,
}

impl ProgramShape {
    /// Recognition for one fn by id. Returns `None` for fns that
    /// weren't included in the call to [`analyze_program`] (e.g. an
    /// out-of-tree id or a stale lookup against a refreshed view).
    pub fn for_fn(&self, fn_id: FnId) -> Option<&FnRecognition> {
        self.per_fn.get(&fn_id)
    }
}

/// Build a [`ProgramShape`] over the post-resolver fn snapshot in one
/// pass. Caller picks which fns participate (typically every
/// `ResolvedTopLevel::FnDef` of the entry module, sometimes plus
/// dep-module fns when a cross-module analysis needs the broader
/// view).
///
/// Two-pass internally: facts first (needed to build the call graph
/// for SCC detection), then classify with the facts + SCC ready.
/// `O(N)` over fn bodies, `O(N+E)` Tarjan over the call graph; the
/// per-compilation cache budget the peer-review pinned.
pub fn analyze_program(resolved_fns: &[&ResolvedFnDef]) -> ProgramShape {
    let mut facts_by_id: std::collections::HashMap<FnId, Facts> =
        std::collections::HashMap::with_capacity(resolved_fns.len());
    for fd in resolved_fns {
        facts_by_id.insert(fd.fn_id, extract_facts(fd));
    }
    let facts_refs: std::collections::HashMap<FnId, &Facts> =
        facts_by_id.iter().map(|(k, v)| (*k, v)).collect();
    let sccs = compute_sccs(resolved_fns, &facts_refs);

    let mut per_fn = std::collections::HashMap::with_capacity(resolved_fns.len());
    for fd in resolved_fns {
        let facts = &facts_by_id[&fd.fn_id];
        let labels = classify(fd, facts, &sccs);
        let primary = primary_label(&labels);
        per_fn.insert(fd.fn_id, FnRecognition { primary, labels });
    }

    ProgramShape {
        per_fn,
        sccs,
        patterns: Vec::new(),
        inductable_sum_types: HashSet::new(),
    }
}

/// Same as [`analyze_program`] but also detects module-level patterns
/// (`ModulePattern::RefinementSmartConstructor`, …) by walking the
/// source `items` and dep modules. Callers that have both the
/// resolved-fn snapshot and the source items should prefer this.
pub fn analyze_program_with_modules(
    resolved_fns: &[&ResolvedFnDef],
    entry_items: &[crate::ast::TopLevel],
    dep_modules: &[crate::codegen::ModuleInfo],
) -> ProgramShape {
    let mut shape = analyze_program(resolved_fns);
    shape.patterns = detect_module_patterns(entry_items, dep_modules);
    shape.inductable_sum_types = collect_inductable_sum_types(entry_items, dep_modules);
    shape
}

// ─── Module-level typed patterns (stage 6 of #232) ───────────────────────────

/// A `ModulePattern` is a *recognized structural fact* about a whole
/// module's surface — the level above per-fn archetypes — carrying the
/// **typed payload** downstream consumers need to act on it.
///
/// The first variant is `RefinementSmartConstructor`, the canonical
/// `refinement-via-opaque` shape (single-field record + validating
/// smart constructor) the proof export already recognizes via
/// [`crate::codegen::common::refinement_info_for`]. Stage 6 lifts the
/// recognition into the analysis tier so other consumers (`aver shape`
/// LSP, future inliner, monomorphizer) don't each re-walk the AST to
/// ask the same question.
///
/// Peer-review note from issue #232: "kind == SmartConstructor is too
/// compressed to be source of truth for proof routing — proof needs
/// typed payload". This enum carries that payload (carrier field +
/// type, constructor fn name, predicate expression).
///
/// Stage 6a (this commit) only **detects** the pattern; the proof
/// export still walks via the legacy `refinement_info_for` API.
/// Stage 6b refactors that fn into a thin adapter over
/// `ProgramShape::patterns`. Stage 6c+ adds the next pattern
/// (`WrapperOverRecursion`, `ResultPipelineChain`, …).
#[derive(Debug, Clone)]
pub enum ModulePattern {
    /// `refinement-via-opaque` shape: a single-field
    /// `record T { <carrier_field>: <carrier_type> }` paired with a
    /// validating smart constructor
    ///   `fn <constructor_fn>(<param_name>: <carrier_type>) -> Result<T, _>`
    ///   `    match <predicate>`
    ///   `        true  -> Result.Ok(T(<carrier_field> = <param_name>))`
    ///   `        false -> Result.Err("...")`
    RefinementSmartConstructor {
        /// Where this pattern lives: `None` = entry items,
        /// `Some(prefix)` = dep module with that prefix. Lets the
        /// scope-aware adapter (`refinement_info_for_in_scope`) pick
        /// the predicate from the right module when two modules
        /// declare a refined record with the same bare name
        /// (e.g. `A.Natural` vs `B.Natural`).
        scope: Option<String>,
        /// Source-level type name (`"Natural"`, `"Positive"`, …).
        /// FnId / TypeId migration deferred — name keys match what
        /// the current `refinement_info_for` adapter uses.
        type_name: String,
        /// Carrier-field name (e.g. `"value"`). Lean projects through
        /// `.val` on a `Subtype`; this is the field that gets renamed
        /// in the lifted view.
        carrier_field: String,
        /// Carrier type annotation as written in the record field
        /// (`"Int"`, `"Float"`, …). Backends emit it as the subset's
        /// underlying type.
        carrier_type: String,
        /// Source-level name of the smart constructor (`"fromInt"`).
        constructor_fn: String,
        /// Parameter name on the smart constructor signature
        /// (`"n"` in `fromInt(n: Int) -> Result<Natural, _>`). Used
        /// when substituting the law's quantified variable into the
        /// predicate.
        param_name: String,
        /// Cloned bool predicate the smart constructor branches on —
        /// the body's `match <predicate>` subject. Owned so
        /// `ProgramShape` doesn't borrow source items.
        predicate: crate::ast::Spanned<crate::ast::Expr>,
    },
    /// `wrapper-over-recursion` shape: a non-recursive `wrapper_fn` whose
    /// body's only recursive call is to a self-recursive `inner_fn`
    /// living in the same scope, with `inner_fn` taking the wrapper's
    /// parameters as a prefix (literally, as `Ident` args) plus at
    /// least one additional argument (typically an accumulator initial
    /// value). `fib(n) -> fibTR(n, 0, 1)` is the canonical example;
    /// `aver fmt` / proof export use this to route the wrapper through
    /// the inner's induction certificate.
    ///
    /// Conservative detection rules (stage 6c):
    /// - wrapper is itself non-recursive (no self-call)
    /// - exactly one inner call to a self-recursive same-scope fn
    /// - every wrapper parameter appears literally (`Ident`) somewhere
    ///   in the inner's argument list
    /// - inner's arity is strictly greater than the wrapper's arity
    ///
    /// These rules keep false positives near zero on the shipped
    /// corpus; mutual recursion across fns isn't claimed yet
    /// (`inner_fn` must self-recurse, not participate in a larger SCC).
    WrapperOverRecursion {
        /// Scope of the wrapper (`None` = entry, `Some(prefix)` = dep
        /// module). `inner_scope` is always equal to `wrapper_scope`
        /// in stage 6c — cross-module wrappers aren't claimed.
        wrapper_scope: Option<String>,
        /// Source-level wrapper fn name (the outer, non-recursive one).
        wrapper_fn: String,
        /// Scope of the recursive inner fn. Mirrors `wrapper_scope`
        /// while stage 6c keeps this same-scope-only.
        inner_scope: Option<String>,
        /// Source-level inner fn name (the recursive one).
        inner_fn: String,
    },
    /// `?`-propagating Result pipeline: a fn whose body is a sequence
    /// of `let x = step()?` bindings followed by a tail expression
    /// (typically `Result.Ok(final)`). Canonical example:
    /// `examples/core/result_pipeline.av::validateAndCombine` — six
    /// `?` steps that short-circuit on the first Err.
    ///
    /// Detection rules (stage 6d):
    /// - fn return type starts with `Result<`
    /// - body has at least two `Stmt::Binding` whose value is
    ///   `Expr::ErrorProp(...)` (the `?` operator)
    /// - the tail stmt is an expression, not a binding
    ///
    /// `step_count` is the number of `?` bindings; downstream
    /// consumers can use it to size the staged result type or to
    /// pick between inlined and trampoline lowerings. No proof-export
    /// consumer yet — this is substrate-only.
    ResultPipelineChain {
        scope: Option<String>,
        fn_name: String,
        step_count: usize,
        /// Source names of the step fns called via `?` in body
        /// order. Captured here because the post-pipeline AST
        /// desugars `?` into nested `match` arms — downstream
        /// consumers that need the original step list (e.g. the
        /// proof_lower `ResultPipelineChain` strategy) read from
        /// this field instead of re-walking.
        step_fns: Vec<String>,
    },
    /// Non-recursive pure renderer: a fn whose return type is `String`,
    /// effects list is empty, and body contains an `InterpolatedStr`
    /// or a `String`-typed `+` concatenation. Canonical examples are
    /// `examples/data/rle.av::showRun` (single interpolation) and the
    /// `show*` family in `examples/data/fibonacci.av`.
    ///
    /// Detection rules (stage 6e):
    /// - return type is exactly `String`
    /// - effects list is empty
    /// - fn does not call itself anywhere in its body
    /// - body contains at least one `Expr::InterpolatedStr` or
    ///   `Expr::BinOp(Add, ..)` reachable through nesting
    ///
    /// Recursive structural renderers (`showRuns`, `showListIntInner`)
    /// are intentionally excluded — they belong to a future
    /// `StructuralRenderer` pattern paired with structural induction.
    RendererFormatter {
        scope: Option<String>,
        fn_name: String,
    },
    /// Self-recursive structural fold over a `List<T>` parameter:
    /// fn body is a single `match <param>` with at minimum
    /// `[] -> ...` and `[head, ..tail] -> ...` arms, and the fn
    /// calls itself somewhere in its body (typically passing
    /// `tail` to recur). `nthOrZero(xs, index)` from
    /// `examples/data/fibonacci.av` is the canonical example.
    ///
    /// Detection rules (stage 6f):
    /// - body is a single `Stmt::Expr(Match)`
    /// - subject is `Ident(p)` where `p` is one of the fn's params
    /// - arms include both `Pattern::EmptyList` and `Pattern::Cons`
    /// - fn is self-recursive
    ///
    /// Aver's stdlib has no `List.map/fold`, so this hand-rolled
    /// structural fold shows up across the corpus. Recognizing it
    /// unlocks two future moves: list-induction proof obligation
    /// emission, and a deforestation rewrite that fuses the fold
    /// with its consumer.
    MatchDispatcherFold {
        scope: Option<String>,
        fn_name: String,
        list_param: String,
    },
}

/// Walk entry items + dep modules and collect the names of sum types
/// that pass the proof-export induction eligibility check: directly
/// self-referential in at least one variant, and not indirectly
/// recursive through nested generics that the per-variant emit
/// would have to give up on. Mirrors the inline scan that
/// `proof_lower::detect_induction_target` used to perform so the
/// detector can read from `ProgramShape.inductable_sum_types` and
/// avoid re-walking the AST.
pub fn collect_inductable_sum_types(
    entry_items: &[crate::ast::TopLevel],
    dep_modules: &[crate::codegen::ModuleInfo],
) -> HashSet<String> {
    use crate::ast::{TopLevel, TypeDef};
    let mut out = HashSet::new();
    let mut consider = |td: &TypeDef| {
        if let TypeDef::Sum { name, variants, .. } = td
            && crate::codegen::common::is_recursive_sum(name, variants)
            && !indirect_rec_variants(variants, name)
        {
            out.insert(name.clone());
        }
    };
    for item in entry_items {
        if let TopLevel::TypeDef(td) = item {
            consider(td);
        }
    }
    for m in dep_modules {
        for td in &m.type_defs {
            consider(td);
        }
    }
    out
}

/// Mirror of `proof_lower::has_indirect_rec_variants`: a variant
/// field that contains `type_name` nested past one `<` is rejected
/// because the per-variant induction case-split can't decompose it.
fn indirect_rec_variants(variants: &[crate::ast::TypeVariant], type_name: &str) -> bool {
    for variant in variants {
        for field in &variant.fields {
            let f = field.trim();
            if f == type_name {
                continue;
            }
            let opens = f.matches('<').count();
            if opens > 1 && f.contains(type_name) {
                return true;
            }
        }
    }
    false
}

/// Walk entry items + dep modules and emit every typed
/// `ModulePattern` we can recognize. Used by `analyze_program_with_modules`
/// to populate `ProgramShape.patterns`.
///
/// Mirrors the recognition rules in
/// `codegen::common::refinement_info_for_walk` so downstream consumers
/// see the same set of refinement records; Stage 6b will retire the
/// legacy fn and route through this output.
pub fn detect_module_patterns(
    entry_items: &[crate::ast::TopLevel],
    dep_modules: &[crate::codegen::ModuleInfo],
) -> Vec<ModulePattern> {
    use crate::ast::{Expr, Stmt, TopLevel, TypeDef};

    let mut out = Vec::new();

    // Per-scope candidate records. `scope = None` = entry items,
    // `scope = Some(prefix)` = dep module. The smart-constructor
    // walk searches inside the same scope as the record — that's the
    // `refinement-via-opaque` invariant (constructor lives next to
    // the carrier field, since `exposes opaque` hides the field from
    // other modules).
    struct CandidateRecord<'a> {
        scope: Option<String>,
        type_name: &'a str,
        carrier_field: &'a str,
        carrier_type: &'a str,
        fns: Vec<&'a crate::ast::FnDef>,
    }

    let entry_fns: Vec<&crate::ast::FnDef> = entry_items
        .iter()
        .filter_map(|i| match i {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();

    let mut candidates: Vec<CandidateRecord<'_>> = Vec::new();
    for td in entry_items.iter().filter_map(|i| match i {
        TopLevel::TypeDef(td) => Some(td),
        _ => None,
    }) {
        if let TypeDef::Product { name, fields, .. } = td
            && fields.len() == 1
        {
            let (fname, ftype) = &fields[0];
            candidates.push(CandidateRecord {
                scope: None,
                type_name: name.as_str(),
                carrier_field: fname.as_str(),
                carrier_type: ftype.as_str(),
                fns: entry_fns.clone(),
            });
        }
    }
    for m in dep_modules {
        let module_fns: Vec<&crate::ast::FnDef> = m.fn_defs.iter().collect();
        for td in &m.type_defs {
            if let TypeDef::Product { name, fields, .. } = td
                && fields.len() == 1
            {
                let (fname, ftype) = &fields[0];
                candidates.push(CandidateRecord {
                    scope: Some(m.prefix.clone()),
                    type_name: name.as_str(),
                    carrier_field: fname.as_str(),
                    carrier_type: ftype.as_str(),
                    fns: module_fns.clone(),
                });
            }
        }
    }

    // Walk fns looking for the smart-constructor shape per candidate
    // record. Mirrors `refinement_info_for_walk`: return type
    // `Result<TypeName, _>`, exactly one param, body is a single
    // `match <pred>` with two arms, bool-shaped `true -> Ok` /
    // `false -> Err` referencing the carrier field + param. The fn
    // must live in the same scope as the carrier record.
    for candidate in &candidates {
        let CandidateRecord {
            scope,
            type_name,
            carrier_field,
            carrier_type,
            fns,
        } = candidate;
        for fd in fns {
            if !fd.return_type.starts_with("Result<") {
                continue;
            }
            if !fd.return_type[7..].starts_with(*type_name) {
                continue;
            }
            if fd.params.len() != 1 {
                continue;
            }
            let (param_name, _) = &fd.params[0];
            let stmts = fd.body.stmts();
            if stmts.len() != 1 {
                continue;
            }
            let Stmt::Expr(body_expr) = &stmts[0] else {
                continue;
            };
            let Expr::Match { subject, arms } = &body_expr.node else {
                continue;
            };
            if !crate::codegen::common::is_refinement_bool_ok_err_match(
                arms,
                type_name,
                carrier_field,
                param_name,
            ) {
                continue;
            }
            out.push(ModulePattern::RefinementSmartConstructor {
                scope: scope.clone(),
                type_name: (*type_name).to_string(),
                carrier_field: (*carrier_field).to_string(),
                carrier_type: (*carrier_type).to_string(),
                constructor_fn: fd.name.clone(),
                param_name: param_name.clone(),
                predicate: (**subject).clone(),
            });
            break;
        }
    }

    // Stage 6c of #232: `WrapperOverRecursion` per-scope detection.
    // Each scope is searched independently (entry items, then each
    // dep module) — cross-module wrappers aren't claimed yet.
    detect_wrapper_over_recursion(None, &entry_fns, &mut out);
    for m in dep_modules {
        let fns: Vec<&crate::ast::FnDef> = m.fn_defs.iter().collect();
        detect_wrapper_over_recursion(Some(m.prefix.clone()), &fns, &mut out);
    }

    // Stage 6d of #232: `ResultPipelineChain` per-scope detection.
    detect_result_pipeline_chain(None, &entry_fns, &mut out);
    for m in dep_modules {
        let fns: Vec<&crate::ast::FnDef> = m.fn_defs.iter().collect();
        detect_result_pipeline_chain(Some(m.prefix.clone()), &fns, &mut out);
    }

    // Stage 6e of #232: `RendererFormatter` per-scope detection.
    detect_renderer_formatter(None, &entry_fns, &mut out);
    for m in dep_modules {
        let fns: Vec<&crate::ast::FnDef> = m.fn_defs.iter().collect();
        detect_renderer_formatter(Some(m.prefix.clone()), &fns, &mut out);
    }

    // Stage 6f of #232: `MatchDispatcherFold` per-scope detection.
    detect_match_dispatcher_fold(None, &entry_fns, &mut out);
    for m in dep_modules {
        let fns: Vec<&crate::ast::FnDef> = m.fn_defs.iter().collect();
        detect_match_dispatcher_fold(Some(m.prefix.clone()), &fns, &mut out);
    }

    out
}

/// Per-scope detector for [`ModulePattern::MatchDispatcherFold`]. See
/// the variant docs for the detection contract.
fn detect_match_dispatcher_fold(
    scope: Option<String>,
    fns: &[&crate::ast::FnDef],
    out: &mut Vec<ModulePattern>,
) {
    use crate::ast::{Expr, Pattern, Stmt};
    for fd in fns {
        let stmts = fd.body.stmts();
        if stmts.len() != 1 {
            continue;
        }
        let Stmt::Expr(body_expr) = &stmts[0] else {
            continue;
        };
        let Expr::Match { subject, arms } = &body_expr.node else {
            continue;
        };
        // Pre-pipeline: subject is `Expr::Ident(name)`. Post-pipeline:
        // the resolver rewrites local/param idents to
        // `Expr::Resolved { name, .. }`. Both forms identify the
        // matched parameter.
        let subj_name = match &subject.node {
            Expr::Ident(n) => n.as_str(),
            Expr::Resolved { name, .. } => name.as_str(),
            _ => continue,
        };
        if !fd.params.iter().any(|(n, _)| n == subj_name) {
            continue;
        }
        let has_nil = arms.iter().any(|a| matches!(a.pattern, Pattern::EmptyList));
        let has_cons = arms
            .iter()
            .any(|a| matches!(a.pattern, Pattern::Cons(_, _)));
        if !(has_nil && has_cons) {
            continue;
        }
        if !body_calls_name(&fd.body, &fd.name) {
            continue;
        }
        out.push(ModulePattern::MatchDispatcherFold {
            scope: scope.clone(),
            fn_name: fd.name.clone(),
            list_param: subj_name.to_string(),
        });
    }
}

/// Per-scope detector for [`ModulePattern::RendererFormatter`]. See
/// the variant docs for the detection contract.
fn detect_renderer_formatter(
    scope: Option<String>,
    fns: &[&crate::ast::FnDef],
    out: &mut Vec<ModulePattern>,
) {
    for fd in fns {
        if fd.return_type != "String" {
            continue;
        }
        if !fd.effects.is_empty() {
            continue;
        }
        if body_calls_name(&fd.body, &fd.name) {
            continue;
        }
        if !body_has_string_building(&fd.body) {
            continue;
        }
        out.push(ModulePattern::RendererFormatter {
            scope: scope.clone(),
            fn_name: fd.name.clone(),
        });
    }
}

/// Walk `body` looking for any `Expr::InterpolatedStr` or string-typed
/// `+` (`BinOp(Add, ..)`). Conservative: any addition counts since
/// the typechecker has already restricted what `+` can do at a
/// `String`-returning callsite — false positives here would be
/// numeric adds that never reach the return slot, which the
/// `return_type == "String"` guard already excludes for trivial
/// arithmetic-only bodies.
fn body_has_string_building(body: &crate::ast::FnBody) -> bool {
    for stmt in body.stmts() {
        let expr = match stmt {
            crate::ast::Stmt::Binding(_, _, e) => e,
            crate::ast::Stmt::Expr(e) => e,
        };
        if expr_has_string_building(expr) {
            return true;
        }
    }
    false
}

fn expr_has_string_building(expr: &crate::ast::Spanned<crate::ast::Expr>) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::InterpolatedStr(_) => true,
        Expr::BinOp(crate::ast::BinOp::Add, _, _) => true,
        Expr::FnCall(callee, args) => {
            expr_has_string_building(callee) || args.iter().any(expr_has_string_building)
        }
        Expr::TailCall(td) => td.args.iter().any(expr_has_string_building),
        Expr::Match { subject, arms } => {
            expr_has_string_building(subject)
                || arms.iter().any(|a| expr_has_string_building(&a.body))
        }
        Expr::BinOp(_, l, r) => expr_has_string_building(l) || expr_has_string_building(r),
        Expr::Neg(e) | Expr::Attr(e, _) | Expr::ErrorProp(e) => expr_has_string_building(e),
        Expr::Constructor(_, Some(e)) => expr_has_string_building(e),
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            xs.iter().any(expr_has_string_building)
        }
        Expr::MapLiteral(pairs) => pairs
            .iter()
            .any(|(k, v)| expr_has_string_building(k) || expr_has_string_building(v)),
        Expr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, e)| expr_has_string_building(e))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            expr_has_string_building(base)
                || updates.iter().any(|(_, e)| expr_has_string_building(e))
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Constructor(_, None) | Expr::Resolved { .. } => {
            false
        }
    }
}

/// Per-scope detector for [`ModulePattern::ResultPipelineChain`].
/// Counts `Stmt::Binding(_, _, ErrorProp(...))` (the `?` operator)
/// in each fn body; emits the pattern when there are ≥2 such
/// bindings, the fn returns `Result<…>`, and the tail stmt is an
/// expression (`Stmt::Expr`, not another binding).
fn detect_result_pipeline_chain(
    scope: Option<String>,
    fns: &[&crate::ast::FnDef],
    out: &mut Vec<ModulePattern>,
) {
    use crate::ast::{Expr, Stmt};
    for fd in fns {
        if !fd.return_type.starts_with("Result<") {
            continue;
        }
        let stmts = fd.body.stmts();
        if stmts.len() < 2 {
            continue;
        }
        if !matches!(stmts.last(), Some(Stmt::Expr(_))) {
            continue;
        }
        let mut step_fns: Vec<String> = Vec::new();
        for stmt in stmts {
            if let Stmt::Binding(_, _, value) = stmt
                && let Expr::ErrorProp(inner) = &value.node
                && let Expr::FnCall(callee, _) = &inner.node
                && let Expr::Ident(name) = &callee.node
            {
                step_fns.push(name.clone());
            }
        }
        if step_fns.len() < 2 {
            continue;
        }
        let step_count = step_fns.len();
        out.push(ModulePattern::ResultPipelineChain {
            scope: scope.clone(),
            fn_name: fd.name.clone(),
            step_count,
            step_fns,
        });
    }
}

/// Per-scope detector for [`ModulePattern::WrapperOverRecursion`].
/// Builds the self-recursive set for `fns` (fns that call themselves
/// by name anywhere in their body), then walks each non-recursive fn's
/// body looking for exactly one qualifying inner call. See the variant
/// docs on `WrapperOverRecursion` for the detection contract.
fn detect_wrapper_over_recursion(
    scope: Option<String>,
    fns: &[&crate::ast::FnDef],
    out: &mut Vec<ModulePattern>,
) {
    if fns.is_empty() {
        return;
    }

    let mut recursive: HashSet<String> = HashSet::new();
    for fd in fns {
        if body_calls_name(&fd.body, &fd.name) {
            recursive.insert(fd.name.clone());
        }
    }
    if recursive.is_empty() {
        return;
    }

    for fd in fns {
        if recursive.contains(&fd.name) {
            continue;
        }
        if fd.params.is_empty() {
            continue;
        }
        let outer_params: Vec<&str> = fd.params.iter().map(|(n, _)| n.as_str()).collect();
        let mut hits: Vec<String> = Vec::new();
        collect_qualifying_inner_calls(&fd.body, &outer_params, &recursive, &mut hits);
        hits.sort();
        hits.dedup();
        if hits.len() != 1 {
            continue;
        }
        let inner = hits.into_iter().next().unwrap();
        out.push(ModulePattern::WrapperOverRecursion {
            wrapper_scope: scope.clone(),
            wrapper_fn: fd.name.clone(),
            inner_scope: scope.clone(),
            inner_fn: inner,
        });
    }
}

/// Whether `body` contains any `FnCall(Ident(name), _)` reachable
/// through expression nesting. Used to build the self-recursive set
/// and to find qualifying inner calls.
fn body_calls_name(body: &crate::ast::FnBody, name: &str) -> bool {
    for stmt in body.stmts() {
        let expr = match stmt {
            crate::ast::Stmt::Binding(_, _, e) => e,
            crate::ast::Stmt::Expr(e) => e,
        };
        if expr_calls_name(expr, name) {
            return true;
        }
    }
    false
}

fn expr_calls_name(expr: &crate::ast::Spanned<crate::ast::Expr>, name: &str) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Expr::Ident(n) = &callee.node
                && n == name
            {
                return true;
            }
            if expr_calls_name(callee, name) {
                return true;
            }
            args.iter().any(|a| expr_calls_name(a, name))
        }
        Expr::TailCall(td) => td.target == name || td.args.iter().any(|a| expr_calls_name(a, name)),
        Expr::Match { subject, arms } => {
            if expr_calls_name(subject, name) {
                return true;
            }
            arms.iter().any(|a| expr_calls_name(&a.body, name))
        }
        Expr::BinOp(_, l, r) => expr_calls_name(l, name) || expr_calls_name(r, name),
        Expr::Neg(e) | Expr::Attr(e, _) | Expr::ErrorProp(e) => expr_calls_name(e, name),
        Expr::Constructor(_, Some(e)) => expr_calls_name(e, name),
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            xs.iter().any(|x| expr_calls_name(x, name))
        }
        Expr::MapLiteral(pairs) => pairs
            .iter()
            .any(|(k, v)| expr_calls_name(k, name) || expr_calls_name(v, name)),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_calls_name(e, name)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_calls_name(base, name) || updates.iter().any(|(_, e)| expr_calls_name(e, name))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            crate::ast::StrPart::Parsed(e) => expr_calls_name(e, name),
            crate::ast::StrPart::Literal(_) => false,
        }),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Constructor(_, None) | Expr::Resolved { .. } => {
            false
        }
    }
}

/// Walk `body` and push every inner-fn name that satisfies the
/// `WrapperOverRecursion` qualification rules: callee is a same-scope
/// self-recursive fn in `recursive`, arity strictly greater than
/// `outer_params.len()`, and every outer-param name appears literally
/// as an `Ident` argument somewhere in the call's argument list.
fn collect_qualifying_inner_calls(
    body: &crate::ast::FnBody,
    outer_params: &[&str],
    recursive: &HashSet<String>,
    out: &mut Vec<String>,
) {
    for stmt in body.stmts() {
        let expr = match stmt {
            crate::ast::Stmt::Binding(_, _, e) => e,
            crate::ast::Stmt::Expr(e) => e,
        };
        collect_qualifying_in_expr(expr, outer_params, recursive, out);
    }
}

fn collect_qualifying_in_expr(
    expr: &crate::ast::Spanned<crate::ast::Expr>,
    outer_params: &[&str],
    recursive: &HashSet<String>,
    out: &mut Vec<String>,
) {
    use crate::ast::Expr;
    let try_qualify = |callee: &str, args: &[crate::ast::Spanned<Expr>], out: &mut Vec<String>| {
        if !recursive.contains(callee) {
            return;
        }
        if args.len() <= outer_params.len() {
            return;
        }
        // Pre-pipeline args are `Expr::Ident(name)`; post-pipeline the
        // resolver rewrites local/param idents to
        // `Expr::Resolved { name, .. }`. Both shapes need to count.
        let mut arg_idents: HashSet<&str> = HashSet::new();
        for a in args {
            match &a.node {
                Expr::Ident(n) => {
                    arg_idents.insert(n.as_str());
                }
                Expr::Resolved { name, .. } => {
                    arg_idents.insert(name.as_str());
                }
                _ => {}
            }
        }
        if outer_params.iter().all(|p| arg_idents.contains(*p)) {
            out.push(callee.to_string());
        }
    };
    if let Expr::FnCall(callee, args) = &expr.node
        && let Expr::Ident(name) = &callee.node
    {
        try_qualify(name, args, out);
    }
    // Post-pipeline AST: tail-position calls become `TailCall`,
    // which loses the `FnCall(Ident, ...)` wrapper. The
    // `fib(n) -> fibTR(n, 0, 1)` shape is a typical case — pipeline
    // recognizes the tail call inside the `match` arm even though
    // `fib` itself isn't recursive.
    if let Expr::TailCall(td) = &expr.node {
        try_qualify(&td.target, &td.args, out);
    }
    match &expr.node {
        Expr::FnCall(callee, args) => {
            collect_qualifying_in_expr(callee, outer_params, recursive, out);
            for a in args {
                collect_qualifying_in_expr(a, outer_params, recursive, out);
            }
        }
        Expr::Match { subject, arms } => {
            collect_qualifying_in_expr(subject, outer_params, recursive, out);
            for a in arms {
                collect_qualifying_in_expr(&a.body, outer_params, recursive, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_qualifying_in_expr(l, outer_params, recursive, out);
            collect_qualifying_in_expr(r, outer_params, recursive, out);
        }
        Expr::Neg(e) | Expr::Attr(e, _) | Expr::ErrorProp(e) => {
            collect_qualifying_in_expr(e, outer_params, recursive, out);
        }
        Expr::Constructor(_, Some(e)) => {
            collect_qualifying_in_expr(e, outer_params, recursive, out);
        }
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            for x in xs {
                collect_qualifying_in_expr(x, outer_params, recursive, out);
            }
        }
        Expr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                collect_qualifying_in_expr(k, outer_params, recursive, out);
                collect_qualifying_in_expr(v, outer_params, recursive, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_qualifying_in_expr(e, outer_params, recursive, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_qualifying_in_expr(base, outer_params, recursive, out);
            for (_, e) in updates {
                collect_qualifying_in_expr(e, outer_params, recursive, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(e) = p {
                    collect_qualifying_in_expr(e, outer_params, recursive, out);
                }
            }
        }
        Expr::TailCall(td) => {
            for a in &td.args {
                collect_qualifying_in_expr(a, outer_params, recursive, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Constructor(_, None) | Expr::Resolved { .. } => {}
    }
}
