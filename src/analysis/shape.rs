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

    if candidates.is_empty() {
        return out;
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

    out
}
