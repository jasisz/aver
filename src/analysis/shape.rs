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
