use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::Path;
use std::process;

use aver::ast::{DecisionBlock, DecisionImpact, FnDef, TypeDef};
use aver::types::{Type, parse_type_str};
use colored::Colorize;

use crate::cli::ContextDepth;
use crate::context_data::{FileContext, collect_contexts};
use crate::context_format::{
    ContextSelection, collect_all_decisions, format_context_json, format_context_md,
    format_decisions_json, format_decisions_md,
};
use crate::shared::resolve_module_root;

#[derive(Clone)]
struct SelectedContext {
    contexts: Vec<FileContext>,
    selection: ContextSelection,
}

struct FocusCatalog {
    entry_module: Option<String>,
    canonical_to_context: HashMap<String, usize>,
    local_to_canonical: Vec<HashMap<String, String>>,
    unique_bare: HashMap<String, String>,
    ambiguous_bare: HashMap<String, Vec<String>>,
    direct_calls: HashMap<String, Vec<String>>,
    spec_targets: HashMap<String, Vec<String>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum CandidateKey {
    Module { ctx_idx: usize },
    Function { ctx_idx: usize, name: String },
    Type { ctx_idx: usize, name: String },
    EffectSet { ctx_idx: usize, name: String },
    Decision { ctx_idx: usize, index: usize },
}

#[derive(Clone, Debug)]
struct Candidate {
    key: CandidateKey,
    score: i32,
    depth: usize,
}

/// Cheap byte cost estimate for a candidate without full rendering.
fn estimate_candidate_cost(candidate: &Candidate, contexts: &[FileContext]) -> usize {
    match &candidate.key {
        CandidateKey::Module { ctx_idx } => {
            let ctx = &contexts[*ctx_idx];
            let module_header = 60; // "## Module: Name\n> intent...\n"
            let intent_len = ctx.intent.as_ref().map(|s| s.len() + 4).unwrap_or(0);
            module_header + intent_len
        }
        CandidateKey::Function { ctx_idx, name } => {
            let ctx = &contexts[*ctx_idx];
            let fd = ctx.fn_defs.iter().find(|fd| &fd.name == name);
            let sig_len = fd
                .map(|f| f.name.len() + f.return_type.len() + f.params.len() * 15 + 20)
                .unwrap_or(40);
            let desc_len = fd
                .and_then(|f| f.desc.as_ref())
                .map(|d| d.len() + 4)
                .unwrap_or(0);
            let verify_len = ctx
                .verify_samples
                .get(name)
                .map(|samples| samples.iter().map(|s| s.len() + 4).sum::<usize>())
                .unwrap_or(0);
            sig_len + desc_len + verify_len + 10
        }
        CandidateKey::Type { ctx_idx, name } => {
            let ctx = &contexts[*ctx_idx];
            ctx.type_defs
                .iter()
                .find(|td| match td {
                    TypeDef::Sum { name: n, .. } | TypeDef::Product { name: n, .. } => n == name,
                })
                .map(|td| match td {
                    TypeDef::Sum { variants, .. } => variants.len() * 20 + 30,
                    TypeDef::Product { fields, .. } => fields.len() * 20 + 30,
                })
                .unwrap_or(30)
        }
        CandidateKey::EffectSet { .. } => 40,
        CandidateKey::Decision { ctx_idx, index } => {
            let ctx = &contexts[*ctx_idx];
            ctx.decisions
                .get(*index)
                .map(|d| d.name.len() + d.reason.len() + 40)
                .unwrap_or(40)
        }
    }
}

#[derive(Clone, Default)]
struct SelectionState {
    modules: HashSet<usize>,
    functions: HashMap<usize, HashSet<String>>,
    types: HashMap<usize, HashSet<String>>,
    effect_sets: HashMap<usize, HashSet<String>>,
    decisions: HashMap<usize, HashSet<usize>>,
}

#[derive(Clone, Default)]
struct TypeCatalog {
    canonical_to_type: HashMap<String, (usize, String)>,
    local_to_canonical: Vec<HashMap<String, String>>,
    unique_bare: HashMap<String, String>,
}

struct ScoringContext {
    module_depths: Vec<usize>,
    focus_symbol: Option<String>,
    focus_depths: HashMap<String, usize>,
    inbound_calls: HashMap<String, usize>,
    fn_type_refs: HashMap<(usize, String), Vec<(usize, String)>>,
    type_usage_counts: HashMap<(usize, String), usize>,
    catalog: FocusCatalog,
}

fn byte_label(bytes: usize) -> String {
    if bytes >= 1024 * 1024 && bytes.is_multiple_of(1024 * 1024) {
        format!("{}mb", bytes / (1024 * 1024))
    } else if bytes >= 1024 && bytes.is_multiple_of(1024) {
        format!("{}kb", bytes / 1024)
    } else {
        format!("{}b", bytes)
    }
}

fn unique_sorted(items: impl IntoIterator<Item = String>) -> Vec<String> {
    let mut out = items
        .into_iter()
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    out.sort();
    out
}

fn render_context_content(
    contexts: &[FileContext],
    entry_label: &str,
    json: bool,
    decisions_only: bool,
    selection: Option<&ContextSelection>,
) -> String {
    if decisions_only {
        let decisions = collect_all_decisions(contexts);
        if json {
            format_decisions_json(&decisions, entry_label, selection)
        } else {
            format_decisions_md(&decisions, entry_label, selection)
        }
    } else if json {
        format_context_json(contexts, entry_label, selection)
    } else {
        format_context_md(contexts, entry_label, selection)
    }
}

fn collect_contexts_at_depth(
    file: &str,
    module_root: &str,
    depth: Option<usize>,
) -> Vec<FileContext> {
    let mut visited = HashSet::new();
    collect_contexts(file, module_root, &mut visited, depth)
}

fn render_with_selection(
    contexts: &[FileContext],
    entry_label: &str,
    json: bool,
    decisions_only: bool,
    mut selection: ContextSelection,
) -> (String, ContextSelection) {
    loop {
        let content = render_context_content(
            contexts,
            entry_label,
            json,
            decisions_only,
            Some(&selection),
        );
        let used_bytes = content.len();
        if used_bytes == selection.used_bytes {
            return (content, selection);
        }
        selection.used_bytes = used_bytes;
    }
}

fn selection_template(
    depth_mode: &str,
    budget_bytes: Option<usize>,
    included_depth: Option<usize>,
    focus_symbol: Option<&str>,
    next_depth: Option<usize>,
    next_used_bytes: Option<usize>,
    truncated: bool,
) -> ContextSelection {
    ContextSelection {
        depth_mode: depth_mode.to_string(),
        budget_bytes,
        included_depth,
        next_depth,
        next_used_bytes,
        truncated,
        used_bytes: 0,
        focus_symbol: focus_symbol.map(str::to_string),
    }
}

fn render_size(
    contexts: &[FileContext],
    entry_label: &str,
    json: bool,
    decisions_only: bool,
    selection: ContextSelection,
) -> usize {
    render_with_selection(contexts, entry_label, json, decisions_only, selection)
        .1
        .used_bytes
}

fn build_module_depths(contexts: &[FileContext]) -> Vec<usize> {
    let mut depths = vec![usize::MAX; contexts.len()];
    if contexts.is_empty() {
        return depths;
    }

    let module_to_idx = contexts
        .iter()
        .enumerate()
        .filter_map(|(idx, ctx)| ctx.module_name.as_ref().map(|name| (name.clone(), idx)))
        .collect::<HashMap<_, _>>();

    let mut queue = VecDeque::from([(0usize, 0usize)]);
    while let Some((ctx_idx, depth)) = queue.pop_front() {
        if depth >= depths[ctx_idx] {
            continue;
        }
        depths[ctx_idx] = depth;
        for dep in &contexts[ctx_idx].depends {
            if let Some(&dep_idx) = module_to_idx.get(dep) {
                queue.push_back((dep_idx, depth + 1));
            }
        }
    }

    for depth in &mut depths {
        if *depth == usize::MAX {
            *depth = 0;
        }
    }
    depths
}

fn collect_named_type_refs(ty: &Type, out: &mut Vec<String>) {
    match ty {
        Type::Named(name) => out.push(name.clone()),
        Type::Result(ok, err) | Type::Map(ok, err) => {
            collect_named_type_refs(ok, out);
            collect_named_type_refs(err, out);
        }
        Type::Option(inner) | Type::List(inner) => collect_named_type_refs(inner, out),
        Type::Tuple(items) => {
            for item in items {
                collect_named_type_refs(item, out);
            }
        }
        Type::Fn(params, ret, _) => {
            for param in params {
                collect_named_type_refs(param, out);
            }
            collect_named_type_refs(ret, out);
        }
        Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit | Type::Unknown => {}
    }
}

fn build_type_catalog(contexts: &[FileContext]) -> TypeCatalog {
    let mut canonical_to_type = HashMap::new();
    let mut local_to_canonical = vec![HashMap::new(); contexts.len()];
    let mut bare_names: HashMap<String, Vec<String>> = HashMap::new();

    for (ctx_idx, ctx) in contexts.iter().enumerate() {
        for td in &ctx.type_defs {
            let local_name = match td {
                TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name.clone(),
            };
            let canonical = canonical_fn_name(ctx.module_name.as_deref(), &local_name);
            canonical_to_type.insert(canonical.clone(), (ctx_idx, local_name.clone()));
            local_to_canonical[ctx_idx].insert(local_name.clone(), canonical.clone());
            bare_names.entry(local_name).or_default().push(canonical);
        }
    }

    let unique_bare = bare_names
        .into_iter()
        .filter_map(|(bare, mut matches)| {
            matches.sort();
            matches.dedup();
            (matches.len() == 1).then(|| (bare, matches[0].clone()))
        })
        .collect::<HashMap<_, _>>();

    TypeCatalog {
        canonical_to_type,
        local_to_canonical,
        unique_bare,
    }
}

fn resolve_type_ref(raw: &str, ctx_idx: usize, catalog: &TypeCatalog) -> Option<(usize, String)> {
    if let Some(canonical) = catalog.local_to_canonical[ctx_idx].get(raw) {
        return catalog.canonical_to_type.get(canonical).cloned();
    }
    if let Some((found_idx, local_name)) = catalog.canonical_to_type.get(raw) {
        return Some((*found_idx, local_name.clone()));
    }
    catalog
        .unique_bare
        .get(raw)
        .and_then(|canonical| catalog.canonical_to_type.get(canonical))
        .cloned()
}

fn build_scoring_context(contexts: &[FileContext], focus_symbol: Option<&str>) -> ScoringContext {
    let module_depths = build_module_depths(contexts);
    let catalog = build_focus_catalog(contexts);
    let type_catalog = build_type_catalog(contexts);

    let mut inbound_calls = HashMap::new();
    for callees in catalog.direct_calls.values() {
        for callee in callees {
            *inbound_calls.entry(callee.clone()).or_insert(0) += 1;
        }
    }

    let mut focus_depths = HashMap::new();
    let mut resolved_focus = None;
    if let Some(focus) = focus_symbol
        && let Ok(canonical_focus) = resolve_focus_symbol(&catalog, focus)
    {
        resolved_focus = Some(canonical_focus.clone());
        let mut queue = VecDeque::from([(canonical_focus.clone(), 0usize)]);
        while let Some((name, depth)) = queue.pop_front() {
            if focus_depths.contains_key(&name) {
                continue;
            }
            focus_depths.insert(name.clone(), depth);
            for callee in catalog.direct_calls.get(&name).into_iter().flatten() {
                queue.push_back((callee.clone(), depth + 1));
            }
            for spec in catalog.spec_targets.get(&name).into_iter().flatten() {
                queue.push_back((spec.clone(), depth + 1));
            }
        }
    }

    let mut fn_type_refs = HashMap::new();
    let mut type_usage_counts = HashMap::new();
    for (ctx_idx, ctx) in contexts.iter().enumerate() {
        for fd in &ctx.fn_defs {
            let mut refs = Vec::new();
            for (_, param_ty) in &fd.params {
                collect_named_type_refs(&parse_type_str(param_ty), &mut refs);
            }
            collect_named_type_refs(&parse_type_str(&fd.return_type), &mut refs);

            let mut resolved_refs = Vec::new();
            for type_ref in refs {
                if let Some((type_ctx_idx, local_name)) =
                    resolve_type_ref(&type_ref, ctx_idx, &type_catalog)
                {
                    *type_usage_counts
                        .entry((type_ctx_idx, local_name.clone()))
                        .or_insert(0) += 1;
                    resolved_refs.push((type_ctx_idx, local_name));
                }
            }
            resolved_refs.sort();
            resolved_refs.dedup();
            fn_type_refs.insert((ctx_idx, fd.name.clone()), resolved_refs);
        }
    }

    ScoringContext {
        module_depths,
        focus_symbol: resolved_focus,
        focus_depths,
        inbound_calls,
        fn_type_refs,
        type_usage_counts,
        catalog,
    }
}

fn add_type_deps_for_function(
    state: &mut SelectionState,
    scoring: &ScoringContext,
    ctx_idx: usize,
    fn_name: &str,
) {
    if let Some(type_refs) = scoring.fn_type_refs.get(&(ctx_idx, fn_name.to_string())) {
        for (type_ctx_idx, type_name) in type_refs {
            state.modules.insert(*type_ctx_idx);
            state
                .types
                .entry(*type_ctx_idx)
                .or_default()
                .insert(type_name.clone());
        }
    }
}

fn apply_candidate(
    state: &SelectionState,
    scoring: &ScoringContext,
    candidate: &CandidateKey,
) -> SelectionState {
    let mut next = state.clone();
    match candidate {
        CandidateKey::Module { ctx_idx } => {
            next.modules.insert(*ctx_idx);
        }
        CandidateKey::Function { ctx_idx, name } => {
            next.modules.insert(*ctx_idx);
            next.functions
                .entry(*ctx_idx)
                .or_default()
                .insert(name.clone());
            add_type_deps_for_function(&mut next, scoring, *ctx_idx, name);
        }
        CandidateKey::Type { ctx_idx, name } => {
            next.modules.insert(*ctx_idx);
            next.types.entry(*ctx_idx).or_default().insert(name.clone());
        }
        CandidateKey::EffectSet { ctx_idx, name } => {
            next.modules.insert(*ctx_idx);
            next.effect_sets
                .entry(*ctx_idx)
                .or_default()
                .insert(name.clone());
        }
        CandidateKey::Decision { ctx_idx, index } => {
            next.modules.insert(*ctx_idx);
            next.decisions.entry(*ctx_idx).or_default().insert(*index);
        }
    }
    next
}

fn materialize_selection(contexts: &[FileContext], state: &SelectionState) -> Vec<FileContext> {
    let mut out = Vec::new();
    for (ctx_idx, ctx) in contexts.iter().enumerate() {
        let module_selected = state.modules.contains(&ctx_idx);
        let selected_fns = state.functions.get(&ctx_idx);
        let selected_types = state.types.get(&ctx_idx);
        let selected_effect_sets = state.effect_sets.get(&ctx_idx);
        let selected_decisions = state.decisions.get(&ctx_idx);

        if !module_selected
            && selected_fns.is_none()
            && selected_types.is_none()
            && selected_effect_sets.is_none()
            && selected_decisions.is_none()
        {
            continue;
        }

        let mut next = ctx.clone();
        if let Some(selected_fns) = selected_fns {
            next.fn_defs = ctx
                .fn_defs
                .iter()
                .filter(|fd| selected_fns.contains(&fd.name))
                .cloned()
                .collect();
            next.all_fn_defs = next.fn_defs.clone();
            next.verify_blocks
                .retain(|verify| selected_fns.contains(&verify.fn_name));
            next.verify_counts
                .retain(|name, _| selected_fns.contains(name));
            next.verify_samples
                .retain(|name, _| selected_fns.contains(name));
            next.fn_auto_memo.retain(|name| selected_fns.contains(name));
            next.fn_memo_qual
                .retain(|name, _| selected_fns.contains(name));
            next.fn_auto_tco.retain(|name| selected_fns.contains(name));
            next.fn_recursive_callsites
                .retain(|name, _| selected_fns.contains(name));
            next.fn_recursive_scc_id
                .retain(|name, _| selected_fns.contains(name));
            next.fn_specs.retain(|name, _| selected_fns.contains(name));
            next.fn_direct_calls
                .retain(|name, _| selected_fns.contains(name));
        } else {
            next.fn_defs.clear();
            next.all_fn_defs.clear();
            next.verify_blocks.clear();
            next.verify_counts.clear();
            next.verify_samples.clear();
            next.fn_auto_memo.clear();
            next.fn_memo_qual.clear();
            next.fn_auto_tco.clear();
            next.fn_recursive_callsites.clear();
            next.fn_recursive_scc_id.clear();
            next.fn_specs.clear();
            next.fn_direct_calls.clear();
        }

        if let Some(selected_types) = selected_types {
            next.type_defs.retain(|td| match td {
                TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => {
                    selected_types.contains(name)
                }
            });
        } else {
            next.type_defs.clear();
        }

        if let Some(selected_effect_sets) = selected_effect_sets {
            next.effect_sets
                .retain(|(name, _)| selected_effect_sets.contains(name));
        } else {
            next.effect_sets.clear();
        }

        if let Some(selected_decisions) = selected_decisions {
            next.decisions = ctx
                .decisions
                .iter()
                .enumerate()
                .filter(|(idx, _)| selected_decisions.contains(idx))
                .map(|(_, decision)| decision.clone())
                .collect();
        } else {
            next.decisions.clear();
        }

        next.module_effects = unique_sorted(
            next.fn_defs
                .iter()
                .flat_map(|fd| fd.effects.iter().cloned()),
        );
        next.api_effects = unique_sorted(
            next.fn_defs
                .iter()
                .filter(|fd| next.exposes.contains(&fd.name))
                .flat_map(|fd| fd.effects.iter().cloned()),
        );
        next.main_effects = next
            .fn_defs
            .iter()
            .find(|fd| fd.name == "main")
            .map(|fd| unique_sorted(fd.effects.iter().cloned()));

        out.push(next);
    }
    out
}

fn score_module(ctx_idx: usize, ctx: &FileContext, scoring: &ScoringContext) -> Candidate {
    let depth = scoring.module_depths[ctx_idx];
    let mut score = if ctx_idx == 0 { 240 } else { 90 };
    if ctx.intent.is_some() {
        score += 40;
    }
    if ctx.main_effects.is_some() {
        score += 20;
    }
    score -= (depth as i32) * 12;
    Candidate {
        key: CandidateKey::Module { ctx_idx },
        score,
        depth,
    }
}

fn score_function(
    ctx_idx: usize,
    ctx: &FileContext,
    fd: &FnDef,
    scoring: &ScoringContext,
) -> Candidate {
    let depth = scoring.module_depths[ctx_idx];
    let canonical = canonical_fn_name(ctx.module_name.as_deref(), &fd.name);
    let verify_count = ctx
        .verify_blocks
        .iter()
        .filter(|vb| vb.fn_name == fd.name)
        .map(|vb| vb.cases.len())
        .sum::<usize>();
    let inbound = scoring.inbound_calls.get(&canonical).copied().unwrap_or(0) as i32;

    let mut score = 160;
    if ctx_idx == 0 {
        score += 60;
    }
    if ctx.exposes.contains(&fd.name) {
        score += 35;
    }
    if fd.desc.is_some() {
        score += 25;
    }
    score += (verify_count.min(6) as i32) * 12;
    score += (ctx.fn_specs.get(&fd.name).map_or(0, Vec::len).min(3) as i32) * 20;
    if !fd.effects.is_empty() {
        score += 20;
    }
    if ctx.fn_auto_memo.contains(&fd.name) {
        score += 15;
    }
    if ctx.fn_auto_tco.contains(&fd.name) {
        score += 15;
    }
    score += inbound.min(4) * 10;
    if let Some(focus_symbol) = &scoring.focus_symbol {
        if &canonical == focus_symbol {
            score += 1000;
        } else if let Some(call_depth) = scoring.focus_depths.get(&canonical) {
            score += match *call_depth {
                0 => 700,
                1 => 350,
                2 => 180,
                3 => 90,
                _ => 40,
            };
        }
    }
    score -= (depth as i32) * 10;

    Candidate {
        key: CandidateKey::Function {
            ctx_idx,
            name: fd.name.clone(),
        },
        score,
        depth,
    }
}

fn score_type(
    ctx_idx: usize,
    _ctx: &FileContext,
    td: &TypeDef,
    scoring: &ScoringContext,
) -> Candidate {
    let depth = scoring.module_depths[ctx_idx];
    let name = match td {
        TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name.clone(),
    };
    let usage = scoring
        .type_usage_counts
        .get(&(ctx_idx, name.clone()))
        .copied()
        .unwrap_or(0) as i32;
    let mut score = 70 + usage * 22;
    if ctx_idx == 0 {
        score += 30;
    }
    score -= (depth as i32) * 8;
    Candidate {
        key: CandidateKey::Type { ctx_idx, name },
        score,
        depth,
    }
}

fn score_effect_set(ctx_idx: usize, name: &str, scoring: &ScoringContext) -> Candidate {
    let depth = scoring.module_depths[ctx_idx];
    let mut score = 45;
    if ctx_idx == 0 {
        score += 20;
    }
    score -= (depth as i32) * 8;
    Candidate {
        key: CandidateKey::EffectSet {
            ctx_idx,
            name: name.to_string(),
        },
        score,
        depth,
    }
}

fn decision_impacts_focus(
    decision: &DecisionBlock,
    focus_symbol: &str,
    module_name: Option<&str>,
) -> bool {
    decision.impacts.iter().any(|impact| match impact {
        DecisionImpact::Symbol(name) => {
            name == focus_symbol
                || module_name.is_some_and(|module_name| {
                    let prefix = format!("{module_name}.");
                    focus_symbol.starts_with(&prefix) && name == &focus_symbol[prefix.len()..]
                })
        }
        DecisionImpact::Semantic(_) => false,
    })
}

fn score_decision(
    ctx_idx: usize,
    ctx: &FileContext,
    decision: &DecisionBlock,
    index: usize,
    scoring: &ScoringContext,
) -> Candidate {
    let depth = scoring.module_depths[ctx_idx];
    let mut score = 110;
    if ctx_idx == 0 {
        score += 40;
    }
    score += (decision.impacts.len().min(4) as i32) * 12;
    if !decision.reason.is_empty() {
        score += 15;
    }
    if let Some(focus_symbol) = &scoring.focus_symbol
        && decision_impacts_focus(decision, focus_symbol, ctx.module_name.as_deref())
    {
        score += 300;
    }
    score -= (depth as i32) * 9;
    Candidate {
        key: CandidateKey::Decision { ctx_idx, index },
        score,
        depth,
    }
}

fn build_candidates(
    contexts: &[FileContext],
    scoring: &ScoringContext,
    decisions_only: bool,
) -> Vec<Candidate> {
    let mut candidates = Vec::new();
    for (ctx_idx, ctx) in contexts.iter().enumerate() {
        if !decisions_only {
            candidates.push(score_module(ctx_idx, ctx, scoring));
            for fd in &ctx.fn_defs {
                candidates.push(score_function(ctx_idx, ctx, fd, scoring));
            }
            for td in &ctx.type_defs {
                candidates.push(score_type(ctx_idx, ctx, td, scoring));
            }
            for (name, _) in &ctx.effect_sets {
                candidates.push(score_effect_set(ctx_idx, name, scoring));
            }
        }
        for (index, decision) in ctx.decisions.iter().enumerate() {
            candidates.push(score_decision(ctx_idx, ctx, decision, index, scoring));
        }
    }
    candidates
}

fn candidate_selected(state: &SelectionState, candidate: &CandidateKey) -> bool {
    match candidate {
        CandidateKey::Module { ctx_idx } => state.modules.contains(ctx_idx),
        CandidateKey::Function { ctx_idx, name } => state
            .functions
            .get(ctx_idx)
            .is_some_and(|names| names.contains(name)),
        CandidateKey::Type { ctx_idx, name } => state
            .types
            .get(ctx_idx)
            .is_some_and(|names| names.contains(name)),
        CandidateKey::EffectSet { ctx_idx, name } => state
            .effect_sets
            .get(ctx_idx)
            .is_some_and(|names| names.contains(name)),
        CandidateKey::Decision { ctx_idx, index } => state
            .decisions
            .get(ctx_idx)
            .is_some_and(|indices| indices.contains(index)),
    }
}

fn ratio_cmp(left_score: i32, left_cost: usize, right_score: i32, right_cost: usize) -> Ordering {
    let left_cost = left_cost.max(1) as i64;
    let right_cost = right_cost.max(1) as i64;
    let left_score = left_score as i64;
    let right_score = right_score as i64;
    (left_score * right_cost)
        .cmp(&(right_score * left_cost))
        .then_with(|| left_score.cmp(&right_score))
        .then_with(|| right_cost.cmp(&left_cost).reverse())
}

fn included_depth_for_state(state: &SelectionState, module_depths: &[usize]) -> Option<usize> {
    state.modules.iter().map(|idx| module_depths[*idx]).max()
}

fn collect_contexts_auto_scored(
    contexts: Vec<FileContext>,
    entry_label: &str,
    json: bool,
    decisions_only: bool,
    budget: usize,
    focus_symbol: Option<&str>,
    depth_mode: &str,
) -> SelectedContext {
    if contexts.is_empty() {
        return SelectedContext {
            contexts,
            selection: selection_template(
                depth_mode,
                Some(budget),
                Some(0),
                focus_symbol,
                None,
                None,
                false,
            ),
        };
    }

    let scoring = build_scoring_context(&contexts, focus_symbol);
    let candidates = build_candidates(&contexts, &scoring, decisions_only);
    let mut state = SelectionState::default();
    if !decisions_only {
        state.modules.insert(0);
        if let Some(focus_symbol) = &scoring.focus_symbol
            && let Some(&ctx_idx) = scoring.catalog.canonical_to_context.get(focus_symbol)
            && let Some(local_name) = contexts[ctx_idx]
                .fn_defs
                .iter()
                .find(|fd| {
                    canonical_fn_name(contexts[ctx_idx].module_name.as_deref(), &fd.name)
                        == *focus_symbol
                })
                .map(|fd| fd.name.clone())
        {
            state = apply_candidate(
                &state,
                &scoring,
                &CandidateKey::Function {
                    ctx_idx,
                    name: local_name,
                },
            );
        }
    }

    let mut selected_contexts = materialize_selection(&contexts, &state);
    let mut base_selection = selection_template(
        depth_mode,
        Some(budget),
        included_depth_for_state(&state, &scoring.module_depths),
        scoring.focus_symbol.as_deref(),
        None,
        None,
        false,
    );
    let mut used_bytes = render_size(
        &selected_contexts,
        entry_label,
        json,
        decisions_only,
        base_selection.clone(),
    );

    // Greedy selection: pick candidates by score/cost ratio until budget is exhausted.
    // Uses cheap cost estimates instead of full rendering to avoid O(n²) blowup.
    let mut remaining_candidates: Vec<usize> = (0..candidates.len()).collect();
    let truncated = loop {
        // Find best candidate that fits in remaining budget
        let best = remaining_candidates
            .iter()
            .copied()
            .filter(|&idx| !candidate_selected(&state, &candidates[idx].key))
            .filter_map(|idx| {
                let cost = estimate_candidate_cost(&candidates[idx], &contexts);
                (used_bytes + cost <= budget || used_bytes == 0).then_some((idx, cost))
            })
            .max_by(|(left_idx, left_cost), (right_idx, right_cost)| {
                ratio_cmp(
                    candidates[*left_idx].score,
                    *left_cost,
                    candidates[*right_idx].score,
                    *right_cost,
                )
                .then_with(|| {
                    candidates[*right_idx]
                        .score
                        .cmp(&candidates[*left_idx].score)
                })
            });

        let Some((best_idx, best_cost)) = best else {
            // No more candidates fit — check if any were skipped (truncation)
            break remaining_candidates
                .iter()
                .any(|&idx| !candidate_selected(&state, &candidates[idx].key));
        };

        state = apply_candidate(&state, &scoring, &candidates[best_idx].key);
        used_bytes += best_cost;
        remaining_candidates.retain(|&idx| idx != best_idx);
        selected_contexts = materialize_selection(&contexts, &state);
        base_selection.included_depth = included_depth_for_state(&state, &scoring.module_depths);
    };

    // Recalculate exact used bytes with final selection for accurate reporting
    used_bytes = render_size(
        &selected_contexts,
        entry_label,
        json,
        decisions_only,
        base_selection.clone(),
    );

    let next_candidate = remaining_candidates
        .iter()
        .filter(|&&idx| !candidate_selected(&state, &candidates[idx].key))
        .map(|&idx| &candidates[idx])
        .max_by(|a, b| a.score.cmp(&b.score));

    let next_included_depth = next_candidate.map(|c| c.depth);
    let next_used_bytes =
        next_candidate.map(|c| used_bytes + estimate_candidate_cost(c, &contexts));

    let selection = selection_template(
        depth_mode,
        Some(budget),
        included_depth_for_state(&state, &scoring.module_depths),
        scoring.focus_symbol.as_deref(),
        next_included_depth,
        next_used_bytes,
        truncated,
    );

    SelectedContext {
        contexts: selected_contexts,
        selection,
    }
}

fn collect_contexts_auto(
    file: &str,
    module_root: &str,
    entry_label: &str,
    json: bool,
    decisions_only: bool,
    budget: usize,
) -> SelectedContext {
    collect_contexts_auto_scored(
        collect_contexts_at_depth(file, module_root, None),
        entry_label,
        json,
        decisions_only,
        budget,
        None,
        "auto",
    )
}

fn canonical_fn_name(module_name: Option<&str>, local_name: &str) -> String {
    match module_name {
        Some(module_name) => format!("{module_name}.{local_name}"),
        None => local_name.to_string(),
    }
}

fn resolve_symbol_in_context_parts(
    raw: &str,
    ctx_idx: usize,
    local_to_canonical: &[HashMap<String, String>],
    unique_bare: &HashMap<String, String>,
    canonical_to_context: &HashMap<String, usize>,
) -> Option<String> {
    if let Some(local) = local_to_canonical[ctx_idx].get(raw) {
        return Some(local.clone());
    }
    if let Some(canonical) = unique_bare.get(raw) {
        return Some(canonical.clone());
    }
    canonical_to_context
        .contains_key(raw)
        .then(|| raw.to_string())
}

fn build_focus_catalog(contexts: &[FileContext]) -> FocusCatalog {
    let mut canonical_to_context = HashMap::new();
    let mut local_to_canonical = vec![HashMap::new(); contexts.len()];
    let mut bare_names: HashMap<String, Vec<String>> = HashMap::new();

    for (ctx_idx, ctx) in contexts.iter().enumerate() {
        for fd in &ctx.all_fn_defs {
            let canonical = canonical_fn_name(ctx.module_name.as_deref(), &fd.name);
            canonical_to_context.insert(canonical.clone(), ctx_idx);
            local_to_canonical[ctx_idx].insert(fd.name.clone(), canonical.clone());
            bare_names
                .entry(fd.name.clone())
                .or_default()
                .push(canonical);
        }
    }

    let mut unique_bare = HashMap::new();
    let mut ambiguous_bare = HashMap::new();
    for (bare, mut matches) in bare_names {
        matches.sort();
        matches.dedup();
        if matches.len() == 1 {
            unique_bare.insert(bare, matches[0].clone());
        } else {
            ambiguous_bare.insert(bare, matches);
        }
    }

    let mut direct_calls = HashMap::new();
    let mut spec_targets = HashMap::new();

    for (ctx_idx, ctx) in contexts.iter().enumerate() {
        for fd in &ctx.all_fn_defs {
            let Some(caller) = local_to_canonical[ctx_idx].get(&fd.name).cloned() else {
                continue;
            };
            let calls = ctx
                .fn_direct_calls
                .get(&fd.name)
                .into_iter()
                .flat_map(|calls| calls.iter())
                .filter_map(|raw| {
                    resolve_symbol_in_context_parts(
                        raw,
                        ctx_idx,
                        &local_to_canonical,
                        &unique_bare,
                        &canonical_to_context,
                    )
                })
                .collect::<Vec<_>>();
            direct_calls.insert(caller.clone(), unique_sorted(calls));

            let specs = ctx
                .fn_specs
                .get(&fd.name)
                .into_iter()
                .flat_map(|specs| specs.iter())
                .filter_map(|raw| {
                    resolve_symbol_in_context_parts(
                        raw,
                        ctx_idx,
                        &local_to_canonical,
                        &unique_bare,
                        &canonical_to_context,
                    )
                })
                .collect::<Vec<_>>();
            spec_targets.insert(caller, unique_sorted(specs));
        }
    }

    FocusCatalog {
        entry_module: contexts.first().and_then(|ctx| ctx.module_name.clone()),
        canonical_to_context,
        local_to_canonical,
        unique_bare,
        ambiguous_bare,
        direct_calls,
        spec_targets,
    }
}

fn resolve_focus_symbol(catalog: &FocusCatalog, focus: &str) -> Result<String, String> {
    if catalog.canonical_to_context.contains_key(focus) {
        return Ok(focus.to_string());
    }

    if let Some(canonical) = catalog.unique_bare.get(focus) {
        return Ok(canonical.clone());
    }

    if let Some(candidates) = catalog.ambiguous_bare.get(focus) {
        if let Some(entry_module) = &catalog.entry_module {
            let prefix = format!("{entry_module}.");
            let preferred = candidates
                .iter()
                .filter(|candidate| candidate.starts_with(&prefix))
                .collect::<Vec<_>>();
            if preferred.len() == 1 {
                return Ok((*preferred[0]).clone());
            }
        }
        return Err(format!(
            "Ambiguous focus symbol '{}'. Use one of: {}",
            focus,
            candidates.join(", ")
        ));
    }

    Err(format!("Focus symbol '{}' not found.", focus))
}

fn selected_symbol_names(
    selected_canonical: &HashSet<String>,
    module_name: Option<&str>,
) -> HashSet<String> {
    let mut names = HashSet::new();
    let Some(module_name) = module_name else {
        return names;
    };
    let prefix = format!("{module_name}.");
    for symbol in selected_canonical {
        if let Some(local) = symbol.strip_prefix(&prefix) {
            names.insert(local.to_string());
        }
    }
    names
}

fn decision_relevant(
    decision: &DecisionBlock,
    selected_canonical: &HashSet<String>,
    selected_local: &HashSet<String>,
    module_name: Option<&str>,
) -> bool {
    decision.impacts.iter().any(|impact| match impact {
        DecisionImpact::Symbol(name) => {
            selected_canonical.contains(name)
                || selected_local.contains(name)
                || module_name.is_some_and(|module_name| {
                    name == module_name
                        || selected_canonical.contains(&format!("{module_name}.{name}"))
                })
        }
        DecisionImpact::Semantic(_) => false,
    })
}

fn filter_contexts_to_focus(
    contexts: &[FileContext],
    catalog: &FocusCatalog,
    focus_symbol: &str,
    max_depth: Option<usize>,
) -> Vec<FileContext> {
    let mut selected = HashSet::from([focus_symbol.to_string()]);
    let mut frontier = vec![focus_symbol.to_string()];
    let depth_limit = max_depth.unwrap_or(usize::MAX);

    for _ in 0..depth_limit {
        let mut next_frontier = Vec::new();
        for caller in frontier {
            for callee in catalog.direct_calls.get(&caller).into_iter().flatten() {
                if selected.insert(callee.clone()) {
                    next_frontier.push(callee.clone());
                }
            }
        }
        if next_frontier.is_empty() {
            break;
        }
        frontier = next_frontier;
    }

    let mut changed = true;
    while changed {
        changed = false;
        let current = selected.iter().cloned().collect::<Vec<_>>();
        for symbol in current {
            for spec in catalog.spec_targets.get(&symbol).into_iter().flatten() {
                if selected.insert(spec.clone()) {
                    changed = true;
                }
            }
        }
    }

    let mut filtered = Vec::new();
    for (ctx_idx, ctx) in contexts.iter().enumerate() {
        let local_selected = selected_symbol_names(&selected, ctx.module_name.as_deref())
            .into_iter()
            .filter(|name| catalog.local_to_canonical[ctx_idx].contains_key(name))
            .collect::<HashSet<_>>();

        let decisions = ctx
            .decisions
            .iter()
            .filter(|decision| {
                decision_relevant(
                    decision,
                    &selected,
                    &local_selected,
                    ctx.module_name.as_deref(),
                )
            })
            .cloned()
            .collect::<Vec<_>>();

        if local_selected.is_empty() && decisions.is_empty() {
            continue;
        }

        let filtered_fns = ctx
            .all_fn_defs
            .iter()
            .filter(|fd| local_selected.contains(&fd.name))
            .cloned()
            .collect::<Vec<_>>();

        let mut next = ctx.clone();
        next.fn_defs = filtered_fns.clone();
        next.all_fn_defs = filtered_fns.clone();
        next.verify_blocks
            .retain(|verify| local_selected.contains(&verify.fn_name));
        next.verify_counts
            .retain(|name, _| local_selected.contains(name));
        next.verify_samples
            .retain(|name, _| local_selected.contains(name));
        next.fn_auto_memo
            .retain(|name| local_selected.contains(name));
        next.fn_memo_qual
            .retain(|name, _| local_selected.contains(name));
        next.fn_auto_tco
            .retain(|name| local_selected.contains(name));
        next.fn_recursive_callsites
            .retain(|name, _| local_selected.contains(name));
        next.fn_recursive_scc_id
            .retain(|name, _| local_selected.contains(name));
        next.fn_specs
            .retain(|name, _| local_selected.contains(name));
        next.fn_direct_calls
            .retain(|name, _| local_selected.contains(name));
        next.decisions = decisions;

        next.module_effects = unique_sorted(
            filtered_fns
                .iter()
                .flat_map(|fd| fd.effects.iter().cloned()),
        );
        next.api_effects = unique_sorted(
            filtered_fns
                .iter()
                .filter(|fd| next.exposes.contains(&fd.name))
                .flat_map(|fd| fd.effects.iter().cloned()),
        );
        next.main_effects = filtered_fns
            .iter()
            .find(|fd| fd.name == "main")
            .map(|fd| unique_sorted(fd.effects.iter().cloned()));

        if next.fn_defs.is_empty() {
            next.type_defs.clear();
            next.effect_sets.clear();
        }

        filtered.push(next);
    }

    filtered
}

fn collect_contexts_focus_auto(
    file: &str,
    module_root: &str,
    entry_label: &str,
    json: bool,
    decisions_only: bool,
    budget: usize,
    focus: &str,
) -> Result<SelectedContext, String> {
    let all_contexts = collect_contexts_at_depth(file, module_root, None);
    if all_contexts.is_empty() {
        return Ok(SelectedContext {
            contexts: all_contexts,
            selection: selection_template(
                "focus-auto",
                Some(budget),
                Some(0),
                Some(focus),
                None,
                None,
                false,
            ),
        });
    }

    let catalog = build_focus_catalog(&all_contexts);
    let canonical_focus = resolve_focus_symbol(&catalog, focus)?;
    let focus_contexts = filter_contexts_to_focus(&all_contexts, &catalog, &canonical_focus, None);
    if focus_contexts.is_empty() {
        return Err(format!(
            "Focus symbol '{}' has no context to display.",
            canonical_focus
        ));
    }
    Ok(collect_contexts_auto_scored(
        focus_contexts,
        entry_label,
        json,
        decisions_only,
        budget,
        Some(&canonical_focus),
        "focus-auto",
    ))
}

fn collect_contexts_focus(
    file: &str,
    module_root: &str,
    focus: &str,
    depth: Option<usize>,
    depth_mode: &str,
) -> Result<SelectedContext, String> {
    let all_contexts = collect_contexts_at_depth(file, module_root, None);
    if all_contexts.is_empty() {
        return Ok(SelectedContext {
            contexts: all_contexts,
            selection: selection_template(depth_mode, None, depth, Some(focus), None, None, false),
        });
    }

    let catalog = build_focus_catalog(&all_contexts);
    let canonical_focus = resolve_focus_symbol(&catalog, focus)?;
    let contexts = filter_contexts_to_focus(&all_contexts, &catalog, &canonical_focus, depth);
    if contexts.is_empty() {
        return Err(format!(
            "Focus symbol '{}' has no context to display.",
            canonical_focus
        ));
    }

    Ok(SelectedContext {
        contexts,
        selection: selection_template(
            depth_mode,
            None,
            depth,
            Some(&canonical_focus),
            None,
            None,
            false,
        ),
    })
}

fn summarize_selection(selection: &ContextSelection) -> String {
    let mut parts = vec![format!("mode {}", selection.depth_mode)];
    if let Some(focus_symbol) = &selection.focus_symbol {
        parts.push(format!("focus {}", focus_symbol));
    }
    if let Some(depth) = selection.included_depth {
        parts.push(format!("included depth {}", depth));
    }
    parts.push(format!("used {}", byte_label(selection.used_bytes)));
    if let Some(budget) = selection.budget_bytes {
        parts.push(format!("budget {}", byte_label(budget)));
    }
    if selection.truncated {
        parts.push("truncated".to_string());
    }
    if let (Some(next_depth), Some(next_used)) = (selection.next_depth, selection.next_used_bytes) {
        parts.push(format!(
            "next depth {} would use {}",
            next_depth,
            byte_label(next_used)
        ));
    }
    parts.join(", ")
}

#[allow(clippy::too_many_arguments)]
pub(super) fn cmd_context(
    file: &str,
    module_root_override: Option<&str>,
    output: Option<&str>,
    json: bool,
    decisions_only: bool,
    focus: Option<&str>,
    depth: ContextDepth,
    budget: usize,
) {
    let module_root = resolve_module_root(module_root_override);

    let entry_label = Path::new(file)
        .strip_prefix(&module_root)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| file.to_string());

    let selected = match (focus, depth) {
        (Some(focus), ContextDepth::Auto) => {
            match collect_contexts_focus_auto(
                file,
                &module_root,
                &entry_label,
                json,
                decisions_only,
                budget,
                focus,
            ) {
                Ok(selected) => selected,
                Err(msg) => {
                    eprintln!("{} {}", "Error:".red(), msg);
                    process::exit(1);
                }
            }
        }
        (Some(focus), ContextDepth::Unlimited) => {
            match collect_contexts_focus(file, &module_root, focus, None, "focus-unlimited") {
                Ok(selected) => selected,
                Err(msg) => {
                    eprintln!("{} {}", "Error:".red(), msg);
                    process::exit(1);
                }
            }
        }
        (Some(focus), ContextDepth::Limited(limit)) => {
            match collect_contexts_focus(file, &module_root, focus, Some(limit), "focus-limited") {
                Ok(selected) => selected,
                Err(msg) => {
                    eprintln!("{} {}", "Error:".red(), msg);
                    process::exit(1);
                }
            }
        }
        (None, ContextDepth::Auto) => collect_contexts_auto(
            file,
            &module_root,
            &entry_label,
            json,
            decisions_only,
            budget,
        ),
        (None, ContextDepth::Unlimited) => SelectedContext {
            contexts: collect_contexts_at_depth(file, &module_root, None),
            selection: selection_template("unlimited", None, None, None, None, None, false),
        },
        (None, ContextDepth::Limited(limit)) => SelectedContext {
            contexts: collect_contexts_at_depth(file, &module_root, Some(limit)),
            selection: selection_template("limited", None, Some(limit), None, None, None, false),
        },
    };

    if selected.contexts.is_empty() {
        eprintln!("{}", "No content found.".yellow());
        process::exit(1);
    }

    let (content, selection) = render_with_selection(
        &selected.contexts,
        &entry_label,
        json,
        decisions_only,
        selected.selection,
    );

    match output {
        None => print!("{}", content),
        Some(out_path) => {
            if let Err(e) = fs::write(out_path, &content) {
                eprintln!("{} Cannot write to '{}': {}", "Error:".red(), out_path, e);
                process::exit(1);
            }
            if decisions_only {
                println!("{}", format!("Decisions written to {}", out_path).green());
            } else {
                println!("{}", format!("Context written to {}", out_path).green());
            }
            println!("{}", summarize_selection(&selection));
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use aver::ast::{DecisionBlock, FnBody, FnDef, VerifyBlock, VerifyKind};

    use super::*;

    fn test_fn(name: &str) -> FnDef {
        FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Block(vec![])),
            resolution: None,
        }
    }

    fn test_context(module_name: &str, fns: &[&str]) -> FileContext {
        let fns = fns.iter().map(|name| test_fn(name)).collect::<Vec<_>>();
        FileContext {
            source_file: format!("examples/{module_name}.av"),
            module_name: Some(module_name.to_string()),
            intent: None,
            depends: vec![],
            exposes: vec![],
            api_effects: vec![],
            module_effects: vec![],
            main_effects: None,
            fn_defs: fns.clone(),
            all_fn_defs: fns,
            fn_auto_memo: HashSet::new(),
            fn_memo_qual: HashMap::new(),
            fn_auto_tco: HashSet::new(),
            fn_recursive_callsites: HashMap::new(),
            fn_recursive_scc_id: HashMap::new(),
            fn_specs: HashMap::new(),
            fn_direct_calls: HashMap::new(),
            type_defs: vec![],
            effect_sets: vec![],
            verify_blocks: vec![VerifyBlock {
                fn_name: "entry".to_string(),
                line: 2,
                cases: vec![],
                kind: VerifyKind::Cases,
            }],
            verify_counts: HashMap::new(),
            verify_samples: HashMap::new(),
            decisions: vec![DecisionBlock {
                name: "Decision".to_string(),
                line: 1,
                date: "2026-03-17".to_string(),
                reason: "".to_string(),
                chosen: DecisionImpact::Symbol("entry".to_string()),
                rejected: vec![],
                impacts: vec![DecisionImpact::Symbol("entry".to_string())],
                author: None,
            }],
        }
    }

    #[test]
    fn resolve_focus_symbol_prefers_entry_module_for_ambiguous_bare_name() {
        let contexts = vec![
            test_context("App", &["shared"]),
            test_context("Other", &["shared"]),
        ];
        let catalog = build_focus_catalog(&contexts);
        assert_eq!(
            resolve_focus_symbol(&catalog, "shared").as_deref(),
            Ok("App.shared")
        );
    }

    #[test]
    fn filter_contexts_to_focus_keeps_selected_functions_only() {
        let mut ctx = test_context("App", &["entry", "helper", "unused"]);
        ctx.fn_direct_calls
            .insert("entry".to_string(), vec!["helper".to_string()]);
        let contexts = vec![ctx];
        let catalog = build_focus_catalog(&contexts);

        let filtered = filter_contexts_to_focus(&contexts, &catalog, "App.entry", Some(1));
        assert_eq!(filtered.len(), 1);
        let fn_names = filtered[0]
            .fn_defs
            .iter()
            .map(|fd| fd.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(fn_names, vec!["entry", "helper"]);
    }
}
