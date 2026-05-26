//! File-local context analysis.
//!
//! `aver context` is inherently multi-file: a project's context is the
//! entry file **and** its dependency graph. This module provides the
//! per-file building block. CLI wraps it with dependency traversal;
//! the playground calls it for a single entry, so the returned
//! [`FileContext`] has `depends` listed by name but no expanded child
//! contexts — the caller is responsible for resolving them (or not).
//!
//! Runtime-neutral: pure computation over parsed items and source text.
//! No filesystem access, no VM, wasm-safe.

use std::collections::{HashMap, HashSet};

use serde::Serialize;

use crate::ast::{DecisionBlock, FnDef, TopLevel, TypeDef, VerifyBlock};
use crate::call_graph::{
    direct_calls, find_recursive_fns, recursive_callsite_counts, recursive_scc_ids,
};
use crate::checker::expr_to_str;
use crate::types::checker::TypeCheckResult;
use crate::verify_law::canonical_spec_ref;

// ─── Canonical, CLI-shaped FileContext ────────────────────────────────────────

#[derive(Clone)]
pub struct FileContext {
    pub source_file: String,
    pub module_name: Option<String>,
    pub intent: Option<String>,
    pub depends: Vec<String>,
    pub exposes: Vec<String>,
    pub exposes_opaque: Vec<String>,
    pub api_effects: Vec<String>,
    pub module_effects: Vec<String>,
    pub main_effects: Option<Vec<String>>,
    /// Public fn defs only (filtered by `exposes` when present).
    pub fn_defs: Vec<FnDef>,
    /// Every fn def, unfiltered. Used by callers that need private fns too.
    pub all_fn_defs: Vec<FnDef>,
    pub fn_auto_memo: HashSet<String>,
    pub fn_memo_qual: HashMap<String, Vec<String>>,
    pub fn_auto_tco: HashSet<String>,
    pub fn_recursive_callsites: HashMap<String, usize>,
    pub fn_recursive_scc_id: HashMap<String, usize>,
    pub fn_specs: HashMap<String, Vec<String>>,
    pub fn_direct_calls: HashMap<String, Vec<String>>,
    pub type_defs: Vec<TypeDef>,
    pub verify_blocks: Vec<VerifyBlock>,
    pub verify_counts: HashMap<String, usize>,
    pub verify_samples: HashMap<String, Vec<String>>,
    pub decisions: Vec<DecisionBlock>,
}

impl FileContext {
    fn empty(source_file: impl Into<String>) -> Self {
        Self {
            source_file: source_file.into(),
            module_name: None,
            intent: None,
            depends: vec![],
            exposes: vec![],
            exposes_opaque: vec![],
            api_effects: vec![],
            module_effects: vec![],
            main_effects: None,
            fn_defs: vec![],
            all_fn_defs: vec![],
            fn_auto_memo: HashSet::new(),
            fn_memo_qual: HashMap::new(),
            fn_auto_tco: HashSet::new(),
            fn_recursive_callsites: HashMap::new(),
            fn_recursive_scc_id: HashMap::new(),
            fn_specs: HashMap::new(),
            fn_direct_calls: HashMap::new(),
            type_defs: vec![],
            verify_blocks: vec![],
            verify_counts: HashMap::new(),
            verify_samples: HashMap::new(),
            decisions: vec![],
        }
    }
}

// ─── Single-file entry point ──────────────────────────────────────────────────

/// Build the per-file context record from parsed items and source text.
///
/// `module_root` is passed to the typechecker so it can resolve external
/// references during signature inference; pass `None` for single-file
/// analysis (playground). Callers that need the dependency graph wrap
/// this with their own recursion.
pub fn build_context_for_items(
    items: &[TopLevel],
    _source: &str,
    file_label: impl Into<String>,
    module_root: Option<&str>,
) -> FileContext {
    let mut ctx = FileContext::empty(file_label);

    let mut declared_module_effects: Option<Vec<String>> = None;
    for item in items {
        match item {
            TopLevel::Module(m) => {
                ctx.module_name = Some(m.name.clone());
                ctx.intent = if m.intent.is_empty() {
                    None
                } else {
                    Some(m.intent.clone())
                };
                ctx.depends = m.depends.clone();
                ctx.exposes = m.exposes.clone();
                ctx.exposes_opaque = m.exposes_opaque.clone();
                declared_module_effects = m.effects.clone();
            }
            TopLevel::FnDef(fd) => {
                ctx.fn_defs.push(fd.clone());
                ctx.all_fn_defs.push(fd.clone());
            }
            TopLevel::TypeDef(td) => ctx.type_defs.push(td.clone()),
            TopLevel::Verify(vb) => ctx.verify_blocks.push(vb.clone()),
            TopLevel::Decision(db) => ctx.decisions.push(db.clone()),
            _ => {}
        }
    }

    let flags = compute_context_fn_flags(items, module_root);
    let ContextFnFlags {
        auto_memo,
        auto_tco,
        memo_qual,
        recursive_callsites,
        recursive_scc_id,
        fn_sigs,
    } = flags;
    ctx.fn_auto_memo = auto_memo;
    ctx.fn_auto_tco = auto_tco;
    ctx.fn_memo_qual = memo_qual;
    ctx.fn_recursive_callsites = recursive_callsites;
    ctx.fn_recursive_scc_id = recursive_scc_id;
    ctx.fn_direct_calls = direct_calls(items);

    for vb in &ctx.verify_blocks {
        let crate::ast::VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        let Some(spec_ref) = canonical_spec_ref(&vb.fn_name, law, &fn_sigs) else {
            continue;
        };
        ctx.fn_specs
            .entry(vb.fn_name.clone())
            .or_default()
            .push(spec_ref.spec_fn_name);
    }
    for specs in ctx.fn_specs.values_mut() {
        specs.sort();
        specs.dedup();
    }

    let (verify_counts, verify_samples) = build_verify_summaries(&ctx.verify_blocks, &fn_sigs);
    ctx.verify_counts = verify_counts;
    ctx.verify_samples = verify_samples;

    // Prefer the module-level `effects [...]` declaration when the
    // user spelled it out — it's the source-of-truth boundary, and
    // may use namespace shorthand (`Disk` covers `Disk.*`) that the
    // per-fn aggregation can't recover. Fall back to walking every
    // fn's `! [...]` only for legacy (0.12-style) modules that omit
    // the boundary.
    ctx.module_effects = if let Some(declared) = declared_module_effects {
        unique_sorted_effects(declared.iter())
    } else {
        unique_sorted_effects(
            ctx.fn_defs
                .iter()
                .flat_map(|fd| fd.effects.iter().map(|e| &e.node)),
        )
    };
    ctx.api_effects = unique_sorted_effects(
        ctx.fn_defs
            .iter()
            .filter(|fd| ctx.exposes.contains(&fd.name))
            .flat_map(|fd| fd.effects.iter().map(|e| &e.node)),
    );
    ctx.main_effects = ctx
        .fn_defs
        .iter()
        .find(|fd| fd.name == "main")
        .map(|fd| unique_sorted_effects(fd.effects.iter().map(|e| &e.node)));

    if !ctx.exposes.is_empty() {
        let exposes = ctx.exposes.clone();
        ctx.fn_defs.retain(|fd| exposes.contains(&fd.name));
    }

    ctx
}

// ─── Serializable projection for playground / LSP ────────────────────────────

#[derive(Clone, Debug, Serialize)]
pub struct ContextSummary {
    pub file_label: String,
    pub module_name: Option<String>,
    pub intent: Option<String>,
    pub depends: Vec<String>,
    pub exposes: Vec<String>,
    pub exposes_opaque: Vec<String>,
    pub api_effects: Vec<String>,
    pub module_effects: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub main_effects: Option<Vec<String>>,
    pub functions: Vec<ContextFnSummary>,
    pub types: Vec<ContextTypeSummary>,
    pub decisions: Vec<ContextDecisionSummary>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ContextFnSummary {
    pub name: String,
    pub signature: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub effects: Vec<String>,
    pub qualifiers: Vec<String>,
    pub auto_memo: bool,
    pub auto_tco: bool,
    pub recursive_callsites: usize,
    pub verify_count: usize,
    pub verify_samples: Vec<String>,
    pub is_exposed: bool,
    pub specs: Vec<String>,
    pub direct_calls: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ContextTypeSummary {
    pub name: String,
    pub kind: &'static str,
    pub fields_or_variants: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ContextDecisionSummary {
    pub name: String,
    pub date: String,
    pub reason_prefix: String,
    pub impacts: Vec<String>,
}

/// Project a full [`FileContext`] into a serde-serializable summary.
///
/// The summary is lossy (signatures rendered as strings, private fns
/// dropped, type bodies reduced to field/variant names). It's designed
/// for JSON emission — playground Context panel, future LSP metadata.
pub fn summarize(ctx: &FileContext) -> ContextSummary {
    let functions = ctx
        .all_fn_defs
        .iter()
        .map(|fd| {
            let effects: Vec<String> = fd.effects.iter().map(|e| e.node.clone()).collect();
            let qualifiers = ctx.fn_memo_qual.get(&fd.name).cloned().unwrap_or_default();
            let description = fd.desc.clone();
            let signature = render_signature(fd);
            let is_exposed = ctx.exposes.is_empty() || ctx.exposes.contains(&fd.name);
            let specs = ctx.fn_specs.get(&fd.name).cloned().unwrap_or_default();
            let direct = ctx
                .fn_direct_calls
                .get(&fd.name)
                .cloned()
                .unwrap_or_default();
            ContextFnSummary {
                name: fd.name.clone(),
                signature,
                description,
                effects,
                qualifiers,
                auto_memo: ctx.fn_auto_memo.contains(&fd.name),
                auto_tco: ctx.fn_auto_tco.contains(&fd.name),
                recursive_callsites: ctx
                    .fn_recursive_callsites
                    .get(&fd.name)
                    .copied()
                    .unwrap_or(0),
                verify_count: ctx.verify_counts.get(&fd.name).copied().unwrap_or(0),
                verify_samples: ctx
                    .verify_samples
                    .get(&fd.name)
                    .cloned()
                    .unwrap_or_default(),
                is_exposed,
                specs,
                direct_calls: direct,
            }
        })
        .collect();

    let types = ctx
        .type_defs
        .iter()
        .map(|td| match td {
            TypeDef::Sum { name, variants, .. } => ContextTypeSummary {
                name: name.clone(),
                kind: "sum",
                fields_or_variants: variants.iter().map(|v| v.name.clone()).collect(),
            },
            TypeDef::Product { name, fields, .. } => ContextTypeSummary {
                name: name.clone(),
                kind: "product",
                fields_or_variants: fields.iter().map(|(n, _)| n.clone()).collect(),
            },
        })
        .collect();

    let decisions = ctx
        .decisions
        .iter()
        .map(|d| {
            let reason_prefix: String = d.reason.chars().take(60).collect();
            let reason_prefix = if d.reason.len() > 60 {
                format!("{}...", reason_prefix.trim_end())
            } else {
                reason_prefix
            };
            ContextDecisionSummary {
                name: d.name.clone(),
                date: d.date.clone(),
                reason_prefix,
                impacts: d
                    .impacts
                    .iter()
                    .map(|i| i.node.text().to_string())
                    .collect(),
            }
        })
        .collect();

    ContextSummary {
        file_label: ctx.source_file.clone(),
        module_name: ctx.module_name.clone(),
        intent: ctx.intent.clone(),
        depends: ctx.depends.clone(),
        exposes: ctx.exposes.clone(),
        exposes_opaque: ctx.exposes_opaque.clone(),
        api_effects: ctx.api_effects.clone(),
        module_effects: ctx.module_effects.clone(),
        main_effects: ctx.main_effects.clone(),
        functions,
        types,
        decisions,
    }
}

/// Render a [`ContextSummary`] as markdown — same shape as the CLI's
/// `aver context --md` for a single-entry project. Playground uses
/// this for the ⬇ Download as .md button so the browser and the CLI
/// emit identical files for identical source.
pub fn render_context_md(summary: &ContextSummary) -> String {
    let mut out = String::new();
    out.push_str(&format!("# Aver Context — {}\n\n", summary.file_label));
    out.push_str("_Generated by `aver context`_\n\n");

    out.push_str("---\n\n");
    if let Some(name) = &summary.module_name {
        out.push_str(&format!("## Module: {}\n\n", name));
    } else {
        out.push_str(&format!("## {}\n\n", summary.file_label));
    }

    if let Some(intent) = &summary.intent {
        out.push_str(&format!("> {}\n\n", intent));
    }

    if !summary.depends.is_empty() {
        out.push_str(&format!("depends: `[{}]`  \n", summary.depends.join(", ")));
    }
    if !summary.exposes.is_empty() {
        out.push_str(&format!("exposes: `[{}]`  \n", summary.exposes.join(", ")));
    }
    if !summary.exposes_opaque.is_empty() {
        out.push_str(&format!(
            "exposes opaque: `[{}]`  \n",
            summary.exposes_opaque.join(", ")
        ));
    }

    // Collapse api/module effects when equal, mirror CLI output.
    if effects_equal(&summary.api_effects, &summary.module_effects) {
        if !summary.module_effects.is_empty() {
            out.push_str(&format!(
                "effects: `[{}]`\n",
                summary.module_effects.join(", ")
            ));
        }
    } else {
        out.push_str(&format!(
            "api_effects: `[{}]`  \nmodule_effects: `[{}]`\n",
            summary.api_effects.join(", "),
            summary.module_effects.join(", ")
        ));
    }
    if let Some(main_effects) = &summary.main_effects {
        out.push_str(&format!("main_effects: `[{}]`\n", main_effects.join(", ")));
    }
    out.push('\n');

    for ty in &summary.types {
        let header = match ty.kind {
            "record" => format!("### record {}\n", ty.name),
            _ => format!("### type {}\n", ty.name),
        };
        out.push_str(&header);
        if !ty.fields_or_variants.is_empty() {
            out.push_str(&format!("`{}`\n\n", ty.fields_or_variants.join("` | `")));
        } else {
            out.push('\n');
        }
    }

    for fd in &summary.functions {
        if fd.name == "main" {
            continue;
        }
        out.push_str(&format!("### `{}`\n", fd.signature));
        if !fd.effects.is_empty() {
            out.push_str(&format!("effects: `[{}]`  \n", fd.effects.join(", ")));
        }
        if fd.auto_memo {
            out.push_str("memo: `true`  \n");
        }
        if fd.auto_tco {
            out.push_str("tco: `true`  \n");
        }
        if !fd.specs.is_empty() {
            let label = if fd.specs.len() == 1 { "spec" } else { "specs" };
            out.push_str(&format!("{}: `[{}]`  \n", label, fd.specs.join(", ")));
        }
        if !fd.is_exposed {
            out.push_str("visibility: `private`  \n");
        }
        if let Some(desc) = &fd.description {
            out.push_str(&format!("> {}\n", desc));
        }
        if !fd.verify_samples.is_empty() {
            let samples: Vec<String> = fd
                .verify_samples
                .iter()
                .map(|s| format!("`{}`", s))
                .collect();
            out.push_str(&format!("verify: {}\n", samples.join(", ")));
        }
        out.push('\n');
    }

    if !summary.decisions.is_empty() {
        out.push_str("---\n\n## Decisions\n\n");
        for dec in &summary.decisions {
            out.push_str(&format!("### {} ({})\n", dec.name, dec.date));
            if !dec.reason_prefix.is_empty() {
                out.push_str(&format!("> {}\n", dec.reason_prefix));
            }
            if !dec.impacts.is_empty() {
                out.push_str(&format!("impacts: `{}`\n", dec.impacts.join("`, `")));
            }
            out.push('\n');
        }
    }

    out
}

fn effects_equal(a: &[String], b: &[String]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut a = a.to_vec();
    let mut b = b.to_vec();
    a.sort();
    b.sort();
    a == b
}

fn render_signature(fd: &FnDef) -> String {
    // Type-only signature: params + return type. Effects live on the
    // separate `effects: [...]` JSON field so renderers (playground,
    // LLMs) can show them alongside without duplicating on screen.
    let params = fd
        .params
        .iter()
        .map(|(name, type_str)| format!("{}: {}", name, type_str))
        .collect::<Vec<_>>()
        .join(", ");
    format!("fn {}({}) -> {}", fd.name, params, fd.return_type)
}

// ─── Pure helpers exposed to CLI / playground ────────────────────────────────

/// Functions that qualify for auto-memoization: pure, recursive with
/// branching, and all parameter types memo-safe.
///
/// Reads recursion facts from `PipelineResult.analysis` when the pipeline
/// has run; falls back to ad-hoc computation when the analysis isn't
/// available (callers that haven't migrated to the pipeline). The fallback
/// path will go away once every consumer reads from analysis directly.
pub fn compute_memo_fns(
    items: &[TopLevel],
    tc_result: &TypeCheckResult,
    analysis: Option<&crate::ir::AnalysisResult>,
) -> HashSet<String> {
    let mut memo = HashSet::new();

    let memo_check = |fn_name: &str, recursive_calls: usize| -> bool {
        let Some((params, _ret, effects)) = tc_result.fn_sigs.get(fn_name) else {
            return false;
        };
        if !effects.is_empty() {
            return false;
        }
        if recursive_calls < 2 {
            return false;
        }
        params
            .iter()
            .all(|ty| is_memo_safe_type(ty, &tc_result.memo_safe_types))
    };

    if let Some(analysis) = analysis {
        for fn_name in &analysis.recursive_fns {
            let calls = analysis
                .fn_analyses
                .get(fn_name)
                .map(|a| a.recursive_call_count)
                .unwrap_or(0);
            if memo_check(fn_name, calls) {
                memo.insert(fn_name.clone());
            }
        }
    } else {
        let recursive = find_recursive_fns(items);
        let recursive_calls = recursive_callsite_counts(items);
        for fn_name in &recursive {
            let calls = recursive_calls.get(fn_name).copied().unwrap_or(0);
            if memo_check(fn_name, calls) {
                memo.insert(fn_name.clone());
            }
        }
    }

    memo
}

pub fn is_memo_safe_type(ty: &crate::types::Type, safe_named: &HashSet<String>) -> bool {
    use crate::types::Type;
    match ty {
        Type::Int | Type::Float | Type::Bool | Type::Unit => true,
        Type::Str => false,
        Type::Tuple(items) => items.iter().all(|item| is_memo_safe_type(item, safe_named)),
        Type::List(_)
        | Type::Vector(_)
        | Type::Map(_, _)
        | Type::Fn(_, _, _)
        | Type::Invalid
        | Type::Var(_) => false,
        Type::Result(_, _) | Type::Option(_) => false,
        Type::Named { name, .. } => safe_named.contains(name),
    }
}

// ─── Internal helpers ────────────────────────────────────────────────────────

const VERIFY_SAMPLE_LIMIT: usize = 3;
const VERIFY_CASE_MAX_LEN: usize = 150;

fn unique_sorted_effects<'a, I>(effects: I) -> Vec<String>
where
    I: Iterator<Item = &'a String>,
{
    let mut uniq = effects
        .cloned()
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    uniq.sort();
    uniq
}

fn classify_verify_case(lhs: &str, rhs: &str, ret_category: Option<&str>) -> Vec<String> {
    let combined = format!("{lhs} -> {rhs}");
    let mut categories = Vec::new();

    match ret_category {
        Some("result") => {
            if rhs.contains("Result.Ok(") || rhs.contains("Ok(") {
                categories.push("ok".to_string());
            }
            if rhs.contains("Result.Err(") || rhs.contains("Err(") {
                categories.push("err".to_string());
            }
        }
        Some("option") => {
            if rhs.contains("Option.Some(") || rhs.contains("Some(") {
                categories.push("some".to_string());
            }
            if rhs.contains("Option.None") || rhs == "None" {
                categories.push("none".to_string());
            }
        }
        Some("bool") => {
            if rhs == "true" {
                categories.push("true".to_string());
            }
            if rhs == "false" {
                categories.push("false".to_string());
            }
        }
        _ => {}
    }

    if combined.contains("[]") || combined.contains("{}") {
        categories.push("empty".to_string());
    }
    if combined.contains("-1") || combined.contains("(0 - ") {
        categories.push("negative".to_string());
    }
    if combined.contains("(0)") || rhs == "0" {
        categories.push("zero".to_string());
    }
    if combined.contains("\"\"") {
        categories.push("empty-string".to_string());
    }

    if ret_category == Some("named")
        && let Some(dot_pos) = rhs.find('.')
    {
        let after_dot = &rhs[dot_pos + 1..];
        let ctor = after_dot.split('(').next().unwrap_or(after_dot);
        categories.push(format!("ctor:{ctor}"));
    }

    categories.sort();
    categories.dedup();
    categories
}

fn base_verify_case_score(lhs: &str, rhs: &str) -> i32 {
    let combined_len = lhs.len() + rhs.len();
    let mut score = 400 - combined_len as i32;
    let combined = format!("{lhs} -> {rhs}");
    if rhs.contains("Result.Err(")
        || rhs.contains("ParseResult.Err(")
        || rhs.contains("Option.None")
    {
        score += 120;
    }
    if combined.contains("[]") || combined.contains("{}") {
        score += 60;
    }
    if combined.contains("\"\"") {
        score += 45;
    }
    if combined.contains("-1") || combined.contains("(0 - ") {
        score += 45;
    }
    if combined.contains(", 0") || combined.contains("(0)") || rhs == "0" {
        score += 30;
    }
    if rhs == "true" || rhs == "false" {
        score += 20;
    }
    score
}

fn scored_verify_samples(cases: &[(String, String)], ret_category: Option<&str>) -> Vec<String> {
    #[derive(Clone)]
    struct ScoredVerifyCase {
        rendered: String,
        base_score: i32,
        categories: Vec<String>,
        original_index: usize,
    }

    let mut scored = cases
        .iter()
        .enumerate()
        .filter_map(|(original_index, (lhs_text, rhs_text))| {
            if lhs_text.len() + rhs_text.len() > VERIFY_CASE_MAX_LEN {
                return None;
            }
            Some(ScoredVerifyCase {
                rendered: format!("{lhs_text} => {rhs_text}"),
                base_score: base_verify_case_score(lhs_text, rhs_text),
                categories: classify_verify_case(lhs_text, rhs_text, ret_category),
                original_index,
            })
        })
        .collect::<Vec<_>>();

    let mut selected = Vec::new();
    let mut seen_categories: HashSet<String> = HashSet::new();
    while selected.len() < VERIFY_SAMPLE_LIMIT && !scored.is_empty() {
        let best_idx = scored
            .iter()
            .enumerate()
            .max_by_key(|(_, case)| {
                let novelty = case
                    .categories
                    .iter()
                    .filter(|cat| !seen_categories.contains(cat.as_str()))
                    .count() as i32;
                (
                    case.base_score + novelty * 35,
                    case.base_score,
                    -(case.original_index as i32),
                )
            })
            .map(|(idx, _)| idx)
            .expect("verify samples should be non-empty");
        let chosen = scored.swap_remove(best_idx);
        for category in &chosen.categories {
            seen_categories.insert(category.clone());
        }
        selected.push(chosen.rendered);
    }
    selected
}

fn return_type_category(
    fn_name: &str,
    fn_sigs: &HashMap<String, (Vec<crate::types::Type>, crate::types::Type, Vec<String>)>,
) -> Option<&'static str> {
    let (_, ret, _) = fn_sigs.get(fn_name)?;
    match ret {
        crate::types::Type::Result(_, _) => Some("result"),
        crate::types::Type::Option(_) => Some("option"),
        crate::types::Type::Bool => Some("bool"),
        crate::types::Type::List(_) => Some("list"),
        crate::types::Type::Named { .. } => Some("named"),
        _ => None,
    }
}

fn build_verify_summaries(
    verify_blocks: &[VerifyBlock],
    fn_sigs: &HashMap<String, (Vec<crate::types::Type>, crate::types::Type, Vec<String>)>,
) -> (HashMap<String, usize>, HashMap<String, Vec<String>>) {
    let mut cases_by_fn: HashMap<String, Vec<(String, String)>> = HashMap::new();
    for block in verify_blocks {
        let entry = cases_by_fn.entry(block.fn_name.clone()).or_default();
        for (lhs, rhs) in &block.cases {
            entry.push((expr_to_str(lhs), expr_to_str(rhs)));
        }
    }

    let verify_counts = cases_by_fn
        .iter()
        .map(|(fn_name, cases)| (fn_name.clone(), cases.len()))
        .collect::<HashMap<_, _>>();
    let verify_samples = cases_by_fn
        .into_iter()
        .map(|(fn_name, cases)| {
            let ret_cat = return_type_category(&fn_name, fn_sigs);
            (fn_name, scored_verify_samples(&cases, ret_cat))
        })
        .collect::<HashMap<_, _>>();

    (verify_counts, verify_samples)
}

struct ContextFnFlags {
    auto_memo: HashSet<String>,
    auto_tco: HashSet<String>,
    memo_qual: HashMap<String, Vec<String>>,
    recursive_callsites: HashMap<String, usize>,
    recursive_scc_id: HashMap<String, usize>,
    fn_sigs: HashMap<String, (Vec<crate::types::Type>, crate::types::Type, Vec<String>)>,
}

fn expr_has_tail_call(expr: &crate::ast::Spanned<crate::ast::Expr>) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::TailCall(_) => true,
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => false,
        Expr::Attr(obj, _) => expr_has_tail_call(obj),
        Expr::FnCall(f, args) => expr_has_tail_call(f) || args.iter().any(expr_has_tail_call),
        Expr::BinOp(_, l, r) => expr_has_tail_call(l) || expr_has_tail_call(r),
        Expr::Neg(inner) => expr_has_tail_call(inner),
        Expr::Match { subject, arms, .. } => {
            expr_has_tail_call(subject) || arms.iter().any(|arm| expr_has_tail_call(&arm.body))
        }
        Expr::Constructor(_, arg) => arg.as_ref().is_some_and(|a| expr_has_tail_call(a)),
        Expr::ErrorProp(inner) => expr_has_tail_call(inner),
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            crate::ast::StrPart::Literal(_) => false,
            crate::ast::StrPart::Parsed(e) => expr_has_tail_call(e),
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(expr_has_tail_call)
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(k, v)| expr_has_tail_call(k) || expr_has_tail_call(v)),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_has_tail_call(e)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_has_tail_call(base) || updates.iter().any(|(_, e)| expr_has_tail_call(e))
        }
    }
}

fn fn_has_tail_call(fd: &FnDef) -> bool {
    fd.body.stmts().iter().any(|stmt| match stmt {
        crate::ast::Stmt::Binding(_, _, expr) | crate::ast::Stmt::Expr(expr) => {
            expr_has_tail_call(expr)
        }
    })
}

fn compute_context_fn_flags(items: &[TopLevel], module_root: Option<&str>) -> ContextFnFlags {
    let mut transformed = items.to_vec();
    crate::ir::pipeline::tco(&mut transformed);
    let tco_fns = transformed
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) if fn_has_tail_call(fd) => Some(fd.name.clone()),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let recursive = find_recursive_fns(&transformed);
    let recursive_callsites = recursive_callsite_counts(&transformed);
    let recursive_scc_id = recursive_scc_ids(&transformed);
    let mut memo_qual = HashMap::new();

    let tc_result = crate::ir::pipeline::typecheck(
        &transformed,
        &crate::ir::TypecheckMode::Full {
            base_dir: module_root,
        },
    );
    if !tc_result.errors.is_empty() {
        for item in &transformed {
            if let TopLevel::FnDef(fd) = item {
                let mut qual = Vec::new();
                if fd.effects.is_empty() {
                    qual.push("PURE".to_string());
                }
                if recursive.contains(&fd.name) {
                    qual.push("RECURSIVE".to_string());
                }
                memo_qual.insert(fd.name.clone(), qual);
            }
        }
        return ContextFnFlags {
            auto_memo: HashSet::new(),
            auto_tco: tco_fns,
            memo_qual,
            recursive_callsites,
            recursive_scc_id,
            fn_sigs: tc_result.fn_sigs,
        };
    }

    for item in &transformed {
        if let TopLevel::FnDef(fd) = item {
            let mut qual = Vec::new();
            if let Some((params, _ret, effects)) = tc_result.fn_sigs.get(&fd.name) {
                if effects.is_empty() {
                    qual.push("PURE".to_string());
                }
                if recursive.contains(&fd.name) {
                    qual.push("RECURSIVE".to_string());
                }
                let safe_args = params
                    .iter()
                    .all(|ty| is_memo_safe_type(ty, &tc_result.memo_safe_types));
                if safe_args {
                    qual.push("SAFE_ARGS".to_string());
                }
            }
            memo_qual.insert(fd.name.clone(), qual);
        }
    }

    ContextFnFlags {
        // `aver context` doesn't run the full pipeline (its IR shape is
        // pre-resolve/pre-analyze for diagnostic display), so pass `None`
        // and let the fallback path compute recursion facts ad-hoc.
        auto_memo: compute_memo_fns(&transformed, &tc_result, None),
        auto_tco: tco_fns,
        memo_qual,
        recursive_callsites,
        recursive_scc_id,
        fn_sigs: tc_result.fn_sigs,
    }
}
