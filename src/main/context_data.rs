use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use aver::ast::{DecisionBlock, FnDef, TopLevel, TypeDef, VerifyBlock};
use aver::call_graph::{
    direct_calls, find_recursive_fns, recursive_callsite_counts, recursive_scc_ids,
};
use aver::checker::expr_to_str;
use aver::source::{find_module_file, parse_source, require_module_declaration};
use aver::tco;
use aver::types::checker::run_type_check_full;
use aver::verify_law::canonical_spec_ref;

use crate::shared::{compute_memo_fns, is_memo_safe_type};

#[derive(Clone, PartialEq)]
pub(super) struct FileContext {
    pub(super) source_file: String,
    pub(super) module_name: Option<String>,
    pub(super) intent: Option<String>,
    pub(super) depends: Vec<String>,
    pub(super) exposes: Vec<String>,
    pub(super) exposes_opaque: Vec<String>,
    pub(super) api_effects: Vec<String>,
    pub(super) module_effects: Vec<String>,
    pub(super) main_effects: Option<Vec<String>>,
    pub(super) fn_defs: Vec<FnDef>,
    pub(super) all_fn_defs: Vec<FnDef>,
    pub(super) fn_auto_memo: HashSet<String>,
    pub(super) fn_memo_qual: HashMap<String, Vec<String>>,
    pub(super) fn_auto_tco: HashSet<String>,
    pub(super) fn_recursive_callsites: HashMap<String, usize>,
    pub(super) fn_recursive_scc_id: HashMap<String, usize>,
    pub(super) fn_specs: HashMap<String, Vec<String>>,
    pub(super) fn_direct_calls: HashMap<String, Vec<String>>,
    pub(super) type_defs: Vec<TypeDef>,
    pub(super) verify_blocks: Vec<VerifyBlock>,
    pub(super) verify_counts: HashMap<String, usize>,
    pub(super) verify_samples: HashMap<String, Vec<String>>,
    pub(super) decisions: Vec<DecisionBlock>,
}

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

    // Type-aware classification from fn return type
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

    // Heuristic categories (work for any return type)
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

    // Constructor diversity for named/sum types
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
                rendered: format!("{lhs_text} -> {rhs_text}"),
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
    fn_sigs: &HashMap<String, (Vec<aver::types::Type>, aver::types::Type, Vec<String>)>,
) -> Option<&'static str> {
    let (_, ret, _) = fn_sigs.get(fn_name)?;
    match ret {
        aver::types::Type::Result(_, _) => Some("result"),
        aver::types::Type::Option(_) => Some("option"),
        aver::types::Type::Bool => Some("bool"),
        aver::types::Type::List(_) => Some("list"),
        aver::types::Type::Named(_) => Some("named"),
        _ => None,
    }
}

fn build_verify_summaries(
    verify_blocks: &[VerifyBlock],
    fn_sigs: &HashMap<String, (Vec<aver::types::Type>, aver::types::Type, Vec<String>)>,
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
    fn_sigs: HashMap<String, (Vec<aver::types::Type>, aver::types::Type, Vec<String>)>,
}

fn expr_has_tail_call(expr: &aver::ast::Expr) -> bool {
    use aver::ast::Expr;
    match expr {
        Expr::TailCall(_) => true,
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => false,
        Expr::Attr(obj, _) => expr_has_tail_call(obj),
        Expr::FnCall(f, args) => expr_has_tail_call(f) || args.iter().any(expr_has_tail_call),
        Expr::BinOp(_, l, r) => expr_has_tail_call(l) || expr_has_tail_call(r),
        Expr::Match { subject, arms, .. } => {
            expr_has_tail_call(subject) || arms.iter().any(|arm| expr_has_tail_call(&arm.body))
        }
        Expr::Constructor(_, arg) => arg.as_ref().is_some_and(|a| expr_has_tail_call(a)),
        Expr::ErrorProp(inner) => expr_has_tail_call(inner),
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            aver::ast::StrPart::Literal(_) => false,
            aver::ast::StrPart::Parsed(e) => expr_has_tail_call(e),
        }),
        Expr::List(items) | Expr::Tuple(items) => items.iter().any(expr_has_tail_call),
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
        aver::ast::Stmt::Binding(_, _, expr) | aver::ast::Stmt::Expr(expr) => {
            expr_has_tail_call(expr)
        }
    })
}

fn compute_context_fn_flags(items: &[TopLevel], module_root: &str) -> ContextFnFlags {
    let mut transformed = items.to_vec();
    tco::transform_program(&mut transformed);
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

    let tc_result = run_type_check_full(&transformed, Some(module_root));
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
        auto_memo: compute_memo_fns(&transformed, &tc_result),
        auto_tco: tco_fns,
        memo_qual,
        recursive_callsites,
        recursive_scc_id,
        fn_sigs: tc_result.fn_sigs,
    }
}

pub(super) fn collect_contexts(
    file: &str,
    module_root: &str,
    visited: &mut HashSet<String>,
    max_depth: Option<usize>,
) -> Vec<FileContext> {
    let canonical = std::fs::canonicalize(file)
        .unwrap_or_else(|_| PathBuf::from(file))
        .to_string_lossy()
        .to_string();

    if visited.contains(&canonical) {
        return vec![];
    }
    visited.insert(canonical);

    let source = match fs::read_to_string(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Cannot read '{}': {}", file, e);
            return vec![];
        }
    };

    let items = match parse_source(&source) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("Parse error in '{}': {}", file, e);
            return vec![];
        }
    };
    if let Err(e) = require_module_declaration(&items, file) {
        eprintln!("{}", e);
        return vec![];
    }

    // Strip module_root prefix so paths are relative to the project root
    let relative_file = Path::new(file)
        .strip_prefix(module_root)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| file.to_string());

    let mut ctx = FileContext {
        source_file: relative_file,
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
    };

    let mut dep_names: Vec<String> = vec![];

    for item in &items {
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
                dep_names = m.depends.clone();
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

    let flags = compute_context_fn_flags(&items, module_root);
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
    ctx.fn_direct_calls = direct_calls(&items);
    for vb in &ctx.verify_blocks {
        let aver::ast::VerifyKind::Law(law) = &vb.kind else {
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

    // Effect summaries are calculated from the full function set
    // before the public-function filtering done for display.
    ctx.module_effects = unique_sorted_effects(ctx.fn_defs.iter().flat_map(|fd| fd.effects.iter()));
    ctx.api_effects = unique_sorted_effects(
        ctx.fn_defs
            .iter()
            .filter(|fd| ctx.exposes.contains(&fd.name))
            .flat_map(|fd| fd.effects.iter()),
    );
    ctx.main_effects = ctx
        .fn_defs
        .iter()
        .find(|fd| fd.name == "main")
        .map(|fd| unique_sorted_effects(fd.effects.iter()));

    // Filter functions by exposes if the list is non-empty
    if !ctx.exposes.is_empty() {
        let exposes = ctx.exposes.clone();
        ctx.fn_defs.retain(|fd| exposes.contains(&fd.name));
    }

    let mut result = vec![ctx];

    // Recurse into dependencies (respecting depth limit)
    let should_recurse = match max_depth {
        None => true,
        Some(0) => false,
        Some(_) => true,
    };
    if should_recurse {
        let next_depth = max_depth.map(|d| d.saturating_sub(1));
        for dep_name in dep_names {
            if let Some(dep_path) = find_module_file(&dep_name, module_root) {
                let dep_file = dep_path.to_string_lossy().to_string();
                let mut sub = collect_contexts(&dep_file, module_root, visited, next_depth);
                result.append(&mut sub);
            }
        }
    }

    result
}
