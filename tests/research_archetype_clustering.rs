//! Issue #104 research: do Aver constraint-forced function archetypes
//! actually cluster as predicted?
//!
//! Walks every `.av` under `examples/`, `projects/`, and `self_hosted/`,
//! runs each through the real Aver pipeline (parse → typecheck →
//! name_resolve), then classifies every `ResolvedFnDef` against the
//! candidate archetypes from the issue + the comment thread.
//!
//! AST-aware: matches on `ResolvedExpr` variants (`Match`, `Call`,
//! `ErrorProp`, `TailCall`, `Ctor`, …) instead of pattern-matching
//! source text. Same data the codegen backends see.
//!
//! Not gated as a pass/fail check — research, not regression. Run with:
//!
//!     cargo test --test research_archetype_clustering --release -- \
//!         --nocapture --ignored
//!
//! Emits a multi-label classification per fn, then aggregate counts:
//! primary archetype distribution, any-label co-occurrence, per-folder
//! breakdown.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use aver::ast::{Spanned, TopLevel};
use aver::ir::hir::{
    ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedFnDef, ResolvedMatchArm,
    ResolvedPattern, ResolvedStmt, ResolvedTopLevel,
};
use aver::ir::{FnId, PipelineConfig, TypecheckMode};
use aver::types::Type;

// ── Dependency loader (private copy — bench/runner.rs and
// wasm_gc_verify.rs already have their own; TODO extract shared util)
// ──────────────────────────────────────────────────────────────────────

fn load_compile_deps(
    items: &[TopLevel],
    module_root: &str,
) -> Result<Vec<aver::codegen::ModuleInfo>, String> {
    let module = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => Some(m),
        _ => None,
    });
    let Some(module) = module else {
        return Ok(vec![]);
    };
    let mut result = Vec::new();
    let mut loaded = HashSet::new();
    for dep_name in &module.depends {
        load_module_recursive(dep_name, module_root, &mut result, &mut loaded)?;
    }
    Ok(result)
}

fn load_module_recursive(
    name: &str,
    module_root: &str,
    result: &mut Vec<aver::codegen::ModuleInfo>,
    loaded: &mut HashSet<String>,
) -> Result<(), String> {
    if !loaded.insert(name.to_string()) {
        return Ok(());
    }
    let path = aver::source::find_module_file(name, module_root).ok_or_else(|| {
        format!(
            "Cannot find module '{}' in module root '{}'",
            name, module_root
        )
    })?;
    let source = std::fs::read_to_string(&path)
        .map_err(|e| format!("Read '{}': {}", path.display(), e))?;
    let mut items = aver::source::parse_source(&source)
        .map_err(|e| format!("Parse '{}': {}", path.display(), e))?;
    aver::source::require_module_declaration(&items, path.to_str().unwrap_or(name))?;

    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let pipeline_result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(module_root),
            }),
            run_interp_lower: false,
            run_buffer_build: false,
            alloc_policy: Some(&neutral_policy),
            ..Default::default()
        },
    );
    if let Some(tc) = pipeline_result.typecheck.as_ref()
        && !tc.errors.is_empty()
    {
        return Err(format!(
            "Type errors in dep '{}'",
            name
        ));
    }

    let transitive: Vec<String> = items
        .iter()
        .find_map(|i| match i {
            TopLevel::Module(m) => Some(m.depends.clone()),
            _ => None,
        })
        .unwrap_or_default();
    for dep in &transitive {
        load_module_recursive(dep, module_root, result, loaded)?;
    }

    let depends = transitive;
    let type_defs: Vec<_> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::TypeDef(td) => Some(td.clone()),
            _ => None,
        })
        .collect();
    let fn_defs: Vec<_> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name != "main" => Some(fd.clone()),
            _ => None,
        })
        .collect();
    result.push(aver::codegen::ModuleInfo {
        prefix: name.to_string(),
        depends,
        type_defs,
        fn_defs,
        analysis: pipeline_result.analysis,
    });
    Ok(())
}

// ── Facts extracted by walking ResolvedExpr ───────────────────────────

#[derive(Debug, Default)]
struct Facts {
    calls_to: HashSet<FnId>,
    tail_calls: HashSet<FnId>,
    builtin_calls: Vec<String>,
    ctor_constructs: Vec<ResolvedCtor>,
    ctor_match_patterns: usize,
    other_match_patterns: usize,
    has_match: bool,
    has_error_prop: bool,
    record_creates: usize,
    last_stmt_is_match: bool,
    only_stmt_is_literal: bool,
    body_stmt_count: usize,
    has_interp_str: bool,
    string_builtin_calls: usize,
    has_match_with_err_arm: bool,
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
                if let aver::ir::hir::ResolvedStrPart::Parsed(e) = p {
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
                ResolvedCtor::Builtin(aver::ir::hir::BuiltinCtor::ResultErr)
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

fn extract_facts(fd: &ResolvedFnDef) -> Facts {
    let mut facts = Facts::default();
    let stmts = fd.body.stmts();
    facts.body_stmt_count = stmts.len();
    for stmt in stmts {
        match stmt {
            ResolvedStmt::Binding { value, .. } => walk_expr(&value.node, &mut facts),
            ResolvedStmt::Expr(value) => walk_expr(&value.node, &mut facts),
        }
    }
    // Last stmt: is it a top-level Match expr? (diagnostic for match-dispatcher)
    if let Some(last) = stmts.last() {
        let expr = match last {
            ResolvedStmt::Binding { value, .. } => &value.node,
            ResolvedStmt::Expr(value) => &value.node,
        };
        facts.last_stmt_is_match = matches!(expr, ResolvedExpr::Match { .. });
    }
    // Single-stmt body of pure literal/list/record_create (data-as-function)
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

// ── Classification ────────────────────────────────────────────────────

fn classify(fd: &ResolvedFnDef, facts: &Facts, scc: &HashSet<FnId>) -> Vec<&'static str> {
    let mut labels = Vec::new();

    let self_call = facts.calls_to.contains(&fd.fn_id) || facts.tail_calls.contains(&fd.fn_id);
    if self_call {
        labels.push("structural-recursion");
    }
    if scc.contains(&fd.fn_id) {
        labels.push("scc-mutual");
    }

    if facts.last_stmt_is_match {
        if facts.ctor_match_patterns >= facts.other_match_patterns && facts.ctor_match_patterns > 0
        {
            labels.push("match-dispatcher");
        } else {
            labels.push("match-on-value");
        }
    }

    if !fd.effects.is_empty() {
        let total_calls = facts.calls_to.len() + facts.builtin_calls.len();
        if total_calls >= 2 {
            labels.push("orchestration");
        } else {
            labels.push("effectful-leaf");
        }
    }

    let is_result_ret = type_is_result(&fd.return_type);
    if is_result_ret && facts.has_error_prop {
        labels.push("pipeline-result");
    } else if is_result_ret && facts.has_match_with_err_arm {
        // Result-returning fn that branches on `Result.Err` manually
        // instead of using `?` propagation — usually to customize the
        // error or convert domain failures (the "manual-result-adapter"
        // shape from the issue comment).
        labels.push("manual-result-adapter");
    }

    // Renderer / formatter: returns String, multi-stmt body, uses
    // interpolated strings or String.* builtins. Common in `show`,
    // `render`, `repr`, `format`, `json`, `label` fns.
    let is_string_ret = matches!(&fd.return_type, Type::Named { name, .. } if name == "String");
    if is_string_ret
        && fd.effects.is_empty()
        && (facts.has_interp_str || facts.string_builtin_calls >= 1)
        && (facts.body_stmt_count >= 2 || facts.has_interp_str)
    {
        labels.push("renderer-formatter");
    }

    // Constructor wrapper: 1-stmt body that's just a Ctor/RecordCreate
    if facts.body_stmt_count == 1
        && (!facts.ctor_constructs.is_empty() || facts.record_creates > 0)
        && !facts.has_match
    {
        labels.push("constructor-wrapper");
    }

    // Data-as-function: 0 params, body is pure constant
    if fd.params.is_empty()
        && facts.calls_to.is_empty()
        && facts.builtin_calls.is_empty()
        && fd.effects.is_empty()
        && facts.only_stmt_is_literal
    {
        labels.push("data-as-function");
    }

    // Trivial helper: 1 stmt, no match, no recursion, no effects, has calls
    if facts.body_stmt_count == 1
        && !facts.has_match
        && !self_call
        && fd.effects.is_empty()
        && (!facts.builtin_calls.is_empty() || !facts.calls_to.is_empty())
    {
        labels.push("trivial-helper");
    }

    // Pure expression: 1 stmt, no match, no recursion, no effects, no calls
    if facts.body_stmt_count == 1
        && !facts.has_match
        && !self_call
        && fd.effects.is_empty()
        && facts.builtin_calls.is_empty()
        && facts.calls_to.is_empty()
        && facts.ctor_constructs.is_empty()
        && !fd.params.is_empty()
    {
        labels.push("pure-expression");
    }

    // Let-pipeline: multi-stmt body building a value through a chain of
    // let bindings, no top-level match, no effects, has fn/builtin
    // calls. Aver's stand-in for "what would be a do-block / sequence
    // of statements elsewhere" — common in domain functions that
    // sequence several pure transformations (`format`, `parseIntSlice`,
    // `calculateTotal`, `evalScanCell`, `applyJump`).
    if facts.body_stmt_count >= 2
        && !facts.last_stmt_is_match
        && fd.effects.is_empty()
        && (!facts.builtin_calls.is_empty() || !facts.calls_to.is_empty())
        && !self_call
    {
        labels.push("let-pipeline");
    }

    labels
}

fn type_is_result(t: &Type) -> bool {
    matches!(t, Type::Named { name, .. } if name == "Result")
}

fn primary_label(labels: &[&str]) -> &'static str {
    const ORDER: &[&str] = &[
        "scc-mutual",
        "structural-recursion",
        "match-dispatcher",
        "pipeline-result",
        "manual-result-adapter",
        "renderer-formatter",
        "match-on-value",
        "orchestration",
        "effectful-leaf",
        "let-pipeline",
        "constructor-wrapper",
        "data-as-function",
        "trivial-helper",
        "pure-expression",
    ];
    for &want in ORDER {
        if labels.iter().any(|l| *l == want) {
            return want;
        }
    }
    "unclassified"
}

// ── SCC over the local module's call graph ────────────────────────────

fn compute_sccs(fns: &[&ResolvedFnDef], facts_by_id: &HashMap<FnId, &Facts>) -> HashSet<FnId> {
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
                    strongconnect(w, graph, index, stack, on_stack, indices, lowlinks, multi_scc);
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

// ── Per-file driver ───────────────────────────────────────────────────

struct FileResult {
    file: String,
    folder: String,
    fns: Vec<FnRecord>,
}

struct FnRecord {
    name: String,
    fn_id: FnId,
    param_count: usize,
    verify_count: usize,
    primary: &'static str,
    labels: Vec<&'static str>,
    calls_to: HashSet<FnId>,
    /// All param types — for accumulator semantic-significance check
    /// in the seeded-driver pair analysis.
    param_types: Vec<Type>,
}

fn analyze_file(path: &Path) -> Result<FileResult, String> {
    let source = std::fs::read_to_string(path).map_err(|e| format!("read: {}", e))?;
    let mut items = aver::source::parse_source(&source).map_err(|e| format!("parse: {}", e))?;

    // Escalate module_root upward — many examples/projects use dotted
    // module names (`App.Parse`, `Modules.Pricing.Discount`) where the
    // resolver expects a root several directories above the entry file.
    // Try entry.parent() first, then walk up until either find_module_file
    // resolves every declared dep or we hit the repo root.
    let module_root = resolve_module_root(path, &items).ok_or_else(|| {
        let dep_names: Vec<String> = items
            .iter()
            .find_map(|i| match i {
                TopLevel::Module(m) => Some(m.depends.clone()),
                _ => None,
            })
            .unwrap_or_default();
        format!(
            "deps: cannot locate module root for {:?}",
            dep_names
        )
    })?;

    let dep_modules = load_compile_deps(&items, &module_root)
        .map_err(|e| format!("deps: {}", e))?;
    let pipeline_result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            dep_modules: &dep_modules,
            ..Default::default()
        },
    );
    if let Some(tc) = pipeline_result.typecheck.as_ref()
        && !tc.errors.is_empty()
    {
        return Err("typecheck errors".to_string());
    }

    let resolved_fns: Vec<&ResolvedFnDef> = pipeline_result
        .resolved_items
        .iter()
        .filter_map(|t| match t {
            ResolvedTopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();

    // Two-pass: facts first (need all for SCC graph), then classify.
    let mut facts_by_id: HashMap<FnId, Facts> = HashMap::new();
    for fd in &resolved_fns {
        facts_by_id.insert(fd.fn_id, extract_facts(fd));
    }
    let facts_refs: HashMap<FnId, &Facts> = facts_by_id.iter().map(|(k, v)| (*k, v)).collect();
    let scc = compute_sccs(&resolved_fns, &facts_refs);

    let mut fns_out = Vec::new();
    for fd in &resolved_fns {
        let facts = &facts_by_id[&fd.fn_id];
        let labels = classify(fd, facts, &scc);
        let prim = primary_label(&labels);
        // Verify count: parse from raw items (ResolvedTopLevel doesn't carry it; TopLevel::Verify does).
        let verify_count: usize = items
            .iter()
            .filter_map(|i| match i {
                TopLevel::Verify(v) if v.fn_name == fd.name => Some(v.cases.len()),
                _ => None,
            })
            .sum();
        fns_out.push(FnRecord {
            name: fd.name.clone(),
            fn_id: fd.fn_id,
            param_count: fd.params.len(),
            verify_count,
            primary: prim,
            labels,
            calls_to: facts.calls_to.clone(),
            param_types: fd.params.iter().map(|(_, ty)| ty.clone()).collect(),
        });
    }
    Ok(FileResult {
        file: String::new(), // filled by caller
        folder: String::new(),
        fns: fns_out,
    })
}

// ── Test entry ────────────────────────────────────────────────────────

#[test]
#[ignore]
fn research_archetype_clustering_full_corpus() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let roots = ["examples", "projects", "self_hosted"];
    let mut all_av: Vec<PathBuf> = Vec::new();
    for root in &roots {
        let abs = repo_root.join(root);
        if abs.exists() {
            collect_av(&abs, &mut all_av);
        }
    }
    all_av.sort();
    eprintln!("# scanning {} .av files", all_av.len());

    let mut all_files: Vec<FileResult> = Vec::new();
    let mut skipped: Vec<(String, String)> = Vec::new();

    for av in &all_av {
        let rel = av.strip_prefix(&repo_root).unwrap_or(av);
        let folder = rel
            .parent()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default();
        match analyze_file(av) {
            Ok(mut r) => {
                r.file = rel.to_string_lossy().to_string();
                r.folder = folder;
                all_files.push(r);
            }
            Err(e) => skipped.push((rel.to_string_lossy().to_string(), e)),
        }
    }

    let total_fns: usize = all_files.iter().map(|r| r.fns.len()).sum();
    eprintln!();
    eprintln!("# total fns classified: {} ({} files skipped)", total_fns, skipped.len());

    use std::collections::BTreeMap;

    // ── Sec 1: primary archetype distribution ─────────────────────────
    let mut primary_counts: BTreeMap<&'static str, usize> = BTreeMap::new();
    let mut label_counts: BTreeMap<&'static str, usize> = BTreeMap::new();
    for r in &all_files {
        for f in &r.fns {
            *primary_counts.entry(f.primary).or_insert(0) += 1;
            for l in &f.labels {
                *label_counts.entry(*l).or_insert(0) += 1;
            }
        }
    }
    eprintln!();
    eprintln!("# Primary archetype distribution");
    let mut sorted_p: Vec<_> = primary_counts.iter().collect();
    sorted_p.sort_by(|a, b| b.1.cmp(a.1));
    for (l, c) in &sorted_p {
        eprintln!("  {:30} {:5}  {:5.1}%", l, c, 100.0 * (**c as f64) / (total_fns.max(1) as f64));
    }

    // ── H2: Verify density per archetype ──────────────────────────────
    eprintln!();
    eprintln!("# H2: Verify density per primary archetype (cases / fn)");
    let mut verify_sum: BTreeMap<&'static str, (usize, usize)> = BTreeMap::new(); // (verify_total, fn_total)
    for r in &all_files {
        for f in &r.fns {
            let entry = verify_sum.entry(f.primary).or_insert((0, 0));
            entry.0 += f.verify_count;
            entry.1 += 1;
        }
    }
    let mut vs: Vec<_> = verify_sum.iter().collect();
    vs.sort_by(|a, b| {
        let da = a.1.0 as f64 / (a.1.1 as f64).max(1.0);
        let db = b.1.0 as f64 / (b.1.1 as f64).max(1.0);
        db.partial_cmp(&da).unwrap()
    });
    for (label, (v, n)) in vs {
        let density = (*v as f64) / (*n as f64).max(1.0);
        eprintln!("  {:30} {:5.2} verify/fn  ({} cases over {} fns)", label, density, v, n);
    }

    // ── H4: Seeded-driver pair detection + semantic-significance split
    // (the question the second AI surfaced: accumulator deserves public
    // review surface iff its type is a user-defined record/sum, not a
    // built-in container).
    eprintln!();
    eprintln!("# H4: Seeded-driver pairs — semantic accumulator vs mechanical duplicate");
    let mut semantic_pairs: Vec<(String, String, String, String)> = Vec::new();   // file, driver, worker, acc_type
    let mut mechanical_pairs: Vec<(String, String, String, String)> = Vec::new();
    for r in &all_files {
        let by_name: HashMap<&str, &FnRecord> = r.fns.iter().map(|f| (f.name.as_str(), f)).collect();
        for f in &r.fns {
            for suffix in &["Acc", "Loop", "Inner", "Helper"] {
                let candidate = format!("{}{}", f.name, suffix);
                if let Some(worker) = by_name.get(candidate.as_str()) {
                    let acc_kind = accumulator_kind(worker);
                    let acc_desc = describe_acc_type(worker);
                    let row = (r.file.clone(), f.name.clone(), candidate, acc_desc);
                    if acc_kind == AccKind::Semantic {
                        semantic_pairs.push(row);
                    } else {
                        mechanical_pairs.push(row);
                    }
                }
            }
        }
    }
    let total_pairs = semantic_pairs.len() + mechanical_pairs.len();
    eprintln!(
        "  {} total pairs:  {} semantic ({:.0}%)  |  {} mechanical ({:.0}%)",
        total_pairs,
        semantic_pairs.len(),
        100.0 * semantic_pairs.len() as f64 / total_pairs.max(1) as f64,
        mechanical_pairs.len(),
        100.0 * mechanical_pairs.len() as f64 / total_pairs.max(1) as f64,
    );
    eprintln!();
    eprintln!("  --- semantic (worker's accumulator is user-defined Named type) ---");
    for (file, driver, worker, acc) in semantic_pairs.iter().take(15) {
        eprintln!("    {}: {} → {}  [acc: {}]", file, driver, worker, acc);
    }
    if semantic_pairs.len() > 15 {
        eprintln!("    … and {} more", semantic_pairs.len() - 15);
    }
    eprintln!();
    eprintln!("  --- mechanical (worker's accumulator is built-in container / primitive) ---");
    for (file, driver, worker, acc) in mechanical_pairs.iter().take(15) {
        eprintln!("    {}: {} → {}  [acc: {}]", file, driver, worker, acc);
    }
    if mechanical_pairs.len() > 15 {
        eprintln!("    … and {} more", mechanical_pairs.len() - 15);
    }

    // ── H5: pure-expression concentration ─────────────────────────────
    eprintln!();
    eprintln!("# H5: pure-expression concentration per top-level folder");
    let mut pe_per_root: BTreeMap<String, (usize, usize)> = BTreeMap::new();
    for r in &all_files {
        let top = r.folder.split('/').next().unwrap_or(&r.folder).to_string();
        let entry = pe_per_root.entry(top).or_insert((0, 0));
        for f in &r.fns {
            entry.1 += 1;
            if f.primary == "pure-expression" {
                entry.0 += 1;
            }
        }
    }
    let mut pe_sorted: Vec<_> = pe_per_root.iter().collect();
    pe_sorted.sort_by(|a, b| {
        let ra = a.1.0 as f64 / (a.1.1 as f64).max(1.0);
        let rb = b.1.0 as f64 / (b.1.1 as f64).max(1.0);
        rb.partial_cmp(&ra).unwrap()
    });
    for (root, (pe, total)) in pe_sorted {
        let pct = 100.0 * (*pe as f64) / (*total as f64).max(1.0);
        eprintln!("  {:20} {:5.1}%  ({} pure-expr of {} fns)", root, pct, pe, total);
    }

    // ── H3: Call graph between archetypes ─────────────────────────────
    eprintln!();
    eprintln!("# H3: Call graph — caller archetype → callee archetype (raw counts)");
    let mut fn_archetype: HashMap<FnId, &'static str> = HashMap::new();
    for r in &all_files {
        for f in &r.fns {
            fn_archetype.insert(f.fn_id, f.primary);
        }
    }
    let mut edge_counts: BTreeMap<(&'static str, &'static str), usize> = BTreeMap::new();
    for r in &all_files {
        for f in &r.fns {
            for callee_id in &f.calls_to {
                if let Some(callee_arch) = fn_archetype.get(callee_id) {
                    *edge_counts.entry((f.primary, *callee_arch)).or_insert(0) += 1;
                }
            }
        }
    }
    let archs = primary_label_order();
    // Header
    eprint!("  {:>26} |", "caller↓ / callee→");
    for a in archs.iter() {
        eprint!(" {:>4}", abbrev(a));
    }
    eprintln!();
    eprintln!("  {:>26}-+{}", "", "-----".repeat(archs.len()));
    for caller in archs.iter() {
        eprint!("  {:>26} |", caller);
        for callee in archs.iter() {
            let v = edge_counts.get(&(*caller, *callee)).copied().unwrap_or(0);
            if v == 0 {
                eprint!("    .");
            } else {
                eprint!(" {:4}", v);
            }
        }
        eprintln!();
    }

    // ── H1: Confusion matrix layer × archetype (z-scores from mean) ───
    eprintln!();
    eprintln!("# H1: Layer × archetype Z-scores (deviation from corpus-wide mean rate)");
    eprintln!("#     z > 2 means archetype is over-represented in that layer.");
    let mut per_folder: BTreeMap<String, BTreeMap<&'static str, usize>> = BTreeMap::new();
    let mut per_folder_totals: BTreeMap<String, usize> = BTreeMap::new();
    for r in &all_files {
        let top = r.folder.split('/').next().unwrap_or(&r.folder).to_string();
        let entry = per_folder.entry(top.clone()).or_default();
        for f in &r.fns {
            *entry.entry(f.primary).or_insert(0) += 1;
            *per_folder_totals.entry(top.clone()).or_insert(0) += 1;
        }
    }
    eprint!("  {:>15} |", "layer ↓");
    for a in archs.iter() {
        eprint!(" {:>5}", abbrev(a));
    }
    eprintln!();
    eprintln!("  {:>15}-+{}", "", "------".repeat(archs.len()));
    for (folder, counts) in &per_folder {
        let total = *per_folder_totals.get(folder).unwrap_or(&1) as f64;
        eprint!("  {:>15} |", folder);
        for arch in archs.iter() {
            let p = primary_counts.get(arch).copied().unwrap_or(0) as f64
                / (total_fns.max(1) as f64);
            let observed = counts.get(arch).copied().unwrap_or(0) as f64 / total.max(1.0);
            // Z-score using normal approximation: (observed - expected) / sqrt(p(1-p)/n)
            let expected = p;
            let stderr = (p * (1.0 - p) / total.max(1.0)).sqrt();
            let z = if stderr > 0.0 {
                (observed - expected) / stderr
            } else {
                0.0
            };
            if z.abs() < 1.0 {
                eprint!("     ·");
            } else {
                eprint!(" {:>+5.1}", z);
            }
        }
        eprintln!();
    }

    // ── Per-folder primary top-3 (qualitative anchor) ─────────────────
    eprintln!();
    eprintln!("# Per-folder primary top-3 (qualitative anchor)");
    let mut folder_top: BTreeMap<String, BTreeMap<&'static str, usize>> = BTreeMap::new();
    for r in &all_files {
        let top = r.folder.split('/').next().unwrap_or(&r.folder).to_string();
        let entry = folder_top.entry(top).or_default();
        for f in &r.fns {
            *entry.entry(f.primary).or_insert(0) += 1;
        }
    }
    for (folder, counts) in &folder_top {
        let total: usize = counts.values().sum();
        let mut top: Vec<_> = counts.iter().collect();
        top.sort_by(|a, b| b.1.cmp(a.1));
        let top3 = top
            .iter()
            .take(3)
            .map(|(l, c)| format!("{}={}", l, c))
            .collect::<Vec<_>>()
            .join(", ");
        eprintln!("  {:30} (n={:4}) {}", folder, total, top3);
    }
}

#[derive(PartialEq, Eq)]
enum AccKind {
    Semantic,
    Mechanical,
}

/// Inspect the worker's non-first params; if ANY of them is a
/// user-defined Named type (`Type::Named { id: Some(_), .. }` with a
/// name that doesn't look like a built-in collection / wrapper), the
/// accumulator is semantic — the worker's signature documents real
/// domain state. Otherwise it's mechanical (just an accumulator List /
/// Result / Tuple / primitive count).
fn accumulator_kind(worker: &FnRecord) -> AccKind {
    if worker.param_types.len() < 2 {
        // Single param worker — no separate accumulator. Treat as
        // mechanical (it's the iteration-variable rename, e.g.
        // `showListInner(xs)` after the driver stripped the brackets).
        return AccKind::Mechanical;
    }
    for ty in worker.param_types.iter().skip(1) {
        if is_user_defined_named(ty) {
            return AccKind::Semantic;
        }
    }
    AccKind::Mechanical
}

fn is_user_defined_named(ty: &Type) -> bool {
    match ty {
        Type::Named { id: Some(_), name } => {
            // Exclude built-in record types the resolver sometimes
            // stamps with an id (HttpResponse, Header, Tcp.Connection,
            // Buffer, etc. — the comment on Type::Named in
            // src/ast/types.rs notes these). They are not user domain
            // types.
            !is_builtin_named(name)
        }
        // Result/Option wrapping a user type still counts as semantic
        // when the inner Ok is user-defined (e.g. `Result<RleAcc, String>`).
        Type::Result(ok, _) => is_user_defined_named(ok),
        Type::Option(inner) => is_user_defined_named(inner),
        Type::Tuple(parts) => parts.iter().any(is_user_defined_named),
        _ => false,
    }
}

fn is_builtin_named(name: &str) -> bool {
    matches!(
        name,
        "HttpResponse"
            | "HttpRequest"
            | "Header"
            | "Tcp.Connection"
            | "Buffer"
            | "Date"
            | "Duration"
            | "Time"
    )
}

fn describe_acc_type(worker: &FnRecord) -> String {
    if worker.param_types.len() < 2 {
        return "<single param>".to_string();
    }
    worker
        .param_types
        .iter()
        .skip(1)
        .map(short_type_name)
        .collect::<Vec<_>>()
        .join(", ")
}

fn short_type_name(ty: &Type) -> String {
    match ty {
        Type::Int => "Int".into(),
        Type::Float => "Float".into(),
        Type::Str => "String".into(),
        Type::Bool => "Bool".into(),
        Type::Unit => "Unit".into(),
        Type::Result(ok, err) => format!("Result<{}, {}>", short_type_name(ok), short_type_name(err)),
        Type::Option(inner) => format!("Option<{}>", short_type_name(inner)),
        Type::List(inner) => format!("List<{}>", short_type_name(inner)),
        Type::Vector(inner) => format!("Vector<{}>", short_type_name(inner)),
        Type::Map(k, v) => format!("Map<{}, {}>", short_type_name(k), short_type_name(v)),
        Type::Tuple(parts) => {
            let inner = parts.iter().map(short_type_name).collect::<Vec<_>>().join(", ");
            format!("({})", inner)
        }
        Type::Fn(args, ret, _) => {
            let a = args.iter().map(short_type_name).collect::<Vec<_>>().join(", ");
            format!("fn({}) -> {}", a, short_type_name(ret))
        }
        Type::Var(name) => name.clone(),
        Type::Invalid => "<invalid>".into(),
        Type::Named { name, .. } => name.clone(),
    }
}

fn primary_label_order() -> &'static [&'static str] {
    &[
        "scc-mutual",
        "structural-recursion",
        "match-dispatcher",
        "match-on-value",
        "orchestration",
        "let-pipeline",
        "constructor-wrapper",
        "trivial-helper",
        "pure-expression",
        "effectful-leaf",
        "data-as-function",
    ]
}

fn abbrev(label: &str) -> &str {
    match label {
        "scc-mutual" => "scc",
        "structural-recursion" => "rec",
        "match-dispatcher" => "mD",
        "match-on-value" => "mV",
        "orchestration" => "orch",
        "let-pipeline" => "let",
        "constructor-wrapper" => "ctor",
        "trivial-helper" => "triv",
        "pure-expression" => "pure",
        "effectful-leaf" => "leaf",
        "data-as-function" => "data",
        _ => label,
    }
}

fn resolve_module_root(entry: &Path, items: &[TopLevel]) -> Option<String> {
    let deps: Vec<String> = items
        .iter()
        .find_map(|i| match i {
            TopLevel::Module(m) => Some(m.depends.clone()),
            _ => None,
        })
        .unwrap_or_default();
    let mut candidate = entry.parent()?.to_path_buf();
    // Walk up to 6 levels — enough for repo_root / projects/foo / app
    for _ in 0..6 {
        let root_str = candidate.to_string_lossy().to_string();
        let all_resolve = deps
            .iter()
            .all(|d| aver::source::find_module_file(d, &root_str).is_some());
        if all_resolve {
            return Some(root_str);
        }
        candidate = match candidate.parent() {
            Some(p) => p.to_path_buf(),
            None => return None,
        };
    }
    None
}

fn collect_av(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_av(&path, out);
        } else if path.extension().and_then(|s| s.to_str()) == Some("av") {
            out.push(path);
        }
    }
}

// Re-export so `cargo check --tests` builds it.
#[allow(dead_code)]
fn _unused_imports_anchor() {
    let _ = Arc::new(0);
    let _ = Spanned::bare(0);
}
