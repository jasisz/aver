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

use aver::analysis::shape::{
    Archetype, Facts, classify, compute_sccs, extract_facts, primary_label,
};
use aver::ast::TopLevel;
use aver::ir::hir::{ResolvedFnDef, ResolvedTopLevel};
use aver::ir::{FnId, PipelineConfig, TypecheckMode};
use aver::types::Type;
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
    primary: Archetype,
    labels: Vec<Archetype>,
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
        format!("deps: cannot locate module root for {:?}", dep_names)
    })?;

    let dep_modules = aver::source::load_compile_deps(&items, &module_root)
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
    eprintln!(
        "# total fns classified: {} ({} files skipped)",
        total_fns,
        skipped.len()
    );

    use std::collections::BTreeMap;

    // ── Sec 1: primary archetype distribution ─────────────────────────
    let mut primary_counts: BTreeMap<Archetype, usize> = BTreeMap::new();
    let mut label_counts: BTreeMap<Archetype, usize> = BTreeMap::new();
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
        eprintln!(
            "  {:30} {:5}  {:5.1}%",
            l.as_str(),
            c,
            100.0 * (**c as f64) / (total_fns.max(1) as f64)
        );
    }

    // ── H2: Verify density per archetype ──────────────────────────────
    eprintln!();
    eprintln!("# H2: Verify density per primary archetype (cases / fn)");
    let mut verify_sum: BTreeMap<Archetype, (usize, usize)> = BTreeMap::new(); // (verify_total, fn_total)
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
        eprintln!(
            "  {:30} {:5.2} verify/fn  ({} cases over {} fns)",
            label.as_str(),
            density,
            v,
            n
        );
    }

    // ── H4: Seeded-driver pair detection + semantic-significance split
    // (the question the second AI surfaced: accumulator deserves public
    // review surface iff its type is a user-defined record/sum, not a
    // built-in container).
    eprintln!();
    eprintln!("# H4: Seeded-driver pairs — semantic accumulator vs mechanical duplicate");
    let mut semantic_pairs: Vec<(String, String, String, String)> = Vec::new(); // file, driver, worker, acc_type
    let mut mechanical_pairs: Vec<(String, String, String, String)> = Vec::new();
    for r in &all_files {
        let by_name: HashMap<&str, &FnRecord> =
            r.fns.iter().map(|f| (f.name.as_str(), f)).collect();
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
            if f.primary == Archetype::PureExpression {
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
        eprintln!(
            "  {:20} {:5.1}%  ({} pure-expr of {} fns)",
            root, pct, pe, total
        );
    }

    // ── H3: Call graph between archetypes ─────────────────────────────
    eprintln!();
    eprintln!("# H3: Call graph — caller archetype → callee archetype (raw counts)");
    let mut fn_archetype: HashMap<FnId, Archetype> = HashMap::new();
    for r in &all_files {
        for f in &r.fns {
            fn_archetype.insert(f.fn_id, f.primary);
        }
    }
    let mut edge_counts: BTreeMap<(Archetype, Archetype), usize> = BTreeMap::new();
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
        eprint!(" {:>4}", abbrev(*a));
    }
    eprintln!();
    eprintln!("  {:>26}-+{}", "", "-----".repeat(archs.len()));
    for caller in archs.iter() {
        eprint!("  {:>26} |", caller.as_str());
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
    let mut per_folder: BTreeMap<String, BTreeMap<Archetype, usize>> = BTreeMap::new();
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
        eprint!(" {:>5}", abbrev(*a));
    }
    eprintln!();
    eprintln!("  {:>15}-+{}", "", "------".repeat(archs.len()));
    for (folder, counts) in &per_folder {
        let total = *per_folder_totals.get(folder).unwrap_or(&1) as f64;
        eprint!("  {:>15} |", folder);
        for arch in archs.iter() {
            let p =
                primary_counts.get(arch).copied().unwrap_or(0) as f64 / (total_fns.max(1) as f64);
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
    let mut folder_top: BTreeMap<String, BTreeMap<Archetype, usize>> = BTreeMap::new();
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
            .map(|(l, c)| format!("{}={}", l.as_str(), c))
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
            // Exclude standard-library and compiler boundary types the resolver
            // stamps with an id (Http.Response, Header, Tcp.Connection, Buffer,
            // etc.). They are not user domain types.
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
        "Http.Response"
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
        Type::Result(ok, err) => {
            format!("Result<{}, {}>", short_type_name(ok), short_type_name(err))
        }
        Type::Option(inner) => format!("Option<{}>", short_type_name(inner)),
        Type::List(inner) => format!("List<{}>", short_type_name(inner)),
        Type::Vector(inner) => format!("Vector<{}>", short_type_name(inner)),
        Type::Map(k, v) => format!("Map<{}, {}>", short_type_name(k), short_type_name(v)),
        Type::Tuple(parts) => {
            let inner = parts
                .iter()
                .map(short_type_name)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", inner)
        }
        Type::Fn(args, ret, _) => {
            let a = args
                .iter()
                .map(short_type_name)
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({}) -> {}", a, short_type_name(ret))
        }
        Type::Var(name) => name.clone(),
        Type::Invalid => "<invalid>".into(),
        Type::Named { name, .. } => name.clone(),
    }
}

fn primary_label_order() -> &'static [Archetype] {
    &[
        Archetype::SccMutual,
        Archetype::StructuralRecursion,
        Archetype::MatchDispatcher,
        Archetype::MatchOnValue,
        Archetype::Orchestration,
        Archetype::LetPipeline,
        Archetype::ConstructorWrapper,
        Archetype::TrivialHelper,
        Archetype::PureExpression,
        Archetype::EffectfulLeaf,
        Archetype::DataAsFunction,
    ]
}

fn abbrev(label: Archetype) -> &'static str {
    match label {
        Archetype::SccMutual => "scc",
        Archetype::StructuralRecursion => "rec",
        Archetype::MatchDispatcher => "mD",
        Archetype::MatchOnValue => "mV",
        Archetype::Orchestration => "orch",
        Archetype::LetPipeline => "let",
        Archetype::ConstructorWrapper => "ctor",
        Archetype::TrivialHelper => "triv",
        Archetype::PureExpression => "pure",
        Archetype::EffectfulLeaf => "leaf",
        Archetype::DataAsFunction => "data",
        _ => label.as_str(),
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
