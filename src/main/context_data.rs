use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;

use aver::ast::{DecisionBlock, FnDef, TopLevel, TypeDef, VerifyBlock};
use aver::call_graph::{find_recursive_fns, recursive_callsite_counts, recursive_scc_ids};
use aver::source::{find_module_file, parse_source};
use aver::tco;
use aver::types::checker::run_type_check_full;

use crate::shared::{compute_memo_fns, is_memo_safe_type};

pub(super) struct FileContext {
    pub(super) source_file: String,
    pub(super) module_name: Option<String>,
    pub(super) intent: Option<String>,
    pub(super) exposes: Vec<String>,
    pub(super) fn_defs: Vec<FnDef>,
    pub(super) fn_auto_memo: HashSet<String>,
    pub(super) fn_memo_qual: HashMap<String, Vec<String>>,
    pub(super) fn_auto_tco: HashSet<String>,
    pub(super) fn_recursive_callsites: HashMap<String, usize>,
    pub(super) fn_recursive_scc_id: HashMap<String, usize>,
    pub(super) type_defs: Vec<TypeDef>,
    pub(super) effect_sets: Vec<(String, Vec<String>)>,
    pub(super) verify_blocks: Vec<VerifyBlock>,
    pub(super) decisions: Vec<DecisionBlock>,
}

fn expr_has_tail_call(expr: &aver::ast::Expr) -> bool {
    use aver::ast::Expr;
    match expr {
        Expr::TailCall(_) => true,
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => false,
        Expr::Attr(obj, _) => expr_has_tail_call(obj),
        Expr::FnCall(f, args) => expr_has_tail_call(f) || args.iter().any(expr_has_tail_call),
        Expr::BinOp(_, l, r) | Expr::Pipe(l, r) => expr_has_tail_call(l) || expr_has_tail_call(r),
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
    match fd.body.as_ref() {
        aver::ast::FnBody::Expr(expr) => expr_has_tail_call(expr),
        aver::ast::FnBody::Block(stmts) => stmts.iter().any(|stmt| match stmt {
            aver::ast::Stmt::Binding(_, _, expr) | aver::ast::Stmt::Expr(expr) => {
                expr_has_tail_call(expr)
            }
        }),
    }
}

fn compute_context_fn_flags(
    items: &[TopLevel],
    module_root: &str,
) -> (
    HashSet<String>,
    HashSet<String>,
    HashMap<String, Vec<String>>,
    HashMap<String, usize>,
    HashMap<String, usize>,
) {
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
        return (
            HashSet::new(),
            tco_fns,
            memo_qual,
            recursive_callsites,
            recursive_scc_id,
        );
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

    (
        compute_memo_fns(&transformed, &tc_result),
        tco_fns,
        memo_qual,
        recursive_callsites,
        recursive_scc_id,
    )
}

pub(super) fn collect_contexts(
    file: &str,
    module_root: &str,
    visited: &mut HashSet<String>,
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

    let mut ctx = FileContext {
        source_file: file.to_string(),
        module_name: None,
        intent: None,
        exposes: vec![],
        fn_defs: vec![],
        fn_auto_memo: HashSet::new(),
        fn_memo_qual: HashMap::new(),
        fn_auto_tco: HashSet::new(),
        fn_recursive_callsites: HashMap::new(),
        fn_recursive_scc_id: HashMap::new(),
        type_defs: vec![],
        effect_sets: vec![],
        verify_blocks: vec![],
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
                ctx.exposes = m.exposes.clone();
                dep_names = m.depends.clone();
            }
            TopLevel::FnDef(fd) => ctx.fn_defs.push(fd.clone()),
            TopLevel::TypeDef(td) => ctx.type_defs.push(td.clone()),
            TopLevel::EffectSet { name, effects } => {
                ctx.effect_sets.push((name.clone(), effects.clone()))
            }
            TopLevel::Verify(vb) => ctx.verify_blocks.push(vb.clone()),
            TopLevel::Decision(db) => ctx.decisions.push(db.clone()),
            _ => {}
        }
    }

    let (auto_memo, auto_tco, memo_qual, recursive_callsites, recursive_scc_id) =
        compute_context_fn_flags(&items, module_root);
    ctx.fn_auto_memo = auto_memo;
    ctx.fn_auto_tco = auto_tco;
    ctx.fn_memo_qual = memo_qual;
    ctx.fn_recursive_callsites = recursive_callsites;
    ctx.fn_recursive_scc_id = recursive_scc_id;

    // Filter functions by exposes if the list is non-empty
    if !ctx.exposes.is_empty() {
        let exposes = ctx.exposes.clone();
        ctx.fn_defs.retain(|fd| exposes.contains(&fd.name));
    }

    let mut result = vec![ctx];

    // Recurse into dependencies
    for dep_name in dep_names {
        if let Some(dep_path) = find_module_file(&dep_name, module_root) {
            let dep_file = dep_path.to_string_lossy().to_string();
            let mut sub = collect_contexts(&dep_file, module_root, visited);
            result.append(&mut sub);
        }
    }

    result
}
