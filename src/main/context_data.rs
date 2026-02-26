use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

use aver::ast::{DecisionBlock, FnDef, TopLevel, TypeDef, VerifyBlock};
use aver::source::{find_module_file, parse_source};

pub(super) struct FileContext {
    pub(super) source_file: String,
    pub(super) module_name: Option<String>,
    pub(super) intent: Option<String>,
    pub(super) exposes: Vec<String>,
    pub(super) fn_defs: Vec<FnDef>,
    pub(super) type_defs: Vec<TypeDef>,
    pub(super) effect_sets: Vec<(String, Vec<String>)>,
    pub(super) verify_blocks: Vec<VerifyBlock>,
    pub(super) decisions: Vec<DecisionBlock>,
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
