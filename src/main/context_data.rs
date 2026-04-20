//! Multi-file context traversal for `aver context`.
//!
//! The per-file build logic lives in the canonical lib
//! (`aver::diagnostics::context`). This module adds filesystem access
//! and recursive dependency expansion — the pieces that don't belong in
//! a wasm-safe library.

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use aver::diagnostics::context::build_context_for_items;
use aver::source::{find_module_file, parse_source, require_module_declaration};

pub(super) use aver::diagnostics::context::FileContext;

/// Walk `file` and its declared dependencies (up to `max_depth`), returning
/// one [`FileContext`] per file visited (leaves last). `visited` is
/// threaded through to prevent cycles.
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

    // Strip module_root prefix so paths are relative to the project root.
    let relative_file = Path::new(file)
        .strip_prefix(module_root)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| file.to_string());

    let ctx = build_context_for_items(&items, &source, relative_file, Some(module_root));
    let dep_names = ctx.depends.clone();

    let mut result = vec![ctx];

    let should_recurse = max_depth.is_none_or(|d| d > 0);
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
