use std::path::PathBuf;

use tower_lsp_server::ls_types::Uri;

use aver::ast::{FnDef, TopLevel, TypeDef};
use aver::source::find_module_file;

use crate::completion;

/// Information about a resolved module dependency.
pub struct ResolvedModule {
    pub name: String,
    pub path: PathBuf,
    pub source: String,
    pub items: Vec<TopLevel>,
}

/// Extract module `depends` list from parsed items.
pub fn extract_depends(items: &[TopLevel]) -> Vec<String> {
    for item in items {
        if let TopLevel::Module(m) = item {
            return m.depends.clone();
        }
    }
    Vec::new()
}

/// Resolve all module dependencies from the current file.
/// Returns a list of resolved modules with their parsed AST.
pub fn resolve_dependencies(source: &str, base_dir: &str) -> Vec<ResolvedModule> {
    let items = completion::parse_items(source);
    let depends = extract_depends(&items);

    let mut modules = Vec::new();
    for dep_name in &depends {
        if let Some(path) = find_module_file(dep_name, base_dir) {
            if let Ok(mod_source) = std::fs::read_to_string(&path) {
                let mod_items = completion::parse_items(&mod_source);
                modules.push(ResolvedModule {
                    name: dep_name.clone(),
                    path,
                    source: mod_source,
                    items: mod_items,
                });
            }
        }
    }
    modules
}

/// Get the base directory from an LSP URI, properly handling percent-encoding.
pub fn base_dir_from_uri(uri: &Uri) -> Option<String> {
    let path = uri.to_file_path()?;
    path.parent()
        .map(|p| p.to_string_lossy().to_string())
}

/// Convert a filesystem path to an LSP Uri.
pub fn path_to_uri(path: &std::path::Path) -> Option<Uri> {
    Uri::from_file_path(path)
}

/// Get exported FnDefs from a module (respecting `exposes`).
pub fn exported_fns(module: &ResolvedModule) -> Vec<&FnDef> {
    let exposed: Option<std::collections::HashSet<&str>> = module.items.iter().find_map(|item| {
        if let TopLevel::Module(m) = item {
            if m.exposes.is_empty() {
                None
            } else {
                Some(m.exposes.iter().map(|s| s.as_str()).collect())
            }
        } else {
            None
        }
    });

    module
        .items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                let include = match &exposed {
                    Some(set) => set.contains(fd.name.as_str()),
                    None => !fd.name.starts_with('_'),
                };
                if include {
                    Some(fd)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect()
}

/// Get exported TypeDefs from a module.
pub fn exported_types(module: &ResolvedModule) -> Vec<&TypeDef> {
    module
        .items
        .iter()
        .filter_map(|item| {
            if let TopLevel::TypeDef(td) = item {
                Some(td)
            } else {
                None
            }
        })
        .collect()
}
