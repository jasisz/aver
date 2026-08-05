use std::cell::RefCell;
use std::path::PathBuf;
use std::time::SystemTime;

use tower_lsp_server::ls_types::Uri;

use aver::ast::{FnDef, TopLevel, TypeDef};
use aver::source::{find_module_file, resolve_standard_module_source};

use crate::completion;

/// Information about a resolved module dependency.
#[derive(Clone)]
pub struct ResolvedModule {
    pub name: String,
    pub path: PathBuf,
    pub source: String,
    pub items: Vec<TopLevel>,
}

#[derive(Clone)]
struct CachedModuleFile {
    modified: Option<SystemTime>,
    source: String,
    items: Vec<TopLevel>,
}

thread_local! {
    static MODULE_FILE_CACHE: RefCell<std::collections::HashMap<PathBuf, CachedModuleFile>> =
        RefCell::new(std::collections::HashMap::new());
}

fn read_module_cached(path: &PathBuf) -> Option<(String, Vec<TopLevel>)> {
    let modified = std::fs::metadata(path).ok().and_then(|m| m.modified().ok());
    let cached = MODULE_FILE_CACHE.with(|cache| {
        cache
            .borrow()
            .get(path)
            .filter(|entry| entry.modified == modified)
            .map(|entry| (entry.source.clone(), entry.items.clone()))
    });
    if let Some(hit) = cached {
        return Some(hit);
    }

    let source = std::fs::read_to_string(path).ok()?;
    let items = completion::parse_items(&source);

    MODULE_FILE_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        cache.insert(
            path.clone(),
            CachedModuleFile {
                modified,
                source: source.clone(),
                items: items.clone(),
            },
        );
    });

    Some((source, items))
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
        if let Some(module) = resolve_standard_module_source(dep_name) {
            let items = completion::parse_items(&module.source);
            modules.push(ResolvedModule {
                name: dep_name.clone(),
                path: module.path,
                source: module.source,
                items,
            });
            continue;
        }
        if let Some(path) = find_module_file(dep_name, base_dir)
            && let Some((mod_source, mod_items)) = read_module_cached(&path)
        {
            modules.push(ResolvedModule {
                name: dep_name.clone(),
                path,
                source: mod_source,
                items: mod_items,
            });
        }
    }
    modules
}

/// Get the base directory from an LSP URI, properly handling percent-encoding.
pub fn base_dir_from_uri(uri: &Uri) -> Option<String> {
    let path = uri.to_file_path()?;
    path.parent().map(|p| p.to_string_lossy().to_string())
}

/// Convert a filesystem path to an LSP Uri.
pub fn path_to_uri(path: &std::path::Path) -> Option<Uri> {
    Uri::from_file_path(path)
}

fn exposed_names(items: &[TopLevel]) -> Option<std::collections::HashSet<&str>> {
    items.iter().find_map(|item| {
        if let TopLevel::Module(m) = item {
            if m.exposes.is_empty() && m.exposes_opaque.is_empty() {
                None
            } else {
                let mut set: std::collections::HashSet<&str> =
                    m.exposes.iter().map(|s| s.as_str()).collect();
                for name in &m.exposes_opaque {
                    set.insert(name.as_str());
                }
                Some(set)
            }
        } else {
            None
        }
    })
}

/// Get exported FnDefs from a module (respecting `exposes`).
pub fn exported_fns(module: &ResolvedModule) -> Vec<&FnDef> {
    let exposed = exposed_names(&module.items);

    module
        .items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                let include = match &exposed {
                    Some(set) => set.contains(fd.name.as_str()),
                    None => !fd.name.starts_with('_'),
                };
                if include { Some(fd) } else { None }
            } else {
                None
            }
        })
        .collect()
}

/// Get exported TypeDefs from a module.
pub fn exported_types(module: &ResolvedModule) -> Vec<&TypeDef> {
    let exposed = exposed_names(&module.items);

    module
        .items
        .iter()
        .filter_map(|item| {
            if let TopLevel::TypeDef(td) = item {
                let include = match (&exposed, td) {
                    (Some(set), TypeDef::Sum { name, .. }) => set.contains(name.as_str()),
                    (Some(set), TypeDef::Product { name, .. }) => set.contains(name.as_str()),
                    (None, _) => true,
                };
                if include { Some(td) } else { None }
            } else {
                None
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::{ResolvedModule, exported_fns, exported_types, resolve_dependencies};
    use crate::completion;

    #[test]
    fn exported_symbols_respect_module_exposes() {
        let source = r#"module Demo
    exposes [publicFn, PublicType]

fn publicFn() -> Int
    1

fn helperFn() -> Int
    2

type PublicType
    Ready

type HiddenType
    Secret
"#;
        let module = ResolvedModule {
            name: "Demo".to_string(),
            path: PathBuf::from("Demo.av"),
            source: source.to_string(),
            items: completion::parse_items(source),
        };

        let fn_names: Vec<&str> = exported_fns(&module)
            .iter()
            .map(|fd| fd.name.as_str())
            .collect();
        let type_names: Vec<&str> = exported_types(&module)
            .iter()
            .map(|td| match td {
                aver::ast::TypeDef::Sum { name, .. } => name.as_str(),
                aver::ast::TypeDef::Product { name, .. } => name.as_str(),
            })
            .collect();

        assert_eq!(fn_names, vec!["publicFn"]);
        assert_eq!(type_names, vec!["PublicType"]);
    }

    #[test]
    fn resolves_embedded_standard_module_for_completion() {
        let source = "module Demo\n    depends [Bytes, Crypto.Digest32]\n";
        let modules = resolve_dependencies(source, "/path/that/does/not/exist");
        assert_eq!(modules.len(), 2);
        assert_eq!(modules[0].name, "Bytes");
        assert_eq!(modules[0].path.to_string_lossy(), "<aver-stdlib>/bytes.av");
        assert_eq!(modules[1].name, "Crypto.Digest32");
        assert_eq!(
            modules[1].path.to_string_lossy(),
            "<aver-stdlib>/crypto/digest32.av"
        );
        assert!(exported_types(&modules[1]).iter().any(|ty| {
            matches!(ty, aver::ast::TypeDef::Product { name, .. } if name == "Digest32")
        }));
    }
}
