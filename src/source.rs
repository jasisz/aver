use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::ast::TopLevel;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::visibility;

pub fn parse_source(source: &str) -> Result<Vec<TopLevel>, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let mut parser = Parser::new(tokens);
    parser.parse().map_err(|e| e.to_string())
}

/// Enforce module contract for file-based programs:
/// exactly one `module` declaration and it must be the first top-level item.
pub fn require_module_declaration(items: &[TopLevel], file: &str) -> Result<(), String> {
    let module_positions: Vec<usize> = items
        .iter()
        .enumerate()
        .filter_map(|(idx, item)| matches!(item, TopLevel::Module(_)).then_some(idx))
        .collect();

    if module_positions.is_empty() {
        return Err(format!(
            "File '{}' must declare `module <Name>` as the first top-level item",
            file
        ));
    }

    if module_positions[0] != 0 {
        return Err(format!(
            "File '{}' must place `module <Name>` as the first top-level item",
            file
        ));
    }

    if module_positions.len() > 1 {
        return Err(format!(
            "File '{}' must contain exactly one module declaration (found {})",
            file,
            module_positions.len()
        ));
    }

    Ok(())
}

pub fn find_module_file(name: &str, module_root: &str) -> Option<PathBuf> {
    let root = Path::new(module_root);
    let parts: Vec<&str> = name.split('.').filter(|s| !s.is_empty()).collect();
    if parts.is_empty() {
        return None;
    }

    let lower_rel = format!(
        "{}.av",
        parts
            .iter()
            .map(|p| p.to_lowercase())
            .collect::<Vec<_>>()
            .join("/")
    );
    let exact_rel = format!("{}.av", parts.join("/"));

    let lower = root.join(&lower_rel);
    if lower.exists() {
        return Some(lower);
    }

    let exact = root.join(&exact_rel);
    if exact.exists() {
        return Some(exact);
    }

    None
}

pub fn canonicalize_path(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

// ---------------------------------------------------------------------------
// Shared module loader — find, read, parse, validate, recurse
// ---------------------------------------------------------------------------

/// A parsed module ready for backend consumption.
pub struct LoadedModule {
    pub dep_name: String,
    pub items: Vec<TopLevel>,
    pub path: PathBuf,
}

/// Load a dependency tree starting from `root_deps`.
/// Returns modules in dependency order (leaves first).
/// Validates module declarations and detects circular imports.
pub fn load_module_tree(
    root_deps: &[String],
    module_root: &str,
) -> Result<Vec<LoadedModule>, String> {
    let mut result = Vec::new();
    let mut loaded = HashSet::new();
    let mut loading = Vec::new();
    for dep in root_deps {
        load_recursive(dep, module_root, &mut loaded, &mut loading, &mut result)?;
    }
    Ok(result)
}

fn load_recursive(
    dep_name: &str,
    module_root: &str,
    loaded: &mut HashSet<String>,
    loading: &mut Vec<String>,
    result: &mut Vec<LoadedModule>,
) -> Result<(), String> {
    let path = find_module_file(dep_name, module_root)
        .ok_or_else(|| format!("Module '{}' not found in '{}'", dep_name, module_root))?;
    let canon = canonicalize_path(&path).to_string_lossy().to_string();

    if loaded.contains(&canon) {
        return Ok(());
    }
    if loading.contains(&canon) {
        let chain: Vec<String> = loading
            .iter()
            .map(|k| {
                Path::new(k)
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or(k)
                    .to_string()
            })
            .chain(std::iter::once(
                Path::new(&canon)
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or(&canon)
                    .to_string(),
            ))
            .collect();
        return Err(format!("Circular import: {}", chain.join(" -> ")));
    }
    loading.push(canon.clone());

    let source = std::fs::read_to_string(&path)
        .map_err(|e| format!("Cannot read '{}': {}", path.display(), e))?;
    let items =
        parse_source(&source).map_err(|e| format!("Parse error in '{}': {}", dep_name, e))?;

    require_module_declaration(&items, &path.to_string_lossy())?;

    if let Some(module) = visibility::module_decl(&items) {
        let expected = dep_name.rsplit('.').next().unwrap_or(dep_name);
        if module.name != expected {
            return Err(format!(
                "Module name mismatch: expected '{}' (from '{}'), found '{}' in '{}'",
                expected,
                dep_name,
                module.name,
                path.display()
            ));
        }
        for sub_dep in &module.depends {
            load_recursive(sub_dep, module_root, loaded, loading, result)?;
        }
    }

    loading.pop();
    loaded.insert(canon);
    result.push(LoadedModule {
        dep_name: dep_name.to_string(),
        items,
        path,
    });
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{parse_source, require_module_declaration};

    #[test]
    fn require_module_accepts_single_first_module() {
        let src = "module Demo\n    intent = \"ok\"\nfn x() -> Int\n    1\n";
        let items = parse_source(src).expect("parse");
        require_module_declaration(&items, "demo.av").expect("module declaration should pass");
    }

    #[test]
    fn require_module_rejects_missing_module() {
        let src = "fn x() -> Int\n    1\n";
        let items = parse_source(src).expect("parse");
        let err = require_module_declaration(&items, "demo.av").expect_err("expected error");
        assert!(err.contains("must declare `module <Name>`"));
    }

    #[test]
    fn require_module_rejects_module_not_first() {
        let src = "fn x() -> Int\n    1\nmodule Demo\n";
        let items = parse_source(src).expect("parse");
        let err = require_module_declaration(&items, "demo.av").expect_err("expected error");
        assert!(err.contains("must place `module <Name>` as the first"));
    }

    #[test]
    fn require_module_rejects_multiple_modules() {
        let src = "module A\nmodule B\n";
        let items = parse_source(src).expect("parse");
        let err = require_module_declaration(&items, "demo.av").expect_err("expected error");
        assert!(err.contains("exactly one module declaration"));
    }

    #[test]
    fn parse_rejects_record_positional_pattern() {
        let src = "module Demo\nrecord User\n    name: String\nfn f(u: User) -> String\n    match u\n        User(name) -> name\n";
        let err = parse_source(src).expect_err("record positional patterns should be rejected");
        assert!(err.contains("bind the whole value with a lower-case name"));
    }

    #[test]
    fn parse_rejects_unqualified_constructor_pattern() {
        let src = "module Demo\ntype Shape\n    Circle(Int)\nfn f(s: Shape) -> Int\n    match s\n        Circle(r) -> r\n";
        let err =
            parse_source(src).expect_err("unqualified constructor patterns should be rejected");
        assert!(err.contains("Constructor patterns must be qualified"));
    }
}
