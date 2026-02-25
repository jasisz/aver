use std::path::{Path, PathBuf};

use crate::ast::TopLevel;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub fn parse_source(source: &str) -> Result<Vec<TopLevel>, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let mut parser = Parser::new(tokens);
    parser.parse().map_err(|e| e.to_string())
}

pub fn find_module_file(name: &str, base_dir: &str) -> Option<PathBuf> {
    let base = Path::new(base_dir);
    let parts: Vec<&str> = name.split('.').filter(|s| !s.is_empty()).collect();
    if parts.is_empty() {
        return None;
    }

    let lower_rel = parts
        .iter()
        .map(|p| p.to_lowercase())
        .collect::<Vec<_>>()
        .join("/");
    let lower = base.join(format!("{}.av", lower_rel));
    if lower.exists() {
        return Some(lower);
    }

    let exact_rel = parts.join("/");
    let exact = base.join(format!("{}.av", exact_rel));
    if exact.exists() {
        return Some(exact);
    }

    None
}

pub fn canonicalize_path(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}
