use tower_lsp_server::ls_types::{GotoDefinitionResponse, Location, Position, Range, Uri};

use aver::ast::{TopLevel, TypeDef};

use crate::completion;
use crate::modules;

/// Find the definition location for a word at the cursor.
pub fn goto_definition(
    word: &str,
    source: &str,
    uri: &Uri,
    base_dir: Option<&str>,
) -> Option<GotoDefinitionResponse> {
    let items = completion::parse_items(source);

    // Check for dotted name like "Shape.Circle" or "Examples.Redis.set"
    if let Some(last_dot) = word.rfind('.') {
        let prefix = &word[..last_dot];
        let member = &word[last_dot + 1..];

        // Try local type member first (single-segment prefix like "Shape")
        if let Some(result) = find_type_member_def(&items, prefix, member, uri) {
            return Some(result);
        }

        // Try cross-module: "Examples.Redis.set" → prefix="Examples.Redis", member="set"
        if let Some(base) = base_dir {
            let deps = modules::resolve_dependencies(source, base);
            for dep in &deps {
                let dep_short = dep.name.rsplit('.').next().unwrap_or(&dep.name);
                if dep.name == prefix || dep_short == prefix {
                    // Find the function in the module
                    for fd in modules::exported_fns(dep) {
                        if fd.name == member {
                            let line = fd.line.saturating_sub(1) as u32;
                            let Some(mod_uri) = modules::path_to_uri(&dep.path) else {
                                continue;
                            };
                            return Some(GotoDefinitionResponse::Scalar(Location {
                                uri: mod_uri,
                                range: Range {
                                    start: Position { line, character: 0 },
                                    end: Position { line, character: 0 },
                                },
                            }));
                        }
                    }
                    // Find type in the module
                    for td in modules::exported_types(dep) {
                        let (name, td_line) = match td {
                            TypeDef::Sum { name, line, .. } => (name.as_str(), *line),
                            TypeDef::Product { name, line, .. } => (name.as_str(), *line),
                        };
                        if name == member {
                            let Some(mod_uri) = modules::path_to_uri(&dep.path) else {
                                continue;
                            };
                            return Some(GotoDefinitionResponse::Scalar(Location {
                                uri: mod_uri,
                                range: Range {
                                    start: Position {
                                        line: td_line.saturating_sub(1) as u32,
                                        character: 0,
                                    },
                                    end: Position {
                                        line: td_line.saturating_sub(1) as u32,
                                        character: 0,
                                    },
                                },
                            }));
                        }
                    }
                }
            }
        }

        return None;
    }

    // Check user-defined functions
    for item in &items {
        if let TopLevel::FnDef(fd) = item
            && fd.name == word
        {
            let line = fd.line.saturating_sub(1) as u32;
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: Range {
                    start: Position { line, character: 0 },
                    end: Position { line, character: 0 },
                },
            }));
        }
    }

    // Check decisions
    for item in &items {
        if let TopLevel::Decision(decision) = item
            && decision.name == word
        {
            let line = decision.line.saturating_sub(1) as u32;
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: Range {
                    start: Position { line, character: 0 },
                    end: Position { line, character: 0 },
                },
            }));
        }
    }

    // Check user-defined types
    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            let (name, line) = match td {
                TypeDef::Sum { name, line, .. } => (name.as_str(), *line),
                TypeDef::Product { name, line, .. } => (name.as_str(), *line),
            };
            if name == word {
                let line = line.saturating_sub(1) as u32;
                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range: Range {
                        start: Position { line, character: 0 },
                        end: Position { line, character: 0 },
                    },
                }));
            }
        }
    }

    // Check bindings at top level
    for item in &items {
        if let TopLevel::Stmt(aver::ast::Stmt::Binding(name, _, _)) = item
            && name == word
            && let Some(line) = find_binding_line(source, name)
        {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: Range {
                    start: Position {
                        line: line as u32,
                        character: 0,
                    },
                    end: Position {
                        line: line as u32,
                        character: 0,
                    },
                },
            }));
        }
    }

    None
}

/// Find the definition of a type member (e.g., Circle in Shape.Circle).
fn find_type_member_def(
    items: &[TopLevel],
    type_name: &str,
    member: &str,
    uri: &Uri,
) -> Option<GotoDefinitionResponse> {
    for item in items {
        if let TopLevel::TypeDef(td) = item {
            match td {
                TypeDef::Sum {
                    name,
                    variants,
                    line,
                } if name == type_name => {
                    for (i, variant) in variants.iter().enumerate() {
                        if variant.name == member {
                            let variant_line = (*line + i) as u32;
                            return Some(GotoDefinitionResponse::Scalar(Location {
                                uri: uri.clone(),
                                range: Range {
                                    start: Position {
                                        line: variant_line,
                                        character: 0,
                                    },
                                    end: Position {
                                        line: variant_line,
                                        character: 0,
                                    },
                                },
                            }));
                        }
                    }
                }
                TypeDef::Product { name, line, .. } if name == type_name => {
                    let line = line.saturating_sub(1) as u32;
                    return Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range: Range {
                            start: Position { line, character: 0 },
                            end: Position { line, character: 0 },
                        },
                    }));
                }
                _ => {}
            }
        }
    }
    None
}

/// Find the 0-based line number of a top-level binding in source.
fn find_binding_line(source: &str, name: &str) -> Option<usize> {
    for (i, line) in source.lines().enumerate() {
        let trimmed = line.trim();
        if let Some(stripped) = trimmed.strip_prefix(name) {
            let rest = stripped.trim_start();
            if rest.starts_with('=') || rest.starts_with(':') {
                return Some(i);
            }
        }
    }
    None
}
