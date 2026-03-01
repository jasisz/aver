use tower_lsp_server::ls_types::{Hover, HoverContents, MarkupContent, MarkupKind};

use aver::ast::{FnBody, FnDef, Stmt, TopLevel, TypeDef};

use crate::completion;
use crate::modules;

/// Try to produce hover info for a word at the cursor position.
pub fn hover_for_word(word: &str, source: &str, base_dir: Option<&str>) -> Option<Hover> {
    // Check if it's a qualified name like "List.map" or "Examples.Redis.set"
    if let Some(last_dot) = word.rfind('.') {
        let prefix = &word[..last_dot];
        let member = &word[last_dot + 1..];

        // Built-in namespaces (single segment prefix like "List", "Console")
        let completions = completion::namespace_completions(prefix);
        if let Some(item) = completions.iter().find(|c| c.label == member) {
            let detail = item.detail.as_deref().unwrap_or("");
            let content = format!("```aver\n{}.{}: {}\n```", prefix, member, detail);
            return Some(make_hover(content));
        }

        // Cross-module: "Examples.Redis.set" → prefix="Examples.Redis", member="set"
        if let Some(base) = base_dir {
            let deps = modules::resolve_dependencies(source, base);
            for dep in &deps {
                let dep_short = dep.name.rsplit('.').next().unwrap_or(&dep.name);
                if dep.name == prefix || dep_short == prefix {
                    for fd in modules::exported_fns(dep) {
                        if fd.name == member {
                            let content = build_fn_hover(fd, &dep.source);
                            return Some(make_hover(format!(
                                "{}\n\n*from module {}*",
                                content, dep.name
                            )));
                        }
                    }
                }
            }
        }

        // User-defined types (single segment prefix like "Shape")
        let items = completion::parse_items(source);
        for item in &items {
            if let TopLevel::TypeDef(td) = item {
                match td {
                    TypeDef::Sum { name, variants, .. } if name == prefix => {
                        if let Some(v) = variants.iter().find(|v| v.name == member) {
                            let detail = if v.fields.is_empty() {
                                name.clone()
                            } else {
                                format!("fn({}) -> {}", v.fields.join(", "), name)
                            };
                            let content = format!("```aver\n{}.{}: {}\n```", name, member, detail);
                            return Some(make_hover(content));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Check if it's a namespace name
    let namespaces = completion::all_namespaces();
    if let Some(ns) = namespaces.iter().find(|c| c.label == word) {
        let detail = ns.detail.as_deref().unwrap_or("");
        let content = format!("**{}** — {}", word, detail);
        return Some(make_hover(content));
    }

    // Check user-defined functions — parse source for rich info
    let items = completion::parse_items(source);
    for item in &items {
        if let TopLevel::FnDef(fd) = item {
            if fd.name == word {
                let content = build_fn_hover(fd, source);
                return Some(make_hover(content));
            }
        }
    }

    // Check user-defined types
    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            match td {
                TypeDef::Sum { name, variants, .. } if name == word => {
                    let variant_strs: Vec<String> = variants
                        .iter()
                        .map(|v| {
                            if v.fields.is_empty() {
                                format!("  {}", v.name)
                            } else {
                                format!("  {}({})", v.name, v.fields.join(", "))
                            }
                        })
                        .collect();
                    let content = format!(
                        "```aver\ntype {}\n{}\n```",
                        name,
                        variant_strs.join("\n")
                    );
                    return Some(make_hover(content));
                }
                TypeDef::Product { name, fields, .. } if name == word => {
                    let field_strs: Vec<String> = fields
                        .iter()
                        .map(|(fname, ftype)| format!("  {}: {}", fname, ftype))
                        .collect();
                    let content = format!(
                        "```aver\nrecord {}\n{}\n```",
                        name,
                        field_strs.join("\n")
                    );
                    return Some(make_hover(content));
                }
                _ => {}
            }
        }
    }

    // Check variable bindings — show type annotation if present
    for item in &items {
        match item {
            TopLevel::Stmt(Stmt::Binding(name, type_ann, _)) if name == word => {
                let ty = type_ann.as_deref().unwrap_or("(inferred)");
                let content = format!("```aver\n{}: {}\n```", name, ty);
                return Some(make_hover(content));
            }
            TopLevel::FnDef(fd) => {
                // Check bindings inside function bodies
                if let FnBody::Block(stmts) = fd.body.as_ref() {
                    for stmt in stmts {
                        if let Stmt::Binding(name, type_ann, _) = stmt {
                            if name == word {
                                let ty = type_ann.as_deref().unwrap_or("(inferred)");
                                let content =
                                    format!("```aver\n{}: {}\n```\n*in {}()*", name, ty, fd.name);
                                return Some(make_hover(content));
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    None
}

/// Max lines to show in hover before truncating.
const MAX_HOVER_LINES: usize = 12;

/// Build rich hover content for a user-defined function.
fn build_fn_hover(fd: &FnDef, source: &str) -> String {
    if let Some(fn_source) = extract_fn_source(fd, source) {
        let lines: Vec<&str> = fn_source.lines().collect();
        if lines.len() <= MAX_HOVER_LINES {
            // Trivial — show full function source
            format!("```aver\n{}\n```", fn_source)
        } else {
            // Long — show first lines + truncation indicator
            let preview: Vec<&str> = lines[..MAX_HOVER_LINES].to_vec();
            let remaining = lines.len() - MAX_HOVER_LINES;
            format!(
                "```aver\n{}\n  // ... ({} more lines)\n```",
                preview.join("\n"),
                remaining
            )
        }
    } else {
        // Fallback: reconstructed signature + docstring
        let mut parts = Vec::new();
        parts.push(format!("```aver\n{}\n```", format_signature(fd)));
        if let Some(desc) = &fd.desc {
            parts.push(format!("*{}*", desc));
        }
        parts.join("\n\n")
    }
}

/// Format a reconstructed function signature (fallback only).
fn format_signature(fd: &FnDef) -> String {
    let params: Vec<String> = fd
        .params
        .iter()
        .map(|(name, ty)| {
            if ty.is_empty() {
                name.clone()
            } else {
                format!("{}: {}", name, ty)
            }
        })
        .collect();

    let mut sig = format!("fn {}({})", fd.name, params.join(", "));

    if !fd.return_type.is_empty() {
        sig.push_str(&format!(" -> {}", fd.return_type));
    }

    if !fd.effects.is_empty() {
        sig.push_str(&format!(" ! [{}]", fd.effects.join(", ")));
    }

    sig
}

/// Extract the full function source (header + ? + ! + body) from the original text.
fn extract_fn_source(fd: &FnDef, source: &str) -> Option<String> {
    let lines: Vec<&str> = source.lines().collect();
    let start = fd.line; // 1-based
    if start == 0 || start > lines.len() {
        return None;
    }

    let header_line = lines[start - 1];
    let header_indent = header_line.len() - header_line.trim_start().len();

    // Collect the fn header line + all subsequent indented lines
    let mut fn_lines = vec![header_line];
    let mut i = start; // next line (0-based index)

    while i < lines.len() {
        let line = lines[i];
        if line.trim().is_empty() {
            // Empty line — include only if more body follows
            let has_more = (i + 1 < lines.len()) && {
                let next = lines[i + 1];
                !next.trim().is_empty()
                    && (next.len() - next.trim_start().len()) > header_indent
            };
            if has_more {
                fn_lines.push(line);
            } else {
                break;
            }
        } else {
            let indent = line.len() - line.trim_start().len();
            if indent <= header_indent {
                break;
            }
            fn_lines.push(line);
        }
        i += 1;
    }

    if fn_lines.len() <= 1 {
        // Expression body on same line: `fn foo(x) = expr`
        // Already captured in header
        match &*fd.body {
            FnBody::Expr(_) => return Some(fn_lines.join("\n")),
            _ => return None,
        }
    }

    Some(fn_lines.join("\n"))
}

fn make_hover(content: String) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    }
}

/// Extract the word at a given position from source text.
/// Handles dotted names like "List.map".
pub fn word_at_position(source: &str, line: usize, character: usize) -> Option<String> {
    let target_line = source.lines().nth(line)?;
    if character > target_line.len() {
        return None;
    }

    let bytes = target_line.as_bytes();
    let mut start = character;
    while start > 0 && is_word_char(bytes[start - 1]) {
        start -= 1;
    }
    let mut end = character;
    while end < bytes.len() && is_word_char(bytes[end]) {
        end += 1;
    }

    if start == end {
        return None;
    }

    Some(target_line[start..end].to_string())
}

fn is_word_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'.'
}
