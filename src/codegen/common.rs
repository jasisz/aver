use crate::ast::{Expr, TypeDef};
use crate::codegen::CodegenContext;
use crate::types::Type;

/// Check if a name is a user-defined type (sum or product), including modules.
pub(crate) fn is_user_type(name: &str, ctx: &CodegenContext) -> bool {
    let check_td = |td: &TypeDef| match td {
        TypeDef::Sum { name: n, .. } => n == name,
        TypeDef::Product { name: n, .. } => n == name,
    };
    ctx.type_defs.iter().any(check_td)
        || ctx.modules.iter().any(|m| m.type_defs.iter().any(check_td))
}

/// Resolve a module-qualified dotted name to `(module_prefix, local_suffix)`.
/// Example: `Models.User.nameById` -> `("Models.User", "nameById")`.
pub(crate) fn resolve_module_call<'a>(
    dotted_name: &'a str,
    ctx: &'a CodegenContext,
) -> Option<(&'a str, &'a str)> {
    let mut best: Option<&str> = None;
    for prefix in &ctx.module_prefixes {
        let dotted_prefix = format!("{}.", prefix);
        if dotted_name.starts_with(&dotted_prefix) && best.is_none_or(|b| prefix.len() > b.len()) {
            best = Some(prefix.as_str());
        }
    }
    best.map(|prefix| (prefix, &dotted_name[prefix.len() + 1..]))
}

pub(crate) fn module_prefix_to_rust_segments(prefix: &str) -> Vec<String> {
    prefix.split('.').map(module_segment_to_rust).collect()
}

pub(crate) fn module_prefix_to_rust_path(prefix: &str) -> String {
    format!(
        "crate::aver_generated::{}",
        module_prefix_to_rust_segments(prefix).join("::")
    )
}

fn module_segment_to_rust(segment: &str) -> String {
    let chars = segment.chars().collect::<Vec<_>>();
    let mut out = String::new();

    for (idx, ch) in chars.iter().enumerate() {
        if ch.is_ascii_alphanumeric() {
            if ch.is_ascii_uppercase() {
                let prev_is_lower_or_digit = idx > 0
                    && (chars[idx - 1].is_ascii_lowercase() || chars[idx - 1].is_ascii_digit());
                let next_is_lower = chars
                    .get(idx + 1)
                    .is_some_and(|next| next.is_ascii_lowercase());
                if idx > 0 && (prev_is_lower_or_digit || next_is_lower) && !out.ends_with('_') {
                    out.push('_');
                }
                out.push(ch.to_ascii_lowercase());
            } else {
                out.push(ch.to_ascii_lowercase());
            }
        } else if !out.ends_with('_') {
            out.push('_');
        }
    }

    let trimmed = out.trim_matches('_');
    let mut normalized = if trimmed.is_empty() {
        "module".to_string()
    } else {
        trimmed.to_string()
    };

    if matches!(
        normalized.as_str(),
        "as" | "break"
            | "const"
            | "continue"
            | "crate"
            | "else"
            | "enum"
            | "extern"
            | "false"
            | "fn"
            | "for"
            | "if"
            | "impl"
            | "in"
            | "let"
            | "loop"
            | "match"
            | "mod"
            | "move"
            | "mut"
            | "pub"
            | "ref"
            | "return"
            | "self"
            | "Self"
            | "static"
            | "struct"
            | "super"
            | "trait"
            | "true"
            | "type"
            | "unsafe"
            | "use"
            | "where"
            | "while"
    ) {
        normalized.push_str("_mod");
    }

    normalized
}

/// Split a type annotation string at top-level delimiters (not inside `<>` or `()`).
///
/// Used by multiple backends to parse Aver type annotation strings like
/// `"Map<String, List<Int>>"` or `"(String, Int)"`.
pub(crate) fn split_type_params(s: &str, delim: char) -> Vec<String> {
    let mut parts = Vec::new();
    let mut depth = 0usize;
    let mut current = String::new();
    for ch in s.chars() {
        match ch {
            '<' | '(' => {
                depth += 1;
                current.push(ch);
            }
            '>' | ')' => {
                depth = depth.saturating_sub(1);
                current.push(ch);
            }
            _ if ch == delim && depth == 0 => {
                parts.push(current.trim().to_string());
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    let rest = current.trim().to_string();
    if !rest.is_empty() {
        parts.push(rest);
    }
    parts
}

/// Escape a string literal for target languages that use C-style escapes.
/// Handles `\\`, `\"`, `\n`, `\r`, `\t`, `\0`,
/// and generic control characters as `\xHH` (Lean/Rust) or `\uHHHH` (Dafny).
///
/// Use `unicode_escapes = true` for Dafny (which needs `\uHHHH`),
/// `false` for Lean/Rust (which accept `\xHH`).
pub(crate) fn escape_string_literal_ext(s: &str, unicode_escapes: bool) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            c if c.is_control() => {
                if unicode_escapes {
                    // Dafny 4+ with Unicode chars enabled: \U{HHHHHH}
                    out.push_str(&format!("\\U{{{:06x}}}", c as u32));
                } else {
                    out.push_str(&format!("\\x{:02x}", c as u32));
                }
            }
            c => out.push(c),
        }
    }
    out
}

/// Convenience: escape with `\xHH` for control chars (Lean, Rust).
pub(crate) fn escape_string_literal(s: &str) -> String {
    escape_string_literal_ext(s, false)
}

/// Convenience: escape with `\u{HHHH}` for control chars (Dafny).
pub(crate) fn escape_string_literal_unicode(s: &str) -> String {
    escape_string_literal_ext(s, true)
}

/// Parse an Aver type annotation string into the internal `Type` enum.
///
/// Thin wrapper around `types::parse_type_str` for use in codegen modules.
pub(crate) fn parse_type_annotation(ann: &str) -> Type {
    crate::types::parse_type_str(ann)
}

/// Check if a `Type` represents a set pattern: `Map<T, Unit>`.
///
/// Aver has no dedicated `Set` type — the idiomatic way to express a set
/// is `Map<T, Unit>`. Codegen backends can lower this to the target
/// language's native set type (Dafny `set<T>`, Lean `Finset T`, etc.).
pub(crate) fn is_set_type(ty: &Type) -> bool {
    matches!(ty, Type::Map(_, v) if matches!(v.as_ref(), Type::Unit))
}

/// Check if a type annotation string represents a set (`Map<T, Unit>`).
pub(crate) fn is_set_annotation(ann: &str) -> bool {
    is_set_type(&parse_type_annotation(ann))
}

/// Check if an expression is a compile-time Unit literal.
pub(crate) fn is_unit_expr(expr: &crate::ast::Expr) -> bool {
    matches!(expr, crate::ast::Expr::Literal(crate::ast::Literal::Unit))
}

/// Escape an Aver identifier if it collides with a target language reserved word.
///
/// `affix` is appended as a suffix (e.g. `"_"` for Dafny, `"'"` for Lean).
/// For prefix escaping (e.g. Rust `r#`), use [`escape_reserved_word_prefix`].
pub(crate) fn escape_reserved_word(name: &str, reserved: &[&str], suffix: &str) -> String {
    if reserved.contains(&name) {
        format!("{}{}", name, suffix)
    } else {
        name.to_string()
    }
}

/// Like [`escape_reserved_word`] but prepends a prefix instead of appending a suffix.
/// Used for Rust's `r#keyword` raw identifier syntax.
pub(crate) fn escape_reserved_word_prefix(name: &str, reserved: &[&str], prefix: &str) -> String {
    if reserved.contains(&name) {
        format!("{}{}", prefix, name)
    } else {
        name.to_string()
    }
}

/// Convert first character of a string to lowercase.
///
/// Used when converting PascalCase type/variant names to camelCase identifiers.
pub(crate) fn to_lower_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_lowercase().to_string() + chars.as_str(),
    }
}

/// Convert an attribute chain into dotted name.
/// Example: `Console.print` -> `Some("Console.print")`.
pub(crate) fn expr_to_dotted_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(obj, field) => {
            let head = expr_to_dotted_name(obj)?;
            Some(format!("{}.{}", head, field))
        }
        _ => None,
    }
}
