use crate::ast::{Expr, FnDef, TypeDef};
use crate::codegen::{CodegenContext, ModuleInfo};
use crate::types::Type;

// ── Symbol resolution structs ────────────────────────────────────────────

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) struct CodegenFnSymbol {
    pub name: String,
    pub module_prefix: Option<String>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) struct CodegenTypeSymbol {
    pub name: String,
    pub module_prefix: Option<String>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) struct CodegenConstructorSymbol {
    /// The type name that owns this constructor.
    pub type_name: String,
    /// The variant/constructor name.
    pub constructor_name: String,
    /// Module prefix where this type is defined, if any.
    pub module_prefix: Option<String>,
}

// ── Visibility-aware resolution ──────────────────────────────────────────

/// Find the `ModuleInfo` for a given prefix.
pub(crate) fn find_module<'a>(prefix: &str, ctx: &'a CodegenContext) -> Option<&'a ModuleInfo> {
    ctx.modules.iter().find(|m| m.prefix == prefix)
}

/// Get the list of modules visible from `current_module_prefix`.
/// Includes the module itself and its declared `depends`.
pub(crate) fn visible_dependency_modules<'a>(
    current_module_prefix: Option<&str>,
    ctx: &'a CodegenContext,
) -> Vec<&'a ModuleInfo> {
    match current_module_prefix {
        Some(prefix) => {
            let mut visible = Vec::new();
            if let Some(module) = find_module(prefix, ctx) {
                // Add direct dependencies
                for dep in &module.depends {
                    if let Some(dep_mod) = find_module(dep, ctx) {
                        visible.push(dep_mod);
                    }
                }
            }
            visible
        }
        // Root module: no prefix filtering, all modules visible (see fallback below)
        None => Vec::new(),
    }
}

/// Resolve a user-defined type by name, respecting module visibility.
///
/// Search order:
/// 1. Root type_defs (entry module)
/// 2. Visible dependency modules (from current_module_prefix)
/// 3. **Fallback**: if current_module_prefix is None (root/entry module),
///    search ALL modules — preserving old `is_user_type` behavior for self-hosted compilation.
pub(crate) fn resolve_codegen_type(
    name: &str,
    current_module_prefix: Option<&str>,
    ctx: &CodegenContext,
) -> Option<CodegenTypeSymbol> {
    let check_td = |td: &TypeDef| match td {
        TypeDef::Sum { name: n, .. } => n == name,
        TypeDef::Product { name: n, .. } => n == name,
    };

    // 1. Root type_defs
    if ctx.type_defs.iter().any(check_td) {
        return Some(CodegenTypeSymbol {
            name: name.to_string(),
            module_prefix: None,
        });
    }

    // 2. Current module's own type_defs (if we're inside a dependent module)
    if let Some(prefix) = current_module_prefix
        && let Some(module) = find_module(prefix, ctx)
        && module.type_defs.iter().any(check_td)
    {
        return Some(CodegenTypeSymbol {
            name: name.to_string(),
            module_prefix: Some(prefix.to_string()),
        });
    }

    // 3. Visible dependency modules
    for dep_mod in visible_dependency_modules(current_module_prefix, ctx) {
        if dep_mod.type_defs.iter().any(check_td) {
            return Some(CodegenTypeSymbol {
                name: name.to_string(),
                module_prefix: Some(dep_mod.prefix.clone()),
            });
        }
    }

    // 4. Fallback for root module (prefix=None): search ALL modules
    if current_module_prefix.is_none() {
        for module in &ctx.modules {
            if module.type_defs.iter().any(check_td) {
                return Some(CodegenTypeSymbol {
                    name: name.to_string(),
                    module_prefix: Some(module.prefix.clone()),
                });
            }
        }
    }

    None
}

/// Resolve a user-defined function by name, respecting module visibility.
///
/// Same fallback pattern as `resolve_codegen_type`: when `current_module_prefix`
/// is None, searches all modules after checking root fn_defs.
#[allow(dead_code)]
pub(crate) fn resolve_codegen_function(
    name: &str,
    current_module_prefix: Option<&str>,
    ctx: &CodegenContext,
) -> Option<CodegenFnSymbol> {
    // 1. Root fn_defs and extra_fn_defs
    if ctx.fn_defs.iter().any(|fd| fd.name == name)
        || ctx.extra_fn_defs.iter().any(|fd| fd.name == name)
    {
        return Some(CodegenFnSymbol {
            name: name.to_string(),
            module_prefix: None,
        });
    }

    // 2. Current module's own fn_defs
    if let Some(prefix) = current_module_prefix
        && let Some(module) = find_module(prefix, ctx)
        && module.fn_defs.iter().any(|fd| fd.name == name)
    {
        return Some(CodegenFnSymbol {
            name: name.to_string(),
            module_prefix: Some(prefix.to_string()),
        });
    }

    // 3. Visible dependency modules
    for dep_mod in visible_dependency_modules(current_module_prefix, ctx) {
        if dep_mod.fn_defs.iter().any(|fd| fd.name == name) {
            return Some(CodegenFnSymbol {
                name: name.to_string(),
                module_prefix: Some(dep_mod.prefix.clone()),
            });
        }
    }

    // 4. Fallback for root module (prefix=None): search ALL modules
    if current_module_prefix.is_none() {
        for module in &ctx.modules {
            if module.fn_defs.iter().any(|fd| fd.name == name) {
                return Some(CodegenFnSymbol {
                    name: name.to_string(),
                    module_prefix: Some(module.prefix.clone()),
                });
            }
        }
    }

    None
}

/// Resolve a constructor (variant) by type name and variant name.
///
/// Same fallback pattern: prefix=None searches all modules.
#[allow(dead_code)]
pub(crate) fn resolve_codegen_constructor(
    type_name: &str,
    constructor_name: &str,
    current_module_prefix: Option<&str>,
    ctx: &CodegenContext,
) -> Option<CodegenConstructorSymbol> {
    let check_td = |td: &TypeDef| match td {
        TypeDef::Sum { name, variants, .. } => {
            name == type_name && variants.iter().any(|v| v.name == constructor_name)
        }
        TypeDef::Product { name, .. } => name == type_name && constructor_name.is_empty(),
    };

    // 1. Root type_defs
    if ctx.type_defs.iter().any(check_td) {
        return Some(CodegenConstructorSymbol {
            type_name: type_name.to_string(),
            constructor_name: constructor_name.to_string(),
            module_prefix: None,
        });
    }

    // 2. Current module's own type_defs
    if let Some(prefix) = current_module_prefix
        && let Some(module) = find_module(prefix, ctx)
        && module.type_defs.iter().any(check_td)
    {
        return Some(CodegenConstructorSymbol {
            type_name: type_name.to_string(),
            constructor_name: constructor_name.to_string(),
            module_prefix: Some(prefix.to_string()),
        });
    }

    // 3. Visible dependency modules
    for dep_mod in visible_dependency_modules(current_module_prefix, ctx) {
        if dep_mod.type_defs.iter().any(check_td) {
            return Some(CodegenConstructorSymbol {
                type_name: type_name.to_string(),
                constructor_name: constructor_name.to_string(),
                module_prefix: Some(dep_mod.prefix.clone()),
            });
        }
    }

    // 4. Fallback for root module (prefix=None): search ALL modules
    if current_module_prefix.is_none() {
        for module in &ctx.modules {
            if module.type_defs.iter().any(check_td) {
                return Some(CodegenConstructorSymbol {
                    type_name: type_name.to_string(),
                    constructor_name: constructor_name.to_string(),
                    module_prefix: Some(module.prefix.clone()),
                });
            }
        }
    }

    None
}

/// Check if a name is a user-defined type, respecting module visibility.
///
/// Drop-in replacement for `is_user_type` that accepts `current_module_prefix`.
/// When prefix is None (root module), searches everything (same as old behavior).
pub(crate) fn is_user_type_in_ctx(
    name: &str,
    current_module_prefix: Option<&str>,
    ctx: &CodegenContext,
) -> bool {
    resolve_codegen_type(name, current_module_prefix, ctx).is_some()
}

/// Check if a name is a user-defined type (sum or product), including modules.
///
/// Legacy function: searches everything unconditionally.
/// Equivalent to `is_user_type_in_ctx(name, None, ctx)`.
pub(crate) fn is_user_type(name: &str, ctx: &CodegenContext) -> bool {
    is_user_type_in_ctx(name, None, ctx)
}

/// Get the record fields for a user-defined product type.
#[allow(dead_code)]
pub(crate) fn record_fields_for_type(
    name: &str,
    current_module_prefix: Option<&str>,
    ctx: &CodegenContext,
) -> Option<Vec<(String, String)>> {
    let check_product = |td: &TypeDef| -> Option<Vec<(String, String)>> {
        match td {
            TypeDef::Product {
                name: n, fields, ..
            } if n == name => Some(fields.clone()),
            _ => None,
        }
    };

    // 1. Root type_defs
    for td in &ctx.type_defs {
        if let Some(fields) = check_product(td) {
            return Some(fields);
        }
    }

    // 2. Current module
    if let Some(prefix) = current_module_prefix
        && let Some(module) = find_module(prefix, ctx)
    {
        for td in &module.type_defs {
            if let Some(fields) = check_product(td) {
                return Some(fields);
            }
        }
    }

    // 3. Visible dependencies
    for dep_mod in visible_dependency_modules(current_module_prefix, ctx) {
        for td in &dep_mod.type_defs {
            if let Some(fields) = check_product(td) {
                return Some(fields);
            }
        }
    }

    // 4. Fallback for root module
    if current_module_prefix.is_none() {
        for module in &ctx.modules {
            for td in &module.type_defs {
                if let Some(fields) = check_product(td) {
                    return Some(fields);
                }
            }
        }
    }

    None
}

/// Qualify a function name with its module prefix for codegen output.
#[allow(dead_code)]
pub(crate) fn qualify_codegen_function_name(
    name: &str,
    current_module_prefix: Option<&str>,
    ctx: &CodegenContext,
) -> String {
    if let Some(sym) = resolve_codegen_function(name, current_module_prefix, ctx) {
        match sym.module_prefix {
            Some(prefix) => format!("{}.{}", prefix, sym.name),
            None => sym.name,
        }
    } else {
        name.to_string()
    }
}

/// Look up the function signature from a FnDef.
pub(crate) fn fn_sig_from_def(
    fd: &FnDef,
) -> (Vec<crate::types::Type>, crate::types::Type, Vec<String>) {
    let param_types: Vec<crate::types::Type> = fd
        .params
        .iter()
        .map(|(_, type_ann)| crate::types::parse_type_str(type_ann))
        .collect();
    let return_type = if fd.return_type.is_empty() {
        crate::types::Type::Unit
    } else {
        crate::types::parse_type_str(&fd.return_type)
    };
    let effects: Vec<String> = fd.effects.iter().map(|e| e.node.clone()).collect();
    (param_types, return_type, effects)
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

/// Check if a spanned expression is a compile-time Unit literal.
pub(crate) fn is_unit_expr_spanned(expr: &crate::ast::Spanned<crate::ast::Expr>) -> bool {
    is_unit_expr(&expr.node)
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
    crate::ir::expr_to_dotted_name(expr)
}
