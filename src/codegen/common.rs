use std::collections::HashSet;

use crate::ast::{
    Expr, FnBody, FnDef, Spanned, Stmt, StrPart, TailCallData, TopLevel, TypeDef, TypeVariant,
    VerifyBlock, VerifyGivenDomain, VerifyKind,
};
use crate::codegen::CodegenContext;
use crate::types::Type;

/// Backend-neutral predicates on AST items — all three codegen backends
/// (Lean, Dafny, Rust) want the same view of "is this pure?",
/// "self-referencing type?", and "what's the name of this type def?".

/// A function is pure if it declares no effects and isn't `main`.
pub fn is_pure_fn(fd: &FnDef) -> bool {
    fd.effects.is_empty() && fd.name != "main"
}

/// True when the type definition mentions its own name somewhere in a
/// field or variant payload (recursive ADT).
pub fn is_recursive_type_def(td: &TypeDef) -> bool {
    match td {
        TypeDef::Sum { name, variants, .. } => is_recursive_sum(name, variants),
        TypeDef::Product { name, fields, .. } => is_recursive_product(name, fields),
    }
}

/// The declared name of a type definition.
pub fn type_def_name(td: &TypeDef) -> &str {
    match td {
        TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
    }
}

/// Granular variant of [`is_recursive_type_def`] taking a sum's
/// `(name, variants)` split — some backends already have the parts
/// separated and don't want to rebuild a `TypeDef` just to query.
pub fn is_recursive_sum(name: &str, variants: &[TypeVariant]) -> bool {
    variants
        .iter()
        .any(|v| v.fields.iter().any(|f| type_ref_contains(f, name)))
}

/// Granular variant of [`is_recursive_type_def`] for products.
pub fn is_recursive_product(name: &str, fields: &[(String, String)]) -> bool {
    fields.iter().any(|(_, ty)| type_ref_contains(ty, name))
}

fn type_ref_contains(annotation: &str, type_name: &str) -> bool {
    // Direct match or any generic position: List<Foo>, Option<Foo>,
    // Map<K, Foo>, (Foo, Bar), etc.
    annotation == type_name
        || annotation.contains(&format!("<{}", type_name))
        || annotation.contains(&format!("{}>", type_name))
        || annotation.contains(&format!(", {}", type_name))
        || annotation.contains(&format!("{},", type_name))
}

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

/// Oracle v1: how to materialise the oracle argument for an effectful
/// fn call in a law body.
///
/// - `LemmaBinding` — use the lemma-local identifier (`rnd`), matching
///   the `given` name. Correct for the universal lemma body.
/// - `SampleValue` — use the first Explicit domain value (the stub
///   fn's identifier, e.g. `stubConst`). Correct for the concrete
///   sample assertions where there's no lemma binding in scope and a
///   single domain value.
/// - `SampleCaseBinding(case_bindings)` — use the per-case binding
///   value (by `given.name`). Correct for sample theorems when the
///   domain has multiple values and each case substitutes a
///   different one (`given stub: Http.get = [httpDown, httpOk]`).
#[derive(Debug, Clone)]
pub(crate) enum OracleInjectionMode<'a> {
    LemmaBinding,
    #[allow(dead_code)]
    SampleValue,
    SampleCaseBinding(&'a [(String, crate::ast::Spanned<Expr>)]),
}

/// Oracle v1: rewrite any call to an effectful fn in a law body so
/// it targets the lifted signature — prepend `BranchPath.root()` (for
/// generative / gen+output effects) plus one argument per classified
/// non-output effect in the callee's signature.
///
/// Backend-agnostic — operates on AST + `CodegenContext`. Both the
/// Dafny and Lean backends call this before emitting the law body so
/// the law statement matches the lifted fn shape emitted alongside.
pub(crate) fn rewrite_effectful_calls_in_law(
    expr: &crate::ast::Spanned<Expr>,
    law: &crate::ast::VerifyLaw,
    ctx: &CodegenContext,
    mode: OracleInjectionMode,
) -> crate::ast::Spanned<Expr> {
    use crate::ast::{Spanned, VerifyGivenDomain};

    let injection_by_effect: std::collections::HashMap<String, Spanned<Expr>> = law
        .givens
        .iter()
        .filter_map(|g| {
            let arg_expr = match &mode {
                OracleInjectionMode::LemmaBinding => Spanned {
                    node: Expr::Ident(g.name.clone()),
                    line: expr.line,
                },
                OracleInjectionMode::SampleValue => match &g.domain {
                    VerifyGivenDomain::Explicit(vals) => vals.first().cloned()?,
                    _ => return None,
                },
                OracleInjectionMode::SampleCaseBinding(case_bindings) => case_bindings
                    .iter()
                    .find(|(name, _)| name == &g.name)
                    .map(|(_, v)| v.clone())?,
            };
            Some((g.type_name.clone(), arg_expr))
        })
        .collect();
    rewrite_effectful_call(expr, &injection_by_effect, ctx)
}

fn rewrite_effectful_call(
    expr: &crate::ast::Spanned<Expr>,
    injection_by_effect: &std::collections::HashMap<String, crate::ast::Spanned<Expr>>,
    ctx: &CodegenContext,
) -> crate::ast::Spanned<Expr> {
    use crate::ast::Spanned;
    use crate::types::checker::effect_classification::{EffectDimension, classify};

    match &expr.node {
        Expr::FnCall(callee, args) => {
            let rewritten_args: Vec<Spanned<Expr>> = args
                .iter()
                .map(|a| rewrite_effectful_call(a, injection_by_effect, ctx))
                .collect();
            let rewritten_callee =
                Box::new(rewrite_effectful_call(callee, injection_by_effect, ctx));

            let callee_name = match &callee.node {
                Expr::Ident(name) => Some(name.clone()),
                Expr::Resolved { name, .. } => Some(name.clone()),
                _ => None,
            };

            if let Some(name) = callee_name
                && let Some(fd) = ctx.fn_defs.iter().find(|fd| fd.name == name)
                && !fd.effects.is_empty()
                && fd
                    .effects
                    .iter()
                    .all(|e| crate::types::checker::effect_classification::is_classified(&e.node))
            {
                let mut injected: Vec<Spanned<Expr>> = Vec::new();
                let needs_path = fd.effects.iter().any(|e| {
                    matches!(
                        classify(&e.node).map(|c| c.dimension),
                        Some(EffectDimension::Generative | EffectDimension::GenerativeOutput)
                    )
                });
                if needs_path {
                    injected.push(Spanned {
                        // `BranchPath.Root` — nullary value
                        // constructor (PascalCase, no parens),
                        // symmetric with `Option.None`.
                        node: Expr::Attr(
                            Box::new(Spanned {
                                node: Expr::Ident("BranchPath".to_string()),
                                line: expr.line,
                            }),
                            "Root".to_string(),
                        ),
                        line: expr.line,
                    });
                }
                let mut seen = std::collections::HashSet::new();
                for e in &fd.effects {
                    if !seen.insert(e.node.clone()) {
                        continue;
                    }
                    let Some(c) = classify(&e.node) else { continue };
                    if matches!(c.dimension, EffectDimension::Output) {
                        continue;
                    }
                    if let Some(inj) = injection_by_effect.get(&e.node) {
                        injected.push(inj.clone());
                    }
                }
                injected.extend(rewritten_args);
                return Spanned {
                    node: Expr::FnCall(rewritten_callee, injected),
                    line: expr.line,
                };
            }

            Spanned {
                node: Expr::FnCall(rewritten_callee, rewritten_args),
                line: expr.line,
            }
        }
        Expr::BinOp(op, l, r) => Spanned {
            node: Expr::BinOp(
                *op,
                Box::new(rewrite_effectful_call(l, injection_by_effect, ctx)),
                Box::new(rewrite_effectful_call(r, injection_by_effect, ctx)),
            ),
            line: expr.line,
        },
        Expr::Tuple(items) => Spanned {
            node: Expr::Tuple(
                items
                    .iter()
                    .map(|i| rewrite_effectful_call(i, injection_by_effect, ctx))
                    .collect(),
            ),
            line: expr.line,
        },
        _ => expr.clone(),
    }
}

/// Oracle v1: set of user fn names that are reachable from any verify
/// block — directly (`verify f ...`) or through the call graph (fn
/// body of a reachable fn mentions them). Used by proof backends to
/// skip emission of effectful fns that nobody verifies. Dead code in
/// a proof output isn't just ugly — a non-terminating effectful fn
/// (e.g. a REPL loop) will make Lean reject the whole module because
/// it can't prove termination for a fn with no decreasing argument.
/// If the user never asked for a proof about that fn, don't force
/// the backend to invent one.
pub(crate) fn verify_reachable_fn_names(items: &[TopLevel]) -> HashSet<String> {
    let mut reachable: HashSet<String> = HashSet::new();
    for item in items {
        if let TopLevel::Verify(vb) = item {
            collect_verify_block_refs(vb, &mut reachable);
        }
    }
    // Fixed-point closure through the call graph.
    loop {
        let mut changed = false;
        for item in items {
            if let TopLevel::FnDef(fd) = item
                && reachable.contains(&fd.name)
            {
                let mut called = HashSet::new();
                collect_called_idents_in_body(&fd.body, &mut called);
                for name in called {
                    if reachable.insert(name) {
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    reachable
}

fn collect_verify_block_refs(vb: &VerifyBlock, out: &mut HashSet<String>) {
    out.insert(vb.fn_name.clone());
    for (lhs, rhs) in &vb.cases {
        collect_called_idents(lhs, out);
        collect_called_idents(rhs, out);
    }
    if let VerifyKind::Law(law) = &vb.kind {
        collect_called_idents(&law.lhs, out);
        collect_called_idents(&law.rhs, out);
        if let Some(when) = &law.when {
            collect_called_idents(when, out);
        }
        for given in &law.givens {
            if let VerifyGivenDomain::Explicit(values) = &given.domain {
                for v in values {
                    collect_called_idents(v, out);
                }
            }
        }
    }
    for given in &vb.cases_givens {
        if let VerifyGivenDomain::Explicit(values) = &given.domain {
            for v in values {
                collect_called_idents(v, out);
            }
        }
    }
}

fn collect_called_idents_in_body(body: &FnBody, out: &mut HashSet<String>) {
    for stmt in body.stmts() {
        match stmt {
            Stmt::Binding(_, _, e) | Stmt::Expr(e) => collect_called_idents(e, out),
        }
    }
}

fn collect_called_idents(expr: &Spanned<Expr>, out: &mut HashSet<String>) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Expr::Ident(name) | Expr::Resolved { name, .. } = &callee.node {
                out.insert(name.clone());
            } else {
                collect_called_idents(callee, out);
            }
            for a in args {
                collect_called_idents(a, out);
            }
        }
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            out.insert(target.clone());
            for a in args {
                collect_called_idents(a, out);
            }
        }
        Expr::Ident(name) | Expr::Resolved { name, .. } => {
            out.insert(name.clone());
        }
        Expr::BinOp(_, l, r) => {
            collect_called_idents(l, out);
            collect_called_idents(r, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_called_idents(subject, out);
            for arm in arms {
                collect_called_idents(&arm.body, out);
            }
        }
        Expr::ErrorProp(inner) | Expr::Attr(inner, _) => {
            collect_called_idents(inner, out);
        }
        Expr::Constructor(_, Some(inner)) => {
            collect_called_idents(inner, out);
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    collect_called_idents(inner, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for i in items {
                collect_called_idents(i, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_called_idents(k, out);
                collect_called_idents(v, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_called_idents(v, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_called_idents(base, out);
            for (_, v) in updates {
                collect_called_idents(v, out);
            }
        }
        Expr::Literal(_) | Expr::Constructor(_, None) => {}
    }
}
