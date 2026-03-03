use crate::ast::{Expr, TypeDef};
use crate::codegen::CodegenContext;

/// Check if a name is a user-defined type (sum or product), including modules.
pub(crate) fn is_user_type(name: &str, ctx: &CodegenContext) -> bool {
    let check_td = |td: &TypeDef| match td {
        TypeDef::Sum { name: n, .. } => n == name,
        TypeDef::Product { name: n, .. } => n == name,
    };
    ctx.type_defs.iter().any(check_td)
        || ctx.modules.iter().any(|m| m.type_defs.iter().any(check_td))
}

/// Resolve a module-qualified dotted name to local inlined symbol name.
/// Example: `Examples.Fibonacci.fib` -> `fib`.
pub(crate) fn resolve_module_call(dotted_name: &str, ctx: &CodegenContext) -> Option<String> {
    let mut best: Option<&str> = None;
    for prefix in &ctx.module_prefixes {
        let dotted_prefix = format!("{}.", prefix);
        if dotted_name.starts_with(&dotted_prefix) && best.is_none_or(|b| prefix.len() > b.len()) {
            best = Some(prefix.as_str());
        }
    }
    best.map(|prefix| dotted_name[prefix.len() + 1..].to_string())
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
