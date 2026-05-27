/// Aver patterns → Lean 4 pattern strings.
use super::shared::to_lower_first;
use crate::ast::Literal;
use crate::ir::hir::{BuiltinCtor, ResolvedCtor, ResolvedPattern};

/// Emit a Lean 4 pattern from a resolved Aver Pattern.
pub fn emit_pattern(pat: &ResolvedPattern) -> String {
    match pat {
        ResolvedPattern::Wildcard => "_".to_string(),
        ResolvedPattern::Literal(lit) => emit_literal_pattern(lit),
        ResolvedPattern::Ident(name) => super::expr::aver_name_to_lean(name),
        ResolvedPattern::EmptyList => "[]".to_string(),
        ResolvedPattern::Cons(head, tail) => {
            format!(
                "{} :: {}",
                super::expr::aver_name_to_lean(head),
                super::expr::aver_name_to_lean(tail)
            )
        }
        ResolvedPattern::Tuple(pats) => {
            let parts: Vec<String> = pats.iter().map(emit_pattern).collect();
            format!("({})", parts.join(", "))
        }
        ResolvedPattern::Ctor(ctor, bindings) => emit_ctor_pattern(ctor, bindings),
    }
}

fn emit_literal_pattern(lit: &Literal) -> String {
    match lit {
        Literal::Int(i) => format!("{}", i),
        Literal::Float(f) => format!("{}", f),
        Literal::Str(s) => format!("{:?}", s),
        Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn emit_ctor_pattern(ctor: &ResolvedCtor, bindings: &[String]) -> String {
    match ctor {
        ResolvedCtor::Builtin(BuiltinCtor::ResultOk) => {
            if bindings.is_empty() {
                ".ok".to_string()
            } else {
                let parts: Vec<String> = bindings
                    .iter()
                    .map(|b| super::expr::aver_name_to_lean(b))
                    .collect();
                format!(".ok {}", parts.join(" "))
            }
        }
        ResolvedCtor::Builtin(BuiltinCtor::ResultErr) => {
            if bindings.is_empty() {
                ".error".to_string()
            } else {
                let parts: Vec<String> = bindings
                    .iter()
                    .map(|b| super::expr::aver_name_to_lean(b))
                    .collect();
                format!(".error {}", parts.join(" "))
            }
        }
        ResolvedCtor::Builtin(BuiltinCtor::OptionSome) => {
            if bindings.is_empty() {
                ".some".to_string()
            } else {
                let parts: Vec<String> = bindings
                    .iter()
                    .map(|b| super::expr::aver_name_to_lean(b))
                    .collect();
                format!(".some {}", parts.join(" "))
            }
        }
        ResolvedCtor::Builtin(BuiltinCtor::OptionNone) => ".none".to_string(),
        ResolvedCtor::User { name, .. } => {
            // Lean convention: lowercase constructor names. The
            // `name` field of `ResolvedCtor::User` is the bare
            // variant name (e.g. `"Point"` for `Shape.Point`); the
            // owning type's emit context already namespaces the
            // ctor at the call site, so no qualified-name handling
            // is needed here.
            let lean_variant = to_lower_first(name);
            if bindings.is_empty() {
                format!(".{}", lean_variant)
            } else {
                let parts: Vec<String> = bindings
                    .iter()
                    .map(|b| super::expr::aver_name_to_lean(b))
                    .collect();
                format!(".{} {}", lean_variant, parts.join(" "))
            }
        }
        ResolvedCtor::Unresolved { name } => {
            // Typecheck-rejected program reached the renderer.
            // Surface the source name in a Lean-shaped placeholder so
            // the user-facing error from the typecheck stage stays
            // the load-bearing diagnostic.
            if bindings.is_empty() {
                format!("⟨⟩ /- {} -/", name)
            } else {
                let parts: Vec<String> = bindings
                    .iter()
                    .map(|b| super::expr::aver_name_to_lean(b))
                    .collect();
                format!("⟨{}⟩ /- {} -/", parts.join(", "), name)
            }
        }
    }
}
