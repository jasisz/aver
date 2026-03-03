/// Aver patterns → Lean 4 pattern strings.
use super::shared::to_lower_first;
use crate::ast::*;

/// Emit a Lean 4 pattern from an Aver Pattern.
pub fn emit_pattern(pat: &Pattern) -> String {
    match pat {
        Pattern::Wildcard => "_".to_string(),
        Pattern::Literal(lit) => emit_literal_pattern(lit),
        Pattern::Ident(name) => super::expr::aver_name_to_lean(name),
        Pattern::EmptyList => "[]".to_string(),
        Pattern::Cons(head, tail) => {
            format!(
                "{} :: {}",
                super::expr::aver_name_to_lean(head),
                super::expr::aver_name_to_lean(tail)
            )
        }
        Pattern::Tuple(pats) => {
            let parts: Vec<String> = pats.iter().map(|p| emit_pattern(p)).collect();
            format!("({})", parts.join(", "))
        }
        Pattern::Constructor(name, bindings) => emit_constructor_pattern(name, bindings),
    }
}

fn emit_literal_pattern(lit: &Literal) -> String {
    match lit {
        Literal::Int(i) => format!("{}", i),
        Literal::Float(f) => format!("{}", f),
        Literal::Str(s) => format!("{:?}", s),
        Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
    }
}

fn emit_constructor_pattern(name: &str, bindings: &[String]) -> String {
    match name {
        "Result.Ok" => {
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
        "Result.Err" => {
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
        "Option.Some" => {
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
        "Option.None" => ".none".to_string(),
        _ => {
            // User-defined type: Shape.Circle → .circle
            if let Some(dot_pos) = name.find('.') {
                let variant = &name[dot_pos + 1..];
                // Lean convention: lowercase constructor names
                let lean_variant = to_lower_first(variant);
                if bindings.is_empty() {
                    format!(".{}", lean_variant)
                } else {
                    let parts: Vec<String> = bindings
                        .iter()
                        .map(|b| super::expr::aver_name_to_lean(b))
                        .collect();
                    format!(".{} {}", lean_variant, parts.join(" "))
                }
            } else {
                // Record pattern: User(name, age) → { name, age }
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
}
