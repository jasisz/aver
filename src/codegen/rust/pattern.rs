/// Aver patterns → Rust pattern strings.
use crate::ast::*;
use crate::codegen::CodegenContext;

/// Emit a Rust pattern from an Aver Pattern.
pub fn emit_pattern(pat: &Pattern, string_context: bool, _ctx: &CodegenContext) -> String {
    match pat {
        Pattern::Wildcard => "_".to_string(),
        Pattern::Literal(lit) => emit_literal_pattern(lit, string_context),
        Pattern::Ident(name) => super::expr::aver_name_to_rust(name),
        Pattern::EmptyList => {
            // Matches on .as_slice()
            "[]".to_string()
        }
        Pattern::Cons(head, tail) => {
            // [h, ..t] with wildcard-aware lowering.
            let h = super::expr::aver_name_to_rust(head);
            let t = super::expr::aver_name_to_rust(tail);
            match (head.as_str(), tail.as_str()) {
                ("_", "_") => "[_, ..]".to_string(),
                (_, "_") => format!("[{}, ..]", h),
                ("_", _) => format!("[_, {} @ ..]", t),
                _ => format!("[{}, {} @ ..]", h, t),
            }
        }
        Pattern::Tuple(pats) => {
            let parts: Vec<String> = pats.iter().map(|p| emit_pattern(p, false, _ctx)).collect();
            format!("({})", parts.join(", "))
        }
        Pattern::Constructor(name, bindings) => emit_constructor_pattern(name, bindings),
    }
}

fn emit_literal_pattern(lit: &Literal, _string_context: bool) -> String {
    match lit {
        Literal::Int(i) => format!("{}i64", i),
        Literal::Float(f) => {
            let s = f.to_string();
            if s.contains('.') || s.contains('e') || s.contains('E') {
                format!("{}f64", s)
            } else {
                format!("{}.0f64", s)
            }
        }
        Literal::Str(s) => {
            // When matching on .as_str(), string patterns are &str
            format!("{:?}", s)
        }
        Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn emit_constructor_pattern(name: &str, bindings: &[String]) -> String {
    // Map Aver constructor names to Rust
    let rust_ctor = match name {
        "Result.Ok" => "Ok",
        "Result.Err" => "Err",
        "Option.Some" => "Some",
        "Option.None" => return "None".to_string(),
        "Tcp.Connection" => return emit_record_or_variant_pattern("Tcp_Connection", bindings),
        _ => {
            // Source syntax only produces qualified constructors here.
            // Keep the bare-name fallback for manually-constructed ASTs in tests.
            if !name.contains('.') {
                return emit_record_or_variant_pattern(name, bindings);
            }
            let rust_name = name.replace('.', "::");
            return if bindings.is_empty() {
                rust_name
            } else {
                let parts: Vec<String> = bindings
                    .iter()
                    .map(|b| super::expr::aver_name_to_rust(b))
                    .collect();
                format!("{}({})", rust_name, parts.join(", "))
            };
        }
    };

    // rust_ctor is a &str at this point (Ok, Err, Some)

    if bindings.is_empty() {
        rust_ctor.to_string()
    } else {
        let parts: Vec<String> = bindings
            .iter()
            .map(|b| {
                if b == "_" {
                    "_".to_string()
                } else {
                    super::expr::aver_name_to_rust(b)
                }
            })
            .collect();
        format!("{}({})", rust_ctor, parts.join(", "))
    }
}

fn emit_record_or_variant_pattern(name: &str, bindings: &[String]) -> String {
    // Parser-rejected source forms should not reach codegen; this remains as a
    // fallback for internal tests that build ASTs directly.
    if bindings.is_empty() {
        name.to_string()
    } else {
        let parts: Vec<String> = bindings
            .iter()
            .map(|b| super::expr::aver_name_to_rust(b))
            .collect();
        format!("{} {{ {} }}", name, parts.join(", "))
    }
}
