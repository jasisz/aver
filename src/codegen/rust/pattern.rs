/// Aver patterns → Rust pattern strings.
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::common::{is_user_type, module_prefix_to_rust_path, resolve_module_call};
use crate::ir::{CallLowerCtx, SemanticConstructor, WrapperKind, classify_constructor_name};

struct RustPatternCtx<'a> {
    ctx: &'a CodegenContext,
}

impl CallLowerCtx for RustPatternCtx<'_> {
    fn is_local_value(&self, _name: &str) -> bool {
        false
    }

    fn is_user_type(&self, name: &str) -> bool {
        is_user_type(name, self.ctx)
    }

    fn resolve_module_call<'a>(&self, dotted: &'a str) -> Option<(&'a str, &'a str)> {
        let mut best = None;
        for (dot_idx, _) in dotted.match_indices('.') {
            let prefix = &dotted[..dot_idx];
            let suffix = &dotted[dot_idx + 1..];
            if self.ctx.module_prefixes.contains(prefix)
                && best.is_none_or(|existing: (&str, &str)| prefix.len() > existing.0.len())
            {
                best = Some((prefix, suffix));
            }
        }
        best
    }
}

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
        Pattern::Constructor(name, bindings) => emit_constructor_pattern(name, bindings, _ctx),
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

fn emit_constructor_pattern(name: &str, bindings: &[String], ctx: &CodegenContext) -> String {
    let lower_ctx = RustPatternCtx { ctx };
    match classify_constructor_name(name, &lower_ctx) {
        SemanticConstructor::Wrapper(kind) => {
            let rust_ctor = match kind {
                WrapperKind::ResultOk => "Ok",
                WrapperKind::ResultErr => "Err",
                WrapperKind::OptionSome => "Some",
            };
            emit_tuple_like_constructor_pattern(rust_ctor, bindings)
        }
        SemanticConstructor::NoneValue => "None".to_string(),
        SemanticConstructor::TypeConstructor {
            qualified_type_name,
            variant_name,
        } => {
            if let Some((prefix, bare_type_name)) = resolve_module_call(&qualified_type_name, ctx) {
                let module_path = module_prefix_to_rust_path(prefix);
                let rust_name = format!("{module_path}::{bare_type_name}::{variant_name}");
                emit_tuple_like_constructor_pattern(&rust_name, bindings)
            } else {
                let rust_name = format!("{qualified_type_name}::{variant_name}");
                emit_tuple_like_constructor_pattern(&rust_name, bindings)
            }
        }
        SemanticConstructor::Unknown(_) => {
            if name == "Tcp.Connection" {
                return emit_record_or_variant_pattern("Tcp_Connection", bindings);
            }
            // Source syntax only produces qualified constructors here.
            // Keep the bare-name fallback for manually-constructed ASTs in tests.
            if !name.contains('.') {
                return emit_record_or_variant_pattern(name, bindings);
            }
            let rust_name = name.replace('.', "::");
            emit_tuple_like_constructor_pattern(&rust_name, bindings)
        }
    }
}

fn emit_tuple_like_constructor_pattern(name: &str, bindings: &[String]) -> String {
    if bindings.is_empty() {
        name.to_string()
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
        format!("{}({})", name, parts.join(", "))
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
