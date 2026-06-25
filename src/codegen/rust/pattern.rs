/// Aver patterns → Rust pattern strings.
use crate::ast::Literal;
use crate::codegen::CodegenContext;
use crate::codegen::common::{module_prefix_to_rust_path, resolve_module_call};
use crate::ir::SemanticConstructor;
use crate::ir::WrapperKind;
use crate::ir::hir::{ResolvedCtor, ResolvedPattern, semantic_constructor_from_resolved_ctor};

/// Emit a Rust pattern from a resolved Aver pattern.
pub fn emit_pattern(pat: &ResolvedPattern, string_context: bool, ctx: &CodegenContext) -> String {
    match pat {
        ResolvedPattern::Wildcard => "_".to_string(),
        ResolvedPattern::Literal(lit) => emit_literal_pattern(lit, string_context),
        ResolvedPattern::Ident(name) => super::expr::aver_name_to_rust(name),
        ResolvedPattern::EmptyList => {
            // Matches on .as_slice()
            "[]".to_string()
        }
        ResolvedPattern::Cons(head, tail) => {
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
        ResolvedPattern::Tuple(pats) => {
            let parts: Vec<String> = pats.iter().map(|p| emit_pattern(p, false, ctx)).collect();
            format!("({})", parts.join(", "))
        }
        ResolvedPattern::Ctor(ctor, bindings) => emit_constructor_pattern(ctor, bindings, ctx),
    }
}

fn emit_literal_pattern(lit: &Literal, _string_context: bool) -> String {
    match lit {
        Literal::Int(i) => format!("{}i64", i),
        // A big-int literal has no valid Rust structural pattern (`AverInt` is
        // not a constant). Such patterns are routed to the equality-guard chain
        // (`try_emit_int_literal_match`) via `pattern_has_int_literal`, so this
        // structural emitter is never reached for a big-int.
        Literal::BigInt(_) => unreachable!(
            "BigInt literal patterns lower via the equality-guard chain, not a structural pattern"
        ),
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

fn emit_constructor_pattern(
    ctor: &ResolvedCtor,
    bindings: &[String],
    ctx: &CodegenContext,
) -> String {
    match semantic_constructor_from_resolved_ctor(ctor, &ctx.symbol_table) {
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
        SemanticConstructor::Unknown(name) => {
            if name == "Tcp.Connection" {
                return emit_record_or_variant_pattern("Tcp_Connection", bindings);
            }
            // Source syntax only produces qualified constructors here.
            // Keep the bare-name fallback for manually-constructed ASTs in tests.
            if !name.contains('.') {
                return emit_record_or_variant_pattern(&name, bindings);
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
