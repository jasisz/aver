/// Aver patterns → Rust pattern strings.
use crate::ast::Literal;
use crate::codegen::CodegenContext;
use crate::codegen::common::{module_prefix_to_rust_path, resolve_module_call};
use crate::ir::SemanticConstructor;
use crate::ir::WrapperKind;
use crate::ir::hir::{ResolvedCtor, ResolvedPattern, semantic_constructor_from_resolved_ctor};

use super::syntax::generated_ident;

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

/// Emit a match-arm pattern together with the guard its string literals need.
///
/// A string literal standing as the whole pattern is matched against `&str`
/// (the caller derefs the subject). Inside a tuple it cannot be: the element is
/// an `AverStr`, and Rust has no literal pattern for it. Each such literal, at
/// any tuple depth, becomes a fresh compiler-owned binder and one conjunct of
/// the arm's `if` guard, `str::eq(&binder, "lit")`. The `&` coerces through
/// `AverStr`'s `Deref` whether the binder is owned or a reference (a match on a
/// borrowed subject), and a guard only borrows, so a failed guard moves
/// nothing out of the subject.
pub fn emit_pattern_with_guard(
    pat: &ResolvedPattern,
    string_context: bool,
    ctx: &CodegenContext,
) -> (String, Option<String>) {
    let ResolvedPattern::Tuple(pats) = pat else {
        return (emit_pattern(pat, string_context, ctx), None);
    };
    let mut guards = Vec::new();
    let text = emit_tuple_with_str_guards(pats, ctx, &mut guards);
    let guard = (!guards.is_empty()).then(|| guards.join(" && "));
    (text, guard)
}

fn emit_tuple_with_str_guards(
    pats: &[ResolvedPattern],
    ctx: &CodegenContext,
    guards: &mut Vec<String>,
) -> String {
    let parts: Vec<String> = pats
        .iter()
        .map(|p| match p {
            ResolvedPattern::Literal(Literal::Str(s)) => {
                let binder = generated_ident(&format!("str_lit{}", guards.len()));
                guards.push(format!("str::eq(&{binder}, {s:?})"));
                binder
            }
            ResolvedPattern::Tuple(inner) => emit_tuple_with_str_guards(inner, ctx, guards),
            other => emit_pattern(other, false, ctx),
        })
        .collect();
    if parts.len() == 1 {
        format!("({},)", parts[0])
    } else {
        format!("({})", parts.join(", "))
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
            let rust_name = match resolve_module_call(&qualified_type_name, ctx) {
                Some((prefix, bare_type_name)) => format!(
                    "{}::{bare_type_name}::{variant_name}",
                    module_prefix_to_rust_path(prefix)
                ),
                None => format!("{qualified_type_name}::{variant_name}"),
            };
            match shared_fields_binder(ctor, bindings, ctx) {
                Some(binder) => format!("{rust_name}({binder})"),
                None => emit_tuple_like_constructor_pattern(&rust_name, bindings),
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

/// The one binder a constructor pattern of a shared sum writes for the `Arc`
/// that holds its fields (see `representation::sum_is_shared`), or `None`
/// when the constructor keeps its fields inline. It is named after the first
/// field the arm binds, so two constructors in one tuple pattern get two
/// names; an arm that binds no field matches `_`.
pub(super) fn shared_fields_binder(
    ctor: &ResolvedCtor,
    bindings: &[String],
    ctx: &CodegenContext,
) -> Option<String> {
    let ResolvedCtor::User { type_id, .. } = ctor else {
        return None;
    };
    if bindings.is_empty() || !super::representation::type_id_is_shared(*type_id, ctx) {
        return None;
    }
    Some(match bindings.iter().find(|b| *b != "_") {
        // Composed from the raw name: inside a longer word it needs no escape.
        Some(first) => format!("__{first}_fields"),
        None => "_".to_string(),
    })
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
