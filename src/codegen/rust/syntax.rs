use crate::ast::{Literal, MatchArm, Pattern, Stmt};
use crate::codegen::CodegenContext;

use super::expr::emit_expr;
use super::liveness::EmitCtx;

pub(super) fn has_string_literal_patterns(arms: &[MatchArm]) -> bool {
    arms.iter()
        .any(|arm| matches!(&arm.pattern, Pattern::Literal(Literal::Str(_))))
}

pub(super) fn has_list_patterns(arms: &[MatchArm]) -> bool {
    arms.iter()
        .any(|arm| matches!(&arm.pattern, Pattern::EmptyList | Pattern::Cons(_, _)))
}

/// Emit a statement as Rust code.
pub fn emit_stmt(stmt: &Stmt, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    match stmt {
        Stmt::Binding(name, _type_ann, expr) => {
            let val = emit_expr(expr, ctx, ectx);
            format!("let {} = {};", aver_name_to_rust(name), val)
        }
        Stmt::Expr(expr) => {
            let val = emit_expr(expr, ctx, ectx);
            format!("{};", val)
        }
    }
}

/// Convert an Aver identifier to a valid Rust identifier.
/// Handles snake_case and reserved words.
pub fn aver_name_to_rust(name: &str) -> String {
    // Rust reserved words that might conflict
    match name {
        "type" => "r#type".to_string(),
        "match" => "r#match".to_string(),
        "fn" => "r#fn".to_string(),
        "let" => "r#let".to_string(),
        "use" => "r#use".to_string(),
        "mod" => "r#mod".to_string(),
        "impl" => "r#impl".to_string(),
        "trait" => "r#trait".to_string(),
        "struct" => "r#struct".to_string(),
        "enum" => "r#enum".to_string(),
        "self" => "r#self".to_string(),
        "super" => "r#super".to_string(),
        "crate" => "r#crate".to_string(),
        "where" => "r#where".to_string(),
        "async" => "r#async".to_string(),
        "await" => "r#await".to_string(),
        "move" => "r#move".to_string(),
        "ref" => "r#ref".to_string(),
        "mut" => "r#mut".to_string(),
        "loop" => "r#loop".to_string(),
        "break" => "r#break".to_string(),
        "continue" => "r#continue".to_string(),
        "return" => "r#return".to_string(),
        "yield" => "r#yield".to_string(),
        "pub" => "r#pub".to_string(),
        "box" => "r#box".to_string(),
        "in" => "r#in".to_string(),
        _ => name.to_string(),
    }
}
