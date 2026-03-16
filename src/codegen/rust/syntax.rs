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

/// Rust reserved words that need raw identifier escaping.
const RUST_RESERVED: &[&str] = &[
    "as", "async", "await", "box", "break", "const", "continue", "crate", "dyn", "else", "enum",
    "extern", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub",
    "ref", "return", "self", "static", "struct", "super", "trait", "type", "unsafe", "use",
    "where", "while", "yield",
];

/// Convert an Aver identifier to a valid Rust identifier.
pub fn aver_name_to_rust(name: &str) -> String {
    crate::codegen::common::escape_reserved_word_prefix(name, RUST_RESERVED, "r#")
}
