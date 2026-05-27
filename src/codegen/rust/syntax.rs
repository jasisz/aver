use crate::ast::Literal;
use crate::codegen::CodegenContext;
use crate::ir::hir::{ResolvedMatchArm, ResolvedPattern, ResolvedStmt};

use super::emit_ctx::EmitCtx;
use super::expr::emit_expr;

pub(super) fn has_string_literal_patterns(arms: &[ResolvedMatchArm]) -> bool {
    arms.iter()
        .any(|arm| matches!(&arm.pattern, ResolvedPattern::Literal(Literal::Str(_))))
}

pub(super) fn has_list_patterns(arms: &[ResolvedMatchArm]) -> bool {
    arms.iter().any(|arm| {
        matches!(
            &arm.pattern,
            ResolvedPattern::EmptyList | ResolvedPattern::Cons(_, _)
        )
    })
}

/// Emit a resolved statement as Rust code.
pub fn emit_stmt(stmt: &ResolvedStmt, ctx: &CodegenContext, ectx: &EmitCtx) -> String {
    match stmt {
        ResolvedStmt::Binding { name, value, .. } => {
            let val = emit_expr(&value.node, ctx, ectx);
            format!("let {} = {};", aver_name_to_rust(name), val)
        }
        ResolvedStmt::Expr(expr) => {
            let val = emit_expr(&expr.node, ctx, ectx);
            format!("{};", val)
        }
    }
}

/// Rust reserved words that need raw identifier escaping.
const RUST_RESERVED: &[&str] = &[
    "as", "async", "await", "box", "break", "const", "continue", "crate", "dyn", "else", "enum",
    "extern", "fn", "for", "gen", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut",
    "pub", "ref", "return", "self", "static", "struct", "super", "trait", "type", "unsafe", "use",
    "where", "while", "yield",
];

/// Convert an Aver identifier to a valid Rust identifier.
pub fn aver_name_to_rust(name: &str) -> String {
    crate::codegen::common::escape_reserved_word_prefix(name, RUST_RESERVED, "r#")
}
