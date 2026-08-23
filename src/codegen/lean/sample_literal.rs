//! Ground-truth literalization of bounded verify checks.
//!
//! Bounded Lean checks (`verify f` example cases, law `_sample_N` /
//! `_checked_domain` theorems) are `impl(sample) = spec(sample)` by
//! construction — BOTH sides route through the exported model. That shape has
//! a kernel-certified failure mode: the fuel wrappers' exhaustion arm is
//! `panic!`, and Lean's `panic!` returns the result type's `default` instead
//! of aborting, so when fuel runs out under `native_decide` both sides reduce
//! to `default` and the kernel certifies a vacuous — possibly FALSE —
//! equation (`lake` exit 0, zero sorries).
//!
//! This module replaces the expected (right) side with the value the VM
//! actually computed during `aver verify` (the ground truth the CLI collects
//! into `CodegenContext::sample_expected`), turning the theorem into
//! `impl(sample) = <program result literal>`: if the model diverges from the
//! program — fuel exhaustion included, unless the divergent side ALSO
//! coincides with the literal, which the `--check` panic-line gate catches —
//! the build fails loudly.
//!
//! Chosen path (documented per the design sketch): the VM value is rendered
//! with `aver_repr_literal`, parsed back into a source `Expr` with the
//! ordinary parser, validated against a strict literal whitelist, and emitted
//! through `emit_expr_legacy` — reusing the exact emission pipeline the
//! source RHS would take (string escaping, `Except`/`Option` mapping, list
//! and map lowering) instead of duplicating a Value→Lean printer.
//!
//! Fallback is always the source RHS: any miss (no entry, parse failure,
//! non-whitelisted shape) keeps current behavior, so literalization can only
//! strengthen a check, never change what a correct model proves.

use crate::ast::{Expr, Literal, Spanned, VerifyBlock};
use crate::codegen::CodegenContext;

use super::expr::emit_expr_legacy;

/// The reason `aver verify` gave for not answering case `global_case_idx`,
/// when it declined it.
///
/// A declined case must not reach the fallback this module documents. The
/// fallback is the source RHS, so a declined case would be emitted as
/// `impl(sample) = <the author's expected expression>` — a claim nothing
/// checked, in exactly the shape the module exists to prevent, and precisely
/// on the big inputs where the model is likeliest to exhaust fuel too. The
/// caller states no theorem for such a case and records a `DeclinedClaim`.
pub(super) fn decline_reason<'a>(
    vb: &VerifyBlock,
    ctx: &'a CodegenContext,
    global_case_idx: usize,
) -> Option<&'a String> {
    let key = (
        ctx.active_module_scope(),
        crate::codegen::common::verify_block_counter_key(vb),
        global_case_idx,
    );
    ctx.declined_cases.get(&key)
}

/// Emit the expected side of case `global_case_idx` (the emitter's running
/// per-key index: `case_index_start + idx`) from VM ground truth, if a safe
/// literal is available. `None` → caller falls back to the source RHS.
pub(super) fn ground_truth_rhs(
    vb: &VerifyBlock,
    ctx: &CodegenContext,
    global_case_idx: usize,
) -> Option<String> {
    let key = (
        ctx.active_module_scope(),
        crate::codegen::common::verify_block_counter_key(vb),
        global_case_idx,
    );
    let repr = ctx.sample_expected.get(&key)?;
    let expr = parse_literal_expr(repr)?;
    if !is_safe_literal_expr(&expr) {
        return None;
    }
    Some(emit_expr_legacy(&expr, ctx, None))
}

/// Parse a value repr back into a single expression. The repr is rendered by
/// `aver_repr_literal`, which is plain expression syntax (`[1, 2]`,
/// `Result.Ok("x")`, `(1, true)`, `{1: "one"}`), so it parses as one
/// top-level expression statement.
fn parse_literal_expr(repr: &str) -> Option<Spanned<Expr>> {
    let mut lexer = crate::lexer::Lexer::new(repr);
    let tokens = lexer.tokenize().ok()?;
    let mut parser = crate::parser::Parser::new(tokens);
    let items = parser.parse().ok()?;
    match items.as_slice() {
        [crate::ast::TopLevel::Stmt(crate::ast::Stmt::Expr(expr))] => Some(expr.clone()),
        _ => None,
    }
}

/// Strict whitelist over the parsed repr: pure data literals plus
/// dotted-capitalized constructor applications (`Result.Ok`, `Option.None`,
/// `Color.Red(…)`). Anything else — bare identifiers (an unqualified variant
/// repr would emit as an unknown call), float literals (decimal repr doesn't
/// round-trip bit-exactly; the CLI already skips Float-carrying values, this
/// is defense in depth), `Vector[…]` / record reprs (not expression syntax —
/// they already fail the parse step) — declines literalization.
fn is_safe_literal_expr(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Literal(Literal::Int(_) | Literal::Str(_) | Literal::Bool(_) | Literal::Unit) => true,
        Expr::Literal(Literal::Float(_)) => false,
        Expr::Neg(inner) => is_safe_literal_expr(inner),
        Expr::List(items) | Expr::Tuple(items) => items.iter().all(is_safe_literal_expr),
        // Map literals are rejected: the VM repr sorts entries while the
        // emitted `AverMap` carries APPEND order, so a literalized map
        // would compare structurally against a differently-ordered (yet
        // equal) model value. The ground-truth collector also skips
        // Map-carrying values (`value_contains_map`) — this arm is the
        // belt-and-suspenders second gate.
        Expr::MapLiteral(_) => false,
        // Nullary constructor: `Option.None`, `Color.Red` — a pure
        // capitalized dotted chain.
        Expr::Attr(_, _) => is_capitalized_dotted_path(expr),
        // Applied constructor: `Result.Ok(3)`, `Color.Rgb(1, 2, 3)`.
        Expr::FnCall(callee, args) => {
            is_capitalized_dotted_path(callee) && args.iter().all(is_safe_literal_expr)
        }
        Expr::Constructor(name, arg) => {
            name.split('.').count() >= 2
                && name.split('.').all(starts_capitalized)
                && arg.as_deref().is_none_or(is_safe_literal_expr)
        }
        _ => false,
    }
}

/// `Type.Variant`-shaped attr chain: every segment a capitalized identifier,
/// at least two segments (so a bare `Foo` never passes — the VM repr of an
/// unqualified variant must not be emitted as a call to an unknown name).
fn is_capitalized_dotted_path(expr: &Spanned<Expr>) -> bool {
    fn walk(expr: &Spanned<Expr>, segments: &mut usize) -> bool {
        match &expr.node {
            Expr::Ident(name) => {
                *segments += 1;
                starts_capitalized(name)
            }
            Expr::Attr(base, field) => {
                *segments += 1;
                starts_capitalized(field) && walk(base, segments)
            }
            _ => false,
        }
    }
    let mut segments = 0;
    walk(expr, &mut segments) && segments >= 2
}

fn starts_capitalized(s: &str) -> bool {
    s.chars().next().is_some_and(|c| c.is_ascii_uppercase())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parses_safe(repr: &str) -> bool {
        parse_literal_expr(repr).is_some_and(|e| is_safe_literal_expr(&e))
    }

    #[test]
    fn whitelists_ground_truth_repr_shapes() {
        assert!(parses_safe("210"));
        assert!(parses_safe("-5"));
        assert!(parses_safe("true"));
        assert!(parses_safe("\"hello\""));
        assert!(parses_safe("[1, 2, 3]"));
        assert!(parses_safe("[]"));
        assert!(parses_safe("(1, \"a\")"));
        assert!(parses_safe("Result.Ok(3)"));
        assert!(parses_safe("Result.Err(\"boom\")"));
        assert!(parses_safe("Option.Some([1])"));
        assert!(parses_safe("Option.None"));
        assert!(parses_safe("Color.Rgb(1, 2, 3)"));
    }

    #[test]
    fn declines_unsafe_or_unparseable_shapes() {
        // Float literals never literalize (decimal repr round-trip).
        assert!(!parses_safe("1.5"));
        assert!(!parses_safe("[1.5]"));
        // Unqualified variant repr — would emit a call to an unknown name.
        assert!(!parses_safe("Red"));
        assert!(!parses_safe("Rgb(1, 2, 3)"));
        // Fn / builtin / namespace reprs are not expression syntax.
        assert!(!parses_safe("<fn foo>"));
        // Vector repr is not expression syntax (indexing into `Vector`).
        assert!(!parses_safe("Vector[1, 2]"));
        // Record repr uses `field: value`, not constructor syntax.
        assert!(!parses_safe("User(name: \"x\")"));
        // Arbitrary computation must never slip through.
        assert!(!parses_safe("f(1)"));
        assert!(!parses_safe("1 + 2"));
        // Map literals: rejected DELIBERATELY (repr sorts entries, the
        // emitted AverMap carries append order — structural comparison
        // would fail on a correct model). Both repr spellings stay out:
        // the VM repr `{k: v}` fails the parser, and even valid map
        // syntax `{k => v}` is rejected by the whitelist arm.
        assert!(!parses_safe("{\"a\": 1}"));
        assert!(!parses_safe("{\"a\" => 1}"));
        assert!(!parses_safe("Option.Some({\"a\" => 1})"));
    }
}
