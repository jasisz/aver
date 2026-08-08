//! Interpolation lowering — desugar `Expr::InterpolatedStr(parts)` into a
//! deforestation buffer pipeline shared with the buffer-build pass.
//!
//! Every `"a${x}b${y}"` in user code becomes:
//!
//! ```text
//! __buf_finalize(
//!   __buf_append(
//!     __buf_append(
//!       __buf_append(
//!         __buf_append(__buf_new(<cap_hint>), "a"),
//!         __to_str(x)),
//!       "b"),
//!     __to_str(y)))
//! ```
//!
//! Why a single IR pass and not per-backend lowering: the chained
//! `str_concat` shape every backend used to emit was O(N²) in total
//! length (each concat allocates a string of cumulative size). The
//! buffer pipeline is O(N + total_len) — append is `memcpy` into a
//! pre-sized buffer, finalize is one allocation. Doing this once at
//! IR level means VM, WASM, and Rust all benefit; each backend only
//! needs to implement the four `__buf_*` intrinsics (already required
//! by the buffer-build pass) plus `__to_str` for runtime coercion of
//! non-string parts.
//!
//! The pass runs *after* the type checker (the checker still sees
//! `InterpolatedStr` and types it as `String`) and *before* the
//! resolver (the desugared form contains bare `Expr::Ident` callees
//! the resolver later annotates).
//!
//! `__to_str` is the whole reason the checker restricts an interpolated
//! part to `Int` / `Float` / `Bool` / `String` (decision:
//! ExplicitStringify — conversion to String is named in source). Those
//! four are what every backend's `__to_str` renders; the VM's lowering
//! (a CONCAT against `""`, which calls `NanValue::repr`) happens to
//! render compounds too, but that is dead weight reachable only from
//! internal pipelines that skip the typecheck. Do not extend it, and do
//! not widen the checker's set without widening the wasm-gc dispatch in
//! `codegen/wasm_gc/body/from_mir/strings.rs` in the same change — the
//! rule and this lowering must agree exactly.

use crate::ast::{Expr, Literal, Spanned, Stmt, StrPart, TopLevel, Type};

/// Walk every fn body / stmt / expression in `items` and replace each
/// `Expr::InterpolatedStr` in place with the buffer pipeline above.
pub fn lower_interpolation_pass(items: &mut [TopLevel]) {
    for item in items.iter_mut() {
        if let TopLevel::FnDef(fd) = item {
            let body_arc = std::sync::Arc::make_mut(&mut fd.body);
            let crate::ast::FnBody::Block(stmts) = body_arc;
            for stmt in stmts.iter_mut() {
                lower_in_stmt(stmt);
            }
        }
    }
}

fn lower_in_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => lower_in_expr(expr),
    }
}

fn lower_in_expr(expr: &mut Spanned<Expr>) {
    // Rewrite children first so nested interpolations get desugared
    // bottom-up — important when an interpolation contains a Parsed
    // expression that itself is an interpolation.
    match &mut expr.node {
        Expr::FnCall(callee, args) => {
            lower_in_expr(callee);
            for a in args.iter_mut() {
                lower_in_expr(a);
            }
        }
        Expr::TailCall(data) => {
            for a in data.args.iter_mut() {
                lower_in_expr(a);
            }
        }
        Expr::Attr(inner, _) => lower_in_expr(inner),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for it in items.iter_mut() {
                lower_in_expr(it);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries.iter_mut() {
                lower_in_expr(k);
                lower_in_expr(v);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields.iter_mut() {
                lower_in_expr(v);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            lower_in_expr(base);
            for (_, v) in updates.iter_mut() {
                lower_in_expr(v);
            }
        }
        Expr::Match { subject, arms } => {
            lower_in_expr(subject);
            for arm in arms.iter_mut() {
                lower_in_expr(&mut arm.body);
            }
        }
        Expr::BinOp(_, left, right) => {
            lower_in_expr(left);
            lower_in_expr(right);
        }
        Expr::Constructor(_, inner) => {
            if let Some(inner) = inner.as_deref_mut() {
                lower_in_expr(inner);
            }
        }
        Expr::ErrorProp(inner) => lower_in_expr(inner),
        Expr::InterpolatedStr(parts) => {
            for part in parts.iter_mut() {
                if let StrPart::Parsed(inner) = part {
                    lower_in_expr(inner);
                }
            }
        }
        _ => {}
    }

    // Now desugar this node if it's an interpolation.
    if let Expr::InterpolatedStr(parts) = &expr.node {
        let line = expr.line;
        let pipeline = build_buffer_pipeline(line, parts);
        *expr = pipeline;
    }
}

/// Build the nested `__buf_finalize(__buf_append(... __buf_new(N), partN))`
/// expression for a list of interpolation parts.
fn build_buffer_pipeline(line: usize, parts: &[StrPart]) -> Spanned<Expr> {
    // Empty interpolation → empty literal. (Parser usually doesn't
    // produce this, but be safe — `""` literal is still a literal.)
    if parts.is_empty() {
        return sp_at(line, Expr::Literal(Literal::Str(String::new())));
    }

    // Estimate the buffer capacity hint from known literal lengths
    // plus a small budget per dynamic part. The synthesizer for the
    // buffer-build pass uses 8192 as a fixed default; for interpolation
    // a tighter estimate is usually better since most interpolations
    // are short and over-allocating bloats the stable lane.
    let cap_hint: i64 = parts
        .iter()
        .map(|p| match p {
            StrPart::Literal(s) => s.len() as i64,
            StrPart::Parsed(_) => 16,
        })
        .sum::<i64>()
        .max(16);

    // Type stamps mirror the typecheck pass: this synthesised IR runs
    // *after* the checker, so the type slot on each new node has to be
    // populated by the synthesiser itself for downstream backends (Rust
    // codegen reads `expr.ty()` to gate hoists on owned-mutable
    // `Buffer`s, see `src/codegen/rust/toplevel.rs`).
    let buffer_ty = Type::named("Buffer");
    let mut buf = intrinsic_call(
        line,
        "__buf_new",
        vec![sp_at_typed(
            line,
            Expr::Literal(Literal::Int(cap_hint)),
            Type::Int,
        )],
    );
    buf.set_ty(buffer_ty.clone());

    for part in parts {
        let part_str = match part {
            StrPart::Literal(s) => {
                sp_at_typed(line, Expr::Literal(Literal::Str(s.clone())), Type::Str)
            }
            StrPart::Parsed(inner) => {
                let call = intrinsic_call(line, "__to_str", vec![(**inner).clone()]);
                call.set_ty(Type::Str);
                call
            }
        };
        buf = intrinsic_call(line, "__buf_append", vec![buf, part_str]);
        buf.set_ty(buffer_ty.clone());
    }

    let finalized = intrinsic_call(line, "__buf_finalize", vec![buf]);
    finalized.set_ty(Type::Str);
    finalized
}

fn sp_at(line: usize, node: Expr) -> Spanned<Expr> {
    Spanned::new(node, line)
}

fn sp_at_typed(line: usize, node: Expr, ty: Type) -> Spanned<Expr> {
    let s = Spanned::new(node, line);
    s.set_ty(ty);
    s
}

fn intrinsic_call(line: usize, name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
    let callee = sp_at(line, Expr::Ident(name.to_string()));
    sp_at(line, Expr::FnCall(Box::new(callee), args))
}
