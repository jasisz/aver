//! The Rust side of `lazy f(…) knowing f.law(…)` (see
//! [`crate::codegen::knowing`]).
//!
//! For each law some deferred call knows a value from, the law's module gets
//! a function of the givens that answers the law's `when` and right side:
//! `Some(R)` when `when` holds, `None` when it does not (or `R` is past
//! `i64`). The deferred call asks it first: on `Some(v)` the node keeps `v`
//! for the law's projection `P`, on `None` the call is made at once. `P`
//! itself starts by asking its argument for a value kept for it, and returns
//! that without making the call.

use std::collections::BTreeSet;

use crate::ast::Expr;
use crate::codegen::CodegenContext;
use crate::codegen::knowing::Law;
use crate::ir::FnId;
use crate::ir::mir::MirExpr;

use super::syntax::aver_name_to_rust;

/// The laws a deferred call of the MIR knows a value from, as the deferred
/// function and the law's name.
pub(super) fn compute(ctx: &CodegenContext) -> Vec<(FnId, String)> {
    fn visit(expr: &MirExpr, out: &mut BTreeSet<(FnId, String)>) {
        if let MirExpr::Call(call) = expr
            && let Some(knowing) = call.lazy.knowing()
            && let crate::ir::mir::MirCallee::Fn(callee) = call.node.callee
        {
            out.insert((callee, knowing.law.clone()));
        }
        crate::ir::mir::expr::walk_children(expr, &mut |child| visit(child, out));
    }
    let mut out = BTreeSet::new();
    for (_, mir_fn) in ctx.mir_program.iter().flat_map(|program| program.iter()) {
        visit(&mir_fn.body.node, &mut out);
    }
    out.into_iter()
        .filter(|(callee, law)| {
            crate::codegen::knowing::law(ctx, *callee, law)
                .ok()
                .and_then(|law| helper(&law, ctx))
                .is_some()
        })
        .collect()
}

/// The law `law` of `callee`, when some deferred call knows a value from it
/// and its module has the function that answers it.
pub(super) fn used<'a>(ctx: &'a CodegenContext, callee: FnId, law: &str) -> Option<Law<'a>> {
    ctx.rust_knowing
        .iter()
        .any(|(c, l)| *c == callee && l == law)
        .then(|| crate::codegen::knowing::law(ctx, callee, law).ok())
        .flatten()
}

/// The Rust path of the function `id`.
pub(super) fn fn_path(id: FnId, ctx: &CodegenContext) -> String {
    let key = &ctx.symbol_table.fn_entry(id).key;
    in_module(key.scope_str(), &aver_name_to_rust(&key.name))
}

/// The Rust path of the function that answers `law`.
pub(super) fn helper_path(law: &Law) -> String {
    in_module(law.scope.as_deref(), &helper_name(law))
}

/// The Rust path of `name` in the module `scope` (`None`: the entry).
fn in_module(scope: Option<&str>, name: &str) -> String {
    match scope {
        Some(prefix) => format!(
            "{}::{name}",
            crate::codegen::common::module_prefix_to_rust_path(prefix)
        ),
        None => format!("crate::aver_generated::entry::{name}"),
    }
}

fn helper_name(law: &Law) -> String {
    format!(
        "__knowing_{}",
        aver_name_to_rust(&law.key.replace('.', "_"))
    )
}

/// The value the law kept for `P` stands for: the projection id a node
/// keeps it under.
pub(super) fn projection_id(law: &Law) -> u32 {
    law.projection.0
}

/// The function that answers `law`: its givens in, `Some(R)` out when the
/// law's `when` holds. `None` when the backend cannot render it, and then no
/// deferred call knows a value from the law.
fn helper(law: &Law, ctx: &CodegenContext) -> Option<String> {
    let scope = law.scope.as_deref();
    let callee = ctx.resolved_program.fn_by_id(law.callee)?;
    let mut params = Vec::new();
    let mut givens = Vec::new();
    for (g, &i) in law.law.givens.iter().zip(&law.given_args) {
        let (_, ty) = callee.params.get(i)?;
        params.push(format!(
            "{}: {}",
            aver_name_to_rust(&g.name),
            super::types::type_to_rust_scoped(ty, ctx, scope)
        ));
        givens.push((g.name.as_str(), ty.clone()));
    }
    let value = format!(
        "aver_rt::AverInt::to_i64(&({}))",
        law_expr(&law.law.rhs, &givens, scope, ctx)?
    );
    let body = match &law.law.when {
        Some(when) => format!(
            "if {} {{ {value} }} else {{ None }}",
            law_expr(when, &givens, scope, ctx)?
        ),
        None => value,
    };
    Some(format!(
        "/// What law `{}` says of a deferred call with these givens.\n#[allow(unused_variables, clippy::all)]\npub fn {}({}) -> Option<i64> {{\n    {body}\n}}",
        law.key,
        helper_name(law),
        params.join(", ")
    ))
}

/// An expression of a law, with each given read from the Rust variable of
/// its name.
fn law_expr(
    expr: &crate::ast::Spanned<Expr>,
    givens: &[(&str, crate::types::Type)],
    scope: Option<&str>,
    ctx: &CodegenContext,
) -> Option<String> {
    fn bind(expr: &mut crate::ast::Spanned<MirExpr>, givens: &[(&str, crate::types::Type)]) {
        if let MirExpr::FnValue(name) = &expr.node
            && let Some(slot) = givens.iter().position(|(g, _)| g == name)
        {
            let local = crate::ir::mir::MirLocal {
                slot: crate::ir::mir::LocalId(slot as u32),
                last_use: false,
                name: name.clone(),
            };
            let ty = givens[slot].1.clone();
            *expr = crate::ast::Spanned::new(
                MirExpr::Local(crate::ast::Spanned::new(local, expr.line)),
                expr.line,
            );
            expr.set_ty(ty);
            return;
        }
        crate::ir::mir::expr::walk_children_mut(&mut expr.node, &mut |child| bind(child, givens));
    }
    let resolved = ctx.resolve_expr(expr, scope);
    super::from_mir::emit_mir_value(&resolved, &mut |mir| bind(mir, givens), ctx)
}

/// The functions that answer the laws of `scope` some deferred call knows a
/// value from.
pub(super) fn emit_helpers(scope: Option<&str>, ctx: &CodegenContext) -> Option<String> {
    let helpers: Vec<String> = ctx
        .rust_knowing
        .iter()
        .filter_map(|(callee, law)| crate::codegen::knowing::law(ctx, *callee, law).ok())
        .filter(|law| law.scope.as_deref() == scope)
        .filter_map(|law| helper(&law, ctx))
        .collect();
    (!helpers.is_empty()).then(|| helpers.join("\n\n"))
}

/// The start of the projection `fn_id`, when some deferred call keeps a
/// value for it: the value a deferred argument keeps, if it keeps one.
pub(super) fn projection_prologue(
    resolved: &crate::ir::hir::ResolvedFnDef,
    bare_return: bool,
    ctx: &CodegenContext,
) -> Option<String> {
    let projects = ctx.rust_knowing.iter().any(|(callee, law)| {
        crate::codegen::knowing::law(ctx, *callee, law)
            .is_ok_and(|law| law.projection == resolved.fn_id)
    });
    if !projects {
        return None;
    }
    let (param, _) = resolved.params.first()?;
    let kept = super::syntax::generated_ident("kept");
    let value = match bare_return {
        true => kept.clone(),
        false => format!("aver_rt::AverInt::from_i64({kept})"),
    };
    Some(format!(
        "    if let Some({kept}) = {}.deferred_known({}) {{\n        return {value};\n    }}",
        aver_name_to_rust(param),
        resolved.fn_id.0
    ))
}

/// For a build `--trust-laws` let laws through unproved: the mark that says
/// so, and which.
pub(super) fn trusted_mark(ctx: &CodegenContext) -> Option<String> {
    if ctx.knowing_trusted.is_empty() {
        return None;
    }
    let mut laws: Vec<&String> = ctx.knowing_trusted.iter().collect();
    laws.sort();
    Some(format!(
        "/// Built with `--trust-laws`: deferred calls know values from these laws, which are not\n/// proved; each is checked every time such a call is made.\npub const UNPROVED_LAWS: &[&str] = &[{}];",
        laws.iter()
            .map(|l| format!("{l:?}"))
            .collect::<Vec<_>>()
            .join(", ")
    ))
}
