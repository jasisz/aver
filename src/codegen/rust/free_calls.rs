//! Rust backend: the calls it emits outside any function body.
//!
//! A `verify` case or law, a module binding and a top-level statement are
//! rendered as free values, so they call the program's functions with
//! whatever they write. The representation analysis lets a param that
//! never changes in its own recursion range over what its callers pass
//! (`bare_i64::caller_ranges`); these calls are callers too. Each argument
//! counts as the literal it is (a `given` bound to a literal counts as that
//! literal), anything else as unknown, and a function named as a value
//! here may be called with anything.

use std::collections::HashMap;

use crate::ast::{Expr, Literal, Spanned, Stmt, TopLevel, VerifyBlock, VerifyKind};
use crate::codegen::CodegenContext;
use crate::codegen::expr_walk::{for_each_child, walk};
use crate::ir::mir::bare_i64::FreeCalls;

/// Every call the backend emits outside a function body.
pub(super) fn collect(ctx: &CodegenContext) -> FreeCalls {
    let mut free = FreeCalls::default();
    let none = HashMap::new();
    for item in &ctx.items {
        match item {
            TopLevel::Verify(block) => verify_block(block, &mut free),
            TopLevel::Stmt(Stmt::Binding(_, _, e) | Stmt::Expr(e)) => expr(e, &none, &mut free),
            _ => {}
        }
    }
    for module in &ctx.modules {
        for block in module.verify_blocks.iter().chain(&module.verify_laws) {
            verify_block(block, &mut free);
        }
        for binding in &module.bindings {
            expr(&binding.value, &none, &mut free);
        }
    }
    free
}

fn verify_block(block: &VerifyBlock, free: &mut FreeCalls) {
    let none = HashMap::new();
    for (k, (left, right)) in block.cases.iter().enumerate() {
        let givens: HashMap<&str, &Spanned<Expr>> = block
            .case_givens
            .get(k)
            .map(|g| g.iter().map(|(name, e)| (name.as_str(), e)).collect())
            .unwrap_or_default();
        expr(left, &givens, free);
        expr(right, &givens, free);
    }
    // A law's own sides and conditions read its givens, which may be any
    // value (a deferred call that knows the law binds them at run time).
    if let VerifyKind::Law(law) = &block.kind {
        let sides = [&law.lhs, &law.rhs]
            .into_iter()
            .chain(&law.when)
            .chain(&law.because)
            .chain(&law.sample_guards);
        for e in sides {
            expr(e, &none, free);
        }
    }
}

/// Note every call in `e` and every function `e` names as a value.
fn expr(e: &Spanned<Expr>, givens: &HashMap<&str, &Spanned<Expr>>, free: &mut FreeCalls) {
    walk(e, &mut |node| {
        let Expr::FnCall(callee, args) = &node.node else {
            return;
        };
        match callee_name(&callee.node) {
            Some(name) => {
                let args: Vec<Option<i64>> = args.iter().map(|a| literal(a, givens)).collect();
                free.call(name, &args);
            }
            // A callee that is not a plain name is visited as a value.
            None => value_names(callee, free),
        }
        for arg in args.iter() {
            value_names(arg, free);
        }
    });
}

/// A function name used as a value at the top of `e` (not called there).
fn value_names(e: &Spanned<Expr>, free: &mut FreeCalls) {
    match &e.node {
        Expr::Ident(name) => free.named(last_segment(name)),
        Expr::Attr(_, name) => free.named(name),
        Expr::FnCall(..) => {}
        _ => for_each_child(e, &mut |child| value_names(child, free)),
    }
}

/// The name a call's callee spells, `f` or `Module.f`.
fn callee_name(callee: &Expr) -> Option<&str> {
    match callee {
        Expr::Ident(name) => Some(last_segment(name)),
        Expr::Attr(_, name) => Some(name),
        _ => None,
    }
}

fn last_segment(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

/// The `Int` literal `e` is, through a `given` bound to one; `None` for
/// anything else.
fn literal(e: &Spanned<Expr>, givens: &HashMap<&str, &Spanned<Expr>>) -> Option<i64> {
    match &e.node {
        Expr::Literal(Literal::Int(k)) => Some(*k),
        Expr::Neg(inner) => literal(inner, givens)?.checked_neg(),
        Expr::Ident(name) => givens
            .get(name.as_str())
            .and_then(|g| literal(g, &HashMap::new())),
        _ => None,
    }
}
