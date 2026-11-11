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
//!
//! A call names its function as the source writes it: unqualified within
//! the module that holds the expression, or by its module's path. It is
//! found by its canonical name (`Module.f`, `f` in the entry module); a name
//! that resolves to none of the program's functions counts for every
//! function with that last segment, which can only take ranges away.

use std::collections::HashMap;

use crate::ast::{Expr, Literal, Spanned, Stmt, TopLevel, VerifyBlock, VerifyKind};
use crate::codegen::CodegenContext;
use crate::codegen::expr_walk::{for_each_child, walk};
use crate::ir::FnId;
use crate::ir::mir::MirProgram;
use crate::ir::mir::bare_i64::FreeCalls;

/// Every call the backend emits outside a function body, by the functions
/// of `program` it may call.
pub(super) fn collect(ctx: &CodegenContext, program: &MirProgram) -> FreeCalls {
    let names = Names::of(ctx, program);
    let mut cx = Collect {
        names: &names,
        free: FreeCalls::default(),
        scope: None,
    };
    let none = HashMap::new();
    for item in &ctx.items {
        match item {
            TopLevel::Verify(block) => cx.verify_block(block),
            TopLevel::Stmt(Stmt::Binding(_, _, e) | Stmt::Expr(e)) => cx.expr(e, &none),
            _ => {}
        }
    }
    for module in &ctx.modules {
        cx.scope = Some(module.prefix.as_str());
        for block in module.verify_blocks.iter().chain(&module.verify_laws) {
            cx.verify_block(block);
        }
        for binding in &module.bindings {
            cx.expr(&binding.value, &none);
        }
    }
    cx.free
}

/// The program's functions by canonical name and by last segment.
struct Names {
    canonical: HashMap<String, FnId>,
    short: HashMap<String, Vec<FnId>>,
}

impl Names {
    fn of(ctx: &CodegenContext, program: &MirProgram) -> Names {
        let mut canonical = HashMap::new();
        let mut short: HashMap<String, Vec<FnId>> = HashMap::new();
        for (id, f) in program.iter() {
            canonical.insert(ctx.symbol_table.fn_entry(*id).key.canonical(), *id);
            short.entry(f.name.clone()).or_default().push(*id);
        }
        Names { canonical, short }
    }

    /// The functions `written` (as the source spells it, in the module
    /// `scope`) may be.
    fn resolve(&self, written: &str, scope: Option<&str>) -> Vec<FnId> {
        let canonical = match (written.contains('.'), scope) {
            (false, Some(prefix)) => format!("{prefix}.{written}"),
            _ => written.to_string(),
        };
        if let Some(id) = self.canonical.get(&canonical) {
            return vec![*id];
        }
        let last = written.rsplit('.').next().unwrap_or(written);
        self.short.get(last).cloned().unwrap_or_default()
    }
}

struct Collect<'a> {
    names: &'a Names,
    free: FreeCalls,
    /// The module whose expressions are read; `None` for the entry module.
    scope: Option<&'a str>,
}

impl Collect<'_> {
    fn verify_block(&mut self, block: &VerifyBlock) {
        let none = HashMap::new();
        for (k, (left, right)) in block.cases.iter().enumerate() {
            let givens: HashMap<&str, &Spanned<Expr>> = block
                .case_givens
                .get(k)
                .map(|g| g.iter().map(|(name, e)| (name.as_str(), e)).collect())
                .unwrap_or_default();
            self.expr(left, &givens);
            self.expr(right, &givens);
        }
        // A law's own sides and conditions read its givens, which may be
        // any value (a deferred call that knows the law binds them at run
        // time).
        if let VerifyKind::Law(law) = &block.kind {
            let sides = [&law.lhs, &law.rhs]
                .into_iter()
                .chain(&law.when)
                .chain(&law.because)
                .chain(&law.sample_guards);
            for e in sides {
                self.expr(e, &none);
            }
        }
    }

    /// Note every call in `e` and every function `e` names as a value.
    fn expr(&mut self, e: &Spanned<Expr>, givens: &HashMap<&str, &Spanned<Expr>>) {
        let mut calls: Vec<(String, Vec<Option<i64>>)> = Vec::new();
        let mut values: Vec<String> = Vec::new();
        walk(e, &mut |node| {
            let Expr::FnCall(callee, args) = &node.node else {
                return;
            };
            match dotted(&callee.node) {
                Some(name) => {
                    let args = args.iter().map(|a| literal(a, givens)).collect();
                    calls.push((name, args));
                }
                // A callee that is not a plain name is visited as a value.
                None => value_names(callee, &mut values),
            }
            for arg in args.iter() {
                value_names(arg, &mut values);
            }
        });
        for (name, args) in calls {
            for id in self.names.resolve(&name, self.scope) {
                self.free.call(id, &args);
            }
        }
        for name in values {
            for id in self.names.resolve(&name, self.scope) {
                self.free.named(id);
            }
        }
    }
}

/// The function names used as values at the top of `e` (not called there).
fn value_names(e: &Spanned<Expr>, out: &mut Vec<String>) {
    match &e.node {
        Expr::Ident(_) | Expr::Attr(..) => out.extend(dotted(&e.node)),
        Expr::FnCall(..) => {}
        _ => for_each_child(e, &mut |child| value_names(child, out)),
    }
}

/// The dotted name `e` spells, `f` or `Module.f`.
fn dotted(e: &Expr) -> Option<String> {
    match e {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(base, field) => Some(format!("{}.{field}", dotted(&base.node)?)),
        _ => None,
    }
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
