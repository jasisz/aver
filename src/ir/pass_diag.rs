//! Per-stage diagnostic counters for the pipeline orchestrator.
//!
//! `aver compile --explain-passes` and the future failable-invariant CI
//! gates need to know what each pass actually did — how many tail calls
//! TCO produced, how many interpolations got lowered, which sinks
//! buffer_build fired on, how many slots the resolver assigned, how
//! many last-use markers got annotated.
//!
//! Strategy: a single walker collects counts of every `Expr` shape that
//! a pass touches, per fn. The orchestrator captures a snapshot before
//! and after each stage and diffs them. No pass internals get
//! instrumented; the IR is the source of truth.

use std::collections::HashMap;

use crate::ast::{Expr, FnBody, Spanned, Stmt, StrPart, TopLevel};

/// Per-fn shape counts. Diff two snapshots to learn what a pass did.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ExprCounts {
    pub tail_calls: usize,
    pub interpolations: usize,
    pub resolved: usize,
    pub last_use_resolved: usize,
}

impl ExprCounts {
    pub fn add(&mut self, other: &Self) {
        self.tail_calls += other.tail_calls;
        self.interpolations += other.interpolations;
        self.resolved += other.resolved;
        self.last_use_resolved += other.last_use_resolved;
    }
}

pub(crate) type CountsByFn = HashMap<String, ExprCounts>;

pub(crate) fn collect(items: &[TopLevel]) -> CountsByFn {
    let mut out: CountsByFn = HashMap::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            let mut acc = ExprCounts::default();
            count_body(&fd.body, &mut acc);
            out.insert(fd.name.clone(), acc);
        }
    }
    out
}

pub(crate) fn total(counts: &CountsByFn) -> ExprCounts {
    let mut acc = ExprCounts::default();
    for v in counts.values() {
        acc.add(v);
    }
    acc
}

/// Returns fn names whose `pick(counts)` strictly grew between `pre` and
/// `post`, sorted alphabetically. Used for "which fns saw new tail calls"
/// style diagnostic detail bullets.
pub(crate) fn fns_that_grew(
    pre: &CountsByFn,
    post: &CountsByFn,
    pick: impl Fn(&ExprCounts) -> usize,
) -> Vec<String> {
    let mut names: Vec<String> = post
        .iter()
        .filter(|(name, after)| {
            let before = pre.get(*name).copied().unwrap_or_default();
            pick(after) > pick(&before)
        })
        .map(|(name, _)| name.clone())
        .collect();
    names.sort();
    names
}

fn count_body(body: &FnBody, acc: &mut ExprCounts) {
    let FnBody::Block(stmts) = body;
    for stmt in stmts {
        count_stmt(stmt, acc);
    }
}

fn count_stmt(stmt: &Stmt, acc: &mut ExprCounts) {
    match stmt {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => count_expr(e, acc),
    }
}

fn count_expr(e: &Spanned<Expr>, acc: &mut ExprCounts) {
    match &e.node {
        Expr::TailCall(boxed) => {
            acc.tail_calls += 1;
            for arg in &boxed.args {
                count_expr(arg, acc);
            }
        }
        Expr::InterpolatedStr(parts) => {
            acc.interpolations += 1;
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    count_expr(inner, acc);
                }
            }
        }
        Expr::Resolved { last_use, .. } => {
            acc.resolved += 1;
            if last_use.0 {
                acc.last_use_resolved += 1;
            }
        }
        Expr::FnCall(func, args) => {
            count_expr(func, acc);
            for arg in args {
                count_expr(arg, acc);
            }
        }
        Expr::BinOp(_, l, r) => {
            count_expr(l, acc);
            count_expr(r, acc);
        }
        Expr::Match { subject, arms } => {
            count_expr(subject, acc);
            for arm in arms {
                count_expr(&arm.body, acc);
            }
        }
        Expr::Attr(obj, _) | Expr::ErrorProp(obj) => count_expr(obj, acc),
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            for x in xs {
                count_expr(x, acc);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                count_expr(k, acc);
                count_expr(v, acc);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                count_expr(v, acc);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            count_expr(base, acc);
            for (_, v) in updates {
                count_expr(v, acc);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(arg) = payload.as_deref() {
                count_expr(arg, acc);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) => {}
    }
}
