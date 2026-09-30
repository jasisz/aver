//! Two-phase integer walks. A guarded member delegates at the same counter;
//! its worker advances one step and returns. The worker is total even when
//! called outside the guard: its gap is offset by -1 before conversion to Nat.

use super::detect::{call_matches, expr_to_dotted_name, pattern_bound_names};
use crate::ast::{BinOp, Expr, FnDef, Literal, Pattern, Spanned, Stmt};

fn ident(expr: &Spanned<Expr>, name: &str) -> bool {
    matches!(&expr.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == name)
}

fn zero(expr: &Spanned<Expr>) -> bool {
    matches!(expr.node, Expr::Literal(Literal::Int(0)))
}

fn progresses(expr: &Spanned<Expr>, name: &str, ascending: bool) -> bool {
    let expected = if ascending { BinOp::Add } else { BinOp::Sub };
    matches!(&expr.node, Expr::BinOp(op, left, right)
        if *op == expected
            && ident(left, name) && matches!(right.node, Expr::Literal(Literal::Int(1))))
}

struct Walk<'a> {
    counter: &'a str,
    bound: Option<&'a str>,
    peer: &'a FnDef,
    counter_index: usize,
    bound_index: Option<usize>,
    worker: bool,
    found: bool,
    valid: bool,
}

impl Walk<'_> {
    fn protects(&self, expr: &Spanned<Expr>, arm: bool) -> bool {
        let Expr::BinOp(op, a, b) = &expr.node else {
            return false;
        };
        if let Some(bound) = self.bound {
            match op {
                BinOp::Lt => arm && ident(a, self.counter) && ident(b, bound),
                BinOp::Gt => arm && ident(a, bound) && ident(b, self.counter),
                BinOp::Gte => !arm && ident(a, self.counter) && ident(b, bound),
                BinOp::Lte => !arm && ident(a, bound) && ident(b, self.counter),
                _ => false,
            }
        } else {
            match op {
                BinOp::Gt => arm && ident(a, self.counter) && zero(b),
                BinOp::Lt => arm && zero(a) && ident(b, self.counter),
                BinOp::Lte => !arm && ident(a, self.counter) && zero(b),
                BinOp::Gte => !arm && zero(a) && ident(b, self.counter),
                _ => false,
            }
        }
    }

    fn shadows(&self, name: &str) -> bool {
        name == self.counter || self.bound == Some(name)
    }

    fn expr(&mut self, expr: &Spanned<Expr>, live: bool, guarded: bool) {
        let call = match &expr.node {
            Expr::FnCall(callee, args) => {
                expr_to_dotted_name(callee).map(|name| (name, args.as_slice()))
            }
            Expr::TailCall(call) => Some((call.target.clone(), call.args.as_slice())),
            _ => None,
        };
        if let Some((name, args)) = call
            && call_matches(&name, &self.peer.name)
        {
            self.found = true;
            self.valid &= live
                && args.len() == self.peer.params.len()
                && args.get(self.counter_index).is_some_and(|arg| {
                    if self.worker {
                        progresses(arg, self.counter, self.bound.is_some())
                    } else {
                        guarded && ident(arg, self.counter)
                    }
                })
                && self
                    .bound_index
                    .zip(self.bound)
                    .is_none_or(|(index, name)| {
                        args.get(index).is_some_and(|arg| ident(arg, name))
                    });
        }
        if let Expr::Match { subject, arms } = &expr.node {
            self.expr(subject, live, guarded);
            for arm in arms {
                let live = live
                    && !pattern_bound_names(&arm.pattern)
                        .iter()
                        .any(|n| self.shadows(n));
                let protected = matches!(arm.pattern, Pattern::Literal(Literal::Bool(value))
                    if self.protects(subject, value));
                self.expr(&arm.body, live, guarded || protected);
            }
        } else {
            crate::codegen::expr_walk::for_each_child(expr, &mut |child| {
                self.expr(child, live, guarded)
            });
        }
    }
}

fn member_matches(
    fd: &FnDef,
    peer: &FnDef,
    counter_index: usize,
    bound_index: Option<usize>,
    worker: bool,
) -> bool {
    let Some((counter, ty)) = fd.params.get(counter_index) else {
        return false;
    };
    if ty != "Int" {
        return false;
    }
    let bound = bound_index.and_then(|i| {
        fd.params
            .get(i)
            .filter(|(_, ty)| ty == "Int")
            .map(|(n, _)| n.as_str())
    });
    if bound_index.is_some() && bound.is_none() {
        return false;
    }
    let mut walk = Walk {
        counter,
        bound,
        peer,
        counter_index,
        bound_index,
        worker,
        found: false,
        valid: true,
    };
    let mut live = true;
    for stmt in fd.body.stmts() {
        let (binding, expr) = match stmt {
            Stmt::Binding(name, _, expr) => (Some(name.as_str()), expr),
            Stmt::Expr(expr) => (None, expr),
        };
        // Self-edges are not part of this two-phase shape, even nested in arguments.
        if crate::codegen::expr_walk::any(expr, &mut |expr| match &expr.node {
            Expr::FnCall(callee, _) => {
                expr_to_dotted_name(callee).is_some_and(|n| call_matches(&n, &fd.name))
            }
            Expr::TailCall(call) => call_matches(&call.target, &fd.name),
            _ => false,
        }) {
            return false;
        }
        walk.expr(expr, live, false);
        if binding.is_some_and(|name| walk.shadows(name)) {
            live = false;
        }
    }
    walk.found && walk.valid
}

/// Return (counter slot, optional upper-bound slot, guarded member index).
/// Only literal unit steps and a preserved parameter bound are accepted.
pub(crate) fn detect(fns: &[&FnDef]) -> Option<(usize, Option<usize>, usize)> {
    if fns.len() != 2 {
        return None;
    }
    for guarded in 0..2 {
        let worker = 1 - guarded;
        for (counter, (_, ty)) in fns[guarded].params.iter().enumerate() {
            if ty != "Int" {
                continue;
            }
            let bounds = std::iter::once(None).chain(
                fns[guarded]
                    .params
                    .iter()
                    .enumerate()
                    .filter(|(i, (_, ty))| *i != counter && ty == "Int")
                    .map(|(i, _)| Some(i)),
            );
            for bound in bounds {
                if member_matches(fns[guarded], fns[worker], counter, bound, false)
                    && member_matches(fns[worker], fns[guarded], counter, bound, true)
                {
                    return Some((counter, bound, guarded));
                }
            }
        }
    }
    None
}

#[cfg(test)]
mod tests;
