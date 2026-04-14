//! Backend-agnostic liveness analysis.
//!
//! Computes `used_after` sets for statement sequences and argument lists.
//! Each set tells which variables are referenced after a given program point,
//! enabling move-instead-of-copy decisions in any backend.
//!
//! Backend-specific policy (is_copy, should_borrow, etc.) stays in
//! `codegen/rust/liveness.rs` and friends.

use std::collections::HashSet;

use crate::ast::*;
use crate::ir::vars::{collect_vars, collect_vars_stmt};

/// Variables known to be live after a specific program point.
#[derive(Debug, Clone, Default)]
pub struct UsedAfter {
    pub vars: HashSet<String>,
}

impl UsedAfter {
    pub fn empty() -> Self {
        Self {
            vars: HashSet::new(),
        }
    }

    /// Is this variable's last use at (or before) the current point?
    /// If not in `used_after`, the current use is the last one → safe to move.
    pub fn is_last_use(&self, name: &str) -> bool {
        !self.vars.contains(name)
    }
}

/// Compute `used_after` sets for a sequence of statements.
///
/// Returns one `UsedAfter` per statement, where entry `[i]` contains
/// variables referenced in `stmts[i+1..]` ∪ `parent_used_after`.
///
/// Walk is backwards: we accumulate from the end so each statement knows
/// what comes after it.
pub fn compute_block_used_after(
    stmts: &[Stmt],
    parent_used_after: &HashSet<String>,
) -> Vec<UsedAfter> {
    let n = stmts.len();
    let mut result = vec![UsedAfter::empty(); n];

    let mut suffix_vars = parent_used_after.clone();
    for i in (0..n).rev() {
        result[i] = UsedAfter {
            vars: suffix_vars.clone(),
        };
        let stmt_vars = collect_vars_stmt(&stmts[i]);
        suffix_vars.extend(stmt_vars);
        // A binding defines a name — remove it from suffix (it doesn't
        // flow from outer scope, it's born here).
        if let Stmt::Binding(name, _, _) = &stmts[i] {
            suffix_vars.remove(name);
        }
    }

    result
}

/// Compute `used_after` sets for a list of expressions (e.g. function arguments).
///
/// Entry `[i]` contains variables referenced in `args[i+1..]` ∪ `parent_used_after`.
/// Evaluated left-to-right, so earlier args know about later args' variable usage.
pub fn compute_args_used_after(
    args: &[Spanned<Expr>],
    parent_used_after: &HashSet<String>,
) -> Vec<UsedAfter> {
    let n = args.len();
    let mut result = vec![UsedAfter::empty(); n];

    let mut suffix_vars = parent_used_after.clone();
    for i in (0..n).rev() {
        result[i] = UsedAfter {
            vars: suffix_vars.clone(),
        };
        let arg_vars = collect_vars(&args[i].node);
        suffix_vars.extend(arg_vars);
    }

    result
}

/// Same as `compute_args_used_after` but for bare `&[Expr]`.
pub fn compute_args_used_after_bare(
    args: &[Expr],
    parent_used_after: &HashSet<String>,
) -> Vec<UsedAfter> {
    let n = args.len();
    let mut result = vec![UsedAfter::empty(); n];

    let mut suffix_vars = parent_used_after.clone();
    for i in (0..n).rev() {
        result[i] = UsedAfter {
            vars: suffix_vars.clone(),
        };
        let arg_vars = collect_vars(&args[i]);
        suffix_vars.extend(arg_vars);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_block_used_after() {
        let stmts = vec![
            Stmt::Binding(
                "a".to_string(),
                None,
                Spanned::bare(Expr::Literal(Literal::Int(1))),
            ),
            Stmt::Expr(Spanned::bare(Expr::BinOp(
                BinOp::Add,
                Box::new(Spanned::bare(Expr::Ident("a".to_string()))),
                Box::new(Spanned::bare(Expr::Ident("b".to_string()))),
            ))),
        ];
        let ctxs = compute_block_used_after(&stmts, &HashSet::new());
        // stmt[0]: used_after = vars_in(stmt[1]) = {a, b}
        assert!(ctxs[0].vars.contains("a"));
        assert!(ctxs[0].vars.contains("b"));
        // stmt[1]: used_after = parent = {}
        assert!(ctxs[1].vars.is_empty());
    }

    #[test]
    fn test_block_binding_removes_from_suffix() {
        let stmts = vec![
            Stmt::Expr(Spanned::bare(Expr::Ident("x".to_string()))),
            Stmt::Binding(
                "x".to_string(),
                None,
                Spanned::bare(Expr::Literal(Literal::Int(1))),
            ),
            Stmt::Expr(Spanned::bare(Expr::Ident("x".to_string()))),
        ];
        let ctxs = compute_block_used_after(&stmts, &HashSet::new());
        // stmt[0]: x from stmt[2] is after a binding of x in stmt[1],
        // so stmt[0]'s used_after should NOT contain x (the binding shadows it)
        assert!(!ctxs[0].vars.contains("x"));
    }

    #[test]
    fn test_args_used_after() {
        let args = vec![
            Spanned::bare(Expr::Ident("x".to_string())),
            Spanned::bare(Expr::Ident("y".to_string())),
            Spanned::bare(Expr::Ident("x".to_string())),
        ];
        let ctxs = compute_args_used_after(&args, &HashSet::new());
        // arg[0]: used_after = {y, x} (from args 1,2)
        assert!(ctxs[0].vars.contains("x"));
        assert!(ctxs[0].vars.contains("y"));
        // arg[1]: used_after = {x} (from arg 2)
        assert!(ctxs[1].vars.contains("x"));
        assert!(!ctxs[1].vars.contains("y"));
        // arg[2]: used_after = parent = {}
        assert!(ctxs[2].vars.is_empty());
    }

    #[test]
    fn test_is_last_use() {
        let ua = UsedAfter {
            vars: HashSet::from(["x".to_string()]),
        };
        assert!(!ua.is_last_use("x")); // x is used after → NOT last use
        assert!(ua.is_last_use("y")); // y is NOT used after → IS last use
    }
}
