/// Rust-specific emission context wrapping backend-agnostic liveness from `ir::liveness`.
///
/// `EmitCtx` adds Rust codegen policy (Copy types, borrow semantics, Rc wrapping)
/// on top of the generic `used_after` sets computed by `ir::liveness`.
use crate::ast::*;
use crate::types::Type;
use std::collections::{HashMap, HashSet};

use crate::ir::vars::collect_vars;

/// Emission context carrying liveness info + Rust-specific policy.
#[derive(Clone)]
pub struct EmitCtx {
    /// Variables known to be used after the current emission point.
    pub used_after: HashSet<String>,
    /// Local variable types (from fn params) for copy-type elision.
    pub local_types: HashMap<String, Type>,
    /// Parameters passed as `&T` borrows (pass-through TCO optimization).
    pub rc_wrapped: HashSet<String>,
    /// Parameters emitted as `&T` borrows (borrow-by-default for non-Copy, non-Str params).
    pub borrowed_params: HashSet<String>,
}

impl EmitCtx {
    /// Empty context — conservative (clones everything).
    pub fn empty() -> Self {
        EmitCtx {
            used_after: HashSet::new(),
            local_types: HashMap::new(),
            rc_wrapped: HashSet::new(),
            borrowed_params: HashSet::new(),
        }
    }

    /// Build context for a function with known parameter types.
    /// Automatically computes `borrowed_params` from param types.
    pub fn for_fn(param_types: HashMap<String, Type>) -> Self {
        let borrowed_params = param_types
            .iter()
            .filter(|(_, ty)| should_borrow_param(ty))
            .map(|(name, _)| name.clone())
            .collect();
        EmitCtx {
            used_after: HashSet::new(),
            local_types: param_types,
            rc_wrapped: HashSet::new(),
            borrowed_params,
        }
    }

    /// Build context for a function WITHOUT borrow-by-default (e.g. TCO, memo).
    pub fn for_fn_no_borrow(param_types: HashMap<String, Type>) -> Self {
        EmitCtx {
            used_after: HashSet::new(),
            local_types: param_types,
            rc_wrapped: HashSet::new(),
            borrowed_params: HashSet::new(),
        }
    }

    /// Can this variable be moved (not cloned)?
    /// True when it's NOT in `used_after` (i.e. this is its last use).
    pub fn can_move(&self, name: &str) -> bool {
        !self.used_after.contains(name)
    }

    /// Is this variable a Copy type in Rust (i64, f64, bool, ())?
    pub fn is_copy(&self, name: &str) -> bool {
        self.local_types.get(name).is_some_and(is_copy_type)
    }

    /// Should `.clone()` be skipped for this variable?
    pub fn skip_clone(&self, name: &str) -> bool {
        if self.rc_wrapped.contains(name) {
            return false;
        }
        if self.borrowed_params.contains(name) {
            return false;
        }
        self.is_copy(name) || self.can_move(name)
    }

    /// Is this variable a pass-through parameter (Rc<T> in self-TCO, &T in mutual-TCO)?
    pub fn is_rc_wrapped(&self, name: &str) -> bool {
        self.rc_wrapped.contains(name)
    }

    /// Is this variable a borrowed parameter (`&T` from borrow-by-default)?
    pub fn is_borrowed_param(&self, name: &str) -> bool {
        self.borrowed_params.contains(name)
    }

    /// Create a child context with additional used_after variables.
    pub fn with_used_after(&self, extra: &HashSet<String>) -> Self {
        let mut ua = self.used_after.clone();
        ua.extend(extra.iter().cloned());
        EmitCtx {
            used_after: ua,
            local_types: self.local_types.clone(),
            rc_wrapped: self.rc_wrapped.clone(),
            borrowed_params: self.borrowed_params.clone(),
        }
    }

    /// Create a context with specified borrowed (`&T`) parameters (TCO pass-through).
    pub fn with_rc_wrapped(&self, rc: HashSet<String>) -> Self {
        EmitCtx {
            used_after: self.used_after.clone(),
            local_types: self.local_types.clone(),
            rc_wrapped: rc,
            borrowed_params: self.borrowed_params.clone(),
        }
    }
}

// ── Rust-specific policy ──────────────────────────────────────────────

/// Is a Type Copy in Rust? (Int, Float, Bool, Unit)
pub fn is_copy_type(ty: &Type) -> bool {
    matches!(ty, Type::Int | Type::Float | Type::Bool | Type::Unit)
}

/// Should this param be borrowed (`&T`) instead of owned?
pub fn should_borrow_param(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Map(_, _)
            | Type::List(_)
            | Type::Vector(_)
            | Type::Result(_, _)
            | Type::Option(_)
            | Type::Tuple(_)
            | Type::Named(_)
    )
}

// ── EmitCtx builders delegating to ir::liveness ───────────────────────

/// Compute `used_after` sets for statements, wrapped in EmitCtx.
pub fn compute_block_used_after(
    stmts: &[Stmt],
    parent_used_after: &HashSet<String>,
    local_types: &HashMap<String, Type>,
) -> Vec<EmitCtx> {
    compute_block_used_after_full(
        stmts,
        parent_used_after,
        local_types,
        &HashSet::new(),
        &HashSet::new(),
    )
}

/// Like `compute_block_used_after` but propagates `rc_wrapped`.
pub fn compute_block_used_after_with_rc(
    stmts: &[Stmt],
    parent_used_after: &HashSet<String>,
    local_types: &HashMap<String, Type>,
    rc_wrapped: &HashSet<String>,
) -> Vec<EmitCtx> {
    compute_block_used_after_full(
        stmts,
        parent_used_after,
        local_types,
        rc_wrapped,
        &HashSet::new(),
    )
}

/// Full version — delegates liveness to `ir::liveness`, wraps in EmitCtx.
pub fn compute_block_used_after_full(
    stmts: &[Stmt],
    parent_used_after: &HashSet<String>,
    local_types: &HashMap<String, Type>,
    rc_wrapped: &HashSet<String>,
    borrowed_params: &HashSet<String>,
) -> Vec<EmitCtx> {
    let ir_results = crate::ir::liveness::compute_block_used_after(stmts, parent_used_after);
    ir_results
        .into_iter()
        .map(|ua| EmitCtx {
            used_after: ua.vars,
            local_types: local_types.clone(),
            rc_wrapped: rc_wrapped.clone(),
            borrowed_params: borrowed_params.clone(),
        })
        .collect()
}

/// Compute `used_after` for argument expressions, wrapped in EmitCtx.
pub fn compute_args_used_after_full(
    args: &[Expr],
    parent_used_after: &HashSet<String>,
    local_types: &HashMap<String, Type>,
    rc_wrapped: &HashSet<String>,
    borrowed_params: &HashSet<String>,
) -> Vec<EmitCtx> {
    // ir::liveness works on Spanned<Expr>, we have bare Expr.
    let ir_results = crate::ir::liveness::compute_args_used_after_bare(args, parent_used_after);
    ir_results
        .into_iter()
        .map(|ua| EmitCtx {
            used_after: ua.vars,
            local_types: local_types.clone(),
            rc_wrapped: rc_wrapped.clone(),
            borrowed_params: borrowed_params.clone(),
        })
        .collect()
}

/// Like `compute_args_used_after_full` but for `&[&Expr]` references.
#[allow(dead_code)]
pub fn compute_args_used_after_full_refs(
    args: &[&Expr],
    parent_used_after: &HashSet<String>,
    local_types: &HashMap<String, Type>,
    rc_wrapped: &HashSet<String>,
    borrowed_params: &HashSet<String>,
) -> Vec<EmitCtx> {
    // Compute inline since ir::liveness doesn't have a &[&Expr] variant
    let n = args.len();
    let mut result = vec![EmitCtx::empty(); n];
    let mut suffix_vars = parent_used_after.clone();
    for i in (0..n).rev() {
        result[i] = EmitCtx {
            used_after: suffix_vars.clone(),
            local_types: local_types.clone(),
            rc_wrapped: rc_wrapped.clone(),
            borrowed_params: borrowed_params.clone(),
        };
        let arg_vars = collect_vars(args[i]);
        suffix_vars.extend(arg_vars);
    }
    result
}

/// Like `compute_args_used_after` but propagates `rc_wrapped`.
#[cfg(test)]
pub fn compute_args_used_after_with_rc(
    args: &[Spanned<Expr>],
    parent_used_after: &HashSet<String>,
    local_types: &HashMap<String, Type>,
    rc_wrapped: &HashSet<String>,
) -> Vec<EmitCtx> {
    let bare_args: Vec<&Expr> = args.iter().map(|a| &a.node).collect();
    compute_args_used_after_full_refs(
        &bare_args,
        parent_used_after,
        local_types,
        rc_wrapped,
        &HashSet::new(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_copy_type() {
        assert!(is_copy_type(&Type::Int));
        assert!(is_copy_type(&Type::Float));
        assert!(is_copy_type(&Type::Bool));
        assert!(is_copy_type(&Type::Unit));
        assert!(!is_copy_type(&Type::Str));
        assert!(!is_copy_type(&Type::List(Box::new(Type::Int))));
        assert!(!is_copy_type(&Type::Named("Foo".to_string())));
    }

    #[test]
    fn test_collect_vars_simple() {
        let expr = Expr::Ident("x".to_string());
        let vars = collect_vars(&expr);
        assert_eq!(vars, HashSet::from(["x".to_string()]));
    }

    #[test]
    fn test_collect_vars_binop() {
        let expr = Expr::BinOp(
            BinOp::Add,
            Box::new(Spanned::bare(Expr::Ident("x".to_string()))),
            Box::new(Spanned::bare(Expr::Ident("y".to_string()))),
        );
        let vars = collect_vars(&expr);
        assert_eq!(vars, HashSet::from(["x".to_string(), "y".to_string()]));
    }

    #[test]
    fn test_skip_clone_copy_type() {
        let mut lt = HashMap::new();
        lt.insert("n".to_string(), Type::Int);
        lt.insert("s".to_string(), Type::Str);
        let ectx = EmitCtx {
            used_after: HashSet::from(["n".to_string(), "s".to_string()]),
            local_types: lt,
            rc_wrapped: HashSet::new(),
            borrowed_params: HashSet::new(),
        };
        assert!(ectx.skip_clone("n"));
        assert!(!ectx.skip_clone("s"));
    }

    #[test]
    fn test_skip_clone_last_use() {
        let mut lt = HashMap::new();
        lt.insert("s".to_string(), Type::Str);
        let ectx = EmitCtx {
            used_after: HashSet::new(),
            local_types: lt,
            rc_wrapped: HashSet::new(),
            borrowed_params: HashSet::new(),
        };
        assert!(ectx.skip_clone("s"));
    }

    #[test]
    fn test_compute_block_used_after() {
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
        let parent = HashSet::new();
        let lt = HashMap::new();
        let ctxs = compute_block_used_after(&stmts, &parent, &lt);
        assert!(ctxs[0].used_after.contains("a"));
        assert!(ctxs[0].used_after.contains("b"));
        assert!(ctxs[1].used_after.is_empty());
    }

    #[test]
    fn test_compute_args_used_after() {
        let args = vec![
            Spanned::bare(Expr::Ident("x".to_string())),
            Spanned::bare(Expr::Ident("y".to_string())),
            Spanned::bare(Expr::Ident("x".to_string())),
        ];
        let parent = HashSet::new();
        let lt = HashMap::new();
        let ctxs = compute_args_used_after_with_rc(&args, &parent, &lt, &HashSet::new());
        assert!(ctxs[0].used_after.contains("x"));
        assert!(ctxs[0].used_after.contains("y"));
        assert!(ctxs[1].used_after.contains("x"));
        assert!(!ctxs[1].used_after.contains("y"));
        assert!(ctxs[2].used_after.is_empty());
    }
}
