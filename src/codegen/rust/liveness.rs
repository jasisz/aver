use crate::ast::*;
use crate::types::Type;
/// Last-use analysis and copy-type elision for Aver→Rust codegen.
///
/// Provides `EmitCtx` — a context threaded through expression emission
/// that tracks which variables are used after the current point, enabling
/// move-instead-of-clone when a variable is at its last use.
use std::collections::{HashMap, HashSet};

/// Emission context carrying liveness info for clone elision.
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
    /// True if it's Copy OR if it's last-use (can move).
    /// Pass-through params (Rc-wrapped in self-TCO, or `&T` in mutual-TCO trampoline)
    /// always need `(*param).clone()` to produce owned T, never skip.
    /// Borrowed params (`&T`) also never skip — they always need explicit handling.
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

/// Is a Type Copy in Rust? (Int, Float, Bool, Unit)
pub fn is_copy_type(ty: &Type) -> bool {
    matches!(ty, Type::Int | Type::Float | Type::Bool | Type::Unit)
}

/// Should this type be passed by borrow (`&T`) in user function parameters?
/// Should this param be borrowed (`&T`) instead of owned?
/// Copy types pass by value. Str (AverStr = Rc<str>) is cheap to clone.
/// Named types stay owned — their fields are mostly Rc-wrapped containers
/// (AverMap, AverList, AverVector, AverStr), so clone is refcount bumps,
/// not deep copies. Borrow still eliminates the clone calls entirely,
/// which reduces refcount churn and can improve optimizer output.
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

/// Collect all variable names referenced in an expression.
/// Does NOT descend into match pattern bindings (they introduce new names).
pub fn collect_vars(expr: &Expr) -> HashSet<String> {
    let mut vars = HashSet::new();
    collect_vars_inner(expr, &mut vars);
    vars
}

fn collect_vars_inner(expr: &Expr, vars: &mut HashSet<String>) {
    match expr {
        Expr::Ident(name) => {
            vars.insert(name.clone());
        }
        Expr::Resolved(_) => {}
        Expr::Literal(_) => {}
        Expr::Attr(obj, _) => collect_vars_inner(&obj.node, vars),
        Expr::FnCall(fn_expr, args) => {
            collect_vars_inner(&fn_expr.node, vars);
            for a in args {
                collect_vars_inner(&a.node, vars);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_vars_inner(&left.node, vars);
            collect_vars_inner(&right.node, vars);
        }
        Expr::Match { subject, arms, .. } => {
            collect_vars_inner(&subject.node, vars);
            for arm in arms {
                // Collect vars from arm body, minus pattern-bound names
                let mut arm_vars = HashSet::new();
                collect_vars_inner(&arm.body.node, &mut arm_vars);
                let bindings = pattern_bindings(&arm.pattern);
                for v in arm_vars {
                    if !bindings.contains(&v) {
                        vars.insert(v);
                    }
                }
            }
        }
        Expr::Constructor(_, Some(inner)) => collect_vars_inner(&inner.node, vars),
        Expr::Constructor(_, None) => {}
        Expr::ErrorProp(inner) => collect_vars_inner(&inner.node, vars),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(expr) = part {
                    collect_vars_inner(&expr.node, vars);
                }
            }
        }
        Expr::List(elements) => {
            for e in elements {
                collect_vars_inner(&e.node, vars);
            }
        }
        Expr::Tuple(items) => {
            for e in items {
                collect_vars_inner(&e.node, vars);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_vars_inner(&k.node, vars);
                collect_vars_inner(&v.node, vars);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                collect_vars_inner(&expr.node, vars);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_vars_inner(&base.node, vars);
            for (_, expr) in updates {
                collect_vars_inner(&expr.node, vars);
            }
        }
        Expr::TailCall(boxed) => {
            let (_, args) = boxed.as_ref();
            for a in args {
                collect_vars_inner(&a.node, vars);
            }
        }
    }
}

/// Collect variables in a statement.
pub fn collect_vars_stmt(stmt: &Stmt) -> HashSet<String> {
    match stmt {
        Stmt::Binding(_, _, expr) => collect_vars(&expr.node),
        Stmt::Expr(expr) => collect_vars(&expr.node),
    }
}

/// Get names bound by a pattern (for excluding from parent scope liveness).
pub fn pattern_bindings(pat: &Pattern) -> HashSet<String> {
    let mut bindings = HashSet::new();
    match pat {
        Pattern::Ident(name) => {
            if name != "_" {
                bindings.insert(name.clone());
            }
        }
        Pattern::Cons(head, tail) => {
            if head != "_" {
                bindings.insert(head.clone());
            }
            if tail != "_" {
                bindings.insert(tail.clone());
            }
        }
        Pattern::Constructor(_, fields) => {
            for f in fields {
                if f != "_" {
                    bindings.insert(f.clone());
                }
            }
        }
        Pattern::Tuple(pats) => {
            for p in pats {
                bindings.extend(pattern_bindings(p));
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
    }
    bindings
}

/// Compute `used_after` sets for a sequence of statements.
/// Returns a Vec of EmitCtx, one per statement, where each ctx
/// has `used_after = vars_in(stmt[i+1..]) ∪ parent_used_after`.
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

/// Like `compute_block_used_after` but propagates `rc_wrapped` and `borrowed_params` to child contexts.
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

/// Full version that propagates all context fields including `borrowed_params`.
pub fn compute_block_used_after_full(
    stmts: &[Stmt],
    parent_used_after: &HashSet<String>,
    local_types: &HashMap<String, Type>,
    rc_wrapped: &HashSet<String>,
    borrowed_params: &HashSet<String>,
) -> Vec<EmitCtx> {
    let n = stmts.len();
    let mut result = vec![EmitCtx::empty(); n];

    // Accumulate from the end: for each stmt[i], used_after = vars_in(stmts[i+1..]) ∪ parent
    let mut suffix_vars = parent_used_after.clone();
    for i in (0..n).rev() {
        result[i] = EmitCtx {
            used_after: suffix_vars.clone(),
            local_types: local_types.clone(),
            rc_wrapped: rc_wrapped.clone(),
            borrowed_params: borrowed_params.clone(),
        };
        // Add vars from this statement to suffix for the next one (going backwards)
        let stmt_vars = collect_vars_stmt(&stmts[i]);
        suffix_vars.extend(stmt_vars);
        // If this is a binding, remove the bound name from suffix_vars
        // (it's defined here, not a use from outer scope)
        if let Stmt::Binding(name, _, _) = &stmts[i] {
            suffix_vars.remove(name);
        }
    }

    result
}

/// Like `compute_args_used_after` but propagates `rc_wrapped` and `borrowed_params` to child contexts.
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

/// Full version that propagates all context fields including `borrowed_params`.
pub fn compute_args_used_after_full(
    args: &[Expr],
    parent_used_after: &HashSet<String>,
    local_types: &HashMap<String, Type>,
    rc_wrapped: &HashSet<String>,
    borrowed_params: &HashSet<String>,
) -> Vec<EmitCtx> {
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
        let arg_vars = collect_vars(&args[i]);
        suffix_vars.extend(arg_vars);
    }

    result
}

/// Like `compute_args_used_after_full` but accepts `&[&Expr]` references.
#[allow(dead_code)]
pub fn compute_args_used_after_full_refs(
    args: &[&Expr],
    parent_used_after: &HashSet<String>,
    local_types: &HashMap<String, Type>,
    rc_wrapped: &HashSet<String>,
    borrowed_params: &HashSet<String>,
) -> Vec<EmitCtx> {
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
    fn test_collect_vars_fn_call() {
        let expr = Expr::FnCall(
            Box::new(Spanned::bare(Expr::Ident("f".to_string()))),
            vec![
                Spanned::bare(Expr::Ident("a".to_string())),
                Spanned::bare(Expr::Ident("b".to_string())),
            ],
        );
        let vars = collect_vars(&expr);
        assert_eq!(
            vars,
            HashSet::from(["f".to_string(), "a".to_string(), "b".to_string()])
        );
    }

    #[test]
    fn test_collect_vars_match_excludes_pattern_bindings() {
        let expr = Expr::Match {
            subject: Box::new(Spanned::bare(Expr::Ident("val".to_string()))),
            arms: vec![MatchArm {
                pattern: Pattern::Ident("x".to_string()),
                body: Box::new(Spanned::bare(Expr::BinOp(
                    BinOp::Add,
                    Box::new(Spanned::bare(Expr::Ident("x".to_string()))),
                    Box::new(Spanned::bare(Expr::Ident("y".to_string()))),
                ))),
            }],
        };
        let vars = collect_vars(&expr);
        // "val" from subject, "y" from arm body; "x" is pattern-bound so excluded
        assert!(vars.contains("val"));
        assert!(vars.contains("y"));
        assert!(!vars.contains("x"));
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
        // n is i64 (Copy) — skip clone even though it's used after
        assert!(ectx.skip_clone("n"));
        // s is String (not Copy) and used after — must clone
        assert!(!ectx.skip_clone("s"));
    }

    #[test]
    fn test_skip_clone_last_use() {
        let mut lt = HashMap::new();
        lt.insert("s".to_string(), Type::Str);
        let ectx = EmitCtx {
            used_after: HashSet::new(), // s NOT in used_after
            local_types: lt,
            rc_wrapped: HashSet::new(),
            borrowed_params: HashSet::new(),
        };
        // s is last use — skip clone
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
        // stmt[0]: used_after = vars_in(stmt[1]) ∪ parent = {a, b}
        assert!(ctxs[0].used_after.contains("a"));
        assert!(ctxs[0].used_after.contains("b"));
        // stmt[1]: used_after = parent = {}
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
        // arg[0]: used_after = vars_in(arg[1..]) = {y, x}
        assert!(ctxs[0].used_after.contains("x"));
        assert!(ctxs[0].used_after.contains("y"));
        // arg[1]: used_after = vars_in(arg[2]) = {x}
        assert!(ctxs[1].used_after.contains("x"));
        assert!(!ctxs[1].used_after.contains("y"));
        // arg[2]: used_after = parent = {}
        assert!(ctxs[2].used_after.is_empty());
    }
}
