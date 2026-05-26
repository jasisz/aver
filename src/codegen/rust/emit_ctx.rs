/// Rust-specific emission context for type and borrow policy.
///
/// Clone/move decisions now come from `last_use` annotations on `Expr::Resolved`
/// nodes (set by `ir::last_use`), NOT from name-based liveness sets. EmitCtx
/// provides only Rust-specific policy: Copy types, borrow semantics, Rc wrapping.
use crate::ast::Expr;
use crate::types::Type;
use std::collections::{HashMap, HashSet};

/// Emission context carrying Rust-specific type/borrow policy.
#[derive(Clone)]
pub struct EmitCtx {
    /// Local variable types (from fn params) for copy-type elision.
    pub local_types: HashMap<String, Type>,
    /// Parameters passed as `Rc<T>` (self-TCO) or `&T` (mutual-TCO pass-through).
    pub rc_wrapped: HashSet<String>,
    /// Parameters emitted as `&T` borrows (borrow-by-default for non-Copy, non-Str params).
    pub borrowed_params: HashSet<String>,
}

impl EmitCtx {
    /// Empty context — conservative (clones everything non-Copy).
    pub fn empty() -> Self {
        EmitCtx {
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
            local_types: param_types,
            rc_wrapped: HashSet::new(),
            borrowed_params,
        }
    }

    /// Build context for a function WITHOUT borrow-by-default (e.g. TCO, memo).
    pub fn for_fn_no_borrow(param_types: HashMap<String, Type>) -> Self {
        EmitCtx {
            local_types: param_types,
            rc_wrapped: HashSet::new(),
            borrowed_params: HashSet::new(),
        }
    }

    /// Is this variable a Copy type in Rust (i64, f64, bool, ())?
    pub fn is_copy(&self, name: &str) -> bool {
        self.local_types.get(name).is_some_and(is_copy_type)
    }

    /// Is this variable a pass-through parameter (Rc<T> in self-TCO, &T in mutual-TCO)?
    pub fn is_rc_wrapped(&self, name: &str) -> bool {
        self.rc_wrapped.contains(name)
    }

    /// Is this variable a borrowed parameter (`&T` from borrow-by-default)?
    pub fn is_borrowed_param(&self, name: &str) -> bool {
        self.borrowed_params.contains(name)
    }

    /// Create a context with specified Rc-wrapped parameters (TCO pass-through).
    pub fn with_rc_wrapped(&self, rc: HashSet<String>) -> Self {
        EmitCtx {
            local_types: self.local_types.clone(),
            rc_wrapped: rc,
            borrowed_params: self.borrowed_params.clone(),
        }
    }
}

// ── Expression-level move/clone decisions ───────────────────────────────

/// Can this expression be moved (not cloned)?
/// Checks `last_use` on Resolved nodes; Ident without local_types entry
/// is treated as a global (always moveable).
pub fn expr_can_move(expr: &Expr) -> bool {
    match expr {
        Expr::Resolved { last_use, .. } => last_use.0,
        Expr::Ident(_) => true, // globals/namespaces never need clone
        _ => false,
    }
}

/// Should `.clone()` be skipped for this expression?
/// True for: Copy types, last-use locals, globals/namespaces.
/// False for: rc_wrapped, borrowed_params (need special clone paths).
///
/// For `Expr::Ident`: in Rust codegen, Ident is used for both globals AND locals
/// (resolver doesn't run). Check ectx to distinguish:
/// - If name is in local_types → it's a local/param, apply rc_wrapped/borrowed checks
/// - If name is NOT in local_types → it's a global/namespace, skip clone
pub fn expr_skip_clone(expr: &Expr, ectx: &EmitCtx) -> bool {
    match expr {
        Expr::Resolved { name, last_use, .. } => {
            if ectx.rc_wrapped.contains(name.as_str()) {
                return false;
            }
            if ectx.borrowed_params.contains(name.as_str()) {
                return false;
            }
            last_use.0 || ectx.is_copy(name)
        }
        Expr::Ident(name) => {
            // If not a known local, treat as global/namespace — skip clone
            if !ectx.local_types.contains_key(name.as_str()) {
                return true;
            }
            // Known local: check special categories
            if ectx.rc_wrapped.contains(name.as_str()) {
                return false;
            }
            if ectx.borrowed_params.contains(name.as_str()) {
                return false;
            }
            // Without last_use info, only skip for Copy types
            ectx.is_copy(name)
        }
        _ => false,
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
            | Type::Named { .. }
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
        assert!(!is_copy_type(&Type::named("Foo")));
    }
}
