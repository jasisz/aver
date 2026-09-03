/// Rust-specific emission context for type and borrow policy.
///
/// Clone/move decisions now come from `last_use` annotations on
/// `ResolvedExpr::Resolved` nodes (set by `ir::last_use` and lifted by the
/// resolver pass), NOT from name-based liveness sets. EmitCtx provides
/// only Rust-specific policy: Copy types, borrow semantics, Rc wrapping.
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
    /// Owning module prefix for the function whose body this context
    /// is emitting (`Some("Domain.Eval.Core")` inside a dep module's
    /// fn body, `None` for entry-scope fns). Used by free-standing top-level /
    /// verify rewrite boundaries; ordinary function bodies already carry
    /// resolved identity.
    pub current_module_scope: Option<String>,
}

impl EmitCtx {
    /// Empty context — conservative (clones everything non-Copy).
    pub fn empty() -> Self {
        EmitCtx {
            local_types: HashMap::new(),
            rc_wrapped: HashSet::new(),
            borrowed_params: HashSet::new(),
            current_module_scope: None,
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
            current_module_scope: None,
        }
    }

    /// Build context for a function WITHOUT borrow-by-default (e.g. TCO).
    pub fn for_fn_no_borrow(param_types: HashMap<String, Type>) -> Self {
        EmitCtx {
            local_types: param_types,
            rc_wrapped: HashSet::new(),
            borrowed_params: HashSet::new(),
            current_module_scope: None,
        }
    }

    /// Stamp the owning module prefix onto a context — chains
    /// fluently after `for_fn` / `for_fn_no_borrow`. `None` for
    /// entry-scope fns keeps the field default.
    pub fn with_scope(mut self, scope: Option<&str>) -> Self {
        self.current_module_scope = scope.map(String::from);
        self
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
}

// ── Rust-specific policy ──────────────────────────────────────────────

/// Is a Type Copy in Rust? (Float, Bool, Unit)
///
/// `Int` is NOT Copy: it now lowers to `aver_rt::AverInt`, which is
/// `Clone`-only (the `Big` variant boxes a `BigInt`). Dropping `Int` from
/// this set is what flips every owning-position non-last-use Int read to a
/// `.clone()` — cheap for the common `Small` case (an `i64` copy + tag).
pub fn is_copy_type(ty: &Type) -> bool {
    matches!(ty, Type::Float | Type::Bool | Type::Unit)
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
        // Int lowers to the non-Copy `aver_rt::AverInt`.
        assert!(!is_copy_type(&Type::Int));
        assert!(is_copy_type(&Type::Float));
        assert!(is_copy_type(&Type::Bool));
        assert!(is_copy_type(&Type::Unit));
        assert!(!is_copy_type(&Type::Str));
        assert!(!is_copy_type(&Type::List(Box::new(Type::Int))));
        assert!(!is_copy_type(&Type::named("Foo")));
    }
}
