//! What `lazy f(…)` may defer (see `docs/language.md`, "Deferred calls").
//!
//! The Rust backend keeps the call in a node of its own until its result is
//! first looked at, and every other backend makes it at once, so a deferred
//! call has to be one whose result can wait in such a node and whose making
//! cannot be seen: a call of a function declared with `fn`, with no effects,
//! returning a recursive sum (whose values the Rust backend keeps as one
//! pointer to a node).

use std::collections::HashSet;

use super::{Type, TypeChecker};
use crate::ast::{Expr, Spanned};

impl TypeChecker {
    /// Reports why `expr`, marked `lazy`, cannot be deferred, if it cannot.
    pub(super) fn check_lazy_call(&mut self, expr: &Spanned<Expr>) {
        let name = match &expr.node {
            Expr::FnCall(callee, _) => match &callee.node {
                Expr::Ident(name) => Some(name.clone()),
                other => Self::callee_key(other),
            },
            Expr::TailCall(call) => Some(call.target.clone()),
            _ => None,
        };
        let why = match name {
            None => Some(
                "`lazy` defers a call of a function named in the program; \
                 this call's function is computed"
                    .to_string(),
            ),
            Some(name) if self.locals.contains_key(&name) => Some(format!(
                "`lazy {name}(…)`: `{name}` is a function passed in as a value; \
                 `lazy` defers only a call of a function declared with `fn`"
            )),
            Some(name) => match self.find_fn_sig_resolved(&name) {
                // Unknown: the call itself reports it.
                None => None,
                Some((None, _)) => Some(format!(
                    "`lazy {name}(…)`: `{name}` is a constructor or a built-in, \
                     not a function declared with `fn`; call it without `lazy`"
                )),
                Some((Some(_), sig)) if !sig.effects.is_empty() => Some(format!(
                    "`lazy {name}(…)`: `{name}` has effects ({}); a deferred call is \
                     made when its result is first looked at, or never, so only a \
                     function without effects can be deferred",
                    sig.effects.join(", ")
                )),
                Some((Some(_), sig)) if !self.is_recursive_sum(&sig.ret) => Some(format!(
                    "`lazy {name}(…)`: `{name}` returns {}, and only a call returning a \
                     recursive sum type (one whose values hold values of the same type, \
                     like a tree) can be deferred; call it without `lazy`",
                    sig.ret.display()
                )),
                Some(_) => None,
            },
        };
        if let Some(why) = why {
            self.error_at_line(expr.line, why);
        }
    }

    /// Whether `ty` is a sum a value of which can hold a value of the same
    /// type with no `List`, `Vector` or `Map` in between: directly, or
    /// through records, other sums, `Option`, `Result` or tuples. The Rust
    /// backend keeps exactly these as one pointer to a node.
    fn is_recursive_sum(&self, ty: &Type) -> bool {
        let Type::Named { name, .. } = ty else {
            return false;
        };
        let target = self.canonical_type_name(name);
        let Some(variants) = self.variants_for(name).cloned() else {
            return false;
        };
        let mut seen = HashSet::from([target.clone()]);
        variants.iter().any(|variant| {
            self.named_variant_arg_types(name, variant)
                .iter()
                .any(|field| self.reaches_type(field, &target, &mut seen))
        })
    }

    fn reaches_type(&self, ty: &Type, target: &str, seen: &mut HashSet<String>) -> bool {
        match ty {
            Type::Option(inner) => self.reaches_type(inner, target, seen),
            Type::Result(ok, err) => {
                self.reaches_type(ok, target, seen) || self.reaches_type(err, target, seen)
            }
            Type::Tuple(items) => items
                .iter()
                .any(|item| self.reaches_type(item, target, seen)),
            Type::Named { name, .. } => {
                let canonical = self.canonical_type_name(name);
                if canonical == target {
                    return true;
                }
                if !seen.insert(canonical.clone()) {
                    return false;
                }
                let fields: Vec<Type> = match self.variants_for(name).cloned() {
                    Some(variants) => variants
                        .iter()
                        .flat_map(|variant| self.named_variant_arg_types(name, variant))
                        .collect(),
                    None => self
                        .record_field_types
                        .iter()
                        .filter(|(key, _)| key.type_name == canonical)
                        .map(|(_, field)| field.clone())
                        .collect(),
                };
                fields
                    .iter()
                    .any(|field| self.reaches_type(field, target, seen))
            }
            _ => false,
        }
    }
}
