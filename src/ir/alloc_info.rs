//! Per-fn heap-allocation analysis.
//!
//! Detects which user functions never allocate on the heap when executed
//! under a backend-specific [`AllocPolicy`]. Backends use the resulting
//! flag to skip GC-related framing (boundary heap_ptr save / rt_truncate
//! call on the WASM side, GC pressure tracking on the VM side) for
//! functions that are guaranteed not to produce garbage.
//!
//! "Allocates" is policy-driven: a `Result.Ok(Int)` value, for example,
//! allocates a heap wrapper under the WASM ABI but stays inline in the
//! VM's NaN-boxed representation. Each backend supplies its own
//! [`AllocPolicy`]; the shared analysis below walks expression trees and
//! reaches a fixpoint over the call graph against that policy.
//!
//! A function is marked allocating if it
//! - has a non-empty `effects` declaration (effects routinely heap-marshal
//!   their arguments — conservative default),
//! - directly allocates via a literal shape (`List`, `Tuple`, `MapLiteral`,
//!   `RecordCreate`, `RecordUpdate`, `InterpolatedStr`, `IndependentProduct`),
//! - calls a built-in or constructor whose policy says it allocates, or
//! - calls a user fn already classified as allocating.
//!
//! Pure-no-alloc fns end up with `false`.

use std::collections::HashMap;

use crate::ast::{Expr, FnBody, FnDef, Stmt, StrPart};

use super::calls::{expr_to_dotted_name, is_builtin_namespace};

/// Backend-specific allocation policy.
///
/// Different backends treat builtins and ADT constructors differently
/// (NaN-boxing vs heap wrappers vs Rust enum variants), so the per-fn
/// classification is parameterised by a policy object.
pub trait AllocPolicy {
    /// Whether a call to a builtin (e.g. `List.prepend`, `Int.toString`)
    /// allocates a heap object on this backend.
    fn builtin_allocates(&self, name: &str) -> bool;

    /// Whether a constructor application (e.g. `Result.Ok(x)`,
    /// `Shape.Circle(r)`) allocates. `has_payload` is true when the
    /// constructor was applied to at least one argument; nullary
    /// constructors (`Option.None`) never allocate.
    fn constructor_allocates(&self, name: &str, has_payload: bool) -> bool;
}

/// Walks an expression. Returns true if any sub-expression allocates
/// directly (via shape) or transitively (via a call to a fn already
/// classified as allocating in `user_allocates`).
fn expr_allocates<P: AllocPolicy>(
    expr: &Expr,
    user_allocates: &HashMap<String, bool>,
    policy: &P,
) -> bool {
    match expr {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => false,
        Expr::Constructor(_, None) => false,

        // Direct-allocation shapes — the syntactic shape itself constructs
        // a heap object regardless of children.
        Expr::List(_)
        | Expr::Tuple(_)
        | Expr::MapLiteral(_)
        | Expr::RecordCreate { .. }
        | Expr::RecordUpdate { .. }
        | Expr::IndependentProduct(_, _) => true,
        Expr::InterpolatedStr(parts) => {
            // An interpolated string with no `Parsed` parts is a static
            // string literal — same as `Literal(Str)`, no allocation.
            // Anything else builds a fresh string at runtime.
            parts.iter().any(|p| matches!(p, StrPart::Parsed(_)))
                || expr_children_allocate(expr, user_allocates, policy)
        }
        Expr::Constructor(name, Some(payload)) => {
            policy.constructor_allocates(name, true)
                || expr_allocates(&payload.node, user_allocates, policy)
        }

        // Calls — dispatch on builtin vs user.
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_to_dotted_name(&callee.node) {
                let ns = name.split('.').next().unwrap_or("");
                if is_builtin_namespace(ns) {
                    if policy.builtin_allocates(&name) {
                        return true;
                    }
                } else if let Some(&true) = user_allocates.get(&name) {
                    return true;
                }
            }
            args.iter()
                .any(|a| expr_allocates(&a.node, user_allocates, policy))
        }
        Expr::TailCall(data) => {
            if let Some(&true) = user_allocates.get(&data.target) {
                return true;
            }
            data.args
                .iter()
                .any(|a| expr_allocates(&a.node, user_allocates, policy))
        }

        // Recurse-only nodes.
        Expr::Attr(base, _) | Expr::ErrorProp(base) => {
            expr_allocates(&base.node, user_allocates, policy)
        }
        Expr::BinOp(_, l, r) => {
            expr_allocates(&l.node, user_allocates, policy)
                || expr_allocates(&r.node, user_allocates, policy)
        }
        Expr::Match { subject, arms } => {
            expr_allocates(&subject.node, user_allocates, policy)
                || arms.iter().any(|a| {
                    expr_allocates(&a.body.node, user_allocates, policy)
                })
        }
    }
}

/// Helper for `InterpolatedStr` recursion into parsed parts.
fn expr_children_allocate<P: AllocPolicy>(
    expr: &Expr,
    user_allocates: &HashMap<String, bool>,
    policy: &P,
) -> bool {
    if let Expr::InterpolatedStr(parts) = expr {
        return parts.iter().any(|p| match p {
            StrPart::Literal(_) => false,
            StrPart::Parsed(e) => expr_allocates(&e.node, user_allocates, policy),
        });
    }
    false
}

/// Walks a function body.
fn body_allocates<P: AllocPolicy>(
    body: &FnBody,
    user_allocates: &HashMap<String, bool>,
    policy: &P,
) -> bool {
    body.stmts().iter().any(|s| match s {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => {
            expr_allocates(&e.node, user_allocates, policy)
        }
    })
}

/// Compute per-fn allocation status for every fn in `fns` under `policy`.
///
/// Iterates to fixpoint: a fn is marked allocating if it has effects, if
/// its body contains a direct allocation, or if it calls (transitively,
/// via the existing `info` map) a user fn already classified as
/// allocating. Mutual recursion is handled by repeating the pass until
/// nothing changes.
///
/// Once a fn flips to `true` it never reverts, so the loop is monotone
/// and converges in at most `fns.len()` iterations.
pub fn compute_alloc_info<P: AllocPolicy>(
    fns: &[&FnDef],
    policy: &P,
) -> HashMap<String, bool> {
    let mut info: HashMap<String, bool> = fns
        .iter()
        .map(|fd| {
            // Effects declaration → conservative "allocates".
            (fd.name.clone(), !fd.effects.is_empty())
        })
        .collect();

    loop {
        let mut changed = false;
        for fd in fns {
            if *info.get(&fd.name).unwrap_or(&false) {
                continue;
            }
            if body_allocates(&fd.body, &info, policy) {
                info.insert(fd.name.clone(), true);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    info
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, FnDef, Literal, Spanned};
    use std::sync::Arc;

    /// Test policy: every builtin in the `Map.*` namespace allocates,
    /// `Int.toString` allocates, everything else doesn't. No constructors
    /// allocate.
    struct TestPolicy;

    impl AllocPolicy for TestPolicy {
        fn builtin_allocates(&self, name: &str) -> bool {
            name.starts_with("Map.") || name == "Int.toString"
        }
        fn constructor_allocates(&self, _name: &str, _has_payload: bool) -> bool {
            false
        }
    }

    fn sp<T>(value: T) -> Spanned<T> {
        Spanned {
            node: value,
            line: 1,
        }
    }

    fn lit_int(n: i64) -> Spanned<Expr> {
        sp(Expr::Literal(Literal::Int(n)))
    }

    fn fn_def_pure(name: &str, body: Expr) -> FnDef {
        FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![],
            return_type: "Int".into(),
            effects: vec![],
            desc: None,
            body: Arc::new(FnBody::from_expr(sp(body))),
            resolution: None,
        }
    }

    #[test]
    fn pure_arithmetic_does_not_allocate() {
        let fd = fn_def_pure(
            "addOne",
            Expr::BinOp(BinOp::Add, Box::new(lit_int(1)), Box::new(lit_int(2))),
        );
        let info = compute_alloc_info(&[&fd], &TestPolicy);
        assert_eq!(info.get("addOne"), Some(&false));
    }

    #[test]
    fn list_literal_allocates() {
        let fd = fn_def_pure("makeList", Expr::List(vec![lit_int(1), lit_int(2)]));
        let info = compute_alloc_info(&[&fd], &TestPolicy);
        assert_eq!(info.get("makeList"), Some(&true));
    }

    #[test]
    fn allocating_builtin_call_allocates() {
        // Int.toString(42)
        let call = Expr::FnCall(
            Box::new(sp(Expr::Attr(
                Box::new(sp(Expr::Ident("Int".into()))),
                "toString".into(),
            ))),
            vec![lit_int(42)],
        );
        let fd = fn_def_pure("stringify", call);
        let info = compute_alloc_info(&[&fd], &TestPolicy);
        assert_eq!(info.get("stringify"), Some(&true));
    }

    #[test]
    fn pure_builtin_call_does_not_allocate() {
        // Int.abs(-5) — Int.abs is not in the test policy's alloc list.
        let call = Expr::FnCall(
            Box::new(sp(Expr::Attr(
                Box::new(sp(Expr::Ident("Int".into()))),
                "abs".into(),
            ))),
            vec![lit_int(-5)],
        );
        let fd = fn_def_pure("absVal", call);
        let info = compute_alloc_info(&[&fd], &TestPolicy);
        assert_eq!(info.get("absVal"), Some(&false));
    }

    #[test]
    fn effects_force_allocating() {
        let mut fd = fn_def_pure("logIt", Expr::Literal(Literal::Int(0)));
        fd.effects = vec![sp("Console.print".into())];
        let info = compute_alloc_info(&[&fd], &TestPolicy);
        assert_eq!(info.get("logIt"), Some(&true));
    }

    #[test]
    fn transitive_user_call_propagates() {
        // makeListInner allocates (list literal).
        // wrapperFn calls makeListInner — also allocates by transitivity.
        let inner = fn_def_pure("makeListInner", Expr::List(vec![lit_int(1)]));

        let call = Expr::FnCall(
            Box::new(sp(Expr::Ident("makeListInner".into()))),
            vec![],
        );
        let wrapper = fn_def_pure("wrapperFn", call);

        let info = compute_alloc_info(&[&inner, &wrapper], &TestPolicy);
        assert_eq!(info.get("makeListInner"), Some(&true));
        assert_eq!(info.get("wrapperFn"), Some(&true));
    }

    #[test]
    fn mutual_recursion_pure_stays_pure() {
        // f calls g, g calls f, both pure (no allocation anywhere).
        let f = fn_def_pure(
            "f",
            Expr::FnCall(Box::new(sp(Expr::Ident("g".into()))), vec![lit_int(1)]),
        );
        let g = fn_def_pure(
            "g",
            Expr::FnCall(Box::new(sp(Expr::Ident("f".into()))), vec![lit_int(2)]),
        );
        let info = compute_alloc_info(&[&f, &g], &TestPolicy);
        assert_eq!(info.get("f"), Some(&false));
        assert_eq!(info.get("g"), Some(&false));
    }

    #[test]
    fn mutual_recursion_one_allocates_taints_the_group() {
        // f calls g, g allocates. Both end up allocating.
        let f = fn_def_pure(
            "f",
            Expr::FnCall(Box::new(sp(Expr::Ident("g".into()))), vec![lit_int(1)]),
        );
        let g = fn_def_pure("g", Expr::List(vec![lit_int(0)]));
        let info = compute_alloc_info(&[&f, &g], &TestPolicy);
        assert_eq!(info.get("f"), Some(&true));
        assert_eq!(info.get("g"), Some(&true));
    }
}
