//! Alias-slot annotation pass.
//!
//! Identifies, per fn, every local slot whose value might share an
//! arena entry with another live binding. Backends with a `mem::take`-
//! style fast path on `Vector.set` / `Map.set` (the VM's
//! `CALL_BUILTIN_OWNED` mask + the fused `VECTOR_SET_OR_KEEP` opcode)
//! must NOT take the fast path on a flagged slot, because the entry
//! they'd `mem::take` from is reachable via another binding and the
//! mutation would be observed there too. Wasm-gc may use the same
//! flag to skip clone-on-write when a slot is provably non-aliased.
//!
//! ## Sources of arena sharing in Aver
//!
//! 1. **Vector / Map params.** A caller may keep its own ref to the
//!    same arena entry, so every `Vector<T>` / `Map<K, V>` parameter
//!    starts out aliased.
//! 2. **`Vector.get` / `Map.get` results.** The returned handle shares
//!    the source vector's arena entry at that index — mutating it
//!    rewrites the source. Pattern variants behind common shapes
//!    (`Option.withDefault(Vector.get(_, i), …)`, `match` arms over
//!    `Vector.get`) all produce the same alias.
//! 3. **`Vector.new(n, compound)`.** Every cell of the produced outer
//!    vec holds the same `compound` ref, so two rows pulled out of
//!    the same outer alias each other transitively.
//! 4. **Transitive copy `b = a` where `a` is already aliased.**
//!
//! ## Conservative
//!
//! False positives only cost the VM's owned fast path for the flagged
//! slot — the slow path (clone backing items, push fresh arena
//! entry) is always sound. False negatives are unsound: they put
//! shared bindings on the fast path and silently mutate the user's
//! data. So we err on the side of flagging — every fn-call result
//! whose tree contains a `Vector.get` / `Map.get` / `Vector.new(_,
//! compound)` anywhere is treated as a possible alias source.
//!
//! Runs after `last_use`. Stamps `FnResolution.aliased_slots` in place.

use std::sync::Arc;

use crate::ast::{Expr, FnBody, FnDef, Stmt, StrPart, TopLevel};

pub fn annotate_program_alias_slots(items: &mut [TopLevel]) {
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            annotate_fn(fd);
        }
    }
}

fn annotate_fn(fd: &mut FnDef) {
    let Some(res) = fd.resolution.clone() else {
        return;
    };
    let local_count = res.local_count as usize;
    let mut aliased = vec![false; local_count];

    // (1) Vector / Map params get flagged unconditionally.
    for (i, (_, ty)) in fd.params.iter().enumerate() {
        if param_type_is_alias_prone(ty)
            && let Some(slot) = aliased.get_mut(i)
        {
            *slot = true;
        }
    }

    // (2)/(3)/(4) walk body bindings forward. Two passes propagate
    // transitive aliases (`b = a` where `a` is rebind earlier in the
    // same statement list).
    let body = fd.body.clone();
    let FnBody::Block(stmts) = body.as_ref();
    for _ in 0..2 {
        for stmt in stmts {
            if let Stmt::Binding(name, _, expr) = stmt {
                let Some(&slot) = res.local_slots.get(name) else {
                    continue;
                };
                if expr_is_alias_source(&expr.node, &aliased)
                    && let Some(s) = aliased.get_mut(slot as usize)
                {
                    *s = true;
                }
            }
        }
    }

    // Re-stamp the resolution. `Arc` swap keeps the rest of the
    // resolution shape unchanged.
    let new_res = crate::ast::FnResolution {
        local_count: res.local_count,
        local_slots: res.local_slots.clone(),
        local_slot_types: res.local_slot_types.clone(),
        aliased_slots: Arc::new(aliased),
    };
    fd.resolution = Some(new_res);
}

fn param_type_is_alias_prone(ty: &str) -> bool {
    let trimmed = ty.trim();
    trimmed.starts_with("Vector<") || trimmed.starts_with("Map<")
}

fn expr_is_alias_source(expr: &Expr, aliased: &[bool]) -> bool {
    if let Expr::Resolved { slot, .. } = expr
        && aliased.get(*slot as usize).copied().unwrap_or(false)
    {
        return true;
    }
    contains_alias_source_call(expr)
}

fn contains_alias_source_call(expr: &Expr) -> bool {
    match expr {
        Expr::FnCall(callee, args) => {
            if let Expr::Attr(parent, member) = &callee.node
                && let Expr::Ident(p) = &parent.node
            {
                if (p == "Vector" || p == "Map") && member == "get" {
                    return true;
                }
                if p == "Vector"
                    && member == "new"
                    && args.len() == 2
                    && let Some(t) = args[1].ty()
                    && type_is_compound(&t.display())
                {
                    return true;
                }
            }
            if contains_alias_source_call(&callee.node) {
                return true;
            }
            args.iter().any(|a| contains_alias_source_call(&a.node))
        }
        Expr::Attr(inner, _) => contains_alias_source_call(&inner.node),
        Expr::BinOp(_, lhs, rhs) => {
            contains_alias_source_call(&lhs.node) || contains_alias_source_call(&rhs.node)
        }
        Expr::Match { subject, arms } => {
            contains_alias_source_call(&subject.node)
                || arms
                    .iter()
                    .any(|a| contains_alias_source_call(&a.body.node))
        }
        Expr::Constructor(_, payload) => payload
            .as_ref()
            .is_some_and(|p| contains_alias_source_call(&p.node)),
        Expr::ErrorProp(inner) => contains_alias_source_call(&inner.node),
        Expr::Tuple(items) | Expr::List(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(|i| contains_alias_source_call(&i.node))
        }
        Expr::MapLiteral(pairs) => pairs.iter().any(|(k, v)| {
            contains_alias_source_call(&k.node) || contains_alias_source_call(&v.node)
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, e)| contains_alias_source_call(&e.node)),
        Expr::RecordUpdate { base, updates, .. } => {
            contains_alias_source_call(&base.node)
                || updates
                    .iter()
                    .any(|(_, e)| contains_alias_source_call(&e.node))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(e) => contains_alias_source_call(&e.node),
            StrPart::Literal(_) => false,
        }),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::TailCall(_) => false,
    }
}

fn type_is_compound(ty: &str) -> bool {
    let trimmed = ty.trim();
    trimmed.starts_with("Vector<")
        || trimmed.starts_with("Map<")
        || trimmed.starts_with("List<")
        || trimmed.starts_with("Tuple<")
        || trimmed.starts_with("Result<")
        || trimmed.starts_with("Option<")
        || (trimmed
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_uppercase())
            && !matches!(trimmed, "Int" | "Float" | "Bool" | "String" | "Unit"))
}
