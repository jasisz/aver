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
//! ## When is a collection slot owned-eligible (NOT flagged)?
//!
//! Sound-by-construction whitelist. A `Vector<T>` / `Map<K, V>` local stays
//! unflagged (eligible for the owned in-place fast path) ONLY when both hold:
//!
//! - **Fresh source (destination half, `rhs_is_fresh_collection`).** Its
//!   binding RHS is a *provably fresh* collection — a `MapLiteral`, an
//!   allocating builtin (`Vector.new` of a non-compound element, `Vector.set`,
//!   `Vector.fromList`, `Map.set`, `Map.remove`, `Map.fromList`), or a
//!   self-keep rebuild
//!   `withDefault(set(L,..), L)`. Any other collection RHS — field / element
//!   extraction (`rec.held`, a tuple item), a `Vector.get` / `Map.get` result,
//!   a rename `b = a`, a user-fn result — may alias an existing arena entry,
//!   so the destination is flagged.
//! - **No escape (source half, `flag_escaping_collection_locals`).** Its
//!   handle is not RETAINED by any other binding — not stored into an
//!   aggregate (record / tuple / list / map value), not passed as a builtin
//!   value-arg, not passed to a user fn that might return it, not renamed. The
//!   receiver (arg 0) of a `Vector` / `Map` builtin and the self-keep fallback
//!   are consuming moves, not escapes.
//!
//! Vector / Map PARAMS are always flagged here (a caller may hold the same
//! entry); the MIR `own_param` pass later clears that bit interprocedurally
//! when every call site passes a uniquely-owned argument.
//!
//! ## Conservative
//!
//! False positives only cost the owned fast path for the flagged slot — the
//! slow path (clone backing, fresh arena entry) is always sound. False
//! negatives are unsound: a shared binding on the fast path silently mutates
//! the user's data (issue #410). So the default is FLAGGED, and a slot clears
//! only on positive proof of fresh-AND-non-escaping.
//!
//! Runs after `last_use`. Stamps `FnResolution.aliased_slots` in place.

use std::sync::Arc;

use crate::ast::{Expr, FnBody, FnDef, Spanned, Stmt, StrPart, TopLevel, Type};

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

    // Body bindings, two forward passes (transitive aliases propagate: a
    // later `b = a` sees `a` flagged in an earlier pass). For each binding of
    // a Vector / Map local, ownership is decided SOUND-BY-CONSTRUCTION by two
    // complementary halves.
    let body = fd.body.clone();
    let FnBody::Block(stmts) = body.as_ref();
    for _ in 0..2 {
        for stmt in stmts {
            if let Stmt::Binding(name, _, expr) = stmt {
                let Some(&slot) = res.local_slots.get(name) else {
                    continue;
                };
                // Source half: flag every bare collection local whose handle
                // ESCAPES into this binding's value — a rename (`b = a`), a
                // match arm tail, an aggregate member (record / tuple / list /
                // map value), a builtin value-arg, or an arg to a user fn that
                // may return it. Without it, `a = {..}; b = a; Map.set(a, ..)`
                // would own-mutate `a` in place and silently rewrite `b`. The
                // receiver (arg 0) of a Vector/Map builtin and the self-keep
                // `withDefault(set(L,..), L)` rebind are consuming moves, NOT
                // escapes, so they stay eligible for the owned fast path.
                flag_escaping_collection_locals(&expr.node, &res.local_slot_types, &mut aliased);
                // Destination half: a collection-typed binding is owned-
                // eligible ONLY if its RHS is a PROVABLY-FRESH collection
                // (literal / allocating builtin / self-keep rebuild). Any other
                // collection RHS — field/element extraction, a `get`, a rename,
                // a user-fn result — yields a handle that may alias an existing
                // arena entry, so flag the destination. This whitelist replaces
                // the former enumerate-the-alias-sources blacklist, which was
                // unsound by omission (it missed aggregate-field extraction —
                // `x = rec.held; Map.set(x, ..)` clobbered `rec`'s field).
                if slot_is_collection(slot, &res.local_slot_types)
                    && !rhs_is_fresh_collection(expr)
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

/// A binding RHS that yields a PROVABLY-FRESH `Vector` / `Map` — a `MapLiteral`,
/// an allocating builtin (`Vector.new` of a non-compound element, `Vector.set`,
/// `Vector.fromList`, `Map.set`, `Map.remove`, `Map.fromList`), a self-keep rebuild
/// `withDefault(set(L,..), L)`, or a `withDefault` whose branches are each
/// fresh — produces a uniquely-owned arena entry, so the destination local is
/// safe for the owned in-place fast path. Every other collection-typed RHS may
/// alias an existing entry (field/element extraction, `get`, a rename, a
/// user-fn result), so the destination must be flagged. Conservative:
/// anything unrecognized is NOT fresh (flagging only costs the fast path).
fn rhs_is_fresh_collection(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::MapLiteral(_) => true,
        // Fresh only if EVERY arm is fresh — one aliasing arm taints the value.
        Expr::Match { arms, .. } => arms.iter().all(|a| rhs_is_fresh_collection(&a.body)),
        Expr::FnCall(callee, args) => {
            if is_option_with_default(&callee.node) && args.len() == 2 {
                return self_keep_slot(&args[0].node, &args[1].node).is_some()
                    || (rhs_is_fresh_collection(&args[0]) && rhs_is_fresh_collection(&args[1]));
            }
            is_fresh_collection_builtin(&callee.node, args)
        }
        _ => false,
    }
}

/// Vector / Map builtins that ALLOCATE a fresh outer collection. `Vector.new`
/// is fresh only when its element is non-compound — a compound element is
/// shared by every cell (the old rule 3 aliasing). `get` is excluded: it
/// returns an element that aliases the source.
///
/// `Map.fromList` is here on the same proof as `Vector.fromList`, which has
/// been in this list from the start. `from_list_nv` (`src/types/map.rs`) builds
/// its table from scratch and returns either the immediate `EMPTY_MAP` or an
/// index it has just pushed into the arena, so nothing else can hold the
/// result — exactly what `vec_from_list_nv` (`src/types/vector.rs`) does for
/// the vector spelling. Both say nothing whatever about the ARGUMENT: the list
/// handed in is retained by the result, and a bare collection local passed
/// there is still flagged by the escape half, which is the separate condition
/// that keeps this entry from widening what the pass promises.
fn is_fresh_collection_builtin(callee: &Expr, args: &[Spanned<Expr>]) -> bool {
    let Expr::Attr(parent, member) = callee else {
        return false;
    };
    let Expr::Ident(ns) = &parent.node else {
        return false;
    };
    match (ns.as_str(), member.as_str()) {
        ("Vector", "set")
        | ("Vector", "fromList")
        | ("Map", "set")
        | ("Map", "remove")
        | ("Map", "fromList") => true,
        ("Vector", "new") => args
            .get(1)
            .and_then(|a| a.ty())
            .is_some_and(|t| !type_is_compound(&t.display())),
        _ => false,
    }
}

fn slot_is_collection(slot: u16, slot_types: &[Type]) -> bool {
    slot_types
        .get(slot as usize)
        .is_some_and(|t| matches!(t, Type::Vector(_) | Type::Map(_, _)))
}

/// `Vector` / `Map` builtin call (`Vector.set`, `Map.get`, …) whose arg 0 is
/// the collection receiver — consumed / read in place, never retained into the
/// result (a `get` result aliases an *element*, handled by rule 2 on the
/// destination, not by the receiver here).
fn is_vector_map_builtin(callee: &Expr) -> bool {
    matches!(callee, Expr::Attr(parent, _)
        if matches!(&parent.node, Expr::Ident(p) if p == "Vector" || p == "Map"))
}

/// `Option.withDefault(Vector.set(L, ..) | Map.set(L, ..), L)` — the self-keep
/// rebuild fusion. The result IS `L` (mutated, or unchanged on the fallback),
/// so this is a consuming rebind of `L`, not an escape. Returns the kept slot.
fn self_keep_slot(op1: &Expr, op2: &Expr) -> Option<u16> {
    let Expr::Resolved { slot: kept, .. } = op2 else {
        return None;
    };
    let Expr::FnCall(callee, set_args) = op1 else {
        return None;
    };
    let Expr::Attr(parent, member) = &callee.node else {
        return None;
    };
    let Expr::Ident(ns) = &parent.node else {
        return None;
    };
    if !((ns == "Vector" || ns == "Map") && member == "set") {
        return None;
    }
    match set_args.first().map(|a| &a.node) {
        Some(Expr::Resolved { slot, .. }) if slot == kept => Some(*kept),
        _ => None,
    }
}

fn is_option_with_default(callee: &Expr) -> bool {
    matches!(callee, Expr::Attr(parent, member)
        if member == "withDefault"
            && matches!(&parent.node, Expr::Ident(p) if p == "Option"))
}

/// Flag (as aliased) every bare collection local whose handle is RETAINED by
/// `expr` (the value of a binding) — see the call site for the rationale and
/// the non-escape exceptions (builtin receiver arg 0, self-keep rebuild).
fn flag_escaping_collection_locals(expr: &Expr, slot_types: &[Type], aliased: &mut [bool]) {
    match expr {
        // A bare collection local in an escaping position: its handle flows
        // into the binding's value, so it can no longer be owned-mutated.
        Expr::Resolved { slot, .. } => {
            if slot_is_collection(*slot, slot_types)
                && let Some(s) = aliased.get_mut(*slot as usize)
            {
                *s = true;
            }
        }
        Expr::FnCall(callee, args) => {
            // Self-keep rebuild `withDefault(set(L,..), L)`: L is consumed, not
            // aliased. Recurse only into the set's value-args (which may carry
            // a *different* escaping collection), skipping the receiver and the
            // kept fallback.
            if is_option_with_default(&callee.node)
                && args.len() == 2
                && self_keep_slot(&args[0].node, &args[1].node).is_some()
            {
                if let Expr::FnCall(_, set_args) = &args[0].node {
                    for a in set_args.iter().skip(1) {
                        flag_escaping_collection_locals(&a.node, slot_types, aliased);
                    }
                }
                return;
            }
            let recv_skip = is_vector_map_builtin(&callee.node);
            for (i, a) in args.iter().enumerate() {
                // Receiver (arg 0 of a Vector/Map builtin): a bare local here is
                // consumed in place, not retained — skip it. A compound arg 0
                // still recurses (its own nested receivers self-handle).
                if recv_skip && i == 0 && matches!(&a.node, Expr::Resolved { .. }) {
                    continue;
                }
                flag_escaping_collection_locals(&a.node, slot_types, aliased);
            }
            flag_escaping_collection_locals(&callee.node, slot_types, aliased);
        }
        Expr::Attr(inner, _) | Expr::Neg(inner) | Expr::ErrorProp(inner) => {
            flag_escaping_collection_locals(&inner.node, slot_types, aliased);
        }
        Expr::BinOp(_, lhs, rhs) => {
            flag_escaping_collection_locals(&lhs.node, slot_types, aliased);
            flag_escaping_collection_locals(&rhs.node, slot_types, aliased);
        }
        // The scrutinee is read/consumed; each arm tail becomes the value.
        Expr::Match { subject: _, arms } => {
            for a in arms {
                flag_escaping_collection_locals(&a.body.node, slot_types, aliased);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload {
                flag_escaping_collection_locals(&p.node, slot_types, aliased);
            }
        }
        Expr::Tuple(items) | Expr::List(items) | Expr::IndependentProduct(items, _) => {
            for i in items {
                flag_escaping_collection_locals(&i.node, slot_types, aliased);
            }
        }
        Expr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                flag_escaping_collection_locals(&k.node, slot_types, aliased);
                flag_escaping_collection_locals(&v.node, slot_types, aliased);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                flag_escaping_collection_locals(&e.node, slot_types, aliased);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            flag_escaping_collection_locals(&base.node, slot_types, aliased);
            for (_, e) in updates {
                flag_escaping_collection_locals(&e.node, slot_types, aliased);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let StrPart::Parsed(e) = p {
                    flag_escaping_collection_locals(&e.node, slot_types, aliased);
                }
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::TailCall(_) => {}
    }
}

/// Shared with the wasm-gc MIR emitter's `mir_expr_is_fresh_collection`,
/// which mirrors [`rhs_is_fresh_collection`]'s `Vector.new` rule for
/// receiver positions — keep the two freshness tests deciding alike.
pub(crate) fn type_is_compound(ty: &str) -> bool {
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

#[cfg(test)]
mod tests {
    use crate::ast::TopLevel;
    use crate::ir::pipeline::{self, PipelineConfig, TypecheckMode};
    use crate::source::parse_source;

    /// Read the alias bit of the local named `local` in fn `f`, after the whole
    /// pipeline.
    ///
    /// The full pipeline is what the assertion needs, not a hand-picked subset:
    /// the destination half of this pass is guarded by `slot_is_collection`,
    /// which reads `FnResolution.local_slot_types`, and those are filled from
    /// typecheck stamps. Without a typecheck every binding slot is
    /// `Type::Invalid`, the destination half never fires at all, and a test
    /// built on a subset pipeline cannot see this list one way or the other.
    fn local_is_flagged(source: &str, f: &str, local: &str) -> bool {
        let mut items = parse_source(source).unwrap_or_else(|e| panic!("parse: {e}"));
        let result = pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );
        let tc = result.typecheck.as_ref().expect("typecheck requested");
        assert!(tc.errors.is_empty(), "typecheck failed: {:?}", tc.errors);
        let fd = items
            .iter()
            .find_map(|i| match i {
                TopLevel::FnDef(fd) if fd.name == f => Some(fd),
                _ => None,
            })
            .unwrap_or_else(|| panic!("fn {f} not found"));
        let res = fd.resolution.as_ref().expect("resolution stamped");
        let slot = *res
            .local_slots
            .get(local)
            .unwrap_or_else(|| panic!("local {local} not found in {f}"));
        res.aliased_slots
            .get(slot as usize)
            .copied()
            .unwrap_or(false)
    }

    const FROM_LIST_BINDING: &str = r#"
fn pairs(n: Int, acc: List<Tuple<String, String>>) -> List<Tuple<String, String>>
    match n > 0
        true -> pairs(n - 1, List.prepend(("k", "v"), acc))
        false -> acc

fn main() -> Int
    m = Map.fromList(pairs(3, []))
    v = Vector.fromList([1, 2, 3])
    Map.len(Map.set(m, "z", "9")) + Vector.len(v)
"#;

    /// `Map.fromList` builds its result from scratch, so a binding of it is a
    /// handle nothing else holds and the next `Map.set` may consume it.
    ///
    /// Freshness is decided in two places that have to agree.
    /// `own_param::uniquely_owned` reads a call ARGUMENT, so it sees
    /// `Map.fromList(..)` written inline at a call site. This pass decides a
    /// BINDING, which is what a *named* result goes through, and the two lists
    /// were one name apart: `Vector.fromList` was here and `Map.fromList` was
    /// not, so `m = Map.fromList(..)` stayed flagged and the following
    /// `Map.set` duplicated the whole map to preserve something unreachable.
    ///
    /// The proof is the builtin's own: `from_list_nv` (`src/types/map.rs`)
    /// returns either the immediate `EMPTY_MAP` or an index it just pushed into
    /// the arena, exactly as `vec_from_list_nv` (`src/types/vector.rs`) does for
    /// the vector spelling already in the list. Neither says anything about the
    /// ARGUMENT: the list handed to `fromList` is retained by the result, which
    /// is why the escape half still flags a bare collection local passed there.
    #[test]
    fn a_named_from_list_result_is_a_fresh_binding_for_both_collections() {
        assert!(
            !local_is_flagged(FROM_LIST_BINDING, "main", "m"),
            "a binding of `Map.fromList(..)` was flagged as possibly-shared, so \
             the `Map.set` after it has to copy the whole map"
        );
        assert!(
            !local_is_flagged(FROM_LIST_BINDING, "main", "v"),
            "the `Vector.fromList` precedent moved — this pair is here to keep \
             the two spellings deciding the same way"
        );
    }

    /// The other half of the same pass, unchanged by the entry above: a
    /// `fromList` result that ESCAPES is still flagged. `held` is stored into a
    /// tuple that outlives it, so consuming it in place would rewrite what the
    /// tuple holds. Freshness of the source and non-escape of the handle are
    /// separate conditions and both still have to hold.
    #[test]
    fn a_from_list_result_that_escapes_is_still_flagged() {
        let src = r#"
fn pairs(n: Int, acc: List<Tuple<String, String>>) -> List<Tuple<String, String>>
    match n > 0
        true -> pairs(n - 1, List.prepend(("k", "v"), acc))
        false -> acc

fn main() -> Int
    held = Map.fromList(pairs(3, []))
    kept = (held, 1)
    match kept
        (stored, one) -> Map.len(Map.set(held, "z", "9")) + Map.len(stored) + one
"#;
        assert!(
            local_is_flagged(src, "main", "held"),
            "a fresh map whose handle was stored into a live tuple must stay \
             flagged — freshness does not survive an escape"
        );
    }
}
