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
//!   value-arg, not passed to a user fn that might return it, not renamed, and
//!   not put inside the subject of a match one of whose arms BINDS. The
//!   receiver (arg 0) of a `Vector` / `Map` builtin and the self-keep fallback
//!   are consuming moves, not escapes.
//!
//! Freshness is a claim about an AGGREGATE, never about its contents, so the
//! two halves are asymmetric on purpose: the escape half runs over every
//! value-producing position whether or not the value is fresh, and only the
//! DESTINATION half is gated on freshness.
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

    // Match-pattern binders. A binder slot is a container read spelled as a
    // pattern: `Option.Some(v)`, `[h, ..t]`, `(a, b)`, or the bare-ident
    // rename `v ->` all hand the arm body a handle INTO the subject's value.
    // The two statement passes above never see those slots — they iterate
    // `Stmt::Binding` only — so a binder used to read as "never shared" and
    // the in-place fast path wrote through the stored entry (issue #953
    // follow-up). Symmetric with the statement rule: a binder is judged by
    // its subject, exactly as a binding is judged by its RHS — and by BOTH
    // halves of it, the subject's freshness and what the subject retains.
    for stmt in stmts {
        let expr = match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr,
        };
        flag_match_binder_slots(expr, &res.local_slot_types, &mut aliased);
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
        // The scrutinee is read/consumed and each arm tail becomes the value,
        // so nothing of the subject escapes through the match's own VALUE —
        // which is all this half is asked about here. What the subject
        // RETAINS is the separate question a BINDER asks, because a binder is
        // the only thing that hands the arm body a handle into the subject;
        // [`flag_match_binder_slots`] runs this same walk over the subject
        // exactly when an arm binds. Skipping the subject here and asking
        // there is what keeps `keeper = match Map.get(m, k) { … }` from
        // costing `m` anything the read never took.
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

/// Flag every binder slot a match pattern introduces, wherever the match sits
/// in `expr`, unless the SUBJECT is provably fresh AND retains nothing.
///
/// A pattern binder is an extraction: `Option.Some(v)` hands the arm body the
/// payload, `[h, ..t]` the head, `(a, b)` the components, and the bare-ident
/// arm `v ->` the whole subject — every one of them a handle that may share an
/// arena entry with whatever the subject was read out of. The same freshness
/// judgment the statement pass applies to a binding RHS
/// ([`rhs_is_fresh_collection`]) applies to the subject here; when it cannot
/// prove the subject fresh, every binder slot in every arm is flagged. The
/// slots come from `MatchArm::binding_slots` — the resolver's positional
/// stamp (#949) — never from a name lookup; wildcard positions (`u16::MAX`)
/// bind nothing and are skipped.
///
/// Every binder slot is flagged, not only the collection-typed ones: the
/// consumers gate on collection receivers anyway, so a flag on an `Int`
/// binder is inert, while skipping a slot whose type stamp is missing would
/// be unsound.
///
/// ## The subject's RETENTION, and why it is two separate obligations
///
/// A binder is the only construct that hands the arm body a handle INTO the
/// subject, so whenever an arm binds anything, the subject's retention has to
/// be settled. It is settled once, by running the escape half
/// ([`flag_escaping_collection_locals`]) over the subject into a scratch
/// buffer, and the answer discharges two different duties:
///
/// - **What the subject retains is shared.** Every local in the buffer is
///   merged into the table. A fresh aggregate still RETAINS what was put in
///   it — `match {"k" => held} { mm -> mm }` hands `mm` a map that holds
///   `held`, so an owned `Vector.set(held, …)` later in the fn would rewrite
///   what `mm` reads back. The escape half used to run only when the subject
///   was NOT provably fresh, which is exactly backwards: freshness is a claim
///   about the aggregate, never about its contents (#953 round 3, probes
///   b/c/d/e). This is the same asymmetry the statement pass already has —
///   for `x = <rhs>` the escape half runs ALWAYS and only the DESTINATION
///   half is gated on freshness.
/// - **The binder itself is exempt only if there is nothing to share.** The
///   exemption's argument is that a fresh subject cannot be reached from
///   anywhere else — which covers the AGGREGATE and says nothing about what is
///   inside it, so it is safe only for a binder that IS the whole aggregate.
///   A destructuring binder over a fresh subject DOES occur — `Vector.set`
///   counts as fresh and returns `Option<Vector<T>>`, which `Option.Some(w)`
///   takes apart — and there the payload is the freshly built collection
///   itself, so the exemption still holds for the shapes that exist today.
///   It holds by what the whitelisted builders happen to return, though, not
///   by the argument above: a future fresh builder whose payload is a handle
///   into something older would break it. The condition below therefore has
///   no runtime witness today and is a fence rather than a bug fix; it is
///   here because the exemption is new in this change and the
///   module's rule is that a slot clears only on positive proof of
///   fresh-AND-non-escaping; a binder resting on freshness alone is the one
///   place that rule was not being applied, and the day a pattern reaches
///   inside a collection the exemption would be wrong rather than merely
///   unproven.
///
/// `match Map.get(m, k) { … }` still costs `m` nothing: the escape half skips
/// arg 0 of a `Vector` / `Map` builtin (a receiver is read, not retained), so
/// the buffer is empty and only the binder — a genuine container read — is
/// flagged, by the not-fresh half of the rule.
fn flag_match_binder_slots(expr: &Spanned<Expr>, slot_types: &[Type], aliased: &mut [bool]) {
    match &expr.node {
        Expr::Match { subject, arms } => {
            flag_match_binder_slots(subject, slot_types, aliased);
            // Only a BINDER makes the subject's retention anyone's business,
            // so a match that binds nothing pays for none of this.
            let binds_anywhere = arms.iter().any(|a| {
                a.binding_slots
                    .get()
                    .is_some_and(|slots| slots.iter().any(|&s| s != u16::MAX))
            });
            if binds_anywhere {
                // What the subject's value still holds onto, computed into a
                // scratch buffer so the same walk answers both duties above.
                let mut retained = vec![false; aliased.len()];
                flag_escaping_collection_locals(&subject.node, slot_types, &mut retained);
                let retains_nothing = !retained.iter().any(|&r| r);
                for (i, _) in retained.iter().enumerate().filter(|&(_, &r)| r) {
                    if let Some(s) = aliased.get_mut(i) {
                        *s = true;
                    }
                }
                if !(retains_nothing && rhs_is_fresh_collection(subject)) {
                    for slot in arms
                        .iter()
                        .filter_map(|a| a.binding_slots.get())
                        .flatten()
                        .copied()
                        .filter(|&s| s != u16::MAX)
                    {
                        if let Some(s) = aliased.get_mut(slot as usize) {
                            *s = true;
                        }
                    }
                }
            }
            for arm in arms {
                flag_match_binder_slots(&arm.body, slot_types, aliased);
            }
        }
        Expr::FnCall(callee, args) => {
            flag_match_binder_slots(callee, slot_types, aliased);
            for a in args {
                flag_match_binder_slots(a, slot_types, aliased);
            }
        }
        Expr::TailCall(tc) => {
            for a in &tc.args {
                flag_match_binder_slots(a, slot_types, aliased);
            }
        }
        Expr::Attr(inner, _) | Expr::Neg(inner) | Expr::ErrorProp(inner) => {
            flag_match_binder_slots(inner, slot_types, aliased);
        }
        Expr::BinOp(_, lhs, rhs) => {
            flag_match_binder_slots(lhs, slot_types, aliased);
            flag_match_binder_slots(rhs, slot_types, aliased);
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload {
                flag_match_binder_slots(p, slot_types, aliased);
            }
        }
        Expr::Tuple(items) | Expr::List(items) | Expr::IndependentProduct(items, _) => {
            for i in items {
                flag_match_binder_slots(i, slot_types, aliased);
            }
        }
        Expr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                flag_match_binder_slots(k, slot_types, aliased);
                flag_match_binder_slots(v, slot_types, aliased);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                flag_match_binder_slots(e, slot_types, aliased);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            flag_match_binder_slots(base, slot_types, aliased);
            for (_, e) in updates {
                flag_match_binder_slots(e, slot_types, aliased);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let StrPart::Parsed(e) = p {
                    flag_match_binder_slots(e, slot_types, aliased);
                }
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
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

    /// A fresh AGGREGATE spelled as a match subject still RETAINS what was put
    /// in it.
    ///
    /// The escape half is deliberately not run on a match subject from the
    /// statement pass — the arm tails become the value, so nothing of the
    /// subject escapes through it — and the binder pass used to run it only
    /// when the subject was NOT provably fresh. Between the two, `held` was
    /// never asked about at all, and the `Vector.set` after it took the owned
    /// in-place path straight through the map the binder is holding.
    ///
    /// Freshness answers for the map literal, never for `held` inside it, so
    /// the retention walk runs whenever an arm BINDS, whatever the subject's
    /// freshness says.
    #[test]
    fn a_fresh_match_subject_still_retains_the_local_it_holds() {
        let src = r#"
fn main() -> Int
    held = Vector.fromList([1, 2])
    keeper = match {"k" => held}
        mm -> mm
    Vector.len(Option.withDefault(Vector.set(held, 0, 5), Vector.fromList([]))) + Map.len(keeper)
"#;
        assert!(
            local_is_flagged(src, "main", "held"),
            "a local retained by a fresh match subject must be flagged — the \
             binder hands the arm body a container that still holds it"
        );
    }

    /// The binder's own exemption, which is the other half of the same
    /// question.
    ///
    /// A binder over a provably-fresh subject is owned-eligible — nothing else
    /// can reach a value nothing else has seen. But that argument only covers
    /// the aggregate: a fresh subject that RETAINS a non-fresh local can hand a
    /// binder that local itself, and then the arm writes through it. So the
    /// exemption is `fresh AND retains nothing`.
    ///
    /// `mm` is read only in receiver position here (`Map.len`), which the
    /// escape half treats as a consuming read — so the flag under test comes
    /// from the binder rule alone and from nowhere else.
    #[test]
    fn a_binder_over_a_retaining_fresh_subject_is_not_exempt() {
        let src = r#"
fn main() -> Int
    held = Vector.fromList([1, 2])
    n = match {"k" => held}
        mm -> Map.len(mm)
    n + Vector.len(held)
"#;
        assert!(
            local_is_flagged(src, "main", "mm"),
            "a binder over a fresh subject that retains a local must stay \
             flagged — the subject can hand the binder the retained local"
        );
    }

    /// The same rule's other direction: a fresh subject that retains NOTHING
    /// keeps its binders owned-eligible. This is what stops the retention walk
    /// from being a blanket "every binder is aliased" and taking the in-place
    /// path away from the shapes it was built for.
    #[test]
    fn a_binder_over_a_fresh_subject_that_retains_nothing_stays_owned() {
        let src = r#"
fn main() -> Int
    n = match Vector.new(4, 0)
        vv -> Vector.len(vv)
    n
"#;
        assert!(
            !local_is_flagged(src, "main", "vv"),
            "a binder over a fresh subject holding nothing was flagged, so the \
             owned fast path is gone from every match on a freshly built \
             collection"
        );
    }

    /// A container READ as a subject costs the container nothing.
    ///
    /// `Map.get(m, k)` reads `m` in receiver position, which is a consuming
    /// read and not a retention, so the walk over the subject leaves `m`
    /// owned-eligible. The BINDER is a different matter — it is a handle into
    /// what the map holds — and it is flagged by the not-fresh half of the
    /// rule, as it was before.
    #[test]
    fn a_match_on_a_container_read_costs_the_receiver_nothing() {
        let src = r#"
fn main() -> Int
    m = Map.set({}, "k", 1)
    n = match Map.get(m, "k")
        Option.Some(v) -> v
        Option.None -> 0
    Map.len(Map.set(m, "z", 9)) + n
"#;
        assert!(
            !local_is_flagged(src, "main", "m"),
            "matching on `Map.get(m, k)` flagged `m`, so every read-then-write \
             on a map now copies the whole map first"
        );
        assert!(
            local_is_flagged(src, "main", "v"),
            "the binder of a container read must stay flagged — it is a handle \
             into what the map holds"
        );
    }
}
