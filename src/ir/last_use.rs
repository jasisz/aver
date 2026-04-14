//! Slot-based last-use annotation pass.
//!
//! After the resolver transforms `Ident(name)` → `Resolved { slot, name, last_use: AnnotBool(false) }`,
//! this pass walks each function body backwards and sets `last_use = true` on every
//! `Resolved` node where the slot is not referenced again afterwards.
//!
//! This enables backends to move instead of copy:
//! - VM emits `MOVE_LOCAL` (clears slot, sole ownership) instead of `LOAD_LOCAL`
//! - Rust codegen skips `.clone()` for last-use locals
//! - Collection builtins (Map.set, Vector.set) mutate in-place via `CALL_BUILTIN_OWNED`

use std::collections::HashSet;

use crate::ast::*;

/// Annotate the entire program — walk all FnDefs after resolution.
pub fn annotate_program_last_use(items: &mut [TopLevel]) {
    for item in items.iter_mut() {
        if let TopLevel::FnDef(fd) = item
            && fd.resolution.is_some()
        {
            let mut body = fd.body.as_ref().clone();
            annotate_body(&mut body);
            fd.body = std::sync::Arc::new(body);
        }
    }
}

/// Annotate a function body.
fn annotate_body(body: &mut FnBody) {
    let stmts = body.stmts_mut();
    annotate_stmts(stmts, &HashSet::new());
}

/// Annotate a block of statements with last-use info.
///
/// `live_after` — slots known to be read after this block.
/// Walk backwards: each statement sees what comes after it.
fn annotate_stmts(stmts: &mut [Stmt], parent_live: &HashSet<u16>) {
    let n = stmts.len();
    // Compute live-after for each statement (backwards).
    let mut live_after_sets: Vec<HashSet<u16>> = vec![HashSet::new(); n];
    let mut suffix_live = parent_live.clone();

    for i in (0..n).rev() {
        live_after_sets[i] = suffix_live.clone();
        // Add slots referenced in this statement to suffix_live
        let stmt_slots = collect_slots_stmt(&stmts[i]);
        suffix_live.extend(stmt_slots);
        // A binding defines a slot — remove it from suffix (born here, not from outer scope).
        if let Stmt::Binding(name, _, _) = &stmts[i] {
            // Find the slot this binding defines by looking at any Resolved in the body
            // that has this name. Alternatively, we can check the resolution metadata.
            // Simpler: scan the RHS won't have it, but the next use will.
            // Actually, the binding name is resolved to a slot. We need to find it.
            // The slot is assigned by the resolver, but we don't have the mapping here.
            // Instead, we note: the binding introduces `name`. Any Resolved { slot, name: n, .. }
            // where n == name in subsequent code is the same slot. We can find it by scanning
            // forward — but that's complex. Instead, we track binding names and remove matching
            // slots from suffix_live.
            remove_slot_for_name(&mut suffix_live, name, stmts);
        }
    }

    // Now walk forward, annotating each statement's expressions.
    for i in 0..n {
        let live = &live_after_sets[i];
        match &mut stmts[i] {
            Stmt::Binding(_, _, expr) => annotate_expr(&mut expr.node, live),
            Stmt::Expr(expr) => annotate_expr(&mut expr.node, live),
        }
    }
}

/// Find the slot for a binding name by scanning statements for a matching Resolved.
fn remove_slot_for_name(live: &mut HashSet<u16>, name: &str, stmts: &[Stmt]) {
    // Scan all expressions in the block for a Resolved with this name
    for stmt in stmts {
        let expr = match stmt {
            Stmt::Binding(_, _, e) => &e.node,
            Stmt::Expr(e) => &e.node,
        };
        if let Some(slot) = find_slot_for_name(expr, name) {
            live.remove(&slot);
            return;
        }
    }
}

/// Find the slot number for a given variable name in an expression tree.
fn find_slot_for_name(expr: &Expr, target_name: &str) -> Option<u16> {
    match expr {
        Expr::Resolved { slot, name, .. } if name == target_name => Some(*slot),
        Expr::FnCall(fn_expr, args) => {
            find_slot_for_name(&fn_expr.node, target_name).or_else(|| {
                args.iter()
                    .find_map(|a| find_slot_for_name(&a.node, target_name))
            })
        }
        Expr::BinOp(_, left, right) => find_slot_for_name(&left.node, target_name)
            .or_else(|| find_slot_for_name(&right.node, target_name)),
        Expr::Match { subject, arms } => {
            find_slot_for_name(&subject.node, target_name).or_else(|| {
                arms.iter()
                    .find_map(|arm| find_slot_for_name(&arm.body.node, target_name))
            })
        }
        Expr::Attr(obj, _) => find_slot_for_name(&obj.node, target_name),
        Expr::ErrorProp(inner) => find_slot_for_name(&inner.node, target_name),
        Expr::Constructor(_, Some(inner)) => find_slot_for_name(&inner.node, target_name),
        Expr::InterpolatedStr(parts) => parts.iter().find_map(|p| match p {
            StrPart::Parsed(e) => find_slot_for_name(&e.node, target_name),
            _ => None,
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => items
            .iter()
            .find_map(|e| find_slot_for_name(&e.node, target_name)),
        Expr::TailCall(boxed) => boxed
            .args
            .iter()
            .find_map(|a| find_slot_for_name(&a.node, target_name)),
        Expr::MapLiteral(entries) => entries.iter().find_map(|(k, v)| {
            find_slot_for_name(&k.node, target_name)
                .or_else(|| find_slot_for_name(&v.node, target_name))
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .find_map(|(_, e)| find_slot_for_name(&e.node, target_name)),
        Expr::RecordUpdate { base, updates, .. } => find_slot_for_name(&base.node, target_name)
            .or_else(|| {
                updates
                    .iter()
                    .find_map(|(_, e)| find_slot_for_name(&e.node, target_name))
            }),
        _ => None,
    }
}

/// Collect all slots referenced in an expression.
fn collect_slots(expr: &Expr) -> HashSet<u16> {
    let mut slots = HashSet::new();
    collect_slots_inner(expr, &mut slots);
    slots
}

fn collect_slots_inner(expr: &Expr, slots: &mut HashSet<u16>) {
    match expr {
        Expr::Resolved { slot, .. } => {
            slots.insert(*slot);
        }
        Expr::Ident(_) | Expr::Literal(_) => {}
        Expr::Attr(obj, _) => collect_slots_inner(&obj.node, slots),
        Expr::FnCall(fn_expr, args) => {
            collect_slots_inner(&fn_expr.node, slots);
            for a in args {
                collect_slots_inner(&a.node, slots);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_slots_inner(&left.node, slots);
            collect_slots_inner(&right.node, slots);
        }
        Expr::Match { subject, arms } => {
            collect_slots_inner(&subject.node, slots);
            for arm in arms {
                collect_slots_inner(&arm.body.node, slots);
            }
        }
        Expr::Constructor(_, Some(inner)) => collect_slots_inner(&inner.node, slots),
        Expr::Constructor(_, None) => {}
        Expr::ErrorProp(inner) => collect_slots_inner(&inner.node, slots),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(expr) = part {
                    collect_slots_inner(&expr.node, slots);
                }
            }
        }
        Expr::List(elements) => {
            for e in elements {
                collect_slots_inner(&e.node, slots);
            }
        }
        Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for e in items {
                collect_slots_inner(&e.node, slots);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_slots_inner(&k.node, slots);
                collect_slots_inner(&v.node, slots);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                collect_slots_inner(&expr.node, slots);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_slots_inner(&base.node, slots);
            for (_, expr) in updates {
                collect_slots_inner(&expr.node, slots);
            }
        }
        Expr::TailCall(boxed) => {
            for a in &boxed.args {
                collect_slots_inner(&a.node, slots);
            }
        }
    }
}

/// Collect slots in a statement.
fn collect_slots_stmt(stmt: &Stmt) -> HashSet<u16> {
    match stmt {
        Stmt::Binding(_, _, expr) => collect_slots(&expr.node),
        Stmt::Expr(expr) => collect_slots(&expr.node),
    }
}

/// Annotate an expression tree: set `last_use = true` on Resolved nodes
/// whose slot is not in `live_after`.
///
/// For compound expressions (FnCall args, BinOp operands), we compute
/// intra-expression liveness: later sub-expressions make earlier ones
/// non-last-use if they share a slot.
fn annotate_expr(expr: &mut Expr, live_after: &HashSet<u16>) {
    match expr {
        Expr::Resolved { slot, last_use, .. } => {
            *last_use = AnnotBool(!live_after.contains(slot));
        }
        Expr::Ident(_) | Expr::Literal(_) => {}
        Expr::Attr(obj, _) => annotate_expr(&mut obj.node, live_after),
        Expr::FnCall(fn_expr, args) => {
            // Intra-expression liveness: later args make earlier args non-last-use.
            // Walk args right-to-left, accumulating live slots.
            let mut cumulative_live = live_after.clone();
            for arg in args.iter_mut().rev() {
                annotate_expr(&mut arg.node, &cumulative_live);
                let arg_slots = collect_slots(&arg.node);
                cumulative_live.extend(arg_slots);
            }
            annotate_expr(&mut fn_expr.node, &cumulative_live);
        }
        Expr::BinOp(_, left, right) => {
            // Right operand evaluated after left — right's slots affect left's liveness.
            let mut left_live = live_after.clone();
            left_live.extend(collect_slots(&right.node));
            annotate_expr(&mut left.node, &left_live);
            annotate_expr(&mut right.node, live_after);
        }
        Expr::Match { subject, arms } => {
            // Subject is evaluated before any arm — all arm slots are "after" subject.
            let mut subject_live = live_after.clone();
            for arm in arms.iter() {
                subject_live.extend(collect_slots(&arm.body.node));
            }
            annotate_expr(&mut subject.node, &subject_live);

            // Each arm is independent (only one executes at runtime).
            for arm in arms.iter_mut() {
                annotate_expr(&mut arm.body.node, live_after);
            }
        }
        Expr::Constructor(_, Some(inner)) => annotate_expr(&mut inner.node, live_after),
        Expr::Constructor(_, None) => {}
        Expr::ErrorProp(inner) => annotate_expr(&mut inner.node, live_after),
        Expr::InterpolatedStr(parts) => {
            // Parts evaluated left-to-right.
            let mut cumulative_live = live_after.clone();
            for part in parts.iter_mut().rev() {
                if let StrPart::Parsed(expr) = part {
                    annotate_expr(&mut expr.node, &cumulative_live);
                    cumulative_live.extend(collect_slots(&expr.node));
                }
            }
        }
        Expr::List(elements) => {
            let mut cumulative_live = live_after.clone();
            for e in elements.iter_mut().rev() {
                annotate_expr(&mut e.node, &cumulative_live);
                cumulative_live.extend(collect_slots(&e.node));
            }
        }
        Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            let mut cumulative_live = live_after.clone();
            for e in items.iter_mut().rev() {
                annotate_expr(&mut e.node, &cumulative_live);
                cumulative_live.extend(collect_slots(&e.node));
            }
        }
        Expr::MapLiteral(entries) => {
            let mut cumulative_live = live_after.clone();
            for (k, v) in entries.iter_mut().rev() {
                annotate_expr(&mut v.node, &cumulative_live);
                cumulative_live.extend(collect_slots(&v.node));
                annotate_expr(&mut k.node, &cumulative_live);
                cumulative_live.extend(collect_slots(&k.node));
            }
        }
        Expr::RecordCreate { fields, .. } => {
            let mut cumulative_live = live_after.clone();
            for (_, e) in fields.iter_mut().rev() {
                annotate_expr(&mut e.node, &cumulative_live);
                cumulative_live.extend(collect_slots(&e.node));
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            let mut cumulative_live = live_after.clone();
            for (_, e) in updates.iter_mut().rev() {
                annotate_expr(&mut e.node, &cumulative_live);
                cumulative_live.extend(collect_slots(&e.node));
            }
            annotate_expr(&mut base.node, &cumulative_live);
        }
        Expr::TailCall(boxed) => {
            // Tail call args evaluated left-to-right, nothing after.
            let mut cumulative_live = live_after.clone();
            for a in boxed.args.iter_mut().rev() {
                annotate_expr(&mut a.node, &cumulative_live);
                cumulative_live.extend(collect_slots(&a.node));
            }
        }
    }
}
