//! Which field reads may move their field out of a record local.
//!
//! A read `s.window.created` normally clones the field: the record `s`
//! may be read again. When nothing reads that part of `s` afterwards, the
//! field can move instead, which keeps a Map or Vector inside it uniquely
//! owned by whoever receives it.
//!
//! The answer here is structural and backend-neutral: it names the
//! projection nodes that no later or still-borrowed read of the same local
//! overlaps. A backend still decides whether the root local is an owned
//! value it may partially move (a borrowed parameter never is).
//!
//! A projection `q` rooted at local `s` is movable when every other read
//! `o` of `s` in the body either
//!
//! - cannot run together with `q` (the two sit in different arms of one
//!   `match`, or in the two branches of one `if`),
//! - finished before `q` began and holds nothing of `s` afterwards (`o` is
//!   in a `let` value and `q` in that `let`'s body), or
//! - reads a disjoint part of `s` (`s.window.spent` beside
//!   `s.window.created`; the base of `T.update(s, window = ...)` at its
//!   last use beside anything under `s.window`).
//!
//! and one of the reads that can run with `q` is the local's last use, so
//! nothing reads `s` after them. A read inside an independent product is
//! never movable: its branches run on their own threads.
//!
//! ## The target of a matched `Vector.set`
//!
//! In
//!
//! ```text
//! match Vector.set(s.cells, i, x)
//!     Option.Some(updated) -> State.update(s, cells = updated)
//!     Option.None -> s
//! ```
//!
//! the `None` arm reads the whole of `s`, so a read of `s.cells` in the
//! subject could never move: the `None` arm still needs the field. But the
//! `None` arm is taken exactly when the index is out of range, and then the
//! set changes nothing. A backend that evaluates the index and the value,
//! checks the index against the length, and only then reads the target, in
//! the `Some` arm, needs the target in the `Some` arm alone
//! ([`vector_set_match`]). The analysis places that read there, so it
//! moves when nothing else in the `Some` arm or after the match needs the
//! field.

use std::collections::{HashMap, HashSet};

use super::expr::{
    MirCallee, MirCtor, MirExpr, MirMatch, MirPattern, MirRecordUpdate, walk_children,
};
use super::program::LocalId;
use crate::ast::Spanned;
use crate::ir::hir::BuiltinCtor;

/// A `match` on `Vector.set(target, index, value)` with one arm for
/// `Option.Some` and one for `Option.None` (or a wildcard after the `Some`
/// arm), whose target is a field path of a local (`s.cells`, `s.a.cells`).
///
/// The `None` arm runs exactly when the index is out of range, and then the
/// set has changed nothing: whatever the arm reads of the target's record
/// is what was there before. A backend that evaluates `index` and `value`,
/// checks the index, and reads `target` only once the `Some` arm is chosen
/// never needs the target in the `None` arm, so the target read belongs to
/// the `Some` arm. Generated Rust emits the match that way when the read may
/// move ([`movable_projections`]); the VM takes the field under the same
/// shape.
pub struct VectorSetMatch<'a> {
    pub target: &'a Spanned<MirExpr>,
    pub index: &'a Spanned<MirExpr>,
    pub value: &'a Spanned<MirExpr>,
    /// Arm positions in `arms`.
    pub some_arm: usize,
    pub none_arm: usize,
    /// The local the `Some` arm binds the updated Vector to, if any.
    pub updated: Option<(LocalId, &'a str)>,
    /// Address of the subject call.
    pub call: usize,
}

/// See [`VectorSetMatch`]. `builtins` names the program's builtin ids.
pub fn vector_set_match<'a>(m: &'a MirMatch, builtins: &[String]) -> Option<VectorSetMatch<'a>> {
    let MirExpr::Call(call) = &m.subject.node else {
        return None;
    };
    let MirCallee::Builtin(id) = call.node.callee else {
        return None;
    };
    if builtins.get(id.0 as usize).map(String::as_str) != Some("Vector.set")
        || call.node.args.len() != 3
    {
        return None;
    }
    let target = &call.node.args[0];
    if !projection_root(&target.node).is_some_and(|(_, _, path)| !path.is_empty()) {
        return None;
    }
    if m.arms.len() != 2 {
        return None;
    }
    let some = |pattern: &'a MirPattern| match pattern {
        MirPattern::Ctor {
            ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
            bindings,
            binding_names,
        } if bindings.len() == 1 => Some(Some((
            bindings[0],
            binding_names.first().map(String::as_str).unwrap_or("_"),
        ))),
        _ => None,
    };
    let none = |pattern: &MirPattern| {
        matches!(
            pattern,
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
                ..
            }
        )
    };
    let (some_arm, none_arm, updated) = match (some(&m.arms[0].pattern), some(&m.arms[1].pattern)) {
        (Some(updated), None)
            if none(&m.arms[1].pattern) || matches!(m.arms[1].pattern, MirPattern::Wildcard) =>
        {
            (0, 1, updated)
        }
        (None, Some(updated)) if none(&m.arms[0].pattern) => (1, 0, updated),
        _ => return None,
    };
    Some(VectorSetMatch {
        target,
        index: &call.node.args[1],
        value: &call.node.args[2],
        some_arm,
        none_arm,
        updated,
        call: &call.node as *const _ as usize,
    })
}

/// The part of a local one read observes.
#[derive(Debug, Clone)]
enum Part {
    /// `s` (empty path) or `s.a.b`.
    Path(Vec<String>),
    /// The base of `T.update(s, a = ...)` (empty prefix) or of
    /// `T.update(s.window, a = ...)` (prefix `window`): every field under
    /// the prefix except the replaced ones.
    AllExcept {
        prefix: Vec<String>,
        replaced: Vec<String>,
    },
}

#[derive(Debug)]
struct Read {
    part: Part,
    /// For the base of an update: the update node's address, so the reads
    /// inside its own field values can be told apart.
    update: Option<usize>,
    /// Address of the read's outermost node (the projection chain or the
    /// local itself).
    addr: usize,
    last_use: bool,
    /// Whether the local read under this part is the local's last use (for
    /// a field chain base, `last_use` is whether the base may move).
    root_last_use: bool,
    in_product: bool,
    /// Each ancestor on the way down from the body: its address and the
    /// index of the child the read sits under.
    trail: Vec<(usize, usize, Branching)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Branching {
    /// Children are alternatives from index 1 on (`match` arms, `if`
    /// branches); index 0 is the subject or condition.
    Alternatives,
    /// `let`: child 0 is the value, child 1 the body.
    Sequence,
    /// A call: its value holds nothing of what its arguments read.
    Call,
    Other,
}

/// Addresses (`&MirExpr as *const _ as usize`) of the projection nodes in
/// `body` that may move their field out of their root local.
///
/// The base of an update of a field chain (`T.update(s.window, a = ...)`)
/// is in the set too when it may move what the update keeps: the rest of
/// `s.window` then moves into the new record instead of being cloned, and
/// the fields it replaces may move out before it.
///
/// `builtins` names the program's builtin ids, for [`vector_set_match`].
pub fn movable_projections(body: &MirExpr, builtins: &[String]) -> HashSet<usize> {
    let mut reads: HashMap<LocalId, Vec<Read>> = HashMap::new();
    let mut trail = Vec::new();
    collect(body, &mut trail, false, builtins, &mut reads);
    let mut out = HashSet::new();
    // A chain base is final once nothing outside its own update reads what
    // it keeps; a base further in may depend on one further out, so this
    // runs until nothing changes.
    loop {
        let mut changed = false;
        for group in reads.values_mut() {
            for bi in 0..group.len() {
                if group[bi].last_use || !base_is_final(group, bi) {
                    continue;
                }
                group[bi].last_use = true;
                out.insert(group[bi].addr);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    for group in reads.values() {
        for (qi, q) in group.iter().enumerate() {
            let Part::Path(path) = &q.part else {
                continue;
            };
            if path.is_empty() || q.in_product {
                continue;
            }
            let mut ends_here = q.last_use;
            let mut blocked = false;
            for (oi, o) in group.iter().enumerate() {
                if oi == qi || apart(o, q) {
                    continue;
                }
                if !disjoint(&o.part, o.last_use, path) {
                    blocked = true;
                    break;
                }
                ends_here |= o.last_use;
            }
            if !blocked && ends_here {
                out.insert(q.addr);
            }
        }
    }
    out
}

/// Whether the base `group[bi]` of an update of a field chain may move what
/// the update keeps: every other read of the local either sits inside the
/// update's own field values (they run first), cannot run together with
/// it, or reads a disjoint part; and one of the reads that can run with it
/// is the local's last use.
fn base_is_final(group: &[Read], bi: usize) -> bool {
    let b = &group[bi];
    let (Part::AllExcept { prefix, .. }, Some(update)) = (&b.part, b.update) else {
        return false;
    };
    if prefix.is_empty() || b.in_product {
        return false;
    }
    let mut ends_here = b.root_last_use;
    for (oi, o) in group.iter().enumerate() {
        if oi == bi || apart(o, b) {
            continue;
        }
        let inside = o
            .trail
            .iter()
            .any(|(addr, child, _)| *addr == update && *child >= 1);
        if !inside && !disjoint(&o.part, o.last_use, prefix) {
            return false;
        }
        ends_here |= o.last_use || o.root_last_use;
    }
    ends_here
}

/// The locals under `body` that give up a field through one of the
/// `movable` projections. Such a local may be partially moved by the time
/// it is read again, so it can only be read by fields it still holds.
pub fn moved_roots(body: &MirExpr, movable: &HashSet<usize>) -> HashSet<LocalId> {
    let mut roots = HashSet::new();
    if !movable.is_empty() {
        gather_moved_roots(body, movable, &mut roots);
    }
    roots
}

fn gather_moved_roots(expr: &MirExpr, movable: &HashSet<usize>, roots: &mut HashSet<LocalId>) {
    if movable.contains(&(expr as *const MirExpr as usize))
        && let Some((slot, _, _)) = projection_root(expr)
    {
        roots.insert(slot);
        return;
    }
    walk_children(expr, &mut |child| gather_moved_roots(child, movable, roots));
}

/// The field path of a projection chain rooted at a local, outermost last.
fn projection_root(expr: &MirExpr) -> Option<(LocalId, bool, Vec<String>)> {
    let mut fields = Vec::new();
    let mut cursor = expr;
    loop {
        match cursor {
            MirExpr::Project(project) => {
                fields.push(project.node.field.clone());
                cursor = &project.node.base.node;
            }
            MirExpr::Local(local) => {
                fields.reverse();
                return Some((local.node.slot, local.node.last_use, fields));
            }
            _ => return None,
        }
    }
}

fn collect(
    expr: &MirExpr,
    trail: &mut Vec<(usize, usize, Branching)>,
    in_product: bool,
    builtins: &[String],
    reads: &mut HashMap<LocalId, Vec<Read>>,
) {
    let addr = expr as *const MirExpr as usize;
    match expr {
        MirExpr::Local(local) => {
            reads.entry(local.node.slot).or_default().push(Read {
                part: Part::Path(Vec::new()),
                update: None,
                addr,
                last_use: local.node.last_use,
                root_last_use: local.node.last_use,
                in_product,
                trail: trail.clone(),
            });
            return;
        }
        MirExpr::Project(_) => {
            if let Some((slot, last_use, path)) = projection_root(expr) {
                reads.entry(slot).or_default().push(Read {
                    part: Part::Path(path),
                    update: None,
                    addr,
                    last_use,
                    root_last_use: last_use,
                    in_product,
                    trail: trail.clone(),
                });
                return;
            }
        }
        MirExpr::RecordUpdate(update) => {
            if let Some((slot, root_last_use, prefix)) = projection_root(&update.node.base.node) {
                let replaced = update
                    .node
                    .updates
                    .iter()
                    .map(|field| field.name.clone())
                    .collect();
                // A local base is final by its last-use flags; a field chain
                // base is decided once every read is known.
                let last_use = prefix.is_empty() && update_base_is_final(&update.node);
                reads.entry(slot).or_default().push(Read {
                    part: Part::AllExcept { prefix, replaced },
                    update: Some(addr),
                    addr: &update.node.base.node as *const MirExpr as usize,
                    last_use,
                    root_last_use,
                    in_product,
                    trail: {
                        let mut t = trail.clone();
                        t.push((addr, 0, Branching::Other));
                        t
                    },
                });
                for (index, field) in update.node.updates.iter().enumerate() {
                    trail.push((addr, index + 1, Branching::Other));
                    collect(&field.value.node, trail, in_product, builtins, reads);
                    trail.pop();
                }
                return;
            }
        }
        MirExpr::Match(m) => {
            if let Some(set) = vector_set_match(&m.node, builtins) {
                // The index and the value run in the subject; the target is
                // read in the `Some` arm (see `VectorSetMatch`).
                trail.push((addr, 0, Branching::Alternatives));
                for (index, arg) in [(1, set.index), (2, set.value)] {
                    trail.push((set.call, index, Branching::Call));
                    collect(&arg.node, trail, in_product, builtins, reads);
                    trail.pop();
                }
                trail.pop();
                trail.push((addr, set.some_arm + 1, Branching::Alternatives));
                trail.push((set.call, 0, Branching::Call));
                collect(&set.target.node, trail, in_product, builtins, reads);
                trail.pop();
                trail.pop();
                for (index, arm) in m.node.arms.iter().enumerate() {
                    trail.push((addr, index + 1, Branching::Alternatives));
                    collect(&arm.body.node, trail, in_product, builtins, reads);
                    trail.pop();
                }
                return;
            }
        }
        _ => {}
    }
    let branching = match expr {
        MirExpr::Match(_) | MirExpr::IfThenElse(_) => Branching::Alternatives,
        MirExpr::Let(_) => Branching::Sequence,
        MirExpr::Call(_) => Branching::Call,
        _ => Branching::Other,
    };
    let in_product = in_product || matches!(expr, MirExpr::IndependentProduct(_));
    let mut index = 0;
    walk_children(expr, &mut |child| {
        trail.push((addr, index, branching));
        collect(child, trail, in_product, builtins, reads);
        trail.pop();
        index += 1;
    });
}

/// Which Map and Vector params of every function are updated in place:
/// the param is the target of `Map.set`, `Map.remove` or `Vector.set`, or
/// is handed to a callee param that is. Moving a field into a param that
/// only reads it saves nothing, so only these params make a field move
/// worth taking a record by value for.
pub fn in_place_collection_params(
    program: &crate::ir::mir::program::MirProgram,
) -> HashMap<crate::ir::FnId, Vec<bool>> {
    let mut updated: HashMap<crate::ir::FnId, Vec<bool>> = program
        .iter()
        .map(|(id, f)| (*id, vec![false; f.params.len()]))
        .collect();
    loop {
        let mut changed = Vec::new();
        for (id, f) in program.iter() {
            for (index, param) in f.params.iter().enumerate() {
                if !updated[id][index]
                    && updates_in_place(&f.body.node, param.local, &updated, &program.builtins)
                {
                    changed.push((*id, index));
                }
            }
        }
        if changed.is_empty() {
            return updated;
        }
        for (id, index) in changed {
            if let Some(params) = updated.get_mut(&id) {
                params[index] = true;
            }
        }
    }
}

fn updates_in_place(
    expr: &MirExpr,
    slot: LocalId,
    updated: &HashMap<crate::ir::FnId, Vec<bool>>,
    builtins: &[String],
) -> bool {
    let is_slot = |arg: &MirExpr| matches!(arg, MirExpr::Local(local) if local.node.slot == slot);
    let hands_on = |callee: crate::ir::FnId, args: &[crate::ast::Spanned<MirExpr>]| {
        args.iter().enumerate().any(|(index, arg)| {
            is_slot(&arg.node)
                && updated
                    .get(&callee)
                    .and_then(|params| params.get(index))
                    .copied()
                    .unwrap_or(false)
        })
    };
    let found = match expr {
        MirExpr::Call(call) => match call.node.callee {
            super::expr::MirCallee::Builtin(id) => {
                matches!(
                    builtins.get(id.0 as usize).map(String::as_str),
                    Some("Map.set" | "Map.remove" | "Vector.set")
                ) && call.node.args.first().is_some_and(|arg| is_slot(&arg.node))
            }
            super::expr::MirCallee::Fn(callee) => hands_on(callee, &call.node.args),
            _ => false,
        },
        MirExpr::TailCall(call) => hands_on(call.node.target, &call.node.args),
        _ => false,
    };
    if found {
        return true;
    }
    let mut below = false;
    walk_children(expr, &mut |child| {
        below = below || updates_in_place(child, slot, updated, builtins);
    });
    below
}

/// Whether this update reads its base local for the last time: the base
/// read is the local's last use, or every last use of the local inside the
/// new field values reads a field the update replaces
/// (`T.update(s, window = f(s.window.created))`). Nothing reads the local
/// after such an update, and a field value can only move a field the update
/// replaces, so the base may move after the field values run.
pub fn update_base_is_final(update: &MirRecordUpdate) -> bool {
    let MirExpr::Local(base) = &update.base.node else {
        return false;
    };
    if base.node.last_use {
        return true;
    }
    let slot = base.node.slot;
    let replaced: Vec<&str> = update.updates.iter().map(|f| f.name.as_str()).collect();
    let mut finals = 0usize;
    let mut stray = false;
    for field in &update.updates {
        final_reads(&field.value.node, slot, &replaced, &mut finals, &mut stray);
    }
    finals > 0 && !stray
}

/// Count the last-use reads of `slot` under `expr` that read a replaced
/// field, and note any last-use read that does not.
fn final_reads(
    expr: &MirExpr,
    slot: LocalId,
    replaced: &[&str],
    finals: &mut usize,
    stray: &mut bool,
) {
    if let Some((root, last_use, path)) = projection_root(expr)
        && root == slot
    {
        if last_use {
            match path.first() {
                Some(first) if replaced.contains(&first.as_str()) => *finals += 1,
                _ => *stray = true,
            }
        }
        return;
    }
    walk_children(expr, &mut |child| {
        final_reads(child, slot, replaced, finals, stray)
    });
}

/// Whether `o` never runs together with `q`, or finishes before `q` starts
/// without holding a borrow of the local.
fn apart(o: &Read, q: &Read) -> bool {
    for (depth, (a, b)) in o.trail.iter().zip(q.trail.iter()).enumerate() {
        if a == b {
            continue;
        }
        let (_, o_child, branching) = *a;
        let q_child = b.1;
        return match branching {
            // Two arms never run together. A read inside a call in the
            // subject or condition is over once that call returns, before
            // any arm runs: the call's value holds nothing of it.
            Branching::Alternatives => {
                (o_child >= 1 && q_child >= 1)
                    || (o_child == 0
                        && q_child >= 1
                        && o.trail[depth + 1..]
                            .iter()
                            .any(|(_, _, below)| *below == Branching::Call))
            }
            Branching::Sequence => o_child == 0 && q_child == 1,
            Branching::Call | Branching::Other => false,
        };
    }
    false
}

/// Whether a read of `part` leaves `path` untouched. The base of an update
/// that may not move is cloned whole, so it overlaps everything under its
/// prefix.
fn disjoint(part: &Part, last_use: bool, path: &[String]) -> bool {
    match part {
        Part::Path(other) => {
            let common = other.len().min(path.len());
            other[..common] != path[..common]
        }
        Part::AllExcept { prefix, replaced } => {
            let common = prefix.len().min(path.len());
            if prefix[..common] != path[..common] {
                return true;
            }
            last_use
                && path.len() > prefix.len()
                && replaced.iter().any(|field| *field == path[prefix.len()])
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Spanned;
    use crate::ir::mir::expr::{
        MirCall, MirCallee, MirLet, MirLocal, MirProject, MirRecordField, MirRecordUpdate,
    };
    use crate::ir::{FnId, TypeId};

    fn sp(expr: MirExpr) -> Spanned<MirExpr> {
        Spanned::bare(expr)
    }

    fn local(slot: u32, last_use: bool) -> Spanned<MirExpr> {
        sp(MirExpr::Local(Spanned::bare(MirLocal {
            slot: LocalId(slot),
            last_use,
            name: "s".to_string(),
        })))
    }

    fn project(base: Spanned<MirExpr>, field: &str) -> Spanned<MirExpr> {
        sp(MirExpr::Project(Spanned::bare(MirProject {
            base: Box::new(base),
            field: field.to_string(),
        })))
    }

    fn call(args: Vec<Spanned<MirExpr>>) -> Spanned<MirExpr> {
        sp(MirExpr::Call(Spanned::bare(MirCall {
            callee: MirCallee::Fn(FnId(0)),
            args,
        })))
    }

    fn addr(expr: &Spanned<MirExpr>) -> usize {
        &expr.node as *const MirExpr as usize
    }

    fn args(expr: &Spanned<MirExpr>) -> &[Spanned<MirExpr>] {
        match &expr.node {
            MirExpr::Call(call) => &call.node.args,
            _ => panic!("not a call"),
        }
    }

    #[test]
    fn disjoint_fields_at_the_last_use_both_move() {
        let body = call(vec![
            project(project(local(0, false), "window"), "created"),
            project(project(local(0, true), "window"), "spent"),
        ]);
        let movable = movable_projections(&body.node, &[]);
        assert!(movable.contains(&addr(&args(&body)[0])));
        assert!(movable.contains(&addr(&args(&body)[1])));
    }

    #[test]
    fn an_overlapping_read_blocks_the_move() {
        let body = call(vec![
            project(project(local(0, false), "window"), "created"),
            project(local(0, true), "window"),
        ]);
        let movable = movable_projections(&body.node, &[]);
        assert!(movable.is_empty());
    }

    #[test]
    fn a_read_before_the_last_use_does_not_move() {
        let body = sp(MirExpr::Let(Spanned::bare(MirLet {
            binding: LocalId(1),
            binding_name: "a".to_string(),
            value: Box::new(call(vec![project(local(0, false), "window")])),
            body: Box::new(call(vec![project(local(0, true), "window")])),
        })));
        let movable = movable_projections(&body.node, &[]);
        let MirExpr::Let(chain) = &body.node else {
            unreachable!()
        };
        assert_eq!(movable.len(), 1);
        assert!(movable.contains(&addr(&args(&chain.node.body)[0])));
    }

    #[test]
    fn a_field_chain_base_moves_what_its_update_keeps() {
        // Setting.update(s, window = Window.update(s.window,
        //     created = f(s.window.created)), height = g(s.height))
        let inner_base = project(local(0, false), "window");
        let inner = sp(MirExpr::RecordUpdate(Spanned::bare(MirRecordUpdate {
            type_id: Some(TypeId(1)),
            type_name: "Window".to_string(),
            base: Box::new(inner_base),
            updates: vec![MirRecordField {
                name: "created".to_string(),
                value: call(vec![project(project(local(0, false), "window"), "created")]),
            }],
        })));
        let body = sp(MirExpr::RecordUpdate(Spanned::bare(MirRecordUpdate {
            type_id: Some(TypeId(0)),
            type_name: "Setting".to_string(),
            base: Box::new(local(0, false)),
            updates: vec![
                MirRecordField {
                    name: "window".to_string(),
                    value: inner,
                },
                MirRecordField {
                    name: "height".to_string(),
                    value: call(vec![project(local(0, true), "height")]),
                },
            ],
        })));
        let movable = movable_projections(&body.node, &[]);
        let MirExpr::RecordUpdate(outer) = &body.node else {
            unreachable!()
        };
        let MirExpr::RecordUpdate(inner) = &outer.node.updates[0].value.node else {
            unreachable!()
        };
        assert!(movable.contains(&addr(&inner.node.base)));
        assert!(movable.contains(&addr(&args(&inner.node.updates[0].value)[0])));
    }

    #[test]
    fn a_field_chain_base_read_again_after_its_update_is_cloned() {
        // f(Window.update(s.window, created = g(s.window.created)), s.window)
        let update = sp(MirExpr::RecordUpdate(Spanned::bare(MirRecordUpdate {
            type_id: Some(TypeId(1)),
            type_name: "Window".to_string(),
            base: Box::new(project(local(0, false), "window")),
            updates: vec![MirRecordField {
                name: "created".to_string(),
                value: call(vec![project(project(local(0, false), "window"), "created")]),
            }],
        })));
        let body = call(vec![update, project(local(0, true), "window")]);
        assert!(movable_projections(&body.node, &[]).len() <= 1);
        let MirExpr::RecordUpdate(update) = &args(&body)[0].node else {
            unreachable!()
        };
        let movable = movable_projections(&body.node, &[]);
        assert!(!movable.contains(&addr(&update.node.base)));
        assert!(!movable.contains(&addr(&args(&update.node.updates[0].value)[0])));
    }

    #[test]
    fn a_read_inside_a_call_in_the_subject_is_over_before_the_arms() {
        use crate::ir::mir::expr::{MirMatch, MirMatchArm, MirPattern};
        // match f(s.jobs)
        //     _ -> T.update(s, jobs = g(s.jobs))
        let update = sp(MirExpr::RecordUpdate(Spanned::bare(MirRecordUpdate {
            type_id: Some(TypeId(0)),
            type_name: "State".to_string(),
            base: Box::new(local(0, false)),
            updates: vec![MirRecordField {
                name: "jobs".to_string(),
                value: call(vec![project(local(0, true), "jobs")]),
            }],
        })));
        let arm_read = addr(
            &args(match &update.node {
                MirExpr::RecordUpdate(update) => &update.node.updates[0].value,
                _ => unreachable!(),
            })[0],
        );
        let subject = call(vec![project(local(0, false), "jobs")]);
        let subject_read = addr(&args(&subject)[0]);
        let body = sp(MirExpr::Match(Spanned::bare(MirMatch {
            subject: Box::new(subject),
            arms: vec![MirMatchArm {
                pattern: MirPattern::Wildcard,
                body: update,
            }],
        })));
        let movable = movable_projections(&body.node, &[]);
        assert!(movable.contains(&arm_read));
        assert!(!movable.contains(&subject_read));
    }

    #[test]
    fn a_subject_bound_whole_is_not_over_before_the_arms() {
        use crate::ir::mir::expr::{MirMatch, MirMatchArm, MirPattern};
        // match s.jobs
        //     _ -> g(s.jobs)
        let arm = call(vec![project(local(0, true), "jobs")]);
        let arm_read = addr(&args(&arm)[0]);
        let body = sp(MirExpr::Match(Spanned::bare(MirMatch {
            subject: Box::new(project(local(0, false), "jobs")),
            arms: vec![MirMatchArm {
                pattern: MirPattern::Wildcard,
                body: arm,
            }],
        })));
        assert!(!movable_projections(&body.node, &[]).contains(&arm_read));
    }

    #[test]
    fn a_field_moves_beside_the_base_of_its_own_update() {
        let body = sp(MirExpr::RecordUpdate(Spanned::bare(MirRecordUpdate {
            type_id: Some(TypeId(0)),
            type_name: "Setting".to_string(),
            base: Box::new(local(0, false)),
            updates: vec![MirRecordField {
                name: "window".to_string(),
                value: call(vec![project(project(local(0, true), "window"), "created")]),
            }],
        })));
        assert_eq!(movable_projections(&body.node, &[]).len(), 1);
    }

    /// `match Vector.set(s.cells, 0, 1)` with `some_arm` as the `Some` arm and
    /// `none_arm` as the `None` arm; returns the body and the target's address.
    fn vector_set_match_body(
        some_arm: Spanned<MirExpr>,
        none_arm: Spanned<MirExpr>,
    ) -> (Spanned<MirExpr>, usize) {
        use crate::ir::BuiltinId;
        use crate::ir::mir::expr::{MirMatch, MirMatchArm};
        let subject = sp(MirExpr::Call(Spanned::bare(MirCall {
            callee: MirCallee::Builtin(BuiltinId(0)),
            args: vec![
                project(local(0, false), "cells"),
                sp(MirExpr::Literal(Spanned::bare(crate::ast::Literal::Int(0)))),
                sp(MirExpr::Literal(Spanned::bare(crate::ast::Literal::Int(1)))),
            ],
        })));
        let target = addr(&args(&subject)[0]);
        let body = sp(MirExpr::Match(Spanned::bare(MirMatch {
            subject: Box::new(subject),
            arms: vec![
                MirMatchArm {
                    pattern: MirPattern::Ctor {
                        ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
                        bindings: vec![LocalId(1)],
                        binding_names: vec!["updated".to_string()],
                    },
                    body: some_arm,
                },
                MirMatchArm {
                    pattern: MirPattern::Ctor {
                        ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
                        bindings: vec![],
                        binding_names: vec![],
                    },
                    body: none_arm,
                },
            ],
        })));
        (body, target)
    }

    fn update_cells(count: Spanned<MirExpr>) -> Spanned<MirExpr> {
        sp(MirExpr::RecordUpdate(Spanned::bare(MirRecordUpdate {
            type_id: Some(TypeId(0)),
            type_name: "State".to_string(),
            base: Box::new(local(0, false)),
            updates: vec![
                MirRecordField {
                    name: "cells".to_string(),
                    value: local(1, true),
                },
                MirRecordField {
                    name: "count".to_string(),
                    value: count,
                },
            ],
        })))
    }

    #[test]
    fn a_matched_vector_set_target_moves_past_a_none_arm_that_reads_the_record() {
        // match Vector.set(s.cells, 0, 1)
        //     Option.Some(updated) -> State.update(s, cells = updated, count = s.count)
        //     Option.None -> s
        let (body, target) = vector_set_match_body(
            update_cells(project(local(0, true), "count")),
            local(0, true),
        );
        let builtins = ["Vector.set".to_string()];
        assert!(movable_projections(&body.node, &builtins).contains(&target));
        // Without knowing the call is `Vector.set`, the None arm blocks it.
        assert!(!movable_projections(&body.node, &[]).contains(&target));
    }

    #[test]
    fn a_matched_vector_set_target_read_again_in_the_some_arm_does_not_move() {
        // Option.Some(updated) -> f(s.cells, updated)
        let (body, target) = vector_set_match_body(
            call(vec![project(local(0, true), "cells"), local(1, true)]),
            local(0, true),
        );
        let builtins = ["Vector.set".to_string()];
        assert!(!movable_projections(&body.node, &builtins).contains(&target));
    }
}
