//! Rust backend: functions that also return a borrow of what they found.
//!
//! A lookup returns a copy of a value it found inside a borrowed argument:
//! `cacheGet(cache, key)` clones the entry out of the map, and a caller that
//! only reads the result drops the copy again, one count up and one down per
//! call. Such a function is emitted a second time, as `__ret_<name>`, with
//! the same parameters and a result that borrows from the arguments it came
//! out of (its *sources*): `&'a T`, or `Option<&'a T>` for an `Option`. A
//! caller that binds the result and only reads it calls that version (see
//! `MirFnEmitPolicy::apply_lent_options`); every other call keeps the copy.
//!
//! A function qualifies when every value it can return, through the matches,
//! conditionals and `let`s of its tail, is borrowed from a param:
//! - a place: a borrowed param, a binding a match on a place binds, a field
//!   of a place, or a call of a qualifying function that returns `&T`;
//! - for an `Option`: `Option.None`, `Option.Some(place)`, `Map.get(place,
//!   key)`, a call of a qualifying function that returns `Option<&T>`, or a
//!   place of an `Option` type.
//!
//! A value built in the function (a constructor, a literal, the result of an
//! ordinary call) cannot be lent, so the function keeps one version.
//!
//! A self-tail loop qualifies only as a walk of a `List` param: the param is
//! read only as the subject of one list match, and each step passes the rest
//! of that match and leaves every other param as it is. Its borrowing
//! version walks the list with an iterator, whose elements borrow from the
//! list the caller lent; a loop that keeps its rest in a slot of its own (a
//! flat list) could not lend an element out of it. Other loops, the members
//! of a mutual tail-call group, the functions with a by-value and a lent
//! version, `main`, the guest entry and a function used as a value keep one
//! version.

use std::collections::{HashMap, HashSet};

use crate::ast::Spanned;
use crate::ast::Type;
use crate::codegen::{CodegenContext, RustLentResult};
use crate::ir::FnId;
use crate::ir::hir::BuiltinCtor;
use crate::ir::mir::{LocalId, MirCallee, MirCtor, MirExpr, MirFn, MirPattern};

use super::emit_ctx::should_borrow_param;

/// The functions with a borrowing version, with the params their result
/// borrows from; `used` marks the ones some call can reach. Each version a
/// call reaches is rendered once to check it: whether a match binds borrows
/// is the emitter's to decide (a match that clones its subject binds owned
/// locals), so one whose body does not render as borrows is left out and the
/// analysis runs again without it.
pub(super) fn compute_checked(ctx: &mut CodegenContext) {
    let mut excluded: HashSet<FnId> = HashSet::new();
    loop {
        ctx.rust_lent_results = compute(ctx, &excluded);
        let mut failed: Vec<FnId> = ctx
            .rust_lent_results
            .iter()
            .filter(|(_, lent)| lent.used)
            .map(|(id, _)| *id)
            .filter(|id| !super::from_mir::lent_body_renders(*id, ctx))
            .collect();
        if failed.is_empty() {
            return;
        }
        failed.sort();
        excluded.extend(failed);
    }
}

/// [`compute_checked`]'s analysis, leaving out the functions in `excluded`.
fn compute(ctx: &CodegenContext, excluded: &HashSet<FnId>) -> HashMap<FnId, RustLentResult> {
    let Some(program) = ctx.mir_program.as_ref() else {
        return HashMap::new();
    };
    let mut values = HashSet::new();
    for (_, mir_fn) in program.iter() {
        super::from_mir::collect_fn_values(&mir_fn.body.node, &mut values);
    }
    // Candidates, each with the result shape and, for a walk, its param.
    let mut candidates: Vec<(FnId, bool, Option<usize>)> = Vec::new();
    for (id, mir_fn) in program.iter() {
        let Some(resolved) = ctx.resolved_program.fn_by_id(*id) else {
            continue;
        };
        if excluded.contains(id)
            || values.contains(&mir_fn.name)
            || mir_fn.name == "main"
            || ctx.guest_entry.as_deref() == Some(resolved.name.as_str())
            || ctx.mutual_tco_members.contains(id)
            || ctx.rust_dual_fns.contains(id)
            || ctx.rust_word_spec.wrapped.contains(id)
            || !resolved.effects.is_empty()
            || resolved.params.len() != mir_fn.params.len()
            || ctx
                .bare_i64
                .for_fn(*id)
                .is_some_and(|facts| facts.bare_return)
        {
            continue;
        }
        let option = match &resolved.return_type {
            Type::Option(inner) if should_borrow_param(inner) => true,
            Type::Option(_) => continue,
            ty if should_borrow_param(ty) => false,
            _ => continue,
        };
        let walk = if super::toplevel::resolved_fn_has_self_tailcall(resolved) {
            match walk_param(mir_fn, resolved, ctx) {
                Some(index) => Some(index),
                None => continue,
            }
        } else {
            None
        };
        candidates.push((*id, option, walk));
    }
    candidates.sort_by_key(|(id, _, _)| *id);

    // A least fixpoint: a function joins once every value it returns is a
    // place of a param or comes from a function that already joined.
    let mut lent: HashMap<FnId, RustLentResult> = HashMap::new();
    loop {
        let mut grew = false;
        for &(id, option, walk) in &candidates {
            if lent.contains_key(&id) {
                continue;
            }
            let Some(mir_fn) = program.fn_by_id(id) else {
                continue;
            };
            let Some(resolved) = ctx.resolved_program.fn_by_id(id) else {
                continue;
            };
            let mask = super::expr::param_borrow_mask(id, resolved, ctx);
            let mut places: HashMap<LocalId, HashSet<usize>> = HashMap::new();
            for (i, param) in mir_fn.params.iter().enumerate() {
                let borrowed = mask.get(i).copied().unwrap_or(false)
                    && !super::lent_option::is_lent(&ctx.rust_lent_options, id, i)
                    && resolved
                        .params
                        .get(i)
                        .is_some_and(|(_, ty)| should_borrow_param(ty));
                if borrowed && Some(i) != walk {
                    places.insert(param.local, HashSet::from([i]));
                }
            }
            let shape = Shape {
                codegen: ctx,
                lent: &lent,
                builtins: &program.builtins,
                option,
                fn_id: id,
                walk: walk.map(|i| (i, mir_fn.params[i].local)),
            };
            let Some(sources) = shape.tail(&mir_fn.body, &places, &HashSet::new()) else {
                continue;
            };
            if sources.is_empty() {
                continue;
            }
            lent.insert(
                id,
                RustLentResult {
                    sources: (0..mir_fn.params.len())
                        .map(|i| sources.contains(&i))
                        .collect(),
                    option,
                    walk,
                    used: false,
                },
            );
            grew = true;
        }
        if !grew {
            break;
        }
    }
    mark_used(ctx, &mut lent);
    lent
}

/// The `List` param a self-tail loop walks (see the module docs): its only
/// read is the subject of one list match in tail position, each step passes
/// that match's rest there and every other param unchanged, and the list
/// holds values a borrow serves.
fn walk_param(
    mir_fn: &MirFn,
    resolved: &crate::ir::hir::ResolvedFnDef,
    ctx: &CodegenContext,
) -> Option<usize> {
    let mut found = None;
    for (i, (_, ty)) in resolved.params.iter().enumerate() {
        let Type::List(elem) = ty else {
            continue;
        };
        if !matches!(elem.as_ref(), Type::Named { .. } | Type::Tuple(_)) {
            continue;
        }
        if !super::from_mir::self_loop_lends(ctx, mir_fn.fn_id, i) {
            continue;
        }
        let slot = mir_fn.params[i].local;
        let mut matches = 0;
        if walks(&mir_fn.body.node, slot, i, mir_fn, true, &mut matches) && matches == 1 {
            if found.is_some() {
                return None;
            }
            found = Some(i);
        }
    }
    found
}

/// Whether the param in `slot` (position `index`) is walked in `expr`: read
/// only as the subject of a list match in tail position (counted in
/// `matches`), whose rest only feeds the self tail calls, which pass every
/// other param unchanged.
fn walks(
    expr: &MirExpr,
    slot: LocalId,
    index: usize,
    mir_fn: &MirFn,
    tail: bool,
    matches: &mut usize,
) -> bool {
    let is_slot = |e: &MirExpr| matches!(e, MirExpr::Local(local) if local.node.slot == slot);
    match expr {
        MirExpr::Local(local) => local.node.slot != slot,
        MirExpr::Match(m) if is_slot(&m.node.subject.node) => {
            *matches += 1;
            tail && m.node.arms.iter().all(|arm| match &arm.pattern {
                MirPattern::EmptyList | MirPattern::Wildcard => {
                    walks(&arm.body.node, slot, index, mir_fn, tail, matches)
                }
                MirPattern::Cons { tail: rest, .. } => {
                    super::from_mir::rebinds_only(&arm.body.node, *rest, mir_fn.fn_id, index)
                        && walks(&arm.body.node, slot, index, mir_fn, tail, matches)
                }
                _ => false,
            })
        }
        MirExpr::Match(m) => {
            walks(&m.node.subject.node, slot, index, mir_fn, false, matches)
                && m.node
                    .arms
                    .iter()
                    .all(|arm| walks(&arm.body.node, slot, index, mir_fn, tail, matches))
        }
        MirExpr::IfThenElse(branches) => {
            walks(
                &branches.node.cond.node,
                slot,
                index,
                mir_fn,
                false,
                matches,
            ) && walks(
                &branches.node.then_branch.node,
                slot,
                index,
                mir_fn,
                tail,
                matches,
            ) && walks(
                &branches.node.else_branch.node,
                slot,
                index,
                mir_fn,
                tail,
                matches,
            )
        }
        MirExpr::Let(binding) => {
            walks(
                &binding.node.value.node,
                slot,
                index,
                mir_fn,
                false,
                matches,
            ) && walks(&binding.node.body.node, slot, index, mir_fn, tail, matches)
        }
        MirExpr::TailCall(tc) if tc.node.target == mir_fn.fn_id => {
            tail && tc.node.args.iter().enumerate().all(|(i, arg)| {
                i == index
                    || matches!(&arg.node, MirExpr::Local(local)
                        if mir_fn.params.get(i).is_some_and(|p| p.local == local.node.slot))
            })
        }
        _ => {
            let mut ok = true;
            crate::ir::mir::expr::walk_children(expr, &mut |child| {
                ok = ok && walks(child, slot, index, mir_fn, false, matches);
            });
            ok
        }
    }
}

/// What the tail of one candidate may return (see the module docs).
struct Shape<'a> {
    codegen: &'a CodegenContext,
    lent: &'a HashMap<FnId, RustLentResult>,
    builtins: &'a [String],
    option: bool,
    fn_id: FnId,
    /// The walked `List` param, by position and slot.
    walk: Option<(usize, LocalId)>,
}

impl Shape<'_> {
    /// The params the values `expr` returns borrow from, or `None` when one
    /// of them is not a borrow. `places` maps each place local to the params
    /// it borrows from; `rests` holds the rests a walk step may pass.
    fn tail(
        &self,
        spanned: &Spanned<MirExpr>,
        places: &HashMap<LocalId, HashSet<usize>>,
        rests: &HashSet<LocalId>,
    ) -> Option<HashSet<usize>> {
        let expr = &spanned.node;
        match expr {
            MirExpr::Match(m) => {
                let subject = &m.node.subject.node;
                // The walk: the head borrows from the walked list.
                if let Some((index, slot)) = self.walk
                    && matches!(subject, MirExpr::Local(local) if local.node.slot == slot)
                {
                    let mut out = HashSet::new();
                    for arm in &m.node.arms {
                        let mut inner = places.clone();
                        let mut inner_rests = rests.clone();
                        if let MirPattern::Cons { head, tail, .. } = &arm.pattern {
                            inner.insert(*head, HashSet::from([index]));
                            inner_rests.insert(*tail);
                        }
                        out.extend(self.tail(&arm.body, &inner, &inner_rests)?);
                    }
                    return Some(out);
                }
                let roots = self.subject_roots(subject, places);
                let mut out = HashSet::new();
                for arm in &m.node.arms {
                    let mut inner = places.clone();
                    if let Some(roots) = &roots {
                        // The rest of a list match is not a list the match
                        // lends (`ListRest`); its head is.
                        let mut bound = Vec::new();
                        match &arm.pattern {
                            MirPattern::Cons { head, .. } => bound.push(*head),
                            pattern => {
                                let mut slots = Vec::new();
                                super::from_mir::pattern_binder_slots(pattern, &mut slots);
                                bound.extend(slots.into_iter().map(|(slot, _)| slot));
                            }
                        }
                        for slot in bound {
                            inner.insert(slot, roots.clone());
                        }
                    }
                    out.extend(self.tail(&arm.body, &inner, rests)?);
                }
                Some(out)
            }
            MirExpr::IfThenElse(branches) => {
                let mut out = self.tail(&branches.node.then_branch, places, rests)?;
                out.extend(self.tail(&branches.node.else_branch, places, rests)?);
                Some(out)
            }
            MirExpr::Let(binding) => {
                // A binding of a borrowing call is a place too: the borrowing
                // body binds the borrow (see `emit_lent_tail`).
                let mut inner = places.clone();
                inner.remove(&binding.node.binding);
                if let Some(roots) = self.place_call_roots(&binding.node.value.node, places) {
                    inner.insert(binding.node.binding, roots);
                }
                self.tail(&binding.node.body, &inner, rests)
            }
            MirExpr::TailCall(tc) if tc.node.target == self.fn_id => {
                let (index, _) = self.walk?;
                match tc.node.args.get(index).map(|arg| &arg.node) {
                    Some(MirExpr::Local(local)) if rests.contains(&local.node.slot) => {
                        Some(HashSet::new())
                    }
                    _ => None,
                }
            }
            MirExpr::Construct(construct) if self.option => match &construct.node.ctor {
                MirCtor::Builtin(BuiltinCtor::OptionNone) => Some(HashSet::new()),
                MirCtor::Builtin(BuiltinCtor::OptionSome) => {
                    self.place_roots(&construct.node.args.first()?.node, places)
                }
                _ => None,
            },
            _ if self.option => self.option_place_roots(spanned, places),
            _ => self.place_roots(expr, places),
        }
    }

    /// The params a match subject the emitter matches by reference borrows
    /// from: a place read where it lies (no call), or `Map.get` of one.
    fn subject_roots(
        &self,
        subject: &MirExpr,
        places: &HashMap<LocalId, HashSet<usize>>,
    ) -> Option<HashSet<usize>> {
        match subject {
            MirExpr::Local(local) => places.get(&local.node.slot).cloned(),
            MirExpr::Project(project) if self.in_place(project) => {
                self.subject_roots(&project.node.base.node, places)
            }
            MirExpr::Call(call) => match (self.builtin(&call.node.callee), &call.node.args[..]) {
                (Some("Map.get"), [map, _]) => self.subject_roots(&map.node, places),
                _ => None,
            },
            _ => None,
        }
    }

    /// The params a call of a qualifying function that returns `&T` borrows
    /// from.
    fn place_call_roots(
        &self,
        expr: &MirExpr,
        places: &HashMap<LocalId, HashSet<usize>>,
    ) -> Option<HashSet<usize>> {
        let (callee, args) = self.lent_call(expr)?;
        if callee.option {
            return None;
        }
        self.source_roots(callee, args, places)
    }

    /// The params a place borrows from (see the module docs).
    fn place_roots(
        &self,
        expr: &MirExpr,
        places: &HashMap<LocalId, HashSet<usize>>,
    ) -> Option<HashSet<usize>> {
        match expr {
            MirExpr::Local(local) => places.get(&local.node.slot).cloned(),
            MirExpr::Project(project) if self.in_place(project) => {
                self.place_roots(&project.node.base.node, places)
            }
            MirExpr::Call(_) | MirExpr::TailCall(_) => {
                let (callee, args) = self.lent_call(expr)?;
                if callee.option {
                    return None;
                }
                self.source_roots(callee, args, places)
            }
            _ => None,
        }
    }

    /// The params an `Option` of borrows borrows from (see the module docs).
    fn option_place_roots(
        &self,
        spanned: &Spanned<MirExpr>,
        places: &HashMap<LocalId, HashSet<usize>>,
    ) -> Option<HashSet<usize>> {
        let expr = &spanned.node;
        if let MirExpr::Call(call) = expr
            && let (Some("Map.get"), [map, _]) =
                (self.builtin(&call.node.callee), &call.node.args[..])
        {
            return self.place_roots(&map.node, places);
        }
        if let Some((callee, args)) = self.lent_call(expr)
            && callee.option
        {
            return self.source_roots(callee, args, places);
        }
        match expr {
            MirExpr::Local(_) | MirExpr::Project(_) if matches!(spanned.ty(), Some(Type::Option(inner)) if should_borrow_param(inner)) => {
                self.place_roots(expr, places)
            }
            _ => None,
        }
    }

    /// The params a call of a qualifying function borrows from through the
    /// args at its sources, each of which must be a place.
    fn source_roots(
        &self,
        callee: &RustLentResult,
        args: &[Spanned<MirExpr>],
        places: &HashMap<LocalId, HashSet<usize>>,
    ) -> Option<HashSet<usize>> {
        let mut out = HashSet::new();
        for (i, arg) in args.iter().enumerate() {
            if callee.sources.get(i).copied().unwrap_or(false) {
                out.extend(self.place_roots(&arg.node, places)?);
            }
        }
        Some(out)
    }

    /// A direct call of a function that already qualified, and its args.
    fn lent_call<'e>(
        &self,
        expr: &'e MirExpr,
    ) -> Option<(&RustLentResult, &'e [Spanned<MirExpr>])> {
        lent_call(expr, self.lent)
    }

    /// Whether a field read renders as the field where it lies.
    fn in_place(&self, project: &Spanned<crate::ir::mir::MirProject>) -> bool {
        super::from_mir::field_read_in_place(project.node.base.ty(), self.codegen)
    }

    fn builtin(&self, callee: &MirCallee) -> Option<&str> {
        match callee {
            MirCallee::Builtin(id) => self.builtins.get(id.0 as usize).map(String::as_str),
            _ => None,
        }
    }
}

/// A direct, eager call of a function in `lent` other than a self tail call,
/// and its args.
pub(super) fn lent_call<'l, 'e>(
    expr: &'e MirExpr,
    lent: &'l HashMap<FnId, RustLentResult>,
) -> Option<(&'l RustLentResult, &'e [Spanned<MirExpr>])> {
    let (id, args) = match expr {
        MirExpr::Call(call) if !call.lazy.is_deferred() => match &call.node.callee {
            MirCallee::Fn(id) => (*id, &call.node.args[..]),
            _ => return None,
        },
        MirExpr::TailCall(tc) => (tc.node.target, &tc.node.args[..]),
        _ => return None,
    };
    Some((lent.get(&id)?, args))
}

/// Mark the functions some call can reach through their borrowing version:
/// a `let` that binds a call of one (see `lent_lookups` in `from_mir`), and
/// then every qualifying function the borrowing body of a marked one calls
/// for its result. A mark is a superset of the calls the emitter makes,
/// which also asks that nothing moves the borrowed args.
fn mark_used(ctx: &CodegenContext, lent: &mut HashMap<FnId, RustLentResult>) {
    let Some(program) = ctx.mir_program.as_ref() else {
        return;
    };
    let mut used: HashSet<FnId> = HashSet::new();
    for (_, mir_fn) in program.iter() {
        bound_calls(&mir_fn.body.node, lent, &mut used);
    }
    let mut work: Vec<FnId> = used.iter().copied().collect();
    while let Some(id) = work.pop() {
        let Some(mir_fn) = program.fn_by_id(id) else {
            continue;
        };
        let mut reached = HashSet::new();
        called_for_result(&mir_fn.body.node, lent, &mut reached);
        for callee in reached {
            if used.insert(callee) {
                work.push(callee);
            }
        }
    }
    for id in used {
        if let Some(entry) = lent.get_mut(&id) {
            entry.used = true;
        }
    }
}

/// The qualifying functions whose call a `let` in `expr` binds, alone or as
/// the map of a `Map.get`, or whose result a field is read out of.
fn bound_calls(expr: &MirExpr, lent: &HashMap<FnId, RustLentResult>, out: &mut HashSet<FnId>) {
    if let MirExpr::Let(binding) = expr {
        called_for_result(&binding.node.value.node, lent, out);
    }
    // A field read straight out of a call (`f(x).field`).
    if let MirExpr::Project(project) = expr {
        called_for_result(&project.node.base.node, lent, out);
    }
    crate::ir::mir::expr::walk_children(expr, &mut |child| bound_calls(child, lent, out));
}

/// Every qualifying function `expr` calls, directly or as the source arg or
/// map of another such call; a superset of the borrowing calls emitted.
fn called_for_result(
    expr: &MirExpr,
    lent: &HashMap<FnId, RustLentResult>,
    out: &mut HashSet<FnId>,
) {
    let id = match expr {
        MirExpr::Call(call) => match &call.node.callee {
            MirCallee::Fn(id) => Some(*id),
            _ => None,
        },
        MirExpr::TailCall(tc) => Some(tc.node.target),
        _ => None,
    };
    if let Some(id) = id
        && lent.contains_key(&id)
    {
        out.insert(id);
    }
    crate::ir::mir::expr::walk_children(expr, &mut |child| called_for_result(child, lent, out));
}

/// The name of the borrowing version of a function.
pub(super) fn ret_name(rust_name: &str) -> String {
    format!("__ret_{rust_name}")
}
