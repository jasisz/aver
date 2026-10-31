//! Which boxed records (see `representation::is_boxed_record`) the program
//! may copy, decided once per program from its MIR before any type is
//! emitted (see `representation::is_counted_record`).
//!
//! A boxed record that is never copied stays a `Box`: it is moved from
//! owner to owner and written in place, and a count would only cost a check
//! at every update. A record that may be copied is kept behind a counted
//! pointer, so a copy is one more owner instead of a copy of every field.
//!
//! A value is copied where the Rust emitter clones it, and the emitter
//! clones a value of a type that holds the record (the record itself, or an
//! `Option`, `Result`, tuple, record or constructor holding it) when it puts
//! one in a position that owns it and cannot move it there: a local read
//! before its last use, a borrowed parameter, a field read that does not
//! move, an item read out of a list or map (a list's head in a match, a
//! built-in's result) or a value taken apart by a match on something it does
//! not own. The walk below names those places from the MIR and the facts the
//! emitter decides moves by (last uses, the params taken by value, the field
//! moves); where it cannot tell, it counts a copy. A wrong answer costs time
//! only: a `Box` clone copies the fields, which is what a copy means.
//!
//! A record the program copies still stays a `Box` when a count would make
//! it worse somewhere: where a field read moves a field out of the record
//! (a `Box` gives its fields away, a counted pointer only lends them) and no
//! update binds the record as a `Box` of its own around that read (see
//! `from_mir::emit_counted_record_update`), the count would turn the move
//! into a copy of the field, a whole map, say.

use std::collections::{HashMap, HashSet};

use crate::ast::TypeDef;
use crate::codegen::CodegenContext;
use crate::ir::FnId;
use crate::ir::mir::{LocalId, MirCallee, MirExpr, MirFn, MirPattern, MirStrPart};
use crate::types::{Type, parse_type_str};

use super::representation::is_boxed_record;

/// The backend type keys of the boxed records the program may copy.
pub(super) fn compute(ctx: &CodegenContext) -> HashSet<String> {
    let mut out = HashSet::new();
    let Some(program) = ctx.mir_program.as_ref() else {
        return out;
    };
    let any_boxed = ctx
        .type_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
        .any(|td| is_boxed_record(td, ctx));
    if !any_boxed {
        return out;
    }
    let mut owned_params = HashMap::new();
    let mut lost = HashSet::new();
    for (id, f) in program.iter() {
        let movable =
            crate::ir::mir::field_moves::movable_projections(&f.body.node, &program.builtins);
        let mut walk = Walk {
            ctx,
            f,
            fn_id: *id,
            builtins: &program.builtins,
            moved_paths: crate::ir::mir::field_moves::moved_paths(&f.body.node, &movable),
            movable,
            owned_params: &mut owned_params,
            loop_carried: HashSet::new(),
            slot_types: slot_types(f, *id, ctx),
            bound: Vec::new(),
            out: &mut out,
            lost: &mut lost,
        };
        walk.expr(&f.body, Pos::Owned);
    }
    // Never worse than a `Box`: a record whose count would turn a field
    // the program moves out of it into a copy stays a `Box`.
    out.retain(|key| !lost.contains(key));
    out
}

/// Whether a position owns the value put there (a copy unless it moves) or
/// only reads it.
#[derive(Clone, Copy, PartialEq)]
enum Pos {
    Owned,
    Read,
}

struct Walk<'a> {
    ctx: &'a CodegenContext,
    f: &'a MirFn,
    fn_id: FnId,
    builtins: &'a [String],
    movable: HashSet<usize>,
    owned_params: &'a mut HashMap<FnId, Vec<bool>>,
    /// The params a self tail call hands on unchanged while it computes its
    /// other arguments: the emitter clones them there.
    loop_carried: HashSet<LocalId>,
    /// The type of each local the function binds, where the MIR knows it.
    slot_types: HashMap<LocalId, Type>,
    /// The field moves of the body (see `field_moves::moved_paths`).
    moved_paths: Vec<(usize, LocalId, Vec<String>)>,
    /// The places (a local and a path of fields) an enclosing update of a
    /// boxed record binds as a `Box` of its own while its new values run
    /// (see `from_mir::emit_counted_record_update`): a field moves out of
    /// them even when the record is counted.
    bound: Vec<(LocalId, Vec<String>)>,
    out: &'a mut HashSet<String>,
    /// The boxed records a field read moves out of where a counted pointer
    /// would make it a copy.
    lost: &'a mut HashSet<String>,
}

type Expr = crate::ast::Spanned<MirExpr>;

impl Walk<'_> {
    /// Count a copy of every boxed record a value of `ty` holds.
    fn copy(&mut self, ty: Option<&Type>, through_collections: bool) {
        // A value whose type the MIR does not carry (a binding a pass made)
        // counts no copy: the answer is a `Box`, which copies correctly.
        let Some(ty) = ty else {
            return;
        };
        holds(
            ty,
            self.ctx,
            None,
            through_collections,
            &mut HashSet::new(),
            self.out,
        );
    }

    /// The place an update of a boxed record binds as a `Box` of its own
    /// for its new values, as the emitter does for a counted record: a base
    /// that moves, held by an owned local, that no value reads whole and
    /// that did not move out before (unless an owned param holds it).
    fn binds_base(
        &mut self,
        upd: &crate::ir::mir::MirRecordUpdate,
        local_moves: bool,
    ) -> Option<(LocalId, Vec<String>)> {
        let td =
            super::representation::named_type_def(upd.type_id, &upd.type_name, self.ctx, None)?;
        if !is_boxed_record(td, self.ctx) {
            return None;
        }
        let (root, _, path) = crate::ir::mir::field_moves::projection_root(&upd.base.node)?;
        let moves = match &upd.base.node {
            MirExpr::Local(_) => local_moves,
            _ => {
                self.movable
                    .contains(&(&upd.base.node as *const MirExpr as usize))
                    && self.local_owned(root)
                    && !self.loop_carried.contains(&root)
            }
        };
        if !moves {
            return None;
        }
        let in_param = self.f.params.iter().any(|p| p.local == root);
        let mut inside = HashSet::from([&upd.base.node as *const MirExpr as usize]);
        fn addresses(expr: &MirExpr, out: &mut HashSet<usize>) {
            out.insert(expr as *const MirExpr as usize);
            crate::ir::mir::expr::walk_children(expr, &mut |child| addresses(child, out));
        }
        for field in &upd.updates {
            addresses(&field.value.node, &mut inside);
        }
        let moved_before = !in_param
            && self.moved_paths.iter().any(|(addr, slot, moved)| {
                *slot == root && moved.as_slice() == path.as_slice() && !inside.contains(addr)
            });
        let beside = upd
            .updates
            .iter()
            .any(|field| super::from_mir::reads_record_holder(&field.value.node, root, &path));
        (!moved_before && !beside).then_some((root, path))
    }

    /// Note every boxed record a moving field read `e` passes through
    /// outside a place an update binds as a `Box`: a counted pointer there
    /// would copy the field instead.
    fn moves_through(&mut self, e: &Expr) {
        if !e
            .ty()
            .is_some_and(|ty| holds_collection(ty, self.ctx, None, &mut HashSet::new()))
        {
            // A copy of a number, a string or a node of a sum only counts
            // one more owner, as a move would hand one over.
            return;
        }
        let mut cur = e;
        while let MirExpr::Project(p) = &cur.node {
            let base = &*p.node.base;
            let bound = crate::ir::mir::field_moves::projection_root(&base.node).is_some_and(
                |(root, _, path)| self.bound.iter().any(|(r, p)| *r == root && *p == path),
            );
            if !bound
                && let Some(Type::Named { id, name }) = base.ty()
                && let Some(td) = super::representation::named_type_def(*id, name, self.ctx, None)
                && is_boxed_record(td, self.ctx)
            {
                self.lost
                    .insert(crate::codegen::common::backend_type_def_key(self.ctx, td));
            }
            cur = base;
        }
    }

    /// Whether the param `index` of `id` is taken by value.
    fn param_owned(&mut self, id: FnId, index: usize) -> bool {
        if let Some(owned) = self.owned_params.get(&id) {
            return owned.get(index).copied().unwrap_or(false);
        }
        let owned = params_owned(id, self.ctx);
        let answer = owned.get(index).copied().unwrap_or(false);
        self.owned_params.insert(id, owned);
        answer
    }

    /// Whether the local `slot` of this function is an owned Rust value.
    fn local_owned(&mut self, slot: LocalId) -> bool {
        match self.f.params.iter().position(|p| p.local == slot) {
            Some(index) => self.param_owned(self.fn_id, index),
            None => true,
        }
    }

    fn expr(&mut self, e: &Expr, pos: Pos) {
        match &e.node {
            MirExpr::Literal(_) | MirExpr::FnValue(_) => {}
            MirExpr::Local(local) => {
                let local = &local.node;
                if pos == Pos::Owned
                    && (!local.last_use
                        || self.loop_carried.contains(&local.slot)
                        || !self.local_owned(local.slot))
                {
                    let ty = e.ty().or(self.slot_types.get(&local.slot)).cloned();
                    self.copy(ty.as_ref(), false);
                }
            }
            MirExpr::Let(l) => {
                self.expr(&l.node.value, Pos::Owned);
                self.expr(&l.node.body, pos);
            }
            MirExpr::Call(call) => {
                match call.node.callee {
                    MirCallee::Fn(id) => {
                        for (i, arg) in call.node.args.iter().enumerate() {
                            let owned = self.param_owned(id, i);
                            self.expr(arg, if owned { Pos::Owned } else { Pos::Read });
                        }
                    }
                    MirCallee::Builtin(_) => {
                        let reads_first =
                            super::from_mir::builtin_only_reads(&call.node.callee, self.builtins);
                        for (i, arg) in call.node.args.iter().enumerate() {
                            let pos = match i == 0 && reads_first {
                                true => Pos::Read,
                                false => Pos::Owned,
                            };
                            self.expr(arg, pos);
                        }
                        // A built-in's result may be an item of a collection
                        // it read (`Map.get`, `List.head`).
                        self.copy(e.ty(), false);
                    }
                    // The string building of an interpolation reads the
                    // values it shows.
                    _ => {
                        for arg in &call.node.args {
                            self.expr(arg, Pos::Read);
                        }
                    }
                }
            }
            MirExpr::TailCall(tc) => {
                let target = tc.node.target;
                let carried: HashSet<LocalId> = match target == self.fn_id {
                    true => tc
                        .node
                        .args
                        .iter()
                        .zip(&self.f.params)
                        .filter(|(arg, p)| {
                            matches!(&arg.node, MirExpr::Local(l) if l.node.slot == p.local)
                        })
                        .map(|(_, p)| p.local)
                        .collect(),
                    false => HashSet::new(),
                };
                let before = std::mem::replace(&mut self.loop_carried, carried.clone());
                for (i, arg) in tc.node.args.iter().enumerate() {
                    if let MirExpr::Local(l) = &arg.node
                        && carried.contains(&l.node.slot)
                    {
                        continue;
                    }
                    let owned = self.param_owned(target, i);
                    self.expr(arg, if owned { Pos::Owned } else { Pos::Read });
                }
                self.loop_carried = before;
            }
            MirExpr::BinOp(b) => {
                self.expr(&b.node.lhs, Pos::Read);
                self.expr(&b.node.rhs, Pos::Read);
            }
            MirExpr::Neg(inner) | MirExpr::Box(inner) | MirExpr::Unbox(inner) => {
                self.expr(inner, Pos::Read)
            }
            MirExpr::IntWord(w) => {
                for leaf in w.node.leaves() {
                    self.expr(leaf, Pos::Read);
                }
            }
            MirExpr::Try(inner) => self.expr(inner, pos),
            MirExpr::Return(inner) => self.expr(inner, Pos::Owned),
            MirExpr::Match(m) => {
                let binds = m.node.arms.iter().any(|arm| binds(&arm.pattern));
                for arm in &m.node.arms {
                    if matches!(arm.pattern, MirPattern::Cons { .. })
                        && let Some(Type::List(item)) = m.node.subject.ty()
                    {
                        // A list's head is read out of its node.
                        self.copy(Some(item), false);
                    }
                }
                self.expr(&m.node.subject, if binds { Pos::Owned } else { Pos::Read });
                for arm in &m.node.arms {
                    self.expr(&arm.body, pos);
                }
            }
            MirExpr::Construct(c) => {
                for arg in &c.node.args {
                    self.expr(arg, Pos::Owned);
                }
            }
            MirExpr::RecordCreate(r) => {
                for field in &r.node.fields {
                    self.expr(&field.value, Pos::Owned);
                }
            }
            MirExpr::RecordUpdate(u) => {
                let upd = &u.node;
                let moves = match &upd.base.node {
                    MirExpr::Local(base) => {
                        let base = &base.node;
                        self.local_owned(base.slot)
                            && !self.loop_carried.contains(&base.slot)
                            && (crate::ir::mir::field_moves::update_base_is_final(upd)
                                || values_end_base(upd, base.slot, &self.movable))
                    }
                    _ => false,
                };
                if !moves {
                    self.expr(&upd.base, Pos::Owned);
                }
                let bound = self.binds_base(upd, moves);
                if let Some(place) = &bound {
                    self.bound.push(place.clone());
                }
                for field in &upd.updates {
                    self.expr(&field.value, Pos::Owned);
                }
                if bound.is_some() {
                    self.bound.pop();
                }
            }
            MirExpr::Project(p) => {
                if pos == Pos::Owned {
                    let moves = self.movable.contains(&(&e.node as *const MirExpr as usize))
                        && crate::ir::mir::field_moves::projection_root(&e.node).is_some_and(
                            |(root, _, _)| {
                                self.local_owned(root) && !self.loop_carried.contains(&root)
                            },
                        );
                    if !moves {
                        self.copy(e.ty(), false);
                    }
                }
                if pos == Pos::Owned
                    && self.movable.contains(&(&e.node as *const MirExpr as usize))
                    && crate::ir::mir::field_moves::projection_root(&e.node)
                        .is_some_and(|(root, _, _)| self.local_owned(root))
                {
                    self.moves_through(e);
                }
                self.expr(&p.node.base, Pos::Read);
            }
            MirExpr::IfThenElse(ite) => {
                self.expr(&ite.node.cond, Pos::Read);
                self.expr(&ite.node.then_branch, pos);
                self.expr(&ite.node.else_branch, pos);
            }
            MirExpr::List(items) | MirExpr::Tuple(items) => {
                for item in items {
                    self.expr(item, Pos::Owned);
                }
            }
            MirExpr::MapLiteral(pairs) => {
                for (k, v) in pairs {
                    self.expr(k, Pos::Owned);
                    self.expr(v, Pos::Owned);
                }
            }
            MirExpr::InterpolatedStr(parts) => {
                for part in parts {
                    if let MirStrPart::Expr(e) = part {
                        self.expr(e, Pos::Read);
                    }
                }
            }
            MirExpr::IndependentProduct(ip) => {
                for item in &ip.node.items {
                    self.expr(item, Pos::Owned);
                }
            }
        }
    }
}

/// The types of the params and of the locals `f` binds with `let` or in a
/// pattern, where the MIR knows them.
fn slot_types(f: &MirFn, id: FnId, ctx: &CodegenContext) -> HashMap<LocalId, Type> {
    let mut types = HashMap::new();
    if let Some(resolved) = ctx.resolved_program.fn_by_id(id) {
        for (param, (_, ty)) in f.params.iter().zip(&resolved.params) {
            types.insert(param.local, ty.clone());
        }
    }
    fn walk(e: &Expr, ctx: &CodegenContext, types: &mut HashMap<LocalId, Type>) {
        match &e.node {
            MirExpr::Let(l) => {
                let ty = l
                    .node
                    .value
                    .ty()
                    .cloned()
                    .or_else(|| match &l.node.value.node {
                        MirExpr::Local(local) => types.get(&local.node.slot).cloned(),
                        _ => None,
                    });
                if let Some(ty) = ty {
                    types.insert(l.node.binding, ty);
                }
            }
            MirExpr::Match(m) => {
                let subject = m
                    .node
                    .subject
                    .ty()
                    .cloned()
                    .or_else(|| match &m.node.subject.node {
                        MirExpr::Local(local) => types.get(&local.node.slot).cloned(),
                        _ => None,
                    });
                if let Some(subject) = subject {
                    for arm in &m.node.arms {
                        bind(&arm.pattern, &subject, ctx, types);
                    }
                }
            }
            _ => {}
        }
        let children: Vec<&Expr> = spanned_children(e);
        for child in children {
            walk(child, ctx, types);
        }
    }
    walk(&f.body, ctx, &mut types);
    types
}

/// The types of the locals `pattern` binds in a value of type `ty`.
fn bind(pattern: &MirPattern, ty: &Type, ctx: &CodegenContext, types: &mut HashMap<LocalId, Type>) {
    match (pattern, ty) {
        (MirPattern::Bind(slot, _), _) => {
            types.insert(*slot, ty.clone());
        }
        (MirPattern::Cons { head, tail, .. }, Type::List(item)) => {
            types.insert(*head, (**item).clone());
            types.insert(*tail, ty.clone());
        }
        (MirPattern::Tuple(items), Type::Tuple(tys)) => {
            for (item, ty) in items.iter().zip(tys) {
                bind(item, ty, ctx, types);
            }
        }
        (MirPattern::Ctor { ctor, bindings, .. }, _) => {
            let fields: Vec<Type> = match (ctor, ty) {
                (crate::ir::mir::MirCtor::Builtin(_), Type::Option(inner)) => {
                    vec![(**inner).clone()]
                }
                (crate::ir::mir::MirCtor::Builtin(b), Type::Result(ok, err)) => match b {
                    crate::ir::hir::BuiltinCtor::ResultErr => vec![(**err).clone()],
                    _ => vec![(**ok).clone()],
                },
                (crate::ir::mir::MirCtor::User(id), _) => {
                    let entry = ctx.symbol_table.ctor_entry(*id);
                    match super::representation::named_type_def(
                        Some(entry.owning_type),
                        "",
                        ctx,
                        None,
                    ) {
                        Some(TypeDef::Sum { variants, .. }) => variants
                            .iter()
                            .find(|v| v.name == entry.name)
                            .map(|v| v.fields.iter().map(|f| parse_type_str(f)).collect())
                            .unwrap_or_default(),
                        _ => Vec::new(),
                    }
                }
                _ => Vec::new(),
            };
            for (slot, ty) in bindings.iter().zip(fields) {
                types.insert(*slot, ty);
            }
        }
        _ => {}
    }
}

/// The direct sub-expressions of `e`, as `Spanned` nodes.
fn spanned_children(e: &Expr) -> Vec<&Expr> {
    match &e.node {
        MirExpr::Literal(_) | MirExpr::Local(_) | MirExpr::FnValue(_) => Vec::new(),
        MirExpr::Let(l) => vec![&*l.node.value, &*l.node.body],
        MirExpr::Call(c) => c.node.args.iter().collect(),
        MirExpr::TailCall(tc) => tc.node.args.iter().collect(),
        MirExpr::BinOp(b) => vec![&*b.node.lhs, &*b.node.rhs],
        MirExpr::Neg(inner)
        | MirExpr::Try(inner)
        | MirExpr::Return(inner)
        | MirExpr::Box(inner)
        | MirExpr::Unbox(inner) => vec![&**inner],
        MirExpr::IntWord(w) => w.node.leaves(),
        MirExpr::Match(m) => std::iter::once(&*m.node.subject)
            .chain(m.node.arms.iter().map(|arm| &arm.body))
            .collect(),
        MirExpr::Construct(c) => c.node.args.iter().collect(),
        MirExpr::RecordCreate(r) => r.node.fields.iter().map(|f| &f.value).collect(),
        MirExpr::RecordUpdate(u) => std::iter::once(&*u.node.base)
            .chain(u.node.updates.iter().map(|f| &f.value))
            .collect(),
        MirExpr::Project(p) => vec![&*p.node.base],
        MirExpr::IfThenElse(ite) => vec![
            &*ite.node.cond,
            &*ite.node.then_branch,
            &*ite.node.else_branch,
        ],
        MirExpr::List(items) | MirExpr::Tuple(items) => items.iter().collect(),
        MirExpr::MapLiteral(pairs) => pairs.iter().flat_map(|(k, v)| [k, v]).collect(),
        MirExpr::InterpolatedStr(parts) => parts
            .iter()
            .filter_map(|p| match p {
                MirStrPart::Expr(e) => Some(e),
                MirStrPart::Literal(_) => None,
            })
            .collect(),
        MirExpr::IndependentProduct(ip) => ip.node.items.iter().collect(),
    }
}

/// Whether a match arm's pattern binds a part of the subject.
fn binds(pattern: &MirPattern) -> bool {
    match pattern {
        MirPattern::Wildcard | MirPattern::Literal(_) | MirPattern::EmptyList => false,
        MirPattern::Bind(..) | MirPattern::Cons { .. } => true,
        MirPattern::Tuple(items) => items.iter().any(binds),
        MirPattern::Ctor { bindings, .. } => !bindings.is_empty(),
    }
}

/// Whether the base `slot` of an update is read for the last time inside its
/// new values, only through fields, and no read there that may move takes a
/// field the update keeps: the emitter then moves the base after the values
/// run instead of copying it (see `from_mir::values_end_base`).
fn values_end_base(
    upd: &crate::ir::mir::MirRecordUpdate,
    slot: LocalId,
    movable: &HashSet<usize>,
) -> bool {
    let replaced: Vec<&str> = upd.updates.iter().map(|f| f.name.as_str()).collect();
    fn walk(
        expr: &MirExpr,
        slot: LocalId,
        replaced: &[&str],
        movable: &HashSet<usize>,
        last: &mut bool,
        bad: &mut bool,
    ) {
        if let Some((root, last_use, path)) = crate::ir::mir::field_moves::projection_root(expr)
            && root == slot
        {
            *last = *last || last_use;
            let moves = movable.contains(&(expr as *const MirExpr as usize));
            let keeps = path.first().is_none_or(|f| !replaced.contains(&f.as_str()));
            *bad = *bad || path.is_empty() || (moves && keeps);
            return;
        }
        crate::ir::mir::expr::walk_children(expr, &mut |child| {
            walk(child, slot, replaced, movable, last, bad)
        });
    }
    let (mut last, mut bad) = (false, false);
    for field in &upd.updates {
        walk(
            &field.value.node,
            slot,
            &replaced,
            movable,
            &mut last,
            &mut bad,
        );
    }
    last && !bad
}

/// Which params of `id` the Rust backend takes by value, as the emitter
/// decides it (`from_mir::MirFnEmitPolicy::apply_own_param`); a param it
/// cannot tell about counts as borrowed.
fn params_owned(id: FnId, ctx: &CodegenContext) -> Vec<bool> {
    let (Some(program), Some(resolved)) =
        (ctx.mir_program.as_ref(), ctx.resolved_program.fn_by_id(id))
    else {
        return Vec::new();
    };
    let Some(mir_fn) = program.iter().find(|(fid, _)| **fid == id).map(|(_, f)| f) else {
        return Vec::new();
    };
    let consumed = ctx.rust_owned_record_params.get(&id);
    resolved
        .params
        .iter()
        .enumerate()
        .map(|(i, (_, ty))| {
            !super::emit_ctx::should_borrow_param(ty)
                || consumed.and_then(|c| c.get(i)).copied() == Some(true)
                || super::from_mir::param_handed_on(ty, mir_fn, i, consumed)
                || super::from_mir::collection_graduates(ty, mir_fn, i, ctx)
        })
        .collect()
}

/// Add to `out` every boxed record a value of `ty` holds in itself: through
/// `Option`, `Result`, tuples and the fields of records and constructors,
/// and through collections when `through_collections`.
fn holds(
    ty: &Type,
    ctx: &CodegenContext,
    scope: Option<&str>,
    through_collections: bool,
    seen: &mut HashSet<String>,
    out: &mut HashSet<String>,
) {
    macro_rules! go {
        ($ty:expr) => {
            holds($ty, ctx, scope, through_collections, seen, out)
        };
    }
    match ty {
        Type::Option(inner) => go!(inner),
        Type::Result(ok, err) => {
            go!(ok);
            go!(err);
        }
        Type::Tuple(items) => {
            for item in items {
                go!(item);
            }
        }
        Type::List(inner) | Type::Vector(inner) if through_collections => go!(inner),
        Type::Map(k, v) if through_collections => {
            go!(k);
            go!(v);
        }
        Type::Named { id, name } => {
            let Some(td) = super::representation::named_type_def(*id, name, ctx, scope) else {
                return;
            };
            let key = crate::codegen::common::backend_type_def_key(ctx, td);
            if !seen.insert(key.clone()) {
                return;
            }
            if is_boxed_record(td, ctx) {
                out.insert(key);
            }
            let decl = crate::codegen::common::type_key_for_decl(ctx, td);
            let fields: Vec<&String> = match td {
                TypeDef::Product { fields, .. } => fields.iter().map(|(_, f)| f).collect(),
                TypeDef::Sum { variants, .. } => {
                    variants.iter().flat_map(|v| v.fields.iter()).collect()
                }
            };
            for field in fields {
                holds(
                    &parse_type_str(field),
                    ctx,
                    decl.scope_str(),
                    through_collections,
                    seen,
                    out,
                );
            }
        }
        _ => {}
    }
}

/// Whether a value of `ty` holds a `List`, `Vector` or `Map`, directly or
/// through `Option`, `Result`, tuples and the fields of records and
/// constructors: a copy of it, unlike a move, leaves the collection two
/// owners, and the next write in place copies it whole.
fn holds_collection(
    ty: &Type,
    ctx: &CodegenContext,
    scope: Option<&str>,
    seen: &mut HashSet<String>,
) -> bool {
    match ty {
        Type::List(_) | Type::Vector(_) | Type::Map(..) => true,
        Type::Option(inner) => holds_collection(inner, ctx, scope, seen),
        Type::Result(ok, err) => {
            holds_collection(ok, ctx, scope, seen) || holds_collection(err, ctx, scope, seen)
        }
        Type::Tuple(items) => items.iter().any(|t| holds_collection(t, ctx, scope, seen)),
        Type::Named { id, name } => {
            let Some(td) = super::representation::named_type_def(*id, name, ctx, scope) else {
                return true;
            };
            if !seen.insert(crate::codegen::common::backend_type_def_key(ctx, td)) {
                return false;
            }
            let decl = crate::codegen::common::type_key_for_decl(ctx, td);
            let fields: Vec<&String> = match td {
                TypeDef::Product { fields, .. } => fields.iter().map(|(_, f)| f).collect(),
                TypeDef::Sum { variants, .. } => {
                    variants.iter().flat_map(|v| v.fields.iter()).collect()
                }
            };
            fields
                .into_iter()
                .any(|f| holds_collection(&parse_type_str(f), ctx, decl.scope_str(), seen))
        }
        _ => false,
    }
}
