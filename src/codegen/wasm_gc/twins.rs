//! A twin of a function that hands its result back unpacked and takes its
//! `Int` params as machine words.
//!
//! On wasm-gc every `Int` is an `$AverInt` object, and an `Option` or a
//! `Result` is a struct. A function that computes its result on
//! a machine word, or builds the struct only to return it, allocates once per
//! call, and a caller that takes the result apart at once throws the object
//! away; an `Int` argument the caller boxes and the callee reads back as a
//! machine word costs the same. Binaryen cannot remove that unless it inlines
//! the function, because it does not change the type of a result and does
//! not know what `$AverInt` means.
//!
//! Such a function gets a twin with the same body:
//!
//! - an `Int` result is `(i64, ref null $AverInt)`, the ref non-null only for
//!   a Big value; an `Option` or `Result` result is its fields;
//! - an `Int` param the body reads as a machine word is an `i64` plus a ref
//!   after the other params, null unless the value comes packed.
//!
//! The function itself becomes a small wrapper that calls the twin and packs
//! the result, so its signature is unchanged for every caller that wants the
//! packed value, a function value included. A caller that takes the result
//! apart at once, or has an `Int` argument on a machine word, calls the twin;
//! a fn no caller would call so gets none. A twin's tail calls go to the
//! twins of their targets, so a tail group keeps running without the stack
//! growing. A `?`, `?!` or early `return` in a twin returns its result
//! unpacked too, and a `?` on a twin's `Result` takes it apart unpacked.
//!
//! A `--certify` build keeps the bytes of every function the certificate plan
//! printer may print: such a function gets no twin
//! ([`crate::ir::cert_shape::printer_may_admit`], the superset the
//! fabricating passes use), and one it prints calls none.

use std::collections::{HashMap, HashSet};

use wasm_encoder::{HeapType, ValType};

use super::WasmGcError;
use super::types::{TypeRegistry, aver_to_wasm};
use crate::ast::FnDef;
use crate::ir::FnId;
use crate::ir::hir::ResolvedFnDef;
use crate::ir::mir::{MirCallee, MirCtor, MirExpr, MirProgram};

/// What a twin returns instead of the packed result.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum TwinKind {
    /// The packed result, as the fn returns it: the twin is there for its
    /// params.
    Plain,
    /// `(i64, ref null $AverInt)`.
    Int,
    /// The fields of struct `struct_idx`.
    Fields {
        struct_idx: u32,
        fields: Vec<ValType>,
    },
}

/// A function's twin as its callers see it.
#[derive(Clone, Debug)]
pub(crate) struct TwinEntry {
    pub(crate) wasm_idx: u32,
    pub(crate) kind: TwinKind,
    /// Per param: an `Int` the twin takes unpacked, as an `i64` and, after
    /// the other params, a ref that is null when the value is the `i64` and
    /// is the packed value otherwise.
    pub(crate) unpacked: Vec<bool>,
}

/// What `plan` gives a fn.
#[derive(Clone, Debug)]
pub(crate) struct TwinPlan {
    pub(crate) kind: TwinKind,
    pub(crate) unpacked: Vec<bool>,
}

impl TwinKind {
    /// The twin's results; `plain` is the fn's own.
    pub(crate) fn results(&self, registry: &TypeRegistry, plain: &[ValType]) -> Vec<ValType> {
        match self {
            TwinKind::Plain => plain.to_vec(),
            TwinKind::Int => vec![
                ValType::I64,
                super::types::struct_ref(registry.aint_struct_idx.unwrap_or_default()),
            ],
            TwinKind::Fields { fields, .. } => fields.clone(),
        }
    }
}

/// The struct fields a result of type `ret` unpacks into, when it does.
fn kind_of(ret: &str, registry: &TypeRegistry) -> Result<Option<TwinKind>, WasmGcError> {
    let ret = ret.trim();
    if ret == "Int" {
        return Ok((registry.bignum && registry.aint_struct_idx.is_some()).then_some(TwinKind::Int));
    }
    let Some(ValType::Ref(r)) = aver_to_wasm(ret, Some(registry))? else {
        return Ok(None);
    };
    let HeapType::Concrete(idx) = r.heap_type else {
        return Ok(None);
    };
    let wasm = |t: &str| -> Result<ValType, WasmGcError> {
        Ok(aver_to_wasm(t, Some(registry))?.unwrap_or(ValType::I32))
    };
    let option = registry.option_types.iter().find(|(_, i)| **i == idx);
    let result = registry.result_types.iter().find(|(_, i)| **i == idx);
    let fields = match (option, result) {
        (Some((c, _)), _) => match TypeRegistry::option_element_type(c) {
            Some(t) => vec![ValType::I32, wasm(t)?],
            None => return Ok(None),
        },
        (_, Some((c, _))) => match TypeRegistry::result_te(c) {
            Some((t, e)) => vec![ValType::I32, wasm(t)?, wasm(e)?],
            None => return Ok(None),
        },
        _ => return Ok(None),
    };
    Ok(Some(TwinKind::Fields {
        struct_idx: idx,
        fields,
    }))
}

/// The expressions whose value is the function's: the body, through the
/// arms, branches and bodies of a `Match`, an `IfThenElse` and a `Let`.
pub(crate) fn tails<'a>(
    e: &'a crate::ast::Spanned<MirExpr>,
    out: &mut Vec<&'a crate::ast::Spanned<MirExpr>>,
) {
    match &e.node {
        MirExpr::Let(l) => tails(&l.node.body, out),
        MirExpr::Match(m) => {
            for arm in &m.node.arms {
                tails(&arm.body, out);
            }
        }
        MirExpr::IfThenElse(ite) => {
            tails(&ite.node.then_branch, out);
            tails(&ite.node.else_branch, out);
        }
        _ => out.push(e),
    }
}

fn tail_targets(e: &MirExpr, out: &mut Vec<FnId>) {
    if let MirExpr::TailCall(tc) = e {
        out.push(tc.node.target);
    }
    crate::ir::mir::expr::walk_children(e, &mut |c| tail_targets(c, out));
}

/// Whether a tail `e` of a function with a `kind` twin can hand its value
/// back unpacked by itself: a literal or `Int` arithmetic, or the struct the
/// twin unpacks built in place.
fn produces_unpacked(e: &MirExpr, kind: &TwinKind) -> bool {
    match kind {
        TwinKind::Int => matches!(
            e,
            MirExpr::Literal(_)
                | MirExpr::Box(_)
                | MirExpr::BinOp(_)
                | MirExpr::Neg(_)
                | MirExpr::Call(_)
        ),
        TwinKind::Plain => false,
        TwinKind::Fields { .. } => {
            matches!(e, MirExpr::Construct(c) if matches!(c.node.ctor, MirCtor::Builtin(_)))
        }
    }
}

/// Whether `body` reads the param in `slot` where its machine word serves:
/// in `Int` arithmetic or a comparison, as an arg of a call, or as a tail.
/// A param read only packed (stored, matched, stringified) is taken packed,
/// so the twin does not box what its caller had boxed already.
fn read_as_word(body: &crate::ast::Spanned<MirExpr>, slot: crate::ir::mir::LocalId) -> bool {
    fn walk(e: &MirExpr, slot: crate::ir::mir::LocalId) -> bool {
        let is = |x: &crate::ast::Spanned<MirExpr>| matches!(&x.node, MirExpr::Local(l) if l.node.slot == slot);
        let direct = match e {
            MirExpr::BinOp(b) => is(&b.node.lhs) || is(&b.node.rhs),
            MirExpr::Neg(x) | MirExpr::Unbox(x) => is(x),
            MirExpr::Call(c) => c.node.args.iter().any(is),
            MirExpr::TailCall(c) => c.node.args.iter().any(is),
            _ => false,
        };
        let mut found = direct;
        if !found {
            crate::ir::mir::expr::walk_children(e, &mut |c| found = found || walk(c, slot));
        }
        found
    }
    let mut ts = Vec::new();
    tails(body, &mut ts);
    ts.iter()
        .any(|t| matches!(&t.node, MirExpr::Local(l) if l.node.slot == slot))
        || walk(&body.node, slot)
}

/// How an expression's value is used: as a machine word (arithmetic, a
/// comparison, a builtin's arg), as a match subject, as its fn's result, or
/// otherwise.
#[derive(Clone, Copy, PartialEq)]
enum Use {
    Word,
    Subject,
    Tail,
    Other,
}

/// A fn's body read for the twins its calls would use (`plan`).
struct Caller<'a> {
    at: usize,
    plans: &'a [Option<TwinPlan>],
    kept: &'a HashSet<usize>,
    pos: &'a HashMap<FnId, usize>,
    params: &'a [crate::ir::mir::MirParam],
}

impl Caller<'_> {
    fn plan(&self, id: FnId) -> Option<(usize, &TwinPlan)> {
        let j = *self.pos.get(&id)?;
        Some((j, self.plans[j].as_ref()?))
    }

    /// Whether `arg` reaches a call as a machine word.
    fn word(&self, arg: &MirExpr) -> bool {
        match arg {
            MirExpr::Literal(l) => matches!(l.node, crate::ast::Literal::Int(_)),
            MirExpr::Box(_) | MirExpr::BinOp(_) | MirExpr::Neg(_) => true,
            MirExpr::Call(c) => match c.node.callee {
                MirCallee::Fn(id) => self
                    .plan(id)
                    .is_some_and(|(j, p)| self.kept.contains(&j) && p.kind == TwinKind::Int),
                _ => true,
            },
            MirExpr::Local(l) => {
                self.kept.contains(&self.at)
                    && self.params.iter().enumerate().any(|(k, p)| {
                        p.local == l.node.slot
                            && self.plans[self.at].as_ref().is_some_and(|p| p.unpacked[k])
                    })
            }
            _ => false,
        }
    }

    fn visit(&self, e: &MirExpr, used: Use, out: &mut Vec<usize>) {
        let words = |args: &[crate::ast::Spanned<MirExpr>], p: &TwinPlan| {
            args.iter()
                .zip(&p.unpacked)
                .any(|(a, u)| *u && self.word(&a.node))
        };
        let own = self.plans[self.at]
            .as_ref()
            .filter(|_| self.kept.contains(&self.at));
        match e {
            MirExpr::Call(c) => {
                if let MirCallee::Fn(id) = c.node.callee
                    && let Some((j, p)) = self.plan(id)
                {
                    let result = match &p.kind {
                        TwinKind::Plain => false,
                        TwinKind::Int => used == Use::Word,
                        TwinKind::Fields { .. } => used == Use::Subject,
                    } || (used == Use::Tail && own.is_some_and(|o| o.kind == p.kind));
                    if result || words(&c.node.args, p) {
                        out.push(j);
                    }
                }
                let inner = match c.node.callee {
                    MirCallee::Fn(_) => Use::Other,
                    _ => Use::Word,
                };
                for a in &c.node.args {
                    self.visit(&a.node, inner, out);
                }
            }
            MirExpr::TailCall(tc) => {
                // A twin tail-calls twins only; a fn without one, its own fn.
                if let Some((j, _)) = self.plan(tc.node.target)
                    && own.is_some()
                {
                    out.push(j);
                }
                for a in &tc.node.args {
                    self.visit(&a.node, Use::Other, out);
                }
            }
            MirExpr::BinOp(b) => {
                for x in [&b.node.lhs, &b.node.rhs] {
                    self.visit(&x.node, Use::Word, out);
                }
            }
            MirExpr::Neg(x) | MirExpr::Unbox(x) => self.visit(&x.node, Use::Word, out),
            MirExpr::Try(x) => self.visit(&x.node, Use::Subject, out),
            MirExpr::Let(l) => {
                self.visit(&l.node.value.node, Use::Other, out);
                self.visit(&l.node.body.node, used, out);
            }
            MirExpr::Match(m) => {
                self.visit(&m.node.subject.node, Use::Subject, out);
                for arm in &m.node.arms {
                    self.visit(&arm.body.node, used, out);
                }
            }
            MirExpr::IfThenElse(ite) => {
                self.visit(&ite.node.cond.node, Use::Other, out);
                self.visit(&ite.node.then_branch.node, used, out);
                self.visit(&ite.node.else_branch.node, used, out);
            }
            other => {
                crate::ir::mir::expr::walk_children(other, &mut |c| self.visit(c, Use::Other, out))
            }
        }
    }
}

/// The twin each user function gets, by position in `fn_defs`.
pub(super) fn plan(
    fn_defs: &[&FnDef],
    resolved: &[ResolvedFnDef],
    mir: &MirProgram,
    registry: &TypeRegistry,
    certify: bool,
) -> Result<Vec<Option<TwinPlan>>, WasmGcError> {
    let mut plans: Vec<Option<TwinPlan>> = vec![None; fn_defs.len()];
    if std::env::var_os("AVER_NO_TWINS").is_some() {
        return Ok(plans);
    }
    let ints = registry.bignum && registry.aint_struct_idx.is_some();
    let params_too = std::env::var_os("AVER_NO_TWIN_PARAMS").is_none();
    let pos: HashMap<FnId, usize> = resolved
        .iter()
        .enumerate()
        .map(|(i, r)| (r.fn_id, i))
        .collect();
    for (i, fd) in fn_defs.iter().enumerate() {
        let Some(mf) = mir.fn_by_id(resolved[i].fn_id) else {
            continue;
        };
        if fd.name == "main"
            || mf.repr.bare_return
            || (certify && crate::ir::cert_shape::printer_may_admit(fd))
        {
            continue;
        }
        let unpacked: Vec<bool> = fd
            .params
            .iter()
            .enumerate()
            .map(|(k, (_, ty))| {
                params_too
                    && ints
                    && k < 32
                    && ty.trim() == "Int"
                    && !mf.repr.param_is_bare(k)
                    && mf
                        .params
                        .get(k)
                        .is_some_and(|p| read_as_word(&mf.body, p.local))
            })
            .collect();
        let kind = kind_of(&fd.return_type, registry)?.unwrap_or(TwinKind::Plain);
        plans[i] = Some(TwinPlan { kind, unpacked });
    }
    // A result comes back unpacked when a tail produces it (a literal,
    // arithmetic, an unpacked param, the struct built in place) or hands on a
    // call's of the same kind; any other twin returns it packed.
    let kinds: Vec<Option<TwinKind>> = plans
        .iter()
        .map(|p| p.as_ref().map(|p| p.kind.clone()))
        .collect();
    for (i, plan) in plans.iter_mut().enumerate() {
        let Some(plan) = plan else { continue };
        let mf = mir.fn_by_id(resolved[i].fn_id).expect("planned fn has MIR");
        let mut ts = Vec::new();
        tails(&mf.body, &mut ts);
        let same = |id: &FnId| {
            pos.get(id)
                .is_some_and(|j| kinds[*j].as_ref() == Some(&plan.kind))
        };
        let unpacked = ts.iter().any(|t| match &t.node {
            MirExpr::Local(l) => mf
                .params
                .iter()
                .zip(&plan.unpacked)
                .any(|(p, u)| *u && p.local == l.node.slot),
            MirExpr::Call(c) if matches!(c.node.callee, MirCallee::Fn(_)) => {
                matches!(c.node.callee, MirCallee::Fn(id) if same(&id))
            }
            MirExpr::TailCall(tc) => same(&tc.node.target),
            e => produces_unpacked(e, &plan.kind),
        });
        if !unpacked {
            plan.kind = TwinKind::Plain;
        }
    }
    // Keep a twin only where a caller uses it: it takes the result apart,
    // hands it a machine word, or is a twin tail-calling it.
    let mut kept: HashSet<usize> = HashSet::new();
    loop {
        let before = kept.len();
        for (i, r) in resolved.iter().enumerate() {
            if let Some(mf) = mir.fn_by_id(r.fn_id) {
                let caller = Caller {
                    at: i,
                    plans: &plans,
                    kept: &kept,
                    pos: &pos,
                    params: &mf.params,
                };
                let mut wanted = Vec::new();
                caller.visit(&mf.body.node, Use::Tail, &mut wanted);
                kept.extend(wanted);
            }
        }
        if kept.len() == before {
            break;
        }
    }
    for (i, plan) in plans.iter_mut().enumerate() {
        if !kept.contains(&i) {
            *plan = None;
        }
    }
    // A twin's tail calls go to twins of the same kind: drop a twin whose
    // tail call has none, until none is left.
    loop {
        let mut changed = false;
        for i in 0..plans.len() {
            let Some(kind) = plans[i].as_ref().map(|p| p.kind.clone()) else {
                continue;
            };
            let mf = mir.fn_by_id(resolved[i].fn_id).expect("planned fn has MIR");
            let mut targets = Vec::new();
            tail_targets(&mf.body.node, &mut targets);
            let closed = targets.iter().all(|t| {
                pos.get(t)
                    .is_some_and(|j| plans[*j].as_ref().map(|p| &p.kind) == Some(&kind))
            });
            if !closed {
                plans[i] = None;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    Ok(plans)
}
