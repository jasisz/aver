//! Which functions may run first on machine words, restarting exactly on an
//! overflow.
//!
//! A function qualifies when it has no effects, its parameters are `Int` or
//! `Bool`, its result is `Int`, `Bool` or `Option<Int>`, and its body only
//! binds, matches and builds values of those types with the pure, total
//! `Int` operations of [`super::int_tree`], `Int.div` and `Int.mod` under
//! `Result.withDefault`, comparisons, `Bool.and`, `Bool.or` and `Bool.not`,
//! calls of qualifying functions, and tail calls to itself. Qualifying is a greatest fixed point: a function that calls
//! one that does not qualify does not qualify either.
//!
//! Every value such a function computes is an integer the machine word holds
//! exactly until an operation would leave the word's range, so a backend can
//! run it on `i64`, check each operation, and on the first overflow abandon
//! the whole computation and run the exact function from the start. Nothing
//! the function does can be observed before that point, so running part of
//! it twice cannot be either.
//!
//! The summary names two sets:
//!
//! - the **entries**: qualifying functions that sit in a cycle (recursion,
//!   including a tail call to themselves) or reach one through qualifying
//!   calls. These have enough work for a restart point to pay off; each
//!   carries the index of its cycle, so the exact version of one member can
//!   call the exact versions of the others directly instead of starting
//!   over again.
//! - the **twins**: every function an entry reaches, entries included. Each
//!   needs its word version, and the word version of a function calls only
//!   word versions.
//!
//! The summary is a fact about the program, not about a target: it contains
//! no types of any backend and no calling convention. The Rust backend reads
//! it (`codegen/rust/speculation.rs`); a backend without a cheap way to
//! abandon a call tree ignores it.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::ast::{BinOp, Literal, Spanned, Type};
use crate::ir::FnId;
use crate::ir::hir::BuiltinCtor;

use super::expr::{MirCallee, MirCtor, MirExpr, MirPattern};
use super::int_tree;
use super::program::{MirFn, MirProgram};

/// The types a qualifying function holds.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WordKind {
    Int,
    Bool,
    OptionInt,
}

impl WordKind {
    pub fn of(ty: &Type) -> Option<Self> {
        match ty {
            Type::Int => Some(WordKind::Int),
            Type::Bool => Some(WordKind::Bool),
            Type::Option(inner) if **inner == Type::Int => Some(WordKind::OptionInt),
            _ => None,
        }
    }

    /// The kind of a [`MirFn`] parameter or result type, which MIR keeps as
    /// the `Debug` text of the resolved type.
    pub fn of_annotation(ty: &str) -> Option<Self> {
        match ty.trim() {
            "Int" => Some(WordKind::Int),
            "Bool" => Some(WordKind::Bool),
            "Option(Int)" => Some(WordKind::OptionInt),
            _ => None,
        }
    }
}

/// What a backend reads: see the module docs.
#[derive(Clone, Debug, Default)]
pub struct WordSpeculation {
    /// The functions a word version is needed for.
    pub twins: BTreeSet<FnId>,
    /// The entries, each with the index of the cycle it sits in, or `None`
    /// when it only reaches one.
    pub entries: BTreeMap<FnId, Option<usize>>,
    /// The functions each twin calls (all of them twins).
    pub calls: BTreeMap<FnId, BTreeSet<FnId>>,
}

impl WordSpeculation {
    pub fn is_empty(&self) -> bool {
        self.twins.is_empty()
    }

    /// The twins the given entries reach, the entries included.
    pub fn reached_from(&self, entries: impl IntoIterator<Item = FnId>) -> BTreeSet<FnId> {
        let mut out = BTreeSet::new();
        let mut stack: Vec<FnId> = entries.into_iter().collect();
        while let Some(id) = stack.pop() {
            if out.insert(id) {
                stack.extend(self.calls.get(&id).into_iter().flatten().copied());
            }
        }
        out
    }

    /// Whether `a` and `b` are entries of one cycle.
    pub fn same_cycle(&self, a: FnId, b: FnId) -> bool {
        matches!(
            (self.entries.get(&a), self.entries.get(&b)),
            (Some(Some(x)), Some(Some(y))) if x == y
        )
    }
}

/// The summary of `program`. With `AVER_NO_WORD_SPECULATION` set it is
/// empty.
pub fn analyze(program: &MirProgram) -> WordSpeculation {
    if std::env::var_os("AVER_NO_WORD_SPECULATION").is_some() {
        return WordSpeculation::default();
    }
    // Each function that qualifies on its own, with the functions it calls.
    let mut calls: HashMap<FnId, BTreeSet<FnId>> = HashMap::new();
    for (id, f) in program.iter() {
        if let Some(callees) = own_shape(*id, f, program) {
            calls.insert(*id, callees);
        }
    }
    // Greatest fixed point: drop every function that calls one outside.
    loop {
        let out: Vec<FnId> = calls
            .iter()
            .filter(|(_, callees)| callees.iter().any(|c| !calls.contains_key(c)))
            .map(|(id, _)| *id)
            .collect();
        if out.is_empty() {
            break;
        }
        for id in out {
            calls.remove(&id);
        }
    }
    let mut nodes: Vec<FnId> = calls.keys().copied().collect();
    nodes.sort();
    let graph: HashMap<FnId, Vec<FnId>> = calls
        .iter()
        .map(|(id, c)| (*id, c.iter().copied().collect()))
        .collect();
    let mut cycle_of: HashMap<FnId, usize> = HashMap::new();
    for (i, component) in crate::scc::tarjan_sccs(&nodes, &graph)
        .into_iter()
        .enumerate()
    {
        let cyclic = component.len() > 1 || calls[&component[0]].contains(&component[0]);
        if cyclic {
            for id in component {
                cycle_of.insert(id, i);
            }
        }
    }
    let mut reaches: HashMap<FnId, bool> = HashMap::new();
    let mut entries = BTreeMap::new();
    for id in &nodes {
        if reaches_cycle(*id, &calls, &cycle_of, &mut reaches) {
            entries.insert(*id, cycle_of.get(id).copied());
        }
    }
    let mut twins = BTreeSet::new();
    let mut stack: Vec<FnId> = entries.keys().copied().collect();
    while let Some(id) = stack.pop() {
        if twins.insert(id) {
            stack.extend(calls[&id].iter().copied());
        }
    }
    // Twins are emitted as the backend needs them; `calls` lets it trim.
    let calls = twins.iter().map(|id| (*id, calls[id].clone())).collect();
    WordSpeculation {
        twins,
        entries,
        calls,
    }
}

fn reaches_cycle(
    id: FnId,
    calls: &HashMap<FnId, BTreeSet<FnId>>,
    cycle_of: &HashMap<FnId, usize>,
    memo: &mut HashMap<FnId, bool>,
) -> bool {
    if let Some(r) = memo.get(&id) {
        return *r;
    }
    if cycle_of.contains_key(&id) {
        memo.insert(id, true);
        return true;
    }
    // Outside a cycle the calls form a DAG, so the walk ends.
    memo.insert(id, false);
    let r = calls[&id]
        .iter()
        .any(|c| reaches_cycle(*c, calls, cycle_of, memo));
    memo.insert(id, r);
    r
}

/// The functions `f` calls when its own signature and body qualify.
fn own_shape(id: FnId, f: &MirFn, program: &MirProgram) -> Option<BTreeSet<FnId>> {
    if !f.effects.is_empty() || f.name == "main" {
        return None;
    }
    let params_ok = f.params.iter().all(|p| {
        matches!(
            WordKind::of_annotation(&p.ty),
            Some(WordKind::Int | WordKind::Bool)
        )
    });
    if !params_ok || WordKind::of_annotation(&f.return_type).is_none() {
        return None;
    }
    let mut callees = BTreeSet::new();
    let mut check = Check {
        id,
        builtins: &program.builtins,
        callees: &mut callees,
    };
    check.expr(&f.body, true).then_some(callees)
}

struct Check<'a> {
    id: FnId,
    builtins: &'a [String],
    callees: &'a mut BTreeSet<FnId>,
}

fn kind(e: &Spanned<MirExpr>) -> Option<WordKind> {
    e.ty().and_then(WordKind::of)
}

impl Check<'_> {
    /// Whether `e` qualifies; `tail` when it is in tail position, where a
    /// tail call to the function itself may stand.
    fn expr(&mut self, e: &Spanned<MirExpr>, tail: bool) -> bool {
        match &e.node {
            MirExpr::Literal(l) => matches!(l.node, Literal::Int(_) | Literal::Bool(_)),
            MirExpr::Local(_) => kind(e).is_some(),
            MirExpr::Let(l) => {
                kind(&l.node.value).is_some()
                    && self.expr(&l.node.value, false)
                    && self.expr(&l.node.body, tail)
            }
            MirExpr::IfThenElse(i) => {
                let i = &i.node;
                kind(&i.cond) == Some(WordKind::Bool)
                    && self.expr(&i.cond, false)
                    && self.expr(&i.then_branch, tail)
                    && self.expr(&i.else_branch, tail)
            }
            MirExpr::Match(m) => {
                let m = &m.node;
                let Some(subject) = kind(&m.subject) else {
                    return false;
                };
                self.expr(&m.subject, false)
                    && !m.arms.is_empty()
                    && m.arms
                        .iter()
                        .all(|arm| pattern_ok(&arm.pattern, subject) && self.expr(&arm.body, tail))
            }
            MirExpr::BinOp(b) => {
                let b = &b.node;
                let compare = matches!(
                    b.op,
                    BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte
                );
                if compare {
                    let sides = (kind(&b.lhs), kind(&b.rhs));
                    let ordered = matches!(b.op, BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte);
                    let ok = match sides {
                        (Some(WordKind::Int), Some(WordKind::Int)) => true,
                        (Some(WordKind::Bool), Some(WordKind::Bool)) => !ordered,
                        _ => false,
                    };
                    return ok && self.expr(&b.lhs, false) && self.expr(&b.rhs, false);
                }
                self.int_op(e)
            }
            MirExpr::Neg(_) => self.int_op(e),
            MirExpr::Call(c) => {
                if c.lazy.is_deferred() {
                    return false;
                }
                let call = &c.node;
                match &call.callee {
                    MirCallee::Fn(callee) => {
                        self.callees.insert(*callee);
                        kind(e).is_some() && call.args.iter().all(|a| self.expr(a, false))
                    }
                    MirCallee::Builtin(id) => {
                        let name = self.builtins.get(id.0 as usize).map(String::as_str);
                        let bools = |n: usize| {
                            call.args.len() == n
                                && call.args.iter().all(|a| kind(a) == Some(WordKind::Bool))
                        };
                        match name {
                            Some("Bool.and" | "Bool.or") if bools(2) => {
                                call.args.iter().all(|a| self.expr(a, false))
                            }
                            Some("Bool.not") if bools(1) => self.expr(&call.args[0], false),
                            Some("Result.withDefault") if call.args.len() == 2 => {
                                match division(&call.args[0], self.builtins) {
                                    Some((a, b)) => {
                                        kind(&call.args[1]) == Some(WordKind::Int)
                                            && self.expr(a, false)
                                            && self.expr(b, false)
                                            && self.expr(&call.args[1], false)
                                    }
                                    None => false,
                                }
                            }
                            _ => self.int_op(e),
                        }
                    }
                    MirCallee::Intrinsic(_) => self.int_op(e),
                    MirCallee::LocalSlot { .. } => false,
                }
            }
            MirExpr::TailCall(tc) => {
                // A loop: the function sits in a cycle of its own.
                self.callees.insert(tc.node.target);
                tail && tc.node.target == self.id
                    && tc.node.args.iter().all(|a| self.expr(a, false))
            }
            MirExpr::Construct(c) => match (&c.node.ctor, c.node.args.as_slice()) {
                (MirCtor::Builtin(BuiltinCtor::OptionSome), [a]) => {
                    kind(a) == Some(WordKind::Int) && self.expr(a, false)
                }
                (MirCtor::Builtin(BuiltinCtor::OptionNone), []) => true,
                _ => false,
            },
            _ => false,
        }
    }

    /// One of the pure, total `Int` operations, over qualifying operands.
    fn int_op(&mut self, e: &Spanned<MirExpr>) -> bool {
        match int_tree::op_of(e, self.builtins) {
            Some((_, a, b)) => self.expr(a, false) && b.is_none_or(|b| self.expr(b, false)),
            None => false,
        }
    }
}

/// `Int.div(a, b)` or `Int.mod(a, b)` with a divisor that may be zero, as
/// the operands of `Result.withDefault(_, fallback)`: the fallback on a zero
/// divisor, the Euclidean quotient or remainder otherwise.
pub fn division<'e>(
    e: &'e Spanned<MirExpr>,
    builtins: &[String],
) -> Option<(&'e Spanned<MirExpr>, &'e Spanned<MirExpr>)> {
    let MirExpr::Call(c) = &e.node else {
        return None;
    };
    let MirCallee::Builtin(id) = c.node.callee else {
        return None;
    };
    match (
        builtins.get(id.0 as usize)?.as_str(),
        c.node.args.as_slice(),
    ) {
        ("Int.div" | "Int.mod", [a, b])
            if kind(a) == Some(WordKind::Int) && kind(b) == Some(WordKind::Int) =>
        {
            Some((a, b))
        }
        _ => None,
    }
}

fn pattern_ok(p: &MirPattern, subject: WordKind) -> bool {
    match (p, subject) {
        (MirPattern::Wildcard | MirPattern::Bind(..), _) => true,
        (MirPattern::Literal(Literal::Int(_)), WordKind::Int) => true,
        (MirPattern::Literal(Literal::Bool(_)), WordKind::Bool) => true,
        (MirPattern::Ctor { ctor, bindings, .. }, WordKind::OptionInt) => matches!(
            (ctor, bindings.len()),
            (MirCtor::Builtin(BuiltinCtor::OptionSome), 1)
                | (MirCtor::Builtin(BuiltinCtor::OptionNone), 0)
        ),
        _ => false,
    }
}
