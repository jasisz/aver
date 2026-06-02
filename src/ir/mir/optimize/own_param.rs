//! Interprocedural Vector/Map parameter ownership refinement.
//!
//! `alias.rs` (RULE 1) conservatively flags **every** `Vector<..>` /
//! `Map<..>` parameter as alias-prone, because a caller *might* keep its
//! own reference to the same arena entry. That kills the owned-mutate
//! fast path (`owned = last_use && !aliased`) on a param even when no
//! caller actually shares it — e.g. a vector threaded linearly through
//! tail recursion (`fn fill(v, ..) -> .. = fill(set(v, ..), ..)`), which
//! clones the backing array on every call: O(n²).
//!
//! This pass refines that decision interprocedurally: a Vector/Map param
//! `p` of fn `f` is un-flagged (cleared from `aliased_slots`) only when
//! **every** call site of `f` passes `p` a *uniquely-owned* argument,
//! computed as a monotone-descending fixpoint (recursion couples a
//! param's ownership to its own call sites). Default is to keep the
//! conservative flag — un-flagging is the exception, gated on proof.
//!
//! ## Soundness
//!
//! A false negative (un-flag a param that *is* aliased) lets a backend
//! mutate shared data in place → silent corruption. Every uncertain
//! point therefore resolves to **not-owned** (keep flagged):
//!
//! - Runs only on a whole-program `MirProgram` (`modules` empty / one):
//!   a multi-module or per-module-VM fragment can have callers we can't
//!   see, and a missed aliased caller is unsound. Multi-module is skipped
//!   wholesale.
//! - `main`/entry and any address-taken fn (its name appears in a
//!   `MirExpr::FnValue`) keep all params flagged — their callers are not
//!   all visible `Call`/`TailCall` edges.
//! - A call site reached through `MirCallee::LocalSlot` (a fn value) has
//!   no statically-known target, so it never counts as a visible caller;
//!   such fns are already pinned via the address-taken set.
//! - An argument is uniquely-owned only when proven so: a fresh
//!   scalar-defaulted constructor, an owned `Vector.set`/`Map.set` chain,
//!   `Option.withDefault` of owned branches, or a `last_use`,
//!   non-aliased local whose binding provenance is itself owned (never a
//!   user-fn-call result — that may alias an argument, the gap `alias.rs`
//!   RULE 2 cannot see). Anything unrecognized is not-owned.
//! - Two alias-prone params receiving the same slot at one call site are
//!   both rejected (the value is shared between them inside the callee).
//!
//! Param slots are flagged *only* by RULE 1 (RULE 2 flags let-binding
//! slots, never params), so a flagged param slot is exactly a Vector/Map
//! param — the pass needs no provenance split and never touches RULE-2
//! bits.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::ast::Spanned;
use crate::ir::FnId;

use super::super::expr::{MirCallee, MirExpr};
use super::super::program::MirProgram;

/// Recursion cap for `uniquely_owned` — defends against a pathological
/// let-provenance chain; exceeding it resolves to not-owned.
const MAX_DEPTH: u32 = 64;

/// A single visible call edge: `target(args…)` made from `caller`.
struct CallSite {
    target: FnId,
    caller: FnId,
    args: Vec<Spanned<MirExpr>>,
}

pub fn own_param_refine(mut program: MirProgram) -> MirProgram {
    // Diagnostic / bench-differential escape hatch: skip the refinement
    // so a run keeps the conservative all-params-aliased baseline.
    if std::env::var("AVER_NO_OWN_PARAM").is_ok() {
        return program;
    }
    // Whole-program gate: only sound when every caller is visible here.
    if program.modules.len() > 1 {
        return program;
    }

    // Which param slots are alias-prone (flagged) per fn. A flagged
    // param slot ⟺ a Vector/Map param (RULE 1 only).
    let mut prone: HashMap<FnId, Vec<usize>> = HashMap::new();
    for (id, f) in program.iter() {
        let nparams = f.params.len();
        let v: Vec<usize> = (0..nparams)
            .filter(|&i| f.aliased_slots.get(i).copied().unwrap_or(false))
            .collect();
        if !v.is_empty() {
            prone.insert(*id, v);
        }
    }
    if prone.is_empty() {
        return program;
    }

    // Aggregate-capture aliasing. A slot whose value is stored DIRECTLY
    // into an aggregate (record / record-update / variant / tuple / list /
    // map literal / independent product) is shared with that aggregate —
    // an in-place mutation of the slot would corrupt the aggregate's copy.
    // `last_use` marks the slot dead (the aggregate field, not the slot,
    // is read later) and alias.rs RULE 2 does not model the capture, so
    // without this the analysis would treat such a slot as uniquely owned
    // and un-flag a param fed by it — a silent corruption (a vector
    // aliased into a record field, then own-mutated through that param).
    // Flag every captured slot up front so it is never un-flagged and
    // never owned-mutated. (Only DIRECT `Local` field/element values
    // alias; a `Local` nested inside a sub-computation is consumed by it,
    // not stored into the aggregate.)
    for (_, f) in program.fns.iter_mut() {
        let mut captured: HashSet<u32> = HashSet::new();
        collect_captured_slots(&f.body.node, &mut captured);
        if captured.is_empty() {
            continue;
        }
        let mut slots = f.aliased_slots.as_ref().clone();
        if let Some(&max) = captured.iter().max()
            && (max as usize) >= slots.len()
        {
            slots.resize(max as usize + 1, false);
        }
        for s in captured {
            slots[s as usize] = true;
        }
        f.aliased_slots = Arc::new(slots);
    }

    // Address-taken fns: any name appearing in a `MirExpr::FnValue`.
    let mut address_taken: HashSet<String> = HashSet::new();
    for (_, f) in program.iter() {
        collect_fn_values(&f.body.node, &mut address_taken);
    }

    // Pin set: fns whose params must stay conservatively flagged.
    // `main`/entry (runtime calls it) + anything address-taken.
    let pinned: HashSet<FnId> = program
        .iter()
        .filter(|(_, f)| f.name == "main" || address_taken.contains(&f.name))
        .map(|(id, _)| *id)
        .collect();

    // Per-fn slot → binding-RHS provenance (from the body's `Let`s).
    let mut provenance: HashMap<FnId, HashMap<u32, Spanned<MirExpr>>> = HashMap::new();
    for (id, f) in program.iter() {
        let mut m = HashMap::new();
        collect_let_bindings(&f.body.node, &mut m);
        provenance.insert(*id, m);
    }

    // Visible call edges (Call(Fn) + TailCall). LocalSlot/Builtin/
    // Intrinsic callees are not attributed to any fn's params.
    let mut call_sites: Vec<CallSite> = Vec::new();
    for (caller, f) in program.iter() {
        collect_call_sites(*caller, &f.body.node, &mut call_sites);
    }

    // Fixpoint lattice: owned[(fn, param_idx)] for alias-prone params.
    // Init optimistic true; pinned fns start (and stay) false.
    let mut owned: HashMap<(FnId, usize), bool> = HashMap::new();
    for (id, idxs) in &prone {
        let pin = pinned.contains(id);
        for &i in idxs {
            owned.insert((*id, i), !pin);
        }
    }

    let builtins = program.builtins.clone();
    loop {
        let mut changed = false;
        for cs in &call_sites {
            let Some(idxs) = prone.get(&cs.target) else {
                continue;
            };
            if pinned.contains(&cs.target) {
                continue;
            }
            // Same-slot-to-two-params rejection: a slot handed to more
            // than one alias-prone param at this site is shared.
            let mut slot_counts: HashMap<u32, u32> = HashMap::new();
            for &i in idxs {
                if let Some(a) = cs.args.get(i)
                    && let MirExpr::Local(l) = &a.node
                {
                    *slot_counts.entry(l.node.slot.0).or_insert(0) += 1;
                }
            }
            for &i in idxs {
                let key = (cs.target, i);
                if !owned.get(&key).copied().unwrap_or(false) {
                    continue; // already false — monotone, skip
                }
                let arg = match cs.args.get(i) {
                    Some(a) => a,
                    None => {
                        // Arity mismatch shouldn't happen post-typecheck;
                        // be conservative.
                        if owned.insert(key, false).unwrap_or(false) {
                            changed = true;
                        }
                        continue;
                    }
                };
                let dup = matches!(&arg.node, MirExpr::Local(l) if slot_counts.get(&l.node.slot.0).copied().unwrap_or(0) > 1);
                let ok = !dup
                    && uniquely_owned(
                        &arg.node,
                        cs.caller,
                        &program,
                        &owned,
                        &provenance,
                        &builtins,
                        0,
                    );
                if !ok && owned.insert(key, false) != Some(false) {
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }

    // Apply: clear the flag for every alias-prone param proven owned.
    let to_clear: Vec<(FnId, usize)> = owned
        .iter()
        .filter_map(|(k, v)| if *v { Some(*k) } else { None })
        .collect();
    for (id, idx) in to_clear {
        if let Some(f) = program.fns.get_mut(&id) {
            let mut slots = f.aliased_slots.as_ref().clone();
            if let Some(bit) = slots.get_mut(idx) {
                *bit = false;
            }
            f.aliased_slots = Arc::new(slots);
        }
    }

    program
}

/// Is `e`, evaluated in `caller`, a uniquely-owned value (does not
/// create / carry an alias of a still-live binding)? Default false.
#[allow(clippy::too_many_arguments)]
fn uniquely_owned(
    e: &MirExpr,
    caller: FnId,
    program: &MirProgram,
    owned: &HashMap<(FnId, usize), bool>,
    provenance: &HashMap<FnId, HashMap<u32, Spanned<MirExpr>>>,
    builtins: &[String],
    depth: u32,
) -> bool {
    if depth > MAX_DEPTH {
        return false;
    }
    match e {
        // Fresh, scalar-defaulted constructors are uniquely owned.
        MirExpr::MapLiteral(_) => true,
        MirExpr::Call(c) => match &c.node.callee {
            MirCallee::Builtin(id) => {
                let name = builtins
                    .get(id.0 as usize)
                    .map(String::as_str)
                    .unwrap_or("");
                match name {
                    // Vector.new(n, default): owned iff the default is a
                    // scalar literal (a compound default makes every cell
                    // share it — alias.rs RULE 3).
                    "Vector.new" => c
                        .node
                        .args
                        .get(1)
                        .is_some_and(|d| matches!(&d.node, MirExpr::Literal(_))),
                    "Map.new" => true,
                    // set returns its (mutated) vector/map — owned iff the
                    // target is owned.
                    "Vector.set" | "Map.set" => c.node.args.first().is_some_and(|v| {
                        uniquely_owned(
                            &v.node,
                            caller,
                            program,
                            owned,
                            provenance,
                            builtins,
                            depth + 1,
                        )
                    }),
                    "Option.withDefault" if c.node.args.len() == 2 => {
                        // Self-keep fusion shape:
                        // `withDefault(Vector.set(Local{s}, ..), Local{s})`
                        // — the fused VECTOR_SET_OR_KEEP consumes slot `s`
                        // exactly once and returns one of its two handles,
                        // so its ownership is the slot's, with `last_use`
                        // OR'd across the two occurrences (mirror of the VM
                        // fusion collapse in `vm/compiler/mir.rs`). Without
                        // this, the inner `Vector.set`'s `Local{s}` carries
                        // last_use=false (the textually-last read is the
                        // default), wrongly poisoning the chain for a
                        // linearly-threaded param.
                        if let MirExpr::Call(inner) = &c.node.args[0].node
                            && let MirCallee::Builtin(iid) = inner.node.callee
                            && matches!(
                                builtins.get(iid.0 as usize).map(String::as_str),
                                Some("Vector.set") | Some("Map.set")
                            )
                            && let Some(set_vec) = inner.node.args.first()
                            && let MirExpr::Local(v) = &set_vec.node
                            && let MirExpr::Local(d) = &c.node.args[1].node
                            && v.node.slot == d.node.slot
                        {
                            let live = v.node.last_use || d.node.last_use;
                            return live
                                && slot_owned(
                                    v.node.slot.0,
                                    caller,
                                    program,
                                    owned,
                                    provenance,
                                    builtins,
                                    depth + 1,
                                );
                        }
                        // General: both branches independently owned (the
                        // surviving handle is one of them).
                        uniquely_owned(
                            &c.node.args[0].node,
                            caller,
                            program,
                            owned,
                            provenance,
                            builtins,
                            depth + 1,
                        ) && uniquely_owned(
                            &c.node.args[1].node,
                            caller,
                            program,
                            owned,
                            provenance,
                            builtins,
                            depth + 1,
                        )
                    }
                    // Vector.get / Map.get return an alias into the source;
                    // every other builtin result is not provably owned.
                    _ => false,
                }
            }
            // User-fn / fn-value / intrinsic results may alias an arg
            // (the RULE-2 gap) — never provably owned without a
            // returns-fresh analysis (deferred).
            MirCallee::Fn(_) | MirCallee::LocalSlot { .. } | MirCallee::Intrinsic(_) => false,
        },
        // A live (last-use), owned slot read.
        MirExpr::Local(l) => {
            l.node.last_use
                && slot_owned(
                    l.node.slot.0,
                    caller,
                    program,
                    owned,
                    provenance,
                    builtins,
                    depth + 1,
                )
        }
        _ => false,
    }
}

/// Is slot `s` (in `caller`) a uniquely-owned binding, IGNORING the
/// per-occurrence `last_use` flag? Factored out of the `Local` arm so
/// the self-keep shape can decide liveness by OR-ing its two
/// occurrences' last-use bits while sharing the same ownership logic.
#[allow(clippy::too_many_arguments)]
fn slot_owned(
    slot: u32,
    caller: FnId,
    program: &MirProgram,
    owned: &HashMap<(FnId, usize), bool>,
    provenance: &HashMap<FnId, HashMap<u32, Spanned<MirExpr>>>,
    builtins: &[String],
    depth: u32,
) -> bool {
    if depth > MAX_DEPTH {
        return false;
    }
    let caller_fn = match program.fn_by_id(caller) {
        Some(f) => f,
        None => return false,
    };
    let is_param = (slot as usize) < caller_fn.params.len();
    // Flagged in the caller's table (RULE 1 param or RULE 2 intra-proc
    // alias such as a Vector.get handle).
    if caller_fn
        .aliased_slots
        .get(slot as usize)
        .copied()
        .unwrap_or(false)
    {
        // A flagged param may yet be un-flagged — consult the lattice.
        // A flagged non-param slot is a real intra-procedural alias.
        if is_param {
            return owned
                .get(&(caller, slot as usize))
                .copied()
                .unwrap_or(false);
        }
        return false;
    }
    if is_param {
        // Alias-prone param already proven owned, or a scalar
        // (non-alias-prone) param — scalars never alias.
        return owned.get(&(caller, slot as usize)).copied().unwrap_or(true);
    }
    // A let-bound local: owned iff its binding RHS is owned.
    match provenance.get(&caller).and_then(|m| m.get(&slot)) {
        Some(rhs) => uniquely_owned(
            &rhs.node,
            caller,
            program,
            owned,
            provenance,
            builtins,
            depth + 1,
        ),
        None => false,
    }
}

/// Collect every fn name referenced as a value (`MirExpr::FnValue`).
fn collect_fn_values(e: &MirExpr, out: &mut HashSet<String>) {
    visit_children(e, &mut |c| collect_fn_values(c, out));
    if let MirExpr::FnValue(name) = e {
        out.insert(name.clone());
    }
}

/// Collect `slot → binding-RHS` for every `Let` in the body.
fn collect_let_bindings(e: &MirExpr, out: &mut HashMap<u32, Spanned<MirExpr>>) {
    if let MirExpr::Let(l) = e {
        out.entry(l.node.binding.0)
            .or_insert_with(|| (*l.node.value).clone());
    }
    visit_children(e, &mut |c| collect_let_bindings(c, out));
}

/// Collect slots whose value is captured DIRECTLY as a field/element of
/// an aggregate constructor — the aggregate then shares the slot's
/// backing, so mutating the slot in place would corrupt the aggregate.
/// Only direct `Local` operands alias; a `Local` nested inside a
/// sub-computation is consumed by it (its result, not the slot, lands in
/// the aggregate), so the recursive `visit_children` walk picks up
/// deeper aggregates without over-flagging those consumed operands.
fn collect_captured_slots(e: &MirExpr, out: &mut HashSet<u32>) {
    fn flag(item: &Spanned<MirExpr>, out: &mut HashSet<u32>) {
        if let MirExpr::Local(l) = &item.node {
            out.insert(l.node.slot.0);
        }
    }
    match e {
        MirExpr::RecordCreate(r) => {
            for f in &r.node.fields {
                flag(&f.value, out);
            }
        }
        MirExpr::RecordUpdate(u) => {
            for f in &u.node.updates {
                flag(&f.value, out);
            }
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                flag(a, out);
            }
        }
        MirExpr::Tuple(items) | MirExpr::List(items) => {
            for i in items {
                flag(i, out);
            }
        }
        MirExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                flag(k, out);
                flag(v, out);
            }
        }
        MirExpr::IndependentProduct(ip) => {
            for i in &ip.node.items {
                flag(i, out);
            }
        }
        _ => {}
    }
    visit_children(e, &mut |c| collect_captured_slots(c, out));
}

/// Collect visible `Call(Fn)` / `TailCall` edges made from `caller`.
fn collect_call_sites(caller: FnId, e: &MirExpr, out: &mut Vec<CallSite>) {
    match e {
        MirExpr::Call(c) => {
            if let MirCallee::Fn(target) = c.node.callee {
                out.push(CallSite {
                    target,
                    caller,
                    args: c.node.args.clone(),
                });
            }
        }
        MirExpr::TailCall(tc) => {
            out.push(CallSite {
                target: tc.node.target,
                caller,
                args: tc.node.args.clone(),
            });
        }
        _ => {}
    }
    visit_children(e, &mut |c| collect_call_sites(caller, c, out));
}

/// Apply `f` to every immediate sub-expression of `e`. Mirrors the
/// exhaustive walk in `instantiations.rs`; keep in sync if MirExpr grows.
fn visit_children(e: &MirExpr, f: &mut dyn FnMut(&MirExpr)) {
    match e {
        MirExpr::Literal(_) | MirExpr::Local(_) | MirExpr::FnValue(_) => {}
        MirExpr::Let(l) => {
            f(&l.node.value.node);
            f(&l.node.body.node);
        }
        MirExpr::Call(c) => {
            for a in &c.node.args {
                f(&a.node);
            }
        }
        MirExpr::TailCall(tc) => {
            for a in &tc.node.args {
                f(&a.node);
            }
        }
        MirExpr::BinOp(b) => {
            f(&b.node.lhs.node);
            f(&b.node.rhs.node);
        }
        MirExpr::Neg(inner) | MirExpr::Try(inner) | MirExpr::Return(inner) => f(&inner.node),
        MirExpr::Match(m) => {
            f(&m.node.subject.node);
            for arm in &m.node.arms {
                f(&arm.body.node);
            }
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                f(&a.node);
            }
        }
        MirExpr::RecordCreate(r) => {
            for field in &r.node.fields {
                f(&field.value.node);
            }
        }
        MirExpr::RecordUpdate(u) => {
            f(&u.node.base.node);
            for field in &u.node.updates {
                f(&field.value.node);
            }
        }
        MirExpr::Project(p) => f(&p.node.base.node),
        MirExpr::IfThenElse(ite) => {
            f(&ite.node.cond.node);
            f(&ite.node.then_branch.node);
            f(&ite.node.else_branch.node);
        }
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for i in items {
                f(&i.node);
            }
        }
        MirExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                f(&k.node);
                f(&v.node);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for p in parts {
                if let super::super::expr::MirStrPart::Expr(e) = p {
                    f(&e.node);
                }
            }
        }
        MirExpr::IndependentProduct(ip) => {
            for i in &ip.node.items {
                f(&i.node);
            }
        }
    }
}
