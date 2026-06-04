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

    // Per-fn slot → binding-RHS provenance (from the body's `Let`s).
    // Computed first: the capture/escape analysis below resolves a
    // captured / escaping value back through its alias chain (a
    // `let w = v` rename, a `match`-bound rename, a passthrough-fn
    // result) to the originating slot, which needs the provenance map.
    let mut provenance: HashMap<FnId, HashMap<u32, Spanned<MirExpr>>> = HashMap::new();
    for (id, f) in program.iter() {
        let mut m = HashMap::new();
        collect_let_bindings(&f.body.node, &mut m);
        provenance.insert(*id, m);
    }

    // Interprocedural capture summary: `captures_param[f] = { i | param
    // i of f escapes into an aggregate, either DIRECTLY in f's body or
    // by being passed (through its alias chain) to a callee at a param
    // index that callee itself captures }`. Monotone-growing fixpoint
    // over the call graph; a captured param can never be in-place
    // mutated by any backend, so this drives both the same-fn pins
    // (below) and the cross-fn escape detection (a caller slot flowing
    // into a capturing callee param escapes in the caller too).
    let captures_param = compute_capture_summary(&program, &provenance);

    // Aggregate-capture + escape aliasing. A slot whose value is shared
    // with an aggregate (record / record-update / variant / tuple / list
    // / map literal / independent product) — DIRECTLY or through an alias
    // binding — must never be mutated in place: the aggregate keeps a
    // handle to the same backing. The same is true for a slot whose value
    // escapes into a callee that captures the corresponding param.
    // `last_use` marks the slot dead at the capture site (only the
    // aggregate field / callee is read after), and `alias.rs` RULE 2 does
    // not model either escape, so without this the analysis would treat
    // such a slot as uniquely owned and un-flag a param fed by it — a
    // silent corruption (the #383 / escape-audit class). `escaping_slots`
    // resolves every captured / escaping operand back through its alias
    // chain to the set of source slots and flags all of them.
    //
    // SOUNDNESS (the #383 corruption class): flagging `aliased_slots`
    // here is NOT enough for a PARAM slot. `prone` (computed above) keys
    // off RULE 1, so every Vector/Map param — captured or not — is
    // already in the owned-lattice; the fixpoint then seeds it optimistic
    // `true` and the apply step (`to_clear`) would CLEAR the very bit we
    // set here whenever the fn's callers all pass a fresh value (e.g. a
    // single `main` caller passing `Vector.new(..)`). That silently
    // un-flags a captured param and re-opens the corruption. So record
    // each fn's escaping PARAM slots and force their owned-lattice entry
    // to `false` (and keep it pinned there) below, so the proof can never
    // un-flag a slot whose value escaped into an aggregate or a
    // capturing callee.
    let builtins_pre = program.builtins.clone();
    let mut captured_param_slots: HashMap<FnId, HashSet<usize>> = HashMap::new();
    let mut escaping: HashMap<FnId, HashSet<u32>> = HashMap::new();
    for (id, f) in program.iter() {
        let prov = provenance.get(id).cloned().unwrap_or_default();
        let mut esc: HashSet<u32> = HashSet::new();
        collect_escaping_slots(
            &f.body.node,
            &prov,
            &captures_param,
            &builtins_pre,
            &mut esc,
        );
        // Live-alias-across-mutation (the `snap = v; … set(v,…); read
        // snap` class): a param `p` that has a still-live alias — a
        // let-binding whose RHS aliases `p` and whose slot is read in the
        // body — is NOT uniquely owned and must never be mutated in
        // place. A genuinely linear param (threaded through builtins /
        // the tail call with no alias binding, e.g. the `fillVector`
        // fast path) creates no such binding, so this never pins it.
        let nparams = f.params.len();
        collect_live_aliased_params(&f.body.node, &prov, &builtins_pre, nparams, &mut esc);
        if !esc.is_empty() {
            escaping.insert(*id, esc);
        }
    }
    for (id, f) in program.fns.iter_mut() {
        let Some(esc) = escaping.get(id) else {
            continue;
        };
        if esc.is_empty() {
            continue;
        }
        let nparams = f.params.len();
        let mut slots = f.aliased_slots.as_ref().clone();
        if let Some(&max) = esc.iter().max()
            && (max as usize) >= slots.len()
        {
            slots.resize(max as usize + 1, false);
        }
        let mut cap_params: HashSet<usize> = HashSet::new();
        for &s in esc {
            slots[s as usize] = true;
            if (s as usize) < nparams {
                cap_params.insert(s as usize);
            }
        }
        if !cap_params.is_empty() {
            captured_param_slots.insert(*id, cap_params);
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

    // Visible call edges (Call(Fn) + TailCall). LocalSlot/Builtin/
    // Intrinsic callees are not attributed to any fn's params.
    let mut call_sites: Vec<CallSite> = Vec::new();
    for (caller, f) in program.iter() {
        collect_call_sites(*caller, &f.body.node, &mut call_sites);
    }

    // Fixpoint lattice: owned[(fn, param_idx)] for alias-prone params.
    // Init optimistic true; pinned fns start (and stay) false. A param
    // slot whose value escaped into an aggregate (captured_param_slots)
    // also starts false — its backing is shared with the aggregate's copy,
    // so it must never be owned-mutated in place (the #383 class). The
    // lattice is monotone-descending, so a `false` seed stays `false`.
    let mut owned: HashMap<(FnId, usize), bool> = HashMap::new();
    for (id, idxs) in &prone {
        let pin = pinned.contains(id);
        let captured = captured_param_slots.get(id);
        for &i in idxs {
            let escaped = captured.is_some_and(|c| c.contains(&i));
            owned.insert((*id, i), !pin && !escaped);
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

/// The set of source slots whose backing the value of `e` may share —
/// the "alias roots" of `e`. Storing `e` into an aggregate, or passing
/// it where it can be retained, shares the backing of every slot in
/// this set, so none of them may be mutated in place afterwards.
///
/// This is the precise replacement for the old "only a DIRECT bare
/// `Local` aliases" rule, which missed every indirection the escape
/// audit found:
/// - `Local(s)` aliases `s` AND (if `s` is a let-bound local) every
///   root of its binding RHS — so a `let w = v` / `match`-bound rename
///   resolves back to the param `v`.
/// - a user-fn call (`f(a, b, …)`) result may alias ANY of its args
///   (the RULE-2 passthrough gap — `idv(v)` returns `v`), so its roots
///   are the union of the args' roots.
/// - `Vector.set` / `Map.set` return a value sharing their target's
///   backing (COW may keep it); `Option.withDefault(a, b)` returns one
///   of its two handles; `Vector.get` / `Map.get` may return an alias
///   into the source (nested collections) — all propagate roots.
///
/// A value built fresh (literal, constructor, `Vector.new` / `Map.new`,
/// arithmetic) has no roots: storing it shares nothing. The recursion
/// is depth-capped like `uniquely_owned`.
fn alias_roots(
    e: &MirExpr,
    prov: &HashMap<u32, Spanned<MirExpr>>,
    builtins: &[String],
    depth: u32,
    out: &mut HashSet<u32>,
) {
    if depth > MAX_DEPTH {
        return;
    }
    match e {
        MirExpr::Local(l) => {
            let slot = l.node.slot.0;
            out.insert(slot);
            // A let-bound local also carries its binding's roots (a
            // rename / fused chain). Params have no provenance entry,
            // so the recursion stops at the param slot itself.
            if let Some(rhs) = prov.get(&slot) {
                alias_roots(&rhs.node, prov, builtins, depth + 1, out);
            }
        }
        MirExpr::Match(m) => {
            for arm in &m.node.arms {
                alias_roots(&arm.body.node, prov, builtins, depth + 1, out);
            }
        }
        MirExpr::IfThenElse(ite) => {
            alias_roots(&ite.node.then_branch.node, prov, builtins, depth + 1, out);
            alias_roots(&ite.node.else_branch.node, prov, builtins, depth + 1, out);
        }
        MirExpr::Call(c) => match &c.node.callee {
            // A user fn / fn-value / intrinsic result may alias any of
            // its args (the RULE-2 passthrough gap).
            MirCallee::Fn(_) | MirCallee::LocalSlot { .. } | MirCallee::Intrinsic(_) => {
                for a in &c.node.args {
                    alias_roots(&a.node, prov, builtins, depth + 1, out);
                }
            }
            MirCallee::Builtin(id) => {
                let name = builtins
                    .get(id.0 as usize)
                    .map(String::as_str)
                    .unwrap_or("");
                match name {
                    // Fresh collections share nothing.
                    "Vector.new" | "Map.new" => {}
                    // set / get return a value sharing the target's
                    // backing; withDefault returns one of its handles.
                    "Vector.set" | "Map.set" | "Vector.get" | "Map.get" => {
                        if let Some(t) = c.node.args.first() {
                            alias_roots(&t.node, prov, builtins, depth + 1, out);
                        }
                    }
                    "Option.withDefault" => {
                        for a in &c.node.args {
                            alias_roots(&a.node, prov, builtins, depth + 1, out);
                        }
                    }
                    // Any other builtin result is not provably fresh;
                    // be conservative and propagate every arg's roots.
                    _ => {
                        for a in &c.node.args {
                            alias_roots(&a.node, prov, builtins, depth + 1, out);
                        }
                    }
                }
            }
        },
        MirExpr::Return(inner) | MirExpr::Try(inner) => {
            alias_roots(&inner.node, prov, builtins, depth + 1, out)
        }
        // Literals, fresh constructors, record/tuple/list/map literals,
        // projections, arithmetic, interpolation, tail calls: building
        // them shares no caller-visible backing slot.
        _ => {}
    }
}

/// Collect every slot that ESCAPES in this fn body: stored (directly or
/// through an alias chain) into an aggregate, or passed to a callee at a
/// param index that callee captures. Such a slot must stay flagged — an
/// in-place mutation would corrupt the aggregate's / callee's retained
/// handle. Generalizes the old direct-`Local`-only capture detection via
/// `alias_roots`, closing the let-rename / match-rename / passthrough-fn
/// capture classes and the cross-fn store-then-mutate class.
fn collect_escaping_slots(
    e: &MirExpr,
    prov: &HashMap<u32, Spanned<MirExpr>>,
    captures_param: &HashMap<FnId, HashSet<usize>>,
    builtins: &[String],
    out: &mut HashSet<u32>,
) {
    let capture = |item: &Spanned<MirExpr>, out: &mut HashSet<u32>| {
        alias_roots(&item.node, prov, builtins, 0, out);
    };
    match e {
        MirExpr::RecordCreate(r) => {
            for f in &r.node.fields {
                capture(&f.value, out);
            }
        }
        MirExpr::RecordUpdate(u) => {
            // The base record retains its own backing; an update keeps
            // the un-updated fields, which may alias the base's slots.
            capture(&u.node.base, out);
            for f in &u.node.updates {
                capture(&f.value, out);
            }
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                capture(a, out);
            }
        }
        MirExpr::Tuple(items) | MirExpr::List(items) => {
            for i in items {
                capture(i, out);
            }
        }
        MirExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                capture(k, out);
                capture(v, out);
            }
        }
        MirExpr::IndependentProduct(ip) => {
            for i in &ip.node.items {
                capture(i, out);
            }
        }
        // Cross-fn escape: an arg passed at a param index the callee
        // captures escapes in this fn too. (TailCall stays in the same
        // SCC and threads ownership through frame reuse — it is the
        // linear fast path, not a capture — so it is NOT an escape.)
        MirExpr::Call(c) => {
            if let MirCallee::Fn(target) = &c.node.callee
                && let Some(captured_idxs) = captures_param.get(target)
            {
                for &i in captured_idxs {
                    if let Some(arg) = c.node.args.get(i) {
                        alias_roots(&arg.node, prov, builtins, 0, out);
                    }
                }
            }
        }
        _ => {}
    }
    visit_children(e, &mut |c| {
        collect_escaping_slots(c, prov, captures_param, builtins, out)
    });
}

/// Pin every PARAM that has a still-live alias: a let-bound slot whose
/// binding RHS aliases the param (its `alias_roots` contains the param
/// slot) and whose own slot is read somewhere in the body. Such a param
/// is not uniquely owned — a second binding observes the same backing —
/// so an in-place mutation would corrupt the alias (the `snap = v; …
/// set(v,…); read snap` class). A genuinely linear param threads through
/// builtins / the tail call without any alias binding, so this never
/// pins the fast path.
fn collect_live_aliased_params(
    body: &MirExpr,
    prov: &HashMap<u32, Spanned<MirExpr>>,
    builtins: &[String],
    nparams: usize,
    out: &mut HashSet<u32>,
) {
    // Every slot read by a bare `Local` anywhere in the body.
    let mut read: HashSet<u32> = HashSet::new();
    collect_local_reads(body, &mut read);
    for (&binding, rhs) in prov {
        // The binding slot must itself be live (read), else no alias is
        // observed.
        if !read.contains(&binding) {
            continue;
        }
        // Use `rename_roots`, NOT `alias_roots`: only a *pure value-copy*
        // alias (a `let w = v` rename, match/if of a param read, a
        // passthrough-fn result) keeps the param co-live for a
        // read-after-mutation. A `Vector.set` / `Map.set` / self-keep
        // `withDefault(set(p,…), p)` is the param's mutated SUCCESSOR
        // (it consumes `p`), so binding the result does NOT keep `p`
        // problematically live — that is the linear fast path, and
        // `alias_roots` (which treats the set result as sharing `p`'s
        // backing, correct for the CAPTURE check) would wrongly pin it.
        let mut roots: HashSet<u32> = HashSet::new();
        rename_roots(&rhs.node, prov, builtins, 0, &mut roots);
        for &r in &roots {
            // `r` is an aliased param distinct from this binding slot.
            if r != binding && (r as usize) < nparams {
                out.insert(r);
            }
        }
    }
}

/// Param slots that `e` is a *pure value-copy alias* of — the subset of
/// `alias_roots` that keeps the source slot CO-LIVE rather than
/// consuming it. Used only by the live-alias-across-mutation pin
/// (`collect_live_aliased_params`); the capture / cross-fn escape paths
/// use the full `alias_roots` semantics.
///
/// The key difference from `alias_roots`: a `Vector.set` / `Map.set`
/// result, and the self-keep `withDefault(set(p,…), p)` fusion shape,
/// are the param's MUTATED SUCCESSOR — they consume `p` and the result
/// IS the new value, so binding them does not create a problematic
/// co-live alias (this is the `fillVector` fast path). They therefore
/// contribute NO rename roots. Everything that genuinely keeps the
/// source live — a bare `Local`, a match/if of one, a passthrough-fn
/// result, a `withDefault`/`get` that selects/projects a still-live
/// source — propagates, so the read-after-mutation classes stay caught.
fn rename_roots(
    e: &MirExpr,
    prov: &HashMap<u32, Spanned<MirExpr>>,
    builtins: &[String],
    depth: u32,
    out: &mut HashSet<u32>,
) {
    if depth > MAX_DEPTH {
        return;
    }
    match e {
        MirExpr::Local(l) => {
            let slot = l.node.slot.0;
            out.insert(slot);
            if let Some(rhs) = prov.get(&slot) {
                rename_roots(&rhs.node, prov, builtins, depth + 1, out);
            }
        }
        MirExpr::Match(m) => {
            for arm in &m.node.arms {
                rename_roots(&arm.body.node, prov, builtins, depth + 1, out);
            }
        }
        MirExpr::IfThenElse(ite) => {
            rename_roots(&ite.node.then_branch.node, prov, builtins, depth + 1, out);
            rename_roots(&ite.node.else_branch.node, prov, builtins, depth + 1, out);
        }
        MirExpr::Call(c) => match &c.node.callee {
            // A user-fn / fn-value / intrinsic result may alias an arg
            // while leaving it live (the RULE-2 passthrough gap).
            MirCallee::Fn(_) | MirCallee::LocalSlot { .. } | MirCallee::Intrinsic(_) => {
                for a in &c.node.args {
                    rename_roots(&a.node, prov, builtins, depth + 1, out);
                }
            }
            MirCallee::Builtin(id) => {
                let name = builtins
                    .get(id.0 as usize)
                    .map(String::as_str)
                    .unwrap_or("");
                match name {
                    // Mutated successor / fresh value — consumes the
                    // target, so it is NOT a co-live alias.
                    "Vector.set" | "Map.set" | "Vector.new" | "Map.new" => {}
                    // `withDefault` surfaces one of its handles; the
                    // self-keep `withDefault(set(p,…), p)` fusion is the
                    // linear successor (consumes `p`) — contribute
                    // nothing for it; any other `withDefault` may select
                    // a still-live aliased branch, so propagate.
                    "Option.withDefault"
                        if c.node.args.len() == 2
                            && is_self_keep_set(
                                &c.node.args[0].node,
                                &c.node.args[1].node,
                                builtins,
                            ) => {}
                    "Option.withDefault" => {
                        for a in &c.node.args {
                            rename_roots(&a.node, prov, builtins, depth + 1, out);
                        }
                    }
                    // `get` may project a still-live inner collection
                    // (nested case); any other builtin is conservative.
                    _ => {
                        for a in &c.node.args {
                            rename_roots(&a.node, prov, builtins, depth + 1, out);
                        }
                    }
                }
            }
        },
        MirExpr::Return(inner) | MirExpr::Try(inner) => {
            rename_roots(&inner.node, prov, builtins, depth + 1, out)
        }
        _ => {}
    }
}

/// Recognize the self-keep fusion shape `withDefault(Vector.set|Map.set(
/// Local(s), …), Local(s))` — the linear in-place mutation idiom whose
/// two operands both reference the same slot `s`. Mirrors the
/// recognition in `uniquely_owned`; used by `rename_roots` to treat the
/// successor as consuming (not aliasing) `s`.
fn is_self_keep_set(set_arg: &MirExpr, default_arg: &MirExpr, builtins: &[String]) -> bool {
    if let MirExpr::Call(inner) = set_arg
        && let MirCallee::Builtin(iid) = inner.node.callee
        && matches!(
            builtins.get(iid.0 as usize).map(String::as_str),
            Some("Vector.set") | Some("Map.set")
        )
        && let Some(set_vec) = inner.node.args.first()
        && let MirExpr::Local(v) = &set_vec.node
        && let MirExpr::Local(d) = default_arg
    {
        return v.node.slot == d.node.slot;
    }
    false
}

/// Collect every slot referenced by a bare `MirExpr::Local` read.
fn collect_local_reads(e: &MirExpr, out: &mut HashSet<u32>) {
    if let MirExpr::Local(l) = e {
        out.insert(l.node.slot.0);
    }
    visit_children(e, &mut |c| collect_local_reads(c, out));
}

/// Interprocedural "fn captures param i" summary. `captures_param[f]`
/// holds the indices of `f`'s params whose value escapes into an
/// aggregate — directly in `f`, or transitively by being passed to a
/// callee that captures the corresponding param. Monotone-growing
/// fixpoint over the visible `Call(Fn)` edges; recursion / cycles are
/// safe because the set only grows. A param not in the summary is NOT a
/// proof of no-capture across unseen edges — but this pass already runs
/// only on whole, single-module programs (every caller visible), and
/// any genuinely externally-reachable fn (`main` / address-taken) keeps
/// all params flagged regardless.
fn compute_capture_summary(
    program: &MirProgram,
    provenance: &HashMap<FnId, HashMap<u32, Spanned<MirExpr>>>,
) -> HashMap<FnId, HashSet<usize>> {
    let builtins = &program.builtins;
    // Param-slot → param-index per fn (params occupy the leading slots,
    // but resolve by the declared `local` to be safe).
    let mut param_slot_to_idx: HashMap<FnId, HashMap<u32, usize>> = HashMap::new();
    for (id, f) in program.iter() {
        let m: HashMap<u32, usize> = f
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| (p.local.0, i))
            .collect();
        param_slot_to_idx.insert(*id, m);
    }

    let mut captures: HashMap<FnId, HashSet<usize>> = HashMap::new();
    loop {
        let mut changed = false;
        for (id, f) in program.iter() {
            let prov = provenance.get(id).cloned().unwrap_or_default();
            let nparams = f.params.len();
            // Slots that escape into an aggregate or a (already-known)
            // capturing callee within this fn body.
            let mut esc: HashSet<u32> = HashSet::new();
            collect_escaping_slots(&f.body.node, &prov, &captures, builtins, &mut esc);
            let idx_map = &param_slot_to_idx[id];
            let entry = captures.entry(*id).or_default();
            for &s in &esc {
                if (s as usize) < nparams
                    && let Some(&i) = idx_map.get(&s)
                    && entry.insert(i)
                {
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    captures
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
