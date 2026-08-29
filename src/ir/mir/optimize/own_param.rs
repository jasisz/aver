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
//! This pass refines that decision interprocedurally and clears a
//! Vector/Map param's `aliased_slots` bit — graduating it to the
//! owned/in-place fast path — only when the clear is **sound by
//! construction**.
//!
//! ## Sound-by-construction: default-flag, clear-on-proof
//!
//! Two adversarial audits found five distinct corruption classes by
//! enumerating escape/capture *sites*; each audit found a site the
//! enumeration missed (direct capture, alias-rename chains, cross-fn
//! capture, live-alias, a param fed as the VALUE/ELEMENT/DEFAULT arg of
//! `Vector.set`/`Map.set`/`Vector.new`/a map literal). A blacklist of
//! escape sites is unsound by omission: any unforeseen site is a silent
//! corruption (a backend mutates shared data in place).
//!
//! For shared-arena backends the polarity is flipped. The default for every
//! collection param is **flagged** (kept aliased — never mutated in place).
//! The bit is cleared only when a *positive proof* succeeds, i.e. when
//! **every** occurrence of the param `P` — and of every let-local that may
//! share `P`'s backing (its alias closure) — is one of these RECOGNIZED
//! non-retaining-linear uses:
//!
//! - `P` (or an alias) as the TARGET/receiver arg (arg 0) of a
//!   mutating/reading collection builtin (`Vector.set`/`get`/`len`,
//!   `Map.set`/`get`/`has`/`remove`/`keys`/`values`/`len`/`entries`) —
//!   these consume/read the target, they do not retain it beyond the
//!   result that replaces it;
//! - the self-keep fusion `Option.withDefault(Vector.set(P,..), P)` /
//!   `Map.set` — the fused `VECTOR_SET_OR_KEEP` consumes `P` once and
//!   returns one of its two handles;
//! - `P` (or an alias) in the fn's tail/return position — `P` moves out
//!   as the result (linear);
//! - `P` (or an alias) threaded to a callee at a param index that an
//!   interprocedural summary PROVES the callee never retains.
//!
//! ANY other occurrence is a POTENTIAL ESCAPE → `P` stays flagged:
//! `P` (or an alias) as a NON-target argument (value/element/default/key
//! arg) of ANY builtin, a field/element of ANY aggregate
//! (record / record-update / variant / tuple / list / map literal /
//! independent product), an argument to ANY user fn that the summary
//! does not clear, or an alias that is read while `P` is still live. The
//! scan defaults every *unrecognized* position to retaining, so it
//! cannot miss an escape — the audits converge.
//!
//! The body-local proof above is one half; the other is the interprocedural
//! call-site proof, computed as a monotone-descending fixpoint. An arena param
//! graduates only when every visible call site passes uniquely-owned storage.
//! The Rust refinement asks the narrower ABI question instead: can every call
//! site provide an owned `T`? Rust's emitter clones a non-final owning use,
//! moves a final one, and its collection mutators enforce COW, so a retained
//! sibling handle may cost a real copy but cannot make the move incorrect.
//! Default is keep-flagged; un-flagging is the gated exception in both models.
//!
//! ## Soundness gates
//!
//! - Runs only on a whole-program `MirProgram` (`modules` empty / one):
//!   a multi-module fragment can have callers we can't see, and a missed
//!   aliased caller is unsound. Multi-module is skipped wholesale.
//! - `main`/entry and any address-taken fn (its name appears in a
//!   `MirExpr::FnValue`) keep all params flagged — their callers are not
//!   all visible `Call`/`TailCall` edges.
//! - A call site reached through `MirCallee::LocalSlot` (a fn value) has
//!   no statically-known target, so it never counts as a visible caller;
//!   such fns are already pinned via the address-taken set.
//! - An arena argument is uniquely-owned only when proven so. A named user-fn
//!   result qualifies only through a return-alias summary that identifies
//!   every parameter backing it may carry; a fn-value result stays unknown.
//!   Arena backends also reject a local with a live caller-side alias. Rust
//!   may still move that local: its owned mutators enforce COW at runtime, so
//!   an actual sibling handle remains correct without the borrowed ABI adding
//!   yet another handle.
//! - On generated Rust, a last-use pattern binding is movable by construction:
//!   matches with bindings materialize their subject by value, and the binding
//!   owns the extracted value. Its collection backing may still be shared,
//!   but the Rust runtime preserves COW semantics; moving the binding avoids
//!   adding the extra owner that a borrowed callee parameter would create.
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
use crate::ir::hir::BuiltinIntrinsic;

use super::super::expr::{MirCallee, MirExpr, MirPattern, walk_children};
use super::super::program::MirProgram;

mod return_alias;
use return_alias::{ReturnAliasSummary, compute_return_alias_summary};

/// Recursion cap for the alias / ownership walks — defends against a
/// pathological let-provenance chain; exceeding it resolves to the
/// conservative answer (not-owned / leaking).
const MAX_DEPTH: u32 = 64;

/// Collection builtins whose arg 0 is the TARGET/receiver: a
/// non-retaining read/consume of the collection. `P` appearing there
/// does not escape — the builtin reads or replaces the target and its
/// result (which may itself alias the target) is checked separately
/// wherever it lands. Args at index >= 1 are value/element/key/default
/// positions and ARE retaining.
///
/// `Vector.new` / `Map.new` / `Vector.fromList` / `Map.fromList` are
/// deliberately absent: they CONSTRUCT a collection, so they have no
/// target arg — their collection-valued args (the `Vector.new` default
/// cell, the `fromList` list elements) are retaining and must flag.
fn is_target_consuming_builtin(name: &str) -> bool {
    matches!(
        name,
        "Vector.set"
            | "Vector.get"
            | "Vector.len"
            | "Map.set"
            | "Map.get"
            | "Map.has"
            | "Map.remove"
            | "Map.keys"
            | "Map.values"
            | "Map.len"
            | "Map.entries"
    )
}

/// A single visible call edge: `target(args…)` made from `caller`.
struct CallSite {
    target: FnId,
    caller: FnId,
    args: Vec<Spanned<MirExpr>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum OwnershipModel {
    /// Arena entries remain holders after a wrapper is destructured, so a
    /// returned `Result.Ok(map)` cannot grant unique slot ownership.
    SharedArenaSafe,
    /// Rust consumes and drops return wrappers at `?` / projection boundaries.
    RustDropAware,
}

impl OwnershipModel {
    fn returned_aggregates_are_consumed(self) -> bool {
        self == Self::RustDropAware
    }

    fn owned_carriers_are_cow_protected(self) -> bool {
        self == Self::RustDropAware
    }
}

pub fn own_param_refine(program: MirProgram) -> MirProgram {
    own_param_refine_for_model(program, OwnershipModel::SharedArenaSafe)
}

/// Rust-only second refinement. Call after the shared optimizer: it may clear
/// additional Map/Vector param flags when ownership crosses a returned
/// aggregate that Rust consumes and drops (`Result<Map, E>` followed by `?`).
/// VM/wasm must never call this — arena entries remain observable holders even
/// after logical unwrapping.
pub fn own_param_refine_for_rust(program: MirProgram) -> MirProgram {
    own_param_refine_for_model(program, OwnershipModel::RustDropAware)
}

fn own_param_refine_for_model(mut program: MirProgram, model: OwnershipModel) -> MirProgram {
    // Diagnostic / bench-differential escape hatch: skip the refinement
    // so a run keeps the conservative all-params-aliased baseline.
    if std::env::var("AVER_NO_OWN_PARAM").is_ok() {
        return program;
    }
    // Whole-program gate: graduating a param to owned is only sound when
    // EVERY caller is visible in this program. A dependency-module
    // fragment (the VM compiles each `depends [...]` module separately)
    // is missing the entry/sibling call sites, so a param could be marked
    // owned here while an unseen caller passes an aliased value — an
    // in-place mutation would then corrupt the caller's value. Bail.
    // (`program.modules` was the intended signal but is not yet populated
    // by lowering; `external_callers_possible` is set explicitly by the
    // VM's per-dep-module build.)
    if program.external_callers_possible || program.modules.len() > 1 {
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

    // Per-fn slot → binding-RHS provenance (from the body's `Let`s). The
    // alias walks (`alias_roots` / `rename_roots`) resolve a value back
    // through its alias chain (a `let w = v` rename, a `match`-bound
    // rename, a passthrough-fn result) to the originating slot.
    let mut provenance: HashMap<FnId, HashMap<u32, Spanned<MirExpr>>> = HashMap::new();
    for (id, f) in program.iter() {
        let mut m = HashMap::new();
        collect_let_bindings(&f.body.node, &mut m);
        provenance.insert(*id, m);
    }

    // Rust match emission materializes every subject that has bindings by
    // value (`mir_clone_arg`) before destructuring it. Consequently each
    // pattern local is an owned Rust value, even when its `Rc` backing still
    // aliases another aggregate component. A last-use read of such a local is
    // movable into an owned callee parameter without manufacturing the extra
    // handle that today's borrowed ABI creates. Arena backends cannot use this
    // fact: their destructured wrapper/tuple entries remain observable holders.
    let mut rust_owned_pattern_slots: HashMap<FnId, HashSet<u32>> = HashMap::new();
    if model.owned_carriers_are_cow_protected() {
        for (id, f) in program.iter() {
            let mut slots = HashSet::new();
            collect_pattern_bound_slots(&f.body.node, &mut slots);
            if !slots.is_empty() {
                rust_owned_pattern_slots.insert(*id, slots);
            }
        }
    }

    // Interprocedural capture summary: `captures_param[f] = { i | param
    // i of f is NOT proven non-retaining in f's body — i.e. some
    // occurrence of param i is not a recognized linear use, OR it is
    // threaded to a callee that itself captures }`. This is the dual of
    // the body-local proof: a param the proof clears does NOT retain its
    // arg, so a caller passing a value at that index does not escape
    // through it. Monotone-growing fixpoint over the call graph; a
    // captured param can never be safely fed an owned value, so this
    // drives the cross-fn escape detection (a caller slot flowing into a
    // capturing callee param escapes in the caller too).
    let builtins = program.builtins.clone();
    let return_aliases = compute_return_alias_summary(&program, &provenance, &builtins, model);
    let captures_param = if model.owned_carriers_are_cow_protected() {
        HashMap::new()
    } else {
        compute_capture_summary(&program, &provenance, &builtins, model)
    };

    // --- BODY-LOCAL PROOF -------------------------------------------------
    //
    // For each arena-backend fn, compute the set of slots that LEAK in its body: slots
    // that appear (directly or through an alias chain) in any position
    // the whitelist does NOT recognize as non-retaining-linear. The scan
    // is positive — it walks every operand position, marks the
    // recognized-safe ones, and treats every other (and every position
    // it does not explicitly recognize) as retaining. A bare `Local` in
    // a retaining position contributes its alias roots to the leak set.
    //
    // An arena param slot in the leak set MUST stay flagged: some occurrence of
    // it (or of a value sharing its backing) is retained beyond a linear
    // consume, so an in-place mutation would corrupt the retained handle.
    // This single scan subsumes the old `collect_escaping_slots`
    // blacklist (aggregate capture, value-into-collection,
    // cross-fn capture) by construction — default is leak.
    //
    // The live-alias class (`snap = v; … set(v,…); read snap`) is a
    // distinct shape: the param appears only in recognized-safe
    // positions, yet a still-live RENAME alias observes the pre-mutation
    // backing. `collect_live_aliased_params` pins those separately.
    //
    // Generated Rust deliberately skips both pins. Graduation there means
    // "every call site can provide an owned `T`", not "the backing has no
    // other handle". MIR liveness makes non-final owning uses clone, final
    // uses move, and Map/Vector mutation crosses a COW gate. A retained alias
    // can therefore force a real copy but cannot observe in-place corruption;
    // keeping the borrowed ABI would only add another handle and can never
    // improve that outcome. This distinction is the core of issue #1196.
    let mut leaking: HashMap<FnId, HashSet<u32>> = HashMap::new();
    if !model.owned_carriers_are_cow_protected() {
        for (id, f) in program.iter() {
            let prov = provenance.get(id).cloned().unwrap_or_default();
            let mut leak: HashSet<u32> = HashSet::new();
            scan_body_leaks(
                model,
                &f.body.node,
                &prov,
                &captures_param,
                &builtins,
                &mut leak,
            );
            let nparams = f.params.len();
            collect_live_aliased_params(&f.body.node, &prov, &builtins, nparams, &mut leak);
            if !leak.is_empty() {
                leaking.insert(*id, leak);
            }
        }
    }

    // Apply the body-local leaks to `aliased_slots` (so a leaked PARAM
    // slot stays flagged) and record which PARAM slots leaked, so the
    // call-site fixpoint can never un-flag them.
    let mut leaked_param_slots: HashMap<FnId, HashSet<usize>> = HashMap::new();
    for (id, f) in program.fns.iter_mut() {
        let Some(leak) = leaking.get(id) else {
            continue;
        };
        if leak.is_empty() {
            continue;
        }
        let nparams = f.params.len();
        let mut slots = f.aliased_slots.as_ref().clone();
        if let Some(&max) = leak.iter().max()
            && (max as usize) >= slots.len()
        {
            slots.resize(max as usize + 1, false);
        }
        let mut leaked_params: HashSet<usize> = HashSet::new();
        for &s in leak {
            slots[s as usize] = true;
            if (s as usize) < nparams {
                leaked_params.insert(s as usize);
            }
        }
        if !leaked_params.is_empty() {
            leaked_param_slots.insert(*id, leaked_params);
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

    // Per-fn set of let-bound slots that have a still-live RENAME alias
    // of another slot — used by `uniquely_owned` to reject a call-site
    // arg whose slot the CALLER still observes through a live alias (the
    // live-alias-at-callsite class). Computed once: a slot `s` is in
    // `live_aliased[caller]` when some read let-binding renames `s`.
    let mut live_aliased: HashMap<FnId, HashSet<u32>> = HashMap::new();
    for (id, f) in program.iter() {
        let prov = provenance.get(id).cloned().unwrap_or_default();
        let mut set: HashSet<u32> = HashSet::new();
        collect_live_aliased_slots(&f.body.node, &prov, &builtins, &mut set);
        if !set.is_empty() {
            live_aliased.insert(*id, set);
        }
    }

    // Fixpoint lattice: owned[(fn, param_idx)] for alias-prone params.
    // Init optimistic true; pinned fns start (and stay) false. A param
    // slot that LEAKED in its body (leaked_param_slots) also starts
    // false — its backing is retained somewhere, so it must never be
    // owned-mutated in place. The lattice is monotone-descending, so a
    // `false` seed stays `false`.
    let mut owned: HashMap<(FnId, usize), bool> = HashMap::new();
    for (id, idxs) in &prone {
        let pin = pinned.contains(id);
        let leaked = leaked_param_slots.get(id);
        for &i in idxs {
            let body_leak = leaked.is_some_and(|c| c.contains(&i));
            owned.insert((*id, i), !pin && !body_leak);
        }
    }

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
            let caller_live_aliased = live_aliased.get(&cs.caller);
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
                // Live-alias-at-callsite: arena backends must reject an owned
                // local if the caller still observes it through a rename
                // alias (`alias = base; mutate(base); read alias`) because
                // their fast path mutates shared storage directly. Generated
                // Rust may move the last-use handle: `Rc::make_mut` / the
                // collection's equivalent COW gate preserves the caller's
                // alias and the move avoids adding one gratuitous handle.
                let caller_aliased = !model.owned_carriers_are_cow_protected()
                    && matches!(&arg.node, MirExpr::Local(l)
                        if caller_live_aliased.is_some_and(|s| s.contains(&l.node.slot.0)));
                let argument_owned = uniquely_owned(
                    &arg.node,
                    cs.caller,
                    &program,
                    &owned,
                    &provenance,
                    &rust_owned_pattern_slots,
                    &return_aliases,
                    model,
                    &builtins,
                    0,
                );
                let ok = !dup && !caller_aliased && argument_owned;
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

/// Can `e`, evaluated in `caller`, supply the model's owned argument?
/// Arena storage must be unique; generated Rust needs a movable owned carrier
/// because its mutators retain COW protection. Default false.
#[allow(clippy::too_many_arguments)]
fn uniquely_owned(
    e: &MirExpr,
    caller: FnId,
    program: &MirProgram,
    owned: &HashMap<(FnId, usize), bool>,
    provenance: &HashMap<FnId, HashMap<u32, Spanned<MirExpr>>>,
    rust_owned_pattern_slots: &HashMap<FnId, HashSet<u32>>,
    return_aliases: &ReturnAliasSummary,
    model: OwnershipModel,
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
                    // `Map.fromList(pairs)` and `Vector.fromList(xs)` build
                    // their collection from scratch and hand back either the
                    // immediate empty value or a slot nothing else has an
                    // index to (`from_list_nv` in `src/types/map.rs`,
                    // `vec_from_list_nv` in `src/types/vector.rs`: both return
                    // `EMPTY_*` or a freshly pushed index), so the result is
                    // as fresh as a literal. Missing that fact was what made
                    // `fold(n, Map.fromList([]))` — the only way to spell an
                    // empty accumulator where no literal is in reach — copy
                    // the whole map on every insert (issue #900); the vector
                    // spelling sat in the same gap. This says nothing about
                    // the argument: a collection passed *into* `fromList` is
                    // retained by the result, which is why neither builtin is
                    // in `is_target_consuming_builtin`.
                    "Map.fromList" | "Vector.fromList" => true,
                    // set returns its (mutated) vector/map — owned iff the
                    // target is owned.
                    "Vector.set" | "Map.set" | "Map.remove" => {
                        c.node.args.first().is_some_and(|v| {
                            uniquely_owned(
                                &v.node,
                                caller,
                                program,
                                owned,
                                provenance,
                                rust_owned_pattern_slots,
                                return_aliases,
                                model,
                                builtins,
                                depth + 1,
                            )
                        })
                    }
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
                                    rust_owned_pattern_slots,
                                    return_aliases,
                                    model,
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
                            rust_owned_pattern_slots,
                            return_aliases,
                            model,
                            builtins,
                            depth + 1,
                        ) && uniquely_owned(
                            &c.node.args[1].node,
                            caller,
                            program,
                            owned,
                            provenance,
                            rust_owned_pattern_slots,
                            return_aliases,
                            model,
                            builtins,
                            depth + 1,
                        )
                    }
                    // Vector.get / Map.get return an alias into the source;
                    // every other builtin result is not provably owned.
                    _ => false,
                }
            }
            // A statically named function is owned exactly when every
            // parameter its result may carry was supplied uniquely. The
            // summary is clear-on-proof: unknown bodies/cycles have no
            // entry and remain conservative. This is the helper-return
            // bridge from issue #890.
            MirCallee::Fn(target) => return_aliases.get(target).is_some_and(|sources| {
                sources.iter().all(|&source| {
                    c.node.args.get(source).is_some_and(|arg| {
                        uniquely_owned(
                            &arg.node,
                            caller,
                            program,
                            owned,
                            provenance,
                            rust_owned_pattern_slots,
                            return_aliases,
                            model,
                            builtins,
                            depth + 1,
                        )
                    })
                })
            }),
            // Literal-discharge Vector.new still allocates fresh backing. As
            // with the surface builtin, a compound fill is shared by every
            // cell and therefore cannot grant deep collection ownership.
            MirCallee::Intrinsic(BuiltinIntrinsic::VectorNew) => c
                .node
                .args
                .get(1)
                .is_some_and(|fill| matches!(&fill.node, MirExpr::Literal(_))),
            // A first-class fn value has no statically-known body. Its result
            // may alias any argument, so it never grants ownership. Other
            // intrinsics do not return a fresh Map/Vector.
            MirCallee::LocalSlot { .. } | MirCallee::Intrinsic(_) => false,
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
                    rust_owned_pattern_slots,
                    return_aliases,
                    model,
                    builtins,
                    depth + 1,
                )
        }
        MirExpr::Try(inner) if model.returned_aggregates_are_consumed() => uniquely_owned(
            &inner.node,
            caller,
            program,
            owned,
            provenance,
            rust_owned_pattern_slots,
            return_aliases,
            model,
            builtins,
            depth + 1,
        ),
        _ => false,
    }
}

/// Can slot `s` (in `caller`) supply the model's owned binding, IGNORING the
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
    rust_owned_pattern_slots: &HashMap<FnId, HashSet<u32>>,
    return_aliases: &ReturnAliasSummary,
    model: OwnershipModel,
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
    if !is_param
        && model.owned_carriers_are_cow_protected()
        && rust_owned_pattern_slots
            .get(&caller)
            .is_some_and(|slots| slots.contains(&slot))
    {
        return true;
    }
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
            rust_owned_pattern_slots,
            return_aliases,
            model,
            builtins,
            depth + 1,
        ),
        None => false,
    }
}

/// Collect every fn name referenced as a value (`MirExpr::FnValue`).
fn collect_fn_values(e: &MirExpr, out: &mut HashSet<String>) {
    walk_children(e, &mut |c| collect_fn_values(c, out));
    if let MirExpr::FnValue(name) = e {
        out.insert(name.clone());
    }
}

/// Collect every local introduced by a match pattern. Generated Rust matches
/// by value whenever a pattern binds, so these slots contain owned values.
/// Nested tuple patterns and constructor payloads are flattened recursively;
/// list patterns expose both their owned head and owned tail locals.
fn collect_pattern_bound_slots(e: &MirExpr, out: &mut HashSet<u32>) {
    if let MirExpr::Match(m) = e {
        for arm in &m.node.arms {
            collect_pattern_slots(&arm.pattern, out);
        }
    }
    walk_children(e, &mut |child| collect_pattern_bound_slots(child, out));
}

fn collect_pattern_slots(pattern: &MirPattern, out: &mut HashSet<u32>) {
    match pattern {
        MirPattern::Bind(local, _) => {
            out.insert(local.0);
        }
        MirPattern::Cons { head, tail, .. } => {
            out.insert(head.0);
            out.insert(tail.0);
        }
        MirPattern::Tuple(items) => {
            for item in items {
                collect_pattern_slots(item, out);
            }
        }
        MirPattern::Ctor { bindings, .. } => {
            out.extend(bindings.iter().map(|local| local.0));
        }
        MirPattern::Wildcard | MirPattern::Literal(_) | MirPattern::EmptyList => {}
    }
}

/// Collect `slot → binding-RHS` for every `Let` in the body.
fn collect_let_bindings(e: &MirExpr, out: &mut HashMap<u32, Spanned<MirExpr>>) {
    if let MirExpr::Let(l) = e {
        out.entry(l.node.binding.0)
            .or_insert_with(|| (*l.node.value).clone());
    }
    walk_children(e, &mut |c| collect_let_bindings(c, out));
}

/// The set of source slots whose backing the value of `e` may share —
/// the "alias roots" of `e`. Storing `e` into an aggregate, or passing
/// it where it can be retained, shares the backing of every slot in
/// this set, so none of them may be mutated in place afterwards.
///
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
            // Literal-discharge Vector.new owns fresh outer backing.
            MirCallee::Intrinsic(BuiltinIntrinsic::VectorNew) => {}
            // A user fn / fn-value / other intrinsic result may alias any of
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

fn scan_body_leaks(
    model: OwnershipModel,
    e: &MirExpr,
    prov: &HashMap<u32, Spanned<MirExpr>>,
    captures_param: &HashMap<FnId, HashSet<usize>>,
    builtins: &[String],
    out: &mut HashSet<u32>,
) {
    if model.returned_aggregates_are_consumed() {
        scan_result_leaks(e, prov, captures_param, builtins, out);
    } else {
        scan_leaks(e, prov, captures_param, builtins, out);
    }
}

/// Rust-only scan of a function-result position. Values placed in a returned
/// aggregate move out of the function and the wrapper is dropped when Rust
/// destructures it. The return-alias summary carries the obligation into the
/// caller. Arena backends must use `scan_leaks`: their wrapper entries remain
/// holders after logical destructuring.
fn scan_result_leaks(
    e: &MirExpr,
    prov: &HashMap<u32, Spanned<MirExpr>>,
    captures_param: &HashMap<FnId, HashSet<usize>>,
    builtins: &[String],
    out: &mut HashSet<u32>,
) {
    match e {
        MirExpr::Let(l) => {
            scan_leaks(&l.node.value.node, prov, captures_param, builtins, out);
            scan_result_leaks(&l.node.body.node, prov, captures_param, builtins, out);
        }
        MirExpr::Match(m) => {
            scan_leaks(&m.node.subject.node, prov, captures_param, builtins, out);
            for arm in &m.node.arms {
                scan_result_leaks(&arm.body.node, prov, captures_param, builtins, out);
            }
        }
        MirExpr::IfThenElse(ite) => {
            scan_leaks(&ite.node.cond.node, prov, captures_param, builtins, out);
            scan_result_leaks(
                &ite.node.then_branch.node,
                prov,
                captures_param,
                builtins,
                out,
            );
            scan_result_leaks(
                &ite.node.else_branch.node,
                prov,
                captures_param,
                builtins,
                out,
            );
        }
        MirExpr::Return(inner)
        | MirExpr::Try(inner)
        | MirExpr::Box(inner)
        | MirExpr::Unbox(inner) => {
            scan_result_leaks(&inner.node, prov, captures_param, builtins, out)
        }
        MirExpr::Construct(construct) => {
            for arg in &construct.node.args {
                scan_result_leaks(&arg.node, prov, captures_param, builtins, out);
            }
        }
        MirExpr::RecordCreate(record) => {
            for field in &record.node.fields {
                scan_result_leaks(&field.value.node, prov, captures_param, builtins, out);
            }
        }
        MirExpr::RecordUpdate(update) => {
            scan_result_leaks(&update.node.base.node, prov, captures_param, builtins, out);
            for field in &update.node.updates {
                scan_result_leaks(&field.value.node, prov, captures_param, builtins, out);
            }
        }
        MirExpr::Tuple(items) | MirExpr::List(items) => {
            for item in items {
                scan_result_leaks(&item.node, prov, captures_param, builtins, out);
            }
        }
        MirExpr::MapLiteral(pairs) => {
            for (key, value) in pairs {
                scan_result_leaks(&key.node, prov, captures_param, builtins, out);
                scan_result_leaks(&value.node, prov, captures_param, builtins, out);
            }
        }
        // Independent products retain branch inputs concurrently. All other
        // expressions already classify their operand roles in the regular
        // positive scan.
        _ => scan_leaks(e, prov, captures_param, builtins, out),
    }
}

/// Positive (whitelist) leak scan. Walks every operand position of the
/// fn body and adds to `out` the alias roots of every value that lands
/// in a RETAINING position — i.e. any position not recognized as a
/// non-retaining-linear use. This is the sound-by-construction core: the
/// default for an unrecognized position is "retaining" (leak), so no
/// escape site can be silently missed.
///
/// Recognized-safe (non-retaining) positions:
/// - the tail/return position (a bare `Local` moves out as the result —
///   handled implicitly: a `Local` reached as a whole sub-expression of
///   a safe-scanned position is a no-op leaf, so a move-out never leaks);
/// - arg 0 of a target-consuming collection builtin
///   (`is_target_consuming_builtin`);
/// - both operands of `Option.withDefault` (it SELECTS and returns one
///   handle, retaining nothing — TRANSPARENT), which subsumes the
///   self-keep fusion `withDefault(set(P,..), P)`;
/// - an arg threaded to a user fn at a param index the interprocedural
///   capture summary PROVES the callee does not retain (not in
///   `captures_param`).
///
/// Everything else — aggregate fields/elements, non-target builtin args,
/// constructor args, the default cell of `Vector.new`, args to an
/// unproven callee, interpolation operands — is retaining.
fn scan_leaks(
    e: &MirExpr,
    prov: &HashMap<u32, Spanned<MirExpr>>,
    captures_param: &HashMap<FnId, HashSet<usize>>,
    builtins: &[String],
    out: &mut HashSet<u32>,
) {
    // A child in a RETAINING position: record the alias roots of any
    // value that flows there, then keep scanning it for nested leaks.
    let mut retain = |child: &Spanned<MirExpr>, out: &mut HashSet<u32>| {
        alias_roots(&child.node, prov, builtins, 0, out);
        scan_leaks(&child.node, prov, captures_param, builtins, out);
    };

    match e {
        // --- Control flow: tail-transparent. A bare `Local` value
        //     produced by any of these positions moves out as the
        //     fn/branch result (linear), so it is NOT retained — scanning
        //     it as a non-retaining read is correct (a Local leaf is a
        //     no-op). Only the SUBJECT / bound-VALUE positions consume a
        //     value to drive the control flow; they are likewise reads. ---
        MirExpr::Let(l) => {
            scan_leaks(&l.node.value.node, prov, captures_param, builtins, out);
            scan_leaks(&l.node.body.node, prov, captures_param, builtins, out);
        }
        MirExpr::Match(m) => {
            scan_leaks(&m.node.subject.node, prov, captures_param, builtins, out);
            for arm in &m.node.arms {
                scan_leaks(&arm.body.node, prov, captures_param, builtins, out);
            }
        }
        MirExpr::IfThenElse(ite) => {
            scan_leaks(&ite.node.cond.node, prov, captures_param, builtins, out);
            scan_leaks(
                &ite.node.then_branch.node,
                prov,
                captures_param,
                builtins,
                out,
            );
            scan_leaks(
                &ite.node.else_branch.node,
                prov,
                captures_param,
                builtins,
                out,
            );
        }
        MirExpr::Return(inner)
        | MirExpr::Try(inner)
        | MirExpr::Box(inner)
        | MirExpr::Unbox(inner) => {
            scan_leaks(&inner.node, prov, captures_param, builtins, out);
        }

        // --- Calls: classify each arg position. ---
        MirExpr::Call(c) => match &c.node.callee {
            MirCallee::Builtin(bid) => {
                let name = builtins
                    .get(bid.0 as usize)
                    .map(String::as_str)
                    .unwrap_or("");
                if name == "Option.withDefault" {
                    // `withDefault(a, b)` SELECTS and returns one of its
                    // two handles — it retains NOTHING beyond the value it
                    // surfaces. So it is TRANSPARENT: scan both operands as
                    // non-retaining reads. Whether the selected value
                    // escapes is decided at the withDefault RESULT's
                    // position (the parent), and a still-live aliased
                    // branch is caught by the live-alias pin (which
                    // propagates through `withDefault` in `rename_roots`).
                    // This subsumes the self-keep fusion
                    // `withDefault(set(P,..), P)`: the inner `set` flags
                    // its own value/index args, `P` at the set's arg 0 and
                    // as the default are both safe reads, so the linear
                    // param does not leak.
                    for a in &c.node.args {
                        scan_leaks(&a.node, prov, captures_param, builtins, out);
                    }
                    return;
                }
                // Target-consuming builtins read/replace arg 0 (the
                // receiver) without retaining it; args >= 1 are
                // value/element/key positions and DO retain. A
                // constructor (`Vector.new` / `Map.new` / `*.fromList`)
                // has no target arg — every collection-valued arg
                // retains, so `target_safe = false`.
                let target_safe = is_target_consuming_builtin(name);
                scan_builtin_args(
                    c,
                    target_safe,
                    prov,
                    captures_param,
                    builtins,
                    &mut retain,
                    out,
                );
            }
            MirCallee::Fn(target) => {
                let captured = captures_param.get(target);
                for (i, a) in c.node.args.iter().enumerate() {
                    // An arg at an index the callee does NOT retain is
                    // safe (the callee threads it linearly); otherwise it
                    // escapes into the callee. The summary is a
                    // least-fixpoint (starts empty = nothing captured,
                    // grows monotonically), so a param ABSENT from the
                    // summary is treated as non-retaining — sound once the
                    // fixpoint reaches it, and it is the only direction
                    // that lets a linear self-recursive param converge.
                    if captured.is_some_and(|c| c.contains(&i)) {
                        retain(a, out);
                    } else {
                        // Provably non-retaining param: safe, scan deeper.
                        scan_leaks(&a.node, prov, captures_param, builtins, out);
                    }
                }
            }
            MirCallee::LocalSlot { .. } | MirCallee::Intrinsic(_) => {
                // No statically-known target / summary — every arg may
                // be retained. Default to retaining.
                for a in &c.node.args {
                    retain(a, out);
                }
            }
        },

        // --- Tail calls: same-SCC threading. An arg at a param index
        //     the callee does NOT retain threads linearly (the fast
        //     path); otherwise it escapes into the callee. ---
        MirExpr::TailCall(tc) => {
            let captured = captures_param.get(&tc.node.target);
            for (i, a) in tc.node.args.iter().enumerate() {
                // Same least-fixpoint semantics as `Call(Fn)`: an arg at
                // a param index NOT in the (monotone-growing) capture
                // summary threads linearly (the fast path); one at a
                // captured index escapes into the callee.
                if captured.is_some_and(|c| c.contains(&i)) {
                    retain(a, out);
                } else {
                    scan_leaks(&a.node, prov, captures_param, builtins, out);
                }
            }
        }

        // --- Aggregates: every field / element / value is retaining. ---
        MirExpr::RecordCreate(r) => {
            for f in &r.node.fields {
                retain(&f.value, out);
            }
        }
        MirExpr::RecordUpdate(u) => {
            retain(&u.node.base, out);
            for f in &u.node.updates {
                retain(&f.value, out);
            }
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                retain(a, out);
            }
        }
        MirExpr::Tuple(items) | MirExpr::List(items) => {
            for i in items {
                retain(i, out);
            }
        }
        MirExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                retain(k, out);
                retain(v, out);
            }
        }
        MirExpr::IndependentProduct(ip) => {
            for i in &ip.node.items {
                retain(i, out);
            }
        }

        // --- Scalar / structural operands: a collection value flowing
        //     into these is consumed for a scalar result, never retained;
        //     recurse to find nested leaks. ---
        MirExpr::BinOp(b) => {
            scan_leaks(&b.node.lhs.node, prov, captures_param, builtins, out);
            scan_leaks(&b.node.rhs.node, prov, captures_param, builtins, out);
        }
        MirExpr::Neg(inner) => scan_leaks(&inner.node, prov, captures_param, builtins, out),
        MirExpr::Project(p) => {
            // `base.field` reads a field; the projected value may itself
            // alias `base`'s backing, but `base` is not retained by the
            // projection — recurse to scan `base`.
            scan_leaks(&p.node.base.node, prov, captures_param, builtins, out)
        }
        MirExpr::InterpolatedStr(parts) => {
            for p in parts {
                if let super::super::expr::MirStrPart::Expr(e) = p {
                    // Stringification reads the value (non-retaining).
                    scan_leaks(&e.node, prov, captures_param, builtins, out);
                }
            }
        }

        // --- Leaves. A bare `Local` reaches here only when its parent
        //     scanned it as a non-retaining position (a safe builtin
        //     receiver / `withDefault` operand / control-flow result / a
        //     scalar operand) — a retaining parent records its roots via
        //     `retain` BEFORE recursing. So a `Local` reaching this arm is
        //     a move-out / read that retains nothing: nothing to add. ---
        MirExpr::Local(_) | MirExpr::Literal(_) | MirExpr::FnValue(_) => {}
    }
}

/// Scan the args of a builtin call where `target_safe` says arg 0 is the
/// non-retaining receiver. Arg 0 (if `target_safe`) is scanned for
/// nested leaks only; every other arg is retaining.
#[allow(clippy::too_many_arguments)]
fn scan_builtin_args(
    c: &Spanned<super::super::expr::MirCall>,
    target_safe: bool,
    prov: &HashMap<u32, Spanned<MirExpr>>,
    captures_param: &HashMap<FnId, HashSet<usize>>,
    builtins: &[String],
    retain: &mut dyn FnMut(&Spanned<MirExpr>, &mut HashSet<u32>),
    out: &mut HashSet<u32>,
) {
    for (i, a) in c.node.args.iter().enumerate() {
        if i == 0 && target_safe {
            // Receiver: consumed/read, not retained. Scan for nested
            // leaks (e.g. a `set(set(P,..),..)` chain) but do not record
            // P here.
            scan_leaks(&a.node, prov, captures_param, builtins, out);
        } else {
            retain(a, out);
        }
    }
}

/// Pin every PARAM that has a still-live alias: a let-bound slot whose
/// binding RHS aliases the param (its `rename_roots` contains the param
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
        // problematically live — that is the linear fast path.
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

/// Every slot that has a still-live RENAME alias in the body: a let-bound
/// slot whose binding RHS renames it AND that is read. Used at the
/// CALL-SITE to reject an owned arg whose slot the caller still observes
/// through an alias (the live-alias-at-callsite class). Unlike
/// `collect_live_aliased_params` (which records the PARAM that is
/// aliased), this records the ROOT slot that an alias still observes —
/// any slot, param or let-local — so a call-site arg reading that slot
/// can be rejected.
fn collect_live_aliased_slots(
    body: &MirExpr,
    prov: &HashMap<u32, Spanned<MirExpr>>,
    builtins: &[String],
    out: &mut HashSet<u32>,
) {
    let mut read: HashSet<u32> = HashSet::new();
    collect_local_reads(body, &mut read);
    for (&binding, rhs) in prov {
        if !read.contains(&binding) {
            continue;
        }
        let mut roots: HashSet<u32> = HashSet::new();
        rename_roots(&rhs.node, prov, builtins, 0, &mut roots);
        for &r in &roots {
            // The binding `r != binding` so the source slot is co-live
            // with a distinct alias binding that is still read.
            if r != binding {
                out.insert(r);
            }
        }
    }
}

/// Param slots that `e` is a *pure value-copy alias* of — the subset of
/// `alias_roots` that keeps the source slot CO-LIVE rather than
/// consuming it. Used only by the live-alias-across-mutation pins; the
/// capture / cross-fn escape paths use the full `alias_roots` semantics.
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
            // Literal-discharge Vector.new is a fresh successor, not a
            // co-live alias of any input collection.
            MirCallee::Intrinsic(BuiltinIntrinsic::VectorNew) => {}
            // A user-fn / fn-value / other intrinsic result may alias an arg
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
/// recognition in `uniquely_owned`; used by `scan_leaks` / `rename_roots`
/// to treat the successor as consuming (not aliasing) `s`.
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
    walk_children(e, &mut |c| collect_local_reads(c, out));
}

/// Interprocedural "fn captures param i" summary. `captures_param[f]`
/// holds the indices of `f`'s params that are NOT proven
/// non-retaining-linear in `f`'s body — i.e. some occurrence escapes
/// (the body-local leak scan flags the param slot), directly or
/// transitively by being passed to a callee that captures the
/// corresponding param. Monotone-growing fixpoint over the visible
/// `Call(Fn)` / `TailCall` edges; recursion / cycles are safe because
/// the set only grows.
///
/// This is exactly the dual the `scan_leaks` whitelist consumes: a param
/// NOT in `captures_param[f]` is proven not to retain its arg, so a
/// caller passing a value at that index does not escape through `f`. A
/// param not yet in the summary during the fixpoint is treated as
/// capturing (the seed is empty and grows), which is the conservative
/// direction. The whole pass already runs only on whole, single-module
/// programs (every caller visible), and any genuinely
/// externally-reachable fn (`main` / address-taken) keeps all params
/// flagged regardless.
fn compute_capture_summary(
    program: &MirProgram,
    provenance: &HashMap<FnId, HashMap<u32, Spanned<MirExpr>>>,
    builtins: &[String],
    model: OwnershipModel,
) -> HashMap<FnId, HashSet<usize>> {
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
            // Slots that leak (escape a recognized linear use) in this fn
            // body given the current (monotone-growing) summary, plus
            // live-alias pins. A param leaking ⇒ it captures its arg.
            let mut leak: HashSet<u32> = HashSet::new();
            scan_body_leaks(model, &f.body.node, &prov, &captures, builtins, &mut leak);
            if !model.owned_carriers_are_cow_protected() {
                collect_live_aliased_params(&f.body.node, &prov, builtins, nparams, &mut leak);
            }
            let idx_map = &param_slot_to_idx[id];
            let entry = captures.entry(*id).or_default();
            for &s in &leak {
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
    walk_children(e, &mut |c| collect_call_sites(caller, c, out));
}

#[cfg(test)]
mod tests {
    use super::own_param_refine;
    use crate::ast::{Literal, Spanned};
    use crate::ir::FnId;
    use crate::ir::mir::expr::MirExpr;
    use crate::ir::mir::program::{LocalId, MirFn, MirParam, MirProgram};
    use std::sync::Arc;

    fn span<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            line: 0,
            ty: std::sync::OnceLock::new(),
        }
    }

    /// A one-fn program: `f(v: Vector<Int>)` with `v` flagged
    /// alias-prone, a body that does not retain `v`, and NO callers.
    /// With every caller visible (a whole-program compile) own_param
    /// graduates `v` to owned and clears the flag. As a dependency-module
    /// fragment (`external_callers_possible`) an unseen cross-module
    /// caller could pass an aliased value, so the guard must bail and
    /// keep `v` flagged.
    fn one_flagged_param_program(external_callers_possible: bool) -> (MirProgram, FnId) {
        let id = FnId(0);
        let mut p = MirProgram {
            external_callers_possible,
            ..MirProgram::default()
        };
        p.fns.insert(
            id,
            MirFn {
                fn_id: id,
                name: "f".to_string(),
                params: vec![MirParam {
                    local: LocalId(0),
                    name: "v".to_string(),
                    ty: "Vector<Int>".to_string(),
                }],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(MirExpr::Literal(span(Literal::Int(0)))),
                local_count: 1,
                aliased_slots: Arc::new(vec![true]),
                repr: crate::ir::mir::program::MirFnRepr::default(),
            },
        );
        (p, id)
    }

    #[test]
    fn whole_program_graduates_unaliased_param() {
        // Guards against a vacuous dependency-fragment test: confirm the
        // param really WOULD graduate when all callers are visible.
        let (prog, id) = one_flagged_param_program(false);
        let out = own_param_refine(prog);
        assert!(
            !out.fns[&id].aliased_slots[0],
            "whole-program: a non-retaining, uncalled collection param must graduate to owned"
        );
    }

    #[test]
    fn dependency_fragment_keeps_param_flagged() {
        let (prog, id) = one_flagged_param_program(true);
        let out = own_param_refine(prog);
        assert!(
            out.fns[&id].aliased_slots[0],
            "dependency fragment: own_param must bail — an unseen cross-module caller may alias the param"
        );
    }
}
