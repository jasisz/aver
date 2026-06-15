//! Int "unboxing": pick a bare `i64` representation for provably-bounded,
//! non-escaping `Int` values so the Rust backend emits native integer
//! arithmetic instead of the default arbitrary-precision `aver_rt::AverInt`.
//!
//! This is a **read-only codegen analysis** — it never mutates the
//! `MirProgram`. It produces a [`BareI64Facts`] table the Rust walker reads
//! to select `i64` vs `AverInt` at each emit site, exactly the way the Rust
//! walker reads `aliased_slots` (the `own_param` pass) to select owned vs
//! borrowed collection params.
//!
//! ## What reuses the #511 interval domain
//!
//! The arithmetic-bound half reuses [`crate::ir::interval`] verbatim: the
//! `Interval` lattice element (i128-saturating, never wraps), its
//! `add`/`sub`/`mul`/`hull`/`fits_i64`, the `OpClass` verdict band, and the
//! `raw_i64_eligible` gate. We only swap the LEAF source — instead of a
//! refined-type carrier read, the leaf is an SSA-value (`LocalId`) range
//! query over the `MirFn` body.
//!
//! ## What reuses the existing escape / use-flow machinery
//!
//! The escape half mirrors the NOTION the `own_param` pass already uses (a
//! single use-flow scan over the body, defaulting every unrecognized
//! position to "escapes"), but with a REPRESENTATION-escape predicate
//! ("does this Int reach a general-Int context") rather than the ownership-
//! escape predicate ("does this collection leave the frame"). The call
//! graph used for the cross-frame summary is the `MirCallee::Fn` /
//! `MirTailCall` edge set, the same edges `own_param` walks.
//!
//! ## Soundness — fail-closed (the C0 guard)
//!
//! A value is `Bare` ONLY when PROVEN `raw_i64_eligible` (interval `Some`,
//! `fits_i64`, every participating op `OverflowFree`) AND non-escaping. Any
//! missing or unknown fact ⇒ `Boxed` (`AverInt`), never `Bare`. So a bug
//! here is a MISSED optimization (lost speed), never a wrong value — the
//! opposite of the wasm-gc silent-wrong-value risk. A wrongly-bare value
//! would reintroduce silent two's-complement wrapping, and additionally
//! the emitted Rust would not type-check (a bare value reaching an
//! `AverInt` slot is a `rustc` error), which is itself a backstop.

use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Literal, Spanned, Type};
use crate::ir::FnId;
use crate::ir::interval::{Interval, OpClass, raw_i64_eligible};

use super::super::expr::{MirCallee, MirExpr, MirPattern};
use super::super::program::{LocalId, MirFn, MirProgram};

/// Representation chosen for a value: a native machine `i64` or the
/// default arbitrary-precision `AverInt`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Repr {
    /// Native `i64` — emitted only when proven sound.
    Bare,
    /// `aver_rt::AverInt` — the safe default.
    Boxed,
}

/// Per-value representation-selection fact, keyed by `LocalId` within a
/// `MirFn`. `repr == Bare` ⟺ `raw_i64_eligible(interval, ops) && !escapes`.
#[derive(Debug, Clone)]
pub struct ValueFact {
    /// Derived interval over-approximation (`None` = the analysis could
    /// not derive a bound, the conservative decline).
    pub interval: Option<Interval>,
    /// Worst-case op class over every arithmetic node the value flows
    /// through. `OverflowFree` is the only band that may be `Bare`.
    pub op_class: OpClass,
    /// `true` when the value reaches a general-Int context (returned as a
    /// general Int, passed to a general-Int param, stored in an aggregate,
    /// or stringified). A `Bare` value never escapes by construction.
    pub escapes: bool,
    /// The chosen representation.
    pub repr: Repr,
}

impl ValueFact {
    /// The safe default — `Boxed`, unknown interval, declined.
    fn boxed() -> Self {
        Self {
            interval: None,
            op_class: OpClass::Unbounded,
            escapes: true,
            repr: Repr::Boxed,
        }
    }

    pub fn is_bare(&self) -> bool {
        self.repr == Repr::Bare
    }
}

/// Per-`MirFn` representation facts the Rust walker consumes.
#[derive(Debug, Clone, Default)]
pub struct FnBareFacts {
    /// Per-`LocalId` value fact. A slot absent from this map defaults to
    /// `Boxed` (fail-closed).
    pub values: HashMap<LocalId, ValueFact>,
    /// Per-param-index representation: `true` ⟺ the param is emitted as a
    /// bare `i64` in the fn signature (and every caller converts at the
    /// boundary). Indexed by declaration order, same indexing
    /// `own_param`'s `aliased_slots` uses.
    pub bare_params: Vec<bool>,
    /// `true` ⟺ the fn's return type is emitted as a bare `i64`.
    pub bare_return: bool,
}

impl FnBareFacts {
    /// Is the value bound to `slot` emitted as a bare `i64`?
    pub fn is_bare(&self, slot: LocalId) -> bool {
        self.values.get(&slot).is_some_and(ValueFact::is_bare)
    }

    /// Is param index `i` bare in the signature?
    pub fn param_is_bare(&self, i: usize) -> bool {
        self.bare_params.get(i).copied().unwrap_or(false)
    }
}

/// Whole-program representation-selection facts, keyed by `FnId`. Built
/// once per compilation alongside the optimized `MirProgram`; the Rust
/// backend reads a per-fn slice at signature + body emit.
#[derive(Debug, Clone, Default)]
pub struct BareI64Facts {
    fns: HashMap<FnId, FnBareFacts>,
}

impl BareI64Facts {
    /// Per-fn facts, or `None` when the fn is absent (fail-closed: the
    /// caller then treats every value as `Boxed`).
    pub fn for_fn(&self, id: FnId) -> Option<&FnBareFacts> {
        self.fns.get(&id)
    }

    /// Total values proven `Bare` across the whole program — a diagnostic
    /// counter (proof the recognizer fired), nothing in codegen keys off it.
    pub fn bare_values(&self) -> usize {
        self.fns
            .values()
            .flat_map(|f| f.values.values())
            .filter(|v| v.is_bare())
            .count()
    }
}

/// Entry point: compute the bare-`i64` representation facts for `program`.
///
/// The analysis is whole-program: a tail-recursion counter that crosses a
/// self-tail-call frame can only go bare if the param it crosses to is
/// ALSO bare, so the per-fn param/return summary is computed against the
/// visible `MirCallee::Fn` / `MirTailCall` call graph. A dependency-module
/// fragment (callers unseen) bails to all-`Boxed`, exactly like
/// `own_param`.
pub fn analyze(program: &MirProgram) -> BareI64Facts {
    // Diagnostic / bench-differential escape hatch: skip the analysis so a
    // run keeps the conservative all-Boxed baseline.
    if std::env::var("AVER_NO_BARE_I64").is_ok() {
        return BareI64Facts::default();
    }
    // Whole-program gate: a bare param/return changes a fn's Rust ABI, so
    // EVERY caller must be visible to convert at the boundary. A
    // dependency-module fragment is missing the entry/sibling call sites,
    // so bail to all-Boxed.
    if program.external_callers_possible || program.modules.len() > 1 {
        return BareI64Facts::default();
    }

    // Param Int-typedness per fn (a bare param must itself be `Int`).
    let mut int_params: HashMap<FnId, Vec<bool>> = HashMap::new();
    for (id, f) in program.iter() {
        int_params.insert(*id, f.params.iter().map(|p| ty_str_is_int(&p.ty)).collect());
    }

    // The minimal cross-frame summary: which params are bare, whether the
    // return is bare.
    let summary = compute_summary(program, &int_params);

    let mut fns: HashMap<FnId, FnBareFacts> = HashMap::new();
    for (id, f) in program.iter() {
        fns.insert(*id, analyze_fn(f, &summary));
    }

    BareI64Facts { fns }
}

/// Cross-frame summary: per-fn, which params are bare and whether the
/// return is bare. Computed as a monotone-descending fixpoint (a param /
/// return starts optimistically bare, demoted to boxed when any
/// constraint fails), mirroring `own_param`'s lattice.
struct Summary {
    bare_params: HashMap<FnId, Vec<bool>>,
    bare_return: HashMap<FnId, bool>,
}

impl Summary {
    fn param_bare(&self, id: FnId, i: usize) -> bool {
        self.bare_params
            .get(&id)
            .and_then(|v| v.get(i).copied())
            .unwrap_or(false)
    }

    fn return_bare(&self, id: FnId) -> bool {
        self.bare_return.get(&id).copied().unwrap_or(false)
    }
}

fn compute_summary(program: &MirProgram, int_params: &HashMap<FnId, Vec<bool>>) -> Summary {
    // Address-taken fns (name appears as a `FnValue`) have callers we
    // cannot attribute — pin all their params/return boxed.
    let mut address_taken: HashSet<String> = HashSet::new();
    for (_, f) in program.iter() {
        collect_fn_values(&f.body.node, &mut address_taken);
    }

    // Seed: optimistic. A param is candidate-bare iff it is Int-typed and
    // the fn is not externally reachable (main / address-taken). The
    // return is candidate-bare iff Int-typed and not externally reachable.
    let mut bare_params: HashMap<FnId, Vec<bool>> = HashMap::new();
    let mut bare_return: HashMap<FnId, bool> = HashMap::new();
    for (id, f) in program.iter() {
        let externally_reachable = f.name == "main" || address_taken.contains(&f.name);
        let ip = &int_params[id];
        let seed: Vec<bool> = ip.iter().map(|&i| i && !externally_reachable).collect();
        bare_params.insert(*id, seed);
        bare_return.insert(*id, ty_str_is_int(&f.return_type) && !externally_reachable);
    }

    // Collect visible call edges (`Call(Fn)` + `TailCall`) with their args.
    let mut edges: Vec<CallEdge> = Vec::new();
    for (caller, f) in program.iter() {
        collect_call_edges(*caller, &f.body.node, &mut edges);
    }

    // A fn's return may be bare only when it has at least one visible
    // NON-tail (ordinary `Call(Fn)`) caller — a fn with no such caller is
    // externally reachable (a library entry), so changing its return ABI
    // to `i64` would break an unseen caller. (A self-tail-call is the
    // recurrence edge, not an external consumer, so it does not count.)
    let mut has_ordinary_caller: HashSet<FnId> = HashSet::new();
    for edge in &edges {
        if !edge.is_tail_self() {
            has_ordinary_caller.insert(edge.target);
        }
    }
    for (id, br) in bare_return.iter_mut() {
        if !has_ordinary_caller.contains(id) {
            *br = false;
        }
    }

    loop {
        let mut changed = false;

        // (A) A bare callee param stays bare only if every caller can
        //     supply a bare/literal/bounded value at that position.
        for edge in &edges {
            let Some(callee_params) = bare_params.get(&edge.target).cloned() else {
                continue;
            };
            for (i, arg) in edge.args.iter().enumerate() {
                if !callee_params.get(i).copied().unwrap_or(false) {
                    continue; // already boxed — skip
                }
                let ok =
                    arg_supplies_bare(&arg.node, edge.caller, program, &bare_params, &bare_return);
                if !ok {
                    demote_param(&mut bare_params, edge.target, i, &mut changed);
                }
            }
        }

        // (B) The counter must remain in `i64` range across the recurrence
        //     and the literal-bounded entry. A param is bare only when its
        //     derived interval provably fits `i64`.
        for (id, f) in program.iter() {
            let ip = &int_params[id];
            for (i, &is_int) in ip.iter().enumerate() {
                if !bare_params[id].get(i).copied().unwrap_or(false) {
                    continue;
                }
                if !is_int {
                    demote_param(&mut bare_params, *id, i, &mut changed);
                    continue;
                }
                let bound = param_recurrence_bound(f, i, &edges);
                let eligible = bound.is_some_and(|iv| {
                    raw_i64_eligible(Some(iv), std::iter::once(&OpClass::OverflowFree))
                });
                if !eligible {
                    demote_param(&mut bare_params, *id, i, &mut changed);
                }
            }
        }

        // (B2) A bare PARAM must not ESCAPE in its own body. If the counter
        //      flows into a general-Int context (a boxed callee param, a
        //      builtin like `Float.fromInt(n)`, an aggregate, or a
        //      stringify), its representation must be `AverInt` — emitting
        //      a bare `i64` param whose body reads it through an `AverInt`
        //      method is a `rustc` type error (and a missed conversion).
        //      This couples the signature (driven by `bare_params`) to the
        //      body's escape predicate so the two never disagree. The
        //      escape set depends on the current bare-param seed (a bare
        //      callee param does not escape its arg), so recompute it inside
        //      the fixpoint; demotion is monotone.
        let tmp = Summary {
            bare_params: bare_params.clone(),
            bare_return: bare_return.clone(),
        };
        for (id, f) in program.iter() {
            let mut escaping: HashSet<LocalId> = HashSet::new();
            scan_escapes(&f.body.node, &tmp, &mut escaping);
            for (i, p) in f.params.iter().enumerate() {
                if bare_params[id].get(i).copied().unwrap_or(false) && escaping.contains(&p.local) {
                    demote_param(&mut bare_params, *id, i, &mut changed);
                }
            }
        }

        // (C) The return is bare only when the body's tail value is itself
        //     bare-eligible. Recompute the body facts against the current
        //     seed and demote when the body says the return cannot be bare.
        for (id, f) in program.iter() {
            if !bare_return.get(id).copied().unwrap_or(false) {
                continue;
            }
            let tmp = Summary {
                bare_params: bare_params.clone(),
                bare_return: bare_return.clone(),
            };
            let facts = analyze_fn(f, &tmp);
            if !facts.bare_return && bare_return.insert(*id, false) != Some(false) {
                changed = true;
            }
        }

        // (C2) A fn's return is bare only if EVERY caller consumes the
        //      result in an i64-safe position. A bare `i64` return value
        //      consumed in a general-Int context (an `AverInt`-method
        //      arithmetic operand, a boxed callee param, an aggregate
        //      field) would be a `rustc` type error and a missed
        //      conversion. We do NOT insert per-call-site return
        //      conversions in this slice, so the conservative rule is:
        //      mark `bare_return` only when every call result is discarded,
        //      stringified, or fed where a bare value is accepted (a bare
        //      param). Any other use demotes the callee's return. This is
        //      what keeps factorial/countdown (their results are discarded
        //      or stringified by `main`) bare while a return flowing into
        //      arithmetic stays boxed.
        let unsafe_returns = collect_unsafe_return_consumers(program, &bare_params);
        for id in &unsafe_returns {
            if bare_return.get(id).copied().unwrap_or(false)
                && bare_return.insert(*id, false) != Some(false)
            {
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }

    Summary {
        bare_params,
        bare_return,
    }
}

/// The set of callee `FnId`s whose return value is consumed in at least
/// one i64-UNSAFE position by some caller. A bare `i64` return reaching
/// such a position (an `AverInt` arithmetic operand, a boxed callee param,
/// an aggregate field, a stringify-incompatible context) would not
/// type-check, and this slice inserts no per-call-site return conversion —
/// so any unsafe consumer demotes the callee's `bare_return`.
///
/// SAFE consumers (do NOT demote): a discarded result (a `Let` whose
/// binding is never re-read as a boxed value — approximated as: the call
/// is the whole RHS and the binding is a fresh i64), a stringify
/// (`String.fromInt` / interpolation embed), the direct tail value of a
/// fn whose own return is bare, and an argument at a bare callee param
/// index. Everything else is unsafe.
fn collect_unsafe_return_consumers(
    program: &MirProgram,
    bare_params: &HashMap<FnId, Vec<bool>>,
) -> HashSet<FnId> {
    let mut unsafe_set: HashSet<FnId> = HashSet::new();
    for (_, f) in program.iter() {
        scan_return_consumers(&f.body.node, program, bare_params, &mut unsafe_set);
    }
    unsafe_set
}

/// Walk `e`, flagging every `Call(Fn(target))` whose RESULT lands in an
/// i64-unsafe position. A call reached as a direct child of a SAFE position
/// (a discard statement, a stringify, a bare param arg, a tail/return that
/// is itself bare) is not flagged; one reached anywhere else IS. We
/// approximate by walking each node and classifying the position of any
/// DIRECT `Call(Fn)` child; nested calls are handled when the walk reaches
/// their own parent.
fn scan_return_consumers(
    e: &MirExpr,
    program: &MirProgram,
    bare_params: &HashMap<FnId, Vec<bool>>,
    unsafe_set: &mut HashSet<FnId>,
) {
    // Helper: flag a direct `Call(Fn)` child as unsafe (its result is
    // consumed in a general-Int position here).
    let flag_if_call = |child: &MirExpr, unsafe_set: &mut HashSet<FnId>| {
        if let MirExpr::Call(c) = child
            && let MirCallee::Fn(t) = c.node.callee
        {
            unsafe_set.insert(t);
        }
    };

    match e {
        // Arithmetic / negation operands are i64-UNSAFE for a boxed result
        // (the boxed-path emit calls `AverInt` methods on the operand).
        MirExpr::BinOp(b) => {
            flag_if_call(&b.node.lhs.node, unsafe_set);
            flag_if_call(&b.node.rhs.node, unsafe_set);
        }
        MirExpr::Neg(inner) => flag_if_call(&inner.node, unsafe_set),
        // Aggregate fields/elements are general-Int contexts.
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for it in items {
                flag_if_call(&it.node, unsafe_set);
            }
        }
        MirExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                flag_if_call(&k.node, unsafe_set);
                flag_if_call(&v.node, unsafe_set);
            }
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                flag_if_call(&a.node, unsafe_set);
            }
        }
        MirExpr::RecordCreate(r) => {
            for fld in &r.node.fields {
                flag_if_call(&fld.value.node, unsafe_set);
            }
        }
        MirExpr::RecordUpdate(u) => {
            flag_if_call(&u.node.base.node, unsafe_set);
            for fld in &u.node.updates {
                flag_if_call(&fld.value.node, unsafe_set);
            }
        }
        // A call's args: a call result passed at a BOXED callee param index
        // is unsafe; at a BARE index it is safe (the param accepts i64). A
        // builtin/intrinsic/fn-value callee that takes Int args treats the
        // result as a general Int (e.g. `String.fromInt` reads the value) —
        // those are SAFE for stringify but UNSAFE for arithmetic builtins.
        // We conservatively treat only `String.fromInt` and interpolation
        // (handled below) as the stringify-safe sinks; every other builtin
        // Int arg is unsafe.
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Fn(target) => {
                for (i, a) in c.node.args.iter().enumerate() {
                    let bare_param = bare_params
                        .get(&target)
                        .and_then(|v| v.get(i).copied())
                        .unwrap_or(false);
                    if !bare_param {
                        flag_if_call(&a.node, unsafe_set);
                    }
                }
            }
            MirCallee::Builtin(bid) => {
                let name = program.builtin_name(bid);
                // `String.fromInt(x)` stringifies its arg — `x.to_string()`
                // works on both `i64` and `AverInt`, so a bare return here
                // is SAFE. Every other builtin reads the Int generally.
                if name != "String.fromInt" {
                    for a in &c.node.args {
                        flag_if_call(&a.node, unsafe_set);
                    }
                }
            }
            MirCallee::Intrinsic(_) | MirCallee::LocalSlot { .. } => {
                for a in &c.node.args {
                    flag_if_call(&a.node, unsafe_set);
                }
            }
        },
        // Interpolation embeds stringify their values — `to_string()` works
        // on `i64` too, so a call result there is SAFE (do not flag).
        // A `Let` whose VALUE is a call: the binding holds the result; we
        // cannot cheaply prove the binding is only re-read safely, so be
        // conservative and DO NOT flag here (the binding's later uses are
        // scanned as their own positions). This keeps the discard /
        // tail-return case bare while still flagging direct arithmetic /
        // aggregate / boxed-param uses above.
        _ => {}
    }

    // Recurse into all children to catch nested consumer positions.
    visit_children(e, &mut |c| {
        scan_return_consumers(c, program, bare_params, unsafe_set)
    });
}

fn demote_param(
    bare_params: &mut HashMap<FnId, Vec<bool>>,
    id: FnId,
    i: usize,
    changed: &mut bool,
) {
    if let Some(v) = bare_params.get_mut(&id)
        && let Some(slot) = v.get_mut(i)
        && *slot
    {
        *slot = false;
        *changed = true;
    }
}

/// A visible call edge `target(args…)` made from `caller`. `tail_self` is
/// `true` for a `MirTailCall` whose target is the caller itself (the
/// recurrence edge).
struct CallEdge {
    target: FnId,
    caller: FnId,
    args: Vec<Spanned<MirExpr>>,
    tail_self: bool,
}

impl CallEdge {
    fn is_tail_self(&self) -> bool {
        self.tail_self
    }
}

fn collect_call_edges(caller: FnId, e: &MirExpr, out: &mut Vec<CallEdge>) {
    match e {
        MirExpr::Call(c) => {
            if let MirCallee::Fn(target) = c.node.callee {
                out.push(CallEdge {
                    target,
                    caller,
                    args: c.node.args.clone(),
                    tail_self: false,
                });
            }
        }
        MirExpr::TailCall(tc) => {
            out.push(CallEdge {
                target: tc.node.target,
                caller,
                args: tc.node.args.clone(),
                tail_self: tc.node.target == caller,
            });
        }
        _ => {}
    }
    visit_children(e, &mut |c| collect_call_edges(caller, c, out));
}

/// Can the call argument `arg` (evaluated in `caller`) supply a bare /
/// bounded value at a bare callee param?
///
/// - A literal `Int` is always supplyable bare.
/// - The recurrence arithmetic `n - 1`, `acc * n`, `acc + n` … over bare
///   operands stays bare (the `i64`-bound is checked separately).
/// - A read of a caller param that is itself bare is supplyable.
/// - A call to another fn whose return is bare supplies bare.
/// - Anything else cannot be supplied bare ⇒ the callee param demotes.
fn arg_supplies_bare(
    arg: &MirExpr,
    caller: FnId,
    program: &MirProgram,
    bare_params: &HashMap<FnId, Vec<bool>>,
    bare_return: &HashMap<FnId, bool>,
) -> bool {
    match arg {
        MirExpr::Literal(l) => matches!(l.node, Literal::Int(_)),
        MirExpr::Neg(inner) => {
            arg_supplies_bare(&inner.node, caller, program, bare_params, bare_return)
        }
        MirExpr::Local(local) => local_supplies_bare(local.node.slot, caller, program, bare_params),
        MirExpr::BinOp(b) if matches!(b.node.op, BinOp::Add | BinOp::Sub | BinOp::Mul) => {
            arg_supplies_bare(&b.node.lhs.node, caller, program, bare_params, bare_return)
                && arg_supplies_bare(&b.node.rhs.node, caller, program, bare_params, bare_return)
        }
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Fn(target) => bare_return.get(&target).copied().unwrap_or(false),
            _ => false,
        },
        _ => false,
    }
}

/// Is the caller's slot `slot` a bare param? (Let-bound locals are not
/// tracked across frames in this minimal summary — they demote.)
fn local_supplies_bare(
    slot: LocalId,
    caller: FnId,
    program: &MirProgram,
    bare_params: &HashMap<FnId, Vec<bool>>,
) -> bool {
    let Some(f) = program.fn_by_id(caller) else {
        return false;
    };
    for (i, p) in f.params.iter().enumerate() {
        if p.local == slot {
            return bare_params
                .get(&caller)
                .and_then(|v| v.get(i).copied())
                .unwrap_or(false);
        }
    }
    false
}

/// Derive the recurrence bound for param index `i` of `f`: combine the
/// base-case guard (which bounds the counter on the decrement side) with
/// the literal entry values from non-recursive callers (which bound it on
/// the other side) and the monotone decrement.
///
/// Returns `Some(interval)` only when the param is provably confined to a
/// finite `i64` range; `None` is the conservative decline.
fn param_recurrence_bound(f: &MirFn, i: usize, edges: &[CallEdge]) -> Option<Interval> {
    let param_slot = f.params.get(i)?.local;

    // 1. The entry envelope: the convex hull of every literal value passed
    //    at param index `i` by a NON-recursive caller. If any non-literal
    //    value reaches the param, we cannot bound the entry — decline.
    let mut entry: Option<Interval> = None;
    let mut saw_entry = false;
    for edge in edges {
        if edge.target != f.fn_id || edge.is_tail_self() {
            continue;
        }
        saw_entry = true;
        let arg = edge.args.get(i)?;
        let iv = literal_interval(&arg.node)?;
        entry = Some(match entry {
            Some(prev) => prev.hull(iv),
            None => iv,
        });
    }
    if !saw_entry {
        // No non-recursive caller — we cannot bound the entry value.
        return None;
    }
    let entry = entry?;

    // 2. The base-case guard + monotone decrement recurrence at the same
    //    param index. Recognizes `match counter { K -> base; _ ->
    //    tailcall(counter - step, …) }` (and the lowered `IfThenElse`).
    let recur = recurrence_for_param(f, param_slot, i)?;

    // 3. Combine. The counter ranges between the base guard (minus the
    //    transient last step) and the entry envelope. For `n - 1` toward
    //    `0` with entry `[lo, hi]`, that is `[min(lo, K-step), max(hi, K)]`.
    let lo = entry.lo.min(Interval::point(recur.guard_k - recur.step).lo);
    let hi = entry.hi.max(Interval::point(recur.guard_k).hi);
    Some(Interval { lo, hi })
}

/// The recognized recurrence shape for a bounded counter.
struct Recurrence {
    /// The base-case guard literal `K` (`match counter { K -> … }`).
    guard_k: i128,
    /// The decrement step magnitude (`counter - step`, positive literal).
    step: i128,
}

/// Recognize the bounded-counter recurrence for param `param_slot` (index
/// `i`) in `f`'s body: a base-case guard on the counter and a monotone
/// `counter - step` recursive arg at the SAME index. Returns `None` for
/// any other shape (accumulators, non-monotone recurrences, missing
/// guard) — the conservative decline.
fn recurrence_for_param(f: &MirFn, param_slot: LocalId, i: usize) -> Option<Recurrence> {
    let guard_k = guard_literal_for(&f.body.node, param_slot)?;
    let mut recur: Option<Recurrence> = None;
    find_self_tailcall_recurrence(&f.body.node, f.fn_id, param_slot, i, guard_k, &mut recur);
    recur
}

/// Walk for a `Match`/`IfThenElse` that guards the counter against a
/// literal `K`. Returns the first literal guard `K` found on the counter.
fn guard_literal_for(e: &MirExpr, counter: LocalId) -> Option<i128> {
    match e {
        MirExpr::Match(m) => {
            if subject_is_local(&m.node.subject.node, counter) {
                for arm in &m.node.arms {
                    if let MirPattern::Literal(Literal::Int(k)) = &arm.pattern {
                        return Some(*k as i128);
                    }
                }
            }
            guard_literal_for(&m.node.subject.node, counter).or_else(|| {
                m.node
                    .arms
                    .iter()
                    .find_map(|arm| guard_literal_for(&arm.body.node, counter))
            })
        }
        MirExpr::IfThenElse(ite) => {
            if let MirExpr::BinOp(b) = &ite.node.cond.node
                && matches!(
                    b.node.op,
                    BinOp::Eq | BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte
                )
            {
                if subject_is_local(&b.node.lhs.node, counter)
                    && let MirExpr::Literal(l) = &b.node.rhs.node
                    && let Literal::Int(k) = l.node
                {
                    return Some(k as i128);
                }
                if subject_is_local(&b.node.rhs.node, counter)
                    && let MirExpr::Literal(l) = &b.node.lhs.node
                    && let Literal::Int(k) = l.node
                {
                    return Some(k as i128);
                }
            }
            guard_literal_for(&ite.node.cond.node, counter)
                .or_else(|| guard_literal_for(&ite.node.then_branch.node, counter))
                .or_else(|| guard_literal_for(&ite.node.else_branch.node, counter))
        }
        MirExpr::Let(l) => guard_literal_for(&l.node.value.node, counter)
            .or_else(|| guard_literal_for(&l.node.body.node, counter)),
        _ => {
            let mut found = None;
            visit_children(e, &mut |c| {
                if found.is_none() {
                    found = guard_literal_for(c, counter);
                }
            });
            found
        }
    }
}

/// Find a self-`TailCall` and check whether its arg at index `i` is a
/// monotone `counter - step` (decrement of a positive literal). Records the
/// [`Recurrence`] on the first match.
fn find_self_tailcall_recurrence(
    e: &MirExpr,
    self_fn: FnId,
    counter: LocalId,
    i: usize,
    guard_k: i128,
    out: &mut Option<Recurrence>,
) {
    if out.is_some() {
        return;
    }
    if let MirExpr::TailCall(tc) = e
        && tc.node.target == self_fn
        && let Some(arg) = tc.node.args.get(i)
        && let Some(step) = decrement_step(&arg.node, counter)
    {
        *out = Some(Recurrence { guard_k, step });
        return;
    }
    visit_children(e, &mut |c| {
        find_self_tailcall_recurrence(c, self_fn, counter, i, guard_k, out)
    });
}

/// If `e` is `counter - K` (a positive literal `K`), return `K`. The only
/// monotone-decrement shape we recognize today.
fn decrement_step(e: &MirExpr, counter: LocalId) -> Option<i128> {
    if let MirExpr::BinOp(b) = e
        && matches!(b.node.op, BinOp::Sub)
        && subject_is_local(&b.node.lhs.node, counter)
        && let MirExpr::Literal(l) = &b.node.rhs.node
        && let Literal::Int(k) = l.node
        && k > 0
    {
        return Some(k as i128);
    }
    None
}

fn subject_is_local(e: &MirExpr, slot: LocalId) -> bool {
    matches!(e, MirExpr::Local(l) if l.node.slot == slot)
}

/// Extract a bounded literal interval from a call argument (a bare `Int`
/// literal, or a negated one). `None` for any non-literal — the
/// conservative decline.
fn literal_interval(arg: &MirExpr) -> Option<Interval> {
    match arg {
        MirExpr::Literal(l) => match l.node {
            Literal::Int(k) => Some(Interval::point(k as i128)),
            _ => None,
        },
        MirExpr::Neg(inner) => match &inner.node {
            MirExpr::Literal(l) => match l.node {
                Literal::Int(k) => Some(Interval::point(-(k as i128))),
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

// ── Per-fn body analysis (range + escape) ───────────────────────────────

/// Compute the per-`LocalId` value facts for `f` given the cross-frame
/// `summary` (which params/returns are bare). The body walk derives an
/// interval per slot (bottom-up over the `Let` chain), an escape predicate
/// (a single use-flow scan), and combines them via `raw_i64_eligible`.
fn analyze_fn(f: &MirFn, summary: &Summary) -> FnBareFacts {
    let mut facts = FnBareFacts::default();

    // 1. Range: seed param intervals from the summary. A bare param is by
    //    construction confined to `i64`, so seed a full-`i64` interval;
    //    a boxed param is unbounded.
    let mut intervals: HashMap<LocalId, Interval> = HashMap::new();
    let mut bare_params = vec![false; f.params.len()];
    for (i, p) in f.params.iter().enumerate() {
        let is_bare = summary.param_bare(f.fn_id, i);
        bare_params[i] = is_bare;
        let iv = if is_bare {
            Interval::between(i64::MIN as i128, i64::MAX as i128)
        } else {
            Interval::unbounded()
        };
        intervals.insert(p.local, iv);
    }
    facts.bare_params = bare_params;

    // 2. Walk the body's `Let` chain, deriving an interval (+ worst-join
    //    op class) for each bound slot.
    let mut op_classes: HashMap<LocalId, OpClass> = HashMap::new();
    walk_let_chain(&f.body.node, &mut intervals, &mut op_classes);

    // 3. Escape: a single use-flow scan. A slot escapes if any use-site is
    //    a general-Int context.
    let mut escaping: HashSet<LocalId> = HashSet::new();
    scan_escapes(&f.body.node, summary, &mut escaping);

    // 4. Combine.
    for (slot, iv) in &intervals {
        let op_class = op_classes
            .get(slot)
            .copied()
            .unwrap_or(OpClass::OverflowFree);
        let escapes = escaping.contains(slot);
        let eligible = raw_i64_eligible(Some(*iv), std::iter::once(&op_class));
        let repr = if eligible && !escapes {
            Repr::Bare
        } else {
            Repr::Boxed
        };
        facts.values.insert(
            *slot,
            ValueFact {
                interval: Some(*iv),
                op_class,
                escapes,
                repr,
            },
        );
    }
    // Params absent from the Let walk still get a fact (their seed).
    for (i, p) in f.params.iter().enumerate() {
        let is_bare = summary.param_bare(f.fn_id, i);
        facts.values.entry(p.local).or_insert_with(|| {
            if is_bare {
                ValueFact {
                    interval: intervals.get(&p.local).copied(),
                    op_class: OpClass::OverflowFree,
                    escapes: false,
                    repr: Repr::Bare,
                }
            } else {
                ValueFact::boxed()
            }
        });
    }

    // 5. Return: bare iff the summary says so AND the body's tail value is
    //    itself bare-eligible.
    facts.bare_return =
        summary.return_bare(f.fn_id) && tail_value_is_bare(&f.body.node, &facts, &escaping);

    facts
}

/// Walk the `Let` chain, recording each bound slot's interval and worst-
/// join op class. `env` holds param + already-bound intervals and is grown
/// in place; branches recurse so nested bindings are seen.
fn walk_let_chain(
    e: &MirExpr,
    env: &mut HashMap<LocalId, Interval>,
    op_classes: &mut HashMap<LocalId, OpClass>,
) {
    match e {
        MirExpr::Let(l) => {
            let mut worst = Interval::point(0);
            let iv = eval_interval(&l.node.value.node, env, &mut worst);
            env.insert(l.node.binding, iv);
            // The binding's op class is the worst-join over its value
            // expression (so a transient out-of-i64 intermediate demotes).
            op_classes.insert(l.node.binding, OpClass::of_interval(worst.hull(iv)));
            walk_let_chain(&l.node.value.node, env, op_classes);
            walk_let_chain(&l.node.body.node, env, op_classes);
        }
        _ => {
            visit_children(e, &mut |c| walk_let_chain(c, env, op_classes));
        }
    }
}

/// Abstractly evaluate a MIR expression to its interval, joining each
/// arithmetic node into `worst` (so a transient out-of-i64 intermediate
/// demotes the binding). Unknown shapes evaluate to `unbounded()`.
fn eval_interval(e: &MirExpr, env: &HashMap<LocalId, Interval>, worst: &mut Interval) -> Interval {
    match e {
        MirExpr::Literal(l) => match l.node {
            Literal::Int(k) => Interval::point(k as i128),
            _ => Interval::unbounded(),
        },
        MirExpr::Local(local) => env
            .get(&local.node.slot)
            .copied()
            .unwrap_or_else(Interval::unbounded),
        MirExpr::Neg(inner) => {
            let r = Interval::point(0).sub(eval_interval(&inner.node, env, worst));
            *worst = worst.hull(r);
            r
        }
        MirExpr::BinOp(b) => {
            let l = eval_interval(&b.node.lhs.node, env, worst);
            let r = eval_interval(&b.node.rhs.node, env, worst);
            let result = match b.node.op {
                BinOp::Add => l.add(r),
                BinOp::Sub => l.sub(r),
                BinOp::Mul => l.mul(r),
                _ => Interval::unbounded(),
            };
            *worst = worst.hull(result);
            result
        }
        _ => Interval::unbounded(),
    }
}

/// Single use-flow escape scan. A slot escapes if it (a) is passed to a
/// general (boxed) Int param of a callee, (b) is stored in an aggregate,
/// or (c) is stringified. We over-approximate: any unrecognized use of a
/// slot defaults to escaping.
fn scan_escapes(e: &MirExpr, summary: &Summary, out: &mut HashSet<LocalId>) {
    match e {
        // Aggregates: every Int element/field escapes.
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for it in items {
                mark_operand_escapes(&it.node, out);
                scan_escapes(&it.node, summary, out);
            }
        }
        MirExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                mark_operand_escapes(&k.node, out);
                mark_operand_escapes(&v.node, out);
                scan_escapes(&k.node, summary, out);
                scan_escapes(&v.node, summary, out);
            }
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                mark_operand_escapes(&a.node, out);
                scan_escapes(&a.node, summary, out);
            }
        }
        MirExpr::RecordCreate(r) => {
            for fld in &r.node.fields {
                mark_operand_escapes(&fld.value.node, out);
                scan_escapes(&fld.value.node, summary, out);
            }
        }
        MirExpr::RecordUpdate(u) => {
            mark_operand_escapes(&u.node.base.node, out);
            scan_escapes(&u.node.base.node, summary, out);
            for fld in &u.node.updates {
                mark_operand_escapes(&fld.value.node, out);
                scan_escapes(&fld.value.node, summary, out);
            }
        }
        MirExpr::IndependentProduct(ip) => {
            for it in &ip.node.items {
                mark_operand_escapes(&it.node, out);
                scan_escapes(&it.node, summary, out);
            }
        }
        // Stringification: every embedded value escapes.
        MirExpr::InterpolatedStr(parts) => {
            for p in parts {
                if let super::super::expr::MirStrPart::Expr(ex) = p {
                    mark_operand_escapes(&ex.node, out);
                    scan_escapes(&ex.node, summary, out);
                }
            }
        }
        // A call: an arg at a BOXED callee-param index escapes; an arg at a
        // BARE param index converts at the boundary and does NOT escape. A
        // builtin / intrinsic / fn-value callee is conservative — every Int
        // arg escapes (we don't model builtin param reps).
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Fn(target) => {
                for (i, a) in c.node.args.iter().enumerate() {
                    if !summary.param_bare(target, i) {
                        mark_operand_escapes(&a.node, out);
                    }
                    scan_escapes(&a.node, summary, out);
                }
            }
            _ => {
                for a in &c.node.args {
                    mark_operand_escapes(&a.node, out);
                    scan_escapes(&a.node, summary, out);
                }
            }
        },
        // A tail call: same, against the (self) callee's bare params.
        MirExpr::TailCall(tc) => {
            for (i, a) in tc.node.args.iter().enumerate() {
                if !summary.param_bare(tc.node.target, i) {
                    mark_operand_escapes(&a.node, out);
                }
                scan_escapes(&a.node, summary, out);
            }
        }
        // Control flow + arithmetic: recurse. A bare Local in tail/return
        // or arithmetic-operand position is consumed bare (no escape).
        _ => {
            visit_children(e, &mut |c| scan_escapes(c, summary, out));
        }
    }
}

/// Mark the slot of a value that flows DIRECTLY into an escaping position.
///
/// Only a bare `Local` read placed verbatim at a general-Int context
/// escapes — its representation must then be `AverInt` (the value itself
/// crosses the boundary). An ARITHMETIC expression (`acc * n`) at an
/// escaping position does NOT escape its operands: the operands are
/// consumed to compute a FRESH result, and that result (a new temp) is
/// what crosses the boundary; each operand is converted at the arithmetic
/// site (`boxed_int_operand` wraps a bare operand with `from_i64`) and
/// keeps its own representation. So a counter read inside `acc * n` that
/// feeds a boxed accumulator must NOT be flagged — flagging it was the bug
/// that demoted the factorial counter. Hence: do not recurse into
/// `BinOp` / `Neg`.
fn mark_operand_escapes(e: &MirExpr, out: &mut HashSet<LocalId>) {
    if let MirExpr::Local(l) = e {
        out.insert(l.node.slot);
    }
}

/// Does the fn's tail value evaluate to a bare-eligible value? The tail is
/// the base-case value(s) reached after the `Let` chain / branches. We
/// require every reachable tail leaf to be a bare-eligible Local or a bare
/// literal; anything else demotes the return.
fn tail_value_is_bare(e: &MirExpr, facts: &FnBareFacts, escaping: &HashSet<LocalId>) -> bool {
    match e {
        MirExpr::Local(l) => facts.is_bare(l.node.slot) && !escaping.contains(&l.node.slot),
        MirExpr::Literal(l) => matches!(l.node, Literal::Int(_)),
        MirExpr::Let(let_node) => tail_value_is_bare(&let_node.node.body.node, facts, escaping),
        MirExpr::Match(m) => m
            .node
            .arms
            .iter()
            .all(|arm| tail_value_is_bare(&arm.body.node, facts, escaping)),
        MirExpr::IfThenElse(ite) => {
            tail_value_is_bare(&ite.node.then_branch.node, facts, escaping)
                && tail_value_is_bare(&ite.node.else_branch.node, facts, escaping)
        }
        // A self-tail-call's value is the recurrence, not a base value; it
        // does not constrain the return repr.
        MirExpr::TailCall(_) => true,
        MirExpr::Return(inner) => tail_value_is_bare(&inner.node, facts, escaping),
        MirExpr::BinOp(b) if matches!(b.node.op, BinOp::Add | BinOp::Sub | BinOp::Mul) => {
            tail_value_is_bare(&b.node.lhs.node, facts, escaping)
                && tail_value_is_bare(&b.node.rhs.node, facts, escaping)
        }
        _ => false,
    }
}

// ── shared traversal + helpers ──────────────────────────────────────────

fn collect_fn_values(e: &MirExpr, out: &mut HashSet<String>) {
    if let MirExpr::FnValue(name) = e {
        out.insert(name.clone());
    }
    visit_children(e, &mut |c| collect_fn_values(c, out));
}

/// `true` when the source type-annotation string is exactly `Int`.
fn ty_str_is_int(ty: &str) -> bool {
    ty == "Int"
}

/// `true` when the `Type` stamp is exactly `Int`. Exposed for the Rust
/// walker's consumption sites.
pub fn type_is_int(ty: Option<&Type>) -> bool {
    matches!(ty, Some(Type::Int))
}

/// Apply `f` to every immediate sub-expression of `e`. Kept in sync with
/// the exhaustive walk in `own_param.rs` / `instantiations.rs`.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::mir::{lower_program, optimize};
    use crate::source::parse_source;

    fn facts_for(src: &str) -> (MirProgram, BareI64Facts) {
        let mut items = parse_source(src).expect("parse");
        let cfg = crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        };
        let result = crate::ir::pipeline::run(&mut items, cfg);
        assert!(
            result
                .typecheck
                .as_ref()
                .is_none_or(|t| t.errors.is_empty()),
            "typecheck errors: {:?}",
            result.typecheck.map(|t| t.errors)
        );
        let mir_items: Vec<crate::ir::hir::ResolvedTopLevel> = result.resolved_items.clone();
        let program = optimize(lower_program(&mir_items));
        let facts = analyze(&program);
        (program, facts)
    }

    fn fn_id_by_name<'a>(program: &'a MirProgram, name: &str) -> crate::ir::FnId {
        program
            .iter()
            .find(|(_, f)| f.name == name)
            .map(|(id, _)| *id)
            .unwrap_or_else(|| panic!("fn `{name}` not in program"))
    }

    #[test]
    fn countdown_counter_is_bare() {
        let src = r#"
module Countdown
    intent = "t"
    depends []

fn countdown(n: Int) -> Int
    match n
        0 -> 0
        _ -> countdown(n - 1)

fn main() -> Int
    countdown(20000)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "countdown");
        let f = facts.for_fn(id).expect("countdown facts");
        assert!(
            f.param_is_bare(0),
            "the countdown counter must be proven bare i64"
        );
    }

    #[test]
    fn factorial_counter_is_bare() {
        let src = r#"
module Factorial
    intent = "t"
    depends []

fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)

fn main() -> Int
    factorial(10, 1)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "factorial");
        let f = facts.for_fn(id).expect("factorial facts");
        assert!(
            f.param_is_bare(0),
            "the factorial counter `n` must be proven bare i64"
        );
    }

    #[test]
    fn unbounded_param_stays_boxed() {
        // A fn called with a non-literal arg cannot bound its counter, so
        // the param must stay boxed (fail-closed).
        let src = r#"
module M
    intent = "t"
    depends []

fn down(n: Int) -> Int
    match n
        0 -> 0
        _ -> down(n - 1)

fn caller(x: Int) -> Int
    down(x)

fn main() -> Int
    caller(5)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "down");
        let f = facts.for_fn(id).expect("down facts");
        // `down` is called with `x` (a non-literal param of `caller`), and
        // `caller`'s `x` comes from a literal — but the minimal summary
        // only bounds DIRECT literal callers, so `down`'s counter is not
        // provably bounded here and must stay boxed.
        assert!(
            !f.param_is_bare(0),
            "a counter reached via a non-literal arg must stay boxed (fail-closed)"
        );
    }

    #[test]
    fn non_recursive_fn_param_stays_boxed() {
        // A non-recursive fn has no recurrence to bound its param; with no
        // base-case guard the counter is unbounded ⇒ boxed (fail-closed).
        let src = r#"
module M
    intent = "t"
    depends []

fn twice(n: Int) -> Int
    n + n

fn main() -> Int
    twice(10)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "twice");
        let f = facts.for_fn(id).expect("twice facts");
        assert!(
            !f.param_is_bare(0),
            "a param with no bounding recurrence must stay boxed"
        );
    }

    #[test]
    fn counter_stored_in_aggregate_demotes_via_escape() {
        // A bounded counter whose value is STORED in a list escapes (reaches
        // a general-Int aggregate), so the analysis must not mark the
        // value bare. We assert the body's escape predicate flags it: a
        // counter read into `[n]` is a general-Int aggregate store.
        let src = r#"
module M
    intent = "t"
    depends []

fn collect(n: Int) -> List<Int>
    match n
        0 -> [0]
        _ -> [n]

fn main() -> List<Int>
    collect(10)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "collect");
        let f = facts.for_fn(id).expect("collect facts");
        // The return type is `List<Int>`, not `Int`, so the return is never
        // bare; and `n` flows into the `[n]` aggregate, an escape. The
        // param must stay boxed.
        assert!(
            !f.param_is_bare(0),
            "a counter stored in a List aggregate escapes → must stay boxed"
        );
        assert!(!f.bare_return, "a List<Int> return is never bare i64");
    }

    #[test]
    fn worst_join_demotes_transient_out_of_i64_intermediate() {
        // The reused #511 worst-join discipline: `(n + i64::MAX) - i64::MAX`
        // cancels back into range, but the transient `n + i64::MAX`
        // overflows i64, so the binding's op class must NOT be
        // `OverflowFree`. Exercise the `eval_interval` + worst-join the body
        // pass uses, over a bare-param-seeded `n ∈ [0, 10]`.
        let mut env = HashMap::new();
        let n = LocalId(0);
        env.insert(n, Interval::between(0, 10));
        let big = i64::MAX as i128;

        // Build the MIR for `(n + MAX) - MAX`.
        let lit = |k: i128| Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Int(k as i64))));
        let local_n = Spanned::bare(MirExpr::Local(Spanned::bare(
            super::super::super::expr::MirLocal::at(n),
        )));
        let add = Spanned::bare(MirExpr::BinOp(Spanned::bare(
            super::super::super::expr::MirBinOp {
                op: BinOp::Add,
                lhs: Box::new(local_n),
                rhs: Box::new(lit(big)),
            },
        )));
        let sub = MirExpr::BinOp(Spanned::bare(super::super::super::expr::MirBinOp {
            op: BinOp::Sub,
            lhs: Box::new(add),
            rhs: Box::new(lit(big)),
        }));

        let mut worst = Interval::point(0);
        let result = eval_interval(&sub, &env, &mut worst);
        // The final value cancels back into [0, 10] (fits i64) …
        assert!(result.fits_i64(), "final value cancels back into range");
        // … but the worst-join saw the transient `n + i64::MAX` (out of i64),
        // so the op class over the whole expression is NOT OverflowFree.
        assert_ne!(
            OpClass::of_interval(worst.hull(result)),
            OpClass::OverflowFree,
            "the transient out-of-i64 intermediate must demote below OverflowFree"
        );
    }
}
