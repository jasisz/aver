//! ETAP-2 SLICE 1 — the Int-representation MIR→MIR rewrite.
//!
//! Turns the [`super::bare_i64`] range+escape ANALYSIS (a side table) into
//! EXPLICIT representation in the MIR: it tags each fn's bare slots /
//! params / return on [`MirFn::repr`](crate::ir::mir::MirFn) and inserts
//! [`MirExpr::Box`] / [`MirExpr::Unbox`] boundary nodes at every position
//! where a raw `i64` value crosses into an `Int` (`AverInt`) context or
//! vice-versa. After this pass, the Rust backend lowers TRIVIALLY: a bare
//! slot reads native `i64`, raw arithmetic stays raw, and a `Box`/`Unbox`
//! node is the only place a boundary conversion is emitted — codegen no
//! longer DECIDES where boundaries go, so the boundary-completeness bug
//! class (PR #519) becomes structurally impossible.
//!
//! ## Where it runs
//!
//! LATE and Rust-codegen-ONLY. The shared `optimize()` pipeline (consumed
//! by the VM, wasm-gc, proof, Dafny, Lean) does NOT run this — those keep
//! the all-`Int` representation. The Rust backend applies it to a CLONE of
//! the optimized MIR at the point it codegens. Running after proof export
//! and shape recognition is mandatory: both key on the syntactic structure
//! this rewrite rewrites.
//!
//! ## Equivalence to the prior side-table codegen
//!
//! The rewrite inserts a boundary node at EXACTLY the positions the old
//! `from_mir.rs` inserted a conversion (the 5 sites: boxed-arithmetic
//! operand, call-arg at a boxed/bare callee param, a boxed `Let` crossing,
//! a boxed-return tail leaf), reusing the SAME [`FnBareFacts`] predicates
//! (`is_bare`, `expr_is_bare_i64`, `param_is_bare`, `bare_return`). So the
//! emitted Rust is behavior-equivalent: the conversions land in the same
//! places, just decided ONCE here instead of re-derived per emit site.
//!
//! ## Fail-closed
//!
//! Pure function of the analysis output. A value is raw only where the
//! analysis proved it `Bare`; a missing/unknown fact ⇒ boxed. A wrongly
//! raw value would still fail to type-check in the emitted Rust (raw `i64`
//! into an `AverInt` slot is `E0308`), the same backstop as before.

use crate::ast::{BinOp, Literal, Spanned};
use crate::ir::mir::program::LocalId;
use crate::ir::mir::{
    BareI64Facts, FnBareFacts, MirCallee, MirExpr, MirFn, MirFnRepr, MirProgram, MirStrPart,
};

/// Rewrite `program` so Int representation is explicit (ETAP-2). Runs the
/// existing range+escape analysis, then for every fn populates `repr` and
/// inserts `Box`/`Unbox` boundary nodes. Pure: returns a fresh program;
/// the caller (the Rust backend) feeds it the clone it codegens from.
///
/// `boxed_signature_fns` lists fns the Rust backend emits with an
/// unconditionally BOXED (`AverInt`) signature regardless of the analysis —
/// today the mutual-TCO members, whose trampoline always returns `AverInt`.
/// The analysis can still flag such a fn's return/params bare (a phantom the
/// codegen overrides), so the rewrite must treat them as boxed everywhere:
/// it leaves their `repr` empty (body stays all-`Int`) AND treats their
/// params/return as boxed at every CALL site, so a caller neither raw-reads
/// nor double-boxes a value the callee actually returns as `AverInt`.
pub fn rewrite_for_rust(
    program: MirProgram,
    boxed_signature_fns: &std::collections::HashSet<crate::ir::FnId>,
    _carrier: &super::bare_i64::CarrierIntervals,
) -> MirProgram {
    // ETAP-2 carrier-`i64` is a WASM-GC-ONLY size lever: the wasm-gc registry
    // erases an eligible carrier's storage to a native `i64` everywhere, so a
    // carrier value IS an i64 and `c.value` can stay raw. The RUST backend
    // keeps the carrier as a real struct (`IntRange { value: AverInt }`), so a
    // raw-i64 carrier path would emit invalid Rust (a `.to_i64()` on a struct,
    // an `i64` where the field is `AverInt`). The Int-recurrence bareness
    // (countdown / factorial counters) is target-agnostic and stays on for
    // Rust; only the carrier-projection leaf is suppressed — pass an EMPTY
    // carrier table so `carrier_interval` declines every carrier.
    let empty = super::bare_i64::CarrierIntervals::new();
    rewrite(program, boxed_signature_fns, &empty)
}

/// ETAP-2 SLICE 2b: the wasm-gc entry. Identical body to
/// [`rewrite_for_rust`] (the rewrite itself is target-agnostic — it only
/// reads the [`super::bare_i64`] analysis output), differing ONLY in who
/// computes `boxed_signature_fns`. The Rust backend feeds its
/// `mutual_tco_members`; the wasm-gc backend has its OWN tail-call
/// lowering (direct `return_call` to each member's own functype), so a
/// mutual-recursion cycle requires ALL its members to AGREE on the bare
/// param/return signature or the `return_call` fails wasm validation. The
/// wasm-gc backend therefore over-approximates: it boxes the signature of
/// every fn in a call-graph SCC of size > 1 (see
/// [`mutual_recursion_box_set`]). Fail-closed — over-boxing a signature is
/// always sound (it just declines an unboxing), under-boxing would desync.
pub fn rewrite_for_wasm_gc(
    program: MirProgram,
    boxed_signature_fns: &std::collections::HashSet<crate::ir::FnId>,
    carrier: &super::bare_i64::CarrierIntervals,
) -> MirProgram {
    rewrite(program, boxed_signature_fns, carrier)
}

/// The shared rewrite both public entries delegate to. Target-agnostic:
/// it consumes only the analysis output + the caller-supplied
/// `boxed_signature_fns`. See [`rewrite_for_rust`] /
/// [`rewrite_for_wasm_gc`] for the two callers and how each computes
/// `boxed_signature_fns`.
fn rewrite(
    program: MirProgram,
    boxed_signature_fns: &std::collections::HashSet<crate::ir::FnId>,
    carrier: &super::bare_i64::CarrierIntervals,
) -> MirProgram {
    let mut program = program;
    let facts = super::bare_i64::analyze(&program, carrier);
    // Collect ids first to avoid borrowing `program.fns` while mutating it.
    let ids: Vec<crate::ir::FnId> = program.fns.keys().copied().collect();
    for id in ids {
        // A boxed-signature fn (mutual-TCO member) is emitted with a boxed
        // (`AverInt`) signature regardless of the analysis. We MUST still
        // rewrite its BODY — but with its OWN repr forced all-boxed (empty
        // `FnBareFacts`) — so two things hold: (1) its own params/return/slots
        // stay `AverInt` (matching the pinned boxed signature; a bare tag
        // would desync), and (2) every boundary INTO another fn's bare repr
        // is still inserted — crucially, a `Box` around a bare-RETURNING
        // callee whose result this fn returns / stores (the Q5 case). Without
        // the body rewrite, a mutual-TCO member calling a `bare_return` fn
        // (`abAtDepth`'s `[] -> abNoMoves(isMax)` arm, `abNoMoves` bare) would
        // emit the callee's raw `i64` result where its own boxed body expects
        // `AverInt` — a wasm VALIDATION error. The all-boxed facts make
        // `rewrite_boxed`/`rewrite_call_args` box that result.
        let fn_facts = if boxed_signature_fns.contains(&id) {
            FnBareFacts::default()
        } else {
            let Some(f) = facts.for_fn(id).cloned() else {
                continue;
            };
            f
        };
        if let Some(f) = program.fns.get_mut(&id) {
            rewrite_fn(f, &fn_facts, &facts, boxed_signature_fns);
        }
    }
    program
}

/// The wasm-gc `boxed_signature_fns` set: every fn that participates in a
/// call-graph cycle of size > 1 (mutual recursion). The wasm-gc tail-call
/// lowering is a direct `return_call <target_fn_idx>` to the callee's OWN
/// functype, so two fns in the same recurrence must carry byte-identical
/// param/return signatures; the simplest sound guarantee is to box the
/// signature of EVERY mutual-recursion member (a self-recursive fn alone is
/// fine — its own signature is internally consistent — so self-edges are
/// excluded, matching [`crate::scc::mutually_recursive`]'s contract).
///
/// The call graph spans BOTH ordinary `Call(Fn)` and `TailCall` edges: a
/// non-tail mutual call still forces the two fns to agree if either is
/// reached by the other's tail recurrence, and including non-tail edges
/// only ever ENLARGES the boxed set (fail-closed). Self-edges are dropped
/// so a plain self-recursive fn (`countdown`, `factorial`) is NOT forced
/// boxed.
pub fn mutual_recursion_box_set(
    program: &MirProgram,
) -> std::collections::HashSet<crate::ir::FnId> {
    use std::collections::HashMap;
    let nodes: Vec<crate::ir::FnId> = program.fns.keys().copied().collect();
    let mut graph: HashMap<crate::ir::FnId, Vec<crate::ir::FnId>> = HashMap::new();
    for (caller, f) in program.iter() {
        let mut callees: Vec<crate::ir::FnId> = Vec::new();
        collect_callees(&f.body.node, *caller, &mut callees);
        graph.insert(*caller, callees);
    }
    crate::scc::mutually_recursive(&nodes, &graph)
}

/// Collect the `Call(Fn)` / `TailCall` callee `FnId`s reachable in `e`,
/// EXCLUDING self-edges (`target == caller`) so a plain self-recursive fn
/// is a 1-element SCC and stays unboxed.
fn collect_callees(e: &MirExpr, caller: crate::ir::FnId, out: &mut Vec<crate::ir::FnId>) {
    match e {
        MirExpr::Call(c) => {
            if let MirCallee::Fn(t) = c.node.callee
                && t != caller
            {
                out.push(t);
            }
            for a in &c.node.args {
                collect_callees(&a.node, caller, out);
            }
        }
        MirExpr::TailCall(tc) => {
            if tc.node.target != caller {
                out.push(tc.node.target);
            }
            for a in &tc.node.args {
                collect_callees(&a.node, caller, out);
            }
        }
        _ => super::bare_i64::visit_children(e, &mut |c| collect_callees(c, caller, out)),
    }
}

/// Populate `f.repr` from the analysis and insert the explicit boundary
/// nodes throughout `f.body`.
fn rewrite_fn(
    f: &mut MirFn,
    facts: &FnBareFacts,
    all: &BareI64Facts,
    boxed_signature_fns: &std::collections::HashSet<crate::ir::FnId>,
) {
    // 1. Make the per-value representation explicit on the fn.
    let mut bare_slots = std::collections::HashSet::new();
    for (slot, fact) in &facts.values {
        if fact.is_bare() {
            bare_slots.insert(*slot);
        }
    }
    // ETAP-2 carrier-`i64`: carry the bare-carrier slots so the wasm-gc emit
    // reads `c.value` as a raw i64 (skip the project bridge). A boxed-signature
    // fn (mutual-TCO member) gets an empty `facts`, so its `carrier_slots` is
    // empty too — its carrier reads stay boxed, matching the pinned signature.
    let carrier_slots: std::collections::HashSet<LocalId> =
        facts.carrier_slots.keys().copied().collect();
    f.repr = MirFnRepr {
        bare_slots,
        bare_params: facts.bare_params.clone(),
        bare_return: facts.bare_return,
        carrier_slots,
    };

    // 2. Insert boundary nodes. The body is in RETURN position: a bare
    //    return fn expects its tail raw; a boxed return fn expects it
    //    boxed (and a bare leaf there is `Box`ed — defect Q5 / subj_ret).
    let ctx = RewriteCtx {
        facts,
        all,
        boxed_signature_fns,
    };
    let body = std::mem::replace(
        &mut f.body,
        Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
    );
    f.body = rewrite_tail(body, facts.bare_return, &ctx);
}

struct RewriteCtx<'a> {
    facts: &'a FnBareFacts,
    all: &'a BareI64Facts,
    /// Fns the Rust backend emits with an unconditionally boxed signature
    /// (mutual-TCO members). A call to such a fn returns `AverInt` and takes
    /// `AverInt` params regardless of the analysis flags — so `call_returns_raw`
    /// and `rewrite_call_args` treat them as boxed.
    boxed_signature_fns: &'a std::collections::HashSet<crate::ir::FnId>,
}

impl RewriteCtx<'_> {
    /// The callee's per-fn facts, BUT only when the callee is emitted with
    /// its analysis-derived signature. A boxed-signature fn (mutual-TCO
    /// member) returns `None` here, so callers treat its params/return boxed.
    fn callee_facts(&self, target: crate::ir::FnId) -> Option<&FnBareFacts> {
        if self.boxed_signature_fns.contains(&target) {
            return None;
        }
        self.all.for_fn(target)
    }
}

/// Wrap `e` in a fresh `Box` node (raw i64 -> Int), preserving the span +
/// the logical `Int` type stamp (a representation boundary keeps the value's
/// logical type — only its machine representation changes — so downstream
/// numeric-disambiguation reads still see `Int`).
fn box_node(e: Spanned<MirExpr>) -> Spanned<MirExpr> {
    let line = e.line;
    let ty = e.ty().cloned();
    let out = Spanned::new(MirExpr::Box(std::boxed::Box::new(e)), line);
    if let Some(t) = ty {
        out.set_ty(t);
    }
    out
}

/// Wrap `e` in a fresh `Unbox` node (Int -> raw i64), preserving the span +
/// logical type stamp (see [`box_node`]).
fn unbox_node(e: Spanned<MirExpr>) -> Spanned<MirExpr> {
    let line = e.line;
    let ty = e.ty().cloned();
    let out = Spanned::new(MirExpr::Unbox(std::boxed::Box::new(e)), line);
    if let Some(t) = ty {
        out.set_ty(t);
    }
    out
}

/// Does `e` render as a raw `i64` value (a bare leaf or a whole-tree-bare
/// `Add`/`Sub`/`Mul`/`Neg`)? Same gate the codegen `emit_bare_i64` path
/// keys on — `FnBareFacts::expr_is_bare_i64`.
fn renders_raw(e: &MirExpr, facts: &FnBareFacts) -> bool {
    facts.expr_is_bare_i64(e)
}

/// Rewrite an expression appearing in a BOXED (`Int`/`AverInt`) context:
/// every Int sub-value here must be an `AverInt`. A sub-expression that
/// renders raw is wrapped in `Box`. This is the structural relocation of
/// the codegen `boxed_int_operand` coercion + the boxed-return tail boxing.
fn rewrite_boxed(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    // A standalone Int literal already lowers to `AverInt::from_i64(N)` on
    // the boxed path — never wrap it (would double-box). Mirrors
    // `boxed_int_operand`'s literal skip.
    if matches!(&e.node, MirExpr::Literal(l) if matches!(l.node, Literal::Int(_))) {
        return rewrite_children(e, ctx);
    }
    // NOTE: a bare-RETURN call is NOT boxed here. The prior side-table
    // codegen only boxed a bare-returning call in RETURN-TAIL position
    // (`emit_boxed_return_tail`); a call result in a non-tail boxed context
    // (a `let` value, an arithmetic operand, an aggregate element) was left
    // as-is, because the analysis's "unsafe return consumer" rule (C2)
    // demotes a callee's `bare_return` whenever its result reaches such a
    // position. Boxing it here would double-box a callee whose ACTUAL
    // emitted signature is boxed anyway (e.g. a mutual-TCO member, which
    // codegen always emits `-> AverInt` regardless of the analysis flag).
    // The Q5 return-tail case is handled by `rewrite_boxed_tail`.
    //
    // A directly raw-rendering leaf / compound (a bare `Local`, or an
    // in-range `Add`/`Sub`/`Mul`/`Neg` tree): box it. Its raw operands stay
    // raw inside (rewrite_children leaves them — they are consumed by the
    // native arithmetic that the Box wraps).
    if renders_raw(&e.node, ctx.facts) {
        return box_node(e);
    }
    // A `Match` / `IfThenElse` in a boxed context yields its result from each
    // arm/branch body, so EACH body is itself a boxed-context value: a body
    // that renders raw (a bare leaf, a raw-arith tree, or a bare-RETURNING
    // call) must be `Box`ed so the block produces an `$AverInt`. `rewrite_children`
    // would route the bodies through plain `rewrite_value`, leaving a bare-
    // returning-call body raw (the boundary-completeness hole: `x = match flag
    // { true -> g(2); false -> 0 }` with `g` bare-return — wasm-gc typed the
    // block `$AverInt` but `g(2)` rendered `i64`). Route the bodies through the
    // boxed-tail path instead, which boxes a bare-returning call + a raw leaf
    // and is a no-op on an already-boxed body.
    if matches!(&e.node, MirExpr::Match(_) | MirExpr::IfThenElse(_)) {
        return rewrite_boxed_branches(e, ctx);
    }
    // Otherwise it is already an `AverInt` — recurse so any nested boxed
    // contexts (call args, aggregate elements, …) get their own boundaries.
    rewrite_children(e, ctx)
}

/// Rewrite a value in a BOXED RETURN-TAIL position (a boxed-return fn's tail
/// leaf). Same as [`rewrite_boxed`] PLUS the Q5 case: a bare-returning call
/// in tail position renders as raw `i64`, so box it (`emit_boxed_return_tail`
/// in the prior codegen). This is restricted to tail position because that
/// is the only place the prior codegen boxed a bare-returning call.
fn rewrite_boxed_tail(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    if matches!(&e.node, MirExpr::Literal(l) if matches!(l.node, Literal::Int(_))) {
        return rewrite_children(e, ctx);
    }
    // Q5 / subj_ret: a bare-returning call in tail position is raw `i64`; box
    // it at the return crossing.
    if call_returns_raw(&e.node, ctx) {
        let inner = rewrite_children(e, ctx);
        return box_node(inner);
    }
    if renders_raw(&e.node, ctx.facts) {
        return box_node(e);
    }
    // Same boxed-branch discipline as `rewrite_boxed`: a `Match` / `IfThenElse`
    // tail in a boxed-return fn yields its result from each arm, so each body
    // must produce an `$AverInt` — box a bare-returning-call / raw-leaf body,
    // leave an already-boxed body untouched.
    if matches!(&e.node, MirExpr::Match(_) | MirExpr::IfThenElse(_)) {
        return rewrite_boxed_branches(e, ctx);
    }
    rewrite_children(e, ctx)
}

/// Rewrite a `Match` / `IfThenElse` in a BOXED context: the subject/condition
/// keep their existing (subject-binding / value) treatment, but each arm or
/// branch BODY is a boxed-context value and is routed through
/// [`rewrite_boxed_tail`] so a body that renders raw (a bare leaf, a raw-arith
/// tree, or a bare-RETURNING call) is `Box`ed to an `$AverInt`. An
/// already-boxed body is unchanged (the boxed-tail path is a no-op there), so
/// this only affects the raw-yielding-arm programs the boundary-completeness
/// hole produced. Mirrors the per-node recursion of `rewrite_children_inner`'s
/// `Match` / `IfThenElse` arms, differing ONLY in the body context (boxed vs
/// plain value).
fn rewrite_boxed_branches(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    let line = e.line;
    let ty = e.ty().cloned();
    let rebuilt = match e.node {
        MirExpr::Match(mut m) => {
            let subj = take_box(&mut m.node.subject);
            m.node.subject = std::boxed::Box::new(rewrite_match_subject(subj, &m.node.arms, ctx));
            for arm in &mut m.node.arms {
                let body = std::mem::replace(
                    &mut arm.body,
                    Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
                );
                arm.body = rewrite_boxed_tail(body, ctx);
            }
            Spanned::new(MirExpr::Match(m), line)
        }
        MirExpr::IfThenElse(mut ite) => {
            let cond = take_box(&mut ite.node.cond);
            ite.node.cond = std::boxed::Box::new(rewrite_value(cond, ctx));
            let then_b = take_box(&mut ite.node.then_branch);
            ite.node.then_branch = std::boxed::Box::new(rewrite_boxed_tail(then_b, ctx));
            let else_b = take_box(&mut ite.node.else_branch);
            ite.node.else_branch = std::boxed::Box::new(rewrite_boxed_tail(else_b, ctx));
            Spanned::new(MirExpr::IfThenElse(ite), line)
        }
        // `rewrite_boxed` / `rewrite_boxed_tail` only dispatch here for a
        // `Match` / `IfThenElse` node; any other node is a caller bug.
        other => rewrite_children(Spanned::new(other, line), ctx),
    };
    if let Some(t) = ty {
        rebuilt.set_ty(t);
    }
    rebuilt
}

/// Rewrite an `InterpolatedStr` embed (a value being stringified). Same
/// boundary discipline as a boxed RETURN tail: a raw leaf / a bare-returning
/// call must be `Box`ed so the embed carries an `$AverInt` (wasm-gc's decimal
/// formatter takes an `$aint` ref; Rust's `from_i64(x).to_string()` matches
/// the raw `x.to_string()` it replaces). The bare-returning-call boxing (the
/// Q5 case) is REQUIRED here because the analysis treats stringify as a safe
/// `bare_return` consumer — it does NOT demote — so the call still renders raw.
fn rewrite_interp_embed(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    rewrite_boxed_tail(e, ctx)
}

/// Rewrite an expression appearing in a RAW (`i64`) context: the consumer
/// expects a native `i64`. A sub-expression that is already raw stays; one
/// that is a boxed `AverInt` is narrowed via `Unbox`. This is the
/// relocation of the codegen call-arg `to_i64` coercion at a bare param.
fn rewrite_raw(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    // A raw-rendering leaf / compound (a bare `Local`, an in-range
    // `Add`/`Sub`/`Mul`/`Neg` tree, an Int literal): stays raw. Recurse so a
    // nested boxed operand inside an arithmetic tree is handled (a no-op by
    // construction — a whole-tree-bare compound has only raw leaves).
    if renders_raw(&e.node, ctx.facts) {
        return rewrite_children(e, ctx);
    }
    // Any other value reaching a raw context (`i64` param / return) is a
    // boxed `AverInt` and must be narrowed via `Unbox` (the checked
    // `to_i64`). This mirrors the prior codegen's bare-param call-arg path,
    // which narrowed every non-`emit_bare_i64` arg — INCLUDING a call result
    // — with `to_i64`, rather than trusting the (mutual-TCO-unreliable)
    // `bare_return` flag.
    let inner = rewrite_children(e, ctx);
    unbox_node(inner)
}

/// Is `e` a `Call(Fn)` / `TailCall` whose callee's return the analysis
/// proved bare (renders as raw `i64`)? Mirror of codegen's
/// `mir_call_returns_bare`.
fn call_returns_raw(e: &MirExpr, ctx: &RewriteCtx<'_>) -> bool {
    let target = match e {
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Fn(t) => t,
            _ => return false,
        },
        MirExpr::TailCall(tc) => tc.node.target,
        _ => return false,
    };
    ctx.callee_facts(target).is_some_and(|f| f.bare_return)
}

/// Rewrite a `match` SUBJECT for the representation its binding aliases
/// require. A `match subject { x -> … }` binds the subject to `x`, so `x`
/// inherits the subject's representation. The analysis already set `x`'s
/// repr (a binding that ESCAPES is boxed — defect esc_match `[x, x]`), so
/// the subject must be rewritten to MATCH the binding: boxed when the
/// binding is boxed (the raw subject is `Box`ed at the bind crossing), raw
/// when the binding is bare. With no `Bind` arm (literal / ctor / wildcard
/// patterns) the subject is an ordinary value position. When arms disagree
/// (multiple bind arms with mixed reps — not produced by lowering), the
/// safe choice is boxed.
fn rewrite_match_subject(
    subj: Spanned<MirExpr>,
    arms: &[crate::ir::mir::MirMatchArm],
    ctx: &RewriteCtx<'_>,
) -> Spanned<MirExpr> {
    use crate::ir::mir::MirPattern;
    let bind_slots: Vec<LocalId> = arms
        .iter()
        .filter_map(|arm| match &arm.pattern {
            MirPattern::Bind(slot, _) => Some(*slot),
            _ => None,
        })
        .collect();
    if bind_slots.is_empty() {
        return rewrite_value(subj, ctx);
    }
    // Every bind slot aliases the same subject, so they share a repr in a
    // well-formed program; require ALL bare to treat the subject raw.
    let all_bare = bind_slots.iter().all(|s| ctx.facts.is_bare(*s));
    if all_bare {
        rewrite_raw(subj, ctx)
    } else {
        rewrite_boxed(subj, ctx)
    }
}

/// Rewrite a value in TAIL/return position. `bare_return` is the enclosing
/// fn's return representation: when bare, the tail is a raw context; when
/// boxed, a boxed context (and the descent boxes a bare leaf — defects
/// Q5 / subj_ret). Match / IfThenElse / Let / Return recurse into their
/// tail leaves so each arm/branch is handled in the right context.
fn rewrite_tail(e: Spanned<MirExpr>, bare_return: bool, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    let line = e.line;
    let ty = e.ty().cloned();
    // Preserve the node's type stamp across the rebuild. The numeric-
    // disambiguation + the wasm-gc emitter (`aver_type_str_of`) read it; a
    // tail reconstruction that dropped the stamp left a rewritten `TailCall`
    // / `Match` un-typed, which panics the wasm-gc reader. Mirror of
    // `rewrite_children`'s stamp preservation, applied to EVERY tail arm.
    let out = rewrite_tail_inner(e.node, line, ty.clone(), bare_return, ctx);
    if let Some(t) = ty {
        out.set_ty(t);
    }
    out
}

fn rewrite_tail_inner(
    node: MirExpr,
    line: usize,
    ty: Option<crate::ast::Type>,
    bare_return: bool,
    ctx: &RewriteCtx<'_>,
) -> Spanned<MirExpr> {
    match node {
        MirExpr::Match(mut m) => {
            // Subject is rewritten for the binding's representation (a bound
            // subject aliases the binding — defect esc_match); the arm bodies
            // are tails.
            let subj = std::mem::replace(
                &mut m.node.subject,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Unit,
                )))),
            );
            m.node.subject = std::boxed::Box::new(rewrite_match_subject(*subj, &m.node.arms, ctx));
            for arm in &mut m.node.arms {
                let body = std::mem::replace(
                    &mut arm.body,
                    Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
                );
                arm.body = rewrite_tail(body, bare_return, ctx);
            }
            Spanned::new(MirExpr::Match(m), line)
        }
        MirExpr::IfThenElse(mut ite) => {
            let cond = std::mem::replace(
                &mut ite.node.cond,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Bool(false),
                )))),
            );
            ite.node.cond = std::boxed::Box::new(rewrite_value(*cond, ctx));
            let then_b = std::mem::replace(
                &mut ite.node.then_branch,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Unit,
                )))),
            );
            ite.node.then_branch = std::boxed::Box::new(rewrite_tail(*then_b, bare_return, ctx));
            let else_b = std::mem::replace(
                &mut ite.node.else_branch,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Unit,
                )))),
            );
            ite.node.else_branch = std::boxed::Box::new(rewrite_tail(*else_b, bare_return, ctx));
            Spanned::new(MirExpr::IfThenElse(ite), line)
        }
        MirExpr::Let(mut l) => {
            // The let VALUE is a value position (boxed unless its binding
            // slot is bare); the let BODY is the tail.
            let value = std::mem::replace(
                &mut l.node.value,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Unit,
                )))),
            );
            l.node.value = std::boxed::Box::new(rewrite_let_value(*value, l.node.binding, ctx));
            let body = std::mem::replace(
                &mut l.node.body,
                std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
                    Literal::Unit,
                )))),
            );
            l.node.body = std::boxed::Box::new(rewrite_tail(*body, bare_return, ctx));
            Spanned::new(MirExpr::Let(l), line)
        }
        MirExpr::Return(inner) => {
            let r = rewrite_tail(*inner, bare_return, ctx);
            Spanned::new(MirExpr::Return(std::boxed::Box::new(r)), line)
        }
        // A (self / mutual) tail-call is the RECURRENCE, not a returned
        // value: it transfers control, it does not cross a representation
        // boundary in the caller's frame. Its ARGS are rewritten in the
        // callee's per-param context (`rewrite_call_args`), but the call
        // itself is never `Box`/`Unbox`ed. Treating it as a value (and
        // unboxing it in a bare-return fn) was the spurious-`Unbox` bug.
        MirExpr::TailCall(mut tc) => {
            let target = tc.node.target;
            let callee = MirCallee::Fn(target);
            let args = std::mem::take(&mut tc.node.args);
            tc.node.args = rewrite_call_args(args, &callee, ctx);
            Spanned::new(MirExpr::TailCall(tc), line)
        }
        // A base-case value (a literal, a bare `Local`, an arithmetic tree,
        // an ordinary `Call`): rewrite for the fn's return representation.
        other => {
            let spanned = Spanned::new(other, line);
            if let Some(t) = ty {
                spanned.set_ty(t);
            }
            if bare_return {
                rewrite_raw(spanned, ctx)
            } else {
                // Boxed-return tail: the Q5 bare-returning-call boxing applies
                // here (and only here — see `rewrite_boxed_tail`).
                rewrite_boxed_tail(spanned, ctx)
            }
        }
    }
}

/// Rewrite the VALUE of a `Let` binding. The binding's representation is
/// its slot repr: a bare binding wants a raw value, a boxed binding wants
/// an `AverInt` (defect esc_match — a `let x = n - 1` whose `x` escapes is
/// boxed, so the raw `n - 1` is `Box`ed at the crossing).
fn rewrite_let_value(
    e: Spanned<MirExpr>,
    binding: LocalId,
    ctx: &RewriteCtx<'_>,
) -> Spanned<MirExpr> {
    if ctx.facts.is_bare(binding) {
        rewrite_raw(e, ctx)
    } else {
        rewrite_boxed(e, ctx)
    }
}

/// Rewrite an expression in an ordinary VALUE position (not tail, not a
/// specially-typed context). Defaults to a boxed context — the general Int
/// world — except where the node itself imposes a raw sub-context
/// (arithmetic over bare operands, a bare callee param). The per-node
/// boundary logic lives in `rewrite_children`.
fn rewrite_value(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    // A raw-rendering compound/leaf in a value position is itself raw; its
    // CONSUMER (the parent) decides whether to box it. So here we only
    // recurse into children (which place their own boundaries); the parent
    // arm that produced this `rewrite_value` call already applied the
    // boxed/raw wrapper if it needed one.
    rewrite_children(e, ctx)
}

/// Recurse into the children of `e`, placing boundary nodes per-node:
///
/// - `BinOp` arithmetic over Int operands: a bare operand consumed by a
///   BOXED `AverInt` op is `Box`ed (`boxed_int_operand`); a whole-tree-bare
///   op keeps raw operands.
/// - `Call(Fn)` / `TailCall`: each arg is rewritten in the callee's i-th
///   param context (raw if the param is bare, boxed otherwise).
/// - aggregates / records / maps / interpolation: each element is a boxed
///   context (a general-Int store).
/// - everything else: structural recursion in a value context.
fn rewrite_children(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    // Preserve the node's type stamp across the rebuild: the codegen
    // `int_arith` / numeric disambiguation reads `lhs.ty()` / `rhs.ty()`, so
    // dropping the stamp here would silently flip an `acc * n` Int multiply
    // onto the raw `*`-operator path (which `AverInt` has no impl for).
    let ty = e.ty().cloned();
    let out = rewrite_children_inner(e, ctx);
    if let Some(t) = ty {
        out.set_ty(t);
    }
    out
}

fn rewrite_children_inner(e: Spanned<MirExpr>, ctx: &RewriteCtx<'_>) -> Spanned<MirExpr> {
    let line = e.line;
    match e.node {
        MirExpr::BinOp(mut b) => {
            let op = b.node.op;
            let int_arith = matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div);
            // Query whole-tree bareness on the ORIGINAL node before taking
            // the operands apart (it inspects `b.node` verbatim).
            let whole_raw =
                int_arith && renders_raw(&MirExpr::BinOp(clone_binop_shell(&b)), ctx.facts);
            let lhs = take_box(&mut b.node.lhs);
            let rhs = take_box(&mut b.node.rhs);
            // Whole-tree-bare arithmetic stays raw: keep operands raw.
            if whole_raw {
                b.node.lhs = std::boxed::Box::new(rewrite_raw(lhs, ctx));
                b.node.rhs = std::boxed::Box::new(rewrite_raw(rhs, ctx));
                return Spanned::new(MirExpr::BinOp(b), line);
            }
            if int_arith
                && (super::bare_i64::type_is_int(operand_ty(&lhs))
                    || super::bare_i64::type_is_int(operand_ty(&rhs)))
            {
                // Boxed `AverInt` arithmetic: each operand must be an
                // `AverInt`. A raw operand is `Box`ed (`boxed_int_operand`).
                b.node.lhs = std::boxed::Box::new(rewrite_boxed(lhs, ctx));
                b.node.rhs = std::boxed::Box::new(rewrite_boxed(rhs, ctx));
                return Spanned::new(MirExpr::BinOp(b), line);
            }
            // Comparison: a comparison between two raw operands stays raw
            // (codegen emits `i64 == i64`); otherwise BOTH operands must be
            // boxed `$AverInt` (the boxed comparison helper). The MIXED case
            // is the carrier-arithmetic crux: `s.x.value == fx.value` reads a
            // carrier FIELD (`Project(Project(..))`, a boxed `$AverInt`) on one
            // side and a bare carrier PARAM's `.value` (raw i64) on the other.
            // Without boxing the raw side, codegen would compare an i64 against
            // an `$AverInt` ref — a wasm VALIDATION error. So if EITHER operand
            // is boxed, route BOTH through `rewrite_boxed` (a no-op on an
            // already-boxed operand, a `Box` on a raw one).
            if matches!(
                op,
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte
            ) {
                let both_raw =
                    renders_raw(&lhs.node, ctx.facts) && renders_raw(&rhs.node, ctx.facts);
                if both_raw {
                    b.node.lhs = std::boxed::Box::new(rewrite_raw(lhs, ctx));
                    b.node.rhs = std::boxed::Box::new(rewrite_raw(rhs, ctx));
                } else {
                    b.node.lhs = std::boxed::Box::new(rewrite_boxed(lhs, ctx));
                    b.node.rhs = std::boxed::Box::new(rewrite_boxed(rhs, ctx));
                }
                return Spanned::new(MirExpr::BinOp(b), line);
            }
            b.node.lhs = std::boxed::Box::new(rewrite_value(lhs, ctx));
            b.node.rhs = std::boxed::Box::new(rewrite_value(rhs, ctx));
            Spanned::new(MirExpr::BinOp(b), line)
        }
        MirExpr::Neg(inner) => {
            // Negation over a bare operand renders boxed by codegen (the
            // bare `Neg` path bails to `AverInt::neg`), so the operand is a
            // boxed context.
            let r = rewrite_boxed(*inner, ctx);
            Spanned::new(MirExpr::Neg(std::boxed::Box::new(r)), line)
        }
        MirExpr::Call(mut c) => {
            let callee = c.node.callee.clone();
            let args = std::mem::take(&mut c.node.args);
            c.node.args = rewrite_call_args(args, &callee, ctx);
            Spanned::new(MirExpr::Call(c), line)
        }
        MirExpr::TailCall(mut tc) => {
            let target = tc.node.target;
            let callee = MirCallee::Fn(target);
            let args = std::mem::take(&mut tc.node.args);
            tc.node.args = rewrite_call_args(args, &callee, ctx);
            Spanned::new(MirExpr::TailCall(tc), line)
        }
        MirExpr::List(items) => Spanned::new(MirExpr::List(rewrite_boxed_each(items, ctx)), line),
        MirExpr::Tuple(items) => Spanned::new(MirExpr::Tuple(rewrite_boxed_each(items, ctx)), line),
        MirExpr::MapLiteral(pairs) => {
            let pairs = pairs
                .into_iter()
                .map(|(k, v)| (rewrite_boxed(k, ctx), rewrite_boxed(v, ctx)))
                .collect();
            Spanned::new(MirExpr::MapLiteral(pairs), line)
        }
        MirExpr::Construct(mut c) => {
            let args = std::mem::take(&mut c.node.args);
            c.node.args = rewrite_boxed_each(args, ctx);
            Spanned::new(MirExpr::Construct(c), line)
        }
        MirExpr::RecordCreate(mut r) => {
            for fld in &mut r.node.fields {
                let v = std::mem::replace(
                    &mut fld.value,
                    Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
                );
                fld.value = rewrite_boxed(v, ctx);
            }
            Spanned::new(MirExpr::RecordCreate(r), line)
        }
        MirExpr::RecordUpdate(mut u) => {
            let base = take_box(&mut u.node.base);
            u.node.base = std::boxed::Box::new(rewrite_value(base, ctx));
            for fld in &mut u.node.updates {
                let v = std::mem::replace(
                    &mut fld.value,
                    Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
                );
                fld.value = rewrite_boxed(v, ctx);
            }
            Spanned::new(MirExpr::RecordUpdate(u), line)
        }
        MirExpr::InterpolatedStr(parts) => {
            // An interpolation embed is a general-Int (BOXED) context: the
            // value is stringified. wasm-gc's `$aint` decimal formatter
            // takes an `$AverInt` ref, so a raw-rendering Int embed (a bare
            // local, a raw-arith tree, a bare-returning call) must be `Box`ed
            // at the embed crossing. (On Rust this is `from_i64(x).to_string()`
            // — identical output to the raw `x.to_string()` it had before, so
            // boxing here is sound for both backends.) Unlike an ordinary
            // boxed value position, a bare-RETURNING call ALSO boxes here (the
            // Q5 rule): the analysis treats stringify as a safe `bare_return`
            // consumer (it does NOT demote), so the call still renders raw and
            // the embed must box its result. `rewrite_interp_embed` handles
            // both the raw-leaf and the bare-returning-call cases.
            let parts = parts
                .into_iter()
                .map(|p| match p {
                    MirStrPart::Expr(ex) => MirStrPart::Expr(rewrite_interp_embed(ex, ctx)),
                    MirStrPart::Literal(s) => MirStrPart::Literal(s),
                })
                .collect();
            Spanned::new(MirExpr::InterpolatedStr(parts), line)
        }
        MirExpr::IndependentProduct(mut ip) => {
            let items = std::mem::take(&mut ip.node.items);
            ip.node.items = items.into_iter().map(|it| rewrite_value(it, ctx)).collect();
            Spanned::new(MirExpr::IndependentProduct(ip), line)
        }
        MirExpr::Project(mut p) => {
            let base = take_box(&mut p.node.base);
            p.node.base = std::boxed::Box::new(rewrite_value(base, ctx));
            Spanned::new(MirExpr::Project(p), line)
        }
        MirExpr::Try(inner) => {
            let r = rewrite_value(*inner, ctx);
            Spanned::new(MirExpr::Try(std::boxed::Box::new(r)), line)
        }
        MirExpr::Return(inner) => {
            let r = rewrite_value(*inner, ctx);
            Spanned::new(MirExpr::Return(std::boxed::Box::new(r)), line)
        }
        MirExpr::Let(mut l) => {
            let value = take_box(&mut l.node.value);
            l.node.value = std::boxed::Box::new(rewrite_let_value(value, l.node.binding, ctx));
            let body = take_box(&mut l.node.body);
            l.node.body = std::boxed::Box::new(rewrite_value(body, ctx));
            Spanned::new(MirExpr::Let(l), line)
        }
        MirExpr::Match(mut m) => {
            let subj = take_box(&mut m.node.subject);
            m.node.subject = std::boxed::Box::new(rewrite_match_subject(subj, &m.node.arms, ctx));
            for arm in &mut m.node.arms {
                let body = std::mem::replace(
                    &mut arm.body,
                    Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Unit))),
                );
                arm.body = rewrite_value(body, ctx);
            }
            Spanned::new(MirExpr::Match(m), line)
        }
        MirExpr::IfThenElse(mut ite) => {
            let cond = take_box(&mut ite.node.cond);
            ite.node.cond = std::boxed::Box::new(rewrite_value(cond, ctx));
            let then_b = take_box(&mut ite.node.then_branch);
            ite.node.then_branch = std::boxed::Box::new(rewrite_value(then_b, ctx));
            let else_b = take_box(&mut ite.node.else_branch);
            ite.node.else_branch = std::boxed::Box::new(rewrite_value(else_b, ctx));
            Spanned::new(MirExpr::IfThenElse(ite), line)
        }
        // Leaves + already-inserted boundary nodes: nothing to recurse.
        node @ (MirExpr::Literal(_)
        | MirExpr::Local(_)
        | MirExpr::FnValue(_)
        | MirExpr::Box(_)
        | MirExpr::Unbox(_)) => Spanned::new(node, line),
    }
}

/// Rewrite the args of a `Call(Fn)` / `TailCall`: each arg is rewritten in
/// the callee's i-th param context — raw if the callee param is bare
/// (`Unbox` a boxed arg, keep a raw one), boxed otherwise (`Box` a raw arg).
/// A non-`Fn` callee (builtin / intrinsic / fn-value) treats every Int arg
/// as a general-Int (boxed) context.
fn rewrite_call_args(
    args: Vec<Spanned<MirExpr>>,
    callee: &MirCallee,
    ctx: &RewriteCtx<'_>,
) -> Vec<Spanned<MirExpr>> {
    let callee_facts = match callee {
        MirCallee::Fn(t) => ctx.callee_facts(*t),
        _ => None,
    };
    args.into_iter()
        .enumerate()
        .map(|(i, a)| {
            if callee_facts.is_some_and(|f| f.param_is_bare(i)) {
                rewrite_raw(a, ctx)
            } else {
                rewrite_boxed(a, ctx)
            }
        })
        .collect()
}

fn rewrite_boxed_each(items: Vec<Spanned<MirExpr>>, ctx: &RewriteCtx<'_>) -> Vec<Spanned<MirExpr>> {
    items.into_iter().map(|it| rewrite_boxed(it, ctx)).collect()
}

/// Take a boxed child out, leaving a Unit placeholder (immediately
/// overwritten by the caller). Avoids cloning a whole subtree.
fn take_box(slot: &mut std::boxed::Box<Spanned<MirExpr>>) -> Spanned<MirExpr> {
    let placeholder = std::boxed::Box::new(Spanned::bare(MirExpr::Literal(Spanned::bare(
        Literal::Unit,
    ))));
    *std::mem::replace(slot, placeholder)
}

/// The operand's type stamp (for the int-arith disambiguation, same as
/// codegen reads `bop.lhs.ty()`).
fn operand_ty(e: &Spanned<MirExpr>) -> Option<&crate::ast::Type> {
    e.ty()
}

/// Build a throwaway `BinOp` shell (same op, cloned operands) used only to
/// query `expr_is_bare_i64` on the WHOLE tree without consuming the real
/// node. Cloning is cheap here (only fires for Int arithmetic nodes).
fn clone_binop_shell(b: &Spanned<crate::ir::mir::MirBinOp>) -> Spanned<crate::ir::mir::MirBinOp> {
    Spanned::new(
        crate::ir::mir::MirBinOp {
            op: b.node.op,
            lhs: b.node.lhs.clone(),
            rhs: b.node.rhs.clone(),
        },
        b.line,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::mir::{lower_program, optimize};
    use crate::source::parse_source;

    fn rewritten(src: &str) -> MirProgram {
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
        let mir_items = result.resolved_items.clone();
        let program = optimize(lower_program(&mir_items));
        rewrite_for_rust(
            program,
            &std::collections::HashSet::new(),
            &super::super::bare_i64::CarrierIntervals::new(),
        )
    }

    fn fn_named<'a>(program: &'a MirProgram, name: &str) -> &'a MirFn {
        program
            .iter()
            .find(|(_, f)| f.name == name)
            .map(|(_, f)| f)
            .unwrap_or_else(|| panic!("fn `{name}` not in program"))
    }

    /// Walk the body counting `Box` / `Unbox` boundary nodes.
    fn count_boundaries(e: &MirExpr) -> (usize, usize) {
        let mut boxes = 0;
        let mut unboxes = 0;
        count_rec(e, &mut boxes, &mut unboxes);
        (boxes, unboxes)
    }

    fn count_rec(e: &MirExpr, boxes: &mut usize, unboxes: &mut usize) {
        match e {
            MirExpr::Box(_) => *boxes += 1,
            MirExpr::Unbox(_) => *unboxes += 1,
            _ => {}
        }
        super::super::bare_i64::tests_visit_children(e, &mut |c| count_rec(c, boxes, unboxes));
    }

    #[test]
    fn countdown_repr_is_explicit_and_no_spurious_boundary() {
        // The countdown counter is bare; its body is pure raw arithmetic
        // toward a bare return, so NO boundary node is needed.
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
        let program = rewritten(src);
        let f = fn_named(&program, "countdown");
        assert!(f.repr.param_is_bare(0), "counter param tagged bare on MIR");
        assert!(f.repr.bare_return, "bare return tagged on MIR");
        let (boxes, unboxes) = count_boundaries(&f.body.node);
        assert_eq!(
            (boxes, unboxes),
            (0, 0),
            "an all-bare countdown needs no boundary node"
        );
    }

    #[test]
    fn factorial_boxes_bare_operand_at_mul() {
        // `acc * n`: `acc` boxed, `n` bare. The rewrite must `Box(n)` at the
        // boxed-multiply boundary (exactly one Box, no Unbox).
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
        let program = rewritten(src);
        let f = fn_named(&program, "factorial");
        assert!(f.repr.param_is_bare(0), "n is bare on MIR");
        assert!(!f.repr.param_is_bare(1), "acc stays boxed on MIR");
        let (boxes, unboxes) = count_boundaries(&f.body.node);
        assert_eq!(boxes, 1, "exactly the `Box(n)` at `acc * n`");
        assert_eq!(unboxes, 0, "no Unbox needed");
    }

    #[test]
    fn q5_bare_return_into_boxed_return_boxes_at_crossing() {
        // Defect Q5: `g` has a bare return; `h` returns the call result as a
        // boxed Int. The rewrite must `Box` the bare-returning call in `h`.
        let src = r#"
module M
    intent = "t"
    depends []

fn g(n: Int) -> Int
    match n
        0 -> 0
        _ -> g(n - 1)

fn h() -> Int
    g(2)

fn main() -> Int
    h()
"#;
        let program = rewritten(src);
        let g = fn_named(&program, "g");
        assert!(g.repr.bare_return, "g keeps its bare return");
        let h = fn_named(&program, "h");
        assert!(!h.repr.bare_return, "h's return stays boxed");
        let (boxes, _unboxes) = count_boundaries(&h.body.node);
        assert!(boxes >= 1, "h boxes the bare-returning call result");
    }
}
