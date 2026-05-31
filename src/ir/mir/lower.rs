//! HIR → MIR lowering, waves 1 + 2 + 3a + 3b + 3c (all sub-waves).
//!
//! Phase 3 of #252 lowers `ResolvedProgramView` into `MirProgram`
//! in widening waves so review surface stays small.
//!
//! **Wave 1** — leaf subset, no callee resolver, no patterns:
//! - `ResolvedExpr::Literal` → `MirExpr::Literal`
//! - `ResolvedExpr::Resolved { slot, .. }` → `MirExpr::Local`
//! - `ResolvedExpr::BinOp` → `MirExpr::BinOp`
//! - `ResolvedExpr::Neg` → `MirExpr::Neg`
//!
//! **Wave 2 (added in this commit)** — call shapes + product types:
//! - `ResolvedExpr::Call(Fn, args)` → `MirExpr::Call` w/ `MirCallee::Fn(FnId)`
//! - `ResolvedExpr::Call(Builtin, args)` → `MirExpr::Call` w/ `MirCallee::Builtin(name)`
//! - `ResolvedExpr::Ctor(User { ctor_id, .. }, args)` → `MirExpr::Construct`
//! - `ResolvedExpr::RecordCreate { type_id: Some(_), … }` → `MirExpr::RecordCreate`
//! - `ResolvedExpr::RecordUpdate { type_id: Some(_), … }` → `MirExpr::RecordUpdate`
//! - `ResolvedExpr::Attr(base, field)` → `MirExpr::Project`
//!
//! **Wave 3a (this commit)** — multi-stmt bodies via `Let` chains:
//! - `Stmt::Binding { name, value }` + `Stmt::Expr(value)` in
//!   sequence ending in `Stmt::Expr` right-folds into nested
//!   `MirExpr::Let { binding, value, body }` nodes.
//! - Binding slot comes from `FnResolution.local_slots[name]`;
//!   intermediate `Stmt::Expr` gets a fresh synthetic `LocalId`
//!   drawn from a counter starting at `local_count`.
//!
//! **Wave 3b (this commit)** — structured `match` + patterns:
//! - `ResolvedExpr::Match { subject, arms }` → `MirExpr::Match`.
//! - User-ctor patterns (`ResolvedCtor::User { ctor_id, .. }`)
//!   lower to `MirPattern::Ctor { ctor: MirCtor::User(_), bindings }`.
//! - Cons / Tuple / Ident / Literal / Wildcard / EmptyList lower
//!   structurally. Pattern bindings draw their `LocalId`s from
//!   `ResolvedMatchArm.binding_slots` (preorder-flat, same shape
//!   `ast_rewrite::pattern_binding_names` walks).
//! - Built-in constructor patterns (`Result.Ok`, `Option.Some`,
//!   …) stay wave 3c territory — fns matching on them are
//!   silently skipped, same skip-rule that wave 2 applied to
//!   built-in ctor *constructions*.
//!
//! **Wave 3c-i (this commit)** — typed identity for built-in
//!   constructors:
//! - `ResolvedExpr::Ctor(Builtin(bc), args)` →
//!   `MirConstruct { ctor: MirCtor::Builtin(bc), args }`.
//! - `ResolvedPattern::Ctor(Builtin(bc), names)` →
//!   `MirPattern::Ctor { ctor: MirCtor::Builtin(bc), bindings }`.
//! - User ctors stay on `MirCtor::User(CtorId)`; the two flavors
//!   ride the same node shape so backends pattern-match once.
//! - `SkipReason::BuiltinCtorConstruction` /
//!   `BuiltinCtorPattern` drop out of `LowerStats.skipped` (the
//!   variants stay in the enum for historical attribution).
//!
//! **Wave 3c-ii** — `Try` (`?` propagation): `ErrorProp(inner)` →
//! `MirExpr::Try(inner)`. RFC pin: stays a node, backends pick
//! the final shape. The `let x = step()?; body` form composes
//! through wave 3a's stmt-chain (`Let { value: Try(_), … }`).
//!
//! **Wave 3c-iii** — tail calls: `TailCall { target, args }` →
//! `MirExpr::TailCall(MirTailCall { target: FnId, args })`. TCO
//! upstream already classified the call; MIR preserves `FnId`
//! identity. Backends pick wasm-gc tail-call insn / VM tail
//! dispatch / Rust loop rewrite.
//!
//! **Wave 3c-iv** — collection literals: `List` / `Tuple` /
//! `MapLiteral` / `InterpolatedStr` all lower element-wise; the
//! outer MIR node carries the structural shape. Note: `interp_lower`
//! upstream of MIR desugars interpolated strings into buffer-build
//! calls before MIR sees the tree, so `InterpolatedStr` is rarely
//! reached in practice — the lowering exists for symmetry.
//!
//! **Wave 3c-v** — `IndependentProduct(items, unwrap_results)` →
//! `MirExpr::IndependentProduct { items, unwrap_results }`. The
//! compile-time independence mode (`complete` / `cancel` /
//! `sequential`) is NOT carried in MIR per RFC — that's an
//! aver.toml runtime policy decision.
//!
//! Final remaining `SkipReason`s after wave 3c:
//! - `UnresolvedIdent` — a fn referenced as a value (`callWith(dbl)`
//!   passes `dbl` as a bare ident). Calling a fn value already lowers;
//!   referencing one is the remaining first-class-fn shape.
//! - `MissingResolution` / `EmptyBody` / `BindingOnlyTail` /
//!   `BindingSlotLookupMissing` / `PatternSlotShortfall` —
//!   defensive guards for upstream pipeline gaps.
//! - `UnsupportedCallee` — only `Unresolved` callees (typecheck-error
//!   recovery) now. Buffer intrinsics (`MirCallee::Intrinsic`) and
//!   first-class-fn *calls* (`MirCallee::LocalSlot` → `CALL_VALUE`)
//!   lower.
//! - `BuiltinRecord` / `BuiltinCtorConstruction` / `BuiltinCtorPattern`
//!   / `UnsupportedTry` / `UnsupportedTailCall` / `UnsupportedList` /
//!   `UnsupportedTuple` / `UnsupportedMap` / `UnsupportedInterpolatedStr`
//!   / `UnsupportedIndependentProduct` — kept in the enum for historical
//!   attribution; no longer reachable from the lowerer (built-in records
//!   now ride `MirRecordCreate.type_name` when they carry no `TypeId`).
//!
//! Functions that use anything outside the waves' supported
//! subset are dropped — `MirProgram.fns` only contains what the
//! waves so far knew how to handle. Every drop bumps a counter
//! on `MirProgram.stats.skipped` keyed by the first
//! [`SkipReason`](crate::ir::mir::SkipReason) the lowerer hit
//! inside the fn. Phase 4's coverage gate consumes that to
//! prove the lowered subset covers the corpus before the VM
//! slice migrates.
//!
//! ## LocalId mapping
//!
//! Wave 1+2 use the resolver's slot index
//! (`ResolvedExpr::Resolved { slot, .. }`) directly as the
//! `LocalId`. Phase 2's RFC pin was "assign at MIR construction
//! time" — that gives the future optimizer freedom to renumber,
//! but during lowering itself we use the slot the resolver
//! already assigned. The slot is already unique per function
//! body, so wave 1+2 don't need a fresh counter yet. Wave 3
//! will introduce one when `Let` bindings start producing fresh
//! locals that the resolver didn't see.

use crate::ast::{FnResolution, Spanned};
use crate::ir::hir::{
    ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedFnBody, ResolvedFnDef, ResolvedMatchArm,
    ResolvedPattern, ResolvedStmt, ResolvedTopLevel,
};

use super::expr::{
    MirBinOp, MirCall, MirCallee, MirConstruct, MirCtor, MirEffectAnnotation, MirExpr, MirLet,
    MirMatch, MirMatchArm, MirPattern, MirProject, MirRecordCreate, MirRecordField,
    MirRecordUpdate,
};
use super::program::{LocalId, MirFn, MirParam, MirProgram};
use super::stats::SkipReason;

/// Lower an entry-module resolved-item list into a `MirProgram`.
/// Function bodies outside the supported subset are dropped —
/// `MirProgram.fns` only contains what the lowerer's waves so far
/// knew how to handle. Every drop bumps a counter on
/// `MirProgram.stats.skipped` keyed by the first `SkipReason` the
/// lowerer hit inside the fn, so Phase 4's coverage gate can prove
/// that the lowered subset actually covers the corpus.
pub fn lower_program(items: &[ResolvedTopLevel]) -> MirProgram {
    let mut program = MirProgram::empty();
    for item in items {
        let ResolvedTopLevel::FnDef(fd) = item else {
            continue;
        };
        match lower_fn(fd, &mut program) {
            Ok(mir_fn) => {
                program.fns.insert(fd.fn_id, mir_fn);
                program.stats.record_lowered();
            }
            Err(reason) => {
                program.stats.record_skip(reason);
            }
        }
    }
    program
}

/// Lower one `ResolvedFnDef` if its body fits the supported subset.
/// Returns `Err(SkipReason)` for the dominant unsupported shape —
/// the caller drops the fn from the MIR program and records the
/// reason in `LowerStats.skipped`.
fn lower_fn(fd: &ResolvedFnDef, program: &mut MirProgram) -> Result<MirFn, SkipReason> {
    let ResolvedFnBody::Block(stmts) = &*fd.body;
    if stmts.is_empty() {
        return Err(SkipReason::EmptyBody);
    }
    // Synthetic-local counter starts past the resolver's slots so the
    // stmt-chain lowering can mint fresh slots for opaque-let temps.
    // Its final value is the function's true local_count — the VM
    // walker must reserve this many frame slots, not just the
    // resolver's `local_count`, or a `STORE_LOCAL` to a synthetic slot
    // overruns the frame.
    let base_local_count = fd
        .resolution
        .as_ref()
        .map_or(fd.params.len() as u32, |r| u32::from(r.local_count));
    let mut next_synthetic_local = base_local_count;
    let body = match stmts.len() {
        1 => {
            // Single-stmt body: must be `Expr`. Bindings alone
            // can't produce a value. No opaque-let temps here, so the
            // counter stays at `base_local_count`.
            let ResolvedStmt::Expr(expr) = &stmts[0] else {
                return Err(SkipReason::BindingOnlyTail);
            };
            lower_expr(expr, program)?
        }
        _ => {
            // Multi-stmt body (wave 3a): right-fold into `Let`
            // chains. Last stmt must be `Expr` — it's the body's
            // final value; everything before it gets opaque-let'd
            // so effectful intermediate calls survive into MIR.
            let resolution = fd
                .resolution
                .as_ref()
                .ok_or(SkipReason::MissingResolution)?;
            lower_stmt_chain(stmts, resolution, &mut next_synthetic_local, program)?
        }
    };
    let local_count = next_synthetic_local;

    let params = fd
        .params
        .iter()
        .enumerate()
        .map(|(i, (name, ty))| MirParam {
            // Wave 1's local numbering matches the resolver's
            // parameter-slot convention (params occupy the lowest
            // slot indices). When wave 2 adds let bindings, they
            // continue from `params.len()`.
            local: LocalId(i as u32),
            name: name.clone(),
            ty: format!("{ty:?}"),
        })
        .collect();
    let effects = fd
        .effects
        .iter()
        .map(|e| MirEffectAnnotation {
            name: e.node.clone(),
        })
        .collect();
    Ok(MirFn {
        fn_id: fd.fn_id,
        name: fd.name.clone(),
        params,
        return_type: format!("{:?}", fd.return_type),
        effects,
        body,
        local_count,
    })
}

/// Expression lowering. Returns `Err(SkipReason)` for any
/// construct outside the supported subset so `lower_fn` can drop
/// the whole function and record the dominant reason.
fn lower_expr(
    expr: &Spanned<ResolvedExpr>,
    program: &mut MirProgram,
) -> Result<Spanned<MirExpr>, SkipReason> {
    let mir = match &expr.node {
        // ── Wave 1 ──────────────────────────────────────────────
        ResolvedExpr::Literal(lit) => MirExpr::Literal(wrap(lit.clone(), expr)),
        ResolvedExpr::Resolved {
            slot,
            name,
            last_use,
        } => {
            let local = super::expr::MirLocal {
                slot: LocalId(u32::from(*slot)),
                last_use: last_use.0,
                name: name.clone(),
            };
            MirExpr::Local(wrap(local, expr))
        }
        ResolvedExpr::BinOp(op, lhs, rhs) => MirExpr::BinOp(wrap(
            MirBinOp {
                op: *op,
                lhs: Box::new(lower_expr(lhs, program)?),
                rhs: Box::new(lower_expr(rhs, program)?),
            },
            expr,
        )),
        ResolvedExpr::Neg(inner) => MirExpr::Neg(Box::new(lower_expr(inner, program)?)),

        // ── Wave 2 ──────────────────────────────────────────────
        ResolvedExpr::Call(callee, args) => {
            let mir_callee = match callee {
                ResolvedCallee::Fn(fn_id) => MirCallee::Fn(*fn_id),
                ResolvedCallee::Builtin(name) => MirCallee::Builtin(program.intern_builtin(name)),
                // Synthesis-only buffer-build / stringify intrinsics from
                // the deforestation pass — carried through so the walker
                // emits the BUFFER_* / CONCAT opcodes instead of dropping
                // the fn to the HIR fallback.
                ResolvedCallee::Intrinsic(intrinsic) => MirCallee::Intrinsic(*intrinsic),
                // First-class fn value in a local slot — `f(x)` where `f`
                // is a `Fn(..)` param / let-bound fn. Dispatches through
                // the VM's `CALL_VALUE` at the walker.
                ResolvedCallee::LocalSlot { slot, last_use, .. } => MirCallee::LocalSlot {
                    slot: *slot,
                    last_use: last_use.0,
                },
                // Unresolved callees (typecheck-rejected recovery) still
                // ride the HIR fallback.
                ResolvedCallee::Unresolved { .. } => {
                    return Err(SkipReason::UnsupportedCallee);
                }
            };
            let mir_args = args
                .iter()
                .map(|e| lower_expr(e, program))
                .collect::<Result<Vec<_>, _>>()?;
            MirExpr::Call(wrap(
                MirCall {
                    callee: mir_callee,
                    args: mir_args,
                },
                expr,
            ))
        }
        ResolvedExpr::Ctor(ResolvedCtor::User { ctor_id, .. }, args) => {
            let mir_args = args
                .iter()
                .map(|e| lower_expr(e, program))
                .collect::<Result<Vec<_>, _>>()?;
            MirExpr::Construct(wrap(
                MirConstruct {
                    ctor: MirCtor::User(*ctor_id),
                    args: mir_args,
                },
                expr,
            ))
        }
        // Wave 3c-i: built-in ctors (`Result.Ok` / `Result.Err` /
        // `Option.Some` / `Option.None`) lower through
        // `MirCtor::Builtin`, riding the same `MirConstruct` shape
        // as user ctors. Backends pick their final emit; until
        // then `Construct(Builtin(_), …)` is the canonical form.
        ResolvedExpr::Ctor(ResolvedCtor::Builtin(bc), args) => {
            let mir_args = args
                .iter()
                .map(|e| lower_expr(e, program))
                .collect::<Result<Vec<_>, _>>()?;
            MirExpr::Construct(wrap(
                MirConstruct {
                    ctor: MirCtor::Builtin(*bc),
                    args: mir_args,
                },
                expr,
            ))
        }
        ResolvedExpr::Ctor(ResolvedCtor::Unresolved { .. }, _) => {
            return Err(SkipReason::UnresolvedCtor);
        }
        // User records carry a `TypeId`; built-in product types
        // (`HttpResponse`, `Header`, `Buffer`, …) carry `None` and ride
        // their canonical `type_name`. Both lower the same shape — the
        // walker resolves the arena type via `TypeId` when present, else
        // by name.
        ResolvedExpr::RecordCreate {
            type_id,
            type_name,
            fields,
        } => {
            let mir_fields = fields
                .iter()
                .map(|(name, value)| {
                    Ok(MirRecordField {
                        name: name.clone(),
                        value: lower_expr(value, program)?,
                    })
                })
                .collect::<Result<Vec<_>, SkipReason>>()?;
            MirExpr::RecordCreate(wrap(
                MirRecordCreate {
                    type_id: *type_id,
                    type_name: type_name.clone(),
                    fields: mir_fields,
                },
                expr,
            ))
        }
        ResolvedExpr::RecordUpdate {
            type_id,
            type_name,
            base,
            updates,
        } => {
            let mir_updates = updates
                .iter()
                .map(|(name, value)| {
                    Ok(MirRecordField {
                        name: name.clone(),
                        value: lower_expr(value, program)?,
                    })
                })
                .collect::<Result<Vec<_>, SkipReason>>()?;
            MirExpr::RecordUpdate(wrap(
                MirRecordUpdate {
                    base: Box::new(lower_expr(base, program)?),
                    type_id: *type_id,
                    type_name: type_name.clone(),
                    updates: mir_updates,
                },
                expr,
            ))
        }
        ResolvedExpr::Attr(base, field) => MirExpr::Project(wrap(
            MirProject {
                base: Box::new(lower_expr(base, program)?),
                field: field.clone(),
            },
            expr,
        )),

        // ── Wave 3b ─────────────────────────────────────────────
        ResolvedExpr::Match { subject, arms } => {
            let mir_subject = lower_expr(subject, program)?;
            let mir_arms = arms
                .iter()
                .map(|a| lower_arm(a, program))
                .collect::<Result<Vec<_>, _>>()?;
            MirExpr::Match(wrap(
                MirMatch {
                    subject: Box::new(mir_subject),
                    arms: mir_arms,
                },
                expr,
            ))
        }

        // ── Wave 3c-ii ──────────────────────────────────────────
        // `expr?` — `?` propagation. RFC-pinned: stays a node.
        // Backends pick the final shape (Rust `?`, VM tag-check +
        // early return, wasm-gc `br_on_struct`). The bind-and-
        // propagate form `let x = step()?; body` composes via Let
        // wrapping a Try (wave 3a's right-fold does this for free
        // — `step()?` lowers to `Try(step())` and the Let-chain
        // walker places it on the Let's `value` side).
        ResolvedExpr::ErrorProp(inner) => MirExpr::Try(Box::new(lower_expr(inner, program)?)),

        // ── Wave 3c-iii ─────────────────────────────────────────
        // Tail call — same SCC as the surrounding fn. The TCO pass
        // upstream of MIR already classified the call as tail
        // position and emitted `ResolvedExpr::TailCall`. Backends
        // pick the final shape: wasm-gc tail-call instructions,
        // VM tail dispatch, Rust loop rewrite. `FnId` is preserved
        // — no name lookup on the backend side.
        ResolvedExpr::TailCall { target, args } => {
            let mir_args = args
                .iter()
                .map(|e| lower_expr(e, program))
                .collect::<Result<Vec<_>, _>>()?;
            MirExpr::TailCall(wrap(
                super::expr::MirTailCall {
                    target: *target,
                    args: mir_args,
                },
                expr,
            ))
        }

        // ── Wave 3c-iv ──────────────────────────────────────────
        // Collection literals — lists, tuples, maps, interpolated
        // strings. Each lowers element-by-element through
        // `lower_expr`; the outer node carries the structural
        // shape so backends pick their build strategy (List
        // prepend chain vs vec-grow, Map flat-hashtable vs HAMT,
        // interpolation buffer-build vs format-string).
        ResolvedExpr::List(items) => {
            let mir_items = items
                .iter()
                .map(|e| lower_expr(e, program))
                .collect::<Result<Vec<_>, _>>()?;
            MirExpr::List(mir_items)
        }
        ResolvedExpr::Tuple(items) => {
            let mir_items = items
                .iter()
                .map(|e| lower_expr(e, program))
                .collect::<Result<Vec<_>, _>>()?;
            MirExpr::Tuple(mir_items)
        }
        ResolvedExpr::MapLiteral(pairs) => {
            let mir_pairs = pairs
                .iter()
                .map(|(k, v)| Ok((lower_expr(k, program)?, lower_expr(v, program)?)))
                .collect::<Result<Vec<_>, SkipReason>>()?;
            MirExpr::MapLiteral(mir_pairs)
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            let mir_parts = parts
                .iter()
                .map(|p| match p {
                    crate::ir::hir::ResolvedStrPart::Literal(s) => {
                        Ok(super::expr::MirStrPart::Literal(s.clone()))
                    }
                    crate::ir::hir::ResolvedStrPart::Parsed(inner) => {
                        Ok(super::expr::MirStrPart::Expr(lower_expr(inner, program)?))
                    }
                })
                .collect::<Result<Vec<_>, SkipReason>>()?;
            MirExpr::InterpolatedStr(mir_parts)
        }

        // ── Wave 3c-v ───────────────────────────────────────────
        // `(a, b, c)!` (raw tuple of Results) or `(a, b, c)?!`
        // (unwrap each Ok, propagate first Err). The compile-time
        // independence mode (`complete` / `cancel` / `sequential`)
        // is NOT carried in MIR per RFC — that's an aver.toml
        // runtime policy. MIR represents only the source-level
        // shape: items + the `unwrap_results` bool.
        ResolvedExpr::IndependentProduct(items, unwrap_results) => {
            let mir_items = items
                .iter()
                .map(|e| lower_expr(e, program))
                .collect::<Result<Vec<_>, _>>()?;
            MirExpr::IndependentProduct(wrap(
                super::expr::MirIndependentProduct {
                    items: mir_items,
                    unwrap_results: *unwrap_results,
                },
                expr,
            ))
        }

        // ── Final catch-all ─────────────────────────────────────
        ResolvedExpr::Ident(_) => return Err(SkipReason::UnresolvedIdent),
    };
    Ok(wrap(mir, expr))
}

/// Wave 3b — lower one resolved match arm. Pattern bindings draw
/// their `LocalId`s from `arm.binding_slots`, which the resolver
/// fills in preorder of the bindings as they appear in the
/// pattern (same walk as `ast_rewrite::pattern_binding_names`).
/// Returns `Err(SkipReason)` for built-in / unresolved ctor
/// patterns or for slot-count desyncs.
fn lower_arm(arm: &ResolvedMatchArm, program: &mut MirProgram) -> Result<MirMatchArm, SkipReason> {
    let slots: &[u16] = arm.binding_slots.get().map(Vec::as_slice).unwrap_or(&[]);
    let mut cursor = 0usize;
    let pattern = lower_pattern(&arm.pattern, slots, &mut cursor)?;
    if cursor != slots.len() {
        return Err(SkipReason::PatternSlotShortfall);
    }
    let body = lower_expr(&arm.body, program)?;
    Ok(MirMatchArm { pattern, body })
}

/// Wave 3b — lower a `ResolvedPattern` while consuming binding
/// slots from `slots[*cursor..]` in preorder. Returns
/// `Err(SkipReason)` on built-in / unresolved ctor patterns or on
/// a slot shortfall.
fn lower_pattern(
    pattern: &ResolvedPattern,
    slots: &[u16],
    cursor: &mut usize,
) -> Result<MirPattern, SkipReason> {
    Ok(match pattern {
        ResolvedPattern::Wildcard => MirPattern::Wildcard,
        ResolvedPattern::Literal(lit) => MirPattern::Literal(lit.clone()),
        ResolvedPattern::Ident(name) => {
            let slot = take_slot(slots, cursor)?;
            MirPattern::Bind(LocalId(u32::from(slot)), name.clone())
        }
        ResolvedPattern::EmptyList => MirPattern::EmptyList,
        ResolvedPattern::Cons(head_name, tail_name) => {
            let head = take_slot(slots, cursor)?;
            let tail = take_slot(slots, cursor)?;
            MirPattern::Cons {
                head: LocalId(u32::from(head)),
                head_name: head_name.clone(),
                tail: LocalId(u32::from(tail)),
                tail_name: tail_name.clone(),
            }
        }
        ResolvedPattern::Tuple(items) => {
            let mir_items = items
                .iter()
                .map(|p| lower_pattern(p, slots, cursor))
                .collect::<Result<Vec<_>, _>>()?;
            MirPattern::Tuple(mir_items)
        }
        ResolvedPattern::Ctor(ResolvedCtor::User { ctor_id, .. }, names) => {
            let bindings = take_pattern_bindings(slots, cursor, names.len())?;
            MirPattern::Ctor {
                ctor: MirCtor::User(*ctor_id),
                bindings,
                binding_names: names.clone(),
            }
        }
        // Wave 3c-i — built-in ctor patterns ride the same shape
        // as user-ctor patterns via `MirCtor::Builtin`.
        ResolvedPattern::Ctor(ResolvedCtor::Builtin(bc), names) => {
            let bindings = take_pattern_bindings(slots, cursor, names.len())?;
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(*bc),
                bindings,
                binding_names: names.clone(),
            }
        }
        // Unresolved ctors still drop — typechecker already
        // surfaced the error; MIR refuses to emit half-resolved
        // identity.
        ResolvedPattern::Ctor(ResolvedCtor::Unresolved { .. }, _) => {
            return Err(SkipReason::UnresolvedCtor);
        }
    })
}

/// Helper for ctor-pattern lowering — consume `arity` slots in
/// preorder. Used by both user-ctor and built-in-ctor branches so
/// the slot-cursor advance rule lives in one place.
fn take_pattern_bindings(
    slots: &[u16],
    cursor: &mut usize,
    arity: usize,
) -> Result<Vec<LocalId>, SkipReason> {
    let mut bindings = Vec::with_capacity(arity);
    for _ in 0..arity {
        let slot = take_slot(slots, cursor)?;
        bindings.push(LocalId(u32::from(slot)));
    }
    Ok(bindings)
}

fn take_slot(slots: &[u16], cursor: &mut usize) -> Result<u16, SkipReason> {
    let slot = *slots.get(*cursor).ok_or(SkipReason::PatternSlotShortfall)?;
    *cursor += 1;
    Ok(slot)
}

/// Wrap a freshly-lowered node in `Spanned` while inheriting the
/// source's line. Phase 6 wave 1: propagate the HIR-side type
/// stamp into MIR so downstream consumers (VM walker's typed
/// opcodes, future inliner / monomorphizer) can read
/// `mir_expr.ty()` without re-deriving the type.
fn wrap<T, U>(node: T, source: &Spanned<U>) -> Spanned<T> {
    let ty = std::sync::OnceLock::new();
    if let Some(t) = source.ty() {
        let _ = ty.set(t.clone());
    }
    Spanned {
        node,
        line: source.line,
        ty,
    }
}

/// Right-fold a list of `ResolvedStmt`s into a single `MirExpr`
/// via `Let` nesting. Wave 3a — the first lowering that produces
/// multi-stmt bodies.
///
/// Rules:
/// - The last stmt must be `Stmt::Expr`; it becomes the innermost
///   body of the resulting `Let` chain.
/// - `Stmt::Binding { name, value }` uses the slot the resolver
///   already assigned (`resolution.local_slots[name]`) as the
///   binding's `LocalId`. Missing slot → the fn is skipped.
/// - `Stmt::Expr` at non-tail position is treated as a let
///   binding to a fresh synthetic `LocalId` so its (potentially
///   effectful) value still gets evaluated. The synthetic
///   counter starts at `resolution.local_count` so it can't
///   collide with the resolver's own slots.
fn lower_stmt_chain(
    stmts: &[ResolvedStmt],
    resolution: &FnResolution,
    next_synthetic_local: &mut u32,
    program: &mut MirProgram,
) -> Result<Spanned<MirExpr>, SkipReason> {
    let (last, rest) = stmts.split_last().ok_or(SkipReason::EmptyBody)?;
    // Last stmt must produce a value.
    let ResolvedStmt::Expr(tail_expr) = last else {
        return Err(SkipReason::BindingOnlyTail);
    };
    let mut body = lower_expr(tail_expr, program)?;

    // Right-fold: walk earlier stmts in reverse, wrapping each
    // around the accumulating `body`.
    for stmt in rest.iter().rev() {
        let (binding, binding_name, value_expr) = match stmt {
            ResolvedStmt::Binding { name, value, .. } if name == "_" => {
                // Discard binding `_ = effect()?` — the idiomatic "run an
                // effect, drop the Ok, continue" form. The resolver
                // assigns `_` no slot (it returns `u16::MAX` and never
                // inserts it), so there's nothing to look up. Evaluate the
                // value for its effects under a fresh synthetic slot the
                // body never reads, exactly as a non-tail `Stmt::Expr`.
                let fresh = LocalId(*next_synthetic_local);
                *next_synthetic_local += 1;
                (fresh, String::new(), value)
            }
            ResolvedStmt::Binding { name, value, .. } => {
                let slot = *resolution
                    .local_slots
                    .get(name)
                    .ok_or(SkipReason::BindingSlotLookupMissing)?;
                (LocalId(u32::from(slot)), name.clone(), value)
            }
            ResolvedStmt::Expr(expr) => {
                let fresh = LocalId(*next_synthetic_local);
                *next_synthetic_local += 1;
                // Synthetic introduction — no source-level name.
                (fresh, String::new(), expr)
            }
        };
        let mir_value = lower_expr(value_expr, program)?;
        let span = mir_value.line;
        body = Spanned {
            node: MirExpr::Let(Spanned {
                node: MirLet {
                    binding,
                    binding_name,
                    value: Box::new(mir_value),
                    body: Box::new(body),
                },
                line: span,
                ty: std::sync::OnceLock::new(),
            }),
            line: span,
            ty: std::sync::OnceLock::new(),
        };
    }
    Ok(body)
}
