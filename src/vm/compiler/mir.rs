//! MIR → VM bytecode lowering (Phase 4 vertical slice).
//!
//! Parallel to the existing `super::expr` module which walks
//! `ResolvedExpr` (HIR), this module walks `crate::ir::mir::MirExpr`
//! and emits the same opcodes. The point is to prove the VM can
//! consume MIR identically — same `FnChunk`, same `NanValue`
//! results on the parity corpus.
//!
//! ## Scope (Phase 4 PoC)
//!
//! Subset of `MirExpr` covered here:
//! - `Literal` — same opcodes as `super::expr::compile_literal`.
//! - `Local(LocalId)` — `LOAD_LOCAL` (no `MOVE_LOCAL` / last-use
//!   optimization yet; MIR doesn't carry last-use bits at this
//!   wave).
//! - `BinOp` — typed dispatch via `emit_binop_typed`; falls back
//!   to the generic opcode when MIR doesn't carry a type stamp.
//! - `Neg` — same untyped fallback for now.
//! - `Let { binding, value, body }` — value first, `STORE_LOCAL`
//!   into the binding slot, body next; body becomes the fn's
//!   return value.
//! - `Call { callee, args }` — `MirCallee::Fn(FnId)` resolves
//!   through the entry's `SymbolTable` (the same path the HIR
//!   compiler uses).
//! - `Return(inner)` — explicit early-return form.
//!
//! Everything else (Match, Try, TailCall, Construct, Record*,
//! Project, List, Tuple, MapLiteral, InterpolatedStr,
//! IndependentProduct) returns `Err(MirVmUnsupported)` so the
//! caller can fall back to HIR compilation for that fn.

use crate::ast::Literal;
use crate::ast::Spanned;
use crate::ir::hir::BuiltinCtor;
use crate::ir::mir::{MirCall, MirCallee, MirCtor, MirExpr, MirFn, MirLet, MirPattern, MirProgram};
use crate::nan_value::NanValue;
use crate::vm::builtin::VmBuiltin;
use crate::vm::opcode::*;

use super::{CompileError, FnCompiler};

/// Reasons the MIR vertical slice can't compile a given MIR fn yet.
/// The Phase 4 callers fall back to the HIR path (`super::compile_fn`)
/// when this fires.
#[derive(Debug)]
pub enum MirVmUnsupported {
    /// Hit a `MirExpr` variant outside the Phase 4 subset.
    UnsupportedExpr(&'static str),
    /// Callee shape not yet covered (builtin / non-FnId).
    UnsupportedCallee,
    /// Underlying `FnCompiler` reported a compile error mid-emit.
    InnerError(CompileError),
}

impl From<CompileError> for MirVmUnsupported {
    fn from(e: CompileError) -> Self {
        MirVmUnsupported::InnerError(e)
    }
}

/// Walk a `MirExpr` and emit VM bytecode into the supplied
/// `FnCompiler`. Returns `Err(MirVmUnsupported)` for any MirExpr
/// variant outside the Phase 4 subset — the caller drops back to
/// HIR compilation in that case.
pub(super) fn compile_mir_expr(
    fc: &mut FnCompiler<'_>,
    expr: &Spanned<MirExpr>,
) -> Result<(), MirVmUnsupported> {
    fc.note_line(expr.line);
    match &expr.node {
        MirExpr::Literal(lit) => {
            fc.compile_literal(&lit.node)?;
            Ok(())
        }
        MirExpr::Local(spanned_local) => {
            let slot = spanned_local.node.0;
            // No last-use info on MIR yet (Phase 6 work), so always
            // emit LOAD_LOCAL — matches a no-last-use HIR slot.
            fc.emit_op(LOAD_LOCAL);
            fc.emit_u8(slot as u8);
            Ok(())
        }
        MirExpr::BinOp(spanned_binop) => {
            let bop = &spanned_binop.node;
            compile_mir_expr(fc, &bop.lhs)?;
            compile_mir_expr(fc, &bop.rhs)?;
            // No type stamp on MIR sub-nodes yet; emit the generic
            // BinOp opcode and let the VM's runtime tag dispatch
            // pick the typed path. Phase 6's type-stamp propagation
            // can later switch this to `emit_binop_typed`.
            emit_binop_generic(fc, bop.op);
            Ok(())
        }
        MirExpr::Neg(inner) => {
            compile_mir_expr(fc, inner)?;
            fc.emit_op(NEG);
            Ok(())
        }
        MirExpr::Let(spanned_let) => {
            let MirLet {
                binding,
                value,
                body,
            } = &spanned_let.node;
            compile_mir_expr(fc, value)?;
            fc.emit_op(STORE_LOCAL);
            fc.emit_u8(binding.0 as u8);
            compile_mir_expr(fc, body)
        }
        MirExpr::Call(spanned_call) => {
            let MirCall { callee, args } = &spanned_call.node;
            match callee {
                MirCallee::Fn(fn_id) => {
                    for arg in args {
                        compile_mir_expr(fc, arg)?;
                    }
                    let name = fc.canonical_fn_name(*fn_id)?;
                    let vm_fn_id = fc.resolve_fn_id_by_name(&name).ok_or_else(|| {
                        MirVmUnsupported::InnerError(CompileError {
                            msg: format!(
                                "MIR-VM: unresolved fn `{name}` (FnId={fn_id:?}) — \
                                 module not loaded?"
                            ),
                        })
                    })?;
                    fc.emit_op(CALL_KNOWN);
                    fc.emit_u16(vm_fn_id as u16);
                    fc.emit_u8(args.len() as u8);
                    Ok(())
                }
                MirCallee::Builtin(name) => {
                    // Phase 4e — generic CALL_BUILTIN dispatch.
                    // The HIR walker specializes ~6 builtins
                    // (ListLen → LIST_LEN, MapGet → MAP_GET,
                    // OptionWithDefault → UNWRAP_OR, …) into
                    // dedicated opcodes; we don't replicate that
                    // here yet, so bytecode parity only holds for
                    // the generic path. Runtime parity holds for
                    // all builtins — the VM's CALL_BUILTIN
                    // dispatch lands on the same handler the
                    // specialised opcodes wrap.
                    let builtin =
                        lookup_vm_builtin(name).ok_or(MirVmUnsupported::UnsupportedCallee)?;
                    for arg in args {
                        compile_mir_expr(fc, arg)?;
                    }
                    let symbol_id = fc.symbols.intern_builtin(builtin).map_err(|e| {
                        MirVmUnsupported::InnerError(CompileError {
                            msg: format!("MIR-VM: intern_builtin failed: {e:?}"),
                        })
                    })?;
                    fc.emit_op(CALL_BUILTIN);
                    fc.emit_u32(symbol_id);
                    fc.emit_u8(args.len() as u8);
                    Ok(())
                }
            }
        }
        MirExpr::Return(inner) => {
            compile_mir_expr(fc, inner)?;
            fc.emit_op(RETURN);
            Ok(())
        }

        // ── Phase 4c: ctor construction ─────────────────────────
        MirExpr::Construct(spanned_construct) => {
            let c = &spanned_construct.node;
            match c.ctor {
                MirCtor::Builtin(BuiltinCtor::ResultOk) => {
                    emit_constructor_arg(fc, c.args.first())?;
                    fc.emit_op(WRAP);
                    fc.emit_u8(0);
                    Ok(())
                }
                MirCtor::Builtin(BuiltinCtor::ResultErr) => {
                    emit_constructor_arg(fc, c.args.first())?;
                    fc.emit_op(WRAP);
                    fc.emit_u8(1);
                    Ok(())
                }
                MirCtor::Builtin(BuiltinCtor::OptionSome) => {
                    emit_constructor_arg(fc, c.args.first())?;
                    fc.emit_op(WRAP);
                    fc.emit_u8(2);
                    Ok(())
                }
                MirCtor::Builtin(BuiltinCtor::OptionNone) => {
                    let idx = fc.add_constant(NanValue::NONE);
                    fc.emit_op(LOAD_CONST);
                    fc.emit_u16(idx);
                    Ok(())
                }
                MirCtor::User(ctor_id) => {
                    // CtorEntry → (owning_type, variant_name) →
                    // canonical type name → arena type_id +
                    // variant_id. Same path the HIR walker uses
                    // for `ResolvedCtor::User`.
                    let entry = fc.symbol_table.ctor_entry(ctor_id);
                    let owning_type = entry.owning_type;
                    let variant_name = entry.name.clone();
                    let qualified_type_name = fc.canonical_type_name(owning_type)?;
                    let arena_type_id =
                        fc.resolve_type_id(&qualified_type_name).ok_or_else(|| {
                            MirVmUnsupported::InnerError(CompileError {
                                msg: format!(
                                    "MIR-VM: unknown arena type for `{qualified_type_name}` \
                                     (CtorId={ctor_id:?})"
                                ),
                            })
                        })?;
                    let variant_id =
                        fc.arena.find_variant_id(arena_type_id, &variant_name).ok_or_else(
                            || {
                                MirVmUnsupported::InnerError(CompileError {
                                    msg: format!(
                                        "MIR-VM: unknown variant `{variant_name}` on `{qualified_type_name}`"
                                    ),
                                })
                            },
                        )?;
                    for arg in &c.args {
                        compile_mir_expr(fc, arg)?;
                    }
                    fc.emit_op(VARIANT_NEW);
                    fc.emit_u16(arena_type_id as u16);
                    fc.emit_u16(variant_id);
                    fc.emit_u8(c.args.len() as u8);
                    Ok(())
                }
            }
        }

        // ── Phase 4c: record field access ───────────────────────
        MirExpr::Project(spanned_proj) => {
            let p = &spanned_proj.node;
            // RECORD_GET_NAMED is the universal path — VM resolves
            // the field by symbol id at runtime. The HIR walker
            // sometimes specializes to RECORD_GET when it can
            // infer field index statically; that's a Phase 6
            // optimization we skip here.
            compile_mir_expr(fc, &p.base)?;
            let field_symbol_id = fc.symbols.intern_name(&p.field);
            fc.emit_op(RECORD_GET_NAMED);
            fc.emit_u32(field_symbol_id);
            Ok(())
        }

        // ── Phase 4d: `?` propagation ───────────────────────────
        MirExpr::Try(inner) => {
            compile_mir_expr(fc, inner)?;
            fc.emit_op(PROPAGATE_ERR);
            Ok(())
        }

        // ── Phase 4d: tail-call dispatch ────────────────────────
        MirExpr::TailCall(spanned_tail) => {
            let tc = &spanned_tail.node;
            for arg in &tc.args {
                compile_mir_expr(fc, arg)?;
            }
            let target_name = fc.canonical_fn_name(tc.target)?;
            // Self-recursive vs cross-fn dispatch. The HIR walker
            // also derives an `owned_mask` from last-use
            // annotations; MIR doesn't carry last-use bits yet
            // (Phase 6 work), so we emit `0` — bytecode stays
            // semantically equivalent, the optimizer pass can
            // later rebuild the mask off MIR liveness.
            if target_name == fc.name() {
                fc.emit_op(TAIL_CALL_SELF);
                fc.emit_u8(tc.args.len() as u8);
                fc.emit_u8(0);
            } else {
                let vm_fn_id = fc.resolve_fn_id_by_name(&target_name).ok_or_else(|| {
                    MirVmUnsupported::InnerError(CompileError {
                        msg: format!(
                            "MIR-VM: unresolved tail-call target `{target_name}` \
                             (FnId={:?})",
                            tc.target
                        ),
                    })
                })?;
                fc.emit_op(TAIL_CALL_KNOWN);
                fc.emit_u16(vm_fn_id as u16);
                fc.emit_u8(tc.args.len() as u8);
                fc.emit_u8(0);
            }
            Ok(())
        }

        // ── Phase 4f: record + list + tuple builders ────────────
        MirExpr::List(items) => {
            if items.is_empty() {
                fc.emit_op(LIST_NIL);
                return Ok(());
            }
            for item in items {
                compile_mir_expr(fc, item)?;
            }
            fc.emit_op(LIST_NEW);
            fc.emit_u8(items.len() as u8);
            Ok(())
        }
        MirExpr::Tuple(items) => {
            for item in items {
                compile_mir_expr(fc, item)?;
            }
            fc.emit_op(TUPLE_NEW);
            fc.emit_u8(items.len() as u8);
            Ok(())
        }
        MirExpr::RecordCreate(spanned_rc) => {
            let rc = &spanned_rc.node;
            // Resolve TypeId → canonical name → arena type id +
            // field order. Same path the HIR walker takes; MIR
            // carries the `TypeId` already, so we just look up
            // the canonical name to ask the arena for the type
            // metadata (the arena's field order is the
            // declaration order, which is what RECORD_NEW
            // expects on the stack).
            let qualified_type_name = fc.canonical_type_name(rc.type_id)?;
            let arena_type_id = fc.resolve_type_id(&qualified_type_name).ok_or_else(|| {
                MirVmUnsupported::InnerError(CompileError {
                    msg: format!(
                        "MIR-VM: unknown arena type `{qualified_type_name}` for \
                         RecordCreate (TypeId={:?})",
                        rc.type_id
                    ),
                })
            })?;
            let field_names = fc.arena.get_field_names(arena_type_id).to_vec();
            // Push fields in declared order.
            for expected_name in &field_names {
                let field = rc.fields.iter().find(|f| f.name == *expected_name).ok_or_else(
                    || {
                        MirVmUnsupported::InnerError(CompileError {
                            msg: format!(
                                "MIR-VM: missing field `{expected_name}` in record `{qualified_type_name}`"
                            ),
                        })
                    },
                )?;
                compile_mir_expr(fc, &field.value)?;
            }
            fc.emit_op(RECORD_NEW);
            fc.emit_u16(arena_type_id as u16);
            fc.emit_u8(field_names.len() as u8);
            Ok(())
        }
        MirExpr::RecordUpdate(spanned_ru) => {
            let ru = &spanned_ru.node;
            let qualified_type_name = fc.canonical_type_name(ru.type_id)?;
            let arena_type_id = fc.resolve_type_id(&qualified_type_name).ok_or_else(|| {
                MirVmUnsupported::InnerError(CompileError {
                    msg: format!(
                        "MIR-VM: unknown arena type `{qualified_type_name}` for \
                         RecordUpdate (TypeId={:?})",
                        ru.type_id
                    ),
                })
            })?;
            let field_names = fc.arena.get_field_names(arena_type_id).to_vec();
            let mut updated_indices = Vec::with_capacity(ru.updates.len());

            compile_mir_expr(fc, &ru.base)?;

            for (field_idx, field_name) in field_names.iter().enumerate() {
                if let Some(field) = ru.updates.iter().find(|f| f.name == *field_name) {
                    compile_mir_expr(fc, &field.value)?;
                    updated_indices.push(field_idx as u8);
                }
            }
            fc.emit_op(RECORD_UPDATE);
            fc.emit_u16(arena_type_id as u16);
            fc.emit_u8(updated_indices.len() as u8);
            for idx in updated_indices {
                fc.emit_u8(idx);
            }
            Ok(())
        }

        // ── Phase 4g-1: match with Wildcard + Literal(Int) arms ──
        // The HIR walker's `compile_match` is 756 lines and
        // includes fast-paths (MATCH_DISPATCH_CONST, bool-branch
        // optimization) we don't replicate here. This sub-PR
        // handles the smallest reviewable subset: arm patterns
        // restricted to `Wildcard` and `Literal(Int)`. Any other
        // pattern variant in any arm falls back to HIR.
        //
        // Emit shape (linear fallback, mirrors HIR's tail of
        // `compile_match`):
        //   <subject>
        //   per arm (except last):
        //     [MATCH_INT_LITERAL imm fail]  // skipped for Wildcard
        //     POP
        //     <body>
        //     JUMP end
        //     fail: <next arm>
        //   last arm: skip pattern check entirely (exhaustive),
        //             POP, <body>
        //   end:
        MirExpr::Match(spanned_match) => {
            let m = &spanned_match.node;
            if !m.arms.iter().all(|arm| pattern_in_4g_subset(&arm.pattern)) {
                return Err(MirVmUnsupported::UnsupportedExpr("Match (complex pattern)"));
            }
            compile_mir_expr(fc, &m.subject)?;

            let mut end_jumps: Vec<usize> = Vec::new();
            let last_idx = m.arms.len() - 1;
            for (i, arm) in m.arms.iter().enumerate() {
                let is_last = i == last_idx;
                let fail_patch: Option<usize> = if is_last {
                    // Last arm — exhaustive, no pattern check.
                    // Cons bindings must still be extracted
                    // unconditionally (the value is on the
                    // stack, we know the shape matches).
                    if let MirPattern::Cons { head, tail } = &arm.pattern {
                        fc.emit_op(DUP);
                        fc.emit_op(LIST_HEAD_TAIL);
                        fc.emit_op(STORE_LOCAL);
                        fc.emit_u8(head.0 as u8);
                        fc.emit_op(STORE_LOCAL);
                        fc.emit_u8(tail.0 as u8);
                    }
                    None
                } else {
                    emit_pattern_check(fc, &arm.pattern)?
                };
                fc.emit_op(POP);
                compile_mir_expr(fc, &arm.body)?;
                if !is_last {
                    end_jumps.push(fc.emit_jump(JUMP));
                    if let Some(patch) = fail_patch {
                        let next_arm_start = fc.offset();
                        fc.patch_jump_to(patch, next_arm_start);
                    }
                }
            }
            for patch in end_jumps {
                fc.patch_jump(patch);
            }
            Ok(())
        }
        MirExpr::MapLiteral(_) => Err(MirVmUnsupported::UnsupportedExpr("MapLiteral")),
        MirExpr::InterpolatedStr(_) => Err(MirVmUnsupported::UnsupportedExpr("InterpolatedStr")),
        MirExpr::IndependentProduct(_) => {
            Err(MirVmUnsupported::UnsupportedExpr("IndependentProduct"))
        }
    }
}

/// Emit a MIR fn's body into the supplied `FnCompiler` and finish
/// with `RETURN`. Caller has already constructed `fc` with the
/// right arity / local_count / local_slots — same path the HIR
/// compiler takes through `compile_fn_with_scope`.
pub(super) fn compile_mir_fn_body(
    fc: &mut FnCompiler<'_>,
    mir_fn: &MirFn,
) -> Result<(), MirVmUnsupported> {
    compile_mir_expr(fc, &mir_fn.body)?;
    fc.emit_op(RETURN);
    Ok(())
}

/// Convenience: walk a `MirProgram` and report which fns the
/// Phase 4 subset can handle vs which still need HIR fallback.
/// Useful for parity tests + Phase 4 coverage tracking.
pub fn classify_mir_program_coverage(mir: &MirProgram) -> MirVmCoverage {
    let mut covered = 0u32;
    let mut needs_hir_fallback = 0u32;
    for mir_fn in mir.fns.values() {
        if can_compile(&mir_fn.body) {
            covered += 1;
        } else {
            needs_hir_fallback += 1;
        }
    }
    MirVmCoverage {
        covered,
        needs_hir_fallback,
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct MirVmCoverage {
    pub covered: u32,
    pub needs_hir_fallback: u32,
}

fn can_compile(expr: &Spanned<MirExpr>) -> bool {
    match &expr.node {
        MirExpr::Literal(_) => true,
        MirExpr::Local(_) => true,
        MirExpr::BinOp(b) => can_compile(&b.node.lhs) && can_compile(&b.node.rhs),
        MirExpr::Neg(inner) => can_compile(inner),
        MirExpr::Let(l) => can_compile(&l.node.value) && can_compile(&l.node.body),
        MirExpr::Call(c) => {
            let callee_ok = match &c.node.callee {
                MirCallee::Fn(_) => true,
                MirCallee::Builtin(name) => lookup_vm_builtin(name).is_some(),
            };
            callee_ok && c.node.args.iter().all(can_compile)
        }
        MirExpr::Return(inner) => can_compile(inner),
        // Phase 4c additions:
        MirExpr::Construct(c) => c.node.args.iter().all(can_compile),
        MirExpr::Project(p) => can_compile(&p.node.base),
        // Phase 4d additions:
        MirExpr::Try(inner) => can_compile(inner),
        MirExpr::TailCall(t) => t.node.args.iter().all(can_compile),
        // Phase 4f additions:
        MirExpr::List(items) => items.iter().all(can_compile),
        MirExpr::Tuple(items) => items.iter().all(can_compile),
        MirExpr::RecordCreate(rc) => rc.node.fields.iter().all(|f| can_compile(&f.value)),
        MirExpr::RecordUpdate(ru) => {
            can_compile(&ru.node.base) && ru.node.updates.iter().all(|f| can_compile(&f.value))
        }
        // Phase 4g-1/2: match with Wildcard + Literal(Int) + Cons + EmptyList arms.
        MirExpr::Match(m) => {
            can_compile(&m.node.subject)
                && m.node
                    .arms
                    .iter()
                    .all(|arm| pattern_in_4g_subset(&arm.pattern) && can_compile(&arm.body))
        }
        _ => false,
    }
}

/// Phase 4g preflight + can_compile — pattern variants the
/// MIR Match walker handles. Wildcard / Literal(Int) (4g-1),
/// Cons + EmptyList (4g-2). Further variants land in 4g-3+.
fn pattern_in_4g_subset(p: &MirPattern) -> bool {
    matches!(
        p,
        MirPattern::Wildcard
            | MirPattern::Literal(Literal::Int(_))
            | MirPattern::Cons { .. }
            | MirPattern::EmptyList
    )
}

/// Emit the pattern check for a non-last arm. Returns the
/// `fail_offset` patch position the caller will fill in to
/// point at the next arm's start (or `None` when the pattern
/// always matches — currently just `Wildcard`).
fn emit_pattern_check(
    fc: &mut FnCompiler<'_>,
    pattern: &MirPattern,
) -> Result<Option<usize>, MirVmUnsupported> {
    match pattern {
        MirPattern::Wildcard => Ok(None),
        MirPattern::Literal(Literal::Int(v)) => {
            fc.emit_op(MATCH_INT_LITERAL);
            fc.emit_i64(*v);
            let patch = fc.offset();
            fc.emit_i16(0);
            Ok(Some(patch))
        }
        MirPattern::EmptyList => {
            fc.emit_op(MATCH_NIL);
            let patch = fc.offset();
            fc.emit_i16(0);
            Ok(Some(patch))
        }
        MirPattern::Cons { head, tail } => {
            fc.emit_op(MATCH_CONS);
            let patch = fc.offset();
            fc.emit_i16(0);
            // Successful match: extract head/tail and bind into
            // the resolver-assigned slots. The HIR walker does
            // the same shape; MIR's `LocalId` directly carries
            // the slot.
            fc.emit_op(DUP);
            fc.emit_op(LIST_HEAD_TAIL);
            fc.emit_op(STORE_LOCAL);
            fc.emit_u8(head.0 as u8);
            fc.emit_op(STORE_LOCAL);
            fc.emit_u8(tail.0 as u8);
            Ok(Some(patch))
        }
        // Preflight in the caller filters everything else out.
        _ => unreachable!("Phase 4g subset preflight should have filtered this out"),
    }
}

/// Linear-search lookup `name → VmBuiltin`. Returns `None` for
/// names not in the builtin table — the caller drops back to HIR
/// via `MirVmUnsupported::UnsupportedCallee`. The table is small
/// (~60 entries) so linear scan is fine; a future Phase 6 can
/// memoize.
fn lookup_vm_builtin(name: &str) -> Option<VmBuiltin> {
    VmBuiltin::ALL.iter().copied().find(|b| b.name() == name)
}

/// Helper: emit a single ctor arg, or `LOAD_UNIT` when the ctor
/// arg is absent (defensive — built-in Wrap-shaped ctors always
/// take exactly one arg in well-typed Aver, but the lowerer
/// only enforces that at the type level).
fn emit_constructor_arg(
    fc: &mut FnCompiler<'_>,
    arg: Option<&Spanned<MirExpr>>,
) -> Result<(), MirVmUnsupported> {
    match arg {
        Some(a) => compile_mir_expr(fc, a),
        None => {
            fc.emit_op(LOAD_UNIT);
            Ok(())
        }
    }
}

fn emit_binop_generic(fc: &mut FnCompiler<'_>, op: crate::ast::BinOp) {
    use crate::ast::BinOp::*;
    match op {
        Add => fc.emit_op(ADD),
        Sub => fc.emit_op(SUB),
        Mul => fc.emit_op(MUL),
        Div => fc.emit_op(DIV),
        Eq => fc.emit_op(EQ),
        Lt => fc.emit_op(LT),
        Gt => fc.emit_op(GT),
        // `Neq` / `Lte` / `Gte` have no dedicated opcodes —
        // they're invert-of-the-corresponding-comparison.
        Neq => {
            fc.emit_op(EQ);
            fc.emit_op(NOT);
        }
        Lte => {
            fc.emit_op(GT);
            fc.emit_op(NOT);
        }
        Gte => {
            fc.emit_op(LT);
            fc.emit_op(NOT);
        }
    }
}
