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
//! After Phase 4h, every `MirExpr` variant has walker coverage.
//! Match accepts the flat-pattern subset (4g-1…5); nested
//! structural subpatterns inside a Tuple still drop to HIR.
//! Bytecode parity holds on the generic-emit path; HIR's
//! specialised opcodes (MATCH_DISPATCH_CONST table fast-path,
//! per-builtin opcodes like LIST_LEN / MAP_GET / UNWRAP_OR)
//! produce different bytes but identical runtime — that's
//! Phase 6 work with type-stamp propagation.

use crate::ast::Literal;
use crate::ast::Spanned;
use crate::ir::hir::BuiltinCtor;
use crate::ir::mir::{
    LocalId, MirCall, MirCallee, MirCtor, MirExpr, MirFn, MirLet, MirPattern, MirProgram,
    MirStrPart,
};
use crate::nan_value::NanValue;
use crate::vm::builtin::VmBuiltin;
use crate::vm::opcode::*;

use super::VmSymbolTable;
use super::resolve_helpers::buffer_intrinsic_opcode;

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

impl MirVmUnsupported {
    /// Surface a walker rejection as a `CompileError`. `compile_top_level`
    /// pre-checks every lowered top-level expression with
    /// `mir_expr_compilable` before committing to the MIR walk, so this
    /// only fires on the optimistic edge cases `can_compile` reports as
    /// compilable (e.g. an unknown builtin name) — a real internal error
    /// rather than a routine fallback.
    pub(super) fn into_compile_error(self, context: &str) -> CompileError {
        match self {
            MirVmUnsupported::InnerError(e) => e,
            MirVmUnsupported::UnsupportedExpr(what) => CompileError {
                msg: format!("internal error: VM backend cannot lower `{what}` in {context}"),
            },
            MirVmUnsupported::UnsupportedCallee => CompileError {
                msg: format!(
                    "internal error: VM backend hit an unsupported callee shape in {context}"
                ),
            },
        }
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
            let local = &spanned_local.node;
            // Phase 6 wave 4: emit MOVE_LOCAL when this is the
            // last read of the slot in the enclosing fn body.
            // Mirrors HIR's last-use revival; skips ref-count
            // bumps + lets the VM yard reclaim the slot's heap
            // value immediately.
            fc.emit_op(if local.last_use {
                MOVE_LOCAL
            } else {
                LOAD_LOCAL
            });
            fc.emit_u8(local.slot.0 as u8);
            Ok(())
        }
        MirExpr::BinOp(spanned_binop) => {
            let bop = &spanned_binop.node;
            compile_mir_expr(fc, &bop.lhs)?;
            compile_mir_expr(fc, &bop.rhs)?;
            // Phase 6 wave 1: type stamps now propagate from HIR
            // into MIR sub-nodes via `lower::wrap`. When both
            // operands carry a primitive numeric stamp, emit the
            // typed opcode (ADD_INT / ADD_FLOAT / …); otherwise
            // fall back to the generic dispatcher.
            emit_binop_typed(fc, bop.op, bop.lhs.ty(), bop.rhs.ty());
            Ok(())
        }
        MirExpr::Neg(inner) => {
            compile_mir_expr(fc, inner)?;
            // Same wave: pick the typed NEG when the operand
            // carries a primitive numeric stamp. Float's typed
            // path preserves `-0.0` IEEE-754 semantics (the
            // generic NEG bounces through `0 - x`).
            let op = match inner.ty() {
                Some(crate::ast::Type::Int) => NEG_INT,
                Some(crate::ast::Type::Float) => NEG_FLOAT,
                _ => NEG,
            };
            fc.emit_op(op);
            Ok(())
        }
        MirExpr::Let(spanned_let) => {
            let MirLet {
                binding,
                binding_name: _,
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
                    // Always plain CALL_KNOWN, matching the HIR walker
                    // exactly. The owned mask is inert for a known
                    // user-fn call at the call boundary (the callee
                    // derives its parameter ownership from its own alias
                    // analysis, and the runtime reads the trailing owned
                    // byte but ignores it), so emitting CALL_KNOWN_OWNED
                    // bought nothing — and it desynced the leaf /
                    // parent-thin classifier, which only recognizes
                    // CALL_KNOWN as a call. A fn calling out via
                    // CALL_KNOWN_OWNED was wrongly flagged `leaf=true`,
                    // its callers' plain CALL_KNOWN got upgraded to the
                    // frameless CALL_LEAF, and the non-leaf fn was then
                    // invoked without a CallFrame → VM out-of-bounds
                    // crash on the shipped default path. A future
                    // cross-call ownership pass that re-enables the owned
                    // variant must also teach the classifier about it
                    // (CALL_KNOWN_OWNED is recognized there now as
                    // defense, but no longer emitted here).
                    fc.emit_op(CALL_KNOWN);
                    fc.emit_u16(vm_fn_id as u16);
                    fc.emit_u8(args.len() as u8);
                    Ok(())
                }
                MirCallee::Builtin(id) => {
                    let name = fc.mir_program.map(|p| p.builtin_name(*id)).unwrap_or("");
                    let builtin =
                        lookup_vm_builtin(name).ok_or(MirVmUnsupported::UnsupportedCallee)?;
                    // Compound leaf-op recognition — parity with the HIR
                    // walker's fused VECTOR_GET_OR / VECTOR_SET_OR_KEEP.
                    // Without it `Option.withDefault(Vector.set(v, i, x), v)`
                    // emits a generic VECTOR_SET + UNWRAP_OR pair that
                    // allocates the intermediate `Option<Vector>` and skips
                    // the in-place mutate / OOB-keep fast path (~+25% on the
                    // vector_ops accumulator loop once MIR became the default).
                    if builtin == VmBuiltin::OptionWithDefault
                        && try_emit_vector_compound(fc, args)?
                    {
                        return Ok(());
                    }
                    for arg in args {
                        compile_mir_expr(fc, arg)?;
                    }
                    // Phase 6 wave 3 — pick the specialised
                    // single-opcode emit when the builtin has
                    // one (mirror of HIR's `emit_builtin_after_args`
                    // dispatch). Owned/CALL_BUILTIN_OWNED variants
                    // wait for wave 4's last-use revival; until
                    // then `owned_mask = 0` everywhere on the
                    // MIR path, same as Phase 4e.
                    let owned_mask = compute_builtin_owned_mask(args, fc);
                    match builtin {
                        VmBuiltin::ListLen => fc.emit_op(LIST_LEN),
                        VmBuiltin::ListPrepend => fc.emit_op(LIST_PREPEND),
                        VmBuiltin::VectorGet => fc.emit_op(VECTOR_GET),
                        VmBuiltin::VectorSet if owned_mask != 0 => {
                            // Phase 6 wave 4: HIR routes the
                            // owned-VectorSet through CALL_BUILTIN_OWNED
                            // (with take optimization).
                            let symbol_id = fc.symbols.intern_builtin(builtin).map_err(|e| {
                                MirVmUnsupported::InnerError(CompileError {
                                    msg: format!("MIR-VM: intern_builtin failed: {e:?}"),
                                })
                            })?;
                            fc.emit_op(CALL_BUILTIN_OWNED);
                            fc.emit_u32(symbol_id);
                            fc.emit_u8(args.len() as u8);
                            fc.emit_u8(owned_mask);
                        }
                        VmBuiltin::VectorSet => fc.emit_op(VECTOR_SET),
                        VmBuiltin::OptionWithDefault => fc.emit_op(UNWRAP_OR),
                        VmBuiltin::ResultWithDefault => fc.emit_op(UNWRAP_RESULT_OR),
                        _ => {
                            let symbol_id = fc.symbols.intern_builtin(builtin).map_err(|e| {
                                MirVmUnsupported::InnerError(CompileError {
                                    msg: format!("MIR-VM: intern_builtin failed: {e:?}"),
                                })
                            })?;
                            if owned_mask != 0 {
                                fc.emit_op(CALL_BUILTIN_OWNED);
                                fc.emit_u32(symbol_id);
                                fc.emit_u8(args.len() as u8);
                                fc.emit_u8(owned_mask);
                            } else {
                                fc.emit_op(CALL_BUILTIN);
                                fc.emit_u32(symbol_id);
                                fc.emit_u8(args.len() as u8);
                            }
                        }
                    }
                    Ok(())
                }
                MirCallee::Intrinsic(intrinsic) => {
                    // Mirror of the HIR walker's `compile_intrinsic_call`:
                    // the deforestation pass's buffer-build ops map to
                    // dedicated BUFFER_* opcodes; `__to_str` lowers to the
                    // CONCAT-against-empty stringify trick.
                    match buffer_intrinsic_opcode(*intrinsic) {
                        Some((opcode, arity)) => {
                            if args.len() != arity {
                                return Err(MirVmUnsupported::InnerError(CompileError {
                                    msg: format!(
                                        "intrinsic {} expects {arity} arg(s), got {}",
                                        intrinsic.name(),
                                        args.len()
                                    ),
                                }));
                            }
                            for arg in args {
                                compile_mir_expr(fc, arg)?;
                            }
                            fc.emit_op(opcode);
                        }
                        None => {
                            // `__to_str(x)`: CONCAT against an empty string
                            // — CONCAT calls `NanValue::repr` on both sides,
                            // so any value lowers to its display string.
                            if args.len() != 1 {
                                return Err(MirVmUnsupported::InnerError(CompileError {
                                    msg: format!(
                                        "intrinsic {} expects 1 arg, got {}",
                                        intrinsic.name(),
                                        args.len()
                                    ),
                                }));
                            }
                            compile_mir_expr(fc, &args[0])?;
                            let empty = fc.nan_literal(&Literal::Str(String::new()));
                            let idx = fc.add_constant(empty);
                            fc.emit_op(LOAD_CONST);
                            fc.emit_u16(idx);
                            fc.emit_op(CONCAT);
                        }
                    }
                    Ok(())
                }
                MirCallee::LocalSlot {
                    slot,
                    last_use,
                    name: _,
                } => {
                    // First-class fn value: push the slot (the callee),
                    // then the args, then dynamic-dispatch via CALL_VALUE.
                    // Mirror of the HIR walker's compile_call fallback +
                    // compile_callee_as_value(LocalSlot). CALL_VALUE reads
                    // the callee at `stack.len() - 1 - argc`, so it must be
                    // pushed before the args.
                    fc.emit_op(if *last_use { MOVE_LOCAL } else { LOAD_LOCAL });
                    fc.emit_u8(*slot as u8);
                    for arg in args {
                        compile_mir_expr(fc, arg)?;
                    }
                    fc.emit_op(CALL_VALUE);
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

        // ── Phase 6: fn referenced as a value ───────────────────
        // `callWith(dbl)` passes `dbl` — a top-level fn / builtin in
        // value position. Reuse the HIR walker's `compile_ident`
        // verbatim: it tries local slot → global → module-scope fn
        // (symbol_ref of the qualified name) → interned symbol with a
        // kind. Sharing the path guarantees byte-identical emit with
        // the HIR walker, and an unresolved name surfaces as the same
        // `CompileError` (folded to `InnerError`, so a malformed input
        // still drops to the HIR fallback rather than miscompiling).
        MirExpr::FnValue(name) => {
            fc.compile_ident(name)?;
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
            // Phase 6 wave 4: derive owned_mask from args' last-
            // use flags BEFORE compiling them (compute looks at
            // the MirExpr tree, not stack state).
            let owned_mask = compute_owned_mask(&tc.args, fc);
            for arg in &tc.args {
                compile_mir_expr(fc, arg)?;
            }
            let target_name = fc.canonical_fn_name(tc.target)?;
            if target_name == fc.name() {
                fc.emit_op(TAIL_CALL_SELF);
                fc.emit_u8(tc.args.len() as u8);
                fc.emit_u8(owned_mask);
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
                fc.emit_u8(owned_mask);
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
            // Resolve to the arena type id + field order. User records
            // carry a `TypeId` → canonical name; built-in records
            // (`HttpResponse`, …) carry no `TypeId` and ride their
            // canonical `type_name` directly (the arena registers
            // built-in record types by that name). The arena's field
            // order is declaration order, which is what RECORD_NEW
            // expects on the stack.
            let qualified_type_name = match rc.type_id {
                Some(id) => fc.canonical_type_name(id)?,
                None => rc.type_name.clone(),
            };
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
            let qualified_type_name = match ru.type_id {
                Some(id) => fc.canonical_type_name(id)?,
                None => ru.type_name.clone(),
            };
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
        MirExpr::IfThenElse(spanned_ite) => {
            // Phase 6 wave 9: direct conditional shape.
            // Emit:
            //   compile_mir_expr(cond)
            //   JUMP_IF_FALSE → else_start
            //   compile_mir_expr(then_branch)
            //   JUMP → end
            //   else_start: compile_mir_expr(else_branch)
            //   end:
            let ite = &spanned_ite.node;
            compile_mir_expr(fc, &ite.cond)?;
            let else_patch = fc.emit_jump(JUMP_IF_FALSE);
            compile_mir_expr(fc, &ite.then_branch)?;
            let end_patch = fc.emit_jump(JUMP);
            fc.patch_jump(else_patch);
            compile_mir_expr(fc, &ite.else_branch)?;
            fc.patch_jump(end_patch);
            Ok(())
        }
        MirExpr::Match(spanned_match) => {
            let m = &spanned_match.node;
            // Phase 6 wave 2 — try the MATCH_DISPATCH_CONST table
            // fast-path before falling back to the linear arm-by-
            // arm emit. When every non-last arm has a literal
            // pattern + literal body and the last arm is a
            // wildcard/bind default, HIR's `compile_match` emits
            // a single jump-table dispatch (MATCH_DISPATCH_CONST)
            // with inline (kind, expected_bits, result_bits)
            // entries — one VM op for the whole match. Mirror it
            // here so MIR matches HIR byte-for-byte on the
            // common shape.
            if try_emit_match_dispatch_const(fc, &m.subject, &m.arms)?.is_some() {
                return Ok(());
            }
            compile_mir_expr(fc, &m.subject)?;

            let mut end_jumps: Vec<usize> = Vec::new();
            let last_idx = m.arms.len() - 1;
            for (i, arm) in m.arms.iter().enumerate() {
                let is_last = i == last_idx;
                let fail_patches: Vec<usize> = if is_last {
                    // Last arm — exhaustive, no pattern check.
                    // Bindings still need extracting (value on
                    // stack, shape known from preceding arm
                    // failures).
                    emit_last_arm_bindings(fc, &arm.pattern)?;
                    Vec::new()
                } else {
                    emit_pattern_check(fc, &arm.pattern)?
                };
                fc.emit_op(POP);
                compile_mir_expr(fc, &arm.body)?;
                if !is_last {
                    end_jumps.push(fc.emit_jump(JUMP));
                    let next_arm_start = fc.offset();
                    for patch in fail_patches {
                        fc.patch_jump_to(patch, next_arm_start);
                    }
                }
            }
            for patch in end_jumps {
                fc.patch_jump(patch);
            }
            Ok(())
        }
        // ── Phase 4h: remaining MirExpr variants ────────────────
        MirExpr::MapLiteral(entries) => {
            // Mirror HIR's `compile_map`: LOAD_CONST an empty
            // `PersistentMap` from the arena, then per entry
            // compile key + value and emit a `MapSet` call.
            let empty_map = fc.arena.push_map(crate::nan_value::PersistentMap::new());
            let nv = NanValue::new_map(empty_map);
            let idx = fc.add_constant(nv);
            fc.emit_op(LOAD_CONST);
            fc.emit_u16(idx);
            for (k, v) in entries {
                compile_mir_expr(fc, k)?;
                compile_mir_expr(fc, v)?;
                let symbol_id = fc.symbols.intern_builtin(VmBuiltin::MapSet).map_err(|e| {
                    MirVmUnsupported::InnerError(CompileError {
                        msg: format!("MIR-VM: intern_builtin(MapSet) failed: {e:?}"),
                    })
                })?;
                fc.emit_op(CALL_BUILTIN);
                fc.emit_u32(symbol_id);
                fc.emit_u8(3);
            }
            Ok(())
        }
        MirExpr::InterpolatedStr(parts) => {
            // `interp_lower` upstream of MIR usually desugars
            // these into buffer-build calls before MIR sees
            // them, so this branch is rarely reached in
            // practice — kept for completeness so the walker
            // covers every `MirExpr` variant.
            if parts.is_empty() {
                let nv = NanValue::new_string_value("", fc.arena);
                let cidx = fc.add_constant(nv);
                fc.emit_op(LOAD_CONST);
                fc.emit_u16(cidx);
                return Ok(());
            }
            let mut first = true;
            for part in parts {
                match part {
                    MirStrPart::Literal(s) => {
                        let nv = NanValue::new_string_value(s, fc.arena);
                        let cidx = fc.add_constant(nv);
                        fc.emit_op(LOAD_CONST);
                        fc.emit_u16(cidx);
                    }
                    MirStrPart::Expr(e) => {
                        compile_mir_expr(fc, e)?;
                        let empty_nv = NanValue::new_string_value("", fc.arena);
                        let empty_const = fc.add_constant(empty_nv);
                        fc.emit_op(LOAD_CONST);
                        fc.emit_u16(empty_const);
                        fc.emit_op(CONCAT);
                    }
                }
                if !first {
                    fc.emit_op(CONCAT);
                }
                first = false;
            }
            Ok(())
        }
        MirExpr::IndependentProduct(spanned_ip) => {
            let ip = &spanned_ip.node;
            // Mirror HIR's `compile_independent_product`. Each
            // item is expected to be a Call (or Try-wrapped
            // Call) so we push the callee as a value followed
            // by its args; the dispatcher then emits CALL_PAR
            // with per-branch arities. If any item shape isn't
            // call-like, fall back to a sequential TUPLE_NEW
            // (the same safety net HIR uses).
            let call_ready = ip.items.iter().all(|item| {
                let inner = match &item.node {
                    MirExpr::Try(boxed) => &boxed.node,
                    other => other,
                };
                matches!(inner, MirExpr::Call(_))
            });
            if !call_ready {
                for item in &ip.items {
                    compile_mir_expr(fc, item)?;
                }
                fc.emit_op(TUPLE_NEW);
                fc.emit_u8(ip.items.len() as u8);
                return Ok(());
            }
            let mut arg_counts: Vec<u8> = Vec::with_capacity(ip.items.len());
            for item in &ip.items {
                let inner_call = match &item.node {
                    MirExpr::Try(boxed) => &boxed.node,
                    other => other,
                };
                let MirExpr::Call(spanned_call) = inner_call else {
                    unreachable!("call_ready preflight just confirmed");
                };
                let call = &spanned_call.node;
                match &call.callee {
                    MirCallee::Fn(fn_id) => {
                        let name = fc.canonical_fn_name(*fn_id)?;
                        let symbol_id = fc.symbols.find(&name).ok_or_else(|| {
                            MirVmUnsupported::InnerError(CompileError {
                                msg: format!("MIR-VM: missing VM symbol for fn `{name}`"),
                            })
                        })?;
                        let idx = fc.add_constant(VmSymbolTable::symbol_ref(symbol_id));
                        fc.emit_op(LOAD_CONST);
                        fc.emit_u16(idx);
                    }
                    MirCallee::Builtin(id) => {
                        let name = fc.mir_program.map(|p| p.builtin_name(*id)).unwrap_or("");
                        let symbol_id = fc.symbols.find(name).ok_or_else(|| {
                            MirVmUnsupported::InnerError(CompileError {
                                msg: format!("MIR-VM: missing VM symbol for builtin `{name}`"),
                            })
                        })?;
                        let idx = fc.add_constant(VmSymbolTable::symbol_ref(symbol_id));
                        fc.emit_op(LOAD_CONST);
                        fc.emit_u16(idx);
                    }
                    // A synthesis intrinsic can't appear as an
                    // independent-product branch callee (intrinsics are
                    // never first-class values); a first-class-fn-valued
                    // IP branch is rare — both fall back conservatively.
                    MirCallee::Intrinsic(_) | MirCallee::LocalSlot { .. } => {
                        return Err(MirVmUnsupported::UnsupportedCallee);
                    }
                }
                for arg in &call.args {
                    compile_mir_expr(fc, arg)?;
                }
                arg_counts.push(call.args.len() as u8);
            }
            fc.emit_op(CALL_PAR);
            fc.emit_u8(ip.items.len() as u8);
            fc.emit_u8(if ip.unwrap_results { 1 } else { 0 });
            for argc in arg_counts {
                fc.emit_u8(argc);
            }
            Ok(())
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

/// Whether the MIR walker can emit bytecode for `expr` without hitting
/// an `MirVmUnsupported`. Used by `compile_top_level` to pre-check a
/// batch of lowered top-level value expressions before committing to
/// the MIR walk, so a mid-emit rejection can't leave half-written
/// bytecode — it falls back to a clean alternative path instead.
pub(super) fn mir_expr_compilable(expr: &Spanned<MirExpr>) -> bool {
    can_compile(expr)
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
                // Phase 6 wave 11: optimistic — the `BuiltinId →
                // name` resolution lives behind `MirProgram` and
                // `can_compile` is a standalone walker. The hot
                // path in `compile_mir_expr` still surfaces
                // `MirVmUnsupported::UnsupportedCallee` when the
                // name isn't in `VmBuiltin::ALL`, and the caller
                // drops back to HIR — so reporting `true` here at
                // worst costs one rejected compilation attempt
                // rather than silently mis-emitting.
                MirCallee::Builtin(_) => true,
                // Buffer-build / stringify intrinsics emit dedicated
                // BUFFER_* / CONCAT opcodes — always compilable.
                MirCallee::Intrinsic(_) => true,
                // First-class fn value → CALL_VALUE dynamic dispatch.
                MirCallee::LocalSlot { .. } => true,
            };
            callee_ok && c.node.args.iter().all(can_compile)
        }
        MirExpr::Return(inner) => can_compile(inner),
        // Phase 6: fn-as-value. Optimistic, same rationale as the
        // `Builtin` callee above — `compile_ident` resolves the
        // symbol on the hot path and surfaces a real error (→ HIR
        // fallback) if the name doesn't resolve.
        MirExpr::FnValue(_) => true,
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
        // Phase 4h additions:
        MirExpr::MapLiteral(entries) => entries
            .iter()
            .all(|(k, v)| can_compile(k) && can_compile(v)),
        MirExpr::InterpolatedStr(parts) => parts.iter().all(|p| match p {
            MirStrPart::Literal(_) => true,
            MirStrPart::Expr(e) => can_compile(e),
        }),
        MirExpr::IndependentProduct(ip) => ip.node.items.iter().all(can_compile),
        // Phase 4i: all `MirPattern` variants supported (Tuple
        // recurses into subpatterns).
        // Phase 6 wave 9: direct conditional shape introduced
        // by `bool_match_to_if`. Every backend (incl. VM)
        // supports it natively as branch + branch.
        MirExpr::IfThenElse(ite) => {
            can_compile(&ite.node.cond)
                && can_compile(&ite.node.then_branch)
                && can_compile(&ite.node.else_branch)
        }
        MirExpr::Match(m) => {
            can_compile(&m.node.subject)
                && m.node
                    .arms
                    .iter()
                    .all(|arm| pattern_supported(&arm.pattern) && can_compile(&arm.body))
        }
    }
}

/// Sentinel `LocalId` value used by the resolver for wildcard
/// bindings (`_`) inside patterns. The HIR walker treats this
/// as "no slot to write — POP the value off the stack instead
/// of `STORE_LOCAL`". MIR carries the sentinel as the same
/// `u32::from(u16::MAX)` so we can detect it here.
const WILDCARD_SLOT_SENTINEL: u32 = u16::MAX as u32;

/// Emit either `STORE_LOCAL slot` for a real binding or `POP`
/// for the wildcard sentinel — mirroring HIR's
/// `bind_top_to_local`.
fn emit_store_or_pop(fc: &mut FnCompiler<'_>, local: LocalId) {
    if local.0 == WILDCARD_SLOT_SENTINEL {
        fc.emit_op(POP);
    } else {
        fc.emit_op(STORE_LOCAL);
        fc.emit_u8(local.0 as u8);
    }
}

/// DUP + (`STORE_LOCAL` or `POP`) — mirror of HIR's
/// `dup_and_bind_top_to_local`. When the binding is the
/// wildcard sentinel, DUP + POP is a no-op, so we emit nothing.
fn emit_dup_and_bind(fc: &mut FnCompiler<'_>, local: LocalId) {
    if local.0 == WILDCARD_SLOT_SENTINEL {
        return;
    }
    fc.emit_op(DUP);
    fc.emit_op(STORE_LOCAL);
    fc.emit_u8(local.0 as u8);
}

/// Recursive predicate: every `MirPattern` variant is now
/// walker-supported. Tuple subpatterns recurse so nested
/// structural patterns work after Phase 4i landed
/// `emit_nested_subpattern`.
fn pattern_supported(p: &MirPattern) -> bool {
    match p {
        MirPattern::Wildcard
        | MirPattern::Bind(_, _)
        | MirPattern::Literal(_)
        | MirPattern::EmptyList
        | MirPattern::Cons { .. }
        | MirPattern::Ctor { .. } => true,
        MirPattern::Tuple(items) => items.iter().all(pattern_supported),
    }
}

/// Emit the pattern check for a non-last arm. Returns the
/// list of `fail_offset` patch positions the caller will fill
/// in to point at the next arm's start. Empty `Vec` = pattern
/// always matches (Wildcard / Bind).
fn emit_pattern_check(
    fc: &mut FnCompiler<'_>,
    pattern: &MirPattern,
) -> Result<Vec<usize>, MirVmUnsupported> {
    match pattern {
        MirPattern::Wildcard => Ok(Vec::new()),
        MirPattern::Bind(local, _name) => {
            // Always matches; DUP the value and bind. Mirror of
            // HIR's `dup_and_bind_top_to_local` on a top-level
            // ident pattern. Wildcard sentinel collapses the
            // DUP+POP to no emit.
            emit_dup_and_bind(fc, *local);
            Ok(Vec::new())
        }
        MirPattern::Literal(Literal::Int(v)) => {
            fc.emit_op(MATCH_INT_LITERAL);
            fc.emit_i64(*v);
            let patch = fc.offset();
            fc.emit_i16(0);
            Ok(vec![patch])
        }
        MirPattern::Literal(lit) => {
            // Generic literal path — DUP + LOAD_CONST + EQ +
            // JUMP_IF_FALSE. Mirror of HIR's fallback for non-Int
            // literals (Bool / Float / Str / Unit).
            fc.emit_op(DUP);
            fc.compile_literal(lit)?;
            fc.emit_op(EQ);
            let patch = fc.emit_jump(JUMP_IF_FALSE);
            Ok(vec![patch])
        }
        MirPattern::EmptyList => {
            fc.emit_op(MATCH_NIL);
            let patch = fc.offset();
            fc.emit_i16(0);
            Ok(vec![patch])
        }
        MirPattern::Cons { head, tail, .. } => {
            fc.emit_op(MATCH_CONS);
            let patch = fc.offset();
            fc.emit_i16(0);
            // Successful match: extract head/tail and bind into
            // the resolver-assigned slots. The HIR walker does
            // the same shape; MIR's `LocalId` directly carries
            // the slot.
            fc.emit_op(DUP);
            fc.emit_op(LIST_HEAD_TAIL);
            emit_store_or_pop(fc, *head);
            emit_store_or_pop(fc, *tail);
            Ok(vec![patch])
        }
        MirPattern::Ctor {
            ctor: MirCtor::User(ctor_id),
            bindings,
            ..
        } => {
            // Resolve `CtorId → arena ctor_id` via the same path
            // the HIR walker uses (symbol table → canonical name
            // → arena type id → variant id → arena ctor id).
            let entry = fc.symbol_table.ctor_entry(*ctor_id);
            let owning_type = entry.owning_type;
            let variant_name = entry.name.clone();
            let qualified_type_name = fc.canonical_type_name(owning_type)?;
            let arena_type_id = fc.resolve_type_id(&qualified_type_name).ok_or_else(|| {
                MirVmUnsupported::InnerError(CompileError {
                    msg: format!(
                        "MIR-VM: unknown arena type `{qualified_type_name}` for ctor pattern"
                    ),
                })
            })?;
            let variant_id = fc
                .arena
                .find_variant_id(arena_type_id, &variant_name)
                .ok_or_else(|| {
                    MirVmUnsupported::InnerError(CompileError {
                        msg: format!(
                            "MIR-VM: unknown variant `{variant_name}` on `{qualified_type_name}`"
                        ),
                    })
                })?;
            let arena_ctor_id =
                fc.arena.find_ctor_id(arena_type_id, variant_id).ok_or_else(|| {
                    MirVmUnsupported::InnerError(CompileError {
                        msg: format!(
                            "MIR-VM: unknown arena ctor id for `{qualified_type_name}.{variant_name}`"
                        ),
                    })
                })?;
            if arena_ctor_id > u16::MAX as u32 {
                return Err(MirVmUnsupported::InnerError(CompileError {
                    msg: format!(
                        "MIR-VM: ctor id too large for MATCH_VARIANT: {qualified_type_name}.{variant_name}"
                    ),
                }));
            }
            fc.emit_op(MATCH_VARIANT);
            fc.emit_u16(arena_ctor_id as u16);
            let patch = fc.offset();
            fc.emit_i16(0);
            // EXTRACT_FIELD doesn't consume the subject — value
            // stays on the stack between field extractions.
            // Wildcard `_` bindings carry the sentinel slot;
            // `emit_store_or_pop` collapses to POP for those.
            for (i, b) in bindings.iter().enumerate() {
                fc.emit_op(EXTRACT_FIELD);
                fc.emit_u8(i as u8);
                emit_store_or_pop(fc, *b);
            }
            Ok(vec![patch])
        }
        MirPattern::Tuple(items) => {
            // Phase 4i — tuple pattern with arbitrarily-nested
            // subpatterns. Emit MATCH_TUPLE for the arity check,
            // then walk each subpattern through
            // `emit_nested_subpattern` (analog of HIR's
            // `compile_extracted_subpattern`) so nested
            // structural patterns (Cons / Ctor / Tuple / …) get
            // a cleanup-jump if they fail mid-extraction.
            fc.emit_op(MATCH_TUPLE);
            fc.emit_u8(items.len() as u8);
            let tuple_fail = fc.offset();
            fc.emit_i16(0);
            let mut all_patches = vec![tuple_fail];
            for (i, sub) in items.iter().enumerate() {
                let nested = emit_nested_subpattern(
                    fc,
                    |fc| {
                        fc.emit_op(EXTRACT_TUPLE_ITEM);
                        fc.emit_u8(i as u8);
                    },
                    sub,
                )?;
                all_patches.extend(nested);
            }
            Ok(all_patches)
        }
        MirPattern::Ctor {
            ctor: MirCtor::Builtin(bc),
            bindings,
            ..
        } => {
            // Built-in wrapper variants (Result.Ok / Result.Err /
            // Option.Some). Option.None is the nullary case —
            // dispatched via the NONE constant compare.
            match bc {
                BuiltinCtor::ResultOk | BuiltinCtor::ResultErr | BuiltinCtor::OptionSome => {
                    let kind: u8 = match bc {
                        BuiltinCtor::ResultOk => 0,
                        BuiltinCtor::ResultErr => 1,
                        BuiltinCtor::OptionSome => 2,
                        BuiltinCtor::OptionNone => unreachable!(),
                    };
                    fc.emit_op(MATCH_UNWRAP);
                    fc.emit_u8(kind);
                    let patch = fc.offset();
                    fc.emit_i16(0);
                    // MATCH_UNWRAP replaces TOS with the inner
                    // value; the binding (when present) takes a
                    // DUP + store-or-pop — same shape the HIR
                    // walker uses (wildcard `_` collapses to no
                    // emit at all, mirroring HIR's `dup_and_bind`
                    // on `_`).
                    if let Some(b) = bindings.first() {
                        emit_dup_and_bind(fc, *b);
                    }
                    Ok(vec![patch])
                }
                BuiltinCtor::OptionNone => {
                    // Nullary: DUP + LOAD_CONST NONE + EQ +
                    // JUMP_IF_FALSE. No bindings to extract.
                    fc.emit_op(DUP);
                    let none_const = fc.add_constant(NanValue::NONE);
                    fc.emit_op(LOAD_CONST);
                    fc.emit_u16(none_const);
                    fc.emit_op(EQ);
                    let patch = fc.emit_jump(JUMP_IF_FALSE);
                    Ok(vec![patch])
                }
            }
        }
    }
}

/// Phase 6 wave 2 — try the MATCH_DISPATCH_CONST table
/// fast-path. Returns `Ok(Some(()))` when the table was emitted
/// (caller skips the linear emit), `Ok(None)` when the arm
/// shape doesn't fit the fast-path (caller proceeds with linear
/// emit). Mirrors HIR's `emit_match_dispatch_const`.
///
/// Fast-path requirements (mirrors HIR's classifier):
/// - At least one dispatchable arm (otherwise the table is
///   pointless).
/// - Every non-last arm: pattern is `Literal(Int | Bool | Unit)`
///   (the bit-comparable subset) with a body that itself
///   reduces to a `Literal` whose `NanValue::bits()` we can
///   inline as the result.
/// - The last arm is the default: `Wildcard` / `Bind`. We do
///   not require its body to be a literal — the opcode pushes
///   the subject back on miss and falls through to the default
///   arm body, which we compile normally.
fn try_emit_match_dispatch_const(
    fc: &mut FnCompiler<'_>,
    subject: &Spanned<MirExpr>,
    arms: &[crate::ir::mir::MirMatchArm],
) -> Result<Option<()>, MirVmUnsupported> {
    if arms.len() < 2 {
        return Ok(None);
    }
    let last_idx = arms.len() - 1;
    let default_arm = &arms[last_idx];
    let default_local = match &default_arm.pattern {
        MirPattern::Wildcard => None,
        MirPattern::Bind(local, _name) => Some(*local),
        _ => return Ok(None),
    };

    // Classify all non-default arms — every one must be a
    // const-dispatchable literal pattern with a const literal
    // body. Anything else aborts the fast-path.
    let mut entries: Vec<(u8, u64, u64)> = Vec::with_capacity(last_idx);
    for arm in &arms[..last_idx] {
        let pattern_lit = match &arm.pattern {
            MirPattern::Literal(lit) => lit,
            _ => return Ok(None),
        };
        let body_lit = match &arm.body.node {
            MirExpr::Literal(spanned_lit) => &spanned_lit.node,
            _ => return Ok(None),
        };
        let expected = literal_dispatch_bits(fc, pattern_lit);
        let result = literal_dispatch_bits(fc, body_lit);
        // Strings would need DISPATCH_KIND_STRING + arena-side
        // interning to compare; keep this wave focused on
        // bit-equal dispatch (Int / Bool / Unit / Float-as-bits).
        // Str / non-bit-comparable literals abort the fast-path.
        if pattern_is_dispatchable_bits(pattern_lit) && body_is_dispatchable_bits(body_lit) {
            entries.push((0u8, expected, result)); // DISPATCH_KIND_EXACT = 0
        } else {
            return Ok(None);
        }
    }

    if entries.is_empty() {
        return Ok(None);
    }

    // ── Emit ────────────────────────────────────────────────
    compile_mir_expr(fc, subject)?;

    fc.emit_op(MATCH_DISPATCH_CONST);
    fc.emit_u8(entries.len() as u8);
    let default_offset_patch = fc.offset();
    fc.emit_i16(0); // default_offset — patched after the table

    for (kind, expected, result) in &entries {
        fc.emit_u8(*kind);
        fc.emit_u64(*expected);
        fc.emit_u64(*result);
    }
    let table_end = fc.offset();

    // On hit: opcode pushes the result and ip lands right after
    // the table — emit a JUMP to skip past the default arm body.
    let hit_skip = fc.emit_jump(JUMP);

    // Default arm starts here. Default offset is relative to
    // `table_end` (the opcode handler adds `default_offset` to
    // ip-after-table-end).
    let default_start = fc.offset();
    let default_rel = (default_start as isize - table_end as isize) as i16;
    let bytes = (default_rel as u16).to_be_bytes();
    fc.code_mut()[default_offset_patch] = bytes[0];
    fc.code_mut()[default_offset_patch + 1] = bytes[1];

    // Default arm body — subject was popped+repushed by the
    // opcode on miss. Bind it if the pattern is `Bind(local)`,
    // then drop it via POP before the body.
    if let Some(local) = default_local {
        emit_dup_and_bind(fc, local);
    }
    fc.emit_op(POP);
    compile_mir_expr(fc, &default_arm.body)?;

    // Patch the hit-skip JUMP to land after the default body.
    fc.patch_jump(hit_skip);
    Ok(Some(()))
}

/// `true` when a literal's runtime bits are dispatch-comparable
/// (Int / Bool / Unit / Float). Strings need a different
/// dispatch kind (interning) so they abort the fast-path.
fn pattern_is_dispatchable_bits(lit: &Literal) -> bool {
    matches!(
        lit,
        Literal::Int(_) | Literal::Bool(_) | Literal::Unit | Literal::Float(_)
    )
}

fn body_is_dispatchable_bits(lit: &Literal) -> bool {
    // String *bodies* could in principle be supported by
    // interning + inline result bits, but HIR's fast-path also
    // skips them. Mirror that.
    pattern_is_dispatchable_bits(lit)
}

fn literal_dispatch_bits(fc: &mut FnCompiler<'_>, lit: &Literal) -> u64 {
    let nv = match lit {
        Literal::Int(i) => NanValue::new_int(*i, fc.arena),
        Literal::Float(f) => NanValue::new_float(*f),
        Literal::Bool(b) => NanValue::new_bool(*b),
        Literal::Unit => NanValue::UNIT,
        Literal::Str(s) => NanValue::new_string_value(s, fc.arena),
    };
    nv.bits()
}

/// Last-arm exhaustive binding extraction. The pattern is
/// guaranteed to match (preceding arms exhausted everything
/// else) so we skip the match-check opcode and just bind
/// whatever the pattern names. Mirror of HIR's last-arm logic
/// in `compile_match`.
fn emit_last_arm_bindings(
    fc: &mut FnCompiler<'_>,
    pattern: &MirPattern,
) -> Result<(), MirVmUnsupported> {
    match pattern {
        MirPattern::Wildcard | MirPattern::Literal(_) | MirPattern::EmptyList => Ok(()),
        MirPattern::Bind(local, _name) => {
            emit_dup_and_bind(fc, *local);
            Ok(())
        }
        MirPattern::Cons { head, tail, .. } => {
            fc.emit_op(DUP);
            fc.emit_op(LIST_HEAD_TAIL);
            emit_store_or_pop(fc, *head);
            emit_store_or_pop(fc, *tail);
            Ok(())
        }
        MirPattern::Ctor {
            ctor: MirCtor::User(_),
            bindings,
            ..
        } => {
            for (i, b) in bindings.iter().enumerate() {
                fc.emit_op(EXTRACT_FIELD);
                fc.emit_u8(i as u8);
                emit_store_or_pop(fc, *b);
            }
            Ok(())
        }
        MirPattern::Ctor {
            ctor: MirCtor::Builtin(bc),
            bindings,
            ..
        } => {
            if let Some(b) = bindings.first() {
                let kind: u8 = match bc {
                    BuiltinCtor::ResultOk => 0,
                    BuiltinCtor::ResultErr => 1,
                    BuiltinCtor::OptionSome => 2,
                    BuiltinCtor::OptionNone => {
                        // Option.None has no bindings — nothing
                        // to extract.
                        return Ok(());
                    }
                };
                fc.emit_op(MATCH_UNWRAP);
                fc.emit_u8(kind);
                fc.emit_i16(0); // no-fail (shape known)
                emit_dup_and_bind(fc, *b);
            }
            Ok(())
        }
        MirPattern::Tuple(items) => {
            // Last-arm Tuple: shape known — for each item emit
            // EXTRACT_TUPLE_ITEM then recurse into the subpattern
            // as if it were itself a last-arm pattern (the outer
            // exhaustiveness propagates inward — every nested
            // subpattern is also guaranteed to match).
            for (i, sub) in items.iter().enumerate() {
                fc.emit_op(EXTRACT_TUPLE_ITEM);
                fc.emit_u8(i as u8);
                emit_last_arm_bindings(fc, sub)?;
                fc.emit_op(POP);
            }
            Ok(())
        }
    }
}

/// Compile a subpattern that operates on an extracted value,
/// with proper cleanup if the inner pattern fails. Mirrors HIR's
/// `compile_extracted_subpattern`:
///
///   emit_subject(fc);             // e.g. EXTRACT_TUPLE_ITEM i
///   <subpattern emit>             // fail_patches
///   POP                           // drop subpattern subject on success
///   if any fail patches:
///     JUMP success_skip
///     cleanup: POP + JUMP outer_fail
///     patch fail_patches → cleanup
///     patch success_skip → here
///     return vec![outer_fail]
///   else:
///     return empty Vec
///
/// The outer-fail patch lets the surrounding match arm jump to
/// the next arm if any nested subpattern fails, without
/// leaking the cleanup state.
fn emit_nested_subpattern<F>(
    fc: &mut FnCompiler<'_>,
    emit_subject: F,
    pattern: &MirPattern,
) -> Result<Vec<usize>, MirVmUnsupported>
where
    F: FnOnce(&mut FnCompiler<'_>),
{
    emit_subject(fc);
    let inner_fail_patches = emit_pattern_check(fc, pattern)?;
    fc.emit_op(POP);

    if inner_fail_patches.is_empty() {
        return Ok(Vec::new());
    }

    let success_skip = fc.emit_jump(JUMP);
    let cleanup_target = fc.offset();
    for patch in inner_fail_patches {
        fc.patch_jump_to(patch, cleanup_target);
    }
    fc.emit_op(POP);
    let outer_fail = fc.emit_jump(JUMP);
    fc.patch_jump(success_skip);
    Ok(vec![outer_fail])
}

/// Extract `(slot, last_use)` if `expr` is a bare local read.
fn mir_local_slot_last_use(expr: &MirExpr) -> Option<(u32, bool)> {
    match expr {
        MirExpr::Local(l) => Some((l.node.slot.0, l.node.last_use)),
        _ => None,
    }
}

/// Recognize the two fused vector compound shapes the HIR walker emits
/// as single opcodes, and emit the same opcode from MIR so the
/// VM-default path keeps the in-place / no-`Option`-alloc fast path:
///
/// - `Option.withDefault(Vector.set(v, i, x), v)` (same `v`) →
///   `VECTOR_SET_OR_KEEP`. `owned` mirrors the HIR rule: last use of the
///   inner vector slot AND the slot is not alias-prone.
/// - `Option.withDefault(Vector.get(v, i), <literal>)` → `VECTOR_GET_OR`
///   with the literal default inlined as a constant.
///
/// Returns `Ok(true)` when it emitted a fused op; `Ok(false)` leaves the
/// generic `UNWRAP_OR` path to the caller.
fn try_emit_vector_compound(
    fc: &mut FnCompiler<'_>,
    args: &[Spanned<MirExpr>],
) -> Result<bool, MirVmUnsupported> {
    if args.len() != 2 {
        return Ok(false);
    }
    let MirExpr::Call(inner_call) = &args[0].node else {
        return Ok(false);
    };
    let MirCall {
        callee: MirCallee::Builtin(inner_id),
        args: inner_args,
    } = &inner_call.node
    else {
        return Ok(false);
    };
    let inner_name = fc
        .mir_program
        .map(|p| p.builtin_name(*inner_id))
        .unwrap_or("");
    match lookup_vm_builtin(inner_name) {
        Some(VmBuiltin::VectorSet) if inner_args.len() == 3 => {
            // The default must be the same vector the set targets —
            // compared by slot, since the two reads carry different
            // last-use bits and a structural compare would miss it.
            let vec = mir_local_slot_last_use(&inner_args[0].node);
            let def = mir_local_slot_last_use(&args[1].node);
            let (Some((vec_slot, vec_last_use)), Some((def_slot, def_last_use))) = (vec, def)
            else {
                return Ok(false);
            };
            if vec_slot != def_slot {
                return Ok(false);
            }
            // Self-keep fusion ownership collapse. The inner `Vector.set`
            // and the `withDefault` default read the SAME slot
            // (vec_slot == def_slot), and the fused `VECTOR_SET_OR_KEEP`
            // returns exactly one of those two handles — so the slot is
            // dead after the op iff EITHER occurrence is its last use.
            // `last_use` annotates only the textually-last read (the
            // default, arg[1]), leaving the inner read last_use=false; OR
            // the two bits so a linearly-threaded vector takes the
            // in-place path. Still gated on `!is_aliased_slot`: the
            // owned-param refinement (`own_param.rs`) is what proves a
            // threaded `Vector`/`Map` param non-aliased, and only then
            // does the in-place set fire — keeping the guard load-bearing.
            let owned = (vec_last_use || def_last_use) && !fc.is_aliased_slot(vec_slot as u16);
            compile_mir_expr(fc, &inner_args[0])?;
            compile_mir_expr(fc, &inner_args[1])?;
            compile_mir_expr(fc, &inner_args[2])?;
            fc.emit_op(VECTOR_SET_OR_KEEP);
            fc.emit_u8(u8::from(owned));
            Ok(true)
        }
        Some(VmBuiltin::VectorGet) if inner_args.len() == 2 => {
            let MirExpr::Literal(lit) = &args[1].node else {
                return Ok(false);
            };
            compile_mir_expr(fc, &inner_args[0])?;
            compile_mir_expr(fc, &inner_args[1])?;
            let default_value = fc.nan_literal(&lit.node);
            let const_idx = fc.add_constant(default_value);
            fc.emit_op(VECTOR_GET_OR);
            fc.emit_u16(const_idx);
            Ok(true)
        }
        _ => Ok(false),
    }
}

/// Tail-call owned mask: bit `i` is set when arg `i` carries a last-use
/// read of slot `i` — i.e. param slot `i` is threaded straight back into
/// the callee's slot `i` and is dead afterwards, so it can be moved
/// rather than copied. Keyed on the arg-index == slot-index convention
/// that only holds for the slot-aligned tail-call args. Caps at 8 args
/// (mask is u8).
///
/// The alias-prone slot guard is load-bearing: the owned path empties
/// the arena slot in place (`take_map_value` / vector take), so marking
/// an aliased `Vector`/`Map` slot owned would mutate a binding the
/// caller still holds.
fn compute_owned_mask(args: &[Spanned<MirExpr>], fc: &FnCompiler<'_>) -> u8 {
    let mut mask = 0u8;
    for (i, arg) in args.iter().enumerate().take(8) {
        if contains_last_use_slot_mir(&arg.node, i as u32) && !fc.is_aliased_slot(i as u16) {
            mask |= 1 << i;
        }
    }
    mask
}

/// Builtin owned mask: bit `i` is set when arg `i` is itself a last-use
/// read of a non-aliased slot (uniquely owned, safe to take in place) —
/// keyed on the arg's OWN slot, NOT the arg index. A builtin's arguments
/// do not line up with slot indices (`Map.set(m, k, v)` reads `m` from
/// whatever slot it was bound to), so the tail-call mask above silently
/// misses them. The runtime honors only bit 0 and only for
/// `Map.set` / `Vector.set` (`invoke_builtin_with_owned`), so a broader
/// mask is harmless for every other builtin. The `!is_aliased_slot`
/// guard stays load-bearing — `own_param` is what makes a linearly
/// threaded `Vector`/`Map` param non-aliased, and only then does the
/// in-place take fire.
fn compute_builtin_owned_mask(args: &[Spanned<MirExpr>], fc: &FnCompiler<'_>) -> u8 {
    let mut mask = 0u8;
    for (i, arg) in args.iter().enumerate().take(8) {
        if let Some((slot, last_use)) = mir_local_slot_last_use(&arg.node)
            && last_use
            && !fc.is_aliased_slot(slot as u16)
        {
            mask |= 1 << i;
        }
    }
    mask
}

/// Recursive search: does `expr` contain a `MirExpr::Local`
/// reading slot `target` with `last_use = true`? Mirror of
/// HIR's `contains_last_use_slot`.
fn contains_last_use_slot_mir(expr: &MirExpr, target: u32) -> bool {
    match expr {
        MirExpr::Local(spanned_local) => {
            spanned_local.node.slot.0 == target && spanned_local.node.last_use
        }
        MirExpr::Call(c) => c
            .node
            .args
            .iter()
            .any(|a| contains_last_use_slot_mir(&a.node, target)),
        MirExpr::TailCall(t) => t
            .node
            .args
            .iter()
            .any(|a| contains_last_use_slot_mir(&a.node, target)),
        MirExpr::BinOp(b) => {
            contains_last_use_slot_mir(&b.node.lhs.node, target)
                || contains_last_use_slot_mir(&b.node.rhs.node, target)
        }
        MirExpr::Neg(inner) => contains_last_use_slot_mir(&inner.node, target),
        MirExpr::Project(p) => contains_last_use_slot_mir(&p.node.base.node, target),
        MirExpr::Try(inner) => contains_last_use_slot_mir(&inner.node, target),
        MirExpr::Construct(c) => c
            .node
            .args
            .iter()
            .any(|a| contains_last_use_slot_mir(&a.node, target)),
        MirExpr::RecordCreate(rc) => rc
            .node
            .fields
            .iter()
            .any(|f| contains_last_use_slot_mir(&f.value.node, target)),
        MirExpr::RecordUpdate(ru) => {
            contains_last_use_slot_mir(&ru.node.base.node, target)
                || ru
                    .node
                    .updates
                    .iter()
                    .any(|f| contains_last_use_slot_mir(&f.value.node, target))
        }
        MirExpr::List(items) | MirExpr::Tuple(items) => items
            .iter()
            .any(|i| contains_last_use_slot_mir(&i.node, target)),
        MirExpr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            contains_last_use_slot_mir(&k.node, target)
                || contains_last_use_slot_mir(&v.node, target)
        }),
        MirExpr::IndependentProduct(ip) => ip
            .node
            .items
            .iter()
            .any(|i| contains_last_use_slot_mir(&i.node, target)),
        MirExpr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            MirStrPart::Expr(e) => contains_last_use_slot_mir(&e.node, target),
            MirStrPart::Literal(_) => false,
        }),
        // Match / IfThenElse / Let / Return: structural
        // recursion stays conservative — we only care about
        // the immediate arg-evaluation context for the
        // owned_mask, so deep recursion into match arms /
        // if branches / let bodies isn't useful.
        MirExpr::Match(_)
        | MirExpr::IfThenElse(_)
        | MirExpr::Let(_)
        | MirExpr::Return(_)
        // FnValue is a bare symbol reference — no slot read.
        | MirExpr::FnValue(_)
        | MirExpr::Literal(_) => false,
    }
}

/// Linear-search lookup `name → VmBuiltin`. Returns `None` for
/// names not in the builtin table — the caller drops back to HIR
/// via `MirVmUnsupported::UnsupportedCallee`. The table is small
/// (~60 entries) so linear scan is fine; a future Phase 6 can
/// cache it.
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

fn emit_binop_typed(
    fc: &mut FnCompiler<'_>,
    op: crate::ast::BinOp,
    lhs_ty: Option<&crate::ast::Type>,
    rhs_ty: Option<&crate::ast::Type>,
) {
    use crate::ast::BinOp::*;
    use crate::ast::Type;
    let both_int = matches!((lhs_ty, rhs_ty), (Some(Type::Int), Some(Type::Int)));
    let both_float = matches!((lhs_ty, rhs_ty), (Some(Type::Float), Some(Type::Float)));
    let lt_op = if both_int {
        LT_INT
    } else if both_float {
        LT_FLOAT
    } else {
        LT
    };
    let gt_op = if both_int {
        GT_INT
    } else if both_float {
        GT_FLOAT
    } else {
        GT
    };
    let add_op = if both_int {
        ADD_INT
    } else if both_float {
        ADD_FLOAT
    } else {
        ADD
    };
    let sub_op = if both_int {
        SUB_INT
    } else if both_float {
        SUB_FLOAT
    } else {
        SUB
    };
    let mul_op = if both_int {
        MUL_INT
    } else if both_float {
        MUL_FLOAT
    } else {
        MUL
    };
    // No DIV_INT — integer division traps on `b == 0` and the
    // generic `arith_div` already does that branch + propagates
    // a typed runtime error.
    let div_op = if both_float { DIV_FLOAT } else { DIV };
    match op {
        Add => fc.emit_op(add_op),
        Sub => fc.emit_op(sub_op),
        Mul => fc.emit_op(mul_op),
        Div => fc.emit_op(div_op),
        Eq => fc.emit_op(if both_int { EQ_INT } else { EQ }),
        Lt => fc.emit_op(lt_op),
        Gt => fc.emit_op(gt_op),
        // `Neq` / `Lte` / `Gte` invert the corresponding
        // comparison (same composition HIR uses).
        Neq => {
            fc.emit_op(if both_int { EQ_INT } else { EQ });
            fc.emit_op(NOT);
        }
        Lte => {
            fc.emit_op(gt_op);
            fc.emit_op(NOT);
        }
        Gte => {
            fc.emit_op(lt_op);
            fc.emit_op(NOT);
        }
    }
}
