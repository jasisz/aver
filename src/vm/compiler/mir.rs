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

use crate::ast::Spanned;
use crate::ir::hir::BuiltinCtor;
use crate::ir::mir::{MirCall, MirCallee, MirCtor, MirExpr, MirFn, MirLet, MirProgram};
use crate::nan_value::NanValue;
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
            let fn_id = match callee {
                MirCallee::Fn(fn_id) => *fn_id,
                MirCallee::Builtin(_) => {
                    return Err(MirVmUnsupported::UnsupportedCallee);
                }
            };
            for arg in args {
                compile_mir_expr(fc, arg)?;
            }
            // Same dispatch path the HIR compiler uses:
            // FnId → canonical name → VM fn_id (u16) via
            // `module_scope` / `code_store.find`.
            let name = fc.canonical_fn_name(fn_id)?;
            let vm_fn_id = fc.resolve_fn_id_by_name(&name).ok_or_else(|| {
                MirVmUnsupported::InnerError(CompileError {
                    msg: format!(
                        "MIR-VM: unresolved fn `{name}` (FnId={fn_id:?}) — \
                         module not loaded?"
                    ),
                })
            })?;
            // CALL_KNOWN layout: fn_id u16, argc u8.
            fc.emit_op(CALL_KNOWN);
            fc.emit_u16(vm_fn_id as u16);
            fc.emit_u8(args.len() as u8);
            Ok(())
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

        // Phase 4 subset boundary — everything else falls back.
        MirExpr::Match(_) => Err(MirVmUnsupported::UnsupportedExpr("Match")),
        MirExpr::TailCall(_) => Err(MirVmUnsupported::UnsupportedExpr("TailCall")),
        MirExpr::RecordCreate(_) => Err(MirVmUnsupported::UnsupportedExpr("RecordCreate")),
        MirExpr::RecordUpdate(_) => Err(MirVmUnsupported::UnsupportedExpr("RecordUpdate")),
        MirExpr::Try(_) => Err(MirVmUnsupported::UnsupportedExpr("Try")),
        MirExpr::List(_) => Err(MirVmUnsupported::UnsupportedExpr("List")),
        MirExpr::Tuple(_) => Err(MirVmUnsupported::UnsupportedExpr("Tuple")),
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
            matches!(c.node.callee, MirCallee::Fn(_)) && c.node.args.iter().all(can_compile)
        }
        MirExpr::Return(inner) => can_compile(inner),
        // Phase 4c additions:
        MirExpr::Construct(c) => c.node.args.iter().all(can_compile),
        MirExpr::Project(p) => can_compile(&p.node.base),
        _ => false,
    }
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
