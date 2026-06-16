//! wasm-gc backend: emit function bodies from Core MIR.
//!
//! Mirror of [`super::emit::emit_expr`] that walks
//! [`crate::ir::mir::MirExpr`] instead of `ResolvedExpr` and emits
//! **byte-identical** wasm — one semantic walker per construct lives in
//! MIR, and every backend reads it instead of forking `ResolvedExpr`.
//!
//! [`emit_mir_expr`] is the dispatcher. Any construct it does not cover
//! returns `Ok(None)`; the caller ([`emit_fn_body_via_mir`]) then
//! discards `func` and re-runs the `ResolvedExpr` emitter for the whole
//! function. Two byte-differential tests compile
//! every single-file example and every multi-module game both ways (MIR
//! on vs forced off) and assert the modules are identical — the gate
//! that keeps each mirror exact.
//!
//! ## Covered constructs
//!
//! Dispatcher ([`emit_mir_expr`], this module): `Literal`, `Local`
//! (`local.get` of the resolver slot), numeric `BinOp` / `Neg`,
//! `Return`, `Let` (both named bindings and the statement-sequencing
//! synthetic lets — `_ = expr` discards and non-tail `Stmt::Expr`, which
//! emit the value, `drop` it if it produced one, then the body),
//! `Call(Fn)` / `TailCall`, `Project` (mirroring `emit_attr_get`), and
//! the `Tuple` / `MapLiteral` literals. First-class fn values are
//! supported: `FnValue(name)` lowers to an `i32.const` of the fn's
//! dense funcref-table index, and a `Fn`-param call (`LocalSlot`)
//! dispatches that index via `call_indirect` on table 0 (the funcref
//! table + functypes are set up in `module.rs`). Only the residual
//! cases with no table slot (a `FnValue` of a builtin / variant, or a
//! `LocalSlot` whose name is a let-bound fn value rather than a `Fn`
//! param) fall back to the trap stub. Registered-helper builtins and effect imports (`Console.*` /
//! `Disk.*` / `Tcp.*` / `Http.*` / `Random.*` / `Time.*`, each carrying
//! the host's `caller_fn` stamp) go through the `fn_map.builtins` /
//! `fn_map.effects` lookups here.
//!
//! - [`pattern_match`] — `Match` over `Bool` / `Int` / `String`,
//!   `Option` / `Result` / `List` carriers, and user sum types.
//! - [`constructors`] — `Construct` (user variants, `Option`, `Result`).
//! - [`records`] — `RecordCreate` / `RecordUpdate`.
//! - [`collections`] — `List` literals.
//! - [`builtins`] — the custom-inline `Float` / `Int` / `Bool` scalar
//!   ops, the custom-inline `String` ops (`length` /
//!   `split` / `join`), the `List` / `Vector` / `Map`
//!   families, the
//!   fused `Option.withDefault(Vector.get(v, i), <literal>)` /
//!   `Option.withDefault(Vector.set(v, i, x), v)` /
//!   `Result.withDefault(Int.mod(a, b), default)`, and the numeric
//!   `BinOp` tail.
//! - [`strings`] — `InterpolatedStr` and the `String` `BinOp` ops.
//! - [`control`] — `Try` (`?` propagation) and `IndependentProduct`
//!   (`(a, b, c)!` / `?!`).
//! - [`coverage`] — the `--explain-mir-coverage` predicate (diagnostic).
//!
//! Each submodule documents the exact `emit_expr` helper it mirrors and
//! the shapes that still fall back to the `ResolvedExpr` emitter.

pub(super) use std::collections::{HashMap, HashSet};

pub(super) use wasm_encoder::{Function, Instruction, ValType};

pub(super) use crate::ast::Spanned;
pub(super) use crate::ast::{BinOp, Literal};
pub(super) use crate::ir::CtorId;
pub(super) use crate::ir::SymbolTable;
pub(super) use crate::ir::hir::{ResolvedFnBody, ResolvedFnDef, ResolvedStmt};
pub(super) use crate::ir::mir::{
    BuiltinCtor, MirCallee, MirCtor, MirExpr, MirFn, MirIndependentProduct, MirMatch, MirMatchArm,
    MirPattern, MirProgram, MirRecordField, MirStrPart,
};
pub(super) use crate::types::Type;

pub(super) use super::super::WasmGcError;
pub(super) use super::super::types::{TypeRegistry, VariantInfo, aver_to_wasm, normalize_compound};
pub(super) use super::builtins::emit_args_get_inline;
pub(super) use super::emit::{
    emit_branch_marker, emit_caller_fn_idx, emit_default_value, emit_group_call,
    emit_return_call_insn, emit_string_literal_bytes, sum_or_record_eq_fn,
};
pub(super) use super::infer::{aver_type_canonical, aver_type_str_of, wasm_type_of};
pub(super) use super::slots::count_value_params;
pub(super) use super::{CallerFnCollector, EmitCtx, FnMap, SlotTable, Wasip2Lowering};

mod builtins;
mod collections;
mod constructors;
mod control;
mod coverage;
mod pattern_match;
mod records;
mod strings;

pub(super) use builtins::*;
pub(super) use collections::*;
pub(super) use constructors::*;
pub(super) use control::*;
pub(super) use pattern_match::*;
pub(super) use records::*;
pub(super) use strings::*;

pub use coverage::{CoverageReport, coverage_report};

/// ETAP-2 carrier-`i64`: narrow the `$AverInt` Int VALUE on the stack down
/// to the native `i64` the carrier slot/field holds. Emitted at the carrier
/// CONSTRUCT boundary (the newtype-erased smart-constructor field value, an
/// `$AverInt`, must land as `i64` in the carrier). Uses
/// `__aint_to_i64_checked` — which TRAPS on an out-of-`i64` Big — but the
/// carrier's proven smart-constructor bound `fits_i64`, so the trap is
/// unreachable for any value that ever reaches a real carrier (opaque type +
/// guarded constructor). A no-op when bignum is off (then `Int` is already a
/// scalar i64 and the carrier field is too).
pub(crate) fn emit_carrier_construct_bridge(
    func: &mut Function,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if ctx.registry.bignum {
        let to_checked = ctx
            .fn_map
            .builtins
            .get("__aint_to_i64_checked")
            .copied()
            .ok_or(WasmGcError::Validation(
                "carrier-i64 construct needs __aint_to_i64_checked but it is not registered".into(),
            ))?;
        func.instruction(&Instruction::Call(to_checked));
    }
    Ok(())
}

/// ETAP-2 carrier-`i64`: lift the native `i64` a carrier slot/field holds
/// back into the `$AverInt` carrier the surrounding `Int` context expects.
/// Emitted at the carrier PROJECT boundary (`c.value` reads the i64 and the
/// result is a plain `Int`). Uses `__aint_from_i64` (the canonical Small
/// constructor — infallible). A no-op when bignum is off.
pub(crate) fn emit_carrier_project_bridge(
    func: &mut Function,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if ctx.registry.bignum {
        let from_i64 =
            ctx.fn_map
                .builtins
                .get("__aint_from_i64")
                .copied()
                .ok_or(WasmGcError::Validation(
                    "carrier-i64 project needs __aint_from_i64 but it is not registered".into(),
                ))?;
        func.instruction(&Instruction::Call(from_i64));
    }
    Ok(())
}

/// `Int = ℤ`: registered `fn_map.builtins` helpers whose Aver return type
/// is `Int` but whose wasm helper computes a raw scalar i64 (a byte / code
/// count), so the result must be lifted into the `$AverInt` carrier. Kept
/// narrow: only the helpers that ride the generic `fn_map.builtins`
/// dispatch path. Int-returning builtins lowered by a dedicated emitter
/// (`List.len` / `Map.len` / `Vector.len` / `String.length` / the `Int.*`
/// / `Float.*` bridges) lift at their own site and are NOT listed here, so
/// nothing double-wraps.
fn registered_builtin_returns_raw_int(dotted: &str) -> bool {
    matches!(dotted, "String.len" | "String.byteLength" | "Char.toCode")
}

/// `Int = ℤ`: zero-based argument positions a registered `fn_map.builtins`
/// helper takes as a raw i64 but which the surface signature types as
/// `Int` (a char code or a string index/length). Under ℤ those args arrive
/// as `$AverInt` refs and must be saturate-lowered to i64 before the call.
/// `Char.fromCode`'s out-of-u32 / negative → `Option.None`, and a string
/// index past the length → empty/None, so a saturated i64::MAX/MIN
/// reproduces the VM's clamping. Empty for every other registered builtin.
fn registered_builtin_int_arg_positions(dotted: &str) -> &'static [usize] {
    match dotted {
        "Char.fromCode" => &[0],
        "String.charAt" => &[1],
        "String.slice" => &[1, 2],
        // `Byte.toHex(Int) -> Result<String,String>` — a byte value
        // 0..255 in an i64 slot; saturate-lower (the helper's range check
        // rejects out-of-byte values exactly as a Big would over-saturate).
        "Byte.toHex" => &[0],
        _ => &[],
    }
}

/// `Int = ℤ`: the HOST-ABI effect-boundary stays i64 (no bignum crosses
/// the wire). Zero-based positions an AverBridge effect IMPORT takes as an
/// `Int` (a machine-range bound / ms / port / coordinate). Under ℤ those
/// args arrive as `$AverInt` refs and must be CHECKED-lowered to i64
/// (`__aint_to_i64_checked`, which TRAPS on an out-of-i64 Big) before the
/// host call — mirroring the VM host services' checked `to_i64()`, which
/// ERRORS rather than saturating an out-of-range effect arg. Empty for
/// effects with no Int arg.
fn effect_int_arg_positions(dotted: &str) -> &'static [usize] {
    match dotted {
        "Random.int" => &[0, 1],
        "Time.sleep" => &[0],
        "Response.text" => &[0],
        // Network port / server bind port / terminal coordinates.
        "Tcp.send" | "Tcp.ping" | "Tcp.connect" => &[1],
        "HttpServer.listen" | "HttpServer.listenWith" => &[0],
        "Terminal.moveTo" => &[0, 1],
        _ => &[],
    }
}

/// `Int = ℤ`: AverBridge effect imports whose RESULT is an `Int` returned
/// as i64 from the host (`Random.int`, `Time.unixMs`). The incoming i64 is
/// lifted to a Small `$AverInt` so the value matches the ref shape Aver
/// uses for Int everywhere off the wire.
fn effect_returns_int(dotted: &str) -> bool {
    matches!(dotted, "Random.int" | "Time.unixMs")
}

/// Lower `mir_fn.body` into `func`, mirroring [`super::emit_fn_body`]
/// byte-for-byte. Returns `Ok(Some(extra_locals))` on full coverage,
/// `Ok(None)` when any node falls outside the supported subset — the
/// caller then discards `func` and re-runs `emit_fn_body`.
///
/// The setup (slot table, binding-name set, `EmitCtx`, return-type
/// string) is identical to `emit_fn_body` and is driven entirely off
/// `rfd` — `SlotTable::build_for_fn` reads the resolver's
/// `local_slot_types`, not the MIR body — so the discovered
/// extra-locals match the `ResolvedExpr` path regardless of which body
/// walk runs. The byte-differential gate depends on that invariant.
#[allow(clippy::too_many_arguments)]
pub(crate) fn emit_fn_body_via_mir(
    func: &mut Function,
    rfd: &ResolvedFnDef,
    mir_fn: &MirFn,
    mir_program: &MirProgram,
    fn_map: &FnMap,
    self_wasm_idx: u32,
    registry: &TypeRegistry,
    symbol_table: &SymbolTable,
    effect_idx_lookup: &HashMap<String, u32>,
    caller_fn_collector: &std::cell::RefCell<CallerFnCollector>,
    wasip2_lowering: Option<&Wasip2Lowering>,
) -> Result<Option<Vec<ValType>>, WasmGcError> {
    // ETAP-2 SLICE 2a: pass the per-slot Int repr (mirror of the alias
    // facts). Default-empty on the un-rewritten wasm-gc MIR, so every slot
    // stays boxed.
    let slots = SlotTable::build_for_fn(rfd, registry, fn_map, &mir_fn.repr)?;
    let return_type_str = rfd.return_type.display();

    // Precollect every `let`-bound name (mirror of `emit_fn_body`) so
    // `CallLowerCtx::is_local_value` recognises locals without a
    // parallel type table. Source it from the HIR `rfd`, NOT from
    // `mir_fn` — `EmitCtx` is shared with the `ResolvedExpr` emitter and
    // its recognition (`classify_leaf_op` / `classify_call_plan`) keys
    // off resolver-assigned names, so this set must stay HIR-sourced —
    // do not repopulate it from `MirExpr`.
    let ResolvedFnBody::Block(stmts) = rfd.body.as_ref();
    let mut binding_names: HashSet<String> = HashSet::new();
    for s in stmts {
        if let ResolvedStmt::Binding { name, .. } = s {
            binding_names.insert(name.clone());
        }
    }

    let ctx = EmitCtx {
        fn_map,
        self_wasm_idx,
        self_fn_name: rfd.name.as_str(),
        return_type: &return_type_str,
        registry,
        symbol_table,
        resolution: rfd.resolution.as_ref(),
        // MIR path: alias facts ride the MIR fn (cloned from the
        // resolver at lowering), so the owned-mutate fast path reads
        // its gate off MIR rather than the AST `FnResolution`. Identical
        // bits to the resolver table today; the home is what changes.
        aliased_slots: mir_fn.aliased_slots.as_slice(),
        // ETAP-2 SLICE 2a: per-slot Int repr rides the MIR fn (mirror of
        // `aliased_slots`). Default-empty today (no wasm-gc rewrite is
        // wired), so every slot stays boxed.
        repr: &mir_fn.repr,
        params: &rfd.params,
        binding_names: &binding_names,
        effect_idx_lookup,
        caller_fn_collector,
        wasip2_lowering,
        mir_builtins: Some(&mir_program.builtins),
        // ETAP-2 SLICE 2b: carry the whole program so call sites can read
        // the CALLEE's bare param/return repr (the per-fn `repr` above only
        // covers the current fn).
        mir_program: Some(mir_program),
        // ETAP-2 SLICE 2b: the body is in RETURN position — colour it raw
        // when THIS fn's return is bare (`repr.bare_return`), so the tail
        // value (and the tails of any `Match` / `IfThenElse` / `Let` it
        // descends through) produces an `i64` to match the bare `i64`
        // signature result. Set just below, before the body walk.
        int_result_raw: std::cell::Cell::new(false),
    };

    // Walk the body. `Ok(None)` mid-walk → caller falls back.
    // ETAP-2 SLICE 2b: a bare-return fn's body is a raw-i64 result tail.
    ctx.int_result_raw.set(mir_fn.repr.bare_return);
    let Some(produces_value) = emit_mir_expr(func, &mir_fn.body, &slots, &ctx)? else {
        return Ok(None);
    };

    // Tail handling — mirror of `emit_fn_body`'s `is_last` arm. The
    // body's value is the fn's return value, left on the stack.
    if return_type_str.trim() == "Unit" && produces_value {
        func.instruction(&Instruction::Drop);
    } else if return_type_str.trim() != "Unit" && !produces_value {
        return Err(WasmGcError::Validation(format!(
            "fn `{}` returns {} but trailing expression yields no value",
            rfd.name, return_type_str
        )));
    }
    func.instruction(&Instruction::End);

    Ok(Some(slots.extra_locals(count_value_params(&rfd.params))))
}

/// Mirror of `emit_expr`'s `nullary_variant_idx`: if `operand` is a
/// nullary user-variant value (`Tile.Floor`, lowered to a `Construct`
/// with no args of a zero-field variant), return that variant struct's
/// type index — so `<expr> == Tile.Floor` can lower to a `ref.test`
/// rather than a structural eq-helper call, matching the oracle.
fn mir_nullary_variant_idx(operand: &Spanned<MirExpr>, ctx: &EmitCtx<'_>) -> Option<u32> {
    let MirExpr::Construct(c) = &operand.node else {
        return None;
    };
    let MirCtor::User(ctor_id) = c.node.ctor else {
        return None;
    };
    if !c.node.args.is_empty() {
        return None;
    }
    let info = mir_user_variant_info(ctor_id, ctx).ok()?;
    if info.fields.is_empty() {
        Some(info.type_idx)
    } else {
        None
    }
}

/// Emit instructions for a MIR `expr`, returning `Ok(Some(produces))`
/// where `produces` is `true` when evaluating `expr` leaves a value on
/// the stack (i.e. its type is not `Unit`) — the same
/// `aver_type_str_of(...).trim() != "Unit"` predicate `emit_fn_body`
/// uses for its drop / local.set decisions. `Ok(None)` signals an
/// unsupported variant; the caller falls back to the `ResolvedExpr`
/// emitter for the whole fn.
pub(crate) fn emit_mir_expr(
    func: &mut Function,
    expr: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    // ETAP-2 SLICE 2b: take-and-CLEAR the raw-result colour at entry. The
    // bare-return tail emitter sets `int_result_raw` true around a result
    // position; reading it here and resetting to `false` means every child
    // this arm emits is BOXED by default — only the `Match` / `IfThenElse` /
    // `Let` arms below RE-set it for their genuine tail children
    // (arm bodies / branches / let body), never for a value position (a
    // subject, a cond, a let value, a call arg, an operand). So the colour
    // can never leak into a boxed context (where a raw `i64.const` literal
    // would be a validation error). The literal arm + the block-type
    // selection are the only readers.
    let result_raw = ctx.int_result_raw.replace(false);
    match &expr.node {
        MirExpr::Literal(lit) => match &lit.node {
            Literal::Int(n) => {
                // bignum slice 1 — under the opt-in flag every Int value
                // is the `$AverInt` struct ref, so an Int literal lowers
                // to `i64.const n; call $__aint_from_i64` (the canonical
                // Small constructor) instead of a bare `i64.const`.
                // ETAP-2 SLICE 2b: in a RAW result context the literal is a
                // bare `i64.const` (no `from_i64`) — the consumer wants an
                // `i64`.
                if result_raw {
                    func.instruction(&Instruction::I64Const(*n));
                } else if ctx.registry.bignum {
                    func.instruction(&Instruction::I64Const(*n));
                    let from_i64 = ctx.fn_map.builtins.get("__aint_from_i64").copied().ok_or(
                        WasmGcError::Validation(
                            "bignum active but __aint_from_i64 helper not registered".into(),
                        ),
                    )?;
                    func.instruction(&Instruction::Call(from_i64));
                } else {
                    func.instruction(&Instruction::I64Const(*n));
                }
                Ok(Some(true))
            }
            Literal::Float(f) => {
                func.instruction(&Instruction::F64Const((*f).into()));
                Ok(Some(true))
            }
            Literal::Bool(b) => {
                func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
                Ok(Some(true))
            }
            Literal::Unit => Ok(Some(false)),
            Literal::Str(s) => {
                // Mirror of `emit_expr`'s `Literal::Str` arm: passive
                // data segment → `array.new_data $string $seg` with
                // offset 0, size len.
                let bytes = s.as_bytes();
                let seg_idx =
                    ctx.registry
                        .string_literal_segment(bytes)
                        .ok_or(WasmGcError::Validation(format!(
                            "String literal `{s:?}` was not registered in the data segment table"
                        )))?;
                let string_type_idx =
                    ctx.registry
                        .string_array_type_idx
                        .ok_or(WasmGcError::Validation(
                            "String literal reachable but no String type slot allocated".into(),
                        ))?;
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::I32Const(bytes.len() as i32));
                func.instruction(&Instruction::ArrayNewData {
                    array_type_index: string_type_idx,
                    array_data_index: seg_idx,
                });
                Ok(Some(true))
            }
        },
        MirExpr::Local(local) => {
            // The MIR `LocalId` is the resolver slot index = wasm local
            // index 1:1 (mirror of `ResolvedExpr::Resolved { slot }`).
            func.instruction(&Instruction::LocalGet(local.node.slot.0));
            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
        }
        MirExpr::BinOp(spanned_binop) => {
            let bop = &spanned_binop.node;
            // Read the operand type from the MIR type stamp. Aver's
            // checker proved both operands share a type, so the LHS
            // suffices. `String` operands take the dedicated concat / eq
            // / compare builtins; numeric operands the primitive op set;
            // compound types (variant / record eq helpers) fall back.
            if aver_type_str_of(&bop.lhs).trim() == "String" {
                return match emit_mir_string_binop(func, bop, slots, ctx)? {
                    Some(()) => Ok(Some(true)),
                    None => Ok(None),
                };
            }
            match bop.lhs.ty() {
                Some(Type::Int) | Some(Type::Float) => {
                    if emit_mir_numeric_binop(func, bop, slots, ctx)?.is_none() {
                        return Ok(None);
                    }
                    Ok(Some(true))
                }
                // `==` / `!=` on a user type — mirror of `emit_expr`'s
                // three sub-branches, in the SAME order so the bytes match:
                // (1) `<expr> == NullaryVariant` and (2) the flipped form
                // lower to a `ref.test` on the variant struct; (3) any
                // other sum / record / carrier compare goes through the
                // per-type `__eq_<T>` helper. `!=` appends `i32.eqz`.
                Some(lty) if matches!(bop.op, BinOp::Eq | BinOp::Neq) => {
                    let neq = matches!(bop.op, BinOp::Neq);
                    if let Some(vidx) = mir_nullary_variant_idx(&bop.rhs, ctx) {
                        if emit_mir_expr(func, &bop.lhs, slots, ctx)?.is_none() {
                            return Ok(None);
                        }
                        func.instruction(&Instruction::RefTestNonNull(
                            wasm_encoder::HeapType::Concrete(vidx),
                        ));
                        if neq {
                            func.instruction(&Instruction::I32Eqz);
                        }
                        return Ok(Some(true));
                    }
                    if let Some(vidx) = mir_nullary_variant_idx(&bop.lhs, ctx) {
                        if emit_mir_expr(func, &bop.rhs, slots, ctx)?.is_none() {
                            return Ok(None);
                        }
                        func.instruction(&Instruction::RefTestNonNull(
                            wasm_encoder::HeapType::Concrete(vidx),
                        ));
                        if neq {
                            func.instruction(&Instruction::I32Eqz);
                        }
                        return Ok(Some(true));
                    }
                    let Some(eq_fn) = sum_or_record_eq_fn(lty, ctx) else {
                        // No `__eq_<T>` helper. Mirror of `emit_expr`'s
                        // final `else`: a single-field record flattens to
                        // a newtype whose wasm representation IS the
                        // underlying primitive, so `==` / `!=` lower to a
                        // plain `i64.eq` / `f64.eq` via the numeric select
                        // (`wasm_type_of` unwraps the newtype). A genuine
                        // unresolvable type yields `None` from
                        // `emit_mir_numeric_binop` → whole-fn fallback.
                        if emit_mir_numeric_binop(func, bop, slots, ctx)?.is_none() {
                            return Ok(None);
                        }
                        return Ok(Some(true));
                    };
                    if emit_mir_expr(func, &bop.lhs, slots, ctx)?.is_none() {
                        return Ok(None);
                    }
                    if emit_mir_expr(func, &bop.rhs, slots, ctx)?.is_none() {
                        return Ok(None);
                    }
                    func.instruction(&Instruction::Call(eq_fn));
                    if neq {
                        func.instruction(&Instruction::I32Eqz);
                    }
                    Ok(Some(true))
                }
                _ => Ok(None),
            }
        }
        MirExpr::Neg(inner) => {
            // Mirror of `emit_expr`'s `Neg` arm. Float keeps the IEEE
            // sign bit via `f64.neg`; Int has no dedicated insn and
            // lowers to `i64.const 0; <operand>; i64.sub`.
            let inner_ty = wasm_type_of(inner, ctx.registry)?;
            // ETAP-2 SLICE 2b: a `Neg` whose operand renders raw `i64` (the
            // rewrite left it un-boxed) is itself a raw value — emit the
            // operand raw and negate with `i64.const 0; …; i64.sub`. A boxed
            // operand (the rewrite wrapped a raw inner in `Box`) keeps the
            // `__aint_neg` path below. This is the same `both-raw` discipline
            // `emit_mir_numeric_binop` uses, kept in lockstep.
            if inner_ty == Some(ValType::F64) {
                if emit_mir_expr(func, inner, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                func.instruction(&Instruction::F64Neg);
            } else if mir_renders_raw_i64(&inner.node, ctx) {
                func.instruction(&Instruction::I64Const(0));
                if emit_mir_int_raw(func, inner, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                func.instruction(&Instruction::I64Sub);
            } else if ctx.registry.bignum {
                // bignum slice 1 — Int Neg routes through `__aint_neg`
                // (handles the `-i64::MIN` promotion to Big); the operand
                // is already an `$AverInt` ref.
                if emit_mir_expr(func, inner, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                let neg = ctx.fn_map.builtins.get("__aint_neg").copied().ok_or(
                    WasmGcError::Validation(
                        "bignum active but __aint_neg helper not registered".into(),
                    ),
                )?;
                func.instruction(&Instruction::Call(neg));
            } else {
                func.instruction(&Instruction::I64Const(0));
                if emit_mir_expr(func, inner, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                func.instruction(&Instruction::I64Sub);
            }
            Ok(Some(true))
        }
        MirExpr::Return(inner) => {
            // Not produced by the current HIR → MIR lowering (carried
            // for symmetry with the Rust walker); emit the value then
            // a wasm `return`. ETAP-2 SLICE 2b: the returned value is a
            // result tail — inherit the raw colour.
            ctx.int_result_raw.set(result_raw);
            let Some(produces) = emit_mir_expr(func, inner, slots, ctx)? else {
                return Ok(None);
            };
            func.instruction(&Instruction::Return);
            Ok(Some(produces))
        }
        MirExpr::Let(spanned_let) => {
            let l = &spanned_let.node;
            if l.binding_name.is_empty() {
                // DIAG: synthetic let coverage (emit value, drop-if-
                // produces, emit body) — reproducing the checkers divergence.
                let Some(value_produces) = emit_mir_expr(func, &l.value, slots, ctx)? else {
                    return Ok(None);
                };
                if value_produces {
                    func.instruction(&Instruction::Drop);
                }
                // ETAP-2 SLICE 2b: the body is the result tail — inherit the
                // raw colour the entry take-and-clear stored in `result_raw`.
                ctx.int_result_raw.set(result_raw);
                return emit_mir_expr(func, &l.body, slots, ctx);
            }
            // Mirror of `emit_fn_body`'s `Binding` arm.
            let slot = ctx
                .self_local_slot(&l.binding_name)
                .ok_or(WasmGcError::Validation(format!(
                    "binding `{}` has no resolver slot",
                    l.binding_name
                )))?;
            // ETAP-2 SLICE 2b: a bare binding slot is a raw `i64` local, so
            // its VALUE must be emitted in raw context — a boxed-path emit
            // (e.g. an `Int` literal lowering to `__aint_from_i64`) would
            // push an `$AverInt` ref into an `i64` slot (validation error).
            // The rewrite already left the value raw-rendering for a bare
            // binding (`rewrite_let_value`).
            let value_produces = if ctx.slot_is_bare(slot as u32) {
                match emit_mir_int_raw(func, &l.value, slots, ctx)? {
                    Some(p) => p,
                    None => return Ok(None),
                }
            } else {
                match emit_mir_expr(func, &l.value, slots, ctx)? {
                    Some(p) => p,
                    None => return Ok(None),
                }
            };
            // Unit-typed values push nothing; the slot may also be an
            // i32 placeholder out of `by_slot` range (preserved for
            // resolver index alignment) — neither stores.
            if value_produces && (slot as usize) < slots.by_slot.len() {
                func.instruction(&Instruction::LocalSet(slot));
            }
            // The chain's tail is the return value left on the stack — a
            // result tail, so inherit the raw colour (ETAP-2 SLICE 2b).
            ctx.int_result_raw.set(result_raw);
            emit_mir_expr(func, &l.body, slots, ctx)
        }
        MirExpr::Call(spanned_call) => {
            let call = &spanned_call.node;
            match call.callee {
                MirCallee::Fn(fn_id) => {
                    // Mirror of `emit_expr`'s `ResolvedCallee::Fn` arm.
                    match ctx.fn_map.by_id.get(&fn_id) {
                        None => {
                            // No wasm idx: a `Fn(..)` value parked in a
                            // local slot (verify-only higher-order) emits
                            // a polymorphic `unreachable`; anything else
                            // is a hard error — identical to `emit_expr`.
                            let name = ctx.symbol_table.fn_entry(fn_id).key.name.clone();
                            if ctx.self_local_slot(&name).is_some() {
                                func.instruction(&Instruction::Unreachable);
                                Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
                            } else {
                                Err(WasmGcError::Validation(format!(
                                    "call to unknown fn `{name}` (FnId {fn_id:?})"
                                )))
                            }
                        }
                        Some(entry) => {
                            let wasm_idx = entry.wasm_idx;
                            // ETAP-2 SLICE 2b: honor the callee's per-param
                            // bare repr — an arg at a bare param is emitted
                            // raw `i64` to match the callee's `i64` param slot.
                            if emit_mir_fn_args_then_call(
                                func, fn_id, &call.args, slots, ctx, wasm_idx,
                            )?
                            .is_none()
                            {
                                return Ok(None);
                            }
                            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
                        }
                    }
                }
                MirCallee::Builtin(id) => {
                    // Resolve the dotted name lowering interned for this
                    // `BuiltinId`, then mirror `emit_dotted_builtin`'s
                    // first branch: a builtin with a registered helper
                    // wasm fn is just "push args, call $idx". List
                    // cons/empty (intercepted by the `ResolvedExpr` Call
                    // arm before `emit_dotted_builtin`) and every custom
                    // inline lowering (effects, Args.get, Float / List /
                    // Map / Vector / String, wasip2) are NOT registered
                    // helpers, so they fall back to the `ResolvedExpr`
                    // emitter.
                    // An out-of-range `BuiltinId` is a lowering-invariant
                    // violation (every `MirCallee::Builtin` is minted via
                    // `program.intern_builtin`, so `id` always indexes
                    // `program.builtins`); fall back safely rather than panic.
                    let Some(dotted) = ctx.mir_builtins.and_then(|names| names.get(id.0 as usize))
                    else {
                        return Ok(None);
                    };
                    let dotted = dotted.as_str();
                    // `--target wasip2`: every effect lowers to a
                    // canonical-ABI call sequence (Console / Args / Env /
                    // Time / Random / Disk / Http / Tcp), NOT the AverBridge
                    // `fn_map.effects` host import — so this runs FIRST,
                    // mirroring the `ctx.wasip2_lowering.is_some()` block at
                    // the top of `emit_dotted_builtin`. It also covers the
                    // wasip2 `Args.get` (canonical-ABI variant), which is
                    // why it precedes the AverBridge `Args.get` inline below.
                    match emit_mir_wasip2_effect(func, dotted, &call.args, expr, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // `Args.get` is intercepted *before* the effect /
                    // builtin dispatch in `emit_dotted_builtin` and expands
                    // to a custom inline (the `Args.len` loop building a
                    // `List<String>`), even though it is also registered in
                    // `fn_map.effects`. Reuse the oracle's inline verbatim —
                    // it is `ResolvedExpr`-free (func + slots + ctx) — so the
                    // bytes match and `Args.get` no longer forces a fallback.
                    if dotted == "Args.get" {
                        emit_args_get_inline(func, slots, ctx)?;
                        return Ok(Some(aver_type_str_of(expr).trim() != "Unit"));
                    }
                    // Registered effect import (`Console.*`, `Disk.*`,
                    // `Tcp.*`, `Http.*`, `Random.*`, `Time.*`, …) on the
                    // AverBridge target — mirror of `emit_dotted_builtin`'s
                    // `fn_map.effects` branch: emit args, push the
                    // `caller_fn` idx the host stamps onto the recorded
                    // effect, then `call` the import. (The wasip2 effect
                    // lowerings are handled by `emit_mir_wasip2_effect`
                    // above; this branch serves only the AverBridge target.)
                    if let Some(&effect_idx) = ctx.fn_map.effects.get(dotted) {
                        // `Int = ℤ`: the host ABI stays i64 — lower any
                        // `Int` arg (`$AverInt` ref) to i64 before the call,
                        // and lift an `Int` result (i64 from the host) back
                        // into the carrier after. No bignum crosses the wire.
                        let int_args = effect_int_arg_positions(dotted);
                        for (i, arg) in call.args.iter().enumerate() {
                            if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
                                return Ok(None);
                            }
                            if ctx.registry.bignum && int_args.contains(&i) {
                                // CHECKED (not saturating): an out-of-i64 Int
                                // crossing the host effect boundary must
                                // REJECT — the VM's host services do a checked
                                // `to_i64()` and ERROR, so wasm-gc TRAPS rather
                                // than silently saturating to i64::MAX/MIN (a
                                // `Time.sleep(2^63)` saturated to a
                                // ~292-million-year hang) and proceeding.
                                let to_checked = ctx
                                    .fn_map
                                    .builtins
                                    .get("__aint_to_i64_checked")
                                    .copied()
                                    .ok_or(WasmGcError::Validation(
                                        "bignum active but __aint_to_i64_checked helper not \
                                         registered"
                                            .into(),
                                    ))?;
                                func.instruction(&Instruction::Call(to_checked));
                            }
                        }
                        emit_caller_fn_idx(func, ctx)?;
                        func.instruction(&Instruction::Call(effect_idx));
                        if effect_returns_int(dotted) {
                            lift_i64_result_to_aint(func, ctx)?;
                        }
                        return Ok(Some(aver_type_str_of(expr).trim() != "Unit"));
                    }
                    // Native scalar builtins (Float / Int / Bool) lower to
                    // an inline instruction sequence, not a registered
                    // helper, so try them before the `fn_map.builtins`
                    // lookup (which would miss them).
                    match emit_mir_native_scalar_builtin(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // `List.*` custom-inline ops (helper dispatch +
                    // prepend / empty) — also not in `fn_map.builtins`.
                    match emit_mir_list_builtin(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // `Vector.*` custom-inline ops (len / new / get / set
                    // / fromList) — likewise not registered helpers.
                    match emit_mir_vector_builtin(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // `Map.*` per-instantiation helper dispatch — also not
                    // in `fn_map.builtins`.
                    match emit_mir_map_builtin(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // Fused `Option.withDefault(Vector.get(v, i), <literal>)`
                    // and `Option.withDefault(Vector.set(v, i, x), v)`.
                    // Other `withDefault` shapes fall through to fallback.
                    match emit_mir_option_with_default(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // `Result.withDefault` — the fused Euclidean-modulo
                    // form `(Int.mod(a, b), default)` and the boxed
                    // tag-dispatch form for every other Result.
                    match emit_mir_result_with_default(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    // Boxed `Int.div(a, b)` / `Int.mod(a, b)` — the
                    // `Result<Int, String>` is consumed directly (e.g.
                    // `match Int.div(a, b) { Ok / Err }`), not the fused
                    // `Result.withDefault` form handled above. Build the
                    // concrete Result struct so the unwrap idiom runs on
                    // wasm-gc identically to the VM. (`Int.div` / `Int.mod`
                    // themselves are not `fn_map.builtins` entries, so the
                    // lookup below would miss and force a trapping fallback.)
                    match emit_mir_int_div_mod_boxed(func, dotted, &call.args, slots, ctx)? {
                        MirBuiltinEmit::Produced(produces) => return Ok(Some(produces)),
                        MirBuiltinEmit::Fallback => return Ok(None),
                        MirBuiltinEmit::NotHandled => {}
                    }
                    match ctx.fn_map.builtins.get(dotted) {
                        Some(&wasm_idx) => {
                            // `Int = ℤ`: a registered helper that takes an
                            // `Int` ARGUMENT in an i64-typed slot (a char
                            // code / string index) needs that arg
                            // saturate-lowered from `$AverInt` to i64.
                            let int_args = registered_builtin_int_arg_positions(dotted);
                            if emit_mir_args_then_call_lowering_int(
                                func, &call.args, slots, ctx, wasm_idx, int_args,
                            )?
                            .is_none()
                            {
                                return Ok(None);
                            }
                            // `Int = ℤ`: a registered helper that returns
                            // `Int` (`String.len`, `String.byteLength`,
                            // `Char.toCode`) computes a raw i64; lift it
                            // into the `$AverInt` carrier so the value
                            // matches the ref shape every Int site expects.
                            if registered_builtin_returns_raw_int(dotted) {
                                lift_i64_result_to_aint(func, ctx)?;
                            }
                            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
                        }
                        None => Ok(None),
                    }
                }
                MirCallee::Intrinsic(intr) => {
                    // `Int = ℤ`: the const-fold pass rewrites a
                    // `Result.withDefault(Int.div/mod(a, k), _)` over a
                    // constant nonzero `k` into a bare `IntDivEuclid` /
                    // `IntModEuclid` intrinsic (no Result, no `b == 0`
                    // guard — `k ∉ {0,-1}` for div, `k != 0` for mod is
                    // already proven). Under bignum `a`/`k` are `$AverInt`
                    // refs, so route to the Euclidean `__aint_divmod`
                    // helper (a `want_mod` flag selects div vs mod) instead
                    // of the i64 `__int_{div,mod}_euclid` — which would
                    // mix an i64 helper with ref operands (invalid wasm)
                    // and silently truncate a Big.
                    use crate::ir::hir::BuiltinIntrinsic;
                    if ctx.registry.bignum
                        && matches!(
                            intr,
                            BuiltinIntrinsic::IntDivEuclid | BuiltinIntrinsic::IntModEuclid
                        )
                        && call.args.len() == 2
                    {
                        let is_mod = matches!(intr, BuiltinIntrinsic::IntModEuclid);
                        let divmod = ctx.fn_map.builtins.get("__aint_divmod").copied().ok_or(
                            WasmGcError::Validation(
                                "bignum active but __aint_divmod helper not registered".into(),
                            ),
                        )?;
                        if emit_mir_expr(func, &call.args[0], slots, ctx)?.is_none()
                            || emit_mir_expr(func, &call.args[1], slots, ctx)?.is_none()
                        {
                            return Ok(None);
                        }
                        func.instruction(&Instruction::I32Const(if is_mod { 1 } else { 0 }));
                        func.instruction(&Instruction::Call(divmod));
                        return Ok(Some(true));
                    }
                    // Mirror of `emit_expr`'s `Intrinsic` arm: route the
                    // bare intrinsic name through the registered-builtin
                    // fast path. (Buffer intrinsics aren't produced on the
                    // wasm-gc path — it skips `buffer_build` — so this is
                    // effectively unreachable; kept for parity.)
                    match ctx.fn_map.builtins.get(intr.name()) {
                        Some(&wasm_idx) => {
                            if emit_mir_args_then_call(func, &call.args, slots, ctx, wasm_idx)?
                                .is_none()
                            {
                                return Ok(None);
                            }
                            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
                        }
                        None => Ok(None),
                    }
                }
                // First-class local-slot call: dispatch the `Fn`-param
                // through `call_indirect` on the funcref table (table 0).
                // The slot holds an i32 = the target fn's dense table
                // index (placed there by the `FnValue` arm at the
                // pass-the-fn call site). Recover the param's `Fn(..)`
                // sig key, look up the pre-registered functype; either
                // missing (e.g. a let-bound fn value with no table slot)
                // → fall back to the trap stub.
                MirCallee::LocalSlot { slot, ref name, .. } => {
                    let Some(key) = ctx.fn_param_fn_sig(name) else {
                        return Ok(None);
                    };
                    let Some(&type_index) = ctx.fn_map.call_indirect_types.get(&key) else {
                        return Ok(None);
                    };
                    // STACK ORDER: args first, then the i32 table index
                    // on top, then `call_indirect` (which pops index +
                    // args, pushes results).
                    for arg in &call.args {
                        if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
                            return Ok(None);
                        }
                    }
                    func.instruction(&Instruction::LocalGet(slot as u32));
                    func.instruction(&Instruction::CallIndirect {
                        type_index,
                        table_index: 0,
                    });
                    Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
                }
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            // Mirror of `emit_tail_call` (emit.rs): validate the target,
            // emit args, then the shared return-call instruction. The
            // `return_call` makes this a terminator; in tail position
            // `emit_fn_body_via_mir`'s trailing `End` is unreachable but
            // valid, exactly as the `ResolvedExpr` path.
            let tc = &spanned_tc.node;
            let wasm_idx = match ctx.fn_map.by_id.get(&tc.target) {
                Some(entry) => entry.wasm_idx,
                None => {
                    let name = ctx.symbol_table.fn_entry(tc.target).key.canonical();
                    return Err(WasmGcError::Validation(format!(
                        "tail call to unknown fn `{name}` (FnId {:?})",
                        tc.target
                    )));
                }
            };
            // ETAP-2 SLICE 2b: honor the (self / mutual) callee's per-param
            // bare repr. A self-tail-call's args feed the SAME param slots,
            // so a bare param (`countdown(n - 1)`, `n` bare) takes a raw
            // `i64` arg; `emit_return_call_insn` then `return_call`s the
            // target's own functype (whose bare param is `i64`). A boxed
            // param keeps the `$AverInt` path.
            for (i, arg) in tc.args.iter().enumerate() {
                if ctx.callee_param_is_bare(tc.target, i) {
                    if emit_mir_int_raw(func, arg, slots, ctx)?.is_none() {
                        return Ok(None);
                    }
                } else if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
                    return Ok(None);
                }
            }
            emit_return_call_insn(func, wasm_idx, ctx.self_wasm_idx);
            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
        }
        MirExpr::Match(spanned_match) => {
            // ETAP-2 SLICE 2b: propagate the raw-result colour into the match
            // (its arm bodies are the result tails). The entry take-and-clear
            // stored it in `result_raw`; re-set the flag so `emit_mir_match`
            // reads it.
            ctx.int_result_raw.set(result_raw);
            emit_mir_match(func, &spanned_match.node, slots, ctx)
        }
        MirExpr::Construct(spanned_ctor) => {
            // Mirror of `emit_expr`'s `Ctor` arm. A constructor always
            // produces a value (never `Unit`).
            let con = &spanned_ctor.node;
            let covered = match con.ctor {
                MirCtor::Builtin(BuiltinCtor::OptionSome) => {
                    if con.args.len() != 1 {
                        return Err(WasmGcError::Validation(format!(
                            "Option.Some constructor requires 1 arg, got {}",
                            con.args.len()
                        )));
                    }
                    emit_mir_option_constructor(func, Some(&con.args[0]), None, slots, ctx)?
                }
                MirCtor::Builtin(BuiltinCtor::OptionNone) => {
                    // Read T from the constructor's stamped type. The
                    // hint contract is the *element* type `T` (the
                    // constructor wraps it in `Option<…>` itself), so
                    // the return-type fallback strips the wrapper too
                    // when it is an Option.
                    let stamped = aver_type_canonical(expr, ctx.return_type, ctx.registry);
                    let hint: String = stamped
                        .strip_prefix("Option<")
                        .and_then(|s| s.strip_suffix('>'))
                        .map(|inner| inner.to_string())
                        .unwrap_or_else(|| {
                            let ret = normalize_compound(ctx.return_type);
                            ret.strip_prefix("Option<")
                                .and_then(|s| s.strip_suffix('>'))
                                .map(|inner| inner.to_string())
                                .unwrap_or(ret)
                        });
                    emit_mir_option_constructor(func, None, Some(&hint), slots, ctx)?
                }
                MirCtor::Builtin(BuiltinCtor::ResultOk) => emit_mir_result_constructor(
                    func,
                    "Ok",
                    con.args.first(),
                    expr.ty(),
                    slots,
                    ctx,
                )?,
                MirCtor::Builtin(BuiltinCtor::ResultErr) => emit_mir_result_constructor(
                    func,
                    "Err",
                    con.args.first(),
                    expr.ty(),
                    slots,
                    ctx,
                )?,
                MirCtor::User(ctor_id) => {
                    let info = mir_user_variant_info(ctor_id, ctx)?;
                    emit_mir_constructor_with_args(func, info, &con.args, slots, ctx)?
                }
            };
            match covered {
                Some(()) => Ok(Some(aver_type_str_of(expr).trim() != "Unit")),
                None => Ok(None),
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            let rec = &spanned_rec.node;
            match emit_mir_record_create(func, &rec.type_name, &rec.fields, slots, ctx)? {
                Some(()) => Ok(Some(aver_type_str_of(expr).trim() != "Unit")),
                None => Ok(None),
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            let upd = &spanned_upd.node;
            match emit_mir_record_update(func, &upd.type_name, &upd.base, &upd.updates, slots, ctx)?
            {
                Some(()) => Ok(Some(aver_type_str_of(expr).trim() != "Unit")),
                None => Ok(None),
            }
        }
        MirExpr::Project(spanned_proj) => {
            // Mirror of `emit_attr_get`. A newtype `.field` is identity —
            // emit the base directly. Unknown / `Invalid` (namespace-
            // handle) record types fall back so the `ResolvedExpr`
            // emitter produces `emit_attr_get`'s diagnostic.
            let proj = &spanned_proj.node;
            let record_name = aver_type_str_of(&proj.base);
            if ctx.registry.newtype_underlying(&record_name).is_some() {
                // ETAP-2 carrier-`i64`: an eligible carrier's field read is
                // still identity (emit the base), but the base is now a
                // native `i64` and the projected `.value` is a plain `Int`,
                // so lift it back into the `$AverInt` carrier.
                let produced = emit_mir_expr(func, &proj.base, slots, ctx)?;
                if produced.is_some() && ctx.registry.is_eligible_carrier(&record_name) {
                    emit_carrier_project_bridge(func, ctx)?;
                }
                return Ok(produced.map(|_| aver_type_str_of(expr).trim() != "Unit"));
            }
            let (Some(type_idx), Some(field_idx)) = (
                ctx.registry.record_type_idx(&record_name),
                ctx.registry.record_field_index(&record_name, &proj.field),
            ) else {
                return Ok(None);
            };
            if emit_mir_expr(func, &proj.base, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::StructGet {
                struct_type_index: type_idx,
                field_index: field_idx,
            });
            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
        }
        MirExpr::Tuple(items) => {
            // Mirror of `emit_tuple_literal`: canonical from the elements'
            // stamped types, then push each element + `struct.new`.
            if items.len() < 2 {
                return Err(WasmGcError::Validation(format!(
                    "Tuple literal needs at least 2 elements; got {}",
                    items.len()
                )));
            }
            let elem_tys: Vec<String> = items.iter().map(aver_type_str_of).collect();
            let canonical = format!("Tuple<{}>", elem_tys.join(","))
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect::<String>();
            let tuple_idx =
                ctx.registry
                    .tuple_type_idx(&canonical)
                    .ok_or(WasmGcError::Validation(format!(
                        "Tuple literal: `{canonical}` slot not registered"
                    )))?;
            for item in items {
                if emit_mir_expr(func, item, slots, ctx)?.is_none() {
                    return Ok(None);
                }
            }
            func.instruction(&Instruction::StructNew(tuple_idx));
            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
        }
        MirExpr::MapLiteral(entries) => {
            // Mirror of `emit_map_literal`: `Map.empty` then a `Map.set`
            // per entry. Canonical from the first entry's K/V stamped
            // types, or the sole registered `Map<K,V>` when empty.
            let canonical: String = if entries.is_empty() {
                if ctx.registry.map_order.len() == 1 {
                    ctx.registry.map_order[0].clone()
                } else {
                    return Err(WasmGcError::Validation(
                        "empty MapLiteral: cannot resolve Map<K,V> instantiation \
                         without context (multiple instantiations registered)"
                            .into(),
                    ));
                }
            } else {
                let k_aver = aver_type_str_of(&entries[0].0);
                let v_aver = aver_type_str_of(&entries[0].1);
                format!("Map<{},{}>", k_aver.trim(), v_aver.trim())
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect()
            };
            let (empty_fn, set_fn) = {
                let helpers =
                    ctx.fn_map
                        .map_helpers
                        .get(&canonical)
                        .ok_or(WasmGcError::Validation(format!(
                            "MapLiteral: helpers missing for `{canonical}`"
                        )))?;
                (helpers.empty, helpers.set)
            };
            func.instruction(&Instruction::Call(empty_fn));
            for (k_expr, v_expr) in entries {
                if emit_mir_expr(func, k_expr, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                if emit_mir_expr(func, v_expr, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                func.instruction(&Instruction::Call(set_fn));
            }
            Ok(Some(aver_type_str_of(expr).trim() != "Unit"))
        }
        MirExpr::List(items) => match emit_mir_list_literal(func, expr, items, slots, ctx)? {
            Some(()) => Ok(Some(aver_type_str_of(expr).trim() != "Unit")),
            None => Ok(None),
        },
        MirExpr::Try(inner) => emit_mir_try(func, inner, slots, ctx),
        MirExpr::InterpolatedStr(parts) => emit_mir_interpolated_str(func, parts, slots, ctx),
        MirExpr::IndependentProduct(spanned_ip) => {
            emit_mir_independent_product(func, &spanned_ip.node, slots, ctx)
        }
        // `if cond { then } else { else }` — produced by the
        // `bool_match_to_if` optimizer from a two-arm `Bool` match.
        // Byte-equivalent to that match's lowering (`emit_mir_match`'s
        // `Bool` arm): the then-branch type is the result type (typecheck
        // proved both branches agree); a `Unit` if produces no value.
        MirExpr::IfThenElse(ite) => {
            let ite = &ite.node;
            // ETAP-2 SLICE 2b: in a RAW result context (a bare-return tail),
            // the if/else block produces an `i64`, not an `$AverInt`. The
            // cond is a value position (flag already cleared at entry); each
            // BRANCH is the result position, so re-set the colour around it.
            let block_ty = if result_raw {
                wasm_encoder::BlockType::Result(ValType::I64)
            } else {
                let result_ty =
                    aver_type_canonical(&ite.then_branch, ctx.return_type, ctx.registry);
                match aver_to_wasm(&result_ty, Some(ctx.registry))? {
                    Some(v) => wasm_encoder::BlockType::Result(v),
                    None => wasm_encoder::BlockType::Empty,
                }
            };
            let produces = !matches!(block_ty, wasm_encoder::BlockType::Empty);
            if emit_mir_expr(func, &ite.cond, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::If(block_ty));
            ctx.int_result_raw.set(result_raw);
            if emit_mir_expr(func, &ite.then_branch, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::Else);
            ctx.int_result_raw.set(result_raw);
            if emit_mir_expr(func, &ite.else_branch, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::End);
            Ok(Some(produces))
        }
        // A fn referenced as a value (`callWith(inc)`): push its dense
        // funcref-table index as an `i32`. A `Fn`-param call later reads
        // this i32 from its slot and dispatches via `call_indirect`.
        // Names absent from the funcref table (builtins / variants) have
        // no table slot — fall back to the trap stub.
        MirExpr::FnValue(name) => match ctx.fn_map.funcref_table.get(name) {
            Some(&idx) => {
                func.instruction(&Instruction::I32Const(idx as i32));
                Ok(Some(true))
            }
            None => Ok(None),
        },
        // ETAP-2 SLICE 2b — the representation boundaries the
        // `bare_i64_rewrite::rewrite_for_wasm_gc` pass inserted. Codegen no
        // longer DECIDES where a boundary goes (the rewrite did); it just
        // lowers the node:
        //   Box(inner)   — `inner` evaluates to a raw `i64`; lift it into the
        //                  `$AverInt` carrier via `__aint_from_i64` (the same
        //                  canonical Small constructor an Int literal uses).
        //   Unbox(inner) — `inner` evaluates to an `$AverInt`; narrow it to a
        //                  raw `i64` via the CHECKED `__aint_to_i64_checked`,
        //                  which TRAPS on an out-of-i64 Big. The analysis
        //                  proved the value fits, so the trap never fires on a
        //                  sound program — but on wasm-gc (where `i64.*` wraps
        //                  silently) it is the ONLY runtime net, so it is
        //                  checked, never saturating.
        MirExpr::Box(inner) => {
            // `inner` is a raw-i64 subtree; emit it in raw context, then box.
            if emit_mir_int_raw(func, inner, slots, ctx)?.is_none() {
                return Ok(None);
            }
            lift_i64_result_to_aint(func, ctx)?;
            Ok(Some(true))
        }
        MirExpr::Unbox(inner) => {
            // `inner` is a boxed `$AverInt`; emit it boxed, then checked-narrow.
            if emit_mir_expr(func, inner, slots, ctx)?.is_none() {
                return Ok(None);
            }
            let to_checked = ctx
                .fn_map
                .builtins
                .get("__aint_to_i64_checked")
                .copied()
                .ok_or(WasmGcError::Validation(
                    "bignum active but __aint_to_i64_checked helper not registered (Unbox)".into(),
                ))?;
            func.instruction(&Instruction::Call(to_checked));
            Ok(Some(true))
        }
    }
}

/// ETAP-2 SLICE 2b: does `e` render as a raw machine `i64` on the stack
/// (rather than an `$AverInt` ref) under the current fn's `repr`? This is
/// the wasm-gc mirror of the Rust backend's `mir_expr_is_bare_i64`, but
/// PURELY STRUCTURAL: the `bare_i64_rewrite` already decided every crossing
/// (it inserted `Box`/`Unbox` and only leaves a raw-rendering subtree where
/// raw is wanted), so no interval re-derivation is needed here — a node
/// renders raw exactly when:
///   - `Local(slot)` and the slot is bare (`repr.slot_is_bare`),
///   - `Unbox(_)` (always narrows to `i64`),
///   - `Neg`/`Add`/`Sub`/`Mul` over operands that ALL render raw,
///   - an `Int` literal (a raw `i64.const` candidate — its actual repr is
///     decided by its PARENT context: a boxed parent emits it via the
///     boxed literal arm (`from_i64`), a raw parent via `emit_mir_int_raw`).
///
/// A `Box(_)` renders an `$AverInt` (NOT raw). Everything else is boxed.
///
/// SOUNDNESS: a wrongly-raw value silently wraps on wasm-gc (no trip-wire).
/// This predicate never INTRODUCES bareness — it only reports the structure
/// the fail-closed rewrite produced. A bare `Local` that should have been
/// boxed would have been wrapped in `Box` by the rewrite, so it would not
/// reach a raw consumer; if it ever did, the stack-type (`i64`) vs declared
/// (`$AverInt` ref) mismatch is a wasm VALIDATION error, not a silent miscompile.
pub(crate) fn mir_renders_raw_i64(e: &MirExpr, ctx: &EmitCtx<'_>) -> bool {
    match e {
        MirExpr::Local(local) => ctx.slot_is_bare(local.node.slot.0),
        MirExpr::Unbox(_) => true,
        MirExpr::Box(_) => false,
        // A standalone `Int` literal is raw-COMPATIBLE (it can lower to an
        // `i64.const`), but on its own it is NOT proof of a raw context — its
        // repr is decided by its parent (a boxed parent emits `from_i64`, a
        // raw parent `i64.const`). So a literal alone does NOT render raw; it
        // only stays raw INSIDE a raw arithmetic tree anchored by a real bare
        // value (handled below). This is what stops a pure-LITERAL arithmetic
        // tree whose PRODUCT overflows i64 (`(-1) * i64::MIN`, a boxed-`Mul`
        // constant in `String.fromInt(...)`) from being mistaken for a bare
        // op — that tree has no bare anchor, so it takes the boxed `__aint_*`
        // path (full precision), matching the VM.
        MirExpr::Literal(_) => false,
        // Arithmetic renders raw only when it is part of a BARE slot's
        // computation: it must contain at least one genuine raw anchor (a bare
        // `Local` or an `Unbox`) AND every leaf must be raw-compatible (a bare
        // `Local`, an `Unbox`, or an `Int` literal). The rewrite already
        // interval-checked the WHOLE tree (it boxed any out-of-`i64` operand),
        // so a tree that still has all-raw-compatible leaves + a bare anchor is
        // one the analysis proved `OverflowFree`. (A pure-literal tree fails
        // the anchor test and is boxed; a tree with a `Box`ed operand fails the
        // leaf test and is boxed.)
        MirExpr::Neg(inner) => {
            mir_arith_leaves_raw_compatible(&inner.node, ctx)
                && mir_has_raw_anchor(&inner.node, ctx)
        }
        MirExpr::BinOp(b) => {
            matches!(b.node.op, BinOp::Add | BinOp::Sub | BinOp::Mul)
                && mir_arith_leaves_raw_compatible(e, ctx)
                && mir_has_raw_anchor(e, ctx)
        }
        _ => false,
    }
}

/// Every leaf of the `Add`/`Sub`/`Mul`/`Neg` tree `e` is raw-compatible: a
/// bare `Local`, an `Unbox`, or an `Int` literal. A `Box`ed operand (the
/// rewrite wrapped an out-of-range / boxed value) makes the whole tree boxed.
fn mir_arith_leaves_raw_compatible(e: &MirExpr, ctx: &EmitCtx<'_>) -> bool {
    match e {
        MirExpr::Local(local) => ctx.slot_is_bare(local.node.slot.0),
        MirExpr::Unbox(_) => true,
        MirExpr::Literal(l) => matches!(l.node, Literal::Int(_)),
        MirExpr::Neg(inner) => mir_arith_leaves_raw_compatible(&inner.node, ctx),
        MirExpr::BinOp(b) => {
            matches!(b.node.op, BinOp::Add | BinOp::Sub | BinOp::Mul)
                && mir_arith_leaves_raw_compatible(&b.node.lhs.node, ctx)
                && mir_arith_leaves_raw_compatible(&b.node.rhs.node, ctx)
        }
        _ => false,
    }
}

/// The `Add`/`Sub`/`Mul`/`Neg` tree `e` contains at least one genuine raw
/// VALUE (a bare `Local` or an `Unbox`) — not just literals. This anchors
/// the tree to a bare slot's computation; a pure-literal tree has no anchor.
fn mir_has_raw_anchor(e: &MirExpr, ctx: &EmitCtx<'_>) -> bool {
    match e {
        MirExpr::Local(local) => ctx.slot_is_bare(local.node.slot.0),
        MirExpr::Unbox(_) => true,
        MirExpr::Neg(inner) => mir_has_raw_anchor(&inner.node, ctx),
        MirExpr::BinOp(b) => {
            mir_has_raw_anchor(&b.node.lhs.node, ctx) || mir_has_raw_anchor(&b.node.rhs.node, ctx)
        }
        _ => false,
    }
}

/// ETAP-2 SLICE 2b: does this Int `BinOp` lower to a RAW `i64.*` op (vs the
/// boxed `$AverInt` limb helpers)? See the call site in
/// `emit_mir_numeric_binop` for the discipline:
///   - ARITHMETIC (`+`/`-`/`*`): raw only when the WHOLE node renders raw
///     (`mir_renders_raw_i64`, which requires a bare anchor + raw-compatible
///     leaves — the rewrite already interval-checked the tree). A pure-literal
///     tree that overflows i64 is boxed (full precision, matching the VM).
///   - COMPARISON (`==`/`!=`/`<`/`>`/`<=`/`>=`): raw when at least one operand
///     is a genuine bare anchor AND both operands are raw-compatible. A
///     comparison never overflows, so a literal operand (`n == 0`) is fine to
///     compare raw against `i64.const`. (The rewrite leaves a bare comparison's
///     operands raw — a boxed operand would have been `Box`ed, failing the
///     leaf test below and routing to the boxed comparison helpers.)
pub(crate) fn mir_int_binop_is_raw(bop: &crate::ir::mir::MirBinOp, ctx: &EmitCtx<'_>) -> bool {
    if matches!(bop.op, BinOp::Div) {
        // `Div` is not produced over bare Int (source `Int.div` is a Result
        // builtin), so it never takes the raw path.
        return false;
    }
    // Both arithmetic AND comparison reduce to the SAME structural test:
    //   - every operand leaf is raw-compatible (a bare `Local`, an `Unbox`, or
    //     an `Int` literal) — a `Box`ed operand fails this, routing to boxed,
    //   - at least one operand carries a genuine bare anchor (a bare `Local` /
    //     `Unbox`) — a pure-literal tree fails this, routing to boxed.
    // For ARITHMETIC the interval-fit is already guaranteed: the rewrite
    // `Box`es the anchor of any out-of-`i64` tree (`n * BigLit` → `Box(n) *
    // …`), so an overflowing tree has a `Box`ed operand and fails the
    // leaf test. For a COMPARISON there is no overflow to worry about; a
    // literal operand (`n == 0`) compares raw against `i64.const`.
    let leaves_ok = mir_arith_leaves_raw_compatible(&bop.lhs.node, ctx)
        && mir_arith_leaves_raw_compatible(&bop.rhs.node, ctx);
    let anchored = mir_has_raw_anchor(&bop.lhs.node, ctx) || mir_has_raw_anchor(&bop.rhs.node, ctx);
    leaves_ok && anchored
}

/// ETAP-2 SLICE 2b: emit `e` (an `Int`-typed MIR expression the rewrite
/// placed in a RAW context — a bare `Let` value, a call arg at a bare
/// callee param, a bare-return tail, a `Box`'s inner, or an operand of raw
/// arithmetic) leaving a raw machine `i64` on the stack. Mirror of the Rust
/// backend's `emit_bare_i64`.
///
/// The cases are exactly the raw-rendering shapes
/// [`mir_renders_raw_i64`] recognizes:
///   - `Int` literal      → `i64.const`,
///   - bare `Local`       → `local.get` (the slot's declared type IS `i64`),
///   - `Neg`/`Add`/`Sub`/`Mul` → recurse operands raw, then the raw `i64.*` op,
///   - `Unbox(inner)`     → emit `inner` boxed, then `__aint_to_i64_checked`.
///
/// A `Box(_)` or any non-raw shape reaching here is a rewrite bug — return
/// `Ok(None)` so the whole-fn fallback engages rather than miscompile.
pub(crate) fn emit_mir_int_raw(
    func: &mut Function,
    e: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    match &e.node {
        MirExpr::Literal(l) => match &l.node {
            Literal::Int(n) => {
                func.instruction(&Instruction::I64Const(*n));
                Ok(Some(true))
            }
            // A non-Int literal in a raw Int context is a rewrite bug.
            _ => Ok(None),
        },
        MirExpr::Local(local) => {
            // A bare slot's declared wasm local type is i64 (SlotTable), so a
            // plain `local.get` already yields a raw i64. Guard: a NON-bare
            // local reaching a raw context is a rewrite bug (it would push an
            // `$AverInt` ref where an i64 is expected → validation error).
            if !ctx.slot_is_bare(local.node.slot.0) {
                return Ok(None);
            }
            func.instruction(&Instruction::LocalGet(local.node.slot.0));
            Ok(Some(true))
        }
        MirExpr::Neg(inner) => {
            // Raw negation: `i64.const 0; <inner>; i64.sub`.
            func.instruction(&Instruction::I64Const(0));
            if emit_mir_int_raw(func, inner, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::I64Sub);
            Ok(Some(true))
        }
        MirExpr::BinOp(b) => {
            let op = b.node.op;
            let inst = match op {
                BinOp::Add => Instruction::I64Add,
                BinOp::Sub => Instruction::I64Sub,
                BinOp::Mul => Instruction::I64Mul,
                // Only Add/Sub/Mul render raw (see `mir_renders_raw_i64`);
                // any other op here is a rewrite bug → fall back.
                _ => return Ok(None),
            };
            if emit_mir_int_raw(func, &b.node.lhs, slots, ctx)?.is_none() {
                return Ok(None);
            }
            if emit_mir_int_raw(func, &b.node.rhs, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&inst);
            Ok(Some(true))
        }
        // An `Unbox` IS a raw value: delegate to the main emitter's `Unbox`
        // arm (the single checked-narrow lowering), which leaves an i64.
        // A `Call`/`TailCall` to a bare-returning callee returns an `i64`
        // (its functype result is `i64`) — the main emitter already lowers
        // it; no result colour needed (the i64 comes from the callee sig).
        MirExpr::Unbox(_) | MirExpr::Call(_) | MirExpr::TailCall(_) => {
            emit_mir_expr(func, e, slots, ctx)
        }
        // A `Match` / `IfThenElse` / `Let` / `Return` whose tail is the raw
        // value: delegate to the main emitter WITH the raw result colour set,
        // so its block type comes out `i64` and its arm/branch/body tails
        // recurse raw (the literal arms emit `i64.const`, not `__aint_from_i64`).
        // The whole-fn fallback still engages if the delegate returns `None`.
        MirExpr::Match(_) | MirExpr::IfThenElse(_) | MirExpr::Let(_) | MirExpr::Return(_) => {
            ctx.int_result_raw.set(true);
            emit_mir_expr(func, e, slots, ctx)
        }
        _ => Ok(None),
    }
}

/// Emit each MIR `arg` (returning `None` if any falls outside the
/// supported subset, propagated as a whole-fn fallback) then
/// `call $wasm_idx`. Shared by the `Fn` / `Builtin` / `Intrinsic`
/// callee arms; the caller adds the `produces_value` read from the
/// call expr's own type stamp.
pub(crate) fn emit_mir_args_then_call(
    func: &mut Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    wasm_idx: u32,
) -> Result<Option<()>, WasmGcError> {
    emit_mir_args_then_call_lowering_int(func, args, slots, ctx, wasm_idx, &[])
}

/// ETAP-2 SLICE 2b: emit a USER `Fn` call's args honoring the CALLEE's
/// per-param representation, then `call $wasm_idx`. An arg at a bare callee
/// param (`callee.repr.bare_params[i]`) is emitted in RAW context (the
/// callee's param slot is `i64`); every other arg keeps the boxed
/// `$AverInt` path. The rewrite already shaped each arg for its param
/// context (`rewrite_call_args`: a bare param gets a raw-rendering arg, a
/// boxed param a `Box`/already-`$AverInt` arg), so this only SELECTS the
/// matching emit path — it inserts no conversion. Used only for direct
/// user-`Fn` calls; builtins / intrinsics / effects keep the boxed
/// `emit_mir_args_then_call*` path (the rewrite boxes their Int args).
fn emit_mir_fn_args_then_call(
    func: &mut Function,
    target: crate::ir::FnId,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    wasm_idx: u32,
) -> Result<Option<()>, WasmGcError> {
    for (i, arg) in args.iter().enumerate() {
        if ctx.callee_param_is_bare(target, i) {
            if emit_mir_int_raw(func, arg, slots, ctx)?.is_none() {
                return Ok(None);
            }
        } else if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    func.instruction(&Instruction::Call(wasm_idx));
    Ok(Some(()))
}

/// As [`emit_mir_args_then_call`], but `int_arg_positions` lists the
/// zero-based args the helper takes as a raw i64 in an `Int`-typed slot
/// (`Char.fromCode` code, `String.charAt`/`slice` indices). Under `Int =
/// ℤ` those args arrive as `$AverInt` refs, so they are saturate-lowered
/// to i64 (`__aint_to_i64_sat`) right after emission. A no-op for the
/// common empty-positions case (every other builtin).
pub(crate) fn emit_mir_args_then_call_lowering_int(
    func: &mut Function,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    wasm_idx: u32,
    int_arg_positions: &[usize],
) -> Result<Option<()>, WasmGcError> {
    for (i, arg) in args.iter().enumerate() {
        if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
            return Ok(None);
        }
        if ctx.registry.bignum && int_arg_positions.contains(&i) {
            let to_sat = ctx
                .fn_map
                .builtins
                .get("__aint_to_i64_sat")
                .copied()
                .ok_or(WasmGcError::Validation(
                    "bignum active but __aint_to_i64_sat helper not registered".into(),
                ))?;
            func.instruction(&Instruction::Call(to_sat));
        }
    }
    func.instruction(&Instruction::Call(wasm_idx));
    Ok(Some(()))
}
