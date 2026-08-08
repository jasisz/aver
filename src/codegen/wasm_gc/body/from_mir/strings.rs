//! String lowering: `MirExpr::InterpolatedStr` and the `String`
//! branches of `BinOp` (`+` / `==` / `<` / …). Mirrors `emit_expr`'s
//! String BinOp branches.

use super::*;

/// Interpolation lowering: build a `Vector<String>` of the parts and
/// concat it with `__wasmgc_concat_n`.
/// Each `Literal` part becomes an `array.new_data` over its segment;
/// each `Expr` part is emitted then stringified by the same
/// `String.from{Int,Float,Bool}` dispatch (a `String` is identity).
/// The result is always a `String`, so `produces` is `true` (empty
/// interpolation allocates a zero-length array directly, same as the
/// oracle).
///
/// An embed of any OTHER type is a hard `WasmGcError`, not an
/// `Ok(None)` bail. `Ok(None)` here means "the MIR walker does not
/// cover this fn", and the caller answers that by giving the whole fn
/// an `unreachable` trap stub (`module.rs`, `emit_trap_stub_body`) —
/// the program would compile clean and then trap at runtime with no
/// diagnostic. Typechecked source cannot reach this arm at all: the
/// checker's interpolation rule (`src/types/checker/infer/expr.rs`)
/// rejects every non-`Int`/`Float`/`Bool`/`String` embed. It stays
/// reachable from internal pipelines that drive codegen without a
/// typecheck, and from any future checker gap — both want the loud
/// error naming the fn and the embed type.
pub(crate) fn emit_mir_interpolated_str(
    func: &mut Function,
    parts: &[MirStrPart],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let string_type_idx = ctx
        .registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "InterpolatedStr reachable but no String type slot allocated".into(),
        ))?;
    if parts.is_empty() {
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        return Ok(Some(true));
    }
    let vec_idx = ctx
        .registry
        .vector_type_idx("Vector<String>")
        .ok_or(WasmGcError::Validation(
            "InterpolatedStr requires Vector<String> slot but it wasn't registered".into(),
        ))?;
    let concat_idx = ctx
        .fn_map
        .builtins
        .get("__wasmgc_concat_n")
        .copied()
        .ok_or(WasmGcError::Validation(
            "InterpolatedStr requires __wasmgc_concat_n builtin but it wasn't registered".into(),
        ))?;
    for part in parts {
        match part {
            MirStrPart::Literal(s) => {
                let bytes = s.as_bytes();
                let seg_idx =
                    ctx.registry
                        .string_literal_segment(bytes)
                        .ok_or(WasmGcError::Validation(format!(
                            "Interpolation literal `{s:?}` not in segment table"
                        )))?;
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::I32Const(bytes.len() as i32));
                func.instruction(&Instruction::ArrayNewData {
                    array_type_index: string_type_idx,
                    array_data_index: seg_idx,
                });
            }
            MirStrPart::Expr(inner) => {
                let aver_ty = aver_type_str_of(inner);
                // `Int = ℤ` size lever: an `Int` embed emits its own value AND
                // the matching decimal formatter via `emit_mir_int_stringify`,
                // which routes a raw-i64 (bare/carrier) embed to the LEAN i64
                // formatter (no box, no ~536 B bignum formatter) and a genuine
                // `$AverInt` embed to the bignum `String.fromInt`. Handled
                // BEFORE the generic emit below so the value is emitted exactly
                // once in the representation its chosen formatter consumes.
                if aver_ty.trim() == "Int" {
                    match emit_mir_int_stringify(func, inner, slots, ctx)? {
                        Some(()) => continue,
                        None => return Ok(None),
                    }
                }
                if emit_mir_expr(func, inner, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                match aver_ty.trim() {
                    "String" => { /* identity */ }
                    "Float" => {
                        let to_string_idx =
                            ctx.fn_map.builtins.get("String.fromFloat").copied().ok_or(
                                WasmGcError::Validation(
                                    "interpolation of Float requires String.fromFloat builtin"
                                        .into(),
                                ),
                            )?;
                        func.instruction(&Instruction::Call(to_string_idx));
                    }
                    "Bool" => {
                        let to_string_idx =
                            ctx.fn_map.builtins.get("String.fromBool").copied().ok_or(
                                WasmGcError::Validation(
                                    "interpolation of Bool requires String.fromBool builtin".into(),
                                ),
                            )?;
                        func.instruction(&Instruction::Call(to_string_idx));
                    }
                    // No stringifier for this embed. Loud, not silent —
                    // see the fn doc comment.
                    other => {
                        return Err(WasmGcError::Validation(format!(
                            "fn `{}`: string interpolation has no stringifier for an \
                             embed of type `{other}` — interpolation renders primitives \
                             only (Int, Float, Bool, String). Convert the value with a \
                             named function returning String and interpolate that.",
                            ctx.self_fn_name
                        )));
                    }
                }
            }
        }
    }
    func.instruction(&Instruction::ArrayNewFixed {
        array_type_index: vec_idx,
        array_size: parts.len() as u32,
    });
    func.instruction(&Instruction::Call(concat_idx));
    Ok(Some(true))
}

/// The `String`-operand branches of `emit_expr`'s `BinOp` arm —
/// byte-for-byte. `+` is `__wasmgc_concat_n` over a 2-element
/// `Vector<String>` (mirror of `emit_string_concat2`); `==` / `!=` is
/// `__wasmgc_string_eq` with an optional `i32.eqz` (mirror of
/// `emit_string_eq`); `<` / `>` / `<=` / `>=` is `__wasmgc_string_compare`
/// post-composed with the matching `i32` comparison against `0`. Each
/// operand recurses `emit_mir_expr`; `None` propagates as whole-fn
/// fallback. Any other op on a `String` (none exist after typecheck)
/// falls back.
pub(crate) fn emit_mir_string_binop(
    func: &mut Function,
    bop: &crate::ir::mir::MirBinOp,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let l = &bop.lhs;
    let r = &bop.rhs;
    macro_rules! e {
        ($x:expr) => {
            if emit_mir_expr(func, $x, slots, ctx)?.is_none() {
                return Ok(None);
            }
        };
    }
    match bop.op {
        BinOp::Add => {
            let vec_idx =
                ctx.registry
                    .vector_type_idx("Vector<String>")
                    .ok_or(WasmGcError::Validation(
                        "String `+` requires Vector<String> slot but it wasn't registered".into(),
                    ))?;
            let concat_idx = ctx
                .fn_map
                .builtins
                .get("__wasmgc_concat_n")
                .copied()
                .ok_or(WasmGcError::Validation(
                    "String `+` requires __wasmgc_concat_n builtin but it wasn't registered".into(),
                ))?;
            e!(l);
            e!(r);
            func.instruction(&Instruction::ArrayNewFixed {
                array_type_index: vec_idx,
                array_size: 2,
            });
            func.instruction(&Instruction::Call(concat_idx));
        }
        BinOp::Eq | BinOp::Neq => {
            let eq_idx = ctx
                .fn_map
                .builtins
                .get("__wasmgc_string_eq")
                .copied()
                .ok_or(WasmGcError::Validation(
                    "String `==`/`!=` requires __wasmgc_string_eq builtin but it wasn't registered"
                        .into(),
                ))?;
            e!(l);
            e!(r);
            func.instruction(&Instruction::Call(eq_idx));
            if matches!(bop.op, BinOp::Neq) {
                func.instruction(&Instruction::I32Eqz);
            }
        }
        BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
            let cmp_idx = ctx
                .fn_map
                .builtins
                .get("__wasmgc_string_compare")
                .copied()
                .ok_or(WasmGcError::Validation(
                    "String comparison requires __wasmgc_string_compare builtin".into(),
                ))?;
            e!(l);
            e!(r);
            func.instruction(&Instruction::Call(cmp_idx));
            func.instruction(&Instruction::I32Const(0));
            let post = match bop.op {
                BinOp::Lt => Instruction::I32LtS,
                BinOp::Gt => Instruction::I32GtS,
                BinOp::Lte => Instruction::I32LeS,
                BinOp::Gte => Instruction::I32GeS,
                _ => unreachable!("outer match restricts op"),
            };
            func.instruction(&post);
        }
        _ => return Ok(None),
    }
    Ok(Some(()))
}
