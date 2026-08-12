//! Built-in call lowering: the custom-inline `Float` / `Int` / `Bool`
//! scalar ops, the custom-inline `String` ops
//! (`length` / `split` / `join`), the `List` / `Vector`
//! / `Map` families, the fused `Option.withDefault(Vector.get,
//! <literal>)` / `Option.withDefault(Vector.set, v)` /
//! `Result.withDefault(Int.mod, default)`, and the numeric `BinOp`
//! tail. Mirrors the custom-inline arms of `emit_dotted_builtin` and
//! `emit_expr`'s numeric `BinOp` branch.

use super::*;

/// Outcome of trying to emit a builtin call through a specialized MIR
/// path, so the `Call(Builtin)` arm can try paths in order without
/// conflating "not this path's builtin" with "fall the whole fn back".
pub(crate) enum MirBuiltinEmit {
    /// Not a builtin this path recognizes — try the next path.
    NotHandled,
    /// Recognized, but an argument couldn't be emitted from MIR — the
    /// whole fn falls back to the resolved-HIR emitter.
    Fallback,
    /// Emitted; `produces` is whether a value is left on the stack.
    Produced(bool),
}

/// Look up a registered `__aint_*` helper fn idx, erroring (not silently
/// miscompiling) when the prelude wasn't registered. Registration is
/// unconditional whenever Int arithmetic is reachable (`module.rs`), so a
/// miss is a real wiring bug.
fn aint_helper(ctx: &EmitCtx<'_>, name: &str) -> Result<u32, WasmGcError> {
    ctx.fn_map
        .aint_helper_idx(name)
        .ok_or(WasmGcError::Validation(format!(
            "bignum active but {name} helper not registered"
        )))
}

/// `Int = ℤ`: lift a raw i64 sitting on the wasm stack into the canonical
/// Small `$AverInt` carrier (`call $__aint_from_i64`). Used at every
/// Int-RETURNING builtin whose helper computes an i64 internally — a
/// length / count / codepoint (`List.len`, `Map.len`, `Vector.len`,
/// `String.len` / `byteLength`, `Char.toCode`) or an effect import return
/// (`Random.int`, `Time.unixMs`) — so the value matches the `(ref null
/// $AverInt)` shape every other Int site expects. A no-op when `bignum`
/// is off (no Int reachable), where these builtins keep their scalar i64.
pub(crate) fn lift_i64_result_to_aint(
    func: &mut Function,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if ctx.registry.bignum {
        let from_i64 = aint_helper(ctx, "__aint_from_i64")?;
        func.instruction(&Instruction::Call(from_i64));
    }
    Ok(())
}

/// `Int = ℤ`: emit an Int builtin ARGUMENT and saturate-lower it from the
/// `$AverInt` carrier to a raw i64, for an i64-typed builtin slot whose
/// receiving helper clamps out-of-range counts/indices (`List.take` /
/// `List.drop` counts, `String.charAt` / `String.slice` indices). A no-op
/// passthrough when `bignum` is off (the arg is already i64).
fn emit_aint_arg_as_i64_sat(
    func: &mut Function,
    arg: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<bool, WasmGcError> {
    if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
        return Ok(false);
    }
    if ctx.registry.bignum {
        let to_sat = aint_helper(ctx, "__aint_to_i64_sat")?;
        func.instruction(&Instruction::Call(to_sat));
    }
    Ok(true)
}

/// bignum slice 3 — emit a `Vector`/`List` index expression and leave a
/// signed i32 on the stack such that `idx >= 0 (signed)` AND
/// `idx < len (unsigned)` is the in-bounds predicate. Under the flag the
/// `$AverInt` index goes through `__aint_to_index` (Big or out-of-range →
/// the sentinel `-1`, which fails BOTH halves of that predicate, so a Big
/// index reads out-of-bounds — the VM's `Option.None` — instead of being
/// `I32WrapI64`-truncated into a wrong slot). Flag-off it is the byte-
/// identical `I32WrapI64`; the caller still guards the lower bound on the
/// raw i64 (`index >= 0`), so off-path behaviour is unchanged.
fn emit_index_i32(
    func: &mut Function,
    index: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<bool, WasmGcError> {
    if emit_mir_expr(func, index, slots, ctx)?.is_none() {
        return Ok(false);
    }
    if ctx.registry.bignum {
        let to_index = aint_helper(ctx, "__aint_to_index")?;
        func.instruction(&Instruction::Call(to_index));
    } else {
        func.instruction(&Instruction::I32WrapI64);
    }
    Ok(true)
}

/// bignum slice 3 — emit the lower-bound half of an index bounds-check:
/// flag-off keeps the raw i64 `index >= 0`; flag-on tests the extracted
/// i32 `idx >= 0` (signed), since `__aint_to_index` already collapses a
/// negative / Big / too-large index to the `-1` sentinel.
fn emit_index_nonneg(
    func: &mut Function,
    index: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<bool, WasmGcError> {
    if ctx.registry.bignum {
        if !emit_index_i32(func, index, slots, ctx)? {
            return Ok(false);
        }
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::I32GeS);
    } else {
        if emit_mir_expr(func, index, slots, ctx)?.is_none() {
            return Ok(false);
        }
        func.instruction(&Instruction::I64Const(0));
        func.instruction(&Instruction::I64GeS);
    }
    Ok(true)
}

/// bignum slice 2 — the `(ref null $AverInt)` block-type for `if`/`else`
/// arms that yield an `Int` under the flag (e.g. the `Int.min`/`max`
/// select).
fn aint_block_ty(ctx: &EmitCtx<'_>) -> Result<wasm_encoder::BlockType, WasmGcError> {
    let idx = ctx.registry.aint_struct_idx.ok_or(WasmGcError::Validation(
        "bignum active but no $AverInt struct slot was allocated".into(),
    ))?;
    Ok(wasm_encoder::BlockType::Result(
        crate::codegen::wasm_gc::types::struct_ref(idx),
    ))
}

/// `--target wasip2` effect lowering — the MIR analogue of the
/// `ctx.wasip2_lowering.is_some()` block in `emit_dotted_builtin`
/// (builtins.rs). On wasip2 every Console / Args / Env / Time / Random
/// / Disk / Http / Tcp effect lowers to a canonical-ABI call sequence
/// (in `builtins_wasip2.rs`), NOT the AverBridge `fn_map.effects` host
/// import — so this must run BEFORE the `fn_map.effects` branch in the
/// `MirCallee::Builtin` arm, exactly as the HIR dispatch ordered it.
/// Returns `NotHandled` when wasip2 lowering isn't active or `dotted`
/// isn't a wasip2-lowered effect (so the caller tries the AverBridge
/// effect / builtin paths next). The wasip2 emitters already take MIR
/// args and recurse `emit_mir_expr`, so no per-arg pre-emission is
/// needed here.
pub(crate) fn emit_mir_wasip2_effect(
    func: &mut Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    expr: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    use super::super::builtins_wasip2::*;
    if ctx.wasip2_lowering.is_none() {
        return Ok(MirBuiltinEmit::NotHandled);
    }
    let produces = aver_type_str_of(expr).trim() != "Unit";
    let Some((parent, method)) = dotted.split_once('.') else {
        return Ok(MirBuiltinEmit::NotHandled);
    };
    // `Args.get()` is intercepted before this (see the
    // `MirCallee::Builtin` arm), but route it here too for symmetry with
    // the HIR dispatch — on wasip2 it's the canonical-ABI variant.
    match (parent, method) {
        ("Args", "get") if args.is_empty() => emit_args_get_wasip2(func, slots, ctx)?,
        ("Console", "print" | "error" | "warn") => {
            emit_console_print_wasip2(func, method, args, slots, ctx)?
        }
        ("Console", "readLine") => emit_console_read_line_wasip2(func, args, ctx)?,
        ("Time", "unixMs") => emit_time_unix_ms_wasip2(func, args, ctx)?,
        ("Time", "now") => emit_time_now_wasip2(func, args, ctx)?,
        ("Time", "sleep") => emit_time_sleep_wasip2(func, args, slots, ctx)?,
        ("Random", "int") => emit_random_int_wasip2(func, args, slots, ctx)?,
        ("Random", "float") => emit_random_float_wasip2(func, args, ctx)?,
        ("Env", "get") => emit_env_get_wasip2(func, args, slots, ctx)?,
        ("Disk", "exists") => emit_disk_exists_wasip2(func, args, slots, ctx)?,
        ("Disk", "readText") => emit_disk_read_text_wasip2(func, args, slots, ctx)?,
        ("Disk", "writeText") => emit_disk_write_text_wasip2(func, args, slots, ctx)?,
        ("Disk", "appendText") => emit_disk_append_text_wasip2(func, args, slots, ctx)?,
        ("Disk", "delete") => emit_disk_delete_wasip2(func, args, slots, ctx)?,
        ("Disk", "deleteDir") => emit_disk_delete_dir_wasip2(func, args, slots, ctx)?,
        ("Disk", "makeDir") => emit_disk_make_dir_wasip2(func, args, slots, ctx)?,
        ("Disk", "listDir") => emit_disk_list_dir_wasip2(func, args, slots, ctx)?,
        ("Http", "get") => emit_http_get_wasip2(func, args, slots, ctx)?,
        ("Http", "head") => emit_http_head_wasip2(func, args, slots, ctx)?,
        ("Http", "delete") => emit_http_delete_wasip2(func, args, slots, ctx)?,
        ("Http", "post") => emit_http_post_wasip2(func, args, slots, ctx)?,
        ("Http", "put") => emit_http_put_wasip2(func, args, slots, ctx)?,
        ("Http", "patch") => emit_http_patch_wasip2(func, args, slots, ctx)?,
        ("Tcp", "connect") => emit_tcp_connect_wasip2(func, args, slots, ctx)?,
        ("Tcp", "close") => emit_tcp_close_wasip2(func, args, slots, ctx)?,
        ("Tcp", "writeLine") => emit_tcp_write_line_wasip2(func, args, slots, ctx)?,
        ("Tcp", "writeBytes") => emit_tcp_write_bytes_wasip2(func, args, slots, ctx)?,
        ("Tcp", "readLine") => emit_tcp_read_line_wasip2(func, args, slots, ctx)?,
        ("Tcp", "readBytes") => emit_tcp_read_bytes_wasip2(func, args, slots, ctx)?,
        ("Tcp", "send") => emit_tcp_send_wasip2(func, args, slots, ctx)?,
        ("Tcp", "sendBytes") => emit_tcp_send_bytes_wasip2(func, args, slots, ctx)?,
        ("Tcp", "ping") => emit_tcp_ping_wasip2(func, args, slots, ctx)?,
        _ => return Ok(MirBuiltinEmit::NotHandled),
    }
    Ok(MirBuiltinEmit::Produced(produces))
}

/// Mirror of the custom-inline arms of `emit_dotted_builtin`
/// (builtins.rs): builtins that lower to a fixed inline wasm sequence
/// rather than a `fn_map.builtins`-keyed helper call, so a plain
/// `fn_map.builtins.get(dotted)` lookup misses them. Covers the native
/// scalar `Float` / `Int` / `Bool` ops and the `String`
/// ops whose helper is keyed under a *different* name than the surface
/// builtin — `String.length` dispatches to the
/// `String.len` helper, and `String.split` / `String.join` to the
/// singleton `string_split_ops`. Each recurses `emit_mir_expr` on its
/// args — the byte-identical analogue of the oracle's `emit_expr`.
/// `Int.abs` / `Int.min` / `Int.max` re-emit an arg more than once (an
/// `if`/`else` select), exactly as the oracle does; a `None` from any
/// re-emission is a clean whole-fn fallback (`func` is reset by the
/// caller). `Int.mod` is deliberately absent: it builds a
/// `Result<Int,String>` carrier and has a fused form, so it stays on the
/// HIR path. (`String.fromInt` / `String.fromFloat` are *not* here —
/// their helper is keyed under the surface name, so the dispatcher's
/// `fn_map.builtins` path already covers them byte-identically.)
pub(crate) fn emit_mir_native_scalar_builtin(
    func: &mut Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    // Emit one arg, mapping a `None` (uncovered sub-expr) to `Fallback`.
    macro_rules! arg {
        ($i:expr) => {
            if emit_mir_expr(func, &args[$i], slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    // Ties-away-from-zero rounding (matches the VM's `f64::round`). wasm has
    // only `F64Nearest` (ties-to-even), so emit it inline as
    // `t = trunc(x); select(t + copysign(1, x), t, |x - t| >= 0.5)`. The
    // half-test is EXACT — there is deliberately NO `+0.5` pre-round, which
    // would double-round a predecessor-of-half like `0.49999999999999994`
    // UP to 1 and is absorbed into a no-op for large odd integers in
    // `[2^52, 2^53)`; both are inputs `F64Nearest` and `floor(|x|+0.5)` get
    // wrong. `x` (`args[$i]`) is re-emitted (the existing `Int.abs`/`min`/
    // `max` arms re-emit args the same way) — safe because `Float.round`'s
    // argument is a pure scalar. Leaves the rounded `f64` on the stack.
    macro_rules! round_ties_away {
        ($i:expr) => {{
            // val1 = trunc(x) + copysign(1.0, x)
            arg!($i);
            func.instruction(&Instruction::F64Trunc);
            func.instruction(&Instruction::F64Const((1.0f64).into()));
            arg!($i);
            func.instruction(&Instruction::F64Copysign);
            func.instruction(&Instruction::F64Add);
            // val2 = trunc(x)
            arg!($i);
            func.instruction(&Instruction::F64Trunc);
            // cond = |x - trunc(x)| >= 0.5
            arg!($i);
            arg!($i);
            func.instruction(&Instruction::F64Trunc);
            func.instruction(&Instruction::F64Sub);
            func.instruction(&Instruction::F64Abs);
            func.instruction(&Instruction::F64Const((0.5f64).into()));
            func.instruction(&Instruction::F64Ge);
            // select val1 (round away) if |frac| >= 0.5 else val2 (trunc)
            func.instruction(&Instruction::Select);
        }};
    }
    let i64_block = wasm_encoder::BlockType::Result(ValType::I64);
    match dotted {
        // bignum slice 3 — Int <-> Float bridges over `$AverInt`. The
        // scalar `F64ConvertI64S` / saturating `I64TruncF64S` are WRONG for
        // values outside i64 (the plan flags this), so under the flag these
        // route through the limb helpers that match `aver-rt`'s `to_f64`
        // (±inf saturation) and `from_f64_trunc` (non-finite → 0, huge
        // finite → exact Big). The i64-scalar arms below stay for flag-off.
        "Float.fromInt" if args.len() == 1 && ctx.registry.bignum => {
            let to_f64 = aint_helper(ctx, "__aint_to_f64")?;
            arg!(0);
            func.instruction(&Instruction::Call(to_f64));
        }
        "Int.fromFloat" if args.len() == 1 && ctx.registry.bignum => {
            let from_f64 = aint_helper(ctx, "__aint_from_f64")?;
            arg!(0);
            func.instruction(&Instruction::Call(from_f64));
        }
        "Float.floor" if args.len() == 1 && ctx.registry.bignum => {
            let from_f64 = aint_helper(ctx, "__aint_from_f64")?;
            arg!(0);
            func.instruction(&Instruction::F64Floor);
            func.instruction(&Instruction::Call(from_f64));
        }
        "Float.ceil" if args.len() == 1 && ctx.registry.bignum => {
            let from_f64 = aint_helper(ctx, "__aint_from_f64")?;
            arg!(0);
            func.instruction(&Instruction::F64Ceil);
            func.instruction(&Instruction::Call(from_f64));
        }
        "Float.round" if args.len() == 1 && ctx.registry.bignum => {
            let from_f64 = aint_helper(ctx, "__aint_from_f64")?;
            round_ties_away!(0);
            func.instruction(&Instruction::Call(from_f64));
        }
        "Float.fromInt" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64ConvertI64S);
        }
        "Int.fromFloat" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::I64TruncF64S);
        }
        "Float.floor" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64Floor);
            func.instruction(&Instruction::I64TruncF64S);
        }
        "Float.ceil" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64Ceil);
            func.instruction(&Instruction::I64TruncF64S);
        }
        "Float.round" if args.len() == 1 => {
            round_ties_away!(0);
            func.instruction(&Instruction::I64TruncF64S);
        }
        "Float.abs" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64Abs);
        }
        "Float.sqrt" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::F64Sqrt);
        }
        "Float.min" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::F64Min);
        }
        "Float.max" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::F64Max);
        }
        "Float.pi" if args.is_empty() => {
            func.instruction(&Instruction::F64Const(std::f64::consts::PI.into()));
        }
        // bignum slice 2 — `Int.abs` / `Int.min` / `Int.max` over
        // `$AverInt` refs route through the limb helpers (`__aint_abs`,
        // `__aint_cmp`) so they're correct across the Small/Big boundary
        // and both signs (incl. `|i64::MIN| = +2^63`). The i64-scalar arms
        // below stay for the default (flag-off) path.
        "Int.abs" if args.len() == 1 && ctx.registry.bignum => {
            let abs_idx = aint_helper(ctx, "__aint_abs")?;
            arg!(0);
            func.instruction(&Instruction::Call(abs_idx));
        }
        "Int.min" if args.len() == 2 && ctx.registry.bignum => {
            // a if cmp(a, b) <= 0 else b  (mirrors `AverInt::min_ref`); both
            // args are pure builtin args, re-emitted per branch.
            let cmp_idx = aint_helper(ctx, "__aint_cmp")?;
            let aint_block = aint_block_ty(ctx)?;
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::Call(cmp_idx));
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32LeS);
            func.instruction(&Instruction::If(aint_block));
            arg!(0);
            func.instruction(&Instruction::Else);
            arg!(1);
            func.instruction(&Instruction::End);
        }
        "Int.max" if args.len() == 2 && ctx.registry.bignum => {
            // a if cmp(a, b) >= 0 else b  (mirrors `AverInt::max_ref`).
            let cmp_idx = aint_helper(ctx, "__aint_cmp")?;
            let aint_block = aint_block_ty(ctx)?;
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::Call(cmp_idx));
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32GeS);
            func.instruction(&Instruction::If(aint_block));
            arg!(0);
            func.instruction(&Instruction::Else);
            arg!(1);
            func.instruction(&Instruction::End);
        }
        "Int.abs" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::I64Const(0));
            func.instruction(&Instruction::I64LtS);
            func.instruction(&Instruction::If(i64_block));
            func.instruction(&Instruction::I64Const(0));
            arg!(0);
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::Else);
            arg!(0);
            func.instruction(&Instruction::End);
        }
        "Int.min" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::I64LtS);
            func.instruction(&Instruction::If(i64_block));
            arg!(0);
            func.instruction(&Instruction::Else);
            arg!(1);
            func.instruction(&Instruction::End);
        }
        "Int.max" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::I64GtS);
            func.instruction(&Instruction::If(i64_block));
            arg!(0);
            func.instruction(&Instruction::Else);
            arg!(1);
            func.instruction(&Instruction::End);
        }
        "Bool.and" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::I32And);
        }
        "Bool.or" if args.len() == 2 => {
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::I32Or);
        }
        "Bool.not" if args.len() == 1 => {
            arg!(0);
            func.instruction(&Instruction::I32Eqz);
        }
        // Alias spelling for `String.len` (scalar count) — the helper
        // is keyed under `String.len`, not the surface name.
        // `String.byteLength` has its own helper (keyed by surface
        // name) and rides the generic `fn_map.builtins` path.
        "String.length" if args.len() == 1 => {
            let len_idx =
                ctx.fn_map
                    .builtins
                    .get("String.len")
                    .copied()
                    .ok_or(WasmGcError::Validation(
                        "String.length requires the String.len builtin".into(),
                    ))?;
            arg!(0);
            func.instruction(&Instruction::Call(len_idx));
            // `String.length` returns `Int` — lift the raw i64 count.
            lift_i64_result_to_aint(func, ctx)?;
        }
        // `String.split` / `String.join` ride the singleton (T=String)
        // `string_split_ops`, not `fn_map.builtins`.
        "String.split" if args.len() == 2 => {
            let ops = ctx.fn_map.string_split_ops.ok_or(WasmGcError::Validation(
                "String.split called but split helper wasn't registered".into(),
            ))?;
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::Call(ops.split));
        }
        "String.join" if args.len() == 2 => {
            let ops = ctx.fn_map.string_split_ops.ok_or(WasmGcError::Validation(
                "String.join called but join helper wasn't registered".into(),
            ))?;
            arg!(0);
            arg!(1);
            func.instruction(&Instruction::Call(ops.join));
        }
        _ => return Ok(MirBuiltinEmit::NotHandled),
    }
    Ok(MirBuiltinEmit::Produced(true))
}

/// Full mirror of `emit_option_with_default`. The two fused leaves —
/// `Option.withDefault(Vector.set(v, i, x), v)` (the
/// `VectorSetOrDefaultSameVector` in-place/clone set, with `default == v`
/// recognised by slot since the occurrences carry different `last_use`)
/// and `Option.withDefault(Vector.get(v, i), <literal>)` (the
/// `VectorGetOrDefaultLiteral` bounds-checked `array.get`) — are emitted
/// inline; every other `Option.withDefault` lowers to the boxed
/// `Option<T>` unwrap (tag-dispatch returning the `Some` payload or the
/// default). `Option.withDefault(Map.get(m, k), …)` returns `NotHandled`
/// so the oracle's `get_or_default` fusion handles it (the MIR emitter
/// doesn't mirror that fusion, so boxing here would diverge).
pub(crate) fn emit_mir_option_with_default(
    func: &mut Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    if dotted != "Option.withDefault" || args.len() != 2 {
        return Ok(MirBuiltinEmit::NotHandled);
    }
    let opt_arg = &args[0];
    let default = &args[1];

    macro_rules! e {
        ($x:expr) => {
            if emit_mir_expr(func, $x, slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }

    // Fused shapes — only when the option-producing arg is the specific
    // builtin call the oracle's `classify_leaf_op` / `Map.get` checks key
    // off. Anything else falls through to the boxed `Option<T>` unwrap.
    if let MirExpr::Call(inner_sp) = &opt_arg.node
        && let MirCallee::Builtin(inner_id) = inner_sp.node.callee
        && let Some(inner_dotted) = ctx.mir_builtins.and_then(|n| n.get(inner_id.0 as usize))
    {
        let inner = &inner_sp.node;

        // `Option.withDefault(Vector.set(v, i, x), v)` — in-place /
        // clone-on-write set. `default == vector` is checked by slot
        // (the two occurrences carry different `last_use`, so the
        // `MirLocal`s aren't structurally equal). A *different* default
        // is a real boxed `Option<Vector>` and falls through below.
        if inner_dotted == "Vector.set"
            && inner.args.len() == 3
            && matches!(
                (&default.node, &inner.args[0].node),
                (MirExpr::Local(d), MirExpr::Local(v)) if d.node.slot == v.node.slot
            )
        {
            return emit_mir_vector_set_or_default(
                func,
                &inner.args[0],
                &inner.args[1],
                &inner.args[2],
                slots,
                ctx,
            );
        }

        // `Option.withDefault(Vector.get(v, i), <literal>)` — the
        // `VectorGetOrDefaultLiteral` leaf: bounds-check + `array.get`,
        // literal default on OOB. A non-literal default boxes (below).
        if inner_dotted == "Vector.get"
            && inner.args.len() == 2
            && matches!(default.node, MirExpr::Literal(_))
        {
            let vector = &inner.args[0];
            let index = &inner.args[1];
            let vec_aver = aver_type_str_of(vector);
            let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
            let vec_idx =
                ctx.registry
                    .vector_type_idx(&canonical)
                    .ok_or(WasmGcError::Validation(format!(
                        "Vector.get: vector arg of type `{vec_aver}` is not a registered Vector<T>"
                    )))?;
            let element =
                TypeRegistry::vector_element_type(&canonical).ok_or(WasmGcError::Validation(
                    format!("Vector.get: cannot parse element type from `{canonical}`"),
                ))?;
            let elem_val =
                aver_to_wasm(element, Some(ctx.registry))?.ok_or(WasmGcError::Validation(
                    format!("Vector.get: element type `{element}` has no wasm representation"),
                ))?;
            let block_ty = wasm_encoder::BlockType::Result(elem_val);
            // bignum slice 3 — index extraction routes through
            // `__aint_to_index` (Big → OOB), see `emit_index_i32`.
            macro_rules! idx_nonneg {
                () => {
                    if !emit_index_nonneg(func, index, slots, ctx)? {
                        return Ok(MirBuiltinEmit::Fallback);
                    }
                };
            }
            macro_rules! idx_i32 {
                () => {
                    if !emit_index_i32(func, index, slots, ctx)? {
                        return Ok(MirBuiltinEmit::Fallback);
                    }
                };
            }
            idx_nonneg!();
            idx_i32!();
            e!(vector);
            func.instruction(&Instruction::ArrayLen);
            func.instruction(&Instruction::I32LtU);
            func.instruction(&Instruction::I32And);
            func.instruction(&Instruction::If(block_ty));
            e!(vector);
            idx_i32!();
            func.instruction(&Instruction::ArrayGet(vec_idx));
            func.instruction(&Instruction::Else);
            e!(default);
            func.instruction(&Instruction::End);
            return Ok(MirBuiltinEmit::Produced(true));
        }

        // `Option.withDefault(Map.get(m, k), default)` — fuse into the
        // per-`Map` `get_or_default` helper (no `Option<V>` ever allocs
        // on the hot lookup path). Mirror of the retired HIR
        // `emit_map_get_or_default`: emit map, key, default, then call
        // the helper. `Map.get` always carries exactly 2 args
        // (map, key); a malformed shape falls back.
        if inner_dotted == "Map.get" && inner.args.len() == 2 {
            let map = &inner.args[0];
            let key = &inner.args[1];
            let map_aver = aver_type_str_of(map);
            let canonical: String = map_aver.chars().filter(|c| !c.is_whitespace()).collect();
            let get_or_default = ctx
                .fn_map
                .map_helpers_lookup(&canonical)
                .ok_or(WasmGcError::Validation(format!(
                    "Map.get fusion: map argument has type `{map_aver}` but no helpers are \
                     registered"
                )))?
                .get_or_default;
            e!(map);
            e!(key);
            e!(default);
            func.instruction(&Instruction::Call(get_or_default));
            return Ok(MirBuiltinEmit::Produced(true));
        }
    }

    // Boxed `Option<T>` unwrap — mirror of `emit_option_with_default_boxed`:
    // stash the option in the subject scratch, read its tag (field 0),
    // return the `Some` payload (field 1) on tag 1, else the default.
    let opt_aver = aver_type_str_of(opt_arg);
    let canonical: String = opt_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let opt_idx = ctx
        .registry
        .option_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option.withDefault: opt arg of type `{opt_aver}` is not a registered Option<T>"
        )))?;
    let element = TypeRegistry::option_element_type(&canonical).ok_or(WasmGcError::Validation(
        format!("Option.withDefault: cannot parse element type from `{canonical}`"),
    ))?;
    let elem_val = aver_to_wasm(element, Some(ctx.registry))?.ok_or(WasmGcError::Validation(
        format!("Option.withDefault: element type `{element}` has no wasm representation"),
    ))?;
    let block_ty = wasm_encoder::BlockType::Result(elem_val);
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Option.withDefault (boxed) needs a scratch slot but none was reserved".into(),
    ))?;
    e!(opt_arg);
    func.instruction(&Instruction::LocalSet(scratch));
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(opt_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 0,
    });
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(block_ty));
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(opt_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::Else);
    e!(default);
    func.instruction(&Instruction::End);
    Ok(MirBuiltinEmit::Produced(true))
}

/// Mirror of `emit_vector_set_or_default`: the fused
/// `Option.withDefault(Vector.set(v, i, x), v)`. When
/// `mir_arg_uniquely_owned` says `v` is a dead, non-aliased binding the
/// engine array is mutated in place (`array.set` on the original handle,
/// no allocation); otherwise the array is cloned (`array.new_default` +
/// `array.copy`) and the copy mutated. The ownership verdict and the
/// instruction sequence match the HIR oracle byte-for-byte —
/// `mir_arg_uniquely_owned` reads the same `last_use` / `aliased_slots`
/// the oracle's `arg_uniquely_owned` does, on the same `v` occurrence
/// (`Vector.set`'s first arg).
fn emit_mir_vector_set_or_default(
    func: &mut Function,
    vector: &Spanned<MirExpr>,
    index: &Spanned<MirExpr>,
    value: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    let vec_aver = aver_type_str_of(vector);
    let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.set: vector arg of type `{vec_aver}` is not a registered Vector<T>"
        )))?;

    macro_rules! e {
        ($x:expr) => {
            if emit_mir_expr(func, $x, slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    // bignum slice 3 — index extraction through `__aint_to_index`.
    macro_rules! idx_nonneg {
        () => {
            if !emit_index_nonneg(func, index, slots, ctx)? {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    macro_rules! idx_i32 {
        () => {
            if !emit_index_i32(func, index, slots, ctx)? {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }

    // Fast path: dead, non-aliased binding → mutate the engine array in
    // place and return the same handle. No scratch, no allocation.
    if mir_arg_uniquely_owned(vector, ctx) {
        idx_nonneg!();
        idx_i32!();
        e!(vector);
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::I32LtU);
        func.instruction(&Instruction::I32And);
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        e!(vector);
        idx_i32!();
        e!(value);
        func.instruction(&Instruction::ArraySet(vec_idx));
        func.instruction(&Instruction::End);
        e!(vector);
        return Ok(MirBuiltinEmit::Produced(true));
    }

    // Slow path (clone-on-write): the slot may share its engine array
    // with another live binding. Allocate a fresh array, copy every
    // cell, mutate the copy.
    let scratch = slots
        .vector_set_scratch
        .get(&canonical)
        .copied()
        .ok_or_else(|| {
            WasmGcError::Validation(format!(
                "Vector.set: scratch local for `{canonical}` not reserved \
                 (slot-pre-pass missed this site)"
            ))
        })?;

    e!(vector);
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::ArrayNewDefault(vec_idx));
    func.instruction(&Instruction::LocalSet(scratch));

    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::I32Const(0));
    e!(vector);
    func.instruction(&Instruction::I32Const(0));
    e!(vector);
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: vec_idx,
        array_type_index_src: vec_idx,
    });

    idx_nonneg!();
    idx_i32!();
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    func.instruction(&Instruction::LocalGet(scratch));
    idx_i32!();
    e!(value);
    func.instruction(&Instruction::ArraySet(vec_idx));
    func.instruction(&Instruction::End);

    func.instruction(&Instruction::LocalGet(scratch));
    Ok(MirBuiltinEmit::Produced(true))
}

/// Boxed `Int.div(a, b)` / `Int.mod(a, b)` — a builtin call whose
/// `Result<Int, String>` is consumed directly (e.g.
/// `match Int.div(a, b) { Result.Ok(q) -> …; Result.Err(_) -> … }`),
/// *not* the fused `Result.withDefault(Int.{div,mod}(a, b), default)`
/// (which `emit_mir_result_with_default` collapses before any struct is
/// built). This materialises the concrete `Result<Int, String>` value
/// the VM produces (`src/types/int.rs`), so the natural unwrap idiom
/// runs on wasm-gc identically:
///
/// - `Int.mod`: `b == 0` → `Err("division by zero")`, else
///   `Ok(__int_mod_euclid(a, b))`.
/// - `Int.div`: `b == 0` → `Err("division by zero")`, else
///   `a == i64::MIN && b == -1` → `Err("division overflow")` (the
///   `i64.div_s` in `__int_div_euclid` would trap on that edge, so it's
///   guarded out before the helper runs), else
///   `Ok(__int_div_euclid(a, b))`.
///
/// The `Result<Int, String>` struct shape is the shared
/// `(mut i32 tag) (mut i64 ok) (mut (ref null $string) err)` — tag 1 =
/// Ok / 0 = Err — exactly as `emit_int_from_string` (`builtins/mod.rs`)
/// and `emit_mir_result_constructor` build it. `a` / `b` are pure
/// builtin args, so re-emitting them per branch matches the fused
/// path's accepted pattern (no double-evaluation hazard).
pub(crate) fn emit_mir_int_div_mod_boxed(
    func: &mut Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    let is_div = match dotted {
        "Int.div" => true,
        "Int.mod" => false,
        _ => return Ok(MirBuiltinEmit::NotHandled),
    };
    if args.len() != 2 {
        return Ok(MirBuiltinEmit::NotHandled);
    }
    let a = &args[0];
    let b = &args[1];

    macro_rules! e {
        ($x:expr) => {
            if emit_mir_expr(func, $x, slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }

    // The carrier `Result<Int, String>` struct slot — discovery
    // (`types_discovery.rs`) registers it for every `Int.div` / `Int.mod`.
    let res_idx =
        ctx.registry
            .result_type_idx("Result<Int,String>")
            .ok_or(WasmGcError::Validation(
                "boxed Int.div/mod requires the `Result<Int,String>` slot to be registered".into(),
            ))?;
    // bignum slice 2 — under the opt-in `$AverInt` representation, `a`/`b`
    // are struct refs (not i64). Route through the single Euclidean
    // `__aint_divmod` helper (a `want_mod` flag selects div vs mod), test
    // `b == 0` on the struct, and DROP the `i64::MIN / -1` overflow guard:
    // over ℤ that quotient is `+2^63`, a valid `Result.Ok` (a Big), not an
    // error — matching the VM (`src/types/int.rs`) and `aver-rt`.
    if ctx.registry.bignum {
        return emit_mir_aint_div_mod_boxed(func, is_div, a, b, slots, ctx, res_idx);
    }

    let helper_name = if is_div {
        "__int_div_euclid"
    } else {
        "__int_mod_euclid"
    };
    let helper_idx =
        ctx.fn_map
            .builtins
            .get(helper_name)
            .copied()
            .ok_or(WasmGcError::Validation(format!(
                "boxed Int.{} requires the {helper_name} helper to be registered",
                if is_div { "div" } else { "mod" }
            )))?;

    // Each branch leaves a `(ref null $result)` on the stack.
    let res_block = wasm_encoder::BlockType::Result(ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(res_idx),
    }));

    // `Result.Err(<msg>)` — tag 0, i64 ok placeholder, the message string.
    macro_rules! emit_err {
        ($msg:expr) => {{
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I64Const(0));
            emit_string_literal_bytes(func, $msg, ctx)?;
            func.instruction(&Instruction::StructNew(res_idx));
        }};
    }
    // `Result.Ok(helper(a, b))` — tag 1, the quotient/modulo, null err.
    macro_rules! emit_ok {
        () => {{
            func.instruction(&Instruction::I32Const(1));
            e!(a);
            e!(b);
            func.instruction(&Instruction::Call(helper_idx));
            emit_default_value(func, "String", ctx.registry)?;
            func.instruction(&Instruction::StructNew(res_idx));
        }};
    }

    // if b == 0 -> Err("division by zero")
    e!(b);
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64Eq);
    func.instruction(&Instruction::If(res_block));
    emit_err!(b"division by zero");
    func.instruction(&Instruction::Else);
    if is_div {
        // else if a == i64::MIN && b == -1 -> Err("division overflow")
        e!(a);
        func.instruction(&Instruction::I64Const(i64::MIN));
        func.instruction(&Instruction::I64Eq);
        e!(b);
        func.instruction(&Instruction::I64Const(-1));
        func.instruction(&Instruction::I64Eq);
        func.instruction(&Instruction::I32And);
        func.instruction(&Instruction::If(res_block));
        emit_err!(b"division overflow");
        func.instruction(&Instruction::Else);
        emit_ok!();
        func.instruction(&Instruction::End);
    } else {
        emit_ok!();
    }
    func.instruction(&Instruction::End);
    Ok(MirBuiltinEmit::Produced(true))
}

/// The `Bits` namespace on wasm-gc. Every operation here is `Int -> Int`
/// under infinite two's complement, on the `$AverInt` carrier — never a
/// fixed-width bit-vector and never a raw i64, so nothing truncates.
///
/// Only the bignum representation is served. That is not a gap: a `Bits`
/// call flips the bignum gate itself (`builtin_touches_int` in `types.rs`),
/// so a reachable `Bits` call always has `$AverInt` active. The scalar-i64
/// arm is therefore unreachable, and it FAILS rather than lowering through
/// the wrapping path — the one outcome `Bits` must never produce.
///
/// - `and` / `or` / `xor` → `__aint_bitwise(a, b, op)`.
/// - `not` → `__aint_sub(-1, a)`, since complement over ℤ is `-1 - a`.
/// - `shiftLeft(x, n)` → `__aint_mul(x, __aint_pow2(n))`.
/// - `shiftRight(x, n)` → `__aint_divmod(x, __aint_pow2(n), 0)` — Euclidean
///   division by a positive divisor is floor division, i.e. the arithmetic
///   shift the specification asks for.
/// - `low(x, w)` → `__aint_divmod(x, __aint_pow2(w), 1)` — the Euclidean
///   remainder is already in `[0, 2^w)`.
///
/// The three count-taking operations build the concrete `Result<Int,String>`
/// struct, guarding a negative count into `Result.Err` with the VM's exact
/// message bytes. The negativity test goes through `__aint_to_i64_sat`,
/// which saturates a Big to `i64::MIN`/`MAX` — so its sign is the value's
/// sign for every operand, Small or Big.
pub(crate) fn emit_mir_bits(
    func: &mut Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    let Some(op) = BitsOp::from_dotted(dotted) else {
        return Ok(MirBuiltinEmit::NotHandled);
    };
    if args.len() != op.arity() {
        return Ok(MirBuiltinEmit::NotHandled);
    }
    if !ctx.registry.bignum {
        return Err(WasmGcError::Validation(format!(
            "{dotted} requires the $AverInt representation; \
             a reachable Bits call must have flipped the bignum gate"
        )));
    }

    macro_rules! e {
        ($x:expr) => {
            if emit_mir_expr(func, $x, slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    let helper = |name: &str| -> Result<u32, WasmGcError> {
        ctx.fn_map
            .builtins
            .get(name)
            .copied()
            .ok_or(WasmGcError::Validation(format!(
                "{dotted} requires the {name} helper to be registered"
            )))
    };

    // The total operations answer with a bare `$AverInt`.
    match op {
        BitsOp::And | BitsOp::Or | BitsOp::Xor => {
            let bitwise = helper("__aint_bitwise")?;
            e!(&args[0]);
            e!(&args[1]);
            func.instruction(&Instruction::I32Const(op.bitwise_selector()));
            func.instruction(&Instruction::Call(bitwise));
            return Ok(MirBuiltinEmit::Produced(true));
        }
        BitsOp::Not => {
            // `-1 - a`, the arithmetic definition of complement over ℤ.
            let from_i64 = helper("__aint_from_i64")?;
            let sub = helper("__aint_sub")?;
            func.instruction(&Instruction::I64Const(-1));
            func.instruction(&Instruction::Call(from_i64));
            e!(&args[0]);
            func.instruction(&Instruction::Call(sub));
            return Ok(MirBuiltinEmit::Produced(true));
        }
        BitsOp::ShiftLeft | BitsOp::ShiftRight | BitsOp::Low => {}
    }

    let res_idx =
        ctx.registry
            .result_type_idx("Result<Int,String>")
            .ok_or(WasmGcError::Validation(format!(
                "{dotted} requires the `Result<Int,String>` slot to be registered"
            )))?;
    let res_block = wasm_encoder::BlockType::Result(ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(res_idx),
    }));
    let to_i64_sat = helper("__aint_to_i64_sat")?;

    // if count < 0 -> Result.Err(<message>)
    e!(&args[1]);
    func.instruction(&Instruction::Call(to_i64_sat));
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64LtS);
    func.instruction(&Instruction::If(res_block));
    func.instruction(&Instruction::I32Const(0));
    emit_default_value(func, "Int", ctx.registry)?;
    emit_string_literal_bytes(func, op.negative_message(), ctx)?;
    func.instruction(&Instruction::StructNew(res_idx));
    func.instruction(&Instruction::Else);
    // else Result.Ok(<the operation>)
    func.instruction(&Instruction::I32Const(1));
    emit_bits_counted_value(func, op, args, slots, ctx, &helper)?;
    emit_default_value(func, "String", ctx.registry)?;
    func.instruction(&Instruction::StructNew(res_idx));
    func.instruction(&Instruction::End);
    Ok(MirBuiltinEmit::Produced(true))
}

/// The bare `$AverInt` value of a count-taking `Bits` operation, with no
/// `Result` wrap. Shared by the guarded surface call above and by the
/// literal-count discharge's intrinsic, so the discharged and undischarged
/// forms cannot compute different things.
pub(crate) fn emit_bits_counted_value(
    func: &mut Function,
    op: BitsOp,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    helper: &impl Fn(&str) -> Result<u32, WasmGcError>,
) -> Result<(), WasmGcError> {
    let pow2 = helper("__aint_pow2")?;
    if emit_mir_expr(func, &args[0], slots, ctx)?.is_none() {
        return Err(WasmGcError::Validation(
            "Bits operand failed to lower".into(),
        ));
    }
    if emit_mir_expr(func, &args[1], slots, ctx)?.is_none() {
        return Err(WasmGcError::Validation("Bits count failed to lower".into()));
    }
    func.instruction(&Instruction::Call(pow2));
    match op {
        BitsOp::ShiftLeft => {
            func.instruction(&Instruction::Call(helper("__aint_mul")?));
        }
        BitsOp::ShiftRight | BitsOp::Low => {
            // `want_mod` 0 = Euclidean quotient (floor, since 2^n > 0),
            // 1 = Euclidean remainder (already in `[0, 2^n)`).
            func.instruction(&Instruction::I32Const(if matches!(op, BitsOp::Low) {
                1
            } else {
                0
            }));
            func.instruction(&Instruction::Call(helper("__aint_divmod")?));
        }
        _ => unreachable!("emit_bits_counted_value is only for the count-taking operations"),
    }
    Ok(())
}

/// The `Bits` operations, as one enum so the surface-call and discharged
/// paths agree on arity, selector and error text by construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BitsOp {
    And,
    Or,
    Xor,
    Not,
    ShiftLeft,
    ShiftRight,
    Low,
}

impl BitsOp {
    pub(crate) fn from_dotted(dotted: &str) -> Option<Self> {
        Some(match dotted {
            "Bits.and" => Self::And,
            "Bits.or" => Self::Or,
            "Bits.xor" => Self::Xor,
            "Bits.not" => Self::Not,
            "Bits.shiftLeft" => Self::ShiftLeft,
            "Bits.shiftRight" => Self::ShiftRight,
            "Bits.low" => Self::Low,
            _ => return None,
        })
    }

    fn arity(self) -> usize {
        match self {
            Self::Not => 1,
            _ => 2,
        }
    }

    /// The `op` argument `__aint_bitwise` switches on.
    fn bitwise_selector(self) -> i32 {
        match self {
            Self::And => 0,
            Self::Or => 1,
            Self::Xor => 2,
            _ => unreachable!("only and/or/xor reach __aint_bitwise"),
        }
    }

    /// Byte-identical to the VM's `Result.Err` payload (`types/bits.rs`).
    fn negative_message(self) -> &'static [u8] {
        match self {
            Self::Low => b"negative bit width",
            _ => b"negative shift count",
        }
    }
}

/// bignum slice 2 — the `$AverInt` analogue of `emit_mir_int_div_mod_boxed`.
/// `a`/`b` are `(ref null $aint)` struct refs; the carrier
/// `Result<Int,String>`'s ok field (field 1) is therefore an `$AverInt`
/// ref, not i64. Builds the same tagged struct the VM produces:
/// `b == 0` → `Err("division by zero")`, else `Ok(__aint_divmod(a, b, m))`
/// with `m = 0` for `div`, `1` for `mod`. There is NO overflow branch —
/// over ℤ `i64::MIN / -1` is the valid Big `+2^63`.
fn emit_mir_aint_div_mod_boxed(
    func: &mut Function,
    is_div: bool,
    a: &Spanned<MirExpr>,
    b: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    res_idx: u32,
) -> Result<MirBuiltinEmit, WasmGcError> {
    let aint_idx = ctx.registry.aint_struct_idx.ok_or(WasmGcError::Validation(
        "bignum Int.div/mod requires the $AverInt struct slot to be registered".into(),
    ))?;
    let divmod_idx =
        ctx.fn_map
            .builtins
            .get("__aint_divmod")
            .copied()
            .ok_or(WasmGcError::Validation(
                "bignum Int.div/mod requires the __aint_divmod helper to be registered".into(),
            ))?;

    macro_rules! e {
        ($x:expr) => {
            if emit_mir_expr(func, $x, slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }

    let res_block = wasm_encoder::BlockType::Result(ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(res_idx),
    }));

    // `Result.Err("division by zero")` — tag 0, an `$AverInt`-typed ok
    // placeholder (default Int = Small(0)), the message string.
    macro_rules! emit_err {
        () => {{
            func.instruction(&Instruction::I32Const(0));
            emit_default_value(func, "Int", ctx.registry)?;
            emit_string_literal_bytes(func, b"division by zero", ctx)?;
            func.instruction(&Instruction::StructNew(res_idx));
        }};
    }
    // `Result.Ok(__aint_divmod(a, b, want_mod))` — tag 1, the result, null err.
    macro_rules! emit_ok {
        () => {{
            func.instruction(&Instruction::I32Const(1));
            e!(a);
            e!(b);
            func.instruction(&Instruction::I32Const(if is_div { 0 } else { 1 }));
            func.instruction(&Instruction::Call(divmod_idx));
            emit_default_value(func, "String", ctx.registry)?;
            func.instruction(&Instruction::StructNew(res_idx));
        }};
    }

    // if b == 0 (canonical zero is Small(0): `$magf == null && $small == 0`)
    e!(b);
    func.instruction(&Instruction::StructGet {
        struct_type_index: aint_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::RefIsNull);
    e!(b);
    func.instruction(&Instruction::StructGet {
        struct_type_index: aint_idx,
        field_index: 0,
    });
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(res_block));
    emit_err!();
    func.instruction(&Instruction::Else);
    emit_ok!();
    func.instruction(&Instruction::End);
    Ok(MirBuiltinEmit::Produced(true))
}

/// Full mirror of `emit_result_with_default`. Two shapes:
///
/// 1. **Fused** `Result.withDefault(Int.mod(a, b), default)` — `Int.mod`
///    lowers to the Euclidean-modulo helper and never materialises a
///    `Result` struct, so emit the guarded form directly: push `default`
///    when `b == 0`, else `__int_mod_euclid(a, b)`.
/// 2. **Boxed** — any other `Result.withDefault(res, default)`: stash the
///    concrete `Result<T,E>` in the subject scratch, read its tag, return
///    the `Ok` payload (field 1) on tag `1`, else the default.
pub(crate) fn emit_mir_result_with_default(
    func: &mut Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    if dotted != "Result.withDefault" || args.len() != 2 {
        return Ok(MirBuiltinEmit::NotHandled);
    }
    let res_arg = &args[0];
    let default = &args[1];

    macro_rules! e {
        ($x:expr) => {
            if emit_mir_expr(func, $x, slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }

    // Fused `Result.withDefault(Int.mod(a, b), default)` and the parallel
    // `Result.withDefault(Int.div(a, b), default)`. Both fold through their
    // Euclidean helper (`__int_mod_euclid` / `__int_div_euclid`) so `div` and
    // `mod` stay consistent: `div(a,b)*b + mod(a,b) == a` for every sign.
    if let MirExpr::Call(inner) = &res_arg.node {
        let inner = &inner.node;
        if let MirCallee::Builtin(inner_id) = inner.callee
            && let Some(inner_dotted) = ctx.mir_builtins.and_then(|n| n.get(inner_id.0 as usize))
            && (inner_dotted == "Int.mod" || inner_dotted == "Int.div")
            && inner.args.len() == 2
        {
            let is_mod = inner_dotted == "Int.mod";
            let a = &inner.args[0];
            let b = &inner.args[1];
            // bignum slice 2 — fold through the Euclidean `__aint_divmod`
            // helper over `$AverInt` refs. `b == 0` is tested on the struct
            // (canonical zero is Small(0)) and routes to `default`; there is
            // no `i64::MIN / -1` trap edge (that quotient is a valid Big).
            if ctx.registry.bignum {
                let aint_idx = ctx.registry.aint_struct_idx.ok_or(WasmGcError::Validation(
                    "bignum Result.withDefault(Int.div/mod) needs the $AverInt slot".into(),
                ))?;
                let divmod_idx = ctx.fn_map.builtins.get("__aint_divmod").copied().ok_or(
                    WasmGcError::Validation(
                        "bignum Result.withDefault(Int.div/mod) needs the __aint_divmod helper"
                            .into(),
                    ),
                )?;
                let block_ty = wasm_encoder::BlockType::Result(
                    crate::codegen::wasm_gc::types::struct_ref(aint_idx),
                );
                // if b == 0 (`$magf == null && $small == 0`) -> default,
                // else __aint_divmod(a, b, want_mod)
                e!(b);
                func.instruction(&Instruction::StructGet {
                    struct_type_index: aint_idx,
                    field_index: 1,
                });
                func.instruction(&Instruction::RefIsNull);
                e!(b);
                func.instruction(&Instruction::StructGet {
                    struct_type_index: aint_idx,
                    field_index: 0,
                });
                func.instruction(&Instruction::I64Eqz);
                func.instruction(&Instruction::I32And);
                func.instruction(&Instruction::If(block_ty));
                e!(default);
                func.instruction(&Instruction::Else);
                e!(a);
                e!(b);
                func.instruction(&Instruction::I32Const(if is_mod { 1 } else { 0 }));
                func.instruction(&Instruction::Call(divmod_idx));
                func.instruction(&Instruction::End);
                return Ok(MirBuiltinEmit::Produced(true));
            }
            // Both fold through a Euclidean helper so the result matches the
            // VM/Rust `checked_{rem,div}_euclid`: `mod` lands in `[0,|b|)`,
            // `div` floors. The `b == 0` guard below routes divide-by-zero to
            // the default; the lone uncovered edge is `i64::MIN / -1`, where
            // the helper's `i64.{rem,div}_s` traps (a contained wasm trap, not
            // a host crash) — parity with VM/Rust returning `Result.Err` there.
            let helper_name = if is_mod {
                "__int_mod_euclid"
            } else {
                "__int_div_euclid"
            };
            let helper_idx =
                ctx.fn_map
                    .builtins
                    .get(helper_name)
                    .copied()
                    .ok_or(WasmGcError::Validation(format!(
                        "Int.{} requires {} helper to be registered",
                        if is_mod { "mod" } else { "div" },
                        helper_name
                    )))?;
            let block_ty = wasm_encoder::BlockType::Result(ValType::I64);
            // if b == 0 -> default, else helper(a, b)
            e!(b);
            func.instruction(&Instruction::I64Const(0));
            func.instruction(&Instruction::I64Eq);
            func.instruction(&Instruction::If(block_ty));
            e!(default);
            func.instruction(&Instruction::Else);
            e!(a);
            e!(b);
            func.instruction(&Instruction::Call(helper_idx));
            func.instruction(&Instruction::End);
            return Ok(MirBuiltinEmit::Produced(true));
        }
    }

    // Boxed path — read the concrete `Result<T,E>` tag, return the `Ok`
    // payload or the default.
    let res_aver = aver_type_str_of(res_arg);
    let canonical: String = res_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Result.withDefault: arg of type `{res_aver}` is not a registered Result<T,E>"
        )))?;
    let (t_aver, _) = TypeRegistry::result_te(&canonical).ok_or(WasmGcError::Validation(
        format!("Result canonical `{canonical}` malformed"),
    ))?;
    let elem_val = aver_to_wasm(t_aver, Some(ctx.registry))?.ok_or(WasmGcError::Validation(
        format!("Result.withDefault: T type `{t_aver}` has no wasm representation"),
    ))?;
    let block_ty = wasm_encoder::BlockType::Result(elem_val);
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Result.withDefault needs a scratch slot but none was reserved".into(),
    ))?;

    e!(res_arg);
    func.instruction(&Instruction::LocalSet(scratch));
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(res_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 0,
    });
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(block_ty));
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(res_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::Else);
    e!(default);
    func.instruction(&Instruction::End);
    Ok(MirBuiltinEmit::Produced(true))
}

/// Mirror of `emit_map_kv_call`: the `Map.*` methods dispatch to the
/// per-`Map<K,V>` helpers (`fn_map.map_helpers_lookup`). `has` reuses
/// the `get_pair` helper and drops the value; `set` picks `set_in_place`
/// vs the clone-on-write `set` by `mir_arg_uniquely_owned` (the MIR
/// analogue of the oracle's `arg_uniquely_owned`). The canonical comes
/// from the map arg's stamped type; every arg recurses `emit_mir_expr`.
pub(crate) fn emit_mir_map_builtin(
    func: &mut Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    // `Map.fromList(l: List<Tuple<K, V>>) -> Map<K, V>` — the arg is a
    // List, not a Map, so it doesn't fit the `args[0]: Map` shape of the
    // methods below. Mirror of the retired HIR `emit_map_from_list_call`:
    // derive `Map<K,V>` from the list element's `Tuple<K,V>`, emit the
    // list, then call the per-(K,V) `from_list` helper.
    if dotted == "Map.fromList" && args.len() == 1 {
        let list_arg = &args[0];
        let list_aver = aver_type_str_of(list_arg);
        let list_canonical: String = list_aver.chars().filter(|c| !c.is_whitespace()).collect();
        let elem =
            TypeRegistry::list_element_type(&list_canonical).ok_or(WasmGcError::Validation(
                format!("Map.fromList: input `{list_aver}` is not a List<Tuple<K,V>>"),
            ))?;
        let (k, v) = TypeRegistry::tuple_ab(elem).ok_or(WasmGcError::Validation(format!(
            "Map.fromList: list element `{elem}` is not a Tuple<K, V>"
        )))?;
        let map_canonical = format!("Map<{},{}>", k.trim(), v.trim());
        let from_list = ctx
            .fn_map
            .map_helpers_lookup(&map_canonical)
            .ok_or(WasmGcError::Validation(format!(
                "Map.fromList: helpers for `{map_canonical}` not registered"
            )))?
            .from_list;
        if emit_mir_expr(func, list_arg, slots, ctx)?.is_none() {
            return Ok(MirBuiltinEmit::Fallback);
        }
        func.instruction(&Instruction::Call(from_list));
        return Ok(MirBuiltinEmit::Produced(true));
    }
    let method = match dotted {
        "Map.set" => "set",
        "Map.get" => "get",
        "Map.has" => "has",
        "Map.len" => "len",
        "Map.keys" => "keys",
        "Map.values" => "values",
        "Map.remove" => "remove",
        "Map.entries" => "entries",
        _ => return Ok(MirBuiltinEmit::NotHandled),
    };
    let arity = match method {
        "set" => 3,
        "get" | "has" | "remove" => 2,
        _ => 1,
    };
    if args.len() != arity {
        return Ok(MirBuiltinEmit::NotHandled);
    }
    let map_aver = aver_type_str_of(&args[0]);
    let canonical: String = map_aver.chars().filter(|c| !c.is_whitespace()).collect();
    macro_rules! emit_args {
        () => {
            for a in args {
                if emit_mir_expr(func, a, slots, ctx)?.is_none() {
                    return Ok(MirBuiltinEmit::Fallback);
                }
            }
        };
    }
    // `has` reuses `get_pair` and drops the value, leaving the `found`
    // i32 — no `Option<V>` ever allocates.
    if method == "has" {
        let get_pair = ctx
            .fn_map
            .map_helpers_lookup(&canonical)
            .ok_or(WasmGcError::Validation(format!(
                "Map.has: map argument has type `{map_aver}` but no helpers are registered"
            )))?
            .get_pair;
        emit_args!();
        func.instruction(&Instruction::Call(get_pair));
        func.instruction(&Instruction::Drop);
        return Ok(MirBuiltinEmit::Produced(true));
    }
    let target = {
        let helpers = ctx
            .fn_map
            .map_helpers_lookup(&canonical)
            .ok_or(WasmGcError::Validation(format!(
                "Map.{method}: map argument has type `{map_aver}` but no helpers are registered"
            )))?;
        match method {
            "set" => {
                if mir_arg_uniquely_owned(&args[0], ctx) {
                    helpers.set_in_place
                } else {
                    helpers.set
                }
            }
            "get" => helpers.get,
            "len" => helpers.len,
            "keys" => helpers.keys,
            "values" => helpers.values,
            "remove" => helpers.remove,
            "entries" => helpers.entries,
            _ => unreachable!("outer match restricts method"),
        }
    };
    emit_args!();
    func.instruction(&Instruction::Call(target));
    // `Map.len` returns `Int`; its helper computes a raw i64, so lift it
    // into the `$AverInt` carrier. (`keys` / `values` / `entries` return
    // Lists, `get` an `Option<V>`, `has` a Bool, `set` / `remove` a Map —
    // none are Int.)
    if method == "len" {
        lift_i64_result_to_aint(func, ctx)?;
    }
    Ok(MirBuiltinEmit::Produced(true))
}

/// Mirror of the custom-inline `List.*` arms of `emit_dotted_builtin`
/// plus the two `List` builtins intercepted in `emit_expr`'s `Call` arm
/// (`List.prepend`, `List.empty`). None are registered helpers:
/// - `reverse` / `len` / `length` dispatch to the per-`List<T>`
///   `fn_map.list_ops` helper (mirror of `emit_list_op_call`); `concat`
///   / `take` / `drop` / `contains` to the 2-arg variant
///   (`emit_list_op_call_2`). The canonical comes from the first list
///   arg's stamped type via `normalize_compound`. `contains` over a
///   non-eq-able `T` has no registered helper → fall back so the HIR
///   path raises the same diagnostic.
/// - `zip` → the per-`Tuple<A,B>` `zip_ops` helper (`emit_list_zip_call`),
///   `fromVector` → the `vfl_ops` `to_list` helper (`emit_vec_to_list_call`).
/// - `prepend` → `struct.new $list_T head tail` (`emit_list_prepend`);
///   `empty` → `ref.null $list_T` resolved from the sole registered
///   `List<T>` or the enclosing fn's return type (`emit_list_empty`).
///
/// Every arg recurses `emit_mir_expr` (the byte-identical analogue of
/// the oracle's `emit_expr`); helper-index lookups and type canonicals
/// are pure and already work on `MirExpr` via `.ty()`.
pub(crate) fn emit_mir_list_builtin(
    func: &mut Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    macro_rules! emit_args {
        () => {
            for a in args {
                if emit_mir_expr(func, a, slots, ctx)?.is_none() {
                    return Ok(MirBuiltinEmit::Fallback);
                }
            }
        };
    }
    match dotted {
        // Per-`List<T>` helper dispatch (1- and 2-arg). Resolve the
        // helper index first (emits nothing), then the args, then call —
        // byte-identical order to `emit_list_op_call` / `_2`.
        "List.reverse" | "List.len" | "List.length" | "List.concat" | "List.take" | "List.drop"
        | "List.contains" => {
            let arity_ok = match dotted {
                "List.reverse" | "List.len" | "List.length" => args.len() == 1,
                _ => args.len() == 2,
            };
            if !arity_ok {
                return Ok(MirBuiltinEmit::NotHandled);
            }
            let list_aver = aver_type_str_of(&args[0]);
            let canonical = normalize_compound(&list_aver);
            let fn_idx = {
                let ops = ctx
                    .fn_map
                    .list_ops_lookup(&canonical)
                    .ok_or(WasmGcError::Validation(format!(
                        "List op called but `{canonical}` helper wasn't registered"
                    )))?;
                match dotted {
                    "List.reverse" => ops.reverse,
                    "List.len" | "List.length" => ops.len,
                    "List.concat" => ops.concat,
                    "List.take" => ops.take,
                    "List.drop" => ops.drop,
                    // `contains` over a non-eq-able T isn't registered —
                    // fall back so the HIR emitter raises the precise error.
                    "List.contains" => match ops.contains {
                        Some(idx) => idx,
                        None => return Ok(MirBuiltinEmit::Fallback),
                    },
                    _ => unreachable!("outer match restricts dotted"),
                }
            };
            // `List.take` / `List.drop` take an i64 COUNT as their second
            // arg; under `Int = ℤ` that arg is an `$AverInt` ref, so it
            // must be saturate-lowered (a Big count just clamps to
            // all/none, which the helper's `i >= n` loop guard handles).
            // The other 2-arg ops (`concat` second arg = List, `contains`
            // = element) carry no Int count, so they ride `emit_args!`.
            if matches!(dotted, "List.take" | "List.drop") {
                if emit_mir_expr(func, &args[0], slots, ctx)?.is_none() {
                    return Ok(MirBuiltinEmit::Fallback);
                }
                if !emit_aint_arg_as_i64_sat(func, &args[1], slots, ctx)? {
                    return Ok(MirBuiltinEmit::Fallback);
                }
            } else {
                emit_args!();
            }
            func.instruction(&Instruction::Call(fn_idx));
            // `List.len` / `List.length` return `Int`; the helper computes
            // a raw i64, so lift it into the `$AverInt` carrier. (`reverse`
            // / `concat` / `take` / `drop` / `contains` return a List or
            // Bool — no lift.)
            if matches!(dotted, "List.len" | "List.length") {
                lift_i64_result_to_aint(func, ctx)?;
            }
        }
        "List.zip" if args.len() == 2 => {
            let la_aver = aver_type_str_of(&args[0]);
            let lb_aver = aver_type_str_of(&args[1]);
            let a = TypeRegistry::list_element_type(&la_aver).ok_or(WasmGcError::Validation(
                format!("List.zip: first arg type `{la_aver}` is not a List<T>"),
            ))?;
            let b = TypeRegistry::list_element_type(&lb_aver).ok_or(WasmGcError::Validation(
                format!("List.zip: second arg type `{lb_aver}` is not a List<T>"),
            ))?;
            let tup_canonical = format!("Tuple<{},{}>", a.trim(), b.trim());
            let zip_fn =
                ctx.fn_map
                    .zip_ops_lookup(&tup_canonical)
                    .ok_or(WasmGcError::Validation(format!(
                        "List.zip: helper for `{tup_canonical}` wasn't registered"
                    )))?;
            emit_args!();
            func.instruction(&Instruction::Call(zip_fn));
        }
        "List.fromVector" if args.len() == 1 => {
            let vec_aver = aver_type_str_of(&args[0]);
            let vec_canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
            let elem = TypeRegistry::vector_element_type(&vec_canonical).ok_or(
                WasmGcError::Validation(format!(
                    "List.fromVector: cannot parse element type from `{vec_canonical}`"
                )),
            )?;
            let list_canonical = format!("List<{}>", elem.trim());
            let to_list = ctx
                .fn_map
                .vfl_ops
                .get(&list_canonical)
                .ok_or(WasmGcError::Validation(format!(
                    "List.fromVector: helper for `{list_canonical}` wasn't registered"
                )))?
                .to_list;
            emit_args!();
            func.instruction(&Instruction::Call(to_list));
        }
        "List.prepend" if args.len() == 2 => {
            let tail_ty = aver_type_str_of(&args[1]);
            let canonical: String = tail_ty.chars().filter(|c| !c.is_whitespace()).collect();
            let list_idx =
                ctx.registry
                    .list_type_idx(&canonical)
                    .ok_or(WasmGcError::Validation(format!(
                        "List.prepend: tail type `{tail_ty}` is not a registered List<T>"
                    )))?;
            emit_args!();
            func.instruction(&Instruction::StructNew(list_idx));
        }
        "List.empty" if args.is_empty() => {
            let canonical = if ctx.registry.list_order.len() == 1 {
                ctx.registry.list_order[0].clone()
            } else {
                ctx.return_type
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect::<String>()
            };
            let list_idx =
                ctx.registry
                    .list_type_idx(&canonical)
                    .ok_or(WasmGcError::Validation(format!(
                        "List.empty: cannot resolve list instantiation (got `{canonical}`)"
                    )))?;
            func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                list_idx,
            )));
        }
        _ => return Ok(MirBuiltinEmit::NotHandled),
    }
    Ok(MirBuiltinEmit::Produced(true))
}

/// MIR analogue of `EmitCtx::arg_uniquely_owned` (body.rs). The
/// ownership fast-path key — `last_use` on a non-alias-prone local — is
/// carried verbatim on `MirLocal` (the lowerer copies it from
/// `ResolvedExpr::Resolved { last_use }`, see `crate::ir::mir::lower`), and
/// `is_aliased_slot` reads the same resolver `aliased_slots` table, so
/// this returns the identical verdict to the HIR oracle for the same
/// source expression. Anonymous / transient args are uniquely owned.
pub(crate) fn mir_arg_uniquely_owned(arg: &Spanned<MirExpr>, ctx: &EmitCtx<'_>) -> bool {
    match &arg.node {
        MirExpr::Local(local) => {
            local.node.last_use && !ctx.is_aliased_slot(local.node.slot.0 as u16)
        }
        _ => true,
    }
}

/// Mirror of the custom-inline `Vector.*` arms of `emit_dotted_builtin`:
/// `len` (`array.len` + widen), `new` (`array.new`), `fromList` (the
/// `vfl_ops` `from_list` helper), and the boxed `get` / `set` (delegated
/// to the helpers below). None are registered helpers. The fused
/// `Option.withDefault(Vector.get/set, …)` shapes live inside the
/// `Option.withDefault` builtin, which this emitter does not cover — so
/// a fused call falls the whole fn back before its inner `Vector` op is
/// ever reached here, and only genuinely-unfused `Vector` ops take this
/// path (byte-identical to `emit_vector_get_boxed` / `_set_boxed`).
pub(crate) fn emit_mir_vector_builtin(
    func: &mut Function,
    dotted: &str,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    match dotted {
        "Vector.len" if args.len() == 1 => {
            if emit_mir_expr(func, &args[0], slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
            func.instruction(&Instruction::ArrayLen);
            func.instruction(&Instruction::I64ExtendI32U);
            // `Vector.len` returns `Int` — lift the i64 length into the
            // `$AverInt` carrier.
            lift_i64_result_to_aint(func, ctx)?;
        }
        "Vector.new" if args.len() == 2 => {
            let elem_aver = aver_type_str_of(&args[1]);
            let canonical: String = format!("Vector<{elem_aver}>")
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            let vec_idx =
                ctx.registry
                    .vector_type_idx(&canonical)
                    .ok_or(WasmGcError::Validation(format!(
                        "Vector.new: instantiation `{canonical}` was not registered"
                    )))?;
            // wasm `array.new` pops [value, size:i32]; Aver pushes size
            // (i64) then fill, so emit fill, then size, then narrow to i32.
            // Under bignum the size is a `$AverInt` ref, narrowed via
            // `__aint_to_index` (a Big / negative size → the `-1` sentinel,
            // i.e. a 0xffffffff length that `array.new` rejects — the VM
            // likewise can't materialise an out-of-i32 vector); flag-off
            // keeps the byte-identical `I32WrapI64`.
            if emit_mir_expr(func, &args[1], slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
            if !emit_index_i32(func, &args[0], slots, ctx)? {
                return Ok(MirBuiltinEmit::Fallback);
            }
            func.instruction(&Instruction::ArrayNew(vec_idx));
        }
        "Vector.get" if args.len() == 2 => {
            return emit_mir_vector_get_boxed(func, &args[0], &args[1], slots, ctx);
        }
        "Vector.set" if args.len() == 3 => {
            return emit_mir_vector_set_boxed(func, &args[0], &args[1], &args[2], slots, ctx);
        }
        "Vector.fromList" if args.len() == 1 => {
            let list_aver = aver_type_str_of(&args[0]);
            let canonical = normalize_compound(&list_aver);
            let from_list = ctx
                .fn_map
                .vfl_ops_lookup(&canonical)
                .ok_or(WasmGcError::Validation(format!(
                    "Vector.fromList: helper for `{canonical}` wasn't registered \
                     (matching Vector<T> may be missing from the registry)"
                )))?
                .from_list;
            if emit_mir_expr(func, &args[0], slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
            func.instruction(&Instruction::Call(from_list));
        }
        _ => return Ok(MirBuiltinEmit::NotHandled),
    }
    Ok(MirBuiltinEmit::Produced(true))
}

/// Mirror of `emit_vector_get_boxed` (builtins.rs): bounds-checked
/// `Option<T>` — `Some(arr[i])` in range, `None` otherwise.
pub(crate) fn emit_mir_vector_get_boxed(
    func: &mut Function,
    vector: &Spanned<MirExpr>,
    index: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    let vec_aver = aver_type_str_of(vector);
    let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.get: vector arg of type `{vec_aver}` is not a registered Vector<T>"
        )))?;
    let element = TypeRegistry::vector_element_type(&canonical).ok_or(WasmGcError::Validation(
        format!("Vector.get: cannot parse element type from `{canonical}`"),
    ))?;
    let opt_canonical = format!("Option<{}>", element.trim());
    let opt_idx = ctx
        .registry
        .option_type_idx(&opt_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.get: `{opt_canonical}` slot was not registered"
        )))?;
    let opt_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(opt_idx),
    });
    let block_ty = wasm_encoder::BlockType::Result(opt_ref);
    macro_rules! e {
        ($x:expr) => {
            if emit_mir_expr(func, $x, slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    // bignum slice 3 — index extraction through `__aint_to_index`.
    macro_rules! idx_nonneg {
        () => {
            if !emit_index_nonneg(func, index, slots, ctx)? {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    macro_rules! idx_i32 {
        () => {
            if !emit_index_i32(func, index, slots, ctx)? {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    idx_nonneg!();
    idx_i32!();
    e!(vector);
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(block_ty));
    func.instruction(&Instruction::I32Const(1));
    e!(vector);
    idx_i32!();
    func.instruction(&Instruction::ArrayGet(vec_idx));
    func.instruction(&Instruction::StructNew(opt_idx));
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::I32Const(0));
    emit_default_value(func, element, ctx.registry)?;
    func.instruction(&Instruction::StructNew(opt_idx));
    func.instruction(&Instruction::End);
    Ok(MirBuiltinEmit::Produced(true))
}

/// Mirror of `emit_vector_set_boxed` (builtins.rs): `Option<Vector<T>>`.
/// Fast path (uniquely-owned arg) mutates the engine array in place;
/// slow path clone-on-writes through the per-`Vector<T>` scratch local.
pub(crate) fn emit_mir_vector_set_boxed(
    func: &mut Function,
    vector: &Spanned<MirExpr>,
    index: &Spanned<MirExpr>,
    value: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<MirBuiltinEmit, WasmGcError> {
    let vec_aver = aver_type_str_of(vector);
    let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.set: vector arg of type `{vec_aver}` is not a registered Vector<T>"
        )))?;
    let opt_canonical = format!("Option<{canonical}>");
    let opt_idx = ctx
        .registry
        .option_type_idx(&opt_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.set: `{opt_canonical}` slot was not registered"
        )))?;
    let opt_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(opt_idx),
    });
    let block_ty = wasm_encoder::BlockType::Result(opt_ref);
    macro_rules! e {
        ($x:expr) => {
            if emit_mir_expr(func, $x, slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    // bignum slice 3 — index extraction through `__aint_to_index`.
    macro_rules! idx_nonneg {
        () => {
            if !emit_index_nonneg(func, index, slots, ctx)? {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    macro_rules! idx_i32 {
        () => {
            if !emit_index_i32(func, index, slots, ctx)? {
                return Ok(MirBuiltinEmit::Fallback);
            }
        };
    }
    if mir_arg_uniquely_owned(vector, ctx) {
        idx_nonneg!();
        idx_i32!();
        e!(vector);
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::I32LtU);
        func.instruction(&Instruction::I32And);
        func.instruction(&Instruction::If(block_ty));
        e!(vector);
        idx_i32!();
        e!(value);
        func.instruction(&Instruction::ArraySet(vec_idx));
        func.instruction(&Instruction::I32Const(1));
        e!(vector);
        func.instruction(&Instruction::StructNew(opt_idx));
        func.instruction(&Instruction::Else);
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
            vec_idx,
        )));
        func.instruction(&Instruction::StructNew(opt_idx));
        func.instruction(&Instruction::End);
        return Ok(MirBuiltinEmit::Produced(true));
    }

    let scratch = slots
        .vector_set_scratch
        .get(&canonical)
        .copied()
        .ok_or_else(|| {
            WasmGcError::Validation(format!(
                "Vector.set: scratch local for `{canonical}` not reserved \
                 (slot-pre-pass missed this site)"
            ))
        })?;
    idx_nonneg!();
    idx_i32!();
    e!(vector);
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(block_ty));
    e!(vector);
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::ArrayNewDefault(vec_idx));
    func.instruction(&Instruction::LocalSet(scratch));
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::I32Const(0));
    e!(vector);
    func.instruction(&Instruction::I32Const(0));
    e!(vector);
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: vec_idx,
        array_type_index_src: vec_idx,
    });
    func.instruction(&Instruction::LocalGet(scratch));
    idx_i32!();
    e!(value);
    func.instruction(&Instruction::ArraySet(vec_idx));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::StructNew(opt_idx));
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        vec_idx,
    )));
    func.instruction(&Instruction::StructNew(opt_idx));
    func.instruction(&Instruction::End);
    Ok(MirBuiltinEmit::Produced(true))
}

/// The numeric (`Int` / `Float`) tail of `emit_expr`'s `BinOp` arm —
/// byte-for-byte. Returns `None` if an operand falls outside the
/// supported subset (propagated as whole-fn fallback). The I64 / F64
/// instruction selection reads `wasm_type_of`, identical to the
/// `ResolvedExpr` path, so `Int op Float` promotion matches.
pub(crate) fn emit_mir_numeric_binop(
    func: &mut Function,
    bop: &crate::ir::mir::MirBinOp,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let l = &bop.lhs;
    let r = &bop.rhs;
    // ETAP-2 SLICE 2b: decide whether this Int op is a RAW (`i64.*`) op or a
    // boxed (`$AverInt` limb-helper) op. `mir_int_binop_is_raw` keys on the
    // `bare_i64_rewrite`'s structural decision:
    //   - ARITHMETIC (`+`/`-`/`*`): raw only when the WHOLE tree renders raw
    //     (a bare anchor + raw-compatible leaves + the interval the rewrite
    //     already proved `OverflowFree`). A pure-literal tree whose product
    //     overflows i64 is NOT raw — it takes the boxed full-precision path.
    //   - COMPARISON (`==`/`<`/…): raw when at least one operand is a bare
    //     anchor and both are raw-compatible — a comparison never overflows,
    //     so `n == 0` (`n` bare, `0` literal) is `i64.eq`.
    // The mixed case (one bare, one boxed operand) cannot occur: the rewrite
    // wraps a raw operand of a boxed op in `Box`, so a boxed op sees two
    // `$AverInt` operands (the path below). `Div` is not produced over bare
    // Int (source `Int.div` is a `Result` builtin), so it is excluded.
    let both_raw = super::mir_int_binop_is_raw(bop, ctx);
    if both_raw {
        if super::emit_mir_int_raw(func, l, slots, ctx)?.is_none() {
            return Ok(None);
        }
        if super::emit_mir_int_raw(func, r, slots, ctx)?.is_none() {
            return Ok(None);
        }
        let inst = match bop.op {
            BinOp::Add => Instruction::I64Add,
            BinOp::Sub => Instruction::I64Sub,
            BinOp::Mul => Instruction::I64Mul,
            BinOp::Eq => Instruction::I64Eq,
            BinOp::Neq => Instruction::I64Ne,
            BinOp::Lt => Instruction::I64LtS,
            BinOp::Gt => Instruction::I64GtS,
            BinOp::Lte => Instruction::I64LeS,
            BinOp::Gte => Instruction::I64GeS,
            _ => unreachable!("both_raw gated to Add/Sub/Mul + the 6 comparisons"),
        };
        func.instruction(&inst);
        return Ok(Some(()));
    }
    let l_ty = wasm_type_of(l, ctx.registry)?;
    let r_ty = wasm_type_of(r, ctx.registry)?;
    let operand = if l_ty == Some(ValType::F64) || r_ty == Some(ValType::F64) {
        Some(ValType::F64)
    } else {
        l_ty
    };
    // bignum const-compare specialization — an `$AverInt` compared
    // against an i64-fitting Int LITERAL lowers to a tag-branch (Small
    // → native i64 compare; Big → sign-determined) instead of
    // allocating the constant as an `$AverInt` and calling
    // `__aint_cmp`. Fail-closed: only fires when both operands are the
    // `$AverInt` ref type, exactly one is a literal that `fits_i64`,
    // and the op is one of the six comparisons. A `$AverInt`-vs-
    // `$AverInt` comparison (`p.x == q.x`) is NOT a literal on either
    // side, so it stays on `__aint_cmp` byte-for-byte.
    if ctx.registry.bignum
        && ctx.registry.aint_struct_idx.is_some()
        && operand != Some(ValType::F64)
        && operand != Some(ValType::I32)
        && l_ty
            == ctx
                .registry
                .aint_struct_idx
                .map(crate::codegen::wasm_gc::types::struct_ref)
        && r_ty
            == ctx
                .registry
                .aint_struct_idx
                .map(crate::codegen::wasm_gc::types::struct_ref)
        && matches!(
            bop.op,
            BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte
        )
    {
        // `const_on_left` records which side the constant sat on so
        // the tag-branch can flip the comparison (`K < x` ≡ `x > K`).
        let lit = mir_int_literal(l)
            .map(|k| (k, true))
            .or_else(|| mir_int_literal(r).map(|k| (k, false)));
        if let Some((k, const_on_left)) = lit {
            let non_lit = if const_on_left { r } else { l };
            // A simple local is pure and cheap to read repeatedly. Re-emit it
            // instead of stashing into the const-compare scratch local so
            // certified scalar predicates keep the same canonical shape as
            // their plan-first lowering.
            if aint_const_cmp_operand_is_reemittable(&non_lit.node) {
                return emit_aint_cmp_const_reemit(
                    func,
                    non_lit,
                    bop.op,
                    k,
                    const_on_left,
                    slots,
                    ctx,
                );
            }
            // General path: evaluate the non-literal operand once, then let
            // the helper stash it before multiple field reads. This avoids
            // duplicating effects or traps for less trivial operands.
            if emit_mir_expr(func, non_lit, slots, ctx)?.is_none() {
                return Ok(None);
            }
            return emit_aint_cmp_const(func, bop.op, k, const_on_left, slots, ctx);
        }
    }
    if emit_mir_expr(func, l, slots, ctx)?.is_none() {
        return Ok(None);
    }
    if operand == Some(ValType::F64) && l_ty == Some(ValType::I64) {
        func.instruction(&Instruction::F64ConvertI64S);
    }
    if emit_mir_expr(func, r, slots, ctx)?.is_none() {
        return Ok(None);
    }
    if operand == Some(ValType::F64) && r_ty == Some(ValType::I64) {
        func.instruction(&Instruction::F64ConvertI64S);
    }
    // bignum slice 1 — when Int is the `$AverInt` ref, both operands are
    // already on the stack as refs; route arithmetic + comparisons
    // through the limb helpers instead of the wrapping `i64.*` opcodes.
    // Div is slice 2 — bail to the whole-fn fallback rather than
    // miscompile (returns `None`).
    if ctx.registry.bignum
        && operand != Some(ValType::F64)
        && operand != Some(ValType::I32)
        && ctx.registry.aint_struct_idx.is_some()
        && l_ty
            == ctx
                .registry
                .aint_struct_idx
                .map(crate::codegen::wasm_gc::types::struct_ref)
    {
        return emit_aint_binop(func, bop.op, ctx);
    }
    let inst = match (operand, bop.op) {
        (Some(ValType::F64), BinOp::Add) => Instruction::F64Add,
        (Some(ValType::F64), BinOp::Sub) => Instruction::F64Sub,
        (Some(ValType::F64), BinOp::Mul) => Instruction::F64Mul,
        (Some(ValType::F64), BinOp::Div) => Instruction::F64Div,
        (Some(ValType::F64), BinOp::Eq) => Instruction::F64Eq,
        (Some(ValType::F64), BinOp::Neq) => Instruction::F64Ne,
        (Some(ValType::F64), BinOp::Lt) => Instruction::F64Lt,
        (Some(ValType::F64), BinOp::Gt) => Instruction::F64Gt,
        (Some(ValType::F64), BinOp::Lte) => Instruction::F64Le,
        (Some(ValType::F64), BinOp::Gte) => Instruction::F64Ge,
        // `i32`-represented operands — `Bool` (or a single-field record
        // newtype over Bool, unwrapped by `wasm_type_of`). The checker
        // admits only `==` / `!=` on Bool (ordering requires Int /
        // Float / String), so equality is the only i32 row needed.
        // Without it Bool `==` fell into the i64 catch-all below and
        // the module failed validation ("expected i64, found i32") —
        // the shape every `verify ... => true` case synthesizes.
        (Some(ValType::I32), BinOp::Eq) => Instruction::I32Eq,
        (Some(ValType::I32), BinOp::Neq) => Instruction::I32Ne,
        (_, BinOp::Add) => Instruction::I64Add,
        (_, BinOp::Sub) => Instruction::I64Sub,
        (_, BinOp::Mul) => Instruction::I64Mul,
        (_, BinOp::Div) => Instruction::I64DivS,
        (_, BinOp::Eq) => Instruction::I64Eq,
        (_, BinOp::Neq) => Instruction::I64Ne,
        (_, BinOp::Lt) => Instruction::I64LtS,
        (_, BinOp::Gt) => Instruction::I64GtS,
        (_, BinOp::Lte) => Instruction::I64LeS,
        (_, BinOp::Gte) => Instruction::I64GeS,
    };
    func.instruction(&inst);
    Ok(Some(()))
}

/// bignum slice 1 — emit the arithmetic/comparison for two `$AverInt`
/// operands already on the stack. Arithmetic (`+`/`-`/`*`) leaves an
/// `$AverInt` ref; comparisons (`==`/`!=`/`<`/`>`/`<=`/`>=`) leave an
/// i32 bool. `Div` is slice 2 — returns `None` to fall back to the
/// whole-fn path rather than emit a wrong result.
fn emit_aint_binop(
    func: &mut Function,
    op: BinOp,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let call = |name: &str| -> Result<u32, WasmGcError> { aint_helper(ctx, name) };
    match op {
        BinOp::Add => {
            func.instruction(&Instruction::Call(call("__aint_add")?));
        }
        BinOp::Sub => {
            func.instruction(&Instruction::Call(call("__aint_sub")?));
        }
        BinOp::Mul => {
            func.instruction(&Instruction::Call(call("__aint_mul")?));
        }
        BinOp::Div => {
            // slice 2 — not yet supported under bignum; bail.
            return Ok(None);
        }
        BinOp::Eq => {
            func.instruction(&Instruction::Call(call("__aint_eq")?));
        }
        BinOp::Neq => {
            func.instruction(&Instruction::Call(call("__aint_eq")?));
            func.instruction(&Instruction::I32Eqz);
        }
        // Ordering: `__aint_cmp` → i32 in {-1,0,1}; compare to 0.
        BinOp::Lt => {
            func.instruction(&Instruction::Call(call("__aint_cmp")?));
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32LtS);
        }
        BinOp::Gt => {
            func.instruction(&Instruction::Call(call("__aint_cmp")?));
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32GtS);
        }
        BinOp::Lte => {
            func.instruction(&Instruction::Call(call("__aint_cmp")?));
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32LeS);
        }
        BinOp::Gte => {
            func.instruction(&Instruction::Call(call("__aint_cmp")?));
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32GeS);
        }
    }
    Ok(Some(()))
}

/// If `expr` is a literal `Int`, return its `i64` value. Used by the
/// const-compare specialization to peel the constant operand of an
/// `$AverInt`-vs-constant comparison. `Literal::Int` is an `i64` in the
/// AST, so the value is always `fits_i64` by construction — there is no
/// out-of-range Int literal to reject. (A source literal that exceeds
/// `i64` is rejected at lex/parse time, before MIR.)
fn mir_int_literal(expr: &Spanned<MirExpr>) -> Option<i64> {
    match &expr.node {
        MirExpr::Literal(l) => match l.node {
            crate::ast::Literal::Int(n) => Some(n),
            _ => None,
        },
        _ => None,
    }
}

fn aint_const_cmp_operand_is_reemittable(e: &MirExpr) -> bool {
    matches!(e, MirExpr::Local(_))
}

fn emit_aint_cmp_const_reemit(
    func: &mut Function,
    operand: &Spanned<MirExpr>,
    op: BinOp,
    k: i64,
    const_on_left: bool,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let aint_idx = ctx.registry.aint_struct_idx.ok_or(WasmGcError::Validation(
        "const-compare specialization requires the $AverInt struct slot".into(),
    ))?;
    let eff = if const_on_left { flip_cmp(op) } else { op };
    let block_ty = wasm_encoder::BlockType::Result(ValType::I32);

    if emit_mir_expr(func, operand, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::StructGet {
        struct_type_index: aint_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::RefIsNull);
    func.instruction(&Instruction::If(block_ty));

    if emit_mir_expr(func, operand, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::StructGet {
        struct_type_index: aint_idx,
        field_index: 0,
    });
    func.instruction(&Instruction::I64Const(k));
    func.instruction(&match eff {
        BinOp::Eq => Instruction::I64Eq,
        BinOp::Neq => Instruction::I64Ne,
        BinOp::Lt => Instruction::I64LtS,
        BinOp::Gt => Instruction::I64GtS,
        BinOp::Lte => Instruction::I64LeS,
        BinOp::Gte => Instruction::I64GeS,
        _ => unreachable!("emit_aint_cmp_const_reemit gated to comparisons"),
    });

    func.instruction(&Instruction::Else);

    match eff {
        BinOp::Lt | BinOp::Lte => {
            if emit_mir_expr(func, operand, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::StructGet {
                struct_type_index: aint_idx,
                field_index: 2,
            });
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32LtS);
        }
        BinOp::Gt | BinOp::Gte => {
            if emit_mir_expr(func, operand, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::StructGet {
                struct_type_index: aint_idx,
                field_index: 2,
            });
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32GtS);
        }
        BinOp::Eq => {
            func.instruction(&Instruction::I32Const(0));
        }
        BinOp::Neq => {
            func.instruction(&Instruction::I32Const(1));
        }
        _ => unreachable!("emit_aint_cmp_const_reemit gated to comparisons"),
    }

    func.instruction(&Instruction::End);
    Ok(Some(()))
}

/// bignum const-compare specialization — the non-literal `$AverInt`
/// operand is already on the stack; emit a tag-branch comparison
/// against the i64 constant `k` and leave an i32 bool, replacing the
/// general `__aint_cmp` call.
///
/// Soundness rests on the carrier invariant (`__aint_normalize` /
/// `__aint_from_i64`): a value is **Small** (`$magf == null`) iff it is
/// in `[i64::MIN, i64::MAX]` — the FULL i64 range, including `i64::MIN`
/// which `normalize` deliberately demotes to a Small. A value is
/// **Big** (`$magf != null`) iff `|value| > i64::MAX`, i.e. strictly
/// outside the i64 range, with `$sign ∈ {-1, +1}`. Therefore:
///   - Small → load `$small` and do the NATIVE signed i64 compare
///     against `k`.
///   - Big → the relation against any i64 constant is fixed by `$sign`
///     alone: a Big-positive is `> k` for every i64 `k`, a Big-negative
///     is `< k`; and a Big NEVER equals an i64 constant.
///
/// `const_on_left` flips the comparison so the `$AverInt` is always the
/// left operand of the EFFECTIVE relation (`k < x` ≡ `x > k`).
fn emit_aint_cmp_const(
    func: &mut Function,
    op: BinOp,
    k: i64,
    const_on_left: bool,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let aint_idx = ctx.registry.aint_struct_idx.ok_or(WasmGcError::Validation(
        "const-compare specialization requires the $AverInt struct slot".into(),
    ))?;
    let scratch = slots.const_cmp_scratch.ok_or(WasmGcError::Validation(
        "const-compare specialization needs a scratch slot but none was reserved".into(),
    ))?;

    // Normalize so the `$AverInt` is the LEFT operand of the effective
    // relation. The source spelled `const OP aint` when `const_on_left`,
    // which is equivalent to `aint OP_flipped const`.
    let eff = if const_on_left { flip_cmp(op) } else { op };

    // Stash the operand so both field reads (`$magf`, then `$small` /
    // `$sign`) read the same value without re-emitting the operand
    // expression.
    func.instruction(&Instruction::LocalSet(scratch));

    let block_ty = wasm_encoder::BlockType::Result(ValType::I32);
    // if ($magf == null) → Small
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::StructGet {
        struct_type_index: aint_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::RefIsNull);
    func.instruction(&Instruction::If(block_ty));

    // ── Small: native i64 compare of `$small` against `k` ──
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::StructGet {
        struct_type_index: aint_idx,
        field_index: 0,
    });
    func.instruction(&Instruction::I64Const(k));
    func.instruction(&match eff {
        BinOp::Eq => Instruction::I64Eq,
        BinOp::Neq => Instruction::I64Ne,
        BinOp::Lt => Instruction::I64LtS,
        BinOp::Gt => Instruction::I64GtS,
        BinOp::Lte => Instruction::I64LeS,
        BinOp::Gte => Instruction::I64GeS,
        _ => unreachable!("emit_aint_cmp_const gated to the six comparisons"),
    });

    func.instruction(&Instruction::Else);

    // ── Big: the result is determined by `$sign` alone ──
    // A Big-positive (`$sign > 0`) exceeds every i64 `k`; a Big-negative
    // (`$sign < 0`) is below every i64 `k`; a Big never equals `k`.
    match eff {
        // x < k  ⟺  x is Big-negative  ⟺  $sign < 0
        // x <= k ⟺  same (Big never == k)
        BinOp::Lt | BinOp::Lte => {
            func.instruction(&Instruction::LocalGet(scratch));
            func.instruction(&Instruction::StructGet {
                struct_type_index: aint_idx,
                field_index: 2,
            });
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32LtS);
        }
        // x > k  ⟺  x is Big-positive  ⟺  $sign > 0
        // x >= k ⟺  same (Big never == k)
        BinOp::Gt | BinOp::Gte => {
            func.instruction(&Instruction::LocalGet(scratch));
            func.instruction(&Instruction::StructGet {
                struct_type_index: aint_idx,
                field_index: 2,
            });
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32GtS);
        }
        // A Big never equals an i64 constant.
        BinOp::Eq => {
            func.instruction(&Instruction::I32Const(0));
        }
        BinOp::Neq => {
            func.instruction(&Instruction::I32Const(1));
        }
        _ => unreachable!("emit_aint_cmp_const gated to the six comparisons"),
    }

    func.instruction(&Instruction::End);
    Ok(Some(()))
}

/// Flip a comparison operator across its operands: `k OP x` ≡
/// `x flip(OP) k`. Equality / inequality are symmetric and unchanged.
fn flip_cmp(op: BinOp) -> BinOp {
    match op {
        BinOp::Lt => BinOp::Gt,
        BinOp::Gt => BinOp::Lt,
        BinOp::Lte => BinOp::Gte,
        BinOp::Gte => BinOp::Lte,
        BinOp::Eq => BinOp::Eq,
        BinOp::Neq => BinOp::Neq,
        other => other,
    }
}
