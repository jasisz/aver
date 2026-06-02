//! Built-in call lowering: the custom-inline `Float` / `Int` / `Bool`
//! scalar ops, the `List` and `Vector` families, and the numeric
//! `BinOp` tail. Mirrors the custom-inline arms of `emit_dotted_builtin`
//! and `emit_expr`'s numeric `BinOp` branch.

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

/// Mirror of the native scalar (`Float` / `Int` / `Bool`) arms of
/// `emit_dotted_builtin` (builtins.rs): builtins that lower to a fixed
/// inline wasm instruction sequence over `f64` / `i64` / `i32` values
/// rather than a registered helper call (so the `fn_map.builtins`
/// lookup misses them). Each recurses
/// `emit_mir_expr` on its args — the byte-identical analogue of the
/// oracle's `emit_expr`. `Int.abs` / `Int.min` / `Int.max` re-emit an
/// arg more than once (an `if`/`else` select), exactly as the oracle
/// does; a `None` from any re-emission is a clean whole-fn fallback
/// (`func` is reset by the caller). `Int.mod` is deliberately absent: it
/// builds a `Result<Int,String>` carrier and has a fused form, so it
/// stays on the HIR path.
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
    let i64_block = wasm_encoder::BlockType::Result(ValType::I64);
    match dotted {
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
            arg!(0);
            func.instruction(&Instruction::F64Nearest);
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
        _ => return Ok(MirBuiltinEmit::NotHandled),
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
            emit_args!();
            func.instruction(&Instruction::Call(fn_idx));
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
            // (i64) then fill, so emit fill, then size, then wrap.
            if emit_mir_expr(func, &args[1], slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
            if emit_mir_expr(func, &args[0], slots, ctx)?.is_none() {
                return Ok(MirBuiltinEmit::Fallback);
            }
            func.instruction(&Instruction::I32WrapI64);
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
    e!(index);
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64GeS);
    e!(index);
    func.instruction(&Instruction::I32WrapI64);
    e!(vector);
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(block_ty));
    func.instruction(&Instruction::I32Const(1));
    e!(vector);
    e!(index);
    func.instruction(&Instruction::I32WrapI64);
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
    if mir_arg_uniquely_owned(vector, ctx) {
        e!(index);
        func.instruction(&Instruction::I64Const(0));
        func.instruction(&Instruction::I64GeS);
        e!(index);
        func.instruction(&Instruction::I32WrapI64);
        e!(vector);
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::I32LtU);
        func.instruction(&Instruction::I32And);
        func.instruction(&Instruction::If(block_ty));
        e!(vector);
        e!(index);
        func.instruction(&Instruction::I32WrapI64);
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
    e!(index);
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64GeS);
    e!(index);
    func.instruction(&Instruction::I32WrapI64);
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
    e!(index);
    func.instruction(&Instruction::I32WrapI64);
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
    let l_ty = wasm_type_of(l, ctx.registry)?;
    let r_ty = wasm_type_of(r, ctx.registry)?;
    let operand = if l_ty == Some(ValType::F64) || r_ty == Some(ValType::F64) {
        Some(ValType::F64)
    } else {
        l_ty
    };
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
