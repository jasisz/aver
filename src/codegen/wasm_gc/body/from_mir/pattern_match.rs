//! Pattern-match lowering: `MirExpr::Match` over primitive, carrier
//! (`Option`/`Result`/`List`), and user-variant subjects. Mirrors
//! `emit_match` and its per-shape helpers in `super::super::emit`.

use super::*;

/// Mirror of `emit_match` (emit.rs) for the primitive-subject shapes:
/// `Bool` (a single `if`/`else`) and `Int` (an `i64.eq` cascade). An
/// arm carrying a constructor or list pattern is routed to the carrier
/// / list / variant paths below; a single-arm flat tuple destructure
/// goes to `emit_mir_tuple_match` and any other tuple shape falls back.
/// `String`-subject matches go to
/// `emit_mir_string_match` below (which uses the reserved subject
/// scratch + `__wasmgc_string_eq`); any other subject type falls back.
/// Shapes `emit_match` rejects
/// outright (a `Bool` match without exactly 2 true/false/wildcard arms,
/// an `Int` match without a wildcard, a bind pattern on a primitive
/// subject) return `Ok(None)` here — the `ResolvedExpr` emitter then
/// reproduces `emit_match`'s exact error, so behavior is unchanged.
pub(crate) fn emit_mir_match(
    func: &mut Function,
    m: &MirMatch,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    if m.arms.is_empty() {
        return Err(WasmGcError::Validation("match has no arms".into()));
    }
    // Result/block type — mirror of `emit_match`. The first arm's body
    // type is the match's type (typecheck proved all arms agree); a
    // `Unit` match lowers to `BlockType::Empty` and produces no value.
    let result_ty_str = aver_type_canonical(&m.arms[0].body, ctx.return_type, ctx.registry);
    let block_ty = match aver_to_wasm(&result_ty_str, Some(ctx.registry))? {
        Some(v) => wasm_encoder::BlockType::Result(v),
        None => wasm_encoder::BlockType::Empty,
    };
    let produces = !matches!(block_ty, wasm_encoder::BlockType::Empty);

    // Tuple arms. The single-arm flat destructure `(a, b, …) -> body`
    // (every component a `Bind` or `Wildcard`) goes to `emit_mir_tuple_match`
    // — mirror of `emit_match`'s `arms.len() == 1 && Tuple && items >= 2`
    // branch. A multi-arm tuple-of-constructors match (`(Result.Ok(a),
    // Result.Err(e)) -> …`) goes to `emit_mir_tuple_constructor_match`,
    // which falls back (`Ok(None)`) for any element shape the oracle's
    // `emit_tuple_constructor_match` doesn't support.
    if m.arms
        .iter()
        .any(|a| matches!(a.pattern, MirPattern::Tuple(_)))
    {
        if m.arms.len() == 1
            && let MirPattern::Tuple(items) = &m.arms[0].pattern
            && items.len() >= 2
            && items
                .iter()
                .all(|p| matches!(p, MirPattern::Bind(..) | MirPattern::Wildcard))
        {
            return emit_mir_tuple_match(func, &m.subject, &m.arms[0], slots, ctx);
        }
        return Ok(
            emit_mir_tuple_constructor_match(func, m, block_ty, slots, ctx)?.map(|()| produces),
        );
    }

    // List match (`[] -> …; [head, ..tail] -> …`). `emit_match` checks
    // this before Result/Option, so mirror that order.
    if m.arms
        .iter()
        .any(|a| matches!(a.pattern, MirPattern::EmptyList | MirPattern::Cons { .. }))
    {
        return Ok(emit_mir_list_match(func, m, block_ty, slots, ctx)?.map(|()| produces));
    }

    // Built-in `Result<T,E>` / `Option<T>` matches — tag-based dispatch.
    // `emit_match` checks Result before Option; mirror that order. An
    // Option match whose subject is `Map.get(m, k)` takes `emit_match`'s
    // fused (no-Option-alloc) path — defer that optimization, fall back.
    if m.arms.iter().any(arm_is_mir_result_ctor) {
        return Ok(emit_mir_result_match(func, m, block_ty, slots, ctx)?.map(|()| produces));
    }
    if m.arms.iter().any(arm_is_mir_option_ctor) {
        if subject_is_map_get(&m.subject, ctx) {
            return Ok(
                emit_mir_map_get_match_fused(func, m, block_ty, slots, ctx)?.map(|()| produces)
            );
        }
        return Ok(emit_mir_option_match(func, m, block_ty, slots, ctx)?.map(|()| produces));
    }

    // User-variant (sum type) matches. `emit_match` routes a single
    // `Ctor` arm to `emit_single_variant_match` (direct cast, no test)
    // and a multi-arm match to `emit_variant_dispatch` (a `ref.test`
    // cascade) — mirror that split.
    if m.arms.iter().any(|a| {
        matches!(
            a.pattern,
            MirPattern::Ctor {
                ctor: MirCtor::User(_),
                ..
            }
        )
    }) {
        if m.arms.len() == 1 {
            return Ok(
                emit_mir_single_variant_match(func, &m.subject, &m.arms[0], slots, ctx)?
                    .map(|()| produces),
            );
        }
        return Ok(emit_mir_variant_dispatch(func, m, block_ty, slots, ctx)?.map(|()| produces));
    }

    match aver_type_str_of(&m.subject).trim() {
        "Bool" => {
            // Mirror of `emit_match`'s Bool special-case: a single
            // `if subject { true_body } else { false_body }`.
            if m.arms.len() != 2 {
                return Ok(None);
            }
            let mut true_body: Option<&Spanned<MirExpr>> = None;
            let mut false_body: Option<&Spanned<MirExpr>> = None;
            for arm in &m.arms {
                match &arm.pattern {
                    MirPattern::Literal(Literal::Bool(true)) => true_body = Some(&arm.body),
                    MirPattern::Literal(Literal::Bool(false)) => false_body = Some(&arm.body),
                    MirPattern::Wildcard => {
                        if true_body.is_none() {
                            true_body = Some(&arm.body);
                        } else {
                            false_body = Some(&arm.body);
                        }
                    }
                    _ => return Ok(None),
                }
            }
            let (Some(t), Some(f)) = (true_body, false_body) else {
                return Ok(None);
            };
            if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::If(block_ty));
            if emit_mir_expr(func, t, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::Else);
            if emit_mir_expr(func, f, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::End);
            Ok(Some(produces))
        }
        "Int" => {
            // Mirror of `emit_match`'s Int path + `emit_int_match_cascade`:
            // first-applicable wins, wildcard required.
            let mut wildcard_body: Option<&Spanned<MirExpr>> = None;
            let mut typed_arms: Vec<(i64, &Spanned<MirExpr>)> = Vec::new();
            for arm in &m.arms {
                match &arm.pattern {
                    MirPattern::Literal(Literal::Int(n)) => typed_arms.push((*n, &arm.body)),
                    MirPattern::Wildcard => {
                        // First wildcard wins (source-order semantics).
                        if wildcard_body.is_none() {
                            wildcard_body = Some(&arm.body);
                        }
                    }
                    _ => return Ok(None),
                }
            }
            let Some(wildcard) = wildcard_body else {
                return Ok(None);
            };
            if emit_mir_int_cascade(
                func,
                &m.subject,
                &typed_arms,
                wildcard,
                block_ty,
                slots,
                ctx,
            )?
            .is_none()
            {
                return Ok(None);
            }
            Ok(Some(produces))
        }
        "String" => {
            if emit_mir_string_match(func, m, block_ty, slots, ctx)?.is_none() {
                return Ok(None);
            }
            Ok(Some(produces))
        }
        // Non-primitive subjects (sum/record/etc.) fall back.
        _ => Ok(None),
    }
}

/// Mirror of `emit_tuple_match` (emit.rs): the single-arm flat tuple
/// destructure `match pair { (a, b, …) -> body }`. Stash the subject in
/// the reserved scratch, then for each `Bind` component `struct.get` the
/// field into its resolver-allocated slot (a `Wildcard` component binds
/// nothing, exactly as the oracle skips non-`Ident` items); finally emit
/// the arm body, whose value is the match's value. Variadic arity
/// (any N >= 2). Returns `None` (whole-fn fallback) if the subject type
/// has no registered `Tuple<…>` slot, the scratch is missing, or a
/// subtree is unsupported.
fn emit_mir_tuple_match(
    func: &mut Function,
    subject: &Spanned<MirExpr>,
    arm: &MirMatchArm,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let MirPattern::Tuple(items) = &arm.pattern else {
        return Ok(None);
    };
    let subj_ty = aver_type_str_of(subject);
    let canonical: String = subj_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let tuple_idx = ctx
        .registry
        .tuple_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Tuple match: subject type `{subj_ty}` is not a registered Tuple<A,B>"
        )))?;
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Tuple match needs a subject scratch slot but none was reserved".into(),
    ))?;
    if emit_mir_expr(func, subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));
    // Each `Bind` field is extracted into its slot; the MIR `Bind`
    // slot is seeded from the resolver's `binding_slots`, so it matches
    // the oracle's `arm_slots` entry. An ignored binding (`u16::MAX`)
    // and a `Wildcard` component both extract nothing.
    for (field_idx, pat) in items.iter().enumerate() {
        if let MirPattern::Bind(slot, _) = pat
            && slot.0 != u32::from(u16::MAX)
        {
            func.instruction(&Instruction::LocalGet(scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(tuple_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: tuple_idx,
                field_index: field_idx as u32,
            });
            func.instruction(&Instruction::LocalSet(slot.0));
        }
    }
    // The arm body's value is the match's value, left on the stack.
    let Some(produces) = emit_mir_expr(func, &arm.body, slots, ctx)? else {
        return Ok(None);
    };
    Ok(Some(produces))
}

/// Mirror of `emit_tuple_constructor_match` + `emit_tuple_constructor_arm_cascade`
/// (emit.rs): a multi-arm match on a tuple whose elements carry built-in
/// `Result` constructors — e.g. `match r { (Result.Ok(a), Result.Err(e))
/// -> …; _ -> … }` — lowered to a nested `if`/`else` cascade. Each arm
/// AND's together a per-element `Result` tag test; on the matching branch
/// it extracts each element's `Ok`/`Err` payload into its binding slot
/// and emits the body; the failure branch recurses into the remaining
/// arms; a trailing `Wildcard` / `Bind` arm closes the cascade. A
/// pre-pass rejects (returns `Ok(None)` → whole-fn fallback) any shape
/// the oracle's cascade doesn't support — a non-`Result` element ctor, an
/// element with no registered `Result<T,E>` slot, an arity mismatch, or a
/// non-tuple/bind/wildcard arm — so the resolved-HIR emitter handles
/// those byte-identically.
fn emit_mir_tuple_constructor_match(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let subject_ty = aver_type_str_of(&m.subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let Some(tuple_idx) = ctx.registry.tuple_type_idx(&canonical) else {
        return Ok(None);
    };
    let Some(elems) = TypeRegistry::tuple_elements(&canonical) else {
        return Ok(None);
    };
    let elems: Vec<String> = elems.into_iter().map(|s| s.to_string()).collect();

    // Pre-pass: only emit when every arm is a shape the cascade fully
    // supports, so the emit pass can't half-write then bail. Require at
    // least one tuple arm carrying a built-in `Result` ctor (otherwise
    // this isn't the constructor-cascade shape and the caller's other
    // dispatch arms own it).
    let mut any_result_ctor = false;
    for arm in &m.arms {
        match &arm.pattern {
            MirPattern::Wildcard | MirPattern::Bind(..) => {}
            MirPattern::Tuple(items) => {
                if items.len() != elems.len() {
                    return Ok(None);
                }
                for (i, pat) in items.iter().enumerate() {
                    match pat {
                        MirPattern::Bind(..) | MirPattern::Wildcard => {}
                        MirPattern::Ctor {
                            ctor: MirCtor::Builtin(BuiltinCtor::ResultOk | BuiltinCtor::ResultErr),
                            ..
                        } => {
                            any_result_ctor = true;
                            let elem_canonical: String =
                                elems[i].chars().filter(|c| !c.is_whitespace()).collect();
                            if ctx.registry.result_type_idx(&elem_canonical).is_none() {
                                return Ok(None);
                            }
                        }
                        _ => return Ok(None),
                    }
                }
            }
            _ => return Ok(None),
        }
    }
    if !any_result_ctor {
        return Ok(None);
    }

    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "multi-arm tuple match needs a subject scratch slot but none was reserved".into(),
    ))?;
    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));
    emit_mir_tuple_constructor_arm_cascade(
        func, scratch, tuple_idx, &elems, &m.arms, block_ty, slots, ctx,
    )
}

/// The recursive cascade body — mirror of `emit_tuple_constructor_arm_cascade`.
/// Reads each binding slot straight off the `MirPattern` node (`Bind`'s
/// `LocalId`, the `Ctor`'s `bindings`), which `lower.rs` seeded from the
/// resolver's `binding_slots` in the same preorder the oracle walks — so
/// no parallel slot cursor is needed.
#[allow(clippy::too_many_arguments)]
fn emit_mir_tuple_constructor_arm_cascade(
    func: &mut Function,
    scratch: u32,
    tuple_idx: u32,
    elems: &[String],
    arms: &[MirMatchArm],
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let Some(arm) = arms.first() else {
        // Exhaustiveness is proven by the typechecker; an empty tail is
        // unreachable, emitted as a stack-polymorphic `unreachable`.
        func.instruction(&Instruction::Unreachable);
        return Ok(Some(()));
    };
    match &arm.pattern {
        MirPattern::Wildcard => {
            if emit_mir_expr(func, &arm.body, slots, ctx)?.is_none() {
                return Ok(None);
            }
        }
        MirPattern::Bind(slot, _) => {
            if slot.0 != u32::from(u16::MAX) {
                func.instruction(&Instruction::LocalGet(scratch));
                func.instruction(&Instruction::LocalSet(slot.0));
            }
            if emit_mir_expr(func, &arm.body, slots, ctx)?.is_none() {
                return Ok(None);
            }
        }
        MirPattern::Tuple(items) => {
            // Verdict: AND together each element's `Result` tag test.
            let mut tests_emitted = 0u32;
            for (i, pat) in items.iter().enumerate() {
                if let MirPattern::Ctor {
                    ctor: MirCtor::Builtin(bc),
                    ..
                } = pat
                    && matches!(bc, BuiltinCtor::ResultOk | BuiltinCtor::ResultErr)
                {
                    let elem_canonical: String =
                        elems[i].chars().filter(|c| !c.is_whitespace()).collect();
                    let res_idx = ctx.registry.result_type_idx(&elem_canonical).ok_or(
                        WasmGcError::Validation(
                            "tuple-of-constructors match: element is not a registered Result"
                                .into(),
                        ),
                    )?;
                    let expected_tag: i32 = if matches!(bc, BuiltinCtor::ResultOk) {
                        1
                    } else {
                        0
                    };
                    func.instruction(&Instruction::LocalGet(scratch));
                    func.instruction(&Instruction::RefCastNonNull(
                        wasm_encoder::HeapType::Concrete(tuple_idx),
                    ));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: tuple_idx,
                        field_index: i as u32,
                    });
                    func.instruction(&Instruction::RefCastNonNull(
                        wasm_encoder::HeapType::Concrete(res_idx),
                    ));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: res_idx,
                        field_index: 0,
                    });
                    func.instruction(&Instruction::I32Const(expected_tag));
                    func.instruction(&Instruction::I32Eq);
                    if tests_emitted > 0 {
                        func.instruction(&Instruction::I32And);
                    }
                    tests_emitted += 1;
                }
            }
            if tests_emitted == 0 {
                func.instruction(&Instruction::I32Const(1));
            }
            func.instruction(&Instruction::If(block_ty));
            // Bindings: a direct `Bind` element captures the whole tuple
            // field; a `Result` ctor element captures its payload.
            for (i, pat) in items.iter().enumerate() {
                match pat {
                    MirPattern::Bind(slot, _) if slot.0 != u32::from(u16::MAX) => {
                        func.instruction(&Instruction::LocalGet(scratch));
                        func.instruction(&Instruction::RefCastNonNull(
                            wasm_encoder::HeapType::Concrete(tuple_idx),
                        ));
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: tuple_idx,
                            field_index: i as u32,
                        });
                        func.instruction(&Instruction::LocalSet(slot.0));
                    }
                    MirPattern::Ctor {
                        ctor: MirCtor::Builtin(bc),
                        bindings,
                        ..
                    } if matches!(bc, BuiltinCtor::ResultOk | BuiltinCtor::ResultErr) => {
                        let elem_canonical: String =
                            elems[i].chars().filter(|c| !c.is_whitespace()).collect();
                        let res_idx =
                            ctx.registry
                                .result_type_idx(&elem_canonical)
                                .ok_or(WasmGcError::Validation(
                                "tuple-of-constructors match: element is not a registered Result"
                                    .into(),
                            ))?;
                        let payload_field: u32 = if matches!(bc, BuiltinCtor::ResultOk) {
                            1
                        } else {
                            2
                        };
                        for binding in bindings {
                            if binding.0 == u32::from(u16::MAX) {
                                continue;
                            }
                            func.instruction(&Instruction::LocalGet(scratch));
                            func.instruction(&Instruction::RefCastNonNull(
                                wasm_encoder::HeapType::Concrete(tuple_idx),
                            ));
                            func.instruction(&Instruction::StructGet {
                                struct_type_index: tuple_idx,
                                field_index: i as u32,
                            });
                            func.instruction(&Instruction::RefCastNonNull(
                                wasm_encoder::HeapType::Concrete(res_idx),
                            ));
                            func.instruction(&Instruction::StructGet {
                                struct_type_index: res_idx,
                                field_index: payload_field,
                            });
                            func.instruction(&Instruction::LocalSet(binding.0));
                        }
                    }
                    _ => {}
                }
            }
            if emit_mir_expr(func, &arm.body, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::Else);
            if emit_mir_tuple_constructor_arm_cascade(
                func,
                scratch,
                tuple_idx,
                elems,
                &arms[1..],
                block_ty,
                slots,
                ctx,
            )?
            .is_none()
            {
                return Ok(None);
            }
            func.instruction(&Instruction::End);
        }
        _ => return Ok(None),
    }
    Ok(Some(()))
}

/// Mirror of `emit_map_get_match_fused` (emit.rs): the fused
/// `match Map.get(m, k) { Option.Some(v) -> …; Option.None -> … }`,
/// lowered through the per-(K,V) `get_pair` helper (multi-result
/// `(i32 found, V value)`) without ever allocating an `Option<V>`. The
/// value is popped into the `Some` binding slot (or dropped when there's
/// no binding) **before** the branch — wasm needs a balanced stack
/// across the `if`/`else` — then `found` selects the arm. Returns
/// `Ok(None)` (whole-fn fallback) if the subject isn't a 2-arg
/// `Map.get`, the map has no registered helpers, or a Some/None arm is
/// missing.
fn emit_mir_map_get_match_fused(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    // Subject is `Map.get(map, key)` — `subject_is_map_get` already
    // proved the builtin name + arity at the call site.
    let MirExpr::Call(call) = &m.subject.node else {
        return Ok(None);
    };
    let call = &call.node;
    if call.args.len() != 2 {
        return Ok(None);
    }
    let map = &call.args[0];
    let key = &call.args[1];

    let map_aver = aver_type_str_of(map);
    let canonical: String = map_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let Some(helpers) = ctx.fn_map.map_helpers_lookup(&canonical) else {
        return Ok(None);
    };
    let get_pair = helpers.get_pair;

    // Locate the Some / None arms (a wildcard is the None catch-all; a
    // second wildcard fills Some) — mirror of the oracle's arm scan.
    let mut some_arm: Option<&MirMatchArm> = None;
    let mut none_arm: Option<&MirMatchArm> = None;
    for arm in &m.arms {
        match &arm.pattern {
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
                ..
            } => some_arm = Some(arm),
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
                ..
            } => none_arm = Some(arm),
            MirPattern::Wildcard => {
                if none_arm.is_none() {
                    none_arm = Some(arm);
                } else if some_arm.is_none() {
                    some_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let (Some(some_arm), Some(none_arm)) = (some_arm, none_arm) else {
        return Ok(None);
    };

    if emit_mir_expr(func, map, slots, ctx)?.is_none() {
        return Ok(None);
    }
    if emit_mir_expr(func, key, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::Call(get_pair));
    // Stack: [..., found(i32), value(V)]. Pop V into the Some binding
    // (or drop it) unconditionally so the stack is balanced across the
    // branch; the value is harmlessly dead on the None side.
    match ctor_arm_binding_slot(some_arm) {
        Some(slot) => {
            func.instruction(&Instruction::LocalSet(slot));
        }
        None => {
            func.instruction(&Instruction::Drop);
        }
    }
    func.instruction(&Instruction::If(block_ty));
    if emit_mir_expr(func, &some_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::Else);
    if emit_mir_expr(func, &none_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::End);
    Ok(Some(()))
}

/// Mirror of `emit_string_match` (emit.rs): stash the subject in the
/// reserved `(ref null eq)` scratch, then a cascade of
/// `if __wasmgc_string_eq(subject, "lit") { body } else { … }` with the
/// first non-literal arm (typically `_`) as the innermost default.
/// Returns `None` (whole-fn fallback) if any subtree is unsupported or
/// the shape lacks the scratch / default the `ResolvedExpr` emitter
/// also requires.
pub(crate) fn emit_mir_string_match(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "String match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let eq_idx = ctx
        .fn_map
        .builtins
        .get("__wasmgc_string_eq")
        .copied()
        .ok_or(WasmGcError::Validation(
            "String match: __wasmgc_string_eq builtin wasn't registered".into(),
        ))?;
    let s_idx = ctx
        .registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "String match needs the String type slot allocated".into(),
        ))?;

    // Stash the subject; read once per arm (cast `(ref null eq)` back to
    // `(ref null $string)` for `__wasmgc_string_eq`'s param shape).
    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));

    // Literal-string arms in source order, then the first non-literal
    // arm as the single default (mirror of `emit_string_match`).
    let mut literal_arms: Vec<(&str, &Spanned<MirExpr>)> = Vec::new();
    let mut default_body: Option<&Spanned<MirExpr>> = None;
    for arm in &m.arms {
        if let MirPattern::Literal(Literal::Str(s)) = &arm.pattern {
            literal_arms.push((s.as_str(), &arm.body));
        } else if default_body.is_none() {
            default_body = Some(&arm.body);
        }
    }
    let Some(default_body) = default_body else {
        // `emit_string_match` raises a Validation error here; fall back
        // so the `ResolvedExpr` emitter reproduces it.
        return Ok(None);
    };

    let mut ends_to_close = 0usize;
    for (lit, body) in &literal_arms {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNullable(
            wasm_encoder::HeapType::Concrete(s_idx),
        ));
        emit_string_literal_bytes(func, lit.as_bytes(), ctx)?;
        func.instruction(&Instruction::Call(eq_idx));
        func.instruction(&Instruction::If(block_ty));
        if emit_mir_expr(func, body, slots, ctx)?.is_none() {
            return Ok(None);
        }
        func.instruction(&Instruction::Else);
        ends_to_close += 1;
    }
    if emit_mir_expr(func, default_body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    for _ in 0..ends_to_close {
        func.instruction(&Instruction::End);
    }
    Ok(Some(()))
}

/// Mirror of `emit_int_match_cascade` (emit.rs): `subject == lit ?
/// body : <rest>`, recomputing the subject per arm (no scratch slot).
/// Returns `None` if any subtree falls outside the supported subset.
pub(crate) fn emit_mir_int_cascade(
    func: &mut Function,
    subject: &Spanned<MirExpr>,
    typed_arms: &[(i64, &Spanned<MirExpr>)],
    wildcard: &Spanned<MirExpr>,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let Some(((pat_lit, body), rest)) = typed_arms.split_first() else {
        // No typed arms left — emit the wildcard body.
        if emit_mir_expr(func, wildcard, slots, ctx)?.is_none() {
            return Ok(None);
        }
        return Ok(Some(()));
    };
    if emit_mir_expr(func, subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::I64Const(*pat_lit));
    func.instruction(&Instruction::I64Eq);
    func.instruction(&Instruction::If(block_ty));
    if emit_mir_expr(func, body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::Else);
    if emit_mir_int_cascade(func, subject, rest, wildcard, block_ty, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::End);
    Ok(Some(()))
}

/// `true` for an arm whose pattern is a built-in `Result.Ok` /
/// `Result.Err` constructor (mirror of `arm_is_result_pattern_resolved`).
pub(crate) fn arm_is_mir_result_ctor(arm: &MirMatchArm) -> bool {
    matches!(
        &arm.pattern,
        MirPattern::Ctor {
            ctor: MirCtor::Builtin(BuiltinCtor::ResultOk | BuiltinCtor::ResultErr),
            ..
        }
    )
}

/// `true` for an arm whose pattern is a built-in `Option.Some` /
/// `Option.None` constructor (mirror of `arm_is_option_pattern_resolved`).
pub(crate) fn arm_is_mir_option_ctor(arm: &MirMatchArm) -> bool {
    matches!(
        &arm.pattern,
        MirPattern::Ctor {
            ctor: MirCtor::Builtin(BuiltinCtor::OptionSome | BuiltinCtor::OptionNone),
            ..
        }
    )
}

/// `true` when `subject` is `Map.get(m, k)` — the fused-match shape
/// `emit_match` lowers without allocating an `Option<V>`. This shape
/// falls back so the plain Option-match emit can't diverge from
/// `emit_map_get_match_fused`.
pub(crate) fn subject_is_map_get(subject: &Spanned<MirExpr>, ctx: &EmitCtx<'_>) -> bool {
    if let MirExpr::Call(call) = &subject.node
        && let MirCallee::Builtin(id) = call.node.callee
        && let Some(name) = ctx.mir_builtins.and_then(|names| names.get(id.0 as usize))
    {
        return name == "Map.get" && call.node.args.len() == 2;
    }
    false
}

/// The payload-binding wasm slot for a constructor-pattern arm, mirror
/// of `emit_option_match` / `emit_result_match`'s
/// `arm.binding_slots.get().first()` + `slot != u16::MAX` guard. The
/// MIR `bindings` are seeded from the resolver's `binding_slots`
/// (`lower.rs::take_pattern_bindings`), so the slot is identical; an
/// ignored / absent binding is `u16::MAX` → `None` (no extraction).
pub(crate) fn ctor_arm_binding_slot(arm: &MirMatchArm) -> Option<u32> {
    if let MirPattern::Ctor { bindings, .. } = &arm.pattern
        && let Some(slot) = bindings.first()
        && slot.0 != u32::from(u16::MAX)
    {
        return Some(slot.0);
    }
    None
}

/// Mirror of `emit_option_match` (emit.rs): stash the subject, test the
/// tag field (struct field 0 == 1 ⇒ `Some`), extract the payload
/// (field 1) into the `Some` arm's binding slot when present, then the
/// `if`/`else` bodies. The wildcard arm is the `None` catch-all.
pub(crate) fn emit_mir_option_match(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Option match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(&m.subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let opt_idx = ctx
        .registry
        .option_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option match: subject type `{subject_ty}` is not a registered Option<T>"
        )))?;

    // Locate Some / None arms; a wildcard is the None catch-all (then
    // Some) — same convention as `emit_option_match`.
    let mut some_arm: Option<&MirMatchArm> = None;
    let mut none_arm: Option<&MirMatchArm> = None;
    for arm in &m.arms {
        match &arm.pattern {
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
                ..
            } => some_arm = Some(arm),
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
                ..
            } => none_arm = Some(arm),
            MirPattern::Wildcard => {
                if none_arm.is_none() {
                    none_arm = Some(arm);
                } else if some_arm.is_none() {
                    some_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let (Some(some_arm), Some(none_arm)) = (some_arm, none_arm) else {
        // `emit_option_match` raises a Validation error here; fall back.
        return Ok(None);
    };

    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
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

    if let Some(slot) = ctor_arm_binding_slot(some_arm) {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(opt_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: opt_idx,
            field_index: 1,
        });
        func.instruction(&Instruction::LocalSet(slot));
    }
    if emit_mir_expr(func, &some_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::Else);
    if emit_mir_expr(func, &none_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::End);
    Ok(Some(()))
}

/// Mirror of `emit_result_match` (emit.rs): tag field 0 == 1 ⇒ `Ok`
/// (payload field 1), else `Err` (payload field 2). Each arm extracts
/// its payload into its binding slot when present. A wildcard is the
/// `Err` catch-all (then `Ok`).
pub(crate) fn emit_mir_result_match(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Result match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(&m.subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Result match: subject type `{subject_ty}` is not a registered Result<T,E>"
        )))?;

    let mut ok_arm: Option<&MirMatchArm> = None;
    let mut err_arm: Option<&MirMatchArm> = None;
    for arm in &m.arms {
        match &arm.pattern {
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::ResultOk),
                ..
            } => ok_arm = Some(arm),
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::ResultErr),
                ..
            } => err_arm = Some(arm),
            MirPattern::Wildcard => {
                if err_arm.is_none() {
                    err_arm = Some(arm);
                } else if ok_arm.is_none() {
                    ok_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let (Some(ok_arm), Some(err_arm)) = (ok_arm, err_arm) else {
        return Ok(None);
    };

    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
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

    if let Some(slot) = ctor_arm_binding_slot(ok_arm) {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: res_idx,
            field_index: 1,
        });
        func.instruction(&Instruction::LocalSet(slot));
    }
    if emit_mir_expr(func, &ok_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::Else);
    if let Some(slot) = ctor_arm_binding_slot(err_arm) {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: res_idx,
            field_index: 2,
        });
        func.instruction(&Instruction::LocalSet(slot));
    }
    if emit_mir_expr(func, &err_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::End);
    Ok(Some(()))
}

/// Mirror of `emit_list_match` (emit.rs): a `ref.is_null` tag test —
/// null ⇒ the `[]` arm, else the `[head, ..tail]` arm, which extracts
/// head (struct field 0) and tail (field 1) into the `Cons` pattern's
/// binding slots (each guarded by the `u16::MAX` sentinel) before
/// emitting the body. A wildcard is the empty (then cons) catch-all.
pub(crate) fn emit_mir_list_match(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "List match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(&m.subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let list_idx = ctx
        .registry
        .list_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List match: subject type `{subject_ty}` is not a registered List<T>"
        )))?;

    let mut empty_arm: Option<&MirMatchArm> = None;
    let mut cons_arm: Option<&MirMatchArm> = None;
    for arm in &m.arms {
        match &arm.pattern {
            MirPattern::EmptyList => empty_arm = Some(arm),
            MirPattern::Cons { .. } => cons_arm = Some(arm),
            MirPattern::Wildcard => {
                if empty_arm.is_none() {
                    empty_arm = Some(arm);
                } else if cons_arm.is_none() {
                    cons_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let (Some(empty_arm), Some(cons_arm)) = (empty_arm, cons_arm) else {
        return Ok(None);
    };

    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));

    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefIsNull);
    func.instruction(&Instruction::If(block_ty));
    if emit_mir_expr(func, &empty_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::Else);
    if let MirPattern::Cons { head, tail, .. } = &cons_arm.pattern {
        if head.0 != u32::from(u16::MAX) {
            func.instruction(&Instruction::LocalGet(scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(list_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: list_idx,
                field_index: 0,
            });
            func.instruction(&Instruction::LocalSet(head.0));
        }
        if tail.0 != u32::from(u16::MAX) {
            func.instruction(&Instruction::LocalGet(scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(list_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: list_idx,
                field_index: 1,
            });
            func.instruction(&Instruction::LocalSet(tail.0));
        }
    }
    if emit_mir_expr(func, &cons_arm.body, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::End);
    Ok(Some(()))
}

/// Resolve a `MirCtor::User(CtorId)` to its registry `VariantInfo`,
/// mirroring `emit_match`'s `ctor_dotted_name` + `variant_in` lookup:
/// the parent type name comes from the ctor's owning type's `key.name`,
/// the bare variant name from the ctor entry; the registry is keyed by
/// `(parent, bare)` (with a bare-name fallback for non-colliding types).
pub(crate) fn mir_user_variant_info<'a>(
    ctor_id: CtorId,
    ctx: &'a EmitCtx<'_>,
) -> Result<&'a VariantInfo, WasmGcError> {
    let ctor_entry = ctx.symbol_table.ctor_entry(ctor_id);
    let bare = ctor_entry.name.as_str();
    let parent = ctx
        .symbol_table
        .type_entry(ctor_entry.owning_type)
        .key
        .name
        .clone();
    ctx.registry
        .variant_in(&parent, bare)
        .or_else(|| ctx.registry.variant(bare))
        .ok_or(WasmGcError::Validation(format!(
            "unknown variant `{parent}.{bare}` in match"
        )))
}

/// Emit a covered arm body, returning `None` if the body falls outside
/// the supported subset (propagated as a whole-fn fallback).
pub(crate) fn emit_mir_arm_body_value(
    func: &mut Function,
    body: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    Ok(emit_mir_expr(func, body, slots, ctx)?.map(|_| ()))
}

/// Mirror of `emit_single_variant_match` (emit.rs): an irrefutable
/// single-arm sum-type destructure (the typechecker proved it's the
/// only variant) — newtype shapes bind the subject directly, nullary
/// constructors just drop it, single-binding uses an inline
/// `ref.cast` + `struct.get`, and multi-binding stashes the cast
/// subject in the reserved scratch and extracts each field. The MIR
/// `Ctor` bindings are the resolver's `binding_slots` (`u16::MAX`
/// sentinel for `_`), so every `local.set` matches byte-for-byte.
pub(crate) fn emit_mir_single_variant_match(
    func: &mut Function,
    subject: &Spanned<MirExpr>,
    arm: &MirMatchArm,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let MirPattern::Ctor {
        ctor: MirCtor::User(ctor_id),
        bindings,
        ..
    } = &arm.pattern
    else {
        return Ok(None);
    };
    let info = mir_user_variant_info(*ctor_id, ctx)?;
    const NO_SLOT: u32 = u16::MAX as u32;

    // Newtype: single-variant sum of a single primitive — bind the
    // subject directly, no cast / struct.get.
    if ctx.registry.newtype_underlying(&info.parent).is_some() && bindings.len() == 1 {
        let slot = bindings[0].0;
        if emit_mir_expr(func, subject, slots, ctx)?.is_none() {
            return Ok(None);
        }
        if slot != NO_SLOT {
            func.instruction(&Instruction::LocalSet(slot));
        } else {
            func.instruction(&Instruction::Drop);
        }
        return emit_mir_arm_body_value(func, &arm.body, slots, ctx);
    }

    let variant_idx = info.type_idx;
    let cast_ty = wasm_encoder::HeapType::Concrete(variant_idx);

    if bindings.is_empty() {
        // Nullary — evaluate the subject for effects, drop, emit body.
        if emit_mir_expr(func, subject, slots, ctx)?.is_none() {
            return Ok(None);
        }
        func.instruction(&Instruction::Drop);
        return emit_mir_arm_body_value(func, &arm.body, slots, ctx);
    }

    if emit_mir_expr(func, subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::RefCastNonNull(cast_ty));

    if bindings.len() == 1 {
        // Single binding — the cast ref is on the stack; `struct.get`
        // field 0 and bind (or drop for `_`).
        let slot = bindings[0].0;
        func.instruction(&Instruction::StructGet {
            struct_type_index: variant_idx,
            field_index: 0,
        });
        if slot != NO_SLOT {
            func.instruction(&Instruction::LocalSet(slot));
        } else {
            func.instruction(&Instruction::Drop);
        }
        return emit_mir_arm_body_value(func, &arm.body, slots, ctx);
    }

    // Multi-binding — stash the cast subject, re-read + re-cast per
    // field. The scratch is `(ref null eq)`, so re-cast on each read.
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "multi-binding variant pattern needs subject_scratch but none was reserved".into(),
    ))?;
    func.instruction(&Instruction::LocalSet(scratch));
    for (i, slot) in bindings.iter().enumerate() {
        if slot.0 == NO_SLOT {
            continue;
        }
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(cast_ty));
        func.instruction(&Instruction::StructGet {
            struct_type_index: variant_idx,
            field_index: i as u32,
        });
        func.instruction(&Instruction::LocalSet(slot.0));
    }
    emit_mir_arm_body_value(func, &arm.body, slots, ctx)
}

/// Mirror of `emit_variant_dispatch` (emit.rs): stash the subject in
/// the reserved scratch, then a `ref.test` cascade over the arms.
pub(crate) fn emit_mir_variant_dispatch(
    func: &mut Function,
    m: &MirMatch,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "multi-arm variant match needs a subject scratch slot but none was reserved".into(),
    ))?;
    if emit_mir_expr(func, &m.subject, slots, ctx)?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::LocalSet(scratch));
    emit_mir_variant_arm_cascade(func, &m.arms, block_ty, scratch, slots, ctx)
}

/// Mirror of `emit_variant_arm_cascade` (emit.rs): one arm left → the
/// default (no test); else `ref.test` the first arm's variant, emit its
/// body on match, recurse on the rest in the `else`.
pub(crate) fn emit_mir_variant_arm_cascade(
    func: &mut Function,
    arms: &[MirMatchArm],
    block_ty: wasm_encoder::BlockType,
    subject_scratch: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if arms.is_empty() {
        // Exhaustiveness already proven; reaching here means no arms —
        // emit `unreachable` so the validator treats it as polymorphic.
        func.instruction(&Instruction::Unreachable);
        return Ok(Some(()));
    }
    if arms.len() == 1 {
        return emit_mir_arm_body(func, &arms[0], subject_scratch, slots, ctx);
    }
    let arm = &arms[0];
    match &arm.pattern {
        MirPattern::Ctor {
            ctor: MirCtor::User(ctor_id),
            ..
        } => {
            let info = mir_user_variant_info(*ctor_id, ctx)?;
            func.instruction(&Instruction::LocalGet(subject_scratch));
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(info.type_idx),
            ));
            func.instruction(&Instruction::If(block_ty));
            if emit_mir_arm_body(func, arm, subject_scratch, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::Else);
            if emit_mir_variant_arm_cascade(
                func,
                &arms[1..],
                block_ty,
                subject_scratch,
                slots,
                ctx,
            )?
            .is_none()
            {
                return Ok(None);
            }
            func.instruction(&Instruction::End);
            Ok(Some(()))
        }
        MirPattern::Wildcard => emit_mir_arm_body(func, arm, subject_scratch, slots, ctx),
        // A non-Ctor / non-Wildcard arm here is `emit_match`'s
        // Unimplemented case — fall back.
        _ => Ok(None),
    }
}

/// Mirror of `emit_arm_body` (emit.rs): extract a `Ctor` arm's fields
/// from the scratch-held subject (newtype binds the scratch directly),
/// then emit the body; a wildcard arm just emits its body.
pub(crate) fn emit_mir_arm_body(
    func: &mut Function,
    arm: &MirMatchArm,
    subject_scratch: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    if let MirPattern::Ctor {
        ctor: MirCtor::User(ctor_id),
        bindings,
        ..
    } = &arm.pattern
    {
        let info = mir_user_variant_info(*ctor_id, ctx)?;
        const NO_SLOT: u32 = u16::MAX as u32;
        if ctx.registry.newtype_underlying(&info.parent).is_some() && bindings.len() == 1 {
            let slot = bindings[0].0;
            if slot != NO_SLOT {
                func.instruction(&Instruction::LocalGet(subject_scratch));
                func.instruction(&Instruction::LocalSet(slot));
            }
            return emit_mir_arm_body_value(func, &arm.body, slots, ctx);
        }
        for (i, slot) in bindings.iter().enumerate() {
            if slot.0 == NO_SLOT {
                continue;
            }
            func.instruction(&Instruction::LocalGet(subject_scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(info.type_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: info.type_idx,
                field_index: i as u32,
            });
            func.instruction(&Instruction::LocalSet(slot.0));
        }
        return emit_mir_arm_body_value(func, &arm.body, slots, ctx);
    }
    // Wildcard / non-pattern arm — just emit the body.
    emit_mir_arm_body_value(func, &arm.body, slots, ctx)
}
