//! Generic expression / match / constructor emit. Split out of
//! `body.rs` — pure code movement, no logic changes.

use wasm_encoder::{Function, Instruction, ValType};

use crate::ast::{BinOp, Expr, Literal, MatchArm, Pattern, Spanned};

use super::super::WasmGcError;
use super::super::types::{TypeRegistry, aver_to_wasm};
use super::builtins::{emit_dotted_builtin, emit_interpolated_str};
use super::infer::{
    arm_is_option_pattern, arm_is_result_pattern, aver_type_canonical, aver_type_str_of,
    wasm_type_of,
};
use super::{EmitCtx, SlotTable};

/// Emit a "default value" of the given Aver primitive / ref type onto
/// the wasm stack. Used by `Option.None` constructor to satisfy
/// `struct.new`'s requirement that every field has an initial value
/// — the value field of a None-tagged Option is never read by
/// well-typed Aver code (pattern match dispatches on tag first).
pub(super) fn emit_default_value(
    func: &mut Function,
    aver_ty: &str,
    registry: &TypeRegistry,
) -> Result<(), WasmGcError> {
    match aver_ty.trim() {
        "Int" => {
            func.instruction(&Instruction::I64Const(0));
            Ok(())
        }
        "Float" => {
            func.instruction(&Instruction::F64Const(0.0));
            Ok(())
        }
        "Bool" => {
            func.instruction(&Instruction::I32Const(0));
            Ok(())
        }
        "Unit" => {
            // Unit lowers to a placeholder i32 slot when it appears as a
            // payload position inside Result<Unit, E> / Result<T, Unit>
            // (struct shape stays uniform; the slot is never read).
            func.instruction(&Instruction::I32Const(0));
            Ok(())
        }
        other => {
            // Single-field records flattened to a primitive (newtype
            // optimisation) need the matching primitive zero, not a
            // ref.null. Ref types emit `ref.null $T`. The exact wasm
            // shape comes from `aver_to_wasm`.
            let val = aver_to_wasm(other, Some(registry))?;
            match val {
                Some(ValType::Ref(rt)) => {
                    func.instruction(&Instruction::RefNull(rt.heap_type));
                    Ok(())
                }
                Some(ValType::I64) => {
                    func.instruction(&Instruction::I64Const(0));
                    Ok(())
                }
                Some(ValType::F64) => {
                    func.instruction(&Instruction::F64Const(0.0));
                    Ok(())
                }
                Some(ValType::I32) => {
                    func.instruction(&Instruction::I32Const(0));
                    Ok(())
                }
                Some(other_ty) => Err(WasmGcError::Validation(format!(
                    "Option.None default for `{other}` resolved to {other_ty:?} — no default emitter for that wasm type"
                ))),
                None => Err(WasmGcError::Validation(format!(
                    "Option.None over `{other}` has no wasm representation"
                ))),
            }
        }
    }
}

/// Emit an `Option<T>` constructor:
/// - `Option.Some(v)` → `i32.const 1; emit v; struct.new $option_T`.
/// - `Option.None`     → `i32.const 0; default<T>; struct.new $option_T`.
///
/// `payload` is `Some(v)` for the wrapper case, `None` for the nullary
/// None. `t_aver_hint` provides the `T` in `Option<T>` when payload is
/// absent — typically the enclosing fn's return type for an
/// `Option.None` written as a value, or the inferred subject type when
/// emitted from a match arm.
pub(super) fn emit_option_constructor(
    func: &mut Function,
    payload: Option<&Spanned<Expr>>,
    t_aver_hint: Option<&str>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    // Resolve T. From payload type if present, else from the hint.
    let t_aver: String = match payload {
        Some(p) => aver_type_str_of(p),
        None => t_aver_hint
            .ok_or(WasmGcError::Validation(
                "Option.None without context — cannot infer the T in Option<T>. \
                 Add a type annotation on the surrounding binding or fn return."
                    .into(),
            ))?
            .to_string(),
    };
    let canonical = if t_aver.starts_with("Option<") {
        // Already an Option<T>; payload type WAS the wrapped Option.
        // This shouldn't happen for Some(v) since v is the inner T,
        // but guard against accidental double-wrapping.
        t_aver.clone()
    } else {
        format!("Option<{}>", t_aver)
    };
    let opt_idx = ctx
        .registry
        .option_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option constructor: instantiation `{canonical}` was not registered. \
             Discovery should have walked fn signatures + bodies."
        )))?;
    let inner_ty = TypeRegistry::option_element_type(&canonical).ok_or(WasmGcError::Validation(
        format!("Option canonical `{canonical}` has no element type"),
    ))?;

    match payload {
        Some(p) => {
            func.instruction(&Instruction::I32Const(1));
            emit_expr(func, p, slots, ctx)?;
        }
        None => {
            func.instruction(&Instruction::I32Const(0));
            emit_default_value(func, inner_ty, ctx.registry)?;
        }
    }
    func.instruction(&Instruction::StructNew(opt_idx));
    Ok(())
}

/// Emit instructions for `expr`. Caller manages stack effect — this
/// function pushes one value (or zero for `Unit`) for every call.
pub(super) fn emit_expr(
    func: &mut Function,
    expr: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) => {
            func.instruction(&Instruction::I64Const(*n));
        }
        Expr::Literal(Literal::Float(f)) => {
            func.instruction(&Instruction::F64Const(*f));
        }
        Expr::Literal(Literal::Bool(b)) => {
            func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
        }
        Expr::Literal(Literal::Unit) => {}
        Expr::Literal(Literal::Str(s)) => {
            // String literal → passive data segment; emit
            // `array.new_data $string $seg` with offset=0, size=len.
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
        }
        Expr::InterpolatedStr(parts) => {
            emit_interpolated_str(func, parts, slots, ctx)?;
        }
        Expr::List(items) => {
            emit_list_literal(func, expr, items, slots, ctx)?;
        }
        Expr::MapLiteral(entries) => {
            emit_map_literal(func, entries, slots, ctx)?;
        }
        Expr::Tuple(items) => {
            emit_tuple_literal(func, items, slots, ctx)?;
        }
        Expr::IndependentProduct(items, unwrap) => {
            // wasm-gc has no parallelism (no `std::thread::scope`
            // analogue), so both `(a, b, c)!` and `(a, b, c)?!` lower
            // as sequential evaluation. `!` (unwrap=false) is just a
            // tuple; `?!` (unwrap=true) requires every element to be
            // `Result<T_i, E>` — unwrap each Ok value into the tuple,
            // early-return a freshly-built `Result<EnclosingT, E>::Err`
            // on the first Err.
            //
            // Around either lowering, emit the structural-scope
            // markers (`enter_group`, `set_branch(i)` per element,
            // `exit_group`) so the recorder annotates contained
            // effects with the same `(group_id, branch_path,
            // effect_occurrence)` tuple the VM does. Replay across
            // backends matches by those, not by strict sequence
            // position. The three host imports get registered by the
            // discovery walker as soon as it sees an independent
            // product anywhere in the program.
            emit_group_call(func, ctx, "__record_enter_group");
            if *unwrap {
                emit_independent_product_unwrap(func, items, slots, ctx)?;
            } else {
                emit_tuple_literal_with_branch_markers(func, items, slots, ctx)?;
            }
            emit_group_call(func, ctx, "__record_exit_group");
        }
        Expr::Ident(_) => {
            return Err(WasmGcError::Unimplemented(
                "bare Ident reached emitter (resolver should have produced Resolved)",
            ));
        }
        Expr::Resolved { slot, .. } => {
            func.instruction(&Instruction::LocalGet(*slot as u32));
        }
        Expr::BinOp(op, l, r) => {
            // Read operand types from typed AST. Aver's type checker
            // has already proven both operands have the same type, so
            // peeking at the LHS suffices.
            let lhs_aver = aver_type_str_of(l);
            let lhs_aver_trim = lhs_aver.trim();
            // String `+` and `==` lower to dedicated builtins, not the
            // primitive numeric op set.
            if matches!(op, BinOp::Add) && lhs_aver_trim == "String" {
                emit_string_concat2(func, l, r, slots, ctx)?;
            } else if matches!(op, BinOp::Eq | BinOp::Neq) && lhs_aver_trim == "String" {
                emit_string_eq(func, l, r, slots, ctx, matches!(op, BinOp::Neq))?;
            } else if matches!(op, BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte)
                && lhs_aver_trim == "String"
            {
                // Lexicographic byte compare via the
                // `__wasmgc_string_compare` builtin (returns -1 / 0 / 1).
                // Then post-compose with the right i32 op so the BinOp
                // returns 0/1 like every other comparison.
                let cmp_idx = ctx
                    .fn_map
                    .builtins
                    .get("__wasmgc_string_compare")
                    .copied()
                    .ok_or(WasmGcError::Validation(
                        "String comparison requires __wasmgc_string_compare builtin".into(),
                    ))?;
                emit_expr(func, l, slots, ctx)?;
                emit_expr(func, r, slots, ctx)?;
                func.instruction(&Instruction::Call(cmp_idx));
                func.instruction(&Instruction::I32Const(0));
                let post = match op {
                    BinOp::Lt => Instruction::I32LtS,
                    BinOp::Gt => Instruction::I32GtS,
                    BinOp::Lte => Instruction::I32LeS,
                    BinOp::Gte => Instruction::I32GeS,
                    _ => unreachable!(),
                };
                func.instruction(&post);
            } else if matches!(op, BinOp::Eq | BinOp::Neq)
                && let Some(variant_idx) = nullary_variant_idx(r, ctx)
            {
                // `<expr> == VariantName` where VariantName is a
                // nullary user-variant constructor — lower as
                // `ref.test (ref $variant_idx) <expr>`. Two distinct
                // `struct.new` calls of the same nullary variant don't
                // share identity, so `ref.eq` won't work; type-test
                // semantics match the user-level "is the value of this
                // variant" intent for nullary variants.
                emit_expr(func, l, slots, ctx)?;
                func.instruction(&Instruction::RefTestNonNull(
                    wasm_encoder::HeapType::Concrete(variant_idx),
                ));
                if matches!(op, BinOp::Neq) {
                    func.instruction(&Instruction::I32Eqz);
                }
            } else if matches!(op, BinOp::Eq | BinOp::Neq)
                && let Some(variant_idx) = nullary_variant_idx(l, ctx)
            {
                emit_expr(func, r, slots, ctx)?;
                func.instruction(&Instruction::RefTestNonNull(
                    wasm_encoder::HeapType::Concrete(variant_idx),
                ));
                if matches!(op, BinOp::Neq) {
                    func.instruction(&Instruction::I32Eqz);
                }
            } else if matches!(op, BinOp::Eq | BinOp::Neq)
                && let Some(eq_fn_idx) = sum_or_record_eq_fn(l, ctx)
            {
                // Sum / record `==` / `!=`: structural equality via
                // per-type `__eq_<TypeName>` helper. Both operands push
                // an eqref; helper returns i32 (1=eq, 0=ne). `Neq`
                // appends `i32.eqz` to flip the verdict.
                emit_expr(func, l, slots, ctx)?;
                emit_expr(func, r, slots, ctx)?;
                func.instruction(&Instruction::Call(eq_fn_idx));
                if matches!(op, BinOp::Neq) {
                    func.instruction(&Instruction::I32Eqz);
                }
            } else {
                // Operand kind: Aver may type-check `Int op Float` as
                // Float (e.g. unary `-273.15` parses to `0 - 273.15`
                // where the `0` is `Literal::Int(0)`). Pick `F64` if
                // either side wants it; emit `f64.convert_i64_s` after
                // any I64-typed operand to promote into the chosen
                // numeric kind. Mirror of `src/codegen/wasm/expr/emit.rs`.
                let l_ty = wasm_type_of(l, ctx.registry)?;
                let r_ty = wasm_type_of(r, ctx.registry)?;
                let operand = if l_ty == Some(ValType::F64) || r_ty == Some(ValType::F64) {
                    Some(ValType::F64)
                } else {
                    l_ty
                };
                emit_expr(func, l, slots, ctx)?;
                if operand == Some(ValType::F64) && l_ty == Some(ValType::I64) {
                    func.instruction(&Instruction::F64ConvertI64S);
                }
                emit_expr(func, r, slots, ctx)?;
                if operand == Some(ValType::F64) && r_ty == Some(ValType::I64) {
                    func.instruction(&Instruction::F64ConvertI64S);
                }
                let inst = match (operand, op) {
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
                    // Default to i64 ops for Int. Bool ops would land here
                    // too if Aver had `&&` / `||` as BinOps; today they're
                    // builtins (Bool.and / Bool.or), routed through FnCall.
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
            }
        }
        Expr::FnCall(callee, args) => {
            // `Type.Variant(args)` parses as `FnCall(Attr(_, name),
            // args)` — route to struct.new when `name` is a known
            // variant, otherwise check for a dotted builtin, otherwise
            // a real fn call.
            if let Expr::Attr(parent, member) = &callee.node {
                // Built-in Option constructors come through here as
                // `Option.Some(v)` / `Option.None`. Catch them before
                // user-variant lookup because Option isn't a TypeDef.
                // Other `Option.<method>` calls (`withDefault`, etc.)
                // fall through to the dotted-builtin dispatch below.
                if let Expr::Ident(p) = &parent.node
                    && p == "Option"
                    && (member == "Some" || member == "None")
                {
                    return match member.as_str() {
                        "Some" if args.len() == 1 => {
                            emit_option_constructor(func, Some(&args[0]), None, slots, ctx)
                        }
                        "None" => {
                            // Read T from the typed AST first; use the
                            // Type::Invalid-recovery reader so post-
                            // error gradual-typing branches still
                            // resolve to a concrete Option<T>.
                            let stamped_canonical =
                                aver_type_canonical(expr, ctx.return_type, ctx.registry);
                            let hint: String = if let Some(inner) = stamped_canonical
                                .strip_prefix("Option<")
                                .and_then(|s| s.strip_suffix('>'))
                            {
                                inner.to_string()
                            } else {
                                ctx.return_type.to_string()
                            };
                            emit_option_constructor(func, None, Some(&hint), slots, ctx)
                        }
                        _ => Err(WasmGcError::Validation(format!(
                            "Option.{member} with {} args is not a valid constructor",
                            args.len()
                        ))),
                    };
                }
                if let Expr::Ident(p) = &parent.node
                    && p == "Result"
                    && (member == "Ok" || member == "Err")
                {
                    return emit_result_constructor(func, member, args.first(), slots, ctx);
                }
                // `List.prepend(head, tail)` — direct Cons cell.
                if let Expr::Ident(p) = &parent.node
                    && p == "List"
                    && member == "prepend"
                    && args.len() == 2
                {
                    return emit_list_prepend(func, &args[0], &args[1], slots, ctx);
                }
                // `List.empty()` — null ref of the surrounding list type.
                if let Expr::Ident(p) = &parent.node
                    && p == "List"
                    && member == "empty"
                    && args.is_empty()
                {
                    return emit_list_empty(func, ctx);
                }
                // `Foo.Bar(args...)` — sum-type variant constructor.
                // Disambiguate by parent so two sumtypes sharing a
                // bare variant name (e.g. `Query.ProviderSummary` vs
                // `QueryOutput.ProviderSummary`) pick up their own
                // concrete struct idx.
                let parent_ident = match &parent.node {
                    Expr::Ident(n) => Some(n.as_str()),
                    Expr::Resolved { name, .. } => Some(name.as_str()),
                    _ => None,
                };
                let info = parent_ident
                    .and_then(|p| ctx.registry.variant_in(p, member))
                    .or_else(|| ctx.registry.variant(member))
                    .cloned();
                if let Some(info) = info {
                    return emit_constructor_with_args(func, &info, args, slots, ctx);
                }
                // Builtins: `Type.method(args...)` shape. We support
                // a curated set today (the ones bench scenarios use);
                // anything else surfaces as Unimplemented.
                if let Some(parent_name) = parent_ident {
                    return emit_dotted_builtin(func, parent_name, member, args, slots, ctx);
                }
            }
            let name = match &callee.node {
                Expr::Ident(n) => n.as_str(),
                Expr::Resolved { name, .. } => name.as_str(),
                _ => {
                    return Err(WasmGcError::Unimplemented(
                        "phase 3b — exotic callee shape (chained Attr, lambda, etc.)",
                    ));
                }
            };
            // Resolve before evaluating args — if `name` is a local
            // (binding/param of a `Fn(...) -> _` shape, only ever
            // reachable through verify-only fns) the wasm-gc backend
            // doesn't emit a higher-order call. Emit `unreachable`,
            // which wasm validation treats as polymorphic — the
            // surrounding block's result type can be anything. Body is
            // dead from a `_start` perspective so the trap never fires.
            let entry = ctx.fn_map.by_name.get(name);
            if entry.is_none() && ctx.self_local_slot(name).is_some() {
                func.instruction(&Instruction::Unreachable);
                return Ok(());
            }
            for arg in args {
                emit_expr(func, arg, slots, ctx)?;
            }
            let entry = entry.ok_or(WasmGcError::Validation(format!(
                "call to unknown fn `{name}`"
            )))?;
            func.instruction(&Instruction::Call(entry.wasm_idx));
        }
        Expr::Match { subject, arms } => emit_match(func, subject, arms, slots, ctx)?,
        Expr::TailCall(boxed) => emit_tail_call(func, &boxed.target, &boxed.args, slots, ctx)?,
        Expr::RecordCreate { type_name, fields } => {
            emit_record_create(func, type_name, fields, slots, ctx)?
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => emit_record_update(func, type_name, base, updates, slots, ctx)?,
        Expr::Attr(obj, field) => {
            // `Option.None` lands here as a bare attribute reference
            // (parser doesn't synthesise a FnCall for nullary
            // constructors). Catch it before falling into struct field
            // access, which would never resolve.
            if let Expr::Ident(p) = &obj.node
                && p == "Option"
                && field == "None"
            {
                // Read T from the typed AST: type checker stamps every
                // `Option.None` with the inferred Option<T>. Use the
                // Type::Invalid-recovery canonical reader so post-error
                // gradual-typing branches still resolve to a concrete
                // instantiation (single-instantiation fallback or
                // enclosing-fn return-type carry-through).
                let stamped_canonical = aver_type_canonical(expr, ctx.return_type, ctx.registry);
                let hint: String = if let Some(inner) = stamped_canonical
                    .strip_prefix("Option<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    inner.to_string()
                } else {
                    ctx.return_type.to_string()
                };
                emit_option_constructor(func, None, Some(&hint), slots, ctx)?;
            } else if let Expr::Ident(parent) = &obj.node
                && let Some(info) = ctx.registry.variant_in(parent, field).cloned()
                && info.fields.is_empty()
            {
                // `Tag.Red` etc. — Attr access on a user-defined sum
                // type's name resolves to a nullary variant constructor.
                // The FnCall path already does this for `Tag.Red()`;
                // mirror it for the bare-attr form so the variant can
                // be used in any value position (Map K, fn arg, return
                // expr) without a trailing `()`.
                emit_constructor_with_args(func, &info, &[], slots, ctx)?;
            } else {
                emit_attr_get(func, obj, field, slots, ctx)?;
            }
        }
        Expr::Constructor(name, payload) => {
            emit_constructor(func, expr, name, payload.as_deref(), slots, ctx)?
        }
        Expr::ErrorProp(inner) => emit_error_prop(func, inner, slots, ctx)?,
        #[allow(unreachable_patterns)]
        other => {
            eprintln!("UNIMPL EMIT shape={:?}", other);
            return Err(WasmGcError::Unimplemented(
                "expression shape outside phase 2/3/4",
            ));
        }
    }
    Ok(())
}

/// Recognise the syntactic form `Tile.Floor` / `EntityKind.WildIfElse`
/// etc. — a bare attribute access on a registered sum-type's name
/// resolving to a nullary variant. Returns the variant's wasm struct
/// type index when the shape matches.
///
/// Also accepts `FnCall(Attr(parent, name), [])` (the `Tile.Floor()`
/// form) since the parser may insert empty arg lists for explicit-paren
/// calls. Matches both `Constructor(name, None)` and `Attr(Ident,
/// field)` shapes.
/// Looks up the per-type `__eq_<TypeName>` helper fn idx for an operand
/// of `BinOp::Eq` / `BinOp::Neq` whose stamped type is a record or sum.
/// Returns `None` for primitive operand types (the caller's default
/// `i64.eq` / `f64.eq` path handles those) or for nominal types whose
/// helper wasn't registered — discovery should've registered every
/// reachable site, so a miss surfaces as `None` and the call falls
/// through to the default arm where wasm validation will catch the
/// type mismatch.
fn sum_or_record_eq_fn(operand: &Spanned<Expr>, ctx: &EmitCtx<'_>) -> Option<u32> {
    let ty = operand.ty()?;
    match ty {
        crate::types::Type::Named(name) => {
            // Newtypes already lower to their underlying primitive —
            // no helper needed (the default i64/f64 eq handles them).
            if ctx.registry.newtype_underlying(name).is_some() {
                return None;
            }
            let is_record = ctx.registry.record_fields.contains_key(name);
            let is_sum = ctx
                .registry
                .variants
                .values()
                .flat_map(|v| v.iter())
                .any(|v| &v.parent == name);
            if !is_record && !is_sum {
                return None;
            }
            ctx.fn_map.eq_helpers.get(name).copied()
        }
        // Generic carriers — `Option<X>`, `Result<X,Y>`, `Tuple<…>`
        // have per-instantiation `__eq_<canonical>` helpers since
        // 0.16.3. Lookup by the type's display canonical (whitespace-
        // free, matching how the discovery walker registered it).
        crate::types::Type::Option(_)
        | crate::types::Type::Result(_, _)
        | crate::types::Type::Tuple(_) => {
            let canonical: String = ty
                .display()
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            ctx.fn_map.eq_helpers.get(&canonical).copied()
        }
        // List / Vector — list_helpers / vfl_helpers slot the per-T
        // eq fn at registration time. Dispatch through there. Both
        // ops carry `eq: Option<u32>` (None when T isn't equality-
        // resolvable; falls through and the surrounding default i64
        // arm fails validation, same as before this PR).
        crate::types::Type::List(_) => {
            let canonical: String = ty
                .display()
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            ctx.fn_map.list_ops.get(&canonical).and_then(|ops| ops.eq)
        }
        crate::types::Type::Vector(_) => {
            let canonical: String = ty
                .display()
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            ctx.fn_map.vfl_ops.get(&canonical).and_then(|ops| ops.eq)
        }
        _ => None,
    }
}

fn nullary_variant_idx(expr: &Spanned<Expr>, ctx: &EmitCtx<'_>) -> Option<u32> {
    match &expr.node {
        Expr::Attr(obj, field) => {
            let parent = match &obj.node {
                Expr::Ident(p) => p.as_str(),
                Expr::Resolved { name, .. } => name.as_str(),
                _ => return None,
            };
            let info = ctx.registry.variant(field)?;
            if info.parent == parent && info.fields.is_empty() {
                Some(info.type_idx)
            } else {
                None
            }
        }
        Expr::FnCall(callee, args) if args.is_empty() => {
            if let Expr::Attr(obj, field) = &callee.node {
                let parent = match &obj.node {
                    Expr::Ident(p) => p.as_str(),
                    Expr::Resolved { name, .. } => name.as_str(),
                    _ => return None,
                };
                let info = ctx.registry.variant(field)?;
                if info.parent == parent && info.fields.is_empty() {
                    Some(info.type_idx)
                } else {
                    None
                }
            } else {
                None
            }
        }
        Expr::Constructor(name, payload) if payload.is_none() => {
            let bare = name.rsplit('.').next().unwrap_or(name);
            let info = ctx.registry.variant(bare)?;
            if info.fields.is_empty() {
                Some(info.type_idx)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// `lhs + rhs` for `String + String` — build a 2-element array of
/// `(ref null $string)` and call `__wasmgc_concat_n`. Reuses the
/// variadic helper already used by interpolation, so no new runtime
/// surface; same O(total_len) bytes copied.
pub(super) fn emit_string_concat2(
    func: &mut Function,
    lhs: &Spanned<Expr>,
    rhs: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let vec_idx = ctx
        .registry
        .vector_type_idx("Vector<String>")
        .ok_or(WasmGcError::Validation(
            "String `+` requires Vector<String> slot but it wasn't registered \
             (discovery should eager-allocate it on any String concat use)"
                .into(),
        ))?;
    let concat_idx = ctx
        .fn_map
        .builtins
        .get("__wasmgc_concat_n")
        .copied()
        .ok_or(WasmGcError::Validation(
            "String `+` requires __wasmgc_concat_n builtin but it wasn't registered \
             (discovery should register it on any String `+` site)"
                .into(),
        ))?;
    emit_expr(func, lhs, slots, ctx)?;
    emit_expr(func, rhs, slots, ctx)?;
    func.instruction(&Instruction::ArrayNewFixed {
        array_type_index: vec_idx,
        array_size: 2,
    });
    func.instruction(&Instruction::Call(concat_idx));
    Ok(())
}

/// `String == String` / `String != String` — call `__wasmgc_string_eq`,
/// optionally invert with `i32.eqz` for `!=`.
pub(super) fn emit_string_eq(
    func: &mut Function,
    lhs: &Spanned<Expr>,
    rhs: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    invert: bool,
) -> Result<(), WasmGcError> {
    let eq_idx = ctx
        .fn_map
        .builtins
        .get("__wasmgc_string_eq")
        .copied()
        .ok_or(WasmGcError::Validation(
            "String `==`/`!=` requires __wasmgc_string_eq builtin but it wasn't registered \
             (discovery should register it on any String comparison site)"
                .into(),
        ))?;
    emit_expr(func, lhs, slots, ctx)?;
    emit_expr(func, rhs, slots, ctx)?;
    func.instruction(&Instruction::Call(eq_idx));
    if invert {
        func.instruction(&Instruction::I32Eqz);
    }
    Ok(())
}

/// `subject?` postfix: subject must be `Result<T, E>`. If `Result.Ok(v)`,
/// push the unwrapped `v` (struct field 1). If `Result.Err`, return the
/// whole subject struct as the surrounding fn's return value (which the
/// type checker has guaranteed is `Result<_, E>` with the same `E`).
///
/// We reuse the existing scratch slot reservation: `expr_needs_scratch`
/// already returns `true` for any FnCall containing `Option.withDefault`
/// shapes etc., but ErrorProp may appear in fns that have no other
/// scratch consumer. To avoid a separate slot allocation pass for one
/// instruction, we stage the subject ref via `local.tee` on the enclosing
/// fn's return-type-shaped local — wrong: we don't have one. Instead use
/// a dedicated `block` with a result of the Ok type: emit subject, dup
/// via tee into the always-reserved scratch (callers of fns with
/// ErrorProp will already need a scratch — the schema-only
/// `expr_needs_scratch` predicate is updated to cover this case).
pub(super) fn emit_error_prop(
    func: &mut Function,
    inner: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "ErrorProp (`?`) requires a subject scratch slot but none was reserved \
         (slots::expr_needs_scratch must opt in for Expr::ErrorProp)"
            .into(),
    ))?;
    let subject_ty = aver_type_str_of(inner);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "ErrorProp: subject type `{subject_ty}` is not a registered Result<T,E>"
        )))?;
    let (t_aver, _e_aver) = TypeRegistry::result_te(&canonical).ok_or(WasmGcError::Validation(
        format!("ErrorProp: Result canonical `{canonical}` malformed"),
    ))?;
    // Result<Unit, E>?: the Ok arm has no observable value to surface.
    // Use an empty block type and skip the `struct.get field 1` push so
    // the trailing stmt sees a true zero-value expression — matches the
    // Aver-side `? : Unit` typing.
    let unit_ok = t_aver.trim() == "Unit";
    let block_ty = if unit_ok {
        wasm_encoder::BlockType::Empty
    } else {
        let ok_wasm = aver_to_wasm(t_aver, Some(ctx.registry))?.ok_or(WasmGcError::Validation(
            format!("ErrorProp: Ok type `{t_aver}` has no wasm representation"),
        ))?;
        wasm_encoder::BlockType::Result(ok_wasm)
    };

    // Resolve the enclosing fn's return Result<EnclosingT, E> so the
    // Err arm can build a fresh Result of the *enclosing* shape. The
    // subject's Result<T, E> may be a different T than the enclosing
    // (e.g. `String.toInt(...)?` inside `fn parseUser(...) ->
    // Result<User, String>` — subject is Result<Int, String>, enclosing
    // is Result<User, String>); returning the subject ref directly
    // would be a type mismatch.
    let enclosing_canonical: String = ctx
        .return_type
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let enclosing_idx =
        ctx.registry
            .result_type_idx(&enclosing_canonical)
            .ok_or(WasmGcError::Validation(format!(
                "ErrorProp: enclosing fn return `{}` is not a registered Result<T,E>",
                ctx.return_type
            )))?;
    let (enclosing_t_aver, _) =
        TypeRegistry::result_te(&enclosing_canonical).ok_or(WasmGcError::Validation(format!(
            "ErrorProp: enclosing Result canonical `{enclosing_canonical}` malformed"
        )))?;

    // Push subject and stash in scratch so we can read tag and either
    // unwrap field 1 or rebuild the Err arm.
    emit_expr(func, inner, slots, ctx)?;
    func.instruction(&Instruction::LocalSet(scratch));

    // tag == 1 (Ok)?
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
    // Ok arm — push field 1 (the T payload). Unit-typed T produces
    // no observable value, so the Ok arm stays empty.
    if !unit_ok {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: res_idx,
            field_index: 1,
        });
    }
    func.instruction(&Instruction::Else);
    // Err arm — extract the err field from the subject and wrap it
    // in a fresh Result<EnclosingT, E>::Err so the return type lines
    // up with what the enclosing fn declared.
    func.instruction(&Instruction::I32Const(0)); // tag=0 (Err)
    emit_default_value(func, enclosing_t_aver, ctx.registry)?;
    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(res_idx),
    ));
    func.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 2,
    });
    func.instruction(&Instruction::StructNew(enclosing_idx));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);
    Ok(())
}

/// Lower `RecordCreate { type_name, fields }` to `struct.new $type_idx`.
/// Aver records have `RecordCreate` field order coming from source
/// position — we re-order to the declaration order from `TypeRegistry`
/// before pushing values, so the wasm struct layout always matches the
/// declared shape.
pub(super) fn emit_record_create(
    func: &mut Function,
    type_name: &str,
    fields: &[(String, Spanned<Expr>)],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    // Newtype optimization: skip struct.new — emit the single field's
    // value directly. Same shape `aver_to_wasm` reports for newtype
    // types, so locals/params match primitive ValType.
    if ctx.registry.newtype_underlying(type_name).is_some() {
        let field = fields.first().ok_or(WasmGcError::Validation(format!(
            "newtype record `{type_name}` requires one field"
        )))?;
        return emit_expr(func, &field.1, slots, ctx);
    }
    let type_idx = ctx
        .registry
        .record_type_idx(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "unknown record type `{type_name}`"
        )))?;
    let decl_fields = ctx
        .registry
        .record_fields
        .get(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "record `{type_name}` missing field list"
        )))?;
    // Push fields in declaration order. Aver guarantees the user
    // supplies every declared field (the type checker enforces
    // exhaustiveness).
    for (decl_name, decl_ty) in decl_fields {
        let provided =
            fields
                .iter()
                .find(|(n, _)| n == decl_name)
                .ok_or(WasmGcError::Validation(format!(
                    "record `{type_name}` missing field `{decl_name}`"
                )))?;
        // Special-case `Option.None` field values — the constructor
        // emitter normally falls back to `ctx.return_type` for the T
        // hint, which is wrong inside a `RecordCreate` (the enclosing
        // fn's return type has nothing to do with the field's
        // declared `Option<T>`). Read T off the field declaration
        // and emit through the constructor directly.
        if is_option_none_expr(&provided.1.node)
            && let Some(inner) = decl_ty
                .trim()
                .strip_prefix("Option<")
                .and_then(|s| s.strip_suffix('>'))
        {
            emit_option_constructor(func, None, Some(inner.trim()), slots, ctx)?;
            continue;
        }
        // Same story for empty-list field values (`pieceMoves = []`
        // inside a `GameState(...)` literal). `emit_list_literal`
        // would otherwise default to `ctx.return_type` or the first
        // registered list canonical — both wrong when the field's
        // declared type is e.g. `List<List<Point>>`.
        if let Expr::List(items) = &provided.1.node
            && items.is_empty()
        {
            let canonical: String = decl_ty.chars().filter(|c| !c.is_whitespace()).collect();
            if let Some(list_idx) = ctx.registry.list_type_idx(&canonical) {
                func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                    list_idx,
                )));
                continue;
            }
        }
        emit_expr(func, &provided.1, slots, ctx)?;
    }
    func.instruction(&Instruction::StructNew(type_idx));
    Ok(())
}

/// True iff `expr` is the literal `Option.None` constructor reference
/// (either as `Expr::Attr(Ident("Option"), "None")` or as a
/// `Constructor("Option.None", None)` after pipeline rewrites).
fn is_option_none_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Attr(obj, field) => {
            field == "None" && matches!(&obj.node, Expr::Ident(name) if name == "Option")
        }
        Expr::Constructor(name, payload) => {
            payload.is_none()
                && (name == "Option.None"
                    || name.rsplit('.').next() == Some("None") && name.starts_with("Option"))
        }
        _ => false,
    }
}

/// Lower `RecordUpdate { type_name, base, updates }` to a fresh
/// `struct.new $type_idx`. Each declared field is either taken from
/// `updates` (the user-supplied override) or copied from `base` via
/// `struct.get $type_idx $field_idx`. The `base` expression is
/// re-emitted once per non-updated field; for the common case where
/// `base` is a `Resolved` local (most update sites in the games look
/// like `state.update(dx = 0, dy = -1)` with `state` being a fn
/// param), re-emit is a single `local.get` and effectively free. A
/// scratch local would be more efficient when `base` is itself a
/// complex computation, but Function locals are reserved up front in
/// `SlotTable::build_for_fn` and the scratch slot allocator only
/// handles the multi-arm-match case today — extending it to record-
/// update sites is a follow-up.
pub(super) fn emit_record_update(
    func: &mut Function,
    type_name: &str,
    base: &Spanned<Expr>,
    updates: &[(String, Spanned<Expr>)],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let type_idx = ctx
        .registry
        .record_type_idx(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "unknown record type `{type_name}`"
        )))?;
    let decl_fields = ctx
        .registry
        .record_fields
        .get(type_name)
        .ok_or(WasmGcError::Validation(format!(
            "record `{type_name}` missing field list"
        )))?
        .clone();
    for (decl_name, decl_ty) in &decl_fields {
        if let Some((_, override_expr)) = updates.iter().find(|(n, _)| n == decl_name) {
            // Same `Option.None` / empty-list field special-cases as
            // `emit_record_create` — the override expression's typed-AST
            // info may resolve to a generic `Type::Var` for these
            // literal forms, but the field's declared type is the
            // authoritative shape to emit against.
            if is_option_none_expr(&override_expr.node)
                && let Some(inner) = decl_ty
                    .trim()
                    .strip_prefix("Option<")
                    .and_then(|s| s.strip_suffix('>'))
            {
                emit_option_constructor(func, None, Some(inner.trim()), slots, ctx)?;
                continue;
            }
            if let Expr::List(items) = &override_expr.node
                && items.is_empty()
            {
                let canonical: String = decl_ty.chars().filter(|c| !c.is_whitespace()).collect();
                if let Some(list_idx) = ctx.registry.list_type_idx(&canonical) {
                    func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                        list_idx,
                    )));
                    continue;
                }
            }
            emit_expr(func, override_expr, slots, ctx)?;
        } else {
            let field_idx = ctx
                .registry
                .record_field_index(type_name, decl_name)
                .ok_or(WasmGcError::Validation(format!(
                    "record `{type_name}` has no field `{decl_name}` to copy from base"
                )))?;
            emit_expr(func, base, slots, ctx)?;
            func.instruction(&Instruction::StructGet {
                struct_type_index: type_idx,
                field_index: field_idx,
            });
        }
    }
    func.instruction(&Instruction::StructNew(type_idx));
    Ok(())
}

/// Lower `Attr(obj, field)` to `obj; struct.get $type_idx $field_idx`.
/// The struct type of `obj` comes straight from `Spanned::ty()` — the
/// type checker has already resolved chained attribute access (e.g.
/// `state.snake.head` walks GameState → Snake → Point), so the
/// backend just reads the stamp.
pub(super) fn emit_attr_get(
    func: &mut Function,
    obj: &Spanned<Expr>,
    field: &str,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let record_name = aver_type_str_of(obj);
    // Newtype: Attr is identity — just emit `obj` and return its
    // primitive value directly. The newtype-underlying check works on
    // the bare type name (no generics involved here).
    if ctx.registry.newtype_underlying(&record_name).is_some() {
        return emit_expr(func, obj, slots, ctx);
    }
    let type_idx = ctx
        .registry
        .record_type_idx(&record_name)
        .ok_or(WasmGcError::Validation(format!(
            "unknown record type `{record_name}` for Attr"
        )))?;
    let field_idx =
        ctx.registry
            .record_field_index(&record_name, field)
            .ok_or(WasmGcError::Validation(format!(
                "record `{record_name}` has no field `{field}`"
            )))?;
    emit_expr(func, obj, slots, ctx)?;
    func.instruction(&Instruction::StructGet {
        struct_type_index: type_idx,
        field_index: field_idx,
    });
    Ok(())
}

/// `Result.Ok(v)` / `Result.Err(e)` — three-field struct (tag, ok,
/// err). Tag=1 for Ok, 0 for Err. Unused payload field gets a default
/// value (zero/null) so `struct.new` always has a balanced argument
/// list — well-typed pattern match never reads it.
pub(super) fn emit_result_constructor(
    func: &mut Function,
    variant: &str,
    payload: Option<&Spanned<Expr>>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let payload = payload.ok_or(WasmGcError::Validation(format!(
        "Result.{variant} requires a payload"
    )))?;
    let payload_ty = aver_type_str_of(payload);
    // Find a registered Result<T,E> where the matching position
    // matches the payload's inferred type. Fallback: pick the only
    // registered Result instantiation, or use the fn's return type.
    let canonical = if ctx.registry.result_order.len() == 1 {
        ctx.registry.result_order[0].clone()
    } else {
        let return_canonical: String = ctx
            .return_type
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        if ctx.registry.result_type_idx(&return_canonical).is_some() {
            return_canonical
        } else {
            // Try matching the payload type against T (Ok) or E (Err).
            ctx.registry
                .result_order
                .iter()
                .find(|c| {
                    if let Some((t, e)) = TypeRegistry::result_te(c) {
                        let match_pos = if variant == "Ok" { t } else { e };
                        match_pos == payload_ty.trim()
                    } else {
                        false
                    }
                })
                .cloned()
                .ok_or(WasmGcError::Validation(format!(
                    "Result.{variant}({payload_ty}) — no registered Result<T,E> instantiation matches"
                )))?
        }
    };
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .expect("just-resolved canonical");
    let (t_aver, e_aver) = TypeRegistry::result_te(&canonical).ok_or(WasmGcError::Validation(
        format!("Result canonical `{canonical}` malformed"),
    ))?;

    // Unit payload doesn't push a value (`emit_expr` for `Literal::Unit`
    // is a no-op). The struct slot is still i32-sized (see module.rs
    // Result type emission), so push the i32 placeholder ourselves.
    let emit_payload = |func: &mut Function, ty: &str| -> Result<(), WasmGcError> {
        if ty.trim() == "Unit" {
            func.instruction(&Instruction::I32Const(0));
            Ok(())
        } else {
            emit_expr(func, payload, slots, ctx)
        }
    };
    if variant == "Ok" {
        func.instruction(&Instruction::I32Const(1));
        emit_payload(func, t_aver)?;
        emit_default_value(func, e_aver, ctx.registry)?;
    } else {
        func.instruction(&Instruction::I32Const(0));
        emit_default_value(func, t_aver, ctx.registry)?;
        emit_payload(func, e_aver)?;
    }
    func.instruction(&Instruction::StructNew(res_idx));
    Ok(())
}

/// `List.prepend(head, tail)` → `struct.new $list_T head tail`.
pub(super) fn emit_list_prepend(
    func: &mut Function,
    head: &Spanned<Expr>,
    tail: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let tail_ty = aver_type_str_of(tail);
    let canonical: String = tail_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let list_idx = ctx
        .registry
        .list_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List.prepend: tail type `{tail_ty}` is not a registered List<T>"
        )))?;
    emit_expr(func, head, slots, ctx)?;
    emit_expr(func, tail, slots, ctx)?;
    func.instruction(&Instruction::StructNew(list_idx));
    Ok(())
}

/// `List.empty()` — `ref.null $list_T` of whatever List<T> the
/// surrounding context expects.
pub(super) fn emit_list_empty(func: &mut Function, ctx: &EmitCtx<'_>) -> Result<(), WasmGcError> {
    let canonical = if ctx.registry.list_order.len() == 1 {
        ctx.registry.list_order[0].clone()
    } else {
        ctx.return_type
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect::<String>()
    };
    let list_idx = ctx
        .registry
        .list_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List.empty: cannot resolve list instantiation (got `{canonical}`)"
        )))?;
    func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        list_idx,
    )));
    Ok(())
}

/// `[a, b, c]` literal → `Cons a (Cons b (Cons c null))`.
/// `MapLiteral` emit: lower `{"k" => "v", ...}` to
/// `Map.empty()` followed by one `Map.set(map, k, v)` per entry.
/// Each `set` consumes the previous map ref, K, V from the stack and
/// returns the updated map ref — so a sequence of set calls leaves
/// the final map on top of the stack with no scratch slot needed.
/// `(a, b)` tuple literal. Lowers to `struct.new $tuple_AB`. Element
/// types come from the items' inferred types; the resulting canonical
/// must already be registered (eager paths cover the common cases:
/// every Map<K,V> registers Tuple<K,V>; user fn signatures register
/// the rest).
/// `(a, b, c)?!` — independent product with Result-unwrapping.
/// Each element evaluates to `Result<T_i, E>`. On Ok, push the inner
/// `T_i` onto the stack; on Err, build a fresh
/// `Result<EnclosingT, E>::Err(<err value>)` and early return. After
/// every element succeeds, materialise `Tuple<T_0, T_1, ..., T_N>`
/// via `struct.new`.
///
/// Type checker proves: every element is `Result<T_i, E>` with the
/// same `E`; the enclosing fn returns `Result<X, E>` with that same
/// `E`. We rely on those invariants here.
pub(super) fn emit_independent_product_unwrap(
    func: &mut Function,
    items: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if items.len() < 2 {
        return Err(WasmGcError::Validation(format!(
            "Independent product `?!` needs at least 2 elements; got {}",
            items.len()
        )));
    }
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "`?!` independent product requires a subject scratch slot but none was reserved \
         (slots::expr_needs_scratch must opt in for Expr::IndependentProduct(_, true))"
            .into(),
    ))?;

    // Resolve enclosing fn's return Result canonical (`Result<X, E>`).
    // Used to materialise the early-return Err with the correct shape.
    let enclosing_canonical: String = ctx
        .return_type
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let enclosing_idx = ctx
        .registry
        .result_type_idx(&enclosing_canonical)
        .ok_or_else(|| {
            WasmGcError::Validation(format!(
                "`?!` independent product: enclosing fn return type `{enclosing_canonical}` \
                 is not a registered Result<X, E>"
            ))
        })?;
    let (enclosing_t_aver, _enclosing_e_aver) = TypeRegistry::result_te(&enclosing_canonical)
        .ok_or_else(|| {
            WasmGcError::Validation(format!(
                "`?!` independent product: enclosing canonical `{enclosing_canonical}` malformed"
            ))
        })?;

    for (idx, item) in items.iter().enumerate() {
        emit_branch_marker(func, ctx, idx as u32);
        // Each element's stamped type must be `Result<T_i, E>`.
        let elem_aver = aver_type_str_of(item);
        let elem_canonical: String = elem_aver.chars().filter(|c| !c.is_whitespace()).collect();
        let elem_res_idx = ctx
            .registry
            .result_type_idx(&elem_canonical)
            .ok_or_else(|| {
                WasmGcError::Validation(format!(
                    "`?!` element type `{elem_aver}` is not a registered Result<T_i, E>"
                ))
            })?;
        let (elem_t_aver, _elem_e_aver) =
            TypeRegistry::result_te(&elem_canonical).ok_or_else(|| {
                WasmGcError::Validation(format!(
                    "`?!` element canonical `{elem_canonical}` malformed"
                ))
            })?;
        // Unit Ok-payload uses i32 placeholder (same convention as the
        // Result<Unit, E> struct slot).
        let ok_wasm = aver_to_wasm(elem_t_aver, Some(ctx.registry))?.unwrap_or(ValType::I32);
        let block_ty = wasm_encoder::BlockType::Result(ok_wasm);

        // Eval, stash in scratch, branch on tag.
        emit_expr(func, item, slots, ctx)?;
        func.instruction(&Instruction::LocalSet(scratch));
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(elem_res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: elem_res_idx,
            field_index: 0, // tag
        });
        func.instruction(&Instruction::I32Const(1)); // OK tag
        func.instruction(&Instruction::I32Eq);
        func.instruction(&Instruction::If(block_ty));
        // Ok arm: push the T_i payload.
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(elem_res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: elem_res_idx,
            field_index: 1, // Ok value
        });
        func.instruction(&Instruction::Else);
        // Err arm: rebuild `Result<EnclosingT, E>::Err(elem_err_value)`
        // and return. Same struct shape as `emit_result_constructor`
        // for variant=Err: tag=0, default(T), then the err value from
        // the failing element.
        func.instruction(&Instruction::I32Const(0)); // ERR tag
        emit_default_value(func, enclosing_t_aver, ctx.registry)?;
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(elem_res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: elem_res_idx,
            field_index: 2, // err value (same E as enclosing)
        });
        func.instruction(&Instruction::StructNew(enclosing_idx));
        func.instruction(&Instruction::Return);
        func.instruction(&Instruction::End);
    }

    // After every element succeeded, all N Ok payloads are on the
    // stack. Materialise the tuple — same shape as `emit_tuple_literal`
    // but without re-emitting the items (they were emitted+unwrapped
    // above). Build the canonical from per-element Ok types.
    let mut elem_t_avers: Vec<String> = Vec::with_capacity(items.len());
    for item in items {
        let elem_aver = aver_type_str_of(item);
        let elem_canonical: String = elem_aver.chars().filter(|c| !c.is_whitespace()).collect();
        let (t, _) = TypeRegistry::result_te(&elem_canonical).expect("re-parse");
        elem_t_avers.push(t.to_string());
    }
    let tuple_canonical: String = format!("Tuple<{}>", elem_t_avers.join(","))
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    let tuple_idx = ctx
        .registry
        .tuple_type_idx(&tuple_canonical)
        .ok_or_else(|| {
            WasmGcError::Validation(format!(
                "`?!` result tuple `{tuple_canonical}` not registered"
            ))
        })?;
    func.instruction(&Instruction::StructNew(tuple_idx));
    Ok(())
}

/// Emit `call $aver/<group_op>` if the program registered the
/// structural-scope marker imports (i.e. discovery saw any `?!` /
/// `!`). No-op otherwise — programs that never use independent
/// products don't pay the import slot. Trailing String arg is the
/// `caller_fn` stamp every effect import now carries; the host
/// ignores it for group markers but the wasm signature still has
/// to match.
fn emit_group_call(func: &mut Function, ctx: &EmitCtx<'_>, op: &str) {
    if let Some(idx) = ctx.effect_idx_lookup.get(op) {
        if emit_caller_fn_idx(func, ctx).is_err() {
            // Should never trip — every fn def has a global allocated
            // in `TypeRegistry::build`.
            return;
        }
        func.instruction(&Instruction::Call(*idx));
    }
}

/// Emit `i64.const i; <caller_fn>; call $aver/__record_set_branch`
/// if the markers are registered. Used inside `?!` / `!` lowering
/// to switch the recorder's active branch before evaluating each
/// element. Trailing caller_fn ignored host-side but its slot is
/// part of the import signature.
fn emit_branch_marker(func: &mut Function, ctx: &EmitCtx<'_>, branch_idx: u32) {
    if let Some(idx) = ctx.effect_idx_lookup.get("__record_set_branch") {
        func.instruction(&Instruction::I64Const(branch_idx as i64));
        if emit_caller_fn_idx(func, ctx).is_err() {
            return;
        }
        func.instruction(&Instruction::Call(*idx));
    }
}

/// `!`-tuple lowering with per-element `set_branch(i)` markers. Same
/// shape as `emit_tuple_literal` but each `emit_expr(item)` is
/// preceded by `emit_branch_marker(i)` so contained effects pick up
/// the right branch index from the recorder. The enclosing
/// `enter_group` / `exit_group` are emitted by the caller arm.
fn emit_tuple_literal_with_branch_markers(
    func: &mut Function,
    items: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if items.len() < 2 {
        return Err(WasmGcError::Validation(format!(
            "Independent-product `!` tuple needs at least 2 elements; got {}",
            items.len()
        )));
    }
    let elem_tys: Vec<String> = items.iter().map(aver_type_str_of).collect();
    let canonical = format!("Tuple<{}>", elem_tys.join(","))
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect::<String>();
    let tuple_idx = ctx
        .registry
        .tuple_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "`!` tuple: `{canonical}` slot not registered"
        )))?;
    for (idx, item) in items.iter().enumerate() {
        emit_branch_marker(func, ctx, idx as u32);
        emit_expr(func, item, slots, ctx)?;
    }
    func.instruction(&Instruction::StructNew(tuple_idx));
    Ok(())
}

pub(super) fn emit_tuple_literal(
    func: &mut Function,
    items: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if items.len() < 2 {
        return Err(WasmGcError::Validation(format!(
            "Tuple literal needs at least 2 elements; got {}",
            items.len()
        )));
    }
    // Build canonical from each element's stamped type. Variadic
    // arity — `struct.new` pops N values regardless.
    let elem_tys: Vec<String> = items.iter().map(aver_type_str_of).collect();
    let canonical = format!("Tuple<{}>", elem_tys.join(","))
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect::<String>();
    let tuple_idx = ctx
        .registry
        .tuple_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Tuple literal: `{canonical}` slot not registered"
        )))?;
    for item in items {
        emit_expr(func, item, slots, ctx)?;
    }
    func.instruction(&Instruction::StructNew(tuple_idx));
    Ok(())
}

pub(super) fn emit_map_literal(
    func: &mut Function,
    entries: &[(Spanned<Expr>, Spanned<Expr>)],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    // Empty map literal — fall back to whichever Map<K,V> the
    // surrounding context expects, if there's exactly one registered.
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
    let helpers = ctx
        .fn_map
        .map_helpers
        .get(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "MapLiteral: helpers missing for `{canonical}`"
        )))?;

    func.instruction(&Instruction::Call(helpers.empty));
    for (k_expr, v_expr) in entries {
        emit_expr(func, k_expr, slots, ctx)?;
        emit_expr(func, v_expr, slots, ctx)?;
        func.instruction(&Instruction::Call(helpers.set));
    }
    Ok(())
}

pub(super) fn emit_list_literal(
    func: &mut Function,
    outer: &Spanned<Expr>,
    items: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    // Prefer the typed-AST canonical when it's a registered List<T>.
    // The type checker stamps every Expr::List with the expected list
    // type from context (call-arg signature, binding annotation, fn
    // return type), which is more reliable than the per-item /
    // per-fn-return heuristics below.
    let stamped = aver_type_canonical(outer, ctx.return_type, ctx.registry);
    let canonical =
        if stamped.starts_with("List<") && ctx.registry.list_type_idx(&stamped).is_some() {
            stamped
        } else if let Some(first) = items.first() {
            // `[Option.None, Option.None, ...]` is a common shape for
            // building filled rows; `infer_aver_type(Option.None)`
            // falls back to `ctx.return_type` and produces a doubly-
            // wrapped `List<List<Option<X>>>` canonical that's never
            // registered. Same trap for empty-list elements. Prefer
            // the enclosing fn's return type when it parses as
            // `List<T>` and the first item lacks its own resolvable
            // type — this gives the correct T for both shapes.
            let needs_hint = matches!(&first.node, Expr::List(xs) if xs.is_empty())
                || is_option_none_expr(&first.node);
            let elem_ty = if needs_hint {
                let ret: String = ctx
                    .return_type
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if let Some(inner) = ret.strip_prefix("List<").and_then(|s| s.strip_suffix('>')) {
                    inner.to_string()
                } else {
                    aver_type_str_of(first)
                }
            } else {
                aver_type_str_of(first)
            };
            format!("List<{elem_ty}>")
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect::<String>()
        } else if ctx.registry.list_order.len() == 1 {
            ctx.registry.list_order[0].clone()
        } else {
            // Empty literal in a context we can't pin down (verify
            // expressions, fn returning a non-List type that wraps `[]`
            // somewhere). Prefer fn return type when it parses as a
            // List, otherwise fall back to the first registered List —
            // a deterministic non-failing choice.
            let ret: String = ctx
                .return_type
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect();
            if ret.starts_with("List<") {
                ret
            } else if let Some(first) = ctx.registry.list_order.first() {
                first.clone()
            } else {
                ret
            }
        };
    let list_idx = ctx
        .registry
        .list_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List literal: cannot resolve list instantiation (got `{canonical}`)"
        )))?;
    if items.is_empty() {
        func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
            list_idx,
        )));
        return Ok(());
    }
    // Call-helper build: emit items left-to-right, then null, then
    // N×`call $cons_T`. `cons_T : (T, list_T) -> list_T` pops the
    // top two stack values per call, so the rightmost element pairs
    // with `null` first (yielding `[last]`), each next-leftward
    // element pairs with the running tail. No scratch local needed —
    // critical for nested literals (`[[1,2,3], [4,5]]`) where a
    // shared scratch slot would race between the outer and inner
    // accumulators.
    let cons_fn = ctx
        .fn_map
        .list_ops_lookup(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List literal: cons helper for `{canonical}` not registered"
        )))?
        .cons;
    // Element type derived from the resolved canonical. Used to give
    // `Option.None` and empty-list elements the correct T even when
    // their typed-AST stamp is a generic `Type::Var` — same trick
    // `emit_record_create` uses for declared field types.
    let elem_ty = TypeRegistry::list_element_type(&canonical).map(|s| s.to_string());
    for item in items {
        if let Some(elem) = elem_ty.as_deref()
            && is_option_none_expr(&item.node)
            && let Some(inner) = elem
                .strip_prefix("Option<")
                .and_then(|s| s.strip_suffix('>'))
        {
            emit_option_constructor(func, None, Some(inner.trim()), slots, ctx)?;
            continue;
        }
        if let Some(elem) = elem_ty.as_deref()
            && let Expr::List(xs) = &item.node
            && xs.is_empty()
            && let Some(list_idx) = ctx.registry.list_type_idx(elem)
        {
            func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                list_idx,
            )));
            continue;
        }
        emit_expr(func, item, slots, ctx)?;
    }
    func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        list_idx,
    )));
    for _ in 0..items.len() {
        func.instruction(&Instruction::Call(cons_fn));
    }
    Ok(())
}

/// `match pair { (a, b) -> body }` — single-arm tuple destructure.
/// Subject is `(ref null $tuple_AB)`; struct.get each field into the
/// corresponding ident binding. `_` skips the bind.
pub(super) fn emit_tuple_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arm: &MatchArm,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let pat_items = match &arm.pattern {
        Pattern::Tuple(items) => items.as_slice(),
        _ => {
            return Err(WasmGcError::Validation(
                "emit_tuple_match called on non-Tuple pattern".into(),
            ));
        }
    };
    let arm_slots = arm.binding_slots.get().ok_or(WasmGcError::Validation(
        "Tuple match arm reached emit without resolver-allocated binding_slots".into(),
    ))?;
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
    emit_expr(func, subject, slots, ctx)?;
    func.instruction(&Instruction::LocalSet(scratch));
    // pat_items align 1:1 with the resolver-allocated arm_slots —
    // tuple destructure currently only supports flat `Pattern::Ident`
    // items (multi-binding nested tuples land elsewhere).
    for (field_idx, (pat, &slot)) in pat_items.iter().zip(arm_slots.iter()).enumerate() {
        if matches!(pat, Pattern::Ident(_)) && slot != u16::MAX {
            func.instruction(&Instruction::LocalGet(scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(tuple_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: tuple_idx,
                field_index: field_idx as u32,
            });
            func.instruction(&Instruction::LocalSet(slot as u32));
        }
    }
    emit_expr(func, &arm.body, slots, ctx)?;
    Ok(())
}

/// Multi-arm tuple match where the per-element patterns include
/// `Pattern::Constructor` (e.g. `Result.Ok` / `Result.Err`) — emits a
/// nested `if`/`else` cascade. For each arm: AND together each
/// element's tag check, branch on the verdict, extract bindings from
/// the per-element variant struct on the success branch, recurse on
/// the failure branch. The trailing wildcard / catch-all is the base
/// case that closes the cascade.
///
/// Currently supports `Result<T, E>` variants inside the tuple
/// (Ok = tag 1, payload at field 1; Err = tag 0, payload at field 2).
/// `Pattern::Ident` and `Pattern::Wildcard` per-element are treated as
/// "always matches" — slots get bound to the raw element ref / value.
/// Other variant kinds (Option, user sums) fall through to an
/// `Unimplemented` so the failure surfaces obvious next steps rather
/// than silently mis-emitting.
pub(super) fn emit_tuple_constructor_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arms: &[MatchArm],
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "multi-arm tuple match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let tuple_idx = ctx
        .registry
        .tuple_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "multi-arm tuple match: subject type `{subject_ty}` is not a registered Tuple<...>"
        )))?;
    let elems_owned: Vec<String> = TypeRegistry::tuple_elements(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "multi-arm tuple match: cannot extract tuple element types from `{canonical}`"
        )))?
        .into_iter()
        .map(|s| s.to_string())
        .collect();

    emit_expr(func, subject, slots, ctx)?;
    func.instruction(&Instruction::LocalSet(scratch));

    emit_tuple_constructor_arm_cascade(
        func,
        scratch,
        tuple_idx,
        &elems_owned,
        arms,
        block_ty,
        slots,
        ctx,
    )
}

#[allow(clippy::too_many_arguments)]
fn emit_tuple_constructor_arm_cascade(
    func: &mut Function,
    scratch: u32,
    tuple_idx: u32,
    elems: &[String],
    arms: &[MatchArm],
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if arms.is_empty() {
        // Type-checker has proven exhaustiveness; reaching here means
        // every arm was already emitted. Emit `unreachable` so the
        // validator's stack-shape inference treats this branch as
        // polymorphic.
        func.instruction(&Instruction::Unreachable);
        return Ok(());
    }
    let arm = &arms[0];
    match &arm.pattern {
        Pattern::Wildcard => emit_expr(func, &arm.body, slots, ctx),
        Pattern::Ident(_) => {
            if let Some(arm_slots) = arm.binding_slots.get()
                && let Some(&slot) = arm_slots.first()
                && slot != u16::MAX
            {
                func.instruction(&Instruction::LocalGet(scratch));
                func.instruction(&Instruction::LocalSet(slot as u32));
            }
            emit_expr(func, &arm.body, slots, ctx)
        }
        Pattern::Tuple(items) => {
            if items.len() != elems.len() {
                return Err(WasmGcError::Validation(format!(
                    "tuple match arm has {} sub-patterns, subject is a {}-tuple",
                    items.len(),
                    elems.len()
                )));
            }
            // Build the verdict: AND of each element's tag check.
            // Pattern::Ident / Pattern::Wildcard contribute nothing
            // (always match) and the cascade short-circuits to "1"
            // when no constructor tests are present.
            let mut tests_emitted = 0u32;
            for (i, pat) in items.iter().enumerate() {
                if let Pattern::Constructor(name, _) = pat {
                    let bare = name.rsplit('.').next().unwrap_or(name);
                    let elem_canonical: String =
                        elems[i].chars().filter(|c| !c.is_whitespace()).collect();
                    let res_idx = ctx.registry.result_type_idx(&elem_canonical).ok_or(
                        WasmGcError::Unimplemented(
                            "tuple-of-constructors match supports Result<T,E> elements only \
                             today (Option / user variants land separately)",
                        ),
                    )?;
                    let expected_tag: i32 = match bare {
                        "Ok" => 1,
                        "Err" => 0,
                        _ => {
                            return Err(WasmGcError::Unimplemented(
                                "tuple-of-constructors match: unknown Result variant \
                                 (expected Ok or Err)",
                            ));
                        }
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
            // Per-arm binding slots from the resolver — pattern bindings
            // never sit in the function-level slot table; resolver
            // walks tuple elements depth-first and pushes one slot per
            // binding (Ident=1, Constructor=arity, Wildcard=0). Same
            // walk order here so the slot index stays in sync.
            let arm_slots = arm.binding_slots.get().ok_or(WasmGcError::Validation(
                "tuple-of-constructors arm reached emit without binding_slots".into(),
            ))?;
            let mut slot_idx = 0;
            for (i, pat) in items.iter().enumerate() {
                match pat {
                    Pattern::Ident(_) => {
                        let slot = arm_slots[slot_idx];
                        slot_idx += 1;
                        if slot != u16::MAX {
                            func.instruction(&Instruction::LocalGet(scratch));
                            func.instruction(&Instruction::RefCastNonNull(
                                wasm_encoder::HeapType::Concrete(tuple_idx),
                            ));
                            func.instruction(&Instruction::StructGet {
                                struct_type_index: tuple_idx,
                                field_index: i as u32,
                            });
                            func.instruction(&Instruction::LocalSet(slot as u32));
                        }
                    }
                    Pattern::Constructor(name, bindings) => {
                        let bare = name.rsplit('.').next().unwrap_or(name);
                        let elem_canonical: String =
                            elems[i].chars().filter(|c| !c.is_whitespace()).collect();
                        let res_idx_opt = ctx.registry.result_type_idx(&elem_canonical);
                        let payload_field = match bare {
                            "Ok" => Some(1u32),
                            "Err" => Some(2u32),
                            _ => None,
                        };
                        for _binding in bindings {
                            let slot = arm_slots[slot_idx];
                            slot_idx += 1;
                            if slot == u16::MAX {
                                continue;
                            }
                            let (Some(res_idx), Some(payload_field)) = (res_idx_opt, payload_field)
                            else {
                                continue;
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
                                field_index: payload_field,
                            });
                            func.instruction(&Instruction::LocalSet(slot as u32));
                        }
                    }
                    _ => {}
                }
            }
            emit_expr(func, &arm.body, slots, ctx)?;
            func.instruction(&Instruction::Else);
            emit_tuple_constructor_arm_cascade(
                func,
                scratch,
                tuple_idx,
                elems,
                &arms[1..],
                block_ty,
                slots,
                ctx,
            )?;
            func.instruction(&Instruction::End);
            Ok(())
        }
        _ => Err(WasmGcError::Unimplemented(
            "tuple-of-constructors match: arm pattern must be Tuple/Ident/Wildcard",
        )),
    }
}

/// `match list { [] -> a; [head, ..tail] -> b }` — null check on the
/// list ref selects the empty branch; otherwise cast + struct.get
/// the head and tail. Subject must be a registered `List<T>`.
pub(super) fn emit_list_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arms: &[MatchArm],
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "List match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let list_idx = ctx
        .registry
        .list_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "List match: subject type `{subject_ty}` is not a registered List<T>"
        )))?;

    let mut empty_arm: Option<&MatchArm> = None;
    let mut cons_arm: Option<&MatchArm> = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => empty_arm = Some(arm),
            Pattern::Cons(_, _) => cons_arm = Some(arm),
            Pattern::Wildcard => {
                if empty_arm.is_none() {
                    empty_arm = Some(arm);
                } else if cons_arm.is_none() {
                    cons_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let empty_arm = empty_arm.ok_or(WasmGcError::Validation(
        "List match missing empty arm".into(),
    ))?;
    let cons_arm = cons_arm.ok_or(WasmGcError::Validation(
        "List match missing cons arm".into(),
    ))?;

    emit_expr(func, subject, slots, ctx)?;
    func.instruction(&Instruction::LocalSet(scratch));

    func.instruction(&Instruction::LocalGet(scratch));
    func.instruction(&Instruction::RefIsNull);
    func.instruction(&Instruction::If(block_ty));
    emit_expr(func, &empty_arm.body, slots, ctx)?;
    func.instruction(&Instruction::Else);
    if let Pattern::Cons(_head_name, _tail_name) = &cons_arm.pattern {
        let arm_slots = cons_arm.binding_slots.get().ok_or(WasmGcError::Validation(
            "Cons match arm reached emit without resolver-allocated binding_slots".into(),
        ))?;
        let head_slot = arm_slots[0];
        if head_slot != u16::MAX {
            func.instruction(&Instruction::LocalGet(scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(list_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: list_idx,
                field_index: 0,
            });
            func.instruction(&Instruction::LocalSet(head_slot as u32));
        }
        let tail_slot = arm_slots[1];
        if tail_slot != u16::MAX {
            func.instruction(&Instruction::LocalGet(scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(list_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: list_idx,
                field_index: 1,
            });
            func.instruction(&Instruction::LocalSet(tail_slot as u32));
        }
    }
    emit_expr(func, &cons_arm.body, slots, ctx)?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// `match res { Result.Ok(v) -> a; Result.Err(e) -> b }` — tag
/// dispatch on field 0 (i32), bind v from field 1 (T) or e from
/// field 2 (E).
pub(super) fn emit_result_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arms: &[MatchArm],
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Result match needs a subject scratch slot but none was reserved".into(),
    ))?;
    let subject_ty = aver_type_str_of(subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let res_idx = ctx
        .registry
        .result_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Result match: subject type `{subject_ty}` is not a registered Result<T,E>"
        )))?;

    let mut ok_arm: Option<&MatchArm> = None;
    let mut err_arm: Option<&MatchArm> = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Constructor(name, _) => {
                let bare = name.rsplit('.').next().unwrap_or(name);
                if bare == "Ok" {
                    ok_arm = Some(arm);
                } else if bare == "Err" {
                    err_arm = Some(arm);
                }
            }
            Pattern::Wildcard => {
                if err_arm.is_none() {
                    err_arm = Some(arm);
                } else if ok_arm.is_none() {
                    ok_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let ok_arm = ok_arm.ok_or(WasmGcError::Validation(
        "Result match missing Ok arm".into(),
    ))?;
    let err_arm = err_arm.ok_or(WasmGcError::Validation(
        "Result match missing Err arm".into(),
    ))?;

    emit_expr(func, subject, slots, ctx)?;
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
    if let Pattern::Constructor(_, _) = &ok_arm.pattern
        && let Some(arm_slots) = ok_arm.binding_slots.get()
        && let Some(&slot) = arm_slots.first()
        && slot != u16::MAX
    {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: res_idx,
            field_index: 1,
        });
        func.instruction(&Instruction::LocalSet(slot as u32));
    }
    emit_expr(func, &ok_arm.body, slots, ctx)?;
    func.instruction(&Instruction::Else);
    if let Pattern::Constructor(_, _) = &err_arm.pattern
        && let Some(arm_slots) = err_arm.binding_slots.get()
        && let Some(&slot) = arm_slots.first()
        && slot != u16::MAX
    {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(res_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: res_idx,
            field_index: 2,
        });
        func.instruction(&Instruction::LocalSet(slot as u32));
    }
    emit_expr(func, &err_arm.body, slots, ctx)?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// Fused `match Map.get(m, k) { Option.Some(v) -> body1; Option.None
/// -> body2 }` — calls the per-(K,V) `get_pair` helper which returns
/// `(i32 found, V value)` as a multi-result. The caller pops `value`
/// into the binding slot, then branches on `found`. Never allocates
/// Option<V>; same probe loop runs but its result lands directly on
/// the wasm stack.
pub(super) fn emit_map_get_match_fused(
    func: &mut Function,
    map: &Spanned<Expr>,
    key: &Spanned<Expr>,
    arms: &[MatchArm],
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let map_aver = aver_type_str_of(map);
    let canonical: String = map_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let helpers = ctx
        .fn_map
        .map_helpers
        .get(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.get match fusion: map type `{map_aver}` has no helpers"
        )))?;

    // Locate the Some / None arms (wildcard counts as None catch-all).
    let mut some_arm: Option<&MatchArm> = None;
    let mut none_arm: Option<&MatchArm> = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Constructor(name, _) => {
                let bare = name.rsplit('.').next().unwrap_or(name);
                if bare == "Some" {
                    some_arm = Some(arm);
                } else if bare == "None" {
                    none_arm = Some(arm);
                }
            }
            Pattern::Wildcard => {
                if none_arm.is_none() {
                    none_arm = Some(arm);
                } else if some_arm.is_none() {
                    some_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let some_arm = some_arm.ok_or(WasmGcError::Validation(
        "Map.get match fusion missing Some arm".into(),
    ))?;
    let none_arm = none_arm.ok_or(WasmGcError::Validation(
        "Map.get match fusion missing None arm".into(),
    ))?;

    emit_expr(func, map, slots, ctx)?;
    emit_expr(func, key, slots, ctx)?;
    func.instruction(&Instruction::Call(helpers.get_pair));
    // Stack now: [..., found(i32), value(V)]. Pop V into the Some
    // binding slot (if any); the value is harmlessly dead in the
    // None branch (we always pop, regardless of which arm fires —
    // wasm requires a balanced stack across the branch boundary).
    if let Pattern::Constructor(_, _) = &some_arm.pattern
        && let Some(arm_slots) = some_arm.binding_slots.get()
        && let Some(&slot) = arm_slots.first()
        && slot != u16::MAX
    {
        func.instruction(&Instruction::LocalSet(slot as u32));
    } else {
        // No binding (or wildcard) — drop the value.
        func.instruction(&Instruction::Drop);
    }
    // Stack: [..., found(i32)]. Branch.
    func.instruction(&Instruction::If(block_ty));
    emit_expr(func, &some_arm.body, slots, ctx)?;
    func.instruction(&Instruction::Else);
    emit_expr(func, &none_arm.body, slots, ctx)?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// `match opt { Option.Some(v) -> ...; Option.None -> ... }` —
/// tag-based dispatch on the Option struct's first field.
///
/// Strategy:
/// 1. Stash subject ref in the per-fn scratch slot (`(ref null eq)`).
/// 2. Emit the test: `local.get scratch; ref.cast (ref $option_T);
///    struct.get $option_T 0; i32.const 1; i32.eq` — true if Some.
/// 3. `if/else`: Some arm extracts value into its bound slot via
///    `struct.get $option_T 1`; None arm emits its body directly.
pub(super) fn emit_option_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arms: &[MatchArm],
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Option match needs a subject scratch slot but none was reserved".into(),
    ))?;

    // Resolve the canonical `Option<T>` and its slot from the subject's
    // inferred Aver type.
    let subject_ty = aver_type_str_of(subject);
    let canonical: String = subject_ty.chars().filter(|c| !c.is_whitespace()).collect();
    let opt_idx = ctx
        .registry
        .option_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option match: subject type `{subject_ty}` is not a registered Option<T>"
        )))?;

    // Locate Some / None arms. Wildcard arm acts as the None
    // catch-all — same convention the variant dispatcher uses.
    let mut some_arm: Option<&MatchArm> = None;
    let mut none_arm: Option<&MatchArm> = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Constructor(name, _) => {
                let bare = name.rsplit('.').next().unwrap_or(name);
                if bare == "Some" {
                    some_arm = Some(arm);
                } else if bare == "None" {
                    none_arm = Some(arm);
                }
            }
            Pattern::Wildcard => {
                if none_arm.is_none() {
                    none_arm = Some(arm);
                } else if some_arm.is_none() {
                    some_arm = Some(arm);
                }
            }
            _ => {}
        }
    }
    let some_arm = some_arm.ok_or(WasmGcError::Validation(
        "Option match missing Some arm".into(),
    ))?;
    let none_arm = none_arm.ok_or(WasmGcError::Validation(
        "Option match missing None arm".into(),
    ))?;

    // Stash subject in scratch, then test tag.
    emit_expr(func, subject, slots, ctx)?;
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

    // Some branch: extract value into the bound slot (if any), then
    // emit body.
    if let Pattern::Constructor(_, _) = &some_arm.pattern
        && let Some(arm_slots) = some_arm.binding_slots.get()
        && let Some(&slot) = arm_slots.first()
        && slot != u16::MAX
    {
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(opt_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: opt_idx,
            field_index: 1,
        });
        func.instruction(&Instruction::LocalSet(slot as u32));
    }
    emit_expr(func, &some_arm.body, slots, ctx)?;

    func.instruction(&Instruction::Else);
    emit_expr(func, &none_arm.body, slots, ctx)?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// Lower a multi-arm `match subject { Foo.A(...) -> a; Foo.B(...) -> b; ... }`
/// to a `ref.test (ref $variant_idx)` cascade. Subject is stashed in
/// the per-fn scratch slot once; each arm's `ref.test` reads from it,
/// then the matched arm's body emits with bindings extracted via
/// `ref.cast` + `struct.get`.
///
/// The last arm is treated as the default ("else of last ref.test")
/// — the type checker has proven exhaustiveness, so an unmatched
/// subject is impossible at runtime. Wildcard arms work the same way.
/// Cascade of string-equality compares for `match s { "lit" -> body; ... }`.
/// One arm at a time: stash subject in scratch, push (subject, literal),
/// call `__wasmgc_string_eq`, branch on the i32 result. Wildcard /
/// catch-all is the final else.
pub(super) fn emit_string_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arms: &[MatchArm],
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
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
    let read_subject = |func: &mut Function| {
        func.instruction(&Instruction::LocalGet(scratch));
        // scratch is `(ref null eq)` — every wasm-gc struct/array
        // subtypes it. Cast back to `(ref null $string)` for
        // `__wasmgc_string_eq`'s param shape.
        func.instruction(&Instruction::RefCastNullable(
            wasm_encoder::HeapType::Concrete(s_idx),
        ));
    };

    // Stash subject; we read it once per arm.
    emit_expr(func, subject, slots, ctx)?;
    func.instruction(&Instruction::LocalSet(scratch));

    // Split arms: literal-string arms first (in source order), then
    // a single default (wildcard or non-literal pattern). The type
    // checker already proved exhaustivity.
    let mut literal_arms: Vec<(&str, &MatchArm)> = Vec::new();
    let mut default_arm: Option<&MatchArm> = None;
    for arm in arms {
        if let Pattern::Literal(Literal::Str(s)) = &arm.pattern {
            literal_arms.push((s.as_str(), arm));
        } else if default_arm.is_none() {
            default_arm = Some(arm);
        }
    }
    let default_arm = default_arm.ok_or(WasmGcError::Validation(
        "String match without a default arm — type checker should have rejected".into(),
    ))?;

    // Cascade: emit one `if (eq subj literal) { body } else { ... }` per arm.
    for _ in &literal_arms {
        // The if's else branch lifts; we need one End per opened If.
    }
    let mut ends_to_close = 0usize;
    for (lit, arm) in &literal_arms {
        // `eq(subject, literal)` — read the cast subject + literal
        read_subject(func);
        // Emit literal as a String (passive data segment lookup).
        emit_string_literal_bytes(func, lit.as_bytes(), ctx)?;
        func.instruction(&Instruction::Call(eq_idx));
        func.instruction(&Instruction::If(block_ty));
        // Bind the variable if the pattern has one (string match
        // patterns don't usually bind, so this is a no-op).
        emit_expr(func, &arm.body, slots, ctx)?;
        func.instruction(&Instruction::Else);
        ends_to_close += 1;
    }
    // Default body in the innermost else. Aver's `_` (Wildcard)
    // binds nothing; named bindings (`x -> body`) on a String match
    // would need the resolver to surface a slot, which the current
    // pattern shape doesn't expose. Surface forms in app.av use `_`,
    // so we don't carry the name-binding case here.
    emit_expr(func, &default_arm.body, slots, ctx)?;
    for _ in 0..ends_to_close {
        func.instruction(&Instruction::End);
    }
    Ok(())
}

/// Push the caller-fn idx as an `i32` immediate. Lazy-registers the
/// current fn name with the collector so the post-emit phase knows
/// exactly which fn names to put in the exported caller-fn table.
/// Single source of truth: any code path that wants to label its
/// effect call site with the originating fn just calls this — no
/// AST walker, no rozjazdy walker↔codegen. Hot path: 2-3 bytes
/// (`i32.const <idx>`), zero allocation.
pub(super) fn emit_caller_fn_idx(
    func: &mut Function,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let idx = ctx
        .caller_fn_collector
        .borrow_mut()
        .register(ctx.self_fn_name);
    func.instruction(&Instruction::I32Const(idx as i32));
    Ok(())
}

/// Push a `(ref null $string)` onto the wasm stack from a UTF-8 byte
/// slice. Looks up the literal in the registry's passive-segment
/// table; the segment is intern-ed by `collect_string_literals_in_*`
/// during pre-emit discovery.
pub(super) fn emit_string_literal_bytes(
    func: &mut Function,
    bytes: &[u8],
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let segment_idx = ctx
        .registry
        .string_literal_segment(bytes)
        .ok_or(WasmGcError::Validation(format!(
            "String literal `{:?}` was not registered in the data segment table",
            std::str::from_utf8(bytes).unwrap_or("<non-utf8>")
        )))?;
    let s_idx = ctx
        .registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "String literal needs string slot allocated".into(),
        ))?;
    func.instruction(&Instruction::I32Const(0)); // offset
    func.instruction(&Instruction::I32Const(bytes.len() as i32));
    func.instruction(&Instruction::ArrayNewData {
        array_type_index: s_idx,
        array_data_index: segment_idx,
    });
    Ok(())
}

pub(super) fn emit_variant_dispatch(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arms: &[MatchArm],
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "multi-arm variant match needs a subject scratch slot but none was reserved".into(),
    ))?;

    // Stash subject in scratch.
    emit_expr(func, subject, slots, ctx)?;
    func.instruction(&Instruction::LocalSet(scratch));

    emit_variant_arm_cascade(func, arms, block_ty, scratch, slots, ctx)
}

pub(super) fn emit_variant_arm_cascade(
    func: &mut Function,
    arms: &[MatchArm],
    block_ty: wasm_encoder::BlockType,
    subject_scratch: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if arms.is_empty() {
        // Type-checker has proven exhaustiveness; reaching here means
        // the match has no arms at all. Emit `unreachable` so the
        // wasm validator's stack-shape inference treats this branch
        // as polymorphic.
        func.instruction(&Instruction::Unreachable);
        return Ok(());
    }

    // If only one arm left, emit it as the "default" — no test
    // needed. Wildcards and trailing Constructor arms both fall here.
    if arms.len() == 1 {
        return emit_arm_body(func, &arms[0], subject_scratch, slots, ctx);
    }

    // Otherwise: ref.test against the first arm's variant. If true,
    // emit its body. Else recurse on the rest.
    let arm = &arms[0];
    match &arm.pattern {
        Pattern::Constructor(name, _) => {
            let bare = name.rsplit('.').next().unwrap_or(name);
            let parent_hint = name.rsplit_once('.').map(|(p, _)| p);
            // Prefer the parent-disambiguated lookup so two sumtypes
            // sharing a bare variant name (e.g. `Query.ProviderSummary`
            // and `QueryOutput.ProviderSummary` in payment_ops) pick
            // up their own concrete struct idx instead of whichever
            // sumtype's TypeDef appeared first.
            let info = parent_hint
                .and_then(|p| ctx.registry.variant_in(p, bare))
                .or_else(|| ctx.registry.variant(bare))
                .ok_or(WasmGcError::Validation(format!(
                    "unknown variant `{name}` in match"
                )))?;
            func.instruction(&Instruction::LocalGet(subject_scratch));
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(info.type_idx),
            ));
            func.instruction(&Instruction::If(block_ty));
            emit_arm_body(func, arm, subject_scratch, slots, ctx)?;
            func.instruction(&Instruction::Else);
            emit_variant_arm_cascade(func, &arms[1..], block_ty, subject_scratch, slots, ctx)?;
            func.instruction(&Instruction::End);
        }
        Pattern::Wildcard => {
            // Wildcard before the end — just emit it (rest unreachable).
            return emit_arm_body(func, arm, subject_scratch, slots, ctx);
        }
        _ => {
            return Err(WasmGcError::Unimplemented(
                "phase 3b — non-Constructor pattern in multi-arm variant match",
            ));
        }
    }
    Ok(())
}

/// Emit one match-arm body, including any pattern-binding extraction.
/// `subject_scratch` holds the original subject (eq ref); the arm's
/// pattern decides what to extract.
pub(super) fn emit_arm_body(
    func: &mut Function,
    arm: &MatchArm,
    subject_scratch: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if let Pattern::Constructor(name, bindings) = &arm.pattern {
        let bare = name.rsplit('.').next().unwrap_or(name);
        let parent_hint = name.rsplit_once('.').map(|(p, _)| p);
        let info = parent_hint
            .and_then(|p| ctx.registry.variant_in(p, bare))
            .or_else(|| ctx.registry.variant(bare))
            .ok_or(WasmGcError::Validation(format!(
                "unknown variant `{name}` in match"
            )))?;
        // Per-arm binding slots from the resolver. Pattern bindings
        // never land in the function-level `local_slots` map; they
        // live in `MatchArm::binding_slots` so two arms can share a
        // binding name (e.g. `deadline` showing up in both
        // `TaskCreated` and `DeadlineSet` with different field types)
        // without collision.
        let arm_slots = arm.binding_slots.get().ok_or(WasmGcError::Validation(
            "match arm reached emit without resolver-allocated binding_slots".into(),
        ))?;
        // Newtype: subject IS the underlying primitive — read the
        // scratch directly. (Won't happen here in practice because
        // newtype matches go through the single-arm path, but
        // handle it for symmetry.)
        if ctx.registry.newtype_underlying(&info.parent).is_some() && bindings.len() == 1 {
            let slot = arm_slots[0];
            if slot != u16::MAX {
                func.instruction(&Instruction::LocalGet(subject_scratch));
                func.instruction(&Instruction::LocalSet(slot as u32));
            }
            return emit_expr(func, &arm.body, slots, ctx);
        }
        // Extract each field into its bound slot.
        for (i, _binding_name) in bindings.iter().enumerate() {
            let slot = arm_slots[i];
            if slot == u16::MAX {
                continue;
            }
            let slot = slot as u32;
            func.instruction(&Instruction::LocalGet(subject_scratch));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(info.type_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: info.type_idx,
                field_index: i as u32,
            });
            func.instruction(&Instruction::LocalSet(slot));
        }
        return emit_expr(func, &arm.body, slots, ctx);
    }
    // Wildcard / non-pattern arms: just emit body.
    emit_expr(func, &arm.body, slots, ctx)
}

/// Lower a single-arm `match subject { Variant(bindings) -> body }`.
/// Used for newtype-style sum types: cast subject down to the concrete
/// variant struct, extract each field into its bound local, then emit
/// the body. No dispatch required — the type checker has already
/// proven this is the only variant the subject can be.
///
/// Multi-arm variant matches need `ref.test` + cascading branches; that
/// lands in phase 3b.
pub(super) fn emit_single_variant_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arm: &MatchArm,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let (constructor, bindings) = match &arm.pattern {
        Pattern::Constructor(c, b) => (c.as_str(), b.as_slice()),
        _ => {
            return Err(WasmGcError::Validation(
                "emit_single_variant_match called on non-Constructor pattern".into(),
            ));
        }
    };
    let body = arm.body.as_ref();
    let arm_slots = arm.binding_slots.get().ok_or(WasmGcError::Validation(
        "single-arm match reached emit without resolver-allocated binding_slots".into(),
    ))?;
    // Constructor names in patterns are dotted (e.g. `UserId.UserId`);
    // the registry stores by the bare variant name.
    let bare = constructor.rsplit('.').next().unwrap_or(constructor);
    let parent_hint = constructor.rsplit_once('.').map(|(p, _)| p);
    let info = parent_hint
        .and_then(|p| ctx.registry.variant_in(p, bare))
        .or_else(|| ctx.registry.variant(bare))
        .ok_or(WasmGcError::Validation(format!(
            "unknown variant `{constructor}` in match pattern"
        )))?;
    if bindings.len() != info.fields.len() {
        return Err(WasmGcError::Validation(format!(
            "variant `{constructor}` has {} field(s) but pattern binds {}",
            info.fields.len(),
            bindings.len()
        )));
    }

    // Newtype optimization: single-variant sum of single primitive →
    // pattern match is just "bind subject to the binding". No cast,
    // no struct.get.
    if ctx.registry.newtype_underlying(&info.parent).is_some() && bindings.len() == 1 {
        let slot = arm_slots[0];
        emit_expr(func, subject, slots, ctx)?;
        if slot != u16::MAX {
            func.instruction(&Instruction::LocalSet(slot as u32));
        } else {
            func.instruction(&Instruction::Drop);
        }
        emit_expr(func, body, slots, ctx)?;
        return Ok(());
    }

    // Phase 3a: single-variant sum types only. `ref.cast` lets us
    // narrow `(ref null eq)` to the concrete variant struct.
    let variant_idx = info.type_idx;
    let cast_ty = wasm_encoder::HeapType::Concrete(variant_idx);

    if bindings.is_empty() {
        // Nullary constructor — body doesn't need any binds, just
        // emit it. Subject still needs to be evaluated for side
        // effects, then dropped.
        emit_expr(func, subject, slots, ctx)?;
        func.instruction(&Instruction::Drop);
        emit_expr(func, body, slots, ctx)?;
        return Ok(());
    }

    // Stash the cast subject in a fresh local slot so we can
    // struct.get each field without recomputing.
    emit_expr(func, subject, slots, ctx)?;
    func.instruction(&Instruction::RefCastNonNull(cast_ty));
    // We need a slot to hold the cast ref. Use a synthetic one — the
    // resolver doesn't allocate extra slots for the implicit cast,
    // but we know the wasm fn has a final scratch slot we can declare.
    // Rather than mutate `slots` mid-emit, we use the binding slots
    // directly: extract each field into its corresponding binding
    // slot and discard the cast ref afterwards. To do that, we need
    // the cast ref on the stack N times (once per binding). Easiest:
    // pre-stash by using local 0 of the variant_idx type… but we
    // can't declare locals here.
    //
    // Workaround: extract directly while consuming the cast each
    // time. wasm doesn't give us a "dup", but `local.tee` writes to
    // a local AND leaves the value on stack. So:
    //
    //   subject → cast → local.tee $b0
    //   struct.get 0   ;; field 0 → on stack
    //   local.set $b0  ;; bind n
    //
    // This works for single-binding case. For multiple bindings we'd
    // need a real scratch slot. Phase 3a covers the single-binding
    // newtype shape and rejects the rest.
    if bindings.len() == 1 {
        let slot = arm_slots[0];
        func.instruction(&Instruction::StructGet {
            struct_type_index: variant_idx,
            field_index: 0,
        });
        if slot != u16::MAX {
            func.instruction(&Instruction::LocalSet(slot as u32));
        } else {
            func.instruction(&Instruction::Drop);
        }
        emit_expr(func, body, slots, ctx)?;
        return Ok(());
    }

    // Multi-binding case: stash the cast subject in the SlotTable's
    // subject_scratch slot, then `struct.get + local.set` once per
    // binding. The scratch slot is `(ref null eq)`, so we re-cast on
    // every read — wasm-gc has no `local.tee` shortcut for refs that
    // would skip this.
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "multi-binding variant pattern needs subject_scratch but none was reserved \
         (slots::expr_needs_scratch must opt in for any Constructor pattern)"
            .into(),
    ))?;
    // We're already past `subject → ref.cast (ref variant_idx)` — keep
    // that on the stack, drop it through the scratch (typed eqref via
    // upcast: every wasm-gc struct subtypes eq).
    func.instruction(&Instruction::LocalSet(scratch));
    for (i, _binding_name) in bindings.iter().enumerate() {
        let slot = arm_slots[i];
        if slot == u16::MAX {
            continue;
        }
        func.instruction(&Instruction::LocalGet(scratch));
        func.instruction(&Instruction::RefCastNonNull(cast_ty));
        func.instruction(&Instruction::StructGet {
            struct_type_index: variant_idx,
            field_index: i as u32,
        });
        func.instruction(&Instruction::LocalSet(slot as u32));
    }
    emit_expr(func, body, slots, ctx)?;
    Ok(())
}

pub(super) fn emit_constructor_with_args(
    func: &mut Function,
    info: &super::super::types::VariantInfo,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if args.len() != info.fields.len() {
        return Err(WasmGcError::Validation(format!(
            "variant has {} field(s) but call supplied {}",
            info.fields.len(),
            args.len()
        )));
    }
    // Newtype optimization for single-payload single-variant sums:
    // skip struct.new — emit the payload directly.
    if ctx.registry.newtype_underlying(&info.parent).is_some() {
        return emit_expr(func, &args[0], slots, ctx);
    }
    for arg in args {
        emit_expr(func, arg, slots, ctx)?;
    }
    func.instruction(&Instruction::StructNew(info.type_idx));
    Ok(())
}

/// Lower `Constructor(name, Some(payload))` or nullary `Constructor(name, None)`
/// to `struct.new $variant_type_idx`. Variants are positional (no field
/// names), so payload values are pushed in source order before
/// `struct.new`.
pub(super) fn emit_constructor(
    func: &mut Function,
    outer: &Spanned<Expr>,
    name: &str,
    payload: Option<&Spanned<Expr>>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    // Built-in `Option` constructors don't go through `TypeRegistry`'s
    // variant table (Option isn't a TypeDef). Route them to the
    // dedicated emitter that picks the right monomorphised slot.
    let bare = name.rsplit('.').next().unwrap_or(name);
    if name == "Option.Some" || (bare == "Some" && name.starts_with("Option")) {
        return emit_option_constructor(func, payload, None, slots, ctx);
    }
    if name == "Option.None" || (bare == "None" && name.starts_with("Option")) {
        // Read T off the typed outer expr (Option<T> stamped by the
        // type checker), with the Type::Invalid-recovery reader so
        // post-error gradual-typing branches still resolve.
        let stamped_canonical = aver_type_canonical(outer, ctx.return_type, ctx.registry);
        let hint: String = if let Some(inner) = stamped_canonical
            .strip_prefix("Option<")
            .and_then(|s| s.strip_suffix('>'))
        {
            inner.to_string()
        } else {
            ctx.return_type.to_string()
        };
        return emit_option_constructor(func, None, Some(&hint), slots, ctx);
    }
    if name == "Result.Ok" || (bare == "Ok" && name.starts_with("Result")) {
        return emit_result_constructor(func, "Ok", payload, slots, ctx);
    }
    if name == "Result.Err" || (bare == "Err" && name.starts_with("Result")) {
        return emit_result_constructor(func, "Err", payload, slots, ctx);
    }
    // Constructor names can be dotted (`Foo.Bar`) or bare (`Bar`).
    // Prefer the parent-disambiguated lookup when the dotted form is
    // available so two sumtypes sharing a bare variant name don't
    // collide.
    let bare = name.rsplit('.').next().unwrap_or(name);
    let parent_hint = name.rsplit_once('.').map(|(p, _)| p);
    let info = parent_hint
        .and_then(|p| ctx.registry.variant_in(p, bare))
        .or_else(|| ctx.registry.variant(bare))
        .ok_or(WasmGcError::Validation(format!(
            "unknown variant constructor `{name}`"
        )))?;
    // Aver's AST treats single-payload constructors as `Some(expr)` —
    // multi-field variants come through as `Some(Tuple(...))`. Phase
    // 3a only handles the single-payload case directly; tuple-payload
    // variants need phase 3b (Tuple lowering) to come online.
    let payload_count = info.fields.len();
    if payload_count == 0 {
        // Nullary constructor — empty struct.
        func.instruction(&Instruction::StructNew(info.type_idx));
        return Ok(());
    }
    if payload_count > 1 {
        return Err(WasmGcError::Unimplemented(
            "phase 3b — multi-field variant constructors (need Tuple lowering)",
        ));
    }
    let payload = payload.ok_or(WasmGcError::Validation(format!(
        "variant `{name}` expects 1 payload but got 0"
    )))?;
    emit_expr(func, payload, slots, ctx)?;
    func.instruction(&Instruction::StructNew(info.type_idx));
    Ok(())
}

/// Lower `match subject { arm0; arm1; ...; default }` into a cascade
/// of `if`/`else` blocks. Phase-4 shape:
/// - subject must be `Int` or `Bool`,
/// - patterns are `Literal(Int|Bool)` or `Wildcard`,
/// - exactly one wildcard, at the end (the type checker has already
///   verified exhaustiveness, so this is a structural simplification).
///
/// Strategy: stash the subject in a fresh local slot, then for each
/// non-wildcard arm emit `local.get $subj; <pat-const>; eq; (if … else)`.
/// The wildcard arm runs in the innermost `else`. Same shape works for
/// Bool subjects (single `if` over the boolean).
pub(super) fn emit_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arms: &[MatchArm],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if arms.is_empty() {
        return Err(WasmGcError::Validation("match has no arms".into()));
    }
    // Match arms always agree on type by typecheck; pick the first one.
    // Generic arm bodies such as `[]` must already be resolved by the
    // type checker before codegen reaches this point.
    let result_ty_str =
        super::infer::aver_type_canonical(&arms[0].body, ctx.return_type, ctx.registry);
    let result_wasm = aver_to_wasm(&result_ty_str, Some(ctx.registry))?;
    let block_ty = match result_wasm {
        Some(v) => wasm_encoder::BlockType::Result(v),
        None => wasm_encoder::BlockType::Empty,
    };

    // Bool subject — special-case to a single `if`/`else`. No subject
    // local needed (wasm `if` consumes the i32 directly).
    let subject_ty = aver_type_str_of(subject);
    if subject_ty == "Bool" {
        if arms.len() != 2 {
            return Err(WasmGcError::Unimplemented(
                "phase 4 — Bool match must have exactly 2 arms (true / false)",
            ));
        }
        // Find which arm is `true` and which is `false`. Wildcard
        // counts as the "other" branch.
        let mut true_body: Option<&Spanned<Expr>> = None;
        let mut false_body: Option<&Spanned<Expr>> = None;
        for arm in arms {
            match &arm.pattern {
                Pattern::Literal(Literal::Bool(true)) => true_body = Some(&arm.body),
                Pattern::Literal(Literal::Bool(false)) => false_body = Some(&arm.body),
                Pattern::Wildcard => {
                    if true_body.is_none() {
                        true_body = Some(&arm.body);
                    } else {
                        false_body = Some(&arm.body);
                    }
                }
                _ => {
                    return Err(WasmGcError::Unimplemented(
                        "phase 4 — Bool match supports only Bool literals + wildcard",
                    ));
                }
            }
        }
        let t = true_body.ok_or(WasmGcError::Validation(
            "Bool match missing true arm".into(),
        ))?;
        let f = false_body.ok_or(WasmGcError::Validation(
            "Bool match missing false arm".into(),
        ))?;
        emit_expr(func, subject, slots, ctx)?;
        func.instruction(&Instruction::If(block_ty));
        emit_expr(func, t, slots, ctx)?;
        func.instruction(&Instruction::Else);
        emit_expr(func, f, slots, ctx)?;
        func.instruction(&Instruction::End);
        return Ok(());
    }

    // List match — `[] -> ...; [head, ..tail] -> ...`. Subject is
    // `(ref null $list_T)`; empty = ref.is_null, cons = struct.get
    // head/tail.
    if arms
        .iter()
        .any(|a| matches!(&a.pattern, Pattern::EmptyList | Pattern::Cons(_, _)))
    {
        return emit_list_match(func, subject, arms, block_ty, slots, ctx);
    }

    // Built-in `Result<T, E>` match — tag-based, two payload fields.
    if arms.iter().any(arm_is_result_pattern) {
        return emit_result_match(func, subject, arms, block_ty, slots, ctx);
    }

    // Built-in `Option<T>` match — tag-based dispatch on the struct's
    // first field. Detected up-front because Option isn't in the
    // user-variant table and the subject is always a (ref null
    // $option_T).
    if arms.iter().any(arm_is_option_pattern) {
        // Fused shape: `match Map.get(m, k) { Option.Some(v) -> ...;
        // Option.None -> ... }` lowers via the per-(K,V) get_pair
        // helper — multi-result `(found, value)` return — without
        // ever allocating an Option<V>.
        if let Expr::FnCall(callee, fn_args) = &subject.node
            && let Expr::Attr(parent, member) = &callee.node
            && let Expr::Ident(p) = &parent.node
            && p == "Map"
            && member == "get"
            && fn_args.len() == 2
        {
            return emit_map_get_match_fused(
                func,
                &fn_args[0],
                &fn_args[1],
                arms,
                block_ty,
                slots,
                ctx,
            );
        }
        return emit_option_match(func, subject, arms, block_ty, slots, ctx);
    }

    // Tuple match — single arm `(a, b, ..., n) -> ...`. Subject is a
    // `(ref null $tuple_A_B_..._N)`; struct.get its fields into
    // bindings. Variadic arity (any N >= 2).
    if arms.len() == 1
        && let Pattern::Tuple(items) = &arms[0].pattern
        && items.len() >= 2
    {
        return emit_tuple_match(func, subject, &arms[0], slots, ctx);
    }

    // Multi-arm tuple-of-constructors — `match flatFail() { (Result.Ok(a),
    // Result.Err(e), Result.Ok(c)) -> ...; _ -> ... }`. Per-element tag
    // tests AND'd into one verdict; bindings extract from the
    // per-element variant struct on the matching branch.
    if arms.iter().any(|a| {
        matches!(&a.pattern, Pattern::Tuple(items)
            if items.iter().any(|i| matches!(i, Pattern::Constructor(_, _))))
    }) && arms.last().is_some_and(|a| {
        matches!(
            &a.pattern,
            Pattern::Wildcard | Pattern::Ident(_) | Pattern::Tuple(_)
        )
    }) {
        return emit_tuple_constructor_match(func, subject, arms, block_ty, slots, ctx);
    }

    // Single-arm Constructor pattern — `match obj { Foo.Bar(n) -> body }`.
    // Common in newtype-style sum types; cast + extract directly without
    // a dispatch test.
    if arms.len() == 1
        && let Pattern::Constructor(_, _) = &arms[0].pattern
    {
        return emit_single_variant_match(func, subject, &arms[0], slots, ctx);
    }

    // Multi-arm Constructor patterns — emit a `ref.test` dispatch
    // cascade against the variant struct types.
    let has_constructor_arm = arms
        .iter()
        .any(|a| matches!(a.pattern, Pattern::Constructor(_, _)));
    if has_constructor_arm {
        return emit_variant_dispatch(func, subject, arms, block_ty, slots, ctx);
    }

    // String subject — `match path { "/" -> ...; "/api" -> ...; _ -> ... }`.
    // Cascade of `__wasmgc_string_eq(subject, "literal")`. Wildcard /
    // catch-all goes to the else branch.
    if subject_ty == "String" {
        return emit_string_match(func, subject, arms, block_ty, slots, ctx);
    }

    if subject_ty != "Int" {
        return Err(WasmGcError::Unimplemented(
            "phase 3b — match subject must be Int / Bool / sum type",
        ));
    }

    // Int subject — cascade. We need a subject scratch local; phase 4
    // hasn't reserved one ahead of time, so we synthesise one here as
    // a fresh slot in the *current* table. The caller already finished
    // local declarations, but `Function` accepts arbitrary local
    // indices ≤ count — module.rs handles count via the dry-run pass.
    //
    // Practical limitation today: we can't grow `slots` mid-emit
    // because slot allocation lives in module.rs's two-pass build.
    // Workaround: use a trailing scratch slot reserved by the
    // module-level pre-pass — simpler approach is to recompute the
    // subject expression at each comparison. Subjects are typically
    // a single `local.get` so the cost is one instruction per arm.
    //
    // This keeps phase 4 contained — phase 5 cleanup can switch to a
    // proper temp-local once we add a per-fn local-allocator.
    let mut wildcard_body: Option<&Spanned<Expr>> = None;
    let mut typed_arms: Vec<(i64, &Spanned<Expr>)> = Vec::new();
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Int(n)) => typed_arms.push((*n, &arm.body)),
            Pattern::Wildcard => wildcard_body = Some(&arm.body),
            _ => {
                return Err(WasmGcError::Unimplemented(
                    "phase 4 — Int match supports only Int literal patterns + wildcard",
                ));
            }
        }
    }
    let wildcard = wildcard_body.ok_or(WasmGcError::Unimplemented(
        "phase 4 — Int match without wildcard (exhaustive Int matching needs phase 5)",
    ))?;

    emit_int_match_cascade(func, subject, &typed_arms, wildcard, block_ty, slots, ctx)?;
    Ok(())
}

pub(super) fn emit_int_match_cascade(
    func: &mut Function,
    subject: &Spanned<Expr>,
    typed_arms: &[(i64, &Spanned<Expr>)],
    wildcard: &Spanned<Expr>,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if typed_arms.is_empty() {
        // No typed arms left — just emit wildcard.
        emit_expr(func, wildcard, slots, ctx)?;
        return Ok(());
    }
    let (pat_lit, body) = typed_arms[0];
    emit_expr(func, subject, slots, ctx)?;
    func.instruction(&Instruction::I64Const(pat_lit));
    func.instruction(&Instruction::I64Eq);
    func.instruction(&Instruction::If(block_ty));
    emit_expr(func, body, slots, ctx)?;
    func.instruction(&Instruction::Else);
    emit_int_match_cascade(
        func,
        subject,
        &typed_arms[1..],
        wildcard,
        block_ty,
        slots,
        ctx,
    )?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// Lower `Expr::TailCall { target, args }` into a native wasm tail
/// call. For a self-recursive call (target == current fn), emit
/// `return_call $self`. Mutual TCO across SCC peers is a phase-4b
/// extension that wires a function table; today it surfaces as
/// `Unimplemented` so the user sees a clear bump line.
pub(super) fn emit_tail_call(
    func: &mut Function,
    target: &str,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let entry = ctx
        .fn_map
        .by_name
        .get(target)
        .ok_or(WasmGcError::Validation(format!(
            "tail call to unknown fn `{target}`"
        )))?;
    for arg in args {
        emit_expr(func, arg, slots, ctx)?;
    }
    // `AVER_WASM_GC_NO_TAIL_CALL=1` swaps `return_call` for a plain
    // `call` + fall-through return — used to A/B whether the
    // tail-call proposal is doing meaningful work on a given bench.
    // Deep recursion will trash the stack with this on; only flip it
    // for shallow scenarios.
    let no_tail_call = std::env::var_os("AVER_WASM_GC_NO_TAIL_CALL").is_some();
    let target_idx = if target == ctx.self_fn_name {
        ctx.self_wasm_idx
    } else {
        entry.wasm_idx
    };
    if no_tail_call {
        func.instruction(&Instruction::Call(target_idx));
    } else {
        func.instruction(&Instruction::ReturnCall(target_idx));
    }
    Ok(())
}
