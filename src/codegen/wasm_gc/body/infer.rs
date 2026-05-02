//! Aver-type inference helpers used by both the slot pre-pass and the
//! emit pipeline. Split out of `body.rs` — pure code movement, no
//! logic changes.

use std::collections::HashMap;

use wasm_encoder::ValType;

use crate::ast::{BinOp, Expr, FnDef, Literal, MatchArm, Pattern};

use super::super::WasmGcError;
use super::super::types::TypeRegistry;
use super::slots::{infer_expr_wasm_type, lookup_var_type};
use super::{EmitCtx, FnMap, SlotTable};

/// True when a match arm matches against `Option.Some(_)` or
/// `Option.None`. Used to opt the surrounding match into the
/// dedicated tag-based dispatch path (instead of the generic
/// `ref.test` cascade for user variants).
pub(super) fn arm_is_option_pattern(arm: &MatchArm) -> bool {
    if let Pattern::Constructor(name, _) = &arm.pattern {
        let bare = name.rsplit('.').next().unwrap_or(name);
        return name == "Option.Some"
            || name == "Option.None"
            || ((bare == "Some" || bare == "None") && name.starts_with("Option"));
    }
    false
}

/// True when a match arm targets `Result.Ok(_)` or `Result.Err(_)`.
pub(super) fn arm_is_result_pattern(arm: &MatchArm) -> bool {
    if let Pattern::Constructor(name, _) = &arm.pattern {
        let bare = name.rsplit('.').next().unwrap_or(name);
        return name == "Result.Ok"
            || name == "Result.Err"
            || ((bare == "Ok" || bare == "Err") && name.starts_with("Result"));
    }
    false
}

/// For an Option-shaped match subject, recover the inner T as an Aver
/// type string by reverse-looking-up the wasm type of the subject in
/// the registry's option table. Used by pre-pass slot allocation
/// (`Option.Some(v)` binds `v` to a slot whose wasm type depends on T).
/// For a Result-shaped match subject, recover (T, E) Aver type
/// strings via reverse-lookup on the registered result_order.
pub(super) fn subject_result_te(
    subject: &Expr,
    registry: &TypeRegistry,
    fd: &FnDef,
    fn_map: &FnMap,
) -> Result<(String, String), WasmGcError> {
    let wasm_ty = infer_expr_wasm_type(subject, registry, fd, fn_map)?;
    if let Some(ValType::Ref(rt)) = wasm_ty
        && let wasm_encoder::HeapType::Concrete(idx) = rt.heap_type
    {
        for canonical in &registry.result_order {
            if registry.result_types.get(canonical).copied() == Some(idx)
                && let Some((t, e)) = TypeRegistry::result_te(canonical)
            {
                return Ok((t.to_string(), e.to_string()));
            }
        }
    }
    Err(WasmGcError::Validation(
        "Result match subject's wasm type doesn't map to a registered Result<T,E>".into(),
    ))
}

/// Resolves the `T` of an `Option<T>` subject from its (possibly
/// nested) Aver-type form. Slot pre-pass
/// uses this so `match Map.get(headers, ...)` over a binding correctly
/// resolves to `Option<List<String>>` (sniff_with_prev sees `headers`
/// is `Map<String, List<String>>`); the bare `infer_expr_wasm_type`
/// path can't, because it only sees params.
pub(super) fn subject_option_inner_type_with_prev(
    subject: &Expr,
    registry: &TypeRegistry,
    fd: &FnDef,
    fn_map: &FnMap,
    prev: &HashMap<String, String>,
) -> Option<String> {
    let aver = sniff_with_prev(subject, fd, fn_map, registry, prev)?;
    let canonical: String = aver.chars().filter(|c| !c.is_whitespace()).collect();
    let inner = canonical.strip_prefix("Option<")?.strip_suffix('>')?;
    Some(inner.to_string())
}

/// Type inference over the limited shape phase 2/4 emits. Returns the
/// Aver type string. Errors on shapes that belong to a later phase,
/// with a message pointing at it.
pub(super) fn infer_aver_type(expr: &Expr, ctx: &EmitCtx<'_>) -> Result<String, WasmGcError> {
    match expr {
        Expr::Literal(Literal::Int(_)) => Ok("Int".into()),
        Expr::Literal(Literal::Float(_)) => Ok("Float".into()),
        Expr::Literal(Literal::Bool(_)) => Ok("Bool".into()),
        Expr::Literal(Literal::Unit) => Ok("Unit".into()),
        Expr::Literal(Literal::Str(_)) => Ok("String".into()),
        Expr::InterpolatedStr(_) => Ok("String".into()),
        Expr::Resolved { name, .. } => {
            // Look up the param/binding type. Falls back to "Int" only
            // if we can't recover the original aver type — most
            // bench scenarios bind only by name and we can find the
            // type via `lookup_var_type`.
            if let Some(ty) = lookup_var_type(name, ctx) {
                Ok(ty)
            } else {
                Ok("Int".into())
            }
        }
        Expr::Ident(_) => Ok("Int".into()),
        Expr::BinOp(op, l, _) => {
            // Comparisons always yield Bool; arithmetic preserves
            // operand type (Float + Float = Float).
            match op {
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                    Ok("Bool".into())
                }
                _ => infer_aver_type(&l.node, ctx),
            }
        }
        Expr::FnCall(callee, args) => {
            // Dotted callee: try variant constructor, then registered
            // builtin, then dotted name. Variants and builtins
            // determined by the parent (Type) name.
            if let Expr::Attr(parent, member) = &callee.node {
                if let Some(info) = ctx.registry.variant(member) {
                    return Ok(info.parent.clone());
                }
                if let Some(parent_name) = parent_dotted_head(&parent.node) {
                    let dotted = format!("{parent_name}.{member}");
                    if ctx.fn_map.builtins.contains_key(&dotted) {
                        return Ok(builtin_aver_result_type(&dotted).into());
                    }
                    if ctx.fn_map.effects.contains_key(&dotted) {
                        return Ok(effect_aver_return_type(&dotted).unwrap_or("Unit").into());
                    }
                    if dotted == "Float.fromInt" {
                        return Ok("Float".into());
                    }
                    if dotted == "Int.fromFloat" {
                        return Ok("Int".into());
                    }
                    // Inline-emitted builtins whose result type depends
                    // on argument types — `Vector.new(_, fill)` returns
                    // `Vector<{fill_type}>`; `Option.withDefault(_, d)`
                    // returns whatever the default's type is.
                    if dotted == "Vector.new" && args.len() == 2 {
                        let elem = infer_aver_type(&args[1].node, ctx)?;
                        return Ok(format!("Vector<{elem}>"));
                    }
                    if dotted == "Vector.set" && args.len() == 3 {
                        // Boxed return: `Option<Vector<T>>`. Used by
                        // pattern-match call sites; the fused
                        // `Option.withDefault(Vector.set(...), v)`
                        // shape collapses earlier in `emit_option_with_default`,
                        // so this only fires for non-fused calls.
                        let v = infer_aver_type(&args[0].node, ctx)?;
                        let canonical: String = v.chars().filter(|c| !c.is_whitespace()).collect();
                        if super::super::types::TypeRegistry::vector_element_type(&canonical)
                            .is_some()
                        {
                            return Ok(format!("Option<{canonical}>"));
                        }
                        return Ok(canonical);
                    }
                    if dotted == "Vector.toList" && args.len() == 1 {
                        let v = infer_aver_type(&args[0].node, ctx)?;
                        let canonical: String = v.chars().filter(|c| !c.is_whitespace()).collect();
                        if let Some(elem) =
                            super::super::types::TypeRegistry::vector_element_type(&canonical)
                        {
                            return Ok(format!("List<{}>", elem.trim()));
                        }
                        return Ok("List<Int>".into());
                    }
                    if dotted == "List.zip" && args.len() == 2 {
                        let la = infer_aver_type(&args[0].node, ctx)?;
                        let lb = infer_aver_type(&args[1].node, ctx)?;
                        let a = super::super::types::TypeRegistry::list_element_type(&la)
                            .map(str::trim)
                            .unwrap_or("Int");
                        let b = super::super::types::TypeRegistry::list_element_type(&lb)
                            .map(str::trim)
                            .unwrap_or("Int");
                        return Ok(format!("List<Tuple<{a},{b}>>"));
                    }
                    if (dotted == "Map.keys" || dotted == "Map.values") && args.len() == 1 {
                        let m = infer_aver_type(&args[0].node, ctx)?;
                        let canonical: String = m.chars().filter(|c| !c.is_whitespace()).collect();
                        if let Some((k, v)) = super::super::types::parse_map_kv(&canonical) {
                            let elem = if dotted == "Map.keys" { k } else { v };
                            return Ok(format!("List<{elem}>"));
                        }
                        return Ok("List<Int>".into());
                    }
                    if dotted == "Map.entries" && args.len() == 1 {
                        let m = infer_aver_type(&args[0].node, ctx)?;
                        let canonical: String = m.chars().filter(|c| !c.is_whitespace()).collect();
                        if let Some((k, v)) = super::super::types::parse_map_kv(&canonical) {
                            return Ok(format!("List<Tuple<{k},{v}>>"));
                        }
                        return Ok("List<Tuple<Int,Int>>".into());
                    }
                    if dotted == "Map.fromList" && args.len() == 1 {
                        let l = infer_aver_type(&args[0].node, ctx)?;
                        let canonical: String = l.chars().filter(|c| !c.is_whitespace()).collect();
                        if let Some(elem) = TypeRegistry::list_element_type(&canonical)
                            && let Some((k, v)) = TypeRegistry::tuple_ab(elem)
                        {
                            return Ok(format!("Map<{k},{v}>"));
                        }
                        return Ok("Map<String,Int>".into());
                    }
                    if dotted == "Map.has" && args.len() == 2 {
                        return Ok("Bool".into());
                    }
                    if dotted == "Vector.get" && args.len() == 2 {
                        let vec_ty = infer_aver_type(&args[0].node, ctx)?;
                        if let Some(elem) =
                            super::super::types::TypeRegistry::vector_element_type(&vec_ty)
                        {
                            return Ok(format!("Option<{}>", elem.trim()));
                        }
                        return Ok("Option<Int>".into());
                    }
                    if dotted == "Option.withDefault" && args.len() == 2 {
                        return infer_aver_type(&args[1].node, ctx);
                    }
                    if dotted == "Result.withDefault" && args.len() == 2 {
                        return infer_aver_type(&args[1].node, ctx);
                    }
                    // Option constructors return `Option<T>` — the
                    // inner T comes from the payload (Some) or the
                    // enclosing fn's return type (None).
                    if dotted == "Option.Some" && args.len() == 1 {
                        let inner = infer_aver_type(&args[0].node, ctx)?;
                        return Ok(format!("Option<{inner}>"));
                    }
                    if dotted == "Option.None" {
                        return Ok(ctx.return_type.to_string());
                    }
                    // Result constructors: payload type fixes T (Ok)
                    // or E (Err); the other position is recovered
                    // from the surrounding Result<T,E> instantiation.
                    if dotted == "Result.Ok" && args.len() == 1 {
                        let t = infer_aver_type(&args[0].node, ctx)?;
                        // Find a registered Result with matching T,
                        // fall back to ctx.return_type.
                        if let Some(c) = ctx.registry.result_order.iter().find(|c| {
                            TypeRegistry::result_te(c).is_some_and(|(t2, _)| t2 == t.trim())
                        }) {
                            return Ok(c.clone());
                        }
                        return Ok(ctx.return_type.to_string());
                    }
                    if dotted == "Result.Err" && args.len() == 1 {
                        let e = infer_aver_type(&args[0].node, ctx)?;
                        if let Some(c) = ctx.registry.result_order.iter().find(|c| {
                            TypeRegistry::result_te(c).is_some_and(|(_, e2)| e2 == e.trim())
                        }) {
                            return Ok(c.clone());
                        }
                        return Ok(ctx.return_type.to_string());
                    }
                    if dotted == "List.prepend" && args.len() == 2 {
                        return infer_aver_type(&args[1].node, ctx);
                    }
                    if dotted == "List.empty" {
                        if ctx.registry.list_order.len() == 1 {
                            return Ok(ctx.registry.list_order[0].clone());
                        }
                        return Ok(ctx.return_type.to_string());
                    }
                    if dotted == "List.length" || dotted == "List.len" {
                        return Ok("Int".into());
                    }
                    // List.reverse(list: List<T>) -> List<T>
                    if dotted == "List.reverse" && args.len() == 1 {
                        return infer_aver_type(&args[0].node, ctx);
                    }
                    // T-preserving list ops — return type matches the
                    // first list arg's inferred type. Without these,
                    // a chained `List.len(List.concat(a, b))` would
                    // fall through to the generic "callee must be
                    // Ident/Resolved" branch and surface as
                    // Unimplemented even though the lowering itself
                    // is supported.
                    if (dotted == "List.concat" && args.len() == 2)
                        || (dotted == "List.take" && args.len() == 2)
                        || (dotted == "List.drop" && args.len() == 2)
                    {
                        return infer_aver_type(&args[0].node, ctx);
                    }
                    // Map.remove(m, k) -> Map<K,V> — same shape as set.
                    if dotted == "Map.remove" && args.len() == 2 {
                        return infer_aver_type(&args[0].node, ctx);
                    }
                    // Vector.fromList(list: List<T>) -> Vector<T>
                    if dotted == "Vector.fromList" && args.len() == 1 {
                        let list_ty = infer_aver_type(&args[0].node, ctx)?;
                        if let Some(elem) = TypeRegistry::list_element_type(&list_ty) {
                            return Ok(format!("Vector<{}>", elem.trim()));
                        }
                        return Ok("Vector<Int>".into());
                    }
                    // Map ops — type derives from the canonical of the
                    // map argument (or from the only registered
                    // instantiation for `Map.empty`).
                    if dotted == "Map.empty" {
                        if ctx.registry.map_order.len() == 1 {
                            return Ok(ctx.registry.map_order[0].clone());
                        }
                        return Ok(ctx.return_type.to_string());
                    }
                    if dotted == "Map.set" && args.len() == 3 {
                        return infer_aver_type(&args[0].node, ctx);
                    }
                    if dotted == "Map.get" && args.len() == 2 {
                        let map_ty = infer_aver_type(&args[0].node, ctx)?;
                        if let Some((_, v)) =
                            super::super::types::parse_map_kv(&map_ty.replace(' ', ""))
                        {
                            return Ok(format!("Option<{v}>"));
                        }
                        return Ok("Option<Int>".into());
                    }
                    if dotted == "Map.len" && args.len() == 1 {
                        return Ok("Int".into());
                    }
                    // Generic stdlib dispatch — return type of any
                    // remaining dotted builtin. `dotted_return_type`
                    // covers String / Int / Float / Bool / List /
                    // Vector / Result / Option result shapes that the
                    // codegen later either resolves through a helper
                    // builtin or surfaces as Unimplemented.
                    if let Some(ty) = dotted_return_type(&dotted) {
                        return Ok(ty.into());
                    }
                }
            }
            let name = match &callee.node {
                Expr::Ident(n) => n.as_str(),
                Expr::Resolved { name, .. } => name.as_str(),
                _ => {
                    return Err(WasmGcError::Unimplemented(
                        "phase 3b — exotic callee shape (chained Attr, lambda)",
                    ));
                }
            };
            let entry = ctx
                .fn_map
                .by_name
                .get(name)
                .ok_or(WasmGcError::Validation(format!("unknown fn `{name}`")))?;
            Ok(entry.return_type.clone())
        }
        Expr::Match { arms, .. } => {
            // Match result type = arm body type; arms are required by
            // the type checker to agree, so any arm tells us. Phase
            // 4 only accepts non-empty matches.
            arms.first()
                .map(|a| infer_aver_type(&a.body.node, ctx))
                .unwrap_or(Err(WasmGcError::Validation("match has no arms".into())))
        }
        // Tail calls are statements at the wasm level (no value pushed
        // back to the caller's frame); for inference purposes we report
        // the enclosing fn's return type.
        Expr::TailCall(_) => Ok(ctx.return_type.to_string()),
        Expr::RecordCreate { type_name, .. } | Expr::RecordUpdate { type_name, .. } => {
            Ok(type_name.clone())
        }
        Expr::Attr(obj, field) => {
            // Bare `Option.None` reference — same shape as a bare attr
            // but resolves to `Option<T>` of the enclosing return type.
            if let Expr::Ident(p) = &obj.node
                && p == "Option"
                && field == "None"
            {
                return Ok(ctx.return_type.to_string());
            }
            // Phase 3a: best-effort — if we can identify the record
            // type of `obj`, look up the field's declared type.
            // Otherwise fall back to "Int" (most bench scenarios with
            // Attr access do unwrap a numeric field).
            if let Ok(Some(record_name)) = struct_name_of_unboxed(&obj.node, ctx)
                && let Some(ty) = ctx.registry.record_field_type(&record_name, field)
            {
                return Ok(ty.into());
            }
            Ok("Int".into())
        }
        Expr::Constructor(name, _) => {
            if let Some(info) = ctx.registry.variant(name) {
                Ok(info.parent.clone())
            } else {
                Ok("Int".into())
            }
        }
        Expr::List(items) => {
            if let Some(first) = items.first() {
                let elem = infer_aver_type(&first.node, ctx)?;
                Ok(format!("List<{elem}>"))
            } else if ctx.registry.list_order.len() == 1 {
                Ok(ctx.registry.list_order[0].clone())
            } else {
                Ok(ctx.return_type.to_string())
            }
        }
        Expr::Tuple(items) if items.len() == 2 => {
            let a = infer_aver_type(&items[0].node, ctx)?;
            let b = infer_aver_type(&items[1].node, ctx)?;
            Ok(format!("Tuple<{a},{b}>"))
        }
        other => Err(WasmGcError::Validation(format!(
            "infer_aver_type: unsupported expression shape: {other:?}"
        ))),
    }
}

/// Best-effort record-type-name lookup that walks the AST plus the
/// fn's param/binding type table — used when the slot's wasm ValType
/// isn't a struct ref (newtype optimization erases it to a primitive,
/// so `struct_name_of` based on slots can't see the original type).
pub(super) fn struct_name_of_unboxed(
    expr: &Expr,
    ctx: &EmitCtx<'_>,
) -> Result<Option<String>, WasmGcError> {
    if let Expr::Resolved { name, .. } = expr {
        // First: is the name itself a record?
        if ctx.registry.records.contains_key(name) {
            return Ok(Some(name.clone()));
        }
        // Otherwise: look up the variable's declared type via the
        // resolution-driven param map (we don't have a binding-type
        // map yet, so this only works for params).
        if let Some(ty) = lookup_var_type(name, ctx)
            && ctx.registry.records.contains_key(&ty)
        {
            return Ok(Some(ty));
        }
    }
    Ok(None)
}

/// Same as `sniff_aver_type` but with access to previously-resolved
/// local bindings — lets a binding's RHS reference earlier bindings
/// without falling back to "Int" for every chained `let`.
pub(super) fn sniff_aver_type_ext(
    expr: &Expr,
    fd: &FnDef,
    fn_map: &FnMap,
    registry: &TypeRegistry,
    prev: &HashMap<String, String>,
) -> Option<String> {
    sniff_with_prev(expr, fd, fn_map, registry, prev)
}

/// Walk an expression collecting Aver types for match-bound names —
/// `Result.Ok(v)` binds `v: T`, `Option.Some(x)` binds `x: T`, etc.
/// Without these, `infer_aver_type` falls back to "Int" for the
/// binding and the surrounding `match`'s block-type inference picks
/// up the wrong wasm type.
pub(super) fn collect_match_pattern_types(
    expr: &Expr,
    fd: &FnDef,
    fn_map: &FnMap,
    registry: &TypeRegistry,
    out: &mut HashMap<String, String>,
) {
    match expr {
        Expr::FnCall(callee, args) => {
            collect_match_pattern_types(&callee.node, fd, fn_map, registry, out);
            for a in args {
                collect_match_pattern_types(&a.node, fd, fn_map, registry, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_match_pattern_types(&l.node, fd, fn_map, registry, out);
            collect_match_pattern_types(&r.node, fd, fn_map, registry, out);
        }
        Expr::TailCall(boxed) => {
            for a in &boxed.args {
                collect_match_pattern_types(&a.node, fd, fn_map, registry, out);
            }
        }
        Expr::Attr(obj, _) => collect_match_pattern_types(&obj.node, fd, fn_map, registry, out),
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                collect_match_pattern_types(&p.node, fd, fn_map, registry, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_match_pattern_types(&e.node, fd, fn_map, registry, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_match_pattern_types(&base.node, fd, fn_map, registry, out);
            for (_, e) in updates {
                collect_match_pattern_types(&e.node, fd, fn_map, registry, out);
            }
        }
        Expr::List(items) => {
            for x in items {
                collect_match_pattern_types(&x.node, fd, fn_map, registry, out);
            }
        }
        Expr::Match { subject, arms } => {
            collect_match_pattern_types(&subject.node, fd, fn_map, registry, out);
            // Recover subject type so Result/Option bindings can
            // pick the right T/E.
            // Use prev-aware sniff so e.g. `match Vector.get(v, 0)` over
            // a `let`-bound `v: Vector<String>` infers the subject as
            // `Option<String>`, which lets the Some-binding pick up
            // the right element type.
            let subject_ty = sniff_with_prev(&subject.node, fd, fn_map, registry, out);
            for arm in arms {
                if let Pattern::Cons(head, tail) = &arm.pattern {
                    if let Some(ref subj) = subject_ty {
                        let canonical: String =
                            subj.chars().filter(|c| !c.is_whitespace()).collect();
                        if let Some(elem) = TypeRegistry::list_element_type(&canonical) {
                            if head != "_" {
                                out.insert(head.clone(), elem.trim().to_string());
                            }
                            if tail != "_" {
                                out.insert(tail.clone(), canonical.clone());
                            }
                        }
                    }
                    collect_match_pattern_types(&arm.body.node, fd, fn_map, registry, out);
                    continue;
                }
                if let Pattern::Tuple(items) = &arm.pattern {
                    if items.len() == 2
                        && let Some(ref subj) = subject_ty
                    {
                        let canonical: String =
                            subj.chars().filter(|c| !c.is_whitespace()).collect();
                        if let Some((a, b)) = TypeRegistry::tuple_ab(&canonical) {
                            for (pat, ty) in items.iter().zip([a, b]) {
                                if let Pattern::Ident(name) = pat
                                    && name != "_"
                                {
                                    out.insert(name.clone(), ty.to_string());
                                }
                            }
                        }
                    }
                    collect_match_pattern_types(&arm.body.node, fd, fn_map, registry, out);
                    continue;
                }
                if let Pattern::Constructor(name, bindings) = &arm.pattern {
                    let bare = name.rsplit('.').next().unwrap_or(name);
                    if let Some(ref subj) = subject_ty {
                        let canonical: String =
                            subj.chars().filter(|c| !c.is_whitespace()).collect();
                        // Result<T, E>
                        if let Some((t, e)) = TypeRegistry::result_te(&canonical) {
                            if (bare == "Ok" || name == "Result.Ok")
                                && let Some(b) = bindings.first()
                                && b != "_"
                            {
                                out.insert(b.clone(), t.to_string());
                            }
                            if (bare == "Err" || name == "Result.Err")
                                && let Some(b) = bindings.first()
                                && b != "_"
                            {
                                out.insert(b.clone(), e.to_string());
                            }
                        }
                        // Option<T>
                        if let Some(t) = TypeRegistry::option_element_type(&canonical)
                            && (bare == "Some" || name == "Option.Some")
                            && let Some(b) = bindings.first()
                            && b != "_"
                        {
                            out.insert(b.clone(), t.to_string());
                        }
                        // User variants — recover field types.
                        if let Some(info) = registry.variant(bare) {
                            for (binding_name, field_ty) in bindings.iter().zip(info.fields.iter())
                            {
                                if binding_name != "_" {
                                    out.insert(binding_name.clone(), field_ty.clone());
                                }
                            }
                        }
                    }
                }
                collect_match_pattern_types(&arm.body.node, fd, fn_map, registry, out);
            }
        }
        Expr::InterpolatedStr(_) => {}
        _ => {}
    }
}

pub(super) fn sniff_with_prev(
    expr: &Expr,
    fd: &FnDef,
    fn_map: &FnMap,
    registry: &TypeRegistry,
    prev: &HashMap<String, String>,
) -> Option<String> {
    match expr {
        Expr::Literal(Literal::Int(_)) => Some("Int".into()),
        Expr::Literal(Literal::Float(_)) => Some("Float".into()),
        Expr::Literal(Literal::Bool(_)) => Some("Bool".into()),
        Expr::Literal(Literal::Str(_)) => Some("String".into()),
        Expr::Literal(Literal::Unit) => Some("Unit".into()),
        Expr::Resolved { name, .. } => {
            if let Some(t) = prev.get(name) {
                return Some(t.clone());
            }
            fd.params
                .iter()
                .find(|(n, _)| n == name)
                .map(|(_, t)| t.clone())
        }
        Expr::FnCall(callee, args) => {
            if let Expr::Attr(parent, member) = &callee.node {
                let parent_name = match &parent.node {
                    Expr::Ident(n) => Some(n.as_str()),
                    Expr::Resolved { name, .. } => Some(name.as_str()),
                    _ => None,
                };
                if let Some(p) = parent_name {
                    let dotted = format!("{p}.{member}");
                    match dotted.as_str() {
                        "Map.empty" if registry.map_order.len() == 1 => {
                            return Some(registry.map_order[0].clone());
                        }
                        "Map.set" | "Map.remove" if !args.is_empty() => {
                            return sniff_with_prev(&args[0].node, fd, fn_map, registry, prev);
                        }
                        "Map.get" if !args.is_empty() => {
                            let m = sniff_with_prev(&args[0].node, fd, fn_map, registry, prev)?;
                            let canonical = m.replace(' ', "");
                            if let Some((_, v)) = super::super::types::parse_map_kv(&canonical) {
                                return Some(format!("Option<{v}>"));
                            }
                            return None;
                        }
                        "Map.keys" if !args.is_empty() => {
                            let m = sniff_with_prev(&args[0].node, fd, fn_map, registry, prev)?;
                            let canonical = m.replace(' ', "");
                            if let Some((k, _)) = super::super::types::parse_map_kv(&canonical) {
                                return Some(format!("List<{k}>"));
                            }
                            return None;
                        }
                        "Map.values" if !args.is_empty() => {
                            let m = sniff_with_prev(&args[0].node, fd, fn_map, registry, prev)?;
                            let canonical = m.replace(' ', "");
                            if let Some((_, v)) = super::super::types::parse_map_kv(&canonical) {
                                return Some(format!("List<{v}>"));
                            }
                            return None;
                        }
                        "Map.len" => return Some("Int".into()),
                        "Map.has" => return Some("Bool".into()),
                        "Vector.new" if args.len() == 2 => {
                            let elem = sniff_with_prev(&args[1].node, fd, fn_map, registry, prev)?;
                            return Some(format!("Vector<{elem}>"));
                        }
                        "Vector.get" if args.len() == 2 => {
                            let v = sniff_with_prev(&args[0].node, fd, fn_map, registry, prev)?;
                            if let Some(elem) = TypeRegistry::vector_element_type(&v) {
                                return Some(format!("Option<{}>", elem.trim()));
                            }
                            return None;
                        }
                        "List.reverse" if args.len() == 1 => {
                            return sniff_with_prev(&args[0].node, fd, fn_map, registry, prev);
                        }
                        "List.concat" | "List.take" | "List.drop" if !args.is_empty() => {
                            return sniff_with_prev(&args[0].node, fd, fn_map, registry, prev);
                        }
                        "List.zip" if args.len() == 2 => {
                            let la = sniff_with_prev(&args[0].node, fd, fn_map, registry, prev)?;
                            let lb = sniff_with_prev(&args[1].node, fd, fn_map, registry, prev)?;
                            let a = TypeRegistry::list_element_type(&la)?.trim();
                            let b = TypeRegistry::list_element_type(&lb)?.trim();
                            return Some(format!("List<Tuple<{a},{b}>>"));
                        }
                        "Map.entries" if args.len() == 1 => {
                            let m = sniff_with_prev(&args[0].node, fd, fn_map, registry, prev)?;
                            let canonical: String =
                                m.chars().filter(|c| !c.is_whitespace()).collect();
                            if let Some((k, v)) = super::super::types::parse_map_kv(&canonical) {
                                return Some(format!("List<Tuple<{k},{v}>>"));
                            }
                            return None;
                        }
                        "Map.fromList" if args.len() == 1 => {
                            let l = sniff_with_prev(&args[0].node, fd, fn_map, registry, prev)?;
                            let canonical: String =
                                l.chars().filter(|c| !c.is_whitespace()).collect();
                            if let Some(elem) = TypeRegistry::list_element_type(&canonical)
                                && let Some((k, v)) = TypeRegistry::tuple_ab(elem)
                            {
                                return Some(format!("Map<{k},{v}>"));
                            }
                            return None;
                        }
                        "Vector.fromList" if args.len() == 1 => {
                            let l = sniff_with_prev(&args[0].node, fd, fn_map, registry, prev)?;
                            if let Some(elem) = TypeRegistry::list_element_type(&l) {
                                return Some(format!("Vector<{}>", elem.trim()));
                            }
                            return None;
                        }
                        "Vector.toList" if args.len() == 1 => {
                            let v = sniff_with_prev(&args[0].node, fd, fn_map, registry, prev)?;
                            if let Some(elem) = TypeRegistry::vector_element_type(&v) {
                                return Some(format!("List<{}>", elem.trim()));
                            }
                            return None;
                        }
                        "Vector.set" if args.len() == 3 => {
                            // Boxed return: `Option<Vector<T>>`. Recover
                            // T from the vector arg.
                            let v = sniff_with_prev(&args[0].node, fd, fn_map, registry, prev)?;
                            let canonical: String =
                                v.chars().filter(|c| !c.is_whitespace()).collect();
                            if TypeRegistry::vector_element_type(&canonical).is_some() {
                                return Some(format!("Option<{canonical}>"));
                            }
                            return None;
                        }
                        "Int.toString" => return Some("String".into()),
                        "String.len" => return Some("Int".into()),
                        _ => {
                            if let Some(ret) = dotted_return_type(&dotted) {
                                return Some(ret.into());
                            }
                        }
                    }
                }
            }
            // User fn call → its declared return type.
            let name = match &callee.node {
                Expr::Ident(n) => n.as_str(),
                Expr::Resolved { name, .. } => name.as_str(),
                _ => return None,
            };
            fn_map.by_name.get(name).map(|e| e.return_type.clone())
        }
        Expr::BinOp(op, l, _) => match op {
            BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                Some("Bool".into())
            }
            _ => sniff_with_prev(&l.node, fd, fn_map, registry, prev),
        },
        Expr::List(items) => {
            let elem = items
                .first()
                .and_then(|x| sniff_with_prev(&x.node, fd, fn_map, registry, prev))?;
            Some(format!("List<{elem}>"))
        }
        Expr::Tuple(items) if items.len() == 2 => {
            let a = sniff_with_prev(&items[0].node, fd, fn_map, registry, prev)?;
            let b = sniff_with_prev(&items[1].node, fd, fn_map, registry, prev)?;
            Some(format!("Tuple<{a},{b}>"))
        }
        Expr::Match { arms, .. } => arms
            .first()
            .and_then(|a| sniff_with_prev(&a.body.node, fd, fn_map, registry, prev)),
        Expr::Attr(obj, field) => {
            // Field access — recover the obj's record type, look up
            // the field's declared Aver type.
            if let Expr::Resolved { name, .. } = &obj.node {
                let obj_ty = prev.get(name).cloned().or_else(|| {
                    fd.params
                        .iter()
                        .find(|(n, _)| n == name)
                        .map(|(_, t)| t.clone())
                })?;
                if let Some(ty) = registry.record_field_type(&obj_ty, field) {
                    return Some(ty.into());
                }
            }
            None
        }
        Expr::RecordCreate { type_name, .. } | Expr::RecordUpdate { type_name, .. } => {
            Some(type_name.clone())
        }
        Expr::Constructor(name, _) => registry.variant(name).map(|info| info.parent.clone()),
        Expr::InterpolatedStr(_) => Some("String".into()),
        _ => None,
    }
}

/// Aver return type string for an effect dotted name (`Time.unixMs`
/// → `"Int"`, `Request.method` → `"String"`, etc.). `None` means
/// the effect returns Unit. Mirrors the `EffectName::results` table
/// in `effects.rs` — keep them in sync.
pub(super) fn effect_aver_return_type(dotted: &str) -> Option<&'static str> {
    Some(match dotted {
        "Time.unixMs" | "Random.int" | "Args._len" | "Args.len" => "Int",
        "Random.float" | "Float.sin" | "Float.cos" | "Float.atan2" | "Float.pow" => "Float",
        "Request.method" | "Request.url" | "Request.path" | "Request.query" | "Request.body"
        | "Request.country" | "Console.readLine" | "Args._get" | "Args.get" | "Time.now"
        | "Env.get" => "String",
        "Request.headersLoad" | "Request.headers" => "Map<String,List<String>>",
        "Terminal.readKey" => "Option<String>",
        "Terminal.size" => "Terminal.Size",
        _ => return None,
    })
}

/// Aver return type for a stdlib dotted builtin. Used by
/// `infer_aver_type` to decide a `Type.method(args)` call's type
/// without consulting `fn_map.builtins` (some builtins lower inline
/// or via Float-direct ops, so they're not registered as helpers).
/// Returns `None` for unknown dotted names so the caller can fall
/// through to a real-fn path or surface the error.
pub(super) fn dotted_return_type(dotted: &str) -> Option<&'static str> {
    Some(match dotted {
        // String results
        "Int.toString" | "Float.toString" | "String.fromInt" | "String.fromFloat"
        | "String.fromBool" | "String.toUpper" | "String.toLower" | "String.trim"
        | "String.replace" | "String.slice" | "String.join" => "String",

        // Int results — `Float.floor/ceil/round` return Int per
        // Aver stdlib semantics (legacy backend matches).
        "String.len" | "String.length" | "String.byteLength" | "List.len" | "List.length"
        | "Vector.len" | "Map.len" | "Char.toCode" | "Int.abs" | "Int.min" | "Int.max"
        | "Float.floor" | "Float.ceil" | "Float.round" => "Int",
        // Float results
        "Float.abs" | "Float.sqrt" | "Float.min" | "Float.max" | "Float.pi" | "Float.sin"
        | "Float.cos" | "Float.pow" | "Int.toFloat" | "Float.fromInt" => "Float",
        // Bool results
        "Bool.and" | "Bool.or" | "Bool.not" | "String.startsWith" | "String.endsWith"
        | "String.contains" | "Map.has" | "List.contains" => "Bool",
        // Char results
        // Note: `Char.fromCode` actually returns `Option<String>`
        // (Aver Char = 1-byte String), handled below.
        // Option-typed
        "String.charAt" | "Char.fromCode" => "Option<String>",
        // Result-typed parsers
        "Float.fromString" => "Result<Float,String>",
        "Int.fromString" | "Byte.fromHex" => "Result<Int,String>",
        "Int.mod" => "Result<Int,String>",
        "Byte.toHex" => "Result<String,String>",
        // List-typed — `List.concat/take/drop` flow through
        // sniff_with_prev (return type matches arg[0]); only the
        // T-fixed `String.split` / `String.chars` land here.
        "String.split" | "String.chars" => "List<String>",
        // Map K/V-derived — actual canonicals come through
        // `infer_aver_type`'s dispatch (which has access to the
        // map's K, V); these are the generic placeholders.
        "Map.keys" => "List<Unknown>",
        "Map.values" => "List<Unknown>",
        // Vector tail
        "Vector.toList" => "List<Unknown>",
        "Vector.set" => "Option<Vector<Unknown>>",
        _ => return None,
    })
}

/// Aver result type for a registered builtin. Mirrors
/// `BuiltinName::results` but returns a `&'static str` for type
/// inference. Adding a new builtin: extend both.
pub(super) fn builtin_aver_result_type(dotted: &str) -> &'static str {
    match dotted {
        // Returns String
        "Int.toString" | "Float.toString" | "String.fromInt" | "String.fromFloat"
        | "String.fromBool" | "String.toUpper" | "String.toLower" | "String.trim"
        | "String.replace" | "String.slice" | "String.join" => "String",

        // Returns Int — `Float.floor / ceil / round` are Aver-Int per
        // stdlib semantics.
        "String.len" | "String.length" | "String.byteLength" | "List.len" | "List.length"
        | "Vector.len" | "Map.len" | "Char.toCode" | "Int.abs" | "Int.min" | "Int.max"
        | "Float.floor" | "Float.ceil" | "Float.round" => "Int",
        // Returns Float
        "Float.abs" | "Float.sqrt" | "Float.min" | "Float.max" | "Float.pi" | "Float.sin"
        | "Float.cos" | "Float.pow" | "Int.toFloat" | "Float.fromInt" => "Float",
        // Returns Bool
        "Bool.and" | "Bool.or" | "Bool.not" | "String.startsWith" | "String.endsWith"
        | "String.contains" | "Map.has" | "List.contains" => "Bool",
        // List-T-preserving — return type matches arg[0] (List<T>).
        // `infer_aver_type` reaches into the call args itself for
        // these, so `builtin_aver_result_type` only sees them as a
        // generic "List" tag; the concrete T comes from the caller.
        "List.concat" | "List.take" | "List.drop" => "List<Unknown>",
        // Option-typed (Aver Char = 1-byte String, both wrap Option<String>)
        "String.charAt" | "Char.fromCode" => "Option<String>",
        // Returns List<String>
        "String.chars" => "List<String>",
        // Result-typed parsers
        "Float.fromString" => "Result<Float,String>",
        "Int.fromString" | "Int.mod" | "Byte.fromHex" => "Result<Int,String>",
        "Byte.toHex" => "Result<String,String>",
        _ => "Int",
    }
}

pub(super) fn parent_dotted_head(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Ident(n) => Some(n.as_str()),
        Expr::Resolved { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

/// Try to find the record-type name that an expression evaluates to.
/// Phase 3a handles the common case: `Resolved` whose slot is a
/// concrete struct ref. For deeper analysis (Attr → Attr chains) we'd
/// need a real type-of-expr pass.
pub(super) fn struct_name_of(
    expr: &Expr,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<String>, WasmGcError> {
    if let Expr::Resolved { slot, .. } = expr
        && let Some(ValType::Ref(rt)) = slots.by_slot.get(*slot as usize)
        && let wasm_encoder::HeapType::Concrete(idx) = rt.heap_type
    {
        // Reverse-lookup the registry by type idx.
        for (name, recorded_idx) in &ctx.registry.records {
            if *recorded_idx == idx {
                return Ok(Some(name.clone()));
            }
        }
    }
    Ok(None)
}
