//! Per-fn slot table + slot-allocation pre-pass + wasm type inference
//! over the resolver's local layout. Split out of `body.rs` as part of
//! the 0.16 wasm-gc submodule reshape — pure code movement, no logic
//! changes.

use std::collections::HashMap;

use wasm_encoder::ValType;

use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, Pattern, Stmt};

use super::super::WasmGcError;
use super::super::types::{TypeRegistry, aver_to_wasm};
use super::infer::{
    arm_is_option_pattern, arm_is_result_pattern, builtin_aver_result_type,
    collect_match_pattern_types, dotted_return_type, effect_aver_return_type, sniff_aver_type_ext,
    struct_name_of_unboxed, subject_option_inner_type_with_prev, subject_result_te,
};
use super::{EmitCtx, FnMap};

/// Per-fn slot table — one entry per local (param or binding) in
/// resolver-allocation order. Slot N maps to `wasm local N`.
pub(super) struct SlotTable {
    /// Element index = slot number; element value = wasm ValType.
    pub(super) by_slot: Vec<ValType>,
    /// Optional scratch slot of `(ref null eq)` reserved for multi-arm
    /// variant dispatch — holds the subject so `ref.test` and
    /// `ref.cast` can read it across arms without recomputing the
    /// match-subject expression. Allocated when the body contains at
    /// least one multi-arm Constructor match. Slot index, when set,
    /// is always the last slot in `by_slot`.
    pub(super) subject_scratch: Option<u32>,
}

impl SlotTable {
    /// Pre-scan a fn's full local layout: params, then every binding
    /// produced by `Stmt::Binding` or pattern-bind in `match`. Slot
    /// indices must match what the resolver assigned, since
    /// `Resolved.slot` and `Pattern::Constructor` bindings reference
    /// slot numbers directly.
    ///
    /// Walks the body, infers each slot's wasm type from its binding
    /// source, builds a dense `Vec<ValType>` indexed by slot number.
    pub(super) fn build_for_fn(
        fd: &FnDef,
        registry: &TypeRegistry,
        fn_map: &FnMap,
    ) -> Result<Self, WasmGcError> {
        let mut by_slot: Vec<ValType> = Vec::new();
        // Params first — slots 0..N.
        for (_, ty) in &fd.params {
            if let Some(v) = aver_to_wasm(ty, Some(registry))? {
                by_slot.push(v);
            }
        }
        // Pre-pass: build a name → Aver-type map for every binding
        // (and match-pattern binding) so chained `let`s and nested
        // pattern bindings get the right wasm slot type even when
        // their RHS references earlier locals.
        let binding_types = collect_binding_types(
            match fd.body.as_ref() {
                FnBody::Block(stmts) => stmts.as_slice(),
            },
            fd,
            fn_map,
            registry,
        );
        // Walk body to collect binding slots (resolver order).
        let FnBody::Block(stmts) = fd.body.as_ref();
        for stmt in stmts {
            collect_binding_slots(stmt, &mut by_slot, registry, fd, fn_map, &binding_types)?;
        }
        // If this fn has any multi-arm Constructor match, reserve a
        // scratch slot at the end for stashing the subject. (ref null eq)
        // is the universal carrier — every wasm-gc struct subtypes it.
        let needs_scratch = fn_needs_subject_scratch(fd, registry);
        let subject_scratch = if needs_scratch {
            let scratch_ty = ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Abstract {
                    shared: false,
                    ty: wasm_encoder::AbstractHeapType::Eq,
                },
            });
            let idx = by_slot.len() as u32;
            by_slot.push(scratch_ty);
            Some(idx)
        } else {
            None
        };
        Ok(Self {
            by_slot,
            subject_scratch,
        })
    }

    pub(super) fn extra_locals(&self, params_count: usize) -> Vec<ValType> {
        self.by_slot.iter().skip(params_count).copied().collect()
    }
}

/// True if the body has at least one multi-arm `match` whose arms are
/// `Pattern::Constructor` against a non-newtype variant. Single-arm
/// matches and newtype matches don't need a scratch (the cast is
/// elided), so we only allocate when really necessary.
pub(super) fn fn_needs_subject_scratch(fd: &FnDef, registry: &TypeRegistry) -> bool {
    let FnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|s| stmt_needs_scratch(s, registry))
}

pub(super) fn stmt_needs_scratch(stmt: &Stmt, registry: &TypeRegistry) -> bool {
    match stmt {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => expr_needs_scratch(&e.node, registry),
    }
}

pub(super) fn expr_needs_scratch(expr: &Expr, registry: &TypeRegistry) -> bool {
    match expr {
        Expr::Match { subject, arms } => {
            if expr_needs_scratch(&subject.node, registry) {
                return true;
            }
            // Built-in Option dispatch needs a scratch (subject ref is
            // read multiple times: tag check, value extraction).
            if arms.iter().any(arm_is_option_pattern) {
                return true;
            }
            if arms.iter().any(arm_is_result_pattern) {
                return true;
            }
            if arms
                .iter()
                .any(|a| matches!(&a.pattern, Pattern::EmptyList | Pattern::Cons(_, _)))
            {
                return true;
            }
            if arms.iter().any(|a| matches!(&a.pattern, Pattern::Tuple(_))) {
                return true;
            }
            // String-subject match (`match s { "literal" -> ... }`)
            // stashes the subject ref in scratch and tests it against
            // each literal — needs a scratch slot.
            if arms
                .iter()
                .any(|a| matches!(&a.pattern, Pattern::Literal(Literal::Str(_))))
            {
                return true;
            }
            // Arm needs a scratch when it's a multi-arm Constructor
            // match against a non-newtype variant.
            let constructor_arms: Vec<_> = arms
                .iter()
                .filter(|a| matches!(a.pattern, Pattern::Constructor(_, _)))
                .collect();
            if constructor_arms.len() > 1 {
                let any_non_newtype = constructor_arms.iter().any(|a| {
                    if let Pattern::Constructor(name, _) = &a.pattern {
                        let bare = name.rsplit('.').next().unwrap_or(name);
                        registry
                            .variant(bare)
                            .map(|info| registry.newtype_underlying(&info.parent).is_none())
                            .unwrap_or(false)
                    } else {
                        false
                    }
                });
                if any_non_newtype {
                    return true;
                }
            }
            arms.iter()
                .any(|a| expr_needs_scratch(&a.body.node, registry))
        }
        Expr::BinOp(_, l, r) => {
            expr_needs_scratch(&l.node, registry) || expr_needs_scratch(&r.node, registry)
        }
        Expr::FnCall(callee, args) => {
            // `Option.withDefault(opt, default)` falls back to the
            // boxed path when the inner shape isn't a fused
            // Vector/Map. The boxed emitter stashes the Option in the
            // scratch slot for tag inspection. Conservatively reserve
            // scratch for any Option.withDefault call — the cost of
            // an unused scratch local is one wasm value, the cost of
            // missing it is a validation crash.
            if let Expr::Attr(parent, member) = &callee.node
                && let Expr::Ident(p) = &parent.node
                && ((p == "Option" || p == "Result") && member == "withDefault")
            {
                return true;
            }
            expr_needs_scratch(&callee.node, registry)
                || args.iter().any(|a| expr_needs_scratch(&a.node, registry))
        }
        Expr::TailCall(boxed) => boxed
            .args
            .iter()
            .any(|a| expr_needs_scratch(&a.node, registry)),
        Expr::Attr(obj, _) => expr_needs_scratch(&obj.node, registry),
        Expr::Constructor(_, payload) => payload
            .as_deref()
            .is_some_and(|p| expr_needs_scratch(&p.node, registry)),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, e)| expr_needs_scratch(&e.node, registry)),
        // List literal with elements uses the scratch slot for the
        // running tail during the right-fold; empty literal lowers to
        // a single ref.null and doesn't need it.
        Expr::List(items) if !items.is_empty() => true,
        Expr::List(items) => items.iter().any(|e| expr_needs_scratch(&e.node, registry)),
        Expr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            expr_needs_scratch(&k.node, registry) || expr_needs_scratch(&v.node, registry)
        }),
        _ => false,
    }
}

pub(super) fn collect_binding_slots(
    stmt: &Stmt,
    out: &mut Vec<ValType>,
    registry: &TypeRegistry,
    fd: &FnDef,
    fn_map: &FnMap,
    binding_types: &HashMap<String, String>,
) -> Result<(), WasmGcError> {
    match stmt {
        Stmt::Binding(name, annot, expr) => {
            let ty = if let Some(t) = annot.as_deref() {
                aver_to_wasm(t, Some(registry))?
            } else if let Some(t) = binding_types.get(name) {
                aver_to_wasm(t, Some(registry))?
            } else {
                infer_expr_wasm_type(&expr.node, registry, fd, fn_map)?
            };
            if let Some(v) = ty {
                out.push(v);
            }
            collect_expr_binding_slots(&expr.node, out, registry, fd, fn_map, binding_types)?;
        }
        Stmt::Expr(spanned) => {
            collect_expr_binding_slots(&spanned.node, out, registry, fd, fn_map, binding_types)?
        }
    }
    Ok(())
}

pub(super) fn collect_expr_binding_slots(
    expr: &Expr,
    out: &mut Vec<ValType>,
    registry: &TypeRegistry,
    fd: &FnDef,
    fn_map: &FnMap,
    binding_types: &HashMap<String, String>,
) -> Result<(), WasmGcError> {
    match expr {
        Expr::Match { subject, arms } => {
            collect_expr_binding_slots(&subject.node, out, registry, fd, fn_map, binding_types)?;
            // Built-in Option arms — `Option.Some(v)` binds v to T
            // (read off the subject's option<T> wasm type).
            let is_option = arms.iter().any(arm_is_option_pattern);
            if is_option {
                let inner = subject_option_inner_type_with_prev(
                    &subject.node,
                    registry,
                    fd,
                    fn_map,
                    binding_types,
                );
                for arm in arms {
                    if let Pattern::Constructor(_, bindings) = &arm.pattern
                        && arm_is_option_pattern(arm)
                    {
                        for binding_name in bindings {
                            if binding_name == "_" {
                                continue;
                            }
                            let inner_ty = inner.as_deref().ok_or(WasmGcError::Validation(
                                "Option.Some binding without resolvable inner type — \
                                 subject's Aver type must reduce to Option<T>"
                                    .into(),
                            ))?;
                            if let Some(v) = aver_to_wasm(inner_ty, Some(registry))? {
                                out.push(v);
                            }
                        }
                    }
                    collect_expr_binding_slots(
                        &arm.body.node,
                        out,
                        registry,
                        fd,
                        fn_map,
                        binding_types,
                    )?;
                }
                return Ok(());
            }
            // Built-in Result arms — Ok binds T (field 1), Err binds
            // E (field 2). Recover canonical from the subject's wasm
            // type via reverse-lookup in result_order.
            let is_result = arms.iter().any(arm_is_result_pattern);
            if is_result {
                let (t_aver, e_aver) = subject_result_te(&subject.node, registry, fd, fn_map)?;
                for arm in arms {
                    if let Pattern::Constructor(name, bindings) = &arm.pattern
                        && arm_is_result_pattern(arm)
                    {
                        let bare = name.rsplit('.').next().unwrap_or(name);
                        let inner_ty = if bare == "Ok" { &t_aver } else { &e_aver };
                        for binding_name in bindings {
                            if binding_name == "_" {
                                continue;
                            }
                            if let Some(v) = aver_to_wasm(inner_ty, Some(registry))? {
                                out.push(v);
                            }
                        }
                    }
                    collect_expr_binding_slots(
                        &arm.body.node,
                        out,
                        registry,
                        fd,
                        fn_map,
                        binding_types,
                    )?;
                }
                return Ok(());
            }
            for arm in arms {
                if let Pattern::Constructor(name, bindings) = &arm.pattern {
                    let bare = name.rsplit('.').next().unwrap_or(name);
                    if let Some(info) = registry.variant(bare).cloned() {
                        let is_newtype = registry.newtype_underlying(&info.parent).is_some();
                        for (binding_name, field_ty) in bindings.iter().zip(info.fields.iter()) {
                            if binding_name != "_" {
                                // Newtype: binding gets the underlying
                                // primitive directly. Otherwise, the
                                // field type from the variant decl.
                                let target_ty = if is_newtype && bindings.len() == 1 {
                                    aver_to_wasm(&info.parent, Some(registry))?
                                } else {
                                    aver_to_wasm(field_ty, Some(registry))?
                                };
                                if let Some(v) = target_ty {
                                    out.push(v);
                                }
                            }
                        }
                    }
                }
                if let Pattern::Tuple(items) = &arm.pattern
                    && items.len() == 2
                {
                    let subject_ty = infer_expr_wasm_type(&subject.node, registry, fd, fn_map)?;
                    if let Some(ValType::Ref(rt)) = subject_ty
                        && let wasm_encoder::HeapType::Concrete(idx) = rt.heap_type
                    {
                        let mut tup_canonical: Option<&str> = None;
                        for canonical in &registry.tuple_order {
                            if registry.tuple_types.get(canonical).copied() == Some(idx) {
                                tup_canonical = Some(canonical.as_str());
                                break;
                            }
                        }
                        if let Some(canonical) = tup_canonical
                            && let Some((a, b)) = TypeRegistry::tuple_ab(canonical)
                        {
                            for (pat, ty) in items.iter().zip([a, b]) {
                                if let Pattern::Ident(name) = pat
                                    && name != "_"
                                    && let Some(v) = aver_to_wasm(ty, Some(registry))?
                                {
                                    let _ = name;
                                    out.push(v);
                                }
                            }
                        }
                    }
                }
                if let Pattern::Cons(head_name, tail_name) = &arm.pattern {
                    // Cons pattern bindings — head: T, tail: (ref null
                    // $list_T). Both come from the subject's
                    // `List<T>` instantiation; recover by inferring
                    // the wasm type of the subject.
                    let subject_ty = infer_expr_wasm_type(&subject.node, registry, fd, fn_map)?;
                    if let Some(ValType::Ref(rt)) = subject_ty
                        && let wasm_encoder::HeapType::Concrete(idx) = rt.heap_type
                    {
                        // Reverse-lookup canonical from list_order.
                        let mut elem_ty: Option<ValType> = None;
                        for canonical in &registry.list_order {
                            if registry.list_types.get(canonical).copied() == Some(idx)
                                && let Some(elem) = TypeRegistry::list_element_type(canonical)
                            {
                                elem_ty = aver_to_wasm(elem, Some(registry))?;
                                break;
                            }
                        }
                        if let Some(et) = elem_ty {
                            if head_name != "_" {
                                out.push(et);
                            }
                            if tail_name != "_" {
                                // Tail has the same list ref shape as
                                // the subject.
                                out.push(ValType::Ref(rt));
                            }
                        }
                    }
                }
                collect_expr_binding_slots(
                    &arm.body.node,
                    out,
                    registry,
                    fd,
                    fn_map,
                    binding_types,
                )?;
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_expr_binding_slots(&l.node, out, registry, fd, fn_map, binding_types)?;
            collect_expr_binding_slots(&r.node, out, registry, fd, fn_map, binding_types)?;
        }
        Expr::FnCall(callee, args) => {
            collect_expr_binding_slots(&callee.node, out, registry, fd, fn_map, binding_types)?;
            for arg in args {
                collect_expr_binding_slots(&arg.node, out, registry, fd, fn_map, binding_types)?;
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                collect_expr_binding_slots(&arg.node, out, registry, fd, fn_map, binding_types)?;
            }
        }
        Expr::Attr(obj, _) => {
            collect_expr_binding_slots(&obj.node, out, registry, fd, fn_map, binding_types)?
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                collect_expr_binding_slots(&p.node, out, registry, fd, fn_map, binding_types)?;
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_expr_binding_slots(&e.node, out, registry, fd, fn_map, binding_types)?;
            }
        }
        _ => {}
    }
    Ok(())
}

/// Best-effort wasm type inference for slot pre-allocation. Mirrors
/// `infer_aver_type` but runs before bodies are emitted, so we
/// reconstruct just enough context (`fn_map` for callee return types,
/// param table for `Resolved` lookups) to handle the cases bench
/// scenarios actually hit.
pub(super) fn infer_expr_wasm_type(
    expr: &Expr,
    registry: &TypeRegistry,
    fd: &FnDef,
    fn_map: &FnMap,
) -> Result<Option<ValType>, WasmGcError> {
    match expr {
        Expr::Literal(Literal::Int(_)) => Ok(Some(ValType::I64)),
        Expr::Literal(Literal::Float(_)) => Ok(Some(ValType::F64)),
        Expr::Literal(Literal::Bool(_)) => Ok(Some(ValType::I32)),
        Expr::Literal(Literal::Unit) => Ok(None),
        Expr::BinOp(op, l, _) => match op {
            BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                Ok(Some(ValType::I32))
            }
            _ => infer_expr_wasm_type(&l.node, registry, fd, fn_map),
        },
        Expr::Resolved { name, .. } => {
            // Param lookup; `let`-bindings don't surface a type via
            // `lookup_var_type` yet, but slots already declared on
            // earlier passes resolve via `wasm_type_of` at emit time.
            for (pname, pty) in &fd.params {
                if pname == name {
                    return aver_to_wasm(pty, Some(registry));
                }
            }
            Ok(Some(ValType::I64))
        }
        Expr::RecordCreate { type_name, .. } => aver_to_wasm(type_name, Some(registry)),
        Expr::Constructor(name, _) => {
            if let Some(info) = registry.variant(name) {
                aver_to_wasm(&info.parent, Some(registry))
            } else {
                Ok(Some(ValType::I64))
            }
        }
        Expr::FnCall(callee, args) => {
            // Dotted callee shapes match `infer_aver_type`'s tail. For
            // pre-emit slot allocation we resolve enough of them to
            // get binding types right.
            if let Expr::Attr(parent, member) = &callee.node {
                if let Some(info) = registry.variant(member) {
                    return aver_to_wasm(&info.parent, Some(registry));
                }
                let parent_name = match &parent.node {
                    Expr::Ident(n) => Some(n.as_str()),
                    Expr::Resolved { name, .. } => Some(name.as_str()),
                    _ => None,
                };
                if let Some(p) = parent_name {
                    let dotted = format!("{p}.{member}");
                    if dotted == "Float.fromInt" {
                        return Ok(Some(ValType::F64));
                    }
                    if dotted == "Int.fromFloat" {
                        return Ok(Some(ValType::I64));
                    }
                    if let Some(&idx) = fn_map.builtins.get(&dotted) {
                        let _ = idx;
                        return aver_to_wasm(builtin_aver_result_type(&dotted), Some(registry));
                    }
                    if fn_map.effects.contains_key(&dotted) {
                        if let Some(ty) = effect_aver_return_type(&dotted) {
                            return aver_to_wasm(ty, Some(registry));
                        }
                        return Ok(None);
                    }
                    if dotted == "Vector.new" && args.len() == 2 {
                        let elem_ty = infer_expr_wasm_type(&args[1].node, registry, fd, fn_map)?;
                        // The slot of a Vector<T> binding is a ref —
                        // resolve via the registered canonical name.
                        // Element type comes from the fill arg's wasm
                        // type via reverse lookup on aver_to_wasm.
                        let _ = elem_ty;
                        // Look up the canonical Vector<T> by walking
                        // registry slots that this fn's signature could
                        // produce. The fill arg's primitive yields the
                        // element type string.
                        let elem_aver =
                            match infer_expr_wasm_type(&args[1].node, registry, fd, fn_map)? {
                                Some(ValType::I64) => "Int",
                                Some(ValType::F64) => "Float",
                                Some(ValType::I32) => "Bool",
                                _ => "Int",
                            };
                        let canonical = format!("Vector<{elem_aver}>");
                        return aver_to_wasm(&canonical, Some(registry));
                    }
                    if dotted == "Vector.set" && args.len() == 3 {
                        return infer_expr_wasm_type(&args[0].node, registry, fd, fn_map);
                    }
                    if dotted == "Vector.get" && args.len() == 2 {
                        // Vector.get returns `Option<T>` — walk param
                        // list to find the vector's `Vector<T>`, derive T,
                        // resolve `Option<T>` slot.
                        if let Expr::Resolved { name, .. } = &args[0].node
                            && let Some(vec_ty) = fd
                                .params
                                .iter()
                                .find(|(n, _)| n == name)
                                .map(|(_, t)| t.as_str())
                            && let Some(elem) = TypeRegistry::vector_element_type(vec_ty)
                        {
                            return aver_to_wasm(
                                &format!("Option<{}>", elem.trim()),
                                Some(registry),
                            );
                        }
                        return Ok(Some(ValType::I64));
                    }
                    if dotted == "Option.withDefault" && args.len() == 2 {
                        return infer_expr_wasm_type(&args[1].node, registry, fd, fn_map);
                    }
                    if dotted == "Option.Some" && args.len() == 1 {
                        // Resolve via inferred inner type → `Option<T>`.
                        let inner_val = infer_expr_wasm_type(&args[0].node, registry, fd, fn_map)?;
                        let inner_aver = match inner_val {
                            Some(ValType::I64) => "Int",
                            Some(ValType::F64) => "Float",
                            Some(ValType::I32) => "Bool",
                            _ => "Int",
                        };
                        return aver_to_wasm(&format!("Option<{inner_aver}>"), Some(registry));
                    }
                    if dotted == "Option.None" {
                        return aver_to_wasm(&fd.return_type, Some(registry));
                    }
                    if dotted == "Map.empty" {
                        if registry.map_order.len() == 1 {
                            return aver_to_wasm(&registry.map_order[0], Some(registry));
                        }
                        return aver_to_wasm(&fd.return_type, Some(registry));
                    }
                    if dotted == "Map.set" && args.len() == 3 {
                        return infer_expr_wasm_type(&args[0].node, registry, fd, fn_map);
                    }
                    if dotted == "Map.get" && args.len() == 2 {
                        // Result is `Option<V>` — infer V from map type.
                        if let Expr::Resolved { name, .. } = &args[0].node
                            && let Some(map_ty) = fd
                                .params
                                .iter()
                                .find(|(n, _)| n == name)
                                .map(|(_, t)| t.as_str())
                            && let Some((_, v)) =
                                super::super::types::parse_map_kv(&map_ty.replace(' ', ""))
                        {
                            return aver_to_wasm(&format!("Option<{v}>"), Some(registry));
                        }
                        return aver_to_wasm("Option<Int>", Some(registry));
                    }
                    if dotted == "Map.len" && args.len() == 1 {
                        return Ok(Some(ValType::I64));
                    }
                    // Builtins whose return is a registered generic
                    // type — Result<T,E> / Option<T> instantiations
                    // collected at TypeRegistry::build time.
                    let canonical_for_dotted: Option<&str> = match dotted.as_str() {
                        "Float.fromString" => Some("Result<Float,String>"),
                        "Int.fromString" => Some("Result<Int,String>"),
                        "Int.mod" => Some("Result<Int,String>"),
                        _ => None,
                    };
                    if let Some(canonical) = canonical_for_dotted {
                        return aver_to_wasm(canonical, Some(registry));
                    }
                    // Single-instruction native builtins that return
                    // primitive wasm types — no registry lookup
                    // needed.
                    let primitive_return = match dotted.as_str() {
                        "Float.floor" | "Float.ceil" | "Float.round" | "Float.abs"
                        | "Float.sqrt" | "Float.min" | "Float.max" | "Float.pi" | "Float.sin"
                        | "Float.cos" | "Float.pow" | "Float.toString" | "Int.toFloat" => {
                            Some(ValType::F64)
                        }
                        "Int.abs" | "Int.min" | "Int.max" | "Int.mod" | "List.len"
                        | "List.length" | "Vector.len" | "Map.len" | "String.len"
                        | "Char.toCode" => Some(ValType::I64),
                        "Bool.and" | "Bool.or" | "Bool.not" | "String.startsWith"
                        | "String.contains" | "Map.has" | "List.contains" => Some(ValType::I32),
                        _ => None,
                    };
                    if let Some(v) = primitive_return {
                        // Float.toString actually returns String —
                        // override above. Same for the *.toString
                        // dotted entries handled via String repr.
                        if matches!(
                            dotted.as_str(),
                            "Float.toString"
                                | "Int.toString"
                                | "String.fromInt"
                                | "String.fromFloat"
                                | "String.toUpper"
                                | "String.toLower"
                                | "String.trim"
                                | "String.replace"
                                | "String.slice"
                        ) {
                            return aver_to_wasm("String", Some(registry));
                        }
                        let _ = v;
                        return Ok(primitive_return);
                    }
                }
            }
            // User-defined fn: look up its return type in fn_map.
            let name = match &callee.node {
                Expr::Ident(n) => n.as_str(),
                Expr::Resolved { name, .. } => name.as_str(),
                _ => return Ok(Some(ValType::I64)),
            };
            if let Some(entry) = fn_map.by_name.get(name) {
                aver_to_wasm(&entry.return_type, Some(registry))
            } else {
                Ok(Some(ValType::I64))
            }
        }
        Expr::TailCall(_) => aver_to_wasm(&fd.return_type, Some(registry)),
        Expr::Match { arms, .. } => arms
            .first()
            .map(|a| infer_expr_wasm_type(&a.body.node, registry, fd, fn_map))
            .unwrap_or(Ok(Some(ValType::I64))),
        // Field access (record.field). Recover the obj's record name —
        // either bare `Resolved` whose param type *is* a record, or a
        // chained `Attr` walked recursively — then look up the field's
        // declared Aver type and lower it to wasm. Without this, e.g.
        // `match state.snake { [h, ..t] -> ... }` infers the subject
        // as the default `i64`, the Cons-pattern slot allocation skips
        // because `ValType::Ref(_)` doesn't match, and the emitted
        // body references locals that were never declared.
        Expr::Attr(obj, field) => {
            if let Some(obj_aver) = attr_obj_aver_type(&obj.node, registry, fd)
                && let Some(field_ty) = registry.record_field_type(&obj_aver, field)
            {
                return aver_to_wasm(field_ty, Some(registry));
            }
            Ok(Some(ValType::I64))
        }
        _ => Ok(Some(ValType::I64)),
    }
}

/// Recover the Aver record name for an Attr's object expression, used
/// by `infer_expr_wasm_type`'s Attr arm. Mirrors the chained-Attr
/// resolution in `infer::struct_name_of_unboxed` — a bare `Resolved`
/// whose param type is a record returns that name; chained
/// `Attr(inner, inner_field)` walks recursively and looks up the
/// nested record by field type.
fn attr_obj_aver_type(expr: &Expr, registry: &TypeRegistry, fd: &FnDef) -> Option<String> {
    match expr {
        Expr::Resolved { name, .. } => fd
            .params
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, t)| t.clone())
            .filter(|t| registry.records.contains_key(t)),
        Expr::Attr(inner, inner_field) => {
            let inner_rec = attr_obj_aver_type(&inner.node, registry, fd)?;
            let field_ty = registry.record_field_type(&inner_rec, inner_field)?;
            if registry.records.contains_key(field_ty) {
                Some(field_ty.to_string())
            } else {
                None
            }
        }
        _ => None,
    }
}

pub(super) fn count_value_params(params: &[(String, String)]) -> usize {
    params.iter().filter(|(_, ty)| ty.trim() != "Unit").count()
}

/// Return the wasm type an expression evaluates to, or `None` for
/// Unit (no value pushed). Used by binop emission to pick i64 vs f64
/// ops without running a separate type-check pass.
pub(super) fn wasm_type_of(
    expr: &Expr,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<ValType>, WasmGcError> {
    match expr {
        Expr::Literal(Literal::Int(_)) => Ok(Some(ValType::I64)),
        Expr::Literal(Literal::Float(_)) => Ok(Some(ValType::F64)),
        Expr::Literal(Literal::Bool(_)) => Ok(Some(ValType::I32)),
        Expr::Literal(Literal::Unit) => Ok(None),
        Expr::Resolved { slot, .. } => Ok(slots.by_slot.get(*slot as usize).copied()),
        Expr::BinOp(op, l, _) => {
            // Comparisons always yield Bool (i32); arithmetic preserves
            // operand type.
            match op {
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                    Ok(Some(ValType::I32))
                }
                _ => wasm_type_of(&l.node, slots, ctx),
            }
        }
        Expr::FnCall(callee, _) => {
            // Dotted callee: builtin or variant constructor.
            if let Expr::Attr(parent, member) = &callee.node {
                let parent_name = match &parent.node {
                    Expr::Ident(n) => Some(n.as_str()),
                    Expr::Resolved { name, .. } => Some(name.as_str()),
                    _ => None,
                };
                if let Some(p) = parent_name {
                    let dotted = format!("{p}.{member}");
                    // Inline numeric conversions FIRST (specific
                    // before fallback).
                    if dotted == "Float.fromInt" {
                        return Ok(Some(ValType::F64));
                    }
                    if dotted == "Int.fromFloat" {
                        return Ok(Some(ValType::I64));
                    }
                    // Registered builtins from the BuiltinRegistry —
                    // checked via the by-name table on `ctx.fn_map`.
                    if ctx.fn_map.builtins.contains_key(&dotted) {
                        let aver_ty = builtin_aver_result_type(&dotted);
                        return aver_to_wasm(aver_ty, Some(ctx.registry));
                    }
                    // Effects: lookup return type via the shared
                    // `effect_aver_return_type` table (mirrors
                    // `EffectName::results` in `effects.rs`).
                    if ctx.fn_map.effects.contains_key(&dotted) {
                        if let Some(ty) = effect_aver_return_type(&dotted) {
                            return aver_to_wasm(ty, Some(ctx.registry));
                        }
                        return Ok(None);
                    }
                    // Variant constructor — returns the parent type's
                    // ref-type carrier.
                    if let Some(info) = ctx.registry.variant(member) {
                        return aver_to_wasm(&info.parent, Some(ctx.registry));
                    }
                    // Fallback: stdlib dotted ops not registered as
                    // helpers (Int.toFloat, Float.floor, Bool.and …).
                    if let Some(ret) = dotted_return_type(&dotted) {
                        return aver_to_wasm(ret, Some(ctx.registry));
                    }
                }
            }
            let name = match &callee.node {
                Expr::Ident(n) => n.as_str(),
                Expr::Resolved { name, .. } => name.as_str(),
                _ => return Ok(None),
            };
            if let Some(entry) = ctx.fn_map.by_name.get(name) {
                Ok(aver_to_wasm(&entry.return_type, Some(ctx.registry))?)
            } else {
                Ok(None)
            }
        }
        Expr::Match { arms, .. } => arms
            .first()
            .map(|a| wasm_type_of(&a.body.node, slots, ctx))
            .unwrap_or(Ok(None)),
        Expr::TailCall(_) => aver_to_wasm(ctx.return_type, Some(ctx.registry)),
        Expr::Attr(obj, field) => {
            // Record field access — recover the field's Aver type
            // from the registered record/variant.
            if let Ok(Some(record_name)) = struct_name_of_unboxed(&obj.node, ctx)
                && let Some(ty) = ctx.registry.record_field_type(&record_name, field)
            {
                return aver_to_wasm(ty, Some(ctx.registry));
            }
            Ok(None)
        }
        _ => Ok(None),
    }
}

/// Look up an Aver type-name string for a local variable — params
/// first, then `let`-binding inferred types.
pub(super) fn lookup_var_type(name: &str, ctx: &EmitCtx<'_>) -> Option<String> {
    if let Some((_, ty)) = ctx.params.iter().find(|(n, _)| n == name) {
        return Some(ty.clone());
    }
    ctx.binding_types.get(name).cloned()
}

/// Pre-pass: collect Aver types for every top-level `let`-binding.
/// Annotation takes precedence; otherwise we sniff the RHS for a
/// known shape (user fn call → its return type; dotted Map.empty →
/// the only registered instantiation; literal → primitive type).
/// Anything we can't classify is left out — `infer_aver_type` falls
/// back to "Int" when a binding type is missing.
pub(super) fn collect_binding_types(
    stmts: &[Stmt],
    fd: &FnDef,
    fn_map: &FnMap,
    registry: &TypeRegistry,
) -> HashMap<String, String> {
    let mut out: HashMap<String, String> = HashMap::new();
    for stmt in stmts {
        if let Stmt::Binding(name, annot, expr) = stmt {
            let ty = annot
                .clone()
                .or_else(|| sniff_aver_type_ext(&expr.node, fd, fn_map, registry, &out));
            if let Some(t) = ty {
                out.insert(name.clone(), t);
            }
        }
        let expr = match stmt {
            Stmt::Binding(_, _, e) | Stmt::Expr(e) => &e.node,
        };
        collect_match_pattern_types(expr, fd, fn_map, registry, &mut out);
    }
    out
}
