//! Expression / statement → wasm-gc instructions.
//!
//! Phase 2 lowering rules (Int / Float / Bool / Unit only):
//!
//! - `Literal(Int|Float|Bool)` → `i64.const` / `f64.const` / `i32.const`.
//! - `Resolved { slot }` → `local.get slot`. Resolver assigns slots in
//!   declaration order (params first, bindings next), which matches
//!   wasm's local-indexing convention 1:1, so no remapping needed.
//! - `BinOp(Add|Sub|Mul|Div, Int, Int)` → `i64.{add,sub,mul,div_s}`.
//! - `BinOp(<comparison>, Int, Int)` → `i64.{eq,ne,lt_s,gt_s,le_s,ge_s}`.
//! - `FnCall(Ident, args)` → `call $idx` after pushing args left-to-right.
//! - `Stmt::Binding` → emit value, then `local.set slot` where slot
//!   is the next-available counter (same convention as resolver).
//!
//! Match / TailCall / dotted calls / compound types belong to phases
//! 3+ and surface as explicit `Unimplemented` errors so a partially-
//! lowered IR can't reach the encoder.

use std::collections::HashMap;

use wasm_encoder::{Function, Instruction, ValType};

use super::WasmGcError;
use super::types::{TypeRegistry, aver_to_wasm};

use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt};
use crate::ir::{CallLowerCtx, LeafOp, classify_leaf_op};

/// Maps fn name → wasm fn index + return type. Built once per module.
pub(super) struct FnMap {
    pub(super) by_name: HashMap<String, FnEntry>,
    /// Dotted builtin name → wasm fn index. Populated by
    /// `module::emit_module` from the `BuiltinRegistry` so call
    /// sites can `call $builtin_idx` for `Int.toString` etc.
    pub(super) builtins: HashMap<String, u32>,
    /// Dotted effect name → wasm fn index (host import). Populated
    /// from `EffectRegistry`. Imports occupy fn idx 0..K so these
    /// indices are always small.
    pub(super) effects: HashMap<String, u32>,
    /// Per-instantiation `Map<K, V>` helpers (empty / set / get / len).
    /// Key is the canonical `Map<K,V>` Aver string. Body emit looks
    /// the canonical up by inferring the type of the map argument.
    pub(super) map_helpers: HashMap<String, super::maps::MapKVHelpers>,
}

pub(super) struct FnEntry {
    pub(super) wasm_idx: u32,
    pub(super) return_type: String,
}

/// Per-fn slot table — one entry per local (param or binding) in
/// resolver-allocation order. Slot N maps to `wasm local N`.
struct SlotTable {
    /// Element index = slot number; element value = wasm ValType.
    by_slot: Vec<ValType>,
    /// Optional scratch slot of `(ref null eq)` reserved for multi-arm
    /// variant dispatch — holds the subject so `ref.test` and
    /// `ref.cast` can read it across arms without recomputing the
    /// match-subject expression. Allocated when the body contains at
    /// least one multi-arm Constructor match. Slot index, when set,
    /// is always the last slot in `by_slot`.
    subject_scratch: Option<u32>,
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
    fn build_for_fn(
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
        // Walk body to collect binding slots (resolver order).
        let FnBody::Block(stmts) = fd.body.as_ref();
        for stmt in stmts {
            collect_binding_slots(stmt, &mut by_slot, registry, fd, fn_map)?;
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

    fn extra_locals(&self, params_count: usize) -> Vec<ValType> {
        self.by_slot.iter().skip(params_count).copied().collect()
    }
}

/// True if the body has at least one multi-arm `match` whose arms are
/// `Pattern::Constructor` against a non-newtype variant. Single-arm
/// matches and newtype matches don't need a scratch (the cast is
/// elided), so we only allocate when really necessary.
fn fn_needs_subject_scratch(fd: &FnDef, registry: &TypeRegistry) -> bool {
    let FnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|s| stmt_needs_scratch(s, registry))
}

fn stmt_needs_scratch(stmt: &Stmt, registry: &TypeRegistry) -> bool {
    match stmt {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => expr_needs_scratch(&e.node, registry),
    }
}

fn expr_needs_scratch(expr: &Expr, registry: &TypeRegistry) -> bool {
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
                && p == "Option"
                && member == "withDefault"
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
        _ => false,
    }
}

/// True when a match arm matches against `Option.Some(_)` or
/// `Option.None`. Used to opt the surrounding match into the
/// dedicated tag-based dispatch path (instead of the generic
/// `ref.test` cascade for user variants).
fn arm_is_option_pattern(arm: &MatchArm) -> bool {
    if let Pattern::Constructor(name, _) = &arm.pattern {
        let bare = name.rsplit('.').next().unwrap_or(name);
        return name == "Option.Some"
            || name == "Option.None"
            || ((bare == "Some" || bare == "None") && name.starts_with("Option"));
    }
    false
}

/// For an Option-shaped match subject, recover the inner T as an Aver
/// type string by reverse-looking-up the wasm type of the subject in
/// the registry's option table. Used by pre-pass slot allocation
/// (`Option.Some(v)` binds `v` to a slot whose wasm type depends on T).
fn subject_option_inner_type(
    subject: &Expr,
    registry: &TypeRegistry,
    fd: &FnDef,
    fn_map: &FnMap,
) -> Option<String> {
    let wasm_ty = infer_expr_wasm_type(subject, registry, fd, fn_map).ok().flatten()?;
    if let ValType::Ref(rt) = wasm_ty
        && let wasm_encoder::HeapType::Concrete(idx) = rt.heap_type
    {
        for canonical in &registry.option_order {
            if registry.option_types.get(canonical).copied() == Some(idx) {
                return TypeRegistry::option_element_type(canonical).map(|s| s.to_string());
            }
        }
    }
    None
}

fn collect_binding_slots(
    stmt: &Stmt,
    out: &mut Vec<ValType>,
    registry: &TypeRegistry,
    fd: &FnDef,
    fn_map: &FnMap,
) -> Result<(), WasmGcError> {
    match stmt {
        Stmt::Binding(_, annot, expr) => {
            let ty = if let Some(t) = annot.as_deref() {
                aver_to_wasm(t, Some(registry))?
            } else {
                infer_expr_wasm_type(&expr.node, registry, fd, fn_map)?
            };
            if let Some(v) = ty {
                out.push(v);
            }
            collect_expr_binding_slots(&expr.node, out, registry, fd, fn_map)?;
        }
        Stmt::Expr(spanned) => {
            collect_expr_binding_slots(&spanned.node, out, registry, fd, fn_map)?
        }
    }
    Ok(())
}

fn collect_expr_binding_slots(
    expr: &Expr,
    out: &mut Vec<ValType>,
    registry: &TypeRegistry,
    fd: &FnDef,
    fn_map: &FnMap,
) -> Result<(), WasmGcError> {
    match expr {
        Expr::Match { subject, arms } => {
            collect_expr_binding_slots(&subject.node, out, registry, fd, fn_map)?;
            // Built-in Option arms — `Option.Some(v)` binds v to T
            // (read off the subject's option<T> wasm type).
            let is_option = arms.iter().any(arm_is_option_pattern);
            if is_option {
                let inner = subject_option_inner_type(&subject.node, registry, fd, fn_map);
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
                                 subject's Aver type must reduce to Option<T>".into(),
                            ))?;
                            if let Some(v) = aver_to_wasm(inner_ty, Some(registry))? {
                                out.push(v);
                            }
                        }
                    }
                    collect_expr_binding_slots(&arm.body.node, out, registry, fd, fn_map)?;
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
                if let Pattern::Cons(_, _) = &arm.pattern {
                    // Phase 3b: List binding slots
                    return Err(WasmGcError::Unimplemented(
                        "phase 3b — Cons pattern bindings",
                    ));
                }
                collect_expr_binding_slots(&arm.body.node, out, registry, fd, fn_map)?;
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_expr_binding_slots(&l.node, out, registry, fd, fn_map)?;
            collect_expr_binding_slots(&r.node, out, registry, fd, fn_map)?;
        }
        Expr::FnCall(callee, args) => {
            collect_expr_binding_slots(&callee.node, out, registry, fd, fn_map)?;
            for arg in args {
                collect_expr_binding_slots(&arg.node, out, registry, fd, fn_map)?;
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                collect_expr_binding_slots(&arg.node, out, registry, fd, fn_map)?;
            }
        }
        Expr::Attr(obj, _) => collect_expr_binding_slots(&obj.node, out, registry, fd, fn_map)?,
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                collect_expr_binding_slots(&p.node, out, registry, fd, fn_map)?;
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_expr_binding_slots(&e.node, out, registry, fd, fn_map)?;
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
fn infer_expr_wasm_type(
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
                        let elem_aver = match infer_expr_wasm_type(
                            &args[1].node,
                            registry,
                            fd,
                            fn_map,
                        )? {
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
                        // Element type of Vector<T>: walk param list to
                        // find the vector's Aver type, derive element.
                        if let Expr::Resolved { name, .. } = &args[0].node
                            && let Some(vec_ty) = fd
                                .params
                                .iter()
                                .find(|(n, _)| n == name)
                                .map(|(_, t)| t.as_str())
                            && let Some(elem) = TypeRegistry::vector_element_type(vec_ty)
                        {
                            return aver_to_wasm(elem, Some(registry));
                        }
                        return Ok(Some(ValType::I64));
                    }
                    if dotted == "Option.withDefault" && args.len() == 2 {
                        return infer_expr_wasm_type(&args[1].node, registry, fd, fn_map);
                    }
                    if dotted == "Option.Some" && args.len() == 1 {
                        // Resolve via inferred inner type → `Option<T>`.
                        let inner_val =
                            infer_expr_wasm_type(&args[0].node, registry, fd, fn_map)?;
                        let inner_aver = match inner_val {
                            Some(ValType::I64) => "Int",
                            Some(ValType::F64) => "Float",
                            Some(ValType::I32) => "Bool",
                            _ => "Int",
                        };
                        return aver_to_wasm(
                            &format!("Option<{inner_aver}>"),
                            Some(registry),
                        );
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
                                super::types::parse_map_kv(&map_ty.replace(' ', ""))
                        {
                            return aver_to_wasm(&format!("Option<{v}>"), Some(registry));
                        }
                        return aver_to_wasm("Option<Int>", Some(registry));
                    }
                    if dotted == "Map.len" && args.len() == 1 {
                        return Ok(Some(ValType::I64));
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
        _ => Ok(Some(ValType::I64)),
    }
}

fn count_value_params(params: &[(String, String)]) -> usize {
    params.iter().filter(|(_, ty)| ty.trim() != "Unit").count()
}

/// Lower the body of `fd` into the supplied wasm `Function` builder.
/// Returns the list of *extra* locals (beyond params) needed for the
/// fn signature; caller passes these to `Function::new`.
///
/// `self_wasm_idx` is the current fn's own wasm index — used for
/// emitting `return_call $self` on `Expr::TailCall` to the same fn.
/// Mutual-TCO across SCC members goes through a `return_call_indirect`
/// table; that wiring lives in module.rs once phase 4b lands.
pub(super) fn emit_fn_body(
    func: &mut Function,
    fd: &FnDef,
    fn_map: &FnMap,
    self_wasm_idx: u32,
    registry: &TypeRegistry,
) -> Result<Vec<ValType>, WasmGcError> {
    let slots = SlotTable::build_for_fn(fd, registry, fn_map)?;
    let FnBody::Block(stmts) = fd.body.as_ref();
    let last_idx = stmts.len().saturating_sub(1);

    // Pre-pass: walk top-level `let`-bindings and collect their Aver
    // types so `infer_aver_type` on a `Resolved` binding gets the
    // right answer. Annotation wins; otherwise fall back to a simple
    // RHS sniff (user fn return type, dotted-builtin result, literal).
    let binding_types = collect_binding_types(stmts, fd, fn_map, registry);

    let ctx = EmitCtx {
        fn_map,
        self_wasm_idx,
        self_fn_name: fd.name.as_str(),
        return_type: fd.return_type.as_str(),
        registry,
        resolution: fd.resolution.as_ref(),
        params: &fd.params,
        binding_types: &binding_types,
    };

    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == last_idx;
        match stmt {
            Stmt::Binding(name, _annot, expr) => {
                emit_expr(func, &expr.node, &slots, &ctx)?;
                let slot = ctx
                    .self_local_slot(name)
                    .ok_or(WasmGcError::Validation(format!(
                        "binding `{name}` has no resolver slot"
                    )))?;
                // Skip the local.set for Unit-typed bindings — they
                // produce no stack value.
                if (slot as usize) < slots.by_slot.len() {
                    func.instruction(&Instruction::LocalSet(slot));
                }
            }
            Stmt::Expr(spanned) => {
                emit_expr(func, &spanned.node, &slots, &ctx)?;
                let aver_ty = infer_aver_type(&spanned.node, &ctx)?;
                let produces_value = aver_to_wasm(&aver_ty, Some(ctx.registry))?.is_some();
                if !is_last && produces_value {
                    func.instruction(&Instruction::Drop);
                }
                if is_last {
                    if fd.return_type.trim() == "Unit" && produces_value {
                        func.instruction(&Instruction::Drop);
                    } else if fd.return_type.trim() != "Unit" && !produces_value {
                        return Err(WasmGcError::Validation(format!(
                            "fn `{}` returns {} but trailing expression yields no value",
                            fd.name, fd.return_type
                        )));
                    }
                }
            }
        }
    }
    func.instruction(&Instruction::End);

    Ok(slots.extra_locals(count_value_params(&fd.params)))
}

/// Per-fn lowering context — read-only state every emit fn needs.
struct EmitCtx<'a> {
    fn_map: &'a FnMap,
    self_wasm_idx: u32,
    self_fn_name: &'a str,
    return_type: &'a str,
    registry: &'a TypeRegistry,
    /// Resolver's local-name → slot map for the current fn. `None`
    /// when the fn was emitted without `resolution` populated (the
    /// pipeline always populates it for production paths; tests may
    /// pre-resolve manually).
    resolution: Option<&'a crate::ast::FnResolution>,
    /// Param name → declared aver type. Used to recover the original
    /// type of a Resolved param when its wasm slot has been erased
    /// to a primitive (newtype optimization).
    params: &'a [(String, String)],
    /// Binding name → inferred Aver type. Built once per fn from
    /// `let`-binding annotations or simple RHS inference (user fn
    /// return type, dotted-builtin result type). Used by
    /// `infer_aver_type` so a Resolved binding resolves to its
    /// declared / inferred Aver type rather than the "Int" fallback.
    binding_types: &'a HashMap<String, String>,
}

impl<'a> EmitCtx<'a> {
    /// Look up a local-name → wasm slot. Resolver slots are 1:1 with
    /// wasm local indices.
    fn self_local_slot(&self, name: &str) -> Option<u32> {
        self.resolution
            .as_ref()
            .and_then(|r| r.local_slots.get(name).copied())
            .map(|s| s as u32)
    }
}

/// `CallLowerCtx` impl so the shared IR-level shape recognition
/// (`classify_leaf_op`, `classify_call_plan`) can be reused here
/// instead of each backend re-implementing the same patterns. Wasm-gc
/// is single-module today so module resolution returns None; the
/// other two predicates fall out of the registry + binding/param
/// tables we already maintain.
impl<'a> CallLowerCtx for EmitCtx<'a> {
    fn is_local_value(&self, name: &str) -> bool {
        self.params.iter().any(|(n, _)| n == name) || self.binding_types.contains_key(name)
    }

    fn is_user_type(&self, name: &str) -> bool {
        self.registry.records.contains_key(name)
            || self.registry.variants.contains_key(name)
            || self
                .registry
                .variants
                .values()
                .any(|info| info.parent == name)
    }

    fn resolve_module_call<'b>(&self, _dotted: &'b str) -> Option<(&'b str, &'b str)> {
        None
    }
}

/// Return the wasm type an expression evaluates to, or `None` for
/// Unit (no value pushed). Used by binop emission to pick i64 vs f64
/// ops without running a separate type-check pass.
fn wasm_type_of(
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
                    // Variant constructor — returns the parent type's
                    // ref-type carrier.
                    if let Some(info) = ctx.registry.variant(member) {
                        return aver_to_wasm(&info.parent, Some(ctx.registry));
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
        _ => Ok(None),
    }
}

/// Type inference over the limited shape phase 2/4 emits. Returns the
/// Aver type string. Errors on shapes that belong to a later phase,
/// with a message pointing at it.
fn infer_aver_type(expr: &Expr, ctx: &EmitCtx<'_>) -> Result<String, WasmGcError> {
    match expr {
        Expr::Literal(Literal::Int(_)) => Ok("Int".into()),
        Expr::Literal(Literal::Float(_)) => Ok("Float".into()),
        Expr::Literal(Literal::Bool(_)) => Ok("Bool".into()),
        Expr::Literal(Literal::Unit) => Ok("Unit".into()),
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
                        return Ok("Unit".into());
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
                        return infer_aver_type(&args[0].node, ctx);
                    }
                    if dotted == "Vector.get" && args.len() == 2 {
                        let vec_ty = infer_aver_type(&args[0].node, ctx)?;
                        if let Some(elem) =
                            super::types::TypeRegistry::vector_element_type(&vec_ty)
                        {
                            return Ok(elem.into());
                        }
                        return Ok("Int".into());
                    }
                    if dotted == "Option.withDefault" && args.len() == 2 {
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
                            super::types::parse_map_kv(&map_ty.replace(' ', ""))
                        {
                            return Ok(format!("Option<{v}>"));
                        }
                        return Ok("Option<Int>".into());
                    }
                    if dotted == "Map.len" && args.len() == 1 {
                        return Ok("Int".into());
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
        Expr::RecordCreate { type_name, .. } => Ok(type_name.clone()),
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
            if let Ok(Some(record_name)) = struct_name_of_unboxed(&obj.node, ctx) {
                if let Some(ty) = ctx.registry.record_field_type(&record_name, field) {
                    return Ok(ty.into());
                }
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
        _ => Err(WasmGcError::Unimplemented(
            "expression shape outside phase 2/3/4",
        )),
    }
}

/// Best-effort record-type-name lookup that walks the AST plus the
/// fn's param/binding type table — used when the slot's wasm ValType
/// isn't a struct ref (newtype optimization erases it to a primitive,
/// so `struct_name_of` based on slots can't see the original type).
fn struct_name_of_unboxed(expr: &Expr, ctx: &EmitCtx<'_>) -> Result<Option<String>, WasmGcError> {
    if let Expr::Resolved { name, .. } = expr {
        // First: is the name itself a record?
        if ctx.registry.records.contains_key(name) {
            return Ok(Some(name.clone()));
        }
        // Otherwise: look up the variable's declared type via the
        // resolution-driven param map (we don't have a binding-type
        // map yet, so this only works for params).
        if let Some(ty) = lookup_var_type(name, ctx) {
            if ctx.registry.records.contains_key(&ty) {
                return Ok(Some(ty));
            }
        }
    }
    Ok(None)
}

/// Look up an Aver type-name string for a local variable — params
/// first, then `let`-binding inferred types.
fn lookup_var_type(name: &str, ctx: &EmitCtx<'_>) -> Option<String> {
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
fn collect_binding_types(
    stmts: &[Stmt],
    fd: &FnDef,
    fn_map: &FnMap,
    registry: &TypeRegistry,
) -> HashMap<String, String> {
    let mut out: HashMap<String, String> = HashMap::new();
    for stmt in stmts {
        if let Stmt::Binding(name, annot, expr) = stmt {
            let ty = annot
                .as_ref()
                .map(|a| a.clone())
                .or_else(|| sniff_aver_type(&expr.node, fd, fn_map, registry));
            if let Some(t) = ty {
                out.insert(name.clone(), t);
            }
        }
    }
    out
}

/// Best-effort Aver type for an RHS expression — same shape as
/// `infer_aver_type`, but operates without the full `EmitCtx` so it
/// can run during pre-pass binding discovery.
fn sniff_aver_type(
    expr: &Expr,
    fd: &FnDef,
    fn_map: &FnMap,
    registry: &TypeRegistry,
) -> Option<String> {
    match expr {
        Expr::Literal(Literal::Int(_)) => Some("Int".into()),
        Expr::Literal(Literal::Float(_)) => Some("Float".into()),
        Expr::Literal(Literal::Bool(_)) => Some("Bool".into()),
        Expr::Literal(Literal::Str(_)) => Some("String".into()),
        Expr::Literal(Literal::Unit) => Some("Unit".into()),
        Expr::Resolved { name, .. } => fd
            .params
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, t)| t.clone()),
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
                        "Map.set" if !args.is_empty() => {
                            return sniff_aver_type(&args[0].node, fd, fn_map, registry);
                        }
                        "Map.get" if !args.is_empty() => {
                            let m = sniff_aver_type(&args[0].node, fd, fn_map, registry)?;
                            let canonical = m.replace(' ', "");
                            if let Some((_, v)) = super::types::parse_map_kv(&canonical) {
                                return Some(format!("Option<{v}>"));
                            }
                            return None;
                        }
                        "Map.len" => return Some("Int".into()),
                        "Vector.new" if args.len() == 2 => {
                            let elem = sniff_aver_type(&args[1].node, fd, fn_map, registry)?;
                            return Some(format!("Vector<{elem}>"));
                        }
                        "Int.toString" => return Some("String".into()),
                        "String.len" => return Some("Int".into()),
                        _ => {}
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
        _ => None,
    }
}


/// Aver result type for a registered builtin. Mirrors
/// `BuiltinName::results` but returns a `&'static str` for type
/// inference. Adding a new builtin: extend both.
fn builtin_aver_result_type(dotted: &str) -> &'static str {
    match dotted {
        "Int.toString" => "String",
        "String.len" => "Int",
        _ => "Int",
    }
}

fn parent_dotted_head(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Ident(n) => Some(n.as_str()),
        Expr::Resolved { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

/// Emit a "default value" of the given Aver primitive / ref type onto
/// the wasm stack. Used by `Option.None` constructor to satisfy
/// `struct.new`'s requirement that every field has an initial value
/// — the value field of a None-tagged Option is never read by
/// well-typed Aver code (pattern match dispatches on tag first).
fn emit_default_value(
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
            func.instruction(&Instruction::F64Const(0.0.into()));
            Ok(())
        }
        "Bool" => {
            func.instruction(&Instruction::I32Const(0));
            Ok(())
        }
        other => {
            // Ref types: emit `ref.null $T`. The exact heap type comes
            // from the resolved wasm representation.
            let val = aver_to_wasm(other, Some(registry))?;
            match val {
                Some(ValType::Ref(rt)) => {
                    func.instruction(&Instruction::RefNull(rt.heap_type));
                    Ok(())
                }
                Some(_) => Err(WasmGcError::Validation(format!(
                    "Option.None default for `{other}` resolved to a non-ref primitive but no default emitter matched"
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
fn emit_option_constructor(
    func: &mut Function,
    payload: Option<&Spanned<Expr>>,
    t_aver_hint: Option<&str>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    // Resolve T. From payload type if present, else from the hint.
    let t_aver: String = match payload {
        Some(p) => infer_aver_type(&p.node, ctx)?,
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
    let opt_idx = ctx.registry.option_type_idx(&canonical).ok_or(
        WasmGcError::Validation(format!(
            "Option constructor: instantiation `{canonical}` was not registered. \
             Discovery should have walked fn signatures + bodies."
        )),
    )?;
    let inner_ty = TypeRegistry::option_element_type(&canonical).ok_or(
        WasmGcError::Validation(format!("Option canonical `{canonical}` has no element type")),
    )?;

    match payload {
        Some(p) => {
            func.instruction(&Instruction::I32Const(1));
            emit_expr(func, &p.node, slots, ctx)?;
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
fn emit_expr(
    func: &mut Function,
    expr: &Expr,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    match expr {
        Expr::Literal(Literal::Int(n)) => {
            func.instruction(&Instruction::I64Const(*n));
        }
        Expr::Literal(Literal::Float(f)) => {
            func.instruction(&Instruction::F64Const((*f).into()));
        }
        Expr::Literal(Literal::Bool(b)) => {
            func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
        }
        Expr::Literal(Literal::Unit) => {}
        Expr::Literal(Literal::Str(s)) => {
            // String literal → passive data segment; emit
            // `array.new_data $string $seg` with offset=0, size=len.
            let bytes = s.as_bytes();
            let seg_idx = ctx.registry.string_literal_segment(bytes).ok_or(
                WasmGcError::Validation(format!(
                    "String literal `{s:?}` was not registered in the data segment table"
                )),
            )?;
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
        Expr::Literal(_) => {
            return Err(WasmGcError::Unimplemented(
                "phase 3 — Char / other literals",
            ));
        }
        Expr::InterpolatedStr(parts) => {
            emit_interpolated_str(func, parts, slots, ctx)?;
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
            emit_expr(func, &l.node, slots, ctx)?;
            emit_expr(func, &r.node, slots, ctx)?;
            // Pick op-set by the operand wasm type. Aver's type checker
            // has already proven both operands have the same type, so
            // peeking at the LHS suffices.
            let operand = wasm_type_of(&l.node, slots, ctx)?;
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
                        "None" => emit_option_constructor(
                            func,
                            None,
                            Some(ctx.return_type),
                            slots,
                            ctx,
                        ),
                        _ => Err(WasmGcError::Validation(format!(
                            "Option.{member} with {} args is not a valid constructor",
                            args.len()
                        ))),
                    };
                }
                if let Some(info) = ctx.registry.variant(member).cloned() {
                    return emit_constructor_with_args(func, &info, args, slots, ctx);
                }
                // Builtins: `Type.method(args...)` shape. We support
                // a curated set today (the ones bench scenarios use);
                // anything else surfaces as Unimplemented.
                if let Expr::Ident(parent_name) = &parent.node {
                    return emit_dotted_builtin(func, parent_name, member, args, slots, ctx);
                }
                if let Expr::Resolved {
                    name: parent_name, ..
                } = &parent.node
                {
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
            for arg in args {
                emit_expr(func, &arg.node, slots, ctx)?;
            }
            let entry = ctx
                .fn_map
                .by_name
                .get(name)
                .ok_or(WasmGcError::Validation(format!(
                    "call to unknown fn `{name}`"
                )))?;
            func.instruction(&Instruction::Call(entry.wasm_idx));
        }
        Expr::Match { subject, arms } => emit_match(func, subject, arms, slots, ctx)?,
        Expr::TailCall(boxed) => emit_tail_call(func, &boxed.target, &boxed.args, slots, ctx)?,
        Expr::RecordCreate { type_name, fields } => {
            emit_record_create(func, type_name, fields, slots, ctx)?
        }
        Expr::Attr(obj, field) => {
            // `Option.None` lands here as a bare attribute reference
            // (parser doesn't synthesise a FnCall for nullary
            // constructors). Catch it before falling into struct field
            // access, which would never resolve.
            if let Expr::Ident(p) = &obj.node
                && p == "Option"
                && field == "None"
            {
                emit_option_constructor(func, None, Some(ctx.return_type), slots, ctx)?;
            } else {
                emit_attr_get(func, obj, field, slots, ctx)?;
            }
        }
        Expr::Constructor(name, payload) => {
            emit_constructor(func, name, payload.as_deref(), slots, ctx)?
        }
        _ => {
            return Err(WasmGcError::Unimplemented(
                "expression shape outside phase 2/3/4",
            ));
        }
    }
    Ok(())
}

/// Lower `RecordCreate { type_name, fields }` to `struct.new $type_idx`.
/// Aver records have `RecordCreate` field order coming from source
/// position — we re-order to the declaration order from `TypeRegistry`
/// before pushing values, so the wasm struct layout always matches the
/// declared shape.
fn emit_record_create(
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
        return emit_expr(func, &field.1.node, slots, ctx);
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
    for (decl_name, _) in decl_fields {
        let provided =
            fields
                .iter()
                .find(|(n, _)| n == decl_name)
                .ok_or(WasmGcError::Validation(format!(
                    "record `{type_name}` missing field `{decl_name}`"
                )))?;
        emit_expr(func, &provided.1.node, slots, ctx)?;
    }
    func.instruction(&Instruction::StructNew(type_idx));
    Ok(())
}

/// Lower `Attr(obj, field)` to `obj; struct.get $type_idx $field_idx`.
/// We need to know the struct type of `obj` — for now we infer it
/// from the slot's wasm type via the registry's reverse map.
fn emit_attr_get(
    func: &mut Function,
    obj: &Spanned<Expr>,
    field: &str,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    // Try the slot table first (Resolved on record-typed slot), then
    // fall back to walking the AST for newtype detection (where the
    // slot type is the underlying primitive, not a struct ref).
    let from_slots = struct_name_of(&obj.node, slots, ctx)?;
    let from_ast = struct_name_of_unboxed(&obj.node, ctx)?;
    if let Some(name) = from_ast.as_deref()
        && ctx.registry.newtype_underlying(name).is_some()
    {
        // Newtype: Attr is identity — just emit `obj` and return its
        // primitive value directly.
        return emit_expr(func, &obj.node, slots, ctx);
    }
    let record_name = from_slots.or(from_ast).ok_or(WasmGcError::Unimplemented(
        "phase 3b — Attr on non-Resolved obj (chained access)",
    ))?;
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
    emit_expr(func, &obj.node, slots, ctx)?;
    func.instruction(&Instruction::StructGet {
        struct_type_index: type_idx,
        field_index: field_idx,
    });
    Ok(())
}

/// Try to find the record-type name that an expression evaluates to.
/// Phase 3a handles the common case: `Resolved` whose slot is a
/// concrete struct ref. For deeper analysis (Attr → Attr chains) we'd
/// need a real type-of-expr pass.
fn struct_name_of(
    expr: &Expr,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<String>, WasmGcError> {
    if let Expr::Resolved { slot, .. } = expr {
        if let Some(ValType::Ref(rt)) = slots.by_slot.get(*slot as usize) {
            if let wasm_encoder::HeapType::Concrete(idx) = rt.heap_type {
                // Reverse-lookup the registry by type idx.
                for (name, recorded_idx) in &ctx.registry.records {
                    if *recorded_idx == idx {
                        return Ok(Some(name.clone()));
                    }
                }
            }
        }
    }
    Ok(None)
}

/// Fused `match Map.get(m, k) { Option.Some(v) -> body1; Option.None
/// -> body2 }` — calls the per-(K,V) `get_pair` helper which returns
/// `(i32 found, V value)` as a multi-result. The caller pops `value`
/// into the binding slot, then branches on `found`. Never allocates
/// Option<V>; same probe loop runs but its result lands directly on
/// the wasm stack.
fn emit_map_get_match_fused(
    func: &mut Function,
    map: &Spanned<Expr>,
    key: &Spanned<Expr>,
    arms: &[MatchArm],
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let map_aver = infer_aver_type(&map.node, ctx)?;
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

    emit_expr(func, &map.node, slots, ctx)?;
    emit_expr(func, &key.node, slots, ctx)?;
    func.instruction(&Instruction::Call(helpers.get_pair));
    // Stack now: [..., found(i32), value(V)]. Pop V into the Some
    // binding slot (if any); the value is harmlessly dead in the
    // None branch (we always pop, regardless of which arm fires —
    // wasm requires a balanced stack across the branch boundary).
    if let Pattern::Constructor(_, bindings) = &some_arm.pattern
        && let Some(binding_name) = bindings.first()
        && binding_name != "_"
    {
        let slot = ctx
            .self_local_slot(binding_name)
            .ok_or(WasmGcError::Validation(format!(
                "Map.get fusion: Some binding `{binding_name}` has no resolver slot"
            )))?;
        func.instruction(&Instruction::LocalSet(slot));
    } else {
        // No binding (or wildcard) — drop the value.
        func.instruction(&Instruction::Drop);
    }
    // Stack: [..., found(i32)]. Branch.
    func.instruction(&Instruction::If(block_ty));
    emit_expr(func, &some_arm.body.node, slots, ctx)?;
    func.instruction(&Instruction::Else);
    emit_expr(func, &none_arm.body.node, slots, ctx)?;
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
fn emit_option_match(
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
    let subject_ty = infer_aver_type(&subject.node, ctx)?;
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
    emit_expr(func, &subject.node, slots, ctx)?;
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
    if let Pattern::Constructor(_, bindings) = &some_arm.pattern
        && let Some(binding_name) = bindings.first()
        && binding_name != "_"
    {
        let slot = ctx
            .self_local_slot(binding_name)
            .ok_or(WasmGcError::Validation(format!(
                "Option.Some binding `{binding_name}` has no resolver slot"
            )))?;
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
    emit_expr(func, &some_arm.body.node, slots, ctx)?;

    func.instruction(&Instruction::Else);
    emit_expr(func, &none_arm.body.node, slots, ctx)?;
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
fn emit_variant_dispatch(
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
    emit_expr(func, &subject.node, slots, ctx)?;
    func.instruction(&Instruction::LocalSet(scratch));

    emit_variant_arm_cascade(func, arms, block_ty, scratch, slots, ctx)
}

fn emit_variant_arm_cascade(
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
            let info = ctx
                .registry
                .variant(bare)
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
fn emit_arm_body(
    func: &mut Function,
    arm: &MatchArm,
    subject_scratch: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if let Pattern::Constructor(name, bindings) = &arm.pattern {
        let bare = name.rsplit('.').next().unwrap_or(name);
        let info = ctx
            .registry
            .variant(bare)
            .ok_or(WasmGcError::Validation(format!(
                "unknown variant `{name}` in match"
            )))?;
        // Newtype: subject IS the underlying primitive — read the
        // scratch directly. (Won't happen here in practice because
        // newtype matches go through the single-arm path, but
        // handle it for symmetry.)
        if ctx.registry.newtype_underlying(&info.parent).is_some() && bindings.len() == 1 {
            let slot = ctx
                .self_local_slot(&bindings[0])
                .ok_or(WasmGcError::Validation(format!(
                    "binding `{}` has no resolver slot",
                    bindings[0]
                )))?;
            func.instruction(&Instruction::LocalGet(subject_scratch));
            func.instruction(&Instruction::LocalSet(slot));
            return emit_expr(func, &arm.body.node, slots, ctx);
        }
        // Extract each field into its bound slot.
        for (i, binding_name) in bindings.iter().enumerate() {
            if binding_name == "_" {
                continue;
            }
            let slot = ctx
                .self_local_slot(binding_name)
                .ok_or(WasmGcError::Validation(format!(
                    "binding `{binding_name}` has no resolver slot"
                )))?;
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
        return emit_expr(func, &arm.body.node, slots, ctx);
    }
    // Wildcard / non-pattern arms: just emit body.
    emit_expr(func, &arm.body.node, slots, ctx)
}

/// Lower a single-arm `match subject { Variant(bindings) -> body }`.
/// Used for newtype-style sum types: cast subject down to the concrete
/// variant struct, extract each field into its bound local, then emit
/// the body. No dispatch required — the type checker has already
/// proven this is the only variant the subject can be.
///
/// Multi-arm variant matches need `ref.test` + cascading branches; that
/// lands in phase 3b.
fn emit_single_variant_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    constructor: &str,
    bindings: &[String],
    body: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    // Constructor names in patterns are dotted (e.g. `UserId.UserId`);
    // the registry stores by the bare variant name.
    let bare = constructor.rsplit('.').next().unwrap_or(constructor);
    let info = ctx
        .registry
        .variant(bare)
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
        let binding_name = &bindings[0];
        let slot = ctx
            .self_local_slot(binding_name)
            .ok_or(WasmGcError::Validation(format!(
                "binding `{binding_name}` has no resolver slot"
            )))?;
        emit_expr(func, &subject.node, slots, ctx)?;
        func.instruction(&Instruction::LocalSet(slot));
        emit_expr(func, &body.node, slots, ctx)?;
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
        emit_expr(func, &subject.node, slots, ctx)?;
        func.instruction(&Instruction::Drop);
        emit_expr(func, &body.node, slots, ctx)?;
        return Ok(());
    }

    // Stash the cast subject in a fresh local slot so we can
    // struct.get each field without recomputing.
    emit_expr(func, &subject.node, slots, ctx)?;
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
        let binding_name = &bindings[0];
        let slot = ctx
            .self_local_slot(binding_name)
            .ok_or(WasmGcError::Validation(format!(
                "binding `{binding_name}` has no resolver slot"
            )))?;
        func.instruction(&Instruction::StructGet {
            struct_type_index: variant_idx,
            field_index: 0,
        });
        func.instruction(&Instruction::LocalSet(slot));
        emit_expr(func, &body.node, slots, ctx)?;
        return Ok(());
    }

    Err(WasmGcError::Unimplemented(
        "phase 3b — multi-binding variant patterns (need a scratch slot)",
    ))
}

/// Lower a `Type.Variant(args...)` call (parsed as `FnCall(Attr, args)`)
/// to `struct.new $variant_type_idx`. Used by both the Constructor expr
/// path and the disguised-FnCall path.
/// Lower a dotted builtin call like `Float.fromInt(n)` or
/// `Int.toString(n)`. The set is curated — phase 3b ships the
/// minimum the bench scenarios need; anything else surfaces an
/// "Unimplemented — phase 3c builtin" error so the missing one is
/// visible.
fn emit_dotted_builtin(
    func: &mut Function,
    parent: &str,
    method: &str,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let dotted = format!("{parent}.{method}");

    // Registered helper builtin? Push args, emit `call $idx`.
    if let Some(&wasm_idx) = ctx.fn_map.builtins.get(&dotted) {
        for arg in args {
            emit_expr(func, &arg.node, slots, ctx)?;
        }
        func.instruction(&Instruction::Call(wasm_idx));
        return Ok(());
    }

    // Registered effect import? Same shape — push args, call by idx.
    // Effects return Unit; the trailing instruction sequence works
    // identically to a Unit-returning user fn call.
    if let Some(&wasm_idx) = ctx.fn_map.effects.get(&dotted) {
        for arg in args {
            emit_expr(func, &arg.node, slots, ctx)?;
        }
        func.instruction(&Instruction::Call(wasm_idx));
        return Ok(());
    }

    match dotted.as_str() {
        // Float.fromInt(Int) -> Float
        "Float.fromInt" => {
            if args.len() != 1 {
                return Err(WasmGcError::Validation(format!(
                    "Float.fromInt expects 1 arg, got {}",
                    args.len()
                )));
            }
            emit_expr(func, &args[0].node, slots, ctx)?;
            func.instruction(&Instruction::F64ConvertI64S);
            Ok(())
        }
        // Int.fromFloat(Float) -> Int
        "Int.fromFloat" => {
            if args.len() != 1 {
                return Err(WasmGcError::Validation(format!(
                    "Int.fromFloat expects 1 arg, got {}",
                    args.len()
                )));
            }
            emit_expr(func, &args[0].node, slots, ctx)?;
            func.instruction(&Instruction::I64TruncF64S);
            Ok(())
        }
        // Vector.new(size, fill) -> Vector<T>. Element type T is read
        // off the fill argument. Lowers to native `array.new $vector_T`.
        "Vector.new" => emit_vector_new(func, args, slots, ctx),
        // Option.withDefault(opt, default) — recognise the two fused
        // shapes that show up in vector_ops without ever materialising
        // an Option<T>. Anything else needs real Option boxing, which
        // a later phase introduces when it stops being avoidable.
        "Option.withDefault" => emit_option_with_default(func, args, slots, ctx),
        // Map<K, V> — dispatch to the per-instantiation helper. The
        // canonical comes from inferring the type of the map argument
        // (or the surrounding context for Map.empty).
        "Map.empty" => emit_map_empty_call(func, args, slots, ctx),
        "Map.set" | "Map.get" | "Map.len" => emit_map_kv_call(func, method, args, slots, ctx),
        other => Err(WasmGcError::Unimplemented(match other {
            "Int.toString" => "phase 3c — Int.toString (needs String repr)",
            "Float.toString" => "phase 3c — Float.toString (needs String repr)",
            "String.length" => "phase 3c — String.length",
            "String.join" => "phase 3c — String.join",
            "List.prepend" => "phase 3c — List.prepend (needs List repr)",
            "List.reverse" => "phase 3c — List.reverse",
            "List.length" => "phase 3c — List.length",
            "Vector.set" => "phase 3c — Vector.set (only fused withDefault shape today)",
            "Vector.get" => "phase 3c — Vector.get (only fused withDefault shape today)",
            "Console.print" => "phase 3c — Console.print (effect lowering)",
            _ => "phase 3c — unknown builtin or method call",
        })),
    }
}

/// `Map.empty()` → `call $map_empty_KV`. With a single registered
/// instantiation the canonical is unambiguous; with several, the type
/// must be deducible from the surrounding context (which today only
/// works when one instantiation exists — generalising would mean
/// threading expected-type through expression emission).
fn emit_map_empty_call(
    func: &mut Function,
    args: &[Spanned<Expr>],
    _slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if !args.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "Map.empty expects 0 args, got {}",
            args.len()
        )));
    }
    let canonical = if ctx.registry.map_order.len() == 1 {
        ctx.registry.map_order[0].clone()
    } else {
        return Err(WasmGcError::Unimplemented(
            "Map.empty across multiple Map<K,V> instantiations needs context-driven type inference",
        ));
    };
    let helpers = ctx
        .fn_map
        .map_helpers
        .get(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.empty: helpers missing for `{canonical}`"
        )))?;
    func.instruction(&Instruction::Call(helpers.empty));
    Ok(())
}

/// Map.set / Map.get / Map.len dispatch — the canonical is recovered
/// from the map argument's inferred type, helper indices come from
/// `fn_map.map_helpers`.
fn emit_map_kv_call(
    func: &mut Function,
    method: &str,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let arity = match method {
        "set" => 3,
        "get" => 2,
        "len" => 1,
        _ => unreachable!("emit_map_kv_call: unknown method `{method}`"),
    };
    if args.len() != arity {
        return Err(WasmGcError::Validation(format!(
            "Map.{method} expects {arity} args, got {}",
            args.len()
        )));
    }
    let map_aver = infer_aver_type(&args[0].node, ctx)?;
    let canonical: String = map_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let helpers = ctx
        .fn_map
        .map_helpers
        .get(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.{method}: map argument has type `{map_aver}` but no helpers are registered"
        )))?;
    let target_idx = match method {
        "set" => helpers.set,
        "get" => helpers.get,
        "len" => helpers.len,
        _ => unreachable!(),
    };
    for arg in args {
        emit_expr(func, &arg.node, slots, ctx)?;
    }
    func.instruction(&Instruction::Call(target_idx));
    Ok(())
}

/// `Vector.new(size, fill)` → `array.new $vector_T`. Element type comes
/// from the fill argument's Aver type; the registry must already have
/// the matching `Vector<T>` slot (`TypeRegistry::build` walks fn
/// signatures so any reachable instantiation registers).
fn emit_vector_new(
    func: &mut Function,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Vector.new expects 2 args, got {}",
            args.len()
        )));
    }
    let elem_aver = infer_aver_type(&args[1].node, ctx)?;
    let canonical = format!("Vector<{}>", elem_aver);
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.new: instantiation `{canonical}` was not registered \
             (TypeRegistry expected to discover it from a signature)"
        )))?;
    // wasm `array.new $T` pops [value, size:i32]. Aver pushes size
    // (i64) then fill — we re-order to match wasm's stack discipline.
    emit_expr(func, &args[1].node, slots, ctx)?; // fill
    emit_expr(func, &args[0].node, slots, ctx)?; // size i64
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::ArrayNew(vec_idx));
    Ok(())
}

/// `Option.withDefault(opt, default)` — recognise fused shapes that
/// avoid ever boxing an Option<T>:
///
/// - `Option.withDefault(Vector.set(v, i, x), v)` where the default IS
///   the same vector. Lowers to a bounds-checked `array.set` (in-place
///   mutation, semantically a fresh array) returning `v`. No Option
///   ever exists at runtime.
/// - `Option.withDefault(Vector.get(v, i), default_literal)` — bounds
///   check + `array.get`, falling through to the literal on out-of-range.
///
/// Anything else is a real Option<T> that survives past optimisation,
/// which a later phase will represent with a struct or a nullable ref;
/// today it surfaces as Unimplemented.
fn emit_option_with_default(
    func: &mut Function,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if args.len() != 2 {
        return Err(WasmGcError::Validation(format!(
            "Option.withDefault expects 2 args, got {}",
            args.len()
        )));
    }
    let opt_arg = &args[0];
    let default_arg = &args[1];

    // Try the shared IR-level leaf classifier first — same code path
    // the Rust / Lean backends use, so adding a new fused shape (e.g.
    // `IntModOrDefaultLiteral`) lights up across every backend
    // automatically. The classifier is re-run on the parent call
    // because `Option.withDefault` is the shape's outer shell.
    let outer_call = Expr::FnCall(
        Box::new(Spanned {
            node: Expr::Attr(
                Box::new(Spanned {
                    node: Expr::Ident("Option".into()),
                    line: 0,
                }),
                "withDefault".into(),
            ),
            line: 0,
        }),
        args.to_vec(),
    );
    if let Some(leaf) = classify_leaf_op(&outer_call, ctx) {
        match leaf {
            LeafOp::VectorSetOrDefaultSameVector {
                vector,
                index,
                value,
            } => {
                return emit_vector_set_or_default(func, vector, index, value, slots, ctx);
            }
            LeafOp::VectorGetOrDefaultLiteral {
                vector,
                index,
                default_literal,
            } => {
                let default_spanned = Spanned {
                    node: Expr::Literal(default_literal.clone()),
                    line: 0,
                };
                return emit_vector_get_or_default(
                    func,
                    vector,
                    index,
                    &default_spanned,
                    slots,
                    ctx,
                );
            }
            _ => {}
        }
    }

    // `Option.withDefault(Map.get(m, k), default)` — Map fusion isn't
    // in `LeafOp` (legacy backends use runtime helpers and don't need
    // a per-shape leaf), so handle it locally.
    if let Expr::FnCall(inner_callee, inner_args) = &opt_arg.node
        && let Expr::Attr(parent, member) = &inner_callee.node
        && let Expr::Ident(p) = &parent.node
        && p == "Map"
        && member == "get"
        && inner_args.len() == 2
    {
        return emit_map_get_or_default(
            func,
            &inner_args[0],
            &inner_args[1],
            default_arg,
            slots,
            ctx,
        );
    }

    // Real Option<T> boxing fallback — `Option.withDefault` over an
    // arbitrary Option-producing call. The value materialises as a
    // concrete struct; dispatch through tag-based pattern match.
    emit_option_with_default_boxed(func, opt_arg, default_arg, slots, ctx)
}

/// Generic `Option.withDefault(opt, default)` — emits `opt`, reads
/// its tag, returns either the value field or the default. Used when
/// no fused shape applies. Allocates the Option if `opt` is itself a
/// shape that allocates (e.g. `Map.get`); the surrounding caller is
/// expected to use a fused emitter when the alloc is avoidable.
fn emit_option_with_default_boxed(
    func: &mut Function,
    opt_arg: &Spanned<Expr>,
    default_arg: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let opt_aver = infer_aver_type(&opt_arg.node, ctx)?;
    let canonical: String = opt_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let opt_idx = ctx
        .registry
        .option_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Option.withDefault: opt arg of type `{opt_aver}` is not a registered Option<T>"
        )))?;
    let element = super::types::TypeRegistry::option_element_type(&canonical).ok_or(
        WasmGcError::Validation(format!(
            "Option.withDefault: cannot parse element type from `{canonical}`"
        )),
    )?;
    let elem_val = aver_to_wasm(element, Some(ctx.registry))?.ok_or(
        WasmGcError::Validation(format!(
            "Option.withDefault: element type `{element}` has no wasm representation"
        )),
    )?;
    let block_ty = wasm_encoder::BlockType::Result(elem_val);

    // Stash opt in scratch, peek at tag.
    let scratch = slots.subject_scratch.ok_or(WasmGcError::Validation(
        "Option.withDefault (boxed) needs a scratch slot but none was reserved".into(),
    ))?;
    emit_expr(func, &opt_arg.node, slots, ctx)?;
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
    emit_expr(func, &default_arg.node, slots, ctx)?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// Fused `Option.withDefault(Map.get(m, k), default)` → call to the
/// per-instantiation `get_or_default` helper. No `Option<V>` ever
/// allocates on the hot lookup path.
fn emit_map_get_or_default(
    func: &mut Function,
    map: &Spanned<Expr>,
    key: &Spanned<Expr>,
    default: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let map_aver = infer_aver_type(&map.node, ctx)?;
    let canonical: String = map_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let helpers = ctx
        .fn_map
        .map_helpers
        .get(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.get fusion: map argument has type `{map_aver}` but no helpers are registered"
        )))?;
    emit_expr(func, &map.node, slots, ctx)?;
    emit_expr(func, &key.node, slots, ctx)?;
    emit_expr(func, &default.node, slots, ctx)?;
    func.instruction(&Instruction::Call(helpers.get_or_default));
    Ok(())
}

/// Fused `Option.withDefault(Vector.set(v, i, x), v)`: bounds-checked
/// `array.set` in place, return `v` regardless. Because the default IS
/// the vector, both arms of the conceptual Option produce the same
/// reference — no need to materialise None.
fn emit_vector_set_or_default(
    func: &mut Function,
    vector: &Spanned<Expr>,
    index: &Spanned<Expr>,
    value: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let vec_aver = infer_aver_type(&vector.node, ctx)?;
    let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.set: vector arg of type `{vec_aver}` is not a registered Vector<T>"
        )))?;

    // 0 <= index < array.len, all i32. Aver Int is i64 → wrap once
    // and reuse via re-emit (cheap when Resolved).
    emit_expr(func, &index.node, slots, ctx)?;
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64GeS);

    emit_expr(func, &index.node, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    emit_expr(func, &vector.node, slots, ctx)?;
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::I32LtU);

    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_expr(func, &vector.node, slots, ctx)?;
    emit_expr(func, &index.node, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    emit_expr(func, &value.node, slots, ctx)?;
    func.instruction(&Instruction::ArraySet(vec_idx));
    func.instruction(&Instruction::End);
    emit_expr(func, &vector.node, slots, ctx)?;
    Ok(())
}

/// `Expr::InterpolatedStr(parts)` — wasm-gc lowers interpolations to
/// `array.new_fixed (array (ref null $string)) N` + a single call to
/// the variadic concat helper. Each part is coerced to `String`:
/// - `String` → identity
/// - `Int` → `call $Int.toString`
/// - other primitives surface as Unimplemented until their helpers land
///
/// `interp_lower` is skipped for this backend (`run_interp_lower=false`
/// in the wasm-gc pipeline config) because the `__buf_*` shape it
/// produces targets bump-allocator backends (linear memory + grow-on-
/// append). The variadic shape is O(total_len) bytes copied — same
/// asymptotics a real mutable buffer would achieve, without the
/// `(struct len array)` wrapper or the per-append realloc cost of a
/// left-folded concat chain. Same primitive will back `String.join`
/// once it lands (interleave separators, then call this helper).
fn emit_interpolated_str(
    func: &mut Function,
    parts: &[crate::ast::StrPart],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    use crate::ast::StrPart;
    let string_type_idx = ctx
        .registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "InterpolatedStr reachable but no String type slot allocated".into(),
        ))?;
    if parts.is_empty() {
        // Empty interpolation → empty String. Allocate a zero-length
        // array directly; cheaper than going through the helper.
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::ArrayNewDefault(string_type_idx));
        return Ok(());
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
            StrPart::Literal(s) => {
                let bytes = s.as_bytes();
                let seg_idx = ctx.registry.string_literal_segment(bytes).ok_or(
                    WasmGcError::Validation(format!(
                        "Interpolation literal `{s:?}` not in segment table"
                    )),
                )?;
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::I32Const(bytes.len() as i32));
                func.instruction(&Instruction::ArrayNewData {
                    array_type_index: string_type_idx,
                    array_data_index: seg_idx,
                });
            }
            StrPart::Parsed(inner) => {
                let aver_ty = infer_aver_type(&inner.node, ctx)?;
                emit_expr(func, &inner.node, slots, ctx)?;
                match aver_ty.trim() {
                    "String" => { /* identity */ }
                    "Int" => {
                        let to_string_idx = ctx.fn_map.builtins.get("Int.toString").copied().ok_or(
                            WasmGcError::Validation(
                                "interpolation of Int requires Int.toString builtin".into(),
                            ),
                        )?;
                        func.instruction(&Instruction::Call(to_string_idx));
                    }
                    other => {
                        return Err(WasmGcError::Unimplemented(match other {
                            "Float" => "phase 3c — interpolation of Float (needs Float.toString)",
                            "Bool" => "phase 3c — interpolation of Bool (needs Bool.toString)",
                            _ => "phase 3c — interpolation of compound type",
                        }));
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
    Ok(())
}

/// Fused `Option.withDefault(Vector.get(v, i), default)`: bounds-checked
/// `array.get`, falls back to the default on out-of-range. The result
/// type is the vector's element type (Aver guarantees `default` agrees).
fn emit_vector_get_or_default(
    func: &mut Function,
    vector: &Spanned<Expr>,
    index: &Spanned<Expr>,
    default: &Spanned<Expr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let vec_aver = infer_aver_type(&vector.node, ctx)?;
    let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
    let vec_idx = ctx
        .registry
        .vector_type_idx(&canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Vector.get: vector arg of type `{vec_aver}` is not a registered Vector<T>"
        )))?;

    // Result wasm type = element type. Block must declare it so both
    // arms unify on the stack shape.
    let element = super::types::TypeRegistry::vector_element_type(&canonical).ok_or(
        WasmGcError::Validation(format!(
            "Vector.get: cannot parse element type from `{canonical}`"
        )),
    )?;
    let elem_val = aver_to_wasm(element, Some(ctx.registry))?.ok_or(WasmGcError::Validation(
        format!("Vector.get: element type `{element}` has no wasm representation"),
    ))?;
    let block_ty = wasm_encoder::BlockType::Result(elem_val);

    emit_expr(func, &index.node, slots, ctx)?;
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64GeS);

    emit_expr(func, &index.node, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    emit_expr(func, &vector.node, slots, ctx)?;
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::I32LtU);

    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::If(block_ty));
    emit_expr(func, &vector.node, slots, ctx)?;
    emit_expr(func, &index.node, slots, ctx)?;
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::ArrayGet(vec_idx));
    func.instruction(&Instruction::Else);
    emit_expr(func, &default.node, slots, ctx)?;
    func.instruction(&Instruction::End);
    Ok(())
}

fn emit_constructor_with_args(
    func: &mut Function,
    info: &super::types::VariantInfo,
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
        return emit_expr(func, &args[0].node, slots, ctx);
    }
    for arg in args {
        emit_expr(func, &arg.node, slots, ctx)?;
    }
    func.instruction(&Instruction::StructNew(info.type_idx));
    Ok(())
}

/// Lower `Constructor(name, Some(payload))` or nullary `Constructor(name, None)`
/// to `struct.new $variant_type_idx`. Variants are positional (no field
/// names), so payload values are pushed in source order before
/// `struct.new`.
fn emit_constructor(
    func: &mut Function,
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
        return emit_option_constructor(func, None, Some(ctx.return_type), slots, ctx);
    }
    let info = ctx
        .registry
        .variant(name)
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
    emit_expr(func, &payload.node, slots, ctx)?;
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
fn emit_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arms: &[MatchArm],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if arms.is_empty() {
        return Err(WasmGcError::Validation("match has no arms".into()));
    }
    let result_ty_str = infer_aver_type(&arms[0].body.node, ctx)?;
    let result_wasm = aver_to_wasm(&result_ty_str, Some(ctx.registry))?;
    let block_ty = match result_wasm {
        Some(v) => wasm_encoder::BlockType::Result(v),
        None => wasm_encoder::BlockType::Empty,
    };

    // Bool subject — special-case to a single `if`/`else`. No subject
    // local needed (wasm `if` consumes the i32 directly).
    let subject_ty = infer_aver_type(&subject.node, ctx)?;
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
        emit_expr(func, &subject.node, slots, ctx)?;
        func.instruction(&Instruction::If(block_ty));
        emit_expr(func, &t.node, slots, ctx)?;
        func.instruction(&Instruction::Else);
        emit_expr(func, &f.node, slots, ctx)?;
        func.instruction(&Instruction::End);
        return Ok(());
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

    // Single-arm Constructor pattern — `match obj { Foo.Bar(n) -> body }`.
    // Common in newtype-style sum types; cast + extract directly without
    // a dispatch test.
    if arms.len() == 1
        && let Pattern::Constructor(name, bindings) = &arms[0].pattern
    {
        return emit_single_variant_match(func, subject, name, bindings, &arms[0].body, slots, ctx);
    }

    // Multi-arm Constructor patterns — emit a `ref.test` dispatch
    // cascade against the variant struct types.
    let has_constructor_arm = arms
        .iter()
        .any(|a| matches!(a.pattern, Pattern::Constructor(_, _)));
    if has_constructor_arm {
        return emit_variant_dispatch(func, subject, arms, block_ty, slots, ctx);
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

fn emit_int_match_cascade(
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
        emit_expr(func, &wildcard.node, slots, ctx)?;
        return Ok(());
    }
    let (pat_lit, body) = typed_arms[0];
    emit_expr(func, &subject.node, slots, ctx)?;
    func.instruction(&Instruction::I64Const(pat_lit));
    func.instruction(&Instruction::I64Eq);
    func.instruction(&Instruction::If(block_ty));
    emit_expr(func, &body.node, slots, ctx)?;
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
fn emit_tail_call(
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
        emit_expr(func, &arg.node, slots, ctx)?;
    }
    if target == ctx.self_fn_name {
        func.instruction(&Instruction::ReturnCall(ctx.self_wasm_idx));
    } else {
        // Direct (non-self) tail call to a known fn — wasm-gc still
        // supports `return_call` here. Mutual-TCO via a function
        // table is a phase 4b refinement once we have an SCC bench
        // that needs it.
        func.instruction(&Instruction::ReturnCall(entry.wasm_idx));
    }
    Ok(())
}
