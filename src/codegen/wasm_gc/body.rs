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
// Re-export the trait so the IntoStatic impls below stay private.
#[allow(unused_imports)]
use IntoStatic as _;

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
    fn build_for_fn(fd: &FnDef, registry: &TypeRegistry) -> Result<Self, WasmGcError> {
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
            collect_binding_slots(stmt, &mut by_slot, registry, fd)?;
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

fn collect_binding_slots(
    stmt: &Stmt,
    out: &mut Vec<ValType>,
    registry: &TypeRegistry,
    fd: &FnDef,
) -> Result<(), WasmGcError> {
    match stmt {
        Stmt::Binding(_, annot, expr) => {
            let ty = if let Some(t) = annot.as_deref() {
                aver_to_wasm(t, Some(registry))?
            } else {
                infer_expr_wasm_type(&expr.node, registry, fd)?
            };
            if let Some(v) = ty {
                out.push(v);
            }
            collect_expr_binding_slots(&expr.node, out, registry, fd)?;
        }
        Stmt::Expr(spanned) => collect_expr_binding_slots(&spanned.node, out, registry, fd)?,
    }
    Ok(())
}

fn collect_expr_binding_slots(
    expr: &Expr,
    out: &mut Vec<ValType>,
    registry: &TypeRegistry,
    fd: &FnDef,
) -> Result<(), WasmGcError> {
    match expr {
        Expr::Match { subject, arms } => {
            collect_expr_binding_slots(&subject.node, out, registry, fd)?;
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
                collect_expr_binding_slots(&arm.body.node, out, registry, fd)?;
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_expr_binding_slots(&l.node, out, registry, fd)?;
            collect_expr_binding_slots(&r.node, out, registry, fd)?;
        }
        Expr::FnCall(callee, args) => {
            collect_expr_binding_slots(&callee.node, out, registry, fd)?;
            for arg in args {
                collect_expr_binding_slots(&arg.node, out, registry, fd)?;
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                collect_expr_binding_slots(&arg.node, out, registry, fd)?;
            }
        }
        Expr::Attr(obj, _) => collect_expr_binding_slots(&obj.node, out, registry, fd)?,
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                collect_expr_binding_slots(&p.node, out, registry, fd)?;
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_expr_binding_slots(&e.node, out, registry, fd)?;
            }
        }
        _ => {}
    }
    Ok(())
}

/// Best-effort wasm type inference for slot pre-allocation. Mirrors
/// `infer_aver_type` but doesn't need an `EmitCtx` — runs before bodies
/// are emitted.
fn infer_expr_wasm_type(
    expr: &Expr,
    registry: &TypeRegistry,
    _fd: &FnDef,
) -> Result<Option<ValType>, WasmGcError> {
    match expr {
        Expr::Literal(Literal::Int(_)) => Ok(Some(ValType::I64)),
        Expr::Literal(Literal::Float(_)) => Ok(Some(ValType::F64)),
        Expr::Literal(Literal::Bool(_)) => Ok(Some(ValType::I32)),
        Expr::Literal(Literal::Unit) => Ok(None),
        Expr::BinOp(op, _, _) => match op {
            BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                Ok(Some(ValType::I32))
            }
            _ => Ok(Some(ValType::I64)),
        },
        Expr::RecordCreate { type_name, .. } => aver_to_wasm(type_name, Some(registry)),
        Expr::Constructor(name, _) => {
            if let Some(info) = registry.variant(name) {
                aver_to_wasm(&info.parent, Some(registry))
            } else {
                Ok(Some(ValType::I64))
            }
        }
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
    let slots = SlotTable::build_for_fn(fd, registry)?;
    let FnBody::Block(stmts) = fd.body.as_ref();
    let last_idx = stmts.len().saturating_sub(1);

    let ctx = EmitCtx {
        fn_map,
        self_wasm_idx,
        self_fn_name: fd.name.as_str(),
        return_type: fd.return_type.as_str(),
        registry,
        resolution: fd.resolution.as_ref(),
        params: &fd.params,
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
                let produces_value = aver_to_wasm(aver_ty, Some(ctx.registry))?.is_some();
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
fn infer_aver_type(expr: &Expr, ctx: &EmitCtx<'_>) -> Result<&'static str, WasmGcError> {
    match expr {
        Expr::Literal(Literal::Int(_)) => Ok("Int"),
        Expr::Literal(Literal::Float(_)) => Ok("Float"),
        Expr::Literal(Literal::Bool(_)) => Ok("Bool"),
        Expr::Literal(Literal::Unit) => Ok("Unit"),
        Expr::Resolved { name, .. } => {
            // Look up the param/binding type. Falls back to "Int" only
            // if we can't recover the original aver type — most
            // bench scenarios bind only by name and we can find the
            // type via `lookup_var_type`.
            if let Some(ty) = lookup_var_type(name, ctx) {
                Ok(static_type_str(&ty))
            } else {
                Ok("Int")
            }
        }
        Expr::Ident(_) => Ok("Int"),
        Expr::BinOp(op, l, _) => {
            // Comparisons always yield Bool; arithmetic preserves
            // operand type (Float + Float = Float).
            match op {
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                    Ok("Bool")
                }
                _ => infer_aver_type(&l.node, ctx),
            }
        }
        Expr::FnCall(callee, _) => {
            // Dotted callee: try variant constructor, then registered
            // builtin, then dotted name. Variants and builtins
            // determined by the parent (Type) name.
            if let Expr::Attr(parent, member) = &callee.node {
                if let Some(info) = ctx.registry.variant(member) {
                    return Ok(static_type_str(&info.parent));
                }
                if let Some(parent_name) = parent_dotted_head(&parent.node) {
                    let dotted = format!("{parent_name}.{member}");
                    if ctx.fn_map.builtins.contains_key(&dotted) {
                        return Ok(builtin_aver_result_type(&dotted));
                    }
                    if ctx.fn_map.effects.contains_key(&dotted) {
                        return Ok("Unit");
                    }
                    if dotted == "Float.fromInt" {
                        return Ok("Float");
                    }
                    if dotted == "Int.fromFloat" {
                        return Ok("Int");
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
            Ok(static_type_str(&entry.return_type))
        }
        Expr::Match { arms, .. } => {
            // Match result type = arm body type; arms are required by
            // the type checker to agree, so any arm tells us. Phase
            // 4 only accepts non-empty matches.
            arms.first()
                .map(|a| infer_aver_type(&a.body.node, ctx))
                .unwrap_or(Err(WasmGcError::Validation("match has no arms".into())))?
                .into_static()
        }
        // Tail calls are statements at the wasm level (no value pushed
        // back to the caller's frame); for inference purposes we report
        // the enclosing fn's return type.
        Expr::TailCall(_) => Ok(static_type_str(ctx.return_type)),
        Expr::RecordCreate { type_name, .. } => Ok(static_type_str(type_name)),
        Expr::Attr(obj, field) => {
            // Phase 3a: best-effort — if we can identify the record
            // type of `obj`, look up the field's declared type.
            // Otherwise fall back to "Int" (most bench scenarios with
            // Attr access do unwrap a numeric field).
            if let Ok(Some(record_name)) = struct_name_of_unboxed(&obj.node, ctx) {
                if let Some(ty) = ctx.registry.record_field_type(&record_name, field) {
                    return Ok(static_type_str(ty));
                }
            }
            Ok("Int")
        }
        Expr::Constructor(name, _) => {
            if let Some(info) = ctx.registry.variant(name) {
                Ok(static_type_str(&info.parent))
            } else {
                Ok("Int")
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

/// Look up an Aver type-name string for a local variable. Today this
/// walks `FnDef.params`; a phase 3b-cleanup binding-type map would
/// extend it to `let`-bindings too.
fn lookup_var_type(name: &str, ctx: &EmitCtx<'_>) -> Option<String> {
    ctx.params
        .iter()
        .find(|(n, _)| n == name)
        .map(|(_, ty)| ty.clone())
}

trait IntoStatic {
    fn into_static(self) -> Result<&'static str, WasmGcError>;
}

impl IntoStatic for Result<&'static str, WasmGcError> {
    fn into_static(self) -> Result<&'static str, WasmGcError> {
        self
    }
}

impl IntoStatic for &'static str {
    fn into_static(self) -> Result<&'static str, WasmGcError> {
        Ok(self)
    }
}

fn binop_result(op: BinOp) -> &'static str {
    match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => "Int",
        BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => "Bool",
    }
}

fn static_type_str(ty: &str) -> &'static str {
    match ty.trim() {
        "Int" => "Int",
        "Float" => "Float",
        "Bool" => "Bool",
        "Unit" => "Unit",
        "String" => "String",
        _ => "Int", // phase-2 fallback — phase 3 introduces real type plumbing
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
        Expr::Literal(_) => {
            return Err(WasmGcError::Unimplemented(
                "phase 3 — String / Char literals",
            ));
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
        Expr::Attr(obj, field) => emit_attr_get(func, obj, field, slots, ctx)?,
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
        other => Err(WasmGcError::Unimplemented(match other {
            "Int.toString" => "phase 3c — Int.toString (needs String repr)",
            "Float.toString" => "phase 3c — Float.toString (needs String repr)",
            "String.length" => "phase 3c — String.length",
            "String.join" => "phase 3c — String.join",
            "List.prepend" => "phase 3c — List.prepend (needs List repr)",
            "List.reverse" => "phase 3c — List.reverse",
            "List.length" => "phase 3c — List.length",
            "Map.empty" => "phase 3c — Map.empty (needs Map repr)",
            "Map.set" => "phase 3c — Map.set",
            "Map.get" => "phase 3c — Map.get",
            "Vector.new" => "phase 3c — Vector.new (needs Vector repr)",
            "Vector.set" => "phase 3c — Vector.set",
            "Vector.get" => "phase 3c — Vector.get",
            "Console.print" => "phase 3c — Console.print (effect lowering)",
            _ => "phase 3c — unknown builtin or method call",
        })),
    }
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
    let result_wasm = aver_to_wasm(result_ty_str, Some(ctx.registry))?;
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
