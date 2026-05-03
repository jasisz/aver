//! Per-fn slot table + slot-allocation pre-pass over the resolver's
//! local layout. Step 3 deleted the parallel `infer_expr_wasm_type` /
//! `lookup_var_type` / `collect_binding_types` ad-hoc inference layer
//! — slot wasm types now come straight from `Spanned::ty()` (set by
//! the type checker in Step 0). Schema-only AST predicates
//! (`expr_needs_scratch` etc.) survive because they classify pattern
//! shape, not type.

use wasm_encoder::ValType;

use crate::ast::{Expr, FnBody, FnDef, Literal, Pattern, Spanned, Stmt};

use super::super::WasmGcError;
use super::super::types::{TypeRegistry, aver_to_wasm};
use super::FnMap;
use super::infer::{arm_is_option_pattern, arm_is_result_pattern, aver_type_str_of, wasm_type_of};

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
    /// Walks the body, reads each binding's wasm type from the typed
    /// AST (`Spanned::ty()`), builds a dense `Vec<ValType>` indexed by
    /// slot number.
    pub(super) fn build_for_fn(
        fd: &FnDef,
        registry: &TypeRegistry,
        _fn_map: &FnMap,
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
            collect_binding_slots(stmt, &mut by_slot, registry)?;
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
            // Reserve a scratch any time the arms include a
            // Constructor pattern. Earlier we tried to be clever —
            // skip when all variants reduce to newtypes, since the
            // newtype unwrap doesn't need the scratch — but multi-
            // module flatten can land variants whose registry entry
            // isn't visible at slot-allocation time (rogue's
            // `EntityKind.WildIfElse` resolves through `types.av`
            // flattened from a different module). The cost of an
            // unused scratch local is one wasm value; the cost of a
            // missing one is `emit_variant_dispatch` crashing with
            // "no scratch reserved".
            if arms
                .iter()
                .any(|a| matches!(a.pattern, Pattern::Constructor(_, _)))
            {
                return true;
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
) -> Result<(), WasmGcError> {
    match stmt {
        Stmt::Binding(_name, annot, expr) => {
            // Annotation wins (matches resolver behavior); otherwise
            // pull the type straight from the typed AST.
            let ty = if let Some(t) = annot.as_deref() {
                aver_to_wasm(t, Some(registry))?
            } else {
                wasm_type_of(expr, registry)?
            };
            if let Some(v) = ty {
                out.push(v);
            }
            collect_expr_binding_slots(expr, out, registry)?;
        }
        Stmt::Expr(spanned) => collect_expr_binding_slots(spanned, out, registry)?,
    }
    Ok(())
}

pub(super) fn collect_expr_binding_slots(
    expr: &Spanned<Expr>,
    out: &mut Vec<ValType>,
    registry: &TypeRegistry,
) -> Result<(), WasmGcError> {
    match &expr.node {
        Expr::Match { subject, arms } => {
            collect_expr_binding_slots(subject, out, registry)?;
            // Built-in Option arms — `Option.Some(v)` binds v to T
            // (read off the subject's stamped Option<T> type).
            let is_option = arms.iter().any(arm_is_option_pattern);
            if is_option {
                let subj_ty = aver_type_str_of(subject);
                let canonical: String = subj_ty.chars().filter(|c| !c.is_whitespace()).collect();
                let inner = TypeRegistry::option_element_type(&canonical);
                for arm in arms {
                    if let Pattern::Constructor(_, bindings) = &arm.pattern
                        && arm_is_option_pattern(arm)
                    {
                        for binding_name in bindings {
                            if binding_name == "_" {
                                continue;
                            }
                            let inner_ty = inner.ok_or(WasmGcError::Validation(
                                "Option.Some binding without resolvable inner type — \
                                 subject's Aver type must reduce to Option<T>"
                                    .into(),
                            ))?;
                            if let Some(v) = aver_to_wasm(inner_ty, Some(registry))? {
                                out.push(v);
                            }
                        }
                    }
                    collect_expr_binding_slots(&arm.body, out, registry)?;
                }
                return Ok(());
            }
            // Built-in Result arms — Ok binds T (field 1), Err binds
            // E (field 2). Recover canonical from the subject's
            // stamped Result<T,E> type.
            let is_result = arms.iter().any(arm_is_result_pattern);
            if is_result {
                let subj_ty = aver_type_str_of(subject);
                let canonical: String = subj_ty.chars().filter(|c| !c.is_whitespace()).collect();
                let (t_aver, e_aver) = TypeRegistry::result_te(&canonical).ok_or_else(|| {
                    WasmGcError::Validation(format!(
                        "Result match subject type `{subj_ty}` does not reduce to Result<T,E>"
                    ))
                })?;
                for arm in arms {
                    if let Pattern::Constructor(name, bindings) = &arm.pattern
                        && arm_is_result_pattern(arm)
                    {
                        let bare = name.rsplit('.').next().unwrap_or(name);
                        let inner_ty = if bare == "Ok" { t_aver } else { e_aver };
                        for binding_name in bindings {
                            if binding_name == "_" {
                                continue;
                            }
                            if let Some(v) = aver_to_wasm(inner_ty, Some(registry))? {
                                out.push(v);
                            }
                        }
                    }
                    collect_expr_binding_slots(&arm.body, out, registry)?;
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
                    let subject_ty_str = aver_type_str_of(subject);
                    let canonical: String = subject_ty_str
                        .chars()
                        .filter(|c| !c.is_whitespace())
                        .collect();
                    if let Some((a, b)) = TypeRegistry::tuple_ab(&canonical) {
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
                if let Pattern::Cons(head_name, tail_name) = &arm.pattern {
                    // Cons pattern bindings — head: T, tail: List<T>.
                    // Both come from the subject's stamped `List<T>`.
                    let subject_ty_str = aver_type_str_of(subject);
                    let canonical: String = subject_ty_str
                        .chars()
                        .filter(|c| !c.is_whitespace())
                        .collect();
                    if let Some(elem) = TypeRegistry::list_element_type(&canonical) {
                        let elem_ty = aver_to_wasm(elem, Some(registry))?;
                        let tail_ty = aver_to_wasm(&canonical, Some(registry))?;
                        if let Some(et) = elem_ty {
                            if head_name != "_" {
                                out.push(et);
                            }
                            if tail_name != "_"
                                && let Some(tt) = tail_ty
                            {
                                out.push(tt);
                            }
                        }
                    }
                }
                collect_expr_binding_slots(&arm.body, out, registry)?;
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_expr_binding_slots(l, out, registry)?;
            collect_expr_binding_slots(r, out, registry)?;
        }
        Expr::FnCall(callee, args) => {
            collect_expr_binding_slots(callee, out, registry)?;
            for arg in args {
                collect_expr_binding_slots(arg, out, registry)?;
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                collect_expr_binding_slots(arg, out, registry)?;
            }
        }
        Expr::Attr(obj, _) => collect_expr_binding_slots(obj, out, registry)?,
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                collect_expr_binding_slots(p, out, registry)?;
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_expr_binding_slots(e, out, registry)?;
            }
        }
        _ => {}
    }
    Ok(())
}

pub(super) fn count_value_params(params: &[(String, String)]) -> usize {
    params.iter().filter(|(_, ty)| ty.trim() != "Unit").count()
}
