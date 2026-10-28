//! Record fields crossing a twin's calls without a temporary record object.
//!
//! Eligibility is the shared MIR parameter-use summary. This module only
//! implements the wasm convention: fields replace the source record param.
//! Fresh producers leave fields on
//! the stack; an already packed argument is read once into field locals.
//! Field evaluation keeps written order, including effects and early exits.

use std::cell::RefCell;
use std::collections::HashMap;

use wasm_encoder::Instruction as I;

use super::*;
use crate::codegen::wasm_gc::twins::{RecordFields, TwinEntry};

#[derive(Clone)]
struct Values {
    layout: RecordFields,
    locals: Vec<u32>,
}

#[derive(Default)]
struct Current {
    params: Vec<(u32, RecordFields)>,
    values: HashMap<u32, Values>,
}

thread_local! {
    static CURRENT: RefCell<Current> = RefCell::new(Current::default());
}

pub(super) fn set_self(twin: Option<(&TwinEntry, &MirFn)>) {
    CURRENT.with(|c| {
        *c.borrow_mut() = Current {
            params: twin.map_or_else(Vec::new, |(entry, f)| {
                f.params
                    .iter()
                    .zip(&entry.records)
                    .filter_map(|(p, r)| Some((p.local.0, r.clone()?)))
                    .collect()
            }),
            values: HashMap::new(),
        };
    });
}

pub(super) fn emit_entry(slots: &SlotTable) {
    CURRENT.with(|c| {
        let mut c = c.borrow_mut();
        c.values = c
            .params
            .iter()
            .map(|(slot, layout)| {
                (
                    *slot,
                    Values {
                        layout: layout.clone(),
                        locals: layout
                            .fields
                            .iter()
                            .map(|(_, _, v)| slots.fresh_local(*v))
                            .collect(),
                    },
                )
            })
            .collect();
    });
}

/// Field locals in parameter/field declaration order. The module emitter
/// copies the extra incoming params into these after remapping local indices.
pub(crate) fn entry_locals() -> Vec<u32> {
    CURRENT.with(|c| {
        let c = c.borrow();
        c.params
            .iter()
            .flat_map(|(slot, _)| c.values[slot].locals.iter().copied())
            .collect()
    })
}

fn local_values(expr: &MirExpr) -> Option<Values> {
    let MirExpr::Local(l) = expr else { return None };
    CURRENT.with(|c| c.borrow().values.get(&l.node.slot.0).cloned())
}

pub(super) fn unpackable(
    expr: &Spanned<MirExpr>,
    layout: &RecordFields,
    ctx: &EmitCtx<'_>,
) -> bool {
    if expr.lazy.is_deferred() {
        return false;
    }
    local_values(&expr.node).is_some_and(|v| v.layout.struct_idx == layout.struct_idx)
        || matches!(expr.node, MirExpr::RecordCreate(_))
        || twin::callee(expr, ctx).is_some_and(|(_, t)| {
            matches!(t.kind,
            TwinKind::Fields { struct_idx, .. } if struct_idx == layout.struct_idx)
        })
}

/// A field projection from an unpacked parameter or producer. `None` leaves
/// the ordinary record emitter in charge.
pub(super) fn project(
    func: &mut Function,
    p: &crate::ir::mir::MirProject,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<bool>>, WasmGcError> {
    if !twin::calls_allowed() {
        return Ok(None);
    }
    let Some(layout) = RecordFields::of(&aver_type_str_of(&p.base), ctx.registry) else {
        return Ok(None);
    };
    if !unpackable(&p.base, &layout, ctx) {
        return Ok(None);
    }
    let Some(field) = layout.fields.iter().position(|(f, _, _)| f == &p.field) else {
        return Ok(None);
    };
    let Some(values) = emit_arg(func, &p.base, &layout, slots, ctx)? else {
        return Ok(Some(None));
    };
    func.instruction(&I::LocalGet(values[field]));
    Ok(Some(Some(true)))
}

/// Reconstitute a parameter when an ordinary callee still needs the whole
/// record. This does not lose or invent a value at a mixed calling boundary.
pub(super) fn packed_local(func: &mut Function, expr: &MirExpr) -> bool {
    let Some(values) = local_values(expr) else {
        return false;
    };
    for slot in values.locals {
        func.instruction(&I::LocalGet(slot));
    }
    func.instruction(&I::StructNew(values.layout.struct_idx));
    true
}

/// Evaluate one argument exactly once and keep its fields in locals. A
/// caller appends these after the original arguments and the Int ref args.
pub(super) fn emit_arg(
    func: &mut Function,
    arg: &Spanned<MirExpr>,
    layout: &RecordFields,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Vec<u32>>, WasmGcError> {
    if let Some(values) = local_values(&arg.node)
        && values.layout.struct_idx == layout.struct_idx
    {
        return Ok(Some(values.locals));
    }
    if !arg.lazy.is_deferred()
        && let MirExpr::RecordCreate(rec) = &arg.node
        && ctx.registry.record_type_idx(&rec.node.type_name) == Some(layout.struct_idx)
    {
        let mut locals = vec![None; layout.fields.len()];
        for written in &rec.node.fields {
            let Some(j) = layout
                .fields
                .iter()
                .position(|(name, _, _)| name == &written.name)
            else {
                continue;
            };
            if locals[j].is_some() {
                continue;
            }
            let (_, ty, val) = &layout.fields[j];
            if emit_mir_record_field_value(func, &written.value, ty, slots, ctx)?.is_none() {
                return Ok(None);
            }
            let slot = slots.fresh_local(*val);
            func.instruction(&I::LocalSet(slot));
            locals[j] = Some(slot);
        }
        return locals
            .into_iter()
            .collect::<Option<Vec<_>>>()
            .map(Some)
            .ok_or_else(|| WasmGcError::Validation("unpacked record is missing a field".into()));
    }
    let locals: Vec<u32> = layout
        .fields
        .iter()
        .map(|(_, _, v)| slots.fresh_local(*v))
        .collect();
    if !arg.lazy.is_deferred()
        && let Some((call, t)) = twin::callee(arg, ctx)
        && matches!(t.kind, TwinKind::Fields { struct_idx, .. } if struct_idx == layout.struct_idx)
    {
        if twin::emit_call(func, call, &t, slots, ctx)?.is_none() {
            return Ok(None);
        }
        for slot in locals.iter().rev() {
            func.instruction(&I::LocalSet(*slot));
        }
    } else {
        if emit_mir_expr(func, arg, slots, ctx)?.is_none() {
            return Ok(None);
        }
        let packed = slots.fresh_local(super::super::super::types::struct_ref(layout.struct_idx));
        func.instruction(&I::LocalSet(packed));
        for (j, slot) in locals.iter().enumerate() {
            func.instruction(&I::LocalGet(packed));
            func.instruction(&I::StructGet {
                struct_type_index: layout.struct_idx,
                field_index: j as u32,
            });
            func.instruction(&I::LocalSet(*slot));
        }
    }
    Ok(Some(locals))
}
