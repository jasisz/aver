//! Taking a field out of a local record that a record literal or update
//! consumes, on the VM.
//!
//! What may be taken is planned in `ir::field_take`, from the reads this
//! module collects out of MIR. A projection the plan names compiles to
//! `RECORD_TAKE_NAMED` (one field) or `RECORD_TAKE_PATH` (a path of fields),
//! which carry the operand-stack cells the compiler knows hold each record on
//! the way down; the runtime takes the field only when exactly those hold it.

use std::collections::HashSet;

use crate::ir::field_take::ReadLog;
pub(super) use crate::ir::field_take::{FieldTakePlan, PathLevel};
use crate::ir::mir::{MirCallee, MirExpr};

/// `local.f1.….fn` as `(slot, last_use, [f1, …, fn])`; a bare local has no
/// fields.
pub(super) fn local_path(expr: &MirExpr) -> Option<(u32, bool, Vec<String>)> {
    match expr {
        MirExpr::Local(local) => Some((local.node.slot.0, local.node.last_use, Vec::new())),
        MirExpr::Project(p) => {
            let (slot, last_use, mut fields) = local_path(&p.node.base.node)?;
            fields.push(p.node.field.clone());
            Some((slot, last_use, fields))
        }
        _ => None,
    }
}

/// The path read of `expr` if it is a projection chain rooted at a local.
pub(super) fn projected_path(expr: &MirExpr) -> Option<(u32, bool, Vec<String>)> {
    local_path(expr).filter(|(_, _, fields)| !fields.is_empty())
}

fn collect(log: &mut ReadLog, expr: &MirExpr) {
    match expr {
        MirExpr::Project(_) | MirExpr::Local(_) if local_path(expr).is_some() => {
            let (slot, last_use, fields) = local_path(expr).expect("checked above");
            log.read(slot, last_use, fields);
        }
        MirExpr::RecordUpdate(update) if local_path(&update.node.base.node).is_some() => {
            let (slot, last_use, fields) =
                local_path(&update.node.base.node).expect("checked above");
            let written = update
                .node
                .updates
                .iter()
                .map(|field| field.name.clone())
                .collect();
            log.enter_base(slot, last_use, fields, written);
            for field in &update.node.updates {
                collect(log, &field.value.node);
            }
            log.leave_base();
        }
        MirExpr::Call(call) => {
            if let MirCallee::LocalSlot { slot, .. } = &call.node.callee {
                log.opaque(u32::from(*slot));
            }
            for arg in &call.node.args {
                collect(log, &arg.node);
            }
        }
        other => crate::ir::mir::expr::walk_children(other, &mut |child| collect(log, child)),
    }
}

/// The takes one record literal (`base == None`) or one update
/// (`base == Some(..)`, when the update's base is a local or a path of one)
/// allows. `written` is the set of fields the update writes; a literal writes
/// every field.
pub(super) fn plan_field_takes<'e>(
    base: Option<(u32, Vec<String>)>,
    written: &HashSet<&str>,
    values: impl IntoIterator<Item = &'e MirExpr>,
) -> Vec<FieldTakePlan> {
    let mut log = ReadLog::default();
    if let Some((slot, fields)) = base {
        let written = written.iter().map(|field| field.to_string()).collect();
        // The base was read before the values; whether that read was the
        // local's last use does not matter, since then the values read
        // nothing of it.
        log.enter_base(slot, false, fields, written);
    }
    for value in values {
        collect(&mut log, value);
    }
    log.plans()
}
