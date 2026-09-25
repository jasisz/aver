//! Taking a field out of a local record that a record literal or update
//! consumes.
//!
//! `T.update(state, counts = Map.set(state.counts, k, v), served = state.served + 1)`
//! reads `state` three times: as the base, for `counts` and for `served`. The
//! read of `counts` is not the last one, so the local still holds the record
//! when `Map.set` runs, the base sits on the operand stack below it, and the
//! record holds the Map. Every one of those is a holder, so the write copies
//! the whole Map, on every call.
//!
//! None of those holders can observe the field once it is gone. The base is
//! written over at exactly that field by the update, and the local is read
//! afterwards only for other fields of the record, and then never again. A
//! field projected under those conditions is compiled to `RECORD_TAKE_NAMED`
//! carrying the number of operand-stack cells the compiler knows hold the
//! record: the base cell of the update, and the local's own cell while a
//! later read still needs it. The runtime removes the field only when the
//! record is held by nothing off the stack and by exactly that many cells on
//! it. Anything else holding it (a caller that kept its handle, a pending
//! argument, another record) makes the count differ, and the read stays an
//! ordinary copy-free read of a field that is still there.
//!
//! The static conditions, over the field values of one record literal or
//! update (the base of an update is read before them):
//!
//! - every read of the local in those values is a direct projection
//!   `local.field`, never the bare local, a pattern subject or a callee;
//! - each field is projected at most once, so no read after the take can see
//!   the emptied field;
//! - one of those reads is the local's last use, so nothing after the literal
//!   or update reads the local at all;
//! - for an update whose base is this local, the field is one the update
//!   writes, so the new record never copies the emptied slot.

use std::collections::{HashMap, HashSet};

use crate::ir::mir::{MirCallee, MirExpr};

/// Fields of one local that a record literal or update may take.
#[derive(Debug, Clone)]
pub(super) struct FieldTakePlan {
    pub(super) slot: u32,
    pub(super) fields: HashSet<String>,
    /// The local is the base of the update, so one operand-stack cell below
    /// the field values holds the record.
    pub(super) base_cell: bool,
}

#[derive(Default)]
struct SlotReads {
    bare: bool,
    projected: HashMap<String, usize>,
    last_use: bool,
}

fn collect_reads(expr: &MirExpr, reads: &mut HashMap<u32, SlotReads>) {
    match expr {
        MirExpr::Project(p) => {
            if let MirExpr::Local(local) = &p.node.base.node {
                let entry = reads.entry(local.node.slot.0).or_default();
                *entry.projected.entry(p.node.field.clone()).or_default() += 1;
                entry.last_use |= local.node.last_use;
                return;
            }
            collect_reads(&p.node.base.node, reads);
        }
        MirExpr::Local(local) => {
            let entry = reads.entry(local.node.slot.0).or_default();
            entry.bare = true;
            entry.last_use |= local.node.last_use;
        }
        MirExpr::Call(call) => {
            if let MirCallee::LocalSlot { slot, .. } = &call.node.callee {
                reads.entry(u32::from(*slot)).or_default().bare = true;
            }
            for arg in &call.node.args {
                collect_reads(&arg.node, reads);
            }
        }
        other => {
            crate::ir::mir::expr::walk_children(other, &mut |child| collect_reads(child, reads))
        }
    }
}

/// The takes one record literal (`base_slot == None`) or one update of a
/// local base (`base_slot == Some(slot)`) allows. `written` is the set of
/// fields the update writes; a literal writes every field.
pub(super) fn plan_field_takes<'e>(
    base_slot: Option<u32>,
    written: &HashSet<&str>,
    values: impl IntoIterator<Item = &'e MirExpr>,
) -> Vec<FieldTakePlan> {
    let mut reads: HashMap<u32, SlotReads> = HashMap::new();
    for value in values {
        collect_reads(value, &mut reads);
    }
    let mut plans = Vec::new();
    for (slot, slot_reads) in reads {
        if slot_reads.bare || !slot_reads.last_use {
            continue;
        }
        let base_cell = base_slot == Some(slot);
        let fields: HashSet<String> = slot_reads
            .projected
            .into_iter()
            .filter(|(field, count)| {
                *count == 1 && (!base_cell || written.contains(field.as_str()))
            })
            .map(|(field, _)| field)
            .collect();
        if !fields.is_empty() {
            plans.push(FieldTakePlan {
                slot,
                fields,
                base_cell,
            });
        }
    }
    plans
}
