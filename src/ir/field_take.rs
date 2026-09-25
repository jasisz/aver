//! Which fields a record literal or update may take out of a local record
//! it consumes: the planning the VM compiles from and `aver check` reads.
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
//! ## Fields further down
//!
//! The same holds a level or more down. In
//! `Setting(window = absorbed(setting.window.created, setting.window.spent), rounds = setting.rounds + 1)`
//! the Maps sit in `window`, which `setting` holds. A path read
//! `local.f1.….fn` is compiled to `RECORD_TAKE_PATH`, which checks every
//! record on the way down: the local's record as above, and each record below
//! it held off the stack by exactly its parent (or by nothing, once the path
//! took it out of the parent) and on the stack by exactly the cells the
//! compiler counted. Those cells are the bases of the updates the read sits
//! in: in `Window.update(setting.window, created = Map.set(setting.window.created, k, v))`
//! the update's base holds `window` while `created` is read.
//!
//! The static conditions, over the field values of one record literal or
//! update (the base of an update is read before them), for one path read `P`
//! of a local:
//!
//! - every read of the local in those values is a path `local.f…`, never the
//!   bare local, a pattern subject or a callee, except as the base of an
//!   update inside them;
//! - no other read goes through `P` or below it, so no read after the take
//!   can see the emptied field;
//! - one of the reads is the local's last use, so nothing after the literal
//!   or update reads the local at all;
//! - every update base that is a proper prefix of `P` (the local itself or a
//!   shorter path) is the base of an update enclosing `P` that writes the next
//!   field of `P`, so no new record copies the emptied slot;
//! - a record on the way down is taken out of its parent too when `P` is the
//!   only read through it besides those bases, so an update of it finds it
//!   held by nothing else and can move its fields.

use std::collections::{HashMap, HashSet};

/// Fields of one local that a record literal or update may take.
#[derive(Debug, Clone)]
pub(crate) struct FieldTakePlan {
    pub(crate) slot: u32,
    pub(crate) paths: Vec<PathTake>,
}

impl FieldTakePlan {
    pub(crate) fn path(&self, fields: &[&str]) -> Option<&PathTake> {
        self.paths.iter().find(|path| {
            path.fields.len() == fields.len() && path.fields.iter().zip(fields).all(|(a, b)| a == b)
        })
    }
}

/// One path read of the local whose last field may be taken.
#[derive(Debug, Clone)]
pub(crate) struct PathTake {
    pub(crate) fields: Vec<String>,
    /// Update bases on the operand stack that hold the local's record while
    /// the path is read. The local's own cell comes on top when the read is
    /// not its last use.
    pub(crate) root_bases: u8,
    /// One entry per record below the local's, from `local.f1` down to
    /// `local.f1.….f(n-1)`.
    pub(crate) levels: Vec<PathLevel>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PathLevel {
    /// Take this record out of its parent on the way down.
    pub(crate) detach: bool,
    /// Update bases on the operand stack that hold this record.
    pub(crate) holders: u8,
}

struct PathRead {
    fields: Vec<String>,
    /// Indices into `SlotReads::bases` of the updates this read sits in.
    enclosing: Vec<usize>,
}

struct BaseRead {
    fields: Vec<String>,
    written: HashSet<String>,
}

#[derive(Default)]
struct SlotReads {
    bare: bool,
    last_use: bool,
    paths: Vec<PathRead>,
    bases: Vec<BaseRead>,
}

/// The reads of locals inside the field values of one record literal or
/// update, in the terms the planning needs. Filled by a walk over MIR in `vm::compiler::field_take` and
/// by the same walk over the checked AST in `checker::shared_update`, so the
/// warning and the VM agree on what is taken.
#[derive(Default)]
pub(crate) struct ReadLog {
    reads: HashMap<u32, SlotReads>,
    /// `(slot, index into bases)` of the updates being walked, outermost first.
    enclosing: Vec<(u32, usize)>,
}

impl ReadLog {
    /// An update whose base is `local.fields…` (the bare local when `fields`
    /// is empty) and that writes `written`. Every read logged until the
    /// matching [`ReadLog::leave_base`] sits inside it.
    pub(crate) fn enter_base(
        &mut self,
        slot: u32,
        last_use: bool,
        fields: Vec<String>,
        written: HashSet<String>,
    ) {
        let entry = self.reads.entry(slot).or_default();
        entry.last_use |= last_use;
        entry.bases.push(BaseRead { fields, written });
        self.enclosing.push((slot, entry.bases.len() - 1));
    }

    pub(crate) fn leave_base(&mut self) {
        self.enclosing.pop();
    }

    /// A read of `local.fields…`; with no fields, a read of the whole local.
    pub(crate) fn read(&mut self, slot: u32, last_use: bool, fields: Vec<String>) {
        let enclosing = self
            .enclosing
            .iter()
            .filter(|(s, _)| *s == slot)
            .map(|(_, index)| *index)
            .collect();
        let entry = self.reads.entry(slot).or_default();
        entry.last_use |= last_use;
        if fields.is_empty() {
            entry.bare = true;
        } else {
            entry.paths.push(PathRead { fields, enclosing });
        }
    }

    /// A use of the local the planning cannot follow, such as a call through
    /// it.
    pub(crate) fn opaque(&mut self, slot: u32) {
        self.reads.entry(slot).or_default().bare = true;
    }

    /// The paths each local lets the literal or update take.
    pub(crate) fn plans(self) -> Vec<FieldTakePlan> {
        let mut plans = Vec::new();
        for (slot, reads) in self.reads {
            if reads.bare || !reads.last_use {
                continue;
            }
            let paths: Vec<PathTake> = (0..reads.paths.len())
                .filter_map(|index| plan_path(&reads, index))
                .collect();
            if !paths.is_empty() {
                plans.push(FieldTakePlan { slot, paths });
            }
        }
        plans
    }
}

fn starts_with(fields: &[String], prefix: &[String]) -> bool {
    fields.len() >= prefix.len() && fields[..prefix.len()] == *prefix
}

/// The take one path read allows, or `None`. `index` is the read's position
/// in `reads.paths`.
fn plan_path(reads: &SlotReads, index: usize) -> Option<PathTake> {
    let read = &reads.paths[index];
    let path = &read.fields;
    let others = reads
        .paths
        .iter()
        .enumerate()
        .filter(|(other, _)| *other != index)
        .map(|(_, other)| other);
    // Nothing else reads the taken field or anything below it.
    if others.clone().any(|other| starts_with(&other.fields, path))
        || reads
            .bases
            .iter()
            .any(|base| starts_with(&base.fields, path))
    {
        return None;
    }
    // Every update base on the way down encloses the read and writes the
    // field the path goes on through.
    let mut holders = Vec::with_capacity(path.len());
    for depth in 0..path.len() {
        let prefix = &path[..depth];
        let mut count = 0u8;
        for (base_index, base) in reads.bases.iter().enumerate() {
            if base.fields != prefix {
                continue;
            }
            if !read.enclosing.contains(&base_index) || !base.written.contains(&path[depth]) {
                return None;
            }
            count = count.checked_add(1)?;
        }
        holders.push(count);
    }
    let levels = (1..path.len())
        .map(|depth| {
            let prefix = &path[..depth];
            // Every other read through this record is one of the enclosing
            // bases above, all read before this path.
            let only_read = !others
                .clone()
                .any(|other| starts_with(&other.fields, prefix))
                && reads.bases.iter().all(|base| {
                    !starts_with(&base.fields, prefix) || starts_with(path, &base.fields)
                });
            PathLevel {
                detach: only_read,
                holders: holders[depth],
            }
        })
        .collect();
    Some(PathTake {
        fields: path.clone(),
        root_bases: holders[0],
        levels,
    })
}
