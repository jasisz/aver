use super::*;

/// Where the evacuation walk has to put an entry back once it has rewritten
/// it: into the compacted region it was moved to, or into the slot it never
/// left.
///
/// The second case is an out-of-region value: an in-place vector write can
/// leave a below-mark slot holding an above-mark index (see
/// [`Arena::take_local_value`]). It mirrors `PromoteRewriteSlot`, which the
/// promotion walk has always had.
#[derive(Clone, Copy, Debug)]
pub(crate) enum LocalRewriteSlot {
    InPlace {
        space: HeapSpace,
        raw_index: usize,
    },
    Evacuated {
        target: AllocSpace,
        compacted_pos: u32,
    },
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum PromoteRewriteSlot {
    InPlace {
        space: HeapSpace,
        raw_index: usize,
    },
    Promoted {
        target: AllocSpace,
        raw_index: usize,
    },
}

/// An entry a boundary walk has taken out of its slot and queued: the handle
/// that names it now, the entry, still to be rewritten, and where `slot` says
/// to put it back.
#[derive(Clone, Debug)]
pub(crate) struct Taken<T: ArenaTypes, S> {
    value: NanValue,
    entry: ArenaEntry<T>,
    slot: S,
}

/// Queue `taken` and hand back the handle its holder should store.
fn queue<T: ArenaTypes, S>(pending: &mut Vec<Taken<T, S>>, taken: Taken<T, S>) -> NanValue {
    let value = taken.value;
    pending.push(taken);
    value
}

impl<T: ArenaTypes> Arena<T> {
    pub fn truncate_to(&mut self, mark: u32) {
        if self.young_entries.len() <= mark as usize {
            return;
        }
        self.begin_lane_rewrite(INVALID_LANE_MARK);
        self.young_entries.truncate(mark as usize);
        self.note_young_truncated(mark);
        self.finish_lane_rewrite();
    }

    pub fn collect_young_from_roots(&mut self, mark: u32, roots: &mut [NanValue]) {
        if self.young_entries.len() <= mark as usize {
            return;
        }
        self.begin_lane_rewrite(INVALID_LANE_MARK);

        let region = self.young_entries.len() - mark as usize;
        let mut relocated = Self::take_u32_scratch(&mut self.scratch_young, region);
        let mut compacted = Vec::with_capacity(region);

        for root in roots {
            *root = self.relocate_young_root(*root, mark, &mut relocated, &mut compacted);
        }

        self.young_entries.truncate(mark as usize);
        self.note_young_truncated(mark);
        self.young_entries.extend(compacted);
        Self::recycle_u32_scratch(&mut self.scratch_young, relocated);
        self.finish_lane_rewrite();
    }

    pub fn truncate_yard_to(&mut self, mark: u32) {
        if self.yard_entries.len() <= mark as usize {
            return;
        }
        self.begin_lane_rewrite(INVALID_LANE_MARK);
        self.yard_entries.truncate(mark as usize);
        self.note_yard_truncated(mark);
        self.finish_lane_rewrite();
    }

    pub fn truncate_handoff_to(&mut self, mark: u32) {
        if self.handoff_entries.len() <= mark as usize {
            return;
        }
        self.begin_lane_rewrite(INVALID_LANE_MARK);
        self.handoff_entries.truncate(mark as usize);
        self.note_handoff_truncated(mark);
        self.finish_lane_rewrite();
    }

    /// `rewrite_out_of_region_roots` reports that the returning frame, or
    /// something it called, stored a value into an arena slot outside its own
    /// regions — see [`Arena::rewrite_out_of_region_roots`].
    pub fn evacuate_frame_to_yard(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        lane_mark: LaneMark,
        roots: &mut [NanValue],
        rewrite_out_of_region_roots: bool,
    ) -> (bool, bool) {
        self.evacuate_frame_locals(
            young_mark,
            yard_mark,
            handoff_mark,
            lane_mark,
            roots,
            AllocSpace::Yard,
            rewrite_out_of_region_roots,
        )
    }

    /// See [`Arena::evacuate_frame_to_yard`] for `rewrite_out_of_region_roots`.
    pub fn evacuate_frame_to_handoff(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        lane_mark: LaneMark,
        roots: &mut [NanValue],
        rewrite_out_of_region_roots: bool,
    ) -> (bool, bool) {
        self.evacuate_frame_locals(
            young_mark,
            yard_mark,
            handoff_mark,
            lane_mark,
            roots,
            AllocSpace::Handoff,
            rewrite_out_of_region_roots,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_frame_locals(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        lane_mark: LaneMark,
        roots: &mut [NanValue],
        young_target: AllocSpace,
        rewrite_out_of_region_roots: bool,
    ) -> (bool, bool) {
        let has_work = self.young_entries.len() > young_mark as usize
            || self.yard_entries.len() > yard_mark as usize
            || self.handoff_entries.len() > handoff_mark as usize;
        if !has_work && !rewrite_out_of_region_roots {
            return (false, false);
        }
        if has_work {
            self.begin_lane_rewrite(lane_mark);
            self.active_lane_hands_to_caller = matches!(young_target, AllocSpace::Handoff);
        }
        self.rewrite_out_of_region_roots = rewrite_out_of_region_roots;
        if rewrite_out_of_region_roots {
            self.forget_handoff_block();
            self.begin_inplace_visit_epoch();
        }
        let mut relocated_young = Self::take_u32_scratch(
            &mut self.scratch_young,
            self.young_entries.len().saturating_sub(young_mark as usize),
        );
        let mut relocated_yard = Self::take_u32_scratch(
            &mut self.scratch_yard,
            self.yard_entries.len().saturating_sub(yard_mark as usize),
        );
        let mut relocated_handoff = Self::take_u32_scratch(
            &mut self.scratch_handoff,
            self.handoff_entries
                .len()
                .saturating_sub(handoff_mark as usize),
        );
        let mut compacted_yard =
            Vec::with_capacity(self.yard_entries.len().saturating_sub(yard_mark as usize));
        let mut compacted_handoff = Vec::with_capacity(
            self.handoff_entries
                .len()
                .saturating_sub(handoff_mark as usize),
        );

        let mut pending = core::mem::take(&mut self.scratch_local_pending);
        for root in roots.iter_mut().filter(|root| root.heap_index().is_some()) {
            *root = self.evacuate_local_root(
                *root,
                young_mark,
                yard_mark,
                handoff_mark,
                young_target,
                &mut relocated_young,
                &mut relocated_yard,
                &mut relocated_handoff,
                &mut compacted_yard,
                &mut compacted_handoff,
                &mut pending,
            );
        }
        self.scratch_local_pending = pending;

        self.young_entries.truncate(young_mark as usize);
        self.note_young_truncated(young_mark);
        self.yard_entries.truncate(yard_mark as usize);
        self.note_yard_truncated(yard_mark);
        self.yard_entries.extend(compacted_yard);
        self.handoff_entries.truncate(handoff_mark as usize);
        self.note_handoff_truncated(handoff_mark);
        self.handoff_entries.extend(compacted_handoff);
        self.note_peak_usage();
        self.rewrite_out_of_region_roots = false;
        Self::recycle_u32_scratch(&mut self.scratch_young, relocated_young);
        Self::recycle_u32_scratch(&mut self.scratch_yard, relocated_yard);
        Self::recycle_u32_scratch(&mut self.scratch_handoff, relocated_handoff);
        if has_work {
            self.finish_lane_rewrite();
        }

        (
            self.yard_entries.len() > yard_mark as usize,
            self.handoff_entries.len() > handoff_mark as usize,
        )
    }

    fn allocate_local_target_slot(
        target: AllocSpace,
        yard_mark: u32,
        handoff_mark: u32,
        compacted_yard: &mut Vec<ArenaEntry<T>>,
        compacted_handoff: &mut Vec<ArenaEntry<T>>,
    ) -> (u32, u32) {
        match target {
            AllocSpace::Yard => {
                let pos = compacted_yard.len() as u32;
                let idx = Self::encode_yard_index(yard_mark + pos);
                compacted_yard.push(ArenaEntry::Int(0));
                (idx, pos)
            }
            AllocSpace::Handoff => {
                let pos = compacted_handoff.len() as u32;
                let idx = Self::encode_handoff_index(handoff_mark + pos);
                compacted_handoff.push(ArenaEntry::Int(0));
                (idx, pos)
            }
            AllocSpace::Young => unreachable!("local evacuation target must be yard or handoff"),
        }
    }

    fn store_local_target_entry(
        target: AllocSpace,
        compacted_pos: u32,
        entry: ArenaEntry<T>,
        compacted_yard: &mut [ArenaEntry<T>],
        compacted_handoff: &mut [ArenaEntry<T>],
    ) {
        match target {
            AllocSpace::Yard => compacted_yard[compacted_pos as usize] = entry,
            AllocSpace::Handoff => compacted_handoff[compacted_pos as usize] = entry,
            AllocSpace::Young => unreachable!(),
        }
    }

    fn store_local_rewrite_slot(
        &mut self,
        slot: LocalRewriteSlot,
        entry: ArenaEntry<T>,
        compacted_yard: &mut [ArenaEntry<T>],
        compacted_handoff: &mut [ArenaEntry<T>],
    ) {
        match slot {
            LocalRewriteSlot::Evacuated {
                target,
                compacted_pos,
            } => Self::store_local_target_entry(
                target,
                compacted_pos,
                entry,
                compacted_yard,
                compacted_handoff,
            ),
            LocalRewriteSlot::InPlace { space, raw_index } => {
                self.store_out_of_region_entry(space, raw_index, entry)
            }
        }
    }

    /// Take an out-of-region entry out of its slot, leaving `Int(0)` behind.
    ///
    /// `None` means there is nothing here worth rewriting: an index past the
    /// end of its space, or one inside the region this boundary is about to
    /// compact — a stale index is only ever read back out of an entry the
    /// boundary KEEPS. `None` is also what a slot this boundary has already
    /// descended into returns, which is what keeps the walk idempotent.
    fn take_out_of_region_entry(
        &mut self,
        space: HeapSpace,
        raw_index: usize,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
    ) -> Option<ArenaEntry<T>> {
        let survives_the_truncate = match space {
            HeapSpace::Young => {
                raw_index < self.young_entries.len() && raw_index < young_mark as usize
            }
            HeapSpace::Yard => {
                raw_index < self.yard_entries.len() && raw_index < yard_mark as usize
            }
            HeapSpace::Handoff => {
                raw_index < self.handoff_entries.len() && raw_index < handoff_mark as usize
            }
            HeapSpace::Stable => raw_index < self.stable_entries.len(),
        };
        if !survives_the_truncate || !self.claim_inplace_visit(space, raw_index) {
            return None;
        }
        Some(match space {
            HeapSpace::Young => {
                core::mem::replace(&mut self.young_entries[raw_index], ArenaEntry::Int(0))
            }
            HeapSpace::Yard => {
                core::mem::replace(&mut self.yard_entries[raw_index], ArenaEntry::Int(0))
            }
            HeapSpace::Handoff => {
                core::mem::replace(&mut self.handoff_entries[raw_index], ArenaEntry::Int(0))
            }
            HeapSpace::Stable => {
                core::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0))
            }
        })
    }

    fn store_out_of_region_entry(
        &mut self,
        space: HeapSpace,
        raw_index: usize,
        entry: ArenaEntry<T>,
    ) {
        match space {
            HeapSpace::Young => self.young_entries[raw_index] = entry,
            HeapSpace::Yard => self.yard_entries[raw_index] = entry,
            HeapSpace::Handoff => self.handoff_entries[raw_index] = entry,
            HeapSpace::Stable => self.stable_entries[raw_index] = entry,
        }
    }

    #[inline(always)]
    fn rewrite_entry_with<F>(&mut self, entry: ArenaEntry<T>, rewrite: &mut F) -> ArenaEntry<T>
    where
        F: FnMut(&mut Arena<T>, NanValue) -> NanValue,
    {
        match entry {
            ArenaEntry::Int(i) => ArenaEntry::Int(i),
            ArenaEntry::BigInt(b) => ArenaEntry::BigInt(b),
            ArenaEntry::String(s) => ArenaEntry::String(s),
            ArenaEntry::Builtin(name) => ArenaEntry::Builtin(name),
            ArenaEntry::Fn(f) => ArenaEntry::Fn(f),
            ArenaEntry::Boxed {
                value,
                holder_count,
            } => ArenaEntry::Boxed {
                value: rewrite(self, value),
                holder_count,
            },
            ArenaEntry::List(list) => ArenaEntry::List(self.rewrite_list_with(list, rewrite)),
            ArenaEntry::Tuple {
                mut items,
                holder_count,
            } => {
                for value in &mut items {
                    *value = rewrite(self, *value);
                }
                ArenaEntry::Tuple {
                    items,
                    holder_count,
                }
            }
            ArenaEntry::Vector {
                mut items,
                scan_receipt,
                all_immediate,
                holder_count,
            } => {
                // `holder_count` travels with the entry untouched — same
                // argument as the map arm: a rewrite renames indices, it
                // neither creates a reference nor discharges one.
                //
                // The all-immediate escape is the vector spelling of the ones
                // `rewrite_list_body_with` and `rewrite_map_with` already have.
                // A rewrite renames arena indices; an entry that holds none has
                // nothing to rename, and reading it in full to find that out is
                // the entire cost of carrying a table of offsets through a
                // loop. A heap-backed vector cannot take that escape, so its
                // lane receipt records the full scan once and lets later frame
                // boundaries skip it until a heap write invalidates the proof.
                //
                // Unlike the map and list escapes, this one does NOT re-prove
                // the flag here under debug assertions. It cannot: the walk
                // that would prove it is the walk being skipped, so a debug
                // build would keep the quadratic the flag removes — and this
                // table is threaded through every step of every indexed string
                // loop, so that is the whole test suite. The promise is proved
                // where it is MADE instead, and every place that makes it
                // decides from the values in hand: `Arena::push_vector` reads
                // each element as it stores it, `Arena::vector_store_in_place`
                // and the owned `Vector.set` inherit a proven flag and clear it
                // for the single element they write, and
                // `Arena::get_vector_mut` clears it because it cannot know.
                // `arena::tests` checks this crate's three producers against a
                // from-scratch walk, and the owned `Vector.set` is checked the
                // same way in the crate it lives in.
                if all_immediate || self.lane_receipt_can_skip(scan_receipt) {
                    return ArenaEntry::Vector {
                        items,
                        scan_receipt,
                        all_immediate,
                        holder_count,
                    };
                }
                self.vector_elements_scanned += items.len() as u64;
                for value in &mut items {
                    *value = rewrite(self, *value);
                }
                ArenaEntry::Vector {
                    items,
                    scan_receipt: self.renewed_lane_receipt(),
                    all_immediate,
                    holder_count,
                }
            }
            ArenaEntry::Map {
                map,
                all_immediate,
                scan_receipt,
                pending_scan_keys,
                holder_count,
            } => self.rewrite_map_with(
                map,
                all_immediate,
                scan_receipt,
                pending_scan_keys,
                holder_count,
                rewrite,
            ),
            ArenaEntry::Record {
                type_id,
                mut fields,
                holder_count,
            } => {
                for value in &mut fields {
                    *value = rewrite(self, *value);
                }
                ArenaEntry::Record {
                    type_id,
                    fields,
                    holder_count,
                }
            }
            ArenaEntry::Variant {
                type_id,
                variant_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = rewrite(self, *value);
                }
                ArenaEntry::Variant {
                    type_id,
                    variant_id,
                    fields,
                }
            }
            ArenaEntry::Namespace { name, mut members } => {
                for (_, value) in &mut members {
                    *value = rewrite(self, *value);
                }
                ArenaEntry::Namespace { name, members }
            }
        }
    }

    /// Rewrite a map entry, keeping the table untouched when nothing in it can
    /// move.
    ///
    /// A table in which no key and no value carries an arena index has nothing
    /// for the rewrite to change, so it is returned without being read at all —
    /// the same escape [`Arena::rewrite_list_body_with`] takes for a body of
    /// immediates, and the reason a fold over a `Map<Int, Int>` no longer
    /// re-reads the whole map on every frame boundary.
    ///
    /// Without it every entry is read to find out whether anything has to move,
    /// and on the stable-promotion path the read also *duplicates* the table:
    /// `promote_value_to_stable` clones the entry out of a slot that still holds
    /// the map, so the `Rc::make_mut` inside `rewrite_values_mut` sees a second
    /// owner and rebuilds the whole thing. That copy is real and is not in
    /// `map_entries_copied`, which counts what the builtins do; the entry count
    /// it costs is the number recorded here.
    ///
    /// It is a separate function rather than a block inside
    /// [`Arena::rewrite_entry_with`] for the same reason `rewrite_list_with` is:
    /// that one is `inline(always)` at eight call sites and should not carry a
    /// per-type body.
    ///
    /// A map that does *not* get the escape pays for it. The same
    /// `Map<Int, String>` fold of 16,000 heap-backed values runs in 0.98 s
    /// without this flag and 1.10 s with it, and the 12% is not the flag test —
    /// it survives moving this body out of the always-inlined caller, and it
    /// survives removing the matching arm in `promote_entry_to_target`, while an
    /// equivalent list program is unchanged to the millisecond. It looks like
    /// code layout in the collector's hot loop, which is the cost of admission
    /// for taking a `Map<Int, Int>` fold from 12.2 s to 0.03 s at 64,000 keys.
    ///
    /// `holder_count` travels with the entry untouched. A rewrite renames
    /// indices; it neither creates a reference nor discharges one, so whoever
    /// held this slot before holds it after.
    fn rewrite_map_with<F>(
        &mut self,
        mut map: T::Map,
        all_immediate: bool,
        scan_receipt: LaneMark,
        mut pending_scan_keys: Vec<u64>,
        holder_count: u32,
        rewrite: &mut F,
    ) -> ArenaEntry<T>
    where
        F: FnMut(&mut Arena<T>, NanValue) -> NanValue,
    {
        debug_assert!(
            !all_immediate || pending_scan_keys.is_empty(),
            "an all-immediate map cannot need remembered heap rewrites"
        );
        if all_immediate && pending_scan_keys.is_empty() {
            return ArenaEntry::Map {
                map,
                all_immediate: true,
                scan_receipt,
                pending_scan_keys,
                holder_count,
            };
        }
        if self.lane_receipt_can_skip(scan_receipt) {
            if !pending_scan_keys.is_empty() {
                // An owned insert remembers a hash without checking whether it
                // already has, and a pair must not be rewritten twice: the
                // second rewrite would read the first one's new index as an
                // old one.
                pending_scan_keys.sort_unstable();
                pending_scan_keys.dedup();
                for key in &pending_scan_keys {
                    let found = map.rewrite_value_mut(key, |pair| {
                        pair.0 = rewrite(self, pair.0);
                        pair.1 = rewrite(self, pair.1);
                    });
                    self.note_map_entries_scanned(usize::from(found));
                }
                // Survivors handed to the caller land in the caller's region,
                // which the caller's own boundary still has to rewrite. A fresh
                // receipt would be younger than the caller's mark and make that
                // boundary read the whole table, so the table keeps its old
                // receipt and the remembered pairs stay remembered. A loop that
                // inserts on every step drops them at its own tail boundary,
                // which renews below; a set that has grown to half the table
                // is renewed here too, since reading it costs as much.
                let keep =
                    self.active_lane_hands_to_caller && 2 * pending_scan_keys.len() <= map.len();
                let scan_receipt = if keep {
                    scan_receipt
                } else {
                    pending_scan_keys.clear();
                    self.renewed_lane_receipt()
                };
                return ArenaEntry::Map {
                    map,
                    all_immediate: false,
                    scan_receipt,
                    pending_scan_keys,
                    holder_count,
                };
            }
            return ArenaEntry::Map {
                map,
                all_immediate: false,
                scan_receipt,
                pending_scan_keys,
                holder_count,
            };
        }
        self.note_map_entries_scanned(map.len());
        map.rewrite_values_mut(|pair| {
            pair.0 = rewrite(self, pair.0);
            pair.1 = rewrite(self, pair.1);
        });
        // A rewrite maps arena indices to arena indices, so nothing in the table
        // can have become immediate; keeping the flag off is both correct and
        // the safe direction anyway.
        ArenaEntry::Map {
            map,
            all_immediate: false,
            scan_receipt: self.renewed_lane_receipt(),
            pending_scan_keys: Vec::new(),
            holder_count,
        }
    }

    #[inline(always)]
    fn rewrite_list_with<F>(&mut self, list: ArenaList, rewrite: &mut F) -> ArenaList
    where
        F: FnMut(&mut Arena<T>, NanValue) -> NanValue,
    {
        match list {
            ArenaList::Flat {
                items,
                start,
                scan_receipt,
            } => {
                let (items, start, scan_receipt) =
                    self.rewrite_list_body_with(items, start, scan_receipt, rewrite);
                ArenaList::Flat {
                    items,
                    start,
                    scan_receipt,
                }
            }
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: rewrite(self, head),
                tail: rewrite(self, tail),
                len,
            },
            ArenaList::Concat { left, right, len } => ArenaList::Concat {
                left: rewrite(self, left),
                right: rewrite(self, right),
                len,
            },
            ArenaList::Segments {
                current,
                rest,
                start,
                len,
            } => {
                let current = rewrite(self, current);
                // `Segments` already fills ArenaEntry's 48-byte payload. A
                // full u64 receipt would grow every arena slot, so segment
                // bodies deliberately remain the conservative always-scan
                // spelling while Flat covers the hot traversal shape.
                let (rest, start, _) =
                    self.rewrite_list_body_with(rest, start, INVALID_LANE_MARK, rewrite);
                ArenaList::Segments {
                    current,
                    rest,
                    start,
                    len,
                }
            }
        }
    }

    /// Rewrite the shared element storage behind a `Flat` or `Segments` view,
    /// keeping the existing allocation whenever nothing actually moves.
    ///
    /// Three outcomes, cheapest first. A body built entirely out of immediates
    /// cannot hold anything the collector could relocate, so it is returned
    /// without being read — literally, in every build, which is what makes
    /// [`Arena::list_elements_scanned`] tell the truth about what a walk over a
    /// list of integers costs. A valid lane receipt proves the body predates the
    /// frame-local suffix and likewise skips the walk, renewing the receipt into
    /// the post-boundary epoch. Otherwise the elements are rewritten, and the
    /// original body is still returned when every rewrite is the identity —
    /// preserving both `Rc` sharing and the O(1) `start` offset that
    /// `list_uncons` hands out. Only a body in which something moved is rebuilt.
    ///
    /// Without this, walking a list while a frame keeps allocating rebuilt the
    /// whole remaining input on every step: n^2/2 element copies for one
    /// traversal (issue #886).
    ///
    /// The receipt closes the repeated-read half of issue #886 for Flat lists:
    /// once a body of strings or records has been proved clean for a frame
    /// watermark, later tail boundaries carry that proof instead of walking the
    /// shrinking suffix again. Segments deliberately pass an invalid receipt
    /// because a u64 carrier would grow every arena slot, so their shared body
    /// remains on the conservative scan path.
    ///
    /// Carrying `start` through the second escape is the delicate part, because
    /// losing it does not fail loudly: the elements the slice had already
    /// stepped past come back, and the list silently regrows its head. Both
    /// places that can lose it — the untouched-body return and the prefix the
    /// rebuild starts from — are covered by
    /// `a_walked_body_that_did_not_move_keeps_the_slice_offset` and
    /// `rebuilding_a_walked_body_copies_the_prefix_from_the_slice_offset`. Both
    /// use heap-backed elements on purpose: a body of immediates leaves above
    /// without ever reaching this code, which is why every test that drives this
    /// through a `List<Int>` program reaches neither line.
    fn rewrite_list_body_with<F>(
        &mut self,
        body: Rc<ListBody>,
        start: usize,
        scan_receipt: LaneMark,
        rewrite: &mut F,
    ) -> (Rc<ListBody>, usize, LaneMark)
    where
        F: FnMut(&mut Arena<T>, NanValue) -> NanValue,
    {
        debug_assert!(start <= body.len());
        // The escape does NOT re-prove the flag here, under debug assertions or
        // otherwise — the same rule the vector escape in `rewrite_entry_with`
        // states, and for the same reason. The walk that would prove it is the
        // walk being skipped, so proving it here costs exactly what the flag
        // saves, and a debug build keeps the quadratic in full: a loop that
        // carries a list past a frame boundary reads the whole body on every
        // step. `Bytes.fromList` over a list of 65,536 integers spent 6.9 s in
        // that re-proof and 116 ms without it.
        //
        // The promise is proved where it is MADE instead, and for a body there
        // is exactly one place: `ListBody::new` decides the flag from the very
        // predicate this line would re-test, over the items it is about to
        // store, and nothing afterwards can change the answer — `items` is
        // private, the body is immutable, and the only borrow it hands out is a
        // shared slice. `a_body_agrees_with_a_from_scratch_walk` checks that
        // single producer against a from-scratch walk.
        if body.all_immediate() {
            return (body, start, scan_receipt);
        }

        if self.lane_receipt_can_skip(scan_receipt) {
            return (body, start, scan_receipt);
        }

        self.list_elements_scanned += (body.len() - start) as u64;
        let mut rewritten: Option<Vec<NanValue>> = None;
        for offset in start..body.len() {
            let value = body[offset];
            let new_value = rewrite(self, value);
            match &mut rewritten {
                Some(out) => out.push(new_value),
                None if new_value.bits() == value.bits() => {}
                None => {
                    let mut out = Vec::with_capacity(body.len() - start);
                    out.extend_from_slice(&body[start..offset]);
                    out.push(new_value);
                    rewritten = Some(out);
                }
            }
        }

        match rewritten {
            None => (body, start, self.renewed_lane_receipt()),
            Some(out) => {
                self.list_elements_copied += out.len() as u64;
                (Rc::new(ListBody::new(out)), 0, self.renewed_lane_receipt())
            }
        }
    }

    /// Flatten a deep list (Prepend/Concat chain) into a single Flat entry.
    pub fn flatten_deep_list(&mut self, value: NanValue) -> NanValue {
        const FLATTEN_THRESHOLD: usize = 64;

        if !value.is_list() || value.is_empty_list_immediate() {
            return value;
        }
        let len = self.list_len_value(value);
        if len <= FLATTEN_THRESHOLD {
            return value;
        }
        // Already one flat entry: there is no chain to flatten.
        if matches!(self.get_list(value.arena_index()), ArenaList::Flat { .. }) {
            return value;
        }
        let elements = self.list_to_vec_value(value);
        let flat = ArenaList::Flat {
            items: Rc::new(ListBody::new(elements)),
            start: 0,
            scan_receipt: self.lane_mark(),
        };
        let index = self.push(ArenaEntry::List(flat));
        NanValue::new_list(index)
    }

    /// Evacuate one root and everything it reaches, on a work list rather
    /// than the Rust call stack.
    ///
    /// Each value the walk claims — moved into the target region, or, for an
    /// owned in-place write, rewritten where it stands — is taken out of its
    /// slot by [`Arena::take_local_value`] and queued, and its children are
    /// claimed the same way when it comes off the queue, so a value of a
    /// recursive sum a million levels deep costs no Rust stack. The
    /// relocation tables, and for an in-place rewrite the visit memo, claim a
    /// value before it is queued, so a value reached twice, or one that
    /// reaches itself, is queued once.
    #[allow(clippy::too_many_arguments)]
    fn evacuate_local_root(
        &mut self,
        value: NanValue,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry<T>>,
        compacted_handoff: &mut Vec<ArenaEntry<T>>,
        pending: &mut Vec<Taken<T, LocalRewriteSlot>>,
    ) -> NanValue {
        let value = self.take_local_value(
            value,
            young_mark,
            yard_mark,
            handoff_mark,
            young_target,
            relocated_young,
            relocated_yard,
            relocated_handoff,
            compacted_yard,
            compacted_handoff,
            pending,
        );
        while let Some(current) = pending.pop() {
            let mut queue_child = |arena: &mut Arena<T>, child: NanValue| {
                arena.take_local_value(
                    child,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                    pending,
                )
            };
            let new_entry = self.rewrite_entry_with(current.entry, &mut queue_child);
            self.store_local_rewrite_slot(
                current.slot,
                new_entry,
                compacted_yard,
                compacted_handoff,
            );
        }
        value
    }

    #[allow(clippy::too_many_arguments)]
    fn take_local_value(
        &mut self,
        value: NanValue,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry<T>>,
        compacted_handoff: &mut Vec<ArenaEntry<T>>,
        pending: &mut Vec<Taken<T, LocalRewriteSlot>>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);

        match space {
            HeapSpace::Young if self.is_young_index_in_region(index, young_mark) => {
                let relocation_slot = (raw_index - young_mark) as usize;
                let relocated_index = relocated_young[relocation_slot];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }

                let (new_index, compacted_pos) = Self::allocate_local_target_slot(
                    young_target,
                    yard_mark,
                    handoff_mark,
                    compacted_yard,
                    compacted_handoff,
                );
                relocated_young[relocation_slot] = new_index;
                self.boundary_entries_moved += 1;
                let entry = core::mem::replace(
                    &mut self.young_entries[raw_index as usize],
                    ArenaEntry::Int(0),
                );
                queue(
                    pending,
                    Taken {
                        value: value.with_heap_index(new_index),
                        entry,
                        slot: LocalRewriteSlot::Evacuated {
                            target: young_target,
                            compacted_pos,
                        },
                    },
                )
            }
            HeapSpace::Yard if self.is_yard_index_in_region(index, yard_mark) => {
                let relocation_slot = (raw_index - yard_mark) as usize;
                let relocated_index = relocated_yard[relocation_slot];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }

                let target = match young_target {
                    AllocSpace::Yard => AllocSpace::Yard,
                    AllocSpace::Handoff => AllocSpace::Handoff,
                    AllocSpace::Young => {
                        unreachable!("local evacuation must target yard or handoff")
                    }
                };
                let (new_index, compacted_pos) = Self::allocate_local_target_slot(
                    target,
                    yard_mark,
                    handoff_mark,
                    compacted_yard,
                    compacted_handoff,
                );
                relocated_yard[relocation_slot] = new_index;
                self.boundary_entries_moved += 1;
                let entry = core::mem::replace(
                    &mut self.yard_entries[raw_index as usize],
                    ArenaEntry::Int(0),
                );
                queue(
                    pending,
                    Taken {
                        value: value.with_heap_index(new_index),
                        entry,
                        slot: LocalRewriteSlot::Evacuated {
                            target,
                            compacted_pos,
                        },
                    },
                )
            }
            HeapSpace::Handoff if self.is_handoff_index_in_region(index, handoff_mark) => {
                let relocation_slot = (raw_index - handoff_mark) as usize;
                let relocated_index = relocated_handoff[relocation_slot];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }

                let target = match young_target {
                    AllocSpace::Yard => AllocSpace::Yard,
                    AllocSpace::Handoff => AllocSpace::Handoff,
                    AllocSpace::Young => {
                        unreachable!("local evacuation must target yard or handoff")
                    }
                };
                let (new_index, compacted_pos) = Self::allocate_local_target_slot(
                    target,
                    yard_mark,
                    handoff_mark,
                    compacted_yard,
                    compacted_handoff,
                );
                relocated_handoff[relocation_slot] = new_index;
                self.boundary_entries_moved += 1;
                let entry = core::mem::replace(
                    &mut self.handoff_entries[raw_index as usize],
                    ArenaEntry::Int(0),
                );
                queue(
                    pending,
                    Taken {
                        value: value.with_heap_index(new_index),
                        entry,
                        slot: LocalRewriteSlot::Evacuated {
                            target,
                            compacted_pos,
                        },
                    },
                )
            }
            // A value outside the regions this boundary is about to truncate.
            // Its own handle does not change, but it may hold references INTO
            // those regions, which still have to be rewritten before the
            // truncate. Ordinarily an older slot cannot point into a younger
            // region, so this descent would be pure cost; the VM's owned
            // in-place vector write (`VECTOR_SET_OR_KEEP`) is the one operation
            // that breaks the rule, and `rewrite_out_of_region_roots` is how the
            // caller says such a write happened. The visit memo inside
            // `take_out_of_region_entry` keeps a value reached twice, or one
            // that reaches itself, from being rewritten twice: this walk moves
            // values into a region whose live entries it has not reached yet,
            // so a second rewrite would read a new index as an old one.
            _ if self.rewrite_out_of_region_roots => {
                match self.take_out_of_region_entry(
                    space,
                    raw_index as usize,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                ) {
                    Some(entry) => queue(
                        pending,
                        Taken {
                            value,
                            entry,
                            slot: LocalRewriteSlot::InPlace {
                                space,
                                raw_index: raw_index as usize,
                            },
                        },
                    ),
                    None => value,
                }
            }
            _ => value,
        }
    }

    fn relocate_young_root(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if matches!(space, HeapSpace::Young)
            && raw_index >= mark
            && raw_index < self.young_entries.len() as u32
        {
            return self.relocate_young_value(value, mark, relocated, compacted);
        }
        self.rewrite_young_refs_in_place(space, raw_index, mark, relocated, compacted);
        value
    }

    fn relocate_young_value(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if !matches!(space, HeapSpace::Young) || raw_index < mark {
            return value;
        }

        let relocation_slot = (raw_index - mark) as usize;
        let relocated_index = relocated[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let compacted_pos = compacted.len() as u32;
        let new_index = Self::encode_index(HeapSpace::Young, mark + compacted_pos);
        relocated[relocation_slot] = new_index;
        compacted.push(ArenaEntry::Int(0));

        let entry = core::mem::replace(
            &mut self.young_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.relocate_young_entry(entry, mark, relocated, compacted);
        compacted[compacted_pos as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn relocate_young_entry(
        &mut self,
        entry: ArenaEntry<T>,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) -> ArenaEntry<T> {
        let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
            arena.relocate_young_value(value, mark, relocated, compacted)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
    }

    fn rewrite_young_refs_in_place(
        &mut self,
        space: HeapSpace,
        raw_index: u32,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) {
        let raw_index = raw_index as usize;
        match space {
            HeapSpace::Young => {
                if raw_index >= self.young_entries.len() || raw_index >= mark as usize {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.young_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.young_entries[raw_index] = new_entry;
            }
            HeapSpace::Yard => {
                if raw_index >= self.yard_entries.len() {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.yard_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.yard_entries[raw_index] = new_entry;
            }
            HeapSpace::Handoff => {
                if raw_index >= self.handoff_entries.len() {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.handoff_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.handoff_entries[raw_index] = new_entry;
            }
            HeapSpace::Stable => {
                if raw_index >= self.stable_entries.len() {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.stable_entries[raw_index] = new_entry;
            }
        }
    }

    fn rewrite_young_entry(
        &mut self,
        entry: ArenaEntry<T>,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) -> ArenaEntry<T> {
        let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
            arena.relocate_young_root(value, mark, relocated, compacted)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
    }

    /// Move `value`, and everything it reaches that this promotion may move,
    /// into `target`, on a work list rather than the Rust call stack.
    ///
    /// Each value the walk claims is taken out of its slot by
    /// [`Arena::take_promote_value`] and queued, and its children are claimed
    /// the same way when it comes off the queue, so a value of a recursive sum
    /// a million levels deep costs no Rust stack. A moved value is claimed in
    /// the relocation table before it is queued, so one reached twice is moved
    /// once.
    ///
    /// The root is the one value descended into where it stands whatever the
    /// caller reports. A ROOT may be a slot this frame built outside the young
    /// region and filled with young values — an ordinary helper return builds
    /// its result directly in handoff — so a root that this promotion cannot
    /// move can still hold everything it must.
    /// `vm_real_json_repeated_from_string_calls_keep_error_strings_alive` is the
    /// shape that says so: skipping the descent here drops the young entries
    /// reachable only through such a result.
    fn promote_region_root_to_target(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
        pending: &mut Vec<Taken<T, PromoteRewriteSlot>>,
    ) -> NanValue {
        let value = self.take_promote_value(value, mark, relocated, target, true, pending);
        while let Some(current) = pending.pop() {
            let mut queue_child = |arena: &mut Arena<T>, child: NanValue| {
                let descend = arena.rewrite_out_of_region_roots;
                arena.take_promote_value(child, mark, relocated, target, descend, pending)
            };
            let new_entry = match current.slot {
                PromoteRewriteSlot::Promoted { .. } => {
                    self.promote_entry_to_target(current.entry, mark, &mut queue_child)
                }
                PromoteRewriteSlot::InPlace { .. } => {
                    self.rewrite_entry_with(current.entry, &mut queue_child)
                }
            };
            self.store_promote_rewrite_slot(current.slot, new_entry);
        }
        value
    }

    /// Move every root, and everything it reaches, out of the young region and
    /// into the yard.
    ///
    /// `rewrite_out_of_region_roots` means the same here as in
    /// [`Arena::evacuate_frame_to_yard`]: the caller is reporting an owned
    /// in-place write that landed outside the frame's regions, so a slot this
    /// call cannot move may nonetheless hold something it must. Without one,
    /// the walk stops when a chain of cons cells leaves the young region rather
    /// than reading the rest of it — see `take_promote_value`.
    ///
    /// What the caller owes for that, beyond the flag: the frame this promotion
    /// belongs to must have no yard entries of its own, and at most one handoff
    /// entry, which must be among `roots`. Every caller here is a branch that
    /// has just established exactly that. A frame-local cons cell in the MIDDLE
    /// of a chain could hold young values while sitting outside the young
    /// region, and stopping above it would drop them.
    pub fn promote_young_roots_to_yard(
        &mut self,
        mark: u32,
        lane_mark: LaneMark,
        roots: &mut [NanValue],
        rewrite_out_of_region_roots: bool,
    ) {
        if self.young_entries.len() <= mark as usize {
            return;
        }
        self.begin_lane_rewrite(lane_mark);
        self.rewrite_out_of_region_roots = rewrite_out_of_region_roots;
        if rewrite_out_of_region_roots {
            self.forget_handoff_block();
            self.begin_inplace_visit_epoch();
        }

        let mut relocated = Self::take_u32_scratch(
            &mut self.scratch_young,
            self.young_entries.len() - mark as usize,
        );

        let mut pending = core::mem::take(&mut self.scratch_promote_pending);
        for root in roots.iter_mut().filter(|root| root.heap_index().is_some()) {
            *root = self.promote_region_root_to_target(
                *root,
                mark,
                &mut relocated,
                AllocSpace::Yard,
                &mut pending,
            );
        }
        self.scratch_promote_pending = pending;

        self.young_entries.truncate(mark as usize);
        self.note_young_truncated(mark);
        self.rewrite_out_of_region_roots = false;
        Self::recycle_u32_scratch(&mut self.scratch_young, relocated);
        self.finish_lane_rewrite();
    }

    /// The handoff spelling of [`Arena::promote_young_roots_to_yard`], including
    /// what `rewrite_out_of_region_roots` promises.
    pub fn promote_young_roots_to_handoff(
        &mut self,
        mark: u32,
        lane_mark: LaneMark,
        roots: &mut [NanValue],
        rewrite_out_of_region_roots: bool,
    ) {
        if self.young_entries.len() <= mark as usize {
            return;
        }
        self.begin_lane_rewrite(lane_mark);
        self.active_lane_hands_to_caller = true;
        self.rewrite_out_of_region_roots = rewrite_out_of_region_roots;
        if rewrite_out_of_region_roots {
            self.forget_handoff_block();
            self.begin_inplace_visit_epoch();
        }

        let mut relocated = Self::take_u32_scratch(
            &mut self.scratch_young,
            self.young_entries.len() - mark as usize,
        );

        let mut pending = core::mem::take(&mut self.scratch_promote_pending);
        for root in roots.iter_mut().filter(|root| root.heap_index().is_some()) {
            *root = self.promote_region_root_to_target(
                *root,
                mark,
                &mut relocated,
                AllocSpace::Handoff,
                &mut pending,
            );
        }
        self.scratch_promote_pending = pending;

        self.young_entries.truncate(mark as usize);
        self.note_young_truncated(mark);
        self.rewrite_out_of_region_roots = false;
        Self::recycle_u32_scratch(&mut self.scratch_young, relocated);
        self.finish_lane_rewrite();
    }

    /// Move every root, and everything it reaches, into stable.
    ///
    /// `rewrite_out_of_region_roots` means the same here as in
    /// [`Arena::evacuate_frame_to_yard`]: the caller is reporting an owned
    /// in-place write that landed outside the returning frame's regions. It
    /// only ever changes what happens to a root that is ALREADY stable.
    /// Promotion takes young, yard and handoff wholesale — no region test, no
    /// marks — so a root in any of those three carries whatever it holds along
    /// with it whether the flag is set or not. A stable root moves nowhere, and
    /// without the flag it is not read either, so a stable vector the frame
    /// wrote into keeps pointing at a value the caller is about to truncate
    /// away. That is the one case the flag buys, and it costs a walk of
    /// everything stable the roots reach, which is why it is not free.
    pub fn promote_roots_to_stable(
        &mut self,
        roots: &mut [NanValue],
        rewrite_out_of_region_roots: bool,
    ) {
        let has_work = rewrite_out_of_region_roots
            || roots.iter().any(|value| {
                value
                    .heap_index()
                    .is_some_and(|index| !matches!(Self::decode_index(index).0, HeapSpace::Stable))
            });
        if !has_work {
            return;
        }
        self.begin_lane_rewrite(INVALID_LANE_MARK);
        self.rewrite_out_of_region_roots = rewrite_out_of_region_roots;
        if rewrite_out_of_region_roots {
            self.forget_handoff_block();
            self.begin_inplace_visit_epoch();
        }
        let mut relocated_young =
            Self::take_u32_scratch(&mut self.scratch_young, self.young_entries.len());
        let mut relocated_yard =
            Self::take_u32_scratch(&mut self.scratch_yard, self.yard_entries.len());
        let mut relocated_handoff =
            Self::take_u32_scratch(&mut self.scratch_handoff, self.handoff_entries.len());

        let mut pending = Vec::new();
        for root in roots {
            *root = self.promote_value_to_stable(
                *root,
                &mut relocated_young,
                &mut relocated_yard,
                &mut relocated_handoff,
                &mut pending,
            );
        }
        self.rewrite_out_of_region_roots = false;
        Self::recycle_u32_scratch(&mut self.scratch_young, relocated_young);
        Self::recycle_u32_scratch(&mut self.scratch_yard, relocated_yard);
        Self::recycle_u32_scratch(&mut self.scratch_handoff, relocated_handoff);
        self.finish_lane_rewrite();
    }

    pub fn collect_yard_from_roots(&mut self, mark: u32, roots: &mut [NanValue]) {
        if self.yard_entries.len() <= mark as usize {
            return;
        }
        self.begin_lane_rewrite(INVALID_LANE_MARK);

        let region = self.yard_entries.len() - mark as usize;
        let mut relocated = Self::take_u32_scratch(&mut self.scratch_yard, region);
        let mut compacted = Vec::with_capacity(region);

        for root in roots {
            *root = self.relocate_yard_root(*root, mark, &mut relocated, &mut compacted);
        }

        self.yard_entries.truncate(mark as usize);
        self.note_yard_truncated(mark);
        self.yard_entries.extend(compacted);
        Self::recycle_u32_scratch(&mut self.scratch_yard, relocated);
        self.finish_lane_rewrite();
    }

    pub fn collect_stable_from_roots(&mut self, roots: &mut [NanValue]) {
        if self.stable_entries.is_empty() {
            return;
        }
        self.begin_lane_rewrite(INVALID_LANE_MARK);

        let mut relocated =
            Self::take_u32_scratch(&mut self.scratch_stable, self.stable_entries.len());
        let mut compacted = Vec::with_capacity(self.stable_entries.len());

        let mut pending = Vec::new();
        for root in roots {
            *root = self.relocate_stable_root(*root, &mut relocated, &mut compacted, &mut pending);
        }

        self.stable_entries = compacted;
        Self::recycle_u32_scratch(&mut self.scratch_stable, relocated);
        self.finish_lane_rewrite();
    }

    fn store_promote_rewrite_slot(&mut self, slot: PromoteRewriteSlot, entry: ArenaEntry<T>) {
        match slot {
            PromoteRewriteSlot::InPlace { space, raw_index } => match space {
                HeapSpace::Young => self.young_entries[raw_index] = entry,
                HeapSpace::Yard => self.yard_entries[raw_index] = entry,
                HeapSpace::Handoff => self.handoff_entries[raw_index] = entry,
                HeapSpace::Stable => self.stable_entries[raw_index] = entry,
            },
            PromoteRewriteSlot::Promoted { target, raw_index } => match target {
                AllocSpace::Yard => self.yard_entries[raw_index] = entry,
                AllocSpace::Handoff => self.handoff_entries[raw_index] = entry,
                AllocSpace::Young => unreachable!("promotion target must be yard or handoff"),
            },
        }
    }

    /// Claim `value` for [`Arena::promote_region_root_to_target`]: move it if
    /// it is in the young region, take it out of its slot to be rewritten where
    /// it stands if `descend` says so, or leave it be.
    ///
    /// `descend` is true for a root and otherwise says whether the caller
    /// reported an owned in-place write. A value the promotion cannot move,
    /// reached INSIDE one it is moving or rewriting, cannot hold anything the
    /// frame made: a frame that promotes has no yard entries of its own and at
    /// most one handoff entry, and that one is the root, so such a child is
    /// older than the frame. Following it further only re-reads what has
    /// already settled — a loop that prepends re-read the whole chain on every
    /// step, and a function that wraps the term it was given in a new node and
    /// returns it read the entire term each time. The exception is an owned
    /// in-place vector write, which can leave a settled slot pointing at a
    /// value this boundary is about to move; the evacuating walk
    /// ([`Arena::take_local_value`]) draws the same line. With that write
    /// reported, the visit memo keeps a slot reached twice, or one that reaches
    /// itself, from being descended into twice.
    fn take_promote_value(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
        descend: bool,
        pending: &mut Vec<Taken<T, PromoteRewriteSlot>>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);

        if matches!(space, HeapSpace::Young)
            && raw_index >= mark
            && raw_index < self.young_entries.len() as u32
        {
            let relocation_slot = (raw_index - mark) as usize;
            let relocated_index = relocated[relocation_slot];
            if relocated_index != u32::MAX {
                return value.with_heap_index(relocated_index);
            }

            let (new_index, target_raw_index) = match target {
                AllocSpace::Yard => {
                    let raw = self.yard_entries.len();
                    self.yard_entries.push(ArenaEntry::Int(0));
                    (Self::encode_yard_index(raw as u32), raw)
                }
                AllocSpace::Handoff => {
                    let raw = self.handoff_entries.len();
                    self.handoff_entries.push(ArenaEntry::Int(0));
                    (Self::encode_handoff_index(raw as u32), raw)
                }
                AllocSpace::Young => unreachable!("promotion target must be yard or handoff"),
            };
            relocated[relocation_slot] = new_index;
            self.boundary_entries_moved += 1;
            self.note_peak_usage();
            let entry = core::mem::replace(
                &mut self.young_entries[raw_index as usize],
                ArenaEntry::Int(0),
            );
            return queue(
                pending,
                Taken {
                    value: value.with_heap_index(new_index),
                    entry,
                    slot: PromoteRewriteSlot::Promoted {
                        target,
                        raw_index: target_raw_index,
                    },
                },
            );
        }

        if !descend {
            return value;
        }

        let raw_index = raw_index as usize;
        let len = match space {
            HeapSpace::Young if raw_index >= mark as usize => return value,
            HeapSpace::Young => self.young_entries.len(),
            HeapSpace::Yard => self.yard_entries.len(),
            HeapSpace::Handoff => self.handoff_entries.len(),
            HeapSpace::Stable => self.stable_entries.len(),
        };
        if raw_index >= len
            || (self.rewrite_out_of_region_roots && !self.claim_inplace_visit(space, raw_index))
        {
            return value;
        }
        let entries = match space {
            HeapSpace::Young => &mut self.young_entries,
            HeapSpace::Yard => &mut self.yard_entries,
            HeapSpace::Handoff => &mut self.handoff_entries,
            HeapSpace::Stable => &mut self.stable_entries,
        };
        let entry = core::mem::replace(&mut entries[raw_index], ArenaEntry::Int(0));
        self.out_of_region_entries_read += 1;

        queue(
            pending,
            Taken {
                value,
                entry,
                slot: PromoteRewriteSlot::InPlace { space, raw_index },
            },
        )
    }

    /// Check if a NanValue references young heap at or after `mark`.
    #[inline(always)]
    fn value_needs_young_promotion(value: NanValue, mark: u32) -> bool {
        if let Some(index) = value.heap_index() {
            let (space, raw_index) = Self::decode_index(index);
            matches!(space, HeapSpace::Young) && raw_index >= mark
        } else {
            false
        }
    }

    fn promote_entry_to_target<F>(
        &mut self,
        mut entry: ArenaEntry<T>,
        mark: u32,
        rewrite: &mut F,
    ) -> ArenaEntry<T>
    where
        F: FnMut(&mut Arena<T>, NanValue) -> NanValue,
    {
        if let ArenaEntry::Map {
            scan_receipt,
            pending_scan_keys,
            ..
        } = &entry
            && pending_scan_keys.is_empty()
            && self.lane_receipt_can_skip(*scan_receipt)
        {
            return entry;
        }
        if let ArenaEntry::Vector { scan_receipt, .. } = &entry
            && self.lane_receipt_can_skip(*scan_receipt)
        {
            return entry;
        }
        // Fast path for bulk-data types: if no NanValue in this entry points
        // to young >= mark, skip the rewrite — move the entry as-is.
        // Only check types where the scan is cheap relative to the rewrite cost.
        match &entry {
            // Nothing in an all-immediate vector can point at young space, so
            // the arm below would reach the same answer — after reading every
            // element to get there. Same trade as the all-immediate map arm
            // further down, and not redundant with the escape in
            // `rewrite_entry_with`: a vector promoted to the long-lived heap
            // comes through here instead. No re-proof here either, for the
            // reason given at that escape.
            ArenaEntry::Vector {
                all_immediate: true,
                ..
            } => {
                return entry;
            }
            ArenaEntry::Vector { items, .. } | ArenaEntry::Tuple { items, .. }
                if !items.is_empty()
                    && !items
                        .iter()
                        .any(|v| Self::value_needs_young_promotion(*v, mark)) =>
            {
                let read = items.len();
                self.note_vector_elements_scanned(read);
                if let ArenaEntry::Vector { scan_receipt, .. } = &mut entry {
                    *scan_receipt = self.renewed_lane_receipt();
                }
                return entry;
            }
            // Nothing in an all-immediate table can point at young space, so the
            // guard below would reach the same answer — after reading every
            // entry to get there. This arm gets it for free, and it is not
            // redundant with the escape in `rewrite_map_with`: a map promoted to
            // the long-lived heap comes through here instead, and without this
            // arm a `Map<Int, Int>` is still read once in full on the way.
            ArenaEntry::Map {
                all_immediate: true,
                pending_scan_keys,
                ..
            } if pending_scan_keys.is_empty() => {
                return entry;
            }
            ArenaEntry::Map {
                map,
                pending_scan_keys,
                ..
            } if pending_scan_keys.is_empty()
                && !map.is_empty()
                && !map.values().any(|(k, v)| {
                    Self::value_needs_young_promotion(*k, mark)
                        || Self::value_needs_young_promotion(*v, mark)
                }) =>
            {
                // Reaching here means the guard read every entry to establish
                // that none of them moves. The entry itself is then moved as-is
                // — no rewrite, no duplication — but the read already cost what
                // a full walk costs, so it belongs in the same counter.
                let entries = map.len();
                self.note_map_entries_scanned(entries);
                if let ArenaEntry::Map { scan_receipt, .. } = &mut entry {
                    *scan_receipt = self.renewed_lane_receipt();
                }
                return entry;
            }
            _ => {}
        }
        self.rewrite_entry_with(entry, rewrite)
    }

    /// Copy `value`, and everything it reaches, into stable, on a work list
    /// rather than the Rust call stack.
    ///
    /// Each value the walk claims is copied out of its slot by
    /// [`Arena::take_stable_value`] and queued, and its children are claimed
    /// the same way when it comes off the queue, so a value of a recursive sum
    /// a million levels deep costs no Rust stack. A value is claimed in the
    /// relocation tables before it is queued, so one reached twice lands in one
    /// stable slot.
    fn promote_value_to_stable(
        &mut self,
        value: NanValue,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        pending: &mut Vec<Taken<T, usize>>,
    ) -> NanValue {
        let value = self.take_stable_value(
            value,
            relocated_young,
            relocated_yard,
            relocated_handoff,
            pending,
        );
        while let Some(current) = pending.pop() {
            let mut queue_child = |arena: &mut Arena<T>, child: NanValue| {
                arena.take_stable_value(
                    child,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    pending,
                )
            };
            let new_entry = self.rewrite_entry_with(current.entry, &mut queue_child);
            self.stable_entries[current.slot] = new_entry;
        }
        value
    }

    fn take_stable_value(
        &mut self,
        value: NanValue,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        pending: &mut Vec<Taken<T, usize>>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        let raw_index = raw_index as usize;

        let (entry, stable_index, rewritten_value) = match space {
            HeapSpace::Young => {
                let relocated_index = relocated_young[raw_index];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }
                let stable_index = self.stable_entries.len();
                let new_index = Self::encode_stable_index(stable_index as u32);
                relocated_young[raw_index] = new_index;
                let entry = self.young_entries[raw_index].clone();
                (entry, stable_index, value.with_heap_index(new_index))
            }
            HeapSpace::Yard => {
                let relocated_index = relocated_yard[raw_index];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }
                let stable_index = self.stable_entries.len();
                let new_index = Self::encode_stable_index(stable_index as u32);
                relocated_yard[raw_index] = new_index;
                let entry = self.yard_entries[raw_index].clone();
                (entry, stable_index, value.with_heap_index(new_index))
            }
            HeapSpace::Handoff => {
                let relocated_index = relocated_handoff[raw_index];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }
                let stable_index = self.stable_entries.len();
                let new_index = Self::encode_stable_index(stable_index as u32);
                relocated_handoff[raw_index] = new_index;
                let entry = self.handoff_entries[raw_index].clone();
                (entry, stable_index, value.with_heap_index(new_index))
            }
            // A stable value does not move, so its handle is returned as it
            // stands either way. What it may still hold is a reference into a
            // region this boundary is about to drop, put there by an owned
            // in-place write, and the descent has to happen INSIDE the
            // promotion rather than before or after it: the entry's own
            // references are promoted through the same `relocated_*` tables as
            // everything else, so a value reached both from here and from the
            // rest of the roots lands in one stable slot instead of two. The
            // memo keeps a slot reached twice, or one that reaches itself (which
            // only an in-place write can build), from being descended into
            // twice.
            HeapSpace::Stable if self.rewrite_out_of_region_roots => {
                if raw_index >= self.stable_entries.len()
                    || !self.claim_inplace_visit(HeapSpace::Stable, raw_index)
                {
                    return value;
                }
                let entry =
                    core::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0));
                return queue(
                    pending,
                    Taken {
                        value,
                        entry,
                        slot: raw_index,
                    },
                );
            }
            HeapSpace::Stable => return value,
        };

        self.stable_entries.push(ArenaEntry::Int(0));
        self.note_peak_usage();
        queue(
            pending,
            Taken {
                value: rewritten_value,
                entry,
                slot: stable_index,
            },
        )
    }

    fn relocate_yard_root(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if matches!(space, HeapSpace::Yard)
            && raw_index >= mark
            && raw_index < self.yard_entries.len() as u32
        {
            return self.relocate_yard_value(value, mark, relocated, compacted);
        }
        self.rewrite_yard_refs_in_place(space, raw_index, mark, relocated, compacted);
        value
    }

    fn relocate_yard_value(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if !matches!(space, HeapSpace::Yard) || raw_index < mark {
            return value;
        }

        let relocation_slot = (raw_index - mark) as usize;
        let relocated_index = relocated[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let compacted_pos = compacted.len() as u32;
        let new_index = Self::encode_yard_index(mark + compacted_pos);
        relocated[relocation_slot] = new_index;
        compacted.push(ArenaEntry::Int(0));

        let entry = core::mem::replace(
            &mut self.yard_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.relocate_yard_entry(entry, mark, relocated, compacted);
        compacted[compacted_pos as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn relocate_yard_entry(
        &mut self,
        entry: ArenaEntry<T>,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) -> ArenaEntry<T> {
        let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
            arena.relocate_yard_value(value, mark, relocated, compacted)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
    }

    fn rewrite_yard_refs_in_place(
        &mut self,
        space: HeapSpace,
        raw_index: u32,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) {
        let raw_index = raw_index as usize;
        match space {
            HeapSpace::Young => {
                if raw_index >= self.young_entries.len() {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.young_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_yard_entry(entry, mark, relocated, compacted);
                self.young_entries[raw_index] = new_entry;
            }
            HeapSpace::Yard => {
                if raw_index >= self.yard_entries.len() || raw_index >= mark as usize {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.yard_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_yard_entry(entry, mark, relocated, compacted);
                self.yard_entries[raw_index] = new_entry;
            }
            HeapSpace::Handoff => {
                if raw_index >= self.handoff_entries.len() {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.handoff_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_yard_entry(entry, mark, relocated, compacted);
                self.handoff_entries[raw_index] = new_entry;
            }
            HeapSpace::Stable => {
                if raw_index >= self.stable_entries.len() {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_yard_entry(entry, mark, relocated, compacted);
                self.stable_entries[raw_index] = new_entry;
            }
        }
    }

    fn rewrite_yard_entry(
        &mut self,
        entry: ArenaEntry<T>,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) -> ArenaEntry<T> {
        let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
            arena.relocate_yard_root(value, mark, relocated, compacted)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
    }

    /// Move `value`, and every stable value it reaches, into `compacted`, on a
    /// work list rather than the Rust call stack: a moved entry is queued, and
    /// rewritten when it comes off the queue.
    fn relocate_stable_root(
        &mut self,
        value: NanValue,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
        pending: &mut Vec<(ArenaEntry<T>, usize)>,
    ) -> NanValue {
        let value = self.take_relocated_stable(value, relocated, compacted, pending);
        while let Some((entry, compacted_pos)) = pending.pop() {
            let mut queue_child = |arena: &mut Arena<T>, child: NanValue| {
                arena.take_relocated_stable(child, relocated, compacted, pending)
            };
            let new_entry = self.rewrite_entry_with(entry, &mut queue_child);
            compacted[compacted_pos] = new_entry;
        }
        value
    }

    /// The new handle of a stable value: claimed in `relocated`, with its slot
    /// reserved in `compacted` and its entry queued on `pending`, the first
    /// time the collection reaches it.
    fn take_relocated_stable(
        &mut self,
        value: NanValue,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
        pending: &mut Vec<(ArenaEntry<T>, usize)>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if !matches!(space, HeapSpace::Stable) {
            return value;
        }

        let relocation_slot = raw_index as usize;
        let relocated_index = relocated[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let compacted_pos = compacted.len();
        let new_index = Self::encode_stable_index(compacted_pos as u32);
        relocated[relocation_slot] = new_index;
        compacted.push(ArenaEntry::Int(0));

        let entry = core::mem::replace(
            &mut self.stable_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        pending.push((entry, compacted_pos));
        value.with_heap_index(new_index)
    }
}
