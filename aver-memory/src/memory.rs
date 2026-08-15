use super::*;

/// Where the evacuation walk has to put a cons cell back once it has rewritten
/// it: into the compacted region it was moved to, or into the slot it never
/// left.
///
/// The second case is the out-of-region tail. It exists for the same reason
/// [`Arena::rewrite_evacuated_refs_in_place`] does — an in-place vector write
/// can leave a below-mark cell holding an above-mark index — and it mirrors
/// `PromoteRewriteSlot`, which the promotion walk has always had.
#[derive(Clone, Copy)]
enum LocalRewriteSlot {
    InPlace {
        space: HeapSpace,
        raw_index: usize,
    },
    Evacuated {
        target: AllocSpace,
        compacted_pos: u32,
    },
}

struct LocalTailEntry<T: ArenaTypes> {
    value: NanValue,
    entry: ArenaEntry<T>,
    slot: LocalRewriteSlot,
}

enum LocalTailStep<T: ArenaTypes> {
    Done(NanValue),
    Taken(LocalTailEntry<T>),
}

#[derive(Clone, Copy)]
enum PromoteRewriteSlot {
    InPlace {
        space: HeapSpace,
        raw_index: usize,
    },
    Promoted {
        target: AllocSpace,
        raw_index: usize,
    },
}

struct PromoteTailEntry<T: ArenaTypes> {
    value: NanValue,
    entry: ArenaEntry<T>,
    slot: PromoteRewriteSlot,
}

enum PromoteTailStep<T: ArenaTypes> {
    Done(NanValue),
    Taken(PromoteTailEntry<T>),
}

impl<T: ArenaTypes> Arena<T> {
    pub fn truncate_to(&mut self, mark: u32) {
        self.young_entries.truncate(mark as usize);
    }

    pub fn collect_young_from_roots(&mut self, mark: u32, roots: &mut [NanValue]) {
        if self.young_entries.len() <= mark as usize {
            return;
        }

        let mut relocated =
            Self::take_u32_scratch(&mut self.scratch_young, self.young_entries.len());
        let mut compacted = Vec::with_capacity(self.young_entries.len() - mark as usize);

        for root in roots {
            *root = self.relocate_young_root(*root, mark, &mut relocated, &mut compacted);
        }

        self.young_entries.truncate(mark as usize);
        self.young_entries.extend(compacted);
        Self::recycle_u32_scratch(&mut self.scratch_young, relocated);
    }

    pub fn truncate_yard_to(&mut self, mark: u32) {
        self.yard_entries.truncate(mark as usize);
    }

    pub fn truncate_handoff_to(&mut self, mark: u32) {
        self.handoff_entries.truncate(mark as usize);
    }

    /// `rewrite_out_of_region_roots` reports that the returning frame, or
    /// something it called, stored a value into an arena slot outside its own
    /// regions — see [`Arena::rewrite_out_of_region_roots`].
    pub fn evacuate_frame_to_yard(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        roots: &mut [NanValue],
        rewrite_out_of_region_roots: bool,
    ) -> (bool, bool) {
        self.evacuate_frame_locals(
            young_mark,
            yard_mark,
            handoff_mark,
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
        roots: &mut [NanValue],
        rewrite_out_of_region_roots: bool,
    ) -> (bool, bool) {
        self.evacuate_frame_locals(
            young_mark,
            yard_mark,
            handoff_mark,
            roots,
            AllocSpace::Handoff,
            rewrite_out_of_region_roots,
        )
    }

    fn evacuate_frame_locals(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        roots: &mut [NanValue],
        young_target: AllocSpace,
        rewrite_out_of_region_roots: bool,
    ) -> (bool, bool) {
        self.rewrite_out_of_region_roots = rewrite_out_of_region_roots;
        if rewrite_out_of_region_roots {
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

        for root in roots {
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
            );
        }

        self.young_entries.truncate(young_mark as usize);
        self.yard_entries.truncate(yard_mark as usize);
        self.yard_entries.extend(compacted_yard);
        self.handoff_entries.truncate(handoff_mark as usize);
        self.handoff_entries.extend(compacted_handoff);
        self.note_peak_usage();
        self.rewrite_out_of_region_roots = false;
        Self::recycle_u32_scratch(&mut self.scratch_young, relocated_young);
        Self::recycle_u32_scratch(&mut self.scratch_yard, relocated_yard);
        Self::recycle_u32_scratch(&mut self.scratch_handoff, relocated_handoff);

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
            ArenaEntry::Boxed(inner) => ArenaEntry::Boxed(rewrite(self, inner)),
            ArenaEntry::List(list) => ArenaEntry::List(self.rewrite_list_with(list, rewrite)),
            ArenaEntry::Tuple(mut items) => {
                for value in &mut items {
                    *value = rewrite(self, *value);
                }
                ArenaEntry::Tuple(items)
            }
            ArenaEntry::Vector(mut items) => {
                for value in &mut items {
                    *value = rewrite(self, *value);
                }
                ArenaEntry::Vector(items)
            }
            ArenaEntry::Map {
                map,
                all_immediate,
                held_elsewhere,
            } => self.rewrite_map_with(map, all_immediate, held_elsewhere, rewrite),
            ArenaEntry::Record {
                type_id,
                mut fields,
            } => {
                for value in &mut fields {
                    *value = rewrite(self, *value);
                }
                ArenaEntry::Record { type_id, fields }
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
    /// `held_elsewhere` travels with the entry untouched. A rewrite renames
    /// indices; it neither creates a reference nor discharges one, so whoever
    /// held this slot before holds it after.
    fn rewrite_map_with<F>(
        &mut self,
        mut map: T::Map,
        all_immediate: bool,
        held_elsewhere: bool,
        rewrite: &mut F,
    ) -> ArenaEntry<T>
    where
        F: FnMut(&mut Arena<T>, NanValue) -> NanValue,
    {
        if all_immediate {
            debug_assert!(
                crate::map_all_immediate(&map),
                "map marked all-immediate holds a heap-backed key or value; \
                 skipping it would leave a stale arena index behind"
            );
            return ArenaEntry::Map {
                map,
                all_immediate: true,
                held_elsewhere,
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
            held_elsewhere,
        }
    }

    #[inline(always)]
    fn rewrite_list_with<F>(&mut self, list: ArenaList, rewrite: &mut F) -> ArenaList
    where
        F: FnMut(&mut Arena<T>, NanValue) -> NanValue,
    {
        match list {
            ArenaList::Flat { items, start } => {
                let (items, start) = self.rewrite_list_body_with(items, start, rewrite);
                ArenaList::Flat { items, start }
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
                let (rest, start) = self.rewrite_list_body_with(rest, start, rewrite);
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
    /// Two escapes, cheapest first. A body built entirely out of immediates
    /// cannot hold anything the collector could relocate, so it is returned
    /// without being read at all. Otherwise the elements are rewritten, and the
    /// original body is still returned when every rewrite turned out to be the
    /// identity — which preserves both the `Rc` sharing and the O(1) `start`
    /// offset that `list_uncons` hands out. Only a body in which something
    /// really moved is rebuilt, and only from that element onwards.
    ///
    /// Without this, walking a list while a frame keeps allocating rebuilt the
    /// whole remaining input on every step: n^2/2 element copies for one
    /// traversal (issue #886).
    ///
    /// The two escapes do not buy the same thing, and the difference is the
    /// standing limit here. The first removes the *read* as well as the copy, so
    /// a list whose elements are all immediates costs nothing per collection.
    /// The second removes only the copy: deciding that nothing moved still means
    /// touching every remaining element, so a list of strings or records is
    /// still walked n^2/2 times over one traversal and its runtime is still
    /// quadratic — n = 128,000 takes 28 s where the same program over `List<Int>`
    /// takes 71 ms. `list_elements_copied` therefore reports linear on a program
    /// that is not; `list_elements_scanned` is the counter that shows it, and
    /// `copying_a_list_of_strings_still_reads_shared_bodies_quadratically` pins
    /// it. Closing that gap needs the collector to skip a live body it has
    /// already proved stable, which is a different change from this one.
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
        rewrite: &mut F,
    ) -> (Rc<ListBody>, usize)
    where
        F: FnMut(&mut Arena<T>, NanValue) -> NanValue,
    {
        debug_assert!(start <= body.len());
        if body.all_immediate() {
            debug_assert!(
                body.iter().all(|value| value.heap_index().is_none()),
                "list body marked all-immediate holds a heap-backed element; \
                 skipping it would leave a stale arena index behind"
            );
            return (body, start);
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
            None => (body, start),
            Some(out) => {
                self.list_elements_copied += out.len() as u64;
                (Rc::new(ListBody::new(out)), 0)
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
        let elements = self.list_to_vec_value(value);
        let flat = ArenaList::Flat {
            items: Rc::new(ListBody::new(elements)),
            start: 0,
        };
        let index = self.push(ArenaEntry::List(flat));
        NanValue::new_list(index)
    }

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
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        match space {
            HeapSpace::Young if self.is_young_index_in_region(index, young_mark) => self
                .evacuate_young_value(
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
                ),
            HeapSpace::Yard if self.is_yard_index_in_region(index, yard_mark) => self
                .evacuate_yard_value(
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
                ),
            HeapSpace::Handoff if self.is_handoff_index_in_region(index, handoff_mark) => self
                .evacuate_handoff_value(
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
                ),
            _ if self.rewrite_out_of_region_roots => {
                // The root lives outside the regions this boundary is about to
                // truncate, so its own handle does not change — but it may hold
                // references INTO those regions and those still have to be
                // rewritten before the truncate.
                self.rewrite_evacuated_refs_in_place(
                    space,
                    raw_index,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                );
                value
            }
            _ => value,
        }
    }

    /// Rewrite, in place, the frame-local references held by a value that lives
    /// OUTSIDE the returning frame's regions.
    ///
    /// Ordinarily an older slot cannot point into a younger region, so this
    /// descent would be pure cost — on a program carrying a long list of
    /// strings, a quadratic one. The VM's owned in-place vector write
    /// (`VECTOR_SET_OR_KEEP`) is the one operation that breaks the rule: it
    /// stores an arbitrary value into an EXISTING arena slot, so a vector held
    /// below the marks can end up holding a value allocated above them. Without
    /// this descent the truncate at the end of `evacuate_frame_locals` drops
    /// that value and the vector element silently reads back as whatever later
    /// reuses the slot. `rewrite_out_of_region_roots` is how the caller says
    /// such a write happened, so ordinary frames keep paying nothing.
    ///
    /// This mirrors `rewrite_young_refs_in_place` (relocation) and
    /// `rewrite_promoted_young_refs_in_place` (promotion), which already do the
    /// same descent for their own families — but NOT their idempotence, which
    /// they get for free rather than by construction. Both of those relocate by
    /// APPENDING (promotion) or into a young region they own outright, and in a
    /// world without in-place writes an old slot holds nothing they would move,
    /// so descending twice rewrites nothing twice. This one relocates into a
    /// region whose live entries the walk has not reached yet, so a second
    /// descent would read an already-rewritten index as an original one. The
    /// visit memo consulted by `take_out_of_region_entry` is what makes the
    /// walk idempotent here, and it doubles as the cycle guard: a value that
    /// reaches itself is claimed already and stops.
    #[allow(clippy::too_many_arguments)]
    fn rewrite_evacuated_refs_in_place(
        &mut self,
        space: HeapSpace,
        raw_index: u32,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry<T>>,
        compacted_handoff: &mut Vec<ArenaEntry<T>>,
    ) {
        let raw_index = raw_index as usize;
        let Some(entry) =
            self.take_out_of_region_entry(space, raw_index, young_mark, yard_mark, handoff_mark)
        else {
            return;
        };
        let new_entry = self.evacuate_local_entry(
            entry,
            young_mark,
            yard_mark,
            handoff_mark,
            young_target,
            relocated_young,
            relocated_yard,
            relocated_handoff,
            compacted_yard,
            compacted_handoff,
        );
        self.store_out_of_region_entry(space, raw_index, new_entry);
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_young_value(
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
    ) -> NanValue {
        let index = value.heap_index().expect("young value must be heap-backed");
        let (_, raw_index) = Self::decode_index(index);
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

        let entry = core::mem::replace(
            &mut self.young_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.evacuate_local_entry(
            entry,
            young_mark,
            yard_mark,
            handoff_mark,
            young_target,
            relocated_young,
            relocated_yard,
            relocated_handoff,
            compacted_yard,
            compacted_handoff,
        );
        Self::store_local_target_entry(
            young_target,
            compacted_pos,
            new_entry,
            compacted_yard,
            compacted_handoff,
        );
        value.with_heap_index(new_index)
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_yard_value(
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
    ) -> NanValue {
        let index = value.heap_index().expect("yard value must be heap-backed");
        let (_, raw_index) = Self::decode_index(index);
        let relocation_slot = (raw_index - yard_mark) as usize;
        let relocated_index = relocated_yard[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let target = match young_target {
            AllocSpace::Yard => AllocSpace::Yard,
            AllocSpace::Handoff => AllocSpace::Handoff,
            AllocSpace::Young => unreachable!("local evacuation must target yard or handoff"),
        };
        let (new_index, compacted_pos) = Self::allocate_local_target_slot(
            target,
            yard_mark,
            handoff_mark,
            compacted_yard,
            compacted_handoff,
        );
        relocated_yard[relocation_slot] = new_index;

        let entry = core::mem::replace(
            &mut self.yard_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.evacuate_local_entry(
            entry,
            young_mark,
            yard_mark,
            handoff_mark,
            young_target,
            relocated_young,
            relocated_yard,
            relocated_handoff,
            compacted_yard,
            compacted_handoff,
        );
        Self::store_local_target_entry(
            target,
            compacted_pos,
            new_entry,
            compacted_yard,
            compacted_handoff,
        );
        value.with_heap_index(new_index)
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_handoff_value(
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
    ) -> NanValue {
        let index = value
            .heap_index()
            .expect("handoff value must be heap-backed");
        let (_, raw_index) = Self::decode_index(index);
        let relocation_slot = (raw_index - handoff_mark) as usize;
        let relocated_index = relocated_handoff[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let target = match young_target {
            AllocSpace::Yard => AllocSpace::Yard,
            AllocSpace::Handoff => AllocSpace::Handoff,
            AllocSpace::Young => unreachable!("local evacuation must target yard or handoff"),
        };
        let (new_index, compacted_pos) = Self::allocate_local_target_slot(
            target,
            yard_mark,
            handoff_mark,
            compacted_yard,
            compacted_handoff,
        );
        relocated_handoff[relocation_slot] = new_index;

        let entry = core::mem::replace(
            &mut self.handoff_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.evacuate_local_entry(
            entry,
            young_mark,
            yard_mark,
            handoff_mark,
            young_target,
            relocated_young,
            relocated_yard,
            relocated_handoff,
            compacted_yard,
            compacted_handoff,
        );
        Self::store_local_target_entry(
            target,
            compacted_pos,
            new_entry,
            compacted_yard,
            compacted_handoff,
        );
        value.with_heap_index(new_index)
    }

    #[allow(clippy::too_many_arguments)]
    fn take_local_tail_value(
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
    ) -> LocalTailStep<T> {
        let Some(index) = value.heap_index() else {
            return LocalTailStep::Done(value);
        };
        let (space, raw_index) = Self::decode_index(index);

        match space {
            HeapSpace::Young if self.is_young_index_in_region(index, young_mark) => {
                let relocation_slot = (raw_index - young_mark) as usize;
                let relocated_index = relocated_young[relocation_slot];
                if relocated_index != u32::MAX {
                    return LocalTailStep::Done(value.with_heap_index(relocated_index));
                }

                let (new_index, compacted_pos) = Self::allocate_local_target_slot(
                    young_target,
                    yard_mark,
                    handoff_mark,
                    compacted_yard,
                    compacted_handoff,
                );
                relocated_young[relocation_slot] = new_index;
                let entry = core::mem::replace(
                    &mut self.young_entries[raw_index as usize],
                    ArenaEntry::Int(0),
                );
                LocalTailStep::Taken(LocalTailEntry {
                    value: value.with_heap_index(new_index),
                    entry,
                    slot: LocalRewriteSlot::Evacuated {
                        target: young_target,
                        compacted_pos,
                    },
                })
            }
            HeapSpace::Yard if self.is_yard_index_in_region(index, yard_mark) => {
                let relocation_slot = (raw_index - yard_mark) as usize;
                let relocated_index = relocated_yard[relocation_slot];
                if relocated_index != u32::MAX {
                    return LocalTailStep::Done(value.with_heap_index(relocated_index));
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
                let entry = core::mem::replace(
                    &mut self.yard_entries[raw_index as usize],
                    ArenaEntry::Int(0),
                );
                LocalTailStep::Taken(LocalTailEntry {
                    value: value.with_heap_index(new_index),
                    entry,
                    slot: LocalRewriteSlot::Evacuated {
                        target,
                        compacted_pos,
                    },
                })
            }
            HeapSpace::Handoff if self.is_handoff_index_in_region(index, handoff_mark) => {
                let relocation_slot = (raw_index - handoff_mark) as usize;
                let relocated_index = relocated_handoff[relocation_slot];
                if relocated_index != u32::MAX {
                    return LocalTailStep::Done(value.with_heap_index(relocated_index));
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
                let entry = core::mem::replace(
                    &mut self.handoff_entries[raw_index as usize],
                    ArenaEntry::Int(0),
                );
                LocalTailStep::Taken(LocalTailEntry {
                    value: value.with_heap_index(new_index),
                    entry,
                    slot: LocalRewriteSlot::Evacuated {
                        target,
                        compacted_pos,
                    },
                })
            }
            // An out-of-region cons cell. Its own handle does not change, but a
            // vector held further down the chain may have been written in place
            // and be pointing above the marks, so the walk has to continue
            // through it rather than stop here — which is what the promotion
            // walk (`take_promote_tail_value`) has always done. The memo inside
            // `take_out_of_region_entry` keeps a chain reached twice, or one
            // that loops back on itself, from being rewritten twice.
            _ if self.rewrite_out_of_region_roots => {
                match self.take_out_of_region_entry(
                    space,
                    raw_index as usize,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                ) {
                    Some(entry) => LocalTailStep::Taken(LocalTailEntry {
                        value,
                        entry,
                        slot: LocalRewriteSlot::InPlace {
                            space,
                            raw_index: raw_index as usize,
                        },
                    }),
                    None => LocalTailStep::Done(value),
                }
            }
            _ => LocalTailStep::Done(value),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_local_list(
        &mut self,
        list: ArenaList,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry<T>>,
        compacted_handoff: &mut Vec<ArenaEntry<T>>,
    ) -> ArenaList {
        match list {
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: self.evacuate_local_root(
                    head,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                ),
                tail: self.evacuate_local_prepend_tail_iterative(
                    tail,
                    young_mark,
                    yard_mark,
                    handoff_mark,
                    young_target,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                    compacted_yard,
                    compacted_handoff,
                ),
                len,
            },
            other => {
                let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
                    arena.evacuate_local_root(
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
                    )
                };
                self.rewrite_list_with(other, &mut rewrite)
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_local_prepend_tail_iterative(
        &mut self,
        tail: NanValue,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry<T>>,
        compacted_handoff: &mut Vec<ArenaEntry<T>>,
    ) -> NanValue {
        let mut current = match self.take_local_tail_value(
            tail,
            young_mark,
            yard_mark,
            handoff_mark,
            young_target,
            relocated_young,
            relocated_yard,
            relocated_handoff,
            compacted_yard,
            compacted_handoff,
        ) {
            LocalTailStep::Done(value) => return value,
            LocalTailStep::Taken(taken) => taken,
        };
        let first_value = current.value;

        loop {
            match current.entry {
                ArenaEntry::List(ArenaList::Prepend { head, tail, len }) => {
                    let rewritten_head = self.evacuate_local_root(
                        head,
                        young_mark,
                        yard_mark,
                        handoff_mark,
                        young_target,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                        compacted_yard,
                        compacted_handoff,
                    );
                    let next = self.take_local_tail_value(
                        tail,
                        young_mark,
                        yard_mark,
                        handoff_mark,
                        young_target,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                        compacted_yard,
                        compacted_handoff,
                    );
                    match next {
                        LocalTailStep::Done(rewritten_tail) => {
                            self.store_local_rewrite_slot(
                                current.slot,
                                ArenaEntry::List(ArenaList::Prepend {
                                    head: rewritten_head,
                                    tail: rewritten_tail,
                                    len,
                                }),
                                compacted_yard,
                                compacted_handoff,
                            );
                            return first_value;
                        }
                        LocalTailStep::Taken(next_entry) => {
                            self.store_local_rewrite_slot(
                                current.slot,
                                ArenaEntry::List(ArenaList::Prepend {
                                    head: rewritten_head,
                                    tail: next_entry.value,
                                    len,
                                }),
                                compacted_yard,
                                compacted_handoff,
                            );
                            current = next_entry;
                        }
                    }
                }
                other => {
                    let new_entry = self.evacuate_local_entry(
                        other,
                        young_mark,
                        yard_mark,
                        handoff_mark,
                        young_target,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                        compacted_yard,
                        compacted_handoff,
                    );
                    self.store_local_rewrite_slot(
                        current.slot,
                        new_entry,
                        compacted_yard,
                        compacted_handoff,
                    );
                    return first_value;
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn evacuate_local_entry(
        &mut self,
        entry: ArenaEntry<T>,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry<T>>,
        compacted_handoff: &mut Vec<ArenaEntry<T>>,
    ) -> ArenaEntry<T> {
        if let ArenaEntry::List(list) = entry {
            return ArenaEntry::List(self.evacuate_local_list(
                list,
                young_mark,
                yard_mark,
                handoff_mark,
                young_target,
                relocated_young,
                relocated_yard,
                relocated_handoff,
                compacted_yard,
                compacted_handoff,
            ));
        }

        let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
            arena.evacuate_local_root(
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
            )
        };
        self.rewrite_entry_with(entry, &mut rewrite)
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

        let relocation_slot = raw_index as usize;
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

    fn promote_region_root_to_target(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if matches!(space, HeapSpace::Young)
            && raw_index >= mark
            && raw_index < self.young_entries.len() as u32
        {
            return self.promote_value_to_target(value, mark, relocated, target);
        }
        self.rewrite_promoted_young_refs_in_place(space, raw_index, mark, relocated, target);
        value
    }

    pub fn promote_young_roots_to_yard(&mut self, mark: u32, roots: &mut [NanValue]) {
        if self.young_entries.len() <= mark as usize {
            return;
        }

        let mut relocated =
            Self::take_u32_scratch(&mut self.scratch_young, self.young_entries.len());

        for root in roots {
            *root = self.promote_region_root_to_yard(*root, mark, &mut relocated);
        }

        self.young_entries.truncate(mark as usize);
        Self::recycle_u32_scratch(&mut self.scratch_young, relocated);
    }

    pub fn promote_young_roots_to_handoff(&mut self, mark: u32, roots: &mut [NanValue]) {
        if self.young_entries.len() <= mark as usize {
            return;
        }

        let mut relocated =
            Self::take_u32_scratch(&mut self.scratch_young, self.young_entries.len());

        for root in roots {
            *root = self.promote_region_root_to_handoff(*root, mark, &mut relocated);
        }

        self.young_entries.truncate(mark as usize);
        Self::recycle_u32_scratch(&mut self.scratch_young, relocated);
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
        self.rewrite_out_of_region_roots = rewrite_out_of_region_roots;
        if rewrite_out_of_region_roots {
            self.begin_inplace_visit_epoch();
        }
        let mut relocated_young =
            Self::take_u32_scratch(&mut self.scratch_young, self.young_entries.len());
        let mut relocated_yard =
            Self::take_u32_scratch(&mut self.scratch_yard, self.yard_entries.len());
        let mut relocated_handoff =
            Self::take_u32_scratch(&mut self.scratch_handoff, self.handoff_entries.len());

        for root in roots {
            *root = self.promote_value_to_stable(
                *root,
                &mut relocated_young,
                &mut relocated_yard,
                &mut relocated_handoff,
            );
        }
        self.rewrite_out_of_region_roots = false;
        Self::recycle_u32_scratch(&mut self.scratch_young, relocated_young);
        Self::recycle_u32_scratch(&mut self.scratch_yard, relocated_yard);
        Self::recycle_u32_scratch(&mut self.scratch_handoff, relocated_handoff);
    }

    pub fn collect_yard_from_roots(&mut self, mark: u32, roots: &mut [NanValue]) {
        if self.yard_entries.len() <= mark as usize {
            return;
        }

        let mut relocated = Self::take_u32_scratch(&mut self.scratch_yard, self.yard_entries.len());
        let mut compacted = Vec::with_capacity(self.yard_entries.len() - mark as usize);

        for root in roots {
            *root = self.relocate_yard_root(*root, mark, &mut relocated, &mut compacted);
        }

        self.yard_entries.truncate(mark as usize);
        self.yard_entries.extend(compacted);
        Self::recycle_u32_scratch(&mut self.scratch_yard, relocated);
    }

    pub fn collect_stable_from_roots(&mut self, roots: &mut [NanValue]) {
        if self.stable_entries.is_empty() {
            return;
        }

        let mut relocated =
            Self::take_u32_scratch(&mut self.scratch_stable, self.stable_entries.len());
        let mut compacted = Vec::with_capacity(self.stable_entries.len());

        for root in roots {
            *root = self.relocate_stable_root(*root, &mut relocated, &mut compacted);
        }

        self.stable_entries = compacted;
        Self::recycle_u32_scratch(&mut self.scratch_stable, relocated);
    }

    fn promote_region_root_to_yard(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
    ) -> NanValue {
        self.promote_region_root_to_target(value, mark, relocated, AllocSpace::Yard)
    }

    fn promote_region_root_to_handoff(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
    ) -> NanValue {
        self.promote_region_root_to_target(value, mark, relocated, AllocSpace::Handoff)
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

    fn take_promote_tail_value(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
    ) -> PromoteTailStep<T> {
        let Some(index) = value.heap_index() else {
            return PromoteTailStep::Done(value);
        };
        let (space, raw_index) = Self::decode_index(index);

        if matches!(space, HeapSpace::Young) && raw_index >= mark {
            let relocation_slot = raw_index as usize;
            let relocated_index = relocated[relocation_slot];
            if relocated_index != u32::MAX {
                return PromoteTailStep::Done(value.with_heap_index(relocated_index));
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
            self.note_peak_usage();
            let entry = core::mem::replace(
                &mut self.young_entries[raw_index as usize],
                ArenaEntry::Int(0),
            );
            return PromoteTailStep::Taken(PromoteTailEntry {
                value: value.with_heap_index(new_index),
                entry,
                slot: PromoteRewriteSlot::Promoted {
                    target,
                    raw_index: target_raw_index,
                },
            });
        }

        let raw_index = raw_index as usize;
        let entry = match space {
            HeapSpace::Young => {
                if raw_index >= self.young_entries.len() || raw_index >= mark as usize {
                    return PromoteTailStep::Done(value);
                }
                core::mem::replace(&mut self.young_entries[raw_index], ArenaEntry::Int(0))
            }
            HeapSpace::Yard => {
                if raw_index >= self.yard_entries.len() {
                    return PromoteTailStep::Done(value);
                }
                core::mem::replace(&mut self.yard_entries[raw_index], ArenaEntry::Int(0))
            }
            HeapSpace::Handoff => {
                if raw_index >= self.handoff_entries.len() {
                    return PromoteTailStep::Done(value);
                }
                core::mem::replace(&mut self.handoff_entries[raw_index], ArenaEntry::Int(0))
            }
            HeapSpace::Stable => {
                if raw_index >= self.stable_entries.len() {
                    return PromoteTailStep::Done(value);
                }
                core::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0))
            }
        };

        PromoteTailStep::Taken(PromoteTailEntry {
            value,
            entry,
            slot: PromoteRewriteSlot::InPlace { space, raw_index },
        })
    }

    fn promote_list_young_refs_to_target(
        &mut self,
        list: ArenaList,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
    ) -> ArenaList {
        match list {
            ArenaList::Prepend { head, tail, len } => ArenaList::Prepend {
                head: self.promote_region_root_to_target(head, mark, relocated, target),
                tail: self.promote_prepend_tail_iterative(tail, mark, relocated, target),
                len,
            },
            other => {
                let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
                    arena.promote_region_root_to_target(value, mark, relocated, target)
                };
                self.rewrite_list_with(other, &mut rewrite)
            }
        }
    }

    fn promote_prepend_tail_iterative(
        &mut self,
        tail: NanValue,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
    ) -> NanValue {
        let mut current = match self.take_promote_tail_value(tail, mark, relocated, target) {
            PromoteTailStep::Done(value) => return value,
            PromoteTailStep::Taken(taken) => taken,
        };
        let first_value = current.value;

        loop {
            match current.entry {
                ArenaEntry::List(ArenaList::Prepend { head, tail, len }) => {
                    let rewritten_head =
                        self.promote_region_root_to_target(head, mark, relocated, target);
                    let next = self.take_promote_tail_value(tail, mark, relocated, target);
                    match next {
                        PromoteTailStep::Done(rewritten_tail) => {
                            self.store_promote_rewrite_slot(
                                current.slot,
                                ArenaEntry::List(ArenaList::Prepend {
                                    head: rewritten_head,
                                    tail: rewritten_tail,
                                    len,
                                }),
                            );
                            return first_value;
                        }
                        PromoteTailStep::Taken(next_entry) => {
                            self.store_promote_rewrite_slot(
                                current.slot,
                                ArenaEntry::List(ArenaList::Prepend {
                                    head: rewritten_head,
                                    tail: next_entry.value,
                                    len,
                                }),
                            );
                            current = next_entry;
                        }
                    }
                }
                other => {
                    let new_entry = match current.slot {
                        PromoteRewriteSlot::Promoted { .. } => {
                            self.promote_entry_to_target(other, mark, relocated, target)
                        }
                        PromoteRewriteSlot::InPlace { .. } => {
                            self.rewrite_promoted_young_entry(other, mark, relocated, target)
                        }
                    };
                    self.store_promote_rewrite_slot(current.slot, new_entry);
                    return first_value;
                }
            }
        }
    }

    fn rewrite_promoted_young_refs_in_place(
        &mut self,
        space: HeapSpace,
        raw_index: u32,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
    ) {
        let raw_index = raw_index as usize;
        match space {
            HeapSpace::Young => {
                if raw_index >= self.young_entries.len() || raw_index >= mark as usize {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.young_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_promoted_young_entry(entry, mark, relocated, target);
                self.young_entries[raw_index] = new_entry;
            }
            HeapSpace::Yard => {
                if raw_index >= self.yard_entries.len() {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.yard_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_promoted_young_entry(entry, mark, relocated, target);
                self.yard_entries[raw_index] = new_entry;
            }
            HeapSpace::Handoff => {
                if raw_index >= self.handoff_entries.len() {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.handoff_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_promoted_young_entry(entry, mark, relocated, target);
                self.handoff_entries[raw_index] = new_entry;
            }
            HeapSpace::Stable => {
                if raw_index >= self.stable_entries.len() {
                    return;
                }
                let entry =
                    core::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_promoted_young_entry(entry, mark, relocated, target);
                self.stable_entries[raw_index] = new_entry;
            }
        }
    }

    fn rewrite_promoted_young_entry(
        &mut self,
        entry: ArenaEntry<T>,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
    ) -> ArenaEntry<T> {
        if let ArenaEntry::List(list) = entry {
            return ArenaEntry::List(
                self.promote_list_young_refs_to_target(list, mark, relocated, target),
            );
        }

        let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
            arena.promote_region_root_to_target(value, mark, relocated, target)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
    }

    fn promote_value_to_target(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        if !matches!(space, HeapSpace::Young) || raw_index < mark {
            return value;
        }

        let relocation_slot = raw_index as usize;
        let relocated_index = relocated[relocation_slot];
        if relocated_index != u32::MAX {
            return value.with_heap_index(relocated_index);
        }

        let new_index = match target {
            AllocSpace::Yard => Self::encode_yard_index(self.yard_entries.len() as u32),
            AllocSpace::Handoff => Self::encode_handoff_index(self.handoff_entries.len() as u32),
            AllocSpace::Young => unreachable!("promotion target must be yard or handoff"),
        };
        relocated[relocation_slot] = new_index;
        match target {
            AllocSpace::Yard => self.yard_entries.push(ArenaEntry::Int(0)),
            AllocSpace::Handoff => self.handoff_entries.push(ArenaEntry::Int(0)),
            AllocSpace::Young => unreachable!(),
        }
        self.note_peak_usage();

        let entry = core::mem::replace(
            &mut self.young_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.promote_entry_to_target(entry, mark, relocated, target);
        match target {
            AllocSpace::Yard => {
                self.yard_entries[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
            }
            AllocSpace::Handoff => {
                self.handoff_entries[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
            }
            AllocSpace::Young => unreachable!(),
        }
        value.with_heap_index(new_index)
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

    fn promote_entry_to_target(
        &mut self,
        entry: ArenaEntry<T>,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
    ) -> ArenaEntry<T> {
        // Fast path for bulk-data types: if no NanValue in this entry points
        // to young >= mark, skip the rewrite — move the entry as-is.
        // Only check types where the scan is cheap relative to the rewrite cost.
        match &entry {
            ArenaEntry::Vector(items) | ArenaEntry::Tuple(items)
                if !items.is_empty()
                    && !items
                        .iter()
                        .any(|v| Self::value_needs_young_promotion(*v, mark)) =>
            {
                return entry;
            }
            // Nothing in an all-immediate table can point at young space, so the
            // guard below would reach the same answer — after reading every
            // entry to get there. This arm gets it for free, and it is not
            // redundant with the escape in `rewrite_map_with`: a map promoted to
            // the long-lived heap comes through here instead, and without this
            // arm a `Map<Int, Int>` is still read once in full on the way.
            ArenaEntry::Map {
                map,
                all_immediate: true,
                ..
            } => {
                debug_assert!(
                    crate::map_all_immediate(map),
                    "map marked all-immediate holds a heap-backed key or value; \
                     skipping it would leave a stale arena index behind"
                );
                return entry;
            }
            ArenaEntry::Map { map, .. }
                if !map.is_empty()
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
                return entry;
            }
            _ => {}
        }
        if let ArenaEntry::List(list) = entry {
            return ArenaEntry::List(
                self.promote_list_young_refs_to_target(list, mark, relocated, target),
            );
        }

        let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
            arena.promote_region_root_to_target(value, mark, relocated, target)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
    }

    fn promote_value_to_stable(
        &mut self,
        value: NanValue,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, raw_index) = Self::decode_index(index);
        match space {
            HeapSpace::Young => {
                let relocation_slot = raw_index as usize;
                let relocated_index = relocated_young[relocation_slot];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }

                let new_index = Self::encode_stable_index(self.stable_entries.len() as u32);
                relocated_young[relocation_slot] = new_index;
                self.stable_entries.push(ArenaEntry::Int(0));
                self.note_peak_usage();

                let entry = self.young_entries[raw_index as usize].clone();
                let new_entry = self.promote_entry_to_stable(
                    entry,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                );
                self.stable_entries[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
                value.with_heap_index(new_index)
            }
            HeapSpace::Yard => {
                let relocation_slot = raw_index as usize;
                let relocated_index = relocated_yard[relocation_slot];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }

                let new_index = Self::encode_stable_index(self.stable_entries.len() as u32);
                relocated_yard[relocation_slot] = new_index;
                self.stable_entries.push(ArenaEntry::Int(0));
                self.note_peak_usage();

                let entry = self.yard_entries[raw_index as usize].clone();
                let new_entry = self.promote_entry_to_stable(
                    entry,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                );
                self.stable_entries[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
                value.with_heap_index(new_index)
            }
            HeapSpace::Handoff => {
                let relocation_slot = raw_index as usize;
                let relocated_index = relocated_handoff[relocation_slot];
                if relocated_index != u32::MAX {
                    return value.with_heap_index(relocated_index);
                }

                let new_index = Self::encode_stable_index(self.stable_entries.len() as u32);
                relocated_handoff[relocation_slot] = new_index;
                self.stable_entries.push(ArenaEntry::Int(0));
                self.note_peak_usage();

                let entry = self.handoff_entries[raw_index as usize].clone();
                let new_entry = self.promote_entry_to_stable(
                    entry,
                    relocated_young,
                    relocated_yard,
                    relocated_handoff,
                );
                self.stable_entries[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
                value.with_heap_index(new_index)
            }
            // A stable root does not move, so its handle is returned as it
            // stands either way. What it may still hold is a reference into a
            // region this boundary is about to drop, put there by an owned
            // in-place write, and the descent has to happen INSIDE the
            // promotion rather than before or after it: the entry's own
            // references are promoted through the same `relocated_*` tables as
            // everything else, so a value reached both from here and from the
            // rest of the roots lands in one stable slot instead of two, and
            // the two references to it stay the same reference.
            //
            // The memo keeps a slot reached twice from being descended into
            // twice. A self-reaching stable value (which only an in-place
            // write can build) terminates even without it — the take below
            // leaves `Int(0)` in the slot, so a re-entry reads the
            // placeholder, exactly as the promotion sibling does — but the
            // memo makes the second visit a no-op by construction instead of
            // by meeting the placeholder.
            HeapSpace::Stable if self.rewrite_out_of_region_roots => {
                let raw_index = raw_index as usize;
                if raw_index < self.stable_entries.len()
                    && self.claim_inplace_visit(HeapSpace::Stable, raw_index)
                {
                    let entry =
                        core::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0));
                    let new_entry = self.promote_entry_to_stable(
                        entry,
                        relocated_young,
                        relocated_yard,
                        relocated_handoff,
                    );
                    self.stable_entries[raw_index] = new_entry;
                }
                value
            }
            HeapSpace::Stable => value,
        }
    }

    fn promote_entry_to_stable(
        &mut self,
        entry: ArenaEntry<T>,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
    ) -> ArenaEntry<T> {
        let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
            arena.promote_value_to_stable(value, relocated_young, relocated_yard, relocated_handoff)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
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

        let relocation_slot = raw_index as usize;
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

    fn relocate_stable_root(
        &mut self,
        value: NanValue,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        if !matches!(Self::decode_index(index).0, HeapSpace::Stable) {
            return value;
        }
        self.relocate_stable_value(value, relocated, compacted)
    }

    fn relocate_stable_value(
        &mut self,
        value: NanValue,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
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

        let new_index = Self::encode_stable_index(compacted.len() as u32);
        relocated[relocation_slot] = new_index;
        compacted.push(ArenaEntry::Int(0));

        let entry = core::mem::replace(
            &mut self.stable_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.relocate_stable_entry(entry, relocated, compacted);
        compacted[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn relocate_stable_entry(
        &mut self,
        entry: ArenaEntry<T>,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry<T>>,
    ) -> ArenaEntry<T> {
        let mut rewrite = |arena: &mut Arena<T>, value: NanValue| {
            arena.relocate_stable_value(value, relocated, compacted)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
    }
}
