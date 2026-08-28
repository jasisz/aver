use super::*;

impl<T: ArenaTypes> Arena<T> {
    pub fn new() -> Self {
        Arena {
            young_entries: Vec::with_capacity(256),
            yard_entries: Vec::with_capacity(64),
            handoff_entries: Vec::with_capacity(64),
            stable_entries: Vec::with_capacity(64),
            scratch_young: Vec::new(),
            scratch_yard: Vec::new(),
            scratch_handoff: Vec::new(),
            scratch_stable: Vec::new(),
            peak_usage: ArenaUsage::default(),
            alloc_space: AllocSpace::Young,
            lane_serial: 1,
            lane_clock_exhausted: false,
            active_lane_source_mark: INVALID_LANE_MARK,
            list_elements_copied: 0,
            list_elements_scanned: 0,
            list_elements_flattened: SharedCount::default(),
            map_entries_copied: 0,
            map_entries_scanned: 0,
            vector_elements_scanned: 0,
            out_of_region_entries_read: 0,
            rewrite_out_of_region_roots: false,
            holds_any_map: false,
            holds_any_vector: false,
            holds_any_record: false,
            inplace_visit_stamps: [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            inplace_visit_epoch: 0,
            type_keys: Vec::new(),
            type_names: Vec::new(),
            type_field_names: Vec::new(),
            type_variant_names: Vec::new(),
            type_variant_ctor_ids: Vec::new(),
            ctor_to_type_variant: Vec::new(),
            symbol_entries: Vec::new(),
            type_aliases: Vec::new(),
        }
    }

    /// Create a fresh Arena with only the static context (symbols, type metadata,
    /// stable constants) from this Arena. Dynamic runtime entries are empty.
    /// Used for independent product threads: each gets a clean Arena with just the
    /// compile-time context needed to execute functions and builtins.
    ///
    /// The copy / scan / flatten counters start at zero in the child, so a child
    /// counts its own work and nothing of its parent's. Nothing is lost by that:
    /// a child arena that runs an independent-product branch is handed back to
    /// the parent at the join, and [`Arena::absorb_copy_counters`] folds its
    /// totals in there. A child that never reaches a join — the base context
    /// built once and cloned per branch, or a branch that failed — takes its
    /// counts with it, which is the one gap in reading these numbers as a
    /// whole-program total.
    pub fn clone_static(&self) -> Self {
        Arena {
            young_entries: Vec::with_capacity(64),
            yard_entries: Vec::new(),
            handoff_entries: Vec::new(),
            stable_entries: self.stable_entries.clone(),
            scratch_young: Vec::new(),
            scratch_yard: Vec::new(),
            scratch_handoff: Vec::new(),
            scratch_stable: Vec::new(),
            peak_usage: ArenaUsage::default(),
            alloc_space: AllocSpace::Young,
            // Stable entries and their receipts are cloned by value, so the
            // child must continue the same authoritative clock. The mutable
            // list bodies themselves carry no arena-local state.
            lane_serial: self.lane_serial,
            lane_clock_exhausted: self.lane_clock_exhausted,
            active_lane_source_mark: INVALID_LANE_MARK,
            list_elements_copied: 0,
            list_elements_scanned: 0,
            list_elements_flattened: SharedCount::default(),
            map_entries_copied: 0,
            map_entries_scanned: 0,
            vector_elements_scanned: 0,
            out_of_region_entries_read: 0,
            rewrite_out_of_region_roots: false,
            // The stable entries come across whole, and a map among them is
            // still a map here.
            holds_any_map: self.holds_any_map,
            holds_any_vector: self.holds_any_vector,
            holds_any_record: self.holds_any_record,
            inplace_visit_stamps: [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            inplace_visit_epoch: 0,
            type_keys: self.type_keys.clone(),
            type_names: self.type_names.clone(),
            type_field_names: self.type_field_names.clone(),
            type_variant_names: self.type_variant_names.clone(),
            type_variant_ctor_ids: self.type_variant_ctor_ids.clone(),
            ctor_to_type_variant: self.ctor_to_type_variant.clone(),
            symbol_entries: self.symbol_entries.clone(),
            type_aliases: self.type_aliases.clone(),
        }
    }

    /// Snapshot the authoritative allocation-lane clock for a frame or a new
    /// immutable collection entry. Zero means receipt skips have failed closed.
    #[inline]
    pub fn lane_mark(&self) -> LaneMark {
        if self.lane_clock_exhausted {
            INVALID_LANE_MARK
        } else {
            self.lane_serial
        }
    }

    #[inline]
    pub fn lane_mark_is_valid(&self, mark: LaneMark) -> bool {
        !self.lane_clock_exhausted && mark != INVALID_LANE_MARK && mark <= self.lane_serial
    }

    /// Start a boundary that can change arena indices.
    ///
    /// Advancing before the walk makes the serial a logical history rather than
    /// a raw allocation count: even a truncate with no surviving pushes cannot
    /// disappear from the clock or reappear through index reuse.
    pub(crate) fn begin_lane_rewrite(&mut self, source_mark: LaneMark) {
        self.advance_lane_serial();
        self.active_lane_source_mark = if self.lane_mark_is_valid(source_mark) {
            source_mark
        } else {
            INVALID_LANE_MARK
        };
    }

    #[inline]
    pub(crate) fn finish_lane_rewrite(&mut self) {
        self.active_lane_source_mark = INVALID_LANE_MARK;
    }

    /// Whether `receipt` proves that this immutable collection was complete by
    /// the active frame's entry watermark. An in-place escape deliberately
    /// withholds the skip: the descent must keep walking through collections to
    /// reach the mutated vector below them.
    #[inline]
    pub(crate) fn lane_receipt_can_skip(&self, receipt: LaneMark) -> bool {
        if self.lane_clock_exhausted
            || self.rewrite_out_of_region_roots
            || receipt == INVALID_LANE_MARK
            || self.active_lane_source_mark == INVALID_LANE_MARK
        {
            return false;
        }
        self.lane_mark_is_valid(receipt) && receipt <= self.active_lane_source_mark
    }

    /// Receipt written after a full scan/rewrite. A proved skip keeps the older
    /// receipt: no reference changed, so making the proof younger would only
    /// lose information at a nested boundary.
    #[inline]
    pub(crate) fn renewed_lane_receipt(&self) -> LaneMark {
        self.lane_mark()
    }

    #[inline]
    fn advance_lane_serial(&mut self) {
        if self.lane_clock_exhausted {
            return;
        }
        if self.lane_serial == LaneMark::MAX {
            self.lane_clock_exhausted = true;
        } else {
            self.lane_serial += 1;
        }
    }

    #[inline]
    fn note_lane_push(&mut self) {
        self.advance_lane_serial();
    }

    /// Deep-import a NanValue from `source` arena into `self`.
    /// Inline values (int, float, bool, unit, none, empty containers) are returned as-is.
    /// Heap-referenced values are recursively copied into `self` with new indices.
    pub fn deep_import(&mut self, value: NanValue, source: &Arena<T>) -> NanValue {
        // Not NaN-boxed = plain float, return as-is
        if !value.is_nan_boxed() {
            return value;
        }
        // Check if it has a heap index — if not, it's inline
        let heap_idx = match value.heap_index() {
            Some(idx) => idx,
            None => return value, // inline int, bool, unit, none, empty list/map/etc
        };

        let entry = source.get(heap_idx).clone();
        match entry {
            ArenaEntry::Int(i) => NanValue::new_int(i, self),
            ArenaEntry::BigInt(b) => NanValue::new_big_int(*b, self),
            ArenaEntry::String(s) => {
                let idx = self.push(ArenaEntry::String(s));
                NanValue::new_string(idx)
            }
            ArenaEntry::Tuple(items) => {
                let imported: Vec<NanValue> =
                    items.iter().map(|v| self.deep_import(*v, source)).collect();
                let idx = self.push_tuple(imported);
                NanValue::new_tuple(idx)
            }
            ArenaEntry::List(_) => {
                // Flatten list and re-import as a fresh flat list
                let flat = source.list_to_vec_value(value);
                let imported: Vec<NanValue> =
                    flat.iter().map(|v| self.deep_import(*v, source)).collect();
                if imported.is_empty() {
                    NanValue::EMPTY_LIST
                } else {
                    let rc_items = Rc::new(ListBody::new(imported));
                    let scan_receipt = self.lane_mark();
                    let idx = self.push(ArenaEntry::List(ArenaList::Flat {
                        items: rc_items,
                        start: 0,
                        scan_receipt,
                    }));
                    NanValue::new_list(idx)
                }
            }
            ArenaEntry::Map { map, .. } => {
                let mut new_map = T::Map::new();
                let mut table = new_map.table_id();
                for (hash, (k, v)) in map.iter() {
                    let ik = self.deep_import(*k, source);
                    let iv = self.deep_import(*v, source);
                    let entries_before = new_map.len();
                    // The map under construction is a local of this call, so it
                    // goes in through the owned path. `insert` — which is what
                    // this line used to be — takes `&self` and therefore has to
                    // preserve the table it is handed, rebuilding the whole
                    // thing once per entry for nobody's benefit: n^2/2 entry
                    // duplications to carry a map across an arena boundary.
                    // The bump is driven by `table_id`, so it counts a rebuild
                    // that happened rather than inferring one from the method
                    // name.
                    new_map = new_map.insert_owned(*hash, (ik, iv));
                    let table_after = new_map.table_id();
                    if table_after != table {
                        self.note_map_entries_copied(entries_before);
                        table = table_after;
                    }
                }
                // The imported keys and values are freshly pushed into this
                // arena, so the source map's flag says nothing about them:
                // `push_map` decides the new one from what actually went in.
                let idx = self.push_map(new_map);
                NanValue::new_map(idx)
            }
            ArenaEntry::Vector { items, .. } => {
                let imported: Vec<NanValue> =
                    items.iter().map(|v| self.deep_import(*v, source)).collect();
                // Through `push_vector`, so imported children that are
                // themselves collections get their held-elsewhere mark.
                let idx = self.push_vector(imported);
                NanValue::new_vector(idx)
            }
            ArenaEntry::Record {
                type_id, fields, ..
            } => {
                let imported: Vec<NanValue> = fields
                    .iter()
                    .map(|v| self.deep_import(*v, source))
                    .collect();
                let idx = self.push(ArenaEntry::Record {
                    type_id,
                    fields: imported,
                    holder_count: 0,
                });
                NanValue::new_record(idx)
            }
            ArenaEntry::Variant {
                type_id,
                variant_id,
                fields,
            } => {
                let imported: Vec<NanValue> = fields
                    .iter()
                    .map(|v| self.deep_import(*v, source))
                    .collect();
                let idx = self.push(ArenaEntry::Variant {
                    type_id,
                    variant_id,
                    fields: imported,
                });
                NanValue::new_variant(idx)
            }
            ArenaEntry::Boxed(inner) => {
                let imported = self.deep_import(inner, source);
                let idx = self.push(ArenaEntry::Boxed(imported));
                NanValue::encode(value.tag(), ARENA_REF_BIT | (idx as u64))
            }
            // Fn/Builtin/Namespace — should not appear in independent product results
            ArenaEntry::Fn(_) | ArenaEntry::Builtin(_) | ArenaEntry::Namespace { .. } => value,
        }
    }

    /// Record that something other than the caller's own handle now holds
    /// `value`'s slot.
    ///
    /// A no-op for anything but a heap-backed map, vector, or record — the
    /// entries the runtime may empty in place. The consumer calls this
    /// for the roots the arena cannot see for itself — a global it stores
    /// into, a constant table it hands the program, a value it writes into an
    /// entry that already exists. The `holder_count` fields carry the full list
    /// of marking sites for maps, vectors, and records.
    #[inline(always)]
    pub fn note_held_elsewhere(&mut self, value: NanValue) {
        if value.is_heap_map()
            || (value.is_vector() && !value.is_empty_vector_immediate())
            || value.is_record()
        {
            self.mark_held_elsewhere(value.arena_index());
        }
    }

    /// The write behind [`Arena::note_held_elsewhere`], kept out of it.
    ///
    /// Indexing a space can panic, and a caller that inlines the panic path
    /// stops being inlined itself — the lesson the incremental counter cost the
    /// last time. The test is what belongs at the call site; the write is rare
    /// enough to reach through a call.
    #[inline(never)]
    fn mark_held_elsewhere(&mut self, index: u32) {
        match self.get_mut(index) {
            ArenaEntry::Map { holder_count, .. }
            | ArenaEntry::Vector { holder_count, .. }
            | ArenaEntry::Record { holder_count, .. } => {
                *holder_count = holder_count.saturating_add(1);
            }
            _ => {}
        }
    }

    /// Remove one registered holder after a uniquely-owned aggregate has
    /// physically stopped holding `value`.
    #[inline(always)]
    fn release_held_elsewhere(&mut self, value: NanValue) {
        if value.is_heap_map()
            || (value.is_vector() && !value.is_empty_vector_immediate())
            || value.is_record()
        {
            self.release_holder(value.arena_index());
        }
    }

    #[inline(never)]
    fn release_holder(&mut self, index: u32) {
        match self.get_mut(index) {
            ArenaEntry::Map { holder_count, .. }
            | ArenaEntry::Vector { holder_count, .. }
            | ArenaEntry::Record { holder_count, .. } => {
                // Saturation is sticky. Once exact cardinality is lost, the
                // safe answer is "held" forever rather than a future false 0.
                if *holder_count == u32::MAX {
                    return;
                }
                debug_assert!(
                    *holder_count > 0,
                    "removed aggregate field had no registered holder"
                );
                *holder_count = holder_count.saturating_sub(1);
            }
            _ => {}
        }
    }

    /// Mark every map, vector, or record `entry` holds DIRECTLY, so the entry
    /// counts as a holder of those slots from here on.
    ///
    /// Direct is enough: a takeable value reachable from the arena at all is a
    /// direct child of SOME entry, and that entry came through here. What this
    /// does not do is walk a map's own table or a vector entry's own elements —
    /// that would put an `O(size)` pass in front of the one insert that is
    /// `O(1)`. The builders mark their own children instead: [`Arena::push_map`]
    /// and the owned map insert for tables, [`Arena::push_vector`] and the
    /// owned `Vector.set` (which marks the one element it stores) for vectors.
    ///
    /// Reached only when the arena has stored a map, vector, or record at all —
    /// [`Arena::push`] reads that off the same discriminant it was already
    /// reading — and deliberately not inlined into it, so a program that never
    /// builds a collection carries none of this in its allocation path.
    #[inline(never)]
    fn note_entry_holds_takeable(&mut self, entry: &ArenaEntry<T>) {
        match entry {
            ArenaEntry::Boxed(value) => self.note_held_elsewhere(*value),
            ArenaEntry::Tuple(items) => {
                for value in items {
                    self.note_held_elsewhere(*value);
                }
            }
            ArenaEntry::Record { fields, .. } | ArenaEntry::Variant { fields, .. } => {
                for value in fields {
                    self.note_held_elsewhere(*value);
                }
            }
            ArenaEntry::List(list) => match list {
                ArenaList::Flat { items, start, .. } => {
                    if items.holds_takeable() {
                        for value in &items[(*start).min(items.len())..] {
                            self.note_held_elsewhere(*value);
                        }
                    }
                }
                ArenaList::Prepend { head, tail, .. } => {
                    self.note_held_elsewhere(*head);
                    self.note_held_elsewhere(*tail);
                }
                ArenaList::Concat { left, right, .. } => {
                    self.note_held_elsewhere(*left);
                    self.note_held_elsewhere(*right);
                }
                ArenaList::Segments {
                    current,
                    rest,
                    start,
                    ..
                } => {
                    self.note_held_elsewhere(*current);
                    if rest.holds_takeable() {
                        for value in &rest[(*start).min(rest.len())..] {
                            self.note_held_elsewhere(*value);
                        }
                    }
                }
            },
            // A map's own table and a vector entry's own elements are marked
            // by whoever built them; see the doc above. Everything else holds
            // no `NanValue` at all.
            ArenaEntry::Map { .. }
            | ArenaEntry::Vector { .. }
            | ArenaEntry::Int(_)
            | ArenaEntry::BigInt(_)
            | ArenaEntry::String(_)
            | ArenaEntry::Fn(_)
            | ArenaEntry::Builtin(_) => {}
            ArenaEntry::Namespace { members, .. } => {
                for (_, value) in members {
                    self.note_held_elsewhere(*value);
                }
            }
        }
    }

    #[inline]
    pub fn push(&mut self, entry: ArenaEntry<T>) -> u32 {
        // The ownership bookkeeping rides on the discriminant read this match
        // was already doing, so an allocation that has nothing to do with a
        // takeable map, vector, or record pays one guard check for it.
        //
        // A takeable entry is where its arena-wide guard comes from: nothing
        // can carry such an index before the value itself exists. Everything
        // else asks the guards first and walks its children only if the answer
        // could be yes.
        match &entry {
            ArenaEntry::Fn(_) | ArenaEntry::Builtin(_) | ArenaEntry::Namespace { .. } => {}
            ArenaEntry::Map { .. } => {
                // Read before writing: a map fold comes through here once per
                // step and the flag is already set on all but the first, so a
                // predictable branch is cheaper than a store into the arena
                // header every time.
                if !self.holds_any_map {
                    self.holds_any_map = true;
                }
                return self.push_heap(entry);
            }
            // Like the map arm: the entry's OWN elements are the builder's
            // to mark ([`Arena::push_vector`] walks them; the owned
            // `Vector.set` marks the one element it stores), so this arm
            // only records that vector indices exist now.
            ArenaEntry::Vector { .. } => {
                if !self.holds_any_vector {
                    self.holds_any_vector = true;
                }
                return self.push_heap(entry);
            }
            ArenaEntry::Record { .. } => {
                if self.holds_any_map || self.holds_any_vector || self.holds_any_record {
                    self.note_entry_holds_takeable(&entry);
                }
                self.holds_any_record = true;
                return self.push_heap(entry);
            }
            // Nothing here holds a `NanValue`, so there is nothing to mark and
            // no reason to leave the match to find that out. A map fold
            // allocates one of these per step for its keys.
            ArenaEntry::Int(_) | ArenaEntry::BigInt(_) | ArenaEntry::String(_) => {
                return self.push_heap(entry);
            }
            _ => {
                if self.holds_any_map || self.holds_any_vector || self.holds_any_record {
                    self.note_entry_holds_takeable(&entry);
                }
                return self.push_heap(entry);
            }
        }
        match entry {
            ArenaEntry::Fn(f) => self.push_symbol(ArenaSymbol::Fn(f)),
            ArenaEntry::Builtin(name) => self.push_symbol(ArenaSymbol::Builtin(name)),
            ArenaEntry::Namespace { name, members } => {
                // A namespace's members are values the symbol table holds for
                // the life of the program, which is exactly what the flag means.
                // Registered once at start-up, so the walk costs nothing worth
                // guarding.
                for (_, value) in &members {
                    self.note_held_elsewhere(*value);
                }
                self.push_symbol(ArenaSymbol::Namespace { name, members })
            }
            _ => unreachable!("non-symbol entry already returned above"),
        }
    }

    /// Put an entry in the space the arena is currently allocating into.
    #[inline]
    fn push_heap(&mut self, entry: ArenaEntry<T>) -> u32 {
        let index = match self.alloc_space {
            AllocSpace::Young => {
                let idx = self.young_entries.len() as u32;
                self.young_entries.push(entry);
                self.note_peak_usage();
                Self::encode_index(HeapSpace::Young, idx)
            }
            AllocSpace::Yard => {
                let idx = self.yard_entries.len() as u32;
                self.yard_entries.push(entry);
                self.note_peak_usage();
                Self::encode_index(HeapSpace::Yard, idx)
            }
            AllocSpace::Handoff => {
                let idx = self.handoff_entries.len() as u32;
                self.handoff_entries.push(entry);
                self.note_peak_usage();
                Self::encode_index(HeapSpace::Handoff, idx)
            }
        };
        self.note_lane_push();
        index
    }

    #[inline]
    pub fn push_symbol(&mut self, symbol: ArenaSymbol<T>) -> u32 {
        let idx = self.symbol_entries.len() as u32;
        self.symbol_entries.push(symbol);
        idx
    }

    #[inline]
    pub fn get(&self, index: u32) -> &ArenaEntry<T> {
        let (space, raw_index) = Self::decode_index(index);
        match space {
            HeapSpace::Young => &self.young_entries[raw_index as usize],
            HeapSpace::Yard => &self.yard_entries[raw_index as usize],
            HeapSpace::Handoff => &self.handoff_entries[raw_index as usize],
            HeapSpace::Stable => &self.stable_entries[raw_index as usize],
        }
    }

    #[inline]
    pub fn get_mut(&mut self, index: u32) -> &mut ArenaEntry<T> {
        let (space, raw_index) = Self::decode_index(index);
        match space {
            HeapSpace::Young => &mut self.young_entries[raw_index as usize],
            HeapSpace::Yard => &mut self.yard_entries[raw_index as usize],
            HeapSpace::Handoff => &mut self.handoff_entries[raw_index as usize],
            HeapSpace::Stable => &mut self.stable_entries[raw_index as usize],
        }
    }

    #[inline]
    pub(crate) fn encode_index(space: HeapSpace, index: u32) -> u32 {
        ((space as u32) << HEAP_SPACE_SHIFT) | index
    }

    #[inline]
    pub(crate) fn encode_yard_index(index: u32) -> u32 {
        Self::encode_index(HeapSpace::Yard, index)
    }

    #[inline]
    pub(crate) fn encode_stable_index(index: u32) -> u32 {
        Self::encode_index(HeapSpace::Stable, index)
    }

    #[inline]
    pub(crate) fn encode_handoff_index(index: u32) -> u32 {
        Self::encode_index(HeapSpace::Handoff, index)
    }

    #[inline]
    pub(crate) fn decode_index(index: u32) -> (HeapSpace, u32) {
        let space = match (index & HEAP_SPACE_MASK_U32) >> HEAP_SPACE_SHIFT {
            0 => HeapSpace::Young,
            1 => HeapSpace::Yard,
            2 => HeapSpace::Handoff,
            3 => HeapSpace::Stable,
            _ => unreachable!("invalid heap space bits"),
        };
        (space, index & HEAP_INDEX_MASK_U32)
    }

    #[inline]
    pub fn is_stable_index(index: u32) -> bool {
        matches!(Self::decode_index(index).0, HeapSpace::Stable)
    }

    #[inline]
    pub fn is_yard_index_in_region(&self, index: u32, mark: u32) -> bool {
        let (space, raw_index) = Self::decode_index(index);
        matches!(space, HeapSpace::Yard)
            && raw_index >= mark
            && raw_index < self.yard_entries.len() as u32
    }

    #[inline]
    pub fn is_handoff_index_in_region(&self, index: u32, mark: u32) -> bool {
        let (space, raw_index) = Self::decode_index(index);
        matches!(space, HeapSpace::Handoff)
            && raw_index >= mark
            && raw_index < self.handoff_entries.len() as u32
    }

    #[inline]
    pub fn is_young_index_in_region(&self, index: u32, mark: u32) -> bool {
        let (space, raw_index) = Self::decode_index(index);
        matches!(space, HeapSpace::Young)
            && raw_index >= mark
            && raw_index < self.young_entries.len() as u32
    }

    #[inline]
    pub fn young_len(&self) -> usize {
        self.young_entries.len()
    }

    #[inline]
    pub fn yard_len(&self) -> usize {
        self.yard_entries.len()
    }

    #[inline]
    pub fn handoff_len(&self) -> usize {
        self.handoff_entries.len()
    }

    #[inline]
    pub fn stable_len(&self) -> usize {
        self.stable_entries.len()
    }

    /// List elements the collector has copied into fresh shared bodies so far.
    ///
    /// This is the structural stand-in for a stopwatch: a collector that keeps
    /// list sharing intact grows this linearly in the elements that actually
    /// relocate, so a quadratic rebuild is visible as a number rather than as a
    /// flaky wall-clock reading.
    ///
    /// It measures memory traffic, not time. Read it together with
    /// [`Arena::list_elements_scanned`] — a traversal can copy nothing and still
    /// be quadratic, because deciding that nothing moved means reading every
    /// element.
    #[inline]
    pub fn list_elements_copied(&self) -> u64 {
        self.list_elements_copied
    }

    /// List elements the collector has read while deciding whether a shared body
    /// needs rebuilding.
    ///
    /// This is the counter that tracks *time*. A body built entirely out of
    /// immediate values is skipped without being read and never reaches this
    /// number; any body holding a heap index is walked in full on every
    /// collection that sees it, whether or not anything turns out to move. So a
    /// traversal over a list of integers leaves this at zero, while the same
    /// traversal over a list of strings or records grows it quadratically —
    /// which is a real, measurable difference in what the two cost.
    #[inline]
    pub fn list_elements_scanned(&self) -> u64 {
        self.list_elements_scanned
    }

    /// List elements flattened out of a shared body into a fresh vector by
    /// [`Arena::list_to_vec`], the whole-list walk behind every builtin that
    /// needs all the elements at once.
    ///
    /// Read it as "no builtin walked a whole list here". A builtin that answers
    /// about a bounded prefix — `List.take`, `List.drop` — adds nothing to it,
    /// so a program stepping through a list with those builtins keeps this at
    /// zero however long the list is; one that flattens the list on every step
    /// makes it quadratic, which is the shape of a walk that used to cost the
    /// whole list per step.
    ///
    /// It measures work done for a builtin, not by the collector: the two
    /// counters above are the collector's, and nothing here is in them.
    #[inline]
    pub fn list_elements_flattened(&self) -> u64 {
        self.list_elements_flattened.get()
    }

    /// Record that `elements` were flattened out of a shared body into a fresh
    /// vector.
    #[inline]
    pub(crate) fn note_list_elements_flattened(&self, elements: usize) {
        self.list_elements_flattened.add(elements as u64);
    }

    /// Map entries duplicated while a map table was rebuilt rather than written
    /// into, the map counterpart of [`Arena::list_elements_copied`].
    ///
    /// A `Map.set` whose target is still reachable has to preserve it, so it
    /// duplicates the whole storage; so does a builder that rebuilds its table
    /// once per entry, and so does [`Arena::deep_import`] carrying a map into
    /// another arena. Any of those makes n^2/2 entries out of one pass, and
    /// consuming the table instead leaves this at zero.
    ///
    /// Read it as "no map table was rebuilt entry by entry", and nothing wider.
    /// It says nothing about time — see [`Arena::map_entries_scanned`] — and it
    /// does not cover the collector, which duplicates a live map's table of its
    /// own accord when it promotes it to the stable space. Zero here is
    /// therefore not a claim that no map storage was copied anywhere.
    ///
    /// It is per-arena, and a child arena starts at zero
    /// ([`Arena::clone_static`]); [`Arena::absorb_copy_counters`] is what brings
    /// a child's total back to its parent at the join.
    #[inline]
    pub fn map_entries_copied(&self) -> u64 {
        self.map_entries_copied
    }

    /// Record that `entries` map entries were duplicated to preserve a target
    /// the caller was not allowed to consume.
    #[inline]
    pub fn note_map_entries_copied(&mut self, entries: usize) {
        self.map_entries_copied += entries as u64;
    }

    /// Add `child`'s copy / scan totals to this arena's.
    ///
    /// A child arena counts from zero ([`Arena::clone_static`]), so work an
    /// independent-product branch did would otherwise disappear when its arena
    /// is dropped — and the deep import that fills a child with its arguments
    /// is exactly the kind of work these counters exist to see. Called at the
    /// join, where the branch hands its arena back.
    #[inline]
    pub fn absorb_copy_counters(&mut self, child: &Arena<T>) {
        self.list_elements_copied += child.list_elements_copied;
        self.list_elements_scanned += child.list_elements_scanned;
        self.list_elements_flattened
            .add(child.list_elements_flattened.get());
        self.map_entries_copied += child.map_entries_copied;
        self.map_entries_scanned += child.map_entries_scanned;
        self.vector_elements_scanned += child.vector_elements_scanned;
        self.out_of_region_entries_read += child.out_of_region_entries_read;
    }

    /// Map entries the collector has read while deciding whether a live map
    /// needs rewriting, the map counterpart of
    /// [`Arena::list_elements_scanned`].
    ///
    /// This is the counter that tracks *time*. Maps now carry the same
    /// all-immediate escape lists have: a table whose keys and values are all
    /// immediate is returned unread by the collector and adds nothing here, so
    /// a live `Map<Int, Int>` threaded through a fold reads as 0. A map
    /// holding anything heap-backed is still read entry by entry on every
    /// collection that sees it, and threading one through a fold grows this
    /// quadratically — the residual cost left over once the duplication above
    /// is gone.
    ///
    /// Reads are missed on purpose in two places: the pre-scan in the
    /// promotion fast path stops at the first entry that has to move (only
    /// runs that scan a map in full are counted), and the all-immediate arms
    /// in `rewrite_map_with` / `promote_entry_to_target` return without
    /// reading anything at all. Both undercount, never the reverse.
    #[inline]
    pub fn map_entries_scanned(&self) -> u64 {
        self.map_entries_scanned
    }

    /// Record that `entries` map entries were read by a collection.
    #[inline]
    pub fn note_map_entries_scanned(&mut self, entries: usize) {
        self.map_entries_scanned += entries as u64;
    }

    /// Vector and tuple elements the collector has *read* while deciding
    /// whether a bulk entry needs rewriting, the vector counterpart of
    /// [`Arena::list_elements_scanned`] and [`Arena::map_entries_scanned`].
    ///
    /// This is the counter that tracks *time* for bulk entries. A vector built
    /// entirely out of immediate values carries the `all_immediate` flag, is
    /// returned unread, and adds nothing here; one holding a heap index is
    /// walked in full on every collection that sees it. So a `Vector<Int>`
    /// carried through a loop reads as 0, while a vector of strings grows this
    /// with every collection.
    ///
    /// The reason it exists: the hidden `String.Index` behind `String.charAt`
    /// is a vector of byte offsets threaded through every step of an indexed
    /// loop. Reading it once per step is what made an indexed string walk
    /// quadratic while the number of characters read stayed linear, and no
    /// opcode or allocation count could see that.
    #[inline]
    pub fn vector_elements_scanned(&self) -> u64 {
        self.vector_elements_scanned
    }

    /// Record that `elements` vector or tuple elements were read by a
    /// collection.
    #[inline]
    pub fn note_vector_elements_scanned(&mut self, elements: usize) {
        self.vector_elements_scanned += elements as u64;
    }

    /// Arena entries a promotion has *read* although they lie outside the
    /// region it is allowed to move — memory that had already settled.
    ///
    /// The three counters above ask what one entry costs to read. This one asks
    /// a different question: how many entries were read that could not possibly
    /// need it. An older slot cannot hold a younger index, so a promotion owes
    /// nothing to anything already settled, and the only thing that changes
    /// that is the runtime's owned in-place vector write — which the caller
    /// reports, and which is the only condition under which this number is
    /// allowed to grow.
    ///
    /// The reason it exists: a list built by prepending is a chain of cells,
    /// one arena entry each. A promotion that follows that chain from the top
    /// reads all of it, and a loop that both reads characters and prepends
    /// collects every few steps, so the whole chain was re-read a linear number
    /// of times for a quadratic program. Nothing was copied and no other
    /// counter moved, which is why the earlier collection-cost guards could not
    /// see it.
    #[inline]
    pub fn out_of_region_entries_read(&self) -> u64 {
        self.out_of_region_entries_read
    }

    #[inline]
    pub fn usage(&self) -> ArenaUsage {
        ArenaUsage {
            young: self.young_entries.len(),
            yard: self.yard_entries.len(),
            handoff: self.handoff_entries.len(),
            stable: self.stable_entries.len(),
        }
    }

    #[inline]
    pub fn peak_usage(&self) -> ArenaUsage {
        self.peak_usage
    }

    #[inline]
    pub(crate) fn note_peak_usage(&mut self) {
        let usage = self.usage();
        self.peak_usage.young = self.peak_usage.young.max(usage.young);
        self.peak_usage.yard = self.peak_usage.yard.max(usage.yard);
        self.peak_usage.handoff = self.peak_usage.handoff.max(usage.handoff);
        self.peak_usage.stable = self.peak_usage.stable.max(usage.stable);
    }

    /// Open a fresh memo for one descent into out-of-region slots.
    ///
    /// Every stamp written by an earlier boundary is now stale by construction,
    /// because nothing equals the new epoch. Wrapping past `u32::MAX` would let
    /// a stamp from 4 billion boundaries ago read as current, so that one case
    /// clears the arrays for real.
    pub(crate) fn begin_inplace_visit_epoch(&mut self) {
        self.inplace_visit_epoch = self.inplace_visit_epoch.wrapping_add(1);
        if self.inplace_visit_epoch == 0 {
            for stamps in &mut self.inplace_visit_stamps {
                stamps.clear();
            }
            self.inplace_visit_epoch = 1;
        }
    }

    /// Claim a slot for the descent now running: `true` the first time this
    /// boundary reaches it, `false` every time after.
    pub(crate) fn claim_inplace_visit(&mut self, space: HeapSpace, raw_index: usize) -> bool {
        let epoch = self.inplace_visit_epoch;
        let stamps = &mut self.inplace_visit_stamps[space as usize];
        if stamps.len() <= raw_index {
            stamps.resize(raw_index + 1, 0);
        }
        if stamps[raw_index] == epoch {
            return false;
        }
        stamps[raw_index] = epoch;
        true
    }

    #[inline]
    pub(crate) fn take_u32_scratch(slot: &mut Vec<u32>, len: usize) -> Vec<u32> {
        let mut scratch = core::mem::take(slot);
        scratch.clear();
        scratch.resize(len, u32::MAX);
        scratch
    }

    #[inline]
    pub(crate) fn recycle_u32_scratch(slot: &mut Vec<u32>, mut scratch: Vec<u32>) {
        scratch.clear();
        *slot = scratch;
    }

    #[inline]
    pub fn is_frame_local_index(
        &self,
        index: u32,
        arena_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
    ) -> bool {
        self.is_young_index_in_region(index, arena_mark)
            || self.is_yard_index_in_region(index, yard_mark)
            || self.is_handoff_index_in_region(index, handoff_mark)
    }

    pub fn with_alloc_space<R>(
        &mut self,
        space: AllocSpace,
        f: impl FnOnce(&mut Arena<T>) -> R,
    ) -> R {
        let prev = self.alloc_space;
        self.alloc_space = space;
        let out = f(self);
        self.alloc_space = prev;
        out
    }

    /// Push an entry, inheriting the allocation space from a source value.
    /// If the source lives in yard or handoff, the result is placed there too,
    /// avoiding a pointless young→yard/handoff promotion later.
    pub fn push_inheriting_source_space(&mut self, entry: ArenaEntry<T>, source: NanValue) -> u32 {
        if let Some(index) = source.heap_index() {
            let (space, _) = Self::decode_index(index);
            let target = match space {
                HeapSpace::Yard => Some(AllocSpace::Yard),
                HeapSpace::Handoff => Some(AllocSpace::Handoff),
                _ => None,
            };
            if let Some(target) = target {
                let prev = self.alloc_space;
                self.alloc_space = target;
                let idx = self.push(entry);
                self.alloc_space = prev;
                return idx;
            }
        }
        self.push(entry)
    }

    // -- Typed push helpers ------------------------------------------------

    pub fn push_i64(&mut self, val: i64) -> u32 {
        self.push(ArenaEntry::Int(val))
    }
    /// Store an arbitrary-precision integer, upholding the canonical-form
    /// invariant: a payload that fits `i64` is stored as `ArenaEntry::Int`
    /// (so `int_ref_at` reports it as `Small`), never as a `BigInt` slot.
    /// Only a genuinely out-of-`i64`-range value allocates a `BigInt`. The
    /// `i64`-fitting case should normally be demoted to the inline NaN-box one
    /// layer up (`NanValue::new_big_int`); this is the backstop.
    pub fn push_bigint(&mut self, val: num_bigint::BigInt) -> u32 {
        match i64::try_from(&val) {
            Ok(n) => self.push(ArenaEntry::Int(n)),
            Err(_) => self.push(ArenaEntry::BigInt(Box::new(val))),
        }
    }
    pub fn push_string(&mut self, s: &str) -> u32 {
        self.push(ArenaEntry::String(Rc::from(s)))
    }
    pub fn push_boxed(&mut self, val: NanValue) -> u32 {
        self.push(ArenaEntry::Boxed(val))
    }
    pub fn push_record(&mut self, type_id: u32, fields: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::Record {
            type_id,
            fields,
            holder_count: 0,
        })
    }
    pub fn push_variant(&mut self, type_id: u32, variant_id: u16, fields: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::Variant {
            type_id,
            variant_id,
            fields,
        })
    }
    pub fn push_list(&mut self, items: Vec<NanValue>) -> u32 {
        let scan_receipt = self.lane_mark();
        self.push(ArenaEntry::List(ArenaList::Flat {
            items: Rc::new(ListBody::new(items)),
            start: 0,
            scan_receipt,
        }))
    }
    /// Store a map, proving its `all_immediate` flag from the table itself and
    /// marking every map it holds as held by this entry.
    ///
    /// This reads every entry, which is why it is the entry point for builders
    /// that already pay for the whole table anyway — `Map.set` on a target it
    /// has to preserve, `Map.remove`, `Map.fromList`, `deep_import`, value
    /// conversion. Both jobs come out of that one pass. The one caller that must
    /// not come through here is the owned `Map.set`, which is O(1) per insert
    /// and derives the flag from the map it consumed and marks the single pair
    /// it added; see `set_nv_owned`.
    pub fn push_map(&mut self, map: T::Map) -> u32 {
        let mut all_immediate = true;
        let mut held: Option<Vec<NanValue>> = None;
        for (key, value) in map.values() {
            all_immediate &= key.is_immediate() && value.is_immediate();
            for child in [*key, *value] {
                if child.is_heap_map()
                    || (child.is_vector() && !child.is_empty_vector_immediate())
                    || child.is_record()
                {
                    held.get_or_insert_default().push(child);
                }
            }
        }
        for child in held.into_iter().flatten() {
            self.note_held_elsewhere(child);
        }
        let scan_receipt = self.lane_mark();
        self.push(ArenaEntry::Map {
            map,
            all_immediate,
            scan_receipt,
            pending_scan_keys: Vec::new(),
            holder_count: 0,
        })
    }
    pub fn push_tuple(&mut self, items: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::Tuple(items))
    }
    /// Store a vector, marking every map or vector it holds as held by this
    /// entry — the vector spelling of [`Arena::push_map`]'s marking pass, made
    /// in the walk this builder can afford because its callers already paid
    /// `O(n)` to build `items`. The one builder that must NOT come through
    /// here is the owned `Vector.set`, which is `O(1)` per write and marks the
    /// single element it stores itself (see `vec_set_nv_owned`).
    pub fn push_vector(&mut self, items: Vec<NanValue>) -> u32 {
        // One pass answers both questions the entry is built with: which
        // children this vector now holds, and whether the collector will ever
        // have anything to rewrite in it. The second is free here, so the
        // builder proves it once instead of the collector re-deciding it on
        // every pass over the entry.
        let marking = self.holds_any_map || self.holds_any_vector || self.holds_any_record;
        let mut all_immediate = true;
        let mut held: Option<Vec<NanValue>> = None;
        for child in &items {
            if child.heap_index().is_some() {
                all_immediate = false;
            }
            if marking
                && (child.is_heap_map()
                    || (child.is_vector() && !child.is_empty_vector_immediate())
                    || child.is_record())
            {
                held.get_or_insert_default().push(*child);
            }
        }
        for child in held.into_iter().flatten() {
            self.note_held_elsewhere(child);
        }
        self.push(ArenaEntry::Vector {
            items,
            all_immediate,
            holder_count: 0,
        })
    }
    pub fn push_fn(&mut self, f: Rc<T::Fn>) -> u32 {
        self.push_symbol(ArenaSymbol::Fn(f))
    }
    pub fn push_builtin(&mut self, name: &str) -> u32 {
        self.push_symbol(ArenaSymbol::Builtin(Rc::from(name)))
    }
    pub fn push_nullary_variant_symbol(&mut self, ctor_id: u32) -> u32 {
        self.push_symbol(ArenaSymbol::NullaryVariant { ctor_id })
    }

    // -- Typed getters -----------------------------------------------------

    pub fn get_i64(&self, index: u32) -> i64 {
        match self.get(index) {
            ArenaEntry::Int(i) => *i,
            _ => panic!("Arena: expected Int at {}", index),
        }
    }
    /// Borrow the out-of-range integer at `index`.
    pub fn get_bigint(&self, index: u32) -> &num_bigint::BigInt {
        match self.get(index) {
            ArenaEntry::BigInt(b) => b,
            _ => panic!("Arena: expected BigInt at {}", index),
        }
    }
    /// Discriminate an arena-stored integer (i64-overflow vs ℤ-overflow)
    /// without materializing it. The runtime side reconstructs the canonical
    /// `AverInt` from this.
    pub fn int_ref_at(&self, index: u32) -> ArenaIntRef<'_> {
        match self.get(index) {
            ArenaEntry::Int(i) => ArenaIntRef::Small(*i),
            ArenaEntry::BigInt(b) => ArenaIntRef::Big(b),
            other => panic!(
                "Arena: expected an integer at {} but found {:?}",
                index, other
            ),
        }
    }
    pub fn get_string(&self, index: u32) -> &str {
        match self.get(index) {
            ArenaEntry::String(s) => s,
            other => panic!("Arena: expected String at {} but found {:?}", index, other),
        }
    }
    pub fn get_string_value(&self, value: NanValue) -> NanString<'_> {
        if let Some(s) = value.small_string() {
            s
        } else {
            NanString::Borrowed(self.get_string(value.arena_index()))
        }
    }
    pub fn get_boxed(&self, index: u32) -> NanValue {
        match self.get(index) {
            ArenaEntry::Boxed(v) => *v,
            _ => panic!("Arena: expected Boxed at {}", index),
        }
    }
    pub fn get_record(&self, index: u32) -> (u32, &[NanValue]) {
        match self.get(index) {
            ArenaEntry::Record {
                type_id, fields, ..
            } => (*type_id, fields),
            _ => panic!("Arena: expected Record at {}", index),
        }
    }

    /// Whether a root or another arena entry has registered a reference to
    /// this record. Operand-stack aliases are deliberately not represented
    /// here; the VM can inspect those directly after popping its operand.
    pub fn record_is_held_elsewhere(&self, record: NanValue) -> bool {
        match self.get(record.arena_index()) {
            ArenaEntry::Record { holder_count, .. } => *holder_count != 0,
            _ => false,
        }
    }

    /// Remove and return one field from a record whose uniqueness the caller
    /// has already established. Removing the direct reference releases exactly
    /// one registered holder on a nested map, vector, or record. The hot path
    /// checks the incremental holder count only; the exhaustive heap-scan
    /// counterpart belongs in tests because it is `O(live heap)`.
    pub fn take_record_field(&mut self, record: NanValue, field_idx: usize) -> NanValue {
        debug_assert!(!self.record_is_held_elsewhere(record));
        let value = match self.get_mut(record.arena_index()) {
            ArenaEntry::Record { fields, .. } => {
                std::mem::replace(&mut fields[field_idx], NanValue::UNIT)
            }
            _ => panic!("Arena: expected Record at {}", record.arena_index()),
        };
        self.release_held_elsewhere(value);
        value
    }
    pub fn get_variant(&self, index: u32) -> (u32, u16, &[NanValue]) {
        match self.get(index) {
            ArenaEntry::Variant {
                type_id,
                variant_id,
                fields,
            } => (*type_id, *variant_id, fields),
            other => panic!("Arena: expected Variant at {} but found {:?}", index, other),
        }
    }
    pub fn get_list(&self, index: u32) -> &ArenaList {
        match self.get(index) {
            ArenaEntry::List(items) => items,
            _ => panic!("Arena: expected List at {}", index),
        }
    }
    pub fn get_tuple(&self, index: u32) -> &[NanValue] {
        match self.get(index) {
            ArenaEntry::Tuple(items) => items,
            _ => panic!("Arena: expected Tuple at {}", index),
        }
    }
    pub fn get_vector(&self, index: u32) -> &[NanValue] {
        match self.get(index) {
            ArenaEntry::Vector { items, .. } => items,
            _ => panic!("Arena: expected Vector at {}", index),
        }
    }
    /// Hand out the elements for arbitrary mutation.
    ///
    /// The caller can store anything, so the all-immediate promise cannot
    /// survive this: it is cleared here rather than at every call site, because
    /// `false` costs a walk and a wrong `true` costs a stale arena index. A
    /// caller that does know what it writes should use
    /// [`Arena::vector_store_in_place`] instead and keep the promise.
    pub fn get_vector_mut(&mut self, index: u32) -> &mut Vec<NanValue> {
        match self.get_mut(index) {
            ArenaEntry::Vector {
                items,
                all_immediate,
                ..
            } => {
                *all_immediate = false;
                items
            }
            _ => panic!("Arena: expected Vector at {}", index),
        }
    }

    /// Write one element in place, keeping the all-immediate promise exact.
    ///
    /// The in-place `Vector.set`'s write, and the reason
    /// [`Arena::get_vector_mut`] can afford to be conservative: a loop writing
    /// integers into a vector it owns keeps the collector's escape, and one
    /// writing a heap value gives it up at the first such write.
    ///
    /// Returns whether `position` was in range.
    pub fn vector_store_in_place(&mut self, index: u32, position: usize, value: NanValue) -> bool {
        match self.get_mut(index) {
            ArenaEntry::Vector {
                items,
                all_immediate,
                ..
            } => {
                if position >= items.len() {
                    return false;
                }
                items[position] = value;
                if value.heap_index().is_some() {
                    *all_immediate = false;
                }
                true
            }
            _ => panic!("Arena: expected Vector at {}", index),
        }
    }

    /// Whether the vector at `index` still promises to hold no arena index.
    ///
    /// Read by the owned `Vector.set`, which takes the elements out and pushes
    /// them back as a new entry and so has to carry the promise across by hand.
    pub fn vector_all_immediate(&self, index: u32) -> bool {
        match self.get(index) {
            ArenaEntry::Vector { all_immediate, .. } => *all_immediate,
            _ => panic!("Arena: expected Vector at {}", index),
        }
    }
    /// Everything a caller deciding whether to empty this vector's slot needs
    /// from the arena, out of one lookup — the vector spelling of
    /// [`Arena::map_slot`]. `None` for anything that is not a heap-backed
    /// vector, the empty-vector immediate among them.
    #[inline]
    pub fn vector_slot(&self, value: NanValue) -> Option<VectorSlot> {
        if !value.is_vector() || value.is_empty_vector_immediate() {
            return None;
        }
        match self.get(value.arena_index()) {
            ArenaEntry::Vector {
                items,
                holder_count,
                ..
            } => Some(VectorSlot {
                held_elsewhere: *holder_count != 0,
                len: items.len(),
            }),
            _ => panic!("Arena: expected Vector at {}", value.arena_index()),
        }
    }
    pub fn vector_ref_value(&self, value: NanValue) -> &[NanValue] {
        if value.is_empty_vector_immediate() {
            return &[];
        }
        self.get_vector(value.arena_index())
    }
    pub fn clone_vector_value(&self, value: NanValue) -> Vec<NanValue> {
        if value.is_empty_vector_immediate() {
            Vec::new()
        } else {
            self.get_vector(value.arena_index()).to_vec()
        }
    }
    /// Take ownership of a vector, replacing the arena slot with an empty vec.
    ///
    /// `holder_count` is deliberately left alone, for the same reason
    /// [`Arena::take_map_value`] leaves the map's: emptying the slot does not
    /// discharge a holder, and a second take over the same slot must be
    /// refused for the same reason the first one should have been.
    pub fn take_vector_value(&mut self, value: NanValue) -> Vec<NanValue> {
        if value.is_empty_vector_immediate() {
            Vec::new()
        } else {
            let index = value.arena_index();
            std::mem::take(self.get_vector_mut(index))
        }
    }
    pub fn get_map(&self, index: u32) -> &T::Map {
        match self.get(index) {
            ArenaEntry::Map { map, .. } => map,
            _ => panic!("Arena: expected Map at {}", index),
        }
    }
    pub fn map_ref_value(&self, map: NanValue) -> &T::Map {
        if map.is_empty_map_immediate() {
            // Use a leaked singleton for the empty map reference.
            // This avoids thread_local! which is not available in no_std.
            use core::sync::atomic::{AtomicPtr, Ordering as AtomicOrdering};
            static EMPTY_MAP_PTR: AtomicPtr<()> = AtomicPtr::new(core::ptr::null_mut());

            let ptr = EMPTY_MAP_PTR.load(AtomicOrdering::Acquire);
            if !ptr.is_null() {
                // SAFETY: ptr was allocated via Box::leak and is valid for 'static
                return unsafe { &*(ptr as *const T::Map) };
            }
            let boxed = alloc::boxed::Box::new(T::Map::new());
            let leaked: &'static T::Map = alloc::boxed::Box::leak(boxed);
            let new_ptr = leaked as *const T::Map as *mut ();
            // If another thread raced us, that's fine — we just leak one extra allocation
            EMPTY_MAP_PTR.store(new_ptr, AtomicOrdering::Release);
            leaked
        } else {
            self.get_map(map.arena_index())
        }
    }
    pub fn clone_map_value(&self, map: NanValue) -> T::Map {
        if map.is_empty_map_immediate() {
            T::Map::new()
        } else {
            self.get_map(map.arena_index()).clone()
        }
    }
    /// Whether the map behind `map` is known to hold only immediate keys and
    /// values — the `all_immediate` flag of its entry, read without touching
    /// the table.
    ///
    /// The empty map has no entry to read and nothing in it that could move.
    pub fn map_all_immediate_value(&self, map: NanValue) -> bool {
        if map.is_empty_map_immediate() {
            return true;
        }
        match self.get(map.arena_index()) {
            ArenaEntry::Map { all_immediate, .. } => *all_immediate,
            _ => panic!("Arena: expected Map at {}", map.arena_index()),
        }
    }

    /// Upper bound on the logical age of the bulk references stored in `map`.
    /// Entries named by the map's remembered set may be newer; callers that
    /// transfer this receipt must transfer those exceptions with it. Empty maps
    /// carry no references and therefore need no receipt.
    pub fn map_scan_receipt_value(&self, map: NanValue) -> LaneMark {
        if map.is_empty_map_immediate() {
            INVALID_LANE_MARK
        } else {
            match self.get(map.arena_index()) {
                ArenaEntry::Map { scan_receipt, .. } => *scan_receipt,
                _ => panic!("Arena: expected Map at {}", map.arena_index()),
            }
        }
    }
    /// Whether any entry directly holds `index`, searched rather than recorded.
    ///
    /// The from-scratch counterpart to `held_elsewhere`: that flag is written
    /// where a reference is made, this reads every entry in every space and asks
    /// the question outright. Nothing in the interpreter's hot path may call
    /// this — it is `O(live heap)` — and its whole value is that it shares no
    /// bookkeeping with the flag it is there to check.
    ///
    /// The entry AT `index` is skipped: a map is not a holder of itself, and
    /// Aver values are acyclic, so no entry can reach `index` except by holding
    /// it directly or by holding something that does — and that something is
    /// itself an entry this search visits.
    ///
    /// The symbol table is searched too. Nothing puts a map in a namespace
    /// today, so that loop finds nothing — but [`Arena::push`] marks namespace
    /// members all the same, and a check that covered fewer roots than the
    /// marking would be agreeing with the marking by construction over the
    /// difference.
    pub fn any_entry_holds_slot(&self, index: u32) -> bool {
        for space in [
            HeapSpace::Young,
            HeapSpace::Yard,
            HeapSpace::Handoff,
            HeapSpace::Stable,
        ] {
            let entries = match space {
                HeapSpace::Young => &self.young_entries,
                HeapSpace::Yard => &self.yard_entries,
                HeapSpace::Handoff => &self.handoff_entries,
                HeapSpace::Stable => &self.stable_entries,
            };
            for (raw, entry) in entries.iter().enumerate() {
                if Self::encode_index(space, raw as u32) == index {
                    continue;
                }
                if entry_holds_slot(entry, index) {
                    return true;
                }
            }
        }
        self.symbol_entries.iter().any(|symbol| match symbol {
            ArenaSymbol::Namespace { members, .. } => members
                .iter()
                .any(|(_, value)| value.heap_index() == Some(index)),
            ArenaSymbol::Fn(_) | ArenaSymbol::Builtin(_) | ArenaSymbol::NullaryVariant { .. } => {
                false
            }
        })
    }

    /// Everything a caller deciding whether to empty this map's slot needs from
    /// the arena, out of one lookup.
    ///
    /// `None` for anything that is not a heap-backed map — the empty map among
    /// them, which is an immediate with no slot to empty and nothing to copy.
    #[inline]
    pub fn map_slot(&self, map: NanValue) -> Option<MapSlot> {
        if !map.is_heap_map() {
            return None;
        }
        match self.get(map.arena_index()) {
            ArenaEntry::Map {
                map, holder_count, ..
            } => Some(MapSlot {
                held_elsewhere: *holder_count != 0,
                entries: map.len(),
            }),
            _ => panic!("Arena: expected Map at {}", map.arena_index()),
        }
    }
    /// Take ownership of a map and its relocation state, replacing it with an
    /// empty map in the arena.
    /// Use when the caller is the sole owner (reuse analysis says `owned = true`).
    /// Avoids the O(n) clone — the original slot becomes empty.
    ///
    /// Read [`Arena::map_all_immediate_value`] before this if the caller needs
    /// the taken map's flag. The scan receipt and remembered keys travel in the
    /// returned [`TakenMap`]; the emptied slot is reset to a fully scanned empty
    /// table.
    ///
    /// `held_elsewhere` is deliberately left alone. Emptying the table does not
    /// discharge a holder — whoever held the slot still holds it, and now reads
    /// an empty map through it. The flag stays true so that a second take over
    /// the same slot is refused for the same reason the first one should have
    /// been.
    pub fn take_map_value(&mut self, map: NanValue) -> TakenMap<T::Map> {
        if map.is_empty_map_immediate() {
            TakenMap {
                map: T::Map::new(),
                scan_receipt: INVALID_LANE_MARK,
                pending_scan_keys: Vec::new(),
            }
        } else {
            let index = map.arena_index();
            let empty_receipt = self.lane_mark();
            match self.get_mut(index) {
                ArenaEntry::Map {
                    map,
                    all_immediate,
                    scan_receipt,
                    pending_scan_keys,
                    ..
                } => {
                    *all_immediate = true;
                    let taken = TakenMap {
                        map: core::mem::replace(map, T::Map::new()),
                        scan_receipt: *scan_receipt,
                        pending_scan_keys: core::mem::take(pending_scan_keys),
                    };
                    *scan_receipt = empty_receipt;
                    taken
                }
                _ => panic!("Arena: expected Map at {}", index),
            }
        }
    }
    pub fn get_fn(&self, index: u32) -> &T::Fn {
        match &self.symbol_entries[index as usize] {
            ArenaSymbol::Fn(f) => f,
            _ => panic!("Arena: expected Fn symbol at {}", index),
        }
    }
    pub fn get_fn_rc(&self, index: u32) -> &Rc<T::Fn> {
        match &self.symbol_entries[index as usize] {
            ArenaSymbol::Fn(f) => f,
            _ => panic!("Arena: expected Fn symbol at {}", index),
        }
    }
    pub fn get_builtin(&self, index: u32) -> &str {
        match &self.symbol_entries[index as usize] {
            ArenaSymbol::Builtin(s) => s,
            _ => panic!("Arena: expected Builtin symbol at {}", index),
        }
    }
    pub fn get_namespace(&self, index: u32) -> (&str, &[(Rc<str>, NanValue)]) {
        match &self.symbol_entries[index as usize] {
            ArenaSymbol::Namespace { name, members } => (name, members),
            _ => panic!("Arena: expected Namespace symbol at {}", index),
        }
    }
    pub fn get_nullary_variant_ctor(&self, index: u32) -> u32 {
        match &self.symbol_entries[index as usize] {
            ArenaSymbol::NullaryVariant { ctor_id } => *ctor_id,
            _ => panic!("Arena: expected NullaryVariant symbol at {}", index),
        }
    }

    // -- Type registry -----------------------------------------------------

    pub fn register_record_type(&mut self, name: &str, field_names: Vec<String>) -> u32 {
        self.register_record_type_keyed(name, name, field_names)
    }

    pub fn register_record_type_keyed(
        &mut self,
        key: &str,
        display: &str,
        field_names: Vec<String>,
    ) -> u32 {
        let id = self.type_keys.len() as u32;
        self.type_keys.push(String::from(key));
        self.type_names.push(String::from(display));
        debug_assert_eq!(self.type_keys.len(), self.type_names.len());
        self.type_field_names.push(field_names);
        self.type_variant_names.push(Vec::new());
        self.type_variant_ctor_ids.push(Vec::new());
        id
    }

    pub fn register_sum_type(&mut self, name: &str, variant_names: Vec<String>) -> u32 {
        self.register_sum_type_keyed(name, name, variant_names)
    }

    pub fn register_sum_type_keyed(
        &mut self,
        key: &str,
        display: &str,
        variant_names: Vec<String>,
    ) -> u32 {
        let id = self.type_keys.len() as u32;
        self.type_keys.push(String::from(key));
        self.type_names.push(String::from(display));
        debug_assert_eq!(self.type_keys.len(), self.type_names.len());
        self.type_field_names.push(Vec::new());
        let ctor_ids: Vec<u32> = (0..variant_names.len())
            .map(|variant_idx| {
                let ctor_id = self.ctor_to_type_variant.len() as u32;
                self.ctor_to_type_variant.push((id, variant_idx as u16));
                ctor_id
            })
            .collect();
        self.type_variant_names.push(variant_names);
        self.type_variant_ctor_ids.push(ctor_ids);
        id
    }

    pub fn register_variant_name(&mut self, type_id: u32, variant_name: String) -> u16 {
        let variants = &mut self.type_variant_names[type_id as usize];
        let variant_id = variants.len() as u16;
        variants.push(variant_name);

        let ctor_id = self.ctor_to_type_variant.len() as u32;
        self.ctor_to_type_variant.push((type_id, variant_id));
        self.type_variant_ctor_ids[type_id as usize].push(ctor_id);

        variant_id
    }

    pub fn get_type_name(&self, type_id: u32) -> &str {
        &self.type_names[type_id as usize]
    }
    pub fn type_count(&self) -> u32 {
        self.type_names.len() as u32
    }
    pub fn get_field_names(&self, type_id: u32) -> &[String] {
        &self.type_field_names[type_id as usize]
    }
    pub fn get_variant_name(&self, type_id: u32, variant_id: u16) -> &str {
        &self.type_variant_names[type_id as usize][variant_id as usize]
    }
    pub fn register_type_alias(&mut self, alias: &str, type_id: u32) {
        self.type_aliases.push((alias.to_string(), type_id));
    }

    pub fn find_type_id(&self, name: &str) -> Option<u32> {
        self.type_keys
            .iter()
            .position(|n| n == name)
            .map(|i| i as u32)
            .or_else(|| {
                self.type_aliases
                    .iter()
                    .find(|(alias, _)| alias == name)
                    .map(|(_, id)| *id)
            })
    }
    pub fn find_variant_id(&self, type_id: u32, variant_name: &str) -> Option<u16> {
        self.type_variant_names
            .get(type_id as usize)?
            .iter()
            .position(|n| n == variant_name)
            .map(|i| i as u16)
    }

    pub fn find_ctor_id(&self, type_id: u32, variant_id: u16) -> Option<u32> {
        self.type_variant_ctor_ids
            .get(type_id as usize)?
            .get(variant_id as usize)
            .copied()
    }

    pub fn get_ctor_parts(&self, ctor_id: u32) -> (u32, u16) {
        self.ctor_to_type_variant
            .get(ctor_id as usize)
            .copied()
            .unwrap_or_else(|| panic!("Arena: expected ctor id {} to be registered", ctor_id))
    }

    pub fn len(&self) -> usize {
        self.young_entries.len()
            + self.yard_entries.len()
            + self.handoff_entries.len()
            + self.stable_entries.len()
    }
    pub fn is_empty(&self) -> bool {
        self.young_entries.is_empty()
            && self.yard_entries.is_empty()
            && self.handoff_entries.is_empty()
            && self.stable_entries.is_empty()
    }
}

impl<T: ArenaTypes> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
