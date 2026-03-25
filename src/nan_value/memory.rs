use super::*;

impl Arena {
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

    pub fn evacuate_frame_to_yard(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        roots: &mut [NanValue],
    ) -> (bool, bool) {
        self.evacuate_frame_locals(young_mark, yard_mark, handoff_mark, roots, AllocSpace::Yard)
    }

    pub fn evacuate_frame_to_handoff(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        roots: &mut [NanValue],
    ) -> (bool, bool) {
        self.evacuate_frame_locals(
            young_mark,
            yard_mark,
            handoff_mark,
            roots,
            AllocSpace::Handoff,
        )
    }

    fn evacuate_frame_locals(
        &mut self,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        roots: &mut [NanValue],
        young_target: AllocSpace,
    ) -> (bool, bool) {
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
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
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
        entry: ArenaEntry,
        compacted_yard: &mut [ArenaEntry],
        compacted_handoff: &mut [ArenaEntry],
    ) {
        match target {
            AllocSpace::Yard => compacted_yard[compacted_pos as usize] = entry,
            AllocSpace::Handoff => compacted_handoff[compacted_pos as usize] = entry,
            AllocSpace::Young => unreachable!(),
        }
    }

    #[inline(always)]
    fn rewrite_entry_with<F>(&mut self, entry: ArenaEntry, rewrite: &mut F) -> ArenaEntry
    where
        F: FnMut(&mut Arena, NanValue) -> NanValue,
    {
        match entry {
            ArenaEntry::Int(i) => ArenaEntry::Int(i),
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
            ArenaEntry::Map(map) => {
                let mut out = PersistentMap::new();
                for (&hash, &(key, value)) in map.iter() {
                    out = out.insert(hash, (rewrite(self, key), rewrite(self, value)));
                }
                ArenaEntry::Map(out)
            }
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

    #[inline(always)]
    fn rewrite_list_with<F>(&mut self, list: ArenaList, rewrite: &mut F) -> ArenaList
    where
        F: FnMut(&mut Arena, NanValue) -> NanValue,
    {
        match list {
            ArenaList::Flat { items, start } => ArenaList::Flat {
                items: Rc::new(
                    items[start..]
                        .iter()
                        .map(|value| rewrite(self, *value))
                        .collect(),
                ),
                start: 0,
            },
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
            } => ArenaList::Segments {
                current: rewrite(self, current),
                rest: Rc::new(
                    rest[start..]
                        .iter()
                        .map(|value| rewrite(self, *value))
                        .collect(),
                ),
                start: 0,
                len,
            },
        }
    }

    /// Flatten a deep list (Prepend/Concat chain) into a single Flat entry.
    /// Returns the original value unchanged if not a deep list.
    /// Uses the existing iterative `list_to_vec` — no recursion, no stack overflow.
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
            items: Rc::new(elements),
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
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
    ) -> NanValue {
        let Some(index) = value.heap_index() else {
            return value;
        };
        let (space, _) = Self::decode_index(index);
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
            // Older immutable prefixes cannot start referencing current-frame
            // locals after the fact, so evacuation only needs to rewrite the
            // actual local suffixes.
            _ => value,
        }
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
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
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

        let entry = std::mem::replace(
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
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
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

        let entry = std::mem::replace(
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
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
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

        let entry = std::mem::replace(
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
    fn evacuate_local_entry(
        &mut self,
        entry: ArenaEntry,
        young_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        young_target: AllocSpace,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
        compacted_yard: &mut Vec<ArenaEntry>,
        compacted_handoff: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        let mut rewrite = |arena: &mut Arena, value: NanValue| {
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
        compacted: &mut Vec<ArenaEntry>,
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
        compacted: &mut Vec<ArenaEntry>,
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

        let entry = std::mem::replace(
            &mut self.young_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.relocate_young_entry(entry, mark, relocated, compacted);
        compacted[compacted_pos as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn relocate_young_entry(
        &mut self,
        entry: ArenaEntry,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        let mut rewrite = |arena: &mut Arena, value: NanValue| {
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
        compacted: &mut Vec<ArenaEntry>,
    ) {
        let raw_index = raw_index as usize;
        match space {
            HeapSpace::Young => {
                if raw_index >= self.young_entries.len() || raw_index >= mark as usize {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.young_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.young_entries[raw_index] = new_entry;
            }
            HeapSpace::Yard => {
                if raw_index >= self.yard_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.yard_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.yard_entries[raw_index] = new_entry;
            }
            HeapSpace::Handoff => {
                if raw_index >= self.handoff_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.handoff_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.handoff_entries[raw_index] = new_entry;
            }
            HeapSpace::Stable => {
                if raw_index >= self.stable_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_young_entry(entry, mark, relocated, compacted);
                self.stable_entries[raw_index] = new_entry;
            }
        }
    }

    fn rewrite_young_entry(
        &mut self,
        entry: ArenaEntry,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        let mut rewrite = |arena: &mut Arena, value: NanValue| {
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

    pub fn promote_roots_to_stable(&mut self, roots: &mut [NanValue]) {
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
                    std::mem::replace(&mut self.young_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_promoted_young_entry(entry, mark, relocated, target);
                self.young_entries[raw_index] = new_entry;
            }
            HeapSpace::Yard => {
                if raw_index >= self.yard_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.yard_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_promoted_young_entry(entry, mark, relocated, target);
                self.yard_entries[raw_index] = new_entry;
            }
            HeapSpace::Handoff => {
                if raw_index >= self.handoff_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.handoff_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_promoted_young_entry(entry, mark, relocated, target);
                self.handoff_entries[raw_index] = new_entry;
            }
            HeapSpace::Stable => {
                if raw_index >= self.stable_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_promoted_young_entry(entry, mark, relocated, target);
                self.stable_entries[raw_index] = new_entry;
            }
        }
    }

    fn rewrite_promoted_young_entry(
        &mut self,
        entry: ArenaEntry,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
    ) -> ArenaEntry {
        let mut rewrite = |arena: &mut Arena, value: NanValue| {
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

        let entry = std::mem::replace(
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

    fn promote_entry_to_target(
        &mut self,
        entry: ArenaEntry,
        mark: u32,
        relocated: &mut [u32],
        target: AllocSpace,
    ) -> ArenaEntry {
        let mut rewrite = |arena: &mut Arena, value: NanValue| {
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
            HeapSpace::Stable => value,
        }
    }

    fn promote_entry_to_stable(
        &mut self,
        entry: ArenaEntry,
        relocated_young: &mut [u32],
        relocated_yard: &mut [u32],
        relocated_handoff: &mut [u32],
    ) -> ArenaEntry {
        let mut rewrite = |arena: &mut Arena, value: NanValue| {
            arena.promote_value_to_stable(value, relocated_young, relocated_yard, relocated_handoff)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
    }

    fn relocate_yard_root(
        &mut self,
        value: NanValue,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
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
        compacted: &mut Vec<ArenaEntry>,
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

        let entry = std::mem::replace(
            &mut self.yard_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.relocate_yard_entry(entry, mark, relocated, compacted);
        compacted[compacted_pos as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn relocate_yard_entry(
        &mut self,
        entry: ArenaEntry,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        let mut rewrite = |arena: &mut Arena, value: NanValue| {
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
        compacted: &mut Vec<ArenaEntry>,
    ) {
        let raw_index = raw_index as usize;
        match space {
            HeapSpace::Young => {
                if raw_index >= self.young_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.young_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_yard_entry(entry, mark, relocated, compacted);
                self.young_entries[raw_index] = new_entry;
            }
            HeapSpace::Yard => {
                if raw_index >= self.yard_entries.len() || raw_index >= mark as usize {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.yard_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_yard_entry(entry, mark, relocated, compacted);
                self.yard_entries[raw_index] = new_entry;
            }
            HeapSpace::Handoff => {
                if raw_index >= self.handoff_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.handoff_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_yard_entry(entry, mark, relocated, compacted);
                self.handoff_entries[raw_index] = new_entry;
            }
            HeapSpace::Stable => {
                if raw_index >= self.stable_entries.len() {
                    return;
                }
                let entry =
                    std::mem::replace(&mut self.stable_entries[raw_index], ArenaEntry::Int(0));
                let new_entry = self.rewrite_yard_entry(entry, mark, relocated, compacted);
                self.stable_entries[raw_index] = new_entry;
            }
        }
    }

    fn rewrite_yard_entry(
        &mut self,
        entry: ArenaEntry,
        mark: u32,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        let mut rewrite = |arena: &mut Arena, value: NanValue| {
            arena.relocate_yard_root(value, mark, relocated, compacted)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
    }

    fn relocate_stable_root(
        &mut self,
        value: NanValue,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
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
        compacted: &mut Vec<ArenaEntry>,
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

        let entry = std::mem::replace(
            &mut self.stable_entries[raw_index as usize],
            ArenaEntry::Int(0),
        );
        let new_entry = self.relocate_stable_entry(entry, relocated, compacted);
        compacted[(new_index & HEAP_INDEX_MASK_U32) as usize] = new_entry;
        value.with_heap_index(new_index)
    }

    fn relocate_stable_entry(
        &mut self,
        entry: ArenaEntry,
        relocated: &mut [u32],
        compacted: &mut Vec<ArenaEntry>,
    ) -> ArenaEntry {
        let mut rewrite = |arena: &mut Arena, value: NanValue| {
            arena.relocate_stable_value(value, relocated, compacted)
        };
        self.rewrite_entry_with(entry, &mut rewrite)
    }
}
