use super::*;

impl Arena {
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
            type_names: Vec::new(),
            type_field_names: Vec::new(),
            type_variant_names: Vec::new(),
            type_variant_ctor_ids: Vec::new(),
            ctor_to_type_variant: Vec::new(),
            symbol_entries: Vec::new(),
        }
    }

    /// Create a fresh Arena with only the static context (symbols, type metadata,
    /// stable constants) from this Arena. Dynamic runtime entries are empty.
    /// Used for independent product threads: each gets a clean Arena with just the
    /// compile-time context needed to execute functions and builtins.
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
            type_names: self.type_names.clone(),
            type_field_names: self.type_field_names.clone(),
            type_variant_names: self.type_variant_names.clone(),
            type_variant_ctor_ids: self.type_variant_ctor_ids.clone(),
            ctor_to_type_variant: self.ctor_to_type_variant.clone(),
            symbol_entries: self.symbol_entries.clone(),
        }
    }

    /// Deep-import a NanValue from `source` arena into `self`.
    /// Inline values (int, float, bool, unit, none, empty containers) are returned as-is.
    /// Heap-referenced values are recursively copied into `self` with new indices.
    pub fn deep_import(&mut self, value: NanValue, source: &Arena) -> NanValue {
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
                    let rc_items = Rc::new(imported);
                    let idx = self.push(ArenaEntry::List(ArenaList::Flat {
                        items: rc_items,
                        start: 0,
                    }));
                    NanValue::new_list(idx)
                }
            }
            ArenaEntry::Map(map) => {
                let mut new_map = super::PersistentMap::new();
                for (hash, (k, v)) in map.iter() {
                    let ik = self.deep_import(*k, source);
                    let iv = self.deep_import(*v, source);
                    new_map = new_map.insert(*hash, (ik, iv));
                }
                let idx = self.push(ArenaEntry::Map(new_map));
                NanValue::new_map(idx)
            }
            ArenaEntry::Vector(items) => {
                let imported: Vec<NanValue> =
                    items.iter().map(|v| self.deep_import(*v, source)).collect();
                let idx = self.push(ArenaEntry::Vector(imported));
                NanValue::new_vector(idx)
            }
            ArenaEntry::Record { type_id, fields } => {
                let imported: Vec<NanValue> = fields
                    .iter()
                    .map(|v| self.deep_import(*v, source))
                    .collect();
                let idx = self.push(ArenaEntry::Record {
                    type_id,
                    fields: imported,
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

    #[inline]
    pub fn push(&mut self, entry: ArenaEntry) -> u32 {
        match &entry {
            ArenaEntry::Fn(_) | ArenaEntry::Builtin(_) | ArenaEntry::Namespace { .. } => {}
            _ => {
                return match self.alloc_space {
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
            }
        }
        match entry {
            ArenaEntry::Fn(f) => self.push_symbol(ArenaSymbol::Fn(f)),
            ArenaEntry::Builtin(name) => self.push_symbol(ArenaSymbol::Builtin(name)),
            ArenaEntry::Namespace { name, members } => {
                self.push_symbol(ArenaSymbol::Namespace { name, members })
            }
            _ => unreachable!("non-symbol entry already returned above"),
        }
    }

    #[inline]
    pub fn push_symbol(&mut self, symbol: ArenaSymbol) -> u32 {
        let idx = self.symbol_entries.len() as u32;
        self.symbol_entries.push(symbol);
        idx
    }

    #[inline]
    pub fn get(&self, index: u32) -> &ArenaEntry {
        let (space, raw_index) = Self::decode_index(index);
        match space {
            HeapSpace::Young => &self.young_entries[raw_index as usize],
            HeapSpace::Yard => &self.yard_entries[raw_index as usize],
            HeapSpace::Handoff => &self.handoff_entries[raw_index as usize],
            HeapSpace::Stable => &self.stable_entries[raw_index as usize],
        }
    }

    #[inline]
    pub(super) fn encode_index(space: HeapSpace, index: u32) -> u32 {
        ((space as u32) << HEAP_SPACE_SHIFT) | index
    }

    #[inline]
    pub(super) fn encode_yard_index(index: u32) -> u32 {
        Self::encode_index(HeapSpace::Yard, index)
    }

    #[inline]
    pub(super) fn encode_stable_index(index: u32) -> u32 {
        Self::encode_index(HeapSpace::Stable, index)
    }

    #[inline]
    pub(super) fn encode_handoff_index(index: u32) -> u32 {
        Self::encode_index(HeapSpace::Handoff, index)
    }

    #[inline]
    pub(super) fn decode_index(index: u32) -> (HeapSpace, u32) {
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
    pub(super) fn note_peak_usage(&mut self) {
        let usage = self.usage();
        self.peak_usage.young = self.peak_usage.young.max(usage.young);
        self.peak_usage.yard = self.peak_usage.yard.max(usage.yard);
        self.peak_usage.handoff = self.peak_usage.handoff.max(usage.handoff);
        self.peak_usage.stable = self.peak_usage.stable.max(usage.stable);
    }

    #[inline]
    pub(super) fn take_u32_scratch(slot: &mut Vec<u32>, len: usize) -> Vec<u32> {
        let mut scratch = std::mem::take(slot);
        scratch.clear();
        scratch.resize(len, u32::MAX);
        scratch
    }

    #[inline]
    pub(super) fn recycle_u32_scratch(slot: &mut Vec<u32>, mut scratch: Vec<u32>) {
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

    pub fn with_alloc_space<T>(&mut self, space: AllocSpace, f: impl FnOnce(&mut Arena) -> T) -> T {
        let prev = self.alloc_space;
        self.alloc_space = space;
        let out = f(self);
        self.alloc_space = prev;
        out
    }

    // -- Typed push helpers ------------------------------------------------

    pub fn push_i64(&mut self, val: i64) -> u32 {
        self.push(ArenaEntry::Int(val))
    }
    pub fn push_string(&mut self, s: &str) -> u32 {
        self.push(ArenaEntry::String(Rc::from(s)))
    }
    pub fn push_boxed(&mut self, val: NanValue) -> u32 {
        self.push(ArenaEntry::Boxed(val))
    }
    pub fn push_record(&mut self, type_id: u32, fields: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::Record { type_id, fields })
    }
    pub fn push_variant(&mut self, type_id: u32, variant_id: u16, fields: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::Variant {
            type_id,
            variant_id,
            fields,
        })
    }
    pub fn push_list(&mut self, items: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::List(ArenaList::Flat {
            items: Rc::new(items),
            start: 0,
        }))
    }
    pub fn push_map(&mut self, map: PersistentMap) -> u32 {
        self.push(ArenaEntry::Map(map))
    }
    pub fn push_tuple(&mut self, items: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::Tuple(items))
    }
    pub fn push_vector(&mut self, items: Vec<NanValue>) -> u32 {
        self.push(ArenaEntry::Vector(items))
    }
    pub fn push_fn(&mut self, f: Rc<FunctionValue>) -> u32 {
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
            ArenaEntry::Record { type_id, fields } => (*type_id, fields),
            _ => panic!("Arena: expected Record at {}", index),
        }
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
            ArenaEntry::Vector(items) => items,
            _ => panic!("Arena: expected Vector at {}", index),
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
    pub fn get_map(&self, index: u32) -> &PersistentMap {
        match self.get(index) {
            ArenaEntry::Map(map) => map,
            _ => panic!("Arena: expected Map at {}", index),
        }
    }
    pub fn map_ref_value(&self, map: NanValue) -> &PersistentMap {
        thread_local! {
            static EMPTY_MAP: PersistentMap = PersistentMap::new();
        }
        if map.is_empty_map_immediate() {
            // SAFETY: thread_local guarantees single-thread; reference is valid
            // for the lifetime of the thread (longer than any Arena borrow).
            return EMPTY_MAP.with(|m| unsafe { &*(m as *const PersistentMap) });
        }
        self.get_map(map.arena_index())
    }
    pub fn clone_map_value(&self, map: NanValue) -> PersistentMap {
        if map.is_empty_map_immediate() {
            PersistentMap::new()
        } else {
            self.get_map(map.arena_index()).clone()
        }
    }
    pub fn get_fn(&self, index: u32) -> &FunctionValue {
        match &self.symbol_entries[index as usize] {
            ArenaSymbol::Fn(f) => f,
            _ => panic!("Arena: expected Fn symbol at {}", index),
        }
    }
    pub fn get_fn_rc(&self, index: u32) -> &Rc<FunctionValue> {
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
        let id = self.type_names.len() as u32;
        self.type_names.push(name.to_string());
        self.type_field_names.push(field_names);
        self.type_variant_names.push(Vec::new());
        self.type_variant_ctor_ids.push(Vec::new());
        id
    }

    pub fn register_sum_type(&mut self, name: &str, variant_names: Vec<String>) -> u32 {
        let id = self.type_names.len() as u32;
        self.type_names.push(name.to_string());
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
    pub fn find_type_id(&self, name: &str) -> Option<u32> {
        self.type_names
            .iter()
            .position(|n| n == name)
            .map(|i| i as u32)
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

impl Default for Arena {
    fn default() -> Self {
        Self::new()
    }
}
