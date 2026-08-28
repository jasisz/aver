use super::*;

#[derive(Clone, Debug)]
struct TestTypes;

#[derive(Clone, Debug)]
struct TestFn;

impl FnValueName for TestFn {
    fn name(&self) -> &str {
        "test"
    }
}

impl ArenaTypes for TestTypes {
    type Fn = TestFn;
    type Map = PersistentMap;
}

type TestArena = Arena<TestTypes>;

#[test]
fn taking_record_fields_releases_exactly_the_removed_holders() {
    let mut arena = TestArena::new();
    let map = NanValue::new_map(arena.push_map(PersistentMap::new()));
    let record = NanValue::new_record(arena.push_record(1, vec![map, map]));

    assert!(arena.map_slot(map).unwrap().held_elsewhere);
    assert_eq!(arena.take_record_field(record, 0).bits(), map.bits());
    assert!(
        arena.map_slot(map).unwrap().held_elsewhere,
        "the record's second field still holds the map"
    );
    assert_eq!(arena.take_record_field(record, 1).bits(), map.bits());
    assert!(
        !arena.map_slot(map).unwrap().held_elsewhere,
        "removing both fields must release both registered holders"
    );
}

#[test]
fn record_holder_count_matches_the_exhaustive_audit_after_a_field_take() {
    let mut arena = TestArena::new();
    let inner = NanValue::new_record(arena.push_record(1, Vec::new()));
    let outer = NanValue::new_record(arena.push_record(2, vec![inner]));

    assert!(arena.record_is_held_elsewhere(inner));
    assert!(arena.any_entry_holds_slot(inner.arena_index()));

    assert_eq!(arena.take_record_field(outer, 0).bits(), inner.bits());
    assert!(!arena.record_is_held_elsewhere(inner));
    assert!(!arena.any_entry_holds_slot(inner.arena_index()));
}

#[test]
fn lane_receipts_must_not_be_newer_than_the_frame_serial() {
    let mut arena = TestArena::new();
    let early_receipt = arena.lane_mark();
    arena.note_lane_push();
    let frame_mark = arena.lane_mark();
    arena.note_lane_push();
    let late_receipt = arena.lane_mark();

    arena.begin_lane_rewrite(frame_mark);

    assert!(arena.lane_receipt_can_skip(early_receipt));
    assert!(arena.lane_receipt_can_skip(frame_mark));
    assert!(!arena.lane_receipt_can_skip(late_receipt));
    assert_ne!(arena.renewed_lane_receipt(), frame_mark);
}

#[test]
fn an_old_receipt_cannot_reappear_after_index_reuse() {
    let mut arena = TestArena::new();
    let original_frame = arena.lane_mark();
    arena.begin_lane_rewrite(original_frame);
    assert!(arena.lane_receipt_can_skip(original_frame));
    let post_boundary_receipt = arena.renewed_lane_receipt();
    arena.finish_lane_rewrite();

    // Model another destructive boundary reusing the same raw slots while an
    // older frame mark remains active. The boundary itself advances the serial,
    // so a receipt created after that frame cannot become old through reuse.
    arena.begin_lane_rewrite(original_frame);

    assert!(!arena.lane_receipt_can_skip(post_boundary_receipt));
}

#[test]
fn lane_clock_overflow_is_sticky_and_fails_closed() {
    let mut boundary_exhausted = TestArena::new();
    boundary_exhausted.lane_serial = LaneMark::MAX;
    let last_boundary_mark = boundary_exhausted.lane_mark();
    boundary_exhausted.begin_lane_rewrite(last_boundary_mark);

    assert!(boundary_exhausted.lane_clock_exhausted);
    assert_eq!(boundary_exhausted.lane_mark(), INVALID_LANE_MARK);
    assert!(!boundary_exhausted.lane_receipt_can_skip(last_boundary_mark));

    let mut push_exhausted = TestArena::new();
    push_exhausted.lane_serial = LaneMark::MAX;
    let last_push_mark = push_exhausted.lane_mark();
    push_exhausted.note_lane_push();

    assert!(push_exhausted.lane_clock_exhausted);
    assert_eq!(push_exhausted.lane_mark(), INVALID_LANE_MARK);
    assert!(!push_exhausted.lane_receipt_can_skip(last_push_mark));

    push_exhausted.begin_lane_rewrite(last_push_mark);
    assert!(push_exhausted.lane_clock_exhausted);
    assert_eq!(push_exhausted.lane_mark(), INVALID_LANE_MARK);
}

#[test]
fn clone_static_preserves_the_clock_and_stable_receipts_by_value() {
    let mut arena = TestArena::new();
    arena.note_lane_push();
    let entry_receipt = arena.lane_mark();
    arena.stable_entries.push(ArenaEntry::Map {
        map: PersistentMap::new(),
        all_immediate: true,
        holder_count: 0,
        scan_receipt: entry_receipt,
        pending_scan_keys: Vec::new(),
    });

    let child = arena.clone_static();
    let ArenaEntry::Map { scan_receipt, .. } = &child.stable_entries[0] else {
        panic!("expected cloned stable map");
    };

    assert_eq!(*scan_receipt, entry_receipt);
    assert_eq!(child.lane_mark(), arena.lane_mark());
}

#[test]
fn deep_import_writes_a_fresh_target_receipt() {
    let mut source = TestArena::new();
    let key = NanValue::new_string_value("heap-backed-key", &mut source);
    let value = NanValue::new_string_value("heap-backed-value", &mut source);
    let mut source_map = PersistentMap::new();
    source_map = source_map.insert_owned(key.map_key_hash(&source), (key, value));
    let source_idx = source.push_map(source_map);
    let source_value = NanValue::new_map(source_idx);
    let ArenaEntry::Map {
        scan_receipt: source_receipt,
        ..
    } = source.get(source_idx)
    else {
        panic!("expected source map");
    };
    let source_receipt = *source_receipt;

    let mut target = TestArena::new();
    for _ in 0..7 {
        target.note_lane_push();
    }
    let imported = target.deep_import(source_value, &source);
    let imported_idx = imported.heap_index().expect("imported map index");
    let ArenaEntry::Map {
        scan_receipt: target_receipt,
        ..
    } = target.get(imported_idx)
    else {
        panic!("expected imported map");
    };

    assert_ne!(*target_receipt, source_receipt);
    assert_eq!(*target_receipt + 1, target.lane_mark());
}

/// Read the flag straight off the entry, so the test asserts what the
/// collector will read rather than what a helper reports.
fn vector_flag(arena: &TestArena, value: NanValue) -> bool {
    let ArenaEntry::Vector { all_immediate, .. } = arena.get(value.arena_index()) else {
        panic!("expected a vector");
    };
    *all_immediate
}

/// A from-scratch walk of the elements — the answer the flag is supposed to
/// stand in for.
fn vector_really_all_immediate(arena: &TestArena, value: NanValue) -> bool {
    let ArenaEntry::Vector { items, .. } = arena.get(value.arena_index()) else {
        panic!("expected a vector");
    };
    items.iter().all(|element| element.heap_index().is_none())
}

/// Every producer of the all-immediate flag, checked against a walk.
///
/// This is where the promise is proved. The collector's two escapes deliberately
/// do NOT re-prove it — the walk that would prove it is the walk they exist to
/// skip, and the table they skip is threaded through every step of every indexed
/// string loop, so re-proving it under debug assertions would put the quadratic
/// straight back into every debug build. Everything that can set the flag to
/// true is here instead, and each one decides from values it has in hand.
#[test]
fn the_vector_all_immediate_flag_is_exact_at_every_producer() {
    let mut arena = TestArena::new();

    // The builder that sees every element, both ways.
    let zero = NanValue::new_int(0, &mut arena);
    let seven = NanValue::new_int(7, &mut arena);
    let offsets = NanValue::new_vector(arena.push_vector(vec![zero, seven]));
    assert!(vector_flag(&arena, offsets));
    assert_eq!(
        vector_flag(&arena, offsets),
        vector_really_all_immediate(&arena, offsets)
    );

    let heap_backed = NanValue::new_map(arena.push_map(PersistentMap::new()));
    assert!(
        heap_backed.heap_index().is_some(),
        "a heap map has an index"
    );
    let mixed = NanValue::new_vector(arena.push_vector(vec![zero, heap_backed]));
    assert!(!vector_flag(&arena, mixed));
    assert_eq!(
        vector_flag(&arena, mixed),
        vector_really_all_immediate(&arena, mixed)
    );

    // The in-place write keeps an immediate table immediate...
    let five = NanValue::new_int(5, &mut arena);
    assert!(arena.vector_store_in_place(offsets.arena_index(), 1, five));
    assert!(vector_flag(&arena, offsets));
    assert_eq!(
        vector_flag(&arena, offsets),
        vector_really_all_immediate(&arena, offsets)
    );

    // ...and gives the promise up at the first heap value it stores.
    assert!(arena.vector_store_in_place(offsets.arena_index(), 0, heap_backed));
    assert!(!vector_flag(&arena, offsets));
    assert_eq!(
        vector_flag(&arena, offsets),
        vector_really_all_immediate(&arena, offsets)
    );

    // The mutable escape hatch cannot know what its caller writes, so it gives
    // the promise up unconditionally.
    let one = NanValue::new_int(1, &mut arena);
    let plain = NanValue::new_vector(arena.push_vector(vec![one]));
    assert!(vector_flag(&arena, plain));
    let _ = arena.get_vector_mut(plain.arena_index());
    assert!(
        !vector_flag(&arena, plain),
        "handing out the elements has to give up the promise"
    );
}

/// The counter that made the cost legible, and the escape it reports.
///
/// A vector of immediates is returned by the collector unread; one holding an
/// arena index is read in full. The difference is what an indexed string walk's
/// residual time follows.
#[test]
fn the_collector_reads_a_mixed_vector_and_skips_an_immediate_one() {
    let mut arena = TestArena::new();
    let mark = arena.young_len() as u32;
    let lane = arena.lane_mark();

    let elements = vec![
        NanValue::new_int(0, &mut arena),
        NanValue::new_int(1, &mut arena),
        NanValue::new_int(2, &mut arena),
    ];
    let immediate = NanValue::new_vector(arena.push_vector(elements));
    let mut roots = [immediate];
    arena.promote_young_roots_to_yard(mark, lane, &mut roots);
    assert_eq!(
        arena.vector_elements_scanned(),
        0,
        "a table of immediates must not be read to promote it"
    );

    let mut arena = TestArena::new();
    let mark = arena.young_len() as u32;
    let lane = arena.lane_mark();
    let inner = NanValue::new_map(arena.push_map(PersistentMap::new()));
    let elements = vec![
        NanValue::new_int(0, &mut arena),
        inner,
        NanValue::new_int(2, &mut arena),
    ];
    let mixed = NanValue::new_vector(arena.push_vector(elements));
    let mut roots = [mixed];
    arena.promote_young_roots_to_yard(mark, lane, &mut roots);
    assert!(
        arena.vector_elements_scanned() >= 3,
        "a table holding an arena index is read in full, got {}",
        arena.vector_elements_scanned()
    );
}
