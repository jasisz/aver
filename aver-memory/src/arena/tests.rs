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
        held_elsewhere: false,
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
