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

fn map_flag(arena: &TestArena, value: NanValue) -> bool {
    let ArenaEntry::Map { all_immediate, .. } = arena.get(value.arena_index()) else {
        panic!("expected a map");
    };
    *all_immediate
}

fn map_really_all_immediate(arena: &TestArena, value: NanValue) -> bool {
    let ArenaEntry::Map { map, .. } = arena.get(value.arena_index()) else {
        panic!("expected a map");
    };
    crate::map_all_immediate(map)
}

/// Prove the map escape at the producer, not inside the collector hot path.
/// Re-proving it while taking the escape would read the whole table on every
/// debug boundary and restore the quadratic cost the flag removes.
#[test]
fn the_map_all_immediate_flag_is_sound_at_the_builder() {
    let mut arena = TestArena::new();
    let key = NanValue::new_int(1, &mut arena);
    let value = NanValue::new_int(2, &mut arena);
    let mut immediate = PersistentMap::new();
    immediate = immediate.insert_owned(key.map_key_hash(&arena), (key, value));
    let immediate = NanValue::new_map(arena.push_map(immediate));
    assert!(map_flag(&arena, immediate));
    assert!(map_really_all_immediate(&arena, immediate));

    let heap_value = NanValue::new_string_value("heap-backed-map-value", &mut arena);
    let mut mixed = PersistentMap::new();
    mixed = mixed.insert_owned(key.map_key_hash(&arena), (key, heap_value));
    let mixed = NanValue::new_map(arena.push_map(mixed));
    assert!(!map_flag(&arena, mixed));
    assert!(!map_really_all_immediate(&arena, mixed));
}

#[test]
fn a_carried_vector_of_strings_is_scanned_once_not_once_per_boundary() {
    const ELEMENTS: usize = 128;
    const BOUNDARIES: usize = 128;

    let mut arena = TestArena::new();
    let initial_mark = arena.young_len() as u32;
    let initial_lane = arena.lane_mark();
    let items = (0..ELEMENTS)
        .map(|i| NanValue::new_string_value(&format!("heap-vector-value-{i}"), &mut arena))
        .collect();
    let vector = NanValue::new_vector(arena.push_vector(items));
    let mut roots = [vector];
    arena.promote_young_roots_to_yard(initial_mark, initial_lane, &mut roots, false);
    let mut vector = roots[0];
    let after_initial_scan = arena.vector_elements_scanned();
    assert!(after_initial_scan >= ELEMENTS as u64);

    for step in 0..BOUNDARIES {
        let mark = arena.young_len() as u32;
        let lane = arena.lane_mark();
        let _garbage = arena.push_string(&format!("boundary-garbage-{step}"));
        let mut roots = [vector];
        arena.promote_young_roots_to_yard(mark, lane, &mut roots, false);
        vector = roots[0];
    }

    assert_eq!(
        arena.vector_elements_scanned(),
        after_initial_scan,
        "a receipt-protected carried vector must not be re-read"
    );
    assert_eq!(arena.get_vector(vector.arena_index()).len(), ELEMENTS);
}

#[test]
fn a_heap_write_invalidates_the_vector_receipt_and_survives_the_boundary() {
    let mut arena = TestArena::new();
    let initial_mark = arena.young_len() as u32;
    let initial_lane = arena.lane_mark();
    let old = NanValue::new_string_value("old-vector-value", &mut arena);
    let vector = NanValue::new_vector(arena.push_vector(vec![old]));
    let mut roots = [vector];
    arena.promote_young_roots_to_yard(initial_mark, initial_lane, &mut roots, false);
    let vector = roots[0];

    let mark = arena.young_len() as u32;
    let lane = arena.lane_mark();
    let fresh = NanValue::new_string_value("fresh-vector-value", &mut arena);
    assert!(arena.vector_store_in_place(vector.arena_index(), 0, fresh));
    let scanned = arena.vector_elements_scanned();
    let mut roots = [vector];
    arena.promote_young_roots_to_yard(mark, lane, &mut roots, true);

    assert_eq!(arena.vector_elements_scanned() - scanned, 1);
    let stored = arena.get_vector(roots[0].arena_index())[0];
    assert_eq!(arena.get_string_value(stored), "fresh-vector-value");
}

fn concat_chain_promoting_each_node(cells: usize) -> (TestArena, NanValue, u64) {
    let mut arena = TestArena::new();
    let first = NanValue::new_int(0, &mut arena);
    let mut list = NanValue::new_list(arena.push_list(vec![first]));
    let before = arena.out_of_region_entries_read();

    for value in 1..cells {
        let mark = arena.young_len() as u32;
        let lane = arena.lane_mark();
        let item = NanValue::new_int(value as i64, &mut arena);
        let singleton = NanValue::new_list(arena.push_list(vec![item]));
        list = NanValue::new_list(arena.push_list_concat(list, singleton));
        let mut roots = [list];
        arena.promote_young_roots_to_yard(mark, lane, &mut roots, false);
        list = roots[0];
    }

    let read = arena.out_of_region_entries_read() - before;
    (arena, list, read)
}

#[test]
fn a_promotion_does_not_walk_the_concat_spine_it_has_already_promoted() {
    const CELLS: usize = 256;
    let (arena, list, read) = concat_chain_promoting_each_node(CELLS);

    assert_eq!(arena.list_len_value(list), CELLS);
    assert!(
        read <= CELLS as u64,
        "promoting {CELLS} concat nodes re-read {read} settled entries"
    );
}

fn deep_concat(arena: &mut TestArena, cells: usize) -> NanValue {
    let first = NanValue::new_int(0, arena);
    let mut list = NanValue::new_list(arena.push_list(vec![first]));
    for value in 1..cells {
        let item = NanValue::new_int(value as i64, arena);
        let singleton = NanValue::new_list(arena.push_list(vec![item]));
        list = NanValue::new_list(arena.push_list_concat(list, singleton));
    }
    list
}

fn assert_last_list_value(arena: &TestArena, list: NanValue, cells: usize) {
    assert_eq!(arena.list_len_value(list), cells);
    assert_eq!(
        arena
            .list_get_value(list, cells - 1)
            .map(|value| value.as_int(arena)),
        Some((cells - 1) as i64)
    );
}

#[test]
fn promoting_a_deep_concat_uses_an_explicit_work_stack() {
    const CELLS: usize = 12_000;
    let mut arena = TestArena::new();
    let mark = arena.young_len() as u32;
    let lane = arena.lane_mark();
    let list = deep_concat(&mut arena, CELLS);

    let mut roots = [list];
    arena.promote_young_roots_to_yard(mark, lane, &mut roots, false);
    assert_last_list_value(&arena, roots[0], CELLS);
}

#[test]
fn evacuating_a_deep_concat_uses_an_explicit_work_stack() {
    const CELLS: usize = 12_000;
    let mut arena = TestArena::new();
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;
    let lane_mark = arena.lane_mark();
    let list = deep_concat(&mut arena, CELLS);

    let mut roots = [list];
    arena.evacuate_frame_to_yard(
        young_mark,
        yard_mark,
        handoff_mark,
        lane_mark,
        &mut roots,
        false,
    );
    assert_last_list_value(&arena, roots[0], CELLS);
}

#[test]
fn moving_a_deep_concat_to_stable_uses_an_explicit_work_stack() {
    const CELLS: usize = 12_000;
    let mut arena = TestArena::new();
    let list = deep_concat(&mut arena, CELLS);

    let mut roots = [list];
    arena.promote_roots_to_stable(&mut roots, false);
    assert_last_list_value(&arena, roots[0], CELLS);
}

fn alternating_concat_and_prepend(arena: &mut TestArena, cells: usize) -> NanValue {
    let first = NanValue::new_int(0, arena);
    let mut list = NanValue::new_list(arena.push_list(vec![first]));
    for value in 1..cells {
        let item = NanValue::new_int(value as i64, arena);
        list = if value % 2 == 0 {
            NanValue::new_list(arena.push_list_prepend(item, list))
        } else {
            let singleton = NanValue::new_list(arena.push_list(vec![item]));
            NanValue::new_list(arena.push_list_concat(list, singleton))
        };
    }
    list
}

#[test]
fn alternating_concat_and_prepend_nodes_are_iterative_on_every_collector_path() {
    const CELLS: usize = 12_000;

    let mut promoted = TestArena::new();
    let mark = promoted.young_len() as u32;
    let lane = promoted.lane_mark();
    let list = alternating_concat_and_prepend(&mut promoted, CELLS);
    let mut roots = [list];
    promoted.promote_young_roots_to_yard(mark, lane, &mut roots, false);
    assert_eq!(promoted.list_len_value(roots[0]), CELLS);

    let mut evacuated = TestArena::new();
    let young_mark = evacuated.young_len() as u32;
    let yard_mark = evacuated.yard_len() as u32;
    let handoff_mark = evacuated.handoff_len() as u32;
    let lane_mark = evacuated.lane_mark();
    let list = alternating_concat_and_prepend(&mut evacuated, CELLS);
    let mut roots = [list];
    evacuated.evacuate_frame_to_yard(
        young_mark,
        yard_mark,
        handoff_mark,
        lane_mark,
        &mut roots,
        false,
    );
    assert_eq!(evacuated.list_len_value(roots[0]), CELLS);

    let mut stable = TestArena::new();
    let list = alternating_concat_and_prepend(&mut stable, CELLS);
    let mut roots = [list];
    stable.promote_roots_to_stable(&mut roots, false);
    assert_eq!(stable.list_len_value(roots[0]), CELLS);
}
