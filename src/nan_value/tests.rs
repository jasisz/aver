use super::*;

#[test]
fn size_is_8_bytes() {
    assert_eq!(std::mem::size_of::<NanValue>(), 8);
}

/// Converting a tree-shaped `Value::Map` into the arena representation is the
/// third place that built a map by rebuilding it — the same one-screen loop as
/// `Map.fromList`, on the path every replayed value and every interop value
/// crosses. With the preserving insert, a 400-entry map cost 79,800 entry
/// duplications and an 800-entry one 319,600. The map under construction is
/// unreachable from anywhere else, so the right answer is zero.
#[test]
fn converting_a_map_value_does_not_rebuild_its_table_per_entry() {
    fn conversion_copies(n: i64) -> u64 {
        let mut arena = Arena::new();
        let entries: std::collections::HashMap<crate::value::Value, crate::value::Value> = (0..n)
            .map(|i| {
                (
                    crate::value::Value::Str(format!("k{i}")),
                    crate::value::Value::Int(aver_rt::AverInt::Small(i)),
                )
            })
            .collect();
        let value = crate::value::Value::Map(entries);
        let converted = NanValue::from_value(&value, &mut arena);
        assert_eq!(arena.map_ref_value(converted).len(), n as usize);
        arena.map_entries_copied()
    }

    let small = conversion_copies(400);
    let large = conversion_copies(800);

    assert_eq!(
        (small, large),
        (0, 0),
        "converting a map value rebuilt the table it was filling: n=400 copied \
         {small} entries, n=800 copied {large}",
    );
}

#[test]
fn float_roundtrip() {
    for &f in &[0.0, -0.0, 1.0, -1.0, 3.14, f64::INFINITY, f64::NEG_INFINITY] {
        let v = NanValue::new_float(f);
        assert!(v.is_float());
        assert_eq!(v.as_float().to_bits(), f.to_bits());
    }
}

#[test]
fn float_nan_roundtrip() {
    let v = NanValue::new_float(f64::NAN);
    assert!(v.as_float().is_nan());
}

#[test]
fn int_inline_roundtrip() {
    let mut arena = Arena::new();
    for i in [
        0,
        1,
        -1,
        42,
        -42,
        1_000_000,
        -1_000_000,
        INT_INLINE_MAX,
        INT_INLINE_MIN,
    ] {
        let v = NanValue::new_int(i, &mut arena);
        assert!(v.is_int());
        assert_eq!(v.as_int(&arena), i);
    }
    assert_eq!(arena.len(), 0);
}

#[test]
fn int_big_roundtrip() {
    let mut arena = Arena::new();
    for i in [i64::MAX, i64::MIN, INT_INLINE_MAX + 1, INT_INLINE_MIN - 1] {
        let v = NanValue::new_int(i, &mut arena);
        assert!(v.is_int());
        assert_eq!(v.as_int(&arena), i);
    }
    assert_eq!(arena.len(), 4);
}

#[test]
fn new_big_int_demotes_in_range_payload() {
    use aver_rt::AverInt;
    use num_bigint::BigInt;
    let mut arena = Arena::new();

    // A BigInt that fits i64 must NOT allocate a BigInt slot — it canonicalizes
    // to the same representation as `new_int`, so it reads back as `Small` and
    // is value-equal to the directly-built int. Otherwise a Map/Set key built
    // from a Big path would never match the same number built inline.
    let from_big = NanValue::new_big_int(BigInt::from(5), &mut arena);
    let inline = NanValue::new_int(5, &mut arena);
    assert_eq!(arena.len(), 0, "in-range value should not allocate");
    assert_eq!(from_big.as_aver_int(&arena), AverInt::from_i64(5));
    assert_eq!(from_big.as_aver_int(&arena), inline.as_aver_int(&arena));
    assert!(matches!(from_big.as_aver_int(&arena), AverInt::Small(5)));

    // i64 boundary values still demote (no BigInt slot).
    let max = NanValue::new_big_int(BigInt::from(i64::MAX), &mut arena);
    assert_eq!(max.as_aver_int(&arena), AverInt::from_i64(i64::MAX));
    assert!(matches!(max.as_aver_int(&arena), AverInt::Small(_)));

    // A genuinely out-of-range payload stays Big.
    let past = NanValue::new_big_int(BigInt::from(i64::MAX) + 1, &mut arena);
    assert!(matches!(past.as_aver_int(&arena), AverInt::Big(_)));
}

#[test]
fn int_arithmetic_promotes_into_big_range() {
    use aver_rt::AverInt;
    let mut arena = Arena::new();
    // i64::MAX + 1 cannot be an i64; it must round-trip as a Big value.
    let max = NanValue::new_int(i64::MAX, &mut arena);
    let one = NanValue::new_int(1, &mut arena);
    let sum = NanValue::from_aver_int(
        max.as_aver_int(&arena).add(&one.as_aver_int(&arena)),
        &mut arena,
    );
    assert!(sum.is_int());
    assert_eq!(
        sum.as_aver_int(&arena),
        AverInt::from_i64(i64::MAX).add(&AverInt::from_i64(1))
    );
    // And the squared value stays exact and positive (the C0 law).
    let sq = NanValue::from_aver_int(
        max.as_aver_int(&arena).mul(&max.as_aver_int(&arena)),
        &mut arena,
    );
    assert!(sq.as_aver_int(&arena) > AverInt::zero());
}

#[test]
fn immediates() {
    assert!(NanValue::TRUE.is_bool());
    assert!(NanValue::FALSE.is_bool());
    assert!(NanValue::UNIT.is_unit());
    assert!(NanValue::NONE.is_none());
    assert!(!NanValue::TRUE.is_float());
    assert!(!NanValue::UNIT.is_int());
}

#[test]
fn empty_collections_stay_inline() {
    let arena = Arena::new();

    assert!(NanValue::EMPTY_LIST.is_list());
    assert!(NanValue::EMPTY_LIST.heap_index().is_none());
    assert_eq!(NanValue::EMPTY_LIST.repr(&arena), "[]");

    assert!(NanValue::EMPTY_MAP.is_map());
    assert!(NanValue::EMPTY_MAP.heap_index().is_none());
    assert_eq!(NanValue::EMPTY_MAP.repr(&arena), "{}");
}

/// `may_hold_heap_index` is a filter put in front of `heap_index` where the tag
/// match would be the bulk of the work — the interpreter's walk over the operand
/// stack looking for holders of a slot — so it has to be a NECESSARY condition:
/// anything it turns away must really carry no arena index. A false negative
/// there is silent, because the reference is simply never seen and the slot
/// reads as unheld, so this walks every tag rather than the handful the callers
/// happen to use today.
#[test]
fn the_cheap_heap_reference_filter_turns_nothing_heap_backed_away() {
    let mut arena = Arena::new();

    let mut cases = vec![
        NanValue::UNIT,
        NanValue::TRUE,
        NanValue::FALSE,
        NanValue::NONE,
        NanValue::EMPTY_LIST,
        NanValue::EMPTY_MAP,
        NanValue::new_int_inline(0),
        NanValue::new_int_inline(-1),
        NanValue::new_int_inline(crate::nan_value::INT_INLINE_MAX),
        NanValue::new_int_inline(crate::nan_value::INT_INLINE_MIN),
        NanValue::new_float(1.5),
        NanValue::new_float(f64::NAN),
        NanValue::new_some(0),
        NanValue::new_ok(0),
        NanValue::new_err(0),
        NanValue::new_string(arena.push_string("heap backed")),
        NanValue::new_fn(arena.push_symbol(ArenaSymbol::Builtin("Map.set".into()))),
    ];
    // An integer past what the payload holds inline, plus the containers.
    cases.push(NanValue::new_big_int(
        num_bigint::BigInt::from(i64::MAX) + 1,
        &mut arena,
    ));
    cases.push(NanValue::new_tuple(
        arena.push_tuple(vec![NanValue::UNIT, NanValue::TRUE]),
    ));
    cases.push(NanValue::new_vector(arena.push(ArenaEntry::Vector {
        items: vec![NanValue::UNIT],
        held_elsewhere: false,
    })));
    cases.push(NanValue::new_list(arena.push(ArenaEntry::List(
        ArenaList::Flat {
            items: std::sync::Arc::new(crate::nan_value::ListBody::new(vec![NanValue::TRUE])),
            start: 0,
            scan_receipt: 0,
        },
    ))));

    for value in cases {
        if value.heap_index().is_some() {
            assert!(
                value.may_hold_heap_index(),
                "{value:?} carries an arena index the cheap filter turns away, so \
                 every reference to it would go uncounted",
            );
        }
    }
}

#[test]
fn empty_collection_immediates_roundtrip_through_value() {
    use crate::value::Value;

    let mut arena = Arena::new();
    let empty_list = NanValue::from_value(
        &Value::List(aver_rt::AverList::from_vec(Vec::new())),
        &mut arena,
    );
    let empty_map = NanValue::from_value(&Value::Map(std::collections::HashMap::new()), &mut arena);

    assert_eq!(empty_list.bits(), NanValue::EMPTY_LIST.bits());
    assert_eq!(empty_map.bits(), NanValue::EMPTY_MAP.bits());
    assert!(matches!(empty_list.to_value(&arena), Value::List(items) if items.is_empty()));
    assert!(matches!(empty_map.to_value(&arena), Value::Map(map) if map.is_empty()));
}

#[test]
fn wrapper_some_roundtrip() {
    let mut arena = Arena::new();
    let inner = NanValue::new_int_inline(42);
    let idx = arena.push_boxed(inner);
    let v = NanValue::new_some(idx);
    assert!(v.is_some());
    assert_eq!(arena.get_boxed(v.wrapper_index()).as_int(&arena), 42);
}

#[test]
fn wrapper_ok_err_roundtrip() {
    let mut arena = Arena::new();
    let ok_idx = arena.push_boxed(NanValue::new_int_inline(100));
    let ok_val = NanValue::new_ok(ok_idx);
    assert!(ok_val.is_ok());

    let err_str_idx = arena.push_string("error");
    let err_idx = arena.push_boxed(NanValue::new_string(err_str_idx));
    let err_val = NanValue::new_err(err_idx);
    assert!(err_val.is_err());

    assert_eq!(arena.get_boxed(ok_val.wrapper_index()).as_int(&arena), 100);
    let inner = arena.get_boxed(err_val.wrapper_index());
    assert_eq!(arena.get_string_value(inner), "error");
}

#[test]
fn wrapped_immediates_stay_inline() {
    let mut arena = Arena::new();
    let before = arena.len();

    let some_true = NanValue::new_some_value(NanValue::TRUE, &mut arena);
    let ok_unit = NanValue::new_ok_value(NanValue::UNIT, &mut arena);
    let err_none = NanValue::new_err_value(NanValue::NONE, &mut arena);

    assert!(some_true.is_some());
    assert!(ok_unit.is_ok());
    assert!(err_none.is_err());
    assert_eq!(arena.len(), before);
    assert!(some_true.heap_index().is_none());
    assert!(ok_unit.heap_index().is_none());
    assert!(err_none.heap_index().is_none());
    assert!(some_true.wrapper_inner(&arena).is_bool());
    assert!(ok_unit.wrapper_inner(&arena).is_unit());
    assert!(err_none.wrapper_inner(&arena).is_none());
}

#[test]
fn inline_and_boxed_wrapped_immediates_compare_equal() {
    let mut arena = Arena::new();
    let inline_ok = NanValue::new_ok_value(NanValue::TRUE, &mut arena);
    let boxed_ok = NanValue::new_ok(arena.push_boxed(NanValue::TRUE));

    assert!(inline_ok.eq_in(boxed_ok, &arena));
    assert_eq!(inline_ok.repr(&arena), "Result.Ok(true)");
    assert_eq!(boxed_ok.repr(&arena), "Result.Ok(true)");
}

#[test]
fn wrapped_inline_ints_stay_inline() {
    let mut arena = Arena::new();
    let before = arena.len();

    let some_int = NanValue::new_some_value(NanValue::new_int_inline(42), &mut arena);
    let ok_int = NanValue::new_ok_value(NanValue::new_int_inline(-7), &mut arena);
    let err_int = NanValue::new_err_value(NanValue::new_int_inline(1234), &mut arena);

    assert!(some_int.is_some());
    assert!(ok_int.is_ok());
    assert!(err_int.is_err());
    assert_eq!(arena.len(), before);
    assert!(some_int.heap_index().is_none());
    assert!(ok_int.heap_index().is_none());
    assert!(err_int.heap_index().is_none());
    assert_eq!(some_int.wrapper_inner(&arena).as_int(&arena), 42);
    assert_eq!(ok_int.wrapper_inner(&arena).as_int(&arena), -7);
    assert_eq!(err_int.wrapper_inner(&arena).as_int(&arena), 1234);
}

#[test]
fn inline_and_boxed_wrapped_ints_compare_equal() {
    let mut arena = Arena::new();
    let inline_some = NanValue::new_some_value(NanValue::new_int_inline(5), &mut arena);
    let boxed_some = NanValue::new_some(arena.push_boxed(NanValue::new_int_inline(5)));

    assert!(inline_some.eq_in(boxed_some, &arena));
    assert_eq!(inline_some.repr(&arena), "Option.Some(5)");
    assert_eq!(boxed_some.repr(&arena), "Option.Some(5)");
}

#[test]
fn string_roundtrip() {
    let mut arena = Arena::new();
    let idx = arena.push_string("hello");
    let v = NanValue::new_string(idx);
    assert!(v.is_string());
    assert_eq!(arena.get_string_value(v), "hello");
}

#[test]
fn small_string_stays_inline() {
    let mut arena = Arena::new();
    let short = NanValue::new_string_value("hello", &mut arena);
    assert!(short.is_string());
    assert!(short.heap_index().is_none());
    assert_eq!(arena.get_string_value(short), "hello");

    let v = NanValue::new_string_value("", &mut arena);
    assert!(v.is_string());
    assert!(v.heap_index().is_none());
    assert_eq!(arena.get_string_value(v), "");
}

#[test]
fn record_roundtrip() {
    let mut arena = Arena::new();
    let fields = vec![NanValue::new_int_inline(1), NanValue::new_int_inline(2)];
    let idx = arena.push_record(0, fields);
    let v = NanValue::new_record(idx);
    assert!(v.is_record());
    let (tid, fields) = arena.get_record(v.arena_index());
    assert_eq!(tid, 0);
    assert_eq!(fields.len(), 2);
}

#[test]
fn nullary_variants_stay_inline() {
    let mut arena = Arena::new();
    let type_id = arena.register_sum_type("Status", vec!["Todo".into(), "Done".into()]);
    let todo_id = arena.find_variant_id(type_id, "Todo").unwrap();
    let todo_ctor = arena.find_ctor_id(type_id, todo_id).unwrap();

    let todo = NanValue::new_nullary_variant(arena.push_nullary_variant_symbol(todo_ctor));
    assert!(todo.is_variant());
    assert!(todo.heap_index().is_none());
    assert_eq!(todo.repr(&arena), "Todo");

    let value = todo.to_value(&arena);
    match value {
        crate::value::Value::Variant {
            type_name,
            variant,
            fields,
        } => {
            assert_eq!(type_name, "Status");
            assert_eq!(variant, "Todo");
            assert!(fields.is_empty());
        }
        other => panic!("expected nullary variant value, got {other:?}"),
    }
}

#[test]
fn inline_and_boxed_nullary_variants_compare_equal() {
    let mut arena = Arena::new();
    let type_id = arena.register_sum_type("Status", vec!["Todo".into()]);
    let variant_id = arena.find_variant_id(type_id, "Todo").unwrap();
    let ctor_id = arena.find_ctor_id(type_id, variant_id).unwrap();
    let inline = NanValue::new_nullary_variant(arena.push_nullary_variant_symbol(ctor_id));
    let boxed = NanValue::new_variant(arena.push_variant(type_id, variant_id, Vec::new()));

    assert!(inline.eq_in(boxed, &arena));
    assert_eq!(inline.repr(&arena), "Todo");
    assert_eq!(boxed.repr(&arena), "Todo");
}

#[test]
fn list_roundtrip() {
    let mut arena = Arena::new();
    let items = vec![NanValue::new_int_inline(10), NanValue::new_int_inline(20)];
    let idx = arena.push_list(items);
    let v = NanValue::new_list(idx);
    assert!(v.is_list());
    assert_eq!(arena.list_len(v.arena_index()), 2);
}

#[test]
fn prepend_with_empty_immediate_tail_traverses_correctly() {
    let mut arena = Arena::new();
    let list = NanValue::new_list(
        arena.push_list_prepend(NanValue::new_int_inline(7), NanValue::EMPTY_LIST),
    );

    assert_eq!(arena.list_len_value(list), 1);
    assert_eq!(arena.list_get_value(list, 0).unwrap().as_int(&arena), 7);
    assert_eq!(arena.list_to_vec_value(list).len(), 1);

    let (head, tail) = arena.list_uncons(list).expect("prepend should uncons");
    assert_eq!(head.as_int(&arena), 7);
    assert!(tail.is_empty_list_immediate());
}

#[test]
fn concat_uncons_returns_segment_view_tail() {
    let mut arena = Arena::new();
    let left = NanValue::new_list(arena.push_list(vec![
        NanValue::new_int_inline(1),
        NanValue::new_int_inline(2),
    ]));
    let right = NanValue::new_list(arena.push_list(vec![
        NanValue::new_int_inline(3),
        NanValue::new_int_inline(4),
    ]));
    let concat = NanValue::new_list(arena.push_list_concat(left, right));

    let (head1, tail1) = arena.list_uncons(concat).expect("first uncons");
    assert_eq!(head1.as_int(&arena), 1);
    match arena.get_list(tail1.arena_index()) {
        ArenaList::Segments { .. } => {}
        other => panic!("expected segment tail view, got {other:?}"),
    }
    assert_eq!(
        arena
            .list_get(tail1.arena_index(), 0)
            .unwrap()
            .as_int(&arena),
        2
    );
    assert_eq!(
        arena
            .list_get(tail1.arena_index(), 1)
            .unwrap()
            .as_int(&arena),
        3
    );
    assert_eq!(
        arena
            .list_get(tail1.arena_index(), 2)
            .unwrap()
            .as_int(&arena),
        4
    );

    let (head2, tail2) = arena.list_uncons(tail1).expect("second uncons");
    assert_eq!(head2.as_int(&arena), 2);
    assert_eq!(
        arena
            .list_get(tail2.arena_index(), 0)
            .unwrap()
            .as_int(&arena),
        3
    );
    assert_eq!(
        arena
            .list_get(tail2.arena_index(), 1)
            .unwrap()
            .as_int(&arena),
        4
    );
}

#[test]
fn types_dont_collide() {
    let mut arena = Arena::new();
    let f = NanValue::new_float(3.14);
    let i = NanValue::new_int(42, &mut arena);
    let b = NanValue::TRUE;
    let u = NanValue::UNIT;
    let n = NanValue::NONE;

    assert!(f.is_float() && !f.is_int() && !f.is_bool());
    assert!(i.is_int() && !i.is_float() && !i.is_bool());
    assert!(b.is_bool() && !b.is_int() && !b.is_float());
    assert!(u.is_unit() && !u.is_bool() && !u.is_none());
    assert!(n.is_none() && !n.is_unit() && !n.is_bool());
}

#[test]
fn nested_record_in_list() {
    let mut arena = Arena::new();
    let p1 = arena.push_record(
        0,
        vec![NanValue::new_int_inline(1), NanValue::new_int_inline(2)],
    );
    let p2 = arena.push_record(
        0,
        vec![NanValue::new_int_inline(3), NanValue::new_int_inline(4)],
    );
    let list_idx = arena.push_list(vec![NanValue::new_record(p1), NanValue::new_record(p2)]);
    let list = NanValue::new_list(list_idx);

    let second = arena.list_get(list.arena_index(), 1).unwrap();
    let (_, fields) = arena.get_record(second.arena_index());
    assert_eq!(fields[1].as_int(&arena), 4);
}

#[test]
fn eq_inline_values() {
    let arena = Arena::new();
    assert!(NanValue::new_int_inline(42).eq_in(NanValue::new_int_inline(42), &arena));
    assert!(!NanValue::new_int_inline(42).eq_in(NanValue::new_int_inline(43), &arena));
    assert!(NanValue::TRUE.eq_in(NanValue::TRUE, &arena));
    assert!(!NanValue::TRUE.eq_in(NanValue::FALSE, &arena));
    assert!(NanValue::UNIT.eq_in(NanValue::UNIT, &arena));
    assert!(NanValue::new_float(3.14).eq_in(NanValue::new_float(3.14), &arena));
}

#[test]
fn eq_string_by_content() {
    let mut arena = Arena::new();
    let a = NanValue::new_string(arena.push_string("hello"));
    let b = NanValue::new_string(arena.push_string("hello"));
    let c = NanValue::new_string(arena.push_string("world"));
    let empty_inline = NanValue::new_string_value("", &mut arena);
    let empty_boxed = NanValue::new_string(arena.push_string(""));
    assert!(a.eq_in(b, &arena));
    assert!(!a.eq_in(c, &arena));
    assert!(empty_inline.eq_in(empty_boxed, &arena));
}

#[test]
fn repr_basics() {
    let mut arena = Arena::new();
    assert_eq!(NanValue::new_int_inline(42).repr(&arena), "42");
    assert_eq!(NanValue::new_float(3.14).repr(&arena), "3.14");
    assert_eq!(NanValue::TRUE.repr(&arena), "true");
    assert_eq!(NanValue::UNIT.repr(&arena), "Unit");
    assert_eq!(NanValue::NONE.repr(&arena), "Option.None");
    assert_eq!(NanValue::EMPTY_STRING.repr(&arena), "");

    let s = NanValue::new_string(arena.push_string("hi"));
    assert_eq!(s.repr(&arena), "hi");

    let ok_idx = arena.push_boxed(NanValue::new_int_inline(1));
    let ok = NanValue::new_ok(ok_idx);
    assert_eq!(ok.repr(&arena), "Result.Ok(1)");
    let some_true = NanValue::new_some_value(NanValue::TRUE, &mut arena);
    assert_eq!(some_true.repr(&arena), "Option.Some(true)");
}

#[test]
fn value_roundtrip_primitives() {
    use crate::value::Value;
    let mut arena = Arena::new();

    let cases: Vec<Value> = vec![
        Value::int(42),
        Value::int(-1),
        Value::int(i64::MAX),
        Value::Float(3.14),
        Value::Bool(true),
        Value::Bool(false),
        Value::Unit,
        Value::None,
        Value::Str("hello".to_string()),
        Value::Ok(Box::new(Value::int(1))),
        Value::Err(Box::new(Value::Str("bad".to_string()))),
        Value::Some(Box::new(Value::Bool(true))),
    ];

    for val in &cases {
        let nv = NanValue::from_value(val, &mut arena);
        let back = nv.to_value(&arena);
        assert_eq!(
            format!("{:?}", val),
            format!("{:?}", back),
            "roundtrip failed for {:?}",
            val
        );
    }
}

/// The backing allocation of a `Flat` list, kept alive so that identity can be
/// compared by pointer rather than by address.
macro_rules! flat_body {
    ($arena:expr, $list:expr) => {
        match $arena.get_list($list.arena_index()) {
            ArenaList::Flat { items, .. } => items.clone(),
            other => panic!("expected a flat list, got {other:?}"),
        }
    };
}

/// A receipt is an upper bound on when every reference in a collection became
/// reachable, not a license to skip every collection built in the same epoch.
/// Both collections here are created after the frame watermark and point into
/// the suffix that the evacuation drops. Their entries must therefore be read
/// and their children moved before the same young slots are reused.
#[test]
fn post_mark_flat_and_map_receipts_do_not_hide_young_payloads() {
    let mut arena = Arena::new();
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;
    let lane_mark = arena.lane_mark();

    let list_payload = NanValue::new_string(arena.push_string("exact-list-payload"));
    let list = NanValue::new_list(arena.push_list(vec![list_payload]));

    let map_key = NanValue::new_string(arena.push_string("exact-map-key"));
    let map_value = NanValue::new_string(arena.push_string("exact-map-value"));
    let map_hash = map_key.map_key_hash(&arena);
    let map = PersistentMap::new().insert_owned(map_hash, (map_key, map_value));
    let map = NanValue::new_map(arena.push_map(map));

    let lists_before = arena.list_elements_scanned();
    let maps_before = arena.map_entries_scanned();
    let mut roots = [list, map];
    arena.evacuate_frame_to_yard(
        young_mark,
        yard_mark,
        handoff_mark,
        lane_mark,
        &mut roots,
        false,
    );

    assert_eq!(arena.list_elements_scanned() - lists_before, 1);
    assert_eq!(arena.map_entries_scanned() - maps_before, 1);
    assert_eq!(arena.young_len(), young_mark as usize);

    // Reoccupy every raw young slot the two collections and their children
    // used. A skipped child now reads one of these strings instead of merely
    // becoming an out-of-bounds index, making the corruption exact and stable.
    for index in 0..5 {
        arena.push_string(&format!("replacement-slot-{index}"));
    }

    let moved_list_payload = arena.list_get_value(roots[0], 0).expect("list payload");
    assert_eq!(
        arena.get_string_value(moved_list_payload).to_string(),
        "exact-list-payload"
    );

    let mut entries = arena.map_ref_value(roots[1]).iter();
    let (_, (moved_key, moved_value)) = entries.next().expect("map entry");
    assert!(entries.next().is_none());
    assert_eq!(arena.get_string_value(*moved_key), "exact-map-key");
    assert_eq!(arena.get_string_value(*moved_value), "exact-map-value");
}

/// In-place escape repair must descend through a Flat body even when its lane
/// receipt would otherwise prove the body older than the frame. The collection
/// may hide the mutated vector the repair is looking for, so the receipt is not
/// applicable to this traversal.
#[test]
fn an_escape_walk_overrides_a_valid_flat_receipt() {
    let mut arena = Arena::new();
    let items: Vec<NanValue> = (0..8)
        .map(|i| NanValue::new_string(arena.push_string(&format!("resident-{i}"))))
        .collect();
    let list = NanValue::new_list(arena.push_list(items));

    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;
    let lane_mark = arena.lane_mark();
    let local = NanValue::new_string(arena.push_string("frame-local-trigger"));

    let scanned_before = arena.list_elements_scanned();
    let mut roots = [list, local];
    arena.evacuate_frame_to_yard(
        young_mark,
        yard_mark,
        handoff_mark,
        lane_mark,
        &mut roots,
        true,
    );

    assert_eq!(
        arena.list_elements_scanned() - scanned_before,
        8,
        "a valid Flat receipt hid the body from an in-place escape repair",
    );
}

/// The other half of the immediate-body shortcut: a body that does hold heap
/// references must still be walked, and every element must come back readable.
#[test]
fn evacuating_a_flat_list_of_heap_backed_elements_relocates_every_element() {
    let mut arena = Arena::new();
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;

    let items: Vec<NanValue> = (0..8)
        .map(|i| NanValue::new_string(arena.push_string(&format!("s{i}"))))
        .collect();
    let list = NanValue::new_list(arena.push_list(items));

    let mut roots = [list];
    arena.evacuate_frame_to_yard(young_mark, yard_mark, handoff_mark, 0, &mut roots, false);
    assert_eq!(
        arena
            .get_string_value(arena.list_get_value(roots[0], 3).unwrap())
            .to_string(),
        "s3"
    );
}

/// A slot BELOW the marks normally cannot hold an index above them, so the
/// evacuation leaves out-of-region roots alone. The runtime's owned in-place
/// vector write is the one thing that breaks the rule — it stores an arbitrary
/// value into an existing slot — and `rewrite_out_of_region_roots` is how the
/// caller says it happened. With it set, the below-mark vector keeps the
/// element it was just given instead of losing it to the truncate.
#[test]
fn evacuation_rewrites_an_out_of_region_slot_written_in_place() {
    let mut arena = Arena::new();

    // The vector is allocated below the marks, where a caller's would be.
    let vector = NanValue::new_vector(arena.push_vector(vec![NanValue::new_int_inline(0)]));
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;

    // The value is allocated above them, where a returning frame's would be.
    let payload = NanValue::new_string(arena.push_string("written-in-place"));
    arena.get_vector_mut(vector.arena_index())[0] = payload;

    let mut roots = [vector];
    arena.evacuate_frame_to_yard(young_mark, yard_mark, handoff_mark, 0, &mut roots, true);

    let element = arena.vector_ref_value(roots[0])[0];
    assert_eq!(
        arena.get_string_value(element).to_string(),
        "written-in-place",
        "the evacuation dropped the element written into a below-mark slot",
    );
}

/// The other side of that flag, and the reason it exists rather than the
/// descent simply always running: without an in-place write to account for,
/// an out-of-region root is not read at all. Reading one is not free — a frame
/// that carries a long list of strings across every boundary pays for the walk
/// each time, which is quadratic in the length of the list.
#[test]
fn evacuation_reads_an_out_of_region_root_only_when_told_to() {
    let mut arena = Arena::new();
    let items: Vec<NanValue> = (0..64)
        .map(|i| NanValue::new_string(arena.push_string(&format!("s{i}"))))
        .collect();
    let list = NanValue::new_list(arena.push_list(items));
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;

    let scanned = arena.list_elements_scanned();
    let mut roots = [list];
    arena.evacuate_frame_to_yard(young_mark, yard_mark, handoff_mark, 0, &mut roots, false);
    assert_eq!(
        arena.list_elements_scanned() - scanned,
        0,
        "the evacuation walked an out-of-region list with no in-place write to repair",
    );

    let scanned = arena.list_elements_scanned();
    let mut roots = [list];
    arena.evacuate_frame_to_yard(young_mark, yard_mark, handoff_mark, 0, &mut roots, true);
    assert_eq!(
        arena.list_elements_scanned() - scanned,
        64,
        "the evacuation skipped an out-of-region list it had been asked to repair",
    );
}

/// The handoff caller of the same descent. `evacuate_frame_to_handoff` is the
/// ordinary mixed-region return, `evacuate_frame_to_yard` the tail call; they
/// differ only in where the survivors land, so the descent has to work through
/// both doors.
#[test]
fn the_handoff_evacuation_rewrites_an_out_of_region_slot_written_in_place() {
    let mut arena = Arena::new();

    let vector = NanValue::new_vector(arena.push_vector(vec![NanValue::new_int_inline(0)]));
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;

    let payload = NanValue::new_string(arena.push_string("written-in-place"));
    arena.get_vector_mut(vector.arena_index())[0] = payload;

    let mut roots = [vector];
    arena.evacuate_frame_to_handoff(young_mark, yard_mark, handoff_mark, 0, &mut roots, true);

    let element = arena.vector_ref_value(roots[0])[0];
    assert_eq!(
        arena.get_string_value(element).to_string(),
        "written-in-place",
        "the handoff evacuation dropped the element written into a below-mark slot",
    );
}

/// Reaching the same out-of-region slot twice must not descend into it twice.
///
/// The first descent rewrites the slot's reference to the index the survivor
/// will occupy AFTER the compaction — an index inside the target region, which
/// during the walk still holds a live, unrelated entry. A second descent reads
/// that already-rewritten index as if it were an original one and relocates it
/// again, and the slot comes back holding whatever happened to sit there.
///
/// Two roots holding the same vector is the shortest way to write it down; a
/// record or tuple mentioning it twice reaches it the same way.
#[test]
fn evacuation_descends_into_a_twice_reached_out_of_region_slot_once() {
    let mut arena = Arena::new();

    // The vector is allocated below the marks, where a caller's would be.
    let vector = NanValue::new_vector(arena.push_vector(vec![NanValue::new_int_inline(0)]));
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;

    // A frame-local yard entry, so the region the survivors compact into is
    // occupied while the walk is running. Without one the re-relocated index
    // lands past the end of yard and the second descent is a silent no-op.
    let bystander = arena.with_alloc_space(AllocSpace::Yard, |arena| {
        NanValue::new_string(arena.push_string("bystander-in-yard"))
    });
    let payload = NanValue::new_string(arena.push_string("written-in-place"));
    arena.get_vector_mut(vector.arena_index())[0] = payload;

    let mut roots = [vector, vector, bystander];
    arena.evacuate_frame_to_yard(young_mark, yard_mark, handoff_mark, 0, &mut roots, true);

    let element = arena.vector_ref_value(roots[0])[0];
    assert_eq!(
        arena.get_string_value(element).to_string(),
        "written-in-place",
        "the second descent into the same out-of-region slot relocated an \
         index the first one had already rewritten",
    );
}

/// The written vector reached through an out-of-region CONS CHAIN.
///
/// A `Prepend` node's head goes through `evacuate_local_root`, which descends;
/// its tail goes through `take_local_tail_value`, which used to stop at the
/// first out-of-region cell. Everything past that cell was therefore never
/// looked at, so a vector held two cells down kept its stale index. The
/// promotion sibling `take_promote_tail_value` walks straight into an
/// out-of-region cell, so this was never the parity the two sides claimed.
#[test]
fn evacuation_rewrites_a_slot_written_in_place_behind_an_out_of_region_list_tail() {
    let mut arena = Arena::new();

    let vector = NanValue::new_vector(arena.push_vector(vec![NanValue::new_int_inline(0)]));
    let head_marker = NanValue::new_string(arena.push_string("head-marker"));
    let tail_cell = NanValue::new_list(arena.push_list_prepend(vector, NanValue::EMPTY_LIST));
    let list = NanValue::new_list(arena.push_list_prepend(head_marker, tail_cell));

    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;

    // Both frame-local, and both in yard so the stale index stays in bounds
    // after the compaction — the element then reads back as the wrong string
    // rather than as an out-of-bounds panic.
    let (payload, survivor) = arena.with_alloc_space(AllocSpace::Yard, |arena| {
        (
            NanValue::new_string(arena.push_string("written-in-place")),
            NanValue::new_string(arena.push_string("survivor-in-yard")),
        )
    });
    arena.get_vector_mut(vector.arena_index())[0] = payload;

    let mut roots = [list, survivor];
    arena.evacuate_frame_to_yard(young_mark, yard_mark, handoff_mark, 0, &mut roots, true);

    // The vector itself lives below the marks, so its handle is unchanged.
    let element = arena.vector_ref_value(vector)[0];
    assert_eq!(
        arena.get_string_value(element).to_string(),
        "written-in-place",
        "the evacuation stopped at an out-of-region list tail and dropped the \
         element written into a vector the chain still holds",
    );
}

/// The promotion sibling, on the same twice-reached shape, for contrast.
///
/// `promote_young_roots_to_yard` appends every survivor to the END of yard
/// (`take_promote_tail_value` / `promote_value_to_target` both push), so a
/// promoted index can never collide with a live slot and a repeated descent
/// rewrites nothing. That is why the memo the evacuation needs has no sibling
/// on this path — not because the promotion walk remembers where it has been.
#[test]
fn promotion_survives_a_twice_reached_out_of_region_slot() {
    let mut arena = Arena::new();

    let vector = NanValue::new_vector(arena.push_vector(vec![NanValue::new_int_inline(0)]));
    let mark = arena.young_len() as u32;

    let bystander = NanValue::new_string(arena.push_string("bystander-in-young"));
    let payload = NanValue::new_string(arena.push_string("written-in-place"));
    arena.get_vector_mut(vector.arena_index())[0] = payload;

    let mut roots = [vector, vector, bystander];
    arena.promote_young_roots_to_yard(mark, 0, &mut roots);

    let element = arena.vector_ref_value(roots[0])[0];
    assert_eq!(
        arena.get_string_value(element).to_string(),
        "written-in-place",
        "the promotion walk lost an element to a repeated descent",
    );
}

#[test]
fn evacuating_a_flat_list_of_immediates_keeps_its_backing_allocation() {
    let mut arena = Arena::new();
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;

    let items: Vec<NanValue> = (0..64).map(NanValue::new_int_inline).collect();
    let list = NanValue::new_list(arena.push_list(items));
    let before = flat_body!(arena, list);
    let copied_before = arena.list_elements_copied();

    let mut roots = [list];
    arena.evacuate_frame_to_yard(young_mark, yard_mark, handoff_mark, 0, &mut roots, false);

    let after = flat_body!(arena, roots[0]);
    assert!(
        std::sync::Arc::ptr_eq(&before, &after),
        "evacuation rebuilt a list body whose every element relocates to itself",
    );
    assert_eq!(
        arena.list_elements_copied() - copied_before,
        0,
        "evacuation copied elements out of a list that did not move",
    );
    assert_eq!(arena.list_len_value(roots[0]), 64);
    assert_eq!(
        arena.list_get_value(roots[0], 63).unwrap().as_int(&arena),
        63
    );
}

#[test]
fn evacuating_a_sliced_flat_list_keeps_the_shared_allocation_and_the_offset() {
    let mut arena = Arena::new();
    let items: Vec<NanValue> = (0..64).map(NanValue::new_int_inline).collect();
    let list = NanValue::new_list(arena.push_list(items));
    let shared = flat_body!(arena, list);

    // `list_uncons` hands out a view over the same body at `start + 1`; the
    // collector must not turn that O(1) slice back into a copy. The view node
    // is built above the mark, so it is the thing being evacuated.
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;
    let (_, tail) = arena.list_uncons(list).expect("uncons");
    let tail = NanValue::new_list(arena.push_list_prepend(NanValue::new_int_inline(-1), tail));

    let mut roots = [tail];
    arena.evacuate_frame_to_yard(young_mark, yard_mark, handoff_mark, 0, &mut roots, false);

    let body = match arena.get_list(roots[0].arena_index()) {
        ArenaList::Prepend { tail, .. } => flat_body!(arena, *tail),
        other => panic!("expected a prepend node, got {other:?}"),
    };
    assert!(
        std::sync::Arc::ptr_eq(&shared, &body),
        "evacuation rebuilt the shared body behind an O(1) tail slice",
    );
    assert_eq!(arena.list_len_value(roots[0]), 64);
    assert_eq!(arena.list_get_value(roots[0], 1).unwrap().as_int(&arena), 1);
}

/// A list of strings sliced by `list_uncons`, evacuated while every string it
/// holds sits below the mark.
///
/// A body of immediates never reaches the element walk — it is skipped whole —
/// so the two tests below are the only ones that run it, and they are the only
/// ones that can see whether the walk carries `start` with it. Here nothing
/// moves, so the body is returned as it stands and the slice offset must come
/// back with it: dropping the offset re-attaches the elements the slice had
/// already stepped past, and the list silently regrows its head.
#[test]
fn a_walked_body_that_did_not_move_keeps_the_slice_offset() {
    let mut arena = Arena::new();
    let items: Vec<NanValue> = (0..8)
        .map(|i| NanValue::new_string(arena.push_string(&format!("s{i}"))))
        .collect();
    let list = NanValue::new_list(arena.push_list(items));
    let shared = flat_body!(arena, list);

    // Every string is allocated below the mark, so it is not frame-local and
    // relocates to itself. The body still holds heap indices, so it is walked
    // rather than skipped, and the walk finds nothing to rewrite.
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;
    let (_, tail) = arena.list_uncons(list).expect("uncons");
    let copied_before = arena.list_elements_copied();

    let mut roots = [tail];
    arena.evacuate_frame_to_yard(young_mark, yard_mark, handoff_mark, 0, &mut roots, false);

    let body = flat_body!(arena, roots[0]);
    assert!(
        std::sync::Arc::ptr_eq(&shared, &body),
        "evacuation rebuilt a walked body in which nothing moved",
    );
    assert_eq!(
        arena.list_elements_copied() - copied_before,
        0,
        "evacuation copied elements out of a body in which nothing moved",
    );
    assert_eq!(
        arena.list_len_value(roots[0]),
        7,
        "evacuation dropped the slice offset and regrew the list head",
    );
    assert_eq!(
        arena
            .get_string_value(arena.list_get_value(roots[0], 0).unwrap())
            .to_string(),
        "s1",
        "evacuation dropped the slice offset and regrew the list head",
    );
}

/// The same slice, but with one element the collector really does relocate.
///
/// The body has to be rebuilt now, and the rebuild starts by copying the part
/// of the slice already walked. That prefix runs from the slice offset, not
/// from the start of the allocation — copying from the allocation instead
/// re-attaches the stepped-past elements, which is the same silent head regrowth
/// as above and just as invisible to a length-blind test. The element that moves
/// is deliberately not the first one, so the prefix is non-empty.
#[test]
fn rebuilding_a_walked_body_copies_the_prefix_from_the_slice_offset() {
    let mut arena = Arena::new();
    let stays: Vec<NanValue> = (0..4)
        .map(|i| NanValue::new_string(arena.push_string(&format!("stays{i}"))))
        .collect();

    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;

    // Allocated above the mark, so this is the one element that is frame-local
    // and comes back with a different heap index.
    let moves = NanValue::new_string(arena.push_string("moves"));
    let list =
        NanValue::new_list(arena.push_list(vec![stays[0], stays[1], stays[2], moves, stays[3]]));
    let (_, tail) = arena.list_uncons(list).expect("uncons");
    let copied_before = arena.list_elements_copied();

    let mut roots = [tail];
    arena.evacuate_frame_to_yard(young_mark, yard_mark, handoff_mark, 0, &mut roots, false);

    assert_eq!(
        arena.list_len_value(roots[0]),
        4,
        "the rebuild copied from the allocation instead of the slice offset, \
         regrowing the list head",
    );
    let elements: Vec<String> = (0..arena.list_len_value(roots[0]))
        .map(|i| {
            arena
                .get_string_value(arena.list_get_value(roots[0], i).unwrap())
                .to_string()
        })
        .collect();
    assert_eq!(elements, ["stays1", "stays2", "moves", "stays3"]);
    assert_eq!(
        arena.list_elements_copied() - copied_before,
        4,
        "the rebuilt body is not exactly the four elements of the slice",
    );
}

#[test]
fn evacuating_a_segments_node_keeps_the_parts_allocation_when_nothing_moves() {
    let mut arena = Arena::new();
    let left = NanValue::new_list(arena.push_list(vec![
        NanValue::new_int_inline(1),
        NanValue::new_int_inline(2),
    ]));
    let right = NanValue::new_list(arena.push_list(vec![
        NanValue::new_int_inline(3),
        NanValue::new_int_inline(4),
    ]));
    let concat = NanValue::new_list(arena.push_list_concat(left, right));

    // Only the nodes built from here on are frame-local, so the segment parts
    // recorded in `rest` all resolve to themselves during evacuation.
    let young_mark = arena.young_len() as u32;
    let yard_mark = arena.yard_len() as u32;
    let handoff_mark = arena.handoff_len() as u32;
    let lane_mark = arena.lane_mark();

    let (_, tail) = arena.list_uncons(concat).expect("uncons");
    let before = match arena.get_list(tail.arena_index()) {
        ArenaList::Segments { rest, .. } => std::sync::Arc::clone(rest),
        other => panic!("expected a segment view, got {other:?}"),
    };
    let scanned_before = arena.list_elements_scanned();

    let mut roots = [tail];
    arena.evacuate_frame_to_yard(
        young_mark,
        yard_mark,
        handoff_mark,
        lane_mark,
        &mut roots,
        false,
    );

    let after = match arena.get_list(roots[0].arena_index()) {
        ArenaList::Segments { rest, .. } => std::sync::Arc::clone(rest),
        other => panic!("expected a segment view, got {other:?}"),
    };
    assert!(
        std::sync::Arc::ptr_eq(&before, &after),
        "evacuation rebuilt a segment part list in which nothing moved",
    );
    assert_eq!(
        arena.list_elements_scanned() - scanned_before,
        1,
        "Segments incorrectly inherited the Flat receipt skip",
    );
    assert_eq!(arena.list_len_value(roots[0]), 3);
    assert_eq!(arena.list_get_value(roots[0], 2).unwrap().as_int(&arena), 4);
}

/// The third boundary, and the third way to reach the same slot.
///
/// `finalize_frame_return` promotes its roots to stable and then truncates all
/// three younger spaces. Promotion covers a root in young, yard or handoff
/// whatever it holds, because it takes those spaces wholesale rather than by
/// region — but a root that is ALREADY stable moves nowhere, and
/// `promote_value_to_stable` used to hand it straight back without reading it.
/// An owned in-place write into such a vector therefore survived the promotion
/// and died in the truncate right after it.
///
/// The sequence below is what the boundary does, in its order: promote a
/// vector to stable the way an earlier return of the same kind would, mark the
/// frame, write into it from above the mark, promote, truncate.
#[test]
fn the_stable_promotion_rewrites_a_stable_slot_written_in_place() {
    let mut arena = Arena::new();

    let mut resident = [NanValue::new_vector(
        arena.push_vector(vec![NanValue::new_int_inline(0)]),
    )];
    arena.promote_roots_to_stable(&mut resident, false);
    let vector = resident[0];

    let young_mark = arena.young_len() as u32;
    let payload = NanValue::new_string(arena.push_string("written-in-place"));
    arena.get_vector_mut(vector.arena_index())[0] = payload;

    let mut roots = [vector];
    arena.promote_roots_to_stable(&mut roots, true);
    arena.truncate_to(young_mark);
    // Whatever the program allocates next takes the slot the payload had, which
    // is how the loss reads back as a wrong value rather than as a panic.
    let _filler = NanValue::new_string(arena.push_string("JUNK-FILLER-ONE"));

    let element = arena.vector_ref_value(roots[0])[0];
    assert_eq!(
        arena.get_string_value(element).to_string(),
        "written-in-place",
        "the stable promotion dropped the element written into a stable slot",
    );
}

/// The other side of the same flag, on the same boundary.
///
/// Without an in-place write to account for, a stable root is not read at all —
/// and reading one is not free, because everything stable the roots reach is
/// walked. A callback that returns a long list of strings crosses this boundary
/// on every call.
#[test]
fn the_stable_promotion_reads_a_stable_root_only_when_told_to() {
    let mut arena = Arena::new();
    let items: Vec<NanValue> = (0..64)
        .map(|i| NanValue::new_string(arena.push_string(&format!("s{i}"))))
        .collect();
    let mut resident = [NanValue::new_list(arena.push_list(items))];
    arena.promote_roots_to_stable(&mut resident, false);

    let scanned = arena.list_elements_scanned();
    arena.promote_roots_to_stable(&mut resident, false);
    assert_eq!(
        arena.list_elements_scanned() - scanned,
        0,
        "the promotion walked a stable list with no in-place write to repair",
    );

    let scanned = arena.list_elements_scanned();
    arena.promote_roots_to_stable(&mut resident, true);
    assert_eq!(
        arena.list_elements_scanned() - scanned,
        64,
        "the promotion skipped a stable list it had been asked to repair",
    );
}

// ─── `List.drop` shares what destructuring shares (issue #913) ───────────────
//
// `List.drop(xs, n)` should cost what it steps over, not what is left. The
// evidence is structural rather than a stopwatch: the list handed back must be
// a *view* over the body it was given — the same allocation at an advanced
// offset, which is exactly what `list_uncons` hands out — so nothing is copied
// and a walk that steps through a list is linear instead of quadratic.
//
// Every test below drives the real builtin (`List.drop` through `call_nv`),
// not the arena helper underneath it, so a builtin that stops using the helper
// is still caught.

/// Drive the `List.drop` builtin the way the VM does.
fn drop_builtin(arena: &mut Arena, list: NanValue, count: i64) -> NanValue {
    let count = NanValue::new_int(count, arena);
    crate::types::list::call_nv("List.drop", &[list, count], arena)
        .expect("List.drop is owned by the list namespace")
        .expect("List.drop over a list and an int")
}

fn list_contents(arena: &Arena, list: NanValue) -> Vec<i64> {
    (0..arena.list_len_value(list))
        .map(|i| arena.list_get_value(list, i).unwrap().as_int(arena))
        .collect()
}

/// The load-bearing one: a flat body is shared, and only the offset moves.
#[test]
fn dropping_a_prefix_shares_the_list_body_and_advances_the_offset() {
    let mut arena = Arena::new();
    let items: Vec<NanValue> = (0..64).map(NanValue::new_int_inline).collect();
    let list = NanValue::new_list(arena.push_list(items));
    let shared = flat_body!(arena, list);

    let dropped = drop_builtin(&mut arena, list, 16);

    let (body, start) = match arena.get_list(dropped.arena_index()) {
        ArenaList::Flat { items, start, .. } => (items.clone(), *start),
        other => panic!("expected a flat view, got {other:?}"),
    };
    assert!(
        std::sync::Arc::ptr_eq(&shared, &body),
        "List.drop copied the remainder into a fresh body instead of sharing \
         the one it was given",
    );
    assert_eq!(start, 16, "List.drop did not advance the slice offset");
    assert_eq!(arena.list_len_value(dropped), 48);
    assert_eq!(arena.list_get_value(dropped, 0).unwrap().as_int(&arena), 16);
    assert_eq!(
        arena.list_get_value(dropped, 47).unwrap().as_int(&arena),
        63
    );
}

/// Dropping nothing must hand the list straight back rather than rebuild it.
#[test]
fn dropping_nothing_returns_the_list_it_was_given() {
    let mut arena = Arena::new();
    let items: Vec<NanValue> = (0..8).map(NanValue::new_int_inline).collect();
    let list = NanValue::new_list(arena.push_list(items));

    for count in [0, -1, -1000] {
        let dropped = drop_builtin(&mut arena, list, count);
        assert_eq!(
            dropped.bits(),
            list.bits(),
            "List.drop({count}) rebuilt a list it was not asked to step into",
        );
    }
}

/// Stepping past the end yields the canonical empty list — the same value
/// unconsing to the end yields, so the two walks stay interchangeable.
#[test]
fn dropping_past_the_end_yields_the_canonical_empty_list() {
    let mut arena = Arena::new();
    let items: Vec<NanValue> = (0..3).map(NanValue::new_int_inline).collect();
    let list = NanValue::new_list(arena.push_list(items));

    let mut unconsed = list;
    while let Some((_, tail)) = arena.list_uncons(unconsed) {
        unconsed = tail;
    }

    for count in [3, 4, 4_000] {
        let dropped = drop_builtin(&mut arena, list, count);
        assert!(
            arena.list_is_empty_value(dropped),
            "List.drop({count}) over a 3-element list is not empty",
        );
        assert_eq!(
            dropped.bits(),
            unconsed.bits(),
            "List.drop({count}) does not agree with unconsing to the end",
        );
    }
}

/// A prepend chain has no offset to advance, so the walk steps link by link —
/// what repeated `list_uncons` does — and lands on the shared body underneath.
#[test]
fn dropping_through_a_prepend_chain_lands_on_the_shared_body() {
    let mut arena = Arena::new();
    let items: Vec<NanValue> = (2..6).map(NanValue::new_int_inline).collect();
    let base = NanValue::new_list(arena.push_list(items));
    let shared = flat_body!(arena, base);
    let one = NanValue::new_list(arena.push_list_prepend(NanValue::new_int_inline(1), base));
    let list = NanValue::new_list(arena.push_list_prepend(NanValue::new_int_inline(0), one));

    let dropped = drop_builtin(&mut arena, list, 2);

    let body = flat_body!(arena, dropped);
    assert!(
        std::sync::Arc::ptr_eq(&shared, &body),
        "stepping over a prepend chain rebuilt the body it arrived at",
    );
    assert_eq!(list_contents(&arena, dropped), vec![2, 3, 4, 5]);
}

/// Landing inside the right half of a concat shares the right half's body.
#[test]
fn dropping_into_the_right_half_of_a_concat_shares_its_body() {
    let mut arena = Arena::new();
    let left_items: Vec<NanValue> = (0..2).map(NanValue::new_int_inline).collect();
    let left = NanValue::new_list(arena.push_list(left_items));
    let right_items: Vec<NanValue> = (2..6).map(NanValue::new_int_inline).collect();
    let right = NanValue::new_list(arena.push_list(right_items));
    let shared = flat_body!(arena, right);
    let joined = NanValue::new_list(arena.push_list_concat(left, right));

    let dropped = drop_builtin(&mut arena, joined, 3);

    let (body, start) = match arena.get_list(dropped.arena_index()) {
        ArenaList::Flat { items, start, .. } => (items.clone(), *start),
        other => panic!("expected a flat view, got {other:?}"),
    };
    assert!(
        std::sync::Arc::ptr_eq(&shared, &body),
        "stepping into the right half of a concat rebuilt it",
    );
    assert_eq!(start, 1);
    assert_eq!(list_contents(&arena, dropped), vec![3, 4, 5]);
}

/// Stepping into the LEFT half must keep the right half attached. This is the
/// trap `list_uncons` carries a comment about: a node reached down the left
/// spine of a concat has right-siblings waiting, and forgetting them silently
/// deletes everything after the step.
#[test]
fn dropping_into_the_left_half_of_a_concat_keeps_the_right_half() {
    let mut arena = Arena::new();
    let left_items: Vec<NanValue> = (0..4).map(NanValue::new_int_inline).collect();
    let left = NanValue::new_list(arena.push_list(left_items));
    let right_items: Vec<NanValue> = (4..7).map(NanValue::new_int_inline).collect();
    let right = NanValue::new_list(arena.push_list(right_items));
    let joined = NanValue::new_list(arena.push_list_concat(left, right));

    let dropped = drop_builtin(&mut arena, joined, 2);

    assert_eq!(list_contents(&arena, dropped), vec![2, 3, 4, 5, 6]);
}

/// The same trap one shape deeper: a segmented list on the left of a concat.
/// This is the shape `list_uncons` had to grow its `parts ++ rights` fold for.
#[test]
fn dropping_into_a_segmented_left_half_keeps_the_right_half() {
    let mut arena = Arena::new();
    let mut appended = NanValue::new_list(arena.push_list(vec![NanValue::new_int_inline(0)]));
    for value in 1..200 {
        let next = arena.push_list_append(appended, NanValue::new_int_inline(value));
        appended = NanValue::new_list(next);
    }
    let right = NanValue::new_list(arena.push_list(vec![NanValue::new_int_inline(999)]));
    let joined = NanValue::new_list(arena.push_list_concat(appended, right));

    let dropped = drop_builtin(&mut arena, joined, 150);

    let contents = list_contents(&arena, dropped);
    assert_eq!(
        contents.len(),
        51,
        "stepping over a segmented list lost elements",
    );
    assert_eq!(contents.first().copied(), Some(150));
    assert_eq!(contents.last().copied(), Some(999));
}

/// Walking a list by repeated `List.drop` must agree with walking it by
/// repeated `list_uncons`, on every shape the arena can hold.
#[test]
fn walking_by_drop_agrees_with_walking_by_uncons() {
    let mut arena = Arena::new();
    let flat_items: Vec<NanValue> = (0..40).map(NanValue::new_int_inline).collect();
    let flat = NanValue::new_list(arena.push_list(flat_items));
    let mut prepended = flat;
    for value in (100..110).rev() {
        let next = arena.push_list_prepend(NanValue::new_int_inline(value), prepended);
        prepended = NanValue::new_list(next);
    }
    let mut appended = NanValue::new_list(arena.push_list(vec![NanValue::new_int_inline(0)]));
    for value in 1..300 {
        let next = arena.push_list_append(appended, NanValue::new_int_inline(value));
        appended = NanValue::new_list(next);
    }
    let concat = NanValue::new_list(arena.push_list_concat(prepended, appended));

    for list in [flat, prepended, appended, concat] {
        for step in [1, 3, 7, 64] {
            let mut by_drop = list;
            let mut by_uncons = list;
            loop {
                let expected = list_contents(&arena, by_uncons);
                assert_eq!(
                    list_contents(&arena, by_drop),
                    expected,
                    "a drop-walk in steps of {step} diverged from an uncons-walk",
                );
                if expected.is_empty() {
                    break;
                }
                by_drop = drop_builtin(&mut arena, by_drop, step);
                for _ in 0..step {
                    by_uncons = match arena.list_uncons(by_uncons) {
                        Some((_, tail)) => tail,
                        None => break,
                    };
                }
            }
        }
    }
}
