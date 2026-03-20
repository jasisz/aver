use super::*;

#[test]
fn size_is_8_bytes() {
    assert_eq!(std::mem::size_of::<NanValue>(), 8);
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
fn immediates() {
    assert!(NanValue::TRUE.is_bool());
    assert!(NanValue::FALSE.is_bool());
    assert!(NanValue::UNIT.is_unit());
    assert!(NanValue::NONE.is_none());
    assert!(!NanValue::TRUE.is_float());
    assert!(!NanValue::UNIT.is_int());
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
    assert_eq!(arena.get_string(inner.arena_index()), "error");
}

#[test]
fn string_roundtrip() {
    let mut arena = Arena::new();
    let idx = arena.push_string("hello");
    let v = NanValue::new_string(idx);
    assert!(v.is_string());
    assert_eq!(arena.get_string(v.arena_index()), "hello");
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
fn list_roundtrip() {
    let mut arena = Arena::new();
    let items = vec![NanValue::new_int_inline(10), NanValue::new_int_inline(20)];
    let idx = arena.push_list(items);
    let v = NanValue::new_list(idx);
    assert!(v.is_list());
    assert_eq!(arena.list_len(v.arena_index()), 2);
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
    assert!(a.eq_in(b, &arena));
    assert!(!a.eq_in(c, &arena));
}

#[test]
fn repr_basics() {
    let mut arena = Arena::new();
    assert_eq!(NanValue::new_int_inline(42).repr(&arena), "42");
    assert_eq!(NanValue::new_float(3.14).repr(&arena), "3.14");
    assert_eq!(NanValue::TRUE.repr(&arena), "true");
    assert_eq!(NanValue::UNIT.repr(&arena), "Unit");
    assert_eq!(NanValue::NONE.repr(&arena), "Option.None");

    let s = NanValue::new_string(arena.push_string("hi"));
    assert_eq!(s.repr(&arena), "hi");

    let ok_idx = arena.push_boxed(NanValue::new_int_inline(1));
    let ok = NanValue::new_ok(ok_idx);
    assert_eq!(ok.repr(&arena), "Result.Ok(1)");
}

#[test]
fn value_roundtrip_primitives() {
    use crate::value::Value;
    let mut arena = Arena::new();

    let cases: Vec<Value> = vec![
        Value::Int(42),
        Value::Int(-1),
        Value::Int(i64::MAX),
        Value::Float(3.14),
        Value::Bool(true),
        Value::Bool(false),
        Value::Unit,
        Value::None,
        Value::Str("hello".to_string()),
        Value::Ok(Box::new(Value::Int(1))),
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
