//! Property tests for the replay JSON codec (Iron — B3).
//!
//! Every replay-safe `Value` must round-trip through the replay JSON
//! encoder + decoder without information loss. The codec is the
//! contract between `aver run --record` (writes JSON, emits trace) and
//! `aver replay --check` (reads JSON, fakes effects, asserts results
//! match) — if encode→decode loses a bit, replay determinism is no
//! longer the invariant the system advertises.
//!
//! The hand-picked tests in `tests/wasm_gc_record_replay_spec.rs` cover
//! the end-to-end pipeline (compile → run → record → replay) on a
//! fixed set of programs; these property tests probe the codec itself
//! across the full set of replay-safe `Value` shapes the generator
//! produces. The two layers are complementary — end-to-end finds bugs
//! in the recording state machine + host shim, the property tests
//! find bugs in the JSON serialisation primitives that the rest sits
//! on.
//!
//! Out of scope by design:
//!   - `Value::Fn` / `Value::Builtin` / `Value::Namespace` — these
//!     return `Err` from `value_to_json` and the generator skips
//!     them. Per-fn closures aren't replayable across runs anyway
//!     (they reference compiled bytecode positions).
//!   - Non-finite floats (NaN / ±inf) — `value_to_json` rejects them
//!     explicitly; the generator stays in the finite range.

use aver::replay::json::{json_to_value, value_to_json};
use aver::value::{Value, list_from_vec, list_to_vec};
use proptest::prelude::*;
use std::collections::HashMap;
use std::sync::Arc as Rc;

/// Normalise `Value::Record` field order alphabetically by name —
/// recursively, so nested records inside lists / tuples / maps /
/// other records also get normalised. The codec round-trip
/// produces alphabetically-sorted record fields (canonical-JSON
/// design choice) but the source can be in any order; this helper
/// lets the property test compare both sides on the same footing
/// without changing `Value::PartialEq`'s semantics (positional
/// slice equality, which other call sites depend on).
fn normalize_record_field_order(value: Value) -> Value {
    match value {
        Value::Record { type_name, fields } => {
            let mut owned: Vec<(String, Value)> = fields
                .iter()
                .map(|(n, v)| (n.clone(), normalize_record_field_order(v.clone())))
                .collect();
            owned.sort_by(|a, b| a.0.cmp(&b.0));
            Value::Record {
                type_name,
                fields: Rc::from(owned),
            }
        }
        Value::Ok(inner) => Value::Ok(Box::new(normalize_record_field_order(*inner))),
        Value::Err(inner) => Value::Err(Box::new(normalize_record_field_order(*inner))),
        Value::Some(inner) => Value::Some(Box::new(normalize_record_field_order(*inner))),
        Value::Tuple(items) => Value::Tuple(
            items
                .into_iter()
                .map(normalize_record_field_order)
                .collect(),
        ),
        Value::List(_) => {
            // `list_to_vec` walks the cons cells; normalise each
            // element then rebuild via `list_from_vec`.
            let items = list_to_vec(&value).expect("list_to_vec on Value::List always succeeds");
            list_from_vec(
                items
                    .into_iter()
                    .map(normalize_record_field_order)
                    .collect(),
            )
        }
        Value::Vector(vec) => Value::Vector(aver_rt::AverVector::from_vec(
            vec.iter()
                .cloned()
                .map(normalize_record_field_order)
                .collect(),
        )),
        Value::Map(m) => {
            let mut out = HashMap::with_capacity(m.len());
            for (k, v) in m {
                out.insert(
                    normalize_record_field_order(k),
                    normalize_record_field_order(v),
                );
            }
            Value::Map(out)
        }
        Value::Variant {
            type_name,
            variant,
            fields,
        } => Value::Variant {
            type_name,
            variant,
            fields: Rc::from(
                fields
                    .iter()
                    .cloned()
                    .map(normalize_record_field_order)
                    .collect::<Vec<_>>(),
            ),
        },
        other => other,
    }
}

/// Generator for `Value`s that the replay codec advertises as
/// round-trippable. Recursive, depth- and breadth-capped so a single
/// property case stays bounded.
fn arb_replay_safe_value() -> impl Strategy<Value = Value> {
    let leaf = prop_oneof![
        any::<i64>().prop_map(Value::Int),
        // Stay in the finite range — `value_to_json` rejects NaN/inf.
        prop::num::f64::POSITIVE
            .prop_filter("finite only", |f| f.is_finite())
            .prop_map(Value::Float),
        prop::num::f64::NEGATIVE
            .prop_filter("finite only", |f| f.is_finite())
            .prop_map(Value::Float),
        Just(Value::Float(0.0)),
        ".{0,20}".prop_map(Value::Str),
        any::<bool>().prop_map(Value::Bool),
        Just(Value::Unit),
        Just(Value::None),
    ];

    leaf.prop_recursive(4, 32, 4, |inner| {
        // Each branch is `.boxed()` to a common `BoxedStrategy<Value>`
        // so `prop_oneof!` has a single concrete type to dispatch on.
        // Without the box, each branch's inferred Strategy type leaks
        // a different closure generic, blocking the dispatch.
        prop_oneof![
            inner.clone().prop_map(|v| Value::Ok(Box::new(v))).boxed(),
            inner.clone().prop_map(|v| Value::Err(Box::new(v))).boxed(),
            inner.clone().prop_map(|v| Value::Some(Box::new(v))).boxed(),
            prop::collection::vec(inner.clone(), 0..4)
                .prop_map(list_from_vec)
                .boxed(),
            prop::collection::vec(inner.clone(), 0..4)
                .prop_map(Value::Tuple)
                .boxed(),
            prop::collection::vec(inner.clone(), 0..4)
                .prop_map(|v| Value::Vector(aver_rt::AverVector::from_vec(v)))
                .boxed(),
            // Maps with String keys only — the codec routes
            // string-keyed maps to a JSON Object and other shapes to
            // a `$map` array of pairs. Both paths should round-trip,
            // but the string-key shape is the common case in user
            // code so the generator prioritises it.
            prop::collection::vec(("[a-z]{1,6}".prop_map(Value::Str), inner.clone()), 0..4,)
                .prop_map(|pairs| {
                    let mut m = HashMap::new();
                    for (k, v) in pairs {
                        m.insert(k, v);
                    }
                    Value::Map(m)
                })
                .boxed(),
            // Record — type_name + field list. Field names are
            // identifier-shaped, field values recurse.
            (
                "[A-Z][a-z]{0,4}",
                prop::collection::vec(("[a-z]{1,4}", inner.clone()), 1..3),
            )
                .prop_map(|(type_name, fields)| Value::Record {
                    type_name: type_name.to_string(),
                    fields: Rc::from(
                        fields
                            .into_iter()
                            .map(|(n, v)| (n.to_string(), v))
                            .collect::<Vec<_>>(),
                    ),
                })
                .boxed(),
            // Variant — fully-qualified constructor + positional
            // fields.
            (
                "[A-Z][a-z]{0,4}",
                "[A-Z][a-z]{0,4}",
                prop::collection::vec(inner.clone(), 0..3),
            )
                .prop_map(|(type_name, variant, fields)| Value::Variant {
                    type_name: type_name.to_string(),
                    variant: variant.to_string(),
                    fields: Rc::from(fields),
                })
                .boxed(),
        ]
    })
}

proptest! {
    /// Replay codec round-trip — for every replay-safe `Value`, the
    /// sequence `value_to_json(v).and_then(json_to_value)` must
    /// return a value that compares equal to the original up to
    /// `Record`-field reordering. The codec uses BTreeMap when
    /// emitting record/map JSON so the wire form is canonical
    /// (same record → same bytes regardless of source field order),
    /// which means the decoded `Value::Record` ends up with fields
    /// in alphabetical order even if the input had them in source
    /// order. We accept that — canonical JSON is a deliberate
    /// design choice the recording/replay diff workflow depends
    /// on — and compare after normalising both sides.
    ///
    /// What this property still catches strictly: any *value-level*
    /// loss in the codec. Integers, floats (bit-equal via
    /// `to_bits` in `PartialEq`), strings, list/tuple/vector
    /// contents, variant constructor + field positions, Map
    /// contents, etc. all must round-trip exactly.
    ///
    /// Iron — B3 found-and-fixed:
    ///   - `Value::Vector` encoded with marker `$vector` but the
    ///     decoder had no matching arm; any replay JSON containing
    ///     a Vector failed to load. Fixed in `decode_marker`.
    #[test]
    fn replay_codec_round_trips_every_safe_value(value in arb_replay_safe_value()) {
        let json = value_to_json(&value).map_err(|e| TestCaseError::fail(format!(
            "value_to_json failed on replay-safe value: {e}"
        )))?;
        let decoded = json_to_value(&json).map_err(|e| TestCaseError::fail(format!(
            "json_to_value failed on encoded JSON: {e}"
        )))?;
        let lhs = normalize_record_field_order(decoded);
        let rhs = normalize_record_field_order(value);
        prop_assert_eq!(lhs, rhs);
    }

    /// Encoding determinism — encoding the same value twice produces
    /// the same JSON. Catches any implicit dependency on RNG, hash
    /// iteration order, or mutable internal state in the codec.
    /// This is the property that lets a recorded JSON file be a
    /// deterministic fingerprint of the value, regardless of which
    /// run produced it.
    #[test]
    fn replay_encoding_is_deterministic(value in arb_replay_safe_value()) {
        let j1 = value_to_json(&value);
        let j2 = value_to_json(&value);
        prop_assert_eq!(j1, j2);
    }
}
