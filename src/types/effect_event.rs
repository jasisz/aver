//! EffectEvent — opaque builtin representing one recorded effect emission.
//!
//! In Oracle v1's trace API, each call to `trace.event(k)` returns an
//! `Option<EffectEvent>`. The event carries enough data to answer:
//!
//! - "What effect method was emitted?" → `event.is(Console.print)`
//! - "What value was passed?" → `match event ... Some(Console.print(msg))`
//! - "Is it equal to this literal?" → `trace.contains(Console.print("hi"))`
//!
//! User code never constructs `EffectEvent` directly. Three elaboration
//! paths in verify-trace context produce event values:
//!
//! - Expression position: `Console.print("x")` → event literal.
//! - Pattern position: `Console.print(msg)` in a `match` arm → destructuring.
//! - Bare reference: `Console.print` → effect-type predicate (used with
//!   `event.is(...)` and overloaded `trace.contains(...)`).
//!
//! Internal representation: `{ method: String, args: List<Value> }` where
//! `args` carries the runtime values emitted, typed per the effect
//! method's signature (polymorphic `T` for `Console.print`, concrete types
//! for classified generative effects).

use crate::nan_value::Arena;

pub const TYPE_NAME: &str = "EffectEvent";
pub const FIELD_METHOD: &str = "method";
pub const FIELD_ARGS: &str = "args";
/// Dewey-decimal path string identifying the structural position
/// where the event was emitted. Empty for sequential-level events
/// (outside any `!`/`?!` group) — matches the `BranchPath.root`
/// convention. `BranchPath.parse(event.path)` validates and round-trips the
/// value as `Result<BranchPath, String>`.
pub const FIELD_PATH: &str = "path";

/// Construct the arena type registration. Called alongside other builtin
/// record types from `vm::register_service_types`.
pub fn register(arena: &mut Arena) {
    arena.register_record_type(
        TYPE_NAME,
        vec![FIELD_METHOD.into(), FIELD_ARGS.into(), FIELD_PATH.into()],
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nan_value::Arena;

    #[test]
    fn registers_type_with_expected_fields() {
        let mut arena = Arena::new();
        register(&mut arena);
        let type_id = arena
            .find_type_id(TYPE_NAME)
            .expect("EffectEvent must register");
        // Round-trip a record with the expected field arity to confirm the
        // registration actually wired through. Fields come back in the order
        // they were registered.
        let method = crate::nan_value::NanValue::new_string_value("Console.print", &mut arena);
        let args = crate::nan_value::NanValue::new_string_value("[]", &mut arena);
        let idx = arena.push_record(type_id, vec![method, args]);
        let nv = crate::nan_value::NanValue::new_record(idx);
        let (tid, fields) = arena.get_record(nv.arena_index());
        assert_eq!(tid, type_id);
        assert_eq!(fields.len(), 2);
    }
}
