//! Trace — opaque builtin exposing a function's structural trace in
//! verify-trace assertions.
//!
//! In Oracle v1, `fn.trace` returns a `Trace` value representing the
//! sequence of effect emissions that occurred during an evaluation of
//! `fn` under a verify-law's stub bindings. Users query it via:
//!
//! - `.event(k) : Option<EffectEvent>` — event at position k
//! - `.length() : Int` — number of events
//! - `.contains(event) : Bool` — does trace contain this exact event
//! - `.contains(effect_ref) : Bool` — any event with this effect type
//!
//! Navigation primitives (`.group(N)`, `.branch(idx)`, `.path()`) and
//! the replay-bridge methods follow in later commits.
//!
//! Internal representation: a record `{ events: List<EffectEvent> }`.
//! User code never constructs Trace values directly — they materialize
//! only as the result of `fn.trace` in verify-trace context.

use crate::nan_value::Arena;

pub const TYPE_NAME: &str = "Trace";
pub const FIELD_EVENTS: &str = "events";

pub fn register(arena: &mut Arena) {
    arena.register_record_type(TYPE_NAME, vec![FIELD_EVENTS.into()]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trace_type_registers_with_events_field() {
        let mut arena = Arena::new();
        register(&mut arena);
        assert!(arena.find_type_id(TYPE_NAME).is_some());
    }
}
