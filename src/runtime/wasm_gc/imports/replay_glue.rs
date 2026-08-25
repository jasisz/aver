//! Recording / replay state-machine glue: `record_effect_if_recording`
//! appends to the trace in Record mode, `try_replay` short-circuits
//! a real call when the host is in Replay mode. The `json_*` helpers
//! build the marker-shape `JsonValue`s the recorder serialises
//! (`$ok` / `$err` / `$some` / `$none` / `$record`).

use super::super::RunWasmGcHost;

/// Append one entry to the trace iff recording is on. Args + outcome
/// land as `aver::replay::JsonValue` so the wasm-gc trace is
/// byte-compatible with the VM's — `aver replay <file>` consumes
/// either without branching on backend. No-op in Replay mode (the
/// `EffectReplayState::replay_effect` path already advances position).
pub(crate) fn record_effect_if_recording(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    effect_type: &str,
    args: Vec<aver::replay::JsonValue>,
    outcome: aver::replay::JsonValue,
    caller_fn: &str,
) {
    if let Some(rec) = caller.data_mut().recorder.as_mut()
        && rec.mode() == aver::replay::EffectReplayMode::Record
    {
        rec.record_effect(
            effect_type,
            args,
            aver::replay::RecordedOutcome::Value(outcome),
            caller_fn,
            0,
        );
    }
}
/// Pull the next outcome from the replay trace if the host is in
/// Replay mode. Returns `Ok(Some(outcome))` when the trace had a
/// matching entry, `Ok(None)` when not in Replay mode (caller falls
/// back to running the real effect), and `Err(...)` when replay
/// rejects the call (sequence mismatch, exhausted trace, args
/// diverge under `--check-args`).
pub(crate) fn try_replay(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    effect_type: &str,
    args: Vec<aver::replay::JsonValue>,
) -> Result<Option<aver::replay::JsonValue>, wasmtime::Error> {
    let in_replay = caller
        .data()
        .recorder
        .as_ref()
        .is_some_and(|r| r.mode() == aver::replay::EffectReplayMode::Replay);
    if !in_replay {
        return Ok(None);
    }
    let outcome = caller
        .data_mut()
        .recorder
        .as_mut()
        .expect("checked above")
        .replay_effect(effect_type, Some(args))
        .map_err(|e| wasmtime::Error::msg(format!("replay {}: {:?}", effect_type, e)))?;
    match outcome {
        aver::replay::RecordedOutcome::Value(json) => Ok(Some(json)),
        aver::replay::RecordedOutcome::RuntimeError(msg) => Err(wasmtime::Error::msg(format!(
            "replay {}: trace recorded a runtime error ({})",
            effect_type, msg
        ))),
    }
}

/// Build the recorder-side JSON for an Aver `Result.Ok(inner)`. Marker
/// shape (`{"$ok": <inner>}`) matches `replay::value_to_json` so VM
/// and wasm-gc traces stay byte-identical for the same effect.
pub(crate) fn json_ok(inner: aver::replay::JsonValue) -> aver::replay::JsonValue {
    let mut obj = std::collections::BTreeMap::new();
    obj.insert("$ok".to_string(), inner);
    aver::replay::JsonValue::Object(obj)
}

/// Build the recorder-side JSON for an Aver `Result.Err(message)`.
/// Always wraps a `String` payload — every host-side `Result` we
/// emit today carries `String` errors, matching the
/// `aver_rt::*` Rust signatures.
pub(crate) fn json_err(msg: &str) -> aver::replay::JsonValue {
    let mut obj = std::collections::BTreeMap::new();
    obj.insert(
        "$err".to_string(),
        aver::replay::JsonValue::String(msg.to_string()),
    );
    aver::replay::JsonValue::Object(obj)
}

/// Build the recorder-side JSON for an Aver `Option.Some(inner)`.
pub(crate) fn json_some(inner: aver::replay::JsonValue) -> aver::replay::JsonValue {
    let mut obj = std::collections::BTreeMap::new();
    obj.insert("$some".to_string(), inner);
    aver::replay::JsonValue::Object(obj)
}

/// Build the recorder-side JSON for an Aver `Option.None`.
pub(crate) fn json_none() -> aver::replay::JsonValue {
    let mut obj = std::collections::BTreeMap::new();
    obj.insert("$none".to_string(), aver::replay::JsonValue::Bool(true));
    aver::replay::JsonValue::Object(obj)
}

/// Build the recorder-side JSON for an Aver record value
/// (`{"$record": {"type": <name>, "fields": {<k>: <v>, …}}}`).
/// Used for `Http.Response`, `Tcp.Connection`, `Terminal.Size`.
pub(crate) fn json_record(
    type_name: &str,
    fields: Vec<(&str, aver::replay::JsonValue)>,
) -> aver::replay::JsonValue {
    let mut fields_obj = std::collections::BTreeMap::new();
    for (k, v) in fields {
        fields_obj.insert(k.to_string(), v);
    }
    let mut payload = std::collections::BTreeMap::new();
    payload.insert(
        "type".to_string(),
        aver::replay::JsonValue::String(type_name.to_string()),
    );
    payload.insert(
        "fields".to_string(),
        aver::replay::JsonValue::Object(fields_obj),
    );
    let mut wrapper = std::collections::BTreeMap::new();
    wrapper.insert(
        "$record".to_string(),
        aver::replay::JsonValue::Object(payload),
    );
    aver::replay::JsonValue::Object(wrapper)
}
