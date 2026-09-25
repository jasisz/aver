//! `Run.*` recorder imports. The module keeps the reason a run failed in a
//! global of its own (see `codegen/wasm_gc/run_fail.rs`); these two imports
//! are how the recording sees `Run.fail` and the loop's `Run.failure`
//! reading, in the same entries the VM writes, and how a replay hands the
//! recorded reading back.

use super::super::RunWasmGcHost;
use super::lm::{lm_string_from_host, lm_string_to_host};
use super::replay_glue::{json_none, json_some, record_effect_if_recording, try_replay};

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use aver::replay::JsonValue;
    use wasmtime::Val;
    match name {
        "run_fail" => {
            let message = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![JsonValue::String(message)];
            if try_replay(caller, "Run.fail", args.clone())?.is_some() {
                return Ok(true);
            }
            record_effect_if_recording(caller, "Run.fail", args, JsonValue::Null, caller_fn);
            Ok(true)
        }
        "run_failure" => {
            if let Some(cached) = try_replay(caller, "Run.failure", vec![])? {
                results[0] = Val::AnyRef(recorded_reason(caller, &cached)?);
                return Ok(true);
            }
            let reason = lm_string_to_host(caller, params.first())?;
            results[0] = params.first().cloned().unwrap_or(Val::AnyRef(None));
            let outcome = match reason {
                Some(reason) => json_some(JsonValue::String(reason)),
                None => json_none(),
            };
            record_effect_if_recording(caller, "Run.failure", vec![], outcome, caller_fn);
            Ok(true)
        }
        _ => Ok(false),
    }
}

/// The reason a recording holds, as the string ref (or null) the module reads.
fn recorded_reason(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    cached: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use aver::replay::JsonValue;
    let JsonValue::Object(marker) = cached else {
        return Err(wasmtime::Error::msg(
            "replay Run.failure: not an Option<String>",
        ));
    };
    if marker.contains_key("$none") {
        return Ok(None);
    }
    match marker.get("$some") {
        Some(JsonValue::String(reason)) => lm_string_from_host(caller, reason),
        _ => Err(wasmtime::Error::msg(
            "replay Run.failure: not an Option<String>",
        )),
    }
}
