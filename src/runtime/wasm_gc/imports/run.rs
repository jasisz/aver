//! `Run.*` recorder imports. The module keeps the reason a run failed in a
//! global of its own (see `codegen/wasm_gc/run_fail.rs`); these two imports
//! are how the recording sees `Run.fail` and the loop's `Run.failure`
//! reading, in the same entries the VM writes, and how a replay hands the
//! recorded reading back.
//!
//! A program that reads `Run.lastTurn` also imports the loop's two marks
//! around its wait, which answer the host's monotonic clock in nanoseconds
//! and are recorded as the VM records them (no arguments, Unit), and
//! `run_last_turn`, which hands the numbers the module made from them through
//! the recorder, the way `run_failure` hands the reason (see
//! `codegen/wasm_gc/run_turn.rs`).

use super::super::RunWasmGcHost;
use super::lm::{lm_string_from_host, lm_string_to_host};
use super::replay_glue::{
    json_none, json_record, json_some, record_effect_if_recording, try_replay,
};

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
        "run_wait_starts" | "run_wait_ends" => {
            let effect = if name == "run_wait_starts" {
                "Run.waitStarts"
            } else {
                "Run.waitEnds"
            };
            // A mark is reissued: a replay consumes the recorded one and still
            // reads the clock, because what the program saw is the recorded
            // `Run.lastTurn`, not this reading.
            if try_replay(caller, effect, vec![])?.is_none() {
                record_effect_if_recording(caller, effect, vec![], JsonValue::Null, caller_fn);
            }
            results[0] = Val::I64(monotonic_nanos());
            Ok(true)
        }
        "run_last_turn" => {
            let (turn, waited, worked) = match try_replay(caller, "Run.lastTurn", vec![])? {
                Some(cached) => recorded_turn(&cached)?,
                None => {
                    let turn = params.first().and_then(Val::i64).unwrap_or_default();
                    let waited = params.get(1).and_then(Val::i64).unwrap_or_default();
                    let worked = params.get(2).and_then(Val::i64).unwrap_or_default();
                    let outcome = json_record(
                        "Run.Turn",
                        vec![
                            ("turn", JsonValue::from(turn)),
                            ("waitedMs", JsonValue::from(waited)),
                            ("workedMs", JsonValue::from(worked)),
                        ],
                    );
                    record_effect_if_recording(caller, "Run.lastTurn", vec![], outcome, caller_fn);
                    (turn, waited, worked)
                }
            };
            results[0] = Val::I64(turn);
            results[1] = Val::I64(waited);
            results[2] = Val::I64(worked);
            Ok(true)
        }
        _ => Ok(false),
    }
}

/// Nanoseconds of the host's monotonic clock, from the first reading this
/// process made.
fn monotonic_nanos() -> i64 {
    static EPOCH: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();
    let epoch = *EPOCH.get_or_init(std::time::Instant::now);
    i64::try_from(epoch.elapsed().as_nanos()).unwrap_or(i64::MAX)
}

/// The three numbers of a recorded `Run.Turn`.
fn recorded_turn(cached: &aver::replay::JsonValue) -> Result<(i64, i64, i64), wasmtime::Error> {
    let invalid = || wasmtime::Error::msg("replay Run.lastTurn: not a Run.Turn");
    let fields = cached
        .get("$record")
        .and_then(|record| record.get("fields"))
        .ok_or_else(invalid)?;
    let field = |name: &str| fields.get(name).and_then(|value| value.as_i64());
    Ok((
        field("turn").ok_or_else(invalid)?,
        field("waitedMs").ok_or_else(invalid)?,
        field("workedMs").ok_or_else(invalid)?,
    ))
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
