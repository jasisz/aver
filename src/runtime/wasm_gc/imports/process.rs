//! `Process.*` host imports — cooperative native shutdown observation.

use super::super::RunWasmGcHost;
use super::replay_glue::{record_effect_if_recording, try_replay};

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    _params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    if name != "process_stop_requested" {
        return Ok(false);
    }
    if let Some(cached) = try_replay(caller, "Process.stopRequested", vec![])? {
        let aver::replay::JsonValue::Bool(requested) = cached else {
            return Err(wasmtime::Error::msg(
                "replay Process.stopRequested: not a Bool",
            ));
        };
        results[0] = Val::I32(if requested { 1 } else { 0 });
        return Ok(true);
    }
    let requested = aver_rt::provider::standard_process_stop_requested()
        .map_err(|fault| wasmtime::Error::msg(format!("provider fault: {fault}")))?;
    results[0] = Val::I32(if requested { 1 } else { 0 });
    record_effect_if_recording(
        caller,
        "Process.stopRequested",
        vec![],
        aver::replay::JsonValue::Bool(requested),
        caller_fn,
    );
    Ok(true)
}
