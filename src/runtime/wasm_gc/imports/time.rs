//! `Time.*` host imports — `Time.now`, `Time.unixMs`, `Time.sleep`.

use super::super::RunWasmGcHost;
use super::super::decode::{decode_result_unit, decode_string};
use super::factories::{host_result_err_unit_string, host_result_ok_unit};
use super::lm::lm_string_from_host;
use super::replay_glue::{json_err, json_ok, record_effect_if_recording, try_replay};
use super::tcp::{decode_guest_int, guest_int_json};

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    match name {
        "time_now" => {
            if let Some(cached) = try_replay(caller, "Time.now", vec![])? {
                let r = decode_string(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let text = aver_rt::provider::standard_time_now();
            let r = lm_string_from_host(caller, &text)?;
            results[0] = Val::AnyRef(r);
            record_effect_if_recording(
                caller,
                "Time.now",
                vec![],
                aver::replay::JsonValue::String(text),
                caller_fn,
            );
            Ok(true)
        }
        "time_unix_ms" => {
            if let Some(cached) = try_replay(caller, "Time.unixMs", vec![])? {
                let aver::replay::JsonValue::Int(ms) = cached else {
                    return Err(wasmtime::Error::msg(
                        "replay Time.unixMs: trace value is not an Int",
                    ));
                };
                results[0] = Val::I64(ms);
                return Ok(true);
            }
            let ms = aver_rt::provider::standard_time_unix_ms()
                .to_i64()
                .expect("standard Time.unixMs always fits the wasm i64 transport");
            results[0] = Val::I64(ms);
            record_effect_if_recording(
                caller,
                "Time.unixMs",
                vec![],
                aver::replay::JsonValue::Int(ms),
                caller_fn,
            );
            Ok(true)
        }
        "time_sleep" => {
            let ms = params
                .first()
                .ok_or_else(|| wasmtime::Error::msg("Time.sleep: missing ms"))?;
            let ms = decode_guest_int(caller, ms, "Time.sleep: malformed ms Int")?;
            let args = vec![guest_int_json(&ms)];
            if let Some(cached) = try_replay(caller, "Time.sleep", args.clone())? {
                results[0] = Val::AnyRef(decode_result_unit(caller, &cached)?);
                return Ok(true);
            }
            let ms = aver_rt::AverInt::from_bigint(ms.big);
            let (result, outcome) = match aver_rt::provider::standard_time_sleep(&ms) {
                Ok(()) => (
                    host_result_ok_unit(caller)?,
                    json_ok(aver::replay::JsonValue::Null),
                ),
                Err(fault) => (
                    host_result_err_unit_string(caller, &fault.message)?,
                    json_err(&fault.message),
                ),
            };
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Time.sleep", args, outcome, caller_fn);
            Ok(true)
        }
        _ => Ok(false),
    }
}
