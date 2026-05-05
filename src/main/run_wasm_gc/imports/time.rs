//! `Time.*` host imports — `Time.now`, `Time.unixMs`, `Time.sleep`.

use super::super::RunWasmGcHost;
use super::super::decode::decode_string;
use super::lm::{lm_string_from_host, val_i64};
use super::replay_glue::{record_effect_if_recording, try_replay};

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
            let text = aver_rt::time_now();
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
            use std::time::{SystemTime, UNIX_EPOCH};
            let ms = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis() as i64)
                .unwrap_or(0);
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
            let ms = params.first().and_then(val_i64).unwrap_or(0);
            if try_replay(caller, "Time.sleep", vec![aver::replay::JsonValue::Int(ms)])?.is_some() {
                // In replay: don't actually sleep — observation-equivalent
                // means we skip the wall-clock side effect.
                return Ok(true);
            }
            std::thread::sleep(std::time::Duration::from_millis(ms.max(0) as u64));
            record_effect_if_recording(
                caller,
                "Time.sleep",
                vec![aver::replay::JsonValue::Int(ms)],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        _ => Ok(false),
    }
}
