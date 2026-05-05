//! `Env.*` host imports — `env_get` / `env_set` against the host
//! process environment.

use super::super::RunWasmGcHost;
use super::super::decode::decode_string;
use super::lm::{lm_string_from_host, lm_string_to_host};
use super::replay_glue::{record_effect_if_recording, try_replay};

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    match name {
        "env_get" => {
            let name = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(name.clone())];
            if let Some(cached) = try_replay(caller, "Env.get", args.clone())? {
                let r = decode_string(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let value = aver_rt::env_get(&name).unwrap_or_default();
            let r = lm_string_from_host(caller, &value)?;
            results[0] = Val::AnyRef(r);
            record_effect_if_recording(
                caller,
                "Env.get",
                args,
                aver::replay::JsonValue::String(value),
            );
            Ok(true)
        }
        "env_set" => {
            let name = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let value = lm_string_to_host(caller, params.get(1))?.unwrap_or_default();
            let args = vec![
                aver::replay::JsonValue::String(name.clone()),
                aver::replay::JsonValue::String(value.clone()),
            ];
            if try_replay(caller, "Env.set", args.clone())?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::env_set(&name, &value);
            record_effect_if_recording(caller, "Env.set", args, aver::replay::JsonValue::Null);
            Ok(true)
        }
        _ => Ok(false),
    }
}
