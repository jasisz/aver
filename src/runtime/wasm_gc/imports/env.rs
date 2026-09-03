//! `Env.*` host imports — `env_get` / `env_set` against the host
//! process environment.

use super::super::RunWasmGcHost;
use super::super::decode::{decode_option_string, decode_result_unit};
use super::factories::{
    host_option_string_none, host_option_string_some, host_result_err_unit_string,
    host_result_ok_unit,
};
use super::lm::lm_string_to_host;
use super::replay_glue::{
    json_err, json_none, json_ok, json_some, record_effect_if_recording, try_replay,
};

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    match name {
        "env_get" => {
            let name = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(name.clone())];
            if let Some(cached) = try_replay(caller, "Env.get", args.clone())? {
                results[0] = Val::AnyRef(decode_option_string(caller, &cached)?);
                return Ok(true);
            }
            let (result, outcome) = match aver_rt::env_get(&name) {
                Some(value) => (
                    host_option_string_some(caller, &value)?,
                    json_some(aver::replay::JsonValue::String(value)),
                ),
                None => (host_option_string_none(caller)?, json_none()),
            };
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Env.get", args, outcome, caller_fn);
            Ok(true)
        }
        "env_set" => {
            let name = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let value = lm_string_to_host(caller, params.get(1))?.unwrap_or_default();
            let args = vec![
                aver::replay::JsonValue::String(name.clone()),
                aver::replay::JsonValue::String(value.clone()),
            ];
            if let Some(cached) = try_replay(caller, "Env.set", args.clone())? {
                results[0] = Val::AnyRef(decode_result_unit(caller, &cached)?);
                return Ok(true);
            }
            let (result, outcome) = match aver_rt::env_set(&name, &value) {
                Ok(()) => (
                    host_result_ok_unit(caller)?,
                    json_ok(aver::replay::JsonValue::Null),
                ),
                Err(error) => (
                    host_result_err_unit_string(caller, &error)?,
                    json_err(&error),
                ),
            };
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Env.set", args, outcome, caller_fn);
            Ok(true)
        }
        _ => Ok(false),
    }
}
