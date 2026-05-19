//! `Args.*` host imports — `args_len` and `args_get` over the
//! program-args slice the host carries in `RunWasmGcHost`.

use super::super::RunWasmGcHost;
use super::super::decode::decode_string;
use super::lm::lm_string_from_host;
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
        "args_len" => {
            if let Some(cached) = try_replay(caller, "Args.len", vec![])? {
                let aver::replay::JsonValue::Int(n) = cached else {
                    return Err(wasmtime::Error::msg("replay Args.len: not an Int"));
                };
                results[0] = Val::I64(n);
                return Ok(true);
            }
            let n = caller.data().program_args.len() as i64;
            results[0] = Val::I64(n);
            record_effect_if_recording(
                caller,
                "Args.len",
                vec![],
                aver::replay::JsonValue::Int(n),
                caller_fn,
            );
            Ok(true)
        }
        "args_get" => {
            let idx = match params[0] {
                Val::I64(n) => n,
                _ => 0,
            };
            if let Some(cached) =
                try_replay(caller, "Args.get", vec![aver::replay::JsonValue::Int(idx)])?
            {
                let r = decode_string(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let text = caller
                .data()
                .program_args
                .get(idx.max(0) as usize)
                .cloned()
                .unwrap_or_default();
            let r = lm_string_from_host(caller, &text)?;
            results[0] = Val::AnyRef(r);
            record_effect_if_recording(
                caller,
                "Args.get",
                vec![aver::replay::JsonValue::Int(idx)],
                aver::replay::JsonValue::String(text),
                caller_fn,
            );
            Ok(true)
        }
        _ => Ok(false),
    }
}
