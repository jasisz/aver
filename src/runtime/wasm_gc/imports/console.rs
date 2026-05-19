//! `Console.*` host imports — `print`, `error`, `warn`, `readLine`.
//! All four cross via the LM transport bridge for their string args.

use super::super::RunWasmGcHost;
use super::super::decode::decode_result_string;
use super::factories::{host_result_err_string, host_result_ok_string};
use super::lm::{host_print, lm_string_to_host};
use super::replay_glue::{json_err, json_ok, record_effect_if_recording, try_replay};

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    match name {
        "console_print" => {
            let text = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            // Replay mode swallows the output too — replays are
            // observation-equivalent to the original run, including
            // suppressing host-side side effects so a re-run doesn't
            // double-print or hit external state. The `replay_effect`
            // call still advances trace position, so a sequence
            // mismatch with the recording surfaces here.
            if try_replay(
                caller,
                "Console.print",
                vec![aver::replay::JsonValue::String(text.clone())],
            )?
            .is_some()
            {
                return Ok(true);
            }
            host_print(caller, params, true)?;
            record_effect_if_recording(
                caller,
                "Console.print",
                vec![aver::replay::JsonValue::String(text)],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "console_error" => {
            let text = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            if try_replay(
                caller,
                "Console.error",
                vec![aver::replay::JsonValue::String(text.clone())],
            )?
            .is_some()
            {
                return Ok(true);
            }
            host_print(caller, params, false)?;
            record_effect_if_recording(
                caller,
                "Console.error",
                vec![aver::replay::JsonValue::String(text)],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "console_warn" => {
            let text = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            if try_replay(
                caller,
                "Console.warn",
                vec![aver::replay::JsonValue::String(text.clone())],
            )?
            .is_some()
            {
                return Ok(true);
            }
            host_print(caller, params, false)?;
            record_effect_if_recording(
                caller,
                "Console.warn",
                vec![aver::replay::JsonValue::String(text)],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "console_read_line" => {
            if let Some(cached) = try_replay(caller, "Console.readLine", vec![])? {
                let r = decode_result_string(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            // Read one line from stdin. EOF / IO error → Result.Err("EOF").
            // Trailing '\n' / '\r\n' is stripped to match VM semantics.
            use std::io::BufRead;
            let mut line = String::new();
            let read = std::io::stdin().lock().read_line(&mut line);
            let (result_ref, outcome) = match read {
                Ok(0) | Err(_) => (host_result_err_string(caller, "EOF")?, json_err("EOF")),
                Ok(_) => {
                    while line.ends_with('\n') || line.ends_with('\r') {
                        line.pop();
                    }
                    (
                        host_result_ok_string(caller, &line)?,
                        json_ok(aver::replay::JsonValue::String(line.clone())),
                    )
                }
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Console.readLine", vec![], outcome, caller_fn);
            Ok(true)
        }
        _ => Ok(false),
    }
}
