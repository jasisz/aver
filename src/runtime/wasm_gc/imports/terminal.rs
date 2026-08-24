//! `Terminal.*` host imports — 12 arms covering raw mode, cursor
//! moves, colour control, clear, flush, key reads, size queries.

use super::super::RunWasmGcHost;
use super::super::decode::{
    decode_result_option_string, decode_result_terminal_size, decode_result_unit,
};
use super::factories::{
    host_option_string_none, host_option_string_some, host_result_err_unit_string,
    host_result_ok_unit, host_result_option_string_err, host_result_option_string_ok,
    host_result_terminal_size_err, host_result_terminal_size_ok, host_terminal_size_make,
};
use super::lm::lm_string_to_host;
use super::replay_glue::{
    json_err, json_none, json_ok, json_record, json_some, record_effect_if_recording, try_replay,
};
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
        "terminal_enable_raw_mode" => {
            dispatch_unit_result(
                caller,
                results,
                "Terminal.enableRawMode",
                vec![],
                caller_fn,
                aver_rt::terminal_enable_raw_mode,
            )?;
            Ok(true)
        }
        "terminal_disable_raw_mode" => {
            dispatch_unit_result(
                caller,
                results,
                "Terminal.disableRawMode",
                vec![],
                caller_fn,
                aver_rt::terminal_disable_raw_mode,
            )?;
            Ok(true)
        }
        "terminal_clear" => {
            dispatch_unit_result(
                caller,
                results,
                "Terminal.clear",
                vec![],
                caller_fn,
                aver_rt::terminal_clear,
            )?;
            Ok(true)
        }
        "terminal_move_to" => {
            let x = params
                .first()
                .ok_or_else(|| wasmtime::Error::msg("Terminal.moveTo: missing x"))?;
            let y = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Terminal.moveTo: missing y"))?;
            let x = decode_guest_int(caller, x, "Terminal.moveTo: malformed x Int")?;
            let y = decode_guest_int(caller, y, "Terminal.moveTo: malformed y Int")?;
            let args = vec![guest_int_json(&x), guest_int_json(&y)];
            if let Some(cached) = try_replay(caller, "Terminal.moveTo", args.clone())? {
                results[0] = Val::AnyRef(decode_result_unit(caller, &cached)?);
                return Ok(true);
            }
            let moved = match (x.value, y.value) {
                (Some(x), Some(y)) => aver_rt::terminal_move_to(x, y),
                _ => Err("Terminal.moveTo: coordinates must fit a 64-bit integer".to_string()),
            };
            let (result, outcome) = match moved {
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
            record_effect_if_recording(caller, "Terminal.moveTo", args, outcome, caller_fn);
            Ok(true)
        }
        "terminal_print" => {
            // Same shape as console_print (any_ref payload through the
            // LM bridge), but writes via aver_rt::terminal_print so it
            // respects raw-mode without injecting a trailing newline.
            let text = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(text.clone())];
            dispatch_unit_result(caller, results, "Terminal.print", args, caller_fn, || {
                aver_rt::terminal_print(&text)
            })?;
            Ok(true)
        }
        "terminal_set_color" => {
            let text = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(text.clone())];
            dispatch_unit_result(
                caller,
                results,
                "Terminal.setColor",
                args,
                caller_fn,
                || aver_rt::terminal_set_color(&text),
            )?;
            Ok(true)
        }
        "terminal_reset_color" => {
            dispatch_unit_result(
                caller,
                results,
                "Terminal.resetColor",
                vec![],
                caller_fn,
                aver_rt::terminal_reset_color,
            )?;
            Ok(true)
        }
        "terminal_hide_cursor" => {
            dispatch_unit_result(
                caller,
                results,
                "Terminal.hideCursor",
                vec![],
                caller_fn,
                aver_rt::terminal_hide_cursor,
            )?;
            Ok(true)
        }
        "terminal_show_cursor" => {
            dispatch_unit_result(
                caller,
                results,
                "Terminal.showCursor",
                vec![],
                caller_fn,
                aver_rt::terminal_show_cursor,
            )?;
            Ok(true)
        }
        "terminal_flush" => {
            dispatch_unit_result(
                caller,
                results,
                "Terminal.flush",
                vec![],
                caller_fn,
                aver_rt::terminal_flush,
            )?;
            Ok(true)
        }
        "terminal_read_key" => {
            if let Some(cached) = try_replay(caller, "Terminal.readKey", vec![])? {
                results[0] = Val::AnyRef(decode_result_option_string(caller, &cached)?);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::terminal_read_key() {
                Ok(Some(text)) => {
                    let option = host_option_string_some(caller, &text)?;
                    (
                        host_result_option_string_ok(caller, option)?,
                        json_ok(json_some(aver::replay::JsonValue::String(text))),
                    )
                }
                Ok(None) => {
                    let option = host_option_string_none(caller)?;
                    (
                        host_result_option_string_ok(caller, option)?,
                        json_ok(json_none()),
                    )
                }
                Err(error) => (
                    host_result_option_string_err(caller, &error)?,
                    json_err(&error),
                ),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Terminal.readKey", vec![], outcome, caller_fn);
            Ok(true)
        }
        "terminal_size" => {
            if let Some(cached) = try_replay(caller, "Terminal.size", vec![])? {
                results[0] = Val::AnyRef(decode_result_terminal_size(caller, &cached)?);
                return Ok(true);
            }
            let (result, outcome) = match aver_rt::terminal_size() {
                Ok((width, height)) => {
                    let record = host_terminal_size_make(caller, width, height)?;
                    let value = json_record(
                        "Terminal.Size",
                        vec![
                            ("width", aver::replay::JsonValue::Int(width)),
                            ("height", aver::replay::JsonValue::Int(height)),
                        ],
                    );
                    (
                        host_result_terminal_size_ok(caller, record)?,
                        json_ok(value),
                    )
                }
                Err(error) => (
                    host_result_terminal_size_err(caller, &error)?,
                    json_err(&error),
                ),
            };
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Terminal.size", vec![], outcome, caller_fn);
            Ok(true)
        }
        _ => Ok(false),
    }
}

fn dispatch_unit_result(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    results: &mut [wasmtime::Val],
    effect: &str,
    args: Vec<aver::replay::JsonValue>,
    caller_fn: &str,
    run: impl FnOnce() -> Result<(), String>,
) -> Result<(), wasmtime::Error> {
    use wasmtime::Val;
    if let Some(cached) = try_replay(caller, effect, args.clone())? {
        results[0] = Val::AnyRef(decode_result_unit(caller, &cached)?);
        return Ok(());
    }
    let (result, outcome) = match run() {
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
    record_effect_if_recording(caller, effect, args, outcome, caller_fn);
    Ok(())
}
