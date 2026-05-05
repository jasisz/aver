//! `Terminal.*` host imports — 12 arms covering raw mode, cursor
//! moves, colour control, clear, flush, key reads, size queries.

use super::super::RunWasmGcHost;
use super::super::decode::{decode_option_string, decode_terminal_size};
use super::factories::{host_option_string_none, host_option_string_some, host_terminal_size_make};
use super::lm::{lm_string_to_host, val_i64};
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
    use wasmtime::Val;
    match name {
        "terminal_enable_raw_mode" => {
            if try_replay(caller, "Terminal.enableRawMode", vec![])?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::terminal_enable_raw_mode();
            record_effect_if_recording(
                caller,
                "Terminal.enableRawMode",
                vec![],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "terminal_disable_raw_mode" => {
            if try_replay(caller, "Terminal.disableRawMode", vec![])?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::terminal_disable_raw_mode();
            record_effect_if_recording(
                caller,
                "Terminal.disableRawMode",
                vec![],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "terminal_clear" => {
            if try_replay(caller, "Terminal.clear", vec![])?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::terminal_clear();
            record_effect_if_recording(
                caller,
                "Terminal.clear",
                vec![],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "terminal_move_to" => {
            let x = params.first().and_then(val_i64).unwrap_or(0);
            let y = params.get(1).and_then(val_i64).unwrap_or(0);
            let args = vec![
                aver::replay::JsonValue::Int(x),
                aver::replay::JsonValue::Int(y),
            ];
            if try_replay(caller, "Terminal.moveTo", args.clone())?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::terminal_move_to(x, y);
            record_effect_if_recording(
                caller,
                "Terminal.moveTo",
                args,
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "terminal_print" => {
            // Same shape as console_print (any_ref payload through the
            // LM bridge), but writes via aver_rt::terminal_print so it
            // respects raw-mode without injecting a trailing newline.
            let text = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(text.clone())];
            if try_replay(caller, "Terminal.print", args.clone())?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::terminal_print(&text);
            record_effect_if_recording(
                caller,
                "Terminal.print",
                args,
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "terminal_set_color" => {
            let text = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(text.clone())];
            if try_replay(caller, "Terminal.setColor", args.clone())?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::terminal_set_color(&text);
            record_effect_if_recording(
                caller,
                "Terminal.setColor",
                args,
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "terminal_reset_color" => {
            if try_replay(caller, "Terminal.resetColor", vec![])?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::terminal_reset_color();
            record_effect_if_recording(
                caller,
                "Terminal.resetColor",
                vec![],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "terminal_hide_cursor" => {
            if try_replay(caller, "Terminal.hideCursor", vec![])?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::terminal_hide_cursor();
            record_effect_if_recording(
                caller,
                "Terminal.hideCursor",
                vec![],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "terminal_show_cursor" => {
            if try_replay(caller, "Terminal.showCursor", vec![])?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::terminal_show_cursor();
            record_effect_if_recording(
                caller,
                "Terminal.showCursor",
                vec![],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "terminal_flush" => {
            if try_replay(caller, "Terminal.flush", vec![])?.is_some() {
                return Ok(true);
            }
            let _ = aver_rt::terminal_flush();
            record_effect_if_recording(
                caller,
                "Terminal.flush",
                vec![],
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        "terminal_read_key" => {
            if let Some(cached) = try_replay(caller, "Terminal.readKey", vec![])? {
                let opt_ref = decode_option_string(caller, &cached)?;
                results[0] = Val::AnyRef(opt_ref);
                return Ok(true);
            }
            let key = aver_rt::terminal_read_key();
            let (opt_ref, outcome) = match &key {
                Some(text) => (
                    host_option_string_some(caller, text)?,
                    json_some(aver::replay::JsonValue::String(text.clone())),
                ),
                None => (host_option_string_none(caller)?, json_none()),
            };
            results[0] = Val::AnyRef(opt_ref);
            record_effect_if_recording(caller, "Terminal.readKey", vec![], outcome, caller_fn);
            Ok(true)
        }
        "terminal_size" => {
            if let Some(cached) = try_replay(caller, "Terminal.size", vec![])? {
                let rec_ref = decode_terminal_size(caller, &cached)?;
                results[0] = Val::AnyRef(rec_ref);
                return Ok(true);
            }
            let (w, h) = aver_rt::terminal_size().unwrap_or((80, 24));
            let rec_ref = host_terminal_size_make(caller, w, h)?;
            results[0] = Val::AnyRef(rec_ref);
            record_effect_if_recording(
                caller,
                "Terminal.size",
                vec![],
                json_record(
                    "Terminal.Size",
                    vec![
                        ("width", aver::replay::JsonValue::Int(w)),
                        ("height", aver::replay::JsonValue::Int(h)),
                    ],
                ),
                caller_fn,
            );
            Ok(true)
        }
        _ => Ok(false),
    }
}
