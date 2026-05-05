//! `aver/*` host import dispatch + per-effect record/replay glue +
//! `__rt_record_*` factory builders. All the wasm-gc-side mechanics
//! that turn a host-imported function call into either a real
//! `aver_rt::*` invocation (Normal/Recording mode) or a trace
//! lookup (Replay mode) live here.
//!
//! Helpers split out into per-domain submodules:
//!
//! - `lm`            — LM transport (string round-trip via `__rt_string_*`)
//! - `replay_glue`   — `try_replay`, `record_effect_if_recording`, `json_*`
//! - `factories`    — `host_*_make` builders for wasm-gc structs
//! - `http`          — HTTP verbs + outcome translation
//!
//! The `dispatch_aver_import` match below is the single 29-arm
//! table that names every effect; helpers above are the leaves it
//! reaches into. A future per-namespace split would carve `args`,
//! `console`, `disk`, `env`, `random`, `tcp`, `terminal`, `time`
//! sub-modules out of that table — see 0.16.2 plan.

#![cfg(feature = "wasm")]

use super::RunWasmGcHost;
use super::decode::{
    decode_option_string, decode_result_list_string, decode_result_string,
    decode_result_tcp_connection, decode_result_unit, decode_string, decode_terminal_size,
};

#[path = "imports/factories.rs"]
mod factories;
#[path = "imports/http.rs"]
mod http;
#[path = "imports/lm.rs"]
mod lm;
#[path = "imports/replay_glue.rs"]
mod replay_glue;

pub(super) use factories::{
    host_http_response_make, host_map_string_list_string_empty, host_option_string_none,
    host_option_string_some, host_result_err_list_string, host_result_err_string,
    host_result_err_unit_string, host_result_http_response_err, host_result_http_response_ok,
    host_result_ok_list_string, host_result_ok_string, host_result_ok_unit,
    host_result_tcp_connection_err, host_result_tcp_connection_ok, host_tcp_connection_make,
    host_terminal_size_make,
};
pub(super) use lm::lm_string_from_host;

use factories::host_tcp_connection_id;
use http::{HttpVerb, http_body_dispatch, http_simple_dispatch};
use lm::{host_print, lm_string_to_host, val_i64};
use replay_glue::{
    json_err, json_none, json_ok, json_record, json_some, record_effect_if_recording, try_replay,
};

pub(super) fn dispatch_aver_import(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
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
            record_effect_if_recording(caller, "Args.len", vec![], aver::replay::JsonValue::Int(n));
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
            );
            Ok(true)
        }
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
            record_effect_if_recording(caller, "Console.readLine", vec![], outcome);
            Ok(true)
        }
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
            );
            Ok(true)
        }
        "random_int" => {
            let (min, max) = match (params.first(), params.get(1)) {
                (Some(Val::I64(a)), Some(Val::I64(b))) => (*a, *b),
                _ => (0, 0),
            };
            if let Some(cached) = try_replay(
                caller,
                "Random.int",
                vec![
                    aver::replay::JsonValue::Int(min),
                    aver::replay::JsonValue::Int(max),
                ],
            )? {
                let aver::replay::JsonValue::Int(v) = cached else {
                    return Err(wasmtime::Error::msg(
                        "replay Random.int: trace value is not an Int",
                    ));
                };
                results[0] = Val::I64(v);
                return Ok(true);
            }
            // aver_rt::random_int returns Result, but the wasm import
            // contract is a plain i64. The host falls back to `min` on
            // an inverted range — same surface the VM exposes.
            let v = aver_rt::random::random_int(min, max).unwrap_or(min);
            results[0] = Val::I64(v);
            record_effect_if_recording(
                caller,
                "Random.int",
                vec![
                    aver::replay::JsonValue::Int(min),
                    aver::replay::JsonValue::Int(max),
                ],
                aver::replay::JsonValue::Int(v),
            );
            Ok(true)
        }
        "random_float" => {
            if let Some(cached) = try_replay(caller, "Random.float", vec![])? {
                let aver::replay::JsonValue::Float(f) = cached else {
                    return Err(wasmtime::Error::msg(
                        "replay Random.float: trace value is not a Float",
                    ));
                };
                results[0] = Val::F64(f.to_bits());
                return Ok(true);
            }
            let f = aver_rt::random::random_float();
            results[0] = Val::F64(f.to_bits());
            record_effect_if_recording(
                caller,
                "Random.float",
                vec![],
                aver::replay::JsonValue::Float(f),
            );
            Ok(true)
        }
        "float_sin" => {
            if let Some(Val::F64(b)) = params.first() {
                results[0] = Val::F64(f64::from_bits(*b).sin().to_bits());
            }
            Ok(true)
        }
        "float_cos" => {
            if let Some(Val::F64(b)) = params.first() {
                results[0] = Val::F64(f64::from_bits(*b).cos().to_bits());
            }
            Ok(true)
        }
        "float_atan2" => {
            if let (Some(Val::F64(y)), Some(Val::F64(x))) = (params.first(), params.get(1)) {
                results[0] = Val::F64(f64::from_bits(*y).atan2(f64::from_bits(*x)).to_bits());
            }
            Ok(true)
        }
        "float_pow" => {
            if let (Some(Val::F64(b)), Some(Val::F64(e))) = (params.first(), params.get(1)) {
                results[0] = Val::F64(f64::from_bits(*b).powf(f64::from_bits(*e)).to_bits());
            }
            Ok(true)
        }
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
            record_effect_if_recording(caller, "Terminal.readKey", vec![], outcome);
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
            );
            Ok(true)
        }
        "disk_read_text" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(path.clone())];
            if let Some(cached) = try_replay(caller, "Disk.readText", args.clone())? {
                let r = decode_result_string(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::read_text(&path) {
                Ok(text) => (
                    host_result_ok_string(caller, &text)?,
                    json_ok(aver::replay::JsonValue::String(text)),
                ),
                Err(e) => (host_result_err_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Disk.readText", args, outcome);
            Ok(true)
        }
        "disk_write_text" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let content = lm_string_to_host(caller, params.get(1))?.unwrap_or_default();
            let args = vec![
                aver::replay::JsonValue::String(path.clone()),
                aver::replay::JsonValue::String(content.clone()),
            ];
            if let Some(cached) = try_replay(caller, "Disk.writeText", args.clone())? {
                let r = decode_result_unit(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::write_text(&path, &content) {
                Ok(()) => (
                    host_result_ok_unit(caller)?,
                    json_ok(aver::replay::JsonValue::Null),
                ),
                Err(e) => (host_result_err_unit_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Disk.writeText", args, outcome);
            Ok(true)
        }
        "disk_append_text" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let content = lm_string_to_host(caller, params.get(1))?.unwrap_or_default();
            let args = vec![
                aver::replay::JsonValue::String(path.clone()),
                aver::replay::JsonValue::String(content.clone()),
            ];
            if let Some(cached) = try_replay(caller, "Disk.appendText", args.clone())? {
                let r = decode_result_unit(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::append_text(&path, &content) {
                Ok(()) => (
                    host_result_ok_unit(caller)?,
                    json_ok(aver::replay::JsonValue::Null),
                ),
                Err(e) => (host_result_err_unit_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Disk.appendText", args, outcome);
            Ok(true)
        }
        "disk_exists" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(path.clone())];
            if let Some(cached) = try_replay(caller, "Disk.exists", args.clone())? {
                let aver::replay::JsonValue::Bool(b) = cached else {
                    return Err(wasmtime::Error::msg("replay Disk.exists: not a Bool"));
                };
                results[0] = Val::I32(if b { 1 } else { 0 });
                return Ok(true);
            }
            let exists = aver_rt::path_exists(&path);
            results[0] = Val::I32(if exists { 1 } else { 0 });
            record_effect_if_recording(
                caller,
                "Disk.exists",
                args,
                aver::replay::JsonValue::Bool(exists),
            );
            Ok(true)
        }
        "disk_delete" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(path.clone())];
            if let Some(cached) = try_replay(caller, "Disk.delete", args.clone())? {
                let r = decode_result_unit(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::delete_file(&path) {
                Ok(()) => (
                    host_result_ok_unit(caller)?,
                    json_ok(aver::replay::JsonValue::Null),
                ),
                Err(e) => (host_result_err_unit_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Disk.delete", args, outcome);
            Ok(true)
        }
        "disk_delete_dir" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(path.clone())];
            if let Some(cached) = try_replay(caller, "Disk.deleteDir", args.clone())? {
                let r = decode_result_unit(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::delete_dir(&path) {
                Ok(()) => (
                    host_result_ok_unit(caller)?,
                    json_ok(aver::replay::JsonValue::Null),
                ),
                Err(e) => (host_result_err_unit_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Disk.deleteDir", args, outcome);
            Ok(true)
        }
        "disk_list_dir" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(path.clone())];
            if let Some(cached) = try_replay(caller, "Disk.listDir", args.clone())? {
                let r = decode_result_list_string(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::list_dir(&path) {
                Ok(list) => {
                    let names: Vec<String> = list.iter().cloned().collect();
                    let arr: Vec<aver::replay::JsonValue> = names
                        .iter()
                        .map(|s| aver::replay::JsonValue::String(s.clone()))
                        .collect();
                    (
                        host_result_ok_list_string(caller, &names)?,
                        json_ok(aver::replay::JsonValue::Array(arr)),
                    )
                }
                Err(e) => (host_result_err_list_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Disk.listDir", args, outcome);
            Ok(true)
        }
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
        "disk_make_dir" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(path.clone())];
            if let Some(cached) = try_replay(caller, "Disk.makeDir", args.clone())? {
                let r = decode_result_unit(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::make_dir(&path) {
                Ok(()) => (
                    host_result_ok_unit(caller)?,
                    json_ok(aver::replay::JsonValue::Null),
                ),
                Err(e) => (host_result_err_unit_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Disk.makeDir", args, outcome);
            Ok(true)
        }
        "tcp_connect" => {
            let host = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let port = params.get(1).and_then(val_i64).unwrap_or(0);
            let args = vec![
                aver::replay::JsonValue::String(host.clone()),
                aver::replay::JsonValue::Int(port),
            ];
            if let Some(cached) = try_replay(caller, "Tcp.connect", args.clone())? {
                let r = decode_result_tcp_connection(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::tcp::connect(&host, port) {
                Ok(conn) => {
                    let id_ref = lm_string_from_host(caller, conn.id.as_ref())?;
                    let host_ref = lm_string_from_host(caller, conn.host.as_ref())?;
                    let rec_ref = host_tcp_connection_make(caller, id_ref, host_ref, conn.port)?;
                    let conn_json = json_record(
                        "Tcp.Connection",
                        vec![
                            (
                                "id",
                                aver::replay::JsonValue::String(conn.id.as_ref().to_string()),
                            ),
                            (
                                "host",
                                aver::replay::JsonValue::String(conn.host.as_ref().to_string()),
                            ),
                            ("port", aver::replay::JsonValue::Int(conn.port)),
                        ],
                    );
                    (
                        host_result_tcp_connection_ok(caller, rec_ref)?,
                        json_ok(conn_json),
                    )
                }
                Err(e) => (host_result_tcp_connection_err(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.connect", args, outcome);
            Ok(true)
        }
        "tcp_write_line" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let line = lm_string_to_host(caller, params.get(1))?.unwrap_or_default();
            let conn_arg = json_record(
                "Tcp.Connection",
                vec![
                    ("id", aver::replay::JsonValue::String(id.clone())),
                    ("host", aver::replay::JsonValue::String(String::new())),
                    ("port", aver::replay::JsonValue::Int(0)),
                ],
            );
            let args = vec![
                conn_arg.clone(),
                aver::replay::JsonValue::String(line.clone()),
            ];
            if let Some(cached) = try_replay(caller, "Tcp.writeLine", args.clone())? {
                let r = decode_result_unit(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let conn = aver_rt::TcpConnection {
                id: aver_rt::AverStr::from(id.as_str()),
                host: aver_rt::AverStr::from(""),
                port: 0,
            };
            let (result_ref, outcome) = match aver_rt::tcp::write_line(&conn, &line) {
                Ok(()) => (
                    host_result_ok_unit(caller)?,
                    json_ok(aver::replay::JsonValue::Null),
                ),
                Err(e) => (host_result_err_unit_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.writeLine", args, outcome);
            Ok(true)
        }
        "tcp_read_line" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let conn_arg = json_record(
                "Tcp.Connection",
                vec![
                    ("id", aver::replay::JsonValue::String(id.clone())),
                    ("host", aver::replay::JsonValue::String(String::new())),
                    ("port", aver::replay::JsonValue::Int(0)),
                ],
            );
            let args = vec![conn_arg];
            if let Some(cached) = try_replay(caller, "Tcp.readLine", args.clone())? {
                let r = decode_result_string(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let conn = aver_rt::TcpConnection {
                id: aver_rt::AverStr::from(id.as_str()),
                host: aver_rt::AverStr::from(""),
                port: 0,
            };
            let (result_ref, outcome) = match aver_rt::tcp::read_line(&conn) {
                Ok(text) => (
                    host_result_ok_string(caller, &text)?,
                    json_ok(aver::replay::JsonValue::String(text)),
                ),
                Err(e) => (host_result_err_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.readLine", args, outcome);
            Ok(true)
        }
        "tcp_close" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let conn_arg = json_record(
                "Tcp.Connection",
                vec![
                    ("id", aver::replay::JsonValue::String(id.clone())),
                    ("host", aver::replay::JsonValue::String(String::new())),
                    ("port", aver::replay::JsonValue::Int(0)),
                ],
            );
            let args = vec![conn_arg];
            if let Some(cached) = try_replay(caller, "Tcp.close", args.clone())? {
                let r = decode_result_unit(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let conn = aver_rt::TcpConnection {
                id: aver_rt::AverStr::from(id.as_str()),
                host: aver_rt::AverStr::from(""),
                port: 0,
            };
            let (result_ref, outcome) = match aver_rt::tcp::close(&conn) {
                Ok(()) => (
                    host_result_ok_unit(caller)?,
                    json_ok(aver::replay::JsonValue::Null),
                ),
                Err(e) => (host_result_err_unit_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.close", args, outcome);
            Ok(true)
        }
        "tcp_send" => {
            let host = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let port = params.get(1).and_then(val_i64).unwrap_or(0);
            let msg = lm_string_to_host(caller, params.get(2))?.unwrap_or_default();
            let args = vec![
                aver::replay::JsonValue::String(host.clone()),
                aver::replay::JsonValue::Int(port),
                aver::replay::JsonValue::String(msg.clone()),
            ];
            if let Some(cached) = try_replay(caller, "Tcp.send", args.clone())? {
                let r = decode_result_string(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::tcp::send(&host, port, &msg) {
                Ok(text) => (
                    host_result_ok_string(caller, &text)?,
                    json_ok(aver::replay::JsonValue::String(text)),
                ),
                Err(e) => (host_result_err_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.send", args, outcome);
            Ok(true)
        }
        "tcp_ping" => {
            let host = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let port = params.get(1).and_then(val_i64).unwrap_or(0);
            let args = vec![
                aver::replay::JsonValue::String(host.clone()),
                aver::replay::JsonValue::Int(port),
            ];
            if let Some(cached) = try_replay(caller, "Tcp.ping", args.clone())? {
                let r = decode_result_unit(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::tcp::ping(&host, port) {
                Ok(()) => (
                    host_result_ok_unit(caller)?,
                    json_ok(aver::replay::JsonValue::Null),
                ),
                Err(e) => (host_result_err_unit_string(caller, &e)?, json_err(&e)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.ping", args, outcome);
            Ok(true)
        }
        "http_get" => http_simple_dispatch(caller, params, results, HttpVerb::Get),
        "http_head" => http_simple_dispatch(caller, params, results, HttpVerb::Head),
        "http_delete" => http_simple_dispatch(caller, params, results, HttpVerb::Delete),
        "http_post" => http_body_dispatch(caller, params, results, HttpVerb::Post),
        "http_put" => http_body_dispatch(caller, params, results, HttpVerb::Put),
        "http_patch" => http_body_dispatch(caller, params, results, HttpVerb::Patch),
        _ => Ok(false),
    }
}
