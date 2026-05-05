//! `aver/*` host import dispatch + per-effect record/replay glue +
//! `__rt_record_*` factory builders. All the wasm-gc-side mechanics
//! that turn a host-imported function call into either a real
//! `aver_rt::*` invocation (Normal/Recording mode) or a trace
//! lookup (Replay mode) live here.
//!
//! Cross-module: arms that need to materialise complex return refs
//! (Result, Option, records) call into `super::decode` for the JSON
//! → Val decoders. The `host_*_make` factories below stay here so
//! the dispatch loop doesn't have to chase across modules to find
//! them.

#![cfg(feature = "wasm")]

use super::RunWasmGcHost;
use super::decode::{
    decode_option_string, decode_result_http_response, decode_result_list_string,
    decode_result_string, decode_result_tcp_connection, decode_result_unit, decode_string,
    decode_terminal_size,
};

/// Append one entry to the trace iff recording is on. Args + outcome
/// land as `aver::replay::JsonValue` so the wasm-gc trace is
/// byte-compatible with the VM's — `aver replay <file>` consumes
/// either without branching on backend. No-op in Replay mode (the
/// `EffectReplayState::replay_effect` path already advances position).
pub(super) fn record_effect_if_recording(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    effect_type: &str,
    args: Vec<aver::replay::JsonValue>,
    outcome: aver::replay::JsonValue,
) {
    if let Some(rec) = caller.data_mut().recorder.as_mut()
        && rec.mode() == aver::replay::EffectReplayMode::Record
    {
        rec.record_effect(
            effect_type,
            args,
            aver::replay::RecordedOutcome::Value(outcome),
            "main",
            0,
        );
    }
}
/// Pull the next outcome from the replay trace if the host is in
/// Replay mode. Returns `Ok(Some(outcome))` when the trace had a
/// matching entry, `Ok(None)` when not in Replay mode (caller falls
/// back to running the real effect), and `Err(...)` when replay
/// rejects the call (sequence mismatch, exhausted trace, args
/// diverge under `--check-args`).
pub(super) fn try_replay(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    effect_type: &str,
    args: Vec<aver::replay::JsonValue>,
) -> Result<Option<aver::replay::JsonValue>, wasmtime::Error> {
    let in_replay = caller
        .data()
        .recorder
        .as_ref()
        .is_some_and(|r| r.mode() == aver::replay::EffectReplayMode::Replay);
    if !in_replay {
        return Ok(None);
    }
    let outcome = caller
        .data_mut()
        .recorder
        .as_mut()
        .expect("checked above")
        .replay_effect(effect_type, Some(args))
        .map_err(|e| wasmtime::Error::msg(format!("replay {}: {:?}", effect_type, e)))?;
    match outcome {
        aver::replay::RecordedOutcome::Value(json) => Ok(Some(json)),
        aver::replay::RecordedOutcome::RuntimeError(msg) => Err(wasmtime::Error::msg(format!(
            "replay {}: trace recorded a runtime error ({})",
            effect_type, msg
        ))),
    }
}

/// Build the recorder-side JSON for an Aver `Result.Ok(inner)`. Marker
/// shape (`{"$ok": <inner>}`) matches `replay::value_to_json` so VM
/// and wasm-gc traces stay byte-identical for the same effect.
pub(super) fn json_ok(inner: aver::replay::JsonValue) -> aver::replay::JsonValue {
    let mut obj = std::collections::BTreeMap::new();
    obj.insert("$ok".to_string(), inner);
    aver::replay::JsonValue::Object(obj)
}

/// Build the recorder-side JSON for an Aver `Result.Err(message)`.
/// Always wraps a `String` payload — every host-side `Result` we
/// emit today carries `String` errors, matching the
/// `aver_rt::*` Rust signatures.
pub(super) fn json_err(msg: &str) -> aver::replay::JsonValue {
    let mut obj = std::collections::BTreeMap::new();
    obj.insert(
        "$err".to_string(),
        aver::replay::JsonValue::String(msg.to_string()),
    );
    aver::replay::JsonValue::Object(obj)
}

/// Build the recorder-side JSON for an Aver `Option.Some(inner)`.
pub(super) fn json_some(inner: aver::replay::JsonValue) -> aver::replay::JsonValue {
    let mut obj = std::collections::BTreeMap::new();
    obj.insert("$some".to_string(), inner);
    aver::replay::JsonValue::Object(obj)
}

/// Build the recorder-side JSON for an Aver `Option.None`.
pub(super) fn json_none() -> aver::replay::JsonValue {
    let mut obj = std::collections::BTreeMap::new();
    obj.insert("$none".to_string(), aver::replay::JsonValue::Bool(true));
    aver::replay::JsonValue::Object(obj)
}

/// Build the recorder-side JSON for an Aver record value
/// (`{"$record": {"type": <name>, "fields": {<k>: <v>, …}}}`).
/// Used for `HttpResponse`, `Tcp.Connection`, `Terminal.Size`.
pub(super) fn json_record(
    type_name: &str,
    fields: Vec<(&str, aver::replay::JsonValue)>,
) -> aver::replay::JsonValue {
    let mut fields_obj = std::collections::BTreeMap::new();
    for (k, v) in fields {
        fields_obj.insert(k.to_string(), v);
    }
    let mut payload = std::collections::BTreeMap::new();
    payload.insert(
        "type".to_string(),
        aver::replay::JsonValue::String(type_name.to_string()),
    );
    payload.insert(
        "fields".to_string(),
        aver::replay::JsonValue::Object(fields_obj),
    );
    let mut wrapper = std::collections::BTreeMap::new();
    wrapper.insert(
        "$record".to_string(),
        aver::replay::JsonValue::Object(payload),
    );
    aver::replay::JsonValue::Object(wrapper)
}

/// Per-effect-name dispatch — returns true if we provided a real
/// implementation, false if the caller should fall back to the
/// typed-zero stub.
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

#[derive(Clone, Copy)]
pub(super) enum HttpVerb {
    Get,
    Head,
    Delete,
    Post,
    Put,
    Patch,
}

pub(super) fn http_simple_dispatch(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    verb: HttpVerb,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    let url = lm_string_to_host(caller, params.first())?.unwrap_or_default();
    let effect_name = match verb {
        HttpVerb::Get => "Http.get",
        HttpVerb::Head => "Http.head",
        HttpVerb::Delete => "Http.delete",
        _ => unreachable!(),
    };
    let args = vec![aver::replay::JsonValue::String(url.clone())];
    if let Some(cached) = try_replay(caller, effect_name, args.clone())? {
        let r = decode_result_http_response(caller, &cached)?;
        results[0] = Val::AnyRef(r);
        return Ok(true);
    }
    let outcome = match verb {
        HttpVerb::Get => aver_rt::http::get(&url),
        HttpVerb::Head => aver_rt::http::head(&url),
        HttpVerb::Delete => aver_rt::http::delete(&url),
        _ => unreachable!(),
    };
    let trace_outcome = http_outcome_to_json(&outcome);
    let result_ref = http_outcome_to_result(caller, outcome)?;
    results[0] = Val::AnyRef(result_ref);
    record_effect_if_recording(caller, effect_name, args, trace_outcome);
    Ok(true)
}

pub(super) fn http_body_dispatch(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    verb: HttpVerb,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    let url = lm_string_to_host(caller, params.first())?.unwrap_or_default();
    let body = lm_string_to_host(caller, params.get(1))?.unwrap_or_default();
    let content_type = lm_string_to_host(caller, params.get(2))?.unwrap_or_default();
    // Headers map (params[3]) is opaque to us today — aver_rt::http::*
    // takes empty headers; the user's map crosses but the host doesn't
    // unpack it yet. Verbs whose status / body we report back are still
    // useful even without forwarding extra request headers, and the
    // common Authorization-via-URL or body-encoded payload paths work.
    let _ = params.get(3);
    let effect_name = match verb {
        HttpVerb::Post => "Http.post",
        HttpVerb::Put => "Http.put",
        HttpVerb::Patch => "Http.patch",
        _ => unreachable!(),
    };
    // Headers map argument is recorded as an empty `Map<String, List<String>>`
    // — the host doesn't unpack the user's wasm-gc map ref yet, so the
    // trace shows the same shape the host actually forwards.
    let empty_headers = aver::replay::JsonValue::Object(std::collections::BTreeMap::new());
    let args = vec![
        aver::replay::JsonValue::String(url.clone()),
        aver::replay::JsonValue::String(body.clone()),
        aver::replay::JsonValue::String(content_type.clone()),
        empty_headers,
    ];
    if let Some(cached) = try_replay(caller, effect_name, args.clone())? {
        let r = decode_result_http_response(caller, &cached)?;
        results[0] = Val::AnyRef(r);
        return Ok(true);
    }
    let outcome = match verb {
        HttpVerb::Post => aver_rt::http::post(&url, &body, &content_type, &Default::default()),
        HttpVerb::Put => aver_rt::http::put(&url, &body, &content_type, &Default::default()),
        HttpVerb::Patch => aver_rt::http::patch(&url, &body, &content_type, &Default::default()),
        _ => unreachable!(),
    };
    let trace_outcome = http_outcome_to_json(&outcome);
    let result_ref = http_outcome_to_result(caller, outcome)?;
    results[0] = Val::AnyRef(result_ref);
    record_effect_if_recording(caller, effect_name, args, trace_outcome);
    Ok(true)
}

pub(super) fn http_outcome_to_result(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    outcome: Result<aver_rt::HttpResponse, String>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    match outcome {
        Ok(resp) => {
            let body_ref = lm_string_from_host(caller, resp.body.as_ref())?;
            let headers_ref = host_map_string_list_string_empty(caller)?;
            let rec_ref = host_http_response_make(caller, resp.status, body_ref, headers_ref)?;
            host_result_http_response_ok(caller, rec_ref)
        }
        Err(e) => host_result_http_response_err(caller, &e),
    }
}

/// Mirror an `aver_rt::HttpResponse` outcome into the JSON shape the
/// VM recorder uses (`{"$ok": {"$record": {"type": "HttpResponse",
/// "fields": {"status": …, "body": …, "headers": {}}}}}`). Headers
/// always serialise as an empty `Map<String, List<String>>` for now —
/// `aver_rt::HttpResponse` carries the fields the host bridge
/// populates, and the host doesn't expose response headers yet (same
/// gap the wasm record-construction path has).
pub(super) fn http_outcome_to_json(
    outcome: &Result<aver_rt::HttpResponse, String>,
) -> aver::replay::JsonValue {
    match outcome {
        Ok(resp) => json_ok(json_record(
            "HttpResponse",
            vec![
                ("status", aver::replay::JsonValue::Int(resp.status)),
                (
                    "body",
                    aver::replay::JsonValue::String(resp.body.as_ref().to_string()),
                ),
                (
                    "headers",
                    aver::replay::JsonValue::Object(std::collections::BTreeMap::new()),
                ),
            ],
        )),
        Err(e) => json_err(e),
    }
}

/// Decode an `i64` param without panicking if it's a different val kind.
pub(super) fn val_i64(v: &wasmtime::Val) -> Option<i64> {
    match v {
        wasmtime::Val::I64(n) => Some(*n),
        _ => None,
    }
}

/// Print one Aver string to stdout/stderr via the LM transport bridge.
pub(super) fn host_print(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    to_stdout: bool,
) -> Result<(), wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match params.first() {
        Some(Val::AnyRef(r)) => *r,
        _ => return Ok(()),
    };
    let Some(any_ref) = any_ref else {
        return Ok(());
    };
    let to_lm = caller
        .get_export("__rt_string_to_lm")
        .and_then(|e| e.into_func());
    let memory = caller.get_export("memory").and_then(|e| e.into_memory());
    let (Some(to_lm), Some(memory)) = (to_lm, memory) else {
        return Ok(());
    };
    let mut out = [Val::I32(0)];
    if to_lm
        .call(&mut *caller, &[Val::AnyRef(Some(any_ref))], &mut out)
        .is_ok()
    {
        let len = match out[0] {
            Val::I32(n) => n,
            _ => 0,
        };
        if len > 0 {
            let mut buf = vec![0u8; len as usize];
            let _ = memory.read(&caller, 0, &mut buf);
            let text = String::from_utf8_lossy(&buf);
            if to_stdout {
                println!("{}", text);
            } else {
                eprintln!("{}", text);
            }
        } else {
            // Empty string still emits a newline in VM semantics.
            if to_stdout {
                println!();
            } else {
                eprintln!();
            }
        }
    }
    Ok(())
}

/// Decode an Aver String ref (passed as `any_ref` in the import ABI)
/// back to a Rust `String` via the LM transport bridge.
pub(super) fn lm_string_to_host(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    val: Option<&wasmtime::Val>,
) -> Result<Option<String>, wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match val {
        Some(Val::AnyRef(r)) => *r,
        _ => return Ok(None),
    };
    let Some(any_ref) = any_ref else {
        return Ok(None);
    };
    let to_lm = caller
        .get_export("__rt_string_to_lm")
        .and_then(|e| e.into_func());
    let memory = caller.get_export("memory").and_then(|e| e.into_memory());
    let (Some(to_lm), Some(memory)) = (to_lm, memory) else {
        return Ok(None);
    };
    let mut out = [Val::I32(0)];
    to_lm.call(&mut *caller, &[Val::AnyRef(Some(any_ref))], &mut out)?;
    let len = match out[0] {
        Val::I32(n) => n.max(0) as usize,
        _ => 0,
    };
    let mut buf = vec![0u8; len];
    if len > 0 {
        memory.read(&caller, 0, &mut buf)?;
    }
    Ok(Some(String::from_utf8_lossy(&buf).into_owned()))
}

/// Build an `Option<String>::Some(text)` ref via the
/// `__rt_option_string_some` factory.
pub(super) fn host_option_string_some(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_option_string_some")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build an `Option<String>::None` ref via `__rt_option_string_none`.
pub(super) fn host_option_string_none(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_option_string_none")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build a `Terminal.Size(width, height)` record via the
/// `__rt_record_terminal_size_make` factory.
pub(super) fn host_terminal_size_make(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    width: i64,
    height: i64,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_record_terminal_size_make")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::I64(width), Val::I64(height)], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build an `HttpResponse(status, body, headers)` ref via the matching
/// factory export.
pub(super) fn host_http_response_make(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    status: i64,
    body: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
    headers: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_record_http_response_make")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(
        &mut *caller,
        &[Val::I64(status), Val::AnyRef(body), Val::AnyRef(headers)],
        &mut out,
    )?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(super) fn host_result_http_response_ok(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    resp: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_http_response_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(resp)], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(super) fn host_result_http_response_err(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_http_response_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(super) fn host_map_string_list_string_empty(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_map_string_list_string_empty")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build a `Tcp.Connection` record from host-side primitives via the
/// `__rt_record_tcp_connection_make(id, host, port)` factory.
pub(super) fn host_tcp_connection_make(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    id: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
    host: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
    port: i64,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_record_tcp_connection_make")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(
        &mut *caller,
        &[Val::AnyRef(id), Val::AnyRef(host), Val::I64(port)],
        &mut out,
    )?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Read the `id` field of a `Tcp.Connection` record via
/// `__rt_tcp_connection_id`. Returns `None` when the record ref is
/// null (shouldn't happen for a successful connect, but bail safely).
pub(super) fn host_tcp_connection_id(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    val: Option<&wasmtime::Val>,
) -> Result<Option<String>, wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match val {
        Some(Val::AnyRef(r)) => *r,
        _ => return Ok(None),
    };
    let Some(_) = any_ref else { return Ok(None) };
    let getter = caller
        .get_export("__rt_tcp_connection_id")
        .and_then(|e| e.into_func());
    let Some(getter) = getter else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    getter.call(&mut *caller, &[Val::AnyRef(any_ref)], &mut out)?;
    lm_string_to_host(caller, Some(&out[0]))
}

pub(super) fn host_result_tcp_connection_ok(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    conn: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_tcp_connection_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(conn)], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(super) fn host_result_tcp_connection_err(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_tcp_connection_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// `Result<Unit, String>::Ok(())` via the matching factory export.
pub(super) fn host_result_ok_unit(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_unit_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(super) fn host_result_err_unit_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_unit_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build a `Result<List<String>, String>::Ok(list)` ref. The list is
/// constructed bottom-up via repeated `__rt_list_string_cons` calls,
/// terminated by `__rt_list_string_nil`.
pub(super) fn host_result_ok_list_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    items: &[String],
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let nil = caller
        .get_export("__rt_list_string_nil")
        .and_then(|e| e.into_func());
    let cons = caller
        .get_export("__rt_list_string_cons")
        .and_then(|e| e.into_func());
    let factory = caller
        .get_export("__rt_result_list_string_string_ok")
        .and_then(|e| e.into_func());
    let (Some(nil), Some(cons), Some(factory)) = (nil, cons, factory) else {
        return Ok(None);
    };
    let mut tail_out = [Val::AnyRef(None)];
    nil.call(&mut *caller, &[], &mut tail_out)?;
    let mut current = match &tail_out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    };
    // Cons in reverse so the resulting list keeps the input order.
    for text in items.iter().rev() {
        let head = match lm_string_from_host(caller, text)? {
            Some(r) => r,
            None => return Ok(None),
        };
        let mut next = [Val::AnyRef(None)];
        cons.call(
            &mut *caller,
            &[Val::AnyRef(Some(head)), Val::AnyRef(current)],
            &mut next,
        )?;
        current = match &next[0] {
            Val::AnyRef(r) => *r,
            _ => None,
        };
    }
    let mut wrapped = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(current)], &mut wrapped)?;
    Ok(match &wrapped[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(super) fn host_result_err_list_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_list_string_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build a `Result<String,String>::Ok(text)` ref by calling the
/// module's exported factory `__rt_result_string_string_ok`. Returns
/// `Ok(None)` if the factory or string bridge isn't exported (the
/// program declared `Console.readLine` but didn't reach the type
/// registration that materialises the factory).
pub(super) fn host_result_ok_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_string_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build a `Result<String,String>::Err(text)` ref via the symmetric
/// `__rt_result_string_string_err` factory.
pub(super) fn host_result_err_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_string_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Materialise a host-supplied UTF-8 string as an Aver string ref via
/// the LM transport bridge: write bytes into linear memory at offset 0,
/// call `__rt_string_from_lm(len)` to wrap them in an `(array i8)`.
/// Returns the resulting AnyRef so the caller can hand it back as a
/// host import result.
pub(super) fn lm_string_from_host<T: 'static>(
    caller: &mut wasmtime::Caller<'_, T>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::*;
    let bytes = text.as_bytes();
    let from_lm = caller
        .get_export("__rt_string_from_lm")
        .and_then(|e| e.into_func());
    let memory = caller.get_export("memory").and_then(|e| e.into_memory());
    let grow = caller
        .get_export("__rt_memory_grow")
        .and_then(|e| e.into_func());
    let pages = caller
        .get_export("__rt_memory_pages")
        .and_then(|e| e.into_func());
    let (Some(from_lm), Some(memory), Some(grow), Some(pages)) = (from_lm, memory, grow, pages)
    else {
        return Ok(None);
    };
    // Grow LM if the string doesn't fit. One page = 64 KiB; small
    // strings fit by default since the bridge ships with `(memory 1)`.
    let needed_pages = ((bytes.len() + 65535) >> 16) as i32;
    let mut current_out = [Val::I32(0)];
    pages.call(&mut *caller, &[], &mut current_out)?;
    let current_pages = match current_out[0] {
        Val::I32(n) => n,
        _ => 0,
    };
    if needed_pages > current_pages {
        let mut grow_out = [Val::I32(0)];
        grow.call(
            &mut *caller,
            &[Val::I32(needed_pages - current_pages)],
            &mut grow_out,
        )?;
    }
    memory.write(&mut *caller, 0, bytes)?;
    let mut from_lm_out = [Val::AnyRef(None)];
    from_lm.call(
        &mut *caller,
        &[Val::I32(bytes.len() as i32)],
        &mut from_lm_out,
    )?;
    let r = match &from_lm_out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    };
    Ok(r)
}
