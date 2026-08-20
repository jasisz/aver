//! `Disk.*` host imports — text and byte read/write/append, metadata size,
//! exists, delete, makeDir, deleteDir, and listDir.

use super::super::RunWasmGcHost;
use super::super::decode::{
    decode_result_bytes, decode_result_int, decode_result_list_string, decode_result_string,
    decode_result_unit,
};
use super::factories::{
    host_result_err_bytes, host_result_err_int, host_result_err_list_string,
    host_result_err_string, host_result_err_unit_string, host_result_ok_bytes, host_result_ok_int,
    host_result_ok_list_string, host_result_ok_string, host_result_ok_unit,
};
use super::lm::lm_string_to_host;
use super::replay_glue::{json_err, json_ok, json_record, record_effect_if_recording, try_replay};

fn bytes_json(bytes: &[u8]) -> aver::replay::JsonValue {
    json_record(
        "Bytes",
        vec![(
            "values",
            aver::replay::JsonValue::Array(
                bytes
                    .iter()
                    .copied()
                    .map(|byte| aver::replay::JsonValue::Int(i64::from(byte)))
                    .collect(),
            ),
        )],
    )
}

fn guest_int_json(value: &super::tcp::GuestInt) -> aver::replay::JsonValue {
    match value.value {
        Some(value) => aver::replay::JsonValue::Int(value),
        None => {
            let mut opaque = std::collections::BTreeMap::new();
            opaque.insert(
                "$opaque".to_string(),
                aver::replay::JsonValue::String(value.display.clone()),
            );
            aver::replay::JsonValue::Object(opaque)
        }
    }
}

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    match name {
        "disk_read_bytes" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(path.clone())];
            if let Some(cached) = try_replay(caller, "Disk.readBytes", args.clone())? {
                let result = decode_result_bytes(caller, &cached)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }
            let (result_ref, outcome) = match aver_rt::read_bytes(&path) {
                Ok(bytes) => {
                    let ints = bytes.iter().copied().map(i64::from).collect::<Vec<_>>();
                    (
                        host_result_ok_bytes(caller, &ints)?,
                        json_ok(bytes_json(&bytes)),
                    )
                }
                Err(error) => (host_result_err_bytes(caller, &error)?, json_err(&error)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Disk.readBytes", args, outcome, caller_fn);
            Ok(true)
        }
        "disk_read_bytes_at" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let offset = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Disk.readBytesAt: missing offset"))?;
            let length = params
                .get(2)
                .ok_or_else(|| wasmtime::Error::msg("Disk.readBytesAt: missing length"))?;
            let offset = super::tcp::decode_guest_int(
                caller,
                offset,
                "Disk.readBytesAt: malformed offset carrier",
            )?;
            let length = super::tcp::decode_guest_int(
                caller,
                length,
                "Disk.readBytesAt: malformed length carrier",
            )?;
            let args = vec![
                aver::replay::JsonValue::String(path.clone()),
                guest_int_json(&offset),
                guest_int_json(&length),
            ];
            if let Some(cached) = try_replay(caller, "Disk.readBytesAt", args.clone())? {
                let result = decode_result_bytes(caller, &cached)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }
            let read = match (offset.value, length.value) {
                (Some(offset), Some(length)) => aver_rt::read_bytes_at(
                    &path,
                    &aver_rt::AverInt::from_i64(offset),
                    &aver_rt::AverInt::from_i64(length),
                ),
                (None, _) => Err("Disk.readBytesAt: offset must fit a 64-bit integer".to_string()),
                (_, None) => Err("Disk.readBytesAt: length must fit a 64-bit integer".to_string()),
            };
            let (result_ref, outcome) = match read {
                Ok(bytes) => {
                    let ints = bytes.iter().copied().map(i64::from).collect::<Vec<_>>();
                    (
                        host_result_ok_bytes(caller, &ints)?,
                        json_ok(bytes_json(&bytes)),
                    )
                }
                Err(error) => (host_result_err_bytes(caller, &error)?, json_err(&error)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Disk.readBytesAt", args, outcome, caller_fn);
            Ok(true)
        }
        "disk_write_bytes" | "disk_append_bytes" => {
            let (effect, append) = if name == "disk_append_bytes" {
                ("Disk.appendBytes", true)
            } else {
                ("Disk.writeBytes", false)
            };
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let (payload, payload_json) =
                super::tcp::decode_byte_payload(caller, params.get(1), effect)?;
            let args = vec![aver::replay::JsonValue::String(path.clone()), payload_json];
            if let Some(cached) = try_replay(caller, effect, args.clone())? {
                let result = decode_result_unit(caller, &cached)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }
            let (result_ref, outcome) = match payload {
                Err(error) => (
                    host_result_err_unit_string(caller, &error)?,
                    json_err(&error),
                ),
                Ok(payload) => {
                    let result = if append {
                        aver_rt::append_bytes(&path, &payload)
                    } else {
                        aver_rt::write_bytes(&path, &payload)
                    };
                    match result {
                        Ok(()) => (
                            host_result_ok_unit(caller)?,
                            json_ok(aver::replay::JsonValue::Null),
                        ),
                        Err(error) => (
                            host_result_err_unit_string(caller, &error)?,
                            json_err(&error),
                        ),
                    }
                }
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, effect, args, outcome, caller_fn);
            Ok(true)
        }
        "disk_size" => {
            let path = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let args = vec![aver::replay::JsonValue::String(path.clone())];
            if let Some(cached) = try_replay(caller, "Disk.size", args.clone())? {
                let result = decode_result_int(caller, &cached)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }
            let measured = aver_rt::file_size(&path).and_then(|size| {
                size.to_i64()
                    .ok_or_else(|| "Disk.size: file size exceeds the wasm host range".to_string())
            });
            let (result_ref, outcome) = match measured {
                Ok(size) => (
                    host_result_ok_int(caller, size)?,
                    json_ok(aver::replay::JsonValue::Int(size)),
                ),
                Err(error) => (host_result_err_int(caller, &error)?, json_err(&error)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Disk.size", args, outcome, caller_fn);
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
            record_effect_if_recording(caller, "Disk.readText", args, outcome, caller_fn);
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
            record_effect_if_recording(caller, "Disk.writeText", args, outcome, caller_fn);
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
            record_effect_if_recording(caller, "Disk.appendText", args, outcome, caller_fn);
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
                caller_fn,
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
            record_effect_if_recording(caller, "Disk.delete", args, outcome, caller_fn);
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
            record_effect_if_recording(caller, "Disk.deleteDir", args, outcome, caller_fn);
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
            record_effect_if_recording(caller, "Disk.listDir", args, outcome, caller_fn);
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
            record_effect_if_recording(caller, "Disk.makeDir", args, outcome, caller_fn);
            Ok(true)
        }
        _ => Ok(false),
    }
}
