//! `Disk.*` host imports — 8 arms covering text read/write/append,
//! exists, delete, makeDir, deleteDir, listDir.

use super::super::RunWasmGcHost;
use super::super::decode::{decode_result_list_string, decode_result_string, decode_result_unit};
use super::factories::{
    host_result_err_list_string, host_result_err_string, host_result_err_unit_string,
    host_result_ok_list_string, host_result_ok_string, host_result_ok_unit,
};
use super::lm::lm_string_to_host;
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
