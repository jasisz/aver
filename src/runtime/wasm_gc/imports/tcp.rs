//! `Tcp.*` host imports — connect / writeLine / writeBytes / readLine /
//! readBytes / close / send / sendBytes / ping. Connection handles cross as opaque wasm-gc structs
//! built by `host_tcp_connection_make`; the host extracts the
//! `id` field via `host_tcp_connection_id`.

use num_bigint::{BigInt, Sign};
use std::str::FromStr;

use super::super::RunWasmGcHost;
use super::super::decode::{
    decode_result_bytes, decode_result_string, decode_result_tcp_connection, decode_result_unit,
};
use super::factories::{
    host_result_err_bytes, host_result_err_list_int, host_result_err_string,
    host_result_err_unit_string, host_result_ok_bytes, host_result_ok_list_int_refs,
    host_result_ok_string, host_result_ok_unit, host_result_tcp_connection_err,
    host_result_tcp_connection_ok, host_tcp_connection_id, host_tcp_connection_make,
};
use super::lm::{lm_string_from_host, lm_string_to_host, val_i64};
use super::replay_glue::{json_err, json_ok, json_record, record_effect_if_recording, try_replay};

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    let tcp_settings = caller.data().tcp_settings;
    match name {
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
            let (result_ref, outcome) =
                match aver_rt::tcp::connect_with_settings(&host, port, tcp_settings) {
                    Ok(conn) => {
                        let id_ref = lm_string_from_host(caller, conn.id.as_ref())?;
                        let host_ref = lm_string_from_host(caller, conn.host.as_ref())?;
                        let rec_ref =
                            host_tcp_connection_make(caller, id_ref, host_ref, conn.port)?;
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
            record_effect_if_recording(caller, "Tcp.connect", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_poll" => {
            let mut entries = decode_poll_entries(caller, params.first())?;
            entries.sort_by(|left, right| left.provider_order.cmp(&right.provider_order));
            let timeout = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Tcp.poll: missing timeoutMs"))?;
            let timeout = decode_guest_int(caller, timeout, "Tcp.poll: malformed timeout carrier")?;
            let args = vec![poll_map_json(&entries), guest_int_json(&timeout)];
            if let Some(cached) = try_replay(caller, "Tcp.poll", args.clone())? {
                let result = replay_poll_result(caller, &cached, &entries)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }

            let polled = match timeout.value {
                Some(timeout) => aver_rt::tcp::poll(
                    &entries
                        .iter()
                        .map(|entry| entry.connection.clone())
                        .collect::<Vec<_>>(),
                    timeout,
                ),
                None => Err(format!(
                    "Tcp.poll: timeoutMs {} exceeds the poll limit",
                    timeout.display
                )),
            };
            let (result_ref, outcome) = match polled {
                Ok(positions) => {
                    let mut ready = positions
                        .into_iter()
                        .filter_map(|position| entries.get(position))
                        .collect::<Vec<_>>();
                    ready.sort_by(|left, right| left.numeric.cmp(&right.numeric));
                    ready.dedup_by(|left, right| left.numeric == right.numeric);
                    let refs = ready.iter().map(|entry| entry.key_ref).collect::<Vec<_>>();
                    let json = ready
                        .iter()
                        .map(|entry| entry.key_json.clone())
                        .collect::<Vec<_>>();
                    (
                        host_result_ok_list_int_refs(caller, &refs)?,
                        json_ok(aver::replay::JsonValue::Array(json)),
                    )
                }
                Err(error) => (host_result_err_list_int(caller, &error)?, json_err(&error)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.poll", args, outcome, caller_fn);
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
            record_effect_if_recording(caller, "Tcp.writeLine", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_write_bytes" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let conn_arg = json_record(
                "Tcp.Connection",
                vec![
                    ("id", aver::replay::JsonValue::String(id.clone())),
                    ("host", aver::replay::JsonValue::String(String::new())),
                    ("port", aver::replay::JsonValue::Int(0)),
                ],
            );
            let (payload, payload_json) =
                decode_byte_payload(caller, params.get(1), "Tcp.writeBytes")?;
            let args = vec![conn_arg, payload_json];
            if let Some(cached) = try_replay(caller, "Tcp.writeBytes", args.clone())? {
                let result = decode_result_unit(caller, &cached)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }
            let conn = aver_rt::TcpConnection {
                id: aver_rt::AverStr::from(id.as_str()),
                host: aver_rt::AverStr::from(""),
                port: 0,
            };
            let (result_ref, outcome) = match payload {
                Err(error) => (
                    host_result_err_unit_string(caller, &error)?,
                    json_err(&error),
                ),
                Ok(payload) => match aver_rt::tcp::write_bytes(&conn, &payload) {
                    Ok(()) => (
                        host_result_ok_unit(caller)?,
                        json_ok(aver::replay::JsonValue::Null),
                    ),
                    Err(error) => (
                        host_result_err_unit_string(caller, &error)?,
                        json_err(&error),
                    ),
                },
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.writeBytes", args, outcome, caller_fn);
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
            record_effect_if_recording(caller, "Tcp.readLine", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_read_bytes" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let conn_arg = json_record(
                "Tcp.Connection",
                vec![
                    ("id", aver::replay::JsonValue::String(id.clone())),
                    ("host", aver::replay::JsonValue::String(String::new())),
                    ("port", aver::replay::JsonValue::Int(0)),
                ],
            );
            let count = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Tcp.readBytes: missing count"))?;
            let count = decode_guest_int(caller, count, "Tcp.readBytes: malformed count carrier")?;
            let count_json = match count.value {
                Some(value) => aver::replay::JsonValue::Int(value),
                None => {
                    let mut opaque = std::collections::BTreeMap::new();
                    opaque.insert(
                        "$opaque".to_string(),
                        aver::replay::JsonValue::String(count.display.clone()),
                    );
                    aver::replay::JsonValue::Object(opaque)
                }
            };
            let args = vec![conn_arg, count_json];
            if let Some(cached) = try_replay(caller, "Tcp.readBytes", args.clone())? {
                let r = decode_result_bytes(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let conn = aver_rt::TcpConnection {
                id: aver_rt::AverStr::from(id.as_str()),
                host: aver_rt::AverStr::from(""),
                port: 0,
            };
            let read = match count.value {
                Some(value) => aver_rt::tcp::read_bytes(&conn, value),
                None => Err(format!(
                    "Tcp.readBytes: count {} exceeds the read limit",
                    count.display
                )),
            };
            let (result_ref, outcome) = match read {
                Ok(bytes) => {
                    let ints: Vec<i64> = bytes.iter().map(|byte| i64::from(*byte)).collect();
                    let json = ints
                        .iter()
                        .copied()
                        .map(aver::replay::JsonValue::Int)
                        .collect();
                    (
                        host_result_ok_bytes(caller, &ints)?,
                        json_ok(json_record(
                            "Bytes",
                            vec![("values", aver::replay::JsonValue::Array(json))],
                        )),
                    )
                }
                Err(error) => (host_result_err_bytes(caller, &error)?, json_err(&error)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.readBytes", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_read_some" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let conn_arg = json_connection(&id);
            let max_bytes = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Tcp.readSome: missing maxBytes"))?;
            let max_bytes = decode_guest_int(
                caller,
                max_bytes,
                "Tcp.readSome: malformed maxBytes carrier",
            )?;
            let max_bytes_json = guest_int_json(&max_bytes);
            let args = vec![conn_arg, max_bytes_json];
            if let Some(cached) = try_replay(caller, "Tcp.readSome", args.clone())? {
                let result = decode_result_bytes(caller, &cached)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }
            let conn = aver_rt::TcpConnection {
                id: aver_rt::AverStr::from(id.as_str()),
                host: aver_rt::AverStr::from(""),
                port: 0,
            };
            let read = match max_bytes.value {
                Some(value) => aver_rt::tcp::read_some(&conn, value),
                None => Err(format!(
                    "Tcp.readSome: maxBytes {} exceeds the read limit",
                    max_bytes.display
                )),
            };
            let (result_ref, outcome) = bytes_outcome(caller, read)?;
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.readSome", args, outcome, caller_fn);
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
            record_effect_if_recording(caller, "Tcp.close", args, outcome, caller_fn);
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
            let (result_ref, outcome) =
                match aver_rt::tcp::send_with_settings(&host, port, &msg, tcp_settings) {
                    Ok(text) => (
                        host_result_ok_string(caller, &text)?,
                        json_ok(aver::replay::JsonValue::String(text)),
                    ),
                    Err(e) => (host_result_err_string(caller, &e)?, json_err(&e)),
                };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.send", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_send_bytes" => {
            let host = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let port = params.get(1).and_then(val_i64).unwrap_or(0);
            let (payload, payload_json) =
                decode_byte_payload(caller, params.get(2), "Tcp.sendBytes")?;
            let args = vec![
                aver::replay::JsonValue::String(host.clone()),
                aver::replay::JsonValue::Int(port),
                payload_json,
            ];
            if let Some(cached) = try_replay(caller, "Tcp.sendBytes", args.clone())? {
                let r = decode_result_bytes(caller, &cached)?;
                results[0] = Val::AnyRef(r);
                return Ok(true);
            }
            let (result_ref, outcome) = match payload {
                Err(e) => (host_result_err_bytes(caller, &e)?, json_err(&e)),
                Ok(payload) => match aver_rt::tcp::send_bytes_with_settings(
                    &host,
                    port,
                    &payload,
                    tcp_settings,
                ) {
                    Ok(bytes) => {
                        let ints: Vec<i64> = bytes.iter().map(|b| i64::from(*b)).collect();
                        let json = ints
                            .iter()
                            .copied()
                            .map(aver::replay::JsonValue::Int)
                            .collect();
                        (
                            host_result_ok_bytes(caller, &ints)?,
                            json_ok(json_record(
                                "Bytes",
                                vec![("values", aver::replay::JsonValue::Array(json))],
                            )),
                        )
                    }
                    Err(e) => (host_result_err_bytes(caller, &e)?, json_err(&e)),
                },
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.sendBytes", args, outcome, caller_fn);
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
            let (result_ref, outcome) =
                match aver_rt::tcp::ping_with_settings(&host, port, tcp_settings) {
                    Ok(()) => (
                        host_result_ok_unit(caller)?,
                        json_ok(aver::replay::JsonValue::Null),
                    ),
                    Err(e) => (host_result_err_unit_string(caller, &e)?, json_err(&e)),
                };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.ping", args, outcome, caller_fn);
            Ok(true)
        }
        _ => Ok(false),
    }
}

fn json_connection(id: &str) -> aver::replay::JsonValue {
    json_record(
        "Tcp.Connection",
        vec![
            ("id", aver::replay::JsonValue::String(id.to_string())),
            ("host", aver::replay::JsonValue::String(String::new())),
            ("port", aver::replay::JsonValue::Int(0)),
        ],
    )
}

struct PollEntry {
    provider_order: Vec<u8>,
    numeric: BigInt,
    key_ref: wasmtime::Rooted<wasmtime::AnyRef>,
    key_json: aver::replay::JsonValue,
    connection: aver_rt::TcpConnection,
    connection_json: aver::replay::JsonValue,
}

fn decode_poll_entries(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    value: Option<&wasmtime::Val>,
) -> Result<Vec<PollEntry>, wasmtime::Error> {
    use wasmtime::Val;
    let map_ref = match value {
        Some(Val::AnyRef(Some(value))) => *value,
        _ => return Err(wasmtime::Error::msg("Tcp.poll: connections must be a Map")),
    };
    let map = map_ref
        .as_struct(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("Tcp.poll: malformed connections Map"))?;
    let capacity = match map.field(&mut *caller, 1)? {
        Val::I32(capacity) if capacity >= 0 => capacity as u32,
        _ => return Err(wasmtime::Error::msg("Tcp.poll: malformed Map capacity")),
    };
    if capacity == 0 {
        return Ok(Vec::new());
    }
    let keys_ref = match map.field(&mut *caller, 2)? {
        Val::AnyRef(Some(value)) => value,
        _ => return Err(wasmtime::Error::msg("Tcp.poll: malformed Map keys")),
    };
    let values_ref = match map.field(&mut *caller, 3)? {
        Val::AnyRef(Some(value)) => value,
        _ => return Err(wasmtime::Error::msg("Tcp.poll: malformed Map values")),
    };
    let keys = keys_ref
        .as_array(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("Tcp.poll: malformed Map keys array"))?;
    let values = values_ref
        .as_array(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("Tcp.poll: malformed Map values array"))?;
    if keys.len(&*caller)? < capacity || values.len(&*caller)? < capacity {
        return Err(wasmtime::Error::msg(
            "Tcp.poll: Map arrays are shorter than its capacity",
        ));
    }

    let mut entries = Vec::new();
    for index in 0..capacity {
        let key_box_ref = match keys.get(&mut *caller, index)? {
            Val::AnyRef(Some(value)) => value,
            Val::AnyRef(None) => continue,
            _ => return Err(wasmtime::Error::msg("Tcp.poll: malformed Int key box")),
        };
        let key_box = key_box_ref
            .as_struct(&*caller)?
            .ok_or_else(|| wasmtime::Error::msg("Tcp.poll: malformed Int key box"))?;
        let key = key_box.field(&mut *caller, 0)?;
        let key_ref = match &key {
            Val::AnyRef(Some(value)) => *value,
            _ => return Err(wasmtime::Error::msg("Tcp.poll: malformed Int key")),
        };
        let key = decode_guest_int(caller, &key, "Tcp.poll: malformed Int key")?;
        let key_value = aver_rt::AverInt::from_str(&key.display)
            .map_err(|_| wasmtime::Error::msg("Tcp.poll: malformed Int key"))?;
        let provider_order = aver_rt::provider::provider_value_order_key(
            &aver_rt::provider::ProviderValue::Int(key_value),
        )
        .map_err(wasmtime::Error::msg)?;

        let connection_value = values.get(&mut *caller, index)?;
        let id = host_tcp_connection_id(caller, Some(&connection_value))?
            .ok_or_else(|| wasmtime::Error::msg("Tcp.poll: malformed Tcp.Connection value"))?;
        entries.push(PollEntry {
            provider_order,
            numeric: key.big.clone(),
            key_ref,
            key_json: guest_int_json(&key),
            connection: aver_rt::TcpConnection {
                id: aver_rt::AverStr::from(id.as_str()),
                host: aver_rt::AverStr::from(""),
                port: 0,
            },
            connection_json: json_connection(&id),
        });
    }
    Ok(entries)
}

fn poll_map_json(entries: &[PollEntry]) -> aver::replay::JsonValue {
    let pairs = entries
        .iter()
        .map(|entry| {
            aver::replay::JsonValue::Array(vec![
                entry.key_json.clone(),
                entry.connection_json.clone(),
            ])
        })
        .collect();
    let mut marker = std::collections::BTreeMap::new();
    marker.insert("$map".to_string(), aver::replay::JsonValue::Array(pairs));
    aver::replay::JsonValue::Object(marker)
}

fn replay_poll_result(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    cached: &aver::replay::JsonValue,
    entries: &[PollEntry],
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let aver::replay::JsonValue::Object(marker) = cached else {
        return Err(wasmtime::Error::msg(
            "replay decode Tcp.poll: expected Result",
        ));
    };
    if let Some(aver::replay::JsonValue::String(error)) = marker.get("$err") {
        return host_result_err_list_int(caller, error);
    }
    let Some(aver::replay::JsonValue::Array(keys)) = marker.get("$ok") else {
        return Err(wasmtime::Error::msg(
            "replay decode Tcp.poll: expected List<Int> success",
        ));
    };
    let mut refs = Vec::with_capacity(keys.len());
    for key in keys {
        let entry = entries
            .iter()
            .find(|entry| &entry.key_json == key)
            .ok_or_else(|| {
                wasmtime::Error::msg(
                    "replay decode Tcp.poll: ready ID is absent from the input Map",
                )
            })?;
        refs.push(entry.key_ref);
    }
    host_result_ok_list_int_refs(caller, &refs)
}

pub(super) fn guest_int_json(value: &GuestInt) -> aver::replay::JsonValue {
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

fn bytes_outcome(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    outcome: Result<Vec<u8>, String>,
) -> Result<
    (
        Option<wasmtime::Rooted<wasmtime::AnyRef>>,
        aver::replay::JsonValue,
    ),
    wasmtime::Error,
> {
    match outcome {
        Ok(bytes) => {
            let ints = bytes.iter().copied().map(i64::from).collect::<Vec<_>>();
            let json = ints
                .iter()
                .copied()
                .map(aver::replay::JsonValue::Int)
                .collect();
            Ok((
                host_result_ok_bytes(caller, &ints)?,
                json_ok(json_record(
                    "Bytes",
                    vec![("values", aver::replay::JsonValue::Array(json))],
                )),
            ))
        }
        Err(error) => Ok((host_result_err_bytes(caller, &error)?, json_err(&error))),
    }
}

pub(super) struct GuestInt {
    pub(super) display: String,
    pub(super) value: Option<i64>,
    pub(super) big: BigInt,
}

pub(super) fn decode_byte_payload(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    val: Option<&wasmtime::Val>,
    effect: &'static str,
) -> Result<(Result<Vec<u8>, String>, aver::replay::JsonValue), wasmtime::Error> {
    use wasmtime::Val;
    let bytes_ref = match val {
        Some(Val::AnyRef(Some(r))) => *r,
        _ => {
            return Err(wasmtime::Error::msg(format!(
                "{effect}: payload must be Bytes"
            )));
        }
    };
    // Proof-packed Bytes crosses the host ABI as `(array i8)`. Keep the old
    // record/List<Int> decoder below as the representation-differential
    // fallback selected by the internal emitter configuration.
    if let Some(array) = bytes_ref.as_array(&*caller)? {
        let len = array.len(&*caller)?;
        let mut bytes = Vec::with_capacity(len as usize);
        for idx in 0..len {
            match array.get(&mut *caller, idx)? {
                Val::I32(value) => bytes.push(value as u8),
                _ => {
                    return Err(wasmtime::Error::msg(format!(
                        "{effect}: malformed packed Bytes carrier"
                    )));
                }
            }
        }
        let values_json = aver::replay::JsonValue::Array(
            bytes
                .iter()
                .copied()
                .map(|value| aver::replay::JsonValue::Int(i64::from(value)))
                .collect(),
        );
        return Ok((
            Ok(bytes),
            json_record("Bytes", vec![("values", values_json)]),
        ));
    }
    let bytes = bytes_ref
        .as_struct(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg(format!("{effect}: payload must be Bytes")))?;
    let malformed = format!("{effect}: malformed Bytes.values carrier");
    let mut current = match bytes.field(&mut *caller, 0)? {
        Val::AnyRef(r) => r,
        _ => return Err(wasmtime::Error::msg(malformed.clone())),
    };
    let mut ints = Vec::new();
    while let Some(node_ref) = current {
        let node = node_ref
            .as_struct(&*caller)?
            .ok_or_else(|| wasmtime::Error::msg(malformed.clone()))?;
        let head = node.field(&mut *caller, 0)?;
        let tail = node.field(&mut *caller, 1)?;
        ints.push(decode_guest_int(caller, &head, &malformed)?);
        current = match tail {
            Val::AnyRef(r) => r,
            _ => return Err(wasmtime::Error::msg(malformed.clone())),
        };
    }

    let values_json = if ints.iter().all(|n| n.value.is_some()) {
        aver::replay::JsonValue::Array(
            ints.iter()
                .filter_map(|n| n.value.map(aver::replay::JsonValue::Int))
                .collect(),
        )
    } else {
        let repr = format!(
            "[{}]",
            ints.iter()
                .map(|n| n.display.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        );
        let mut opaque = std::collections::BTreeMap::new();
        opaque.insert("$opaque".to_string(), aver::replay::JsonValue::String(repr));
        aver::replay::JsonValue::Object(opaque)
    };

    let mut bytes = Vec::with_capacity(ints.len());
    for (idx, int) in ints.iter().enumerate() {
        let Some(n) = int.value else {
            return Ok((
                Err(byte_range_error(effect, &int.display, idx)),
                json_record("Bytes", vec![("values", values_json)]),
            ));
        };
        match u8::try_from(n) {
            Ok(byte) => bytes.push(byte),
            Err(_) => {
                return Ok((
                    Err(byte_range_error(effect, &int.display, idx)),
                    json_record("Bytes", vec![("values", values_json)]),
                ));
            }
        }
    }
    Ok((
        Ok(bytes),
        json_record("Bytes", vec![("values", values_json)]),
    ))
}

pub(super) fn decode_guest_int(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    val: &wasmtime::Val,
    malformed: &str,
) -> Result<GuestInt, wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match val {
        Val::AnyRef(Some(r)) => *r,
        _ => return Err(wasmtime::Error::msg(malformed.to_owned())),
    };
    let int_ref = any_ref
        .as_struct(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg(malformed.to_owned()))?;
    let small = match int_ref.field(&mut *caller, 0)? {
        Val::I64(n) => n,
        _ => return Err(wasmtime::Error::msg(malformed.to_owned())),
    };
    let magnitude = int_ref.field(&mut *caller, 1)?;
    let magnitude_ref = match magnitude {
        Val::AnyRef(r) => r,
        _ => return Err(wasmtime::Error::msg(malformed.to_owned())),
    };
    let Some(magnitude_ref) = magnitude_ref else {
        return Ok(GuestInt {
            display: small.to_string(),
            value: Some(small),
            big: BigInt::from(small),
        });
    };

    let sign = match int_ref.field(&mut *caller, 2)? {
        Val::I32(n) if n < 0 => Sign::Minus,
        Val::I32(0) => Sign::NoSign,
        Val::I32(_) => Sign::Plus,
        _ => return Err(wasmtime::Error::msg(malformed.to_owned())),
    };
    let magnitude = magnitude_ref
        .as_array(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg(malformed.to_owned()))?;
    let len = magnitude.len(&*caller)?;
    let mut limbs = Vec::with_capacity(len as usize);
    for idx in 0..len {
        let limb = match magnitude.get(&mut *caller, idx)? {
            Val::I64(n) => {
                u32::try_from(n).map_err(|_| wasmtime::Error::msg(malformed.to_owned()))?
            }
            _ => return Err(wasmtime::Error::msg(malformed.to_owned())),
        };
        limbs.push(limb);
    }
    let big = BigInt::from_slice(sign, &limbs);
    Ok(GuestInt {
        display: big.to_string(),
        value: None,
        big,
    })
}

fn byte_range_error(effect: &str, value: &str, idx: usize) -> String {
    format!(
        "{}: byte {} at index {} is out of range (0\u{2013}255)",
        effect, value, idx
    )
}
