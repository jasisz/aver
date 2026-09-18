//! `Tcp.*` host imports — connect / writeLine / writeBytes / readLine /
//! readBytes / close / send / sendBytes / ping. Connection handles cross as opaque wasm-gc structs
//! built by `host_tcp_connection_make`; the host extracts the
//! `id` field via `host_tcp_connection_id`.

use num_bigint::{BigInt, Sign};
use std::str::FromStr;

use super::super::RunWasmGcHost;
use super::super::decode::{
    decode_result_bytes, decode_result_int, decode_result_string, decode_result_tcp_connection,
    decode_result_unit, expect_marker, expect_record,
};
use super::factories::{
    host_result_err_bytes, host_result_err_int, host_result_err_list_int, host_result_err_string,
    host_result_err_unit_string, host_result_ok_bytes, host_result_ok_int,
    host_result_ok_list_int_refs, host_result_ok_string, host_result_ok_unit,
    host_result_option_bytes_err, host_result_option_bytes_none, host_result_option_bytes_some,
    host_result_option_tcp_connection_err, host_result_option_tcp_connection_none,
    host_result_option_tcp_connection_some, host_result_tcp_connection_err,
    host_result_tcp_connection_ok, host_result_tcp_dial_err, host_result_tcp_dial_ok,
    host_result_tcp_listener_err, host_result_tcp_listener_ok, host_tcp_connection_id,
    host_tcp_connection_make, host_tcp_socket_kind, host_wait_item_kind,
};
use super::lm::{lm_string_from_host, lm_string_to_host, val_i64};
use super::replay_glue::{
    json_err, json_none, json_ok, json_record, json_some, record_effect_if_recording, try_replay,
};

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
                aver::replay::JsonValue::from(port),
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
                                ("port", aver::replay::JsonValue::from(conn.port)),
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
        "tcp_begin_connect" => {
            let host = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let port = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Tcp.beginConnect: missing port"))?;
            let port = decode_guest_int(caller, port, "Tcp.beginConnect: malformed port")?;
            let args = vec![
                aver::replay::JsonValue::String(host.clone()),
                guest_int_json(&port),
            ];
            if let Some(cached) = try_replay(caller, "Tcp.beginConnect", args.clone())? {
                results[0] = Val::AnyRef(decode_result_tcp_dial(caller, &cached)?);
                return Ok(true);
            }
            let started = match port.value {
                Some(port) => aver_rt::tcp::begin_connect_with_settings(&host, port, tcp_settings),
                None => Err("Tcp.beginConnect: port must fit a 64-bit integer".to_string()),
            };
            let (result, outcome) = match started {
                Ok(dial) => {
                    let id = dial.id.as_ref().to_string();
                    (
                        host_result_tcp_dial_ok(caller, &id)?,
                        json_ok(json_dial(&id)),
                    )
                }
                Err(error) => (host_result_tcp_dial_err(caller, &error)?, json_err(&error)),
            };
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Tcp.beginConnect", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_dialled" => {
            let id = socket_resource_id(
                caller,
                params
                    .first()
                    .ok_or_else(|| wasmtime::Error::msg("Tcp.dialled: missing Dial"))?,
            )?;
            let args = vec![json_dial(&id)];
            if let Some(cached) = try_replay(caller, "Tcp.dialled", args.clone())? {
                results[0] = Val::AnyRef(decode_result_option_tcp_connection(caller, &cached)?);
                return Ok(true);
            }
            let dial = aver_rt::TcpDial::from_id(id);
            let (result, outcome) =
                option_connection_outcome(caller, aver_rt::tcp::dialled(&dial))?;
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Tcp.dialled", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_listen" => {
            let port = params
                .first()
                .ok_or_else(|| wasmtime::Error::msg("Tcp.listen: missing port"))?;
            let port = decode_guest_int(caller, port, "Tcp.listen: malformed port")?;
            let backlog = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Tcp.listen: missing backlog"))?;
            let backlog = decode_guest_int(caller, backlog, "Tcp.listen: malformed backlog")?;
            let args = vec![guest_int_json(&port), guest_int_json(&backlog)];
            if let Some(cached) = try_replay(caller, "Tcp.listen", args.clone())? {
                results[0] = Val::AnyRef(decode_result_tcp_listener(caller, &cached)?);
                return Ok(true);
            }
            let listened = match (port.value, backlog.value) {
                (Some(port), Some(backlog)) => {
                    aver_rt::tcp::listen_with_settings(port, backlog, tcp_settings)
                }
                (None, _) => Err(format!(
                    "Tcp.listen: port {} exceeds the host integer range",
                    port.display
                )),
                (_, None) => Err(format!(
                    "Tcp.listen: backlog {} exceeds the host integer range",
                    backlog.display
                )),
            };
            let (result, outcome) = match listened {
                Ok(listener) => {
                    let id = listener.id.as_ref().to_string();
                    (
                        host_result_tcp_listener_ok(caller, &id)?,
                        json_ok(json_listener(&id)),
                    )
                }
                Err(error) => (
                    host_result_tcp_listener_err(caller, &error)?,
                    json_err(&error),
                ),
            };
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Tcp.listen", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_accept" => {
            let id = socket_resource_id(
                caller,
                params
                    .first()
                    .ok_or_else(|| wasmtime::Error::msg("Tcp.accept: missing Listener"))?,
            )?;
            let args = vec![json_listener(&id)];
            if let Some(cached) = try_replay(caller, "Tcp.accept", args.clone())? {
                results[0] = Val::AnyRef(decode_result_option_tcp_connection(caller, &cached)?);
                return Ok(true);
            }
            let listener = aver_rt::TcpListener::from_id(id);
            let (result, outcome) =
                option_connection_outcome(caller, aver_rt::tcp::accept(&listener))?;
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Tcp.accept", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_peer_address" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let args = vec![json_connection(&id)];
            if let Some(cached) = try_replay(caller, "Tcp.peerAddress", args.clone())? {
                results[0] = Val::AnyRef(decode_result_string(caller, &cached)?);
                return Ok(true);
            }
            let connection = aver_rt::TcpConnection::from_parts(id, String::new(), 0);
            let (result, outcome) = match aver_rt::tcp::peer_address(&connection) {
                Ok(address) => (
                    host_result_ok_string(caller, &address)?,
                    json_ok(aver::replay::JsonValue::String(address)),
                ),
                Err(error) => (host_result_err_string(caller, &error)?, json_err(&error)),
            };
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Tcp.peerAddress", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_poll" => {
            let mut entries = decode_poll_entries(caller, params.first())?;
            entries.sort_by(|left, right| left.provider_order.cmp(&right.provider_order));
            let timeout = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Tcp.poll: missing timeoutMs"))?;
            let timeout = decode_guest_int(caller, timeout, "Tcp.poll: malformed timeout carrier")?;
            let args = vec![
                poll_map_json(&entries, "Tcp.poll")?,
                guest_int_json(&timeout),
            ];
            if let Some(cached) = try_replay(caller, "Tcp.poll", args.clone())? {
                let result = replay_poll_result(caller, &cached, &entries)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }

            let polled = match timeout.value {
                Some(timeout) => aver_rt::tcp::poll(
                    &entries
                        .iter()
                        .filter_map(|entry| entry.socket.clone())
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
                    let json = recorded_keys(&ready, "Tcp.poll")?;
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
                    ("port", aver::replay::JsonValue::from(0)),
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
                    ("port", aver::replay::JsonValue::from(0)),
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
        "tcp_write_now" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let conn_arg = json_connection(&id);
            let (payload, payload_json) =
                decode_byte_payload(caller, params.get(1), "Tcp.writeNow")?;
            let args = vec![conn_arg, payload_json];
            if let Some(cached) = try_replay(caller, "Tcp.writeNow", args.clone())? {
                let result = decode_result_int(caller, &cached)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }
            let conn = aver_rt::TcpConnection::from_parts(id, String::new(), 0);
            let written = payload.and_then(|payload| aver_rt::tcp::write_now(&conn, &payload));
            let (result_ref, outcome) = match written {
                Ok(accepted) => (
                    host_result_ok_int(caller, accepted)?,
                    json_ok(aver::replay::JsonValue::from(accepted)),
                ),
                Err(error) => (host_result_err_int(caller, &error)?, json_err(&error)),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.writeNow", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_read_line" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let conn_arg = json_record(
                "Tcp.Connection",
                vec![
                    ("id", aver::replay::JsonValue::String(id.clone())),
                    ("host", aver::replay::JsonValue::String(String::new())),
                    ("port", aver::replay::JsonValue::from(0)),
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
                    ("port", aver::replay::JsonValue::from(0)),
                ],
            );
            let count = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Tcp.readBytes: missing count"))?;
            let count = decode_guest_int(caller, count, "Tcp.readBytes: malformed count carrier")?;
            let count_json = match count.value {
                Some(value) => aver::replay::JsonValue::from(value),
                None => {
                    let mut opaque = serde_json::Map::new();
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
                        .map(aver::replay::JsonValue::from)
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
        "tcp_read_now" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let conn_arg = json_connection(&id);
            let max_bytes = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Tcp.readNow: missing maxBytes"))?;
            let max_bytes =
                decode_guest_int(caller, max_bytes, "Tcp.readNow: malformed maxBytes carrier")?;
            let args = vec![conn_arg, guest_int_json(&max_bytes)];
            if let Some(cached) = try_replay(caller, "Tcp.readNow", args.clone())? {
                let result = decode_result_option_bytes(caller, &cached)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }
            let conn = aver_rt::TcpConnection::from_parts(id, String::new(), 0);
            let read = match max_bytes.value {
                Some(value) => aver_rt::tcp::read_now(&conn, value),
                None => Err(format!(
                    "Tcp.readNow: maxBytes {} exceeds the read limit",
                    max_bytes.display
                )),
            };
            let (result_ref, outcome) = option_bytes_outcome(caller, read)?;
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Tcp.readNow", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_close" => {
            let id = host_tcp_connection_id(caller, params.first())?.unwrap_or_default();
            let conn_arg = json_record(
                "Tcp.Connection",
                vec![
                    ("id", aver::replay::JsonValue::String(id.clone())),
                    ("host", aver::replay::JsonValue::String(String::new())),
                    ("port", aver::replay::JsonValue::from(0)),
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
        "tcp_close_dial" => {
            let id = socket_resource_id(
                caller,
                params
                    .first()
                    .ok_or_else(|| wasmtime::Error::msg("Tcp.closeDial: missing Dial"))?,
            )?;
            let args = vec![json_dial(&id)];
            if let Some(cached) = try_replay(caller, "Tcp.closeDial", args.clone())? {
                results[0] = Val::AnyRef(decode_result_unit(caller, &cached)?);
                return Ok(true);
            }
            let dial = aver_rt::TcpDial::from_id(id);
            let (result, outcome) = unit_outcome(caller, aver_rt::tcp::close_dial(&dial))?;
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Tcp.closeDial", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_close_listener" => {
            let id = socket_resource_id(
                caller,
                params
                    .first()
                    .ok_or_else(|| wasmtime::Error::msg("Tcp.closeListener: missing Listener"))?,
            )?;
            let args = vec![json_listener(&id)];
            if let Some(cached) = try_replay(caller, "Tcp.closeListener", args.clone())? {
                results[0] = Val::AnyRef(decode_result_unit(caller, &cached)?);
                return Ok(true);
            }
            let listener = aver_rt::TcpListener::from_id(id);
            let (result, outcome) = unit_outcome(caller, aver_rt::tcp::close_listener(&listener))?;
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Tcp.closeListener", args, outcome, caller_fn);
            Ok(true)
        }
        "tcp_send" => {
            let host = lm_string_to_host(caller, params.first())?.unwrap_or_default();
            let port = params.get(1).and_then(val_i64).unwrap_or(0);
            let msg = lm_string_to_host(caller, params.get(2))?.unwrap_or_default();
            let args = vec![
                aver::replay::JsonValue::String(host.clone()),
                aver::replay::JsonValue::from(port),
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
                aver::replay::JsonValue::from(port),
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
                            .map(aver::replay::JsonValue::from)
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
                aver::replay::JsonValue::from(port),
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
            ("port", aver::replay::JsonValue::from(0)),
        ],
    )
}

fn json_dial(id: &str) -> aver::replay::JsonValue {
    json_record(
        "Tcp.Dial",
        vec![("id", aver::replay::JsonValue::String(id.to_string()))],
    )
}

fn json_listener(id: &str) -> aver::replay::JsonValue {
    json_record(
        "Tcp.Listener",
        vec![("id", aver::replay::JsonValue::String(id.to_string()))],
    )
}

fn resource_id_from_recorded(
    value: &aver::replay::JsonValue,
    type_name: &str,
) -> Result<String, wasmtime::Error> {
    let fields = expect_record(value, type_name)?;
    match fields.get("id") {
        Some(aver::replay::JsonValue::String(id)) => Ok(id.clone()),
        _ => Err(wasmtime::Error::msg(format!(
            "replay decode {type_name}: missing resource id"
        ))),
    }
}

fn decode_result_tcp_dial(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let (marker, value) = expect_marker(json, &["$ok", "$err"])?;
    match marker {
        "$ok" => host_result_tcp_dial_ok(caller, &resource_id_from_recorded(value, "Tcp.Dial")?),
        "$err" => match value {
            aver::replay::JsonValue::String(error) => host_result_tcp_dial_err(caller, error),
            _ => Err(wasmtime::Error::msg(
                "replay decode Result<Tcp.Dial,String>: Err is not String",
            )),
        },
        _ => unreachable!(),
    }
}

fn decode_result_tcp_listener(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let (marker, value) = expect_marker(json, &["$ok", "$err"])?;
    match marker {
        "$ok" => {
            host_result_tcp_listener_ok(caller, &resource_id_from_recorded(value, "Tcp.Listener")?)
        }
        "$err" => match value {
            aver::replay::JsonValue::String(error) => host_result_tcp_listener_err(caller, error),
            _ => Err(wasmtime::Error::msg(
                "replay decode Result<Tcp.Listener,String>: Err is not String",
            )),
        },
        _ => unreachable!(),
    }
}

fn decode_result_option_tcp_connection(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let (marker, value) = expect_marker(json, &["$ok", "$err"])?;
    match marker {
        "$err" => match value {
            aver::replay::JsonValue::String(error) => {
                host_result_option_tcp_connection_err(caller, error)
            }
            _ => Err(wasmtime::Error::msg(
                "replay decode Result<Option<Tcp.Connection>,String>: Err is not String",
            )),
        },
        "$ok" => {
            let (option, value) = expect_marker(value, &["$some", "$none"])?;
            match option {
                "$none" => host_result_option_tcp_connection_none(caller),
                "$some" => {
                    let fields = expect_record(value, "Tcp.Connection")?;
                    let id = match fields.get("id") {
                        Some(aver::replay::JsonValue::String(id)) => id.clone(),
                        _ => String::new(),
                    };
                    let host = match fields.get("host") {
                        Some(aver::replay::JsonValue::String(host)) => host.clone(),
                        _ => String::new(),
                    };
                    let port = fields
                        .get("port")
                        .and_then(|v| v.as_i64())
                        .unwrap_or_default();
                    let id = lm_string_from_host(caller, &id)?;
                    let host = lm_string_from_host(caller, &host)?;
                    let connection = host_tcp_connection_make(caller, id, host, port)?;
                    host_result_option_tcp_connection_some(caller, connection)
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn option_connection_outcome(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    outcome: Result<Option<aver_rt::TcpConnection>, String>,
) -> Result<
    (
        Option<wasmtime::Rooted<wasmtime::AnyRef>>,
        aver::replay::JsonValue,
    ),
    wasmtime::Error,
> {
    match outcome {
        Ok(Some(connection)) => {
            let id = lm_string_from_host(caller, connection.id.as_ref())?;
            let host = lm_string_from_host(caller, connection.host.as_ref())?;
            let value = host_tcp_connection_make(caller, id, host, connection.port)?;
            let json = json_record(
                "Tcp.Connection",
                vec![
                    (
                        "id",
                        aver::replay::JsonValue::String(connection.id.as_ref().to_string()),
                    ),
                    (
                        "host",
                        aver::replay::JsonValue::String(connection.host.as_ref().to_string()),
                    ),
                    ("port", aver::replay::JsonValue::from(connection.port)),
                ],
            );
            Ok((
                host_result_option_tcp_connection_some(caller, value)?,
                json_ok(json_some(json)),
            ))
        }
        Ok(None) => Ok((
            host_result_option_tcp_connection_none(caller)?,
            json_ok(json_none()),
        )),
        Err(error) => Ok((
            host_result_option_tcp_connection_err(caller, &error)?,
            json_err(&error),
        )),
    }
}

fn unit_outcome(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    outcome: Result<(), String>,
) -> Result<
    (
        Option<wasmtime::Rooted<wasmtime::AnyRef>>,
        aver::replay::JsonValue,
    ),
    wasmtime::Error,
> {
    match outcome {
        Ok(()) => Ok((
            host_result_ok_unit(caller)?,
            json_ok(aver::replay::JsonValue::Null),
        )),
        Err(error) => Ok((
            host_result_err_unit_string(caller, &error)?,
            json_err(&error),
        )),
    }
}

fn json_socket(variant: &str, resource: aver::replay::JsonValue) -> aver::replay::JsonValue {
    let mut payload = serde_json::Map::new();
    payload.insert(
        "type".to_string(),
        aver::replay::JsonValue::String("Tcp.Socket".to_string()),
    );
    payload.insert(
        "name".to_string(),
        aver::replay::JsonValue::String(variant.to_string()),
    );
    payload.insert(
        "fields".to_string(),
        aver::replay::JsonValue::Array(vec![resource]),
    );
    let mut wrapper = serde_json::Map::new();
    wrapper.insert(
        "$variant".to_string(),
        aver::replay::JsonValue::Object(payload),
    );
    aver::replay::JsonValue::Object(wrapper)
}

fn socket_resource_id(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    value: &wasmtime::Val,
) -> Result<String, wasmtime::Error> {
    use wasmtime::Val;
    let resource_ref = match value {
        Val::AnyRef(Some(value)) => *value,
        _ => return Err(wasmtime::Error::msg("Tcp.poll: malformed socket resource")),
    };
    let resource = resource_ref
        .as_struct(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("Tcp.poll: malformed socket resource"))?;
    let id = resource.field(&mut *caller, 0)?;
    lm_string_to_host(caller, Some(&id))?
        .ok_or_else(|| wasmtime::Error::msg("Tcp.poll: malformed socket resource id"))
}

pub(super) struct PollEntry {
    pub(super) provider_order: Vec<u8>,
    /// Numeric value of an `Int` key. `None` for a wait keyed by anything
    /// else: this host is handed the key as a reference and never reads it.
    pub(super) numeric: Option<BigInt>,
    /// Where this entry sits in its own map's key order. The map states that
    /// order itself, so the wait can answer in it without the host knowing
    /// how the key compares.
    pub(super) order: usize,
    pub(super) key_ref: wasmtime::Rooted<wasmtime::AnyRef>,
    /// How the key records. `None` for a key this host cannot read, which
    /// makes a recording of that wait refuse rather than write a key it
    /// guessed at.
    pub(super) key_json: Option<aver::replay::JsonValue>,
    /// `None` for a job: jasisz/aver#1329 lets one wait set hold both, and a
    /// job is watched by the module rather than by the reactor.
    pub(super) socket: Option<aver_rt::tcp::TcpSocket>,
    pub(super) job_id: Option<i64>,
    pub(super) socket_json: aver::replay::JsonValue,
}

/// Decode one `Map<Int, Tcp.Socket>` into the entries `Tcp.poll` watches.
fn decode_poll_entries(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    value: Option<&wasmtime::Val>,
) -> Result<Vec<PollEntry>, wasmtime::Error> {
    decode_map_entries(caller, value, "Tcp.poll", false)
}

/// Decode one `Map<Int, Wait.Item>` into the entries the one wait of a turn
/// watches: sockets the reactor polls, and jobs that are ready the moment
/// they began (jasisz/aver#1329).
pub(super) fn decode_wait_entries(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    value: Option<&wasmtime::Val>,
) -> Result<Vec<PollEntry>, wasmtime::Error> {
    decode_map_entries(caller, value, "Wait.poll", true)
}

fn decode_map_entries(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    value: Option<&wasmtime::Val>,
    operation: &str,
    wait_items: bool,
) -> Result<Vec<PollEntry>, wasmtime::Error> {
    use wasmtime::Val;
    let map_ref = match value {
        Some(Val::AnyRef(Some(value))) => *value,
        _ => {
            return Err(wasmtime::Error::msg(format!(
                "{operation}: sockets must be a Map"
            )));
        }
    };
    let map = map_ref
        .as_struct(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg(format!("{operation}: malformed sockets Map")))?;
    let capacity = match map.field(&mut *caller, 1)? {
        Val::I32(capacity) if capacity >= 0 => capacity as u32,
        _ => {
            return Err(wasmtime::Error::msg(format!(
                "{operation}: malformed Map capacity"
            )));
        }
    };
    if capacity == 0 {
        return Ok(Vec::new());
    }
    let keys_ref = match map.field(&mut *caller, 2)? {
        Val::AnyRef(Some(value)) => value,
        _ => {
            return Err(wasmtime::Error::msg(format!(
                "{operation}: malformed Map keys"
            )));
        }
    };
    let values_ref = match map.field(&mut *caller, 3)? {
        Val::AnyRef(Some(value)) => value,
        _ => {
            return Err(wasmtime::Error::msg(format!(
                "{operation}: malformed Map values"
            )));
        }
    };
    let keys = keys_ref
        .as_array(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg(format!("{operation}: malformed Map keys array")))?;
    let values = values_ref
        .as_array(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg(format!("{operation}: malformed Map values array")))?;
    if keys.len(&*caller)? < capacity || values.len(&*caller)? < capacity {
        return Err(wasmtime::Error::msg(format!(
            "{operation}: Map arrays are shorter than its capacity"
        )));
    }

    // A wait set answers in the order its own map puts its keys in. The map
    // already knows that order and exports it as the sorted list of its
    // occupied buckets, so the host walks the buckets in it and carries a
    // position rather than a key it would have to know how to compare. The
    // socket poll needs none of this: its keys are whole numbers by contract,
    // so it walks bucket by bucket and orders numerically afterwards, exactly
    // as before. A wait with no such export is refused rather than answered
    // in an order it did not promise.
    let bucket_order = if wait_items {
        Some(map_key_order(caller, &map_ref, capacity)?.ok_or_else(|| {
            wasmtime::Error::msg(
                "Wait.poll: this module exports no wait set key order, so the order its answer owes the caller cannot be read",
            )
        })?)
    } else {
        None
    };
    let walk: Vec<u32> = match &bucket_order {
        Some(order) => order.clone(),
        None => (0..capacity).collect(),
    };

    // A key of the program's own type sits in the map directly while a
    // primitive key sits boxed, so the guest reads a bucket for this host
    // whenever it exports the accessor. The socket poll reads the boxed
    // layout in place, because its keys are whole numbers by contract.
    let key_at = match bucket_order {
        Some(_) => Some(
            caller
                .get_export("__rt_wait_set_key_at")
                .and_then(|export| export.into_func())
                .ok_or_else(|| {
                    wasmtime::Error::msg("Wait.poll: this module exports no wait set bucket reader")
                })?,
        ),
        None => None,
    };

    let mut entries = Vec::new();
    for (position, index) in walk.into_iter().enumerate() {
        let key = match &key_at {
            Some(key_at) => {
                let mut out = [Val::AnyRef(None)];
                key_at.call(
                    &mut *caller,
                    &[Val::AnyRef(Some(map_ref)), Val::I32(index as i32)],
                    &mut out,
                )?;
                out[0]
            }
            None => {
                let key_box_ref = match keys.get(&mut *caller, index)? {
                    Val::AnyRef(Some(value)) => value,
                    Val::AnyRef(None) => continue,
                    _ => {
                        return Err(wasmtime::Error::msg(format!(
                            "{operation}: malformed Int key box"
                        )));
                    }
                };
                let key_box = key_box_ref.as_struct(&*caller)?.ok_or_else(|| {
                    wasmtime::Error::msg(format!("{operation}: malformed Int key box"))
                })?;
                key_box.field(&mut *caller, 0)?
            }
        };
        if matches!(key, Val::AnyRef(None)) {
            continue;
        }
        let key_ref = match &key {
            Val::AnyRef(Some(value)) => *value,
            _ => {
                return Err(wasmtime::Error::msg(format!(
                    "{operation}: malformed map key"
                )));
            }
        };
        // `Tcp.poll` is keyed by whole numbers and stays so. A wait may be
        // keyed by anything a map accepts, so the numeric read is attempted
        // and allowed to fail: what the wait needs from a key is the
        // reference it was handed and where the map puts it.
        let numeric = read_int_key(caller, &key, operation, !wait_items)?;
        let provider_order = match &numeric {
            Some(key) => {
                let key_value = aver_rt::AverInt::from_str(&key.display)
                    .map_err(|_| wasmtime::Error::msg(format!("{operation}: malformed Int key")))?;
                aver_rt::provider::provider_value_order_key(&aver_rt::provider::ProviderValue::Int(
                    key_value,
                ))
                .map_err(wasmtime::Error::msg)?
            }
            None => Vec::new(),
        };
        let key_json = numeric.as_ref().map(guest_int_json);
        let numeric_value = numeric.as_ref().map(|key| key.big.clone());

        let mut socket_value = values.get(&mut *caller, index)?;
        // One wait set holds both kinds of thing. A `Wait.Item.Job` is ready
        // by construction on this target — the job ran at `begin` — so it
        // needs no socket at all; a `Wait.Item.Socket` unwraps to exactly the
        // value `Tcp.poll` would have been handed.
        if wait_items {
            match host_wait_item_kind(caller, Some(&socket_value))? {
                Some(1) => {
                    let job_id = wait_item_job_id(caller, &socket_value)?;
                    entries.push(PollEntry {
                        provider_order,
                        numeric: numeric_value,
                        order: position,
                        key_ref,
                        key_json,
                        socket: None,
                        job_id: Some(job_id),
                        socket_json: json_wait_item(
                            "Job",
                            json_capability_resource("Work.Job", job_id),
                        ),
                    });
                    continue;
                }
                Some(0) => {
                    socket_value = wait_item_payload(caller, &socket_value)?;
                }
                _ => {
                    return Err(wasmtime::Error::msg("Wait.poll: malformed Wait.Item value"));
                }
            }
        }
        let kind = host_tcp_socket_kind(caller, Some(&socket_value))?.ok_or_else(|| {
            wasmtime::Error::msg(format!("{operation}: malformed Tcp.Socket value"))
        })?;
        let wrapper_ref = match socket_value {
            Val::AnyRef(Some(value)) => value,
            _ => {
                return Err(wasmtime::Error::msg(format!(
                    "{operation}: malformed Tcp.Socket value"
                )));
            }
        };
        let wrapper = wrapper_ref.as_struct(&*caller)?.ok_or_else(|| {
            wasmtime::Error::msg(format!("{operation}: malformed Tcp.Socket variant"))
        })?;
        let resource = wrapper.field(&mut *caller, 0)?;
        let id = socket_resource_id(caller, &resource)?;
        let (variant, socket, resource_json) = match kind {
            0 => (
                "Listening",
                aver_rt::tcp::TcpSocket::Listening(aver_rt::TcpListener::from_id(id.clone())),
                json_record(
                    "Tcp.Listener",
                    vec![("id", aver::replay::JsonValue::String(id.clone()))],
                ),
            ),
            1 => (
                "Dialing",
                aver_rt::tcp::TcpSocket::Dialing(aver_rt::TcpDial::from_id(id.clone())),
                json_record(
                    "Tcp.Dial",
                    vec![("id", aver::replay::JsonValue::String(id.clone()))],
                ),
            ),
            2 => (
                "Connected",
                aver_rt::tcp::TcpSocket::Connected(aver_rt::TcpConnection::from_parts(
                    id.clone(),
                    String::new(),
                    0,
                )),
                json_connection(&id),
            ),
            3 => (
                "Sending",
                aver_rt::tcp::TcpSocket::Sending(aver_rt::TcpConnection::from_parts(
                    id.clone(),
                    String::new(),
                    0,
                )),
                json_connection(&id),
            ),
            _ => {
                return Err(wasmtime::Error::msg(format!(
                    "{operation}: unknown Tcp.Socket variant"
                )));
            }
        };
        let socket_json = json_socket(variant, resource_json);
        entries.push(PollEntry {
            provider_order,
            numeric: numeric_value,
            order: position,
            key_ref,
            key_json,
            socket: Some(socket),
            job_id: None,
            socket_json: if wait_items {
                json_wait_item("Socket", socket_json)
            } else {
                socket_json
            },
        });
    }
    Ok(entries)
}

/// The occupied buckets of one map, in the order that map puts its keys in.
///
/// `__rt_wait_set_order` is the map's own `order_slots` helper under a name
/// the host knows: it collects the occupied buckets and sorts them by the
/// same canonical key order `Map.keys` uses. Reading the order from the guest
/// is what lets a wait be keyed by any type a map accepts — the host never
/// compares a key, it only asks where the map put it.
///
/// `None` when the module exports no such helper, which is every module built
/// before the wait key became a choice and every module whose wait keys are
/// whole numbers the caller sorts numerically anyway.
fn map_key_order(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    map: &wasmtime::Rooted<wasmtime::AnyRef>,
    capacity: u32,
) -> Result<Option<Vec<u32>>, wasmtime::Error> {
    use wasmtime::Val;
    let Some(order_fn) = caller
        .get_export("__rt_wait_set_order")
        .and_then(|export| export.into_func())
    else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    order_fn.call(&mut *caller, &[Val::AnyRef(Some(*map))], &mut out)?;
    let Val::AnyRef(Some(order_ref)) = out[0] else {
        return Ok(None);
    };
    let order = order_ref
        .as_array(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("Wait.poll: malformed wait set key order"))?;
    let len = order.len(&*caller)?;
    let mut buckets = Vec::with_capacity(len as usize);
    for index in 0..len {
        let Val::I32(bucket) = order.get(&mut *caller, index)? else {
            return Err(wasmtime::Error::msg(
                "Wait.poll: malformed wait set key order entry",
            ));
        };
        if bucket < 0 || bucket as u32 >= capacity {
            return Err(wasmtime::Error::msg(
                "Wait.poll: wait set key order names a bucket outside the map",
            ));
        }
        buckets.push(bucket as u32);
    }
    Ok(Some(buckets))
}

/// Read one map key as a whole number.
///
/// `required` is true for the socket poll, whose keys are whole numbers by
/// contract, and false for the wait, whose key is whatever the caller keyed
/// its map by: there, a key this host cannot read as a number is an ordinary
/// key that travels by reference, not an error.
fn read_int_key(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    key: &wasmtime::Val,
    operation: &str,
    required: bool,
) -> Result<Option<GuestInt>, wasmtime::Error> {
    match decode_guest_int(caller, key, "Tcp.poll: malformed Int key") {
        Ok(value) => Ok(Some(value)),
        Err(error) => {
            if required {
                Err(error)
            } else {
                let _ = operation;
                Ok(None)
            }
        }
    }
}

/// The `$variant` shape one `Wait.Item` records as, matching the VM's.
fn json_wait_item(variant: &str, payload: aver::replay::JsonValue) -> aver::replay::JsonValue {
    let mut fields = serde_json::Map::new();
    fields.insert(
        "type".to_string(),
        aver::replay::JsonValue::String("Item".to_string()),
    );
    fields.insert(
        "name".to_string(),
        aver::replay::JsonValue::String(variant.to_string()),
    );
    fields.insert(
        "fields".to_string(),
        aver::replay::JsonValue::Array(vec![payload]),
    );
    let mut wrapper = serde_json::Map::new();
    wrapper.insert(
        "$variant".to_string(),
        aver::replay::JsonValue::Object(fields),
    );
    aver::replay::JsonValue::Object(wrapper)
}

/// The `$capabilityResource` shape a provider-owned handle records as.
pub(in crate::runtime::wasm_gc) fn json_capability_resource(
    type_name: &str,
    trace: i64,
) -> aver::replay::JsonValue {
    let mut payload = serde_json::Map::new();
    payload.insert(
        "trace".to_string(),
        aver::replay::JsonValue::String(trace.to_string()),
    );
    payload.insert(
        "type".to_string(),
        aver::replay::JsonValue::String(type_name.to_string()),
    );
    let mut wrapper = serde_json::Map::new();
    wrapper.insert(
        "$capabilityResource".to_string(),
        aver::replay::JsonValue::Object(payload),
    );
    aver::replay::JsonValue::Object(wrapper)
}

/// The one reference a `Wait.Item` variant carries.
fn wait_item_payload(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    value: &wasmtime::Val,
) -> Result<wasmtime::Val, wasmtime::Error> {
    use wasmtime::Val;
    let item_ref = match value {
        Val::AnyRef(Some(value)) => *value,
        _ => return Err(wasmtime::Error::msg("Wait.poll: malformed Wait.Item value")),
    };
    let item = item_ref
        .as_struct(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("Wait.poll: malformed Wait.Item value"))?;
    item.field(&mut *caller, 0)
}

/// The id a `Wait.Item.Job` handle carries, which is the handle's identity
/// and the trace token a recording names it by.
fn wait_item_job_id(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    value: &wasmtime::Val,
) -> Result<i64, wasmtime::Error> {
    let handle = wait_item_payload(caller, value)?;
    job_handle_id(caller, &handle)
}

/// The id field of one `Work.Job` handle.
pub(in crate::runtime::wasm_gc) fn job_handle_id(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    value: &wasmtime::Val,
) -> Result<i64, wasmtime::Error> {
    use wasmtime::Val;
    let handle_ref = match value {
        Val::AnyRef(Some(value)) => *value,
        _ => return Err(wasmtime::Error::msg("work: malformed job handle")),
    };
    let handle = handle_ref
        .as_struct(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("work: malformed job handle"))?;
    match handle.field(&mut *caller, 0)? {
        Val::I64(id) => Ok(id),
        _ => Err(wasmtime::Error::msg("work: malformed job handle id")),
    }
}

pub(super) fn poll_map_json(
    entries: &[PollEntry],
    operation: &str,
) -> Result<aver::replay::JsonValue, wasmtime::Error> {
    let mut pairs = Vec::with_capacity(entries.len());
    for entry in entries {
        pairs.push(aver::replay::JsonValue::Array(vec![
            recorded_key(entry, operation)?,
            entry.socket_json.clone(),
        ]));
    }
    let mut marker = serde_json::Map::new();
    marker.insert("$map".to_string(), aver::replay::JsonValue::Array(pairs));
    Ok(aver::replay::JsonValue::Object(marker))
}

/// How one wait-set key records.
///
/// A recording states the values an effect was given and the values it
/// answered, and a wasm-gc module hands this host its keys as bare
/// references: whole numbers it can read, anything else it cannot. Rather
/// than write a key it guessed at, a recording of such a wait is refused —
/// the same program records and replays on the bytecode VM and under
/// `--target rust`, where the key crosses as a value of a known type.
fn recorded_key(
    entry: &PollEntry,
    operation: &str,
) -> Result<aver::replay::JsonValue, wasmtime::Error> {
    entry.key_json.clone().ok_or_else(|| {
        wasmtime::Error::msg(format!(
            "{operation}: recording a wait keyed by anything but Int is not supported on wasm-gc; record this program on the bytecode VM or under --target rust"
        ))
    })
}

pub(super) fn recorded_keys(
    entries: &[&PollEntry],
    operation: &str,
) -> Result<Vec<aver::replay::JsonValue>, wasmtime::Error> {
    entries
        .iter()
        .map(|entry| recorded_key(entry, operation))
        .collect()
}

pub(super) fn replay_poll_result(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    cached: &aver::replay::JsonValue,
    entries: &[PollEntry],
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    replay_ready_result(caller, cached, entries, false)
}

/// The same, for the one wait of a turn.
///
/// A wait answers through its own factories, because its keys are the
/// program's own type while the socket poll's are always whole numbers, and
/// the two families are named apart for exactly that reason.
pub(super) fn replay_wait_result(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    cached: &aver::replay::JsonValue,
    entries: &[PollEntry],
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    replay_ready_result(caller, cached, entries, true)
}

fn replay_ready_result(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    cached: &aver::replay::JsonValue,
    entries: &[PollEntry],
    wait: bool,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let aver::replay::JsonValue::Object(marker) = cached else {
        return Err(wasmtime::Error::msg(
            "replay decode Tcp.poll: expected Result",
        ));
    };
    if let Some(aver::replay::JsonValue::String(error)) = marker.get("$err") {
        return if wait {
            super::factories::host_wait_result_err(caller, error)
        } else {
            host_result_err_list_int(caller, error)
        };
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
            .find(|entry| entry.key_json.as_ref() == Some(key))
            .ok_or_else(|| {
                wasmtime::Error::msg(
                    "replay decode Tcp.poll: ready ID is absent from the input Map",
                )
            })?;
        refs.push(entry.key_ref);
    }
    if wait {
        super::factories::host_wait_result_ok(caller, &refs)
    } else {
        host_result_ok_list_int_refs(caller, &refs)
    }
}

pub(super) fn guest_int_json(value: &GuestInt) -> aver::replay::JsonValue {
    match value.value {
        Some(value) => aver::replay::JsonValue::from(value),
        None => {
            let mut opaque = serde_json::Map::new();
            opaque.insert(
                "$opaque".to_string(),
                aver::replay::JsonValue::String(value.display.clone()),
            );
            aver::replay::JsonValue::Object(opaque)
        }
    }
}

fn bytes_json(bytes: &[u8]) -> aver::replay::JsonValue {
    json_record(
        "Bytes",
        vec![(
            "values",
            aver::replay::JsonValue::Array(
                bytes
                    .iter()
                    .copied()
                    .map(i64::from)
                    .map(aver::replay::JsonValue::from)
                    .collect(),
            ),
        )],
    )
}

fn option_bytes_outcome(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    outcome: Result<Option<Vec<u8>>, String>,
) -> Result<
    (
        Option<wasmtime::Rooted<wasmtime::AnyRef>>,
        aver::replay::JsonValue,
    ),
    wasmtime::Error,
> {
    match outcome {
        Ok(Some(bytes)) => {
            let ints = bytes.iter().copied().map(i64::from).collect::<Vec<_>>();
            Ok((
                host_result_option_bytes_some(caller, &ints)?,
                json_ok(json_some(bytes_json(&bytes))),
            ))
        }
        Ok(None) => Ok((host_result_option_bytes_none(caller)?, json_ok(json_none()))),
        Err(error) => Ok((
            host_result_option_bytes_err(caller, &error)?,
            json_err(&error),
        )),
    }
}

fn decode_result_option_bytes(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let (marker, value) = expect_marker(json, &["$ok", "$err"])?;
    match marker {
        "$err" => match value {
            aver::replay::JsonValue::String(error) => host_result_option_bytes_err(caller, error),
            _ => Err(wasmtime::Error::msg(
                "replay decode Result<Option<Bytes>,String>: Err is not String",
            )),
        },
        "$ok" => {
            let (option, value) = expect_marker(value, &["$some", "$none"])?;
            match option {
                "$none" => host_result_option_bytes_none(caller),
                "$some" => {
                    let fields = expect_record(value, "Bytes")?;
                    let items = match fields.get("values") {
                        Some(aver::replay::JsonValue::Array(items)) => items,
                        _ => {
                            return Err(wasmtime::Error::msg(
                                "replay decode Bytes: missing List<Int> values field",
                            ));
                        }
                    };
                    let ints = items
                        .iter()
                        .map(|item| {
                            item.as_i64().ok_or_else(|| {
                                wasmtime::Error::msg(format!(
                                    "replay decode Bytes.values: element is {item:?}"
                                ))
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    host_result_option_bytes_some(caller, &ints)
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
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
                .map(aver::replay::JsonValue::from)
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
                .map(|value| aver::replay::JsonValue::from(i64::from(value)))
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
                .filter_map(|n| n.value.map(aver::replay::JsonValue::from))
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
        let mut opaque = serde_json::Map::new();
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
