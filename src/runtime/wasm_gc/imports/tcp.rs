//! `Tcp.*` host imports — connect / writeLine / readLine / close /
//! send / sendBytes / ping. Connection handles cross as opaque wasm-gc structs
//! built by `host_tcp_connection_make`; the host extracts the
//! `id` field via `host_tcp_connection_id`.

use num_bigint::{BigInt, Sign};

use super::super::RunWasmGcHost;
use super::super::decode::{
    decode_result_bytes, decode_result_string, decode_result_tcp_connection, decode_result_unit,
};
use super::factories::{
    host_result_err_bytes, host_result_err_string, host_result_err_unit_string,
    host_result_ok_bytes, host_result_ok_string, host_result_ok_unit,
    host_result_tcp_connection_err, host_result_tcp_connection_ok, host_tcp_connection_id,
    host_tcp_connection_make,
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
            record_effect_if_recording(caller, "Tcp.connect", args, outcome, caller_fn);
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
            let (result_ref, outcome) = match aver_rt::tcp::send(&host, port, &msg) {
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
            let (payload, payload_json) = decode_byte_payload(caller, params.get(2))?;
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
                Ok(payload) => match aver_rt::tcp::send_bytes(&host, port, &payload) {
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
            let (result_ref, outcome) = match aver_rt::tcp::ping(&host, port) {
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

struct GuestInt {
    display: String,
    value: Option<i64>,
}

fn decode_byte_payload(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    val: Option<&wasmtime::Val>,
) -> Result<(Result<Vec<u8>, String>, aver::replay::JsonValue), wasmtime::Error> {
    use wasmtime::Val;
    let bytes_ref = match val {
        Some(Val::AnyRef(Some(r))) => *r,
        _ => {
            return Err(wasmtime::Error::msg("Tcp.sendBytes: payload must be Bytes"));
        }
    };
    let bytes = bytes_ref
        .as_struct(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("Tcp.sendBytes: payload must be Bytes"))?;
    let mut current = match bytes.field(&mut *caller, 0)? {
        Val::AnyRef(r) => r,
        _ => {
            return Err(wasmtime::Error::msg(
                "Tcp.sendBytes: malformed Bytes.values carrier",
            ));
        }
    };
    let mut ints = Vec::new();
    while let Some(node_ref) = current {
        let node = node_ref
            .as_struct(&*caller)?
            .ok_or_else(|| wasmtime::Error::msg("Tcp.sendBytes: malformed Bytes.values carrier"))?;
        let head = node.field(&mut *caller, 0)?;
        let tail = node.field(&mut *caller, 1)?;
        ints.push(decode_guest_int(caller, &head)?);
        current = match tail {
            Val::AnyRef(r) => r,
            _ => {
                return Err(wasmtime::Error::msg(
                    "Tcp.sendBytes: malformed Bytes.values carrier",
                ));
            }
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
                Err(byte_range_error(&int.display, idx)),
                json_record("Bytes", vec![("values", values_json)]),
            ));
        };
        match u8::try_from(n) {
            Ok(byte) => bytes.push(byte),
            Err(_) => {
                return Ok((
                    Err(byte_range_error(&int.display, idx)),
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

fn decode_guest_int(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    val: &wasmtime::Val,
) -> Result<GuestInt, wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match val {
        Val::AnyRef(Some(r)) => *r,
        _ => {
            return Err(wasmtime::Error::msg(
                "Tcp.sendBytes: malformed Bytes.values carrier",
            ));
        }
    };
    let int_ref = any_ref
        .as_struct(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("Tcp.sendBytes: malformed Bytes.values carrier"))?;
    let small = match int_ref.field(&mut *caller, 0)? {
        Val::I64(n) => n,
        _ => {
            return Err(wasmtime::Error::msg(
                "Tcp.sendBytes: malformed Bytes.values carrier",
            ));
        }
    };
    let magnitude = int_ref.field(&mut *caller, 1)?;
    let magnitude_ref = match magnitude {
        Val::AnyRef(r) => r,
        _ => {
            return Err(wasmtime::Error::msg(
                "Tcp.sendBytes: malformed Bytes.values carrier",
            ));
        }
    };
    let Some(magnitude_ref) = magnitude_ref else {
        return Ok(GuestInt {
            display: small.to_string(),
            value: Some(small),
        });
    };

    let sign = match int_ref.field(&mut *caller, 2)? {
        Val::I32(n) if n < 0 => Sign::Minus,
        Val::I32(0) => Sign::NoSign,
        Val::I32(_) => Sign::Plus,
        _ => {
            return Err(wasmtime::Error::msg(
                "Tcp.sendBytes: malformed Bytes.values carrier",
            ));
        }
    };
    let magnitude = magnitude_ref
        .as_array(&*caller)?
        .ok_or_else(|| wasmtime::Error::msg("Tcp.sendBytes: malformed Bytes.values carrier"))?;
    let len = magnitude.len(&*caller)?;
    let mut limbs = Vec::with_capacity(len as usize);
    for idx in 0..len {
        let limb = match magnitude.get(&mut *caller, idx)? {
            Val::I64(n) => u32::try_from(n)
                .map_err(|_| wasmtime::Error::msg("Tcp.sendBytes: malformed Int magnitude limb"))?,
            _ => {
                return Err(wasmtime::Error::msg(
                    "Tcp.sendBytes: malformed Int magnitude limb",
                ));
            }
        };
        limbs.push(limb);
    }
    Ok(GuestInt {
        display: BigInt::from_slice(sign, &limbs).to_string(),
        value: None,
    })
}

fn byte_range_error(value: &str, idx: usize) -> String {
    format!(
        "Tcp.sendBytes: byte {} at index {} is out of range (0\u{2013}255)",
        value, idx
    )
}
