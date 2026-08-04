/// Tcp service — raw TCP client.
///
/// One-shot methods:
///   `Tcp.send(host, port, message)` — connect, write message, read response, close.
///   `Tcp.sendBytes(host, port, payload)` — same, but byte-clean: `List<Int>` in,
///       `List<Int>` out, no UTF-8 encoding or decoding on either side. `send`
///       decodes the response with `String::from_utf8_lossy`, which destroys any
///       non-UTF-8 byte irrecoverably; binary protocols need this variant.
///   `Tcp.ping(host, port)`          — check whether the port accepts connections.
///
/// Persistent-connection methods:
///   `Tcp.connect(host, port)`           → Result<Tcp.Connection, String>  (returns opaque connection record)
///   `Tcp.writeLine(conn, line)`         → Result<Unit, String>    (writes line + \r\n)
///   `Tcp.readLine(conn)`                → Result<String, String>  (reads until \n, strips \r\n)
///   `Tcp.readBytes(conn, n)`            → Result<List<Int>, String>  (exactly n bytes, no decoding)
///   `Tcp.close(conn)`                   → Result<Unit, String>
///
/// Each method requires its own exact effect (`Tcp.send`, `Tcp.ping`, etc.).
use std::collections::HashMap;
use std::sync::Arc as Rc;

use aver_rt::{AverList, TcpConnection};

use crate::nan_value::{Arena, NanValue, NanValueConvert};
use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "send",
        "sendBytes",
        "ping",
        "connect",
        "writeLine",
        "readLine",
        "readBytes",
        "close",
    ] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Tcp.{}", method)),
        );
    }
    global.insert(
        "Tcp".to_string(),
        Value::Namespace {
            name: "Tcp".to_string(),
            members,
        },
    );
}

pub const DECLARED_EFFECTS: &[&str] = &[
    "Tcp.send",
    "Tcp.sendBytes",
    "Tcp.ping",
    "Tcp.connect",
    "Tcp.writeLine",
    "Tcp.readLine",
    "Tcp.readBytes",
    "Tcp.close",
];

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Tcp.send" => &["Tcp.send"],
        "Tcp.sendBytes" => &["Tcp.sendBytes"],
        "Tcp.ping" => &["Tcp.ping"],
        "Tcp.connect" => &["Tcp.connect"],
        "Tcp.writeLine" => &["Tcp.writeLine"],
        "Tcp.readLine" => &["Tcp.readLine"],
        "Tcp.readBytes" => &["Tcp.readBytes"],
        "Tcp.close" => &["Tcp.close"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Tcp.send" => Some(tcp_send(args)),
        "Tcp.sendBytes" => Some(tcp_send_bytes(args)),
        "Tcp.ping" => Some(tcp_ping(args)),
        "Tcp.connect" => Some(tcp_connect(args)),
        "Tcp.writeLine" => Some(tcp_write_line(args)),
        "Tcp.readLine" => Some(tcp_read_line(args)),
        "Tcp.readBytes" => Some(tcp_read_bytes(args)),
        "Tcp.close" => Some(tcp_close(args)),
        _ => None,
    }
}

fn tcp_send(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "Tcp.send() takes 3 arguments (host, port, message), got {}",
            args.len()
        )));
    }
    let host = str_arg(&args[0], "Tcp.send: host must be a String")?;
    let port = int_arg(&args[1], "Tcp.send: port must be an Int")?;
    let message = str_arg(&args[2], "Tcp.send: message must be a String")?;

    match aver_rt::tcp::send(&host, port, &message) {
        Ok(response) => Ok(Value::Ok(Box::new(Value::Str(response)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

fn tcp_send_bytes(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "Tcp.sendBytes() takes 3 arguments (host, port, payload), got {}",
            args.len()
        )));
    }
    let host = str_arg(&args[0], "Tcp.sendBytes: host must be a String")?;
    let port = int_arg(&args[1], "Tcp.sendBytes: port must be an Int")?;
    let payload = match bytes_arg(&args[2], "Tcp.sendBytes")? {
        Ok(bytes) => bytes,
        // Out-of-range byte values are a value error, not a type error, so they
        // surface as a catchable `Result.Err` — same treatment the port range
        // gets in `aver-rt::tcp` rather than a VM-only trap.
        Err(msg) => return Ok(Value::Err(Box::new(Value::Str(msg)))),
    };

    match aver_rt::tcp::send_bytes(&host, port, &payload) {
        Ok(response) => Ok(Value::Ok(Box::new(bytes_to_value(&response)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

fn tcp_ping(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Tcp.ping() takes 2 arguments (host, port), got {}",
            args.len()
        )));
    }
    let host = str_arg(&args[0], "Tcp.ping: host must be a String")?;
    let port = int_arg(&args[1], "Tcp.ping: port must be an Int")?;

    match aver_rt::tcp::ping(&host, port) {
        Ok(()) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

fn tcp_connect(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Tcp.connect() takes 2 arguments (host, port), got {}",
            args.len()
        )));
    }
    let host = str_arg(&args[0], "Tcp.connect: host must be a String")?;
    let port = int_arg(&args[1], "Tcp.connect: port must be an Int")?;

    match aver_rt::tcp::connect(&host, port) {
        Ok(conn) => Ok(Value::Ok(Box::new(tcp_connection_to_value(conn)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

fn tcp_write_line(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Tcp.writeLine() takes 2 arguments (conn, line), got {}",
            args.len()
        )));
    }
    let conn = tcp_connection_arg(&args[0], "Tcp.writeLine")?;
    let line = str_arg(&args[1], "Tcp.writeLine: line must be a String")?;

    match aver_rt::tcp::write_line(&conn, &line) {
        Ok(()) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

fn tcp_read_line(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Tcp.readLine() takes 1 argument (conn), got {}",
            args.len()
        )));
    }
    let conn = tcp_connection_arg(&args[0], "Tcp.readLine")?;

    match aver_rt::tcp::read_line(&conn) {
        Ok(line) => Ok(Value::Ok(Box::new(Value::Str(line)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

fn tcp_read_bytes(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Tcp.readBytes() takes 2 arguments (conn, count), got {}",
            args.len()
        )));
    }
    let conn = tcp_connection_arg(&args[0], "Tcp.readBytes")?;
    let count = match count_arg(&args[1], "Tcp.readBytes")? {
        Ok(n) => n,
        Err(msg) => return Ok(Value::Err(Box::new(Value::Str(msg)))),
    };

    match aver_rt::tcp::read_bytes(&conn, count) {
        Ok(bytes) => Ok(Value::Ok(Box::new(bytes_to_value(&bytes)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

fn tcp_close(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Tcp.close() takes 1 argument (conn), got {}",
            args.len()
        )));
    }
    let conn = tcp_connection_arg(&args[0], "Tcp.close")?;

    match aver_rt::tcp::close(&conn) {
        Ok(()) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

fn tcp_connection_to_value(conn: TcpConnection) -> Value {
    Value::Record {
        type_name: "Tcp.Connection".to_string(),
        fields: vec![
            ("id".to_string(), Value::Str(conn.id.to_string())),
            ("host".to_string(), Value::Str(conn.host.to_string())),
            ("port".to_string(), Value::int(conn.port)),
        ]
        .into(),
    }
}

fn tcp_connection_arg(val: &Value, method: &str) -> Result<TcpConnection, RuntimeError> {
    match val {
        Value::Record { fields, .. } => {
            let mut id = None;
            let mut host = None;
            let mut port = None;
            for (name, value) in fields.iter() {
                match (name.as_str(), value) {
                    ("id", Value::Str(s)) => id = Some(s.clone()),
                    ("host", Value::Str(s)) => host = Some(s.clone()),
                    ("port", Value::Int(n)) => port = n.to_i64(),
                    _ => {}
                }
            }
            let id = id.ok_or_else(|| {
                RuntimeError::Error(format!(
                    "{}: Tcp.Connection record missing 'id' field",
                    method
                ))
            })?;
            let host = host.ok_or_else(|| {
                RuntimeError::Error(format!(
                    "{}: Tcp.Connection record missing 'host' field",
                    method
                ))
            })?;
            let port = port.ok_or_else(|| {
                RuntimeError::Error(format!(
                    "{}: Tcp.Connection record missing 'port' field",
                    method
                ))
            })?;
            Ok(TcpConnection::from_parts(id, host, port))
        }
        _ => Err(RuntimeError::Error(format!(
            "{}: first argument must be a Tcp.Connection, got {:?}",
            method, val
        ))),
    }
}

fn str_arg(val: &Value, msg: &str) -> Result<String, RuntimeError> {
    match val {
        Value::Str(s) => Ok(s.clone()),
        _ => Err(RuntimeError::Error(msg.to_string())),
    }
}

/// Convert a `List<Int>` argument into raw bytes.
///
/// The outer `Result` is the type check (wrong shape is a `RuntimeError`); the
/// inner one is the value check (an Int outside `0..=255` is a catchable Aver
/// `Result.Err`, reported with its index so a long payload is debuggable).
#[allow(clippy::type_complexity)]
fn bytes_arg(val: &Value, method: &str) -> Result<Result<Vec<u8>, String>, RuntimeError> {
    let items = match val {
        Value::List(items) => items,
        _ => {
            return Err(RuntimeError::Error(format!(
                "{}: payload must be a List<Int>",
                method
            )));
        }
    };
    let mut out = Vec::with_capacity(items.len());
    for (idx, item) in items.iter().enumerate() {
        let n = match item {
            Value::Int(n) => match n.to_i64() {
                Some(n) => n,
                // An `Int` outside `i64` is still an `Int` — a fortiori out
                // of byte range, so it takes the catchable value-error path,
                // not the type-error one.
                None => {
                    return Ok(Err(format!(
                        "{}: byte {} at index {} is out of range (0\u{2013}255)",
                        method, n, idx
                    )));
                }
            },
            _ => {
                return Err(RuntimeError::Error(format!(
                    "{}: payload must be a List<Int>",
                    method
                )));
            }
        };
        match u8::try_from(n) {
            Ok(b) => out.push(b),
            Err(_) => {
                return Ok(Err(format!(
                    "{}: byte {} at index {} is out of range (0\u{2013}255)",
                    method, n, idx
                )));
            }
        }
    }
    Ok(Ok(out))
}

/// Read a byte-count argument.
///
/// Outer `Result` is the type check; inner is the value check. An `Int` too
/// large for `i64` is still an `Int` — a fortiori past the read limit — so it
/// takes the catchable value-error path rather than being reported as a bogus
/// type error, matching how `bytes_arg` treats an out-of-range byte.
#[allow(clippy::type_complexity)]
fn count_arg(val: &Value, method: &str) -> Result<Result<i64, String>, RuntimeError> {
    match val {
        Value::Int(n) => match n.to_i64() {
            Some(n) => Ok(Ok(n)),
            None => Ok(Err(format!(
                "{}: count {} exceeds the read limit",
                method, n
            ))),
        },
        _ => Err(RuntimeError::Error(format!(
            "{}: count must be an Int",
            method
        ))),
    }
}

fn bytes_to_value(bytes: &[u8]) -> Value {
    let items: Vec<Value> = bytes.iter().map(|b| Value::int(*b as i64)).collect();
    Value::List(AverList::from_vec(items))
}

fn int_arg(val: &Value, msg: &str) -> Result<i64, RuntimeError> {
    // Phase 4.7+ fix #13 — type check only; the port-range check
    // moved into `aver-rt::tcp::{connect, send, ping}` so every
    // backend surfaces the same catchable Aver-side
    // `Result.Err("Tcp: port N is out of range")` instead of the
    // VM-only `RuntimeError` trap.
    match val {
        Value::Int(n) => n
            .to_i64()
            .ok_or_else(|| RuntimeError::Error(msg.to_string())),
        _ => Err(RuntimeError::Error(msg.to_string())),
    }
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &[
        "send",
        "sendBytes",
        "ping",
        "connect",
        "writeLine",
        "readLine",
        "readBytes",
        "close",
    ];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Tcp.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Tcp"),
        members,
    });
    global.insert("Tcp".to_string(), NanValue::new_namespace(ns_idx));
}

/// Bridge: convert NanValue args to Value, call old implementation, convert result back.
pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    if !matches!(
        name,
        "Tcp.send"
            | "Tcp.sendBytes"
            | "Tcp.ping"
            | "Tcp.connect"
            | "Tcp.writeLine"
            | "Tcp.readLine"
            | "Tcp.readBytes"
            | "Tcp.close"
    ) {
        return None;
    }
    let old_args: Vec<Value> = args.iter().map(|nv| nv.to_value(arena)).collect();
    let result = call(name, &old_args)?;
    Some(result.map(|v| NanValue::from_value(&v, arena)))
}
