/// Tcp service — raw TCP client.
///
/// One-shot methods:
///   `Tcp.send(host, port, message)` — connect, write message, read response, close.
///   `Tcp.ping(host, port)`          — check whether the port accepts connections.
///
/// Persistent-connection methods:
///   `Tcp.connect(host, port)`           → Result<Tcp.Connection, String>  (returns opaque connection record)
///   `Tcp.writeLine(conn, line)`         → Result<Unit, String>    (writes line + \r\n)
///   `Tcp.readLine(conn)`                → Result<String, String>  (reads until \n, strips \r\n)
///   `Tcp.close(conn)`                   → Result<Unit, String>
///
/// All methods require `! [Tcp]`.
use std::collections::HashMap;

use aver_rt::TcpConnection;

use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["send", "ping", "connect", "writeLine", "readLine", "close"] {
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

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Tcp.send" => &["Tcp.send"],
        "Tcp.ping" => &["Tcp.ping"],
        "Tcp.connect" => &["Tcp.connect"],
        "Tcp.writeLine" => &["Tcp.writeLine"],
        "Tcp.readLine" => &["Tcp.readLine"],
        "Tcp.close" => &["Tcp.close"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Tcp.send" => Some(tcp_send(args)),
        "Tcp.ping" => Some(tcp_ping(args)),
        "Tcp.connect" => Some(tcp_connect(args)),
        "Tcp.writeLine" => Some(tcp_write_line(args)),
        "Tcp.readLine" => Some(tcp_read_line(args)),
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
            ("id".to_string(), Value::Str(conn.id)),
            ("host".to_string(), Value::Str(conn.host)),
            ("port".to_string(), Value::Int(conn.port)),
        ],
    }
}

fn tcp_connection_arg(val: &Value, method: &str) -> Result<TcpConnection, RuntimeError> {
    match val {
        Value::Record { type_name, fields } if type_name == "Tcp.Connection" => {
            let mut id = None;
            let mut host = None;
            let mut port = None;
            for (name, value) in fields {
                match (name.as_str(), value) {
                    ("id", Value::Str(s)) => id = Some(s.clone()),
                    ("host", Value::Str(s)) => host = Some(s.clone()),
                    ("port", Value::Int(n)) => port = Some(*n),
                    _ => {}
                }
            }
            let id = id.ok_or_else(|| {
                RuntimeError::Error(format!("{}: Tcp.Connection record missing 'id' field", method))
            })?;
            let host = host.ok_or_else(|| {
                RuntimeError::Error(format!("{}: Tcp.Connection record missing 'host' field", method))
            })?;
            let port = port.ok_or_else(|| {
                RuntimeError::Error(format!("{}: Tcp.Connection record missing 'port' field", method))
            })?;
            Ok(TcpConnection { id, host, port })
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

fn int_arg(val: &Value, msg: &str) -> Result<i64, RuntimeError> {
    match val {
        Value::Int(n) if (0..=65535).contains(n) => Ok(*n),
        Value::Int(n) => Err(RuntimeError::Error(format!(
            "Tcp: port {} is out of range (0–65535)",
            n
        ))),
        _ => Err(RuntimeError::Error(msg.to_string())),
    }
}
