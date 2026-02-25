/// Tcp service — raw TCP client.
///
/// Two methods:
///   `Tcp.send(host, port, message)` — connect, write message, read response, close.
///   `Tcp.ping(host, port)`          — check whether the port accepts connections.
///
/// Both require `! [Tcp]`.
use std::collections::HashMap;
use std::io::{Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::time::Duration;

use crate::value::{RuntimeError, Value};

const CONNECT_TIMEOUT: Duration = Duration::from_secs(5);
const IO_TIMEOUT: Duration = Duration::from_secs(30);
const BODY_LIMIT: usize = 10 * 1024 * 1024; // 10 MB

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["send", "ping"] {
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
        "Tcp.send" | "Tcp.ping" => &["Tcp"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: Vec<Value>) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Tcp.send" => Some(tcp_send(args)),
        "Tcp.ping" => Some(tcp_ping(args)),
        _ => None,
    }
}

// ─── Private helpers ──────────────────────────────────────────────────────────

fn tcp_send(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "Tcp.send() takes 3 arguments (host, port, message), got {}",
            args.len()
        )));
    }
    let host = str_arg(&args[0], "Tcp.send: host must be a String")?;
    let port = int_arg(&args[1], "Tcp.send: port must be an Int")?;
    let message = str_arg(&args[2], "Tcp.send: message must be a String")?;

    let addr = format!("{}:{}", host, port);
    let socket_addr = match resolve(&addr) {
        Ok(a) => a,
        Err(e) => return Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    };
    let mut stream = match TcpStream::connect_timeout(&socket_addr, CONNECT_TIMEOUT) {
        Ok(s) => s,
        Err(e) => return Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    };

    stream.set_read_timeout(Some(IO_TIMEOUT)).ok();
    stream.set_write_timeout(Some(IO_TIMEOUT)).ok();

    if let Err(e) = stream.write_all(message.as_bytes()) {
        return Ok(Value::Err(Box::new(Value::Str(e.to_string()))));
    }

    // Signal end-of-write so the server can detect EOF if it reads until close.
    stream.shutdown(std::net::Shutdown::Write).ok();

    let mut buf = Vec::new();
    if let Err(e) = stream.take(BODY_LIMIT as u64 + 1).read_to_end(&mut buf) {
        return Ok(Value::Err(Box::new(Value::Str(e.to_string()))));
    }

    if buf.len() > BODY_LIMIT {
        return Ok(Value::Err(Box::new(Value::Str(
            "Tcp.send: response exceeds 10 MB limit".to_string(),
        ))));
    }

    let response = String::from_utf8_lossy(&buf).into_owned();
    Ok(Value::Ok(Box::new(Value::Str(response))))
}

fn tcp_ping(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Tcp.ping() takes 2 arguments (host, port), got {}",
            args.len()
        )));
    }
    let host = str_arg(&args[0], "Tcp.ping: host must be a String")?;
    let port = int_arg(&args[1], "Tcp.ping: port must be an Int")?;

    let addr = format!("{}:{}", host, port);
    let socket_addr = resolve(&addr)?;
    match TcpStream::connect_timeout(&socket_addr, CONNECT_TIMEOUT) {
        Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn resolve(addr: &str) -> Result<std::net::SocketAddr, RuntimeError> {
    addr.to_socket_addrs()
        .map_err(|e| RuntimeError::Error(format!("Tcp: DNS resolution failed for {}: {}", addr, e)))?
        .next()
        .ok_or_else(|| RuntimeError::Error(format!("Tcp: no address found for {}", addr)))
}

fn str_arg(val: &Value, msg: &str) -> Result<String, RuntimeError> {
    match val {
        Value::Str(s) => Ok(s.clone()),
        _ => Err(RuntimeError::Error(msg.to_string())),
    }
}

fn int_arg(val: &Value, msg: &str) -> Result<u16, RuntimeError> {
    match val {
        Value::Int(n) if (0..=65535).contains(n) => Ok(*n as u16),
        Value::Int(n) => Err(RuntimeError::Error(format!(
            "Tcp: port {} is out of range (0–65535)", n
        ))),
        _ => Err(RuntimeError::Error(msg.to_string())),
    }
}
