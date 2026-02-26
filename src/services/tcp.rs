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
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use crate::value::{RuntimeError, Value};

const CONNECT_TIMEOUT: Duration = Duration::from_secs(5);
const IO_TIMEOUT: Duration = Duration::from_secs(30);
const BODY_LIMIT: usize = 10 * 1024 * 1024; // 10 MB
const MAX_CONNECTIONS: usize = 256;

static NEXT_ID: AtomicU64 = AtomicU64::new(1);

thread_local! {
    static CONNECTIONS: RefCell<HashMap<String, BufReader<TcpStream>>> =
        RefCell::new(HashMap::new());
}

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
        "Tcp.send" | "Tcp.ping" | "Tcp.connect" | "Tcp.writeLine" | "Tcp.readLine"
        | "Tcp.close" => &["Tcp"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Tcp.send" => Some(tcp_send(&args)),
        "Tcp.ping" => Some(tcp_ping(&args)),
        "Tcp.connect" => Some(tcp_connect(&args)),
        "Tcp.writeLine" => Some(tcp_write_line(&args)),
        "Tcp.readLine" => Some(tcp_read_line(&args)),
        "Tcp.close" => Some(tcp_close(&args)),
        _ => None,
    }
}

// ─── One-shot helpers ─────────────────────────────────────────────────────────

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
    use std::io::Read;
    if let Err(e) = std::io::Read::by_ref(&mut stream)
        .take(BODY_LIMIT as u64 + 1)
        .read_to_end(&mut buf)
    {
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

fn tcp_ping(args: &[Value]) -> Result<Value, RuntimeError> {
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

// ─── Persistent-connection helpers ────────────────────────────────────────────

fn tcp_connect(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Tcp.connect() takes 2 arguments (host, port), got {}",
            args.len()
        )));
    }
    let host = str_arg(&args[0], "Tcp.connect: host must be a String")?;
    let port = int_arg(&args[1], "Tcp.connect: port must be an Int")?;

    // Guard against unbounded connection growth
    let count = CONNECTIONS.with(|map| map.borrow().len());
    if count >= MAX_CONNECTIONS {
        return Ok(Value::Err(Box::new(Value::Str(format!(
            "Tcp.connect: connection limit reached ({} max). Close unused connections first.",
            MAX_CONNECTIONS
        )))));
    }

    let addr = format!("{}:{}", host, port);
    let socket_addr = match resolve(&addr) {
        Ok(a) => a,
        Err(e) => return Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    };

    let stream = match TcpStream::connect_timeout(&socket_addr, CONNECT_TIMEOUT) {
        Ok(s) => s,
        Err(e) => return Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    };

    stream.set_read_timeout(Some(IO_TIMEOUT)).ok();
    stream.set_write_timeout(Some(IO_TIMEOUT)).ok();

    let id = format!("tcp-{}", NEXT_ID.fetch_add(1, Ordering::Relaxed));
    CONNECTIONS.with(|map| {
        map.borrow_mut().insert(id.clone(), BufReader::new(stream));
    });

    let conn_record = Value::Record {
        type_name: "Tcp.Connection".to_string(),
        fields: vec![
            ("id".to_string(), Value::Str(id)),
            ("host".to_string(), Value::Str(host)),
            ("port".to_string(), Value::Int(port as i64)),
        ],
    };
    Ok(Value::Ok(Box::new(conn_record)))
}

fn tcp_write_line(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Tcp.writeLine() takes 2 arguments (conn, line), got {}",
            args.len()
        )));
    }
    let conn_id = conn_id_arg(&args[0], "Tcp.writeLine")?;
    let line = str_arg(&args[1], "Tcp.writeLine: line must be a String")?;

    let result = CONNECTIONS.with(|map| {
        let mut borrow = map.borrow_mut();
        match borrow.get_mut(&conn_id) {
            None => Err(format!("Tcp.writeLine: unknown connection '{}'", conn_id)),
            Some(reader) => {
                let msg = format!("{}\r\n", line);
                reader
                    .get_mut()
                    .write_all(msg.as_bytes())
                    .map_err(|e| e.to_string())
            }
        }
    });

    match result {
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
    let conn_id = conn_id_arg(&args[0], "Tcp.readLine")?;

    let result = CONNECTIONS.with(|map| {
        let mut borrow = map.borrow_mut();
        match borrow.get_mut(&conn_id) {
            None => Err(format!("Tcp.readLine: unknown connection '{}'", conn_id)),
            Some(reader) => {
                let mut line = String::new();
                reader.read_line(&mut line).map_err(|e| e.to_string())?;
                // Strip trailing \r\n or \n
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                Ok(line)
            }
        }
    });

    match result {
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
    let conn_id = conn_id_arg(&args[0], "Tcp.close")?;

    let removed = CONNECTIONS.with(|map| map.borrow_mut().remove(&conn_id));
    match removed {
        Some(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        None => Ok(Value::Err(Box::new(Value::Str(format!(
            "Tcp.close: unknown connection '{}'",
            conn_id
        ))))),
    }
}

// ─── Shared utilities ─────────────────────────────────────────────────────────

fn resolve(addr: &str) -> Result<std::net::SocketAddr, RuntimeError> {
    addr.to_socket_addrs()
        .map_err(|e| {
            RuntimeError::Error(format!("Tcp: DNS resolution failed for {}: {}", addr, e))
        })?
        .next()
        .ok_or_else(|| RuntimeError::Error(format!("Tcp: no address found for {}", addr)))
}

fn conn_id_arg(val: &Value, method: &str) -> Result<String, RuntimeError> {
    match val {
        Value::Record { type_name, fields } if type_name == "Tcp.Connection" => {
            for (name, v) in fields {
                if name == "id" {
                    if let Value::Str(s) = v {
                        return Ok(s.clone());
                    }
                }
            }
            Err(RuntimeError::Error(format!(
                "{}: Tcp.Connection record missing 'id' field",
                method
            )))
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

fn int_arg(val: &Value, msg: &str) -> Result<u16, RuntimeError> {
    match val {
        Value::Int(n) if (0..=65535).contains(n) => Ok(*n as u16),
        Value::Int(n) => Err(RuntimeError::Error(format!(
            "Tcp: port {} is out of range (0–65535)",
            n
        ))),
        _ => Err(RuntimeError::Error(msg.to_string())),
    }
}
