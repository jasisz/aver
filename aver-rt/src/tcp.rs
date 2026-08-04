use crate::{AverStr, TcpConnection};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

const CONNECT_TIMEOUT: Duration = Duration::from_secs(5);
const IO_TIMEOUT: Duration = Duration::from_secs(30);
const BODY_LIMIT: usize = 10 * 1024 * 1024;
const MAX_CONNECTIONS: usize = 256;

static NEXT_ID: AtomicU64 = AtomicU64::new(1);

thread_local! {
    static CONNECTIONS: RefCell<HashMap<String, BufReader<TcpStream>>> =
        RefCell::new(HashMap::new());
}

/// Phase 4.7+ fix #13 — cross-backend port validation.
/// Returns the port as `u16` when it fits, otherwise an Aver-side
/// error string. Lifts what used to be a VM `RuntimeError` panic
/// into a `Result.Err` so every backend (VM via `services::tcp`,
/// self-host via this fn, wasm-gc-bridge via this fn, wasip2 via
/// its own up-front check) surfaces the same catchable shape.
fn validate_port(port: i64) -> Result<u16, String> {
    if (0..=65535).contains(&port) {
        Ok(port as u16)
    } else {
        Err(format!("Tcp: port {port} is out of range (0\u{2013}65535)"))
    }
}

pub fn connect(host: &str, port: i64) -> Result<TcpConnection, String> {
    validate_port(port)?;
    let count = CONNECTIONS.with(|map| map.borrow().len());
    if count >= MAX_CONNECTIONS {
        return Err(format!(
            "Tcp.connect: connection limit reached ({} max)",
            MAX_CONNECTIONS
        ));
    }

    let socket_addr = resolve(&format!("{}:{}", host, port))?;
    let stream =
        TcpStream::connect_timeout(&socket_addr, CONNECT_TIMEOUT).map_err(|e| e.to_string())?;
    stream.set_read_timeout(Some(IO_TIMEOUT)).ok();
    stream.set_write_timeout(Some(IO_TIMEOUT)).ok();

    let id = format!("tcp-{}", NEXT_ID.fetch_add(1, Ordering::Relaxed));
    CONNECTIONS.with(|map| {
        map.borrow_mut().insert(id.clone(), BufReader::new(stream));
    });

    Ok(TcpConnection {
        id: AverStr::from(id),
        host: AverStr::from(host),
        port,
    })
}

pub fn write_line(conn: &TcpConnection, line: &str) -> Result<(), String> {
    CONNECTIONS.with(|map| {
        let mut borrow = map.borrow_mut();
        let id: &str = &conn.id;
        match borrow.get_mut(id) {
            None => Err(format!("Tcp.writeLine: unknown connection '{}'", conn.id)),
            Some(reader) => {
                let msg = format!("{}\r\n", line);
                reader
                    .get_mut()
                    .write_all(msg.as_bytes())
                    .map_err(|e| e.to_string())
            }
        }
    })
}

/// Byte-clean sibling of [`write_line`].
///
/// `write_line` appends `\r\n` and takes a `&str`, which is always valid UTF-8,
/// so `as_bytes()` re-encodes anything above `0x7F` into a multi-byte sequence:
/// the single byte `0xF9` cannot be put on the wire at all. Both behaviours are
/// wrong for a binary protocol — the appended bytes desynchronise a
/// length-prefixed stream, and the encoding corrupts the payload.
///
/// This writes `payload` exactly as given: nothing appended, nothing encoded.
/// An empty payload is a no-op.
pub fn write_bytes(conn: &TcpConnection, payload: &[u8]) -> Result<(), String> {
    CONNECTIONS.with(|map| {
        let mut borrow = map.borrow_mut();
        let id: &str = &conn.id;
        match borrow.get_mut(id) {
            None => Err(format!("Tcp.writeBytes: unknown connection '{}'", conn.id)),
            Some(reader) => reader
                .get_mut()
                .write_all(payload)
                .map_err(|e| e.to_string()),
        }
    })
}

pub fn read_line(conn: &TcpConnection) -> Result<String, String> {
    CONNECTIONS.with(|map| {
        let mut borrow = map.borrow_mut();
        let id: &str = &conn.id;
        match borrow.get_mut(id) {
            None => Err(format!("Tcp.readLine: unknown connection '{}'", conn.id)),
            Some(reader) => {
                let mut line = String::new();
                reader.read_line(&mut line).map_err(|e| e.to_string())?;
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                Ok(line)
            }
        }
    })
}

pub fn close(conn: &TcpConnection) -> Result<(), String> {
    let id: &str = &conn.id;
    let removed = CONNECTIONS.with(|map| map.borrow_mut().remove(id));
    match removed {
        Some(_) => Ok(()),
        None => Err(format!("Tcp.close: unknown connection '{}'", conn.id)),
    }
}

pub fn send(host: &str, port: i64, message: &str) -> Result<String, String> {
    validate_port(port)?;
    let socket_addr = resolve(&format!("{}:{}", host, port))?;
    let mut stream =
        TcpStream::connect_timeout(&socket_addr, CONNECT_TIMEOUT).map_err(|e| e.to_string())?;
    stream.set_read_timeout(Some(IO_TIMEOUT)).ok();
    stream.set_write_timeout(Some(IO_TIMEOUT)).ok();
    stream
        .write_all(message.as_bytes())
        .map_err(|e| e.to_string())?;
    stream.shutdown(std::net::Shutdown::Write).ok();

    let mut buf = Vec::new();
    Read::by_ref(&mut stream)
        .take(BODY_LIMIT as u64 + 1)
        .read_to_end(&mut buf)
        .map_err(|e| e.to_string())?;
    if buf.len() > BODY_LIMIT {
        return Err("Tcp.send: response exceeds 10 MB limit".to_string());
    }
    Ok(String::from_utf8_lossy(&buf).into_owned())
}

/// Byte-clean sibling of [`send`].
///
/// Identical socket behaviour — open, write, `shutdown(Write)`, read to EOF —
/// but the payload and the response stay `Vec<u8>` end to end. `send` converts
/// the response with `String::from_utf8_lossy`, which replaces every non-UTF-8
/// sequence with U+FFFD and cannot be undone; that makes it unusable for binary
/// protocols whose framing bytes are not valid UTF-8. This function performs no
/// encoding or decoding, so the caller sees exactly what the peer sent.
pub fn send_bytes(host: &str, port: i64, payload: &[u8]) -> Result<Vec<u8>, String> {
    validate_port(port)?;
    let socket_addr = resolve(&format!("{}:{}", host, port))?;
    let mut stream =
        TcpStream::connect_timeout(&socket_addr, CONNECT_TIMEOUT).map_err(|e| e.to_string())?;
    stream.set_read_timeout(Some(IO_TIMEOUT)).ok();
    stream.set_write_timeout(Some(IO_TIMEOUT)).ok();
    stream.write_all(payload).map_err(|e| e.to_string())?;
    stream.shutdown(std::net::Shutdown::Write).ok();

    let mut buf = Vec::new();
    Read::by_ref(&mut stream)
        .take(BODY_LIMIT as u64 + 1)
        .read_to_end(&mut buf)
        .map_err(|e| e.to_string())?;
    if buf.len() > BODY_LIMIT {
        return Err("Tcp.sendBytes: response exceeds 10 MB limit".to_string());
    }
    Ok(buf)
}

pub fn ping(host: &str, port: i64) -> Result<(), String> {
    validate_port(port)?;
    let socket_addr = resolve(&format!("{}:{}", host, port))?;
    TcpStream::connect_timeout(&socket_addr, CONNECT_TIMEOUT).map_err(|e| e.to_string())?;
    Ok(())
}

fn resolve(addr: &str) -> Result<std::net::SocketAddr, String> {
    addr.to_socket_addrs()
        .map_err(|e| format!("Tcp: DNS resolution failed for {}: {}", addr, e))?
        .next()
        .ok_or_else(|| format!("Tcp: no address found for {}", addr))
}
