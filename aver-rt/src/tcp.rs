use crate::{AverStr, TcpConnection};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

pub const DEFAULT_CONNECT_TIMEOUT_SECS: u64 = 5;
pub const DEFAULT_REQUEST_IDLE_TIMEOUT_SECS: u64 = 30;
const BODY_LIMIT: usize = 10 * 1024 * 1024;
const MAX_CONNECTIONS: usize = 256;

/// Deployment settings for the standard native Tcp provider.
///
/// These deadlines apply only while opening a socket and to the bounded
/// one-shot request/response operations. Persistent session I/O deliberately
/// has no deadline: timing out after consuming part of a frame would leave the
/// caller with a silently desynchronised stream.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TcpSettings {
    pub connect_timeout: Duration,
    pub request_idle_timeout: Duration,
}

impl TcpSettings {
    pub fn from_secs(
        connect_timeout_secs: u64,
        request_idle_timeout_secs: u64,
    ) -> Result<Self, String> {
        if connect_timeout_secs == 0 {
            return Err("Tcp connect timeout must be greater than zero".to_string());
        }
        if request_idle_timeout_secs == 0 {
            return Err("Tcp request idle timeout must be greater than zero".to_string());
        }
        Ok(Self {
            connect_timeout: Duration::from_secs(connect_timeout_secs),
            request_idle_timeout: Duration::from_secs(request_idle_timeout_secs),
        })
    }
}

impl Default for TcpSettings {
    fn default() -> Self {
        Self {
            connect_timeout: Duration::from_secs(DEFAULT_CONNECT_TIMEOUT_SECS),
            request_idle_timeout: Duration::from_secs(DEFAULT_REQUEST_IDLE_TIMEOUT_SECS),
        }
    }
}

static NEXT_ID: AtomicU64 = AtomicU64::new(1);

thread_local! {
    static CONNECTIONS: RefCell<HashMap<String, BufReader<TcpStream>>> =
        RefCell::new(HashMap::new());
}

/// Phase 4.7+ fix #13 — cross-backend port validation.
/// Returns the port as `u16` when it fits, otherwise an Aver-side
/// error string. Lifts what used to be a VM `RuntimeError` panic
/// into a `Result.Err` so every backend (VM and generated Rust via the
/// standard Tcp provider, self-host and wasm-gc bridge via this module,
/// wasip2 via its own up-front check) surfaces the same catchable shape.
fn validate_port(port: i64) -> Result<u16, String> {
    if (0..=65535).contains(&port) {
        Ok(port as u16)
    } else {
        Err(format!("Tcp: port {port} is out of range (0\u{2013}65535)"))
    }
}

pub fn connect(host: &str, port: i64) -> Result<TcpConnection, String> {
    connect_with_settings(host, port, TcpSettings::default())
}

pub fn connect_with_settings(
    host: &str,
    port: i64,
    settings: TcpSettings,
) -> Result<TcpConnection, String> {
    validate_port(port)?;
    let count = CONNECTIONS.with(|map| map.borrow().len());
    if count >= MAX_CONNECTIONS {
        return Err(format!(
            "Tcp.connect: connection limit reached ({} max)",
            MAX_CONNECTIONS
        ));
    }

    let socket_addr = resolve(&format!("{}:{}", host, port))?;
    let stream = TcpStream::connect_timeout(&socket_addr, settings.connect_timeout)
        .map_err(|error| format_io_error("Tcp.connect", &error))?;

    // A persistent session has no read/write deadline. Any later I/O error
    // poisons the handle instead of letting a caller continue after an
    // unknown partial read or write.

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
    let msg = format!("{}\r\n", line);
    with_connection_io(conn, "Tcp.writeLine", |reader| {
        reader.get_mut().write_all(msg.as_bytes())
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
    with_connection_io(conn, "Tcp.writeBytes", |reader| {
        reader.get_mut().write_all(payload)
    })
}

pub fn read_line(conn: &TcpConnection) -> Result<String, String> {
    with_connection_io(conn, "Tcp.readLine", |reader| {
        let mut line = String::new();
        reader.read_line(&mut line)?;
        if line.ends_with('\n') {
            line.pop();
            if line.ends_with('\r') {
                line.pop();
            }
        }
        Ok(line)
    })
}

/// Byte-clean, exact-length sibling of [`read_line`].
///
/// `read_line` frames on `\n` and goes through `BufRead::read_line`, which
/// rejects non-UTF-8 outright. Neither works for a length-prefixed binary
/// protocol, where framing is "read a fixed-size header, decode a length, read
/// exactly that many bytes" and the payload may carry `0x0A` at any offset.
/// This reads exactly `n` bytes and decodes nothing.
///
/// A short read is an error rather than a truncated success: fewer bytes than
/// the length prefix promised means the peer went away mid-message, and
/// silently returning a partial frame would desynchronise the caller's parser.
///
/// `n` is capped at `BODY_LIMIT`. Length prefixes arrive from an untrusted
/// peer — Bitcoin's is four bytes, so it can ask for 4 GiB — and the cap stops
/// a hostile or corrupt prefix from becoming an allocation that size.
pub fn read_bytes(conn: &TcpConnection, n: i64) -> Result<Vec<u8>, String> {
    if n < 0 {
        return Err(format!("Tcp.readBytes: count {n} is negative"));
    }
    let want = usize::try_from(n).unwrap_or(usize::MAX);
    if want > BODY_LIMIT {
        return Err(format!(
            "Tcp.readBytes: count {n} exceeds the {BODY_LIMIT} byte limit"
        ));
    }
    with_connection_io(conn, "Tcp.readBytes", |reader| {
        let mut buf = vec![0u8; want];
        reader.read_exact(&mut buf)?;
        Ok(buf)
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
    send_with_settings(host, port, message, TcpSettings::default())
}

pub fn send_with_settings(
    host: &str,
    port: i64,
    message: &str,
    settings: TcpSettings,
) -> Result<String, String> {
    validate_port(port)?;
    let socket_addr = resolve(&format!("{}:{}", host, port))?;
    let mut stream = request_stream(&socket_addr, settings, "Tcp.send")?;
    stream
        .write_all(message.as_bytes())
        .map_err(|error| format_io_error("Tcp.send", &error))?;
    stream.shutdown(std::net::Shutdown::Write).ok();

    let mut buf = Vec::new();
    Read::by_ref(&mut stream)
        .take(BODY_LIMIT as u64 + 1)
        .read_to_end(&mut buf)
        .map_err(|error| format_io_error("Tcp.send", &error))?;
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
    send_bytes_with_settings(host, port, payload, TcpSettings::default())
}

pub fn send_bytes_with_settings(
    host: &str,
    port: i64,
    payload: &[u8],
    settings: TcpSettings,
) -> Result<Vec<u8>, String> {
    validate_port(port)?;
    let socket_addr = resolve(&format!("{}:{}", host, port))?;
    let mut stream = request_stream(&socket_addr, settings, "Tcp.sendBytes")?;
    stream
        .write_all(payload)
        .map_err(|error| format_io_error("Tcp.sendBytes", &error))?;
    stream.shutdown(std::net::Shutdown::Write).ok();

    let mut buf = Vec::new();
    Read::by_ref(&mut stream)
        .take(BODY_LIMIT as u64 + 1)
        .read_to_end(&mut buf)
        .map_err(|error| format_io_error("Tcp.sendBytes", &error))?;
    if buf.len() > BODY_LIMIT {
        return Err("Tcp.sendBytes: response exceeds 10 MB limit".to_string());
    }
    Ok(buf)
}

pub fn ping(host: &str, port: i64) -> Result<(), String> {
    ping_with_settings(host, port, TcpSettings::default())
}

pub fn ping_with_settings(host: &str, port: i64, settings: TcpSettings) -> Result<(), String> {
    validate_port(port)?;
    let socket_addr = resolve(&format!("{}:{}", host, port))?;
    TcpStream::connect_timeout(&socket_addr, settings.connect_timeout)
        .map_err(|error| format_io_error("Tcp.ping", &error))?;
    Ok(())
}

fn request_stream(
    socket_addr: &std::net::SocketAddr,
    settings: TcpSettings,
    operation: &str,
) -> Result<TcpStream, String> {
    let stream = TcpStream::connect_timeout(socket_addr, settings.connect_timeout)
        .map_err(|error| format_io_error(operation, &error))?;
    stream
        .set_read_timeout(Some(settings.request_idle_timeout))
        .map_err(|error| format_io_error(operation, &error))?;
    stream
        .set_write_timeout(Some(settings.request_idle_timeout))
        .map_err(|error| format_io_error(operation, &error))?;
    Ok(stream)
}

fn with_connection_io<T>(
    conn: &TcpConnection,
    operation: &str,
    io: impl FnOnce(&mut BufReader<TcpStream>) -> io::Result<T>,
) -> Result<T, String> {
    CONNECTIONS.with(|map| {
        let mut connections = map.borrow_mut();
        let id: &str = &conn.id;
        let Some(reader) = connections.get_mut(id) else {
            return Err(format!("{operation}: unknown connection '{}'", conn.id));
        };
        match io(reader) {
            Ok(value) => Ok(value),
            Err(error) => {
                // A failed read may already have consumed bytes and a failed write
                // may already have sent bytes. The stream position is unknowable;
                // keeping the handle would permit silent protocol corruption.
                connections.remove(id);
                Err(format_io_error(operation, &error))
            }
        }
    })
}

fn format_io_error(operation: &str, error: &io::Error) -> String {
    if matches!(
        error.kind(),
        io::ErrorKind::TimedOut | io::ErrorKind::WouldBlock
    ) {
        format!("{operation}: I/O timed out")
    } else {
        format!("{operation}: {error}")
    }
}

fn resolve(addr: &str) -> Result<std::net::SocketAddr, String> {
    addr.to_socket_addrs()
        .map_err(|e| format!("Tcp: DNS resolution failed for {}: {}", addr, e))?
        .next()
        .ok_or_else(|| format!("Tcp: no address found for {}", addr))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::{Shutdown, TcpListener};
    use std::thread;

    fn loopback_connection(
        server: impl FnOnce(TcpStream) + Send + 'static,
    ) -> (TcpConnection, thread::JoinHandle<()>) {
        let listener = TcpListener::bind(("127.0.0.1", 0)).expect("bind loopback listener");
        let port = listener.local_addr().expect("listener address").port();
        let server = thread::spawn(move || {
            let (stream, _) = listener.accept().expect("accept loopback connection");
            server(stream);
        });
        let connection = connect("127.0.0.1", i64::from(port)).expect("connect loopback client");
        (connection, server)
    }

    #[test]
    fn persistent_connections_have_no_read_or_write_deadline() {
        let (connection, server) = loopback_connection(|_| {});
        CONNECTIONS.with(|map| {
            let connections = map.borrow();
            let reader = connections
                .get(connection.id.as_ref())
                .expect("live handle");
            assert_eq!(reader.get_ref().read_timeout().unwrap(), None);
            assert_eq!(reader.get_ref().write_timeout().unwrap(), None);
        });
        close(&connection).expect("close client");
        server.join().expect("server thread");
    }

    #[test]
    fn failed_exact_read_poisons_the_connection() {
        let (connection, server) = loopback_connection(|mut stream| {
            stream.write_all(&[1, 2, 3]).expect("write partial frame");
            stream
                .shutdown(Shutdown::Write)
                .expect("finish partial frame");
        });

        let first = read_bytes(&connection, 10).expect_err("short frame must fail");
        assert!(first.starts_with("Tcp.readBytes:"), "{first}");
        let second = read_bytes(&connection, 1).expect_err("failed handle must be gone");
        assert!(second.contains("unknown connection"), "{second}");
        server.join().expect("server thread");
    }

    #[test]
    fn argument_validation_does_not_poison_the_connection() {
        let (connection, server) = loopback_connection(|_| {});
        assert!(read_bytes(&connection, -1).is_err());
        CONNECTIONS.with(|map| {
            assert!(map.borrow().contains_key(connection.id.as_ref()));
        });
        close(&connection).expect("close client");
        server.join().expect("server thread");
    }

    #[test]
    fn timeout_error_text_is_platform_independent() {
        for kind in [io::ErrorKind::TimedOut, io::ErrorKind::WouldBlock] {
            let error = io::Error::from(kind);
            assert_eq!(
                format_io_error("Tcp.readBytes", &error),
                "Tcp.readBytes: I/O timed out"
            );
        }
    }
}
