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

/// Read the bytes currently available from a persistent connection, up to
/// `max_bytes`, without waiting to fill the requested maximum.
///
/// The first byte may still wait indefinitely when the caller did not precede
/// this operation with [`poll`]. An empty success means clean EOF. As with the
/// other session operations, a real I/O error poisons the connection while
/// argument validation leaves it live.
pub fn read_some(conn: &TcpConnection, max_bytes: i64) -> Result<Vec<u8>, String> {
    if max_bytes <= 0 {
        return Err(format!(
            "Tcp.readSome: maxBytes {max_bytes} must be positive"
        ));
    }
    let limit = usize::try_from(max_bytes).unwrap_or(usize::MAX);
    if limit > BODY_LIMIT {
        return Err(format!(
            "Tcp.readSome: maxBytes {max_bytes} exceeds the {BODY_LIMIT} byte limit"
        ));
    }
    with_connection_io(conn, "Tcp.readSome", |reader| {
        let mut buf = vec![0u8; limit];
        let read = reader.read(&mut buf)?;
        buf.truncate(read);
        Ok(buf)
    })
}

/// Return the positions of persistent connections that can make read
/// progress without blocking. Positions preserve duplicate handles in the
/// input; the capability adapter maps them back to caller-owned peer IDs.
///
/// Bytes already held by a connection's [`BufReader`] count as readable even
/// when the underlying socket itself is idle. Closed/broken sockets also count
/// as readable so the following read can surface EOF or the concrete I/O
/// failure. Polling itself never poisons a session.
#[cfg(not(target_family = "wasm"))]
pub fn poll(connections: &[TcpConnection], timeout_ms: i64) -> Result<Vec<usize>, String> {
    if timeout_ms < 0 {
        return Err(format!("Tcp.poll: timeoutMs {timeout_ms} is negative"));
    }

    CONNECTIONS.with(|map| {
        let live = map.borrow();
        let mut ready = vec![false; connections.len()];
        let mut group_by_id: HashMap<String, usize> = HashMap::new();
        let mut groups: Vec<(&TcpStream, Vec<usize>)> = Vec::new();

        for (position, connection) in connections.iter().enumerate() {
            let id: &str = &connection.id;
            let Some(reader) = live.get(id) else {
                return Err(format!("Tcp.poll: unknown connection '{}'", connection.id));
            };
            if !reader.buffer().is_empty() {
                ready[position] = true;
                continue;
            }
            if let Some(group) = group_by_id.get(id).copied() {
                groups[group].1.push(position);
            } else {
                let group = groups.len();
                group_by_id.insert(id.to_string(), group);
                groups.push((reader.get_ref(), vec![position]));
            }
        }

        let poller = polling::Poller::new().map_err(|error| format_io_error("Tcp.poll", &error))?;
        for (group, (stream, _)) in groups.iter().enumerate() {
            // SAFETY: `live` immutably borrows the connection table until
            // after `poller` is dropped, so every registered TcpStream remains
            // alive and at the same address for the whole registration.
            unsafe {
                poller
                    .add(*stream, polling::Event::readable(group))
                    .map_err(|error| format_io_error("Tcp.poll", &error))?;
            }
        }

        let timeout = if ready.iter().any(|is_ready| *is_ready) {
            Duration::ZERO
        } else {
            Duration::from_millis(timeout_ms as u64)
        };
        let mut events = polling::Events::new();
        poller
            .wait(&mut events, Some(timeout))
            .map_err(|error| format_io_error("Tcp.poll", &error))?;
        for event in events.iter() {
            if (event.readable || event.is_err().unwrap_or(false))
                && let Some((_, positions)) = groups.get(event.key)
            {
                for position in positions {
                    ready[*position] = true;
                }
            }
        }

        Ok(ready
            .into_iter()
            .enumerate()
            .filter_map(|(position, is_ready)| is_ready.then_some(position))
            .collect())
    })
}

#[cfg(target_family = "wasm")]
pub fn poll(_connections: &[TcpConnection], _timeout_ms: i64) -> Result<Vec<usize>, String> {
    Err("Tcp.poll: native socket polling is unavailable on this target".to_string())
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
    use std::sync::mpsc;
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
        assert!(read_some(&connection, 0).is_err());
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

    #[test]
    fn read_some_returns_available_bytes_without_filling_the_maximum() {
        let (written_tx, written_rx) = mpsc::channel();
        let (release_tx, release_rx) = mpsc::channel();
        let (connection, server) = loopback_connection(move |mut stream| {
            stream.write_all(&[1, 2, 3]).expect("write available bytes");
            written_tx.send(()).expect("announce write");
            release_rx.recv().expect("hold peer open");
        });
        written_rx.recv().expect("peer wrote bytes");

        assert_eq!(read_some(&connection, 64).expect("read some"), [1, 2, 3]);

        close(&connection).expect("close client");
        release_tx.send(()).expect("release peer");
        server.join().expect("server thread");
    }

    #[test]
    fn poll_sees_bytes_already_buffered_by_read_line() {
        let (written_tx, written_rx) = mpsc::channel();
        let (release_tx, release_rx) = mpsc::channel();
        let (connection, server) = loopback_connection(move |mut stream| {
            stream
                .write_all(b"hello\r\nrest")
                .expect("write line and trailing bytes");
            written_tx.send(()).expect("announce write");
            release_rx.recv().expect("hold peer open");
        });
        written_rx.recv().expect("peer wrote bytes");

        assert_eq!(read_line(&connection).expect("read line"), "hello");
        assert_eq!(poll(std::slice::from_ref(&connection), 1_000).unwrap(), [0]);
        assert_eq!(
            read_some(&connection, 64).expect("read buffered rest"),
            b"rest"
        );

        close(&connection).expect("close client");
        release_tx.send(()).expect("release peer");
        server.join().expect("server thread");
    }

    #[test]
    fn poll_times_out_cleanly_and_rejects_unknown_handles() {
        let (release_tx, release_rx) = mpsc::channel();
        let (connection, server) = loopback_connection(move |_| {
            release_rx.recv().expect("hold quiet peer open");
        });

        assert!(
            poll(std::slice::from_ref(&connection), 5)
                .expect("quiet poll")
                .is_empty()
        );
        let unknown = TcpConnection::from_parts("tcp-missing".to_string(), String::new(), 0);
        assert!(
            poll(&[unknown], 0)
                .expect_err("unknown handle")
                .contains("unknown connection")
        );

        close(&connection).expect("close client");
        release_tx.send(()).expect("release peer");
        server.join().expect("server thread");
    }
}
