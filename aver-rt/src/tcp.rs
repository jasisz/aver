use crate::{TcpConnection, TcpDial, TcpListener};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::time::Duration;

#[cfg(not(target_family = "wasm"))]
mod reactor;

pub const DEFAULT_CONNECT_TIMEOUT_SECS: u64 = 5;
pub const DEFAULT_REQUEST_IDLE_TIMEOUT_SECS: u64 = 30;
pub const DEFAULT_MAX_CONNECTIONS: usize = 256;
const BODY_LIMIT: usize = 10 * 1024 * 1024;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TcpSocket {
    Listening(TcpListener),
    Dialing(TcpDial),
    Connected(TcpConnection),
}

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
    pub max_connections: usize,
}

impl TcpSettings {
    pub fn from_secs(
        connect_timeout_secs: u64,
        request_idle_timeout_secs: u64,
    ) -> Result<Self, String> {
        Self::from_policy(
            connect_timeout_secs,
            request_idle_timeout_secs,
            DEFAULT_MAX_CONNECTIONS,
        )
    }

    pub fn from_policy(
        connect_timeout_secs: u64,
        request_idle_timeout_secs: u64,
        max_connections: usize,
    ) -> Result<Self, String> {
        if connect_timeout_secs == 0 {
            return Err("Tcp connect timeout must be greater than zero".to_string());
        }
        if request_idle_timeout_secs == 0 {
            return Err("Tcp request idle timeout must be greater than zero".to_string());
        }
        if max_connections == 0 {
            return Err("Tcp connection limit must be greater than zero".to_string());
        }
        Ok(Self {
            connect_timeout: Duration::from_secs(connect_timeout_secs),
            request_idle_timeout: Duration::from_secs(request_idle_timeout_secs),
            max_connections,
        })
    }
}

impl Default for TcpSettings {
    fn default() -> Self {
        Self {
            connect_timeout: Duration::from_secs(DEFAULT_CONNECT_TIMEOUT_SECS),
            request_idle_timeout: Duration::from_secs(DEFAULT_REQUEST_IDLE_TIMEOUT_SECS),
            max_connections: DEFAULT_MAX_CONNECTIONS,
        }
    }
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

#[cfg(not(target_family = "wasm"))]
pub fn connect_with_settings(
    host: &str,
    port: i64,
    settings: TcpSettings,
) -> Result<TcpConnection, String> {
    validate_port(port)?;
    let count = reactor::live_connection_count();
    if count >= settings.max_connections {
        return Err(format!(
            "Tcp.connect: connection limit reached ({} max)",
            settings.max_connections
        ));
    }

    let socket_addr = resolve(&format!("{}:{}", host, port))?;
    let stream = TcpStream::connect_timeout(&socket_addr, settings.connect_timeout)
        .map_err(|error| format_connect_error("Tcp.connect", &error, settings.connect_timeout))?;

    // A persistent session has no read/write deadline. Any later I/O error
    // poisons the handle instead of letting a caller continue after an
    // unknown partial read or write.

    Ok(reactor::register_connection(stream, host.to_string(), port))
}

#[cfg(target_family = "wasm")]
pub fn connect_with_settings(
    _host: &str,
    _port: i64,
    _settings: TcpSettings,
) -> Result<TcpConnection, String> {
    Err("Tcp.connect: native sockets are unavailable on this target".to_string())
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

#[cfg(not(target_family = "wasm"))]
pub fn poll(sockets: &[TcpSocket], timeout_ms: i64) -> Result<Vec<usize>, String> {
    reactor::poll(sockets, timeout_ms)
}

#[cfg(target_family = "wasm")]
pub fn poll(_sockets: &[TcpSocket], _timeout_ms: i64) -> Result<Vec<usize>, String> {
    Err("Tcp.poll: native socket polling is unavailable on this target".to_string())
}

#[cfg(not(target_family = "wasm"))]
pub fn listen(port: i64, backlog: i64) -> Result<TcpListener, String> {
    listen_with_settings(port, backlog, TcpSettings::default())
}

#[cfg(not(target_family = "wasm"))]
pub fn listen_with_settings(
    port: i64,
    backlog: i64,
    settings: TcpSettings,
) -> Result<TcpListener, String> {
    reactor::listen(validate_port(port)?, backlog, settings)
}

#[cfg(target_family = "wasm")]
pub fn listen(_port: i64, _backlog: i64) -> Result<TcpListener, String> {
    Err("Tcp.listen: native socket listening is unavailable on this target".to_string())
}

#[cfg(target_family = "wasm")]
pub fn listen_with_settings(
    _port: i64,
    _backlog: i64,
    _settings: TcpSettings,
) -> Result<TcpListener, String> {
    Err("Tcp.listen: native socket listening is unavailable on this target".to_string())
}

#[cfg(not(target_family = "wasm"))]
pub fn accept(listener: &TcpListener) -> Result<Option<TcpConnection>, String> {
    reactor::accept(listener)
}

#[cfg(target_family = "wasm")]
pub fn accept(_listener: &TcpListener) -> Result<Option<TcpConnection>, String> {
    Err("Tcp.accept: native socket listening is unavailable on this target".to_string())
}

#[cfg(not(target_family = "wasm"))]
pub fn begin_connect_with_settings(
    host: &str,
    port: i64,
    settings: TcpSettings,
) -> Result<TcpDial, String> {
    validate_port(port)?;
    let address = resolve(&format!("{}:{}", host, port))?;
    reactor::begin_connect(host, port, address, settings)
}

pub fn begin_connect(host: &str, port: i64) -> Result<TcpDial, String> {
    begin_connect_with_settings(host, port, TcpSettings::default())
}

#[cfg(target_family = "wasm")]
pub fn begin_connect_with_settings(
    _host: &str,
    _port: i64,
    _settings: TcpSettings,
) -> Result<TcpDial, String> {
    Err("Tcp.beginConnect: native sockets are unavailable on this target".to_string())
}

#[cfg(not(target_family = "wasm"))]
pub fn dialled(dial: &TcpDial) -> Result<Option<TcpConnection>, String> {
    reactor::dialled(dial)
}

#[cfg(target_family = "wasm")]
pub fn dialled(_dial: &TcpDial) -> Result<Option<TcpConnection>, String> {
    Err("Tcp.dialled: native sockets are unavailable on this target".to_string())
}

#[cfg(not(target_family = "wasm"))]
pub fn close_listener(listener: &TcpListener) -> Result<(), String> {
    reactor::close_listener(listener)
}

#[cfg(target_family = "wasm")]
pub fn close_listener(_listener: &TcpListener) -> Result<(), String> {
    Err("Tcp.closeListener: native sockets are unavailable on this target".to_string())
}

#[cfg(not(target_family = "wasm"))]
pub fn close_dial(dial: &TcpDial) -> Result<(), String> {
    reactor::close_dial(dial)
}

#[cfg(target_family = "wasm")]
pub fn close_dial(_dial: &TcpDial) -> Result<(), String> {
    Err("Tcp.closeDial: native sockets are unavailable on this target".to_string())
}

#[cfg(not(target_family = "wasm"))]
pub fn peer_address(connection: &TcpConnection) -> Result<String, String> {
    reactor::peer_address(connection)
}

#[cfg(target_family = "wasm")]
pub fn peer_address(_connection: &TcpConnection) -> Result<String, String> {
    Err("Tcp.peerAddress: native sockets are unavailable on this target".to_string())
}

#[cfg(not(target_family = "wasm"))]
pub fn close(conn: &TcpConnection) -> Result<(), String> {
    reactor::close_connection(conn)
}

#[cfg(target_family = "wasm")]
pub fn close(_conn: &TcpConnection) -> Result<(), String> {
    Err("Tcp.close: native sockets are unavailable on this target".to_string())
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
        .map_err(|error| format_connect_error("Tcp.ping", &error, settings.connect_timeout))?;
    Ok(())
}

fn request_stream(
    socket_addr: &std::net::SocketAddr,
    settings: TcpSettings,
    operation: &str,
) -> Result<TcpStream, String> {
    let stream = TcpStream::connect_timeout(socket_addr, settings.connect_timeout)
        .map_err(|error| format_connect_error(operation, &error, settings.connect_timeout))?;
    stream
        .set_read_timeout(Some(settings.request_idle_timeout))
        .map_err(|error| format_io_error(operation, &error))?;
    stream
        .set_write_timeout(Some(settings.request_idle_timeout))
        .map_err(|error| format_io_error(operation, &error))?;
    Ok(stream)
}

#[cfg(not(target_family = "wasm"))]
fn with_connection_io<T>(
    conn: &TcpConnection,
    operation: &str,
    io: impl FnOnce(&mut BufReader<TcpStream>) -> io::Result<T>,
) -> Result<T, String> {
    reactor::with_connection_io(conn, operation, io)
}

#[cfg(target_family = "wasm")]
fn with_connection_io<T>(
    _conn: &TcpConnection,
    operation: &str,
    _io: impl FnOnce(&mut BufReader<TcpStream>) -> io::Result<T>,
) -> Result<T, String> {
    Err(format!(
        "{operation}: native sockets are unavailable on this target"
    ))
}

pub(super) fn format_io_error(operation: &str, error: &io::Error) -> String {
    if matches!(
        error.kind(),
        io::ErrorKind::TimedOut | io::ErrorKind::WouldBlock
    ) {
        format!("{operation}: I/O timed out")
    } else {
        format!("{operation}: {error}")
    }
}

pub(super) fn format_connect_error(
    operation: &str,
    error: &io::Error,
    deadline: Duration,
) -> String {
    if matches!(
        error.kind(),
        io::ErrorKind::TimedOut | io::ErrorKind::WouldBlock
    ) {
        format!(
            "{operation}: socket establishment timed out (deadline: {} ms)",
            deadline.as_millis()
        )
    } else {
        format_io_error(operation, error)
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
    use std::time::Instant;

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
        assert_eq!(
            reactor::connection_timeouts(&connection),
            Some((None, None))
        );
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
        assert!(reactor::connection_exists(&connection));
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
    fn connect_timeout_error_names_the_selected_deadline() {
        for kind in [io::ErrorKind::TimedOut, io::ErrorKind::WouldBlock] {
            let error = io::Error::from(kind);
            assert_eq!(
                format_connect_error("Tcp.connect", &error, Duration::from_secs(7)),
                "Tcp.connect: socket establishment timed out (deadline: 7000 ms)"
            );
        }
    }

    #[test]
    fn dialled_promotes_connected_socket_even_after_the_deadline() {
        let listener = TcpListener::bind(("127.0.0.1", 0)).expect("bind connected dial target");
        let address = listener
            .local_addr()
            .expect("connected dial target address");
        let (release_tx, release_rx) = mpsc::channel();
        let server = thread::spawn(move || {
            let (_stream, _) = listener.accept().expect("accept connected dial");
            release_rx.recv().expect("hold connected dial peer open");
        });
        let stream = TcpStream::connect(address).expect("connect test dial");
        stream
            .set_nonblocking(true)
            .expect("make test dial nonblocking");
        let expired_deadline = Instant::now()
            .checked_sub(Duration::from_secs(1))
            .unwrap_or_else(Instant::now);
        let dial = reactor::insert_test_dial_with_deadline(
            stream,
            expired_deadline,
            Duration::from_secs(5),
        );

        let connection = dialled(&dial)
            .expect("inspect expired connected dial")
            .expect("expired connected dial is still connected");
        assert!(reactor::connection_exists(&connection));

        close(&connection).expect("close promoted dial");
        release_tx.send(()).expect("release connected dial peer");
        server.join().expect("connected dial server");
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
        assert_eq!(
            poll(&[TcpSocket::Connected(connection.clone())], 1_000).unwrap(),
            [0]
        );
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
            poll(&[TcpSocket::Connected(connection.clone())], 5)
                .expect("quiet poll")
                .is_empty()
        );
        let unknown = TcpConnection::from_parts("tcp-missing".to_string(), String::new(), 0);
        assert!(
            poll(&[TcpSocket::Connected(unknown)], 0)
                .expect_err("unknown handle")
                .contains("unknown connection")
        );

        close(&connection).expect("close client");
        release_tx.send(()).expect("release peer");
        server.join().expect("server thread");
    }

    #[test]
    fn max_connections_is_shared_and_a_full_pool_does_not_drain_the_backlog() {
        let settings = TcpSettings::from_policy(1, 1, 1).expect("one connection policy");
        let listener = listen_with_settings(0, 16, settings).expect("open runtime listener");
        let listener_address = reactor::listener_local_address(&listener);
        let waiting_client = TcpStream::connect(listener_address).expect("queue inbound client");

        let (release_tx, release_rx) = mpsc::channel();
        let (occupied, occupied_server) = loopback_connection(move |_| {
            release_rx.recv().expect("hold occupied connection open");
        });

        let error = accept(&listener).expect_err("full shared pool must reject accept");
        assert!(
            error.contains("connection limit reached (1 max)"),
            "{error}"
        );

        close(&occupied).expect("free shared pool slot");
        let accepted = accept(&listener)
            .expect("accept after freeing slot")
            .expect("queued client must remain in backlog");
        close(&accepted).expect("close accepted client");
        close_listener(&listener).expect("close listener");
        drop(waiting_client);
        release_tx.send(()).expect("release occupied peer");
        occupied_server.join().expect("occupied server");
    }

    #[test]
    fn one_poller_handles_connections_dials_and_listeners_and_clips_to_dial_deadline() {
        let (written_tx, written_rx) = mpsc::channel();
        let (release_connection_tx, release_connection_rx) = mpsc::channel();
        let (connection, connection_server) = loopback_connection(move |mut stream| {
            stream
                .write_all(b"line\r\nbuffered")
                .expect("write buffered connection bytes");
            written_tx.send(()).expect("announce connection bytes");
            release_connection_rx
                .recv()
                .expect("hold connection peer open");
        });
        written_rx.recv().expect("connection peer wrote");
        assert_eq!(read_line(&connection).expect("prime BufReader"), "line");

        let listener = listen(0, 16).expect("open runtime listener");
        let listener_address = reactor::listener_local_address(&listener);
        let waiting_client = TcpStream::connect(listener_address).expect("queue inbound client");

        let dial_target = TcpListener::bind(("127.0.0.1", 0)).expect("bind dial target");
        let dial_port = dial_target
            .local_addr()
            .expect("dial target address")
            .port();
        let (release_dial_tx, release_dial_rx) = mpsc::channel();
        let dial_server = thread::spawn(move || {
            let (_stream, _) = dial_target.accept().expect("accept outbound dial");
            release_dial_rx.recv().expect("hold dial target open");
        });
        let dial = begin_connect("127.0.0.1", i64::from(dial_port)).expect("begin dial");
        assert_eq!(
            poll(&[TcpSocket::Dialing(dial.clone())], 1_000).expect("wait for outbound dial"),
            [0]
        );

        let ready = poll(
            &[
                TcpSocket::Connected(connection.clone()),
                TcpSocket::Dialing(dial.clone()),
                TcpSocket::Listening(listener.clone()),
            ],
            1_000,
        )
        .expect("poll three socket states");
        assert_eq!(ready, [0, 1, 2]);

        let promoted = dialled(&dial)
            .expect("inspect ready dial")
            .expect("ready dial connected");
        close(&promoted).expect("close promoted dial");
        let accepted = accept(&listener)
            .expect("accept queued client")
            .expect("queued client present");
        close(&accepted).expect("close accepted client");
        close_listener(&listener).expect("close listener");
        drop(waiting_client);
        close(&connection).expect("close buffered connection");
        release_connection_tx
            .send(())
            .expect("release connection peer");
        release_dial_tx.send(()).expect("release dial target");
        connection_server.join().expect("connection server");
        dial_server.join().expect("dial server");

        let blocked_listener = TcpListener::bind(("127.0.0.1", 0)).expect("bind quiet peer");
        let blocked_address = blocked_listener.local_addr().expect("quiet peer address");
        let (release_blocked_tx, release_blocked_rx) = mpsc::channel();
        let blocked_server = thread::spawn(move || {
            let (_stream, _) = blocked_listener.accept().expect("accept quiet stream");
            release_blocked_rx.recv().expect("hold quiet stream open");
        });
        let blocked_stream = TcpStream::connect(blocked_address).expect("connect quiet stream");
        blocked_stream
            .set_nonblocking(true)
            .expect("make quiet stream nonblocking");

        let deadline = Duration::from_millis(40);
        let blocked_dial = reactor::insert_test_dial(blocked_stream, deadline);
        let started = Instant::now();
        let ready = poll(&[TcpSocket::Dialing(blocked_dial.clone())], 1_000)
            .expect("deadline-clipped poll");
        let elapsed = started.elapsed();
        assert_eq!(ready, [0]);
        assert!(
            elapsed >= Duration::from_millis(20),
            "returned too early: {elapsed:?}"
        );
        assert!(
            elapsed < Duration::from_millis(300),
            "ignored dial deadline: {elapsed:?}"
        );
        let promoted_after_deadline = dialled(&blocked_dial)
            .expect("inspect deadline-ready connected dial")
            .expect("connected dial survives an expired deadline");
        close(&promoted_after_deadline).expect("close deadline-ready dial");
        release_blocked_tx.send(()).expect("release blocked peer");
        blocked_server.join().expect("blocked server");
    }
}
