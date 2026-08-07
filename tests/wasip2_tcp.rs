//! Wasip2 `Tcp.*` end-to-end tests (Phase 4 / 0.20 "Pulse").
//!
//! Eight scenarios share a tiny Python TCP server bound on an
//! OS-assigned port (avoids fixed-port flakes in parallel runs):
//!
//! - `tcp_connect_close_round_trip` — connect + close, verify
//!   the returned `Tcp.Connection.id` shape (`"tcp-N"`) and the
//!   host / port fields.
//! - `tcp_write_line_round_trip` — write a single line, the
//!   Python server reads + closes, Aver gets `Result.Ok(())`.
//! - `tcp_write_bytes_exact_binary_frame` — persistent raw-byte write,
//!   including non-UTF-8 and an embedded newline, with no framing added.
//! - `tcp_read_line_echo_round_trip` — write+read against an
//!   echo server, assert the line round-trips byte-for-byte.
//! - `tcp_read_bytes_exact_binary_frame` — exact non-UTF-8 read split
//!   across two host chunks.
//! - `tcp_read_bytes_big_count_is_a_result_error` — hostile frame length
//!   remains a catchable error.
//! - `tcp_send_one_shot` — `Tcp.send` orchestrator: connect +
//!   write + read + close in one call, return the response line.
//! - `tcp_send_bytes_one_shot` — the byte-clean sibling round-trips
//!   a non-UTF-8 payload without decoding it as text.
//! - `tcp_ping_live_and_closed` — `Tcp.ping` returns Ok against
//!   the listener and Err against a closed port.
//!
//! Skipped automatically when `python3` is not on PATH.

#![cfg(feature = "wasip2")]

use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Server that accepts a connection and immediately closes it.
/// Used by the connect/close + ping-live tests where the guest
/// never expects any bytes back.
const ACCEPT_AND_CLOSE_SCRIPT: &str = r#"
import socket, sys, threading

s = socket.socket()
s.bind(("127.0.0.1", 0))
s.listen(16)
sys.stdout.write(f"PORT:{s.getsockname()[1]}\n")
sys.stdout.flush()

def serve():
    while True:
        try:
            c, _ = s.accept()
            c.close()
        except OSError:
            break

threading.Thread(target=serve, daemon=True).start()
import time
time.sleep(60)
"#;

/// Server that reads one line, echoes it back, closes.
const ECHO_SCRIPT: &str = r#"
import socket, sys, threading

s = socket.socket()
s.bind(("127.0.0.1", 0))
s.listen(16)
sys.stdout.write(f"PORT:{s.getsockname()[1]}\n")
sys.stdout.flush()

def serve():
    while True:
        try:
            c, _ = s.accept()
            data = b""
            while b"\n" not in data:
                chunk = c.recv(4096)
                if not chunk:
                    break
                data += chunk
            c.sendall(data)
            c.close()
        except OSError:
            break

threading.Thread(target=serve, daemon=True).start()
import time
time.sleep(60)
"#;

/// Server that reads one line, replies with a fixed `pong\n`,
/// closes. Used by `Tcp.send` so the assertion can match a known
/// byte sequence rather than echoing back the request.
const PONG_SCRIPT: &str = r#"
import socket, sys, threading

s = socket.socket()
s.bind(("127.0.0.1", 0))
s.listen(16)
sys.stdout.write(f"PORT:{s.getsockname()[1]}\n")
sys.stdout.flush()

def serve():
    while True:
        try:
            c, _ = s.accept()
            data = b""
            while b"\n" not in data:
                chunk = c.recv(4096)
                if not chunk:
                    break
                data += chunk
            c.sendall(b"pong\n")
            c.close()
        except OSError:
            break

threading.Thread(target=serve, daemon=True).start()
import time
time.sleep(60)
"#;

/// Sends one non-UTF-8 frame in two chunks, exercising the exact-length
/// read loop rather than relying on a single host `blocking-read` result.
const FIXED_BINARY_SCRIPT: &str = r#"
import socket, sys, threading, time

s = socket.socket()
s.bind(("127.0.0.1", 0))
s.listen(16)
sys.stdout.write(f"PORT:{s.getsockname()[1]}\n")
sys.stdout.flush()

def serve():
    while True:
        try:
            c, _ = s.accept()
            c.sendall(bytes([249]))
            time.sleep(0.05)
            c.sendall(bytes([190, 180, 217]))
            c.close()
        except OSError:
            break

threading.Thread(target=serve, daemon=True).start()
time.sleep(60)
"#;

/// Reads one exact binary frame and acknowledges whether the wire bytes match.
const BINARY_SINK_SCRIPT: &str = r#"
import socket, sys, threading, time

s = socket.socket()
s.bind(("127.0.0.1", 0))
s.listen(16)
sys.stdout.write(f"PORT:{s.getsockname()[1]}\n")
sys.stdout.flush()

def serve():
    expected = bytes([249, 190, 180, 217, 10, 255])
    while True:
        try:
            c, _ = s.accept()
            data = b""
            while len(data) < len(expected):
                chunk = c.recv(len(expected) - len(data))
                if not chunk:
                    break
                data += chunk
            c.sendall(b"exact\n" if data == expected else b"wrong\n")
            c.close()
        except OSError:
            break

threading.Thread(target=serve, daemon=True).start()
time.sleep(60)
"#;

fn tempdir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("aver-wasip2-tcp-{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create tempdir");
    dir
}

fn write_fixture(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, source).expect("write fixture");
    path
}

fn spawn_python_server(dir: &Path, script: &str) -> Option<(Child, u16)> {
    // `None` ⇒ skip the test; reserved for `python3` literally not
    // being on PATH. Any other spawn failure or a server that exits
    // before printing `PORT:<n>` panics with stderr — see the matching
    // helper in `wasip2_tcp_stress.rs` for the rationale.
    let mut child = match Command::new("python3")
        .args(["-c", script])
        .current_dir(dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return None,
        Err(e) => panic!("python3 spawn failed (not NotFound): {e}"),
    };
    let stdout = child.stdout.take().expect("stdout pipe");
    let mut reader = BufReader::new(stdout);
    let mut line = String::new();
    let read = match reader.read_line(&mut line) {
        Ok(n) => n,
        Err(e) => {
            drop(reader);
            let _ = child.kill();
            panic!(
                "failed to read PORT: line from python: {e}\nstderr:\n{}",
                drain_child_stderr(&mut child)
            );
        }
    };
    if read == 0 {
        drop(reader);
        let _ = child.kill();
        panic!(
            "python server exited before printing PORT: line\nstderr:\n{}",
            drain_child_stderr(&mut child)
        );
    }
    let port: u16 = match line
        .trim()
        .strip_prefix("PORT:")
        .and_then(|s| s.parse().ok())
    {
        Some(p) => p,
        None => {
            drop(reader);
            let _ = child.kill();
            panic!(
                "expected `PORT:<num>` from python, got {line:?}\nstderr:\n{}",
                drain_child_stderr(&mut child)
            );
        }
    };
    drop(reader);
    Some((child, port))
}

fn drain_child_stderr(child: &mut Child) -> String {
    use std::io::Read;
    let mut buf = String::new();
    if let Some(mut stderr) = child.stderr.take() {
        let _ = stderr.read_to_string(&mut buf);
    }
    buf
}

fn run_wasip2(dir: &Path, fixture: &Path) -> std::process::Output {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(dir).arg("run").arg("--wasip2").arg(fixture);
    cmd.output().expect("aver run --wasip2 to launch")
}

#[test]
fn tcp_connect_close_round_trip() {
    let dir = tempdir("connect");
    let Some((mut server, port)) = spawn_python_server(&dir, ACCEPT_AND_CLOSE_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wasip2_tcp connect test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn doClose(c: Tcp.Connection) -> Unit
    ! [Tcp.close, Console.print]
    _ = Console.print("connect-ok")
    match Tcp.close(c)
        Result.Ok(_) -> Console.print(" closed-ok")
        Result.Err(e) -> Console.print(" closed-err: {{e}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doClose(c)
        Result.Err(e) -> Console.print("connect err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "connect.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wasip2_tcp connect failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    // `Tcp.Connection` is opaque (Phase 4.7+ fix #11), so the
    // test doesn't try to inspect the handle's bytes; it just
    // checks that connect + close both came back Ok.
    assert!(s.contains("connect-ok"), "expected connect-ok, got:\n{s}");
    assert!(s.contains(" closed-ok"), "expected closed-ok, got:\n{s}");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn tcp_write_line_round_trip() {
    let dir = tempdir("writeLine");
    let Some((mut server, port)) = spawn_python_server(&dir, ECHO_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wasip2_tcp writeLine test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn afterWrite(c: Tcp.Connection, note: String) -> Unit
    ! [Tcp.close, Console.print]
    _ = Console.print(note)
    closeRes = Tcp.close(c)
    Console.print(" closed")

fn doWrite(c: Tcp.Connection) -> Unit
    ! [Tcp.writeLine, Tcp.close, Console.print]
    match Tcp.writeLine(c, "hello tcp")
        Result.Ok(_) -> afterWrite(c, " wrote-ok")
        Result.Err(e) -> afterWrite(c, " write-err")

fn main() -> Unit
    ! [Tcp.connect, Tcp.writeLine, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doWrite(c)
        Result.Err(e) -> Console.print("connect err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "write.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wasip2_tcp writeLine failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(s.contains(" wrote-ok"), "expected wrote-ok, got:\n{s}");
    assert!(s.contains(" closed"), "expected closed, got:\n{s}");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn tcp_write_bytes_exact_binary_frame() {
    let dir = tempdir("writeBytes");
    let Some((mut server, port)) = spawn_python_server(&dir, BINARY_SINK_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wasip2_tcp writeBytes test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
module WriteBytes
    intent = "Write one exact nominal Bytes frame on WASI."
    depends [Bytes]
    effects [Tcp, Console]

fn awaitAck(c: Tcp.Connection) -> Unit
    ! [Tcp.readLine, Tcp.close, Console.print]
    match Tcp.readLine(c)
        Result.Ok(ack) -> Console.print(ack)
        Result.Err(e) -> Console.print("read err: {{e}}")

fn writeFrame(c: Tcp.Connection, payload: Bytes) -> Unit
    ! [Tcp.writeBytes, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.writeBytes(c, payload)
        Result.Ok(_) -> awaitAck(c)
        Result.Err(e) -> Console.print("write err: {{e}}")

fn usePayload(c: Tcp.Connection, payload: Result<Bytes, String>) -> Unit
    ! [Tcp.writeBytes, Tcp.readLine, Tcp.close, Console.print]
    match payload
        Result.Ok(bytes) -> writeFrame(c, bytes)
        Result.Err(e) -> Console.print("bytes err: {{e}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.writeBytes, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> usePayload(c, Bytes.fromList([249, 190, 180, 217, 10, 255]))
        Result.Err(e) -> Console.print("connect err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "write_bytes.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wasip2_tcp writeBytes failed (exit {:?})\nstdout:\n{stdout}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        stdout.contains("exact"),
        "expected exact binary payload from Tcp.writeBytes, got:\n{stdout}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn tcp_read_line_echo_round_trip() {
    let dir = tempdir("readLine");
    let Some((mut server, port)) = spawn_python_server(&dir, ECHO_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wasip2_tcp readLine test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn doRead(c: Tcp.Connection) -> Unit
    ! [Tcp.readLine, Tcp.close, Console.print]
    match Tcp.readLine(c)
        Result.Ok(line) -> Console.print("echo: {{line}}")
        Result.Err(e) -> Console.print("read err: {{e}}")

fn doWrite(c: Tcp.Connection) -> Unit
    ! [Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.writeLine(c, "ping")
        Result.Ok(_) -> doRead(c)
        Result.Err(e) -> Console.print("write err: {{e}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doWrite(c)
        Result.Err(e) -> Console.print("connect err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "echo.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wasip2_tcp readLine failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains("echo: ping"),
        "expected `echo: ping` round trip, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn tcp_read_bytes_exact_binary_frame() {
    let dir = tempdir("readBytes");
    let Some((mut server, port)) = spawn_python_server(&dir, FIXED_BINARY_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wasip2_tcp readBytes test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
module ReadBytes
    intent = "Read one exact nominal Bytes frame on WASI."
    depends [Bytes]
    effects [Tcp, Console]

fn renderBytes(bytes: List<Int>) -> String
    match bytes
        [] -> ""
        [head, ..tail] -> "{{head}},{{renderBytes(tail)}}"

fn doRead(c: Tcp.Connection) -> Unit
    ! [Tcp.readBytes, Console.print]
    match Tcp.readBytes(c, 4)
        Result.Ok(frame) -> Console.print("got: {{renderBytes(Bytes.toList(frame))}}")
        Result.Err(e) -> Console.print("read err: {{e}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.readBytes, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doRead(c)
        Result.Err(e) -> Console.print("connect err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "read_bytes.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wasip2_tcp readBytes failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains("got: 249,190,180,217,"),
        "expected exact non-UTF-8 frame from Tcp.readBytes, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn tcp_read_bytes_big_count_is_a_result_error() {
    let dir = tempdir("readBytes-big-count");
    let Some((mut server, port)) = spawn_python_server(&dir, ACCEPT_AND_CLOSE_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wasip2_tcp big readBytes count test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));
    let src = format!(
        r#"
module ReadBytesBigCount
    intent = "Reject hostile frame lengths without trapping."
    depends [Bytes]
    effects [Tcp, Console]

fn rejectBigCount(conn: Tcp.Connection) -> Unit
    ! [Tcp.readBytes, Console.print]
    match Tcp.readBytes(conn, 1208925819614629174706176)
        Result.Ok(_) -> Console.print("unexpected-ok")
        Result.Err(_) -> Console.print("range-error")

fn main() -> Unit
    ! [Tcp.connect, Tcp.readBytes, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(conn) -> rejectBigCount(conn)
        Result.Err(e) -> Console.print("connect err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "read_bytes_big_count.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wasip2 big Tcp.readBytes count trapped (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains("range-error"),
        "big count must be catchable, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn tcp_send_one_shot() {
    let dir = tempdir("send");
    let Some((mut server, port)) = spawn_python_server(&dir, PONG_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wasip2_tcp send test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn main() -> Unit
    ! [Tcp.send, Console.print]
    match Tcp.send("127.0.0.1", {port}, "ping")
        Result.Ok(r) -> Console.print("got: {{r}}")
        Result.Err(e) -> Console.print("err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "send.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wasip2_tcp send failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains("got: pong"),
        "expected `got: pong` from Tcp.send orchestrator, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn tcp_send_bytes_one_shot() {
    let dir = tempdir("send-bytes");
    let Some((mut server, port)) = spawn_python_server(&dir, ECHO_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wasip2_tcp sendBytes test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
module SendBytes
    intent = "Round-trip nominal Bytes over the WASI TCP backend."
    depends [Bytes]
    effects [Tcp, Console]

fn renderBytes(bytes: List<Int>) -> String
    match bytes
        [] -> ""
        [head, ..tail] -> "{{head}},{{renderBytes(tail)}}"

fn main() -> Unit
    ! [Tcp.sendBytes, Console.print]
    match Bytes.fromList([249, 190, 180, 217])
        Result.Err(e) -> Console.print("err: {{e}}")
        Result.Ok(payload) -> match Tcp.sendBytes("127.0.0.1", {port}, payload)
            Result.Ok(r) -> Console.print("got: {{renderBytes(Bytes.toList(r))}}")
            Result.Err(e) -> Console.print("err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "send_bytes.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wasip2_tcp sendBytes failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains("got: 249,190,180,217,"),
        "expected non-UTF-8 bytes from Tcp.sendBytes, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn bytes_from_list_rejects_out_of_range_before_tcp() {
    let dir = tempdir("send-bytes-range");
    let src = r#"
module SendBytesRange
    intent = "Reject an invalid octet at the Bytes refinement boundary."
    depends [Bytes]
    effects [Console.print]

fn main() -> Unit
    ! [Console.print]
    match Bytes.fromList([65, 256])
        Result.Ok(_) -> Console.print("unexpected valid Bytes")
        Result.Err(e) -> Console.print(e)
"#;
    let fixture = write_fixture(&dir, "send_bytes_range.av", src);
    let out = run_wasip2(&dir, &fixture);
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wasip2 Bytes range check failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(s, "byte 256 at index 1 is outside 0..=255\n");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn tcp_ping_live_and_closed() {
    let dir = tempdir("ping");
    let Some((mut server, port)) = spawn_python_server(&dir, ACCEPT_AND_CLOSE_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wasip2_tcp ping test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    // Port 1 is privileged + virtually-never bound on a dev box —
    // good enough as a "closed" stand-in. Anything < 1024 the host
    // won't let us bind to without root, so we won't accidentally
    // race a real listener here.
    let src = format!(
        r#"
fn main() -> Unit
    ! [Tcp.ping, Console.print]
    _ = match Tcp.ping("127.0.0.1", {port})
        Result.Ok(_) -> Console.print("live=ok")
        Result.Err(e) -> Console.print("live=err: {{e}}")
    match Tcp.ping("127.0.0.1", 1)
        Result.Ok(_) -> Console.print(" | closed=ok-unexpected")
        Result.Err(e) -> Console.print(" | closed=err")
"#
    );
    let fixture = write_fixture(&dir, "ping.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wasip2_tcp ping failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(s.contains("live=ok"), "expected live=ok, got:\n{s}");
    assert!(
        s.contains(" | closed=err"),
        "expected closed=err (port 1 refused), got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
