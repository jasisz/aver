//! Wasip2 `Tcp.*` end-to-end tests (Phase 4 / 0.20 "Pulse").
//!
//! Five scenarios share a tiny Python TCP server bound on an
//! OS-assigned port (avoids fixed-port flakes in parallel runs):
//!
//! - `tcp_connect_close_round_trip` — connect + close, verify
//!   the returned `Tcp.Connection.id` shape (`"tcp-N"`) and the
//!   host / port fields.
//! - `tcp_write_line_round_trip` — write a single line, the
//!   Python server reads + closes, Aver gets `Result.Ok(())`.
//! - `tcp_read_line_echo_round_trip` — write+read against an
//!   echo server, assert the line round-trips byte-for-byte.
//! - `tcp_send_one_shot` — `Tcp.send` orchestrator: connect +
//!   write + read + close in one call, return the response line.
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
    let mut child = match Command::new("python3")
        .args(["-c", script])
        .current_dir(dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(_) => return None,
    };
    let stdout = child.stdout.take().expect("stdout pipe");
    let mut reader = BufReader::new(stdout);
    let mut line = String::new();
    let read = reader.read_line(&mut line).ok()?;
    if read == 0 {
        let _ = child.kill();
        return None;
    }
    let port: u16 = line
        .trim()
        .strip_prefix("PORT:")
        .and_then(|s| s.parse().ok())?;
    drop(reader);
    Some((child, port))
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
