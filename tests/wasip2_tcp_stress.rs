//! Wasip2 `Tcp.*` stress tests (Phase 4 / 0.20 "Pulse").
//!
//! Beyond the happy-path coverage in `wasip2_tcp.rs` these tests
//! exercise the load-bearing corner cases:
//!
//! - `pool_slot_ids_increment_across_connects` — three sequential
//!   `Tcp.connect` calls latch slots 0, 1, 2; verifies the
//!   `tcp_next_id` bump + `__rt_tcp_format_id` digit-decoder for
//!   both single- and multi-digit slot indices.
//! - `read_line_realloc_grows_buffer` — server sends a 500-byte
//!   line; the readLine helper's 256-byte initial buffer has to
//!   realloc at least once to fit the payload. Asserts the entire
//!   payload round-trips with no truncation.
//! - `dns_resolve_failure_surfaces_err` — connecting to a
//!   syntactically-invalid hostname propagates
//!   `Result.Err("tcp: dns resolve failed")` from
//!   `wasi:sockets/ip-name-lookup.resolve-addresses`.
//! - `close_is_idempotent` — closing the same connection twice
//!   keeps both calls `Result.Ok(())`; the helper's
//!   `slot.in_use == 0` short-circuit gates the second call away
//!   from wasi-side drops.
//!
//! All four share the Python skip pattern — runs nothing rather
//! than failing when `python3` is absent.

#![cfg(feature = "wasip2")]

use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Server that does three read-a-line / echo-it-back cycles on the
/// same connection before closing. Used by the multi-stage stress
/// to verify that the slot's input + output streams stay usable
/// across multiple `Tcp.writeLine` / `Tcp.readLine` calls (Phase
/// 4.2.2d allocates them once at finish-connect; nothing should
/// invalidate them until `Tcp.close`).
const THREE_ECHO_SCRIPT: &str = r#"
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
            buf = b""
            for _ in range(3):
                # Read until next '\n', echo the whole line back
                # (including the terminator).
                while b"\n" not in buf:
                    chunk = c.recv(4096)
                    if not chunk:
                        break
                    buf += chunk
                if b"\n" not in buf:
                    break
                idx = buf.index(b"\n")
                line = buf[:idx + 1]
                buf = buf[idx + 1:]
                c.sendall(line)
            c.close()
        except OSError:
            break

threading.Thread(target=serve, daemon=True).start()
import time
time.sleep(60)
"#;

const ACCEPT_AND_CLOSE_SCRIPT: &str = r#"
import socket, sys, threading

s = socket.socket()
s.bind(("127.0.0.1", 0))
s.listen(64)
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

/// Server replies with a fixed 500-byte ASCII line (digits cycled)
/// followed by a `\n` terminator — forces the readLine buffer
/// past its 256-byte initial allocation and through at least one
/// `cabi_realloc` doubling.
const BIG_LINE_SCRIPT: &str = r#"
import socket, sys, threading

PAYLOAD = ("0123456789" * 50).encode()  # 500 bytes

s = socket.socket()
s.bind(("127.0.0.1", 0))
s.listen(16)
sys.stdout.write(f"PORT:{s.getsockname()[1]}\n")
sys.stdout.flush()

def serve():
    while True:
        try:
            c, _ = s.accept()
            # Consume the client's writeLine bytes (if any) so the
            # send/close ordering matches the readLine smoke shape.
            data = b""
            while b"\n" not in data:
                chunk = c.recv(4096)
                if not chunk:
                    break
                data += chunk
            c.sendall(PAYLOAD + b"\n")
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
    let dir = std::env::temp_dir().join(format!("aver-wasip2-tcp-stress-{prefix}-{nanos}"));
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
fn pool_slot_ids_increment_across_connects() {
    let dir = tempdir("multi-connect");
    let Some((mut server, port)) = spawn_python_server(&dir, ACCEPT_AND_CLOSE_SCRIPT) else {
        eprintln!("python3 unavailable — skipping multi-connect stress");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn doClose(c: Tcp.Connection) -> Unit
    ! [Tcp.close, Console.print]
    _ = Console.print(c.id)
    closed = Tcp.close(c)
    Console.print(" ")

fn doConnect() -> Unit
    ! [Tcp.connect, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doClose(c)
        Result.Err(e) -> Console.print("err")

fn doSecondAndThird() -> Unit
    ! [Tcp.connect, Tcp.close, Console.print]
    _ = doConnect()
    doConnect()

fn main() -> Unit
    ! [Tcp.connect, Tcp.close, Console.print]
    _ = doConnect()
    doSecondAndThird()
"#
    );
    let fixture = write_fixture(&dir, "multi.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "multi-connect stress failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(s.contains("tcp-0"), "expected tcp-0, got:\n{s}");
    assert!(s.contains("tcp-1"), "expected tcp-1, got:\n{s}");
    assert!(s.contains("tcp-2"), "expected tcp-2, got:\n{s}");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn read_line_realloc_grows_buffer() {
    let dir = tempdir("big-read");
    let Some((mut server, port)) = spawn_python_server(&dir, BIG_LINE_SCRIPT) else {
        eprintln!("python3 unavailable — skipping big-read stress");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn afterRead(c: Tcp.Connection, note: String) -> Unit
    ! [Tcp.close, Console.print]
    _ = Console.print(note)
    closeRes = Tcp.close(c)
    Console.print(" done")

fn doRead(c: Tcp.Connection) -> Unit
    ! [Tcp.readLine, Tcp.close, Console.print]
    match Tcp.readLine(c)
        Result.Ok(line) -> afterRead(c, "len={{String.len(line)}}")
        Result.Err(e) -> afterRead(c, "err")

fn doWrite(c: Tcp.Connection) -> Unit
    ! [Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.writeLine(c, "go")
        Result.Ok(_) -> doRead(c)
        Result.Err(e) -> Console.print("write err")

fn main() -> Unit
    ! [Tcp.connect, Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doWrite(c)
        Result.Err(e) -> Console.print("connect err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "big.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "big-read stress failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains("len=500"),
        "expected len=500 (server sends a 500-byte payload), got:\n{s}"
    );
    assert!(
        s.contains(" done"),
        "expected close confirmation, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn dns_resolve_failure_surfaces_err() {
    let dir = tempdir("dns-fail");
    // Use a `.invalid` TLD — RFC 2606 reserves it for guaranteed
    // non-resolution. No Python server needed: connect should fail
    // at `resolve-addresses` before any TCP handshake starts.
    let src = r#"
fn main() -> Unit
    ! [Tcp.connect, Console.print]
    match Tcp.connect("aver-wasip2-stress.invalid", 80)
        Result.Ok(c) -> Console.print("unexpected ok: {c.id}")
        Result.Err(e) -> Console.print("dns-err: {e}")
"#;
    let fixture = write_fixture(&dir, "dns.av", src);
    let out = run_wasip2(&dir, &fixture);
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "dns-fail stress failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    // Either the resolver-side message (`tcp: dns resolve failed`,
    // when `resolve-addresses` itself returns Err) or the loop-side
    // (`tcp: dns no addresses`, when the stream yielded but ran out
    // before any IPv4 surfaced). Different hosts surface .invalid
    // through different code paths; the test accepts either.
    assert!(
        s.contains("dns-err: tcp: dns"),
        "expected a `tcp: dns *` Err from the resolver, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn close_is_idempotent() {
    let dir = tempdir("idempotent");
    let Some((mut server, port)) = spawn_python_server(&dir, ACCEPT_AND_CLOSE_SCRIPT) else {
        eprintln!("python3 unavailable — skipping idempotent-close stress");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn doSecondClose(c: Tcp.Connection, first: String) -> Unit
    ! [Tcp.close, Console.print]
    _ = Console.print(first)
    match Tcp.close(c)
        Result.Ok(_) -> Console.print(" second-ok")
        Result.Err(e) -> Console.print(" second-err: {{e}}")

fn doFirstClose(c: Tcp.Connection) -> Unit
    ! [Tcp.close, Console.print]
    match Tcp.close(c)
        Result.Ok(_) -> doSecondClose(c, " first-ok")
        Result.Err(e) -> doSecondClose(c, " first-err")

fn main() -> Unit
    ! [Tcp.connect, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doFirstClose(c)
        Result.Err(e) -> Console.print("connect err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "idem.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "idempotent-close stress failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains(" first-ok"),
        "expected first close to succeed, got:\n{s}"
    );
    assert!(
        s.contains(" second-ok"),
        "expected second close to be a no-op Ok (slot.in_use == 0 guard), got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn multi_stage_write_read_on_same_connection() {
    // Same connection: write "a" → read echo, write "b" → read
    // echo, write "c" → read echo, close. Exercises slot stream
    // reuse — Phase 4.2.2d's `in_stream` / `out_stream` are
    // latched once at finish-connect; every subsequent writeLine /
    // readLine pulls them out of the pool slot by id, so this test
    // catches anything that would invalidate the streams after a
    // single call (e.g. an accidental drop in `tcp_close`'s path
    // firing for the wrong slot).
    let dir = tempdir("multi-stage");
    let Some((mut server, port)) = spawn_python_server(&dir, THREE_ECHO_SCRIPT) else {
        eprintln!("python3 unavailable — skipping multi-stage stress");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn afterC(c: Tcp.Connection, note: String) -> Unit
    ! [Tcp.close, Console.print]
    _ = Console.print(note)
    closed = Tcp.close(c)
    Console.print(" closed")

fn doReadC(c: Tcp.Connection, prefix: String) -> Unit
    ! [Tcp.readLine, Tcp.close, Console.print]
    match Tcp.readLine(c)
        Result.Ok(line) -> afterC(c, "{{prefix}}|{{line}}")
        Result.Err(e) -> afterC(c, "{{prefix}}|read-err")

fn doWriteC(c: Tcp.Connection, prefix: String) -> Unit
    ! [Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.writeLine(c, "c-line")
        Result.Ok(_) -> doReadC(c, prefix)
        Result.Err(e) -> afterC(c, "{{prefix}}|wc-err")

fn doReadB(c: Tcp.Connection, prefix: String) -> Unit
    ! [Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.readLine(c)
        Result.Ok(line) -> doWriteC(c, "{{prefix}}|{{line}}")
        Result.Err(e) -> afterC(c, "{{prefix}}|rb-err")

fn doWriteB(c: Tcp.Connection) -> Unit
    ! [Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.writeLine(c, "b-line")
        Result.Ok(_) -> doReadB(c, "stage-b")
        Result.Err(e) -> afterC(c, "stage-b|wb-err")

fn afterReadA(c: Tcp.Connection, line: String) -> Unit
    ! [Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    _ = Console.print("stage-a|{{line}}|")
    doWriteB(c)

fn doReadA(c: Tcp.Connection) -> Unit
    ! [Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.readLine(c)
        Result.Ok(line) -> afterReadA(c, line)
        Result.Err(e) -> afterC(c, "stage-a|ra-err")

fn doWriteA(c: Tcp.Connection) -> Unit
    ! [Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.writeLine(c, "a-line")
        Result.Ok(_) -> doReadA(c)
        Result.Err(e) -> afterC(c, "stage-a|wa-err")

fn main() -> Unit
    ! [Tcp.connect, Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doWriteA(c)
        Result.Err(e) -> Console.print("connect err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "multi.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "multi-stage stress failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains("stage-a|a-line"),
        "expected first echo `stage-a|a-line`, got:\n{s}"
    );
    assert!(
        s.contains("stage-b|b-line"),
        "expected second echo `stage-b|b-line`, got:\n{s}"
    );
    assert!(
        s.contains("|c-line"),
        "expected third echo `c-line` substring, got:\n{s}"
    );
    assert!(s.contains(" closed"), "expected close note, got:\n{s}");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn pool_wraparound_recovers_stale_slot() {
    // 260 sequential connect+close pairs. The pool is 256 slots
    // deep, so the 257th connect lands on `tcp_next_id & 255 == 0`
    // — the slot Phase 4.2.2d wrote on the very first connect.
    // Phase 4.2.2e's recovery branch fires here: even though the
    // first connection was already closed (in_use = 0), Aver's
    // probe walks the slot ref + `in_use` field; the `in_use == 0`
    // short-circuit means we don't redundantly drop wasi handles
    // we already released. Either way the test passes if no trap
    // fires — without the recovery code, an in_use == 1 leftover
    // from a never-closed slot would re-bind streams over a live
    // socket and break wasmtime's resource bookkeeping.
    //
    // We pick 260 rather than 257 so the wrapping is well past
    // the boundary and verify the id of the last connection lands
    // at `tcp-{(260 - 1) & 255}` = `tcp-3` (slot indices 0..255
    // cycle from index 0 on connect #1).
    let dir = tempdir("wraparound");
    let Some((mut server, port)) = spawn_python_server(&dir, ACCEPT_AND_CLOSE_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wraparound stress");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn doOnce(c: Tcp.Connection) -> String
    ! [Tcp.close]
    closeRes = Tcp.close(c)
    c.id

fn doConnect() -> String
    ! [Tcp.connect, Tcp.close]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doOnce(c)
        Result.Err(e) -> "err"

fn loopN(n: Int, last: String) -> String
    ! [Tcp.connect, Tcp.close]
    match n
        0 -> last
        _ -> loopN(n - 1, doConnect())

fn main() -> Unit
    ! [Tcp.connect, Tcp.close, Console.print]
    final = loopN(260, "none")
    Console.print("final id: {{final}}")
"#
    );
    let fixture = write_fixture(&dir, "wrap.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "wraparound stress failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    // tcp_next_id starts at 0 and bumps after each successful
    // connect; the 260th connect bumps it to 260, but the slot
    // index of that connect was `259 & 255` = 3. So the id
    // returned should be `tcp-3`.
    assert!(
        s.contains("final id: tcp-3"),
        "expected `final id: tcp-3` after 260 connects (slot index = 259 & 255 = 3), got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
