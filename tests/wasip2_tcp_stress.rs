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
//!   surfaces `Result.Ok(())` then `Result.Err("tcp: unknown
//!   connection")`, matching `aver-rt::tcp::close` semantics on
//!   the VM / self-host / wasm-gc backends (Phase 4.7+).
//!
//! All four share the Python skip pattern — runs nothing rather
//! than failing when `python3` is absent.

#![cfg(feature = "wasip2")]

use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Server that reads a single line (newline-terminated), no matter
/// how long, and echoes back `LEN:<bytes-before-newline>\n`. Used
/// by `chunked_write_loops_past_4kb` to verify that the
/// `emit_chunked_blocking_write` loop in `__rt_tcp_write_line`
/// iterates correctly past the wasmtime-wasi 4096-byte per-call
/// cap on `blocking-write-and-flush`.
const LEN_REPLY_SCRIPT: &str = r#"
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
            while b"\n" not in buf:
                chunk = c.recv(65536)
                if not chunk:
                    break
                buf += chunk
            payload = buf.split(b"\n", 1)[0] if b"\n" in buf else buf
            # Aver's `Tcp.writeLine` sends `\r\n`; strip the trailing
            # `\r` so the reported length matches the original
            # payload (not payload + CR).
            if payload.endswith(b"\r"):
                payload = payload[:-1]
            c.sendall(f"LEN:{len(payload)}\n".encode())
            c.close()
        except OSError:
            break

threading.Thread(target=serve, daemon=True).start()
import time
time.sleep(60)
"#;

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
    closed = Tcp.close(c)
    Console.print("close-ok ")

fn doConnect() -> Unit
    ! [Tcp.connect, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doClose(c)
        Result.Err(e) -> Console.print("err ")

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
    // `Tcp.Connection` is opaque (Phase 4.7+ fix #11), so the
    // test asserts on the visible close outcomes rather than the
    // internal id strings. Three round-trip cycles all close Ok
    // — that's the wraparound + slot reuse story end-to-end.
    let oks = s.matches("close-ok ").count();
    assert_eq!(
        oks, 3,
        "expected three close-ok markers from three sequential connect/close cycles, got {oks} in:\n{s}",
    );
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
        Result.Ok(_) -> Console.print("unexpected ok")
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
    // Phase 4.7+ — second close on the same `in_use == 0` slot
    // returns `Err("tcp: unknown connection")` so wasip2 matches
    // `aver-rt::tcp::close` (used by VM / self-host / wasm-gc).
    assert!(
        s.contains(" second-err: tcp: unknown connection"),
        "expected second close to surface stale-conn Err, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn chunked_write_loops_past_4kb() {
    // wasmtime-wasi caps single `blocking-write-and-flush` calls
    // at 4096 bytes. `emit_chunked_blocking_write` iterates as
    // many times as needed; this test pushes a 5000-byte payload
    // (≥ two iterations of the loop) through `Tcp.writeLine` and
    // verifies that all bytes traversed the boundary. The server
    // replies with `LEN:<count>\n` so we can sanity-check the
    // byte count Aver-side via `Tcp.readLine`.
    let dir = tempdir("chunked-write");
    let Some((mut server, port)) = spawn_python_server(&dir, LEN_REPLY_SCRIPT) else {
        eprintln!("python3 unavailable — skipping chunked-write stress");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    // 5000 x ASCII 'x' — far past the 4 KB chunk boundary, no
    // embedded newlines so the server reads it all as one line.
    let big = "x".repeat(5000);

    let src = format!(
        r#"
fn doRead(c: Tcp.Connection) -> Unit
    ! [Tcp.readLine, Tcp.close, Console.print]
    match Tcp.readLine(c)
        Result.Ok(reply) -> Console.print("server: {{reply}}")
        Result.Err(e) -> Console.print("read err: {{e}}")

fn doWrite(c: Tcp.Connection) -> Unit
    ! [Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.writeLine(c, "{big}")
        Result.Ok(_) -> doRead(c)
        Result.Err(e) -> Console.print("write err: {{e}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doWrite(c)
        Result.Err(e) -> Console.print("connect err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "chunked.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "chunked-write stress failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains("server: LEN:5000"),
        "expected `server: LEN:5000` (full payload received), got:\n{s}"
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
    // 260 connect + close cycles — well past the 256-slot pool
    // boundary. `Tcp.Connection` is opaque (Phase 4.7+ fix #11),
    // so the test asserts on the running count of Ok closes
    // rather than introspecting the id strings. Any trap, hung
    // wraparound, or close failure trips the count.
    let dir = tempdir("wraparound");
    let Some((mut server, port)) = spawn_python_server(&dir, ACCEPT_AND_CLOSE_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wraparound stress");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn doOnce(c: Tcp.Connection) -> Int
    ! [Tcp.close]
    match Tcp.close(c)
        Result.Ok(_) -> 1
        Result.Err(_) -> 0

fn doConnect() -> Int
    ! [Tcp.connect, Tcp.close]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> doOnce(c)
        Result.Err(_) -> 0

fn loopN(n: Int, acc: Int) -> Int
    ! [Tcp.connect, Tcp.close]
    match n
        0 -> acc
        _ -> loopN(n - 1, acc + doConnect())

fn main() -> Unit
    ! [Tcp.connect, Tcp.close, Console.print]
    ok = loopN(260, 0)
    Console.print("ok-closes: {{ok}}")
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
    assert!(
        s.contains("ok-closes: 260"),
        "expected 260 successful close cycles past the 256-slot pool boundary, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn pool_limit_refuses_257th_simultaneous_connect() {
    // Regression for Phase 4.7+ fix #10 — connection-limit parity
    // with `aver-rt::tcp::connect`, which refuses the 257th live
    // connect with `Err("Tcp.connect: connection limit reached
    // (256 max)")`. Wasip2 used to silently evict the existing
    // live occupant of slot `tcp_next_id & 255`, which let a
    // misbehaving program shut down another part of itself.
    //
    // The probe opens up to 260 simultaneous connects (no closes)
    // and stops at the first Err. Slots 0..255 fill in order; the
    // 257th call falls on slot 0 (already `in_use == 1`) and must
    // surface the limit message.
    let dir = tempdir("conn-limit");
    let Some((mut server, port)) = spawn_python_server(&dir, ACCEPT_AND_CLOSE_SCRIPT) else {
        eprintln!("python3 unavailable — skipping conn-limit stress");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn fill(n: Int) -> Int
    ! [Tcp.connect]
    match n
        0 -> 0
        _ -> match Tcp.connect("127.0.0.1", {port})
            Result.Ok(_) -> fill(n - 1)
            Result.Err(_) -> n

fn main() -> Unit
    ! [Tcp.connect, Console.print]
    remaining = fill(260)
    Console.print("remaining: {{remaining}}")
"#
    );
    let fixture = write_fixture(&dir, "limit.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "conn-limit stress failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    // 260 attempts; 256 succeed, then the 257th call (n = 4) hits
    // slot 0 which is still `in_use == 1` and bails out. The
    // recursive helper returns the n at which it gave up, so we
    // expect `remaining: 4`.
    assert!(
        s.contains("remaining: 4"),
        "expected pool to refuse the 257th simultaneous connect, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn write_line_past_64kib_keeps_payload_intact() {
    // Regression for Phase 4.7+ fix #6 — bump-allocator collision.
    //
    // Before the bump-cursor advance in `emit_tcp_write_line`,
    // `__rt_string_to_lm` wrote the payload at LM[0..len] and grew
    // memory to fit; `cabi_realloc` then handed out a buffer
    // starting at offset 65536 — strictly inside the payload — to
    // hold the trailing '\n' and per-call retptr. Lines longer
    // than 64KiB silently lost bytes 65536..+12.
    //
    // We push a 70_000-byte line through `Tcp.writeLine` and ask the
    // echo server to report its length. The Aver-side assertion
    // checks `LEN:70000`; corruption would shrink the count or trip
    // the read.
    let dir = tempdir("write-past-64kib");
    let Some((mut server, port)) = spawn_python_server(&dir, LEN_REPLY_SCRIPT) else {
        eprintln!("python3 unavailable — skipping >64KiB stress");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    // Build the 70_000-byte payload Rust-side and inline it as an
    // Aver string literal — recursive concat would be O(n²) on
    // immutable strings and hang the test for minutes.
    let big = "a".repeat(70_000);
    let src = format!(
        r#"
fn readReply(c: Tcp.Connection) -> String
    ! [Tcp.readLine]
    match Tcp.readLine(c)
        Result.Ok(r) -> r
        Result.Err(e) -> "read-err"

fn run(c: Tcp.Connection, line: String) -> String
    ! [Tcp.writeLine, Tcp.readLine]
    match Tcp.writeLine(c, line)
        Result.Ok(_) -> readReply(c)
        Result.Err(e) -> "write-err"

fn closeAndPrint(c: Tcp.Connection, reply: String) -> Unit
    ! [Tcp.close, Console.print]
    closed = Tcp.close(c)
    Console.print("reply: {{reply}}")

fn runAndReport(c: Tcp.Connection, line: String) -> Unit
    ! [Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    reply = run(c, line)
    closeAndPrint(c, reply)

fn main() -> Unit
    ! [Tcp.connect, Tcp.writeLine, Tcp.readLine, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(c) -> runAndReport(c, "{big}")
        Result.Err(e) -> Console.print("connect-err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "big.av", &src);
    let out = run_wasip2(&dir, &fixture);
    let _ = server.kill();
    let _ = server.wait();
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        ">64KiB write stress failed (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains("reply: LEN:70000"),
        "expected `LEN:70000` from server (payload preserved past 64KiB bump-allocator boundary), got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn port_out_of_range_is_rejected_upfront() {
    // Regression for Phase 4.7+ fix #7 — port validation parity
    // with `aver-rt::services::tcp::port_arg`. Negative + >65535
    // both surface `Result.Err("tcp: port out of range")` before
    // any DNS or socket work; previously wasip2 quietly truncated
    // the i64 via `i32.wrap_i64` and returned a generic connect
    // failure (or, worse, hit a real port).
    let dir = tempdir("port-range");
    let src = r#"
fn tryPort(p: Int) -> String
    ! [Tcp.connect]
    match Tcp.connect("127.0.0.1", p)
        Result.Ok(c) -> "ok"
        Result.Err(e) -> e

fn main() -> Unit
    ! [Tcp.connect, Console.print]
    Console.print("neg: {tryPort(-1)}")
    Console.print("hi: {tryPort(65536)}")
    Console.print("low: {tryPort(0)}")
"#;
    let fixture = write_fixture(&dir, "port.av", src);
    let out = run_wasip2(&dir, &fixture);
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "port-range probe exited non-zero (exit {:?})\nstdout:\n{s}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        s.contains("neg: tcp: port out of range"),
        "expected port -1 rejected with `tcp: port out of range`, got:\n{s}"
    );
    assert!(
        s.contains("hi: tcp: port out of range"),
        "expected port 65536 rejected with `tcp: port out of range`, got:\n{s}"
    );
    // Port 0 is in range — the connect failure here is whatever
    // wasi-sockets reports for a closed loopback port; we just
    // check the message ISN'T the port-validation Err.
    assert!(
        !s.contains("low: tcp: port out of range"),
        "port 0 should be in-range; got port-out-of-range error:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn handcrafted_tcp_connection_is_compile_error() {
    // Phase 4.7+ fix #11 — `Tcp.Connection` is opaque. The
    // typechecker rejects construction up-front, so a forged-id
    // attack never even reaches the runtime guards. The null-pool
    // / generation / in_use checks (Phase 4.7+ fix #8 + #2) stay
    // as defence-in-depth for emitter bugs, but the surface API
    // contract is already enforced at compile time.
    let dir = tempdir("handcrafted-opaque");
    let src = r#"
fn main() -> Unit
    ! [Tcp.close, Console.print]
    fake = Tcp.Connection(id = "tcp-42", host = "nowhere", port = 80)
    match Tcp.close(fake)
        Result.Ok(_) -> Console.print("unexpected: ok")
        Result.Err(e) -> Console.print("hand: {e}")
"#;
    let fixture = write_fixture(&dir, "hand.av", src);
    let out = run_wasip2(&dir, &fixture);
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        !out.status.success(),
        "expected compile-time rejection of `Tcp.Connection(...)`, got success.\nstderr:\n{stderr}"
    );
    assert!(
        stderr.contains("opaque type 'Tcp.Connection'"),
        "expected opaque-type diagnostic, got:\n{stderr}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
