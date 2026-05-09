//! Wasip2 `Http.get` end-to-end test.
//!
//! Spins up a small Python HTTP server bound to an OS-assigned
//! port (avoids fixed-port flakes in parallel test runs), serves a
//! known fixture, then compiles + runs an Aver program that calls
//! `Http.get` against it. Asserts the surfaced HTTP status and
//! that the body bytes round-trip into the printed Aver String.
//!
//! Pre-fix this didn't compile at all (`Http.*` rejected by
//! `effect_check.rs`). Post-fix the wasip2 backend lowers
//! `Http.get` to the wasi:http/outgoing-handler.handle pipeline
//! via `__rt_http_get`.
//!
//! Skipped automatically if `python3` is not on PATH.

#![cfg(feature = "wasip2")]

use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

const SERVER_SCRIPT: &str = r#"
import http.server, socketserver, sys

class H(http.server.SimpleHTTPRequestHandler):
    def log_message(self, *a, **k):
        pass

socketserver.TCPServer.allow_reuse_address = True
with socketserver.TCPServer(('127.0.0.1', 0), H) as srv:
    sys.stdout.write(f'PORT:{srv.server_address[1]}\n')
    sys.stdout.flush()
    srv.serve_forever()
"#;

fn tempdir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("aver-wasip2-http-{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create tempdir");
    dir
}

fn write_fixture(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, source).expect("write fixture");
    path
}

/// Spawn `python3 -c SERVER_SCRIPT` in `dir`, wait for the
/// `PORT:N` line on stdout, return `(child, port)`. Returns
/// `None` when `python3` is not available — tests skip in that
/// case rather than fail (CI environments without python should
/// not block PR merges).
fn spawn_python_server(dir: &Path) -> Option<(Child, u16)> {
    let mut child = match Command::new("python3")
        .args(["-c", SERVER_SCRIPT])
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
    // Drop the reader so the stdout pipe stays open and the child
    // doesn't SIGPIPE on subsequent prints (Python rebinds stdout
    // back to the inherited fd).
    drop(reader);
    Some((child, port))
}

fn run_wasip2(dir: &Path, fixture: &Path, args: &[&str]) -> std::process::Output {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(dir).arg("run").arg("--wasip2").arg(fixture);
    if !args.is_empty() {
        cmd.arg("--");
        for a in args {
            cmd.arg(a);
        }
    }
    cmd.output().expect("aver run --wasip2 to launch")
}

#[test]
fn http_get_returns_status_and_body() {
    let dir = tempdir("get");
    // Fixture: a small HTML doc the server will return for `/`.
    std::fs::write(
        dir.join("index.html"),
        "<!doctype html><h1>aver-wasip2-http</h1>\n",
    )
    .expect("write fixture html");

    let Some((mut server, port)) = spawn_python_server(&dir) else {
        eprintln!("python3 unavailable — skipping wasip2_http test");
        return;
    };
    // Give the server a tick to start accept()'ing.
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn main() -> Unit
    ! [Http.get, Console.print]
    response = Http.get("http://127.0.0.1:{port}/")
    match response
        Result.Ok(r) -> Console.print("status={{r.status}} body_len={{String.len(r.body)}}")
        Result.Err(e) -> Console.print("err: {{e}}")
"#
    );
    // Aver string interpolation `{x}` in the source must remain
    // braces in the file. The format! macro above uses `{{` /
    // `}}` to escape — so the on-disk source has the right `{x}`.

    let fixture = write_fixture(&dir, "get.av", &src);
    let out = run_wasip2(&dir, &fixture, &[]);

    let _ = server.kill();
    let _ = server.wait();

    assert!(
        out.status.success(),
        "wasip2_http compile/run failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        s.contains("status=200"),
        "expected status=200 from Http.get, got:\n{s}"
    );
    // The fixture body is 41 bytes; assert non-empty and matches
    // the on-disk length.
    let expected_len = std::fs::metadata(dir.join("index.html"))
        .expect("stat fixture")
        .len();
    assert!(
        s.contains(&format!("body_len={expected_len}")),
        "expected body_len={expected_len} (matches fixture file size), got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
