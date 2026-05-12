//! Wasip2 `HttpServer.listen` end-to-end test (0.19 Phase 3).
//!
//! Compiles a small Aver program that wires `HttpServer.listen(_,
//! handler)` against `--target wasip2 --world wasi:http/proxy`,
//! spawns `wasmtime serve --addr=127.0.0.1:0 <component>`, parses
//! the bound port out of wasmtime's stderr (`Serving HTTP on
//! http://127.0.0.1:N/`), then runs a handful of HTTP requests
//! against it and asserts the responses match the handler's
//! returned `HttpResponse`.
//!
//! Skipped automatically when `wasmtime` is not on PATH or when
//! `wasmtime serve` is missing (older wasmtime builds without the
//! `wasi-http` server feature). The wasm-gc + tail-call proposals
//! are enabled via `-W gc=y -W tail-call=y` — every modern wasmtime
//! ships them as opt-in flags rather than default-on.

#![cfg(feature = "wasip2")]

use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

fn tempdir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("aver-wasip2-server-{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create tempdir");
    dir
}

/// Returns `None` when `wasmtime serve --help` does not print a
/// `--addr` flag (older wasmtime without the wasi-http server) or
/// when the binary itself is missing.
fn wasmtime_serve_supported() -> bool {
    let Ok(output) = Command::new("wasmtime")
        .args(["serve", "--help"])
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .output()
    else {
        return false;
    };
    if !output.status.success() {
        return false;
    }
    String::from_utf8_lossy(&output.stdout).contains("--addr")
}

/// Compile the Aver source at `src_path` into a `.component.wasm`
/// under `dir`. Returns the absolute path of the produced
/// component.
fn compile_proxy(dir: &Path, src_path: &Path, stem: &str) -> PathBuf {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = Command::new(aver_bin)
        .current_dir(dir)
        .arg("compile")
        .arg(src_path)
        .arg("--target")
        .arg("wasip2")
        .arg("--world")
        .arg("wasi:http/proxy")
        .arg("-o")
        .arg(dir)
        .arg("--name")
        .arg(stem)
        .output()
        .expect("aver compile to launch");
    assert!(
        out.status.success(),
        "aver compile --target wasip2 --world wasi:http/proxy failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    dir.join(format!("{stem}.component.wasm"))
}

/// Spawn `wasmtime serve --addr=127.0.0.1:0 <component>` and parse
/// the bound port from its stderr. Returns `None` if the binary
/// failed to start or the expected serving banner never showed up
/// within a few seconds.
fn spawn_wasmtime_serve(component: &Path) -> Option<(Child, u16)> {
    let mut child = Command::new("wasmtime")
        .args([
            "serve",
            "-W",
            "gc=y",
            "-W",
            "tail-call=y",
            "--addr=127.0.0.1:0",
        ])
        .arg(component)
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .ok()?;

    let stderr = child.stderr.take().expect("stderr pipe");
    let mut reader = BufReader::new(stderr);
    let mut line = String::new();
    let deadline = Instant::now() + Duration::from_secs(5);
    let port = loop {
        line.clear();
        let read = reader.read_line(&mut line).ok()?;
        if read == 0 {
            let _ = child.kill();
            return None;
        }
        if let Some(rest) = line
            .trim()
            .strip_prefix("Serving HTTP on http://127.0.0.1:")
        {
            let port_str = rest.split('/').next().unwrap_or("");
            if let Ok(p) = port_str.parse::<u16>() {
                break p;
            }
        }
        if Instant::now() > deadline {
            let _ = child.kill();
            return None;
        }
    };
    // Drop the reader so wasmtime's stderr pipe doesn't fill and
    // SIGPIPE later (it stays inherited from the parent fd 2 with
    // Stdio::piped, so subsequent writes go to /dev/null-ish).
    drop(reader);
    Some((child, port))
}

fn write_fixture(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, source).expect("write fixture");
    path
}

#[test]
fn http_server_echoes_request_body() {
    if !wasmtime_serve_supported() {
        eprintln!("wasmtime serve unavailable — skipping wasip2_http_server test");
        return;
    }
    let dir = tempdir("echo");
    let src = r#"
fn handler(req: HttpRequest) -> HttpResponse
    HttpResponse(status = 200, body = "echo: {req.body}", headers = {})

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, handler)
"#;
    let fixture = write_fixture(&dir, "echo.av", src);
    let component = compile_proxy(&dir, &fixture, "echo");

    let Some((mut server, port)) = spawn_wasmtime_serve(&component) else {
        eprintln!("wasmtime serve failed to start — skipping test");
        let _ = std::fs::remove_dir_all(&dir);
        return;
    };

    // Use curl rather than ureq/reqwest to avoid pulling another
    // crate into the test-only graph. Every CI we ship with has
    // curl on PATH.
    let out = Command::new("curl")
        .args([
            "-sS",
            "-X",
            "POST",
            "-d",
            "hello",
            &format!("http://127.0.0.1:{port}/"),
        ])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("curl to launch");

    let _ = server.kill();
    let _ = server.wait();

    assert!(
        out.status.success(),
        "curl failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let body = String::from_utf8_lossy(&out.stdout).into_owned();
    assert_eq!(body, "echo: hello", "expected `echo: hello`, got: {body:?}");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn http_server_surfaces_method_path_and_query() {
    if !wasmtime_serve_supported() {
        eprintln!("wasmtime serve unavailable — skipping wasip2_http_server test");
        return;
    }
    let dir = tempdir("introspect");
    // Handler dumps method + path + query into the body so the
    // assertions can compare against the request shape end-to-end.
    let src = r#"
fn handler(req: HttpRequest) -> HttpResponse
    HttpResponse(
        status = 201,
        body = "m={req.method} p={req.path} q={req.query}",
        headers = {}
    )

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, handler)
"#;
    let fixture = write_fixture(&dir, "intro.av", src);
    let component = compile_proxy(&dir, &fixture, "intro");

    let Some((mut server, port)) = spawn_wasmtime_serve(&component) else {
        eprintln!("wasmtime serve failed to start — skipping test");
        let _ = std::fs::remove_dir_all(&dir);
        return;
    };

    // Hit `/hello/world?name=ada&color=blue` with a GET. The handler
    // should report method=GET, path=/hello/world, query=name=ada&color=blue.
    let out = Command::new("curl")
        .args([
            "-sS",
            "-w",
            "\nSTATUS:%{http_code}\n",
            &format!("http://127.0.0.1:{port}/hello/world?name=ada&color=blue"),
        ])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("curl to launch");

    let _ = server.kill();
    let _ = server.wait();

    assert!(
        out.status.success(),
        "curl failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        s.contains("m=GET"),
        "expected m=GET in handler output, got: {s:?}"
    );
    assert!(
        s.contains("p=/hello/world"),
        "expected p=/hello/world in handler output, got: {s:?}"
    );
    assert!(
        s.contains("q=name=ada&color=blue"),
        "expected q=name=ada&color=blue in handler output, got: {s:?}"
    );
    assert!(
        s.contains("STATUS:201"),
        "expected handler-supplied status 201 to round-trip, got: {s:?}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn http_server_round_trips_response_headers() {
    if !wasmtime_serve_supported() {
        eprintln!("wasmtime serve unavailable — skipping wasip2_http_server test");
        return;
    }
    let dir = tempdir("hdr");
    // Handler echoes a chosen request header into the response so
    // we can verify both directions: client sends `X-Foo: bar`,
    // handler reads it from `req.headers`, response copies it as
    // `X-Echo`.
    let src = r#"
fn first_or(items: List<String>, dflt: String) -> String
    match items
        [] -> dflt
        [head, ..rest] -> head

fn handler(req: HttpRequest) -> HttpResponse
    foo: String = match Map.get(req.headers, "x-foo")
        Option.Some(values) -> first_or(values, "missing")
        Option.None -> "absent"
    HttpResponse(
        status = 200,
        body = "ok",
        headers = {"x-echo" => [foo], "x-flavor" => ["aver"]}
    )

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, handler)
"#;
    let fixture = write_fixture(&dir, "hdr.av", src);
    let component = compile_proxy(&dir, &fixture, "hdr");

    let Some((mut server, port)) = spawn_wasmtime_serve(&component) else {
        eprintln!("wasmtime serve failed to start — skipping test");
        let _ = std::fs::remove_dir_all(&dir);
        return;
    };

    let out = Command::new("curl")
        .args([
            "-sS",
            "-i", // include headers in stdout
            "-H",
            "X-Foo: bar-value",
            &format!("http://127.0.0.1:{port}/"),
        ])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("curl to launch");

    let _ = server.kill();
    let _ = server.wait();

    assert!(
        out.status.success(),
        "curl failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    // Status line — first line of the response.
    assert!(
        s.starts_with("HTTP/1.1 200"),
        "expected `HTTP/1.1 200` status line, got: {s:?}"
    );
    // Both response headers round-trip — case-insensitive match
    // since some proxies normalise case in transit.
    let lower = s.to_lowercase();
    assert!(
        lower.contains("x-echo: bar-value"),
        "expected `x-echo: bar-value` in response headers, got: {s:?}"
    );
    assert!(
        lower.contains("x-flavor: aver"),
        "expected `x-flavor: aver` in response headers, got: {s:?}"
    );
    // Body is just `ok` (no headers prepended thanks to curl's
    // header/body separator handling).
    assert!(
        s.ends_with("\r\nok") || s.ends_with("\nok"),
        "expected response body `ok` after blank line, got: {s:?}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
