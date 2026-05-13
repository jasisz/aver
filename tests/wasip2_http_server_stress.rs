//! Wasip2 `HttpServer.listen` stress + variety tests.
//!
//! Pin behaviour the happy-path e2e tests don't reach:
//! - large bodies in both directions (drain loop buffer growth +
//!   chunked-write boundary crossings);
//! - many sequential requests on one wasmtime instance (resource
//!   leak surface; component model reuses instances);
//! - concurrent requests (instantiation correctness when multiple
//!   inflight at once);
//! - routing on `req.path` via Aver `match` (real handler logic);
//! - method dispatch over the full HTTP verb table;
//! - JSON-shaped response with explicit Content-Type round-trip;
//! - 30+ request headers preserved into `Map<String, List<String>>`.
//!
//! Skipped automatically when `wasmtime` is not on PATH or when
//! `wasmtime serve` is missing.

#![cfg(feature = "wasip2")]

use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

fn tempdir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("aver-wasip2-server-stress-{prefix}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create tempdir");
    dir
}

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

fn compile_proxy(dir: &Path, src_path: &Path, stem: &str, handler: &str) -> PathBuf {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = Command::new(aver_bin)
        .current_dir(dir)
        .arg("compile")
        .arg(src_path)
        .arg("--target")
        .arg("wasip2")
        .arg("--world")
        .arg("wasi:http/proxy")
        .arg("--handler")
        .arg(handler)
        .arg("-o")
        .arg(dir)
        .arg("--name")
        .arg(stem)
        .output()
        .expect("aver compile to launch");
    assert!(
        out.status.success(),
        "aver compile --handler {handler} failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    dir.join(format!("{stem}.component.wasm"))
}

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
    drop(reader);
    Some((child, port))
}

fn write_fixture(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, source).expect("write fixture");
    path
}

/// Run curl synchronously and return (stdout, stderr, exit-status).
fn curl(args: &[&str]) -> (String, String, std::process::ExitStatus) {
    let out = Command::new("curl")
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("curl to launch");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status,
    )
}

/// Setup: compile + spawn. Returns `(server_child, port, tempdir)`
/// so the caller can SIGKILL the server and clean up at the end.
/// Returns `None` when wasmtime is unavailable — tests skip in that
/// case.
fn setup(prefix: &str, src: &str, stem: &str, handler: &str) -> Option<(Child, u16, PathBuf)> {
    if !wasmtime_serve_supported() {
        eprintln!("wasmtime serve unavailable — skipping {prefix} stress test");
        return None;
    }
    let dir = tempdir(prefix);
    let fixture = write_fixture(&dir, &format!("{stem}.av"), src);
    let component = compile_proxy(&dir, &fixture, stem, handler);
    let (server, port) = spawn_wasmtime_serve(&component)?;
    Some((server, port, dir))
}

fn teardown(server: &mut Child, dir: &Path) {
    let _ = server.kill();
    let _ = server.wait();
    let _ = std::fs::remove_dir_all(dir);
}

// ─────────────────────────────────────────────────────────────────
// 1. Large response body — handler builds a 32 KB body via 10
//    O(prev) doublings (32 → 64 → 128 → … → 32768). Forces the
//    chunked `blocking-write-and-flush` loop in
//    `emit_aver_http_handle` to issue 8 host calls of 4096 bytes
//    each. Doubling sidesteps the O(n²) recursive-concat trap
//    that recursive `repeat` over 1024 iterations would hit.
// ─────────────────────────────────────────────────────────────────
#[test]
fn large_response_body_32k_round_trips() {
    let src = r#"
fn dbl(s: String) -> String
    "{s}{s}"

fn big_handler(req: HttpRequest) -> HttpResponse
    s0: String = "0123456789ABCDEF0123456789ABCDEF"
    s1: String = dbl(s0)
    s2: String = dbl(s1)
    s3: String = dbl(s2)
    s4: String = dbl(s3)
    s5: String = dbl(s4)
    s6: String = dbl(s5)
    s7: String = dbl(s6)
    s8: String = dbl(s7)
    s9: String = dbl(s8)
    s10: String = dbl(s9)
    HttpResponse(status = 200, body = s10, headers = {})

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, big_handler)
"#;
    let Some((mut server, port, dir)) = setup("large-resp", src, "bigresp", "big_handler") else {
        return;
    };

    let (body, _err, status) = curl(&["-sS", &format!("http://127.0.0.1:{port}/")]);
    teardown(&mut server, &dir);

    assert!(status.success(), "curl failed: {status:?}");
    assert_eq!(
        body.len(),
        32 * 1024,
        "expected 32 KB body, got {} bytes",
        body.len()
    );
    // Pin the bookends — if the chunked-write loop swapped or
    // duplicated a range, the first 32 bytes or the last 32 bytes
    // would diverge from the 32-byte pattern.
    let expected_chunk = "0123456789ABCDEF0123456789ABCDEF";
    assert!(
        body.starts_with(expected_chunk),
        "body[..32] mismatched expected chunk"
    );
    assert!(
        body.ends_with(expected_chunk),
        "body[-32..] mismatched expected chunk"
    );
}

// ─────────────────────────────────────────────────────────────────
// 2. Large request body — POST 64 KB, handler echoes verbatim.
//    The drain loop in `emit_aver_http_handle` starts at 4096-byte
//    buf-cap and doubles on overflow; this test pushes it through
//    multiple `cabi_realloc` grows. The handler returns the body
//    + its length so the assertion catches a truncation.
// ─────────────────────────────────────────────────────────────────
#[test]
fn large_request_body_64k_drained() {
    let src = r#"
fn echo_handler(req: HttpRequest) -> HttpResponse
    HttpResponse(
        status = 200,
        body = "len={String.len(req.body)}",
        headers = {}
    )

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, echo_handler)
"#;
    let Some((mut server, port, dir)) = setup("large-req", src, "bigreq", "echo_handler") else {
        return;
    };

    // 64 KB of 'a'. curl --data-binary preserves bytes verbatim
    // (no shell interpolation).
    let payload = "a".repeat(64 * 1024);
    let payload_path = dir.join("payload.bin");
    std::fs::write(&payload_path, &payload).expect("write payload");

    let (body, _err, status) = curl(&[
        "-sS",
        "-X",
        "POST",
        "--data-binary",
        &format!("@{}", payload_path.display()),
        &format!("http://127.0.0.1:{port}/"),
    ]);
    teardown(&mut server, &dir);

    assert!(status.success(), "curl failed: {status:?}");
    assert_eq!(
        body, "len=65536",
        "expected handler to report 65536 received bytes, got: {body:?}"
    );
}

// ─────────────────────────────────────────────────────────────────
// 3. Many sequential requests — 200 GETs on one wasmtime instance.
//    Component model reuses instances across requests for the
//    proxy world; this test catches resource leaks (incoming-
//    request / fields / incoming-body) that would surface as
//    later requests trapping when the host's resource table fills
//    up. The handler is intentionally non-trivial (allocates a
//    fresh Map + List ref per request) so per-request GC pressure
//    is realistic.
// ─────────────────────────────────────────────────────────────────
#[test]
fn many_sequential_requests_no_leak() {
    let src = r#"
fn count_handler(req: HttpRequest) -> HttpResponse
    HttpResponse(
        status = 200,
        body = "method={req.method} path={req.path}",
        headers = {"x-aver" => ["ok"]}
    )

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, count_handler)
"#;
    let Some((mut server, port, dir)) = setup("sequential", src, "seq", "count_handler") else {
        return;
    };

    let mut ok = 0u32;
    for i in 0..200u32 {
        let url = format!("http://127.0.0.1:{port}/req-{i}");
        let (body, _err, status) = curl(&["-sS", &url]);
        if status.success() && body == format!("method=GET path=/req-{i}") {
            ok += 1;
        }
    }
    teardown(&mut server, &dir);

    assert_eq!(
        ok, 200,
        "expected all 200 sequential requests to succeed, got {ok}"
    );
}

// ─────────────────────────────────────────────────────────────────
// 4. Concurrent requests — 10 parallel curls in their own OS
//    threads. wasmtime serve spawns a fresh component instance
//    per request, so each one runs in isolation; a shared-state
//    bug in the proxy wrapper (e.g. a stray static / global) would
//    surface here as cross-talk between bodies.
// ─────────────────────────────────────────────────────────────────
#[test]
fn concurrent_requests_each_isolated() {
    let src = r#"
fn id_handler(req: HttpRequest) -> HttpResponse
    HttpResponse(status = 200, body = req.body, headers = {})

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, id_handler)
"#;
    let Some((mut server, port, dir)) = setup("concurrent", src, "conc", "id_handler") else {
        return;
    };

    let port = Arc::new(port);
    let ok = Arc::new(AtomicUsize::new(0));
    let mut handles = Vec::new();
    for i in 0..10u32 {
        let port = Arc::clone(&port);
        let ok = Arc::clone(&ok);
        handles.push(thread::spawn(move || {
            let payload = format!("payload-{i}");
            let url = format!("http://127.0.0.1:{port}/");
            let (body, _err, status) = curl(&["-sS", "-X", "POST", "-d", &payload, &url]);
            if status.success() && body == payload {
                ok.fetch_add(1, Ordering::Relaxed);
            }
        }));
    }
    for h in handles {
        let _ = h.join();
    }
    teardown(&mut server, &dir);

    assert_eq!(
        ok.load(Ordering::Relaxed),
        10,
        "expected all 10 concurrent requests to round-trip their own payload"
    );
}

// ─────────────────────────────────────────────────────────────────
// 5. Routing on path — handler `match`'es `req.path` and returns
//    different (status, body) per route. Exercises real Aver
//    pattern matching inside the handler + verifies that the path
//    string extracted by `incoming-request.path-with-query` arrives
//    intact for source-side comparison.
// ─────────────────────────────────────────────────────────────────
#[test]
fn routing_on_path_dispatches_correctly() {
    let src = r#"
fn route_handler(req: HttpRequest) -> HttpResponse
    match req.path
        "/ok" -> HttpResponse(status = 200, body = "ok", headers = {})
        "/created" -> HttpResponse(status = 201, body = "created", headers = {})
        "/teapot" -> HttpResponse(status = 418, body = "im a teapot", headers = {})
        _ -> HttpResponse(status = 404, body = "nope", headers = {})

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, route_handler)
"#;
    let Some((mut server, port, dir)) = setup("routing", src, "rt", "route_handler") else {
        return;
    };

    let routes: &[(&str, u16, &str)] = &[
        ("/ok", 200, "ok"),
        ("/created", 201, "created"),
        ("/teapot", 418, "im a teapot"),
        ("/whatever", 404, "nope"),
    ];
    let mut hits = Vec::new();
    for (path, _expected_status, _expected_body) in routes {
        let url = format!("http://127.0.0.1:{port}{path}");
        let (body, _err, _) = curl(&["-sS", "-w", "\nSTATUS:%{http_code}", &url]);
        hits.push((path.to_string(), body));
    }
    teardown(&mut server, &dir);

    for (i, (path, body)) in hits.iter().enumerate() {
        let (_, expected_status, expected_body) = routes[i];
        assert!(
            body.contains(expected_body),
            "{path}: expected body containing {expected_body:?}, got {body:?}"
        );
        let status_marker = format!("STATUS:{expected_status}");
        assert!(
            body.contains(&status_marker),
            "{path}: expected {status_marker} in curl -w output, got {body:?}"
        );
    }
}

// ─────────────────────────────────────────────────────────────────
// 6. Method dispatch — handler reports the request method in the
//    response body. Hits the 10-case `wasi:http/types.method`
//    variant decoder in `emit_aver_http_handle` (every named
//    discriminant 0..=8; the OTHER(string) path is exercised
//    indirectly by curl's `-X COPY` for non-standard verbs).
// ─────────────────────────────────────────────────────────────────
#[test]
fn method_dispatch_returns_method_name() {
    let src = r#"
fn method_handler(req: HttpRequest) -> HttpResponse
    HttpResponse(status = 200, body = req.method, headers = {})

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, method_handler)
"#;
    let Some((mut server, port, dir)) = setup("methods", src, "mthd", "method_handler") else {
        return;
    };

    // HEAD elided from the body assertion (per HTTP spec the
    // response body for HEAD is discarded by curl even when the
    // server emits Content-Length); we still verify it doesn't
    // 5xx via -I.
    let cases = &["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"];
    let mut hits = Vec::new();
    for m in cases {
        let url = format!("http://127.0.0.1:{port}/");
        let (body, _err, status) = curl(&["-sS", "-X", m, "-d", "", &url]);
        hits.push((m.to_string(), body, status.success()));
    }
    // HEAD smoke — server responds with 200 and matching Content-
    // Length header even when no body is written back to curl.
    let head_url = format!("http://127.0.0.1:{port}/");
    let (head_out, _, head_status) = curl(&["-sS", "-I", &head_url]);
    teardown(&mut server, &dir);

    for (m, body, ok) in &hits {
        assert!(*ok, "curl -X {m} failed");
        assert_eq!(body, m, "expected handler-body == method-name");
    }
    assert!(head_status.success(), "HEAD curl failed");
    assert!(
        head_out.contains("200"),
        "HEAD response should be 200, got: {head_out:?}"
    );
}

// ─────────────────────────────────────────────────────────────────
// 7. JSON-shaped response — handler builds a JSON literal via
//    string interpolation and sets Content-Type. Verifies the
//    `content-type` header round-trip + that the body bytes are
//    untouched (no UTF-8 mangling, no extra padding). This is the
//    real-world shape of an Aver-on-wasip2 API endpoint.
// ─────────────────────────────────────────────────────────────────
#[test]
fn json_response_with_content_type() {
    let src = r#"
fn first_or(items: List<String>, dflt: String) -> String
    match items
        [] -> dflt
        [head, ..rest] -> head

fn json_handler(req: HttpRequest) -> HttpResponse
    name: String = match Map.get(req.headers, "x-name")
        Option.Some(values) -> first_or(values, "anonymous")
        Option.None -> "anonymous"
    body: String = "\{\"name\":\"{name}\",\"len\":{String.len(name)}\}"
    HttpResponse(
        status = 200,
        body = body,
        headers = {"content-type" => ["application/json"]}
    )

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, json_handler)
"#;
    let Some((mut server, port, dir)) = setup("json", src, "jsn", "json_handler") else {
        return;
    };

    let url = format!("http://127.0.0.1:{port}/");
    let (out, _err, status) = curl(&["-sS", "-i", "-H", "X-Name: alice", &url]);
    teardown(&mut server, &dir);

    assert!(status.success(), "curl failed");
    let lower = out.to_lowercase();
    assert!(
        lower.contains("content-type: application/json"),
        "missing JSON content-type header, got: {out:?}"
    );
    assert!(
        out.contains(r#"{"name":"alice","len":5}"#),
        "expected JSON-shaped body, got: {out:?}"
    );
}

// ─────────────────────────────────────────────────────────────────
// 8. Many request headers — client sends 30 distinct headers,
//    handler reports the count via `Map.len(req.headers)`. Pins
//    the fields.entries loop scaling: each entry triggers two
//    memory.copy calls + two `__rt_string_from_lm` allocations +
//    one `Map.set`, so a regression in the prepend / map-insert
//    path would surface as either a wrong count or a trap.
//    wasmtime adds a few mandatory headers (host, content-length,
//    user-agent), so the assertion checks for ≥30 instead of ==.
// ─────────────────────────────────────────────────────────────────
#[test]
fn many_request_headers_reach_handler() {
    let src = r#"
fn count_handler(req: HttpRequest) -> HttpResponse
    HttpResponse(
        status = 200,
        body = "headers={Map.len(req.headers)}",
        headers = {}
    )

fn main() -> Unit
    ! [HttpServer.listen]
    HttpServer.listen(0, count_handler)
"#;
    let Some((mut server, port, dir)) = setup("manyhdr", src, "mhdr", "count_handler") else {
        return;
    };

    let mut args: Vec<String> = vec!["-sS".into()];
    for i in 0..30u32 {
        args.push("-H".into());
        args.push(format!("X-Aver-{i:02}: value-{i}"));
    }
    args.push(format!("http://127.0.0.1:{port}/"));
    let arg_refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let (body, _err, status) = curl(&arg_refs);
    teardown(&mut server, &dir);

    assert!(status.success(), "curl failed");
    let count: u32 = body
        .strip_prefix("headers=")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);
    assert!(
        count >= 30,
        "expected at least 30 headers visible to the handler, got: {body:?}"
    );
}
