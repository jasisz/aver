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

/// Custom server that handles GET, HEAD, DELETE — each tags the
/// response body / headers with the method name so the Aver test
/// can verify which path the helper took. HEAD per HTTP spec
/// returns no body even though the Content-Length header reflects
/// what GET would return.
const ALL_METHODS_SCRIPT: &str = r#"
import http.server, socketserver, sys

class H(http.server.BaseHTTPRequestHandler):
    def log_message(self, *a, **k):
        pass
    def _respond(self, body):
        self.send_response(200)
        self.send_header('Content-Type', 'text/plain')
        self.send_header('X-Method', self.command)
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        if self.command != 'HEAD':
            self.wfile.write(body)
    def do_GET(self):
        self._respond(b'got-it')
    def do_HEAD(self):
        self._respond(b'got-it')
    def do_DELETE(self):
        self._respond(b'deleted')

socketserver.TCPServer.allow_reuse_address = True
with socketserver.TCPServer(('127.0.0.1', 0), H) as srv:
    sys.stdout.write(f'PORT:{srv.server_address[1]}\n')
    sys.stdout.flush()
    srv.serve_forever()
"#;

/// Custom server that emits multiple Set-Cookie headers + a
/// custom X-Order header to exercise the multi-value header path
/// (Map.get + List.prepend) and the field-name lowercasing.
const MULTI_VALUE_SCRIPT: &str = r#"
import http.server, socketserver, sys

class H(http.server.BaseHTTPRequestHandler):
    def log_message(self, *a, **k):
        pass
    def do_GET(self):
        self.send_response(200)
        self.send_header('Content-Type', 'text/plain')
        self.send_header('Set-Cookie', 'first=1')
        self.send_header('Set-Cookie', 'second=2')
        self.send_header('Set-Cookie', 'third=3')
        self.send_header('X-Order', 'alpha')
        self.send_header('X-Order', 'beta')
        self.end_headers()
        self.wfile.write(b'multi-value-test')

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

/// Spawn `python3 -c <script>` in `dir`, wait for the `PORT:N`
/// line on stdout, return `(child, port)`. Returns `None` when
/// `python3` is not available — tests skip in that case rather
/// than fail (CI environments without python should not block
/// PR merges).
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

    let Some((mut server, port)) = spawn_python_server(&dir, SERVER_SCRIPT) else {
        eprintln!("python3 unavailable — skipping wasip2_http test");
        return;
    };
    // Give the server a tick to start accept()'ing.
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn first_or(items: List<String>, dflt: String) -> String
    match items
        [] -> dflt
        [head, ..rest] -> head

fn header(headers: Map<String, List<String>>, name: String) -> String
    match Map.get(headers, name)
        Option.Some(values) -> first_or(values, "missing")
        Option.None -> "absent"

fn dump(r: HttpResponse) -> Unit
    ! [Console.print]
    ct: String = header(r.headers, "content-type")
    Console.print("status={{r.status}} body_len={{String.len(r.body)}} headers_len={{Map.len(r.headers)}} ct={{ct}}")

fn main() -> Unit
    ! [Http.get, Console.print]
    response = Http.get("http://127.0.0.1:{port}/")
    match response
        Result.Ok(r) -> dump(r)
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
    // Headers map populated — Python http.server always emits at
    // least content-type, content-length, server, date (wasi-http
    // normalises field names to lowercase). Asserting a positive
    // count + the content-type lookup hitting "text/html" proves
    // both incoming-response.headers + fields.entries + Map.set
    // wiring round-trips correctly.
    assert!(
        s.contains("headers_len=") && !s.contains("headers_len=0"),
        "expected non-zero headers count, got:\n{s}"
    );
    assert!(
        s.contains("ct=text/html"),
        "expected ct=text/html (Python http.server default for .html), got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn http_get_preserves_multi_value_header_order() {
    // Custom Python server emits three Set-Cookie + two X-Order
    // headers in a specific order. The Aver helper iterates the
    // wasi-http entries list in REVERSE and prepends each value
    // onto the existing List<String>, which yields forward order
    // in the final map (matches RFC 6265's "preserve emit order"
    // requirement for Set-Cookie).
    let dir = tempdir("multi");
    let Some((mut server, port)) = spawn_python_server(&dir, MULTI_VALUE_SCRIPT) else {
        eprintln!("python3 unavailable — skipping multi-value header test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn list_join(items: List<String>, sep: String) -> String
    match items
        [] -> ""
        [head, ..rest] -> match rest
            [] -> head
            [_, ..__] -> "{{head}}{{sep}}{{list_join(rest, sep)}}"

fn header_join(headers: Map<String, List<String>>, name: String) -> String
    match Map.get(headers, name)
        Option.Some(values) -> list_join(values, ",")
        Option.None -> "absent"

fn dump(r: HttpResponse) -> Unit
    ! [Console.print]
    cookies: String = header_join(r.headers, "set-cookie")
    order: String = header_join(r.headers, "x-order")
    Console.print("status={{r.status}} cookies={{cookies}} order={{order}}")

fn main() -> Unit
    ! [Http.get, Console.print]
    response = Http.get("http://127.0.0.1:{port}/")
    match response
        Result.Ok(r) -> dump(r)
        Result.Err(e) -> Console.print("err: {{e}}")
"#
    );

    let fixture = write_fixture(&dir, "multi.av", &src);
    let out = run_wasip2(&dir, &fixture, &[]);

    let _ = server.kill();
    let _ = server.wait();

    assert!(
        out.status.success(),
        "wasip2_http multi-value compile/run failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(s.contains("status=200"), "expected status=200, got:\n{s}");
    // Cookies emitted in order first/second/third — the wasm
    // helper's reverse-iterate-prepend scheme should yield this
    // exact comma-joined string. Any reorder (e.g. forward-iter
    // prepend → reversed) or loss (overwrite-on-duplicate) would
    // fail this assertion.
    assert!(
        s.contains("cookies=first=1,second=2,third=3"),
        "expected cookies=first=1,second=2,third=3 (Set-Cookie multi-value order), got:\n{s}"
    );
    assert!(
        s.contains("order=alpha,beta"),
        "expected order=alpha,beta (X-Order multi-value order), got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn http_head_and_delete_dispatch_correct_method() {
    // Custom server tags the response with the request method
    // via X-Method header. Aver fires GET/HEAD/DELETE in turn
    // and asserts the server saw each verb. HEAD also asserts
    // an empty body (server skips wfile.write on HEAD per HTTP).
    let dir = tempdir("methods");
    let Some((mut server, port)) = spawn_python_server(&dir, ALL_METHODS_SCRIPT) else {
        eprintln!("python3 unavailable — skipping HEAD/DELETE test");
        return;
    };
    std::thread::sleep(Duration::from_millis(100));

    let src = format!(
        r#"
fn first_or(items: List<String>, dflt: String) -> String
    match items
        [] -> dflt
        [head, ..rest] -> head

fn header(headers: Map<String, List<String>>, name: String) -> String
    match Map.get(headers, name)
        Option.Some(values) -> first_or(values, "missing")
        Option.None -> "absent"

fn dump(label: String, r: HttpResponse) -> Unit
    ! [Console.print]
    method: String = header(r.headers, "x-method")
    Console.print("{{label}} status={{r.status}} method={{method}} body_len={{String.len(r.body)}}")

fn main() -> Unit
    ! [Http.get, Http.head, Http.delete, Console.print]
    g = Http.get("http://127.0.0.1:{port}/")
    h = Http.head("http://127.0.0.1:{port}/")
    d = Http.delete("http://127.0.0.1:{port}/")
    match g
        Result.Ok(r) -> dump("GET", r)
        Result.Err(e) -> Console.print("GET err: {{e}}")
    match h
        Result.Ok(r) -> dump("HEAD", r)
        Result.Err(e) -> Console.print("HEAD err: {{e}}")
    match d
        Result.Ok(r) -> dump("DELETE", r)
        Result.Err(e) -> Console.print("DELETE err: {{e}}")
"#
    );
    let fixture = write_fixture(&dir, "methods.av", &src);
    let out = run_wasip2(&dir, &fixture, &[]);

    let _ = server.kill();
    let _ = server.wait();

    assert!(
        out.status.success(),
        "wasip2_http HEAD/DELETE compile/run failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        s.contains("GET status=200 method=GET body_len=6"),
        "expected GET line with method=GET and body_len=6 (\"got-it\"), got:\n{s}"
    );
    // HEAD: server still sets Content-Length but writes no body —
    // wasi-http surfaces the empty body as body_len=0.
    assert!(
        s.contains("HEAD status=200 method=HEAD body_len=0"),
        "expected HEAD line with method=HEAD and empty body, got:\n{s}"
    );
    assert!(
        s.contains("DELETE status=200 method=DELETE body_len=7"),
        "expected DELETE line with method=DELETE and body_len=7 (\"deleted\"), got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn http_get_surfaces_connection_refused() {
    // Bind to port 0, immediately close — gives us a port number
    // that's unlikely to be in use. The kernel's TCP backoff means
    // a connect() to that port within ~10s reliably gets RST →
    // wasi-http returns error-code::connection-refused (disc 6),
    // and our error-code dispatcher should surface the variant
    // name verbatim.
    let dir = tempdir("refused");
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind");
    let port = listener.local_addr().expect("local_addr").port();
    drop(listener);

    let src = format!(
        r#"
fn main() -> Unit
    ! [Http.get, Console.print]
    response = Http.get("http://127.0.0.1:{port}/")
    match response
        Result.Ok(r) -> Console.print("status={{r.status}}")
        Result.Err(e) -> Console.print("err={{e}}")
"#
    );
    let fixture = write_fixture(&dir, "refused.av", &src);
    let out = run_wasip2(&dir, &fixture, &[]);
    assert!(
        out.status.success(),
        "wasip2_http connection-refused compile/run failed (exit {:?})\nstdout:\n{}\nstderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let s = String::from_utf8_lossy(&out.stdout).into_owned();
    // The wasi-http impl might surface the failure at handle()
    // (connection-failed retptr tag) OR at future.get's inner
    // result Err arm (error-code::connection-refused). Accept
    // either path — both are "host couldn't reach the server".
    let acceptable = s.contains("err=http: connection-refused")
        || s.contains("err=http: connection failed")
        || s.contains("err=http: connection-terminated");
    assert!(
        acceptable,
        "expected a connection-refused/failed Err message from a port nobody listens on, got:\n{s}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
