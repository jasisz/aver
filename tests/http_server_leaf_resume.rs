//! Starting an HTTP server from a frameless leaf must resume in the leaf.
//!
//! `CALL_BUILTIN`'s `is_http_server()` branch (`src/vm/execute/dispatch.rs`)
//! parks the running chunk's `ip` in `self.frames.last_mut()` across the server
//! call and reads `(fn_id, ip, bp)` back from `self.frames.last()` afterwards.
//! It does that because the server runs request handlers through a nested
//! `call_function`, which pushes and pops frames of its own.
//!
//! `self.frames.last()` is only this chunk's frame when this chunk HAS one. A
//! chunk entered through `CALL_LEAF` has none — its caller's position lives in
//! the interpreter-local `leaf_return` — so `frames.last()` is the CALLER's
//! frame. Parking there overwrote the caller's saved `ip` with the leaf's, and
//! reading it back resumed the CALLER's function at the LEAF's offset, with the
//! caller's base pointer and a `leaf_return` still holding a live return
//! address.
//!
//! `classify_leaf_chunk` (`src/vm/compiler/classify.rs`) does not disqualify
//! `CALL_BUILTIN`, so a body whose only call is `HttpServer.listen` is a leaf,
//! and `local_count == arity` holds as long as it binds no name — which is how
//! a one-line `serve` helper gets written. Before the fix the witness below
//! panicked in the dispatch loop with `index out of bounds: the len is 4 but
//! the index is 126`: `main`'s bytecode indexed at the leaf's offset.
//!
//! Both programs run under `--record`, which is the one mode where
//! `HttpServer.listen` returns instead of blocking (`dispatch_http_server`
//! passes `skip = execution_mode() == Record`, and `services::http_server`
//! answers `Unit` without binding a socket). That is a real user-facing mode,
//! not a test seam: `aver run --record` is how a server program's effects get
//! captured for replay.

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver-http-leaf-{prefix}-{nanos}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    fs::write(&path, source).expect("write temp module source");
    path
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

/// Run under `--record`, and keep only the program's own output — the
/// `Recording saved: <path>` line the CLI prints last is not part of it.
fn run(prefix: &str, source: &str, extra: &[&str]) -> String {
    let path = temp_module(prefix, source);
    let module_root = path.parent().expect("temp module has parent").to_path_buf();
    let record_dir = module_root.join("recording");
    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(&module_root)
        .arg("run")
        .arg(&path)
        .arg("--module-root")
        .arg(&module_root)
        .arg("--record")
        .arg(&record_dir)
        .args(extra)
        .output()
        .expect("expected `aver run` to execute");
    let rendered = format_output(&out);
    let succeeded = out.status.success();
    let stdout = String::from_utf8_lossy(&out.stdout)
        .lines()
        .filter(|line| !line.starts_with("Recording saved:"))
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string();
    let _ = fs::remove_dir_all(&module_root);
    assert!(
        succeeded,
        "{prefix}: `aver run --record {extra:?}` exited non-zero — starting the \
         server did not return to the chunk that started it:\n{rendered}"
    );
    stdout
}

/// The VM and the self-hosted interpreter (the semantics oracle) must agree,
/// and both must equal `expected`.
fn assert_backends_agree(prefix: &str, source: &str, expected: &str) {
    let vm = run(prefix, source, &[]);
    assert_eq!(
        vm, expected,
        "{prefix}: the VM did not resume where it left off around the server call"
    );
    let self_host = run(&format!("{prefix}-sh"), source, &["--self-host"]);
    assert_eq!(
        self_host, vm,
        "{prefix}: the VM disagrees with the self-hosted interpreter"
    );
}

const HANDLER: &str = r#"fn handleRequest(req: HttpRequest) -> HttpResponse
    ? "Answers every request the same way."
    HttpResponse(status = 200, body = "ok", headers = {"content-type" => ["text/plain"]})
"#;

/// The witness. `serve` calls no user function and binds no name, so its
/// caller's `CALL_KNOWN` is upgraded to the frameless `CALL_LEAF` and `serve`
/// runs without a `CallFrame`. `main` has to get its own position back.
#[test]
fn starting_a_server_from_a_frameless_leaf_returns_to_the_leaf() {
    let src = format!(
        r#"module HttpServerLeaf
    intent = "starting the server from a body that owns no frame"
    depends []
    effects [Console, HttpServer]

{HANDLER}
fn serve(port: Int) -> Unit
    ? "No user call and no binding, so this chunk is a frameless leaf."
    ! [HttpServer.listen]
    HttpServer.listen(port, handleRequest)

fn main() -> Unit
    ! [Console.print, HttpServer.listen]
    Console.print("before")
    serve(8080)
    Console.print("reached the end")
"#
    );
    assert_backends_agree("leaf", &src, "before\nreached the end");
}

/// Control: the same program with one local bound first. That pushes
/// `local_count` past `arity`, blocks the `CALL_LEAF` upgrade, and gives the
/// chunk a real `CallFrame` — the path that was always correct. It has to keep
/// answering exactly as the frameless spelling does.
#[test]
fn starting_a_server_from_a_framed_body_is_unchanged() {
    let src = format!(
        r#"module HttpServerFramed
    intent = "the same server start from a body that does own a frame"
    depends []
    effects [Console, HttpServer]

{HANDLER}
fn serve(port: Int) -> Unit
    ? "Binds a local first, so this chunk keeps a frame of its own."
    ! [HttpServer.listen]
    p = port
    HttpServer.listen(p, handleRequest)

fn main() -> Unit
    ! [Console.print, HttpServer.listen]
    Console.print("before")
    serve(8080)
    Console.print("reached the end")
"#
    );
    assert_backends_agree("framed", &src, "before\nreached the end");
}

/// A frameless leaf with bytecode still to run after the server call — the
/// spelling `examples/apps/notepad/app.av` uses, `Result.Ok(HttpServer.listen(
/// ...))`. The wrap has to happen in the leaf, and `RETURN` has to find the
/// `leaf_return` the server branch must not have disturbed.
///
/// (A leaf body is a single expression: a second statement binds the first
/// one's value, which pushes `local_count` past `arity` and gives the chunk a
/// frame. Wrapping the call is how a leaf does anything after it.)
#[test]
fn a_frameless_leaf_wraps_the_server_result_and_returns_it() {
    let src = format!(
        r#"module HttpServerLeafWrap
    intent = "a frameless leaf wraps the outcome of starting the server"
    depends []
    effects [Console, HttpServer]

{HANDLER}
fn serve(port: Int) -> Result<Unit, String>
    ? "Starts the server and wraps the outcome, with no binding of its own."
    ! [HttpServer.listen]
    Result.Ok(HttpServer.listen(port, handleRequest))

fn main() -> Unit
    ! [Console.print, HttpServer.listen]
    match serve(8080)
        Result.Ok(_) -> Console.print("started")
        Result.Err(_) -> Console.print("failed")
    Console.print("reached the end")
"#
    );
    assert_backends_agree("leaf_wrap", &src, "started\nreached the end");
}
