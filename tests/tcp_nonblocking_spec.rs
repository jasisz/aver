//! Real-socket conformance tests for the non-blocking `Tcp` primitives:
//! `readNow`, `writeNow`, and `poll` on a `Tcp.Socket.Sending` key.
//!
//! Every test drives one Aver program against a loopback peer owned by the
//! test. The program prints one summary line; the bytecode VM and the hosted
//! wasm-gc runtime must print the same line, because both bind the same
//! `aver-rt` provider.

use std::io::{BufRead, BufReader, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::path::PathBuf;
use std::sync::mpsc;
use std::thread;

use aver::codegen::ModuleInfo;
use aver::ir::{NeutralAllocPolicy, PipelineConfig, TypecheckMode};
use aver::nan_value::NanValueConvert;
use aver::value::Value;
use aver::vm;

fn temp_root(tag: &str) -> PathBuf {
    let nonce = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("clock after epoch")
        .as_nanos();
    let root = std::env::temp_dir().join(format!(
        "aver-tcp-nonblocking-{tag}-{}-{nonce}",
        std::process::id()
    ));
    std::fs::create_dir_all(&root).expect("create fixture root");
    root
}

/// Run `main` on the bytecode VM and return what it printed.
fn run_vm(tag: &str, source: &str) -> String {
    let root = temp_root(tag);
    std::fs::write(root.join("main.av"), source).expect("write entry");
    let mut items = aver::source::parse_source(source).expect("parse entry source");
    let mut depends = items
        .iter()
        .find_map(|item| match item {
            aver::ast::TopLevel::Module(module) => Some(module.depends.clone()),
            _ => None,
        })
        .unwrap_or_default();
    depends.extend(aver::stdlib::implicit_stdlib_deps(&items));
    depends.sort();
    depends.dedup();
    let root_text = root.to_str().expect("UTF-8 fixture root");
    let loaded = aver::source::load_module_tree(&depends, root_text).expect("load capabilities");
    let modules: Vec<ModuleInfo> = loaded.iter().map(ModuleInfo::from_loaded).collect();
    let pipeline = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(root_text),
            }),
            dep_modules: &modules,
            ..Default::default()
        },
    );
    let typecheck = pipeline.typecheck.expect("typecheck result");
    assert!(
        typecheck.errors.is_empty(),
        "type errors: {:?}",
        typecheck.errors
    );
    let mut arena = aver::nan_value::Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_modules(
        &pipeline.resolved_items,
        &pipeline.symbol_table,
        &mut arena,
        Some(root_text),
        "main.av",
        pipeline.analysis.as_ref(),
    )
    .expect("compile program");
    let mut machine = vm::VM::new(code, globals, arena);
    let (outcome, stdout, _stderr) = aver::services::console::capture_output(|| machine.run());
    let value = outcome.expect("program run").to_value(&machine.arena);
    assert_eq!(value, Value::Unit, "main must return Unit");
    let _ = std::fs::remove_dir_all(root);
    String::from_utf8_lossy(&stdout).into_owned()
}

/// Run `main` through the in-process wasm-gc runtime (the `aver run --wasm-gc`
/// pipeline) and return what it printed.
#[cfg(feature = "wasm")]
fn run_wasm_gc(source: &str) -> String {
    let mut lexer = aver::lexer::Lexer::new(source);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = aver::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    let prepared_deps = aver::source::load_compile_deps(&items, env!("CARGO_MANIFEST_DIR"))
        .expect("load standard modules");
    let dep_modules = prepared_deps.modules;
    let neutral_policy = NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithCheckedLoaded(&prepared_deps.loaded)),
            dep_modules: &dep_modules,
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: false,
            run_list_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        panic!("typecheck failed: {:?}", tc.errors);
    }
    let type_aliases = aver::codegen::wasm_gc::flatten_multimodule(
        &mut items,
        &dep_modules,
        &result
            .typecheck
            .as_ref()
            .expect("typecheck requested")
            .capabilities,
        aver::codegen::wasm_gc::CapabilityFunctionSurface::Runtime,
    );
    aver::ir::pipeline::resolve(&mut items);
    let (run_res, stdout, _stderr) = aver::services::console::capture_output(|| {
        aver::runtime::wasm_gc::run_in_process(
            &items,
            result.analysis.as_ref(),
            aver::runtime::wasm_gc::RunConfig {
                tcp_settings: aver_rt::tcp::TcpSettings::default(),
                mode: aver::runtime::wasm_gc::EffectMode::Normal,
                type_aliases,
                ..Default::default()
            },
        )
    });
    run_res.expect("hosted wasm-gc run");
    String::from_utf8_lossy(&stdout).into_owned()
}

fn read_now_program(port: u16) -> String {
    format!(
        r#"module ReadNowProbe
    intent = "observe non-blocking reads on one loopback connection"
    depends [Bytes]
    exposes [main]
    effects [Tcp.connect, Tcp.readNow, Tcp.writeLine, Tcp.poll, Tcp.close, Console.print]

fn awaitReadable(conn: Tcp.Connection) -> Result<List<Int>, String>
    ? "Wait up to five seconds for the peer's bytes."
    ! [Tcp.poll]
    Tcp.poll({{1 => Tcp.Socket.Connected(conn)}}, 5000)

fn describe(chunk: Option<Bytes>) -> String
    ? "Render one readNow outcome."
    match chunk
        Option.None -> "none"
        Option.Some(bytes) -> match Bytes.len(bytes)
            0 -> "eof"
            _ -> Bytes.toHex(bytes)

fn probe() -> Result<String, String>
    ? "Probe an idle socket, ask the peer for bytes, drain them, then observe EOF."
    ! [Tcp.connect, Tcp.readNow, Tcp.writeLine, Tcp.poll, Tcp.close]
    conn = Tcp.connect("127.0.0.1", {port})?
    idle = Tcp.readNow(conn, 64)?
    Tcp.writeLine(conn, "go")?
    _ready = awaitReadable(conn)?
    data = Tcp.readNow(conn, 64)?
    _again = awaitReadable(conn)?
    end = Tcp.readNow(conn, 64)?
    Tcp.close(conn)?
    Result.Ok("{{describe(idle)}}|{{describe(data)}}|{{describe(end)}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.readNow, Tcp.writeLine, Tcp.poll, Tcp.close, Console.print]
    match probe()
        Result.Ok(line) -> Console.print(line)
        Result.Err(error) -> Console.print("error: {{error}}")
"#
    )
}

/// The peer answers the client's "go" line with two bytes, half-closes, and
/// stays open until the client hangs up.
fn read_now_peer(listener: TcpListener) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let (stream, _) = listener.accept().expect("accept readNow client");
        let mut reader = BufReader::new(stream.try_clone().expect("clone peer stream"));
        let mut line = String::new();
        reader.read_line(&mut line).expect("read the go line");
        assert_eq!(line, "go\r\n");
        let mut writer = stream;
        writer.write_all(b"hi").expect("write two bytes");
        writer
            .shutdown(Shutdown::Write)
            .expect("half-close toward the client");
        let mut rest = Vec::new();
        reader
            .read_to_end(&mut rest)
            .expect("wait for the client to hang up");
    })
}

fn write_now_program(port: u16) -> String {
    format!(
        r#"module WriteNowProbe
    intent = "fill a loopback socket with non-blocking writes"
    depends [Bytes]
    exposes [main]
    effects [Tcp.connect, Tcp.writeNow, Tcp.poll, Tcp.close, Console.print]

fn payload() -> Result<Bytes, String>
    ? "Sixty-four kibibytes of the same octet."
    Bytes.fromList(List.fromVector(Vector.new(65536, 7)))

fn sendingReady(conn: Tcp.Connection, timeoutMs: Int) -> Result<List<Int>, String>
    ? "Ask whether the socket accepts a write."
    ! [Tcp.poll]
    Tcp.poll({{7 => Tcp.Socket.Sending(conn)}}, timeoutMs)

fn fill(conn: Tcp.Connection, chunk: Bytes, budget: Int) -> Result<String, String>
    ? "Write until the kernel refuses and the socket stays unwritable, or the budget runs out."
    ! [Tcp.writeNow, Tcp.poll]
    match budget
        0 -> Result.Ok("budget")
        _ -> match Tcp.writeNow(conn, chunk)?
            0 -> match sendingReady(conn, 50)?
                [] -> Result.Ok("settled")
                _ -> fill(conn, chunk, budget - 1)
            _ -> fill(conn, chunk, budget - 1)

fn probe() -> Result<String, String>
    ? "Write three bytes, confirm write readiness, then fill the socket."
    ! [Tcp.connect, Tcp.writeNow, Tcp.poll, Tcp.close]
    conn = Tcp.connect("127.0.0.1", {port})?
    first = Tcp.writeNow(conn, Bytes.fromList([1, 2, 3]))?
    ready = sendingReady(conn, 1000)?
    readiness = match List.len(ready)
        1 -> "sending"
        _ -> "quiet"
    outcome = fill(conn, payload()?, 4096)?
    Tcp.close(conn)?
    Result.Ok("{{first}}|{{readiness}}|{{outcome}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.writeNow, Tcp.poll, Tcp.close, Console.print]
    match probe()
        Result.Ok(line) -> Console.print(line)
        Result.Err(error) -> Console.print("error: {{error}}")
"#
    )
}

/// The peer accepts and then reads nothing until the test releases it, so the
/// client's kernel buffers fill up.
fn write_now_peer(listener: TcpListener, release: mpsc::Receiver<()>) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let (stream, _) = listener.accept().expect("accept writeNow client");
        release.recv().expect("hold the unread stream open");
        drop(stream);
    })
}

fn loopback_listener() -> (TcpListener, u16) {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener address").port();
    (listener, port)
}

fn assert_write_now_line(line: &str) {
    let parts: Vec<&str> = line.trim_end().split('|').collect();
    assert_eq!(parts.len(), 3, "unexpected summary: {line}");
    assert_eq!(
        parts[0], "3",
        "an idle socket accepts the whole payload: {line}"
    );
    assert_eq!(parts[1], "sending", "an idle socket is writable: {line}");
    assert!(
        parts[2] == "settled" || parts[2] == "budget",
        "filling must end with a refused write or exhaust its budget: {line}"
    );
}

#[test]
fn vm_read_now_reports_none_then_data_then_eof() {
    let (listener, port) = loopback_listener();
    let peer = read_now_peer(listener);
    let out = run_vm("read-now", &read_now_program(port));
    peer.join().expect("peer thread");
    assert_eq!(out, "none|6869|eof\n");
}

#[test]
fn vm_write_now_accepts_then_refuses_and_sending_polls_writable() {
    let (listener, port) = loopback_listener();
    let (release_tx, release_rx) = mpsc::channel();
    let peer = write_now_peer(listener, release_rx);
    let out = run_vm("write-now", &write_now_program(port));
    release_tx.send(()).expect("release peer");
    peer.join().expect("peer thread");
    assert_write_now_line(&out);
}

#[test]
fn vm_read_now_validation_does_not_poison_the_connection() {
    let (listener, port) = loopback_listener();
    let (release_tx, release_rx) = mpsc::channel();
    let peer = write_now_peer(listener, release_rx);
    let source = format!(
        r#"module ReadNowValidation
    intent = "invalid maxBytes is a catchable error that keeps the handle live"
    depends [Bytes]
    exposes [main]
    effects [Tcp.connect, Tcp.readNow, Tcp.close, Console.print]

fn probe() -> Result<String, String>
    ? "Reject a non-positive maximum, then use the same handle again."
    ! [Tcp.connect, Tcp.readNow, Tcp.close]
    conn = Tcp.connect("127.0.0.1", {port})?
    invalid = match Tcp.readNow(conn, 0)
        Result.Err(message) -> message
        Result.Ok(_) -> "accepted"
    stillOpen = match Tcp.readNow(conn, 8)?
        Option.None -> "none"
        Option.Some(_) -> "some"
    Tcp.close(conn)?
    Result.Ok("{{invalid}}|{{stillOpen}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.readNow, Tcp.close, Console.print]
    match probe()
        Result.Ok(line) -> Console.print(line)
        Result.Err(error) -> Console.print("error: {{error}}")
"#
    );
    let out = run_vm("read-now-validation", &source);
    release_tx.send(()).expect("release peer");
    peer.join().expect("peer thread");
    assert_eq!(out, "Tcp.readNow: maxBytes 0 must be positive|none\n");
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_read_now_reports_none_then_data_then_eof() {
    let (listener, port) = loopback_listener();
    let peer = read_now_peer(listener);
    let out = run_wasm_gc(&read_now_program(port));
    peer.join().expect("peer thread");
    assert_eq!(out, "none|6869|eof\n");
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_write_now_accepts_then_refuses_and_sending_polls_writable() {
    let (listener, port) = loopback_listener();
    let (release_tx, release_rx) = mpsc::channel();
    let peer = write_now_peer(listener, release_rx);
    let out = run_wasm_gc(&write_now_program(port));
    release_tx.send(()).expect("release peer");
    peer.join().expect("peer thread");
    assert_write_now_line(&out);
}

/// The Rust-level peer helpers must themselves be correct on this platform:
/// a plain blocking stream sees the two bytes the readNow peer sends.
#[test]
fn read_now_peer_sends_two_bytes_after_the_go_line() {
    let (listener, port) = loopback_listener();
    let peer = read_now_peer(listener);
    let mut client = TcpStream::connect(("127.0.0.1", port)).expect("connect to peer");
    client.write_all(b"go\r\n").expect("send go");
    let mut received = Vec::new();
    client.read_to_end(&mut received).expect("read to EOF");
    assert_eq!(received, b"hi");
    drop(client);
    peer.join().expect("peer thread");
}
