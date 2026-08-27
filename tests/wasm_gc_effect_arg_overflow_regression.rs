//! Regression: under `Int = ℤ`, an out-of-i64 `Int` passed to a fallible
//! HOST EFFECT must remain a catchable `Result.Err` on wasm-gc, matching the
//! VM.
//!
//! The bug (confirmed in wasmtime on the bignum default-flip branch): an
//! out-of-i64 Big Int passed as an i64-typed effect argument silently
//! SATURATED on wasm-gc and proceeded, where the VM raises a runtime error.
//! `effect_int_arg_positions` lowered the Int args of host effects via
//! `__aint_to_i64_sat` (`2^63 -> i64::MAX`) BEFORE the host call, for
//! `Random.int` bounds, `Time.sleep` ms, and `Tcp.*` ports
//! bind ports, and `Terminal.moveTo` coordinates. The VM's host services do
//! a CHECKED `to_i64()` and ERROR instead (e.g. `Random.int: bounds must
//! fit a 64-bit integer`). Worst case: `Time.sleep(2^63)` saturated to
//! i64::MAX ms (a ~292-million-year hang) where the VM errors.
//!
//! `Random.int` and `Time.sleep` now import boxed Aver Ints and return the
//! same `Result` carrier as every other backend. Their provider validates the
//! host range before doing any work, so the error is observable Aver data —
//! not a wasm trap and, critically, not a saturated host call. The saturating
//! path stays in place for pure builtins where saturation matches the VM
//! (`String.charAt`/`slice` indices, `List.take`/`drop` counts,
//! `String.fromCodePoint`).

#![cfg(feature = "wasm")]

use aver::ir::{NeutralAllocPolicy, PipelineConfig, TypecheckMode};

/// Parse + typecheck + run a program through the in-process wasm-gc runtime,
/// mirroring the CLI's `aver run --wasm-gc` pipeline (`try_run_wasm_gc` in
/// `src/main/run_wasm_gc.rs`): the neutral alloc policy + the `analysis`
/// facts must be threaded into `run_in_process`, otherwise codegen takes a
/// different (analysis-less) path. Returns `Ok` with captured stdout on a
/// clean run, or `Err(message)` when wasm execution traps / the backend
/// rejects.
fn run_wasm_gc(source: &str) -> Result<String, String> {
    run_wasm_gc_with_mode(source, aver::runtime::wasm_gc::EffectMode::Normal)
        .map(|(stdout, _)| stdout)
}

fn run_wasm_gc_with_mode(
    source: &str,
    mode: aver::runtime::wasm_gc::EffectMode,
) -> Result<(String, aver::runtime::wasm_gc::RunOutcome), String> {
    let mut lexer = aver::lexer::Lexer::new(source);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = aver::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    let prepared_deps = aver::source::load_compile_deps(&items, env!("CARGO_MANIFEST_DIR"))?;
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
        let rendered = tc
            .errors
            .iter()
            .map(|e| format!("  [{}] {}", e.line, e.message))
            .collect::<Vec<_>>()
            .join("\n");
        panic!(
            "typecheck failed — `aver run --wasm-gc` rejects this program before codegen, so the \
             harness must not run it either:\n{rendered}"
        );
    }
    // This harness flattens multi-module input, so it must thread the
    // REAL alias map the flattener derived — the same production path
    // `src/main/run_wasm_gc.rs` takes.
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
                mode,
                type_aliases: type_aliases.clone(),
                ..Default::default()
            },
        )
    });

    run_res
        .map(|outcome| (String::from_utf8_lossy(&stdout).into_owned(), outcome))
        .map_err(|e| e.to_string())
}

/// `9223372036854775807 + 1` builds `2^63` (the first value past i64::MAX)
/// by arithmetic, so the `$AverInt` carrier holds a Big. Passing it as the
/// `Random.int` upper bound must return the same catchable error as the VM.
#[test]
fn out_of_i64_random_bound_is_catchable_on_wasm_gc() {
    let src = r#"module M
    intent =
        "out-of-i64 Random.int bound rejects"
    effects [Random, Console]

fn main() -> Unit
    ! [Random.int, Console.print]
    big = 9223372036854775807 + 1
    match Random.int(1, big)
        Result.Ok(n) -> Console.print("n = {n}")
        Result.Err(e) -> Console.print(e)
"#;
    let out = run_wasm_gc(src).expect("Random.int validation must be catchable Aver data");
    assert_eq!(out, "Random.int: bounds must fit a 64-bit integer\n");
}

/// `Time.sleep(2^63)` must return `Result.Err`, not saturate to i64::MAX ms
/// (a ~292-million-year hang). A clean, fast, catchable error is the guard.
#[test]
fn out_of_i64_sleep_ms_is_catchable_on_wasm_gc() {
    let src = r#"module M
    intent =
        "out-of-i64 Time.sleep ms rejects (no 292-million-year hang)"
    effects [Time, Console]

fn main() -> Unit
    ! [Time.sleep, Console.print]
    big = 9223372036854775807 + 1
    match Time.sleep(big)
        Result.Ok(_) -> Console.print("woke up")
        Result.Err(e) -> Console.print(e)
"#;
    let out = run_wasm_gc(src).expect("Time.sleep validation must be catchable Aver data");
    assert_eq!(out, "Time.sleep: ms must fit a 64-bit integer\n");
}

/// IN-RANGE effect args STILL WORK: a small `Random.int` bound and a small
/// `Time.sleep` lower fine through the checked helper (a Small passes its
/// `$small` through), so the program runs to completion and prints.
#[test]
fn in_range_effect_args_still_run_on_wasm_gc() {
    let src = r#"module M
    intent =
        "in-range effect args still run"
    effects [Random, Time, Console]

fn main() -> Unit
    ! [Random.int, Time.sleep, Console.print]
    n = Random.int(1, 6)
    _ = Time.sleep(1)
    Console.print("done")
"#;
    let out = run_wasm_gc(src).expect("in-range effect args must run cleanly on wasm-gc");
    assert_eq!(
        out, "done\n",
        "a normal program with small Random.int / Time.sleep args must run to completion; \
         got stdout {out:?}"
    );
}

#[test]
fn bytes_from_list_rejects_an_out_of_range_octet_on_wasm_gc() {
    let src = r#"module M
    intent =
        "Bytes validates octets before Tcp.sendBytes can be called"
    depends [Bytes]
    effects [Console]

fn main() -> Unit
    ! [Console.print]
    match Bytes.fromList([65, 256])
        Result.Ok(_) -> Console.print("unexpected valid Bytes")
        Result.Err(e) -> Console.print(e)
"#;
    let out = run_wasm_gc(src).expect("Bytes range failure must be a catchable Result.Err");
    assert_eq!(out, "byte 256 at index 1 is outside 0..=255\n");
}

#[test]
fn bytes_from_list_rejects_a_bigint_octet_on_wasm_gc() {
    let src = r#"module M
    intent =
        "Bytes rejects arbitrary-precision integers outside octet range"
    depends [Bytes]
    effects [Console]

fn main() -> Unit
    ! [Console.print]
    match Bytes.fromList([65, 1208925819614629174706176])
        Result.Ok(_) -> Console.print("unexpected valid Bytes")
        Result.Err(e) -> Console.print(e)
"#;
    let out = run_wasm_gc(src).expect("big Bytes range failure must be a catchable Result.Err");
    assert_eq!(
        out,
        "byte 1208925819614629174706176 at index 1 is outside 0..=255\n"
    );
}

#[test]
fn tcp_send_bytes_round_trips_nominal_bytes_on_wasm_gc() {
    use std::io::{Read, Write};
    use std::net::TcpListener;

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener address").port();
    let server = std::thread::spawn(move || {
        let (mut stream, _) = listener.accept().expect("accept Tcp.sendBytes");
        let mut payload = Vec::new();
        stream.read_to_end(&mut payload).expect("read payload");
        stream.write_all(&payload).expect("echo payload");
        payload
    });

    let src = format!(
        r#"module M
    intent = "Round-trip nominal Bytes through the hosted wasm-gc TCP bridge."
    depends [Bytes]
    effects [Tcp, Console]

fn main() -> Unit
    ! [Tcp.sendBytes, Console.print]
    match Tcp.sendBytes("127.0.0.1", {port}, Bytes.fromList([249, 190, 180, 217]))
        Result.Err(e) -> Console.print("err: {{e}}")
        Result.Ok(response) -> Console.print("{{Bytes.octets(response) == [249, 190, 180, 217]}}")
"#
    );
    let (out, recorded) = run_wasm_gc_with_mode(&src, aver::runtime::wasm_gc::EffectMode::Record)
        .expect("hosted wasm-gc Tcp.sendBytes round-trip");
    assert_eq!(server.join().expect("echo server"), [249, 190, 180, 217]);
    assert_eq!(out, "true\n");

    let effects = recorded
        .recorded_effects
        .expect("record mode must return the effect trace");
    let recording = aver::replay::SessionRecording {
        schema_version: 1,
        request_id: "tcp-send-bytes-test".to_string(),
        timestamp: String::new(),
        program_file: String::new(),
        module_root: String::new(),
        entry_fn: "main".to_string(),
        input: aver::replay::JsonValue::Null,
        capabilities: Vec::new(),
        effects,
        output: aver::replay::RecordedOutcome::Value(recorded.output),
    };
    let (replay_stdout, replayed) = run_wasm_gc_with_mode(
        &src,
        aver::runtime::wasm_gc::EffectMode::Replay(Box::new(recording), true),
    )
    .expect("recorded nominal Bytes response must decode during replay");
    assert!(
        replay_stdout.is_empty(),
        "replay must suppress Console.print"
    );
    assert_eq!(replayed.effects_consumed, replayed.effects_total);
}

#[test]
fn tcp_read_bytes_records_and_replays_nominal_bytes_on_wasm_gc() {
    use std::io::Write;
    use std::net::TcpListener;

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener address").port();
    let server = std::thread::spawn(move || {
        let (mut stream, _) = listener.accept().expect("accept Tcp.readBytes");
        stream
            .write_all(&[249, 190, 180, 217])
            .expect("write binary frame");
    });

    let src = format!(
        r#"module M
    intent = "Read nominal Bytes through the hosted wasm-gc TCP bridge."
    depends [Bytes]
    effects [Tcp, Console]

fn readFrame(conn: Tcp.Connection) -> Unit
    ! [Tcp.readBytes, Console.print]
    match Tcp.readBytes(conn, 4)
        Result.Err(e) -> Console.print("err: {{e}}")
        Result.Ok(frame) -> Console.print("{{Bytes.octets(frame) == [249, 190, 180, 217]}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.readBytes, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Err(e) -> Console.print("connect err: {{e}}")
        Result.Ok(conn) -> readFrame(conn)
"#
    );
    let (out, recorded) = run_wasm_gc_with_mode(&src, aver::runtime::wasm_gc::EffectMode::Record)
        .expect("hosted wasm-gc Tcp.readBytes round-trip");
    server.join().expect("binary frame server");
    assert_eq!(out, "true\n");

    let effects = recorded
        .recorded_effects
        .expect("record mode must return the effect trace");
    let recording = aver::replay::SessionRecording {
        schema_version: 1,
        request_id: "tcp-read-bytes-test".to_string(),
        timestamp: String::new(),
        program_file: String::new(),
        module_root: String::new(),
        entry_fn: "main".to_string(),
        input: aver::replay::JsonValue::Null,
        capabilities: Vec::new(),
        effects,
        output: aver::replay::RecordedOutcome::Value(recorded.output),
    };
    let (replay_stdout, replayed) = run_wasm_gc_with_mode(
        &src,
        aver::runtime::wasm_gc::EffectMode::Replay(Box::new(recording), true),
    )
    .expect("recorded Tcp.readBytes response must decode during replay");
    assert!(
        replay_stdout.is_empty(),
        "replay must suppress Console.print"
    );
    assert_eq!(replayed.effects_consumed, replayed.effects_total);
}

#[test]
fn tcp_poll_then_read_some_records_and_replays_caller_ids_on_wasm_gc() {
    use std::io::Write;
    use std::net::TcpListener;

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener address").port();
    let server = std::thread::spawn(move || {
        let (mut stream, _) = listener.accept().expect("accept Tcp.poll client");
        stream
            .write_all(&[249, 190, 180, 217])
            .expect("write readable chunk");
    });

    let src = format!(
        r#"module M
    intent = "Poll caller-owned peer IDs and consume one bounded binary chunk."
    depends [Bytes]
    effects [Tcp, Console]

fn report(conn: Tcp.Connection, ready: List<Int>, chunk: Bytes) -> Unit
    ! [Tcp.close, Console.print]
    Tcp.close(conn)
    Console.print("{{ready == [1208925819614629174706176]}}:{{Bytes.octets(chunk) == [249, 190, 180, 217]}}")

fn pollAndRead(conn: Tcp.Connection, sockets: Map<Int, Tcp.Socket>) -> Unit
    ! [Tcp.poll, Tcp.readSome, Tcp.close, Console.print]
    match Tcp.poll(sockets, 1000)
        Result.Err(e) -> Console.print("poll err: {{e}}")
        Result.Ok(ready) -> match Tcp.readSome(conn, 64)
            Result.Err(e) -> Console.print("read err: {{e}}")
            Result.Ok(chunk) -> report(conn, ready, chunk)

fn main() -> Unit
    ! [Tcp.connect, Tcp.poll, Tcp.readSome, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Err(e) -> Console.print("connect err: {{e}}")
        Result.Ok(conn) -> pollAndRead(conn, {{1208925819614629174706176 => Tcp.Socket.Connected(conn)}})
"#
    );
    let (out, recorded) = run_wasm_gc_with_mode(&src, aver::runtime::wasm_gc::EffectMode::Record)
        .expect("hosted wasm-gc Tcp.poll/readSome round-trip");
    server.join().expect("readable chunk server");
    assert_eq!(out, "true:true\n");

    let effects = recorded
        .recorded_effects
        .expect("record mode must return the effect trace");
    let recording = aver::replay::SessionRecording {
        schema_version: 1,
        request_id: "tcp-poll-read-some-test".to_string(),
        timestamp: String::new(),
        program_file: String::new(),
        module_root: String::new(),
        entry_fn: "main".to_string(),
        input: aver::replay::JsonValue::Null,
        capabilities: Vec::new(),
        effects,
        output: aver::replay::RecordedOutcome::Value(recorded.output),
    };
    let (replay_stdout, replayed) = run_wasm_gc_with_mode(
        &src,
        aver::runtime::wasm_gc::EffectMode::Replay(Box::new(recording), true),
    )
    .expect("recorded Tcp.poll/readSome results must decode during replay");
    assert!(
        replay_stdout.is_empty(),
        "replay must suppress Console.print"
    );
    assert_eq!(replayed.effects_consumed, replayed.effects_total);
}

#[test]
fn tcp_begin_connect_dialled_and_peer_address_record_and_replay_on_wasm_gc() {
    use std::net::TcpListener;

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback dial target");
    let port = listener.local_addr().expect("dial target address").port();
    let server = std::thread::spawn(move || {
        let (_stream, _) = listener.accept().expect("accept non-blocking Aver dial");
    });
    let src = format!(
        r#"module M
    intent = "Settle one non-blocking connection through the shared poll map."
    effects [Tcp, Console]

fn awaitDial(dial: Tcp.Dial) -> Result<Tcp.Connection, String>
    ? "Poll until the dial is either connected or rejected."
    ! [Tcp.poll, Tcp.dialled]
    _ = Tcp.poll({{9 => Tcp.Socket.Dialing(dial)}}, 1000)?
    match Tcp.dialled(dial)?
        Option.None -> awaitDial(dial)
        Option.Some(connection) -> Result.Ok(connection)

fn finish(connection: Tcp.Connection, address: String) -> Unit
    ! [Tcp.close, Console.print]
    _ = Tcp.close(connection)
    Console.print("connected:{{String.len(address) > 0}}")

fn main() -> Unit
    ! [Tcp.beginConnect, Tcp.poll, Tcp.dialled, Tcp.peerAddress, Tcp.close, Console.print]
    match Tcp.beginConnect("127.0.0.1", {port})
        Result.Err(error) -> Console.print("begin err: {{error}}")
        Result.Ok(dial) -> match awaitDial(dial)
            Result.Err(error) -> Console.print("dial err: {{error}}")
            Result.Ok(connection) -> match Tcp.peerAddress(connection)
                Result.Err(error) -> Console.print("peer err: {{error}}")
                Result.Ok(address) -> finish(connection, address)
"#
    );

    let (stdout, recorded) =
        run_wasm_gc_with_mode(&src, aver::runtime::wasm_gc::EffectMode::Record)
            .expect("wasm-gc non-blocking dial lifecycle");
    server.join().expect("dial target server");
    assert_eq!(stdout, "connected:true\n");

    let recording = aver::replay::SessionRecording {
        schema_version: 1,
        request_id: "tcp-dial-test".to_string(),
        timestamp: String::new(),
        program_file: String::new(),
        module_root: String::new(),
        entry_fn: "main".to_string(),
        input: aver::replay::JsonValue::Null,
        capabilities: Vec::new(),
        effects: recorded
            .recorded_effects
            .expect("record mode must return the dial trace"),
        output: aver::replay::RecordedOutcome::Value(recorded.output),
    };
    let (stdout, replayed) = run_wasm_gc_with_mode(
        &src,
        aver::runtime::wasm_gc::EffectMode::Replay(Box::new(recording), true),
    )
    .expect("replay the non-blocking dial lifecycle");
    assert!(stdout.is_empty());
    assert_eq!(replayed.effects_consumed, replayed.effects_total);
}

#[test]
fn tcp_listen_accept_and_close_listener_run_on_wasm_gc() {
    use std::io::Write;
    use std::net::{TcpListener, TcpStream};
    use std::time::{Duration, Instant};

    let reservation = TcpListener::bind("127.0.0.1:0").expect("reserve listener port");
    let port = reservation.local_addr().expect("reserved address").port();
    drop(reservation);

    let client = std::thread::spawn(move || {
        let deadline = Instant::now() + Duration::from_secs(3);
        loop {
            match TcpStream::connect(("127.0.0.1", port)) {
                Ok(mut stream) => {
                    stream.write_all(b"ready").expect("write accepted payload");
                    return;
                }
                Err(error) if Instant::now() < deadline => {
                    let _ = error;
                    std::thread::sleep(Duration::from_millis(5));
                }
                Err(error) => panic!("connect to Aver listener: {error}"),
            }
        }
    });

    let src = format!(
        r#"module M
    intent = "Accept one inbound connection through Tcp.Socket.Listening."
    effects [Tcp, Console]

fn awaitClient(listener: Tcp.Listener) -> Result<Tcp.Connection, String>
    ? "Poll and drain one listener backlog entry."
    ! [Tcp.poll, Tcp.accept]
    _ = Tcp.poll({{4 => Tcp.Socket.Listening(listener)}}, 1000)?
    match Tcp.accept(listener)?
        Option.None -> awaitClient(listener)
        Option.Some(connection) -> Result.Ok(connection)

fn accepted(listener: Tcp.Listener, connection: Tcp.Connection, address: String) -> Unit
    ! [Tcp.close, Tcp.closeListener, Console.print]
    closedConnection = Tcp.close(connection)
    closedListener = Tcp.closeListener(listener)
    Console.print("accepted:{{String.len(address) > 0}}")

fn finish(listener: Tcp.Listener, connection: Tcp.Connection) -> Unit
    ! [Tcp.peerAddress, Tcp.close, Tcp.closeListener, Console.print]
    match Tcp.peerAddress(connection)
        Result.Err(error) -> Console.print("peer err: {{error}}")
        Result.Ok(address) -> accepted(listener, connection, address)

fn main() -> Unit
    ! [Tcp.listen, Tcp.poll, Tcp.accept, Tcp.peerAddress, Tcp.close, Tcp.closeListener, Console.print]
    match Tcp.listen({port}, 16)
        Result.Err(error) -> Console.print("listen err: {{error}}")
        Result.Ok(listener) -> match awaitClient(listener)
            Result.Err(error) -> Console.print("accept err: {{error}}")
            Result.Ok(connection) -> finish(listener, connection)
"#
    );
    let stdout = run_wasm_gc(&src).expect("wasm-gc listener lifecycle");
    client.join().expect("loopback listener client");
    assert_eq!(stdout, "accepted:true\n");
}

#[test]
fn tcp_write_bytes_records_and_replays_nominal_bytes_on_wasm_gc() {
    use std::io::Read;
    use std::net::TcpListener;

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener address").port();
    let server = std::thread::spawn(move || {
        let (mut stream, _) = listener.accept().expect("accept Tcp.writeBytes");
        let mut payload = [0u8; 4];
        stream
            .read_exact(&mut payload)
            .expect("read exact binary frame");
        payload
    });

    let src = format!(
        r#"module M
    intent = "Write nominal Bytes through the hosted wasm-gc TCP bridge."
    depends [Bytes]
    effects [Tcp, Console]

fn writeFrame(conn: Tcp.Connection, payload: Bytes) -> Result<Unit, String>
    ? "Write one exact binary frame."
    ! [Tcp.writeBytes]
    Tcp.writeBytes(conn, payload)

fn main() -> Unit
    ! [Tcp.connect, Tcp.writeBytes, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Err(e) -> Console.print("connect err: {{e}}")
        Result.Ok(conn) -> match writeFrame(conn, Bytes.fromList([249, 190, 180, 217]))
            Result.Err(e) -> Console.print("write err: {{e}}")
            Result.Ok(_) -> match Tcp.close(conn)
                Result.Err(e) -> Console.print("close err: {{e}}")
                Result.Ok(_) -> Console.print("written")
"#
    );
    let (out, recorded) = run_wasm_gc_with_mode(&src, aver::runtime::wasm_gc::EffectMode::Record)
        .expect("hosted wasm-gc Tcp.writeBytes round-trip");
    assert_eq!(
        server.join().expect("binary sink server"),
        [249, 190, 180, 217]
    );
    assert_eq!(out, "written\n");

    let recording = aver::replay::SessionRecording {
        schema_version: 1,
        request_id: "tcp-write-bytes-test".to_string(),
        timestamp: String::new(),
        program_file: String::new(),
        module_root: String::new(),
        entry_fn: "main".to_string(),
        input: aver::replay::JsonValue::Null,
        capabilities: Vec::new(),
        effects: recorded
            .recorded_effects
            .expect("record mode must return the effect trace"),
        output: aver::replay::RecordedOutcome::Value(recorded.output),
    };
    let (replay_stdout, replayed) = run_wasm_gc_with_mode(
        &src,
        aver::runtime::wasm_gc::EffectMode::Replay(Box::new(recording), true),
    )
    .expect("recorded Tcp.writeBytes result must decode during replay");
    assert!(
        replay_stdout.is_empty(),
        "replay must suppress Console.print"
    );
    assert_eq!(replayed.effects_consumed, replayed.effects_total);
}

/// A count too large for i64 (2^80) must surface as a catchable
/// `Result.Err` on wasm-gc, not a trap. Uses a real loopback listener
/// because `Tcp.Connection` is a capability resource — Aver source cannot construct one
/// (the typechecker rejects `Tcp.Connection(id = ..., ...)`).
#[test]
fn tcp_read_bytes_big_count_is_catchable_on_wasm_gc() {
    use std::io::Read;
    use std::net::TcpListener;

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener address").port();
    let server = std::thread::spawn(move || {
        let (mut stream, _) = listener.accept().expect("accept Tcp.readBytes");
        // Hold the peer open until the guest closes: the oversized count
        // must fail on the count check, never by racing a dropped socket.
        let mut buf = [0u8; 1];
        let _ = stream.read(&mut buf);
    });

    let src = format!(
        r#"module M
    intent = "Reject an unbounded binary frame length without trapping."
    depends [Bytes]
    effects [Tcp, Console]

fn rejectBigCount(conn: Tcp.Connection) -> Unit
    ! [Tcp.readBytes, Tcp.close, Console.print]
    result = Tcp.readBytes(conn, 1208925819614629174706176)
    _ = Tcp.close(conn)
    match result
        Result.Ok(_) -> Console.print("unexpected-ok")
        Result.Err(_) -> Console.print("range-error")

fn main() -> Unit
    ! [Tcp.connect, Tcp.readBytes, Tcp.close, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Err(e) -> Console.print("connect err: {{e}}")
        Result.Ok(conn) -> rejectBigCount(conn)
"#
    );
    let out = run_wasm_gc(&src).expect("big read count must return Result.Err, not trap");
    server.join().expect("held-open listener");
    assert_eq!(out, "range-error\n");
}

/// The harness must reject every program `aver run --wasm-gc` rejects: a
/// knowingly ill-typed probe (constructing the opaque `Tcp.Connection`
/// outside its defining module — exactly the source the pre-fix version of
/// `tcp_read_bytes_big_count_is_catchable_on_wasm_gc` ran) must panic with
/// the rendered typecheck error instead of silently compiling and running.
/// Reverting the harness typecheck fix turns this test red.
#[test]
#[should_panic(expected = "typecheck failed")]
fn harness_panics_on_ill_typed_probe_source() {
    let src = r#"module M
    intent = "harness must respect typecheck errors"
    effects [Console]

fn main() -> Unit
    ! [Console.print]
    conn = Tcp.Connection(id = "missing", host = "", port = 0)
    Console.print("unreachable")
"#;
    let _ = run_wasm_gc(src);
}

/// PURE-builtin saturation is UNCHANGED: `String.charAt` with an out-of-i64
/// index past the string end must SATURATE to `Option.None` on wasm-gc (the
/// VM's clamp), NOT trap. This pins that the fix touched only the EFFECT-arg
/// boundary and left the saturating `__aint_to_i64_sat` path intact for the
/// pure builtins where saturation matches the VM.
#[test]
fn pure_builtin_out_of_i64_index_still_saturates_on_wasm_gc() {
    let src = r#"module M
    intent =
        "pure-builtin out-of-i64 index saturates to None (unchanged)"
    effects [Console]

fn main() -> Unit
    ! [Console.print]
    big = 9223372036854775807 + 1
    c = String.charAt("hello", big)
    match c
        Option.Some(ch) -> Console.print("got {ch}")
        Option.None -> Console.print("none")
"#;
    let out = run_wasm_gc(src)
        .expect("String.charAt with an out-of-range index must SATURATE to None, not trap");
    assert_eq!(
        out, "none\n",
        "an out-of-i64 String.charAt index must saturate past the string end to Option.None \
         (the VM's clamp), matching the pure-builtin saturating path; got stdout {out:?}"
    );
}
