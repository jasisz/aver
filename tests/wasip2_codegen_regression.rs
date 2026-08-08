//! wasip2 codegen regression suite.
//!
//! Sister to `tests/wasm_gc_codegen_regression.rs`. For every
//! single-file `examples/**/*.av` program (no `depends [...]`,
//! no `examples/diagnostics/` typecheck-broken examples, no
//! `oracle_independent_products.av` `BranchPath` carrier — same
//! exclusions as the wasm-gc regression), drive the full frontend
//! → IR pipeline → `compile_to_wasm_gc_for_wasip2` →
//! `compile_to_component` → component-model validation.
//!
//! Catches regressions in:
//!   - the wasm-gc emitter's `TargetMode::Wasip2` arm (canonical
//!     ABI shape, distinct from the bridge target wasm-gc uses)
//!   - the wit-component wrap pass that adapts the core module
//!     into a `wasi:cli/command` component
//!   - the wasi-package bundle / WIT generator output
//!
//! Skips programs the wasm-gc regression already skips; if a
//! program's bridge-mode codegen panics the wasip2 mode won't get
//! a chance to fail differently, so the two suites share a corpus.

#![cfg(feature = "wasip2")]

use std::fs;
use std::path::{Path, PathBuf};

use aver::ast::TopLevel;
use aver::ir;
use aver::lexer::Lexer;
use aver::parser::Parser;

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples")
}

/// Paths skipped from the regression walk:
///   * `examples/diagnostics/` — same exclusions as
///     `tests/wasm_gc_codegen_regression.rs`.
///   * Three `Terminal.*` users — `status_board.av`,
///     `oracle_trace.av`, `terminal_size_snapshot.av`. The wasip2
///     backend lowered 10 effects in 0.18 "Span" (all of `Disk`,
///     `Time.sleep`, `Console.readLine`) but `Terminal.moveTo` /
///     `Terminal.readKey` / `Terminal.size` are not yet wired —
///     codegen errors with "no helper registered, no effect
///     import, no inline lowering". Tracked as a follow-up; this
///     suite is the regression net, the wiring is its own ticket.
///
/// `oracle_independent_products.av` used to be skipped (its
/// higher-order `Fn(BranchPath, …)` spec param was mis-discovered as a
/// phantom `Tuple<BranchPath, …>` carrier that the eq helper couldn't
/// dispatch). That discovery bug is fixed, so it rides the walk like
/// every other single-file example.
fn is_skipped(path: &Path) -> bool {
    let s = path.to_string_lossy();
    s.contains("/examples/diagnostics/")
        || s.ends_with("/examples/apps/status_board.av")
        || s.ends_with("/examples/formal/oracle_trace.av")
        || s.ends_with("/examples/formal/terminal_size_snapshot.av")
}

fn single_file_examples() -> Vec<PathBuf> {
    let mut out = Vec::new();
    walk(&examples_dir(), &mut out);
    out.sort();
    out
}

fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(read) = fs::read_dir(dir) else { return };
    for entry in read.flatten() {
        let path = entry.path();
        if path.is_dir() {
            walk(&path, out);
        } else if path.extension().and_then(|s| s.to_str()) == Some("av") && !is_skipped(&path) {
            let Ok(text) = fs::read_to_string(&path) else {
                continue;
            };
            if !text
                .lines()
                .any(|ln| ln.trim_start().starts_with("depends ["))
            {
                out.push(path);
            }
        }
    }
}

fn parse_pipeline(source: &str) -> Result<Vec<TopLevel>, String> {
    parse_pipeline_with_module_root(source, None).map(|(items, _)| items)
}

/// Returns the flattened items PLUS the identity-preserving qualified
/// type-name aliases `flatten_multimodule` derived — harnesses that
/// flatten multi-module input must thread the real alias map into the
/// compile so they exercise the same path `aver compile` takes.
fn parse_pipeline_with_module_root(
    source: &str,
    module_root: Option<&str>,
) -> Result<(Vec<TopLevel>, std::collections::HashMap<String, String>), String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| format!("lex: {:?}", e))?;
    let mut parser = Parser::new(tokens);
    let mut items = parser.parse().map_err(|e| format!("parse: {:?}", e))?;
    // Mirror what `aver compile --target wasm-gc` does internally:
    // skip the VM-specific `run_interp_lower` + `run_buffer_build`
    // passes (they emit `__buf_*` / `__interp_*` calls the wasm-gc
    // codegen doesn't link against). The wasip2 wrap pass sits on
    // top of the same wasm-gc emitter, so this matches.
    let result = ir::pipeline::run(
        &mut items,
        ir::PipelineConfig {
            typecheck: Some(ir::TypecheckMode::Full {
                base_dir: module_root,
            }),
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        return Err(format!(
            "typecheck: {} error(s) — first: {:?}",
            tc.errors.len(),
            tc.errors.first()
        ));
    }
    let mut type_aliases = std::collections::HashMap::new();
    if let Some(root) = module_root {
        let dep_modules = aver::source::load_compile_deps(&items, root)?;
        type_aliases = aver::codegen::wasm_gc::flatten_multimodule(&mut items, &dep_modules);
        aver::ir::pipeline::resolve(&mut items);
    }
    Ok((items, type_aliases))
}

/// Compile the wasip2 core module through the flattened entry with the
/// real alias map, mirroring the CLI's multi-module `--target wasip2`
/// path.
fn compile_core_flattened(
    items: &[TopLevel],
    type_aliases: &std::collections::HashMap<String, String>,
) -> Result<Vec<u8>, aver::codegen::wasm_gc::WasmGcError> {
    aver::codegen::wasm_gc::compile_to_wasm_gc_flattened(
        items,
        None,
        None,
        aver::codegen::wasm_gc::TargetMode::Wasip2,
        type_aliases,
    )
    .map(|out| out.bytes)
}

#[test]
fn tcp_send_bytes_compiles_and_validates_as_component() {
    let source = r#"module Probe
    intent = "Compile the byte-clean TCP request path."
    depends [Bytes]
    exposes [main]
    effects [Tcp.sendBytes]

fn main() -> Result<Bytes, String>
    ? "Send and receive bytes without UTF-8 conversion."
    ! [Tcp.sendBytes]
    payload = Bytes.fromList([249, 190, 180, 217])
    Tcp.sendBytes("127.0.0.1", 9, payload)
"#;
    let (items, type_aliases) =
        parse_pipeline_with_module_root(source, Some(env!("CARGO_MANIFEST_DIR")))
            .unwrap_or_else(|e| panic!("{e}\n--- source ---\n{source}"));
    let core_bytes = compile_core_flattened(&items, &type_aliases)
        .unwrap_or_else(|e| panic!("wasip2 core compile: {e}\n--- source ---\n{source}"));
    let (component_bytes, _) = aver::codegen::wasip2::compile_to_component(
        &core_bytes,
        aver::codegen::wasip2::Wasip2World::CliCommand,
    )
    .unwrap_or_else(|e| panic!("wasip2 component wrap: {e}\n--- source ---\n{source}"));
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::default())
        .validate_all(&component_bytes)
        .unwrap_or_else(|e| panic!("component validate: {e}\n--- source ---\n{source}"));
}

#[test]
fn tcp_read_bytes_compiles_and_validates_as_component() {
    let source = r#"module Probe
    intent = "Compile exact-length binary reads."
    depends [Bytes]
    exposes [read]
    effects [Tcp.readBytes]

fn read(conn: Tcp.Connection, count: Int) -> Result<Bytes, String>
    ? "Read one length-delimited binary frame."
    ! [Tcp.readBytes]
    Tcp.readBytes(conn, count)
"#;
    let (items, type_aliases) =
        parse_pipeline_with_module_root(source, Some(env!("CARGO_MANIFEST_DIR")))
            .unwrap_or_else(|e| panic!("{e}\n--- source ---\n{source}"));
    let core_bytes = compile_core_flattened(&items, &type_aliases)
        .unwrap_or_else(|e| panic!("wasip2 core compile: {e}\n--- source ---\n{source}"));
    let (component_bytes, _) = aver::codegen::wasip2::compile_to_component(
        &core_bytes,
        aver::codegen::wasip2::Wasip2World::CliCommand,
    )
    .unwrap_or_else(|e| panic!("wasip2 component wrap: {e}\n--- source ---\n{source}"));
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::default())
        .validate_all(&component_bytes)
        .unwrap_or_else(|e| panic!("component validate: {e}\n--- source ---\n{source}"));
}

#[test]
fn tcp_write_bytes_compiles_and_validates_as_component() {
    let source = r#"module Probe
    intent = "Compile exact binary writes on a persistent connection."
    depends [Bytes]
    exposes [write]
    effects [Tcp.writeBytes]

fn write(conn: Tcp.Connection, payload: Bytes) -> Result<Unit, String>
    ? "Write one binary frame without encoding or framing."
    ! [Tcp.writeBytes]
    Tcp.writeBytes(conn, payload)
"#;
    let (items, type_aliases) =
        parse_pipeline_with_module_root(source, Some(env!("CARGO_MANIFEST_DIR")))
            .unwrap_or_else(|e| panic!("{e}\n--- source ---\n{source}"));
    let core_bytes = compile_core_flattened(&items, &type_aliases)
        .unwrap_or_else(|e| panic!("wasip2 core compile: {e}\n--- source ---\n{source}"));
    let (component_bytes, _) = aver::codegen::wasip2::compile_to_component(
        &core_bytes,
        aver::codegen::wasip2::Wasip2World::CliCommand,
    )
    .unwrap_or_else(|e| panic!("wasip2 component wrap: {e}\n--- source ---\n{source}"));
    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::default())
        .validate_all(&component_bytes)
        .unwrap_or_else(|e| panic!("component validate: {e}\n--- source ---\n{source}"));
}

#[test]
fn wasip2_codegen_emits_valid_component_for_every_single_file_example() {
    let files = single_file_examples();
    assert!(
        !files.is_empty(),
        "no single-file examples found under examples/ — did the corpus move?"
    );

    let mut failures: Vec<String> = Vec::new();
    let mut compiled = 0usize;

    for path in &files {
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                failures.push(format!("{}: read: {}", path.display(), e));
                continue;
            }
        };
        let items = match parse_pipeline(&source) {
            Ok(i) => i,
            Err(e) => {
                failures.push(format!("{}: {}", path.display(), e));
                continue;
            }
        };
        let core_bytes = match aver::codegen::wasm_gc::compile_to_wasm_gc_for_wasip2(&items, None) {
            Ok(b) => b,
            Err(e) => {
                failures.push(format!(
                    "{}: compile_to_wasm_gc_for_wasip2: {}",
                    path.display(),
                    e
                ));
                continue;
            }
        };
        let component_bytes = match aver::codegen::wasip2::compile_to_component(
            &core_bytes,
            aver::codegen::wasip2::Wasip2World::CliCommand,
        ) {
            Ok((bytes, _wit)) => bytes,
            Err(e) => {
                failures.push(format!("{}: compile_to_component: {}", path.display(), e));
                continue;
            }
        };
        // Component validation needs the component-model feature
        // explicitly enabled — `WasmFeatures::default()` enables it,
        // but spell it out so a future wasmparser default flip
        // doesn't silently turn this into a core-module check.
        let mut validator =
            wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::default());
        if let Err(e) = validator.validate_all(&component_bytes) {
            failures.push(format!(
                "{}: component validate ({} bytes): {}",
                path.display(),
                component_bytes.len(),
                e
            ));
            continue;
        }
        compiled += 1;
    }

    if !failures.is_empty() {
        panic!(
            "{} of {} single-file examples failed wasip2 codegen + component validate:\n  - {}",
            failures.len(),
            files.len(),
            failures.join("\n  - ")
        );
    }
    eprintln!(
        "wasip2_codegen_emits_valid_component_for_every_single_file_example: {} files compiled + validated",
        compiled
    );
}

/// Map every passive data segment in a core module to its index, and
/// collect the `array.new_data` data-segment references of every code
/// body (one Vec per body, in code-section order). Helper for the
/// TCP helper-gating and readBytes-classification regressions below.
fn segments_and_body_data_refs(bytes: &[u8]) -> (Vec<Vec<u8>>, Vec<Vec<u32>>) {
    use wasmparser::{Operator, Parser as WasmParser, Payload};

    let mut segments: Vec<Vec<u8>> = Vec::new();
    let mut body_refs: Vec<Vec<u32>> = Vec::new();
    for payload in WasmParser::new(0).parse_all(bytes) {
        match payload.expect("generated module must parse") {
            Payload::DataSection(reader) => {
                for data in reader {
                    segments.push(data.expect("data segment").data.to_vec());
                }
            }
            Payload::CodeSectionEntry(body) => {
                let mut refs = Vec::new();
                let mut ops = body.get_operators_reader().expect("operators");
                while !ops.eof() {
                    if let Operator::ArrayNewData {
                        array_data_index, ..
                    } = ops.read().expect("operator")
                    {
                        refs.push(array_data_index);
                    }
                }
                body_refs.push(refs);
            }
            _ => {}
        }
    }
    (segments, body_refs)
}

fn segment_idx(segments: &[Vec<u8>], text: &[u8]) -> u32 {
    segments
        .iter()
        .position(|seg| seg == text)
        .unwrap_or_else(|| {
            panic!(
                "data segment {:?} must be present",
                String::from_utf8_lossy(text)
            )
        }) as u32
}

/// TCP helper allocation is gated on declared effects, not on
/// coincidental registry / wasi-import availability. This program
/// declares `Tcp.writeLine` (plus connect/close and the Console pair
/// whose imports made the leak possible): `Console.readLine` registers
/// the `input-stream.blocking-read` import and interns
/// `Result<String,String>`, which used to be enough for the unused
/// `__rt_tcp_read_line` and `__rt_tcp_send` helper bodies to ride
/// along as dead module bytes.
#[test]
fn wasip2_tcp_helpers_absent_without_their_declared_effects() {
    let source = r#"module Probe
    intent = "Persistent line-oriented TCP client."
    exposes [main]
    effects [Tcp.connect, Tcp.writeLine, Tcp.close, Console.readLine, Console.print]

fn chat(conn: Tcp.Connection, line: String) -> Result<Unit, String>
    ? "Write one line, then close the connection."
    ! [Tcp.writeLine, Tcp.close]
    _w = Tcp.writeLine(conn, line)?
    Tcp.close(conn)

fn main() -> Unit
    ! [Tcp.connect, Tcp.writeLine, Tcp.close, Console.readLine, Console.print]
    match Console.readLine()
        Result.Ok(line) -> match Tcp.connect("127.0.0.1", 4242)
            Result.Ok(conn) -> match chat(conn, line)
                Result.Ok(_) -> Console.print("sent")
                Result.Err(e) -> Console.print("tcp error: {e}")
            Result.Err(e) -> Console.print("connect error: {e}")
        Result.Err(e) -> Console.print("stdin error: {e}")
"#;
    let items = parse_pipeline(source).unwrap_or_else(|e| panic!("{e}\n--- source ---\n{source}"));
    let bytes = aver::codegen::wasm_gc::compile_to_wasm_gc_for_wasip2(&items, None)
        .unwrap_or_else(|e| panic!("wasip2 core compile: {e}\n--- source ---\n{source}"));

    let (segments, body_refs) = segments_and_body_data_refs(&bytes);
    let referenced = |seg: u32| -> bool { body_refs.iter().any(|refs| refs.contains(&seg)) };

    // Present: the declared effects' own helpers keep their bodies.
    let write_failed = segment_idx(&segments, b"tcp: write failed");
    let connect_limit = segment_idx(&segments, b"tcp: connection limit reached (256 max)");
    assert!(
        referenced(write_failed),
        "Tcp.writeLine is declared — its helper body must reference \"tcp: write failed\""
    );
    assert!(
        referenced(connect_limit),
        "Tcp.connect is declared — its helper body must reference the pool-limit segment"
    );

    // Absent: no declared effect needs these helpers, so no body may
    // reference their distinctive segments. "tcp: eof" is only used
    // by `__rt_tcp_read_line`; the 10 MiB response cap is only used
    // by `__rt_tcp_send` / `__rt_tcp_send_bytes`.
    let eof = segment_idx(&segments, b"tcp: eof");
    assert!(
        !referenced(eof),
        "Tcp.readLine is not declared — the read_line helper must not be emitted"
    );
    let response_cap = segment_idx(&segments, b"tcp: response exceeds 10 MiB limit");
    assert!(
        !referenced(response_cap),
        "Tcp.send / Tcp.sendBytes are not declared — their helpers must not be emitted"
    );
}

/// `Tcp.readBytes` count-error classification on native wasip2 must
/// match the VM (`src/services/tcp.rs` `count_arg` +
/// `aver_rt::tcp::read_bytes`), branch by branch:
///
/// 1. count outside i64 (either sign) → "exceeds the read limit"
/// 2. in-i64 count < 0 → "is negative"
/// 3. in-i64 count > 10485760 → "exceeds the 10485760 byte limit"
///
/// Residual difference: the VM interpolates the offending count
/// (`"Tcp.readBytes: count -1 is negative"`), while the wasip2 helper
/// builds its Err strings from static data segments and cannot splice
/// the number in — so the wasip2 texts are the VM texts minus the
/// `count N` value. The error class (catchable Result.Err) is
/// identical everywhere.
#[test]
fn wasip2_tcp_read_bytes_count_error_classification_matches_vm() {
    let source = r#"module Probe
    intent = "Compile exact-length binary reads."
    depends [Bytes]
    exposes [read]
    effects [Tcp.readBytes]

fn read(conn: Tcp.Connection, count: Int) -> Result<Bytes, String>
    ? "Read one length-delimited binary frame."
    ! [Tcp.readBytes]
    Tcp.readBytes(conn, count)
"#;
    let (items, _aliases) =
        parse_pipeline_with_module_root(source, Some(env!("CARGO_MANIFEST_DIR")))
            .unwrap_or_else(|e| panic!("{e}\n--- source ---\n{source}"));
    let bytes = aver::codegen::wasm_gc::compile_to_wasm_gc_for_wasip2(&items, None)
        .unwrap_or_else(|e| panic!("wasip2 core compile: {e}\n--- source ---\n{source}"));

    let (segments, body_refs) = segments_and_body_data_refs(&bytes);
    let read_limit = segment_idx(&segments, b"Tcp.readBytes: count exceeds the read limit");
    let negative = segment_idx(&segments, b"Tcp.readBytes: count is negative");
    let byte_limit = segment_idx(
        &segments,
        b"Tcp.readBytes: count exceeds the 10485760 byte limit",
    );

    // The read_bytes helper is the only body referencing the
    // "is negative" segment. Its first three Err materializations
    // appear in check order, pinning the branch classification:
    // out-of-i64 first (read limit), then sign, then the byte limit.
    let helper_refs = body_refs
        .iter()
        .find(|refs| refs.contains(&negative))
        .expect("a body referencing the readBytes negative-count segment must exist");
    assert_eq!(
        &helper_refs[..3],
        &[read_limit, negative, byte_limit],
        "readBytes count checks must classify: out-of-i64 -> read limit, \
         in-i64 negative -> is negative, in-i64 over-limit -> 10485760 byte limit"
    );
}
