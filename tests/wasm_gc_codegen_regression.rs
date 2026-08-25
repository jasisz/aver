//! wasm-gc codegen regression suite.
//!
//! For every single-file `examples/**/*.av` program (no explicit `depends`
//! clause), drive the full frontend → dependency loader → IR pipeline →
//! flattened wasm-gc codegen → `wasmparser::Validator` walk. Implicit
//! standard capability dependencies are therefore exercised exactly like
//! the CLI. Asserts: no panic, no compile error, no validator rejection.
//!
//! Catches:
//!   - codegen panics on real-world shapes (caught early, no AFL needed)
//!   - emitted wasm that wasmparser rejects (the codegen target
//!     fuzzes this for adversarial input; this suite pins it for
//!     vetted examples so a regression is obvious)
//!   - silent breakage from compiler refactors (a renamed pass, a
//!     dropped invariant) showing up as one of 40+ tests failing
//!     instead of an obscure end-to-end break

#![cfg(feature = "wasm-compile")]

use std::fs;
use std::path::{Path, PathBuf};

use aver::ast::TopLevel;
use aver::ir;
use aver::lexer::Lexer;
use aver::parser::Parser;

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples")
}

/// Recursively collect every `*.av` under `examples/` that does NOT
/// declare `depends [...]` — those need the multi-module loader
/// which lives in the CLI binary today.
fn single_file_examples() -> Vec<PathBuf> {
    let mut out = Vec::new();
    walk(&examples_dir(), &mut out);
    out.sort();
    out
}

/// Paths skipped from the regression walk:
///   * `examples/diagnostics/` — intentionally type-broken examples used
///     to demo error messages; they must not typecheck cleanly.
///
/// `examples/formal/oracle_independent_products.av` used to be skipped
/// because its `pairSpec` higher-order spec fn carries a
/// `Fn(BranchPath, …)` param whose param list was mis-discovered as a
/// phantom `Tuple<BranchPath, …>`, whose eq helper then tried to
/// dispatch on the opaque `BranchPath` carrier. That discovery bug is
/// fixed (a `Fn(`-preceded paren group is a function-type parameter
/// list, not a tuple), so the file now compiles + validates and rides
/// the walk like every other single-file example.
fn is_skipped(path: &Path) -> bool {
    let s = path.to_string_lossy();
    s.contains("/examples/diagnostics/")
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
    // codegen doesn't link against — it has its own deforestation
    // path). Same shape `vm_verify.rs` uses for wasm-gc verify.
    let result = ir::pipeline::run(
        &mut items,
        ir::PipelineConfig {
            typecheck: Some(ir::TypecheckMode::Full {
                base_dir: module_root,
            }),
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
        return Err(format!(
            "typecheck: {} error(s) — first: {:?}",
            tc.errors.len(),
            tc.errors.first()
        ));
    }
    let mut type_aliases = std::collections::HashMap::new();
    if let Some(root) = module_root {
        let dep_modules = aver::source::load_compile_deps(&items, root)?;
        type_aliases = aver::codegen::wasm_gc::flatten_multimodule(
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
    }
    Ok((items, type_aliases))
}

/// Compile through the flattened entry with the real alias map, mirroring
/// the CLI's multi-module `--target wasm-gc` path.
fn compile_flattened(
    items: &[TopLevel],
    type_aliases: &std::collections::HashMap<String, String>,
) -> Result<Vec<u8>, aver::codegen::wasm_gc::WasmGcError> {
    aver::codegen::wasm_gc::compile_to_wasm_gc_flattened(
        items,
        None,
        None,
        aver::codegen::wasm_gc::TargetMode::AverBridge,
        type_aliases,
    )
    .map(|out| out.bytes)
}

fn assert_compiles_and_validates(source: &str) {
    let items = parse_pipeline(source).unwrap_or_else(|e| panic!("{e}\n--- source ---\n{source}"));
    let bytes = aver::codegen::wasm_gc::compile_to_wasm_gc(&items, None)
        .unwrap_or_else(|e| panic!("wasm-gc compile: {e}\n--- source ---\n{source}"));
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .unwrap_or_else(|e| panic!("wasmparser validate: {e}\n--- source ---\n{source}"));
}

#[test]
fn tcp_send_bytes_imports_host_function_and_validates() {
    use wasmparser::{Parser as WasmParser, Payload};

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
    let bytes = compile_flattened(&items, &type_aliases)
        .unwrap_or_else(|e| panic!("wasm-gc compile: {e}\n--- source ---\n{source}"));
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .unwrap_or_else(|e| panic!("wasmparser validate: {e}\n--- source ---\n{source}"));

    let mut found = false;
    for payload in WasmParser::new(0).parse_all(&bytes) {
        if let Payload::ImportSection(reader) = payload.expect("generated module must parse") {
            found = reader.into_imports().flatten().any(|import| {
                import.module == "aver"
                    && import.name == "tcp_send_bytes"
                    && matches!(import.ty, wasmparser::TypeRef::Func(_))
            });
        }
    }
    assert!(
        found,
        "Tcp.sendBytes must lower to the aver.tcp_send_bytes host import"
    );
}

#[test]
fn tcp_read_bytes_imports_host_function_and_validates() {
    use wasmparser::{Parser as WasmParser, Payload};

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
    let bytes = compile_flattened(&items, &type_aliases)
        .unwrap_or_else(|e| panic!("wasm-gc compile: {e}\n--- source ---\n{source}"));
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .unwrap_or_else(|e| panic!("wasmparser validate: {e}\n--- source ---\n{source}"));

    let mut found = false;
    for payload in WasmParser::new(0).parse_all(&bytes) {
        if let Payload::ImportSection(reader) = payload.expect("generated module must parse") {
            found = reader.into_imports().flatten().any(|import| {
                import.module == "aver"
                    && import.name == "tcp_read_bytes"
                    && matches!(import.ty, wasmparser::TypeRef::Func(_))
            });
        }
    }
    assert!(
        found,
        "Tcp.readBytes must lower to the aver.tcp_read_bytes host import"
    );
}

#[test]
fn tcp_poll_and_read_some_import_host_functions_and_validate() {
    use wasmparser::{Parser as WasmParser, Payload};

    let source = r#"module Probe
    intent = "Compile readiness polling and bounded stream reads."
    depends [Bytes]
    exposes [ready, chunk]
    effects [Tcp.poll, Tcp.readSome]

fn ready(sockets: Map<Int, Tcp.Socket>, timeoutMs: Int) -> Result<List<Int>, String>
    ? "Return caller IDs whose sockets can make progress without waiting."
    ! [Tcp.poll]
    Tcp.poll(sockets, timeoutMs)

fn chunk(conn: Tcp.Connection, maxBytes: Int) -> Result<Bytes, String>
    ? "Read one available chunk without requiring the buffer to fill."
    ! [Tcp.readSome]
    Tcp.readSome(conn, maxBytes)
"#;
    let (items, type_aliases) =
        parse_pipeline_with_module_root(source, Some(env!("CARGO_MANIFEST_DIR")))
            .unwrap_or_else(|e| panic!("{e}\n--- source ---\n{source}"));
    let bytes = compile_flattened(&items, &type_aliases)
        .unwrap_or_else(|e| panic!("wasm-gc compile: {e}\n--- source ---\n{source}"));
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .unwrap_or_else(|e| panic!("wasmparser validate: {e}\n--- source ---\n{source}"));

    let mut poll_found = false;
    let mut read_some_found = false;
    for payload in WasmParser::new(0).parse_all(&bytes) {
        if let Payload::ImportSection(reader) = payload.expect("generated module must parse") {
            for import in reader.into_imports().flatten() {
                if import.module == "aver" && matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                    poll_found |= import.name == "tcp_poll";
                    read_some_found |= import.name == "tcp_read_some";
                }
            }
        }
    }
    assert!(
        poll_found && read_some_found,
        "Tcp.poll/Tcp.readSome must lower to aver.tcp_poll/aver.tcp_read_some host imports"
    );
}

#[test]
fn tcp_write_bytes_imports_host_function_and_validates() {
    use wasmparser::{Parser as WasmParser, Payload};

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
    let bytes = compile_flattened(&items, &type_aliases)
        .unwrap_or_else(|e| panic!("wasm-gc compile: {e}\n--- source ---\n{source}"));
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .unwrap_or_else(|e| panic!("wasmparser validate: {e}\n--- source ---\n{source}"));

    let mut found = false;
    for payload in WasmParser::new(0).parse_all(&bytes) {
        if let Payload::ImportSection(reader) = payload.expect("generated module must parse") {
            found = reader.into_imports().flatten().any(|import| {
                import.module == "aver"
                    && import.name == "tcp_write_bytes"
                    && matches!(import.ty, wasmparser::TypeRef::Func(_))
            });
        }
    }
    assert!(
        found,
        "Tcp.writeBytes must lower to the aver.tcp_write_bytes host import"
    );
}

/// `Crypto.sha256` produces a `Digest32` even when `depends` never names
/// `Crypto.Digest32`. This build goes through the shared
/// `aver::source::load_compile_deps` loader, which must include the
/// standard modules implied by source-typed builtins — without them the
/// wasm-gc backend has no `Digest32` record to emit and compilation fails
/// after check/verify already passed.
#[test]
fn sha256_compiles_without_digest32_in_depends() {
    let source = r#"module Probe
    intent = "Hash bytes while depends omits Crypto.Digest32."
    depends [Bytes]
    exposes [main]
    effects [Console.print]

fn main() -> Result<String, String>
    ? "Hash a payload and report that a digest was produced."
    ! [Console.print]
    payload = Bytes.fromList([1, 2, 3])
    digest = Crypto.sha256(payload)
    Console.print("hashed")
    Result.Ok("hashed")
"#;
    let (items, type_aliases) =
        parse_pipeline_with_module_root(source, Some(env!("CARGO_MANIFEST_DIR")))
            .unwrap_or_else(|e| panic!("{e}\n--- source ---\n{source}"));
    let bytes = compile_flattened(&items, &type_aliases)
        .unwrap_or_else(|e| panic!("wasm-gc compile: {e}\n--- source ---\n{source}"));
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .unwrap_or_else(|e| panic!("wasmparser validate: {e}\n--- source ---\n{source}"));
}

/// A program can hold `Bytes` values without ever naming the module:
/// `Tcp.readBytes` RETURNS `Bytes`, so a read→write relay is expressible
/// with an empty `depends` list. The implicit stdlib-dep table must load
/// `Bytes` for the Tcp byte methods too, and both lowered host imports
/// must survive the round trip.
#[test]
fn tcp_byte_relay_compiles_without_bytes_in_depends() {
    use wasmparser::{Parser as WasmParser, Payload};

    let source = r#"module Relay
    intent = "Echo binary frames without naming Bytes in depends."
    depends []
    exposes [relay]
    effects [Tcp.readBytes, Tcp.writeBytes]

fn relay(conn: Tcp.Connection) -> Result<Unit, String>
    ? "Echo one 4-byte frame back to the peer."
    ! [Tcp.readBytes, Tcp.writeBytes]
    frame = Tcp.readBytes(conn, 4)?
    Tcp.writeBytes(conn, frame)
"#;
    let (items, type_aliases) =
        parse_pipeline_with_module_root(source, Some(env!("CARGO_MANIFEST_DIR")))
            .unwrap_or_else(|e| panic!("{e}\n--- source ---\n{source}"));
    let bytes = compile_flattened(&items, &type_aliases)
        .unwrap_or_else(|e| panic!("wasm-gc compile: {e}\n--- source ---\n{source}"));
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .unwrap_or_else(|e| panic!("wasmparser validate: {e}\n--- source ---\n{source}"));

    let mut read_found = false;
    let mut write_found = false;
    for payload in WasmParser::new(0).parse_all(&bytes) {
        if let Payload::ImportSection(reader) = payload.expect("generated module must parse") {
            for import in reader.into_imports().flatten() {
                if import.module == "aver" && matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                    read_found |= import.name == "tcp_read_bytes";
                    write_found |= import.name == "tcp_write_bytes";
                }
            }
        }
    }
    assert!(
        read_found && write_found,
        "Tcp byte methods must lower to aver.tcp_read_bytes/aver.tcp_write_bytes host imports"
    );
}

/// A bare `List<Int>` literal in argument position. The literal used to
/// sit directly inside an interpolation (`"{[1, 2, 3]}"`); interpolation
/// renders primitives only now, so it reaches the same lowering through
/// the named conversion a user writes instead.
#[test]
fn bare_list_literal_through_a_named_conversion_compiles() {
    assert_compiles_and_validates(
        r#"module Probe
    intent = "Minimal List<Int> literal for wasm-gc compilation."
    exposes [main]
    effects [Console.print]

fn describe(xs: List<Int>) -> String
    ? "Name the conversion to String."
    "len={List.len(xs)}"

fn main() -> Unit
    ? "Print a list literal."
    ! [Console.print]
    Console.print(describe([1, 2, 3]))
"#,
    );
}

#[test]
fn unannotated_list_binding_compiles() {
    assert_compiles_and_validates(
        r#"module Probe
    intent = "Compile an unannotated list binding."
    exposes [main]

fn main() -> Int
    xs = [1, 2, 3]
    List.len(xs)
"#,
    );
}

#[test]
fn nested_unannotated_list_literal_compiles() {
    assert_compiles_and_validates(
        r#"module Probe
    intent = "Compile a nested unannotated list literal."
    exposes [main]

fn main() -> Int
    xss = [[1], [2, 3]]
    List.len(xss)
"#,
    );
}

#[test]
fn wasm_gc_codegen_emits_valid_module_for_every_single_file_example() {
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
        let (items, type_aliases) =
            match parse_pipeline_with_module_root(&source, Some(env!("CARGO_MANIFEST_DIR"))) {
                Ok(result) => result,
                Err(e) => {
                    failures.push(format!("{}: {}", path.display(), e));
                    continue;
                }
            };
        let bytes = match compile_flattened(&items, &type_aliases) {
            Ok(b) => b,
            Err(e) => {
                failures.push(format!("{}: compile_to_wasm_gc: {}", path.display(), e));
                continue;
            }
        };
        let mut validator = wasmparser::Validator::new();
        if let Err(e) = validator.validate_all(&bytes) {
            failures.push(format!(
                "{}: wasmparser validate ({} bytes): {}",
                path.display(),
                bytes.len(),
                e
            ));
            continue;
        }
        compiled += 1;
    }

    if !failures.is_empty() {
        panic!(
            "{} of {} single-file examples failed wasm-gc codegen + validate:\n  - {}",
            failures.len(),
            files.len(),
            failures.join("\n  - ")
        );
    }
    eprintln!(
        "wasm_gc_codegen_emits_valid_module_for_every_single_file_example: {} files compiled + validated",
        compiled
    );
}

#[test]
fn json_sum_uses_nominal_root_in_variants_tuple_and_dispatch() {
    use wasmparser::{CompositeInnerType, Operator, Parser as WasmParser, Payload, StorageType};

    let source = fs::read_to_string(examples_dir().join("data/json.av")).unwrap();
    let module_root = env!("CARGO_MANIFEST_DIR");
    let (items, type_aliases) =
        parse_pipeline_with_module_root(&source, Some(module_root)).unwrap();
    let bytes = compile_flattened(&items, &type_aliases).unwrap();
    wasmparser::Validator::new().validate_all(&bytes).unwrap();

    let mut types = Vec::new();
    let mut dispatch_targets = Vec::new();
    for payload in WasmParser::new(0).parse_all(&bytes) {
        match payload.unwrap() {
            Payload::TypeSection(reader) => {
                for group in reader {
                    types.extend(group.unwrap().into_types());
                }
            }
            Payload::CodeSectionEntry(body) => {
                let mut ops = body.get_operators_reader().unwrap();
                while !ops.eof() {
                    match ops.read().unwrap() {
                        Operator::RefTestNonNull { hty }
                        | Operator::RefTestNullable { hty }
                        | Operator::RefCastNonNull { hty }
                        | Operator::RefCastNullable { hty } => {
                            if let wasmparser::HeapType::Concrete(idx)
                            | wasmparser::HeapType::Exact(idx) = hty
                                && let Some(idx) = idx.as_module_index()
                            {
                                dispatch_targets.push(idx);
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    let root = &types[0];
    assert!(!root.is_final, "Json root must be non-final");
    assert!(
        root.supertype_idx.is_none(),
        "Json root must have no parent"
    );
    assert!(matches!(
        &root.composite_type.inner,
        CompositeInnerType::Struct(st) if st.fields.is_empty()
    ));
    for variant in &types[1..=6] {
        assert!(variant.is_final, "Json variants remain final");
        assert_eq!(
            variant.supertype_idx.and_then(|idx| idx.as_module_index()),
            Some(0),
            "every Json variant must subtype the Json root"
        );
    }

    let tuple_holds_json_root = types.iter().any(|sub| {
        let CompositeInnerType::Struct(st) = &sub.composite_type.inner else {
            return false;
        };
        if st.fields.len() != 2 {
            return false;
        }
        matches!(
            st.fields[1].element_type,
            StorageType::Val(wasmparser::ValType::Ref(rt))
                if rt.is_nullable()
                    && matches!(rt.heap_type(), wasmparser::HeapType::Concrete(idx)
                        if idx.as_module_index() == Some(0))
        )
    });
    assert!(
        tuple_holds_json_root,
        "Tuple<String, Json> field 1 must be `(ref null $Json)`, not eqref"
    );
    assert!(!dispatch_targets.is_empty());
    assert!(
        dispatch_targets.iter().all(|idx| *idx != 0),
        "specific pattern tests/casts must target concrete variants, not the sum root"
    );
}

// ─── Compound string interpolation is a LOUD codegen error ──────────────
//
// The typechecker rejects a non-primitive interpolation embed outright,
// so `aver run` / `aver compile` never reach the wasm-gc emitter with
// one. Internal pipelines that drive codegen WITHOUT gating on the
// checker's errors still can, and so would any future checker gap. The
// emitter must answer with an error naming the fn and the embed type —
// not by bailing to `Ok(None)`, which hands the whole fn an
// `unreachable` trap stub and ships a program that compiles clean and
// then traps at runtime with no diagnostic.

/// Frontend pipeline that IGNORES typecheck errors — the shape an
/// eval-style internal harness has. `pipeline::run` skips every later
/// stage once the checker reports an error, so the typecheck is driven
/// separately (it stamps types into the AST as it walks, errors or not)
/// and the pipeline then runs unguarded. Codegen therefore sees the same
/// stamped `List<Int>` embed a checker gap would let through.
fn parse_pipeline_ignoring_type_errors(source: &str) -> Vec<TopLevel> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    let tc = ir::pipeline::typecheck(&items, &ir::TypecheckMode::Full { base_dir: None });
    assert!(
        !tc.errors.is_empty(),
        "this harness exists to model an UNGATED pipeline — the program is \
         expected to be rejected by the checker"
    );
    ir::pipeline::run(
        &mut items,
        ir::PipelineConfig {
            typecheck: None,
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: false,
            run_list_build: false,
            ..Default::default()
        },
    );
    items
}

#[test]
fn compound_interpolation_embed_is_a_loud_codegen_error_not_a_trap_stub() {
    let source = r#"module M
    intent = "reach the interpolation emitter with a compound embed"
    effects [Console]

fn describe(xs: List<Int>) -> String
    "xs={xs}"

fn main() -> Unit
    ! [Console.print]
    Console.print(describe([1, 2, 3]))
"#;
    let items = parse_pipeline_ignoring_type_errors(source);
    let err = aver::codegen::wasm_gc::compile_to_wasm_gc(&items, None)
        .expect_err("a compound interpolation embed must fail the wasm-gc compile");
    let msg = err.to_string();
    assert!(
        msg.contains("describe"),
        "the codegen error must name the offending fn: {msg}"
    );
    assert!(
        msg.contains("List<Int>"),
        "the codegen error must name the embed type: {msg}"
    );
    assert!(
        msg.contains("no stringifier"),
        "the codegen error must say what is missing: {msg}"
    );
}

/// Positive control: the same program with the conversion NAMED compiles
/// and validates, so the test above is pinning the compound embed and
/// not some unrelated breakage in the shape.
#[test]
fn named_conversion_interpolation_still_compiles_and_validates() {
    let source = r#"module M
    intent = "render a list through a named conversion"
    effects [Console]

fn joinInts(xs: List<Int>) -> String
    match xs
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            _ -> "{head}, {joinInts(tail)}"

fn describe(xs: List<Int>) -> String
    "xs=[{joinInts(xs)}]"

fn main() -> Unit
    ! [Console.print]
    Console.print(describe([1, 2, 3]))
"#;
    assert_compiles_and_validates(source);
}

/// An empty literal defaulted through `Result.withDefault` /
/// `Option.withDefault` in a binding with no expected type anywhere.
///
/// This shape used to type-check clean while stamping `List<Var("T")>` /
/// `Map<Var("K"), Var("V")>`, and the backend then refused it — "List op
/// called but `List<T>` helper wasn't registered", "cannot lower type
/// `K` to a wasm representation". `aver check` passing and
/// `--target wasm-gc` failing is the pairing this pins shut: the
/// default now takes its element type from the subject's payload, so
/// the stamp is concrete and the instantiation registry has an entry.
#[test]
fn empty_default_with_no_expected_type_compiles() {
    assert_compiles_and_validates(
        r#"module Probe
    intent = "Empty defaults with nothing to fix their element type."
    exposes [countList, countMap]

fn countList(r: Result<List<Int>, String>) -> Int
    xs = Result.withDefault(r, [])
    List.len(xs)

fn countMap(o: Option<Map<String, Int>>) -> Int
    m = Option.withDefault(o, {})
    Map.len(m)
"#,
    );
}

/// Two SIBLING match arms binding the same name in an inline-eligible
/// fn (issue #948's disease, in the escape pass). `classify_fn` used
/// to resolve arm-binder slots by name through the fn-level last-wins
/// `local_slots` map, handing BOTH arms the second arm's slot — the
/// first arm's body then spliced into `main` with its binder
/// unsubstituted, a dangling local index the wasmparser validator
/// rejected (recorded pre-fix red: this exact source failed
/// validation). Slots now come from `MatchArm::binding_slots`. The
/// behavioural half (all backends answering 6/30) lives in
/// `tests/vm_pattern_shadow_matrix.rs`.
#[test]
fn sibling_arms_reusing_a_binder_name_compile_and_validate() {
    assert_compiles_and_validates(
        r#"module Tmp

type Shape
    Circle(Int)
    Square(Int)

fn eval(p: Shape) -> Int
    match p
        Shape.Circle(n) -> n + 1
        Shape.Square(n) -> n * 10

fn main()
    ! [Console.print]
    Console.print(String.fromInt(eval(Shape.Circle(5))))
    Console.print(String.fromInt(eval(Shape.Square(3))))
"#,
    );
}

/// Regression for #1084: flattening rewrites an entry signature from the
/// dependency-qualified spelling to the linked type name. The wasm function
/// type and the MIR body must read the same post-link identity.
#[test]
fn entry_returning_qualified_dependency_type_compiles_and_validates() {
    let root = tempfile::tempdir().expect("temporary module root");
    fs::write(
        root.path().join("tmpreviewc.av"),
        r#"module TmpReviewC
    intent = "Own the Step value returned through the entry boundary."
    exposes [Step, made]
    depends []

type Step
    Made(Int)

fn made(n: Int) -> Step
    Step.Made(n)
"#,
    )
    .expect("write dependency");

    let entry = r#"module Main
    intent = "Return a qualified dependency type from an entry function."
    depends [TmpReviewC]

fn made(n: Int) -> TmpReviewC.Step
    TmpReviewC.made(n)

fn main() -> Int
    match made(7)
        TmpReviewC.Step.Made(n) -> n
"#;
    let root_str = root.path().to_str().expect("utf-8 module root");
    let (items, type_aliases) =
        parse_pipeline_with_module_root(entry, Some(root_str)).expect("pipeline");
    let bytes = compile_flattened(&items, &type_aliases).expect("wasm-gc compile");
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .expect("qualified dependency return must validate");
}
