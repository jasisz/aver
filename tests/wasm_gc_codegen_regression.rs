//! wasm-gc codegen regression suite.
//!
//! For every single-file `examples/**/*.av` program (no `depends`
//! clause), drive the full frontend → IR pipeline → `compile_to_wasm_gc`
//! → `wasmparser::Validator` walk. Asserts: no panic, no compile
//! error, no validator rejection. Multi-module games / apps need
//! `load_compile_deps` which lives in the CLI binary today; they get a
//! follow-up subprocess-based test when that helper moves to lib.
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
            typecheck: Some(ir::TypecheckMode::Full { base_dir: None }),
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
    Ok(items)
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
        let items = match parse_pipeline(&source) {
            Ok(i) => i,
            Err(e) => {
                failures.push(format!("{}: {}", path.display(), e));
                continue;
            }
        };
        let bytes = match aver::codegen::wasm_gc::compile_to_wasm_gc(&items, None) {
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
    let items = parse_pipeline(&source).unwrap();
    let bytes = aver::codegen::wasm_gc::compile_to_wasm_gc(&items, None).unwrap();
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
