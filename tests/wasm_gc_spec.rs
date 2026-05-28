#![cfg(feature = "wasm")]

//! Compile-and-run smoke tests for the wasm-gc backend. Each test
//! compiles a small Aver program through the same pipeline shape as
//! `aver compile --target=wasm-gc` (typecheck=Full, neutral alloc
//! policy, interp_lower / buffer_build OFF), drives the produced
//! module under wasmtime with the GC + tail-call config, and asserts
//! on the `Int` returned from `main`.
//!
//! Coverage targets the parity matrix added in 0.16: `List<T>`,
//! `Map<K, V>` for K ∈ {String, Int, user record, user sum}, `Vector<T>`,
//! `Tuple<A, B>`, `Option<T>`, `Result<T, E>`, nested compounds, and
//! the `String` builtin surface. Compound types are surfaced through
//! helper-fn signatures so the wasm-gc backend's discovery pass picks
//! them up — pure body-level literals (`[1, 2, 3]`) never expose
//! `List<Int>` to the registry on their own.

use aver::codegen::wasm_gc::compile_to_wasm_gc;
use aver::ir::{PipelineConfig, TypecheckMode, pipeline};
use aver::source::parse_source;

fn compile_bytes(source: &str) -> Vec<u8> {
    let mut items = parse_source(source).unwrap_or_else(|e| {
        panic!("parse failed: {e}\n--- source ---\n{source}");
    });
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        panic!(
            "typecheck failed: {:?}\n--- source ---\n{source}",
            tc.errors
        );
    }
    compile_to_wasm_gc(&items, result.analysis.as_ref()).unwrap_or_else(|e| {
        panic!("wasm-gc compile failed: {e:?}\n--- source ---\n{source}");
    })
}

fn run_int(source: &str) -> i64 {
    let bytes = compile_bytes(source);
    let mut config = wasmtime::Config::new();
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    config.cranelift_opt_level(wasmtime::OptLevel::Speed);
    config.max_wasm_stack(8 * 1024 * 1024);
    // `component-model-async` (pulled in by the `wasip2` feature)
    // enforces `max_wasm_stack <= async_stack_size` at Engine::new.
    config.async_stack_size(12 * 1024 * 1024);
    let engine = wasmtime::Engine::new(&config).expect("wasmtime engine");
    let module = wasmtime::Module::new(&engine, &bytes).unwrap_or_else(|e| {
        panic!("wasmtime rejected wasm-gc bytes: {e}");
    });
    let mut store = wasmtime::Store::new(&engine, ());
    let mut linker = wasmtime::Linker::new(&engine);
    stub_imports(&module, &engine, &mut linker);
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap_or_else(|e| panic!("instantiate failed: {e}"));
    let main = instance
        .get_typed_func::<(), i64>(&mut store, "main")
        .unwrap_or_else(|e| panic!("main: () -> Int export missing: {e}"));
    main.call(&mut store, ())
        .unwrap_or_else(|e| panic!("main trapped: {e}"))
}

/// Walk the module's import section and register a default-value stub
/// for each `(module, name)` pair under its declared signature. Keeps
/// tests immune to unused builtin helpers that pull in `aver/console_*`
/// or `aver/time_*` even when the test program never calls them.
fn stub_imports(
    module: &wasmtime::Module,
    engine: &wasmtime::Engine,
    linker: &mut wasmtime::Linker<()>,
) {
    use wasmtime::ExternType;
    for import in module.imports() {
        let ExternType::Func(ft) = import.ty() else {
            continue;
        };
        let result_tys: Vec<wasmtime::ValType> = ft.results().collect();
        let func_ty = wasmtime::FuncType::new(engine, ft.params(), ft.results());
        let module_name = import.module().to_string();
        let field_name = import.name().to_string();
        let _ = linker.func_new(
            &module_name,
            &field_name,
            func_ty,
            move |_caller, _params, results| {
                for (slot, ty) in results.iter_mut().zip(result_tys.iter()) {
                    *slot = default_val(ty);
                }
                Ok(())
            },
        );
    }
}

fn default_val(ty: &wasmtime::ValType) -> wasmtime::Val {
    use wasmtime::{Val, ValType};
    match ty {
        ValType::I32 => Val::I32(0),
        ValType::I64 => Val::I64(0),
        ValType::F32 => Val::F32(0),
        ValType::F64 => Val::F64(0),
        ValType::V128 => Val::V128(0u128.into()),
        ValType::Ref(_) => Val::AnyRef(None),
    }
}

/// Multi-module compile: parses entry + dep sources, runs the
/// pipeline with `WithLoaded`, flattens dep fns into the entry
/// namespace, runs the post-link resolver pass, and emits wasm
/// bytes. Mirrors the playground's multi-file path so tests can
/// exercise cross-module identity through the wasm-gc backend's
/// `flatten_multimodule` + `WasmGcLinkedView` (epic #170 Phase 6).
fn compile_multi_module_bytes(entry_src: &str, dep_sources: &[(&str, &str)]) -> Vec<u8> {
    let mut entry_items = parse_source(entry_src).unwrap_or_else(|e| {
        panic!("entry parse failed: {e}\n--- entry ---\n{entry_src}");
    });
    let loaded: Vec<aver::source::LoadedModule> = dep_sources
        .iter()
        .map(|(prefix, src)| aver::source::LoadedModule {
            dep_name: prefix.to_string(),
            items: parse_source(src).unwrap_or_else(|e| {
                panic!("dep '{prefix}' parse failed: {e}\n--- dep ---\n{src}");
            }),
            path: std::path::PathBuf::from(format!("{prefix}.av")),
        })
        .collect();

    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = pipeline::run(
        &mut entry_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithLoaded(&loaded)),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        panic!("typecheck failed: {:?}", tc.errors);
    }
    let modules: Vec<aver::codegen::ModuleInfo> = loaded
        .into_iter()
        .map(|m| aver::codegen::ModuleInfo::from_loaded(&m))
        .collect();
    aver::codegen::wasm_gc::flatten_multimodule(&mut entry_items, &modules);
    aver::ir::pipeline::resolve(&mut entry_items);
    compile_to_wasm_gc(&entry_items, result.analysis.as_ref()).unwrap_or_else(|e| {
        panic!("wasm-gc multi-module compile failed: {e:?}");
    })
}

fn run_int_multi(entry_src: &str, dep_sources: &[(&str, &str)]) -> i64 {
    let bytes = compile_multi_module_bytes(entry_src, dep_sources);
    let mut config = wasmtime::Config::new();
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    config.cranelift_opt_level(wasmtime::OptLevel::Speed);
    config.max_wasm_stack(8 * 1024 * 1024);
    config.async_stack_size(12 * 1024 * 1024);
    let engine = wasmtime::Engine::new(&config).expect("wasmtime engine");
    let module = wasmtime::Module::new(&engine, &bytes)
        .unwrap_or_else(|e| panic!("wasmtime rejected wasm-gc bytes: {e}"));
    let mut store = wasmtime::Store::new(&engine, ());
    let mut linker = wasmtime::Linker::new(&engine);
    stub_imports(&module, &engine, &mut linker);
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap_or_else(|e| panic!("instantiate failed: {e}"));
    let main = instance
        .get_typed_func::<(), i64>(&mut store, "main")
        .unwrap_or_else(|e| panic!("main: () -> Int export missing: {e}"));
    main.call(&mut store, ())
        .unwrap_or_else(|e| panic!("main trapped: {e}"))
}

// ────────────────────────────────────────────────────────────────────
// Cross-module identity (epic #170 Phase 6)
// ────────────────────────────────────────────────────────────────────

#[test]
fn cross_module_same_bare_name_fns_resolve_via_flatten_and_link_view() {
    // Two same-bare `helper(n: Int) -> Int` fns — one in entry, one
    // in dep module `Worker`. Different bodies (+1 vs +100). The
    // wasm-gc backend's `flatten_multimodule` mangles dep fn names
    // (`Worker.helper` → `Worker_helper`), then `WasmGcLinkedView`
    // re-resolves against the flattened namespace and indexes by
    // `FnId`. If either step regressed to bare-name keying, the two
    // fns would collide on `helper` and main's `1 + 100 = 101`
    // result would shift (e.g. to `1 + 1 = 2` if entry's body
    // shadowed Worker's, or `100 + 100 = 200` the other way).
    let entry_src = r#"
module Entry
    intent = "cross-module same-bare-name regression"
    depends [Worker]

fn helper(n: Int) -> Int
    n + 1

fn main() -> Int
    helper(0) + Worker.helper(0)
"#;
    let dep_src = r#"
module Worker
    intent = "Worker module with same-bare 'helper'"
    exposes [helper]
    depends []

fn helper(n: Int) -> Int
    n + 100
"#;
    let result = run_int_multi(entry_src, &[("Worker", dep_src)]);
    assert_eq!(
        result, 101,
        "expected Entry.helper(0) + Worker.helper(0) = 1 + 100 = 101; \
         a divergence here means flatten / link-view crossed wires"
    );
}

// Cross-module same-bare-name `TypeDef` regression for wasm-gc
// — known-failing pin, `#[ignore]`'d until the canonical-key
// migration lands.
//
// Epic #180 Phase 6 PR A (#189) migrated Dafny + Rust registries
// to canonical keys (`Left.Box` vs `Right.Box`). wasm-gc's
// `TypeRegistry` plus the `flatten_multimodule` field-type
// stripping pass are interconnected: changing one without the
// other breaks the wider lookup chain (record field types →
// record_type_idx → struct emission, constructor name normalise
// → variant routing, etc.). A complete migration touches
// `flatten`, `TypeRegistry::build_with_handler`, and every
// downstream walk that lookups TypeDef.name — meaningfully
// bigger scope than fits a single PR alongside the Phase 6
// follow-ups.
//
// Pinned here as a RED test so the regression has a name and
// the canonical-key migration has a clear acceptance gate.
// Three `temporary-migration-bridge` tagged sites in
// `wasm_gc/{module,body/emit}.rs` (PR #191) are the entry
// points that flip together with the registry storage.
#[test]
fn cross_module_same_bare_name_types_resolve_to_distinct_records() {
    // Two dep modules each declare `record Box { value: Int }`
    // — same bare name, different field schemas. Entry creates
    // one of each and reads `.value`. Pre-migration,
    // `flatten_multimodule` strips module prefixes from field
    // types, both `Box` records land under the bare-name `Box`
    // slot in `TypeRegistry`, and the second
    // `record_fields.insert` wins → record-create / projection
    // picks the wrong field schema (or, more commonly, validation
    // fails before that point).
    //
    // Post-migration, wasm-gc `TypeRegistry` would key by
    // canonical name (`Left.Box` vs `Right.Box`), so the two
    // records occupy distinct slots. Expected `5 + 10 = 15`.
    let entry_src = r#"
module Entry
    intent = "cross-module same-bare-name TYPE regression"
    depends [Left, Right]

fn main() -> Int
    Left.Box(value = 5).value + Right.Box(value = 10).value
"#;
    let left_src = r#"
module Left
    intent = "left container"
    exposes [Box]
    depends []

record Box
    value: Int
"#;
    let right_src = r#"
module Right
    intent = "right container"
    exposes [Box]
    depends []

record Box
    value: Int
"#;
    let result = run_int_multi(entry_src, &[("Left", left_src), ("Right", right_src)]);
    assert_eq!(
        result, 15,
        "expected Left.Box(5).value + Right.Box(10).value = 15; \
         a divergence here means cross-module TypeDef bare names \
         collided in the wasm-gc registry"
    );
}

// ────────────────────────────────────────────────────────────────────
// List<T>
// ────────────────────────────────────────────────────────────────────

#[test]
fn list_int_len() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "list len"
    depends []

fn build() -> List<Int>
    [1, 2, 3, 4, 5]

fn main() -> Int
    List.len(build())
"#
        ),
        5
    );
}

#[test]
fn list_int_reverse_first() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "list reverse head"
    depends []

fn build() -> List<Int>
    [1, 2, 3]

fn head(xs: List<Int>) -> Int
    match xs
        [h, ..t] -> h
        _        -> -1

fn main() -> Int
    head(List.reverse(build()))
"#
        ),
        3
    );
}

#[test]
fn list_int_concat_len() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "list concat len"
    depends []

fn lhs() -> List<Int>
    [1, 2]

fn rhs() -> List<Int>
    [3, 4, 5]

fn joined() -> List<Int>
    List.concat(lhs(), rhs())

fn main() -> Int
    List.len(joined())
"#
        ),
        5
    );
}

#[test]
fn list_int_contains_true() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "list contains true"
    depends []

fn build() -> List<Int>
    [1, 2, 3]

fn main() -> Int
    match List.contains(build(), 2)
        true  -> 1
        false -> 0
"#
        ),
        1
    );
}

#[test]
fn list_string_contains_false() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "list string contains false"
    depends []

fn build() -> List<String>
    ["alpha", "beta"]

fn main() -> Int
    match List.contains(build(), "gamma")
        true  -> 1
        false -> 0
"#
        ),
        0
    );
}

#[test]
fn list_int_take_drop() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "list take drop"
    depends []

fn build() -> List<Int>
    [1, 2, 3, 4, 5, 6]

fn dropped() -> List<Int>
    List.drop(build(), 1)

fn taken() -> List<Int>
    List.take(dropped(), 3)

fn main() -> Int
    List.len(taken())
"#
        ),
        3
    );
}

// ────────────────────────────────────────────────────────────────────
// Map<K, V>
// ────────────────────────────────────────────────────────────────────

#[test]
fn map_string_int_get_after_set() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map<string,int> get"
    depends []

fn build() -> Map<String, Int>
    Map.set({}, "k", 42)

fn main() -> Int
    Option.withDefault(Map.get(build(), "k"), -1)
"#
        ),
        42
    );
}

#[test]
fn map_string_int_overwrite_keeps_last() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map overwrite same key"
    depends []

fn build() -> Map<String, Int>
    Map.set(Map.set({}, "k", 1), "k", 99)

fn main() -> Int
    Option.withDefault(Map.get(build(), "k"), -1)
"#
        ),
        99
    );
}

#[test]
fn map_int_int_roundtrip() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map<int,int> roundtrip"
    depends []

fn build() -> Map<Int, Int>
    Map.set(Map.set({}, 7, 70), 13, 130)

fn main() -> Int
    Option.withDefault(Map.get(build(), 13), -1)
"#
        ),
        130
    );
}

#[test]
fn map_has_returns_one_for_present_key() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map has present"
    depends []

fn build() -> Map<String, Int>
    Map.set({}, "k", 1)

fn main() -> Int
    match Map.has(build(), "k")
        true  -> 1
        false -> 0
"#
        ),
        1
    );
}

#[test]
fn map_remove_then_get_returns_default() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map remove then default"
    depends []

fn seeded() -> Map<String, Int>
    Map.set({}, "k", 7)

fn build() -> Map<String, Int>
    Map.remove(seeded(), "k")

fn main() -> Int
    Option.withDefault(Map.get(build(), "k"), -1)
"#
        ),
        -1
    );
}

#[test]
fn map_keys_count() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map keys len"
    depends []

fn build() -> Map<String, Int>
    Map.set(Map.set(Map.set({}, "a", 1), "b", 2), "c", 3)

fn main() -> Int
    List.len(Map.keys(build()))
"#
        ),
        3
    );
}

#[test]
fn map_string_list_int_nested_value_len() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map<string, list<int>>"
    depends []

fn nums() -> List<Int>
    [10, 20, 30, 40]

fn build() -> Map<String, List<Int>>
    Map.set({}, "k", nums())

fn fallback() -> List<Int>
    []

fn main() -> Int
    List.len(Option.withDefault(Map.get(build(), "k"), fallback()))
"#
        ),
        4
    );
}

#[test]
fn map_record_key_get() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map<record, int>"
    depends []

record Point
    x: Int
    y: Int

fn key() -> Point
    Point(x = 1, y = 2)

fn build() -> Map<Point, Int>
    Map.set({}, key(), 99)

fn main() -> Int
    Option.withDefault(Map.get(build(), key()), -1)
"#
        ),
        99
    );
}

#[test]
fn map_sum_key_get() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map<sum, int>"
    depends []

type Tag
    Red
    Green
    Blue

fn redKey() -> Tag
    Tag.Red

fn greenKey() -> Tag
    Tag.Green

fn blueKey() -> Tag
    Tag.Blue

fn build() -> Map<Tag, Int>
    m1 = Map.set({}, redKey(), 1)
    m2 = Map.set(m1, greenKey(), 2)
    Map.set(m2, blueKey(), 3)

fn main() -> Int
    Option.withDefault(Map.get(build(), greenKey()), -1)
"#
        ),
        2
    );
}

// ────────────────────────────────────────────────────────────────────
// Vector<T>
// ────────────────────────────────────────────────────────────────────

#[test]
fn vector_get_after_set() {
    // Mirrors `bench/scenarios/vector_ops.av` shape: tail-recursive
    // fill writes i*i at position i. After fill the cell at index 2
    // holds 4 (= 2*2). Verifies the owned-mutate fast path through
    // a TCO loop, the same shape the bench has been runtime-validated
    // against on every wasm-gc commit.
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "vector get/set"
    depends []

fn fill(v: Vector<Int>, n: Int, i: Int) -> Vector<Int>
    match i == n
        true  -> v
        false -> fill(Option.withDefault(Vector.set(v, i, i * i), v), n, i + 1)

fn main() -> Int
    v = fill(Vector.new(3, 0), 3, 0)
    Option.withDefault(Vector.get(v, 2), -1)
"#
        ),
        4
    );
}

#[test]
fn vector_to_list_len_matches() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "vector toList"
    depends []

fn build() -> Vector<Int>
    Vector.fromList([1, 2, 3, 4])

fn main() -> Int
    List.len(List.fromVector(build()))
"#
        ),
        4
    );
}

#[test]
fn vector_len_is_size() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "vector len"
    depends []

fn build() -> Vector<Int>
    Vector.fromList([1, 2, 3, 4, 5, 6, 7])

fn main() -> Int
    Vector.len(build())
"#
        ),
        7
    );
}

// ────────────────────────────────────────────────────────────────────
// Tuple<A, B>
// ────────────────────────────────────────────────────────────────────

#[test]
fn tuple_destructure_sum() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "tuple destructure"
    depends []

fn pair() -> Tuple<Int, Int>
    (3, 4)

fn main() -> Int
    match pair()
        (a, b) -> a + b
"#
        ),
        7
    );
}

// ────────────────────────────────────────────────────────────────────
// Option / Result
// ────────────────────────────────────────────────────────────────────

#[test]
fn option_with_default_some() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "Option.withDefault Some"
    depends []

fn produce() -> Option<Int>
    Option.Some(7)

fn main() -> Int
    Option.withDefault(produce(), 0)
"#
        ),
        7
    );
}

#[test]
fn option_with_default_none() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "Option.withDefault None"
    depends []

fn produce() -> Option<Int>
    Option.None

fn main() -> Int
    Option.withDefault(produce(), 42)
"#
        ),
        42
    );
}

#[test]
fn result_with_default_ok() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "Result.withDefault Ok"
    depends []

fn produce() -> Result<Int, String>
    Result.Ok(5)

fn main() -> Int
    Result.withDefault(produce(), 0)
"#
        ),
        5
    );
}

#[test]
fn result_with_default_err() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "Result.withDefault Err"
    depends []

fn produce() -> Result<Int, String>
    Result.Err("boom")

fn main() -> Int
    Result.withDefault(produce(), 99)
"#
        ),
        99
    );
}

// ────────────────────────────────────────────────────────────────────
// String surface (numeric reductions only — return Int)
// ────────────────────────────────────────────────────────────────────

#[test]
fn string_len_byte_count() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "String.len"
    depends []

fn main() -> Int
    String.len("hello")
"#
        ),
        5
    );
}

#[test]
fn string_starts_with_true() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "String.startsWith"
    depends []

fn main() -> Int
    match String.startsWith("hello world", "hello")
        true  -> 1
        false -> 0
"#
        ),
        1
    );
}

#[test]
fn string_split_join_roundtrip_len() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "String.split + List.len"
    depends []

fn pieces() -> List<String>
    String.split("a,b,c,d", ",")

fn main() -> Int
    List.len(pieces())
"#
        ),
        4
    );
}
