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
            run_chars_fusion: false,
            run_list_build: false,
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
    run_int_result(source).unwrap_or_else(|e| panic!("main trapped: {e}"))
}

/// Same drive as [`run_int`], but hands back the wasmtime trap instead
/// of panicking on it — for the tests that assert on a trap's message.
fn run_int_result(source: &str) -> Result<i64, String> {
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
    call_main_aint_result(&mut store, &instance)
}

/// Run the same source on the bytecode VM and read `main`'s `Int`. The
/// VM is the reference answer both backends owe: a wasm-gc test that
/// asserts a constant pins what the module does, and pairing it with
/// this pins that the two backends agree. Gated on `runtime` so a
/// `--no-default-features --features wasm` build still compiles.
#[cfg(feature = "runtime")]
fn run_int_on_vm(source: &str) -> i64 {
    use aver::nan_value::{Arena, NanValueConvert};
    use aver::value::Value;

    let mut items = aver::source::parse_source(source).unwrap_or_else(|e| {
        panic!("parse failed: {e}\n--- source ---\n{source}");
    });
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
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
    let mut arena = Arena::new();
    aver::vm::register_service_types(&mut arena);
    let (code, globals) = aver::vm::compile_program_with_mir_fallback(
        &result.resolved_items,
        &result.symbol_table,
        &mut arena,
        result.analysis.as_ref(),
    )
    .unwrap_or_else(|e| panic!("VM compile failed: {e:?}"));
    let mut machine = aver::vm::VM::new(code, globals, arena);
    let returned = machine
        .run_named_function("main", &[])
        .unwrap_or_else(|e| panic!("VM run failed: {e:?}"));
    match returned.to_value(&machine.arena) {
        Value::Int(n) => n.to_i64().expect("VM main returned an out-of-range Int"),
        other => panic!("VM main returned a non-Int: {other:?}"),
    }
}

/// Call the `main` export and extract its `Int` return. `Int = ℤ`:
/// `main` now returns a `(ref null $AverInt)` carrier, not a scalar
/// `i64`. Every program in this spec returns a value that fits i64 (a
/// length, a small arithmetic result), so it is always the Small
/// representation (`$magf == null`); read field 0 (`$small`). A Big
/// result would need the limb-array decode, which these in-range tests
/// never produce.
fn call_main_aint(store: &mut wasmtime::Store<()>, instance: &wasmtime::Instance) -> i64 {
    call_main_aint_result(store, instance).unwrap_or_else(|e| panic!("main trapped: {e}"))
}

/// The call itself, shared by [`run_int_result`] (which reports a trap)
/// and [`call_main_aint`] (which panics on one).
fn call_main_aint_result(
    store: &mut wasmtime::Store<()>,
    instance: &wasmtime::Instance,
) -> Result<i64, String> {
    let main = instance
        .get_func(&mut *store, "main")
        .unwrap_or_else(|| panic!("main export missing"));
    let mut results = [wasmtime::Val::I32(0)];
    main.call(&mut *store, &[], &mut results)
        .map_err(|e| format!("{e:#}"))?;
    Ok(extract_aint_small(store, &results[0]))
}

/// Read the Small (`$small`, field 0) i64 out of an `$AverInt` carrier
/// returned by `main`. Asserts the value is in the Small representation
/// (the spec programs are all in-range); a non-null `$magf` (field 1)
/// would mean a Big result this helper does not decode.
fn extract_aint_small(store: &mut wasmtime::Store<()>, val: &wasmtime::Val) -> i64 {
    let anyref = match val {
        wasmtime::Val::AnyRef(Some(a)) => *a,
        wasmtime::Val::AnyRef(None) => panic!("main returned a null Int carrier"),
        other => panic!("main returned a non-ref value: {other:?}"),
    };
    let structref = anyref
        .as_struct(&mut *store)
        .expect("anyref→struct query")
        .expect("Int carrier is not a struct");
    let mag = structref.field(&mut *store, 1).expect("read $magf field");
    match mag {
        wasmtime::Val::AnyRef(None) => {}
        _ => panic!("main returned a Big Int; this spec only covers Small results"),
    }
    match structref.field(&mut *store, 0).expect("read $small field") {
        wasmtime::Val::I64(v) => v,
        other => panic!("$small field was not an i64: {other:?}"),
    }
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
    let modules: Vec<aver::codegen::ModuleInfo> = loaded
        .into_iter()
        .map(|m| aver::codegen::ModuleInfo::from_loaded(&m))
        .collect();
    let type_aliases = aver::codegen::wasm_gc::flatten_multimodule(
        &mut entry_items,
        &modules,
        &result
            .typecheck
            .as_ref()
            .expect("typecheck requested")
            .capabilities,
        aver::codegen::wasm_gc::CapabilityFunctionSurface::Runtime,
    );
    aver::ir::pipeline::resolve(&mut entry_items);
    aver::codegen::wasm_gc::compile_to_wasm_gc_flattened(
        &entry_items,
        result.analysis.as_ref(),
        None,
        aver::codegen::wasm_gc::TargetMode::AverBridge,
        &type_aliases,
    )
    .unwrap_or_else(|e| {
        panic!("wasm-gc multi-module compile failed: {e:?}");
    })
    .bytes
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
    call_main_aint(&mut store, &instance)
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

// `String.fromFloat` shortest-roundtrip on a 17-significant-digit
// f64 — pins the WAT helper's frac-digit cap at 17 (was 15 pre-#203,
// which truncated `1.6181818181818182` to `1.618181818181818` and
// surfaced as a VM↔wasm-gc parity divergence on every Aver source
// that printed a Float of ≥16 fractional digits — e.g. the
// `goldenApprox(n) = Float.fromInt(fib(n + 1)) / Float.fromInt(fib(n))`
// line in `examples/data/fibonacci.av`).
//
// Rust's `f64::to_string` rounds the golden-ratio approximation
// to exactly `"1.6181818181818182"` (18 chars). The helper now
// matches.
#[test]
fn string_from_float_emits_17_digit_shortest_roundtrip() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "String.fromFloat shortest-roundtrip"
    depends []

fn main() -> Int
    String.len(String.fromFloat(1.6181818181818182))
"#
        ),
        18,
        "expected 18-char `1.6181818181818182` shortest-roundtrip; \
         a shorter result means the WAT helper's frac-digit cap \
         regressed below 17",
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
// Pinned here as a regression for the canonical-key migration.
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

// Cross-module same-bare-name `==`/`!=` dispatch — exercises the
// canonical-key routing through `named_type_registry_key` at the three
// eq-helper sites (`register_nominal_in_type`, the `BinOp Eq/Neq` arm of
// `discover_builtins_in_expr`,
// `sum_or_record_eq_fn`). Pre-#180-Phase-6-PR-3 these resolved the
// per-type `__eq_<Box>` slot by the bare `Type::Named.name` of the
// operand's stamped type — fine for non-colliding dep types, but in
// the collision case the registry had renamed `Left.Box` / `Right.Box`
// canonical entries and the bare lookup would miss, leaving the eq
// helper unregistered and the BinOp emit unable to dispatch.
#[test]
fn cross_module_same_bare_name_records_dispatch_eq_via_canonical_key() {
    let entry_src = r#"
module Entry
    intent = "cross-module same-bare-name == dispatch"
    depends [Left, Right]

fn main() -> Int
    a = Left.Box(value = 7)
    b = Left.Box(value = 7)
    c = Right.Box(value = 7)
    d = Right.Box(value = 9)
    match Bool.and(a == b, c != d)
        true -> 1
        false -> 0
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
        result, 1,
        "expected Left.Box eq + Right.Box neq to dispatch through canonical \
         per-type __eq_<X> helpers post #180 Phase 6 PR 3 — got {result}"
    );
}

// ────────────────────────────────────────────────────────────────────
// A local type shadows a dependency's same-named one (#848)
//
// The rule is settled for records (#818) and for sum types, and both
// the VM and the self-hosted interpreter implement it. Before the fix
// the wasm-gc flatten pass only counted a bare name as ambiguous when
// two DEP modules declared it, so an entry declaration sharing the name
// left both `TypeDef`s under one bare registry key. The two same-named
// sums then merged their slot indices and the module failed validation
// with `supertypes must be defined before subtypes`.
//
// The first two tests below are the shadowing cases; the two after them
// are controls that must keep passing, so the feature cannot be
// "fixed" by refusing multi-module programs or sum types outright.
// ────────────────────────────────────────────────────────────────────

#[test]
fn local_sum_type_shadows_dep_sum_type_of_the_same_name() {
    // The issue's reproduction. Both sums also share the variant name
    // `Red`, so the entry's `Colour.Red` must route to the entry's own
    // constructor (2) and not to the dependency's.
    let entry_src = r#"
module Entry
    intent = "a local sum type shadows the dependency's same-named one"
    depends [Palette]

type Colour
    Green
    Red

fn code(c: Colour) -> Int
    match c
        Colour.Green -> 1
        Colour.Red -> 2

fn main() -> Int
    code(Colour.Red)
"#;
    let palette_src = r#"
module Palette
    intent = "dependency declaring the shadowed name"
    exposes [Colour]
    depends []

type Colour
    Red
    Blue
"#;
    let result = run_int_multi(entry_src, &[("Palette", palette_src)]);
    assert_eq!(
        result, 2,
        "expected the entry module's own `Colour.Red` to number 2, \
         matching the VM and the self-host"
    );
}

#[test]
fn local_sum_type_shadowing_keeps_the_dep_type_reachable_by_module_path() {
    // Identity check for the same shadowing pair: the entry uses its own
    // `Colour` AND the dependency's, the latter through the module path.
    // A single merged registry slot would answer both with one layout.
    // Disjoint variant names on the two sums, so nothing but the type
    // identity can carry the result: 2 + 20 = 22.
    let entry_src = r#"
module Entry
    intent = "both the shadowing type and the shadowed one are used"
    depends [Palette]

type Colour
    Green
    Red

fn code(c: Colour) -> Int
    match c
        Colour.Green -> 1
        Colour.Red -> 2

fn main() -> Int
    code(Colour.Red) + Palette.weight(Palette.Colour.Blue)
"#;
    let palette_src = r#"
module Palette
    intent = "dependency declaring the shadowed name"
    exposes [Colour, weight]
    depends []

type Colour
    Cyan
    Blue

fn weight(c: Colour) -> Int
    match c
        Colour.Cyan -> 10
        Colour.Blue -> 20
"#;
    let result = run_int_multi(entry_src, &[("Palette", palette_src)]);
    assert_eq!(
        result, 22,
        "expected the entry `Colour.Red` (2) plus the dependency's \
         `Palette.Colour.Blue` (20); a divergence means the two \
         same-named sums share one wasm-gc slot"
    );
}

#[test]
fn local_sum_type_shadowing_allows_a_qualified_signature_and_pattern() {
    // The entry module names the shadowed dependency type in a signature
    // and in match patterns. That reaches the fn-signature prefix strip
    // and the pattern head, not just the constructor expression.
    let entry_src = r#"
module Entry
    intent = "entry-side qualified signature over a shadowed dep type"
    depends [Palette]

type Colour
    Green
    Red

fn code(c: Colour) -> Int
    match c
        Colour.Green -> 1
        Colour.Red -> 2

fn depCode(c: Palette.Colour) -> Int
    match c
        Palette.Colour.Cyan -> 10
        Palette.Colour.Blue -> 20

fn main() -> Int
    code(Colour.Red) + depCode(Palette.Colour.Blue)
"#;
    let palette_src = r#"
module Palette
    intent = "dependency declaring the shadowed name"
    exposes [Colour]
    depends []

type Colour
    Cyan
    Blue
"#;
    let result = run_int_multi(entry_src, &[("Palette", palette_src)]);
    assert_eq!(
        result, 22,
        "expected 2 from the entry `Colour` plus 20 from the qualified \
         `Palette.Colour`"
    );
}

#[test]
fn single_module_sum_type_control() {
    // Control: one module, one sum type, no dependency. Pins that the
    // shadowing fix is not paid for by dropping plain sum-type support.
    let source = r#"module Tmp
    intent = "single-module sum type control"
    depends []

type Colour
    Green
    Red

fn code(c: Colour) -> Int
    match c
        Colour.Green -> 1
        Colour.Red -> 2

fn main() -> Int
    code(Colour.Red)
"#;
    assert_eq!(
        run_int(source),
        2,
        "single-module sum type must still compile"
    );
}

#[test]
fn two_modules_sum_types_without_a_name_collision_control() {
    // Control: two modules, a sum type in each, different bare names —
    // the dependency's type keeps its bare spelling through flatten.
    // Pins that the fix is not paid for by canonicalising every dep type
    // (which would break the bare-name lookups this path relies on).
    let entry_src = r#"
module Entry
    intent = "two modules, sum types, no name collision"
    depends [Palette]

type Colour
    Green
    Red

fn code(c: Colour) -> Int
    match c
        Colour.Green -> 1
        Colour.Red -> 2

fn main() -> Int
    code(Colour.Red)
"#;
    let palette_src = r#"
module Palette
    intent = "dependency with an unrelated sum type"
    exposes [Shade]
    depends []

type Shade
    Dark
    Light
"#;
    let result = run_int_multi(entry_src, &[("Palette", palette_src)]);
    assert_eq!(
        result, 2,
        "two modules whose sum types do not share a name must still compile"
    );
}

#[test]
fn local_sum_type_shadowing_carries_through_a_string_returning_fn() {
    // The `String` payload path reaches a different set of emit sites
    // than the `Int` one (string literal segments, `$string` array slots).
    // `String.len("red")` is 3.
    let entry_src = r#"
module Entry
    intent = "shadowed sum type feeding a String-returning fn"
    depends [Palette]

type Colour
    Green
    Red

fn label(c: Colour) -> String
    match c
        Colour.Green -> "green"
        Colour.Red -> "red"

fn main() -> Int
    String.len(label(Colour.Red))
"#;
    let palette_src = r#"
module Palette
    intent = "dependency declaring the shadowed name"
    exposes [Colour]
    depends []

type Colour
    Red
    Blue
"#;
    let result = run_int_multi(entry_src, &[("Palette", palette_src)]);
    assert_eq!(
        result, 3,
        "expected `String.len(\"red\")` = 3 from the entry module's own \
         `Colour.Red` arm"
    );
}

#[test]
fn local_record_type_shadows_dep_record_type_of_the_same_name() {
    // Record counterpart of the sum-type shadowing pair. The two `Box`
    // records carry different field names, so a merged registry slot
    // surfaces as a missing field list rather than a wrong number.
    let entry_src = r#"
module Entry
    intent = "a local record shadows the dependency's same-named one"
    depends [Store]

record Box
    value: Int

fn read(b: Box) -> Int
    b.value

fn main() -> Int
    read(Box(value = 2)) + Store.unwrap(Store.Box(weight = 20))
"#;
    let store_src = r#"
module Store
    intent = "dependency declaring the shadowed record name"
    exposes [Box, unwrap]
    depends []

record Box
    weight: Int

fn unwrap(b: Box) -> Int
    b.weight
"#;
    let result = run_int_multi(entry_src, &[("Store", store_src)]);
    assert_eq!(
        result, 22,
        "expected the entry `Box(value = 2)` plus the dependency's \
         `Store.Box(weight = 20)`; the two same-named records must keep \
         separate field lists"
    );
}

// Packed-sequence layouts must resolve by EXACT type name. The entry
// module's gated `Octets` refinement earns a packed u8 layout; two dep
// modules declare plain ungated `record Octets { values: List<Int> }`
// records whose bare name collides, so flatten renames them to
// `Left.Octets` / `Right.Octets`. Those renamed records are unrelated
// to the refinement and must keep the ordinary boxed representation.
// A qualified→bare fallback in the packed lookups would hand
// `Left.Octets` the entry refinement's u8 layout, and the ungated
// construct below would silently truncate 1000 to 1000 mod 256 = 232.
#[test]
fn cross_module_same_bare_name_record_does_not_inherit_packed_layout() {
    let entry_src = r#"
module Entry
    intent = "packed layout must not leak to same-bare-name dep types"
    depends [Left, Right]

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")

fn main() -> Int
    plain = Left.Octets(values = [1000])
    match plain.values
        [head, .._] -> head
        [] -> 0
"#;
    let left_src = r#"
module Left
    intent = "plain record sharing the refinement's bare name"
    exposes [Octets]
    depends []

record Octets
    values: List<Int>
"#;
    let right_src = r#"
module Right
    intent = "second declarer forcing the bare-name collision rename"
    exposes [Octets]
    depends []

record Octets
    values: List<Int>
"#;
    let result = run_int_multi(entry_src, &[("Left", left_src), ("Right", right_src)]);
    assert_eq!(
        result, 1000,
        "expected the ungated Left.Octets record to stay boxed and return \
         1000; 232 means it inherited the entry refinement's packed u8 \
         layout through a qualified→bare name fallback and truncated"
    );
}

// Availability counterpart of the exact-name test above: an ENTRY-side
// local-binding annotation may spell a dep type qualified
// (`o: Dep.Octets = ...`) even though flatten keeps the sole-declarer
// dep `TypeDef` bare. The pre-flatten typechecker stamps the binding
// slot with the qualified name, and that stamp survives into codegen,
// so the packed lookups must accept `Dep.Octets` as an alias for the
// unique `Octets` layout. The alias is identity-correct ONLY because
// `Dep` is the sole declarer — a collision-renamed spelling must keep
// declining (previous test).
#[test]
fn entry_qualified_annotation_over_sole_declarer_packed_dep_type() {
    let entry_src = r#"
module Entry
    intent = "qualified local annotation over a sole-declarer packed dep type"
    depends [Dep]

fn firstValue(o: Dep.Octets) -> Int
    match o.values
        [head, .._] -> head
        [] -> 0 - 1

fn run() -> Result<Int, String>
    o: Dep.Octets = Dep.fromList([200])
    Result.Ok(firstValue(o))

fn main() -> Int
    match run()
        Result.Ok(n) -> n
        Result.Err(_) -> 0 - 2
"#;
    let dep_src = r#"
module Dep
    intent = "sole-declarer gated Octets refinement"
    exposes [Octets, fromList]
    depends []

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")
"#;
    let result = run_int_multi(entry_src, &[("Dep", dep_src)]);
    assert_eq!(
        result, 200,
        "expected the qualified `o: Dep.Octets` binding to resolve the \
         unique packed dep layout and return 200"
    );
}

// Fail-closed exclusion branch of the alias derivation: when the ENTRY
// module declares the same bare type name a dep declares, the qualified
// dep spelling gets NO alias — post-flatten two `TypeDef`s would share
// the bare key, so aliasing `Dep.Octets` → `Octets` could hand the entry
// type's layout facts to the dep spelling (or vice versa). The dep type
// is still the sole declarer AMONG DEPS, which is exactly why the
// entry-shadow check must be its own condition; an unshadowed sibling
// dep type keeps its alias (positive control).
#[test]
fn entry_shadowed_bare_name_derives_no_alias() {
    let entry_src = r#"
module Entry
    intent = "entry-shadowed bare name must not get a qualified alias"
    depends [Dep]

record Octets
    values: List<Int>

fn main() -> Int
    7
"#;
    let dep_src = r#"
module Dep
    intent = "dep declaring a shadowed and an unshadowed type"
    exposes [Octets, Other]
    depends []

record Octets
    values: List<Int>

record Other
    value: Int
"#;
    let mut entry_items = parse_source(entry_src).expect("entry parse");
    let loaded = aver::source::LoadedModule {
        dep_name: "Dep".to_string(),
        items: parse_source(dep_src).expect("dep parse"),
        path: std::path::PathBuf::from("Dep.av"),
    };
    let modules = vec![aver::codegen::ModuleInfo::from_loaded(&loaded)];
    let capabilities = aver::capability::CapabilityRegistry::default();
    let type_aliases = aver::codegen::wasm_gc::flatten_multimodule(
        &mut entry_items,
        &modules,
        &capabilities,
        aver::codegen::wasm_gc::CapabilityFunctionSurface::Runtime,
    );
    assert!(
        !type_aliases.contains_key("Dep.Octets"),
        "entry declares its own `Octets`, so the qualified dep spelling \
         must decline fail-closed (no alias registered): {type_aliases:?}"
    );
    assert_eq!(
        type_aliases.get("Dep.Other").map(String::as_str),
        Some("Other"),
        "the unshadowed sibling dep type keeps its identity-preserving \
         alias: {type_aliases:?}"
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
fn map_iteration_order_matches_the_canonical_string_key_order() {
    let source = r#"module MapOrder
    intent = "Map iteration has one canonical order on every backend."

fn build() -> Map<String, Int>
    m0 = Map.set({}, "z", 1)
    m1 = Map.set(m0, "alpha", 2)
    m2 = Map.set(m1, "m", 3)
    m3 = Map.set(m2, "k", 4)
    m4 = Map.set(m3, "epsilon", 5)
    m5 = Map.set(m4, "theta", 6)
    m6 = Map.set(m5, "beta", 7)
    m7 = Map.set(m6, "iota", 8)
    m8 = Map.set(m7, "delta", 9)
    Map.set(m8, "gamma", 10)

fn main() -> Int
    m = build()
    keysOk = Map.keys(m) == ["alpha", "beta", "delta", "epsilon", "gamma", "iota", "k", "m", "theta", "z"]
    valuesOk = Map.values(m) == [2, 7, 9, 5, 10, 8, 4, 3, 6, 1]
    entriesOk = Map.entries(m) == [("alpha", 2), ("beta", 7), ("delta", 9), ("epsilon", 5), ("gamma", 10), ("iota", 8), ("k", 4), ("m", 3), ("theta", 6), ("z", 1)]
    match Bool.and(keysOk, Bool.and(valuesOk, entriesOk))
        true -> 1
        false -> 0
"#;

    #[cfg(feature = "runtime")]
    assert_eq!(run_int_on_vm(source), 1, "VM canonical order changed");
    assert_eq!(
        run_int(source),
        1,
        "wasm-gc map iteration diverged from the canonical key order"
    );
}

#[test]
fn map_iteration_order_compares_record_keys_by_field_name() {
    let source = r#"module Tmp
    intent = "record map keys have backend-independent order"
    depends []

record Key
    z: Int
    a: String

fn build() -> Map<Key, Int>
    m1 = Map.set({}, Key(z = 1, a = "b"), 10)
    m2 = Map.set(m1, Key(z = 9, a = "a"), 20)
    Map.set(m2, Key(z = 2, a = "b"), 30)

fn main() -> Int
    m = build()
    keysOk = Map.keys(m) == [Key(z = 9, a = "a"), Key(z = 1, a = "b"), Key(z = 2, a = "b")]
    valuesOk = Map.values(m) == [20, 10, 30]
    entriesOk = Map.entries(m) == [(Key(z = 9, a = "a"), 20), (Key(z = 1, a = "b"), 10), (Key(z = 2, a = "b"), 30)]
    match keysOk
        false -> 0
        true -> match valuesOk
            false -> 0
            true -> match entriesOk
                false -> 0
                true -> 1
"#;
    #[cfg(feature = "runtime")]
    assert_eq!(run_int_on_vm(source), 1, "VM reference order changed");
    assert_eq!(run_int(source), 1, "wasm-gc must match VM record order");
}

#[test]
fn map_iteration_order_compares_variants_by_constructor_then_payload() {
    let source = r#"module Tmp
    intent = "variant map keys have backend-independent order"
    depends []

type Key
    Zebra(Int)
    Alpha(Int)
    Middle

fn build() -> Map<Key, Int>
    m1 = Map.set({}, Key.Zebra(0), 40)
    m2 = Map.set(m1, Key.Alpha(2), 20)
    m3 = Map.set(m2, Key.Middle, 30)
    Map.set(m3, Key.Alpha(1), 10)

fn main() -> Int
    m = build()
    keysOk = Map.keys(m) == [Key.Alpha(1), Key.Alpha(2), Key.Middle, Key.Zebra(0)]
    valuesOk = Map.values(m) == [10, 20, 30, 40]
    match Bool.and(keysOk, valuesOk)
        true -> 1
        false -> 0
"#;
    #[cfg(feature = "runtime")]
    assert_eq!(run_int_on_vm(source), 1, "VM reference order changed");
    assert_eq!(run_int(source), 1, "wasm-gc must match VM variant order");
}

#[test]
fn map_iteration_order_compares_tuple_keys_componentwise() {
    let source = r#"module Tmp
    intent = "tuple map keys have backend-independent order"
    depends []

fn build() -> Map<Tuple<Int, String>, Int>
    m1 = Map.set({}, (10, "a"), 40)
    m2 = Map.set(m1, (2, "b"), 30)
    m3 = Map.set(m2, (1, "z"), 10)
    Map.set(m3, (2, "a"), 20)

fn main() -> Int
    m = build()
    keysOk = Map.keys(m) == [(1, "z"), (2, "a"), (2, "b"), (10, "a")]
    valuesOk = Map.values(m) == [10, 20, 30, 40]
    match Bool.and(keysOk, valuesOk)
        true -> 1
        false -> 0
"#;
    #[cfg(feature = "runtime")]
    assert_eq!(run_int_on_vm(source), 1, "VM reference order changed");
    assert_eq!(run_int(source), 1, "wasm-gc must match VM tuple order");
}

#[test]
fn map_iteration_order_compares_bool_option_and_result_keys_by_language_tags() {
    let source = r#"module Tmp
    intent = "generic carrier map keys have backend-independent order"
    depends []

fn noneInt() -> Option<Int>
    Option.None

fn okInt(value: Int) -> Result<Int, String>
    Result.Ok(value)

fn errInt(message: String) -> Result<Int, String>
    Result.Err(message)

fn boolMap() -> Map<Bool, Int>
    Map.set(Map.set({}, true, 20), false, 10)

fn optionMap() -> Map<Option<Int>, Int>
    m1 = Map.set({}, Option.Some(2), 30)
    m2 = Map.set(m1, noneInt(), 10)
    Map.set(m2, Option.Some(1), 20)

fn resultMap() -> Map<Result<Int, String>, Int>
    m1 = Map.set({}, errInt("a"), 30)
    m2 = Map.set(m1, okInt(2), 20)
    Map.set(m2, okInt(1), 10)

fn main() -> Int
    boolsOk = Map.values(boolMap()) == [10, 20]
    optionsOk = Map.values(optionMap()) == [10, 20, 30]
    resultsOk = Map.values(resultMap()) == [10, 20, 30]
    match Bool.and(boolsOk, Bool.and(optionsOk, resultsOk))
        true -> 1
        false -> 0
"#;
    #[cfg(feature = "runtime")]
    assert_eq!(run_int_on_vm(source), 1, "VM reference order changed");
    assert_eq!(
        run_int(source),
        1,
        "wasm-gc must match VM Option/Result order"
    );
}

#[test]
fn map_iteration_order_compares_list_keys_lexicographically() {
    let source = r#"module Tmp
    intent = "list map keys have backend-independent order"
    depends []

fn noInts() -> List<Int>
    []

fn build() -> Map<List<Int>, Int>
    m1 = Map.set({}, [2], 50)
    m2 = Map.set(m1, [1, 2], 40)
    m3 = Map.set(m2, noInts(), 10)
    m4 = Map.set(m3, [1, 1], 30)
    Map.set(m4, [1], 20)

fn main() -> Int
    m = build()
    keysOk = Map.keys(m) == [noInts(), [1], [1, 1], [1, 2], [2]]
    valuesOk = Map.values(m) == [10, 20, 30, 40, 50]
    emptyLookupOk = Option.withDefault(Map.get(m, noInts()), -1) == 10
    match Bool.and(keysOk, Bool.and(valuesOk, emptyLookupOk))
        true -> 1
        false -> 0
"#;
    #[cfg(feature = "runtime")]
    assert_eq!(run_int_on_vm(source), 1, "VM reference order changed");
    assert_eq!(run_int(source), 1, "wasm-gc must match VM list order");
}

#[test]
fn map_iteration_order_compares_proof_packed_bytes_lexicographically() {
    let entry = r#"module Tmp
    intent = "packed Bytes map keys have backend-independent order"
    depends [Bytes]

fn build() -> Map<Bytes, Int>
    k2 = Bytes.fromList([2])
    k11 = Bytes.fromList([1, 1])
    k0 = Bytes.fromList([])
    k1 = Bytes.fromList([1])
    m1 = Map.set({}, k2, 40)
    m2 = Map.set(m1, k11, 30)
    m3 = Map.set(m2, k0, 10)
    Map.set(m3, k1, 20)

fn main() -> Int
    match Map.values(build()) == [10, 20, 30, 40]
        true -> 1
        false -> 0
"#;
    assert_eq!(
        run_int_multi(entry, &[("Bytes", include_str!("../stdlib/bytes.av"))]),
        1,
        "wasm-gc must compare packed Bytes by their octets"
    );
}

/// Sixteen thousand keys into a table that starts at sixteen buckets:
/// eleven doublings, each one rehashing everything inserted so far. `len`
/// counting them all is the cheapest statement that nothing was
/// dropped or duplicated on the way through.
#[test]
fn map_filled_across_eleven_doublings_counts_every_key() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map filled across eleven wasm-gc table doublings"
    depends []

fn fill(n: Int, acc: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, n, n))

fn main() -> Int
    m = fill(16384, {})
    Map.len(m)
"#
        ),
        16384
    );
}

/// The key that used to have nowhere to go. A map with a fixed 16384
/// buckets trapped on the 16385th distinct key — the table could not
/// grow, so the entry had no slot. The table grows now, so this is an
/// ordinary insert and the count is the count.
#[test]
fn map_past_the_old_fixed_capacity_keeps_inserting() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "map one key past the old fixed wasm-gc capacity"
    depends []

fn fill(n: Int, acc: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, n, n))

fn main() -> Int
    m = fill(16385, {})
    Map.len(m) + Option.withDefault(Map.get(m, 16385), -1)
"#
        ),
        16385 + 16385
    );
}

/// Reads on a table that has been through eleven doublings. The rehash
/// rebuilds every probe run under a wider mask, so a hit has to be
/// found wherever it landed last and a miss has to stop.
///
/// This used to be the full-table read: 16384 keys filled a table of
/// 16384 buckets, and the miss was answered by the wrap guard. A map
/// never fills now, so the guard is unreachable and what is left to
/// pin is that reads survive the rehashing.
#[test]
fn map_lookup_after_many_doublings_finds_hits_and_reports_misses() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "lookup in a map that has doubled eleven times"
    depends []

fn fill(n: Int, acc: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, n, n))

fn main() -> Int
    m = fill(16384, {})
    hit = Option.withDefault(Map.get(m, 7), -1)
    miss = Option.withDefault(Map.get(m, 99999), -1)
    hit + miss
"#
        ),
        6
    );
}

/// The test above reads the map through `Option.withDefault(Map.get(…))`,
/// which the emitter fuses into the `get_or_default` helper — so it
/// never touches the `get` helper that actually returns an `Option`.
/// Here the `Option` has to exist: it is returned across a function
/// boundary and then matched. Same grown table, same absent key, and
/// the answer is still `None`.
#[test]
fn map_grown_table_miss_through_an_option_returning_call_is_none() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "unfused Option lookup in a map that has doubled eleven times"
    depends []

fn fill(n: Int, acc: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, n, n))

fn lookup(m: Map<Int, Int>, k: Int) -> Option<Int>
    Map.get(m, k)

fn main() -> Int
    m = fill(16384, {})
    hit = lookup(m, 7)
    miss = lookup(m, 99999)
    hitValue = match hit
        Option.Some(v) -> v
        Option.None -> -1
    missValue = match miss
        Option.Some(v) -> v
        Option.None -> -1
    hitValue + missValue
"#
        ),
        6
    );
}

/// `Map.has` is the third lookup helper, `get_pair` — it answers from
/// the same probe loop and drops the value. A grown table must still
/// report membership honestly in both directions.
#[test]
fn map_grown_table_membership_answers_both_ways() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "membership in a map that has doubled eleven times"
    depends []

fn fill(n: Int, acc: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, n, n))

fn main() -> Int
    m = fill(16384, {})
    match Bool.and(Map.has(m, 7), Bool.not(Map.has(m, 99999)))
        true -> 1
        false -> 0
"#
        ),
        1
    );
}

/// Overwriting a key that is already there must not change the count.
/// The insert helper tests the load factor before it probes, so it
/// cannot yet know the key is present and may grow the table for an
/// entry that never gets added — the count is what proves it did not
/// also leave a phantom entry behind while it was there.
#[test]
fn map_update_of_a_present_key_in_a_grown_table_writes_the_value() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "update an existing key in a map that has doubled eleven times"
    depends []

fn fill(n: Int, acc: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, n, n))

fn main() -> Int
    m = fill(16384, {})
    updated = Map.set(m, 7, 42)
    Map.len(updated) + Option.withDefault(Map.get(updated, 7), -1)
"#
        ),
        16426
    );
}

/// Removal from a table that has been through eleven doublings. The
/// removed key must be gone and every other key still findable,
/// including the one inserted last and the one next to the hole.
///
/// This test used to fill the table exactly and so drove the shift
/// scan's wrap guard, which was the only witness that EXECUTED it.
/// Growth took that away: at 16384 entries the table holds 32768
/// buckets, so the scan stops on a null slot like any other. The
/// guard is unreachable from Aver source now, and its remaining job
/// is to bound the scan if the growth above it ever breaks.
#[test]
fn map_remove_from_a_grown_table_keeps_every_other_key() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "remove from a map that has doubled eleven times"
    depends []

fn fill(n: Int, acc: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, n, n))

fn main() -> Int
    m = fill(16384, {})
    smaller = Map.remove(m, 7)
    gone = Option.withDefault(Map.get(smaller, 7), -1)
    kept = Option.withDefault(Map.get(smaller, 8), -1)
    last = Option.withDefault(Map.get(smaller, 16384), -1)
    Map.len(smaller) + gone + kept + last
"#
        ),
        16383 + -1 + 8 + 16384
    );
}

/// Two keys that share a bucket sit in one probe run, and removing the
/// first must not cut the run in half. Small `Int` keys hash to
/// themselves and the bucket is `hash & (cap - 1)`, so `1` and `16385`
/// collide at every capacity a three-entry map can have — 16384 is a
/// multiple of all of them. The map here never grows (three entries in
/// sixteen buckets), so inserting `1`, `2`, `16385` in that order puts
/// them in slots 1, 2, 3. `16385` is reachable only by probing through
/// slot 1, so removing `1` has to walk the rest of the run and pull
/// `16385` back into the hole it left.
///
/// The answer packs the count and three lookups into one Int —
/// `len * 1000 + get(16385) * 10 + get(2) + get(1)`, with `-1` for a
/// miss: two entries left, `16385 -> 33`, `2 -> 22`, `1` gone.
const REMOVE_SHARED_BUCKET_SRC: &str = r#"module Tmp
    intent = "remove a key that another key probed past"
    depends []

fn build() -> Map<Int, Int>
    withOne = Map.set({}, 1, 11)
    withTwo = Map.set(withOne, 2, 22)
    Map.set(withTwo, 16385, 33)

fn main() -> Int
    m = Map.remove(build(), 1)
    shared = Option.withDefault(Map.get(m, 16385), -1)
    neighbour = Option.withDefault(Map.get(m, 2), -1)
    removed = Option.withDefault(Map.get(m, 1), -1)
    Map.len(m) * 1000 + shared * 10 + neighbour + removed
"#;

#[test]
fn map_remove_keeps_a_key_that_probed_past_the_removed_bucket() {
    assert_eq!(
        run_int(REMOVE_SHARED_BUCKET_SRC),
        2 * 1000 + 33 * 10 + 22 + -1
    );
}

/// The same question over two probe runs, one of which crosses the end
/// of the table. Ten entries stay inside the sixteen buckets a map
/// starts with, so the table never grows here and the buckets are
/// `key & 15`. After the ten inserts the run in slots 5..10 is:
/// 5(home 5), 6(home 6), 16389(home 5), 7(home 7, displaced), 32773
/// (home 5), 8(home 8, displaced). Removing `5` reads one entry that
/// stays put (`6`, at its home) and then a run of four that must each
/// move — so the hole travels several slots and entries are pulled back
/// across occupied ones. The wrap run has the same shape: one stay,
/// then moves. Not exercised by any witness here: a STAY evaluated
/// after the hole has already moved (gap > 1 at the comparison); that
/// case rests on the loop's invariant argument, not on a test.
///
/// The second run starts at the last bucket: `16383` and `32767` hash to
/// bucket 15, `16384` and `32768` to bucket 0, and inserting them as
/// `16383`, `16384`, `32767`, `32768` puts them in slots 15, 0, 1, 2
/// — a run that wraps. Removing `16383` empties the last slot, and
/// `32767` is only reachable through it.
///
/// The answer is `kept * 100 + missing * 10 + len`: all eight survivors
/// found under their own value, both removed keys absent, count 8.
const REMOVE_CLUSTER_SRC: &str = r#"module Tmp
    intent = "remove across a colliding run and across the wrap point"
    depends []

fn hit(m: Map<Int, Int>, k: Int) -> Int
    match Option.withDefault(Map.get(m, k), -1) == k
        true  -> 1
        false -> 0

fn absent(m: Map<Int, Int>, k: Int) -> Int
    match Map.has(m, k)
        true  -> 0
        false -> 1

fn build() -> Map<Int, Int>
    s1 = Map.set({}, 5, 5)
    s2 = Map.set(s1, 6, 6)
    s3 = Map.set(s2, 16389, 16389)
    s4 = Map.set(s3, 7, 7)
    s5 = Map.set(s4, 32773, 32773)
    s6 = Map.set(s5, 8, 8)
    s7 = Map.set(s6, 16383, 16383)
    s8 = Map.set(s7, 16384, 16384)
    s9 = Map.set(s8, 32767, 32767)
    Map.set(s9, 32768, 32768)

fn main() -> Int
    withoutFive = Map.remove(build(), 5)
    m = Map.remove(withoutFive, 16383)
    keptLow = hit(m, 6) + hit(m, 7) + hit(m, 8) + hit(m, 16389) + hit(m, 32773)
    keptWrap = hit(m, 16384) + hit(m, 32767) + hit(m, 32768)
    missing = absent(m, 5) + absent(m, 16383)
    (keptLow + keptWrap) * 100 + missing * 10 + Map.len(m)
"#;

#[test]
fn map_remove_keeps_every_survivor_of_a_colliding_run() {
    assert_eq!(run_int(REMOVE_CLUSTER_SRC), 8 * 100 + 2 * 10 + 8);
}

/// The two witnesses above stay inside the initial sixteen buckets, so
/// they say nothing about removal from a table that has grown. These do.
/// A grow rehashes every entry under a wider mask and resets the
/// displacements the old mask built up, which is exactly the state the
/// backwards-shift scan reads — so the collision runs have to be built
/// AFTER the growth, at the capacity the map actually ends up with.
///
/// Thirty `Int` keys leave a map at 64 buckets: it doubles at 12 and at
/// 24 entries (three quarters of 16 and of 32), and 30 is below the 48
/// that would double it again. Keys `1..30` sit at their own buckets
/// `1..30`, leaving `0` and `31..63` free to build runs in.
///
/// `40`, `104` and `168` are `k`, `k + 64` and `k + 2 * 64`: one home
/// bucket, three entries, slots 40, 41, 42. Removing `40` must pull both
/// of the others back — a two-step shift where the hole travels.
const REMOVE_AFTER_RESIZE_SRC: &str = r#"module Tmp
    intent = "remove a key that another key probed past, after the table grew"
    depends []

fn hit(m: Map<Int, Int>, k: Int) -> Int
    match Option.withDefault(Map.get(m, k), -1) == k
        true  -> 1
        false -> 0

fn absent(m: Map<Int, Int>, k: Int) -> Int
    match Map.has(m, k)
        true  -> 0
        false -> 1

fn fill(n: Int, acc: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, n, n))

fn build() -> Map<Int, Int>
    grown = fill(30, {})
    s1 = Map.set(grown, 40, 40)
    s2 = Map.set(s1, 104, 104)
    Map.set(s2, 168, 168)

fn main() -> Int
    m = Map.remove(build(), 40)
    kept = hit(m, 104) + hit(m, 168) + hit(m, 1) + hit(m, 30)
    missing = absent(m, 40)
    kept * 100 + missing * 10 + Map.len(m)
"#;

#[test]
fn map_remove_after_a_resize_keeps_the_keys_that_probed_past() {
    assert_eq!(run_int(REMOVE_AFTER_RESIZE_SRC), 4 * 100 + 10 + 32);
}

/// The same, over the end of the grown table. `63` is the last bucket at
/// 64 and `127` is `63 + 64`, so `127` wraps into slot 0 — which the
/// growth left free, since the thirty keys `1..30` claimed slots `1..30`
/// and nothing claimed `0`. Removing `63` empties the last slot, and
/// `127` is reachable only through it, so the shift scan has to cross
/// the wrap point to pull it back.
///
/// It then has to STOP pulling: `1` sits at its own bucket and moving it
/// into the hole at slot 0 would make it unfindable. That is the case
/// the run before the growth could not stage.
const WRAP_AFTER_RESIZE_SRC: &str = r#"module Tmp
    intent = "remove across the wrap point of a table that grew"
    depends []

fn hit(m: Map<Int, Int>, k: Int) -> Int
    match Option.withDefault(Map.get(m, k), -1) == k
        true  -> 1
        false -> 0

fn absent(m: Map<Int, Int>, k: Int) -> Int
    match Map.has(m, k)
        true  -> 0
        false -> 1

fn fill(n: Int, acc: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, n, n))

fn build() -> Map<Int, Int>
    grown = fill(30, {})
    s1 = Map.set(grown, 63, 63)
    Map.set(s1, 127, 127)

fn main() -> Int
    m = Map.remove(build(), 63)
    kept = hit(m, 127) + hit(m, 1) + hit(m, 2) + hit(m, 30)
    missing = absent(m, 63)
    kept * 100 + missing * 10 + Map.len(m)
"#;

#[test]
fn map_remove_after_a_resize_keeps_a_key_that_wrapped_past_the_end() {
    assert_eq!(run_int(WRAP_AFTER_RESIZE_SRC), 4 * 100 + 10 + 31);
}

/// Growth has to rehash, not copy: a key's bucket is `hash & (cap - 1)`,
/// so doubling `cap` exposes one more hash bit and moves about half the
/// entries. A grow that copied slot for slot would leave every entry
/// findable only at the bucket it had under the old mask, which is not
/// where a probe under the new one looks.
///
/// These thirty keys are all multiples of sixteen, so at the starting
/// capacity every one of them hashes to bucket 0 — one probe run that
/// reaches twelve of the sixteen starting slots before the load check
/// grows the table (it is never full; that is the point), then two more
/// masks that each split the run in half again on the way to 64 buckets. Summing what
/// comes back out reads every entry, and the sum only comes to
/// `1 + … + 30` if each key kept its own value across both rebuilds.
const REHASH_COLLIDING_RUN_SRC: &str = r#"module Tmp
    intent = "grow a table whose keys all start in one bucket"
    depends []

fn fill(n: Int, acc: Map<Int, Int>) -> Map<Int, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, n * 16, n))

fn total(m: Map<Int, Int>, n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> total(m, n - 1, acc + Option.withDefault(Map.get(m, n * 16), -1000))

fn main() -> Int
    m = fill(30, {})
    total(m, 30, 0) * 100 + Map.len(m)
"#;

#[test]
fn map_growth_rehashes_a_run_that_shared_one_bucket() {
    assert_eq!(run_int(REHASH_COLLIDING_RUN_SRC), 465 * 100 + 30);
}

/// The VM is the oracle: the same program has to give the same answer on
/// both backends. The first two witnesses were wasm-gc-only divergences —
/// the VM never lost a key — so pin the pair, not just the constant. The
/// resize witnesses join them because the same reasoning applies: only
/// the wasm-gc map has buckets at all, so only it can rehash them wrong.
#[test]
#[cfg(feature = "runtime")]
fn map_remove_witnesses_answer_the_vm() {
    for src in [
        REMOVE_SHARED_BUCKET_SRC,
        REMOVE_CLUSTER_SRC,
        REMOVE_AFTER_RESIZE_SRC,
        WRAP_AFTER_RESIZE_SRC,
        REHASH_COLLIDING_RUN_SRC,
    ] {
        assert_eq!(run_int(src), run_int_on_vm(src));
    }
}

/// The shape the report was about: an index of tens of thousands of
/// String keys, which is what a map is for and what the fixed 16384
/// buckets made impossible. A hundred thousand distinct keys is
/// fourteen doublings from sixteen, and every one of them rehashes
/// every entry inserted so far — so this also states that the
/// amortised cost stays affordable, since a rebuild that was not
/// amortised at this size would not finish.
///
/// The spot checks are the first key in, the last, one from the middle
/// and one that was never there.
#[test]
fn map_of_a_hundred_thousand_string_keys_holds_all_of_them() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "a hundred thousand distinct String keys on wasm-gc"
    depends []

fn fill(n: Int, acc: Map<String, Int>) -> Map<String, Int>
    match n
        0 -> acc
        _ -> fill(n - 1, Map.set(acc, "key-{n}", n))

fn main() -> Int
    m = fill(100000, {})
    first = Option.withDefault(Map.get(m, "key-1"), -1)
    middle = Option.withDefault(Map.get(m, "key-50000"), -1)
    last = Option.withDefault(Map.get(m, "key-100000"), -1)
    absent = Option.withDefault(Map.get(m, "key-100001"), -1)
    Map.len(m) + first + middle + last + absent
"#
        ),
        100000 + 1 + 50000 + 100000 + -1
    );
}

/// `Map.fromList` builds its map by calling the insert helper once per
/// pair, so it grows the same way — and it is where the cost of NOT
/// growing used to land hardest. It walked the list through the
/// clone-on-write insert, which copied every bucket for every pair:
/// one thousand pairs took over two minutes, and there was no size at
/// which it became reasonable. It builds its map in place now, because
/// the map is allocated in that helper's own body and nobody else can
/// see it until the walk is done.
///
/// Five thousand pairs is the statement. Under the copying insert this
/// test would take minutes; the answer it asserts is the correctness
/// half, and finishing at all is the other.
#[test]
fn map_from_list_builds_a_map_far_larger_than_the_old_capacity() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "Map.fromList over a long list of pairs"
    depends []

fn pairs(i: Int, n: Int, acc: List<Tuple<Int, Int>>) -> List<Tuple<Int, Int>>
    match i >= n
        true  -> acc
        false -> pairs(i + 1, n, List.prepend((i, i), acc))

fn main() -> Int
    m = Map.fromList(pairs(0, 5000, []))
    first = Option.withDefault(Map.get(m, 0), -1)
    last = Option.withDefault(Map.get(m, 4999), -1)
    absent = Option.withDefault(Map.get(m, 5000), -1)
    Map.len(m) + first + last + absent
"#
        ),
        5000 + 0 + 4999 + -1
    );
}

/// The insert helpers are named in the module so a stop inside one is
/// attributable, and the `--optimize` path reads that back to warn it
/// is about to strip it (`finalize_wasm_artifact`). Both halves of the
/// predicate matter: a map-using module carries the names, a map-free
/// one carries nothing to lose.
#[test]
fn capacity_helper_names_are_present_exactly_when_a_map_is() {
    let with_map = compile_bytes(
        r#"module Tmp
    intent = "map program carries helper names"
    depends []

fn build() -> Map<Int, Int>
    Map.set({}, 1, 1)

fn main() -> Int
    Map.len(build())
"#,
    );
    assert!(
        aver::codegen::wasm_gc::carries_capacity_helper_names(&with_map),
        "a program that instantiates a Map must name its insert helpers"
    );

    let without_map = compile_bytes(
        r#"module Tmp
    intent = "map-free program carries no helper names"
    depends []

fn main() -> Int
    1 + 1
"#,
    );
    assert!(
        !aver::codegen::wasm_gc::carries_capacity_helper_names(&without_map),
        "a program with no Map must not carry map helper names"
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
fn vector_new_honours_the_shared_materialization_boundary() {
    let limit = aver_rt::MAX_MATERIALIZED_VECTOR_ELEMENTS;
    let error = aver_rt::vector_size_error_message();
    let source = format!(
        r#"module Tmp
    intent = "vector materialization boundary"
    depends []

fn allocate(size: Int) -> Result<Vector<Int>, String>
    Vector.new(size, 0)

fn rejected() -> Int
    match allocate({})
        Result.Ok(_) -> -1
        Result.Err(message) -> match message == "{error}"
            true -> 0
            false -> -2

fn main() -> Int
    Vector.len(Vector.new({limit}, 0)) + rejected()
"#,
        limit + 1
    );
    assert_eq!(run_int(&source), limit as i64);
    #[cfg(feature = "runtime")]
    assert_eq!(run_int_on_vm(&source), limit as i64);
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

// Regression: a tuple-destructure binding that follows a wildcard must
// still be extracted. The original `emit_tuple_match` zipped the tuple
// fields against the (bindings-only) slot list positionally, which
// dropped `value` in `(_, value)` and left its slot at its zero
// default — `second((7, 42))` returned 0, not 42.
#[test]
fn tuple_match_binding_after_wildcard() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "tuple destructure binding after wildcard"
    depends []

fn second(pair: Tuple<Int, Int>) -> Int
    match pair
        (_, value) -> value

fn main() -> Int
    second((7, 42))
"#
        ),
        42
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

#[test]
fn recursive_string_access_uses_the_hidden_unicode_index() {
    let source = r#"module Tmp
    intent = "recursive charAt and slice share one hidden codepoint index"
    depends []

fn walk(text: String, pos: Int, seen: Int) -> Int
    match String.charAt(text, pos)
        Option.None -> seen + String.len(String.slice(text, 1, 3)) * 10
        Option.Some(_) -> walk(text, pos + 1, seen + 1)

fn main() -> Int
    walk("aą😀z", 0, 0)
"#;
    assert_eq!(run_int(source), 24);
    #[cfg(feature = "runtime")]
    assert_eq!(run_int_on_vm(source), 24);
}

#[test]
fn higher_order_fn_param_via_call_indirect() {
    // First-class `Fn`-param: `inc` is passed as a value (FnValue → i32
    // funcref-table index) and applied twice through the `f` param
    // (LocalSlot → call_indirect). Previously the wasm-gc backend
    // compiled this to an `unreachable` trap stub; it must now return
    // inc(inc(5)) = 7, matching the VM.
    assert_eq!(
        run_int(
            r#"
module ApplyTwice

fn inc(n: Int) -> Int
    n + 1

fn applyTwice(f: Fn(Int) -> Int, x: Int) -> Int
    f(f(x))

fn main() -> Int
    applyTwice(inc, 5)
"#
        ),
        7
    );
}

#[test]
fn higher_order_two_distinct_fn_values() {
    // Two distinct address-taken fns → two funcref-table entries; both
    // share one `call_indirect` functype (same `Fn(Int) -> Int` sig).
    // inc(10) + dbl(10) = 11 + 20 = 31.
    assert_eq!(
        run_int(
            r#"
module Combine

fn inc(n: Int) -> Int
    n + 1

fn dbl(n: Int) -> Int
    n + n

fn applyBoth(f: Fn(Int) -> Int, g: Fn(Int) -> Int, x: Int) -> Int
    f(x) + g(x)

fn main() -> Int
    applyBoth(inc, dbl, 10)
"#
        ),
        31
    );
}

// Boxed `match Int.div(a, b) { Ok / Err }` — the `Result<Int, String>` is
// consumed directly (not the fused `Result.withDefault` form), so the backend
// must materialise the concrete Result struct. Euclidean (flooring) quotient.
#[test]
fn boxed_int_div_ok() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "boxed Int.div Ok"
    depends []

fn sd(a: Int, b: Int) -> Int
    match Int.div(a, b)
        Result.Ok(v)  -> v
        Result.Err(_) -> 0 - 999

fn main() -> Int
    sd(0 - 7, 2)
"#
        ),
        -4
    );
}

// `b == 0` routes to `Result.Err("division by zero")` — the Err arm fires
// (returns its sentinel) rather than trapping.
#[test]
fn boxed_int_div_err_div_by_zero() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "boxed Int.div Err (div by zero)"
    depends []

fn sd(a: Int, b: Int) -> Int
    match Int.div(a, b)
        Result.Ok(v)  -> v
        Result.Err(_) -> 0 - 1

fn main() -> Int
    sd(7, 0)
"#
        ),
        -1
    );
}

// `Int = ℤ`: `i64::MIN / -1` is NOT an overflow Err — over ℤ it is the
// valid Ok Big `+2^63 = i64::MAX + 1`. The slice-2 semantics deleted the
// old wrapping/trap guard. To keep the assert in `run_int`'s Small range
// while exercising the Big quotient, subtract `i64::MAX`: `(+2^63) -
// i64::MAX == 1`, which demotes back to a Small the harness can read. A
// wrong (wrapping) quotient would not land on exactly 1.
#[test]
fn boxed_int_div_min_over_neg_one_is_big_ok() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "boxed Int.div i64::MIN / -1 = Ok Big +2^63"
    depends []

fn sd(a: Int, b: Int) -> Int
    match Int.div(a, b)
        Result.Ok(v)  -> v - 9223372036854775807
        Result.Err(_) -> 0 - 999

fn main() -> Int
    sd(0 - 9223372036854775807 - 1, 0 - 1)
"#
        ),
        1
    );
}

// Boxed `match Int.mod(a, b)` — Euclidean modulo on Ok (always `[0, |b|)`),
// Err on `b == 0`.
#[test]
fn boxed_int_mod_ok_and_err() {
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "boxed Int.mod Ok"
    depends []

fn sm(a: Int, b: Int) -> Int
    match Int.mod(a, b)
        Result.Ok(v)  -> v
        Result.Err(_) -> 0 - 999

fn main() -> Int
    sm(0 - 7, 2)
"#
        ),
        1
    );
    assert_eq!(
        run_int(
            r#"module Tmp
    intent = "boxed Int.mod Err"
    depends []

fn sm(a: Int, b: Int) -> Int
    match Int.mod(a, b)
        Result.Ok(v)  -> v
        Result.Err(_) -> 0 - 1

fn main() -> Int
    sm(7, 0)
"#
        ),
        -1
    );
}
// Scalar sibling of the packed-sequence exact-name test above: the
// carrier-i64 eligibility lookups must also resolve by EXACT type name.
// The entry module's gated `IntRange` carrier earns i64 erasure; the
// collision-renamed `Left.IntRange` / `Right.IntRange` dep records are
// unrelated plain records and must stay boxed (`$AverInt`). A
// qualified→bare fallback would erase `Left.IntRange` to i64 and the
// beyond-i64 construct below would trap in `__aint_to_i64_checked`
// instead of round-tripping through the bignum representation.
#[test]
fn cross_module_same_bare_name_record_does_not_inherit_carrier_i64_erasure() {
    let entry_src = r#"
module Entry
    intent = "carrier i64 erasure must not leak to same-bare-name dep types"
    depends [Left, Right]

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")

fn main() -> Int
    plain = Left.IntRange(value = 10000000000000000000)
    match plain.value == 10000000000000000000
        true -> 1
        false -> 0
"#;
    let left_src = r#"
module Left
    intent = "plain record sharing the carrier's bare name"
    exposes [IntRange]
    depends []

record IntRange
    value: Int
"#;
    let right_src = r#"
module Right
    intent = "second declarer forcing the bare-name collision rename"
    exposes [IntRange]
    depends []

record IntRange
    value: Int
"#;
    let result = run_int_multi(entry_src, &[("Left", left_src), ("Right", right_src)]);
    assert_eq!(
        result, 1,
        "expected the plain Left.IntRange record to stay boxed and hold a \
         beyond-i64 Int; a trap or 0 means it inherited the entry carrier's \
         i64 erasure through a qualified→bare name fallback"
    );
}

// Availability counterpart for the scalar carrier path: an entry-side
// qualified local-binding annotation over a SOLE-declarer gated dep
// carrier (`r: Dep.IntRange = ...`) must resolve the same carrier-i64
// eligibility fact as the bare post-flatten name, because `Dep` is the
// unique declarer. Only collision-renamed spellings keep declining.
#[test]
fn entry_qualified_annotation_over_sole_declarer_carrier_dep_type() {
    let entry_src = r#"
module Entry
    intent = "qualified local annotation over a sole-declarer carrier dep type"
    depends [Dep]

fn value(r: Dep.IntRange) -> Int
    r.value

fn run() -> Result<Int, String>
    r: Dep.IntRange = Dep.fromInt(70)?
    Result.Ok(value(r))

fn main() -> Int
    match run()
        Result.Ok(n) -> n
        Result.Err(_) -> 0 - 1
"#;
    let dep_src = r#"
module Dep
    intent = "sole-declarer gated IntRange carrier"
    exposes [IntRange, fromInt]
    depends []

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")
"#;
    let result = run_int_multi(entry_src, &[("Dep", dep_src)]);
    assert_eq!(
        result, 70,
        "expected the qualified `r: Dep.IntRange` binding to agree with the \
         unique carrier's representation and return 70"
    );
}

// A carrier used as a `Map` KEY must stay boxed (the Map-key codegen
// expects the struct-ref key layout), and that demotion must fire even
// when the ONLY spelling naming the carrier in Map-key position is the
// entry-side QUALIFIED one (`m: Map<Dep.IntRange, Int>` — the resolved
// key type keeps the qualified stamp). The alias-aware Scan 2 must
// canonicalize the resolved key spelling to the same bare key the
// eligibility set uses; missing it leaves the carrier i64-erased and
// the module fails wasm validation.
#[test]
fn qualified_map_key_spelling_over_sole_declarer_carrier_demotes() {
    let entry_src = r#"
module Entry
    intent = "qualified Map-key spelling over a sole-declarer carrier dep type"
    depends [Dep]

fn lookup(r: Dep.IntRange) -> Int
    m: Map<Dep.IntRange, Int> = Map.set({}, r, 41)
    match Map.get(m, r)
        Option.Some(v) -> v
        Option.None -> 0 - 1

fn run() -> Result<Int, String>
    r: Dep.IntRange = Dep.fromInt(70)?
    Result.Ok(lookup(r))

fn main() -> Int
    match run()
        Result.Ok(n) -> n
        Result.Err(_) -> 0 - 2
"#;
    let dep_src = r#"
module Dep
    intent = "sole-declarer gated IntRange carrier"
    exposes [IntRange, fromInt]
    depends []

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true -> Result.Ok(IntRange(value = n))
        false -> Result.Err("oob")
"#;
    let result = run_int_multi(entry_src, &[("Dep", dep_src)]);
    assert_eq!(
        result, 41,
        "expected the qualified Map-key spelling to demote the carrier to \
         its boxed struct-ref key layout and read back 41"
    );
}

// Availability counterpart for the MULTI-FIELD carrier path: an
// entry-side qualified local-binding annotation over a sole-declarer
// gated multi-field dep record (`c: Dep.Coord = ...`) must resolve the
// same per-`(record, field)` i64-erasure facts as the bare post-flatten
// name — this exercises the mirrored `field_carrier_intervals` alias
// entries. Only collision-renamed spellings keep declining.
#[test]
fn entry_qualified_annotation_over_sole_declarer_multi_field_carrier_dep_type() {
    let entry_src = r#"
module Entry
    intent = "qualified local annotation over a sole-declarer multi-field carrier dep type"
    depends [Dep]

fn total(c: Dep.Coord) -> Int
    c.x + c.y

fn run() -> Result<Int, String>
    c: Dep.Coord = Dep.coord(3, 4)?
    Result.Ok(total(c))

fn main() -> Int
    match run()
        Result.Ok(n) -> n
        Result.Err(_) -> 0 - 1
"#;
    let dep_src = r#"
module Dep
    intent = "sole-declarer gated multi-field Coord record"
    exposes [Coord, coord]
    depends []

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 100), Bool.and(y >= 0, y <= 100))
        true -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")
"#;
    let result = run_int_multi(entry_src, &[("Dep", dep_src)]);
    assert_eq!(
        result, 7,
        "expected the qualified `c: Dep.Coord` binding to agree with the \
         unique multi-field carrier's field representation and return 7"
    );
}

/// A program shaped so the chars-fusion pass fires: a linear loop over
/// `String.chars` that the pass rewrites into a `__cursor` variant full
/// of `__str_*` intrinsics, with the call site moved onto the variant.
const FUSIBLE_DIGITS: &str = r#"
module Digits
    intent = "counts the decimal digits in a string, shaped so chars fusion fires"
    exposes [count]
    effects []

fn digitCount(chars: List<String>, acc: Int) -> Int
    match chars
        [] -> acc
        [head, ..tail] -> match head
            "0" -> digitCount(tail, acc + 1)
            "1" -> digitCount(tail, acc + 1)
            _ -> digitCount(tail, acc)

fn count(text: String) -> Int
    digitCount(String.chars(text), 0)

fn main() -> Int
    count("a1b0c1")
"#;

/// The fusion passes synthesize intrinsic calls (`__str_*`, `__buf_*`,
/// `__lst_*`) that only the VM and the Rust codegen can lower; the
/// wasm-gc family excludes those passes via pipeline flags. If that
/// exclusion ever regresses, the intrinsics reach this backend's
/// emitter — and the emitter must REFUSE with a named compile error,
/// not fall back to a trap stub that ships a module whose rewritten
/// call sites trap at runtime (the silent-miscompile hole recorded in
/// the change that added the list-build pass). Before the refusal
/// existed, this fixture did not even reach that fall-through: the
/// emitter's type reader panicked on a synthesized `__str_code1` node
/// carrying no type stamp — an internal panic either way, never a
/// compile error.
#[test]
fn a_fabricated_intrinsic_reaching_the_emitter_is_refused() {
    // Drive the hazard for real: the exclusion flag deliberately wrong
    // (chars fusion ON on the wasm-gc path), everything else as
    // `compile_bytes` sets it.
    let mut items = parse_source(FUSIBLE_DIGITS).expect("fixture parses");
    let neutral_policy = aver::ir::NeutralAllocPolicy;
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: true,
            run_list_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        panic!("typecheck failed: {:?}", tc.errors);
    }
    let err = match compile_to_wasm_gc(&items, result.analysis.as_ref()) {
        Err(e) => e,
        Ok(bytes) => panic!(
            "a fused cursor variant slipped through the wasm-gc emitter: the module \
             compiled to {} bytes — the fabricated intrinsics fell back to a trap \
             stub instead of being refused",
            bytes.len()
        ),
    };
    let msg = err.to_string();
    assert!(
        msg.contains("fabricated intrinsic `__str_"),
        "the refusal names the intrinsic it cannot lower: {msg}"
    );
    assert!(
        msg.contains("does not lower fabricated intrinsics"),
        "the refusal states the wasm-gc family's contract: {msg}"
    );

    // The properly-excluded path is untouched: the same program, with
    // the fusion flags off as every wasm-gc call site sets them,
    // compiles and answers as it always did.
    assert_eq!(run_int(FUSIBLE_DIGITS), 3);
}
