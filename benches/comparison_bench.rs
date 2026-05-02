#![allow(clippy::approx_constant)]
//! Benchmark comparing execution modes on the same programs:
//!   VM (bytecode), codegen (native Rust), WASM (legacy linear-memory),
//!   WASM-GC (native WebAssembly GC + tail-call), self-hosted
//!   (`aver run --self-host`).
//!
//! Codegen runs as an external compiled binary. Self-hosted runs through the real CLI path,
//! which builds and reuses the cached Aver-in-Aver binary behind `aver run --self-host`.
//! Pre-compile before running:
//!
//!   cargo bench --bench comparison_bench

use criterion::{
    BenchmarkGroup, BenchmarkId, Criterion, criterion_group, criterion_main, measurement::WallTime,
};
use std::io::Write;
use std::process::Command;

use aver::nan_value::Arena;
use aver::resolver;
use aver::source::parse_source;
use aver::tco;
use aver::vm;
use wasmtime::{Caller, Config, Engine, FuncType, Linker, Module, Store, Val, ValType};

/// In-process embedded wasmtime harness — pre-built once per program,
/// reused across every Criterion iteration. Eliminates the
/// ~5-7 ms `wasmtime` binary cold-start that was floor-ing every WASM
/// measurement (`fib(15)` 6.28 ms despite the actual fib taking
/// nanoseconds). The bench programs are pure compute that don't
/// touch host effects, so the linker only needs to stub
/// `wasi_snapshot_preview1.fd_write` (the one import the WASI bridge
/// declares unconditionally).
struct WasmHarness {
    engine: Engine,
    module: Module,
}

fn build_wasm_harness(wasm_path: &std::path::Path) -> WasmHarness {
    let bytes = std::fs::read(wasm_path).expect("read pre-compiled wasm");
    let engine = Engine::default();
    let module = Module::new(&engine, &bytes).expect("wasmtime compile module");
    WasmHarness { engine, module }
}

/// In-process embedded wasmtime harness for the wasm-gc backend —
/// mirrors `WasmHarness` shape but configures the engine with GC +
/// tail-call proposals on (matches `aver compile --target=wasm-gc`'s
/// runtime expectations). Module compiled once, fresh Store +
/// Instance per Criterion iter.
struct WasmGcHarness {
    engine: Engine,
    module: Module,
    /// What `main` returns — picked at harness build time so the
    /// per-iter call uses the matching `get_typed_func` signature
    /// (avoids three speculative lookups per iteration).
    main_kind: WasmGcMainKind,
}

#[derive(Clone, Copy)]
enum WasmGcMainKind {
    Int,
    Float,
    Unit,
}

fn build_wasm_gc_harness(wasm_path: &std::path::Path) -> WasmGcHarness {
    let bytes = std::fs::read(wasm_path).expect("read pre-compiled wasm-gc bytes");
    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    config.cranelift_opt_level(wasmtime::OptLevel::Speed);
    config.max_wasm_stack(8 * 1024 * 1024);
    let engine = Engine::new(&config).expect("wasmtime gc engine");
    let module = Module::new(&engine, &bytes).expect("wasmtime gc compile module");
    // Probe `main`'s declared return type once. `_start` always exists
    // (it calls main and drops the result), but the typed-func per-iter
    // path is faster and we need to pick the right return type.
    let main_kind = {
        let mut store = Store::new(&engine, ());
        let mut linker = Linker::new(&engine);
        stub_wasm_gc_imports(&mut linker, &engine, &module);
        let instance = linker
            .instantiate(&mut store, &module)
            .expect("instantiate to probe main signature");
        if instance
            .get_typed_func::<(), i64>(&mut store, "main")
            .is_ok()
        {
            WasmGcMainKind::Int
        } else if instance
            .get_typed_func::<(), f64>(&mut store, "main")
            .is_ok()
        {
            WasmGcMainKind::Float
        } else {
            WasmGcMainKind::Unit
        }
    };
    WasmGcHarness {
        engine,
        module,
        main_kind,
    }
}

/// Walk the module's import section and register a default-value stub
/// for each `(module, name)` pair. Bench programs never call host
/// effects, but the wasm-gc backend declares `aver/console_*` and
/// `aver/time_*` imports unconditionally for any program that
/// type-checks against an effect surface — the linker has to satisfy
/// them before instantiate.
fn stub_wasm_gc_imports(linker: &mut Linker<()>, engine: &Engine, module: &Module) {
    use wasmtime::ExternType;
    for import in module.imports() {
        let ExternType::Func(ft) = import.ty() else {
            continue;
        };
        let result_tys: Vec<ValType> = ft.results().collect();
        let func_ty = FuncType::new(engine, ft.params(), ft.results());
        let module_name = import.module().to_string();
        let field_name = import.name().to_string();
        let _ = linker.func_new(
            &module_name,
            &field_name,
            func_ty,
            move |_caller, _params, results| {
                for (slot, ty) in results.iter_mut().zip(result_tys.iter()) {
                    *slot = wasm_gc_default_val(ty);
                }
                Ok(())
            },
        );
    }
}

fn wasm_gc_default_val(ty: &ValType) -> Val {
    match ty {
        ValType::I32 => Val::I32(0),
        ValType::I64 => Val::I64(0),
        ValType::F32 => Val::F32(0),
        ValType::F64 => Val::F64(0),
        ValType::V128 => Val::V128(0u128.into()),
        ValType::Ref(_) => Val::AnyRef(None),
    }
}

fn run_wasm_gc_iter(harness: &WasmGcHarness) {
    let mut store = Store::new(&harness.engine, ());
    let mut linker = Linker::new(&harness.engine);
    stub_wasm_gc_imports(&mut linker, &harness.engine, &harness.module);
    let instance = linker
        .instantiate(&mut store, &harness.module)
        .expect("instantiate wasm-gc module");
    match harness.main_kind {
        WasmGcMainKind::Int => {
            let f = instance
                .get_typed_func::<(), i64>(&mut store, "main")
                .expect("main: () -> i64");
            f.call(&mut store, ()).expect("wasm-gc main trap");
        }
        WasmGcMainKind::Float => {
            let f = instance
                .get_typed_func::<(), f64>(&mut store, "main")
                .expect("main: () -> f64");
            f.call(&mut store, ()).expect("wasm-gc main trap");
        }
        WasmGcMainKind::Unit => {
            let f = instance
                .get_typed_func::<(), ()>(&mut store, "main")
                .expect("main: () -> ()");
            f.call(&mut store, ()).expect("wasm-gc main trap");
        }
    }
}

fn run_wasm_iter(harness: &WasmHarness) {
    let mut store = Store::new(&harness.engine, ());
    let mut linker = Linker::new(&harness.engine);
    // The bench programs return Int from main and never print; the
    // bridge still emits a `(import "wasi_snapshot_preview1" "fd_write"
    // ...)` declaration unconditionally, so the linker has to satisfy
    // it before instantiate. A no-op stub returning 0 is enough since
    // it's never called.
    linker
        .func_wrap(
            "wasi_snapshot_preview1",
            "fd_write",
            |_caller: Caller<'_, ()>,
             _fd: i32,
             _iovec_ptr: i32,
             _iovec_count: i32,
             _nwritten_ptr: i32|
             -> i32 { 0 },
        )
        .expect("stub fd_write");
    let instance = linker
        .instantiate(&mut store, &harness.module)
        .expect("instantiate user.wasm");
    let start = instance
        .get_typed_func::<(), ()>(&mut store, "_start")
        .expect("_start export");
    start.call(&mut store, ()).expect("wasm _start trap");
}

// ── Runners ──────────────────────────────────────────────────────────────────

fn run_vm(source: &str) {
    let mut items = parse_source(source).expect("parse error");
    tco::transform_program(&mut items);
    resolver::resolve_program(&mut items);
    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program(&items, &mut arena, None).expect("compile error");
    let mut machine = vm::VM::new(code, globals, arena);
    let _ = machine.run().expect("VM error");
}

fn run_external(bin: &str, args: &[&str]) {
    let output = Command::new(bin)
        .args(args)
        .output()
        .unwrap_or_else(|e| panic!("failed to run {}: {}", bin, e));
    assert!(
        output.status.success(),
        "{} failed: {}",
        bin,
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Compile an Aver source to a temp binary via `aver compile` + `cargo build --release`.
/// Returns the path to the compiled binary.
fn compile_to_native(source: &str, name: &str) -> std::path::PathBuf {
    let dir = tempfile::tempdir().expect("create temp dir");
    let src_path = dir.path().join("main.av");
    std::fs::write(&src_path, source).expect("write source");

    let out_dir = dir.path().join("out");
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let rt_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("aver-rt");

    let status = Command::new(aver_bin)
        .arg("compile")
        .arg(&src_path)
        .arg("-o")
        .arg(&out_dir)
        .arg("--name")
        .arg(name)
        .env("AVER_RUNTIME_PATH", &rt_path)
        .status()
        .expect("aver compile");
    assert!(status.success(), "aver compile failed");

    let status = Command::new("cargo")
        .arg("build")
        .arg("--release")
        .current_dir(&out_dir)
        .status()
        .expect("cargo build");
    assert!(status.success(), "cargo build failed");

    let binary = out_dir.join("target/release").join(name);
    // Copy to a stable location so the tempdir can be kept alive
    let stable = std::env::temp_dir().join(format!("aver_bench_{}", name));
    std::fs::copy(&binary, &stable).expect("copy binary");
    stable
}

/// Pre-compile an Aver source to a standalone WASI-bundled `.wasm` so
/// the bench loop can spawn `wasmtime` on a stable file instead of
/// re-running `aver compile` every iteration. Without this, the WASM
/// number was dominated by lex/parse/typecheck/emit/wasm-merge cost
/// per iter (~15 ms baseline) — same shape as how `compile_to_native`
/// caches the codegen binary.
fn compile_to_wasm(source: &str, name: &str) -> std::path::PathBuf {
    let dir = tempfile::tempdir().expect("create wasm bench temp dir");
    let src_path = dir.path().join("main.av");
    std::fs::write(&src_path, source).expect("write source");

    let out_dir = dir.path().join("out");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let status = Command::new(aver_bin)
        .arg("compile")
        .arg(&src_path)
        .arg("--target")
        .arg("wasm")
        .arg("--bridge")
        .arg("wasip1")
        .arg("--optimize")
        .arg("size")
        .arg("--name")
        .arg(name)
        .arg("-o")
        .arg(&out_dir)
        .status()
        .expect("aver compile --target wasm");
    assert!(status.success(), "aver compile --target wasm failed");

    let built = out_dir.join(format!("{}.wasm", name));
    let stable = std::env::temp_dir().join(format!("aver_bench_{}.wasm", name));
    std::fs::copy(&built, &stable).expect("copy wasm artifact");
    stable
}

/// Pre-compile an Aver source through `--target wasm-gc`. Same shape
/// as `compile_to_wasm` but no `--bridge` (the wasm-gc backend has its
/// own native ABI; effects come in as `aver/*` imports). Output is a
/// stable `.wasm` ready for the in-process wasmtime harness.
fn compile_to_wasm_gc(source: &str, name: &str) -> std::path::PathBuf {
    let dir = tempfile::tempdir().expect("create wasm-gc bench temp dir");
    let src_path = dir.path().join("main.av");
    std::fs::write(&src_path, source).expect("write source");

    let out_dir = dir.path().join("out");
    let aver_bin = env!("CARGO_BIN_EXE_aver");

    let status = Command::new(aver_bin)
        .arg("compile")
        .arg(&src_path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--name")
        .arg(name)
        .arg("-o")
        .arg(&out_dir)
        .status()
        .expect("aver compile --target wasm-gc");
    assert!(status.success(), "aver compile --target wasm-gc failed");

    let built = out_dir.join(format!("{}.wasm", name));
    let stable = std::env::temp_dir().join(format!("aver_bench_gc_{}.wasm", name));
    std::fs::copy(&built, &stable).expect("copy wasm-gc artifact");
    stable
}

fn write_temp_source(root: &std::path::Path, name: &str, source: &str) -> std::path::PathBuf {
    let path = root.join(format!("{}.av", name));
    let mut f = std::fs::File::create(&path).expect("create temp source file");
    f.write_all(source.as_bytes())
        .expect("write temp source file");
    path
}

// ── Test programs ────────────────────────────────────────────────────────────
//
// Sources live as individual `.av` files in `bench/scenarios/`, shared
// with `aver bench` so cargo bench (criterion timing across VM/WASM/Rust/
// self-host) and the in-process scenario harness measure the exact same
// programs. Newtype trio compares the same compute three ways; comparing
// per-backend numbers tells us how much current lowering pays for
// nominal newtype safety on each backend.

const FIB_SRC: &str = include_str!("../bench/scenarios/fib.av");
const COUNTDOWN_SRC: &str = include_str!("../bench/scenarios/countdown.av");
const RECORD_SRC: &str = include_str!("../bench/scenarios/record.av");
const MAP_BUILD_SRC: &str = include_str!("../bench/scenarios/map_build.av");
const MAP_LOOKUP_SRC: &str = include_str!("../bench/scenarios/map_lookup.av");
const MATCH_SRC: &str = include_str!("../bench/scenarios/match_dispatch.av");
const STRING_SRC: &str = include_str!("../bench/scenarios/string_interp.av");
const VECTOR_SRC: &str = include_str!("../bench/scenarios/vector_ops.av");
const NEWTYPE_BARE_SRC: &str = include_str!("../bench/scenarios/newtype_bare.av");
const NEWTYPE_RECORD_SRC: &str = include_str!("../bench/scenarios/newtype_record.av");
const NEWTYPE_VARIANT_SRC: &str = include_str!("../bench/scenarios/newtype_variant.av");

// ── Benchmark groups ─────────────────────────────────────────────────────────

struct BenchArtifacts<'a> {
    native_bin: &'a std::path::Path,
    wasm_harness: &'a WasmHarness,
    wasm_gc_harness: &'a WasmGcHarness,
    aver_bin: &'a str,
    module_root: &'a std::path::Path,
    source_file: &'a std::path::Path,
}

fn bench_all_modes(
    group: &mut BenchmarkGroup<WallTime>,
    label: &str,
    source: &str,
    artifacts: &BenchArtifacts<'_>,
) {
    group.bench_with_input(BenchmarkId::new("vm", label), source, |b, src| {
        b.iter(|| run_vm(src));
    });
    group.bench_function(BenchmarkId::new("wasm", label), |b| {
        // In-process embedded wasmtime — Engine + Module compiled
        // once in setup, b.iter creates a fresh Store + Instance and
        // invokes `_start`. Symmetric with how `run_vm` runs the VM
        // in-process; eliminates spawn cost and aver-compile cost,
        // measures the actual run.
        b.iter(|| run_wasm_iter(artifacts.wasm_harness));
    });
    group.bench_function(BenchmarkId::new("wasm-gc", label), |b| {
        // Same in-process pattern as `wasm`, but the engine is
        // configured with the GC + tail-call proposals on and the
        // harness invokes `main` as a typed function (the wasm-gc
        // backend exports user fns directly under their Aver name).
        // Stubs `aver/*` host imports so pure-compute scenarios link.
        b.iter(|| run_wasm_gc_iter(artifacts.wasm_gc_harness));
    });
    group.bench_function(BenchmarkId::new("codegen", label), |b| {
        b.iter(|| run_external(artifacts.native_bin.to_str().unwrap(), &[]));
    });
    group.bench_function(BenchmarkId::new("self-hosted", label), |b| {
        b.iter(|| {
            run_external(
                artifacts.aver_bin,
                &[
                    "run",
                    artifacts.source_file.to_str().unwrap(),
                    "--module-root",
                    artifacts.module_root.to_str().unwrap(),
                    "--self-host",
                ],
            )
        });
    });
}

fn comparison_benches(c: &mut Criterion) {
    let tests: &[(&str, &str, &str)] = &[
        ("fib(15)", "bench_fib", FIB_SRC),
        ("countdown(20k)", "bench_countdown", COUNTDOWN_SRC),
        ("record access 20k", "bench_record", RECORD_SRC),
        ("map build 5k", "bench_map", MAP_BUILD_SRC),
        ("map lookup 20k/2k", "bench_map_lookup", MAP_LOOKUP_SRC),
        ("pattern match 30k", "bench_match", MATCH_SRC),
        ("string interp 5k", "bench_string", STRING_SRC),
        ("vector get/set 5k", "bench_vector", VECTOR_SRC),
        (
            "newtype baseline 20k",
            "bench_newtype_bare",
            NEWTYPE_BARE_SRC,
        ),
        (
            "newtype record 20k",
            "bench_newtype_record",
            NEWTYPE_RECORD_SRC,
        ),
        (
            "newtype variant 20k",
            "bench_newtype_variant",
            NEWTYPE_VARIANT_SRC,
        ),
    ];

    // Pre-compile all native binaries
    let natives: Vec<std::path::PathBuf> = tests
        .iter()
        .map(|(label, name, src)| {
            eprintln!("Compiling {} to native...", label);
            compile_to_native(src, name)
        })
        .collect();

    // Pre-compile WASM artifacts once + build the embedded wasmtime
    // harness per program: the Engine and Module are constructed in
    // setup, only Instance creation + invoke happens per Criterion
    // iter.
    let wasm_files: Vec<std::path::PathBuf> = tests
        .iter()
        .map(|(label, name, src)| {
            eprintln!("Compiling {} to WASI-bundled wasm...", label);
            compile_to_wasm(src, name)
        })
        .collect();
    let wasm_harnesses: Vec<WasmHarness> =
        wasm_files.iter().map(|p| build_wasm_harness(p)).collect();

    // Same idea for the wasm-gc backend — separate file path + engine
    // config so the two harnesses live side by side and don't poison
    // each other's caches.
    let wasm_gc_files: Vec<std::path::PathBuf> = tests
        .iter()
        .map(|(label, name, src)| {
            eprintln!("Compiling {} to wasm-gc...", label);
            compile_to_wasm_gc(src, name)
        })
        .collect();
    let wasm_gc_harnesses: Vec<WasmGcHarness> = wasm_gc_files
        .iter()
        .map(|p| build_wasm_gc_harness(p))
        .collect();

    // Write source files for the real `aver run --self-host` path under one shared module root
    let self_host_root = tempfile::tempdir().expect("create self-host bench root");
    let source_files: Vec<std::path::PathBuf> = tests
        .iter()
        .map(|(_, name, src)| write_temp_source(self_host_root.path(), name, src))
        .collect();

    let aver_bin = env!("CARGO_BIN_EXE_aver");

    // Warm the cached self-host binary once before measuring.
    run_external(
        aver_bin,
        &[
            "run",
            source_files[0].to_str().unwrap(),
            "--module-root",
            self_host_root.path().to_str().unwrap(),
            "--self-host",
        ],
    );

    for (i, (label, _, src)) in tests.iter().enumerate() {
        let mut group = c.benchmark_group(*label);
        let artifacts = BenchArtifacts {
            native_bin: &natives[i],
            wasm_harness: &wasm_harnesses[i],
            wasm_gc_harness: &wasm_gc_harnesses[i],
            aver_bin,
            module_root: self_host_root.path(),
            source_file: &source_files[i],
        };
        bench_all_modes(&mut group, label, src, &artifacts);

        group.finish();
    }
}

criterion_group!(benches, comparison_benches);
criterion_main!(benches);
