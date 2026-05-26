//! Scenario runners — compile entry once per target, run N+warmup
//! iterations timing each. Three targets:
//!
//! - `vm` — in-process `vm::compile_program_with_modules` + `VM::run`.
//! - `wasm-gc` — `aver compile --target wasm-gc` (engine GC + tail
//!   calls, no NaN-boxing); in-process wasmtime harness with
//!   `wasm_gc` / `wasm_tail_call` / `wasm_function_references`
//!   enabled. Console / Time imports get bench-mode no-op stubs.
//! - `wasm-gc-v8` — same wasm-gc bytes executed under V8 via Node 22+.
//! - `rust` — `aver compile --target rust` + `cargo build --release`
//!   produces a native binary; each iteration spawns it once. Includes
//!   process spawn overhead (~1-2 ms on macOS) — for programs that
//!   take <1 ms in pure compute the spawn dominates, just like in
//!   the cargo bench measurements.

use std::process::Command;
use std::time::Instant;

use crate::ast::TopLevel;
use crate::bench::manifest::{BenchTarget, Manifest};
use crate::bench::report::{BackendInfo, BenchReport, HostInfo, IterationStats, ScenarioMetadata};
use crate::ir::{PipelineConfig, PipelineStage, TypecheckMode};
use crate::nan_value::Arena;
use crate::source::parse_source;
use crate::vm;

#[derive(Debug)]
pub enum RunError {
    Read(String),
    Parse(String),
    Typecheck(String),
    Compile(String),
    Runtime(String),
    Setup(String),
}

impl std::fmt::Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Read(m)
            | Self::Parse(m)
            | Self::Typecheck(m)
            | Self::Compile(m)
            | Self::Runtime(m)
            | Self::Setup(m) => f.write_str(m),
        }
    }
}

/// Run `manifest` against the requested target. Dispatches to the
/// per-target runner; the report shape is identical across targets so
/// downstream tools (`--compare`, NDJSON consumers) don't care which
/// backend produced the numbers.
pub fn run_scenario(manifest: &Manifest, target: BenchTarget) -> Result<BenchReport, RunError> {
    match target {
        BenchTarget::Vm => run_vm(manifest),
        BenchTarget::WasmGc => run_wasm_gc(manifest),
        BenchTarget::WasmGcV8 => run_wasm_gc_v8(manifest),
        BenchTarget::Rust => run_rust(manifest),
    }
}

// ── VM target ──────────────────────────────────────────────────────────

fn run_vm(manifest: &Manifest) -> Result<BenchReport, RunError> {
    let entry_str = manifest.entry.to_string_lossy().into_owned();
    let module_root = manifest
        .entry
        .parent()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_default();

    let source = std::fs::read_to_string(&manifest.entry)
        .map_err(|e| RunError::Read(format!("{}: {}", entry_str, e)))?;
    let mut items: Vec<TopLevel> = parse_source(&source).map_err(RunError::Parse)?;

    let passes_applied = std::cell::RefCell::new(Vec::<String>::new());
    let pipeline_result = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            on_after_pass: Some(Box::new(|stage: PipelineStage, _| {
                passes_applied.borrow_mut().push(stage.name().to_string());
            })),
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        let msg = tc_result
            .errors
            .iter()
            .map(|err| format!("error[{}:{}]: {}", err.line, err.col, err.message))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(RunError::Typecheck(msg));
    }

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_modules(
        &pipeline_result.resolved_items,
        &pipeline_result.symbol_table,
        &mut arena,
        Some(&module_root),
        &entry_str,
        pipeline_result.analysis.as_ref(),
    )
    .map_err(|e| RunError::Compile(format!("VM compile: {}", e)))?;

    let mut samples: Vec<f64> = Vec::with_capacity(manifest.iterations);

    for _ in 0..manifest.warmup {
        run_one_vm(&code, &globals, &arena, &manifest.args)?;
    }
    let mut last_response_bytes: Option<usize> = None;
    for _ in 0..manifest.iterations {
        let t = Instant::now();
        let bytes = run_one_vm(&code, &globals, &arena, &manifest.args)?;
        samples.push(t.elapsed().as_secs_f64() * 1000.0);
        last_response_bytes = bytes;
    }

    let policy = crate::ir::NeutralAllocPolicy;
    let visible_allocs = crate::ir::count_alloc_sites_in_program(&items, &policy);
    let mut report = build_report(
        manifest,
        BenchTarget::Vm,
        &samples,
        passes_applied.into_inner(),
        Some(visible_allocs),
    );
    report.response_bytes = last_response_bytes;
    Ok(report)
}

/// Run one bench iteration and return the byte count of `main`'s
/// rendered return value. Aver `main` is conventionally `() -> T` for
/// some `T`; we serialise the resulting `NanValue` through `aver_display`
/// (the same code path `Console.print` uses) and count UTF-8 bytes.
/// `Unit` returns `Some(0)`. `None` is reserved for cases where the
/// value isn't displayable — none of the bench scenarios hit that path
/// today.
fn run_one_vm(
    code: &vm::CodeStore,
    globals: &[crate::nan_value::NanValue],
    arena: &Arena,
    args: &[String],
) -> Result<Option<usize>, RunError> {
    let mut machine = vm::VM::new(code.clone(), globals.to_vec(), arena.clone());
    machine.set_silent_console(true);
    machine.set_cli_args(args.to_vec());
    use crate::nan_value::NanValueConvert;
    let result = machine
        .run()
        .map_err(|e| RunError::Runtime(format!("{}", e)))?;
    // Render `main`'s return value through the same `aver_display` path
    // `Console.print` uses, so `response_bytes` matches what the user
    // would see if their program piped `main` through `print`. The
    // VM's arena is borrowed read-only for the conversion.
    let value = result.to_value(&machine.arena);
    let bytes = crate::value::aver_display(&value).map(|s| s.len());
    Ok(bytes)
}

// ── wasm-gc target (0.15.3 probe) ──────────────────────────────────────

#[cfg(feature = "wasm")]
fn run_wasm_gc(manifest: &Manifest) -> Result<BenchReport, RunError> {
    use wasmtime::{Config, Engine, Module, Store};

    // Compile once. We shell out to `aver compile --target=wasm-gc`
    // so the produced bytes are identical to what users get — bench
    // measures the production codegen output, not a special path.
    let temp = tempfile::tempdir()
        .map_err(|e| RunError::Setup(format!("create wasm-gc bench tempdir: {}", e)))?;
    let out_dir = temp.path().join("out");
    let aver_bin = std::env::current_exe()
        .map_err(|e| RunError::Setup(format!("locate current aver binary: {}", e)))?;

    let mut compile = Command::new(&aver_bin);
    compile
        .arg("compile")
        .arg(&manifest.entry)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--name")
        .arg(&manifest.name)
        .arg("-o")
        .arg(&out_dir);
    if let Some(root) = manifest.entry.parent() {
        compile.arg("--module-root").arg(root);
    }
    let status = compile
        .status()
        .map_err(|e| RunError::Setup(format!("spawn aver compile --target=wasm-gc: {}", e)))?;
    if !status.success() {
        return Err(RunError::Compile(format!(
            "aver compile --target=wasm-gc exited with {}",
            status
        )));
    }

    let wasm_path = out_dir.join(format!("{}.wasm", manifest.name));
    let bytes = std::fs::read(&wasm_path)
        .map_err(|e| RunError::Setup(format!("read {}: {}", wasm_path.display(), e)))?;

    // Engine config: enable GC + tail calls. wasmtime defaults vary
    // by version; pin them on so the bench doesn't depend on cwd or
    // env vars. Multi-value, bulk memory, etc. are commonly default-
    // on but we set them explicitly to match the CLI's `--wasm gc
    // --wasm tail-call` shape.
    let mut config = Config::new();
    // Match the wasmtime CLI's `--wasm gc --wasm tail-call` runtime
    // configuration. `wasm_gc` requires the cranelift backend to know
    // about gc; `wasm_function_references` is a transitive dep of GC;
    // `wasm_tail_call` enables `return_call(_indirect)` execution.
    // `cranelift_opt_level(Speed)` matches the CLI's default which
    // turns on the codegen paths that handle ref.cast properly.
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    config.cranelift_opt_level(wasmtime::OptLevel::Speed);
    // Be generous with wasm stack — tail-call return_call should
    // not need extra room, but if cranelift generates a regular
    // call we don't want to fail at 10K iterations. Default is
    // 1MB; bump to 8MB.
    config.max_wasm_stack(8 * 1024 * 1024);
    // `component-model-async` (pulled in by the `wasip2` feature)
    // enforces `max_wasm_stack <= async_stack_size` at Engine::new.
    config.async_stack_size(12 * 1024 * 1024);
    let engine = Engine::new(&config)
        .map_err(|e| RunError::Setup(format!("wasmtime engine config: {}", e)))?;
    let module = Module::new(&engine, &bytes)
        .map_err(|e| RunError::Setup(format!("wasmtime compile module: {}", e)))?;

    let run_one = |module: &Module, engine: &Engine| -> Result<String, RunError> {
        let mut store = Store::new(engine, ());
        // wasm-gc effects: stub `aver/*` imports so bench mode runs
        // silently. `Console.print(s)` becomes a no-op that drops
        // the String ref. Real hosts (browser, workerd, wasmtime+wasi)
        // wire these to actual stdout / stderr.
        let mut linker = wasmtime::Linker::new(engine);
        // Match the wasm-gc import shape from our codegen: each
        // Console method takes `(ref null $string, i32 caller_fn_idx)`
        // — the i32 was added in 0.16 for per-fn replay attribution.
        // The most permissive wasmtime type for the string ref is
        // `(ref null any)`. Bench-mode stubs drop both args.
        let console_print_ty = wasmtime::FuncType::new(
            engine,
            [
                wasmtime::ValType::Ref(wasmtime::RefType::new(true, wasmtime::HeapType::Any)),
                wasmtime::ValType::I32,
            ],
            std::iter::empty(),
        );
        linker
            .func_new(
                "aver",
                "console_print",
                console_print_ty.clone(),
                |_caller, _params, _results| Ok(()),
            )
            .map_err(|e| RunError::Setup(format!("stub aver/console_print: {}", e)))?;
        for fname in ["console_error", "console_warn"] {
            linker
                .func_new(
                    "aver",
                    fname,
                    console_print_ty.clone(),
                    |_caller, _params, _results| Ok(()),
                )
                .map_err(|e| RunError::Setup(format!("stub aver/{fname}: {e}")))?;
        }
        // Time.unixMs — bench mode uses a fixed value to keep runs
        // deterministic; the production host wires this to the real
        // `Date.now()` / `clock_gettime` equivalent.
        // `time_unix_ms(caller_fn_idx: i32) -> i64` — same per-fn idx
        // tagging as `console_print`. Drop the arg, return 0.
        let time_unix_ms_ty = wasmtime::FuncType::new(
            engine,
            std::iter::once(wasmtime::ValType::I32),
            std::iter::once(wasmtime::ValType::I64),
        );
        linker
            .func_new(
                "aver",
                "time_unix_ms",
                time_unix_ms_ty,
                |_caller, _params, results| {
                    results[0] = wasmtime::Val::I64(0);
                    Ok(())
                },
            )
            .map_err(|e| RunError::Setup(format!("stub aver/time_unix_ms: {}", e)))?;
        let instance = linker
            .instantiate(&mut store, module)
            .map_err(|e| RunError::Runtime(format!("instantiate: {}", e)))?;
        // Try `main: () -> i64` first (Int return), then `() -> f64`
        // (Float), then `() -> ()` (Unit). The order matches the
        // most common bench shapes.
        if let Ok(f) = instance.get_typed_func::<(), i64>(&mut store, "main") {
            let v = f
                .call(&mut store, ())
                .map_err(|e| RunError::Runtime(format!("invoke main: {}", e)))?;
            return Ok(v.to_string());
        }
        if let Ok(f) = instance.get_typed_func::<(), f64>(&mut store, "main") {
            let v = f
                .call(&mut store, ())
                .map_err(|e| RunError::Runtime(format!("invoke main: {}", e)))?;
            return Ok(format!("{v}"));
        }
        if let Ok(f) = instance.get_typed_func::<(), ()>(&mut store, "main") {
            f.call(&mut store, ())
                .map_err(|e| RunError::Runtime(format!("invoke main: {}", e)))?;
            return Ok(String::new());
        }
        // Reference returns (e.g. `main: () -> String`, where `String`
        // is `(ref null $string_array)`): fall back to a dynamic call.
        // Bench mode only needs the timing; rendering the byte content
        // would require introspecting the array element-by-element via
        // the wasmtime GC API. Report `[ref]` as the rendered result.
        if let Some(f) = instance.get_func(&mut store, "main") {
            let n_results = f.ty(&store).results().len();
            let mut out: Vec<wasmtime::Val> = (0..n_results)
                .map(|_| wasmtime::Val::AnyRef(None))
                .collect();
            f.call(&mut store, &[], &mut out)
                .map_err(|e| RunError::Runtime(format!("invoke main: {}", e)))?;
            return Ok(String::from("[ref]"));
        }
        Err(RunError::Runtime("main export must be a function".into()))
    };

    let mut samples: Vec<f64> = Vec::with_capacity(manifest.iterations);
    for _ in 0..manifest.warmup {
        run_one(&module, &engine)?;
    }
    let mut last_result = String::new();
    for _ in 0..manifest.iterations {
        let t = Instant::now();
        last_result = run_one(&module, &engine)?;
        samples.push(t.elapsed().as_secs_f64() * 1000.0);
    }

    let passes = canonical_passes();
    let mut report = build_report(
        manifest,
        BenchTarget::WasmGc,
        &samples,
        passes,
        compute_visible_allocs(manifest),
    );
    // wasm-gc invokes `main` directly. Use the same "rendered return
    // value" semantic as the VM target: count bytes of the decimal
    // representation.
    report.response_bytes = Some(last_result.len());
    Ok(report)
}

#[cfg(not(feature = "wasm"))]
fn run_wasm_gc(_manifest: &Manifest) -> Result<BenchReport, RunError> {
    Err(RunError::Setup(
        "wasm-gc target requires the `wasm` feature; rebuild with `cargo build --features wasm`"
            .to_string(),
    ))
}

// ── wasm-gc target under V8 ────────────────────────────────────────────

/// Same `aver compile --target wasm-gc` bytes as the wasmtime
/// runner above, executed under V8 (Node 22+) via
/// `tools/wasm-gc-bench-v8.mjs`. Spawns one Node process per scenario;
/// the script reports raw sample timings as JSON on stdout
/// (`{"samples_ms": [...]}`) and the harness here computes stats
/// then stitches the canonical `BenchReport` shape so the report
/// layout matches every other target.
///
/// Why a second wasm-gc column at all: wasmtime 44's GC heap is the
/// engine ceiling for alloc-heavy workloads — `string_interp` runs
/// 2300× faster on V8 than on wasmtime as of 0.17.2 (memory
/// `project_v8_vs_wasmtime_gc.md`). Keeping both visible prevents
/// the wasm-gc backend from looking falsely slow when the bottleneck
/// is the embed engine, not the codegen.
#[cfg(feature = "wasm")]
fn run_wasm_gc_v8(manifest: &Manifest) -> Result<BenchReport, RunError> {
    use std::path::PathBuf;

    let temp = tempfile::tempdir()
        .map_err(|e| RunError::Setup(format!("create wasm-gc-v8 bench tempdir: {}", e)))?;
    let out_dir = temp.path().join("out");
    let aver_bin = std::env::current_exe()
        .map_err(|e| RunError::Setup(format!("locate current aver binary: {}", e)))?;

    // Compile once via the same `aver compile --target wasm-gc` path
    // the wasmtime runner uses — the bytes are identical, only the
    // host runtime differs.
    let mut compile = Command::new(&aver_bin);
    compile
        .arg("compile")
        .arg(&manifest.entry)
        .arg("--target")
        .arg("wasm-gc")
        .arg("--name")
        .arg(&manifest.name)
        .arg("-o")
        .arg(&out_dir);
    if let Some(root) = manifest.entry.parent() {
        compile.arg("--module-root").arg(root);
    }
    let status = compile
        .status()
        .map_err(|e| RunError::Setup(format!("spawn aver compile --target wasm-gc: {}", e)))?;
    if !status.success() {
        return Err(RunError::Compile(format!(
            "aver compile (wasm-gc-v8) exited with {}",
            status
        )));
    }
    let wasm_path = out_dir.join(format!("{}.wasm", manifest.name));

    // Locate Node 22+ — V8's wasm-gc support stabilised there. Earlier
    // Nodes ship a V8 that rejects packed `i8` arrays. Try `node` on
    // PATH first; fall back to `~/.nvm/versions/node/v22.*/bin/node`
    // if installed via nvm.
    let node_bin = locate_node22()
        .ok_or_else(|| RunError::Setup(
            "wasm-gc-v8 requires Node 22+ on PATH or under ~/.nvm/versions/node/v22.* — install via nvm or your package manager".to_string()
        ))?;

    // Locate the harness script. Prefer CARGO_MANIFEST_DIR (cargo
    // run/test); fall back to the repo root via the binary's parent
    // chain (production aver binary lives at <repo>/target/release/aver).
    let harness = std::env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .map(|p| p.join("tools").join("wasm-gc-bench-v8.mjs"))
        .or_else(|_| {
            aver_bin
                .parent()
                .and_then(|p| p.parent())
                .and_then(|p| p.parent())
                .map(|repo| repo.join("tools").join("wasm-gc-bench-v8.mjs"))
                .ok_or(())
        })
        .map_err(|_| RunError::Setup("locate tools/wasm-gc-bench-v8.mjs".to_string()))?;
    if !harness.exists() {
        return Err(RunError::Setup(format!(
            "wasm-gc-v8 harness not found at {}",
            harness.display()
        )));
    }

    let total_iters = manifest.iterations + manifest.warmup;
    let output = Command::new(&node_bin)
        .arg(&harness)
        .arg(&wasm_path)
        .arg("--json")
        .arg("--iters")
        .arg(total_iters.to_string())
        .arg("--warmup")
        .arg("0") // we strip warmup ourselves to keep the slicing identical to other targets
        .output()
        .map_err(|e| RunError::Setup(format!("spawn node wasm-gc-bench-v8.mjs: {}", e)))?;
    if !output.status.success() {
        return Err(RunError::Runtime(format!(
            "wasm-gc-bench-v8 exited with {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    // Parse `{"samples_ms": [..]}` from stdout. Single line, last
    // line wins (so `console.warn`s from V8 about deprecation etc.
    // can land earlier without breaking the parse).
    let stdout = String::from_utf8_lossy(&output.stdout);
    let json_line = stdout
        .lines()
        .rev()
        .find(|l| l.trim_start().starts_with('{'))
        .ok_or_else(|| {
            RunError::Runtime(format!(
                "wasm-gc-bench-v8 stdout had no JSON line: {}",
                stdout
            ))
        })?;
    let parsed: serde_json::Value = serde_json::from_str(json_line)
        .map_err(|e| RunError::Runtime(format!("parse wasm-gc-bench-v8 output: {}", e)))?;
    let samples_arr = parsed
        .get("samples_ms")
        .and_then(|v| v.as_array())
        .ok_or_else(|| {
            RunError::Runtime("wasm-gc-bench-v8 output missing samples_ms array".to_string())
        })?;
    let samples: Vec<f64> = samples_arr
        .iter()
        .filter_map(|v| v.as_f64())
        .skip(manifest.warmup)
        .collect();

    let passes = canonical_passes();
    let mut report = build_report(
        manifest,
        BenchTarget::WasmGcV8,
        &samples,
        passes,
        compute_visible_allocs(manifest),
    );
    // Same convention as the wasmtime wasm-gc runner: response_bytes
    // is informational only — the harness doesn't decode return
    // values across the JS boundary.
    report.response_bytes = None;
    Ok(report)
}

#[cfg(not(feature = "wasm"))]
fn run_wasm_gc_v8(_manifest: &Manifest) -> Result<BenchReport, RunError> {
    Err(RunError::Setup(
        "wasm-gc-v8 target requires the `wasm` feature; rebuild with `cargo build --features wasm`"
            .to_string(),
    ))
}

#[cfg(feature = "wasm")]
fn locate_node22() -> Option<std::path::PathBuf> {
    // 1) Try `node` on PATH and check `--version >= v22.0.0`.
    if let Ok(out) = Command::new("node").arg("--version").output()
        && out.status.success()
    {
        let v = String::from_utf8_lossy(&out.stdout);
        if let Some(major) = v.trim_start_matches('v').split('.').next()
            && let Ok(n) = major.parse::<u32>()
            && n >= 22
        {
            return Some(std::path::PathBuf::from("node"));
        }
    }
    // 2) Fall back to `~/.nvm/versions/node/v22.*/bin/node`.
    let home = std::env::var("HOME").ok()?;
    let nvm_dir = std::path::PathBuf::from(home)
        .join(".nvm")
        .join("versions")
        .join("node");
    let mut v22_dirs: Vec<_> = std::fs::read_dir(&nvm_dir)
        .ok()?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| {
            p.file_name()
                .and_then(|n| n.to_str())
                .is_some_and(|n| n.starts_with("v22."))
        })
        .collect();
    v22_dirs.sort();
    v22_dirs.last().map(|p| p.join("bin").join("node"))
}

// ── Rust target ────────────────────────────────────────────────────────

fn run_rust(manifest: &Manifest) -> Result<BenchReport, RunError> {
    // Compile to a native Rust binary once, then spawn it per iteration.
    // Spawn cost (1-2 ms on macOS) is part of what's measured — the same
    // shape the cargo bench reports, so the numbers stay comparable.
    let temp = tempfile::tempdir()
        .map_err(|e| RunError::Setup(format!("create rust bench tempdir: {}", e)))?;
    let out_dir = temp.path().join("out");
    let aver_bin = std::env::current_exe()
        .map_err(|e| RunError::Setup(format!("locate current aver binary: {}", e)))?;

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .map(std::path::PathBuf::from)
        .ok();
    let mut compile_cmd = Command::new(&aver_bin);
    compile_cmd
        .arg("compile")
        .arg(&manifest.entry)
        .arg("--name")
        .arg(&manifest.name)
        .arg("-o")
        .arg(&out_dir);
    if let Some(root) = manifest.entry.parent() {
        compile_cmd.arg("--module-root").arg(root);
    }
    if let Some(root) = manifest_dir.as_ref() {
        compile_cmd.env("AVER_RUNTIME_PATH", root.join("aver-rt"));
    }
    let status = compile_cmd
        .status()
        .map_err(|e| RunError::Setup(format!("spawn aver compile --target rust: {}", e)))?;
    if !status.success() {
        return Err(RunError::Compile(format!(
            "aver compile (rust) exited with {}",
            status
        )));
    }

    let status = Command::new("cargo")
        .arg("build")
        .arg("--release")
        .current_dir(&out_dir)
        .status()
        .map_err(|e| RunError::Setup(format!("spawn cargo build: {}", e)))?;
    if !status.success() {
        return Err(RunError::Compile(format!(
            "cargo build (rust) exited with {}",
            status
        )));
    }

    let binary = out_dir.join("target/release").join(&manifest.name);
    if !binary.exists() {
        return Err(RunError::Setup(format!(
            "rust target binary not found at {}",
            binary.display()
        )));
    }

    // In-process bench loop: one spawn, the generated binary calls
    // `aver_generated::entry::main` N times under `AVER_BENCH_ITER` and
    // emits one `__bench_iter_ms__: <ms>` line per iter on stderr. The
    // alternative — spawn the binary N times from here — bottoms out at
    // ~2–3 ms macOS process-spawn cost and reports noise on anything
    // under 1 ms (`fib`, `factorial`, `record`). The codegen-side
    // dispatch is gated on the env var, so production builds pay one
    // env-var read at process start and nothing else.
    let output = Command::new(&binary)
        .args(&manifest.args)
        .env("AVER_BENCH_ITER", manifest.iterations.to_string())
        .env("AVER_BENCH_WARMUP", manifest.warmup.to_string())
        .output()
        .map_err(|e| RunError::Runtime(format!("spawn {}: {}", binary.display(), e)))?;
    if !output.status.success() {
        return Err(RunError::Runtime(format!(
            "{} exited with {}: {}",
            binary.display(),
            output.status,
            String::from_utf8_lossy(&output.stderr)
        )));
    }
    let stderr_text = String::from_utf8_lossy(&output.stderr);
    let mut samples: Vec<f64> = Vec::with_capacity(manifest.iterations);
    for line in stderr_text.lines() {
        if let Some(rest) = line.strip_prefix("__bench_iter_ms__: ")
            && let Ok(ms) = rest.trim().parse::<f64>()
        {
            samples.push(ms);
        }
    }
    if samples.is_empty() {
        return Err(RunError::Runtime(format!(
            "rust target produced no `__bench_iter_ms__` lines (stderr: {})",
            &stderr_text[..stderr_text.len().min(200)]
        )));
    }
    let last_bytes = output.stdout.len();

    let passes = canonical_passes();
    let mut report = build_report(
        manifest,
        BenchTarget::Rust,
        &samples,
        passes,
        compute_visible_allocs(manifest),
    );
    // Rust target captures actual stdout from the spawned binary.
    // Same "actual bytes printed" semantics as wasm-gc; differs
    // from the VM target's "rendered return value" semantics, but
    // baselines compare same-target only.
    report.response_bytes = Some(last_bytes);
    Ok(report)
}

// ── Shared helpers ─────────────────────────────────────────────────────

fn canonical_passes() -> Vec<String> {
    [
        "tco",
        "typecheck",
        "interp_lower",
        "buffer_build",
        "resolve",
        "last_use",
        "analyze",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect()
}

fn build_report(
    manifest: &Manifest,
    target: BenchTarget,
    samples: &[f64],
    passes_applied: Vec<String>,
    compiler_visible_allocs: Option<usize>,
) -> BenchReport {
    let stats = IterationStats::from_samples(samples);
    BenchReport {
        scenario: ScenarioMetadata {
            name: manifest.name.clone(),
            entry: manifest.entry.to_string_lossy().into_owned(),
            target: target.name().to_string(),
            iterations_count: manifest.iterations,
            warmup_count: manifest.warmup,
        },
        backend: BackendInfo::for_target(target),
        host: HostInfo::capture(),
        iterations: stats,
        response_bytes: None,
        expected_match: None,
        passes_applied,
        compiler_visible_allocs,
    }
}

/// Parse + run pipeline + count IR-level alloc sites. Same numbers
/// across `vm` / `wasm-gc` / `rust` since the policy is target-stable
/// (`NeutralAllocPolicy`). `None` only when parse/typecheck fails — in
/// that case the runner already returned an error before calling this,
/// so in practice the field is always populated for successful runs.
fn compute_visible_allocs(manifest: &Manifest) -> Option<usize> {
    let source = std::fs::read_to_string(&manifest.entry).ok()?;
    let mut items: Vec<TopLevel> = parse_source(&source).ok()?;
    let module_root = manifest
        .entry
        .parent()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_default();
    let res = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            ..Default::default()
        },
    );
    if let Some(tc) = &res.typecheck
        && !tc.errors.is_empty()
    {
        return None;
    }
    let policy = crate::ir::NeutralAllocPolicy;
    Some(crate::ir::count_alloc_sites_in_program(&items, &policy))
}
