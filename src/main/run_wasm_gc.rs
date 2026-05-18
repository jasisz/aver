//! `aver run --wasm-gc` — embedded wasmtime host for the wasm-gc backend.
//!
//! Compile-and-run path configured for engine
//! GC + tail calls. Effect imports are wired against native Rust
//! implementations (Console.print/error/warn cross via the
//! `__rt_string_*` LM transport bridge that every emitted module
//! exports). Imports we don't have a real impl for get an auto-stub
//! that returns the zero value of the declared result type — so a
//! program that *declares* an effect but doesn't run it through code
//! still instantiates cleanly.

use colored::Colorize;
use std::process;

#[cfg(feature = "wasm")]
use super::shared;
#[cfg(feature = "wasm")]
use super::shared::{parse_file, read_file, resolve_module_root_for_entry};

#[cfg(feature = "wasm")]
use super::commands::{flatten_multimodule, load_compile_deps};

/// What the recorder/replayer should do for this run.
///
/// `Replaying` carries a heap-allocated `SessionRecording` (kilobytes
/// of effect trace) so the enum stays small enough to pass by value
/// through the run pipeline without inflating every `Normal` /
/// `Recording` call to recording size — clippy's `large_enum_variant`
/// would otherwise flag the size mismatch.
pub(super) enum EffectMode<'a> {
    /// Production path — real effects run, no recording, no replay.
    Normal,
    /// `aver run --wasm-gc --record <dir>` — real effects run, every
    /// call appended to the trace, persisted to `<dir>/<request>.json`.
    Recording(#[allow(dead_code)] &'a str),
    /// `aver replay <file> --wasm-gc` — effects bypass real I/O, values
    /// come from the trace via `EffectReplayState::replay_effect`.
    /// `bool` is `--check-args`: when true the recorded args must match
    /// the args the program supplies, else only effect type + sequence.
    #[cfg(feature = "wasm")]
    #[allow(dead_code)]
    Replaying(Box<aver::replay::SessionRecording>, bool),
    #[cfg(not(feature = "wasm"))]
    #[allow(dead_code)]
    Replaying((), bool),
}

pub(super) fn cmd_run_wasm_gc(
    file: &str,
    module_root_override: Option<&str>,
    program_args: Vec<String>,
    record_dir: Option<&str>,
    entry_expr: Option<&str>,
) {
    let mode = match record_dir {
        Some(dir) => EffectMode::Recording(dir),
        None => EffectMode::Normal,
    };
    let entry_info = match entry_expr {
        Some(src) => match super::shared::parse_call_expression(src) {
            Ok(info) => Some(info),
            Err(e) => {
                eprintln!("{}", format!("--expr: {}", e).red());
                process::exit(1);
            }
        },
        None => None,
    };
    cmd_run_wasm_gc_with_mode(file, module_root_override, program_args, mode, entry_info);
}

/// CLI wrapper: pretty-prints any failure (parse / typecheck /
/// codegen / runtime) and exits 1. Internal callers that need to
/// keep iterating (`aver replay <dir>`) should use
/// `try_run_wasm_gc` directly and inspect the `Result`.
pub(super) fn cmd_run_wasm_gc_with_mode(
    file: &str,
    module_root_override: Option<&str>,
    program_args: Vec<String>,
    mode: EffectMode<'_>,
    entry_info: Option<(String, Vec<aver::value::Value>)>,
) {
    if let Err(e) = try_run_wasm_gc(file, module_root_override, program_args, mode, entry_info) {
        eprintln!("{}", e.red());
        process::exit(1);
    }
}

/// What one `try_run_wasm_gc` invocation actually produced. Carries
/// just the fields the replay path consumes — the decoded entry-fn
/// return value (compared against `recording.output`), how many
/// effects the program consumed vs how many the recording held, and
/// the soft-warning count of `replay_effect` calls whose args
/// diverged from the recording without `--check-args`. The
/// unconsumed-trailer check is enforced inside `try_run_wasm_gc`
/// itself by calling `EffectReplayState::ensure_replay_consumed` —
/// callers see the failure as an `Err(String)`, not as a derived
/// field here.
pub(super) struct RunOutcome {
    #[allow(dead_code)]
    pub output: aver::replay::JsonValue,
    #[allow(dead_code)]
    pub effects_consumed: usize,
    #[allow(dead_code)]
    pub effects_total: usize,
    #[allow(dead_code)]
    pub args_diff_count: usize,
}

/// Pure `Result`-returning entry point for the wasm-gc executor.
/// Compiles, instantiates, runs the entry fn, persists the trace
/// (record mode) or asserts full trace consumption (replay mode),
/// and returns progress info — all without `process::exit`. Every
/// pipeline stage (parse, typecheck, codegen, instantiate, trap,
/// replay failure) maps onto a single `Err(String)` so batch
/// callers can keep going on the next file.
pub(super) fn try_run_wasm_gc(
    file: &str,
    module_root_override: Option<&str>,
    program_args: Vec<String>,
    mode: EffectMode<'_>,
    entry_info: Option<(String, Vec<aver::value::Value>)>,
) -> Result<RunOutcome, String> {
    #[cfg(not(feature = "wasm"))]
    {
        let _ = (file, module_root_override, program_args, mode, entry_info);
        Err("WASM requires --features wasm".to_string())
    }

    #[cfg(feature = "wasm")]
    {
        use aver::codegen::wasm_gc;
        use aver::ir::{NeutralAllocPolicy, PipelineConfig, TypecheckMode};

        let module_root = resolve_module_root_for_entry(file, module_root_override);
        let source = read_file(file)?;
        let mut items = parse_file(&source)?;
        let neutral_policy = NeutralAllocPolicy;
        let result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full {
                    base_dir: Some(&module_root),
                }),
                alloc_policy: Some(&neutral_policy),
                run_interp_lower: false,
                run_buffer_build: false,
                ..Default::default()
            },
        );
        if let Some(tc) = &result.typecheck
            && !tc.errors.is_empty()
        {
            return Err(shared::format_type_errors(&tc.errors));
        }
        let dep_modules = load_compile_deps(&items, &module_root, false, false, false);
        flatten_multimodule(&mut items, &dep_modules);
        // Re-run resolver after multi-module flatten so the freshly
        // appended dep fns get a `FnResolution` (slot map + slot_types).
        // The first `pipeline::run` only saw entry items; without this
        // pass, dep fn bodies fall back to the `slots::build_for_fn`
        // params-only path and any local beyond a param trips the wasm
        // validator with a slot-type mismatch.
        aver::ir::pipeline::resolve(&mut items);

        let bytes = wasm_gc::compile_to_wasm_gc(&items, result.analysis.as_ref())
            .map_err(|e| format!("{e}"))?;

        let entry_fn_name: &str = entry_info
            .as_ref()
            .map(|(n, _)| n.as_str())
            .unwrap_or("main");
        let return_ty = find_fn_return_type(&items, entry_fn_name);
        run_wasm_gc_with_host(
            &bytes,
            &program_args,
            &mode,
            file,
            &module_root,
            entry_info.as_ref(),
            &return_ty,
        )
        .map_err(|e| format!("WASM execution error: {}", e))
    }
}

#[cfg(feature = "wasm")]
pub(super) struct RunWasmGcHost {
    pub(super) program_args: Vec<String>,
    /// Recording state. `Some` only when the user passed `--record <dir>`;
    /// every effect call routes through `record_effect` before returning,
    /// so the resulting trace is identical in shape to the VM recorder's
    /// output. `None` is the production path — zero overhead beyond the
    /// `Option::is_some` check per effect call.
    pub(super) recorder: Option<aver::replay::EffectReplayState>,
    /// Caller-fn name table, materialised at instantiation by walking
    /// `__caller_fn_count` + `__caller_fn_name(0..count)`. Per effect
    /// call, `imports.rs::dispatch_aver_import` looks up
    /// `caller_fn_table[idx]` to stamp the recorded effect's
    /// `caller_fn` field. Cleared (kept empty) for modules that don't
    /// export the table.
    pub(super) caller_fn_table: Vec<String>,
}

/// Walk `__caller_fn_count` + `__caller_fn_name(0..count)` once at
/// instance creation, decode each name via the LM bridge, return the
/// resulting `Vec<String>` so per-call dispatch can index into it
/// without per-call LM round-trips. Empty when the module doesn't
/// export the table (programs without effect-emitting fns).
#[cfg(feature = "wasm")]
fn build_caller_fn_table(
    store: &mut wasmtime::Store<RunWasmGcHost>,
    instance: &wasmtime::Instance,
) -> Result<Vec<String>, String> {
    use wasmtime::Val;
    let count_fn = match instance.get_func(&mut *store, "__caller_fn_count") {
        Some(f) => f,
        None => return Ok(Vec::new()),
    };
    let name_fn = match instance.get_func(&mut *store, "__caller_fn_name") {
        Some(f) => f,
        None => return Ok(Vec::new()),
    };
    let to_lm = match instance.get_func(&mut *store, "__rt_string_to_lm") {
        Some(f) => f,
        None => return Ok(Vec::new()),
    };
    let memory = match instance.get_memory(&mut *store, "memory") {
        Some(m) => m,
        None => return Ok(Vec::new()),
    };

    let mut count_out = [Val::I32(0)];
    count_fn
        .call(&mut *store, &[], &mut count_out)
        .map_err(|e| format!("__caller_fn_count: {e:#}"))?;
    let count = match count_out[0] {
        Val::I32(n) => n.max(0) as usize,
        _ => 0,
    };

    let mut out = Vec::with_capacity(count);
    let mut name_out = [Val::AnyRef(None)];
    let mut len_out = [Val::I32(0)];
    for i in 0..count {
        name_fn
            .call(&mut *store, &[Val::I32(i as i32)], &mut name_out)
            .map_err(|e| format!("__caller_fn_name({i}): {e:#}"))?;
        // Decode via __rt_string_to_lm: writes bytes into LM at
        // offset 0, returns the byte length on the i32 return.
        let any_ref = match &name_out[0] {
            Val::AnyRef(Some(r)) => Val::AnyRef(Some(*r)),
            _ => {
                out.push("main".to_string());
                continue;
            }
        };
        to_lm
            .call(&mut *store, &[any_ref], &mut len_out)
            .map_err(|e| format!("__rt_string_to_lm: {e:#}"))?;
        let len = match len_out[0] {
            Val::I32(n) => n.max(0) as usize,
            _ => 0,
        };
        let mut buf = vec![0u8; len];
        if len > 0 {
            memory
                .read(&*store, 0, &mut buf)
                .map_err(|e| format!("read caller_fn name {i}: {e:#}"))?;
        }
        out.push(String::from_utf8_lossy(&buf).into_owned());
    }
    Ok(out)
}

/// Walk the parsed AST for a `fn <name>(...) -> T` definition and
/// return `T` as a structured `Type`. Falls back to `Type::Unit` when
/// the function isn't declared at module level (e.g. `_start`-only
/// shapes, or a user-supplied entry that doesn't match anything).
/// Multi-module loading appends dep fns to `items`, so we filter on
/// name only.
#[cfg(feature = "wasm")]
fn find_fn_return_type(items: &[aver::ast::TopLevel], name: &str) -> aver::ast::Type {
    use aver::ast::TopLevel;
    for item in items {
        if let TopLevel::FnDef(fn_def) = item
            && fn_def.name == name
        {
            return aver::types::parse_type_str(&fn_def.return_type);
        }
    }
    aver::ast::Type::Unit
}

#[cfg(feature = "wasm")]
#[allow(clippy::too_many_arguments)]
fn run_wasm_gc_with_host(
    wasm_bytes: &[u8],
    program_args: &[String],
    mode: &EffectMode<'_>,
    source_file: &str,
    module_root: &str,
    entry_info: Option<&(String, Vec<aver::value::Value>)>,
    return_ty: &aver::ast::Type,
) -> Result<RunOutcome, String> {
    use wasmtime::*;

    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    config.cranelift_opt_level(OptLevel::Speed);
    config.max_wasm_stack(8 * 1024 * 1024);
    // `component-model-async` (pulled in by the `wasip2` feature) enables
    // the runtime's async path; in that mode wasmtime enforces
    // `max_wasm_stack <= async_stack_size`. Default async stack is 2 MiB,
    // so the 8 MiB max above would trip `Engine::new` validation. Pin the
    // async stack at 12 MiB to keep both paths happy. No-op when async
    // support is off (unrelated builds).
    config.async_stack_size(12 * 1024 * 1024);
    let engine = Engine::new(&config).map_err(|e| format!("engine: {e:#}"))?;
    let module = Module::new(&engine, wasm_bytes).map_err(|e| format!("module: {e:#}"))?;

    let mut recorder = match mode {
        EffectMode::Normal => None,
        EffectMode::Recording(_) => {
            let mut r = aver::replay::EffectReplayState::default();
            r.start_recording();
            Some(r)
        }
        EffectMode::Replaying(recording, check_args) => {
            let mut r = aver::replay::EffectReplayState::default();
            r.start_replay(recording.effects.clone(), *check_args);
            Some(r)
        }
    };
    let mut store = Store::new(
        &engine,
        RunWasmGcHost {
            program_args: program_args.to_vec(),
            recorder: recorder.take(),
            caller_fn_table: Vec::new(),
        },
    );
    let mut linker: Linker<RunWasmGcHost> = Linker::new(&engine);

    // One walk over imports — for every `(module, name)` declared by
    // the wasm module, register a host fn that uses the import's own
    // FuncType (so engine-side type identity matches without manual
    // sub-typing) and dispatches per name. Defaults to a typed-zero
    // stub when we don't have a real impl. Programs that declare an
    // effect but never call it instantiate cleanly; programs that do
    // call it get real semantics.
    for import in module.imports() {
        let ExternType::Func(ft) = import.ty() else {
            continue;
        };
        let module_name = import.module().to_string();
        let field_name = import.name().to_string();
        let result_tys: Vec<ValType> = ft.results().collect();
        let func_ty = FuncType::new(&engine, ft.params(), ft.results());
        let module_name_for_closure = module_name.clone();
        let field_name_for_closure = field_name.clone();
        linker
            .func_new(
                &module_name,
                &field_name,
                func_ty,
                move |mut caller: Caller<'_, RunWasmGcHost>,
                      params: &[Val],
                      results: &mut [Val]|
                      -> Result<(), wasmtime::Error> {
                    if module_name_for_closure == "aver"
                        && imports::dispatch_aver_import(
                            &field_name_for_closure,
                            &mut caller,
                            params,
                            results,
                        )?
                    {
                        return Ok(());
                    }
                    for (slot, ty) in results.iter_mut().zip(result_tys.iter()) {
                        *slot = match ty {
                            ValType::I32 => Val::I32(0),
                            ValType::I64 => Val::I64(0),
                            ValType::F32 => Val::F32(0),
                            ValType::F64 => Val::F64(0),
                            ValType::V128 => Val::V128(0u128.into()),
                            ValType::Ref(_) => Val::AnyRef(None),
                        };
                    }
                    Ok(())
                },
            )
            .map_err(|e| format!("link {module_name}.{field_name}: {e:#}"))?;
    }

    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|e| format!("instantiate: {e:#}"))?;

    // Materialise the caller-fn name table. The compiler exports
    // `__caller_fn_count() -> i32` and `__caller_fn_name(i32) -> ref
    // null $string` whenever any user fn might emit caller_fn (i.e.
    // the program has fn defs); we walk `0..count` once, decode each
    // ref via the LM bridge, and cache the strings in
    // `RunWasmGcHost::caller_fn_table`. Per effect call,
    // `imports.rs::dispatch_aver_import` reads the trailing `i32`
    // arg as an idx into this vector — no LM round-trip per call.
    let caller_fn_table = build_caller_fn_table(&mut store, &instance)?;
    store.data_mut().caller_fn_table = caller_fn_table;

    // Two entry shapes:
    //
    // - `entry_info = Some((fn_name, args))` — `aver run --wasm-gc -e
    //   'add(7, 35)'` or replay of an `--expr` recording. Look the
    //   named export up directly, convert the literal `Value` args to
    //   `wasmtime::Val`, decode the return through the typed decoder.
    // - `entry_info = None` — the default whole-program flow. Prefer
    //   `main` over `_start` when both are exported. The wasm-gc
    //   codegen synthesises `_start` as a thin `call $main; drop`
    //   wrapper (void return), so calling it would discard the user
    //   `main`'s return value — and that value is what the recorder
    //   persists as `output` and what the replayer compares against.
    //   `_start` remains the fallback for WASI / synth-handler shapes
    //   where there is no Aver-level `main` export.
    let main_output: aver::replay::JsonValue = if let Some((fn_name, args)) = entry_info {
        let func = instance.get_func(&mut store, fn_name).ok_or_else(|| {
            format!(
                "entry function '{}' not exported by wasm-gc module",
                fn_name
            )
        })?;
        let arg_vals = decode::encode_entry_args_for_wasm_gc(&mut store, &instance, args)?;
        let n = func.ty(&store).results().len();
        let mut out: Vec<Val> = (0..n).map(|_| Val::I32(0)).collect();
        func.call(&mut store, &arg_vals, &mut out)
            .map_err(|e| format!("entry '{}' trap: {e:#}", fn_name))?;
        decode::decode_main_return_typed(&mut store, &instance, &out, return_ty)?
    } else if let Some(main) = instance.get_func(&mut store, "main") {
        let n = main.ty(&store).results().len();
        let mut out: Vec<Val> = (0..n).map(|_| Val::I32(0)).collect();
        main.call(&mut store, &[], &mut out)
            .map_err(|e| format!("main trap: {e:#}"))?;
        decode::decode_main_return_typed(&mut store, &instance, &out, return_ty)?
    } else if let Some(start) = instance.get_func(&mut store, "_start") {
        start
            .call(&mut store, &[], &mut [])
            .map_err(|e| format!("_start trap: {e:#}"))?;
        aver::replay::JsonValue::Null
    } else {
        return Err("module exports neither _start nor main".into());
    };

    // Snapshot replay/record progress + arg-diff count from the
    // recorder before any further consumption. We need these for
    // `RunOutcome` regardless of whether we also persist a trace
    // below (record path) or surface the values to the replay
    // caller (replay path).
    let (effects_consumed, effects_total, args_diff_count) = match store.data().recorder.as_ref() {
        Some(r) if r.mode() == aver::replay::EffectReplayMode::Replay => {
            let (consumed, total) = r.replay_progress();
            (consumed, total, r.args_diff_count())
        }
        Some(r) if r.mode() == aver::replay::EffectReplayMode::Record => {
            let n = r.recorded_effects().len();
            (n, n, 0)
        }
        _ => (0, 0, 0),
    };

    // In replay mode, fail if the program didn't consume the whole
    // trace. A prefix-match with the recorded `output` would
    // otherwise pass as MATCH even though the original run produced
    // strictly more effects than this re-run did. Mirrors what the
    // VM replayer does with `machine.ensure_replay_consumed()`.
    if matches!(mode, EffectMode::Replaying(_, _))
        && let Some(r) = store.data().recorder.as_ref()
        && let Err(e) = r.ensure_replay_consumed()
    {
        return Err(format!("replay incomplete: {:?}", e));
    }

    // Persist the trace. Same JSON shape the VM recorder writes, so
    // existing `aver replay <file>` consumers (CLI, tests, agent
    // tooling) handle wasm-gc traces identically.
    if let EffectMode::Recording(dir) = mode
        && let Some(mut rec) = store.data_mut().recorder.take()
    {
        let request_id = super::commands::generate_request_id();
        let timestamp = super::commands::generate_timestamp();
        let (record_program_file, record_module_root) =
            super::commands::recording_paths(source_file, module_root);
        // For `--expr` runs use the readable `add-7-35` style stem;
        // default `main` runs keep the timestamped request id. Same
        // shape the VM recorder writes, so existing tooling that
        // looks up traces by filename works either way.
        let file_stem = match entry_info {
            Some((fn_name, args)) => aver::replay::recording_stem(fn_name, args),
            None => request_id.clone(),
        };
        let out_path = super::commands::prepare_recording_path(dir, &file_stem)
            .map_err(|e| format!("prepare recording path: {}", e))?;
        let entry_fn_label = entry_info
            .map(|(n, _)| n.clone())
            .unwrap_or_else(|| "main".to_string());
        let input = match entry_info {
            Some((_, args)) => aver::replay::encode_entry_args(args)
                .map_err(|e| format!("encode entry args: {}", e))?,
            None => aver::replay::JsonValue::Null,
        };
        let recording = aver::replay::SessionRecording {
            schema_version: 1,
            request_id,
            timestamp,
            program_file: record_program_file,
            module_root: record_module_root,
            entry_fn: entry_fn_label,
            input,
            effects: rec.take_recorded_effects(),
            output: aver::replay::RecordedOutcome::Value(main_output.clone()),
        };
        let json = aver::replay::session_recording_to_string_pretty(&recording);
        std::fs::write(&out_path, json)
            .map_err(|e| format!("write recording {}: {}", out_path.display(), e))?;
        eprintln!("Recorded → {}", out_path.display());
    }

    Ok(RunOutcome {
        output: main_output,
        effects_consumed,
        effects_total,
        args_diff_count,
    })
}

#[cfg(feature = "wasm")]
#[path = "run_wasm_gc/decode.rs"]
mod decode;

#[cfg(feature = "wasm")]
#[path = "run_wasm_gc/imports.rs"]
mod imports;
