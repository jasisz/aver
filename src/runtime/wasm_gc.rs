//! In-process wasm-gc executor — library entry point.
//!
//! The CLI binary's `aver run --wasm-gc` shells out to
//! [`run_in_process`] after parse + multi-module flatten; the
//! parity fuzz target and the verify executor share the same path
//! without paying a subprocess spawn per execution. Recording is
//! handed back as `RunOutcome::recorded_effects` rather than
//! persisted to disk — file I/O is a CLI concern, not the
//! runtime's, so embedders can decide whether to keep, compare,
//! or discard the trace.
//!
//! Compile-and-run path configured for engine GC + tail calls.
//! Effect imports are wired against native Rust implementations
//! (Console.print/error/warn cross via the `__rt_string_*` LM
//! transport bridge that every emitted module exports). Imports we
//! don't have a real impl for get an auto-stub that returns the
//! zero value of the declared result type — so a program that
//! *declares* an effect but doesn't run it through code still
//! instantiates cleanly.
//!
//! Pre-compile concerns (file I/O, module-graph loading, typecheck
//! formatting) stay in `src/main/run_wasm_gc.rs`; this module
//! starts at "already-parsed items" and ends at "decoded entry-fn
//! return value + recorder state".

#![cfg(feature = "wasm")]

use aver::ast::{TopLevel, Type};
use aver::ir::AnalysisResult;
use aver::replay::{EffectRecord, EffectReplayState, JsonValue, SessionRecording};
use aver::value::Value;

/// Contract plan plus the exact process-local Rust providers selected by the
/// project's target-neutral `[providers]` composition.
#[derive(Clone)]
pub struct CustomProviderConfig {
    pub plan: aver::codegen::wasm_gc::CapabilityWasmGcPlan,
    pub providers: aver::provider::ProviderRegistry,
}

/// What the recorder/replayer should do for this run.
///
/// `Replay` carries a heap-allocated `SessionRecording` (kilobytes
/// of effect trace) so the enum stays small enough to pass by value
/// through the run pipeline without inflating every `Normal` /
/// `Record` call to recording size — clippy's `large_enum_variant`
/// would otherwise flag the size mismatch.
#[derive(Default)]
pub enum EffectMode {
    /// Production path — real effects run, no recording, no replay.
    #[default]
    Normal,
    /// `aver run --wasm-gc --record <dir>` — real effects run, every
    /// call appended to the trace, returned through
    /// `RunOutcome::recorded_effects`. The lib executor never
    /// persists to disk; the CLI wrapper writes the JSON file when
    /// the user asked for it.
    Record,
    /// `aver replay <file> --wasm-gc` — effects bypass real I/O,
    /// values come from the trace via
    /// `EffectReplayState::replay_effect`. `bool` is `--check-args`:
    /// when true the recorded args must match the args the program
    /// supplies, else only effect type + sequence.
    Replay(Box<SessionRecording>, bool),
}

/// Caller-supplied configuration for one `run_in_process` call.
///
/// `Default` is the single-file embedding shape: no argv, whole-program
/// entry, real effects, empty alias map. A caller that flattened a
/// multi-module program MUST override `type_aliases` with the map
/// `flatten_multimodule` returned — leaving it empty makes qualified
/// dep-type spellings decline fail-closed instead of resolving their
/// canonical layouts.
pub struct RunConfig {
    /// Argv visible to the program through `Args.get` / `Args.all`.
    pub program_args: Vec<String>,
    /// `Some((fn_name, args))` selects an alternate entry function —
    /// `aver run --wasm-gc -e 'add(7, 35)'` or an `--expr` replay.
    /// `None` runs whichever of `main` / `_start` the module exports.
    pub entry_info: Option<(String, Vec<Value>)>,
    /// Recorder / replayer state machine. See [`EffectMode`].
    pub mode: EffectMode,
    /// Deployment settings for the native Tcp host bridge.
    pub tcp_settings: aver_rt::tcp::TcpSettings,
    /// Identity-preserving qualified type-name aliases returned by
    /// `flatten_multimodule` when the caller flattened a multi-module
    /// program. Empty for single-file programs (the fuzz harness path).
    pub type_aliases: std::collections::HashMap<String, String>,
    /// Internal differential hook. Production callers keep this `true`;
    /// representation tests set it to `false` to exercise boxed sequences.
    #[doc(hidden)]
    pub packed_sequences_enabled: bool,
    /// Program-defined capability imports served by the cached Rust provider
    /// host. `None` is the ordinary no-custom-provider embedding path.
    pub custom_providers: Option<CustomProviderConfig>,
}

impl Default for RunConfig {
    fn default() -> Self {
        Self {
            program_args: Vec::new(),
            entry_info: None,
            mode: EffectMode::default(),
            tcp_settings: aver_rt::tcp::TcpSettings::default(),
            type_aliases: std::collections::HashMap::new(),
            packed_sequences_enabled: true,
            custom_providers: None,
        }
    }
}

/// What one `run_in_process` invocation actually produced. Carries
/// the decoded entry-fn return value, recorder/replay progress, and
/// — only when `mode` was `Record` — the freshly captured effect
/// trace. The CLI wrapper turns that trace into a `SessionRecording`
/// + writes JSON; the fuzz harness drops it.
pub struct RunOutcome {
    pub output: JsonValue,
    pub effects_consumed: usize,
    pub effects_total: usize,
    pub args_diff_count: usize,
    /// `Some(effects)` only when `RunConfig::mode == EffectMode::Record`.
    /// Empty `Vec` is still `Some` — distinguishes "recorded zero
    /// effects" from "didn't record at all".
    pub recorded_effects: Option<Vec<EffectRecord>>,
}

/// Per-instance wasmtime store data. Exposed because the host
/// imports module ([`imports`]) needs to mutate it from inside the
/// linker closures; not part of the embedding contract.
pub struct RunWasmGcHost {
    pub program_args: Vec<String>,
    /// Recording state. `Some` only when the user passed `Record` or
    /// `Replay`; every effect call routes through `record_effect`
    /// before returning, so the resulting trace is identical in
    /// shape to the VM recorder's output. `None` is the production
    /// path — zero overhead beyond the `Option::is_some` check per
    /// effect call.
    pub recorder: Option<EffectReplayState>,
    /// Caller-fn name table, materialised at instantiation by walking
    /// `__caller_fn_count` + `__caller_fn_name(0..count)`. Per effect
    /// call, `imports::dispatch_aver_import` looks up
    /// `caller_fn_table[idx]` to stamp the recorded effect's
    /// `caller_fn` field. Cleared (kept empty) for modules that
    /// don't export the table.
    pub caller_fn_table: Vec<String>,
    /// Settings applied by the native host implementation of Tcp imports.
    pub tcp_settings: aver_rt::tcp::TcpSettings,
}

/// Compile `items` to wasm-gc bytes, instantiate inside an embedded
/// wasmtime engine, call the selected entry function, and return a
/// structured outcome. Every pipeline failure (codegen, instantiate,
/// trap, replay mismatch) maps onto a single `Err(String)` so batch
/// callers can keep iterating on the next program.
///
/// Caller is expected to have already run `aver::ir::pipeline::run`
/// (and any multi-module flatten + re-resolve) on `items`. Skipping
/// the multi-module step is fine for single-file programs — the fuzz
/// harness does this — but it means cross-module fn refs in `items`
/// would trip the wasm validator. The CLI wrapper handles the
/// multi-module path before getting here.
pub fn run_in_process(
    items: &[TopLevel],
    analysis: Option<&AnalysisResult>,
    config: RunConfig,
) -> Result<RunOutcome, String> {
    let bytes = match config.custom_providers.as_ref() {
        Some(custom) => {
            aver::codegen::wasm_gc::compile_to_wasm_gc_flattened_with_custom_capabilities(
                items,
                analysis,
                None,
                &config.type_aliases,
                &custom.plan,
                config.packed_sequences_enabled,
            )
        }
        None => aver::codegen::wasm_gc::compile_to_wasm_gc_flattened_with_options(
            items,
            analysis,
            None,
            aver::codegen::wasm_gc::TargetMode::AverBridge,
            &config.type_aliases,
            config.packed_sequences_enabled,
        ),
    }
    .map(|out| out.bytes)
    .map_err(|e| format!("{e}"))?;

    let entry_fn_name: &str = config
        .entry_info
        .as_ref()
        .map(|(n, _)| n.as_str())
        .unwrap_or("main");
    let return_ty = find_fn_return_type(items, entry_fn_name);
    run_wasm_gc_with_host(
        &bytes,
        &config.program_args,
        &config.mode,
        config.tcp_settings,
        config.entry_info.as_ref(),
        &return_ty,
        config.custom_providers.as_ref(),
    )
    .map_err(|e| format!("WASM execution error: {}", e))
}

/// Walk `__caller_fn_count` + `__caller_fn_name(0..count)` once at
/// instance creation, decode each name via the LM bridge, return the
/// resulting `Vec<String>` so per-call dispatch can index into it
/// without per-call LM round-trips. Empty when the module doesn't
/// export the table (programs without effect-emitting fns).
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
fn find_fn_return_type(items: &[TopLevel], name: &str) -> Type {
    for item in items {
        if let TopLevel::FnDef(fn_def) = item
            && fn_def.name == name
        {
            return aver::types::parse_type_str(&fn_def.return_type);
        }
    }
    Type::Unit
}

#[allow(clippy::too_many_arguments)]
fn run_wasm_gc_with_host(
    wasm_bytes: &[u8],
    program_args: &[String],
    mode: &EffectMode,
    tcp_settings: aver_rt::tcp::TcpSettings,
    entry_info: Option<&(String, Vec<Value>)>,
    return_ty: &Type,
    custom_providers: Option<&CustomProviderConfig>,
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
    // Wasm emission and Cranelift are separate compilation stages. Persist the
    // latter in Wasmtime's content/config-addressed cache so repeated
    // `aver run --wasm-gc` processes rehydrate native code instead of JITing
    // the identical module again.
    // A missing/unwritable user cache must not turn a valid program into a
    // runtime failure; it only forfeits the warm-start optimisation.
    if let Ok(cache) = Cache::from_file(None) {
        config.cache(Some(cache));
    }
    let engine = Engine::new(&config).map_err(|e| format!("engine: {e:#}"))?;
    let module = Module::new(&engine, wasm_bytes).map_err(|e| format!("module: {e:#}"))?;

    let mut recorder = match mode {
        EffectMode::Normal => None,
        EffectMode::Record => {
            let mut r = EffectReplayState::default();
            r.start_recording();
            Some(r)
        }
        EffectMode::Replay(recording, check_args) => {
            let mut r = EffectReplayState::default();
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
            tcp_settings,
        },
    );
    let mut linker: Linker<RunWasmGcHost> = Linker::new(&engine);
    let custom_imports = custom_providers
        .map(provider_host::operation_imports)
        .transpose()?;

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
        let custom_operation = custom_imports
            .as_ref()
            .and_then(|imports| imports.get(&(module_name.clone(), field_name.clone())))
            .cloned();
        if module_name.starts_with("aver:user/") && custom_operation.is_none() {
            return Err(format!(
                "error[capability-provider-adapter]: wasm-gc module imports unknown custom provider operation `{module_name}.{field_name}`"
            ));
        }
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
                    if let Some(operation) = custom_operation.as_ref() {
                        return provider_host::invoke(operation, &mut caller, params, results);
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
    // `imports::dispatch_aver_import` reads the trailing `i32` arg as
    // an idx into this vector — no LM round-trip per call.
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
    let main_output: JsonValue = if let Some((fn_name, args)) = entry_info {
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
        JsonValue::Null
    } else {
        return Err("module exports neither _start nor main".into());
    };

    // Snapshot replay/record progress + arg-diff count from the
    // recorder before any further consumption. We need these for
    // `RunOutcome` regardless of whether we also surface the
    // recorded trace to the caller.
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
    if matches!(mode, EffectMode::Replay(_, _))
        && let Some(r) = store.data().recorder.as_ref()
        && let Err(e) = r.ensure_replay_consumed()
    {
        return Err(format!("replay incomplete: {:?}", e));
    }

    // Hand the recorded trace back to the caller in Record mode so a
    // CLI wrapper can persist it as JSON, the fuzz harness can drop
    // it, etc. File I/O is intentionally not part of the lib path —
    // recording is just data here.
    let recorded_effects = if matches!(mode, EffectMode::Record)
        && let Some(rec) = store.data_mut().recorder.as_mut()
    {
        Some(rec.take_recorded_effects())
    } else {
        None
    };

    Ok(RunOutcome {
        output: main_output,
        effects_consumed,
        effects_total,
        args_diff_count,
        recorded_effects,
    })
}

mod decode;
mod imports;
mod provider_host;
