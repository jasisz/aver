//! Per-backend replay drivers + the unified outcome shape they
//! report back. Lifted out of `replay_cmd.rs` so the file's
//! `cmd_replay` / renderer / argument-decoding code stays separate
//! from the "compile, run, capture" mechanics each backend needs.
//!
//! Every backend exposes a `run_<x>_replay(...) ->
//! Result<BackendReplayOutcome, String>` that does the live re-run
//! under `EffectReplayState::Replay`, asserts the trace was fully
//! consumed, and reports back the four fields `build_replay_result`
//! turns into a `ReplayResult`. New backends only need to fill the
//! same shape — they can't sneak a `args_diffs: 0` hardcode back in
//! without the field-level compiler error pointing at it.

use std::path::Path;
use std::process::Command;

use aver::nan_value::{NanValue, NanValueConvert};
use aver::replay::{
    JsonValue, RecordedOutcome, SessionRecording, first_diff_path, format_json, value_to_json,
};
use aver::value::Value;
use aver::vm;

use super::super::commands::find_self_host_binary;
use super::super::shared::apply_runtime_policy_to_vm;
#[cfg(feature = "wasm")]
use super::super::shared::{parse_file, read_file};
#[cfg(feature = "wasm")]
use super::find_fn_line;
use super::{
    ReplayError, ReplayResult, decode_entry_args, find_json_line, resolve_replay_program_file,
};

/// Backend-agnostic shape every replay backend reports back. Lets
/// `replay_recording_file_*` functions stay focused on the
/// per-backend mechanics (compile, run, parse subprocess output)
/// while `build_replay_result` owns the `RecordedOutcome` ↔
/// `recording.output` comparison and the `ReplayResult` shape the
/// CLI renderer reads.
pub(super) struct BackendReplayOutcome {
    /// What the live re-run produced — JSON for normal returns, a
    /// runtime-error string for traps / unhandled errors.
    pub actual: RecordedOutcome,
    /// `replay_pos` after the entry fn returned (`replay_progress.0`).
    pub effects_consumed: usize,
    /// `replay_effects.len()` from the recording (`replay_progress.1`).
    pub effects_total: usize,
    /// Soft-warning counter: `replay_effect` calls whose args didn't
    /// match the recorded args. Only meaningful when `--check-args`
    /// is OFF — with the flag, mismatches are already hard errors
    /// inside `EffectReplayState::replay_effect`.
    pub args_diff_count: usize,
}

/// Build the renderer-facing `ReplayResult` from a single backend's
/// outcome. Centralises the `actual == recording.output` comparison
/// plus the `output_diff` / `recording_output_line` plumbing so each
/// backend doesn't reinvent them and a stray `args_diffs: 0`
/// hardcode can't sneak back in.
pub(super) fn build_replay_result(
    path: &Path,
    raw: &str,
    recording: &SessionRecording,
    replay_program_file: String,
    entry_line: usize,
    outcome: Result<BackendReplayOutcome, String>,
) -> ReplayResult {
    let recording_output_line = find_json_line(raw, "output");
    match outcome {
        Ok(out) => {
            let matched = out.actual == recording.output;
            let output_diff = if !matched {
                build_output_diff(&recording.output, &out.actual)
            } else {
                None
            };
            ReplayResult {
                recording_path: path.display().to_string(),
                program_file: replay_program_file,
                entry_fn: recording.entry_fn.clone(),
                entry_line,
                matched,
                effects_consumed: out.effects_consumed,
                effects_total: out.effects_total,
                error: None,
                output_diff,
                args_diffs: out.args_diff_count,
                recording_output_line,
            }
        }
        Err(e) => ReplayResult {
            recording_path: path.display().to_string(),
            program_file: replay_program_file,
            entry_fn: recording.entry_fn.clone(),
            entry_line,
            matched: false,
            effects_consumed: 0,
            effects_total: recording.effects.len(),
            error: Some(ReplayError::Generic(e)),
            output_diff: None,
            args_diffs: 0,
            recording_output_line,
        },
    }
}

fn build_output_diff(
    expected: &RecordedOutcome,
    actual: &RecordedOutcome,
) -> Option<(String, String, Option<String>)> {
    match (expected, actual) {
        (RecordedOutcome::Value(exp), RecordedOutcome::Value(got)) => {
            let diff_path = first_diff_path(exp, got).map(|p| p.to_string());
            Some((format_json(exp), format_json(got), diff_path))
        }
        (RecordedOutcome::RuntimeError(exp), RecordedOutcome::RuntimeError(got)) => Some((
            format!("runtime_error: {}", exp),
            format!("runtime_error: {}", got),
            None,
        )),
        (exp, got) => Some((format!("{:?}", exp), format!("{:?}", got), None)),
    }
}

/// Core VM replay path, returning the backend-agnostic outcome. Wraps
/// the original "compile, instantiate VM, drive replay" sequence.
/// Pipeline / `vm::compile_program_with_modules` errors come back as
/// `Err` instead of `?` so a single bad recording in `aver replay
/// <dir>` no longer skips later files via early return.
pub(super) fn run_vm_replay(
    recording: &SessionRecording,
    replay_module_root: &str,
    items: &mut Vec<aver::ast::TopLevel>,
    check_args: bool,
) -> Result<BackendReplayOutcome, String> {
    // Preload dep modules so the entry SymbolTable knows about every
    // cross-module call — same shape as `cmd_run_vm`. Without this
    // the VM compiler's per-dep resolver would re-derive identities
    // it could read straight off `pipeline_result.symbol_table`.
    let depends = items
        .iter()
        .find_map(|i| match i {
            aver::ast::TopLevel::Module(m) => Some(m.depends.clone()),
            _ => None,
        })
        .unwrap_or_default();
    let loaded = aver::source::load_module_tree(&depends, replay_module_root)
        .map_err(|e| format!("dep load: {}", e))?;
    let dep_modules: Vec<aver::codegen::ModuleInfo> = loaded
        .iter()
        .map(aver::codegen::ModuleInfo::from_loaded)
        .collect();

    // Full pipeline. Recordings store effect syscalls (`Console.print(...)`
    // with concrete args) — they don't reference IR shape or bytecode, so
    // running interp_lower + buffer_build during replay produces the same
    // effect sequence as the original recording.
    let pipeline_result = aver::ir::pipeline::run(
        items,
        aver::ir::PipelineConfig {
            typecheck: Some(aver::ir::TypecheckMode::Full {
                base_dir: Some(replay_module_root),
            }),
            dep_modules: &dep_modules,
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        return Err(crate::shared::format_type_errors(&tc_result.errors));
    }

    let mut arena = aver::nan_value::Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_modules(
        &pipeline_result.resolved_items,
        &pipeline_result.symbol_table,
        &mut arena,
        Some(replay_module_root),
        &recording.program_file,
        pipeline_result.analysis.as_ref(),
    )
    .map_err(|e| format!("VM compile error: {}", e))?;
    let providers = aver::provider::ProviderRegistry::for_program(tc_result.capabilities.clone())
        .map(std::sync::Arc::new)?;
    let mut machine = vm::VM::new(code, globals, arena);
    machine.set_provider_registry(providers);
    apply_runtime_policy_to_vm(&mut machine, replay_module_root)?;
    machine.start_replay_with_provenance(
        recording.effects.clone(),
        &recording.capabilities,
        check_args,
    )?;

    let progress_msg = |machine: &vm::VM, e: &dyn std::fmt::Display| -> String {
        let (consumed, total) = machine.replay_progress();
        format!(
            "Replay failed: {}\nProgress: consumed {} of {} recorded effects",
            e, consumed, total
        )
    };

    machine
        .run_top_level()
        .map_err(|e| progress_msg(&machine, &e))?;

    let entry_args = decode_entry_args(&recording.input)?;
    let nv_args: Vec<NanValue> = entry_args
        .iter()
        .map(|v| NanValue::from_value(v, &mut machine.arena))
        .collect();

    let run_out = machine
        .run_named_function(&recording.entry_fn, &nv_args)
        .map_err(|e| progress_msg(&machine, &e))?;

    let actual = if run_out.is_err() {
        let inner = run_out.wrapper_inner(&machine.arena);
        RecordedOutcome::RuntimeError(format!(
            "{} returned error: {}",
            recording.entry_fn,
            inner.repr(&machine.arena)
        ))
    } else {
        let val = run_out.to_value(&machine.arena);
        RecordedOutcome::Value(value_to_json(&val)?)
    };

    machine
        .ensure_replay_consumed()
        .map_err(|e| progress_msg(&machine, &format!("{:?}", e) as &dyn std::fmt::Display))?;

    let (effects_consumed, effects_total) = machine.replay_progress();
    Ok(BackendReplayOutcome {
        actual,
        effects_consumed,
        effects_total,
        args_diff_count: machine.args_diff_count(),
    })
}

/// Core wasm-gc replay path, returning the backend-agnostic outcome.
/// Re-materialises the entry call from the recording (default
/// `main` ↔ no `entry_info`; `--expr` recording ↔ `(fn_name,
/// decoded_args)`), drives the wasm-gc executor under
/// `EffectMode::Replay`, and maps `RunOutcome` onto the shared
/// shape `build_replay_result` consumes.
#[cfg(feature = "wasm")]
pub(super) fn run_wasm_gc_replay(
    recording: &SessionRecording,
    replay_module_root: &str,
    replay_program_file: &str,
    check_args: bool,
) -> Result<BackendReplayOutcome, String> {
    let entry_info: Option<(String, Vec<Value>)> =
        if recording.entry_fn == "main" && matches!(&recording.input, JsonValue::Null) {
            None
        } else {
            let args = decode_entry_args(&recording.input)?;
            Some((recording.entry_fn.clone(), args))
        };
    let mode =
        super::super::run_wasm_gc::EffectMode::Replay(Box::new(recording.clone()), check_args);
    let run = super::super::run_wasm_gc::try_run_wasm_gc(
        replay_program_file,
        Some(replay_module_root),
        Vec::new(),
        mode,
        entry_info,
        None,
    )?;
    Ok(BackendReplayOutcome {
        actual: RecordedOutcome::Value(run.output),
        effects_consumed: run.effects_consumed,
        effects_total: run.effects_total,
        args_diff_count: run.args_diff_count,
    })
}

/// Look up a function definition's source line by re-parsing the
/// program file. Best-effort: returns 1 if the file is unreadable or
/// the function isn't found at module level — same fallback the VM
/// path uses through its already-parsed AST.
#[cfg(feature = "wasm")]
pub(super) fn find_fn_line_in_file(program_file: &str, name: &str) -> usize {
    match read_file(program_file).and_then(|src| parse_file(&src)) {
        Ok(items) => find_fn_line(&items, name),
        Err(_) => 1,
    }
}

/// Core self-host replay path, returning the backend-agnostic
/// outcome. Spawns the cached self-host binary against the recording
/// and treats a successful exit as a full match — the self-host
/// runtime owns its own `ensure_replay_consumed` check internally and
/// signals failure via non-zero exit, so a clean exit means the
/// entire trace was consumed. Output value comparison is not yet
/// wired here (the binary doesn't surface the entry-fn return as
/// JSON), so `actual` mirrors `recording.output` to keep the unified
/// outcome shape honest with what the binary actually verified.
pub(super) fn run_self_host_replay(
    recording: &SessionRecording,
    replay_module_root: &str,
    recording_path: &Path,
    check_args: bool,
) -> Result<BackendReplayOutcome, String> {
    let replay_program_file = resolve_replay_program_file(recording, replay_module_root);
    // Same host/guest divergence gate as `aver run --self-host`: a
    // discharged literal smart-constructor call means the guest resolver
    // would build a `Result` the host-checked source no longer expects.
    if let Ok(source) = super::super::shared::read_file(&replay_program_file)
        && let Ok(items) = super::super::shared::parse_file(&source)
    {
        super::super::commands::reject_literal_refinement_discharge(
            &items,
            Some(replay_module_root),
        )?;
        // The recording names a source path, and nothing binds that file to
        // what was recorded — it can have been edited into a program no other
        // door accepts. Ask the shadowing ban about it before handing the
        // source to a runtime that will execute it.
        let shadow = aver::resolver::check_shadowing(&items);
        if let Some(first) = shadow.first() {
            return Err(format!(
                "error[{}:{}]: {}",
                first.line, first.col, first.message
            ));
        }
    }
    let binary_path = find_self_host_binary()?;
    let guest_args = decode_self_host_guest_args(&recording.input)?;

    let mut command = Command::new(&binary_path);
    command
        .arg(&replay_program_file)
        .arg(replay_module_root)
        .args(&guest_args)
        .env("AVER_REPLAY_ENTRY_FN", "main")
        .env("AVER_REPLAY_REPLAY", recording_path)
        .env("AVER_REPLAY_MODULE_ROOT", replay_module_root)
        .env_remove("AVER_REPLAY_RECORD")
        .env_remove("AVER_REPLAY_REQUEST_ID")
        .env_remove("AVER_REPLAY_TIMESTAMP")
        .env_remove("AVER_REPLAY_PROGRAM_FILE");
    if check_args {
        command.env("AVER_REPLAY_CHECK_ARGS", "1");
    } else {
        command.env_remove("AVER_REPLAY_CHECK_ARGS");
    }

    let output = command.output().map_err(|e| {
        format!(
            "Failed to run cached self-host replay binary '{}': {}",
            binary_path.display(),
            e
        )
    })?;

    let stdout_text = String::from_utf8_lossy(&output.stdout);
    // Self-host runtime prints `__aver_return__: <json>` right
    // before its own internal output comparison. Pulling it out
    // here lets `build_replay_result` do the `actual ==
    // recording.output` check via the unified shape instead of us
    // relying on the subprocess exit code as a proxy for "matched"
    // (which previously hardcoded `actual = recording.output.clone()`
    // and reported `MATCH` even when the underlying value diverged).
    let parsed_actual = parse_self_host_return_marker(&stdout_text);

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        let mut msg = "Self-host replay failed".to_string();
        let trimmed_stdout = stdout_text.trim();
        if !trimmed_stdout.is_empty() {
            msg.push_str(&format!("\nstdout:\n{}", trimmed_stdout));
        }
        if !stderr.is_empty() {
            msg.push_str(&format!("\nstderr:\n{}", stderr));
        }
        return Err(msg);
    }

    let n = recording.effects.len();
    let actual = parsed_actual.unwrap_or_else(|| recording.output.clone());
    Ok(BackendReplayOutcome {
        actual,
        effects_consumed: n,
        effects_total: n,
        args_diff_count: 0,
    })
}

/// Extract the recorded return value from the self-host subprocess'
/// stdout. The runtime emits `__aver_return__: <json>` (one line,
/// JSON value only) right before its replay-mode output comparison,
/// so scanning the output for the prefix and parsing the rest of
/// the line through `aver::replay::parse_json` recovers the live
/// `actual` value. Returns `None` if the subprocess never emitted
/// the marker (e.g. a self-host binary built before this contract
/// landed) so callers can fall back to the previous behaviour.
fn parse_self_host_return_marker(stdout: &str) -> Option<RecordedOutcome> {
    const MARKER: &str = "__aver_return__:";
    for line in stdout.lines() {
        if let Some(rest) = line.strip_prefix(MARKER) {
            let json_str = rest.trim();
            if let Ok(json) = aver::replay::parse_json(json_str) {
                return Some(RecordedOutcome::Value(json));
            }
        }
    }
    None
}

pub(super) fn decode_self_host_guest_args(input: &JsonValue) -> Result<Vec<String>, String> {
    decode_entry_args(input)?
        .into_iter()
        .enumerate()
        .map(|(idx, value)| match value {
            Value::Str(s) => Ok(s),
            other => Err(format!(
                "Self-host replay expects guest input as List<String>; item {} was {}",
                idx,
                aver::value::aver_repr(&other)
            )),
        })
        .collect()
}
