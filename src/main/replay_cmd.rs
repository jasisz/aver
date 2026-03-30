use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Command};

use colored::Colorize;

use aver::ast::TopLevel;
use aver::interpreter::{Interpreter, Value, aver_repr};
use aver::nan_value::NanValue;
use aver::replay::{
    JsonValue, RecordedOutcome, SessionRecording, first_diff_path, format_json, json_to_value,
    parse_session_recording, value_to_json,
};
use aver::resolver;
use aver::tco;
use aver::types::checker::run_type_check_full;
use aver::value::{RuntimeError, list_to_vec};
use aver::vm;

use crate::commands::build_self_host_binary;
use crate::shared::{apply_runtime_policy_to_vm, compile_program_for_exec, parse_file, read_file};

fn collect_recording_files_from_dir(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries = fs::read_dir(dir)
        .map_err(|e| format!("Cannot read recording directory '{}': {}", dir.display(), e))?;
    for entry in entries {
        let entry = entry.map_err(|e| e.to_string())?;
        let entry_path = entry.path();
        if entry_path.is_dir() {
            collect_recording_files_from_dir(&entry_path, files)?;
            continue;
        }
        if entry_path.is_file()
            && entry_path
                .extension()
                .and_then(|s| s.to_str())
                .map(|s| s.eq_ignore_ascii_case("json"))
                .unwrap_or(false)
        {
            files.push(entry_path);
        }
    }
    Ok(())
}

pub(super) fn collect_recording_files(path: &str) -> Result<Vec<PathBuf>, String> {
    let p = Path::new(path);
    if p.is_file() {
        return Ok(vec![p.to_path_buf()]);
    }
    if !p.is_dir() {
        return Err(format!(
            "Recording path '{}' is neither file nor directory",
            path
        ));
    }
    let mut files = Vec::new();
    collect_recording_files_from_dir(p, &mut files)?;
    files.sort();
    if files.is_empty() {
        return Err(format!(
            "No .json recordings found in '{}' or its subdirectories",
            path
        ));
    }
    Ok(files)
}

pub(super) fn decode_entry_args(input: &JsonValue) -> Result<Vec<Value>, String> {
    let val = json_to_value(input)?;
    if matches!(&val, Value::Unit) {
        Ok(vec![])
    } else if let Some(args) = list_to_vec(&val) {
        Ok(args)
    } else {
        Ok(vec![val])
    }
}

fn decode_self_host_guest_args(input: &JsonValue) -> Result<Vec<String>, String> {
    decode_entry_args(input)?
        .into_iter()
        .enumerate()
        .map(|(idx, value)| match value {
            Value::Str(s) => Ok(s),
            other => Err(format!(
                "Self-host replay expects guest input as List<String>; item {} was {}",
                idx,
                aver_repr(&other)
            )),
        })
        .collect()
}

fn run_top_level_statements_runtime(
    interp: &mut Interpreter,
    items: &[TopLevel],
) -> Result<(), RuntimeError> {
    for item in items {
        if let TopLevel::Stmt(stmt) = item {
            interp.exec_stmt(stmt)?;
        }
    }
    Ok(())
}

fn run_entry_function_runtime(
    interp: &mut Interpreter,
    entry_fn: &str,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    let fn_val = interp
        .lookup(entry_fn)
        .map_err(|_| RuntimeError::Error(format!("Entry function '{}' not found", entry_fn)))?;
    let allowed = Interpreter::callable_declared_effects(&fn_val);
    interp.call_value_with_effects_pub(fn_val, args, &format!("<{}>", entry_fn), allowed)
}

fn truncate_for_cli(s: String, max_chars: usize) -> String {
    if s.chars().count() <= max_chars {
        return s;
    }
    let mut out = s
        .chars()
        .take(max_chars.saturating_sub(3))
        .collect::<String>();
    out.push_str("...");
    out
}

fn compact_json(v: &JsonValue) -> String {
    let compact = format_json(v)
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");
    truncate_for_cli(compact, 240)
}

fn compact_outcome(outcome: &RecordedOutcome) -> String {
    match outcome {
        RecordedOutcome::Value(v) => format!("value {}", compact_json(v)),
        RecordedOutcome::RuntimeError(msg) => format!("runtime_error {:?}", msg),
    }
}

fn compact_args(args: &[JsonValue]) -> String {
    compact_json(&JsonValue::Array(args.to_vec()))
}

fn format_replay_runtime_error(
    err: &RuntimeError,
    recording: &SessionRecording,
    interp: &Interpreter,
) -> String {
    let (consumed, total) = interp.replay_progress();
    let mut lines = vec![
        format!("Replay failed: {}", err),
        format!(
            "Progress: consumed {} of {} recorded effects",
            consumed, total
        ),
    ];

    match err {
        RuntimeError::ReplayMismatch { seq, expected, got } => {
            lines.push(format!(
                "Effect mismatch at seq {}: expected '{}', got '{}'",
                seq, expected, got
            ));
            if let Some(rec) = recording.effects.iter().find(|r| r.seq == *seq) {
                lines.push(format!("Expected args: {}", compact_args(&rec.args)));
                lines.push(format!(
                    "Expected outcome: {}",
                    compact_outcome(&rec.outcome)
                ));
            }
        }
        RuntimeError::ReplayArgsMismatch {
            seq,
            effect_type,
            expected,
            got,
        } => {
            lines.push(format!("Args mismatch at seq {} ('{}')", seq, effect_type));
            lines.push(format!("Expected args: {}", expected));
            lines.push(format!("Got args:      {}", got));
            if let Some(rec) = recording.effects.iter().find(|r| r.seq == *seq) {
                lines.push(format!(
                    "Expected outcome: {}",
                    compact_outcome(&rec.outcome)
                ));
            }
        }
        RuntimeError::ReplayExhausted {
            effect_type,
            position,
        } => {
            lines.push(format!(
                "No recorded effect at position {} for call '{}'",
                position, effect_type
            ));
            if let Some(next) = recording.effects.get(*position) {
                lines.push(format!(
                    "Next recorded effect: seq {} '{}'",
                    next.seq, next.effect_type
                ));
            }
        }
        RuntimeError::ReplayUnconsumed { remaining } => {
            let start = recording.effects.len().saturating_sub(*remaining);
            if let Some(next) = recording.effects.get(start) {
                lines.push(format!(
                    "First unconsumed effect: seq {} '{}', args={}, outcome={}",
                    next.seq,
                    next.effect_type,
                    compact_args(&next.args),
                    compact_outcome(&next.outcome)
                ));
            }
        }
        _ => {}
    }

    lines.join("\n")
}

fn resolve_replay_module_root(path: &Path, recording: &SessionRecording) -> String {
    let module_root = Path::new(&recording.module_root);
    if module_root.is_absolute() {
        return recording.module_root.clone();
    }

    if module_root.join(&recording.program_file).exists() {
        return recording.module_root.clone();
    }

    if let Some(parent) = path.parent() {
        for ancestor in parent.ancestors() {
            let candidate = ancestor.join(module_root);
            if candidate.join(&recording.program_file).exists() {
                return candidate.to_string_lossy().into_owned();
            }
        }
    }

    recording.module_root.clone()
}

fn resolve_replay_program_file(recording: &SessionRecording, module_root: &str) -> String {
    let program_file = Path::new(&recording.program_file);
    if program_file.is_absolute() {
        return recording.program_file.clone();
    }

    if program_file.exists() {
        return recording.program_file.clone();
    }

    let rooted = Path::new(module_root).join(program_file);
    if rooted.exists() {
        return rooted.to_string_lossy().into_owned();
    }

    recording.program_file.clone()
}

pub(super) fn replay_recording_file(
    path: &Path,
    _diff: bool,
    check_args: bool,
) -> Result<ReplayResult, String> {
    let raw = fs::read_to_string(path)
        .map_err(|e| format!("Cannot read recording '{}': {}", path.display(), e))?;
    let recording: SessionRecording = parse_session_recording(&raw)
        .map_err(|e| format!("Invalid recording JSON '{}': {}", path.display(), e))?;

    let replay_module_root = resolve_replay_module_root(path, &recording);
    let replay_program_file = resolve_replay_program_file(&recording, &replay_module_root);
    let (mut interp, items, _) =
        compile_program_for_exec(&replay_program_file, Some(&replay_module_root))?;
    interp.start_replay(recording.effects.clone(), check_args);

    run_top_level_statements_runtime(&mut interp, &items).map_err(|e| {
        format!(
            "Replay: {}\n{}",
            path.display(),
            format_replay_runtime_error(&e, &recording, &interp)
        )
    })?;
    let entry_args = decode_entry_args(&recording.input)?;
    let run_out = run_entry_function_runtime(&mut interp, &recording.entry_fn, entry_args)
        .map_err(|e| {
            format!(
                "Replay: {}\n{}",
                path.display(),
                format_replay_runtime_error(&e, &recording, &interp)
            )
        })?;
    let actual_outcome = match run_out {
        Value::Err(err) => RecordedOutcome::RuntimeError(format!(
            "{} returned error: {}",
            recording.entry_fn,
            aver_repr(&err)
        )),
        v => RecordedOutcome::Value(value_to_json(&v)?),
    };
    interp.ensure_replay_consumed().map_err(|e| {
        format!(
            "Replay: {}\n{}",
            path.display(),
            format_replay_runtime_error(&e, &recording, &interp)
        )
    })?;

    let (consumed, total) = interp.replay_progress();
    let matched = actual_outcome == recording.output;

    let output_diff = if !matched {
        build_output_diff(&recording.output, &actual_outcome)
    } else {
        None
    };

    Ok(ReplayResult {
        path: path.display().to_string(),
        matched,
        effects_consumed: consumed,
        effects_total: total,
        error: None,
        output_diff,
    })
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

fn replay_recording_file_vm(
    path: &Path,
    _diff: bool,
    check_args: bool,
) -> Result<ReplayResult, String> {
    let raw = fs::read_to_string(path)
        .map_err(|e| format!("Cannot read recording '{}': {}", path.display(), e))?;
    let recording: SessionRecording = parse_session_recording(&raw)
        .map_err(|e| format!("Invalid recording JSON '{}': {}", path.display(), e))?;

    let replay_module_root = resolve_replay_module_root(path, &recording);
    let replay_program_file = resolve_replay_program_file(&recording, &replay_module_root);
    let source = read_file(&replay_program_file)?;
    let mut items = parse_file(&source)?;
    tco::transform_program(&mut items);

    let tc_result = run_type_check_full(&items, Some(&replay_module_root));
    if !tc_result.errors.is_empty() {
        return Err(crate::shared::format_type_errors(&tc_result.errors));
    }

    resolver::resolve_program(&mut items);

    let mut arena = aver::nan_value::Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) =
        vm::compile_program_with_modules(&items, &mut arena, Some(&replay_module_root))
            .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);
    apply_runtime_policy_to_vm(&mut machine, &replay_module_root)?;
    machine.start_replay(recording.effects.clone(), check_args);

    machine.run_top_level().map_err(|e| {
        let (consumed, total) = machine.replay_progress();
        format!(
            "Replay: {}\nReplay failed: {}\nProgress: consumed {} of {} recorded effects",
            path.display(),
            e,
            consumed,
            total
        )
    })?;

    let entry_args = decode_entry_args(&recording.input)?;
    let nv_args: Vec<NanValue> = entry_args
        .iter()
        .map(|v| NanValue::from_value(v, &mut machine.arena))
        .collect();

    let run_out = machine
        .run_named_function(&recording.entry_fn, &nv_args)
        .map_err(|e| {
            let (consumed, total) = machine.replay_progress();
            format!(
                "Replay: {}\nReplay failed: {}\nProgress: consumed {} of {} recorded effects",
                path.display(),
                e,
                consumed,
                total
            )
        })?;

    let actual_outcome = if run_out.is_err() {
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

    machine.ensure_replay_consumed().map_err(|e| {
        let (consumed, total) = machine.replay_progress();
        format!(
            "Replay: {}\nReplay failed: {}\nProgress: consumed {} of {} recorded effects",
            path.display(),
            e,
            consumed,
            total
        )
    })?;

    let (consumed, total) = machine.replay_progress();
    let matched = actual_outcome == recording.output;

    let output_diff = if !matched {
        build_output_diff(&recording.output, &actual_outcome)
    } else {
        None
    };

    Ok(ReplayResult {
        path: path.display().to_string(),
        matched,
        effects_consumed: consumed,
        effects_total: total,
        error: None,
        output_diff,
    })
}

fn replay_recording_file_self_host(
    path: &Path,
    _diff: bool,
    check_args: bool,
) -> Result<ReplayResult, String> {
    let raw = fs::read_to_string(path)
        .map_err(|e| format!("Cannot read recording '{}': {}", path.display(), e))?;
    let recording: SessionRecording = parse_session_recording(&raw)
        .map_err(|e| format!("Invalid recording JSON '{}': {}", path.display(), e))?;

    let replay_module_root = resolve_replay_module_root(path, &recording);
    let replay_program_file = resolve_replay_program_file(&recording, &replay_module_root);
    let binary_path = build_self_host_binary(false)?;
    let guest_args = decode_self_host_guest_args(&recording.input)?;

    let mut command = Command::new(&binary_path);
    command
        .arg(&replay_program_file)
        .arg(&replay_module_root)
        .args(&guest_args)
        .env("AVER_REPLAY_ENTRY_FN", "main")
        .env("AVER_REPLAY_REPLAY", path)
        .env("AVER_REPLAY_MODULE_ROOT", &replay_module_root)
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

    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        let mut msg = format!("Replay: {}\nSelf-host replay failed", path.display());
        if !stdout.is_empty() {
            msg.push_str(&format!("\nstdout:\n{}", stdout));
        }
        if !stderr.is_empty() {
            msg.push_str(&format!("\nstderr:\n{}", stderr));
        }
        return Err(msg);
    }

    let n = recording.effects.len();
    Ok(ReplayResult {
        path: path.display().to_string(),
        matched: true,
        effects_consumed: n,
        effects_total: n,
        error: None,
        output_diff: None,
    })
}

pub(super) struct ReplayResult {
    path: String,
    matched: bool,
    effects_consumed: usize,
    effects_total: usize,
    error: Option<String>,
    /// For output mismatch: expected, actual, diff_path
    output_diff: Option<(String, String, Option<String>)>,
}

fn render_replay_result(result: &ReplayResult, diff: bool, json: bool) {
    if json {
        let status = if result.error.is_some() {
            "error"
        } else if result.matched {
            "pass"
        } else {
            "mismatch"
        };
        let mut parts = vec![
            "\"schema_version\":1".to_string(),
            "\"kind\":\"replay-result\"".to_string(),
            format!("\"file\":{}", crate::diagnostic::json_escape(&result.path)),
            format!("\"status\":\"{}\"", status),
            format!("\"effects_consumed\":{}", result.effects_consumed),
            format!("\"effects_total\":{}", result.effects_total),
        ];
        if let Some(ref err) = result.error {
            parts.push(format!("\"error\":{}", crate::diagnostic::json_escape(err)));
        }
        if let Some((expected, actual, diff_path)) = &result.output_diff {
            parts.push(format!(
                "\"expected\":{}",
                crate::diagnostic::json_escape(expected)
            ));
            parts.push(format!(
                "\"actual\":{}",
                crate::diagnostic::json_escape(actual)
            ));
            if let Some(dp) = diff_path {
                parts.push(format!(
                    "\"diff_path\":{}",
                    crate::diagnostic::json_escape(dp)
                ));
            }
        }
        println!("{{{}}}", parts.join(","));
    } else {
        println!();
        println!("Replay: {}", result.path);
        if let Some(ref err) = result.error {
            for line in err.lines() {
                eprintln!("  {}", line.red());
            }
            return;
        }
        println!(
            "Effects: {} replayed ({} matched)",
            result.effects_consumed, result.effects_total
        );
        println!(
            "Output:  {}",
            if result.matched {
                "MATCH".green().to_string()
            } else {
                "DIFFERS".red().to_string()
            }
        );
        if diff
            && !result.matched
            && let Some((expected, actual, diff_path)) = &result.output_diff
        {
            println!();
            println!("Expected: {}", expected);
            println!("Got:      {}", actual);
            if let Some(dp) = diff_path {
                println!("Diff at:  {}", dp);
            }
        }
    }
}

pub(super) fn cmd_replay(
    recording: &str,
    diff: bool,
    test_mode: bool,
    check_args: bool,
    vm_mode: bool,
    self_host_mode: bool,
    json: bool,
) {
    let files = match collect_recording_files(recording) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let mut all_match = true;
    let mut total_replayed = 0usize;
    let mut total_matched = 0usize;
    let mut total_failed = 0usize;

    for file in &files {
        let result = if self_host_mode {
            replay_recording_file_self_host(file, diff, check_args)
        } else if vm_mode {
            replay_recording_file_vm(file, diff, check_args)
        } else {
            replay_recording_file(file, diff, check_args)
        };
        match result {
            Ok(rr) => {
                render_replay_result(&rr, diff, json);
                total_replayed += 1;
                if rr.matched {
                    total_matched += 1;
                } else {
                    total_failed += 1;
                    all_match = false;
                }
            }
            Err(e) => {
                let rr = ReplayResult {
                    path: file.display().to_string(),
                    matched: false,
                    effects_consumed: 0,
                    effects_total: 0,
                    error: Some(e),
                    output_diff: None,
                };
                render_replay_result(&rr, diff, json);
                total_replayed += 1;
                total_failed += 1;
                all_match = false;
            }
        }
    }

    // Summary
    if json {
        println!(
            "{{\"schema_version\":1,\"kind\":\"summary\",\"recordings\":{},\"matched\":{},\"failed\":{}}}",
            total_replayed, total_matched, total_failed
        );
    } else if files.len() > 1 {
        println!();
        let summary = format!(
            "Summary: {} recording{} | {} matched | {} failed",
            total_replayed,
            if total_replayed == 1 { "" } else { "s" },
            total_matched,
            total_failed,
        );
        if all_match {
            println!("{}", summary.green());
        } else {
            println!("{}", summary.red());
        }
    }

    if test_mode && !all_match {
        process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::{collect_recording_files, decode_self_host_guest_args};
    use aver::replay::JsonValue;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir() -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time before unix epoch")
            .as_nanos();
        std::env::temp_dir().join(format!("aver_replay_collect_{nanos}"))
    }

    #[test]
    fn collect_recording_files_recurses_into_subdirectories() {
        let root = unique_temp_dir();
        let nested = root.join("services/console");
        fs::create_dir_all(&nested).expect("create nested recording dirs");
        let top = root.join("top.json");
        let nested_json = nested.join("nested.json");
        let ignored = nested.join("notes.txt");
        fs::write(&top, "{}").expect("write top recording");
        fs::write(&nested_json, "{}").expect("write nested recording");
        fs::write(&ignored, "ignore").expect("write ignored file");

        let mut files = collect_recording_files(root.to_str().expect("temp path should be utf-8"))
            .expect("collect recordings");
        files.sort();

        assert_eq!(files, vec![nested_json, top]);

        fs::remove_dir_all(&root).expect("remove temp recording tree");
    }

    #[test]
    fn decode_self_host_guest_args_accepts_string_arrays() {
        let args = decode_self_host_guest_args(&JsonValue::Array(vec![
            JsonValue::String("a".to_string()),
            JsonValue::String("b".to_string()),
        ]))
        .expect("decode guest args");
        assert_eq!(args, vec!["a".to_string(), "b".to_string()]);
    }
}
