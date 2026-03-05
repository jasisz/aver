use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use colored::Colorize;

use aver::ast::TopLevel;
use aver::interpreter::{Interpreter, Value, aver_repr};
use aver::replay::{
    JsonValue, RecordedOutcome, SessionRecording, first_diff_path, format_json, json_to_value,
    parse_session_recording, value_to_json,
};
use aver::value::RuntimeError;

use crate::shared::compile_program_for_exec;

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
    let entries = fs::read_dir(p)
        .map_err(|e| format!("Cannot read recording directory '{}': {}", path, e))?;
    for entry in entries {
        let entry = entry.map_err(|e| e.to_string())?;
        let entry_path = entry.path();
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
    files.sort();
    if files.is_empty() {
        return Err(format!("No .json recordings found in '{}'", path));
    }
    Ok(files)
}

pub(super) fn decode_entry_args(input: &JsonValue) -> Result<Vec<Value>, String> {
    let val = json_to_value(input)?;
    match val {
        Value::Unit => Ok(vec![]),
        Value::List(args) => Ok(args),
        other => Ok(vec![other]),
    }
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
    diff: bool,
    check_args: bool,
) -> Result<bool, String> {
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

    println!();
    println!("Replay: {}", path.display());
    println!("Effects: {} replayed ({} matched)", consumed, total);
    println!(
        "Output:  {}",
        if matched {
            "MATCH".green().to_string()
        } else {
            "DIFFERS".red().to_string()
        }
    );

    if diff && !matched {
        match (&recording.output, &actual_outcome) {
            (RecordedOutcome::Value(expected), RecordedOutcome::Value(got)) => {
                println!();
                println!("Expected: {}", format_json(expected));
                println!("Got:      {}", format_json(got));
                if let Some(path) = first_diff_path(expected, got) {
                    println!("Diff at:  {}", path);
                }
            }
            (RecordedOutcome::RuntimeError(expected), RecordedOutcome::RuntimeError(got)) => {
                println!("Expected runtime error: {}", expected);
                println!("Got runtime error:      {}", got);
            }
            (expected, got) => {
                println!("Expected outcome: {:?}", expected);
                println!("Got outcome:      {:?}", got);
            }
        }
    }

    Ok(matched)
}

pub(super) fn cmd_replay(recording: &str, diff: bool, test_mode: bool, check_args: bool) {
    let files = match collect_recording_files(recording) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let mut all_match = true;
    for file in files {
        match replay_recording_file(&file, diff, check_args) {
            Ok(matched) => {
                if !matched {
                    all_match = false;
                }
            }
            Err(e) => {
                eprintln!("{}", e.red());
                all_match = false;
            }
        }
    }

    if test_mode && !all_match {
        process::exit(1);
    }
}
