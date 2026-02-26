use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use colored::Colorize;

use aver::interpreter::{aver_repr, Value};
use aver::replay::{
    first_diff_path, format_json, json_to_value, parse_session_recording, value_to_json, JsonValue,
    RecordedOutcome, SessionRecording,
};

use crate::shared::{compile_program_for_exec, run_entry_function, run_top_level_statements};

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

pub(super) fn replay_recording_file(
    path: &Path,
    diff: bool,
    check_args: bool,
) -> Result<bool, String> {
    let raw = fs::read_to_string(path)
        .map_err(|e| format!("Cannot read recording '{}': {}", path.display(), e))?;
    let recording: SessionRecording = parse_session_recording(&raw)
        .map_err(|e| format!("Invalid recording JSON '{}': {}", path.display(), e))?;

    let (mut interp, items, _) =
        compile_program_for_exec(&recording.program_file, Some(&recording.module_root))?;
    interp.start_replay(recording.effects.clone(), check_args);

    run_top_level_statements(&mut interp, &items)?;
    let entry_args = decode_entry_args(&recording.input)?;
    let run_out = run_entry_function(&mut interp, &recording.entry_fn, entry_args);
    let actual_outcome = match run_out {
        Ok(Value::Err(err)) => RecordedOutcome::RuntimeError(format!(
            "{} returned error: {}",
            recording.entry_fn,
            aver_repr(&err)
        )),
        Ok(v) => RecordedOutcome::Value(value_to_json(&v)?),
        Err(e) => RecordedOutcome::RuntimeError(e),
    };
    interp.ensure_replay_consumed().map_err(|e| e.to_string())?;

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
