use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use colored::Colorize;

use aver::ast::TopLevel;
use aver::replay::{JsonValue, SessionRecording, json_to_value, parse_session_recording};
use aver::value::{Value, list_to_vec};

use crate::cli_entry::shared::{parse_file, read_file};
use aver::tty_render::render_tty;

#[path = "replay_cmd/backends.rs"]
mod backends;

use backends::{build_replay_result, run_self_host_replay, run_vm_replay};
#[cfg(feature = "wasm")]
use backends::{find_fn_line_in_file, run_wasm_gc_replay};

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

pub(super) fn resolve_replay_program_file(
    recording: &SessionRecording,
    module_root: &str,
) -> String {
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

/// Find the line number of a key in raw JSON text (1-based, 0 = not found).
pub(super) fn find_json_line(raw: &str, key: &str) -> usize {
    let pattern = format!("\"{}\"", key);
    for (idx, line) in raw.lines().enumerate() {
        if line.trim_start().starts_with(&pattern) {
            return idx + 1;
        }
    }
    0
}

pub(super) fn find_fn_line(items: &[TopLevel], name: &str) -> usize {
    for item in items {
        if let TopLevel::FnDef(fd) = item
            && fd.name == name
        {
            return fd.line;
        }
    }
    1
}

// Per-backend replay drivers + the unified `BackendReplayOutcome`
// shape they return live in `replay_cmd/backends.rs`. Everything
// below this line concerns CLI orchestration: file discovery,
// rendering, the `cmd_replay` entry point.

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
    let entry_line = find_fn_line(&items, &recording.entry_fn);

    let outcome = run_vm_replay(&recording, &replay_module_root, &mut items, check_args);
    Ok(build_replay_result(
        path,
        &raw,
        &recording,
        replay_program_file,
        entry_line,
        outcome,
    ))
}

/// Replay a recording through the wasm-gc backend. Phase 4a — wires
/// the trace into the wasm-gc host's `EffectReplayState::Replay` mode
/// so each `aver/*` host import pulls its outcome from the trace
/// instead of running the real effect. All 29 effects route through
/// `try_replay` → per-type `decode_*` (phase 4b), so a recorded trace
/// replays without touching disk / network / clock / RNG. The decoded
/// `main` return value is compared against `recording.output` —
/// scalar / Unit returns get exact equality, ref returns hit an
/// opaque `<wasm-gc:ref>` marker on both sides (see
/// `decode_main_return` for the coverage matrix).
fn replay_recording_file_wasm_gc(
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

    #[cfg(feature = "wasm")]
    {
        let entry_line = find_fn_line_in_file(&replay_program_file, &recording.entry_fn);
        let outcome = run_wasm_gc_replay(
            &recording,
            &replay_module_root,
            &replay_program_file,
            check_args,
        );
        Ok(build_replay_result(
            path,
            &raw,
            &recording,
            replay_program_file,
            entry_line,
            outcome,
        ))
    }
    #[cfg(not(feature = "wasm"))]
    {
        let _ = (recording, check_args, replay_program_file);
        Err("--wasm-gc replay requires building aver with --features wasm".to_string())
    }
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

    let outcome = run_self_host_replay(&recording, &replay_module_root, path, check_args);
    Ok(build_replay_result(
        path,
        &raw,
        &recording,
        replay_program_file,
        // Self-host re-parsing the program just for the entry_fn line
        // would double the per-file cost; keep the prior 0-line value
        // until we wire a cheaper lookup.
        0,
        outcome,
    ))
}

pub(super) enum ReplayError {
    Generic(String),
}

pub(super) struct ReplayResult {
    recording_path: String,
    program_file: String,
    #[allow(dead_code)]
    entry_fn: String,
    entry_line: usize,
    matched: bool,
    effects_consumed: usize,
    effects_total: usize,
    error: Option<ReplayError>,
    /// For output mismatch: expected, actual, diff_path
    output_diff: Option<(String, String, Option<String>)>,
    /// Number of effects with differing args (even without --check-args).
    args_diffs: usize,
    /// Line of "output" key in recording JSON (for diagnostics).
    recording_output_line: usize,
}

fn build_replay_error_diagnostic(
    err: &ReplayError,
    program_file: &str,
    recording_path: &str,
    entry_fn: &str,
    entry_line: usize,
) -> super::diagnostic::Diagnostic {
    use super::diagnostic::replay_effect_error_diagnostic;

    let ReplayError::Generic(msg) = err;
    replay_effect_error_diagnostic(program_file, recording_path, msg, entry_fn, entry_line)
}

fn render_replay_result(result: &ReplayResult, _diff: bool, json: bool) {
    use super::diagnostic::replay_output_mismatch_diagnostic;

    if json {
        let status = if result.error.is_some() {
            "error"
        } else if result.matched {
            "pass"
        } else {
            "mismatch"
        };
        let parts = [
            "\"schema_version\":1".to_string(),
            "\"kind\":\"replay-result\"".to_string(),
            format!(
                "\"recording\":{}",
                crate::cli_entry::diagnostic::json_escape(&result.recording_path)
            ),
            format!(
                "\"file\":{}",
                crate::cli_entry::diagnostic::json_escape(&result.program_file)
            ),
            format!("\"status\":\"{}\"", status),
            format!("\"effects_consumed\":{}", result.effects_consumed),
            format!("\"effects_total\":{}", result.effects_total),
            format!("\"args_diffs\":{}", result.args_diffs),
        ];
        println!("{{{}}}", parts.join(","));

        let at_file = if result.program_file.is_empty() {
            &result.recording_path
        } else {
            &result.program_file
        };
        let diag = if let Some(ref err) = result.error {
            Some(build_replay_error_diagnostic(
                err,
                at_file,
                &result.recording_path,
                &result.entry_fn,
                result.entry_line,
            ))
        } else if let Some((expected, actual, diff_path)) = &result.output_diff {
            Some(replay_output_mismatch_diagnostic(
                at_file,
                &result.recording_path,
                expected,
                actual,
                diff_path.as_deref(),
                &result.entry_fn,
                result.entry_line,
                result.recording_output_line,
            ))
        } else {
            None
        };
        if let Some(d) = diag {
            println!("{}", serde_json::to_string(&d).unwrap_or_default());
        }
    } else {
        println!();
        println!("Replay: {}", result.recording_path);

        let at_file = if result.program_file.is_empty() {
            &result.recording_path
        } else {
            &result.program_file
        };

        if let Some(ref err) = result.error {
            let diag = build_replay_error_diagnostic(
                err,
                at_file,
                &result.recording_path,
                &result.entry_fn,
                result.entry_line,
            );
            print!("{}", render_tty(&diag, true));
            return;
        }

        println!(
            "Effects: {} replayed ({} matched)",
            result.effects_consumed, result.effects_total
        );

        if result.matched {
            println!("Output:  {}", "MATCH".green());
            if result.args_diffs > 0 {
                println!(
                    "  {}",
                    format!(
                        "{} effect(s) had different args (use --check-args to enforce)",
                        result.args_diffs
                    )
                    .yellow()
                );
            }
        } else {
            println!("Output:  {}", "DIFFERS".red());
            if let Some((expected, actual, diff_path)) = &result.output_diff {
                let diag = replay_output_mismatch_diagnostic(
                    at_file,
                    &result.recording_path,
                    expected,
                    actual,
                    diff_path.as_deref(),
                    &result.entry_fn,
                    result.entry_line,
                    result.recording_output_line,
                );
                println!();
                print!("{}", render_tty(&diag, true));
            }
        }
    }
}

pub(super) fn cmd_replay(
    recording: &str,
    diff: bool,
    test_mode: bool,
    check_args: bool,
    self_host_mode: bool,
    wasm_gc_mode: bool,
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
        } else if wasm_gc_mode {
            replay_recording_file_wasm_gc(file, diff, check_args)
        } else {
            replay_recording_file_vm(file, diff, check_args)
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
                    recording_path: file.display().to_string(),
                    program_file: String::new(),
                    entry_fn: String::new(),
                    entry_line: 0,
                    matched: false,
                    effects_consumed: 0,
                    effects_total: 0,
                    error: Some(ReplayError::Generic(e)),
                    output_diff: None,
                    args_diffs: 0,
                    recording_output_line: 0,
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
    use super::backends::decode_self_host_guest_args;
    use super::collect_recording_files;
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
