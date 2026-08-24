#![cfg(feature = "wasm")]

//! End-to-end smoke tests for `aver run --wasm-gc --record` and
//! `aver replay --wasm-gc`. Drives the real binary so the test
//! covers the actual host-side effect dispatch + recording state
//! machine + JSON serialisation, not just the in-process compile
//! path that `wasm_gc_spec.rs` exercises.
//!
//! Coverage:
//!
//! - Unit-returning `main` (Disk-style effect sequence) records and
//!   replays cleanly: every host import is short-circuited from the
//!   trace, no real disk write happens during replay.
//! - Int-returning `main` with `Time.unixMs` records the actual
//!   integer return; replay → `Output: MATCH`.
//! - `Process.stopRequested` reaches the native provider and preserves its
//!   Bool observation through record/replay.
//! - A replayed `Err` at a literal-discharge boundary reaches the native
//!   provider-contract diagnostic before the guest traps.
//! - Tampering the recorded `output` value flips replay to
//!   `Output: DIFFERS` with a non-zero exit shape that the JSON
//!   diagnostic surfaces (`expected: …, actual: …`).
//! - Trailing effect (recording strictly longer than what the
//!   program produces) → `Unconsumed` failure, not silent MATCH.
//! - Mismatched effect args without `--check-args` → soft warning
//!   (`args_diffs > 0`); with `--check-args` → hard failure.
//! - Per-function `--expr` recordings replay cleanly through the
//!   wasm-gc backend, mirroring the VM `--expr` flow.
//! - VM-recorded trace replays cleanly under `--wasm-gc` (cross-
//!   backend JSON shape parity).
//! - Batch replay (`aver replay --wasm-gc <dir>`) keeps iterating
//!   past a bad recording instead of `process::exit`-ing.

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = std::env::temp_dir().join(format!("{prefix}-{nanos}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn write_program(dir: &std::path::Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, source).expect("write temp program");
    path
}

fn format_output(output: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

fn record_via_cli(prog: &PathBuf, rec_dir: &PathBuf) -> PathBuf {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let out = Command::new(aver_bin)
        .arg("run")
        .arg("--wasm-gc")
        .arg("--record")
        .arg(rec_dir)
        .arg(prog)
        .output()
        .expect("spawn aver run --wasm-gc --record");
    assert!(
        out.status.success(),
        "record run failed:\n{}",
        format_output(&out)
    );
    let entries: Vec<PathBuf> = fs::read_dir(rec_dir)
        .expect("read rec dir")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("json"))
        .collect();
    assert_eq!(
        entries.len(),
        1,
        "expected exactly one recording file in {}, found: {:?}",
        rec_dir.display(),
        entries
    );
    entries.into_iter().next().unwrap()
}

fn replay_via_cli(rec: &PathBuf) -> std::process::Output {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    Command::new(aver_bin)
        .arg("replay")
        .arg("--wasm-gc")
        .arg(rec)
        .output()
        .expect("spawn aver replay --wasm-gc")
}

#[test]
fn wasm_gc_record_replay_unit_main_round_trips() {
    let work = temp_dir("aver-wasm-gc-rec-unit");
    let prog = write_program(
        &work,
        "main.av",
        "module Smoke\n    intent = \"unit-main record/replay smoke\"\n\n\
         fn main() -> Unit\n    ! [Console.print, Time.unixMs]\n    \
         t = Time.unixMs()\n    Console.print(\"t={t}\")\n",
    );
    let rec_dir = temp_dir("aver-wasm-gc-rec-unit-traces");

    let rec_path = record_via_cli(&prog, &rec_dir);
    let raw = fs::read_to_string(&rec_path).expect("read recording");
    assert!(
        raw.contains("\"Time.unixMs\""),
        "recording missing Time.unixMs entry:\n{raw}"
    );
    assert!(
        raw.contains("\"Console.print\""),
        "recording missing Console.print entry:\n{raw}"
    );

    let replay = replay_via_cli(&rec_path);
    assert!(
        replay.status.success(),
        "clean replay failed:\n{}",
        format_output(&replay)
    );
    let stdout = String::from_utf8_lossy(&replay.stdout);
    assert!(
        stdout.contains("Output:  MATCH"),
        "expected `Output: MATCH` for clean replay, got:\n{stdout}"
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}

#[test]
fn wasm_gc_process_stop_observation_records_and_replays() {
    let work = temp_dir("aver-wasm-gc-rec-process");
    let prog = write_program(
        &work,
        "main.av",
        "module ProcessReplay\n    intent = \"Process host record/replay smoke\"\n\n\
         fn main() -> Bool\n    ! [Process.stopRequested]\n    \
         Process.stopRequested()\n",
    );
    let rec_dir = temp_dir("aver-wasm-gc-rec-process-traces");

    let rec_path = record_via_cli(&prog, &rec_dir);
    let raw = fs::read_to_string(&rec_path).expect("read Process recording");
    assert!(
        raw.contains("\"Process.stopRequested\"") && raw.contains("\"value\": false"),
        "recording must contain the initial false Process observation:\n{raw}"
    );

    let replay = replay_via_cli(&rec_path);
    assert!(
        replay.status.success(),
        "Process replay failed:\n{}",
        format_output(&replay)
    );
    assert!(
        String::from_utf8_lossy(&replay.stdout).contains("Output:  MATCH"),
        "Process replay output must match:\n{}",
        String::from_utf8_lossy(&replay.stdout)
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}

#[test]
fn wasm_gc_replay_reports_discharged_provider_error_before_trapping() {
    let work = temp_dir("aver-wasm-gc-rec-contract-violation");
    let prog = write_program(
        &work,
        "main.av",
        "module ContractViolationReplay\n    intent = \"diagnostic boundary smoke\"\n\n\
         fn main() -> Unit\n    ! [Time.sleep]\n    Time.sleep(1)\n",
    );
    let rec_dir = temp_dir("aver-wasm-gc-rec-contract-violation-traces");
    let rec_path = record_via_cli(&prog, &rec_dir);
    let raw = fs::read_to_string(&rec_path).expect("read Time.sleep recording");
    let mut recording =
        aver::replay::parse_session_recording(&raw).expect("parse Time.sleep recording");
    let sleep = recording
        .effects
        .iter_mut()
        .find(|effect| effect.effect_type == "Time.sleep")
        .expect("Time.sleep effect record");
    let mut marker = std::collections::BTreeMap::new();
    marker.insert(
        "$err".to_string(),
        aver::replay::JsonValue::String("hostile replay provider".to_string()),
    );
    sleep.outcome = aver::replay::RecordedOutcome::Value(aver::replay::JsonValue::Object(marker));
    let hostile_path = work.join("hostile.json");
    fs::write(
        &hostile_path,
        aver::replay::session_recording_to_string_pretty(&recording),
    )
    .expect("write hostile Time.sleep recording");

    let replay = replay_via_cli(&hostile_path);
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&replay.stdout),
        String::from_utf8_lossy(&replay.stderr)
    );
    assert!(
        combined.contains(
            "provider contract violated: discharged Result returned Err(hostile replay provider)"
        ),
        "native wasm-gc host lost the provider diagnostic:\n{combined}"
    );
    assert!(
        combined.contains("fail[replay-error]")
            && combined.contains("wasm `unreachable` instruction executed"),
        "discharged provider Err must trap instead of producing a fallback:\n{combined}"
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}

#[test]
fn wasm_gc_record_persists_int_main_return() {
    let work = temp_dir("aver-wasm-gc-rec-int");
    let prog = write_program(
        &work,
        "main.av",
        "module SmokeInt\n    intent = \"int-main record/replay smoke\"\n\n\
         fn main() -> Int\n    ! [Time.unixMs]\n    \
         _ = Time.unixMs()\n    42\n",
    );
    let rec_dir = temp_dir("aver-wasm-gc-rec-int-traces");

    let rec_path = record_via_cli(&prog, &rec_dir);
    let raw = fs::read_to_string(&rec_path).expect("read recording");
    // The actual return value (42) round-trips through the typed
    // decoder into `output: { kind: value, value: 42 }`.
    assert!(
        raw.contains("\"value\": 42"),
        "recording.output should hold the Int return value:\n{raw}"
    );

    let replay = replay_via_cli(&rec_path);
    let stdout = String::from_utf8_lossy(&replay.stdout);
    assert!(
        stdout.contains("Output:  MATCH"),
        "clean Int replay should match, got:\n{stdout}"
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}

#[test]
fn wasm_gc_replay_detects_tampered_output() {
    let work = temp_dir("aver-wasm-gc-rec-tamper");
    let prog = write_program(
        &work,
        "main.av",
        "module SmokeTamper\n    intent = \"output mismatch smoke\"\n\n\
         fn main() -> Int\n    ! [Time.unixMs]\n    \
         _ = Time.unixMs()\n    42\n",
    );
    let rec_dir = temp_dir("aver-wasm-gc-rec-tamper-traces");
    let rec_path = record_via_cli(&prog, &rec_dir);

    // Swap the recorded `42` for `999`. Replay decodes the live
    // wasm-gc return as 42 and the comparison against
    // `recording.output` should fail with a structured diff.
    let raw = fs::read_to_string(&rec_path).expect("read recording");
    assert!(
        raw.contains("\"value\": 42"),
        "expected baseline recording to hold value: 42, got:\n{raw}"
    );
    let tampered = raw.replace("\"value\": 42", "\"value\": 999");
    let tampered_path = work.join("tampered.json");
    fs::write(&tampered_path, &tampered).expect("write tampered recording");

    let replay = replay_via_cli(&tampered_path);
    let stdout = String::from_utf8_lossy(&replay.stdout);
    assert!(
        stdout.contains("Output:  DIFFERS"),
        "tampered output should DIFFER, got:\n{stdout}"
    );
    // The structured diagnostic prints to stdout (with effects /
    // output summary); merge both streams to keep the test resilient
    // if the printer ever splits them.
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&replay.stdout),
        String::from_utf8_lossy(&replay.stderr)
    );
    assert!(
        combined.contains("expected: 999") && combined.contains("actual: 42"),
        "diagnostic should surface expected/actual swap, got:\n{combined}"
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}

#[test]
fn wasm_gc_records_and_replays_per_function_entry() {
    // `aver run --wasm-gc -e 'add(7, 35)' --record` should produce a
    // recording whose entry_fn / input / output reproduce the VM's
    // per-function recording shape, and the same recording must
    // replay cleanly (effects matched, output MATCH).
    let work = temp_dir("aver-wasm-gc-rec-perfn");
    let prog = write_program(
        &work,
        "main.av",
        "module PerFnSmoke\n    intent = \"per-fn entry smoke\"\n\n\
         fn add(a: Int, b: Int) -> Int\n    ! [Console.print]\n    \
         Console.print(\"{a}+{b}\")\n    a + b\n\n\
         fn main() -> Unit\n    ! [Console.print, add]\n    \
         _ = add(2, 3)\n",
    );
    let rec_dir = temp_dir("aver-wasm-gc-rec-perfn-traces");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let record = Command::new(aver_bin)
        .arg("run")
        .arg("--wasm-gc")
        .arg(&prog)
        .arg("-e")
        .arg("add(7, 35)")
        .arg("--record")
        .arg(&rec_dir)
        .output()
        .expect("spawn --wasm-gc -e add(7, 35) --record");
    assert!(
        record.status.success(),
        "per-fn record failed:\n{}",
        format_output(&record)
    );

    let entries: Vec<PathBuf> = fs::read_dir(&rec_dir)
        .expect("read rec dir")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("json"))
        .collect();
    assert_eq!(entries.len(), 1, "expected one per-fn recording");
    let rec_path = entries.into_iter().next().unwrap();
    let raw = fs::read_to_string(&rec_path).expect("read recording");
    assert!(
        raw.contains("\"entry_fn\": \"add\""),
        "recording entry_fn should be `add`:\n{raw}"
    );
    // The replay schema serialises a multi-arg input as a JSON array
    // of bare scalars (not wrapped in `{"value": …}` envelopes —
    // those are only on the `output` outcome).
    assert!(
        raw.contains("\"input\": [\n    7,\n    35\n  ]"),
        "recording.input should hold (7, 35) literal args:\n{raw}"
    );
    assert!(
        raw.contains("\"value\": 42"),
        "recording.output should hold the Int return value:\n{raw}"
    );

    let replay = replay_via_cli(&rec_path);
    let stdout = String::from_utf8_lossy(&replay.stdout);
    assert!(
        replay.status.success() && stdout.contains("Output:  MATCH"),
        "per-fn replay should match, got:\n{}",
        format_output(&replay)
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}

#[test]
fn wasm_gc_replay_rejects_recording_with_trailing_effect() {
    // A recording whose effect prefix matches the program but holds
    // one extra event past the program's last replay_effect call
    // must fail replay — the live run is a strict prefix of the
    // recording, so the comparison can't claim MATCH even if the
    // return value happens to line up. Mirrors the VM replayer's
    // `ensure_replay_consumed` contract.
    let work = temp_dir("aver-wasm-gc-rec-unconsumed");
    let prog = write_program(
        &work,
        "main.av",
        "module SmokeUnconsumed\n    intent = \"ensure_replay_consumed smoke\"\n\n\
         fn main() -> Unit\n    ! [Console.print, Time.unixMs]\n    \
         _ = Time.unixMs()\n    Console.print(\"only one print\")\n",
    );
    let rec_dir = temp_dir("aver-wasm-gc-rec-unconsumed-traces");
    let rec_path = record_via_cli(&prog, &rec_dir);

    // Parse the recording, push a synthetic effect onto the end,
    // serialise it back. Mechanical (no fragile substring slicing
    // against the pretty-printer's spacing) and survives any future
    // change in `session_recording_to_string_pretty` formatting.
    let raw = fs::read_to_string(&rec_path).expect("read recording");
    let mut recording =
        aver::replay::parse_session_recording(&raw).expect("parse recorded session");
    let next_seq = recording.effects.last().map(|e| e.seq + 1).unwrap_or(1);
    recording.effects.push(aver::replay::EffectRecord {
        seq: next_seq,
        effect_type: "Console.print".to_string(),
        args: vec![aver::replay::JsonValue::String("ghost".to_string())],
        outcome: aver::replay::RecordedOutcome::Value(aver::replay::JsonValue::Null),
        caller_fn: "main".to_string(),
        source_line: 0,
        group_id: None,
        branch_path: None,
        effect_occurrence: None,
    });
    let tampered_path = work.join("tampered.json");
    fs::write(
        &tampered_path,
        aver::replay::session_recording_to_string_pretty(&recording),
    )
    .expect("write tampered recording");

    let replay = replay_via_cli(&tampered_path);
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&replay.stdout),
        String::from_utf8_lossy(&replay.stderr)
    );
    assert!(
        combined.contains("Unconsumed") || combined.contains("replay incomplete"),
        "trailing-effect recording should fail with an `Unconsumed` diagnostic, got:\n{combined}"
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}

#[test]
fn wasm_gc_records_independent_product_with_group_annotations() {
    // `?!` programs lower with `enter_group` / `set_branch(i)` /
    // `exit_group` host calls so contained effects pick up
    // (group_id, branch_path, effect_occurrence). Without the
    // markers, traces are flat and cross-backend replay against a
    // VM-recorded trace fails. This test pins the wasm-gc recorder
    // to emit non-null group_id on every effect inside `?!`, and
    // verifies cross-backend replay (wasm-gc → VM and VM →
    // wasm-gc) round-trips cleanly.
    let work = temp_dir("aver-wasm-gc-rec-groups");
    let prog = write_program(
        &work,
        "main.av",
        "module CrossGroups\n    intent = \"?! cross-backend smoke\"\n\n\
         fn loadOk(n: Int) -> Result<Int, String>\n    ! [Console.print, Time.unixMs]\n    \
         _ = Time.unixMs()\n    Console.print(\"loaded {n}\")\n    Result.Ok(n * 10)\n\n\
         fn computeAll() -> Result<Tuple<Int, Int, Int>, String>\n    ! [Console.print, Time.unixMs]\n    \
         data = (loadOk(1), loadOk(2), loadOk(3))?!\n    Result.Ok(data)\n\n\
         fn main() -> Unit\n    ! [Console.print, Time.unixMs]\n    \
         match computeAll()\n        Result.Ok(_) -> Console.print(\"ok\")\n        \
         Result.Err(e) -> Console.print(\"err: {e}\")\n",
    );
    let wgc_dir = temp_dir("aver-wasm-gc-rec-groups-wgc");
    let vm_dir = temp_dir("aver-wasm-gc-rec-groups-vm");

    // Record under wasm-gc. Trace must hold `group_id` on every
    // contained effect — this is the new emit pinning.
    let wgc_path = record_via_cli(&prog, &wgc_dir);
    let wgc_raw = fs::read_to_string(&wgc_path).expect("read wasm-gc recording");
    assert!(
        wgc_raw.contains("\"group_id\": 1"),
        "wasm-gc trace must annotate `?!` contained effects with group_id; got:\n{wgc_raw}"
    );
    assert!(
        wgc_raw.contains("\"branch_path\""),
        "wasm-gc trace must annotate branch_path; got:\n{wgc_raw}"
    );

    // Record under VM for the cross-backend replay leg.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let vm_record = Command::new(aver_bin)
        .arg("run")
        .arg("--record")
        .arg(&vm_dir)
        .arg(&prog)
        .output()
        .expect("spawn vm record");
    assert!(
        vm_record.status.success(),
        "vm record failed:\n{}",
        format_output(&vm_record)
    );
    let vm_entries: Vec<PathBuf> = fs::read_dir(&vm_dir)
        .expect("read vm rec dir")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("json"))
        .collect();
    let vm_path = vm_entries.into_iter().next().expect("one vm recording");

    // Cross-backend replay: wasm-gc trace → VM replay, VM trace →
    // wasm-gc replay. Both must MATCH.
    let cross_a = Command::new(aver_bin)
        .arg("replay")
        .arg(&wgc_path)
        .output()
        .expect("spawn cross VM replay");
    let cross_a_stdout = String::from_utf8_lossy(&cross_a.stdout);
    assert!(
        cross_a.status.success() && cross_a_stdout.contains("Output:  MATCH"),
        "wasm-gc-recorded `?!` trace must replay cleanly via VM, got:\n{}",
        format_output(&cross_a)
    );

    let cross_b = replay_via_cli(&vm_path);
    let cross_b_stdout = String::from_utf8_lossy(&cross_b.stdout);
    assert!(
        cross_b.status.success() && cross_b_stdout.contains("Output:  MATCH"),
        "VM-recorded `?!` trace must replay cleanly via wasm-gc, got:\n{}",
        format_output(&cross_b)
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&wgc_dir);
    let _ = fs::remove_dir_all(&vm_dir);
}

#[test]
fn self_host_replay_compares_main_return_value() {
    // Self-host previously dropped the user `main()` return value
    // inside `runGuestCliProgram`'s Unit-mapping wrapper, so every
    // replay claimed `Output: MATCH` regardless of what the
    // subprocess returned. The Step 3 fix surfaces the live `Val`
    // up the call stack so the replay scope's `finish_scope_success`
    // serialises the actual value into `recording.output`. Tampering
    // a recorded `42` → `999` must now flip replay to a panic-driven
    // failure with a structured `expected ... got ...` diagnostic.
    let work = temp_dir("aver-self-host-output");
    let prog = write_program(
        &work,
        "main.av",
        "module SelfHostOutput\n    intent = \"self-host output value comparison\"\n\n\
         fn main() -> Int\n    ! [Console.print, Time.unixMs]\n    \
         _ = Time.unixMs()\n    Console.print(\"self-host out\")\n    42\n",
    );
    let rec_dir = temp_dir("aver-self-host-output-rec");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let record = Command::new(aver_bin)
        .arg("run")
        .arg("--self-host")
        .arg("--record")
        .arg(&rec_dir)
        .arg(&prog)
        .output()
        .expect("spawn self-host record");
    assert!(
        record.status.success(),
        "self-host record failed:\n{}",
        format_output(&record)
    );
    let rec_path: PathBuf = fs::read_dir(&rec_dir)
        .expect("read rec dir")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .find(|p| p.extension().and_then(|s| s.to_str()) == Some("json"))
        .expect("one self-host recording");
    let raw = fs::read_to_string(&rec_path).expect("read recording");
    // The user's `42` reaches `recording.output` as a `Val::ValInt(42)`
    // ADT — that's how self-host serialises its interpreter's value
    // type. Pre-Step-3 the field would have been `null`.
    assert!(
        raw.contains("\"name\": \"ValInt\"") && raw.contains("42"),
        "self-host recording must capture the live user main return value, got:\n{raw}"
    );

    // Clean replay: matches.
    let clean = Command::new(aver_bin)
        .arg("replay")
        .arg("--self-host")
        .arg(&rec_path)
        .output()
        .expect("spawn clean self-host replay");
    let clean_stdout = String::from_utf8_lossy(&clean.stdout);
    assert!(
        clean.status.success() && clean_stdout.contains("Output:  MATCH"),
        "clean self-host replay must MATCH, got:\n{}",
        format_output(&clean)
    );

    // Tamper the recorded ValInt payload `42` → `999`. Replay-mode
    // self-host runtime compares actual against recording.output,
    // panics on mismatch, subprocess exits non-zero; the host turns
    // it into a `ReplayError::Generic`. Use a structurally-anchored
    // replacement so we only touch the ValInt payload — `42` could
    // appear in tempdir paths or other parts of the JSON.
    let tampered = raw.replace(
        "\"fields\": [\n          42\n        ]",
        "\"fields\": [\n          999\n        ]",
    );
    let tampered_path = work.join("tampered.json");
    fs::write(&tampered_path, &tampered).expect("write tampered recording");
    // Run with `--test` so a tampered recording flips `aver replay`
    // to a non-zero exit (CI gate semantics) on top of surfacing the
    // self-host runtime's `Replay output mismatch` diagnostic.
    let strict = Command::new(aver_bin)
        .arg("replay")
        .arg("--self-host")
        .arg("--test")
        .arg(&tampered_path)
        .output()
        .expect("spawn tampered self-host replay");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&strict.stdout),
        String::from_utf8_lossy(&strict.stderr)
    );
    assert!(
        !strict.status.success() && combined.contains("Replay output mismatch"),
        "tampered self-host replay must surface a Replay-output-mismatch diagnostic and exit non-zero under --test, got:\n{}",
        format_output(&strict)
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}

#[test]
fn wasm_gc_replays_a_vm_recorded_trace() {
    // Record under VM, replay under wasm-gc. Both backends share the
    // same JSON shape (markers + outcome enum); the wasm-gc replayer
    // must accept a trace it didn't write itself.
    let work = temp_dir("aver-wasm-gc-rec-cross");
    let prog = write_program(
        &work,
        "main.av",
        "module SmokeCross\n    intent = \"cross-backend trace smoke\"\n\n\
         fn main() -> Unit\n    ! [Console.print, Time.unixMs]\n    \
         t = Time.unixMs()\n    Console.print(\"vm-recorded {t}\")\n",
    );
    let rec_dir = temp_dir("aver-wasm-gc-rec-cross-traces");

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let vm_record = Command::new(aver_bin)
        .arg("run")
        .arg("--record")
        .arg(&rec_dir)
        .arg(&prog)
        .output()
        .expect("spawn vm --record");
    assert!(
        vm_record.status.success(),
        "vm record run failed:\n{}",
        format_output(&vm_record)
    );

    let entries: Vec<PathBuf> = fs::read_dir(&rec_dir)
        .expect("read vm rec dir")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("json"))
        .collect();
    assert_eq!(
        entries.len(),
        1,
        "expected one VM recording in {}, found: {:?}",
        rec_dir.display(),
        entries
    );

    let replay = replay_via_cli(&entries[0]);
    let stdout = String::from_utf8_lossy(&replay.stdout);
    assert!(
        replay.status.success() && stdout.contains("Output:  MATCH"),
        "wasm-gc should accept a VM-recorded trace, got:\n{}",
        format_output(&replay)
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}

#[test]
fn wasm_gc_replay_args_mismatch_warns_without_check_args_and_fails_with() {
    // Mutating a recorded effect's `args` should:
    //  - replay clean (sequence matches, output decodes the same) but
    //    bump `args_diffs` to N>0 — the renderer surfaces this as a
    //    soft warning regardless of `--check-args`,
    //  - hard-fail under `--check-args` (each `replay_effect` call
    //    raises a `ReplayFailure::ArgMismatch`).
    let work = temp_dir("aver-wasm-gc-rec-argsdiff");
    let prog = write_program(
        &work,
        "main.av",
        "module SmokeArgsDiff\n    intent = \"args_diffs warning smoke\"\n\n\
         fn main() -> Unit\n    ! [Console.print, Time.unixMs]\n    \
         _ = Time.unixMs()\n    Console.print(\"hello\")\n",
    );
    let rec_dir = temp_dir("aver-wasm-gc-rec-argsdiff-traces");
    let rec_path = record_via_cli(&prog, &rec_dir);

    // Swap "hello" → "changed" inside the recorded effect args. Done
    // structurally via `parse_session_recording` so the test isn't
    // tied to the pretty-printer's exact whitespace.
    let raw = fs::read_to_string(&rec_path).expect("read recording");
    let mut recording =
        aver::replay::parse_session_recording(&raw).expect("parse recorded session");
    let print = recording
        .effects
        .iter_mut()
        .find(|e| e.effect_type == "Console.print")
        .expect("recording must have a Console.print effect");
    print.args = vec![aver::replay::JsonValue::String("changed".to_string())];
    let tampered_path = work.join("argsdiff.json");
    fs::write(
        &tampered_path,
        aver::replay::session_recording_to_string_pretty(&recording),
    )
    .expect("write tampered recording");

    // Without --check-args: replay should still complete cleanly with
    // a non-zero `Args diff` count surfaced in the renderer line.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let lenient = Command::new(aver_bin)
        .arg("replay")
        .arg("--wasm-gc")
        .arg(&tampered_path)
        .output()
        .expect("spawn lenient replay");
    let lenient_combined = format!(
        "{}{}",
        String::from_utf8_lossy(&lenient.stdout),
        String::from_utf8_lossy(&lenient.stderr)
    );
    assert!(
        lenient.status.success(),
        "args-mismatch replay without --check-args should still exit 0:\n{}",
        format_output(&lenient)
    );
    // The renderer prints `N effect(s) had different args (use
    // --check-args to enforce)` when `args_diffs` is non-zero.
    assert!(
        lenient_combined.contains("had different args"),
        "renderer should surface an args-diff warning when args_diffs > 0, got:\n{lenient_combined}"
    );

    // With --check-args: each replay_effect call with mismatched args
    // raises a hard failure — replay must fail.
    let strict = Command::new(aver_bin)
        .arg("replay")
        .arg("--wasm-gc")
        .arg("--check-args")
        .arg(&tampered_path)
        .output()
        .expect("spawn strict replay");
    let strict_combined = format!(
        "{}{}",
        String::from_utf8_lossy(&strict.stdout),
        String::from_utf8_lossy(&strict.stderr)
    );
    assert!(
        strict_combined.contains("fail")
            || strict_combined.contains("error")
            || strict_combined.contains("ArgMismatch")
            || !strict.status.success(),
        "args-mismatch replay with --check-args should report a failure, got:\n{strict_combined}"
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}

#[test]
fn wasm_gc_batch_replay_keeps_going_past_a_bad_recording() {
    // `aver replay --wasm-gc <dir>` iterates over every .json in the
    // directory. A single failure must not `process::exit` and skip
    // the rest — the bug review #3 from the unified-shape patch.
    //
    // Critical detail: the bad recording must be *structurally
    // valid* JSON that parses cleanly into a `SessionRecording` and
    // then fails inside `try_run_wasm_gc` (which is the path the
    // review actually touched). Using malformed JSON would only
    // exercise the outer parse-error early-return, not the wasm-gc
    // runner's no-exit contract. We achieve this by recording two
    // programs, then mutating the *second* recording to hold a
    // trailing effect the program never produces — replay parses
    // fine, executes the program through wasm-gc, then trips
    // `ensure_replay_consumed` on the way out.
    let work = temp_dir("aver-wasm-gc-rec-batch");
    let prog = write_program(
        &work,
        "main.av",
        "module SmokeBatch\n    intent = \"batch replay no-exit smoke\"\n\n\
         fn main() -> Unit\n    ! [Console.print, Time.unixMs]\n    \
         _ = Time.unixMs()\n    Console.print(\"batch ok\")\n",
    );
    let rec_dir = temp_dir("aver-wasm-gc-rec-batch-traces");

    // Two recordings: the second one will get a trailing-effect
    // tamper so it fails inside wasm-gc replay.
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    for _ in 0..2 {
        let out = Command::new(aver_bin)
            .arg("run")
            .arg("--wasm-gc")
            .arg("--record")
            .arg(&rec_dir)
            .arg(&prog)
            .output()
            .expect("spawn record");
        assert!(
            out.status.success(),
            "record failed:\n{}",
            format_output(&out)
        );
        // Sleep 2ms so the timestamp-based stem differs between runs.
        std::thread::sleep(std::time::Duration::from_millis(2));
    }
    let mut entries: Vec<PathBuf> = fs::read_dir(&rec_dir)
        .expect("read rec dir")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("json"))
        .collect();
    entries.sort();
    assert_eq!(entries.len(), 2, "expected two recordings, got {entries:?}");

    // Tamper the lexicographically-earlier recording so the runner
    // hits the failure first — the assertion below proves the
    // *second* file was still processed afterward.
    let bad_path = &entries[0];
    let bad_raw = fs::read_to_string(bad_path).expect("read bad recording");
    let mut bad = aver::replay::parse_session_recording(&bad_raw).expect("parse bad recording");
    let next_seq = bad.effects.last().map(|e| e.seq + 1).unwrap_or(1);
    bad.effects.push(aver::replay::EffectRecord {
        seq: next_seq,
        effect_type: "Console.print".to_string(),
        args: vec![aver::replay::JsonValue::String("ghost".to_string())],
        outcome: aver::replay::RecordedOutcome::Value(aver::replay::JsonValue::Null),
        caller_fn: "main".to_string(),
        source_line: 0,
        group_id: None,
        branch_path: None,
        effect_occurrence: None,
    });
    fs::write(
        bad_path,
        aver::replay::session_recording_to_string_pretty(&bad),
    )
    .expect("rewrite bad recording");

    let replay = Command::new(aver_bin)
        .arg("replay")
        .arg("--wasm-gc")
        .arg("--test")
        .arg(&rec_dir)
        .output()
        .expect("spawn batch replay");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&replay.stdout),
        String::from_utf8_lossy(&replay.stderr)
    );
    let bad_name = bad_path
        .file_name()
        .and_then(|s| s.to_str())
        .expect("bad rec filename utf-8");
    let good_name = entries[1]
        .file_name()
        .and_then(|s| s.to_str())
        .expect("good rec filename utf-8");
    // Both files must appear in the summary: the bad one as a
    // failure, the good one with a clean outcome. If wasm-gc still
    // `process::exit`-ed on the bad one we'd never see the good
    // one's section.
    assert!(
        combined.contains(bad_name),
        "batch summary should mention the bad recording {bad_name}, got:\n{combined}"
    );
    assert!(
        combined.contains(good_name),
        "batch summary should mention the good recording {good_name}, got:\n{combined}"
    );
    assert!(
        combined.contains("Unconsumed") || combined.contains("replay incomplete"),
        "bad recording should surface the wasm-gc unconsumed-trace failure, got:\n{combined}"
    );

    let _ = fs::remove_dir_all(&work);
    let _ = fs::remove_dir_all(&rec_dir);
}
