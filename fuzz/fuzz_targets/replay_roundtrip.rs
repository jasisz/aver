// Coverage-guided fuzz target: source → VM record → VM replay
// roundtrip.
//
// Different surface from `fuzz_replay_codec`. That target hammers
// the JSON serialisation / deserialisation of a `SessionRecording`.
// This one checks the *semantic* invariant the recorder + replayer
// are supposed to maintain: replay of a recording must reproduce
// the original run bit-for-bit.
//
// Pipeline per AFL exec:
//   1. lex / parse / typecheck / pipeline (zero-error gate)
//   2. compile + VM run with `start_recording()`, capture stdout +
//      decoded entry-fn return value + effect trace.
//   3. fresh compile + VM run with `start_replay(effects)`,
//      capture stdout + decoded return value.
//   4. assert stdout identical, output identical, replay consumed
//      the entire trace (`ensure_replay_consumed` Ok).
//
// Bug class this surfaces:
//   - recorder dropping events / under-counting (replay runs out of
//     trace mid-execution)
//   - replayer mis-decoding effect args (return-value mismatch even
//     when the original ran cleanly)
//   - non-determinism in the VM itself that the recorder can't
//     bridge (real bug; recording shape should fail loudly, not
//     silently produce wrong output on replay)
//
// In-process throughput pattern from `parity_vm_vs_wasm_gc`:
// `aver::services::console::capture_output` redirects stdout
// per-thread so we read what the program "printed" without
// touching the real fd.

#[path = "common.rs"]
mod common;

use aver::ir::hir::ResolvedTopLevel;
use aver::ir::{PipelineConfig, SymbolTable, TypecheckMode};
use aver::replay::JsonValue;
use std::panic::AssertUnwindSafe;

const MAX_INPUT_SIZE: usize = 4 * 1024;

/// Decoded outcome of one VM run. Mirror of the parity target's
/// `BackendOutcome` shape — same comparison currency (stdout
/// bytes + `JsonValue` output) so a future "record on backend A,
/// replay on backend B" target can reuse it.
struct RunOutcome {
    stdout: Vec<u8>,
    value: JsonValue,
    /// Recorded effect trace — populated only on the record run.
    /// The replay run takes this as input and the comparison
    /// asserts `ensure_replay_consumed`.
    recorded_effects: Option<Vec<aver::replay::EffectRecord>>,
}

fn main() {
    afl::fuzz_nohook!(|data: &[u8]| {
        if data.len() > MAX_INPUT_SIZE {
            return;
        }
        let c = common::counters();
        c.record_exec();
        let Ok(source) = std::str::from_utf8(data) else {
            return;
        };

        let mut lexer = aver::lexer::Lexer::new(source);
        let Ok(tokens) = lexer.tokenize() else { return };
        c.record_lex_ok();
        let mut parser = aver::parser::Parser::new(tokens);
        let Ok(mut items) = parser.parse() else { return };
        let (nodes, depth) = common::ast_metrics(&items);
        c.record_parse_ok(nodes, depth);
        let errors = aver::types::checker::run_type_check(&items);
        if !errors.is_empty() {
            return;
        }
        c.record_typecheck_clean();

        let pipeline_result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );

        // First run: record. Captures the canonical (stdout,
        // value, trace) triple the replay must reproduce.
        let Some(record) = run_vm(
            &pipeline_result.resolved_items,
            &pipeline_result.symbol_table,
            RecordMode::Record,
        ) else {
            return;
        };
        let trace = match record.recorded_effects.as_ref() {
            Some(t) => t.clone(),
            None => return,
        };

        // Second run: replay against the recorded trace. Same
        // program, fresh VM, recorder swapped for replayer.
        let Some(replay) = run_vm(
            &pipeline_result.resolved_items,
            &pipeline_result.symbol_table,
            RecordMode::Replay(trace),
        ) else {
            // Replay refused / panicked under catch_unwind. Real
            // bug class — record produced a trace the replay
            // can't consume.
            eprintln!(
                "replay_roundtrip: record succeeded but replay refused on the recorded trace"
            );
            std::process::abort();
        };

        if record.stdout != replay.stdout {
            let r = String::from_utf8_lossy(&record.stdout);
            let p = String::from_utf8_lossy(&replay.stdout);
            eprintln!(
                "replay_roundtrip: stdout divergence\n--- record ---\n{r}\n--- replay ---\n{p}"
            );
            std::process::abort();
        }
        if record.value != replay.value {
            eprintln!(
                "replay_roundtrip: output value divergence\n--- record: {:?}\n--- replay: {:?}",
                record.value, replay.value
            );
            std::process::abort();
        }
    });
    common::counters().flush();
}

enum RecordMode {
    Record,
    Replay(Vec<aver::replay::EffectRecord>),
}

fn run_vm(
    items: &[ResolvedTopLevel],
    symbols: &SymbolTable,
    mode: RecordMode,
) -> Option<RunOutcome> {
    let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let mut arena = aver::nan_value::Arena::new();
        aver::vm::register_service_types(&mut arena);
        let (code, globals) =
            aver::vm::compile_program_with_modules(items, symbols, &mut arena, None, "", None)
                .ok()?;
        let mut machine = aver::vm::VM::new(code, globals, arena);
        machine.set_cli_args(Vec::new());
        let want_record = match &mode {
            RecordMode::Record => {
                machine.start_recording();
                true
            }
            RecordMode::Replay(trace) => {
                // `validate_args = true` so a replay that supplies
                // mismatched effect args fails loudly instead of
                // silently consuming the wrong slot.
                machine.start_replay(trace.clone(), true);
                false
            }
        };
        let (run_res, stdout, _stderr) =
            aver::services::console::capture_output(|| machine.run());
        let nv = run_res.ok()?;
        if let RecordMode::Replay(_) = &mode {
            // `ensure_replay_consumed` is the load-bearing assertion
            // here: a recording that under-runs (program reads N
            // events on record, M < N on replay) silently passes
            // the stdout/value check if both runs stop at the same
            // point but diverged earlier in the trace. The recorder
            // is supposed to capture every effect the program
            // touched; if replay leaves trailing events, the
            // semantic invariant is broken.
            machine.ensure_replay_consumed().ok()?;
        }
        let value = <aver::nan_value::NanValue as aver::nan_value::NanValueConvert>::to_value(
            nv,
            &machine.arena,
        );
        let json = aver::replay::value_to_json(&value).ok()?;
        let recorded_effects = if want_record {
            Some(machine.recorded_effects().to_vec())
        } else {
            None
        };
        Some(RunOutcome {
            stdout,
            value: json,
            recorded_effects,
        })
    }));
    match result {
        Ok(outcome) => outcome,
        Err(_) => None,
    }
}
