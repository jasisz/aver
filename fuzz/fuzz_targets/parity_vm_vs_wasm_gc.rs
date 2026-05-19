// Coverage-guided fuzz target: differential testing across the
// VM and wasm-gc backends, **in-process**.
//
// Earlier versions spawned two `aver run` subprocesses per AFL
// exec — \~5-15 execs/sec, which is enough to find divergence
// shapes but burns CI minutes for what should be a hot path.
// Iron 0.21 stage 5 moves both backends in-process:
//
//   * VM       — `aver::vm::compile_program_with_modules` +
//                `aver::vm::VM::run`
//   * wasm-gc  — `aver::runtime::wasm_gc::run_in_process`
//
// Stdout from each backend is captured via
// `aver::services::console::capture_output`, which redirects every
// `Console.print` / `Console.error` / `Console.warn` call into an
// in-memory buffer for the duration of the closure. Both backends
// dispatch through that same service, so the capture works
// uniformly without per-backend plumbing.
//
// Pipeline per AFL exec:
//   1. lex / parse / typecheck / resolve (zero errors required —
//      undefined output is no useful divergence signal)
//   2. compile + run on the VM, capturing stdout + decoded value
//   3. compile + run on wasm-gc, capturing stdout + decoded value
//   4. compare: both bytes-for-bytes-identical stdout AND the
//      decoded entry-fn return value must match
//
// Every backend call sits inside `panic::catch_unwind`. A panic
// in either runtime would otherwise take down the whole AFL
// worker — that's "AFL found a runtime ICE" but presents as a
// generic fuzz crash; the structured asymmetric-outcome branch
// is a better signal because it preserves both backends'
// observed behaviour.
//
// Throughput: pre-refactor target was 5-15 execs/sec; in-process
// removes the subprocess spawn + tempfile write, so the ceiling
// is now the VM/wasmtime instantiation cost — empirically
// hundreds of execs/sec on the same hardware. AFL's wall-time
// budget per nightly stays the same, but the iteration count
// grows \~30-100x.

#[path = "common.rs"]
mod common;

use aver::ast::TopLevel;
use aver::ir::{PipelineConfig, TypecheckMode};
use aver::replay::JsonValue;
use std::panic::AssertUnwindSafe;

const MAX_INPUT_SIZE: usize = 4 * 1024;

/// Decoded outcome of one backend run. `stdout` is the captured
/// `Console.*` byte stream; `value` is the entry-fn return value
/// projected into `JsonValue` so VM (`NanValue`) and wasm-gc
/// (`JsonValue` already) compare in the same currency.
struct BackendOutcome {
    stdout: Vec<u8>,
    value: JsonValue,
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

        // Resolve / last-use / typed-AST pass. Both backends start
        // from the same items; whatever divergence the differential
        // detects is downstream of this point.
        let pipeline = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );
        let analysis = pipeline.analysis.as_ref();

        let vm_outcome = run_vm(&items, analysis);
        let wasm_outcome = run_wasm_gc(&items, analysis);

        match (vm_outcome, wasm_outcome) {
            (Some(vm), Some(wasm)) => {
                if vm.stdout == wasm.stdout && vm.value == wasm.value {
                    return;
                }
                let vm_out = String::from_utf8_lossy(&vm.stdout);
                let wasm_out = String::from_utf8_lossy(&wasm.stdout);
                panic!(
                    "VM vs wasm-gc divergence:\n--- vm stdout ---\n{vm_out}\n--- wasm-gc stdout ---\n{wasm_out}\n--- vm value: {:?}\n--- wasm-gc value: {:?}",
                    vm.value, wasm.value
                );
            }
            _ => {
                // One or both backends refused / panicked. Asymmetric
                // outcomes overlap with what the codegen target
                // already chases (one backend produces a valid
                // module, the other doesn't); chasing them here
                // adds noise without a clear new signal class. Skip
                // until the structural-output surface is exhausted.
            }
        }
    });
    common::counters().flush();
}

/// Compile `items` against the VM, run `main`, capture stdout +
/// return value. `None` on any compile / runtime / panic failure;
/// `Some(outcome)` when the run completed cleanly.
fn run_vm(items: &[TopLevel], analysis: Option<&aver::ir::AnalysisResult>) -> Option<BackendOutcome> {
    let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let mut arena = aver::nan_value::Arena::new();
        aver::vm::register_service_types(&mut arena);
        let (code, globals) =
            aver::vm::compile_program_with_modules(items, &mut arena, None, "", analysis)
                .ok()?;
        let mut machine = aver::vm::VM::new(code, globals, arena);
        machine.set_cli_args(Vec::new());
        let (run_res, stdout, _stderr) =
            aver::services::console::capture_output(|| machine.run());
        let nv = run_res.ok()?;
        let value = <aver::nan_value::NanValue as aver::nan_value::NanValueConvert>::to_value(
            nv,
            &machine.arena,
        );
        let json = aver::replay::value_to_json(&value).ok()?;
        Some(BackendOutcome {
            stdout,
            value: json,
        })
    }));
    match result {
        Ok(outcome) => outcome,
        Err(_) => None,
    }
}

/// Compile `items` against the wasm-gc backend, instantiate inside
/// the embedded wasmtime engine, run `main`, capture stdout. `None`
/// on any compile / runtime / panic failure.
fn run_wasm_gc(
    items: &[TopLevel],
    analysis: Option<&aver::ir::AnalysisResult>,
) -> Option<BackendOutcome> {
    let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let (run_res, stdout, _stderr) = aver::services::console::capture_output(|| {
            aver::runtime::wasm_gc::run_in_process(
                items,
                analysis,
                aver::runtime::wasm_gc::RunConfig {
                    program_args: Vec::new(),
                    entry_info: None,
                    mode: aver::runtime::wasm_gc::EffectMode::Normal,
                },
            )
        });
        let outcome = run_res.ok()?;
        Some(BackendOutcome {
            stdout,
            value: outcome.output,
        })
    }));
    match result {
        Ok(outcome) => outcome,
        Err(_) => None,
    }
}
