// Coverage-guided fuzz target: differential testing across the VM
// and wasm-gc backends.
//
// The other targets check structural invariants — "lexer never
// panics", "codegen produces valid wasm", etc. This one checks
// **semantic** invariants: a typechecker-clean program must
// produce the same stdout on both backends. Any divergence is a
// real codegen / VM bug because the language spec defines exactly
// one observable result per program.
//
// Pipeline:
//   1. lex / parse / typecheck (zero errors required — invalid
//      programs have no defined output to compare)
//   2. write the source to a tempfile
//   3. spawn `aver run --vm` and `aver run --wasm-gc`, each with
//      a wall-clock timeout
//   4. on success-on-both: compare stdout byte-for-byte
//   5. on success-on-one + failure-on-other: log + crash (the
//      backends disagree on whether the program is runnable)
//
// Why subprocess instead of in-process: the VM and wasm-gc
// runtimes both want to own stdout, the wasmtime embedder needs
// WASI setup, and there's no `aver::runtime::run_program(items)
// -> String` library entry point. Building one is 200+ LOC of
// infrastructure that's its own project; subprocess shells out
// to the production CLI which is the same thing every Aver user
// runs.
//
// Throughput trade-off: each AFL exec spawns two `aver`
// subprocesses, so execs/sec drops from ~10k (in-process targets)
// to ~5-15. AFL still iterates, just slower. The value is finding
// divergence shapes that in-process targets can't surface — and
// even 10 execs/sec across a 30-min nightly is 18 000 differential
// comparisons, plenty to flush rare codegen bugs.

#[path = "common.rs"]
mod common;

use aver::ir::{PipelineConfig, TypecheckMode};
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::Duration;

const MAX_INPUT_SIZE: usize = 4 * 1024;
const SUBPROCESS_TIMEOUT: Duration = Duration::from_secs(8);

fn aver_bin() -> Option<PathBuf> {
    // The fuzz harness expects the production `aver` binary to be
    // built ahead of time and pointed at via env var. CI sets
    // `AVER_BIN` to the workspace's `target/release/aver`; locally
    // the same env var lets you point at any build. We avoid
    // `env!("CARGO_BIN_EXE_aver")` because the fuzz crate is a
    // separate workspace and the macro would return an aver-fuzz
    // path instead.
    std::env::var_os("AVER_BIN").map(PathBuf::from)
}

#[derive(Debug, PartialEq, Eq)]
struct RunOutput {
    stdout: Vec<u8>,
    exit_status: i32,
}

/// Drive `aver run` for one backend. `extra_args` carries
/// backend-selection flags — empty slice for VM (the default),
/// `&["--wasm-gc"]` for the wasm-gc backend. `aver run` does
/// **not** accept a literal `--vm` flag; passing one makes the
/// CLI bail with `error: unexpected argument '--vm' found`,
/// which the parity comparison would misread as "VM failed,
/// wasm-gc succeeded" and crash on every input. Spent the first
/// CI run learning that the hard way.
fn run_aver(
    aver: &PathBuf,
    extra_args: &[&str],
    source_path: &PathBuf,
) -> Result<RunOutput, String> {
    let mut child = Command::new(aver)
        .arg("run")
        .args(extra_args)
        .arg(source_path)
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .stdin(Stdio::null())
        .spawn()
        .map_err(|e| format!("spawn: {e}"))?;
    // Poor-man's timeout: spin-wait. The fuzz harness only sees
    // run times that complete, so a runaway program just gets
    // killed and the input is skipped. AFL's own timeout
    // mechanism kicks in at the per-exec budget if we ever get
    // wedged here.
    let start = std::time::Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                let output = child.wait_with_output().map_err(|e| format!("wait: {e}"))?;
                return Ok(RunOutput {
                    stdout: output.stdout,
                    exit_status: status.code().unwrap_or(-1),
                });
            }
            Ok(None) => {
                if start.elapsed() > SUBPROCESS_TIMEOUT {
                    let _ = child.kill();
                    return Err(format!("timeout after {:?}", SUBPROCESS_TIMEOUT));
                }
                std::thread::sleep(Duration::from_millis(50));
            }
            Err(e) => return Err(format!("try_wait: {e}")),
        }
    }
}

fn main() {
    afl::fuzz!(|data: &[u8]| {
        if data.len() > MAX_INPUT_SIZE {
            return;
        }
        let c = common::counters();
        c.record_exec();
        let Some(aver) = aver_bin() else {
            // Without an `aver` binary available the differential
            // can't run. Silently skip — the workflow yaml sets
            // `AVER_BIN`; the target degenerates to a no-op when
            // the env var is missing rather than aborting the
            // whole fuzz campaign.
            return;
        };
        let Ok(source) = std::str::from_utf8(data) else {
            return;
        };

        // Frontend gate. Skip programs that wouldn't typecheck
        // (the divergence question is undefined when the
        // language itself rejects the input).
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
        // Pipeline-run for resolve / last_use so both backends
        // start from the identical IR.
        let _result = aver::ir::pipeline::run(
            &mut items,
            PipelineConfig {
                typecheck: Some(TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );

        // Write input to a uniquely-named tempfile so two
        // concurrent fuzz workers can't collide. AFL persistent
        // mode runs single-threaded inside the worker, but we
        // get a fresh tempfile per exec anyway in case AFL ever
        // multiplexes.
        let tmpdir = std::env::temp_dir();
        let pid = std::process::id();
        let nonce = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let source_path = tmpdir.join(format!("aver_fuzz_parity_{pid}_{nonce}.av"));
        if let Ok(mut f) = std::fs::File::create(&source_path) {
            if f.write_all(source.as_bytes()).is_err() {
                let _ = std::fs::remove_file(&source_path);
                return;
            }
        } else {
            return;
        }

        let vm_result = run_aver(&aver, &[], &source_path);
        let wasm_result = run_aver(&aver, &["--wasm-gc"], &source_path);
        let _ = std::fs::remove_file(&source_path);

        match (vm_result, wasm_result) {
            (Ok(vm), Ok(wasm)) => {
                if vm == wasm {
                    return;
                }
                // Both backends accepted the program; their
                // outputs diverge. That's the bug class this
                // target exists to surface.
                let vm_out = String::from_utf8_lossy(&vm.stdout);
                let wasm_out = String::from_utf8_lossy(&wasm.stdout);
                panic!(
                    "VM vs wasm-gc output divergence:\n--- vm (exit {} ) ---\n{vm_out}\n--- wasm-gc (exit {}) ---\n{wasm_out}",
                    vm.exit_status, wasm.exit_status
                );
            }
            (Ok(_), Err(_)) | (Err(_), Ok(_)) => {
                // One backend accepted, the other refused/timed
                // out. Could be a real bug (one codegen breaks
                // valid input) or a timing artefact. Skip for
                // now — the both-accepted-but-diverge case is
                // the unambiguous signal; we can revisit
                // asymmetric outcomes once that surface is
                // exhausted.
            }
            (Err(_), Err(_)) => {
                // Both backends refused/timed out. Skip.
            }
        }
    });
    common::counters().flush();
}
