//! Embedded runtimes — entry points that compile + execute Aver
//! programs without going through the `aver` CLI binary.
//!
//! Currently `wasm_gc` only. Each module exposes a `run_in_process`
//! variant that takes already-parsed `TopLevel`s plus a
//! `RunConfig`, returns a `RunOutcome`, and never touches
//! `process::exit` / file persistence — recording is handed back
//! to the caller as `RunOutcome::recorded_effects`. That contract
//! lets the fuzz harness, the verify executor, and (eventually)
//! the REPL share one execution path with the production
//! `aver run --wasm-gc` CLI.

#[cfg(feature = "wasm")]
pub mod wasm_gc;
