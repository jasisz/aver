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

/// One Wasmtime configuration for every host that executes Aver wasm-gc.
///
/// Starting the copying collector at zero makes allocation-heavy programs pay
/// for a ladder of heap growths and collections while the heap discovers its
/// working size. Wasmtime reserves this initial space virtually and commits it
/// lazily, so 16 MiB avoids that warm-up cliff without eagerly consuming 16 MiB
/// of physical memory per store.
#[cfg(any(feature = "wasm", feature = "wasip2"))]
pub fn wasmtime_gc_engine_config() -> wasmtime::Config {
    let mut config = wasmtime::Config::new();
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    config.cranelift_opt_level(wasmtime::OptLevel::Speed);
    config.collector(wasmtime::Collector::Copying);
    config.gc_heap_initial_size(16 * 1024 * 1024);
    config.max_wasm_stack(8 * 1024 * 1024);
    // `component-model-async` requires max_wasm_stack <= async_stack_size.
    config.async_stack_size(12 * 1024 * 1024);
    config
}
