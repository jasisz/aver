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
    config.max_wasm_stack(WASM_STACK_BYTES);
    // `component-model-async` requires max_wasm_stack <= async_stack_size.
    config.async_stack_size(aver_rt::THREAD_STACK_BYTES);
    config
}

/// How much stack a wasm guest may use before Wasmtime stops it with a
/// stack-overflow trap. The guest runs on the thread that calls it, so that
/// thread needs this much and room for the host on top: [`on_wasm_stack`]
/// gives it [`aver_rt::THREAD_STACK_BYTES`], the stack compiled Rust gives
/// `main`, and keeps 32 MiB of it for the host. A thread with less stack than
/// this overflows before the guest does, and the process aborts.
#[cfg(any(feature = "wasm", feature = "wasip2"))]
pub const WASM_STACK_BYTES: usize = aver_rt::THREAD_STACK_BYTES - 32 * 1024 * 1024;

/// Run `body`, which calls into a wasm guest, on a thread with
/// [`aver_rt::THREAD_STACK_BYTES`] of stack, so deep recursion in the guest
/// ends in Wasmtime's stack-overflow trap rather than in the host's guard
/// page.
#[cfg(any(feature = "wasm", feature = "wasip2"))]
pub fn on_wasm_stack<T: Send>(body: impl FnOnce() -> T + Send) -> T {
    std::thread::scope(|scope| match aver_rt::spawn_scoped(scope, body).join() {
        Ok(value) => value,
        Err(panic) => std::panic::resume_unwind(panic),
    })
}

/// The message for a guest that ran out of stack, or `None` for any other
/// error.
#[cfg(any(feature = "wasm", feature = "wasip2"))]
pub fn stack_overflow_message(error: &wasmtime::Error) -> Option<String> {
    matches!(
        error.downcast_ref::<wasmtime::Trap>(),
        Some(wasmtime::Trap::StackOverflow)
    )
    .then(|| {
        format!(
            "stack overflow: recursion too deep (the wasm stack limit is {} MiB)",
            WASM_STACK_BYTES / (1024 * 1024)
        )
    })
}

/// Work workers need interruption points even inside a pure recursive body.
#[cfg(feature = "wasm")]
pub fn wasmtime_work_engine_config() -> wasmtime::Config {
    let mut config = wasmtime_gc_engine_config();
    config.epoch_interruption(true);
    config
}
