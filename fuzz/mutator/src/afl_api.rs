//! AFL++ custom mutator C ABI surface.
//!
//! Loaded by `cargo afl fuzz` via
//! `AFL_CUSTOM_MUTATOR_LIBRARY=path/to/libaver_fuzz_mutator.so`.
//! AFL drives the harness coverage feedback + seed selection;
//! this library replaces (or augments — see `splice_optout`
//! decision below) the byte-level mutation stage with AST-aware
//! mutations that produce parseable Aver source.
//!
//! ## Pipeline per `afl_custom_fuzz` call
//!
//!   1. Parse the input bytes into `Vec<TopLevel>` with the host
//!      parser. If parsing fails → return 0 (skip this mutation).
//!      AFL's byte-havoc stage handles unparseable inputs; this
//!      mutator is for structurally-valid programs only.
//!   2. Pick a mutation strategy uniformly at random from
//!      `mutations::STRATEGY_COUNT`. Apply it. If the strategy
//!      found no applicable site → return 0.
//!   3. Unparse the mutated AST via the round-trip-tested
//!      `unparse` entry point.
//!   4. Re-parse the unparsed source as a safety net. The
//!      `corpus_roundtrips_after_one_mutation` test in
//!      `tests/roundtrip.rs` covers this contract but the
//!      defensive check on the fuzz hot path catches any
//!      mutation strategy that ever produces an AST shape the
//!      unparser doesn't handle.
//!   5. Write the result into the caller's output buffer (well,
//!      into our own buffer, then hand AFL a pointer to it).
//!
//! Every step runs inside `panic::catch_unwind`. The mutator
//! must never abort the AFL fuzz process — a panic here looks
//! identical to a target crash from AFL's perspective, taking
//! down the whole campaign instead of reporting one bug.
//!
//! ## Phase 1 decisions
//!
//! - `splice_optout = true`. AFL won't feed us a second input
//!   via `add_buf`. Phase 2 removes this and adds cross-input
//!   mutations (transplant a fn body from program B into the
//!   same position in program A, etc.).
//! - `AFL_CUSTOM_MUTATOR_ONLY` not enabled. Byte-level mutations
//!   still run, so we don't lose coverage on the raw-byte
//!   surface this mutator can't touch (lexer rejection, indent
//!   stack, malformed UTF-8).

use rand::{Rng, SeedableRng};
use std::ffi::{c_char, c_void};
use std::panic::AssertUnwindSafe;

use crate::mutations;
use crate::unparse::unparse;

/// Hard cap on input we'll parse. AFL can feed pathologically
/// large inputs; spending parser cycles on a 10 MiB input would
/// tank our execs/sec without producing useful mutations.
const MAX_INPUT_SIZE: usize = 8 * 1024;

/// Hard cap on output we'll produce. A mutation that bloats the
/// program past AFL's `max_size` would be truncated anyway.
const MAX_OUTPUT_SIZE: usize = 16 * 1024;

/// Per-worker AFL mutator state. AFL allocates one via
/// `afl_custom_init` and threads it back into every later call.
pub struct State {
    rng: rand::rngs::SmallRng,
    /// Reusable output buffer. AFL hands us back the same pointer
    /// across calls, so we own a stable buffer and AFL keeps
    /// `out_buf` valid until the next `afl_custom_fuzz` returns.
    out_buffer: Vec<u8>,
    /// Label returned by `afl_custom_describe`. Re-built per fuzz
    /// call so the AFL queue / crash filename embeds the strategy
    /// that produced the input (`aver-fuzz-mutator-v0:swap-binop`,
    /// etc.). Updated by [`try_mutate`] on success; left as the
    /// neutral version-only string when no mutation was applied so
    /// the describe never refers to a stale strategy.
    describe: std::ffi::CString,
}

const DESCRIBE_BASE: &str = "aver-fuzz-mutator-v0";

impl State {
    fn new(seed: u32) -> Self {
        // Splat the 32-bit AFL seed into a 64-bit SmallRng seed so
        // a re-run with the same AFL seed produces the same
        // mutation chain bit-for-bit.
        let seed64 = (u64::from(seed) << 32) | u64::from(seed);
        State {
            rng: rand::rngs::SmallRng::seed_from_u64(seed64),
            out_buffer: Vec::with_capacity(MAX_OUTPUT_SIZE),
            // No `/` in the describe string. AFL splices it into
            // the queue entry filename ("id:NNN,src:...,<describe>"),
            // and a slash turns the filename into a subdir path
            // that AFL doesn't create — producing
            //   `Unable to create 'out/.../queue/id:000075,src:000000,...,aver-fuzz-mutator/0.1'`
            // and a SYSTEM ERROR that aborts the whole 30-min
            // nightly. Caught on the first nightly run after the
            // mutator landed.
            describe: std::ffi::CString::new(DESCRIBE_BASE).unwrap(),
        }
    }

    /// Rewrite the describe label with the strategy that just
    /// produced an input. AFL reads it synchronously after every
    /// `afl_custom_fuzz` so the pointer only needs to be valid until
    /// the next call replaces it.
    fn set_describe_strategy(&mut self, strategy: &str) {
        let label = format!("{}:{}", DESCRIBE_BASE, strategy);
        // `CString::new` only fails on interior NUL bytes; strategy
        // labels are static kebab-case ASCII, so unwrap is safe.
        self.describe = std::ffi::CString::new(label).unwrap();
    }
}

/// Parse → mutate → unparse → re-parse, all inside `catch_unwind`.
/// Returns the length of the produced output (caller's perspective)
/// or 0 to signal "skip this mutation". The output bytes live in
/// `state.out_buffer`; the C entry point hands AFL a pointer to
/// that buffer after this returns.
fn try_mutate(state: &mut State, input: &[u8]) -> usize {
    if input.is_empty() || input.len() > MAX_INPUT_SIZE {
        return 0;
    }
    let Ok(source) = std::str::from_utf8(input) else {
        return 0;
    };

    // Parse. catch_unwind is mandatory — the Iron — B4 work just
    // closed four classes of parser stack overflow but an
    // adversarial input shape could re-introduce one.
    let parsed = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let mut lexer = aver::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().ok()?;
        let mut parser = aver::parser::Parser::new(tokens);
        parser.parse().ok()
    }));
    let Ok(Some(mut items)) = parsed else {
        return 0;
    };

    // Pick a mutation strategy. If the strategy returns false
    // ("no applicable site"), skip — AFL falls back to byte
    // havoc on this input.
    let strategy = state.rng.random_range(0..mutations::STRATEGY_COUNT);
    let applied = std::panic::catch_unwind(AssertUnwindSafe(|| {
        mutations::apply(strategy, &mut state.rng, &mut items)
    }));
    if !matches!(applied, Ok(true)) {
        return 0;
    }
    // Tag the describe label with the strategy that just applied so
    // AFL embeds it into queue + crash filenames; per-strategy
    // attribution lands in triage automatically.
    state.set_describe_strategy(mutations::strategy_name(strategy));

    // Unparse. catch_unwind because the unparser is full of
    // recursive calls — `Expr::Resolved` etc. are explicit
    // errors but a future strategy could produce something the
    // unparser hasn't seen yet.
    let unparsed = std::panic::catch_unwind(AssertUnwindSafe(|| unparse(&items)));
    let Ok(Ok(out)) = unparsed else {
        return 0;
    };

    // Defensive re-parse. The corpus round-trip test pins this
    // contract for the existing strategies but a regression on
    // the production hot path is worth catching cheaply rather
    // than producing AFL inputs that immediately re-fail their
    // own parse step.
    let reparsed = std::panic::catch_unwind(AssertUnwindSafe(|| {
        let mut lexer = aver::lexer::Lexer::new(&out);
        let tokens = lexer.tokenize().ok()?;
        let mut parser = aver::parser::Parser::new(tokens);
        parser.parse().ok()
    }));
    if !matches!(reparsed, Ok(Some(_))) {
        return 0;
    }

    let bytes = out.into_bytes();
    if bytes.is_empty() || bytes.len() > MAX_OUTPUT_SIZE {
        return 0;
    }

    state.out_buffer.clear();
    state.out_buffer.extend_from_slice(&bytes);
    state.out_buffer.len()
}

// ---------------------------------------------------------------------------
// C ABI surface — AFL++ `custom_mutator.h` entry points
// ---------------------------------------------------------------------------

/// AFL calls this once per worker at startup with the worker's
/// seed. We return an opaque pointer AFL threads through every
/// later call.
///
/// # Safety
/// Caller (AFL) treats the returned pointer as opaque. The
/// pointer must outlive every other call into this library;
/// `afl_custom_deinit` reclaims it.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn afl_custom_init(_afl: *mut c_void, seed: u32) -> *mut State {
    Box::into_raw(Box::new(State::new(seed)))
}

/// Run one mutation. Returns the number of bytes written to
/// `*out_buf`, or 0 to skip this input.
///
/// # Safety
/// `state` must be the pointer `afl_custom_init` returned. `buf`
/// must point to `buf_size` readable bytes. `out_buf` is an
/// out-parameter: on success we overwrite `*out_buf` with a
/// pointer into our own buffer, valid until the next
/// `afl_custom_fuzz` call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn afl_custom_fuzz(
    state: *mut State,
    buf: *const u8,
    buf_size: usize,
    out_buf: *mut *mut u8,
    _add_buf: *const u8,
    _add_buf_size: usize,
    max_size: usize,
) -> usize {
    if state.is_null() || buf.is_null() || out_buf.is_null() {
        return 0;
    }
    let state = unsafe { &mut *state };
    let input = unsafe { std::slice::from_raw_parts(buf, buf_size) };

    let produced = try_mutate(state, input);
    if produced == 0 || produced > max_size {
        return 0;
    }

    unsafe {
        *out_buf = state.out_buffer.as_mut_ptr();
    }
    produced
}

/// Tell AFL not to invoke `add_buf` splicing in Phase 1. The
/// mere existence of this symbol opts the mutator out — AFL++
/// inspects the function pointer at init time.
///
/// # Safety
/// `state` must be the pointer `afl_custom_init` returned.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn afl_custom_splice_optout(_state: *mut State) {}

/// Short human-readable label AFL prints in its status line.
///
/// # Safety
/// `state` must be the pointer `afl_custom_init` returned. The
/// returned pointer is valid for the lifetime of the `State`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn afl_custom_describe(state: *mut State, _max_len: usize) -> *const c_char {
    if state.is_null() {
        return std::ptr::null();
    }
    let state = unsafe { &*state };
    state.describe.as_ptr()
}

/// Release the mutator state. AFL calls this once per worker at
/// shutdown.
///
/// # Safety
/// `state` must be the pointer `afl_custom_init` returned, and
/// must not be used after this call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn afl_custom_deinit(state: *mut State) {
    if !state.is_null() {
        drop(unsafe { Box::from_raw(state) });
    }
}
