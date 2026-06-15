//! Regression: under `Int = ℤ`, an out-of-i64 `Int` passed as an i64-typed
//! HOST EFFECT argument must REJECT on wasm-gc, matching the VM.
//!
//! The bug (confirmed in wasmtime on the bignum default-flip branch): an
//! out-of-i64 Big Int passed as an i64-typed effect argument silently
//! SATURATED on wasm-gc and proceeded, where the VM raises a runtime error.
//! `effect_int_arg_positions` lowered the Int args of host effects via
//! `__aint_to_i64_sat` (`2^63 -> i64::MAX`) BEFORE the host call, for
//! `Random.int` bounds, `Time.sleep` ms, `Tcp.*` ports, `HttpServer.*`
//! bind ports, and `Terminal.moveTo` coordinates. The VM's host services do
//! a CHECKED `to_i64()` and ERROR instead (e.g. `Random.int: bounds must
//! fit a 64-bit integer`). Worst case: `Time.sleep(2^63)` saturated to
//! i64::MAX ms (a ~292-million-year hang) where the VM errors.
//!
//! The fix replaces the saturating lower at the EFFECT-arg boundary ONLY
//! with `__aint_to_i64_checked`, which TRAPS (`unreachable`) on an out-of-
//! i64 Big. So an out-of-range effect arg now rejects on wasm-gc (a wasm
//! trap → `run_in_process` returns `Err`) just as the VM errors. The
//! SATURATING path stays in place for the PURE builtins where saturation
//! MATCHES the VM (`String.charAt`/`slice` indices, `List.take`/`drop`
//! counts, `Char.fromCode`) — this test pins that the pure-builtin
//! saturation is NOT regressed.

#![cfg(feature = "wasm")]

use aver::ir::{NeutralAllocPolicy, PipelineConfig, TypecheckMode};

/// Parse + typecheck + run a program through the in-process wasm-gc runtime,
/// mirroring the CLI's `aver run --wasm-gc` pipeline (`try_run_wasm_gc` in
/// `src/main/run_wasm_gc.rs`): the neutral alloc policy + the `analysis`
/// facts must be threaded into `run_in_process`, otherwise codegen takes a
/// different (analysis-less) path. Returns `Ok` with captured stdout on a
/// clean run, or `Err(message)` when wasm execution traps / the backend
/// rejects.
fn run_wasm_gc(source: &str) -> Result<String, String> {
    let mut lexer = aver::lexer::Lexer::new(source);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = aver::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    let neutral_policy = NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );

    let (run_res, stdout, _stderr) = aver::services::console::capture_output(|| {
        aver::runtime::wasm_gc::run_in_process(
            &items,
            result.analysis.as_ref(),
            aver::runtime::wasm_gc::RunConfig {
                program_args: Vec::new(),
                entry_info: None,
                mode: aver::runtime::wasm_gc::EffectMode::Normal,
            },
        )
    });

    run_res
        .map(|_| String::from_utf8_lossy(&stdout).into_owned())
        .map_err(|e| e.to_string())
}

/// `9223372036854775807 + 1` builds `2^63` (the first value past i64::MAX)
/// by arithmetic, so the `$AverInt` carrier holds a Big. Passing it as the
/// `Random.int` upper bound (an i64-typed effect arg) must TRAP on wasm-gc
/// — the VM errors with `Random.int: bounds must fit a 64-bit integer`.
#[test]
fn out_of_i64_random_bound_traps_on_wasm_gc() {
    let src = r#"module M
    intent =
        "out-of-i64 Random.int bound rejects"
    effects [Random, Console]

fn main() -> Unit
    ! [Random.int, Console.print]
    big = 9223372036854775807 + 1
    n = Random.int(1, big)
    Console.print("n = {n}")
"#;
    let res = run_wasm_gc(src);
    assert!(
        res.is_err(),
        "an out-of-i64 Random.int upper bound must REJECT on wasm-gc (matching the VM's \
         checked-bounds error), but the run succeeded with stdout: {res:?}"
    );
}

/// `Time.sleep(2^63)` must TRAP on wasm-gc, not saturate to i64::MAX ms (a
/// ~292-million-year hang). The VM errors (`Time.sleep: ms must fit a
/// 64-bit integer`). The test would HANG, not just fail, on the unfixed
/// saturating lowering — so a clean (fast) `Err` is the regression guard.
#[test]
fn out_of_i64_sleep_ms_traps_on_wasm_gc() {
    let src = r#"module M
    intent =
        "out-of-i64 Time.sleep ms rejects (no 292-million-year hang)"
    effects [Time, Console]

fn main() -> Unit
    ! [Time.sleep, Console.print]
    big = 9223372036854775807 + 1
    _ = Time.sleep(big)
    Console.print("woke up")
"#;
    let res = run_wasm_gc(src);
    assert!(
        res.is_err(),
        "Time.sleep(2^63) must TRAP on wasm-gc (terminate with an error), not saturate to \
         i64::MAX ms and hang; run returned: {res:?}"
    );
}

/// IN-RANGE effect args STILL WORK: a small `Random.int` bound and a small
/// `Time.sleep` lower fine through the checked helper (a Small passes its
/// `$small` through), so the program runs to completion and prints.
#[test]
fn in_range_effect_args_still_run_on_wasm_gc() {
    let src = r#"module M
    intent =
        "in-range effect args still run"
    effects [Random, Time, Console]

fn main() -> Unit
    ! [Random.int, Time.sleep, Console.print]
    n = Random.int(1, 6)
    _ = Time.sleep(1)
    Console.print("done")
"#;
    let out = run_wasm_gc(src).expect("in-range effect args must run cleanly on wasm-gc");
    assert_eq!(
        out, "done\n",
        "a normal program with small Random.int / Time.sleep args must run to completion; \
         got stdout {out:?}"
    );
}

/// PURE-builtin saturation is UNCHANGED: `String.charAt` with an out-of-i64
/// index past the string end must SATURATE to `Option.None` on wasm-gc (the
/// VM's clamp), NOT trap. This pins that the fix touched only the EFFECT-arg
/// boundary and left the saturating `__aint_to_i64_sat` path intact for the
/// pure builtins where saturation matches the VM.
#[test]
fn pure_builtin_out_of_i64_index_still_saturates_on_wasm_gc() {
    let src = r#"module M
    intent =
        "pure-builtin out-of-i64 index saturates to None (unchanged)"
    effects [Console]

fn main() -> Unit
    ! [Console.print]
    big = 9223372036854775807 + 1
    c = String.charAt("hello", big)
    match c
        Option.Some(ch) -> Console.print("got {ch}")
        Option.None -> Console.print("none")
"#;
    let out = run_wasm_gc(src)
        .expect("String.charAt with an out-of-range index must SATURATE to None, not trap");
    assert_eq!(
        out, "none\n",
        "an out-of-i64 String.charAt index must saturate past the string end to Option.None \
         (the VM's clamp), matching the pure-builtin saturating path; got stdout {out:?}"
    );
}
