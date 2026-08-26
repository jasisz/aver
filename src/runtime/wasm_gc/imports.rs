//! `aver/*` host import dispatch root. Effect and internal protocol arms are
//! carved into focused submodules under `imports/`; this file is the chained
//! `dispatch_aver_import` that walks them in order and the central re-export
//! hub for helpers `decode.rs` consumes.
//!
//! Per-namespace layout:
//!
//! - `args` / `console` / `diagnostics` / `disk` / `env` / `numeric` (Random + float
//!   math) / `process` / `tcp` / `terminal` / `time` — one-effect-namespace each,
//!   each exposing a `dispatch(name, caller, params, results) ->
//!   Result<bool>` that returns `true` when it handled the name and
//!   `false` otherwise.
//! - `http` — verbs are one-liner aliases, kept inline below.
//!
//! Helper modules that every namespace needs:
//!
//! - `lm` — LM transport (string round-trip via `__rt_string_*`)
//! - `replay_glue` — `try_replay`, `record_effect_if_recording`, `json_*`
//! - `factories` — `host_*_make` builders for wasm-gc structs

#![cfg(feature = "wasm")]

use super::RunWasmGcHost;

#[path = "imports/args.rs"]
mod args;
#[path = "imports/console.rs"]
mod console;
#[path = "imports/diagnostics.rs"]
mod diagnostics;
#[path = "imports/disk.rs"]
mod disk;
#[path = "imports/env.rs"]
mod env;
#[path = "imports/factories.rs"]
mod factories;
#[path = "imports/groups.rs"]
mod groups;
#[path = "imports/http.rs"]
mod http;
#[path = "imports/lm.rs"]
mod lm;
#[path = "imports/numeric.rs"]
mod numeric;
#[path = "imports/process.rs"]
mod process;
#[path = "imports/replay_glue.rs"]
mod replay_glue;
#[path = "imports/tcp.rs"]
mod tcp;
#[path = "imports/terminal.rs"]
mod terminal;
#[path = "imports/time.rs"]
mod time;

pub(super) use factories::{
    host_http_response_make, host_map_string_list_string_empty, host_option_string_none,
    host_option_string_some, host_result_err_bytes, host_result_err_int,
    host_result_err_list_string, host_result_err_string, host_result_err_unit_string,
    host_result_http_response_err, host_result_http_response_ok, host_result_ok_bytes,
    host_result_ok_int, host_result_ok_list_string, host_result_ok_string, host_result_ok_unit,
    host_result_option_string_err, host_result_option_string_ok, host_result_tcp_connection_err,
    host_result_tcp_connection_ok, host_result_terminal_size_err, host_result_terminal_size_ok,
    host_tcp_connection_make, host_terminal_size_make,
};
pub(super) use lm::{lm_string_from_host, lm_string_to_host};

use http::{HttpVerb, http_body_dispatch, http_simple_dispatch};

pub(super) fn dispatch_aver_import(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
) -> Result<bool, wasmtime::Error> {
    // Every effect import now carries a trailing `caller_fn:
    // any_ref` param emitted by the codegen — see
    // `effects.rs::params` and `body/builtins.rs::FnCall`. Decode
    // it once, stash on the host so per-namespace dispatch arms
    // can hand it to `record_effect_if_recording` without each arm
    // re-running the LM transport, then forward `real_params`
    // (params without the trailing slot) into the per-namespace
    // dispatch chain. Falls back to "main" when the trailing arg
    // is missing or not decodable — keeps replay of older recorded
    // traces / hand-rolled wasm-gc modules working.
    // Trailing arg is now an `i32` idx into the caller-fn name table
    // the host materialised at instantiation (see
    // `run_wasm_gc::build_caller_fn_table`). Vector index lookup
    // replaces the per-call LM round-trip from 0.16.2.
    let caller_fn_idx = params
        .last()
        .and_then(|v| match v {
            wasmtime::Val::I32(n) => Some(*n),
            _ => None,
        })
        .unwrap_or(-1);
    let caller_fn: String = caller
        .data()
        .caller_fn_table
        .get(caller_fn_idx as usize)
        .cloned()
        .unwrap_or_else(|| "main".to_string());
    let caller_fn_ref: &str = &caller_fn;
    let real_params: &[wasmtime::Val] = if params.is_empty() {
        params
    } else {
        &params[..params.len() - 1]
    };
    let params = real_params;

    if args::dispatch(name, caller, params, results, caller_fn_ref)? {
        return Ok(true);
    }
    if console::dispatch(name, caller, params, results, caller_fn_ref)? {
        return Ok(true);
    }
    if diagnostics::dispatch(name, caller, params, results, caller_fn_ref)? {
        return Ok(true);
    }
    if disk::dispatch(name, caller, params, results, caller_fn_ref)? {
        return Ok(true);
    }
    if env::dispatch(name, caller, params, results, caller_fn_ref)? {
        return Ok(true);
    }
    if groups::dispatch(name, caller, params, results)? {
        return Ok(true);
    }
    if numeric::dispatch(name, caller, params, results, caller_fn_ref)? {
        return Ok(true);
    }
    if process::dispatch(name, caller, params, results, caller_fn_ref)? {
        return Ok(true);
    }
    if tcp::dispatch(name, caller, params, results, caller_fn_ref)? {
        return Ok(true);
    }
    if terminal::dispatch(name, caller, params, results, caller_fn_ref)? {
        return Ok(true);
    }
    if time::dispatch(name, caller, params, results, caller_fn_ref)? {
        return Ok(true);
    }
    match name {
        "http_get" => http_simple_dispatch(caller, params, results, HttpVerb::Get, caller_fn_ref),
        "http_head" => http_simple_dispatch(caller, params, results, HttpVerb::Head, caller_fn_ref),
        "http_delete" => {
            http_simple_dispatch(caller, params, results, HttpVerb::Delete, caller_fn_ref)
        }
        "http_post" => http_body_dispatch(caller, params, results, HttpVerb::Post, caller_fn_ref),
        "http_put" => http_body_dispatch(caller, params, results, HttpVerb::Put, caller_fn_ref),
        "http_patch" => http_body_dispatch(caller, params, results, HttpVerb::Patch, caller_fn_ref),
        _ => Ok(false),
    }
}
