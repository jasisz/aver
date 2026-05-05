//! `aver/*` host import dispatch root. The 29 effect arms have been
//! carved into per-namespace submodules under `imports/`; this file
//! is the chained `dispatch_aver_import` that walks them in order
//! and the central re-export hub for helpers `decode.rs` consumes.
//!
//! Per-namespace layout:
//!
//! - `args` / `console` / `disk` / `env` / `numeric` (Random + float
//!   math) / `tcp` / `terminal` / `time` — one-effect-namespace each,
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
    host_option_string_some, host_result_err_list_string, host_result_err_string,
    host_result_err_unit_string, host_result_http_response_err, host_result_http_response_ok,
    host_result_ok_list_string, host_result_ok_string, host_result_ok_unit,
    host_result_tcp_connection_err, host_result_tcp_connection_ok, host_tcp_connection_make,
    host_terminal_size_make,
};
pub(super) use lm::lm_string_from_host;

use http::{HttpVerb, http_body_dispatch, http_simple_dispatch};

pub(super) fn dispatch_aver_import(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
) -> Result<bool, wasmtime::Error> {
    if args::dispatch(name, caller, params, results)? {
        return Ok(true);
    }
    if console::dispatch(name, caller, params, results)? {
        return Ok(true);
    }
    if disk::dispatch(name, caller, params, results)? {
        return Ok(true);
    }
    if env::dispatch(name, caller, params, results)? {
        return Ok(true);
    }
    if groups::dispatch(name, caller, params, results)? {
        return Ok(true);
    }
    if numeric::dispatch(name, caller, params, results)? {
        return Ok(true);
    }
    if tcp::dispatch(name, caller, params, results)? {
        return Ok(true);
    }
    if terminal::dispatch(name, caller, params, results)? {
        return Ok(true);
    }
    if time::dispatch(name, caller, params, results)? {
        return Ok(true);
    }
    match name {
        "http_get" => http_simple_dispatch(caller, params, results, HttpVerb::Get),
        "http_head" => http_simple_dispatch(caller, params, results, HttpVerb::Head),
        "http_delete" => http_simple_dispatch(caller, params, results, HttpVerb::Delete),
        "http_post" => http_body_dispatch(caller, params, results, HttpVerb::Post),
        "http_put" => http_body_dispatch(caller, params, results, HttpVerb::Put),
        "http_patch" => http_body_dispatch(caller, params, results, HttpVerb::Patch),
        _ => Ok(false),
    }
}
