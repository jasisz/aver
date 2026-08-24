//! Internal wasm-gc diagnostics. These imports are compiler/runtime protocol,
//! not Aver effects: they are neither capability-gated nor recorded/replayed.

use super::super::RunWasmGcHost;
use super::lm::lm_string_to_host;

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    _results: &mut [wasmtime::Val],
    _caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    if name != "provider_contract_violation" {
        return Ok(false);
    }
    let error = lm_string_to_host(caller, params.first())?.unwrap_or_default();
    crate::services::console::write_stderr_plain_str(&format!(
        "provider contract violated: discharged Result returned Err({error})"
    ));
    Ok(true)
}
