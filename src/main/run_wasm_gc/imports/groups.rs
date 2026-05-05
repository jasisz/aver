//! Structural-scope marker host imports for `?!` / `!` independent
//! products. The wasm-gc compiler emits `call $aver/record_enter_group`,
//! `call $aver/record_set_branch(i)`, `call $aver/record_exit_group`
//! around any independent-product literal so the `EffectReplayState`
//! tags contained effects with the same `(group_id, branch_path,
//! effect_occurrence)` tuple the VM recorder produces. Without these
//! markers, wasm-gc traces are flat and cross-backend replay against
//! a VM-recorded `?!` trace breaks.
//!
//! Replay mode treats these the same: each marker advances the
//! recorder's structural scope so subsequent `replay_effect` calls
//! match by `(branch_path, effect_occurrence, type, args)` — the same
//! contract the VM enforces.

use super::super::RunWasmGcHost;

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    _results: &mut [wasmtime::Val],
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    match name {
        "record_enter_group" => {
            if let Some(rec) = caller.data_mut().recorder.as_mut() {
                rec.enter_group();
            }
            Ok(true)
        }
        "record_set_branch" => {
            let branch = match params.first() {
                Some(Val::I64(n)) => *n as u32,
                _ => 0,
            };
            if let Some(rec) = caller.data_mut().recorder.as_mut() {
                rec.set_branch(branch);
            }
            Ok(true)
        }
        "record_exit_group" => {
            if let Some(rec) = caller.data_mut().recorder.as_mut() {
                rec.exit_group();
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
