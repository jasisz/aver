//! Jobs and the one wait of a turn on wasm-gc (jasisz/aver#1329).
//!
//! A job runs inline at `begin` on this target, so the module owns everything
//! about it: the handle is the job, and `Work.cancel` has already happened by
//! the time this host arm is reached. What is left here is what only the host
//! can do — watch the sockets a wait set holds, and put the turn in the
//! recording with the VM's shape, so a recording crosses backends in both
//! directions.
//!
//! `Wait.poll` splits one wait set the way the VM does: sockets go to the
//! same reactor `Tcp.poll` uses, and every job key is ready at once, because
//! a job on this target is done the moment it began. A wait set holding any
//! job therefore returns immediately, which is the timeout such a set asked
//! for.

use super::super::RunWasmGcHost;
use super::factories::{host_result_err_list_int, host_result_ok_list_int_refs};
use super::replay_glue::{json_ok, record_effect_if_recording, try_replay};
use super::tcp::{
    PollEntry, decode_guest_int, decode_wait_entries, guest_int_json, job_handle_id,
    json_capability_resource, poll_map_json, replay_poll_result,
};

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    match name {
        "wait_poll" => {
            let entries = decode_wait_entries(caller, params.first())?;
            let timeout = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Wait.poll: missing timeoutMs"))?;
            let timeout =
                decode_guest_int(caller, timeout, "Wait.poll: malformed timeout carrier")?;
            let args = vec![poll_map_json(&entries), guest_int_json(&timeout)];
            if let Some(cached) = try_replay(caller, "Wait.poll", args.clone())? {
                let result = replay_poll_result(caller, &cached, &entries)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }

            let (result_ref, outcome) = match wait(caller, &entries, &timeout)? {
                Ok(ready) => {
                    let refs = ready.iter().map(|entry| entry.key_ref).collect::<Vec<_>>();
                    let json = ready
                        .iter()
                        .map(|entry| entry.key_json.clone())
                        .collect::<Vec<_>>();
                    (
                        host_result_ok_list_int_refs(caller, &refs)?,
                        json_ok(aver::replay::JsonValue::Array(json)),
                    )
                }
                Err(error) => (
                    host_result_err_list_int(caller, &error)?,
                    super::replay_glue::json_err(&error),
                ),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Wait.poll", args, outcome, caller_fn);
            Ok(true)
        }
        "work_cancel" => {
            // The module has already dropped the answer and marked the slot.
            // This is the turn's record of it, and in replay it is what keeps
            // the recorded stream in step.
            let id = job_handle_id(
                caller,
                params
                    .first()
                    .ok_or_else(|| wasmtime::Error::msg("Work.cancel: missing job handle"))?,
            )?;
            let args = vec![json_capability_resource("Work.Job", id)];
            if try_replay(caller, "Work.cancel", args.clone())?.is_some() {
                return Ok(true);
            }
            record_effect_if_recording(
                caller,
                "Work.cancel",
                args,
                aver::replay::JsonValue::Null,
                caller_fn,
            );
            Ok(true)
        }
        _ => Ok(false),
    }
}

/// One wait over a set of sockets and jobs.
///
/// A job is ready at once, so a set holding one waits no longer than it takes
/// to ask the reactor what is ready right now. A set holding only sockets
/// waits exactly as `Tcp.poll` does.
fn wait<'entries>(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    entries: &'entries [PollEntry],
    timeout: &super::tcp::GuestInt,
) -> Result<Result<Vec<&'entries PollEntry>, String>, wasmtime::Error> {
    let _ = caller;
    let Some(timeout_ms) = timeout.value else {
        return Ok(Err(format!(
            "Wait.poll: timeoutMs {} exceeds the poll limit",
            timeout.display
        )));
    };
    let jobs = entries
        .iter()
        .filter(|entry| entry.socket.is_none())
        .collect::<Vec<_>>();
    let mut sockets = entries
        .iter()
        .filter(|entry| entry.socket.is_some())
        .collect::<Vec<_>>();
    sockets.sort_by(|left, right| left.provider_order.cmp(&right.provider_order));

    let mut ready = jobs;
    if !sockets.is_empty() {
        let handles = sockets
            .iter()
            .filter_map(|entry| entry.socket.clone())
            .collect::<Vec<_>>();
        let waited = if ready.is_empty() { timeout_ms } else { 0 };
        match aver_rt::tcp::poll(&handles, waited) {
            Ok(positions) => {
                for position in positions {
                    if let Some(entry) = sockets.get(position) {
                        ready.push(entry);
                    }
                }
            }
            Err(error) => return Ok(Err(error)),
        }
    }
    ready.sort_by(|left, right| left.numeric.cmp(&right.numeric));
    ready.dedup_by(|left, right| left.numeric == right.numeric);
    Ok(Ok(ready))
}
