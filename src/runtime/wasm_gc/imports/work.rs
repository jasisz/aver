//! Work cancellation and the shared socket/job wait on wasm-gc.
//!
//! Versioned Work modules use the host scheduler; legacy inline modules keep
//! their original recording hooks and immediate job readiness.

use super::super::RunWasmGcHost;
use super::factories::{host_wait_result_err, host_wait_result_ok};
use super::replay_glue::{json_ok, record_effect_if_recording, try_replay};
use super::tcp::{
    PollEntry, decode_guest_int, decode_wait_entries, guest_int_json, job_handle_id,
    json_capability_resource, poll_map_json,
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
            // A wait set is the largest argument any door renders, and a key
            // this host cannot read refuses the render outright, so a run
            // that records nothing never asks for one.
            let args = if super::replay_glue::replay_is_active(caller) {
                vec![
                    poll_map_json(&entries, "Wait.poll")?,
                    guest_int_json(&timeout),
                ]
            } else {
                Vec::new()
            };
            if let Some(cached) = try_replay(caller, "Wait.poll", args.clone())? {
                let result = super::tcp::replay_wait_result(caller, &cached, &entries)?;
                results[0] = Val::AnyRef(result);
                return Ok(true);
            }

            let (result_ref, outcome) = match wait(caller, &entries, &timeout)? {
                Ok(ready) => {
                    let refs = ready.iter().map(|entry| entry.key_ref).collect::<Vec<_>>();
                    let json = if super::replay_glue::replay_is_active(caller) {
                        super::tcp::recorded_keys(&ready, "Wait.poll")?
                    } else {
                        Vec::new()
                    };
                    (
                        host_wait_result_ok(caller, &refs)?,
                        json_ok(aver::replay::JsonValue::Array(json)),
                    )
                }
                Err(error) => (
                    host_wait_result_err(caller, &error)?,
                    super::replay_glue::json_err(&error),
                ),
            };
            results[0] = Val::AnyRef(result_ref);
            record_effect_if_recording(caller, "Wait.poll", args, outcome, caller_fn);
            Ok(true)
        }
        "work_cancel" => {
            // Cancel the host job too; legacy inline modules already updated
            // their handle before entering this recording boundary.
            let id = job_handle_id(
                caller,
                params
                    .first()
                    .ok_or_else(|| wasmtime::Error::msg("Work.cancel: missing job handle"))?,
            )?;
            let args = vec![json_capability_resource("Work.Job", id)];
            if let Some(work) = &caller.data().host_work
                && let Ok(job) = work.job(id, None)
            {
                job.cancel();
            }
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
        "work_begin" => {
            let kind = kind_index(params.first(), "Work.begin")?;
            let handle = job_handle_id(
                caller,
                params
                    .get(2)
                    .ok_or_else(|| wasmtime::Error::msg("Work.begin: missing job handle"))?,
            )?;
            let task = params.get(1).copied();
            super::super::provider_host::record_job_operation(
                caller,
                kind,
                false,
                task.as_ref(),
                handle,
                caller_fn,
            )?;
            Ok(true)
        }
        "work_take" => {
            let kind = kind_index(params.first(), "Work.take")?;
            let handle = job_handle_id(
                caller,
                params
                    .get(1)
                    .ok_or_else(|| wasmtime::Error::msg("Work.take: missing job handle"))?,
            )?;
            let answer = params.get(2).copied();
            let replayed = super::super::provider_host::record_job_operation(
                caller,
                kind,
                true,
                answer.as_ref(),
                handle,
                caller_fn,
            )?;
            // Outside replay the module's own answer is the answer.
            results[0] = match replayed {
                Some(recorded) => Val::AnyRef(recorded),
                None => answer.unwrap_or(Val::AnyRef(None)),
            };
            Ok(true)
        }
        _ => Ok(false),
    }
}

/// The job kind one of the two recording imports names.
fn kind_index(value: Option<&wasmtime::Val>, operation: &str) -> Result<i32, wasmtime::Error> {
    match value {
        Some(wasmtime::Val::I32(kind)) => Ok(*kind),
        _ => Err(wasmtime::Error::msg(format!(
            "{operation}: missing the job kind index"
        ))),
    }
}

/// One wait over a set of sockets and jobs.
///
/// Versioned modules watch host completions. Legacy inline modules treat
/// every job as ready and only probe their sockets.
fn wait<'entries>(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    entries: &'entries [PollEntry],
    timeout: &super::tcp::GuestInt,
) -> Result<Result<Vec<&'entries PollEntry>, String>, wasmtime::Error> {
    if let Some(work) = &caller.data().host_work {
        return Ok(wait_host(work, entries, timeout));
    }
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
    // The entries came off the map in its own key order, so the wait answers
    // in it by sorting on where each entry sat. Two passes over the jobs can
    // name the same entry twice; the same position collapses them.
    ready.sort_by_key(|entry| entry.order);
    ready.dedup_by_key(|entry| entry.order);
    Ok(Ok(ready))
}

/// The host-scheduled wait: the same loop the VM and generated Rust run,
/// from `aver-rt`, over this host's sockets and the jobs its handles name. A
/// handle this host no longer knows is reported ready, and `take` then gives
/// the real answer.
fn wait_host<'a>(
    work: &std::sync::Arc<super::super::host_work::HostWork>,
    entries: &'a [PollEntry],
    timeout: &super::tcp::GuestInt,
) -> Result<Vec<&'a PollEntry>, String> {
    let timeout = timeout.value.ok_or_else(|| {
        format!(
            "Wait.poll: timeoutMs {} exceeds the poll limit",
            timeout.display
        )
    })?;
    if timeout < 0 {
        return Err(format!("Wait.poll: timeoutMs {timeout} is negative"));
    }
    let deadline = aver_rt::provider::wait_deadline(std::time::Instant::now(), timeout);
    let mut sockets: Vec<_> = entries
        .iter()
        .filter(|entry| entry.socket.is_some())
        .collect();
    sockets.sort_by(|left, right| left.provider_order.cmp(&right.provider_order));
    let handles: Vec<_> = sockets
        .iter()
        .filter_map(|entry| entry.socket.clone())
        .collect();
    let job_entries: Vec<_> = entries
        .iter()
        .filter(|entry| entry.job_id.is_some())
        .collect();
    let jobs: Vec<_> = job_entries
        .iter()
        .map(|entry| entry.job_id.and_then(|id| work.job(id, None).ok()))
        .collect();
    let found = aver_rt::provider::wait_ready(&handles, &jobs, deadline)?;
    let mut ready: Vec<&PollEntry> = found
        .sockets
        .iter()
        .filter_map(|position| sockets.get(*position).copied())
        .chain(
            found
                .jobs
                .iter()
                .filter_map(|position| job_entries.get(*position).copied()),
        )
        .collect();
    ready.sort_by_key(|entry| entry.order);
    ready.dedup_by_key(|entry| entry.order);
    Ok(ready)
}
