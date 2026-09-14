//! Work cancellation and the shared socket/job wait on wasm-gc.
//!
//! Versioned Work modules use the host scheduler; legacy inline modules keep
//! their original recording hooks and immediate job readiness.

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
    ready.sort_by(|left, right| left.numeric.cmp(&right.numeric));
    ready.dedup_by(|left, right| left.numeric == right.numeric);
    Ok(Ok(ready))
}

/// Register the completion waker before inspecting jobs: a completion between
/// inspection and socket polling must wake the poll instead of being lost.
fn wait_host<'a>(
    work: &std::sync::Arc<super::super::host_work::HostWork>,
    entries: &'a [PollEntry],
    timeout: &super::tcp::GuestInt,
) -> Result<Vec<&'a PollEntry>, String> {
    use std::time::{Duration, Instant};
    let timeout = timeout.value.ok_or_else(|| {
        format!(
            "Wait.poll: timeoutMs {} exceeds the poll limit",
            timeout.display
        )
    })?;
    if timeout < 0 {
        return Err(format!("Wait.poll: timeoutMs {timeout} is negative"));
    }
    let timeout = timeout.min(1000 * 60 * 60 * 24 * 365 * 100);
    let deadline = Instant::now()
        .checked_add(Duration::from_millis(timeout as u64))
        .ok_or("Wait.poll: timeoutMs exceeds the host clock")?;
    let mut sockets: Vec<_> = entries
        .iter()
        .filter(|entry| entry.socket.is_some())
        .collect();
    sockets.sort_by(|left, right| left.provider_order.cmp(&right.provider_order));
    let handles: Vec<_> = sockets
        .iter()
        .filter_map(|entry| entry.socket.clone())
        .collect();
    loop {
        // poll_with_waker registers each socket once on its poller. A wake
        // outside this wait set needs a fresh poller for the next attempt.
        let waker = aver_rt::tcp::PollWaker::new("Wait.poll")?;
        let _guard = work.engine.wake_with(std::sync::Arc::new(waker.clone()));
        let generation = work.engine.generation();
        let mut ready: Vec<_> = entries
            .iter()
            .filter(|entry| {
                entry
                    .job_id
                    .is_some_and(|id| work.job(id, None).map_or(true, |job| job.is_ready()))
            })
            .collect();
        let remaining = deadline.saturating_duration_since(Instant::now());
        if !handles.is_empty() {
            let timeout = if ready.is_empty() {
                remaining.as_millis() as i64
            } else {
                0
            };
            for index in aver_rt::tcp::poll_with_waker(&handles, timeout, &waker, "Wait.poll")? {
                ready.push(sockets[index]);
            }
            // A job may have completed while the reactor was blocked.
            ready.extend(entries.iter().filter(|entry| {
                entry
                    .job_id
                    .is_some_and(|id| work.job(id, None).map_or(true, |job| job.is_ready()))
            }));
        }
        if !ready.is_empty() || Instant::now() >= deadline {
            ready.sort_by(|left, right| left.numeric.cmp(&right.numeric));
            ready.dedup_by(|left, right| left.numeric == right.numeric);
            return Ok(ready);
        }
        if handles.is_empty() {
            work.engine.wait_until(generation, deadline);
        }
    }
}
