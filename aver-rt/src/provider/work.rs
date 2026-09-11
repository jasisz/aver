//! The two stdlib capabilities the job engine answers directly.
//!
//! `Work.cancel` stops one job. `Wait.poll` is the one wait of a turn: it
//! watches sockets through the Tcp reactor and jobs through the engine, and
//! returns as soon as either is ready or the timeout elapses. Neither needs
//! to know which job kind started a job: the handle carries its engine.

use std::time::{Duration, Instant};

use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderValue};
use crate::work::Job;

pub const STANDARD_WORK_NATIVE_IDENTITY: &str = "aver.standard.Work/native";
pub const STANDARD_WORK_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));
pub const STANDARD_WAIT_NATIVE_IDENTITY: &str = "aver.standard.Wait/native";
pub const STANDARD_WAIT_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

/// Standard native provider for `Work`: the handle's own operations.
#[derive(Debug, Clone, Copy, Default)]
pub struct StandardWorkProvider;

/// Standard native provider for `Wait`: the one wait of a turn.
#[derive(Debug, Clone, Copy, Default)]
pub struct StandardWaitProvider;

fn job<'a>(operation: &str, value: &'a ProviderValue) -> Result<&'a Job, ProviderFault> {
    super::tcp::resource::<Job>(operation, value, "Work.Job")
}

impl CapabilityProvider for StandardWorkProvider {
    fn identity(&self) -> &str {
        STANDARD_WORK_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_WORK_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        let operation = context.operation.as_str();
        match operation {
            "Work.cancel" => {
                let [handle] = args else {
                    return Err(ProviderFault::new(
                        "invalid_arguments",
                        format!(
                            "{operation} expects (Work.Job job), got {} argument(s)",
                            args.len()
                        ),
                    ));
                };
                job(operation, handle)?.cancel();
                Ok(ProviderValue::ResultOk(Box::new(ProviderValue::Unit)))
            }
            other => Err(ProviderFault::new(
                "unknown_operation",
                format!("the standard Work provider does not implement '{other}'"),
            )),
        }
    }
}

/// One entry of a wait set, already resolved to what the host watches.
enum WaitItem {
    Socket(crate::tcp::TcpSocket),
    Job(Job),
}

fn wait_items(
    operation: &str,
    value: &ProviderValue,
) -> Result<Vec<(crate::AverInt, WaitItem)>, ProviderFault> {
    let ProviderValue::Map(entries) = value else {
        return Err(ProviderFault::new(
            "invalid_arguments",
            format!("{operation} expects a Map<Int, Wait.Item>"),
        ));
    };
    let mut items = Vec::with_capacity(entries.len());
    for (key, item) in entries {
        let ProviderValue::Int(key) = key else {
            return Err(ProviderFault::new(
                "invalid_arguments",
                format!("{operation} expects Int map keys"),
            ));
        };
        let ProviderValue::Variant {
            type_name,
            variant,
            fields,
        } = item
        else {
            return Err(ProviderFault::new(
                "invalid_arguments",
                format!("{operation} expects Wait.Item map values"),
            ));
        };
        if type_name != "Wait.Item" || fields.len() != 1 {
            return Err(ProviderFault::new(
                "invalid_arguments",
                format!(
                    "{operation} expects one-field Wait.Item variants, got {type_name}.{variant} with {} field(s)",
                    fields.len()
                ),
            ));
        }
        let resolved = match variant.as_str() {
            "Socket" => WaitItem::Socket(super::tcp::tcp_socket(operation, &fields[0])?),
            "Job" => WaitItem::Job(job(operation, &fields[0])?.clone()),
            other => {
                return Err(ProviderFault::new(
                    "invalid_arguments",
                    format!("{operation} received unknown Wait.Item variant '{other}'"),
                ));
            }
        };
        items.push((key.clone(), resolved));
    }
    Ok(items)
}

/// The waker a settling job rings while this wait sleeps on sockets.
///
/// It is armed before the wait looks at its jobs, so a job that settles during
/// that look already has somewhere to ring: arming it afterwards leaves a
/// window in which a finished job rings nothing and the wait sleeps out its
/// whole timeout with an answer waiting for it.
#[cfg(not(target_family = "wasm"))]
#[derive(Default)]
struct JobWake {
    waker: Option<crate::tcp::PollWaker>,
    /// Held for as long as the wait runs; dropping it unregisters the waker.
    _guard: Option<crate::work::WakerGuard>,
}

#[cfg(not(target_family = "wasm"))]
fn arm_job_wake(
    engine: Option<&std::sync::Arc<crate::work::JobEngine>>,
) -> Result<JobWake, String> {
    let Some(engine) = engine else {
        return Ok(JobWake::default());
    };
    let waker = crate::tcp::PollWaker::new("Wait.poll")?;
    let guard = engine.wake_with(std::sync::Arc::new(waker.clone()));
    Ok(JobWake {
        waker: Some(waker),
        _guard: Some(guard),
    })
}

/// Sleep on the sockets of the wait set, returning the ready positions.
#[cfg(not(target_family = "wasm"))]
fn poll_sockets_beside_jobs(
    sockets: &[crate::tcp::TcpSocket],
    timeout_ms: i64,
    wake: &JobWake,
) -> Result<Vec<usize>, String> {
    match &wake.waker {
        Some(waker) => crate::tcp::poll_with_waker(sockets, timeout_ms, waker, "Wait.poll"),
        None => crate::tcp::poll(sockets, timeout_ms),
    }
}

#[cfg(target_family = "wasm")]
#[derive(Default)]
struct JobWake;

#[cfg(target_family = "wasm")]
fn arm_job_wake(
    _engine: Option<&std::sync::Arc<crate::work::JobEngine>>,
) -> Result<JobWake, String> {
    Ok(JobWake)
}

#[cfg(target_family = "wasm")]
fn poll_sockets_beside_jobs(
    sockets: &[crate::tcp::TcpSocket],
    timeout_ms: i64,
    _wake: &JobWake,
) -> Result<Vec<usize>, String> {
    crate::tcp::poll(sockets, timeout_ms)
}

impl CapabilityProvider for StandardWaitProvider {
    fn identity(&self) -> &str {
        STANDARD_WAIT_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_WAIT_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        let operation = context.operation.as_str();
        if operation != "Wait.poll" {
            return Err(ProviderFault::new(
                "unknown_operation",
                format!("the standard Wait provider does not implement '{operation}'"),
            ));
        }
        let [items_value, ProviderValue::Int(timeout_ms)] = args else {
            return Err(ProviderFault::new(
                "invalid_arguments",
                format!(
                    "{operation} expects (Map<Int, Wait.Item> items, Int timeoutMs), got {} argument(s)",
                    args.len()
                ),
            ));
        };
        let Some(timeout_ms) = timeout_ms.to_i64() else {
            return Ok(err(format!(
                "Wait.poll: timeoutMs {timeout_ms} exceeds the poll limit"
            )));
        };
        if timeout_ms < 0 {
            return Ok(err(format!(
                "Wait.poll: timeoutMs {timeout_ms} is negative"
            )));
        }
        let items = wait_items(operation, items_value)?;

        let mut socket_keys = Vec::new();
        let mut sockets = Vec::new();
        let mut jobs = Vec::new();
        for (key, item) in items {
            match item {
                WaitItem::Socket(socket) => {
                    socket_keys.push(key);
                    sockets.push(socket);
                }
                WaitItem::Job(handle) => jobs.push((key, handle)),
            }
        }
        let engine = jobs.first().map(|(_, handle)| handle.engine().clone());
        // The timeout is the upper bound on this one wait, so the deadline is
        // taken once, before anything sleeps, and every sleep below stops at
        // it. Sleeping the full timeout twice — once on the sockets, once on
        // the engine — would make the wait take twice as long as it promised.
        let deadline = Instant::now() + Duration::from_millis(timeout_ms as u64);
        // Read the settle generation and arm the waker before deciding nothing
        // is ready, so a job that finishes between the two never sleeps out
        // the timeout.
        let generation = engine.as_ref().map(|engine| engine.generation());
        let wake = match arm_job_wake(engine.as_ref()) {
            Ok(wake) => wake,
            Err(message) => return Ok(err(message)),
        };

        let mut ready = Vec::new();
        for (key, handle) in &jobs {
            if handle.is_ready() {
                ready.push(key.clone());
            }
        }

        if !sockets.is_empty() {
            let socket_timeout = if ready.is_empty() {
                remaining_ms(deadline)
            } else {
                0
            };
            match poll_sockets_beside_jobs(&sockets, socket_timeout, &wake) {
                Ok(positions) => {
                    for position in positions {
                        if let Some(key) = socket_keys.get(position) {
                            ready.push(key.clone());
                        }
                    }
                }
                Err(message) => return Ok(err(message)),
            }
        }

        if ready.is_empty() {
            if let (Some(engine), Some(generation)) = (engine.as_ref(), generation) {
                // A job-only wait has no socket to sleep on; the engine's own
                // signal is the wait. A wait that already slept on its sockets
                // finds the deadline passed and returns at once.
                engine.wait_until(generation, deadline);
            } else if sockets.is_empty() {
                // Neither socket nor job: the wait is the timeout itself.
                let left = remaining_ms(deadline);
                if left > 0 {
                    std::thread::sleep(Duration::from_millis(left as u64));
                }
            }
        }
        for (key, handle) in &jobs {
            if handle.is_ready() {
                ready.push(key.clone());
            }
        }

        ready.sort();
        ready.dedup();
        Ok(ProviderValue::ResultOk(Box::new(ProviderValue::List(
            ready.into_iter().map(ProviderValue::Int).collect(),
        ))))
    }
}

/// How much of this wait's timeout is left, in milliseconds.
fn remaining_ms(deadline: Instant) -> i64 {
    let now = Instant::now();
    if now >= deadline {
        return 0;
    }
    i64::try_from(deadline.duration_since(now).as_millis()).unwrap_or(i64::MAX)
}

fn err(message: String) -> ProviderValue {
    ProviderValue::ResultErr(Box::new(ProviderValue::String(message)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::work::JobEngine;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicBool, Ordering};

    fn context() -> ProviderContext {
        ProviderContext {
            capability: "Wait".to_string(),
            operation: "Wait.poll".to_string(),
            contract_hash: "test-contract".to_string(),
            model_hash: "test-model".to_string(),
        }
    }

    fn wait_set(job: &Job) -> ProviderValue {
        ProviderValue::Map(vec![(
            ProviderValue::Int(crate::AverInt::from_i64(1)),
            ProviderValue::Variant {
                type_name: "Wait.Item".to_string(),
                variant: "Job".to_string(),
                fields: vec![ProviderValue::Resource(job.clone().into_resource())],
            },
        )])
    }

    fn poll(job: &Job, timeout_ms: i64) -> ProviderValue {
        StandardWaitProvider
            .invoke(
                &context(),
                &[
                    wait_set(job),
                    ProviderValue::Int(crate::AverInt::from_i64(timeout_ms)),
                ],
            )
            .expect("the wait answers")
    }

    #[test]
    fn a_job_only_wait_sleeps_its_timeout_once() {
        let engine = JobEngine::new(2);
        let release = Arc::new(AtomicBool::new(false));
        let held = release.clone();
        let job = engine
            .begin(Box::new(move |cancel| {
                while !held.load(Ordering::Relaxed) && !cancel.load(Ordering::Relaxed) {
                    std::thread::yield_now();
                }
                Ok(ProviderValue::Int(crate::AverInt::from_i64(1)))
            }))
            .expect("begin");

        let started = Instant::now();
        let answer = poll(&job, 300);
        let elapsed = started.elapsed();
        release.store(true, Ordering::Relaxed);

        assert!(
            matches!(&answer, ProviderValue::ResultOk(inner) if matches!(&**inner, ProviderValue::List(keys) if keys.is_empty()))
        );
        // The timeout is the upper bound on the whole wait, so sleeping it
        // twice — once on the sockets, once on the engine — is the failure
        // this guards against. The generous margin keeps a slow machine from
        // failing a test about a doubled sleep.
        assert!(
            elapsed < Duration::from_millis(600),
            "a 300ms job-only wait took {elapsed:?}"
        );
    }

    #[test]
    fn a_settling_job_ends_the_wait_early() {
        let engine = JobEngine::new(2);
        let job = engine
            .begin(Box::new(|_| {
                std::thread::sleep(Duration::from_millis(50));
                Ok(ProviderValue::Int(crate::AverInt::from_i64(2)))
            }))
            .expect("begin");

        let started = Instant::now();
        let answer = poll(&job, 5000);
        let elapsed = started.elapsed();

        assert!(
            matches!(&answer, ProviderValue::ResultOk(inner) if matches!(&**inner, ProviderValue::List(keys) if keys.len() == 1))
        );
        assert!(
            elapsed < Duration::from_millis(2500),
            "a job that settled after 50ms left the wait sleeping for {elapsed:?}"
        );
    }
}
