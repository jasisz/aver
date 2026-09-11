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
                Ok(ProviderValue::Unit)
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

/// Sleep on sockets and jobs at once, returning the ready socket positions.
///
/// The wait leaves a waker with the engine before it sleeps, so a job that
/// settles ends the wait instead of letting it sit out the whole timeout.
#[cfg(not(target_family = "wasm"))]
fn poll_sockets_beside_jobs(
    sockets: &[crate::tcp::TcpSocket],
    timeout_ms: i64,
    engine: Option<&std::sync::Arc<crate::work::JobEngine>>,
) -> Result<Vec<usize>, String> {
    let Some(engine) = engine else {
        return crate::tcp::poll(sockets, timeout_ms);
    };
    let waker = crate::tcp::PollWaker::new("Wait.poll")?;
    let _guard = engine.wake_with(std::sync::Arc::new(waker.clone()));
    crate::tcp::poll_with_waker(sockets, timeout_ms, &waker, "Wait.poll")
}

#[cfg(target_family = "wasm")]
fn poll_sockets_beside_jobs(
    sockets: &[crate::tcp::TcpSocket],
    timeout_ms: i64,
    _engine: Option<&std::sync::Arc<crate::work::JobEngine>>,
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
        // Read the settle generation before deciding nothing is ready, so a
        // job that finishes between the two never sleeps out the timeout.
        let generation = engine.as_ref().map(|engine| engine.generation());

        let mut ready = Vec::new();
        for (key, handle) in &jobs {
            if handle.is_ready() {
                ready.push(key.clone());
            }
        }

        let socket_timeout = if ready.is_empty() { timeout_ms } else { 0 };
        match poll_sockets_beside_jobs(&sockets, socket_timeout, engine.as_ref()) {
            Ok(positions) => {
                for position in positions {
                    if let Some(key) = socket_keys.get(position) {
                        ready.push(key.clone());
                    }
                }
            }
            Err(message) => return Ok(err(message)),
        }

        if ready.is_empty()
            && sockets.is_empty()
            && let (Some(engine), Some(generation)) = (engine.as_ref(), generation)
        {
            // A job-only wait has no socket to sleep on; the engine's own
            // signal is the wait.
            engine.wait_until(
                generation,
                Instant::now() + Duration::from_millis(timeout_ms as u64),
            );
        }
        if sockets.is_empty() && jobs.is_empty() && timeout_ms > 0 {
            std::thread::sleep(Duration::from_millis(timeout_ms as u64));
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

fn err(message: String) -> ProviderValue {
    ProviderValue::ResultErr(Box::new(ProviderValue::String(message)))
}
