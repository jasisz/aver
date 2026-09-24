//! The two stdlib capabilities the job engine answers directly.
//!
//! `Work.cancel` stops one job. `Wait.poll` is the one wait of a turn: it
//! watches sockets through the Tcp reactor and jobs through the engine, and
//! returns as soon as either is ready or the timeout elapses. Neither needs
//! to know which job kind started a job: the handle carries its engine.
//!
//! One program has one job engine — every job kind of a program is bound to
//! the same one — so `Wait.poll` sleeps on the engine of the first job in
//! its wait set and that is the engine of all of them. A debug assertion
//! holds the build to it.

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
) -> Result<Vec<(ProviderValue, WaitItem)>, ProviderFault> {
    let ProviderValue::Map(entries) = value else {
        return Err(ProviderFault::new(
            "invalid_arguments",
            format!("{operation} expects a Map<K, Wait.Item>"),
        ));
    };
    let mut items = Vec::with_capacity(entries.len());
    for (key, item) in entries {
        // The key is whatever the caller keyed its map by. This provider
        // correlates it and orders it and never reads it, so the only key it
        // refuses is one whose identity it could not tell apart from another:
        // a capability resource has no observable identity, so readiness for
        // one would be delivered as readiness for the other.
        if matches!(key, ProviderValue::Resource(_)) {
            return Err(ProviderFault::new(
                "invalid_arguments",
                format!("{operation} cannot key a wait set by a capability resource"),
            ));
        }
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

/// What one wait found ready, as positions into the sockets and the jobs it
/// was handed.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct WaitReady {
    pub sockets: Vec<usize>,
    pub jobs: Vec<usize>,
}

/// The longest one sleep of a wait lasts once the program watches for a stop
/// request. A wait longer than this sleeps in slices and looks at the stop
/// flag between them, so a stop request ends it within this long rather than
/// at its deadline.
const STOP_SLICE_MS: i64 = 100;

/// How long the next sleep of a wait may last: what is left of the wait,
/// rounded up to a whole millisecond so a wait never spins on its last
/// fraction of one, and at most [`STOP_SLICE_MS`] while a stop request is
/// something this process can notice.
fn next_sleep_ms(deadline: Instant) -> i64 {
    let now = Instant::now();
    if now >= deadline {
        return 0;
    }
    let left = deadline.duration_since(now).as_micros().div_ceil(1000);
    let left = i64::try_from(left).unwrap_or(i64::MAX);
    if super::process::stop_watch_installed() {
        left.min(STOP_SLICE_MS)
    } else {
        left
    }
}

/// The one wait of a turn, over sockets and jobs, shared by every native
/// backend: the VM, a generated Rust artifact and the Wasmtime host.
///
/// It returns as soon as something in the set is ready, once `deadline`
/// passes, or once a stop request has arrived in a process that watches for
/// one. A job that is `None` is a handle the host no longer knows, which is
/// reported ready: false readiness is allowed and `take` gives the real
/// answer.
///
/// A settling job rings every wait blocked on its engine, including a job
/// outside this set. That is a wake, not an answer: the wait looks at its
/// whole set again, sockets included, with a fresh poller, and goes back to
/// sleep while nothing in it is ready. Returning on that wake, or sleeping
/// the rest of the timeout on the engine alone, would leave a socket that
/// becomes ready in the meantime unreported.
pub fn wait_ready(
    sockets: &[crate::tcp::TcpSocket],
    jobs: &[Option<Job>],
    deadline: Instant,
) -> Result<WaitReady, String> {
    // One program has one job engine, so every job in a wait set belongs to
    // the same one and the first handle's engine is the engine of the whole
    // set. The debug assertion is where that would be noticed if a build
    // ever gave a program two.
    let engine = jobs.iter().flatten().next().map(|job| job.engine().clone());
    debug_assert!(
        engine.as_ref().is_none_or(|engine| jobs
            .iter()
            .flatten()
            .all(|job| std::sync::Arc::ptr_eq(job.engine(), engine))),
        "Wait.poll received jobs from more than one engine"
    );
    let ready_jobs = || -> Vec<usize> {
        jobs.iter()
            .enumerate()
            .filter(|(_, job)| job.as_ref().is_none_or(Job::is_ready))
            .map(|(position, _)| position)
            .collect()
    };
    loop {
        // Read the settle generation and arm the waker before deciding
        // nothing is ready, so a job that finishes between the two wakes the
        // sleep instead of being slept through. The waker is fresh for every
        // attempt because the reactor registers each socket once per poller.
        let wake = if sockets.is_empty() {
            JobWake::default()
        } else {
            arm_job_wake(engine.as_ref())?
        };
        let generation = engine.as_ref().map(|engine| engine.generation());
        let mut ready = WaitReady {
            sockets: Vec::new(),
            jobs: ready_jobs(),
        };
        let sleep_ms = next_sleep_ms(deadline);
        if !sockets.is_empty() {
            let timeout = if ready.jobs.is_empty() { sleep_ms } else { 0 };
            ready.sockets = poll_sockets_beside_jobs(sockets, timeout, &wake)?;
            if !jobs.is_empty() && ready.jobs.is_empty() {
                // A job may have settled while the reactor slept.
                ready.jobs = ready_jobs();
            }
        }
        if !ready.sockets.is_empty()
            || !ready.jobs.is_empty()
            || Instant::now() >= deadline
            || super::process::stop_watch_fired()
        {
            return Ok(ready);
        }
        if sockets.is_empty() {
            let until = Instant::now()
                .checked_add(Duration::from_millis(sleep_ms.max(0) as u64))
                .map_or(deadline, |until| until.min(deadline));
            match (engine.as_ref(), generation) {
                (Some(engine), Some(generation)) => engine.wait_until(generation, until),
                _ => {
                    let now = Instant::now();
                    if until > now {
                        std::thread::sleep(until - now);
                    }
                }
            }
        }
    }
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
                    "{operation} expects (Map<K, Wait.Item> items, Int timeoutMs), got {} argument(s)",
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
        let mut job_keys = Vec::new();
        let mut jobs = Vec::new();
        for (key, item) in items {
            match item {
                WaitItem::Socket(socket) => {
                    socket_keys.push(key);
                    sockets.push(socket);
                }
                WaitItem::Job(handle) => {
                    job_keys.push(key);
                    jobs.push(Some(handle));
                }
            }
        }
        // The timeout is the upper bound on this one wait, so the deadline is
        // taken once, before anything sleeps, and every sleep stops at it.
        let deadline = wait_deadline(Instant::now(), timeout_ms);
        let found = match wait_ready(&sockets, &jobs, deadline) {
            Ok(found) => found,
            Err(message) => return Ok(err(message)),
        };
        let mut ready: Vec<ProviderValue> = found
            .sockets
            .iter()
            .filter_map(|position| socket_keys.get(*position).cloned())
            .chain(
                found
                    .jobs
                    .iter()
                    .filter_map(|position| job_keys.get(*position).cloned()),
            )
            .collect();
        // The contract answers in the order the caller's own map puts its
        // keys in, whatever that map is keyed by.
        ready.sort_by(super::compare_map_keys);
        ready.dedup_by(|left, right| {
            super::compare_map_keys(left, right) == std::cmp::Ordering::Equal
        });
        Ok(ProviderValue::ResultOk(Box::new(ProviderValue::List(
            ready,
        ))))
    }
}

/// The longest wait this provider names a deadline for. A timeout above it
/// is a wait with no deadline within reach — a century outlives every
/// process that could be waiting — and capping it is what keeps a timeout
/// the contract admits, up to `i64::MAX` milliseconds, from overflowing the
/// host's own clock.
pub const MAX_WAIT_MS: i64 = 1000 * 60 * 60 * 24 * 365 * 100;

/// The instant one wait stops at: `now` plus its timeout, capped at
/// [`MAX_WAIT_MS`] and saturating at whatever this host's clock can hold.
pub fn wait_deadline(now: Instant, timeout_ms: i64) -> Instant {
    let capped = timeout_ms.clamp(0, MAX_WAIT_MS);
    now.checked_add(Duration::from_millis(capped as u64))
        .unwrap_or(now)
}

/// What one job of a generated artifact runs: the compiled Aver function the
/// manifest bound to the job kind, wrapped so the engine sees only values.
///
/// A function pointer rather than a boxed closure because the generated crate
/// has exactly one such wrapper per job kind and nothing to capture: the task
/// and the result cross as [`ProviderValue`], and the codecs that convert them
/// are the crate's own.
pub type WorkKindBody = fn(ProviderValue) -> Result<ProviderValue, String>;

pub const WORK_KIND_NATIVE_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

/// One job kind of a generated artifact, bound to one pure function of the
/// program compiled beside it.
///
/// The bytecode VM answers a job kind with a child VM over the same program
/// (`src/provider/work.rs` in the compiler); a generated artifact has the
/// function itself, so the seam is this much smaller. The answers are the
/// same, deliberately: `Ok(None)` while the job is queued or runs,
/// `Ok(Some(r))` once, `Err("work: job already taken")`,
/// `Err("work: job cancelled")`, and `Err("work: unknown job")` once the
/// engine has forgotten the slot. `begin` never refuses at the job limit: the
/// engine queues the job instead.
pub struct WorkKindProvider {
    capability: String,
    identity: String,
    engine: std::sync::Arc<crate::work::JobEngine>,
    body: WorkKindBody,
    /// This job kind's owner tag in the engine, which is how `take` tells a
    /// handle this kind began from one another kind began. The engine keeps
    /// the tag in the job's own slot, so it is bounded exactly as the slots
    /// are and nothing here grows with the number of jobs.
    owner: u64,
}

impl WorkKindProvider {
    pub fn new(
        capability: &str,
        engine: std::sync::Arc<crate::work::JobEngine>,
        body: WorkKindBody,
    ) -> Self {
        let owner = engine.new_owner();
        Self {
            capability: capability.to_string(),
            identity: format!("aver.work.{capability}/native"),
            engine,
            body,
            owner,
        }
    }

    pub fn engine(&self) -> &std::sync::Arc<crate::work::JobEngine> {
        &self.engine
    }

    fn begin(&self, task: ProviderValue) -> ProviderValue {
        let body = self.body;
        match self
            .engine
            .begin_owned(self.owner, Box::new(move |_cancel| body(task)))
        {
            Ok(job) => {
                ProviderValue::ResultOk(Box::new(ProviderValue::Resource(job.into_resource())))
            }
            Err(message) => ProviderValue::ResultErr(Box::new(ProviderValue::String(message))),
        }
    }

    /// What `take` answers for a handle: a forgotten slot is unknown
    /// whoever asks, and a slot another kind began is refused by name.
    fn take(&self, job: &Job) -> ProviderValue {
        match job.engine().owner(job.id()) {
            None => err("work: unknown job".to_string()),
            Some(owner) if owner != self.owner => err(format!(
                "work: this job was not started by job kind '{}'",
                self.capability
            )),
            Some(_) => Self::answer(job.take()),
        }
    }

    fn answer(outcome: Result<Option<ProviderValue>, String>) -> ProviderValue {
        match outcome {
            Ok(Some(value)) => {
                ProviderValue::ResultOk(Box::new(ProviderValue::OptionSome(Box::new(value))))
            }
            Ok(None) => ProviderValue::ResultOk(Box::new(ProviderValue::OptionNone)),
            Err(message) => ProviderValue::ResultErr(Box::new(ProviderValue::String(message))),
        }
    }
}

impl CapabilityProvider for WorkKindProvider {
    fn identity(&self) -> &str {
        &self.identity
    }

    fn fingerprint(&self) -> &str {
        WORK_KIND_NATIVE_FINGERPRINT
    }

    fn invoke_owned(
        &self,
        context: &ProviderContext,
        mut args: Vec<ProviderValue>,
    ) -> Result<ProviderValue, ProviderFault> {
        if context.operation.rsplit_once('.').map(|(_, name)| name) == Some("begin")
            && args.len() == 1
        {
            return Ok(self.begin(args.pop().expect("one argument checked above")));
        }
        self.invoke(context, &args)
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        let operation = context.operation.as_str();
        let [single] = args else {
            return Err(ProviderFault::new(
                "invalid_arguments",
                format!(
                    "{operation} expects exactly one argument, got {}",
                    args.len()
                ),
            ));
        };
        match operation.rsplit_once('.').map(|(_, name)| name) {
            Some("begin") => Ok(self.begin(single.clone())),
            Some("take") => Ok(self.take(job(operation, single)?)),
            _ => Err(ProviderFault::new(
                "unknown_operation",
                format!(
                    "job kind '{}' does not implement '{operation}'",
                    self.capability
                ),
            )),
        }
    }
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

    #[test]
    fn owned_work_dispatch_transfers_the_task_allocation() {
        use crate::provider::{NativeProviderRegistry, ProviderBinding, ProviderContractSpec};
        fn same_allocation(task: ProviderValue) -> Result<ProviderValue, String> {
            let ProviderValue::Tuple(fields) = task else {
                panic!("tuple task")
            };
            let [
                ProviderValue::String(payload),
                ProviderValue::String(address),
            ] = fields.as_slice()
            else {
                panic!("payload and its original address")
            };
            Ok(ProviderValue::Bool(
                format!("{:p}", payload.as_ptr()) == *address,
            ))
        }
        let mut registry = NativeProviderRegistry::new([ProviderContractSpec::new(
            "CopyProbe",
            "contract",
            "model",
            vec!["CopyProbe.begin", "CopyProbe.take"],
        )])
        .unwrap();
        registry
            .bind(ProviderBinding::new(
                "CopyProbe",
                "contract",
                vec!["CopyProbe.begin", "CopyProbe.take"],
                Arc::new(WorkKindProvider::new(
                    "CopyProbe",
                    JobEngine::new(1),
                    same_allocation,
                )),
            ))
            .unwrap();
        let payload = "nonempty task data".repeat(1024);
        let address = format!("{:p}", payload.as_ptr());
        let answer = registry
            .invoke_owned(
                "CopyProbe.begin",
                vec![ProviderValue::Tuple(vec![
                    ProviderValue::String(payload),
                    ProviderValue::String(address),
                ])],
            )
            .unwrap();
        let ProviderValue::ResultOk(handle) = answer else {
            panic!("begin succeeded")
        };
        let ProviderValue::Resource(resource) = &*handle else {
            panic!("job resource")
        };
        let job = resource.downcast_ref::<Job>().unwrap();
        poll(job, 5000);
        let answer = registry
            .invoke_owned("CopyProbe.take", vec![*handle])
            .unwrap();
        assert!(matches!(answer, ProviderValue::ResultOk(value)
            if matches!(&*value, ProviderValue::OptionSome(result)
                if matches!(&**result, ProviderValue::Bool(true)))));
        // A consumed-buffer call must still use the normal arity diagnostics.
        assert!(
            registry
                .invoke_owned("CopyProbe.begin", vec![])
                .unwrap_err()
                .contains("expects exactly one argument")
        );
    }

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

    /// The largest timeout the contract admits is larger than any instant
    /// this host can name. Adding it unchecked panics; the wait must answer.
    #[test]
    fn the_largest_admitted_timeout_does_not_panic() {
        let engine = JobEngine::new(2);
        let job = engine
            .begin(Box::new(|_| {
                Ok(ProviderValue::Int(crate::AverInt::from_i64(4)))
            }))
            .expect("begin");
        // The job settles at once, so the wait answers without ever reaching
        // its deadline; what is under test is that taking that deadline is
        // not a panic.
        let answer = poll(&job, i64::MAX);
        assert!(
            matches!(&answer, ProviderValue::ResultOk(inner) if matches!(&**inner, ProviderValue::List(keys) if keys.len() == 1)),
            "the wait answered {answer:?}"
        );

        // The deadline such a timeout names is the cap, not the number: past
        // the cap the wait has no deadline within reach, and the host's own
        // clock is never asked to hold an instant it cannot.
        let now = Instant::now();
        assert_eq!(
            wait_deadline(now, i64::MAX),
            wait_deadline(now, MAX_WAIT_MS)
        );
        assert_eq!(wait_deadline(now, 0), now);
        assert!(wait_deadline(now, i64::MAX) > now + Duration::from_secs(60 * 60 * 24 * 365));
    }

    /// The engine's settle generation is per engine, so a job outside the
    /// wait set wakes a job-only wait. Returning on that wake answers `[]`
    /// while the set's own job is still running and the timeout is far from
    /// spent: the wait has to look again and go back to sleep.
    #[test]
    fn a_job_outside_the_wait_set_does_not_end_the_wait() {
        let engine = JobEngine::new(4);
        let release = Arc::new(AtomicBool::new(false));
        let held = release.clone();
        // The job the wait is over: slow, and released by hand.
        let slow = engine
            .begin(Box::new(move |cancel| {
                while !held.load(Ordering::Relaxed) && !cancel.load(Ordering::Relaxed) {
                    std::thread::yield_now();
                }
                Ok(ProviderValue::Int(crate::AverInt::from_i64(1)))
            }))
            .expect("the slow job starts");
        // A job nobody is waiting for, which settles first and wakes the
        // engine while the wait sleeps.
        let _fast = engine
            .begin(Box::new(|_| {
                std::thread::sleep(Duration::from_millis(50));
                Ok(ProviderValue::Int(crate::AverInt::from_i64(2)))
            }))
            .expect("the fast job starts");

        let started = Instant::now();
        let waiter = {
            let slow = slow.clone();
            std::thread::spawn(move || {
                let answer = StandardWaitProvider
                    .invoke(
                        &context(),
                        &[
                            wait_set(&slow),
                            ProviderValue::Int(crate::AverInt::from_i64(3000)),
                        ],
                    )
                    .expect("the wait answers");
                (answer, started.elapsed())
            })
        };
        // Well after the fast job settled, release the one the wait is over.
        std::thread::sleep(Duration::from_millis(400));
        release.store(true, Ordering::Relaxed);
        let (answer, elapsed) = waiter.join().expect("the wait thread finishes");

        assert!(
            matches!(&answer, ProviderValue::ResultOk(inner) if matches!(&**inner, ProviderValue::List(keys) if keys.len() == 1)),
            "the wait answered {answer:?} rather than its own ready job"
        );
        assert!(
            elapsed >= Duration::from_millis(300),
            "the wait returned after {elapsed:?}, before the job it was over settled"
        );
        let _ = slow;
    }

    /// A mixed wait woken by a job outside its set keeps watching its
    /// sockets. The foreign job rings the engine's waker, which ends the
    /// socket poll early; the wait must poll its sockets again rather than
    /// sleep the rest of its timeout on the engine alone and miss the bytes
    /// that arrive meanwhile.
    #[cfg(not(target_family = "wasm"))]
    #[test]
    fn a_wake_from_outside_the_set_does_not_deafen_the_sockets() {
        use std::io::Write;
        let listener = std::net::TcpListener::bind(("127.0.0.1", 0)).expect("bind loopback");
        let port = listener.local_addr().expect("address").port();
        let server = std::thread::spawn(move || {
            let (mut stream, _) = listener.accept().expect("accept");
            std::thread::sleep(Duration::from_millis(600));
            stream.write_all(b"ping\n").expect("write");
            std::thread::sleep(Duration::from_millis(2000));
        });
        let connection = crate::tcp::connect("127.0.0.1", i64::from(port)).expect("connect");

        let engine = JobEngine::new(4);
        let release = Arc::new(AtomicBool::new(false));
        let held = release.clone();
        let slow = engine
            .begin(Box::new(move |cancel| {
                while !held.load(Ordering::Relaxed) && !cancel.load(Ordering::Relaxed) {
                    std::thread::sleep(Duration::from_millis(5));
                }
                Ok(ProviderValue::Int(crate::AverInt::from_i64(1)))
            }))
            .expect("the slow job starts");
        // Settles while the wait sleeps on its socket, and is in no wait set.
        let _foreign = engine
            .begin(Box::new(|_| {
                std::thread::sleep(Duration::from_millis(100));
                Ok(ProviderValue::Int(crate::AverInt::from_i64(2)))
            }))
            .expect("the foreign job starts");

        let started = Instant::now();
        let found = wait_ready(
            &[crate::tcp::TcpSocket::Connected(connection.clone())],
            &[Some(slow.clone())],
            started + Duration::from_secs(5),
        )
        .expect("the wait answers");
        let elapsed = started.elapsed();
        release.store(true, Ordering::Relaxed);

        assert_eq!(
            found.sockets,
            vec![0],
            "the socket that became readable was not reported"
        );
        assert!(found.jobs.is_empty(), "the slow job was reported ready");
        assert!(
            elapsed < Duration::from_millis(2500),
            "the socket that became readable after 600ms was reported after {elapsed:?}"
        );
        let _ = crate::tcp::close(&connection);
        server.join().expect("server thread");
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
