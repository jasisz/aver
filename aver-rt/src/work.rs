//! The job engine: pure work a program starts off the turn.
//!
//! A job is the only resource the concurrency work adds. `begin` hands the
//! engine a body and gets a handle back at once; the body runs on its own
//! thread and writes its answer into the job table; `take` collects it in a
//! later turn; `cancel` stops it at the body's next cancellation check. The
//! engine deliberately knows nothing about Aver: a body is a closure and an
//! answer is a [`ProviderValue`], so the same table serves the bytecode VM,
//! a generated artifact and the Wasmtime host.
//!
//! The limit is how much of the host a program uses, not something the
//! program observes. While fewer than `limit` bodies run, a begun job starts
//! at once; otherwise it waits in the admission queue, in the order it was
//! begun, and starts as soon as a running body stops. `begin` never refuses
//! at the limit, so the same program gives the same answers on a host with
//! one core and on a host with sixty-four, and a recording made on either
//! replays on the other.

use std::collections::{BTreeMap, VecDeque};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::{Duration, Instant};

use crate::provider::{ProviderResource, ProviderValue};

/// What one finished job produced: the bound function's value, or the
/// message explaining why it could not produce one.
pub type JobOutcome = Result<ProviderValue, String>;

/// The work itself. The flag is the job's own cancellation flag: a body that
/// runs long checks it periodically and stops as soon as it is set.
pub type JobBody = Box<dyn FnOnce(Arc<AtomicBool>) -> JobOutcome + Send>;

/// How long a shutting-down engine waits for the jobs it just cancelled.
const SHUTDOWN_GRACE: Duration = Duration::from_millis(250);

/// How many dead slots — collected or cancelled jobs — the table keeps.
///
/// A dead slot is kept so a second `take` can say "already taken" rather
/// than "unknown job", and that answer is worth keeping; keeping it for
/// every job a long-running program ever started is not. Past this many,
/// the oldest dead slot is forgotten and its id answers `work: unknown job`
/// again.
pub const DEAD_SLOT_LIMIT: usize = 4096;

/// Something a settling job should wake. A wait that is already blocked on
/// sockets cannot also block on the engine's condition variable, so it leaves
/// a waker behind and the engine rings it.
pub trait CompletionWaker: Send + Sync {
    fn wake(&self);
}

enum JobState {
    /// Begun, waiting in the admission queue for a running body to stop.
    Queued(JobBody),
    Running,
    Finished(JobOutcome),
    Cancelled,
    Taken,
}

struct JobSlot {
    cancel: Arc<AtomicBool>,
    /// Which job kind began this job; see [`JobEngine::new_owner`].
    owner: u64,
    state: JobState,
}

/// One queued job leaving the admission queue for a thread of its own.
struct Start {
    id: u64,
    cancel: Arc<AtomicBool>,
    body: JobBody,
}

#[derive(Default)]
struct JobTable {
    next_id: u64,
    next_owner: u64,
    jobs: BTreeMap<u64, JobSlot>,
    running: usize,
    /// The queued jobs, in the order they were begun.
    queue: VecDeque<u64>,
    /// Bumped whenever a job settles. A waiter that observed generation `g`
    /// and finds it unchanged knows nothing settled while it slept.
    settled: u64,
    /// The ids of the dead slots, oldest first, bounded by
    /// [`DEAD_SLOT_LIMIT`].
    dead: VecDeque<u64>,
}

impl JobTable {
    /// Record one slot as dead and forget the oldest dead slots past the
    /// bound. Only a slot that has just become dead is passed here, so an
    /// id never appears twice.
    fn retire(&mut self, id: u64) {
        self.dead.push_back(id);
        while self.dead.len() > DEAD_SLOT_LIMIT {
            if let Some(oldest) = self.dead.pop_front() {
                self.jobs.remove(&oldest);
            }
        }
    }

    /// Move queued jobs to running while there is room under `limit`, oldest
    /// first. The caller starts their threads once the table is unlocked.
    fn admit(&mut self, limit: usize) -> Vec<Start> {
        let mut starts = Vec::new();
        while self.running < limit {
            let Some(id) = self.queue.pop_front() else {
                break;
            };
            let Some(slot) = self.jobs.get_mut(&id) else {
                continue;
            };
            if !matches!(slot.state, JobState::Queued(_)) {
                continue;
            }
            let JobState::Queued(body) = std::mem::replace(&mut slot.state, JobState::Running)
            else {
                unreachable!("the state was just matched as queued");
            };
            self.running += 1;
            starts.push(Start {
                id,
                cancel: slot.cancel.clone(),
                body,
            });
        }
        starts
    }
}

/// One program's jobs.
pub struct JobEngine {
    limit: usize,
    table: Mutex<JobTable>,
    settled: Condvar,
    wakers: Mutex<Vec<Arc<dyn CompletionWaker>>>,
}

/// A job handle, as the capability boundary carries it.
///
/// The handle owns a reference to the engine that runs it, so `Work.cancel`
/// and `Wait.poll` reach the right table without any global state: the
/// program's job kinds, its `Work` binding and its `Wait` binding all speak
/// about the engine the handle names.
#[derive(Clone)]
pub struct Job {
    engine: Arc<JobEngine>,
    id: u64,
}

impl Job {
    pub fn id(&self) -> u64 {
        self.id
    }

    pub fn engine(&self) -> &Arc<JobEngine> {
        &self.engine
    }

    /// Collect this job's answer, consuming it.
    pub fn take(&self) -> Result<Option<ProviderValue>, String> {
        self.engine.take(self.id)
    }

    /// Read this job's answer without consuming it. Replay uses it to compare
    /// a recomputed result against the recorded one.
    pub fn peek(&self) -> Result<Option<ProviderValue>, String> {
        self.engine.peek(self.id)
    }

    pub fn cancel(&self) {
        self.engine.cancel(self.id);
    }

    /// Whether a wait may report this job's key ready.
    pub fn is_ready(&self) -> bool {
        self.engine.is_ready(self.id)
    }

    /// Block until this job settles or `deadline` passes.
    pub fn settle_by(&self, deadline: Instant) {
        while !self.is_ready() && Instant::now() < deadline {
            let generation = self.engine.generation();
            if self.is_ready() {
                return;
            }
            self.engine.wait_until(generation, deadline);
        }
    }

    /// Wrap the handle for the capability boundary.
    pub fn into_resource(self) -> ProviderResource {
        ProviderResource::new(self)
    }
}

impl JobEngine {
    /// A new engine running at most `limit` bodies at once. A zero limit is a
    /// configuration error the manifest rejects; treat it as one here too.
    pub fn new(limit: usize) -> Arc<Self> {
        Arc::new(Self {
            limit: limit.max(1),
            table: Mutex::new(JobTable::default()),
            settled: Condvar::new(),
            wakers: Mutex::new(Vec::new()),
        })
    }

    /// The host's own idea of how much work can run at once, used when
    /// `aver.toml` names no limit.
    pub fn default_limit() -> usize {
        std::thread::available_parallelism()
            .map(std::num::NonZeroUsize::get)
            .unwrap_or(1)
    }

    pub fn limit(&self) -> usize {
        self.limit
    }

    /// A fresh owner tag for one job kind.
    ///
    /// Every job kind of a program shares one engine, and `Work.Job` is one
    /// type, so a handle begun by one kind type-checks as an argument to
    /// another kind's `take`. The engine keeps the tag of the kind that began
    /// each job in the job's own slot, so the answer "not started by this
    /// kind" lives exactly as long as the slot does, and a forgotten slot
    /// answers `work: unknown job` whoever asks.
    pub fn new_owner(&self) -> u64 {
        match self.table.lock() {
            Ok(mut table) => {
                table.next_owner += 1;
                table.next_owner
            }
            Err(_) => 0,
        }
    }

    /// Start `body` off the turn and return its handle at once, under no
    /// particular owner.
    pub fn begin(self: &Arc<Self>, body: JobBody) -> Result<Job, String> {
        self.begin_owned(0, body)
    }

    /// Start `body` off the turn for the job kind tagged `owner`, and return
    /// its handle at once.
    ///
    /// The turn is never blocked and nothing is refused at the limit: a job
    /// begun while `limit` bodies run waits in the admission queue, reads as
    /// not ready, and starts when a running body stops.
    pub fn begin_owned(self: &Arc<Self>, owner: u64, body: JobBody) -> Result<Job, String> {
        let (id, starts) = {
            let mut table = self.lock()?;
            table.next_id += 1;
            let id = table.next_id;
            table.jobs.insert(
                id,
                JobSlot {
                    cancel: Arc::new(AtomicBool::new(false)),
                    owner,
                    state: JobState::Queued(body),
                },
            );
            table.queue.push_back(id);
            (id, table.admit(self.limit))
        };
        self.launch(starts);
        Ok(Job {
            engine: self.clone(),
            id,
        })
    }

    /// Give every admitted job its thread. A thread the host cannot start
    /// settles that job with the reason, which frees its place for the next
    /// queued one.
    fn launch(self: &Arc<Self>, mut starts: Vec<Start>) {
        while let Some(Start { id, cancel, body }) = starts.pop() {
            let engine = Arc::downgrade(self);
            let spawned = std::thread::Builder::new()
                .name(format!("aver-job-{id}"))
                .spawn(move || {
                    // A body that unwinds must still settle its slot:
                    // otherwise the job stays Running for ever, the limit
                    // keeps counting it and every `take` answers `Ok(None)`
                    // with nothing left to produce an answer.
                    let outcome =
                        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                            body(cancel)
                        })) {
                            Ok(outcome) => outcome,
                            Err(_) => Err("work: the job's body stopped unexpectedly".to_string()),
                        };
                    if let Some(engine) = engine.upgrade() {
                        engine.settle(id, outcome);
                    }
                });
            if let Err(error) = spawned {
                starts.extend(self.finish(
                    id,
                    Err(format!("work: the host could not start a job: {error}")),
                ));
            }
        }
    }

    fn settle(self: &Arc<Self>, id: u64, outcome: JobOutcome) {
        let starts = self.finish(id, outcome);
        self.launch(starts);
    }

    /// Record that one running body stopped, and admit whatever its place
    /// lets start. A cancelled job keeps its place until its body stops, so
    /// this is also where a cancelled job gives its place back.
    fn finish(&self, id: u64, outcome: JobOutcome) -> Vec<Start> {
        let starts = {
            let Ok(mut table) = self.table.lock() else {
                return Vec::new();
            };
            table.running = table.running.saturating_sub(1);
            table.settled += 1;
            if let Some(slot) = table.jobs.get_mut(&id)
                && matches!(slot.state, JobState::Running)
            {
                slot.state = JobState::Finished(outcome);
            }
            table.admit(self.limit)
        };
        self.settled.notify_all();
        self.ring_wakers();
        starts
    }

    fn ring_wakers(&self) {
        let wakers = match self.wakers.lock() {
            Ok(wakers) => wakers.clone(),
            Err(_) => return,
        };
        for waker in wakers {
            waker.wake();
        }
    }

    fn lock(&self) -> Result<std::sync::MutexGuard<'_, JobTable>, String> {
        self.table
            .lock()
            .map_err(|_| "work: the job table is poisoned".to_string())
    }

    /// Collect a job's answer. `Ok(None)` means it is queued or still
    /// running, and the handle stays usable: a job reported ready that has
    /// not finished is collected in a later turn.
    ///
    /// A job whose slot was already forgotten — more than
    /// [`DEAD_SLOT_LIMIT`] jobs have died since it was collected — answers
    /// `work: unknown job`.
    pub fn take(&self, id: u64) -> Result<Option<ProviderValue>, String> {
        let mut table = self.lock()?;
        let Some(slot) = table.jobs.get_mut(&id) else {
            return Err("work: unknown job".to_string());
        };
        match &slot.state {
            JobState::Queued(_) | JobState::Running => return Ok(None),
            JobState::Cancelled => return Err("work: job cancelled".to_string()),
            JobState::Taken => return Err("work: job already taken".to_string()),
            JobState::Finished(_) => {}
        }
        let JobState::Finished(outcome) = std::mem::replace(&mut slot.state, JobState::Taken)
        else {
            unreachable!("the state was just matched as finished");
        };
        // A job that had finished becomes a dead slot here, so this is where
        // its id joins the bounded tombstone list.
        table.retire(id);
        outcome.map(Some)
    }

    /// The same answer without consuming it.
    pub fn peek(&self, id: u64) -> Result<Option<ProviderValue>, String> {
        let table = self.lock()?;
        let Some(slot) = table.jobs.get(&id) else {
            return Err("work: unknown job".to_string());
        };
        match &slot.state {
            JobState::Queued(_) | JobState::Running => Ok(None),
            JobState::Finished(Ok(value)) => Ok(Some(value.clone())),
            JobState::Finished(Err(message)) => Err(message.clone()),
            JobState::Cancelled => Err("work: job cancelled".to_string()),
            JobState::Taken => Err("work: job already taken".to_string()),
        }
    }

    /// Stop a job. A queued job leaves the queue and never starts. A running
    /// body stops at its next cancellation check and keeps its place under
    /// the limit until it does. A finished job drops its answer, and
    /// cancelling twice changes nothing. A job whose answer was already
    /// collected is left alone: its story is that it was taken, and a later
    /// take must still say so.
    pub fn cancel(&self, id: u64) {
        let Ok(mut table) = self.table.lock() else {
            return;
        };
        let mut retired = false;
        let mut dropped = None;
        if let Some(slot) = table.jobs.get_mut(&id) {
            slot.cancel.store(true, Ordering::Relaxed);
            match slot.state {
                JobState::Taken | JobState::Cancelled => {}
                _ => {
                    retired = true;
                    dropped = Some(std::mem::replace(&mut slot.state, JobState::Cancelled));
                }
            }
            table.settled += 1;
        }
        if matches!(dropped, Some(JobState::Queued(_))) {
            table.queue.retain(|queued| *queued != id);
        }
        if retired {
            table.retire(id);
        }
        drop(table);
        // A queued body holds its task; it is dropped outside the lock.
        drop(dropped);
        self.settled.notify_all();
        self.ring_wakers();
    }

    /// Whether a wait may report this job ready. A job the table no longer
    /// knows counts as ready: false-positive readiness is allowed, and a
    /// caller that acts on it gets the real answer from `take`.
    pub fn is_ready(&self, id: u64) -> bool {
        let Ok(table) = self.table.lock() else {
            return true;
        };
        match table.jobs.get(&id) {
            Some(slot) => !matches!(slot.state, JobState::Queued(_) | JobState::Running),
            None => true,
        }
    }

    /// Whether this job is still waiting in the admission queue.
    pub fn is_queued(&self, id: u64) -> bool {
        self.table.lock().is_ok_and(|table| {
            table
                .jobs
                .get(&id)
                .is_some_and(|slot| matches!(slot.state, JobState::Queued(_)))
        })
    }

    /// Whether the table still holds a slot for this id: a queued or running
    /// job, or a dead one whose tombstone has not been forgotten yet.
    pub fn knows(&self, id: u64) -> bool {
        self.table
            .lock()
            .map(|table| table.jobs.contains_key(&id))
            .unwrap_or(true)
    }

    /// The owner tag of the job kind that began this job, or `None` once the
    /// table has forgotten the slot.
    pub fn owner(&self, id: u64) -> Option<u64> {
        self.table
            .lock()
            .ok()
            .and_then(|table| table.jobs.get(&id).map(|slot| slot.owner))
    }

    /// The current settle generation. Read it before deciding nothing is
    /// ready, and hand it to [`JobEngine::wait_until`] so a job that settles
    /// in between is not slept through.
    pub fn generation(&self) -> u64 {
        self.table.lock().map(|table| table.settled).unwrap_or(0)
    }

    /// Sleep until a job settles or `deadline` passes.
    pub fn wait_until(&self, generation: u64, deadline: Instant) {
        let Ok(mut table) = self.table.lock() else {
            return;
        };
        while table.settled == generation {
            let now = Instant::now();
            if now >= deadline {
                return;
            }
            let (next, timeout) = match self.settled.wait_timeout(table, deadline - now) {
                Ok(pair) => pair,
                Err(_) => return,
            };
            table = next;
            if timeout.timed_out() {
                return;
            }
        }
    }

    /// Leave a waker with the engine for as long as the guard lives.
    pub fn wake_with(self: &Arc<Self>, waker: Arc<dyn CompletionWaker>) -> WakerGuard {
        if let Ok(mut wakers) = self.wakers.lock() {
            wakers.push(waker.clone());
        }
        WakerGuard {
            engine: self.clone(),
            waker,
        }
    }

    /// Cancel every job and wait a bounded moment for the threads to notice.
    /// A queued job never starts. A job that outlives the grace period is
    /// left to the process exit: the runtime promised not to wait for it
    /// beyond a bounded join.
    pub fn shutdown(&self) {
        let mut dropped = Vec::new();
        {
            let Ok(mut table) = self.table.lock() else {
                return;
            };
            table.queue.clear();
            for slot in table.jobs.values_mut() {
                slot.cancel.store(true, Ordering::Relaxed);
                if !matches!(slot.state, JobState::Taken) {
                    dropped.push(std::mem::replace(&mut slot.state, JobState::Cancelled));
                }
            }
            table.settled += 1;
        }
        drop(dropped);
        self.settled.notify_all();
        self.ring_wakers();
        let deadline = Instant::now() + SHUTDOWN_GRACE;
        let Ok(mut table) = self.table.lock() else {
            return;
        };
        while table.running > 0 {
            let now = Instant::now();
            if now >= deadline {
                return;
            }
            let Ok((next, _)) = self.settled.wait_timeout(table, deadline - now) else {
                return;
            };
            table = next;
        }
    }
}

impl Drop for JobEngine {
    fn drop(&mut self) {
        // A worker holds only a `Weak` back to the engine, so reaching Drop
        // with jobs still running is possible. Tell them to stop.
        if let Ok(mut table) = self.table.lock() {
            for slot in table.jobs.values_mut() {
                slot.cancel.store(true, Ordering::Relaxed);
            }
        }
    }
}

/// Removes its waker from the engine when it goes out of scope.
pub struct WakerGuard {
    engine: Arc<JobEngine>,
    waker: Arc<dyn CompletionWaker>,
}

impl Drop for WakerGuard {
    fn drop(&mut self) {
        if let Ok(mut wakers) = self.engine.wakers.lock() {
            let mine = Arc::as_ptr(&self.waker) as *const ();
            wakers.retain(|waker| Arc::as_ptr(waker) as *const () != mine);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn value(n: i64) -> ProviderValue {
        ProviderValue::Int(crate::AverInt::from_i64(n))
    }

    fn settled(engine: &Arc<JobEngine>, id: u64) {
        let deadline = Instant::now() + Duration::from_secs(5);
        while !engine.is_ready(id) && Instant::now() < deadline {
            engine.wait_until(
                engine.generation(),
                Instant::now() + Duration::from_millis(20),
            );
        }
    }

    #[test]
    fn a_finished_job_is_taken_once() {
        let engine = JobEngine::new(4);
        let job = engine.begin(Box::new(|_| Ok(value(7)))).expect("begin");
        settled(&engine, job.id());
        assert!(matches!(job.take(), Ok(Some(ProviderValue::Int(_)))));
        assert_eq!(
            job.take().err(),
            Some("work: job already taken".to_string())
        );
    }

    fn held_job(engine: &Arc<JobEngine>, release: &Arc<AtomicBool>, answer: i64) -> Job {
        let held = release.clone();
        engine
            .begin(Box::new(move |cancel| {
                while !held.load(Ordering::Relaxed) && !cancel.load(Ordering::Relaxed) {
                    std::thread::yield_now();
                }
                Ok(value(answer))
            }))
            .expect("begin never refuses")
    }

    /// At the limit `begin` answers a handle, never a refusal: the job waits
    /// in the admission queue, reads as not ready, and starts once the
    /// running body stops.
    #[test]
    fn the_limit_queues_instead_of_refusing() {
        let engine = JobEngine::new(1);
        let release = Arc::new(AtomicBool::new(false));
        let first = held_job(&engine, &release, 1);
        let second = engine
            .begin(Box::new(|_| Ok(value(2))))
            .expect("a begin at the limit is queued, not refused");
        assert!(engine.is_queued(second.id()));
        assert!(!second.is_ready());
        assert!(matches!(second.take(), Ok(None)));
        release.store(true, Ordering::Relaxed);
        settled(&engine, first.id());
        settled(&engine, second.id());
        assert!(matches!(second.take(), Ok(Some(ProviderValue::Int(_)))));
        assert!(matches!(first.take(), Ok(Some(ProviderValue::Int(_)))));
    }

    /// Queued jobs start in the order they were begun.
    #[test]
    fn queued_jobs_start_in_the_order_they_were_begun() {
        let engine = JobEngine::new(1);
        let release = Arc::new(AtomicBool::new(false));
        let order = Arc::new(Mutex::new(Vec::new()));
        let first = held_job(&engine, &release, 0);
        let jobs: Vec<Job> = (1..=3)
            .map(|n| {
                let order = order.clone();
                engine
                    .begin(Box::new(move |_| {
                        order.lock().expect("order").push(n);
                        Ok(value(n))
                    }))
                    .expect("queued")
            })
            .collect();
        release.store(true, Ordering::Relaxed);
        settled(&engine, first.id());
        for job in &jobs {
            settled(&engine, job.id());
        }
        assert_eq!(*order.lock().expect("order"), vec![1, 2, 3]);
    }

    /// A queued job that is cancelled never starts, and its place in the
    /// queue goes to the next one.
    #[test]
    fn a_cancelled_queued_job_never_starts() {
        let engine = JobEngine::new(1);
        let release = Arc::new(AtomicBool::new(false));
        let ran = Arc::new(AtomicBool::new(false));
        let first = held_job(&engine, &release, 1);
        let marker = ran.clone();
        let queued = engine
            .begin(Box::new(move |_| {
                marker.store(true, Ordering::Relaxed);
                Ok(value(2))
            }))
            .expect("queued");
        let after = engine.begin(Box::new(|_| Ok(value(3)))).expect("queued");
        queued.cancel();
        assert!(queued.is_ready(), "a cancelled job is ready at once");
        release.store(true, Ordering::Relaxed);
        settled(&engine, first.id());
        settled(&engine, after.id());
        assert!(matches!(after.take(), Ok(Some(_))));
        assert!(!ran.load(Ordering::Relaxed), "a cancelled queued job ran");
        assert_eq!(queued.take().err(), Some("work: job cancelled".to_string()));
    }

    /// A running job that is cancelled keeps its place until its body
    /// stops; the stop is what lets the next queued job start.
    #[test]
    fn a_cancelled_running_job_frees_its_place_when_its_body_stops() {
        let engine = JobEngine::new(1);
        let never = Arc::new(AtomicBool::new(false));
        let running = held_job(&engine, &never, 1);
        let queued = engine.begin(Box::new(|_| Ok(value(2)))).expect("queued");
        assert!(engine.is_queued(queued.id()));
        running.cancel();
        settled(&engine, queued.id());
        assert!(matches!(queued.take(), Ok(Some(_))));
    }

    /// Each job kind has its own owner tag, and the tag is forgotten with
    /// the slot.
    #[test]
    fn the_owner_of_a_job_is_its_kind_while_the_slot_lives() {
        let engine = JobEngine::new(2);
        let kind = engine.new_owner();
        let other = engine.new_owner();
        assert_ne!(kind, other);
        let job = engine
            .begin_owned(kind, Box::new(|_| Ok(value(1))))
            .expect("begin");
        assert_eq!(engine.owner(job.id()), Some(kind));
        assert_eq!(engine.owner(job.id() + 1000), None);
    }

    #[test]
    fn a_cancelled_job_reports_cancellation_and_cancels_idempotently() {
        let engine = JobEngine::new(2);
        let job = engine
            .begin(Box::new(|cancel| {
                while !cancel.load(Ordering::Relaxed) {
                    std::thread::yield_now();
                }
                Ok(value(3))
            }))
            .expect("begin");
        job.cancel();
        job.cancel();
        assert!(job.is_ready());
        assert_eq!(job.take().err(), Some("work: job cancelled".to_string()));
    }

    #[test]
    fn cancelling_a_collected_job_keeps_it_collected() {
        let engine = JobEngine::new(2);
        let job = engine.begin(Box::new(|_| Ok(value(5)))).expect("begin");
        settled(&engine, job.id());
        assert!(matches!(job.take(), Ok(Some(ProviderValue::Int(_)))));
        job.cancel();
        assert_eq!(
            job.take().err(),
            Some("work: job already taken".to_string())
        );
    }

    #[test]
    fn a_body_that_stops_unexpectedly_settles_its_slot() {
        let engine = JobEngine::new(1);
        let hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let job = engine
            .begin(Box::new(|_| panic!("the body of a job gave up")))
            .expect("begin");
        settled(&engine, job.id());
        std::panic::set_hook(hook);
        assert!(job.take().is_err());
        // The place the stopped job held is free again, so the limit did not
        // leak it: the next job starts rather than queueing for ever.
        let next = engine.begin(Box::new(|_| Ok(value(1)))).expect("begin");
        settled(&engine, next.id());
        assert!(matches!(next.take(), Ok(Some(_))));
    }

    /// The table answers "already taken" out of a bounded number of dead
    /// slots. Past the bound the oldest is forgotten and its id is unknown
    /// again, so a long-running program's job table does not grow for ever.
    #[test]
    fn the_oldest_dead_slot_is_forgotten_and_the_newest_still_answers() {
        let engine = JobEngine::new(8);
        let collect = |engine: &Arc<JobEngine>| {
            let job = engine.begin(Box::new(|_| Ok(value(1)))).expect("begin");
            settled(engine, job.id());
            assert!(matches!(job.take(), Ok(Some(_))));
            job
        };
        let oldest = collect(&engine);
        assert_eq!(
            oldest.take().err(),
            Some("work: job already taken".to_string()),
            "a just-collected job still says it was taken"
        );
        let mut newest = None;
        for _ in 0..DEAD_SLOT_LIMIT {
            newest = Some(collect(&engine));
        }
        assert_eq!(
            oldest.take().err(),
            Some("work: unknown job".to_string()),
            "the oldest dead slot outlived the bound"
        );
        assert_eq!(
            newest.expect("the last job").take().err(),
            Some("work: job already taken".to_string()),
            "the newest dead slot was forgotten"
        );
    }

    /// Cancelled slots are bounded the same way, and cancelling twice still
    /// adds only one tombstone.
    #[test]
    fn a_cancelled_slot_is_one_tombstone_however_often_it_is_cancelled() {
        let engine = JobEngine::new(4);
        let job = engine.begin(Box::new(|_| Ok(value(2)))).expect("begin");
        settled(&engine, job.id());
        job.cancel();
        job.cancel();
        job.cancel();
        let dead = engine.table.lock().expect("the table").dead.len();
        assert_eq!(dead, 1, "cancelling one job left {dead} tombstones");
    }

    /// What a job kind may forget about an id is what the engine has already
    /// forgotten: a slot still in the table, dead or alive, is one the engine
    /// knows, and only an evicted one is not.
    #[test]
    fn the_engine_knows_a_slot_until_it_evicts_it() {
        let engine = JobEngine::new(8);
        let collect = |engine: &Arc<JobEngine>| {
            let job = engine.begin(Box::new(|_| Ok(value(1)))).expect("begin");
            settled(engine, job.id());
            assert!(matches!(job.take(), Ok(Some(_))));
            job
        };
        let oldest = collect(&engine);
        assert!(engine.knows(oldest.id()), "a dead slot is one it knows");
        for _ in 0..DEAD_SLOT_LIMIT {
            collect(&engine);
        }
        assert!(
            !engine.knows(oldest.id()),
            "an evicted slot is still reported as known"
        );
    }

    #[test]
    fn a_settling_job_wakes_a_waiting_turn() {
        let engine = JobEngine::new(2);
        let generation = engine.generation();
        let job = engine.begin(Box::new(|_| Ok(value(9)))).expect("begin");
        engine.wait_until(generation, Instant::now() + Duration::from_secs(5));
        settled(&engine, job.id());
        assert!(job.is_ready());
    }
}
