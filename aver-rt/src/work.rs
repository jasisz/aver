//! The job engine: pure work a program starts off the turn.
//!
//! A job is the only resource the concurrency work adds. `begin` hands the
//! engine a body and gets a handle back at once; the body runs on its own
//! thread and writes its answer into the job table; `take` collects it in a
//! later turn; `cancel` stops it at the body's next cancellation check. The
//! engine deliberately knows nothing about Aver: a body is a closure and an
//! answer is a [`ProviderValue`], so the same table serves the bytecode VM
//! today and a generated artifact later.

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
    Running,
    Finished(JobOutcome),
    Cancelled,
    Taken,
}

struct JobSlot {
    cancel: Arc<AtomicBool>,
    state: JobState,
}

#[derive(Default)]
struct JobTable {
    next_id: u64,
    jobs: BTreeMap<u64, JobSlot>,
    running: usize,
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
}

/// One program's running jobs.
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
    /// A new engine bounded by `limit` running jobs. A zero limit is a
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

    /// Start `body` off the turn and return its handle at once.
    ///
    /// The turn is never blocked: at the limit this refuses instead of
    /// waiting, and the refusal is the `Result.Err` the program reads.
    pub fn begin(self: &Arc<Self>, body: JobBody) -> Result<Job, String> {
        let id = {
            let mut table = self.lock()?;
            if table.running >= self.limit {
                return Err(format!("work: job limit {} reached", self.limit));
            }
            table.next_id += 1;
            let id = table.next_id;
            table.jobs.insert(
                id,
                JobSlot {
                    cancel: Arc::new(AtomicBool::new(false)),
                    state: JobState::Running,
                },
            );
            table.running += 1;
            id
        };
        let cancel = {
            let table = self.lock()?;
            table.jobs[&id].cancel.clone()
        };
        let engine = Arc::downgrade(self);
        let spawned = std::thread::Builder::new()
            .name(format!("aver-job-{id}"))
            .spawn(move || {
                // A body that unwinds must still settle its slot: otherwise
                // the job stays Running for ever, the limit keeps counting it
                // and every `take` answers `Ok(None)` with nothing left to
                // produce an answer.
                let outcome =
                    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| body(cancel))) {
                        Ok(outcome) => outcome,
                        Err(_) => Err("work: the job's body stopped unexpectedly".to_string()),
                    };
                if let Some(engine) = engine.upgrade() {
                    engine.settle(id, outcome);
                }
            });
        match spawned {
            Ok(_) => Ok(Job {
                engine: self.clone(),
                id,
            }),
            Err(error) => {
                let mut table = self.lock()?;
                table.jobs.remove(&id);
                table.running -= 1;
                Err(format!("work: the host could not start a job: {error}"))
            }
        }
    }

    fn settle(&self, id: u64, outcome: JobOutcome) {
        {
            let Ok(mut table) = self.table.lock() else {
                return;
            };
            table.running = table.running.saturating_sub(1);
            table.settled += 1;
            if let Some(slot) = table.jobs.get_mut(&id)
                && matches!(slot.state, JobState::Running)
            {
                slot.state = JobState::Finished(outcome);
            }
        }
        self.settled.notify_all();
        self.ring_wakers();
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

    /// Collect a job's answer. `Ok(None)` means it is still running, and
    /// the handle stays usable: a job reported ready that has not finished
    /// is collected in a later turn.
    ///
    /// A job whose slot was already forgotten — more than
    /// [`DEAD_SLOT_LIMIT`] jobs have died since it was collected — answers
    /// `work: unknown job`.
    pub fn take(&self, id: u64) -> Result<Option<ProviderValue>, String> {
        let mut table = self.lock()?;
        let Some(slot) = table.jobs.get_mut(&id) else {
            return Err("work: unknown job".to_string());
        };
        // A job that had finished becomes a dead slot here, so this is where
        // its id joins the bounded tombstone list.
        let mut retired = false;
        let answer = match std::mem::replace(&mut slot.state, JobState::Taken) {
            JobState::Running => {
                slot.state = JobState::Running;
                Ok(None)
            }
            JobState::Finished(Ok(value)) => {
                retired = true;
                Ok(Some(value))
            }
            JobState::Finished(Err(message)) => {
                retired = true;
                Err(message)
            }
            JobState::Cancelled => {
                slot.state = JobState::Cancelled;
                Err("work: job cancelled".to_string())
            }
            JobState::Taken => {
                slot.state = JobState::Taken;
                Err("work: job already taken".to_string())
            }
        };
        if retired {
            table.retire(id);
        }
        answer
    }

    /// The same answer without consuming it.
    pub fn peek(&self, id: u64) -> Result<Option<ProviderValue>, String> {
        let table = self.lock()?;
        let Some(slot) = table.jobs.get(&id) else {
            return Err("work: unknown job".to_string());
        };
        match &slot.state {
            JobState::Running => Ok(None),
            JobState::Finished(Ok(value)) => Ok(Some(value.clone())),
            JobState::Finished(Err(message)) => Err(message.clone()),
            JobState::Cancelled => Err("work: job cancelled".to_string()),
            JobState::Taken => Err("work: job already taken".to_string()),
        }
    }

    /// Stop a job. Running work stops at its next cancellation check, a
    /// finished job drops its answer, and cancelling twice changes nothing.
    /// A job whose answer was already collected is left alone: its story is
    /// that it was taken, and a later take must still say so.
    pub fn cancel(&self, id: u64) {
        let Ok(mut table) = self.table.lock() else {
            return;
        };
        let mut retired = false;
        if let Some(slot) = table.jobs.get_mut(&id) {
            slot.cancel.store(true, Ordering::Relaxed);
            if !matches!(slot.state, JobState::Taken) {
                retired = !matches!(slot.state, JobState::Cancelled);
                slot.state = JobState::Cancelled;
            }
            table.settled += 1;
        }
        if retired {
            table.retire(id);
        }
        drop(table);
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
            Some(slot) => !matches!(slot.state, JobState::Running),
            None => true,
        }
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
    /// A job that outlives the grace period is left to the process exit: the
    /// runtime promised not to wait for it beyond a bounded join.
    pub fn shutdown(&self) {
        {
            let Ok(mut table) = self.table.lock() else {
                return;
            };
            for slot in table.jobs.values_mut() {
                slot.cancel.store(true, Ordering::Relaxed);
                if !matches!(slot.state, JobState::Taken) {
                    slot.state = JobState::Cancelled;
                }
            }
            table.settled += 1;
        }
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

    #[test]
    fn the_limit_refuses_instead_of_blocking_the_turn() {
        let engine = JobEngine::new(1);
        let release = Arc::new(AtomicBool::new(false));
        let held = release.clone();
        let _first = engine
            .begin(Box::new(move |cancel| {
                while !held.load(Ordering::Relaxed) && !cancel.load(Ordering::Relaxed) {
                    std::thread::yield_now();
                }
                Ok(value(1))
            }))
            .expect("first job starts");
        let second = engine.begin(Box::new(|_| Ok(value(2))));
        assert_eq!(second.err(), Some("work: job limit 1 reached".to_string()));
        release.store(true, Ordering::Relaxed);
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
        // The slot the stopped job held is free again, so the limit did not
        // leak it.
        assert!(engine.begin(Box::new(|_| Ok(value(1)))).is_ok());
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
