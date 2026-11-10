//! Workers for the elements of independent products.
//!
//! An element of `(a, b)!` used to get a thread of its own, which ended
//! with the element. Starting and ending a thread costs far more than a
//! small element, and memory the ended thread allocated is freed later by
//! another thread into pages nobody owns any more. Here an element is a
//! task handed to a worker: a thread that stays after its task, waiting for
//! the next one.
//!
//! Every task starts at once, as a thread did: a task goes to a waiting
//! worker, or a new worker is started for it. So elements still run at the
//! same time as one another, however many there are and however many cores
//! the host has, and the answers do not depend on either. A worker that
//! finishes a task waits for another one, unless enough workers are
//! waiting already, and then it ends. The tasks do not count towards the
//! limit of [`super::JobEngine`]: a product waits for its elements, and an
//! element queued behind background jobs could wait for ever.
//!
//! The thread that joins a task runs it itself when no worker has taken it
//! yet, so a product nested in an element never waits for a worker, and
//! nothing is lost when the host refuses a new thread.
//!
//! A task runs as if on a thread of its own ([`crate::as_own_thread`]): it
//! starts with no `Terminal` output, sockets or console capture, and when it
//! ends its output is flushed and its sockets are closed, as when its thread
//! ended. Its stack is a worker's, of [`crate::THREAD_STACK_BYTES`].
//!
//! [`scope`] has the shape of [`std::thread::scope`]: an element may borrow
//! from the caller, and `scope` returns only once every task begun in it has
//! finished.

use std::collections::VecDeque;
use std::marker::PhantomData;
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};
use std::sync::{Arc, Condvar, Mutex, MutexGuard, OnceLock};

/// A task with its borrows erased; see the safety argument in
/// [`Scope::spawn`].
type Task = Box<dyn FnOnce() + Send + 'static>;

struct Queue {
    /// Tasks no worker has taken yet, oldest first.
    tasks: VecDeque<(u64, Task)>,
    next_id: u64,
    /// Workers waiting for a task.
    waiting: usize,
    /// Of those, how many have been told to take one and have not yet.
    told: usize,
}

static QUEUE: Mutex<Queue> = Mutex::new(Queue {
    tasks: VecDeque::new(),
    next_id: 0,
    waiting: 0,
    told: 0,
});
static WAKE: Condvar = Condvar::new();

fn queue() -> MutexGuard<'static, Queue> {
    // A task never runs while the lock is held, so the lock cannot be
    // poisoned by one; recover it anyway rather than stop every product.
    QUEUE
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

/// How many workers may wait for a task: as many as the host runs at once.
fn most_waiting() -> usize {
    static MOST: OnceLock<usize> = OnceLock::new();
    *MOST.get_or_init(super::JobEngine::default_limit)
}

/// Queue `task` and see that a worker takes it.
fn submit(task: Task) -> u64 {
    let mut queue = queue();
    queue.next_id += 1;
    let id = queue.next_id;
    queue.tasks.push_back((id, task));
    if queue.waiting > queue.told {
        queue.told += 1;
        drop(queue);
        WAKE.notify_one();
    } else {
        drop(queue);
        // A host that cannot start a thread leaves the task queued: the
        // thread that joins it runs it.
        let _ = std::thread::Builder::new()
            .stack_size(crate::THREAD_STACK_BYTES)
            .spawn(work);
    }
    id
}

/// Take back a task no worker has taken yet.
fn take_back(id: u64) -> Option<Task> {
    let mut queue = queue();
    let at = queue.tasks.iter().position(|(queued, _)| *queued == id)?;
    queue.tasks.remove(at).map(|(_, task)| task)
}

/// A worker: runs tasks while there are any, then waits for one, or ends
/// when enough workers are waiting already.
fn work() {
    let mut queue = queue();
    loop {
        if let Some((_, task)) = queue.tasks.pop_front() {
            drop(queue);
            task();
            queue = self::queue();
            continue;
        }
        if queue.waiting >= most_waiting() {
            return;
        }
        queue.waiting += 1;
        while queue.told == 0 {
            queue = WAKE
                .wait(queue)
                .unwrap_or_else(|poisoned| poisoned.into_inner());
        }
        queue.told -= 1;
        queue.waiting -= 1;
    }
}

#[derive(Default)]
struct ScopeState {
    /// Tasks begun in the scope that have not finished.
    running: usize,
    /// Tasks that panicked and whose panic no `join` has taken.
    unjoined_panics: usize,
    /// Every task begun in the scope, to take back the ones no worker took.
    ids: Vec<u64>,
}

#[derive(Default)]
struct ScopeData {
    state: Mutex<ScopeState>,
    finished: Condvar,
}

impl ScopeData {
    fn state(&self) -> MutexGuard<'_, ScopeState> {
        self.state
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }
}

/// Where the tasks of one product are begun; see [`scope`].
pub struct Scope<'scope, 'env: 'scope> {
    data: Arc<ScopeData>,
    scope: PhantomData<&'scope mut &'scope ()>,
    env: PhantomData<&'env mut &'env ()>,
}

/// One task's answer, kept until `join` takes it.
struct Packet<T> {
    answer: Mutex<Option<std::thread::Result<T>>>,
}

/// A task begun in a [`Scope`].
pub struct ScopedTask<'scope, T> {
    id: u64,
    packet: Arc<Packet<T>>,
    data: Arc<ScopeData>,
    scope: PhantomData<&'scope ()>,
}

/// Runs `body` with a [`Scope`] to begin tasks in, and returns once every
/// task begun in it has finished, as [`std::thread::scope`] does: a panic
/// of `body` is passed on after that, and a task that panicked without
/// being joined panics the scope.
pub fn scope<'env, F, T>(body: F) -> T
where
    F: for<'scope> FnOnce(&'scope Scope<'scope, 'env>) -> T,
{
    let scope = Scope {
        data: Arc::new(ScopeData::default()),
        scope: PhantomData,
        env: PhantomData,
    };
    let answer = catch_unwind(AssertUnwindSafe(|| body(&scope)));
    scope.finish();
    let unjoined_panics = scope.data.state().unjoined_panics;
    match answer {
        Err(payload) => resume_unwind(payload),
        Ok(_) if unjoined_panics > 0 => panic!("a scoped thread panicked"),
        Ok(value) => value,
    }
}

impl<'scope, 'env> Scope<'scope, 'env> {
    /// Begin `body` as a task.
    pub fn spawn<F, T>(&'scope self, body: F) -> ScopedTask<'scope, T>
    where
        F: FnOnce() -> T + Send + 'scope,
        T: Send + 'scope,
    {
        let packet = Arc::new(Packet {
            answer: Mutex::new(None),
        });
        let data = self.data.clone();
        data.state().running += 1;
        let task_packet = packet.clone();
        let task_data = data.clone();
        let task: Box<dyn FnOnce() + Send + 'scope> = Box::new(move || {
            let answer = catch_unwind(AssertUnwindSafe(|| crate::as_own_thread(body)));
            let panicked = answer.is_err();
            *task_packet
                .answer
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner()) = Some(answer);
            drop(task_packet);
            let mut state = task_data.state();
            state.running -= 1;
            if panicked {
                state.unjoined_panics += 1;
            }
            drop(state);
            task_data.finished.notify_all();
        });
        // SAFETY: the task borrows nothing that lives shorter than
        // `'scope`, and it has finished running before `'scope` ends:
        // - `scope` calls `finish` before it returns or passes a panic on,
        //   and `finish` returns only once `running` is back to zero, which
        //   the task makes it only as its last step, after it last touched
        //   `body`, its answer or anything they borrow;
        // - every task begun here was counted in `running` above, before it
        //   could start, and `Scope` is only ever lent to `body`, so no task
        //   can be begun after `finish`;
        // - a task runs at most once: whoever removes it from the queue,
        //   under the queue's lock, is the only one to run it, and `finish`
        //   runs every task of the scope still queued, so none is left
        //   behind when the scope ends. A `ScopedTask` that is forgotten
        //   changes none of this.
        // Only the lifetime changes; the box's contents stay as they are.
        let task: Task =
            unsafe { std::mem::transmute::<Box<dyn FnOnce() + Send + 'scope>, Task>(task) };
        let id = submit(task);
        data.state().ids.push(id);
        ScopedTask {
            id,
            packet,
            data,
            scope: PhantomData,
        }
    }

    /// Run the scope's tasks no worker has taken, then wait for the rest.
    fn finish(&self) {
        let ids = std::mem::take(&mut self.data.state().ids);
        for id in ids {
            if let Some(task) = take_back(id) {
                task();
            }
        }
        let mut state = self.data.state();
        while state.running > 0 {
            state = self
                .data
                .finished
                .wait(state)
                .unwrap_or_else(|poisoned| poisoned.into_inner());
        }
    }
}

impl<T> ScopedTask<'_, T> {
    /// Wait for the task and take its answer: its value, or the payload of
    /// its panic, as joining a thread gives. A task no worker has taken yet
    /// runs here.
    pub fn join(self) -> std::thread::Result<T> {
        if let Some(task) = take_back(self.id) {
            task();
        }
        let mut state = self.data.state();
        loop {
            let answer = self
                .packet
                .answer
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner())
                .take();
            if let Some(answer) = answer {
                if answer.is_err() {
                    state.unjoined_panics -= 1;
                }
                return answer;
            }
            state = self
                .data
                .finished
                .wait(state)
                .unwrap_or_else(|poisoned| poisoned.into_inner());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    // Miri runs these slowly: smaller sizes there.
    const TASKS: usize = if cfg!(miri) { 8 } else { 64 };
    const LEAVES: u64 = if cfg!(miri) { 16 } else { 4096 };

    #[test]
    fn answers_come_back_in_order_and_borrows_are_seen() {
        let words = ["a", "bb", "ccc"];
        let lengths = scope(|s| {
            let tasks: Vec<_> = words.iter().map(|w| s.spawn(move || w.len())).collect();
            tasks
                .into_iter()
                .map(|t| t.join().unwrap())
                .collect::<Vec<_>>()
        });
        assert_eq!(lengths, [1, 2, 3]);
    }

    #[test]
    fn a_panic_comes_back_from_join_and_the_scope_goes_on() {
        let answer = scope(|s| {
            let task = s.spawn(|| -> i32 { std::panic::panic_any("element panicked") });
            let other = s.spawn(|| 7);
            let panic = task.join().expect_err("the task panicked");
            assert_eq!(panic.downcast_ref::<&str>(), Some(&"element panicked"));
            other.join().unwrap()
        });
        assert_eq!(answer, 7);
    }

    #[test]
    fn a_panic_no_join_takes_panics_the_scope() {
        let outcome = catch_unwind(|| {
            scope(|s| {
                s.spawn(|| -> i32 { std::panic::panic_any("element panicked") });
            })
        });
        let payload = outcome.expect_err("the scope panics");
        assert_eq!(
            payload.downcast_ref::<&str>(),
            Some(&"a scoped thread panicked")
        );
    }

    #[test]
    fn every_task_has_finished_when_the_scope_ends() {
        let done = AtomicUsize::new(0);
        scope(|s| {
            for _ in 0..TASKS {
                s.spawn(|| {
                    done.fetch_add(1, Ordering::Relaxed);
                });
            }
        });
        assert_eq!(done.load(Ordering::Relaxed), TASKS);
    }

    /// A product inside an element, many levels deep and wide: the joiner
    /// runs a task no worker has taken, so nothing waits for a free worker.
    #[test]
    fn nested_scopes_finish() {
        fn sum(from: u64, to: u64) -> u64 {
            if to - from <= 1 {
                return from;
            }
            let mid = from + (to - from) / 2;
            scope(|s| {
                let right = s.spawn(move || sum(mid, to));
                let left = sum(from, mid);
                left + right.join().unwrap()
            })
        }
        assert_eq!(sum(0, LEAVES), (0..LEAVES).sum());
    }

    /// A task runs as if on a thread of its own: the console capture of the
    /// thread that runs it is out of its sight.
    #[test]
    fn a_task_does_not_see_the_capture_of_the_thread_that_runs_it() {
        let ((), out, _) = crate::capture_console_output(|| {
            scope(|s| {
                let task = s.spawn(|| crate::console_print(&crate::AverStr::from("x")));
                task.join().unwrap();
            });
        });
        assert!(out.is_empty());
    }
}
