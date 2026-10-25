pub mod aver_rt {
    pub use ::aver_rt::*;
}

use ::aver_rt::AverStr;
use std::cell::{Cell, RefCell};
use std::sync::{
    Arc,
    atomic::{AtomicBool, AtomicUsize, Ordering},
};

/// Convert String results from aver_rt to AverStr for generated code.
pub trait IntoAverStr {
    type Output;
    fn into_aver(self) -> Self::Output;
}
impl IntoAverStr for String {
    type Output = AverStr;
    fn into_aver(self) -> AverStr {
        AverStr::from(self)
    }
}
impl IntoAverStr for Result<String, String> {
    type Output = Result<AverStr, AverStr>;
    fn into_aver(self) -> Result<AverStr, AverStr> {
        self.map(AverStr::from).map_err(AverStr::from)
    }
}
impl IntoAverStr for Result<(), String> {
    type Output = Result<(), AverStr>;
    fn into_aver(self) -> Result<(), AverStr> {
        self.map_err(AverStr::from)
    }
}
impl IntoAverStr for Option<String> {
    type Output = Option<AverStr>;
    fn into_aver(self) -> Option<AverStr> {
        self.map(AverStr::from)
    }
}
impl IntoAverStr for Result<Option<String>, String> {
    type Output = Result<Option<AverStr>, AverStr>;
    fn into_aver(self) -> Result<Option<AverStr>, AverStr> {
        self.map(|value| value.map(AverStr::from))
            .map_err(AverStr::from)
    }
}
impl IntoAverStr for aver_rt::AverList<String> {
    type Output = aver_rt::AverList<AverStr>;
    fn into_aver(self) -> aver_rt::AverList<AverStr> {
        aver_rt::AverList::from_vec(self.to_vec().into_iter().map(AverStr::from).collect())
    }
}
impl IntoAverStr for Result<aver_rt::AverList<String>, String> {
    type Output = Result<aver_rt::AverList<AverStr>, AverStr>;
    fn into_aver(self) -> Result<aver_rt::AverList<AverStr>, AverStr> {
        self.map(|l| l.into_aver()).map_err(AverStr::from)
    }
}
impl IntoAverStr for Result<i64, String> {
    type Output = Result<i64, AverStr>;
    fn into_aver(self) -> Result<i64, AverStr> {
        self.map_err(AverStr::from)
    }
}
impl IntoAverStr for Result<f64, String> {
    type Output = Result<f64, AverStr>;
    fn into_aver(self) -> Result<f64, AverStr> {
        self.map_err(AverStr::from)
    }
}
impl IntoAverStr for Result<aver_rt::AverInt, String> {
    type Output = Result<aver_rt::AverInt, AverStr>;
    fn into_aver(self) -> Result<aver_rt::AverInt, AverStr> {
        self.map_err(AverStr::from)
    }
}

/// Saturate an `AverInt` to `i64` for an index/slice boundary that clamps
/// (never errors): a Big-positive value clamps to `i64::MAX`, a Big-negative
/// to `i64::MIN`. The downstream `string_slice` then clamps the i64 into the
/// string's actual range, matching the VM exactly.
pub fn aver_int_clamp_i64(n: &aver_rt::AverInt) -> i64 {
    n.to_i64().unwrap_or_else(|| {
        if *n < aver_rt::AverInt::zero() {
            i64::MIN
        } else {
            i64::MAX
        }
    })
}

/// Convert an `AverInt` to `i64` at an internal host boundary with no recovery
/// channel. Surface operations whose contract is fallible (`Random.int`,
/// `Time.sleep`, `Terminal.moveTo`, and similar) validate into their `Result`
/// before crossing such a boundary. Never silently substitute a value here.
pub fn to_host_i64(n: &aver_rt::AverInt, vm_message: &str) -> i64 {
    n.to_i64().expect(vm_message)
}

fn independence_mode_is_cancel() -> bool {
    false
}

#[derive(Debug)]
struct AverCancelled;

pub enum ParallelBranch<T> {
    Completed(T),
    Cancelled,
}

/// Whether this program can run in cancel mode: compiled in it, or reading
/// its mode at run time.
const MAY_CANCEL: bool = false;

/// How many steps a branch that can be cancelled takes between two reads of
/// its flags: a cancelled branch stops at most this many steps late.
const CANCEL_CHECK_EVERY: u32 = 1024;

/// Branches that can be cancelled running now, on any thread. While there
/// are none a step reads only this, not a thread local.
static CANCELABLE_BRANCHES: AtomicUsize = AtomicUsize::new(0);

thread_local! {
    static ACTIVE_CANCEL_FLAGS: RefCell<Vec<Arc<AtomicBool>>> = const { RefCell::new(Vec::new()) };
    /// Steps left before the next read of the flags; 0 on a thread that
    /// runs no branch that can be cancelled.
    static CANCEL_STEPS_LEFT: Cell<u32> = const { Cell::new(0) };
}

/// Pops the branch's flag and gives back the step count it found.
struct CancelFlagGuard(u32);

impl Drop for CancelFlagGuard {
    fn drop(&mut self) {
        ACTIVE_CANCEL_FLAGS.with(|cell| {
            cell.borrow_mut().pop();
        });
        CANCEL_STEPS_LEFT.with(|left| left.set(self.0));
        CANCELABLE_BRANCHES.fetch_sub(1, Ordering::Relaxed);
    }
}

fn with_cancel_flag<T, F>(flag: Arc<AtomicBool>, run: F) -> T
where
    F: FnOnce() -> T,
{
    ACTIVE_CANCEL_FLAGS.with(|cell| {
        cell.borrow_mut().push(flag);
    });
    CANCELABLE_BRANCHES.fetch_add(1, Ordering::Relaxed);
    let _guard = CancelFlagGuard(CANCEL_STEPS_LEFT.with(|left| left.replace(CANCEL_CHECK_EVERY)));
    run()
}

fn is_cancel_panic(payload: &(dyn std::any::Any + Send)) -> bool {
    payload.is::<AverCancelled>()
}

/// A step: a function entered, a loop or a trampoline going round. While no
/// branch that can be cancelled runs it reads one counter; inside one it
/// reads the flags every `CANCEL_CHECK_EVERY` steps.
#[inline]
pub fn cancel_checkpoint() {
    if MAY_CANCEL && CANCELABLE_BRANCHES.load(Ordering::Relaxed) != 0 {
        cancel_step();
    }
}

#[inline]
fn cancel_step() {
    let due = CANCEL_STEPS_LEFT.with(|left| match left.get() {
        0 => false,
        1 => {
            left.set(CANCEL_CHECK_EVERY);
            true
        }
        steps => {
            left.set(steps - 1);
            false
        }
    });
    if due {
        cancel_checkpoint_now();
    }
}

/// Before an effect and where a branch starts: reads the flags now, so a
/// cancelled branch does nothing more that can be seen outside it.
pub fn cancel_checkpoint_now() {
    if !independence_mode_is_cancel() {
        return;
    }

    let cancelled = ACTIVE_CANCEL_FLAGS.with(|cell| {
        cell.borrow()
            .iter()
            .any(|flag| flag.load(Ordering::Relaxed))
    });
    if cancelled {
        std::panic::panic_any(AverCancelled);
    }
}

pub fn run_cancelable_branch<T, F>(flag: Arc<AtomicBool>, run: F) -> ParallelBranch<T>
where
    F: FnOnce() -> T,
{
    with_cancel_flag(flag, || {
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            cancel_checkpoint_now();
            run()
        }));
        match result {
            Ok(value) => ParallelBranch::Completed(value),
            Err(payload) => {
                if is_cancel_panic(payload.as_ref()) {
                    ParallelBranch::Cancelled
                } else {
                    std::panic::resume_unwind(payload);
                }
            }
        }
    })
}
