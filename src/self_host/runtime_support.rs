pub mod aver_rt {
    pub use ::aver_rt::*;
}

use ::aver_rt::AverStr;
use std::cell::RefCell;
use std::sync::{Arc, atomic::AtomicBool};

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
    crate::aver_replay::independence_mode_is_cancel()
}

#[derive(Debug)]
struct AverCancelled;

pub enum ParallelBranch<T> {
    Completed(T),
    Cancelled,
}

thread_local! {
    static ACTIVE_CANCEL_FLAGS: RefCell<Vec<Arc<AtomicBool>>> = const { RefCell::new(Vec::new()) };
}

struct CancelFlagGuard;

impl Drop for CancelFlagGuard {
    fn drop(&mut self) {
        ACTIVE_CANCEL_FLAGS.with(|cell| {
            cell.borrow_mut().pop();
        });
    }
}

fn with_cancel_flag<T, F>(flag: Arc<AtomicBool>, run: F) -> T
where
    F: FnOnce() -> T,
{
    ACTIVE_CANCEL_FLAGS.with(|cell| {
        cell.borrow_mut().push(flag);
    });
    let _guard = CancelFlagGuard;
    run()
}

fn is_cancel_panic(payload: &(dyn std::any::Any + Send)) -> bool {
    payload.is::<AverCancelled>()
}

pub fn cancel_checkpoint() {
    if !independence_mode_is_cancel() {
        return;
    }

    let cancelled = ACTIVE_CANCEL_FLAGS.with(|cell| {
        cell.borrow()
            .iter()
            .any(|flag| flag.load(std::sync::atomic::Ordering::Relaxed))
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
            cancel_checkpoint();
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

pub use aver_rt::BranchPath;
