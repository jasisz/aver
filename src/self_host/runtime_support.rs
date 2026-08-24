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

pub(crate) fn should_skip_http_server() -> bool {
    crate::aver_replay::is_record_mode()
}

pub fn http_server_listen<F>(port: i64, mut handler: F) -> Result<(), AverStr>
where
    F: FnMut(&aver_rt::HttpRequest) -> HttpResponse,
{
    if should_skip_http_server() {
        return Ok(());
    }
    // User handlers are emitted borrow-by-default (`fn(&HttpRequest)`),
    // so adapt the owned-value `aver_rt` callback to a by-reference call.
    // The handler yields the surface `HttpResponse` (`AverInt` status); lower
    // it to the host struct (`i64` status) before `aver_rt::http_server`.
    aver_rt::http_server::listen(port, move |req| http_response_to_host(handler(&req)))
        .map_err(AverStr::from)
}

pub fn http_server_listen_with<C, F>(port: i64, context: C, mut handler: F) -> Result<(), AverStr>
where
    C: Clone,
    F: FnMut(&C, &aver_rt::HttpRequest) -> HttpResponse,
{
    if should_skip_http_server() {
        return Ok(());
    }
    // User handlers take both the context and the request by reference
    // (`fn(&Ctx, &HttpRequest)`); adapt the owned-value `aver_rt`
    // callback accordingly.
    aver_rt::http_server::listen_with(port, context, move |ctx, req| {
        http_response_to_host(handler(&ctx, &req))
    })
    .map_err(AverStr::from)
}

#[derive(Clone, Debug, PartialEq)]
pub struct HttpResponse {
    pub status: aver_rt::AverInt,
    pub body: aver_rt::AverStr,
    pub headers: aver_rt::HttpHeaders,
}
impl aver_rt::AverDisplay for HttpResponse {
    fn aver_display(&self) -> String {
        format!(
            "HttpResponse(status: {}, body: {}, headers: {})",
            self.status.aver_display_inner(),
            self.body.aver_display_inner(),
            self.headers.aver_display_inner()
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}
/// Host `aver_rt::HttpResponse` (`i64` status) → surface (`AverInt` status),
/// applied at the `Http.*` effect boundary via `.into_aver()`.
fn convert_http_response(r: aver_rt::HttpResponse) -> HttpResponse {
    HttpResponse {
        status: aver_rt::AverInt::from_i64(r.status),
        body: r.body,
        headers: r.headers,
    }
}
/// Surface (`AverInt` status) → host `aver_rt::HttpResponse` (`i64` status),
/// applied before a handler's response reaches `aver_rt::http_server`.
pub fn http_response_to_host(r: HttpResponse) -> aver_rt::HttpResponse {
    aver_rt::HttpResponse {
        status: r.status.to_i64().unwrap_or(0),
        body: r.body,
        headers: r.headers,
    }
}
impl IntoAverStr for Result<aver_rt::HttpResponse, String> {
    type Output = Result<HttpResponse, AverStr>;
    fn into_aver(self) -> Result<HttpResponse, AverStr> {
        self.map(convert_http_response).map_err(AverStr::from)
    }
}

pub use aver_rt::HttpRequest;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Terminal_Size {
    pub width: aver_rt::AverInt,
    pub height: aver_rt::AverInt,
}
impl aver_rt::AverDisplay for Terminal_Size {
    fn aver_display(&self) -> String {
        format!(
            "Terminal.Size {{ width: {}, height: {} }}",
            self.width, self.height
        )
    }
}

pub use aver_rt::BranchPath;
