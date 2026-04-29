/// Generate the inline `mod aver_rt { ... }` runtime bridge.
///
/// This module is embedded in the generated `main.rs` and re-exports pieces
/// from the shared `aver-rt` crate.
pub fn generate_runtime(
    has_replay: bool,
    has_http_server_runtime: bool,
    embedded_independence_cancel: bool,
) -> String {
    let mut sections = vec![base_runtime(has_replay, embedded_independence_cancel)];
    if has_http_server_runtime {
        sections.push(http_server_helpers(has_replay));
    }
    sections.join("\n\n")
}

fn base_runtime(has_replay: bool, embedded_independence_cancel: bool) -> String {
    let independence_mode = if has_replay {
        "crate::aver_replay::independence_mode_is_cancel()".to_string()
    } else if embedded_independence_cancel {
        "true".to_string()
    } else {
        "false".to_string()
    };

    BASE_RUNTIME_TEMPLATE.replace("__INDEPENDENCE_MODE_CANCEL__", &independence_mode)
}

const BASE_RUNTIME_TEMPLATE: &str = r##"pub mod aver_rt {
    pub use ::aver_rt::*;
}

use ::aver_rt::AverStr;
use std::cell::RefCell;
use std::sync::{
    Arc,
    atomic::AtomicBool,
};

/// Convert String results from aver_rt to AverStr for generated code.
pub trait IntoAverStr {
    type Output;
    fn into_aver(self) -> Self::Output;
}
impl IntoAverStr for String {
    type Output = AverStr;
    fn into_aver(self) -> AverStr { AverStr::from(self) }
}
impl IntoAverStr for Result<String, String> {
    type Output = Result<AverStr, AverStr>;
    fn into_aver(self) -> Result<AverStr, AverStr> { self.map(AverStr::from).map_err(AverStr::from) }
}
impl IntoAverStr for Result<(), String> {
    type Output = Result<(), AverStr>;
    fn into_aver(self) -> Result<(), AverStr> { self.map_err(AverStr::from) }
}
impl IntoAverStr for Option<String> {
    type Output = Option<AverStr>;
    fn into_aver(self) -> Option<AverStr> { self.map(AverStr::from) }
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
impl IntoAverStr for Result<aver_rt::HttpResponse, String> {
    type Output = Result<aver_rt::HttpResponse, AverStr>;
    fn into_aver(self) -> Result<aver_rt::HttpResponse, AverStr> {
        self.map_err(AverStr::from)
    }
}
impl IntoAverStr for Result<aver_rt::TcpConnection, String> {
    type Output = Result<aver_rt::TcpConnection, AverStr>;
    fn into_aver(self) -> Result<aver_rt::TcpConnection, AverStr> {
        self.map_err(AverStr::from)
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

fn independence_mode_is_cancel() -> bool {
    __INDEPENDENCE_MODE_CANCEL__
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
"##;

fn http_server_helpers(has_replay: bool) -> String {
    let replay_guard = if has_replay {
        "crate::aver_replay::is_record_mode()"
    } else {
        "false"
    };

    format!(
        r#"
pub(crate) fn should_skip_http_server() -> bool {{
    {replay_guard}
}}

pub fn http_server_listen<F>(port: i64, handler: F) -> Result<(), AverStr>
where
    F: FnMut(aver_rt::HttpRequest) -> aver_rt::HttpResponse,
{{
    if should_skip_http_server() {{
        return Ok(());
    }}
    aver_rt::http_server::listen(port, handler).map_err(AverStr::from)
}}

pub fn http_server_listen_with<C, F>(port: i64, context: C, handler: F) -> Result<(), AverStr>
where
    C: Clone,
    F: FnMut(C, aver_rt::HttpRequest) -> aver_rt::HttpResponse,
{{
    if should_skip_http_server() {{
        return Ok(());
    }}
    aver_rt::http_server::listen_with(port, context, handler).map_err(AverStr::from)
}}"#
    )
}

/// Bring the shared Tcp connection type into the generated program under the
/// legacy codegen name used for dotted `Tcp.Connection`.
pub fn generate_tcp_types() -> String {
    "pub use aver_rt::TcpConnection as Tcp_Connection;".to_string()
}

/// Bring shared HTTP record types into the generated program.
pub fn generate_http_types() -> String {
    "pub use aver_rt::HttpResponse;".to_string()
}

/// Bring shared HTTP server request type into the generated program.
pub fn generate_http_server_types() -> String {
    "pub use aver_rt::HttpRequest;".to_string()
}
