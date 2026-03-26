/// Generate the inline `mod aver_rt { ... }` runtime bridge.
///
/// This module is embedded in the generated `main.rs` and re-exports pieces
/// from the shared `aver-rt` crate.
pub fn generate_runtime(
    has_replay: bool,
    has_http_server_runtime: bool,
    emit_self_host_runtime: bool,
) -> String {
    let mut sections = vec![BASE_RUNTIME.to_string()];
    if has_http_server_runtime || emit_self_host_runtime {
        sections.push(http_server_helpers(has_replay, emit_self_host_runtime));
    }
    sections.join("\n\n")
}

const BASE_RUNTIME: &str = r##"pub mod aver_rt {
    pub use ::aver_rt::*;
}

use ::aver_rt::AverStr;

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
}"##;

fn http_server_helpers(has_replay: bool, emit_self_host_runtime: bool) -> String {
    let replay_guard = if has_replay {
        "crate::aver_replay::is_record_mode()"
    } else {
        "false"
    };

    let self_host_helpers = if emit_self_host_runtime {
        SELF_HOST_HTTP_SERVER_HELPERS
    } else {
        ""
    };

    format!(
        r#"
fn should_skip_http_server() -> bool {{
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
}}

{self_host_helpers}"#
    )
}

const SELF_HOST_HTTP_SERVER_HELPERS: &str = r#"
use std::cell::RefCell;

thread_local! {
    static SELF_HOST_FN_STORE: RefCell<Option<crate::aver_generated::domain::eval::FnStore>> =
        const { RefCell::new(None) };
}

pub fn with_self_host_fn_store<T, F>(
    fns: crate::aver_generated::domain::eval::FnStore,
    run: F,
) -> T
where
    F: FnOnce() -> T,
{
    let previous = SELF_HOST_FN_STORE.with(|cell| cell.replace(Some(fns)));
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(run));
    SELF_HOST_FN_STORE.with(|cell| {
        cell.replace(previous);
    });
    match result {
        Ok(value) => value,
        Err(payload) => std::panic::resume_unwind(payload),
    }
}

fn current_self_host_fn_store() -> Option<crate::aver_generated::domain::eval::FnStore> {
    SELF_HOST_FN_STORE.with(|cell| cell.borrow().clone())
}

pub fn self_host_http_server_listen(
    port: i64,
    handler: crate::aver_generated::domain::value::Val,
) -> Result<(), AverStr> {
    if should_skip_http_server() {
        return Ok(());
    }
    aver_rt::http_server::listen(port, move |request| {
        dispatch_self_host_http_handler(handler.clone(), None, request)
    })
    .map_err(AverStr::from)
}

pub fn self_host_http_server_listen_with(
    port: i64,
    context: crate::aver_generated::domain::value::Val,
    handler: crate::aver_generated::domain::value::Val,
) -> Result<(), AverStr> {
    if should_skip_http_server() {
        return Ok(());
    }
    aver_rt::http_server::listen_with(port, context, move |ctx, request| {
        dispatch_self_host_http_handler(handler.clone(), Some(ctx), request)
    })
    .map_err(AverStr::from)
}

fn dispatch_self_host_http_handler(
    handler: crate::aver_generated::domain::value::Val,
    context: Option<crate::aver_generated::domain::value::Val>,
    request: aver_rt::HttpRequest,
) -> aver_rt::HttpResponse {
    use crate::aver_generated::domain::value::Val;

    let handler_name = match handler {
        Val::ValFnRef(name) => name,
        other => {
            return self_host_http_error_response(format!(
                "HttpServer handler must be a function reference, got {}",
                aver_rt::aver_display(&other)
            ));
        }
    };

    let Some(fns) = current_self_host_fn_store() else {
        return self_host_http_error_response(
            "Self-host HttpServer callback store is not active".to_string(),
        );
    };

    let Some(fd) = crate::aver_generated::domain::eval::lookupFnOption(fns.clone(), handler_name)
    else {
        return self_host_http_error_response(
            "Self-host HttpServer handler is not loaded".to_string(),
        );
    };

    let mut callback_args = Vec::new();
    if let Some(ctx) = context {
        callback_args.push(ctx);
    }
    callback_args.push(self_host_http_request_to_val(request));

    match crate::aver_generated::domain::eval::callResolved(
        fd,
        aver_rt::AverList::from_vec(callback_args),
        fns,
    ) {
        Ok(value) => match self_host_val_to_http_response(value) {
            Ok(resp) => resp,
            Err(err) => self_host_http_error_response(err.to_string()),
        },
        Err(err) => self_host_http_error_response(err.to_string()),
    }
}

fn self_host_http_request_to_val(
    request: aver_rt::HttpRequest,
) -> crate::aver_generated::domain::value::Val {
    use crate::aver_generated::domain::value::Val;

    Val::ValRecord(
        AverStr::from("HttpRequest"),
        aver_rt::AverList::from_vec(vec![
            (AverStr::from("method"), Val::ValStr(request.method)),
            (AverStr::from("path"), Val::ValStr(request.path)),
            (AverStr::from("body"), Val::ValStr(request.body)),
            (
                AverStr::from("headers"),
                Val::ValList(self_host_headers_to_val(request.headers)),
            ),
        ]),
    )
}

fn self_host_headers_to_val(
    headers: aver_rt::AverList<aver_rt::Header>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    use crate::aver_generated::domain::value::Val;

    aver_rt::AverList::from_vec(
        headers
            .to_vec()
            .into_iter()
            .map(|header| {
                Val::ValRecord(
                    AverStr::from("Header"),
                    aver_rt::AverList::from_vec(vec![
                        (AverStr::from("name"), Val::ValStr(header.name)),
                        (AverStr::from("value"), Val::ValStr(header.value)),
                    ]),
                )
            })
            .collect(),
    )
}

fn self_host_val_to_http_response(
    value: crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::HttpResponse, AverStr> {
    use crate::aver_generated::domain::value::Val;

    let Val::ValRecord(type_name, fields) = value else {
        return Err(AverStr::from(
            "HttpServer handler must return HttpResponse record",
        ));
    };

    if &*type_name != "HttpResponse" {
        return Err(AverStr::from(format!(
            "HttpServer handler must return HttpResponse, got {}",
            type_name
        )));
    }

    let mut status = None;
    let mut body = None;
    let mut headers = aver_rt::AverList::empty();

    for (field_name, field_value) in fields.to_vec() {
        match &*field_name {
            "status" => match field_value {
                Val::ValInt(n) => status = Some(n),
                _ => return Err(AverStr::from("HttpResponse.status must be Int")),
            },
            "body" => match field_value {
                Val::ValStr(s) => body = Some(s),
                _ => return Err(AverStr::from("HttpResponse.body must be String")),
            },
            "headers" => {
                headers = self_host_val_to_headers(field_value)?;
            }
            _ => {}
        }
    }

    Ok(aver_rt::HttpResponse {
        status: status.ok_or_else(|| AverStr::from("HttpResponse.status is required"))?,
        body: body.ok_or_else(|| AverStr::from("HttpResponse.body is required"))?,
        headers,
    })
}

fn self_host_val_to_headers(
    value: crate::aver_generated::domain::value::Val,
) -> Result<aver_rt::AverList<aver_rt::Header>, AverStr> {
    use crate::aver_generated::domain::value::Val;

    let Val::ValList(items) = value else {
        return Err(AverStr::from("HttpResponse.headers must be List<Header>"));
    };

    let mut out = Vec::new();
    for item in items.to_vec() {
        let Val::ValRecord(_, fields) = item else {
            return Err(AverStr::from(
                "HttpResponse.headers entries must be Header records",
            ));
        };

        let mut name = None;
        let mut value = None;
        for (field_name, field_value) in fields.to_vec() {
            match &*field_name {
                "name" => match field_value {
                    Val::ValStr(s) => name = Some(s),
                    _ => return Err(AverStr::from("Header.name must be String")),
                },
                "value" => match field_value {
                    Val::ValStr(s) => value = Some(s),
                    _ => return Err(AverStr::from("Header.value must be String")),
                },
                _ => {}
            }
        }

        out.push(aver_rt::Header {
            name: name.ok_or_else(|| AverStr::from("Header.name is required"))?,
            value: value.ok_or_else(|| AverStr::from("Header.value is required"))?,
        });
    }

    Ok(aver_rt::AverList::from_vec(out))
}

fn self_host_http_error_response(message: String) -> aver_rt::HttpResponse {
    aver_rt::HttpResponse {
        status: 500,
        body: AverStr::from(message),
        headers: aver_rt::AverList::empty(),
    }
}
"#;

/// Bring the shared Tcp connection type into the generated program under the
/// legacy codegen name used for dotted `Tcp.Connection`.
pub fn generate_tcp_types() -> String {
    "pub use aver_rt::TcpConnection as Tcp_Connection;".to_string()
}

/// Bring shared HTTP record types into the generated program.
pub fn generate_http_types() -> String {
    "pub use aver_rt::{Header, HttpResponse};".to_string()
}

/// Bring shared HTTP server request type into the generated program.
pub fn generate_http_server_types() -> String {
    "pub use aver_rt::HttpRequest;".to_string()
}
