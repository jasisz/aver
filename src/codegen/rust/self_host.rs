/// Self-host-only support emitted into generated Rust projects.
///
/// This is intentionally separate from `runtime_support`: it depends on the
/// generated self-host evaluator/value model and is not part of normal Rust
/// codegen output.
pub fn generate_self_host_support() -> String {
    format!("{}\n", SELF_HOST_SUPPORT)
}

const SELF_HOST_SUPPORT: &str = r#"
use std::cell::RefCell;

use crate::AverStr;
use crate::aver_generated::domain::ast::FnDef;
use crate::aver_generated::domain::eval::callResolved;
use crate::aver_generated::domain::eval::store::{FnStore, fnsToStore, lookupFnOption};
use crate::aver_generated::domain::value::Val;

thread_local! {
    static SELF_HOST_FN_STORE: RefCell<Option<FnStore>> = const { RefCell::new(None) };
}

pub fn with_fn_store<T, F>(fns: FnStore, run: F) -> T
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

pub fn with_program_fn_store<T, F>(
    local_fns: crate::aver_rt::AverList<FnDef>,
    module_fns: crate::aver_rt::AverList<FnDef>,
    run: F,
) -> T
where
    F: FnOnce() -> T,
{
    let all_fns = crate::aver_rt::AverList::concat(&module_fns, &local_fns);
    let fns = fnsToStore(&all_fns);
    with_fn_store(fns, run)
}

fn current_fn_store() -> Option<FnStore> {
    SELF_HOST_FN_STORE.with(|cell| cell.borrow().clone())
}

pub fn http_server_listen(port: i64, handler: Val) -> Result<(), AverStr> {
    if crate::runtime_support::should_skip_http_server() {
        return Ok(());
    }
    crate::aver_rt::http_server::listen(port, move |request| {
        dispatch_http_handler(handler.clone(), None, request)
    })
    .map_err(AverStr::from)
}

pub fn http_server_listen_with(
    port: i64,
    context: Val,
    handler: Val,
) -> Result<(), AverStr> {
    if crate::runtime_support::should_skip_http_server() {
        return Ok(());
    }
    crate::aver_rt::http_server::listen_with(port, context, move |ctx, request| {
        dispatch_http_handler(handler.clone(), Some(ctx), request)
    })
    .map_err(AverStr::from)
}

fn dispatch_http_handler(
    handler: Val,
    context: Option<Val>,
    request: crate::aver_rt::HttpRequest,
) -> crate::aver_rt::HttpResponse {
    let handler_name = match handler {
        Val::ValFnRef(name) => name,
        other => {
            return http_error_response(format!(
                "HttpServer handler must be a function reference, got {}",
                crate::aver_rt::aver_display(&other)
            ));
        }
    };

    let Some(fns) = current_fn_store() else {
        return http_error_response("Self-host HttpServer callback store is not active".to_string());
    };

    let Some(fd) = lookupFnOption(&fns, handler_name) else {
        return http_error_response("Self-host HttpServer handler is not loaded".to_string());
    };

    let mut callback_args = Vec::new();
    if let Some(ctx) = context {
        callback_args.push(ctx);
    }
    callback_args.push(http_request_to_val(request));

    let args_list = crate::aver_rt::AverList::from_vec(callback_args);
    match callResolved(&fd, &args_list, &fns) {
        Ok(value) => match val_to_http_response(value) {
            Ok(resp) => resp,
            Err(err) => http_error_response(err.to_string()),
        },
        Err(err) => http_error_response(err.to_string()),
    }
}

fn http_request_to_val(request: crate::aver_rt::HttpRequest) -> Val {
    Val::ValRecord(
        AverStr::from("HttpRequest"),
        crate::aver_rt::AverList::from_vec(vec![
            (AverStr::from("method"), Val::ValStr(request.method)),
            (AverStr::from("path"), Val::ValStr(request.path)),
            (AverStr::from("body"), Val::ValStr(request.body)),
            (
                AverStr::from("headers"),
                Val::ValList(headers_to_val(request.headers)),
            ),
        ]),
    )
}

fn headers_to_val(headers: crate::aver_rt::AverList<crate::aver_rt::Header>) -> crate::aver_rt::AverList<Val> {
    crate::aver_rt::AverList::from_vec(
        headers
            .to_vec()
            .into_iter()
            .map(|header| {
                Val::ValRecord(
                    AverStr::from("Header"),
                    crate::aver_rt::AverList::from_vec(vec![
                        (AverStr::from("name"), Val::ValStr(header.name)),
                        (AverStr::from("value"), Val::ValStr(header.value)),
                    ]),
                )
            })
            .collect(),
    )
}

fn val_to_http_response(value: Val) -> Result<crate::aver_rt::HttpResponse, AverStr> {
    let Val::ValRecord(type_name, fields) = value else {
        return Err(AverStr::from("HttpServer handler must return HttpResponse record"));
    };

    if &*type_name != "HttpResponse" {
        return Err(AverStr::from(format!(
            "HttpServer handler must return HttpResponse, got {}",
            type_name
        )));
    }

    let mut status = None;
    let mut body = None;
    let mut headers = crate::aver_rt::AverList::empty();

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
                headers = val_to_headers(field_value)?;
            }
            _ => {}
        }
    }

    Ok(crate::aver_rt::HttpResponse {
        status: status.ok_or_else(|| AverStr::from("HttpResponse.status is required"))?,
        body: body.ok_or_else(|| AverStr::from("HttpResponse.body is required"))?,
        headers,
    })
}

fn val_to_headers(value: Val) -> Result<crate::aver_rt::AverList<crate::aver_rt::Header>, AverStr> {
    let Val::ValList(items) = value else {
        return Err(AverStr::from("HttpResponse.headers must be List<Header>"));
    };

    let mut out = Vec::new();
    for item in items.to_vec() {
        let Val::ValRecord(_, fields) = item else {
            return Err(AverStr::from("HttpResponse.headers entries must be Header records"));
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

        out.push(crate::aver_rt::Header {
            name: name.ok_or_else(|| AverStr::from("Header.name is required"))?,
            value: value.ok_or_else(|| AverStr::from("Header.value is required"))?,
        });
    }

    Ok(crate::aver_rt::AverList::from_vec(out))
}

fn http_error_response(message: String) -> crate::aver_rt::HttpResponse {
    crate::aver_rt::HttpResponse {
        status: 500,
        body: AverStr::from(message),
        headers: crate::aver_rt::AverList::empty(),
    }
}
"#;
