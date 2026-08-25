//! HTTP namespace dispatch + helper machinery. `http_simple_dispatch`
//! handles GET/HEAD/DELETE (no body), `http_body_dispatch` handles
//! POST/PUT/PATCH (body + content-type). Both route through
//! `aver_rt::http::*` for real calls and through `try_replay` for
//! trace-driven replay, then materialise the response via the
//! `host_*_make` factories. The `http_outcome_*` functions translate
//! between `aver_rt::HttpResponse` and the recorder marker shape.

use super::super::RunWasmGcHost;
use super::super::decode::decode_result_http_response;
use super::factories::{
    host_http_response_make, host_map_string_list_string_empty, host_result_http_response_err,
    host_result_http_response_ok,
};
use super::lm::{lm_string_from_host, lm_string_to_host};
use super::replay_glue::{json_err, json_ok, json_record, record_effect_if_recording, try_replay};

#[derive(Clone, Copy)]
pub(crate) enum HttpVerb {
    Get,
    Head,
    Delete,
    Post,
    Put,
    Patch,
}

pub(crate) fn http_simple_dispatch(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    verb: HttpVerb,
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    let url = lm_string_to_host(caller, params.first())?.unwrap_or_default();
    let effect_name = match verb {
        HttpVerb::Get => "Http.get",
        HttpVerb::Head => "Http.head",
        HttpVerb::Delete => "Http.delete",
        _ => unreachable!(),
    };
    let args = vec![aver::replay::JsonValue::String(url.clone())];
    if let Some(cached) = try_replay(caller, effect_name, args.clone())? {
        let r = decode_result_http_response(caller, &cached)?;
        results[0] = Val::AnyRef(r);
        return Ok(true);
    }
    let outcome = match verb {
        HttpVerb::Get => aver_rt::http::get(&url),
        HttpVerb::Head => aver_rt::http::head(&url),
        HttpVerb::Delete => aver_rt::http::delete(&url),
        _ => unreachable!(),
    };
    let trace_outcome = http_outcome_to_json(&outcome);
    let result_ref = http_outcome_to_result(caller, outcome)?;
    results[0] = Val::AnyRef(result_ref);
    record_effect_if_recording(caller, effect_name, args, trace_outcome, caller_fn);
    Ok(true)
}

pub(crate) fn http_body_dispatch(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    verb: HttpVerb,
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    let url = lm_string_to_host(caller, params.first())?.unwrap_or_default();
    let body = lm_string_to_host(caller, params.get(1))?.unwrap_or_default();
    let content_type = lm_string_to_host(caller, params.get(2))?.unwrap_or_default();
    // Headers map (params[3]) is opaque to us today — aver_rt::http::*
    // takes empty headers; the user's map crosses but the host doesn't
    // unpack it yet. Verbs whose status / body we report back are still
    // useful even without forwarding extra request headers, and the
    // common Authorization-via-URL or body-encoded payload paths work.
    let _ = params.get(3);
    let effect_name = match verb {
        HttpVerb::Post => "Http.post",
        HttpVerb::Put => "Http.put",
        HttpVerb::Patch => "Http.patch",
        _ => unreachable!(),
    };
    // Headers map argument is recorded as an empty `Map<String, List<String>>`
    // — the host doesn't unpack the user's wasm-gc map ref yet, so the
    // trace shows the same shape the host actually forwards.
    let empty_headers = aver::replay::JsonValue::Object(std::collections::BTreeMap::new());
    let args = vec![
        aver::replay::JsonValue::String(url.clone()),
        aver::replay::JsonValue::String(body.clone()),
        aver::replay::JsonValue::String(content_type.clone()),
        empty_headers,
    ];
    if let Some(cached) = try_replay(caller, effect_name, args.clone())? {
        let r = decode_result_http_response(caller, &cached)?;
        results[0] = Val::AnyRef(r);
        return Ok(true);
    }
    let outcome = match verb {
        HttpVerb::Post => aver_rt::http::post(&url, &body, &content_type, &Default::default()),
        HttpVerb::Put => aver_rt::http::put(&url, &body, &content_type, &Default::default()),
        HttpVerb::Patch => aver_rt::http::patch(&url, &body, &content_type, &Default::default()),
        _ => unreachable!(),
    };
    let trace_outcome = http_outcome_to_json(&outcome);
    let result_ref = http_outcome_to_result(caller, outcome)?;
    results[0] = Val::AnyRef(result_ref);
    record_effect_if_recording(caller, effect_name, args, trace_outcome, caller_fn);
    Ok(true)
}

pub(crate) fn http_outcome_to_result(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    outcome: Result<aver_rt::HttpResponse, String>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    match outcome {
        Ok(resp) => {
            let body_ref = lm_string_from_host(caller, resp.body.as_ref())?;
            let headers_ref = host_map_string_list_string_empty(caller)?;
            let rec_ref = host_http_response_make(caller, resp.status, body_ref, headers_ref)?;
            host_result_http_response_ok(caller, rec_ref)
        }
        Err(e) => host_result_http_response_err(caller, &e),
    }
}

/// Mirror an `aver_rt::HttpResponse` outcome into the JSON shape the
/// VM recorder uses (`{"$ok": {"$record": {"type": "Http.Response",
/// "fields": {"status": …, "body": …, "headers": {}}}}}`). Headers
/// always serialise as an empty `Map<String, List<String>>` for now —
/// `aver_rt::HttpResponse` carries the fields the host bridge
/// populates, and the host doesn't expose response headers yet (same
/// gap the wasm record-construction path has).
pub(crate) fn http_outcome_to_json(
    outcome: &Result<aver_rt::HttpResponse, String>,
) -> aver::replay::JsonValue {
    match outcome {
        Ok(resp) => json_ok(json_record(
            "Http.Response",
            vec![
                ("status", aver::replay::JsonValue::Int(resp.status)),
                (
                    "body",
                    aver::replay::JsonValue::String(resp.body.as_ref().to_string()),
                ),
                (
                    "headers",
                    aver::replay::JsonValue::Object(std::collections::BTreeMap::new()),
                ),
            ],
        )),
        Err(e) => json_err(e),
    }
}
