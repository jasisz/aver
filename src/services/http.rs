/// Http service — HTTP client built on `ureq`.
///
/// Exposes six methods mirroring the HTTP verb set:
///   GET / HEAD / DELETE  — `Http.get(url)`, `Http.head(url)`, `Http.delete(url)`
///   POST / PUT / PATCH   — `Http.post(url, body, contentType, headers)`, etc.
///
/// Each method requires its own exact effect (`Http.get`, `Http.post`, etc.). Responses are wrapped in `Ok(HttpResponse)`
/// for any completed HTTP exchange (including 4xx/5xx). Transport failures return
/// `Err(String)`. Response bodies are capped at 10 MB.
use std::collections::HashMap;

use aver_rt::{AverList, AverStr, HttpResponse};

use crate::nan_value::{Arena, NanValue, NanValueConvert};
use crate::value::{RuntimeError, Value, list_from_vec, list_view};

pub const DECLARED_EFFECTS: &[&str] = &[
    "Http.get",
    "Http.head",
    "Http.delete",
    "Http.post",
    "Http.put",
    "Http.patch",
];

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Http.get" | "Http.head" | "Http.delete" => Some(call_simple(name, args)),
        "Http.post" | "Http.put" | "Http.patch" => Some(call_with_body(name, args)),
        _ => None,
    }
}

fn call_simple(name: &str, args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Http.{}() takes 1 argument (url), got {}",
            name.trim_start_matches("Http."),
            args.len()
        )));
    }
    let url = str_arg(&args[0], "Http: url must be a String")?;
    let result = match name {
        "Http.get" => aver_rt::http::get(&url),
        "Http.head" => aver_rt::http::head(&url),
        "Http.delete" => aver_rt::http::delete(&url),
        _ => unreachable!(),
    };
    response_value(result)
}

fn call_with_body(name: &str, args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 4 {
        return Err(RuntimeError::Error(format!(
            "Http.{}() takes 4 arguments (url, body, contentType, headers), got {}",
            name.trim_start_matches("Http."),
            args.len()
        )));
    }
    let url = str_arg(&args[0], "Http: url must be a String")?;
    let body = str_arg(&args[1], "Http: body must be a String")?;
    let content_type = str_arg(&args[2], "Http: contentType must be a String")?;
    let extra_headers = parse_request_headers(&args[3])?;

    let result = match name {
        "Http.post" => aver_rt::http::post(&url, &body, &content_type, &extra_headers),
        "Http.put" => aver_rt::http::put(&url, &body, &content_type, &extra_headers),
        "Http.patch" => aver_rt::http::patch(&url, &body, &content_type, &extra_headers),
        _ => unreachable!(),
    };
    response_value(result)
}

fn str_arg(val: &Value, msg: &str) -> Result<String, RuntimeError> {
    match val {
        Value::Str(s) => Ok(s.clone()),
        _ => Err(RuntimeError::Error(msg.to_string())),
    }
}

fn parse_request_headers(val: &Value) -> Result<aver_rt::HttpHeaders, RuntimeError> {
    let map = match val {
        Value::Map(m) => m,
        _ => {
            return Err(RuntimeError::Error(
                "Http: headers must be a Map<String, List<String>>".to_string(),
            ));
        }
    };
    let mut out = aver_rt::HttpHeaders::default();
    for (k, v) in map.iter() {
        let name = match k {
            Value::Str(s) => s.to_ascii_lowercase(),
            _ => {
                return Err(RuntimeError::Error(
                    "Http: header map keys must be Strings".to_string(),
                ));
            }
        };
        let values = list_view(v)
            .ok_or_else(|| RuntimeError::Error("Http: header values must be a List".to_string()))?;
        let mut buf = Vec::new();
        for item in values.iter() {
            match item {
                Value::Str(s) => buf.push(AverStr::from(s.as_str())),
                _ => {
                    return Err(RuntimeError::Error(
                        "Http: header values must be Strings".to_string(),
                    ));
                }
            }
        }
        out = out.insert_owned(AverStr::from(name), AverList::from_vec(buf));
    }
    Ok(out)
}

fn response_value(result: Result<HttpResponse, String>) -> Result<Value, RuntimeError> {
    match result {
        Ok(resp) => Ok(Value::Ok(Box::new(http_response_to_value(resp)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

/// Bridge: convert NanValue args to Value, call old implementation, convert result back.
pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    // Check if this name is owned by us
    if !matches!(
        name,
        "Http.get" | "Http.head" | "Http.delete" | "Http.post" | "Http.put" | "Http.patch"
    ) {
        return None;
    }
    let old_args: Vec<Value> = args.iter().map(|nv| nv.to_value(arena)).collect();
    let result = call(name, &old_args)?;
    Some(result.map(|v| NanValue::from_value(&v, arena)))
}

fn http_response_to_value(resp: HttpResponse) -> Value {
    let mut headers = HashMap::new();
    for (name, values) in resp.headers.iter() {
        let value_list: Vec<Value> = values.iter().map(|v| Value::Str(v.to_string())).collect();
        headers.insert(Value::Str(name.to_string()), list_from_vec(value_list));
    }

    Value::Record {
        type_name: "HttpResponse".to_string(),
        fields: vec![
            ("status".to_string(), Value::int(resp.status)),
            ("body".to_string(), Value::Str(resp.body.to_string())),
            ("headers".to_string(), Value::Map(headers)),
        ]
        .into(),
    }
}
