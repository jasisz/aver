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

use aver_rt::{AverList, Header, HttpResponse};

use crate::value::{RuntimeError, Value, list_from_vec, list_view};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["get", "head", "delete", "post", "put", "patch"] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Http.{}", method)),
        );
    }
    global.insert(
        "Http".to_string(),
        Value::Namespace {
            name: "Http".to_string(),
            members,
        },
    );
}

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Http.get" => &["Http.get"],
        "Http.head" => &["Http.head"],
        "Http.delete" => &["Http.delete"],
        "Http.post" => &["Http.post"],
        "Http.put" => &["Http.put"],
        "Http.patch" => &["Http.patch"],
        _ => &[],
    }
}

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

fn parse_request_headers(val: &Value) -> Result<AverList<Header>, RuntimeError> {
    let items = list_view(val)
        .ok_or_else(|| RuntimeError::Error("Http: headers must be a List".to_string()))?;
    let mut out = Vec::new();
    for item in items.iter() {
        let fields = match item {
            Value::Record { fields, .. } => fields,
            _ => {
                return Err(RuntimeError::Error(
                    "Http: each header must be a record with 'name' and 'value' String fields"
                        .to_string(),
                ));
            }
        };
        let get = |key: &str| -> Result<String, RuntimeError> {
            fields
                .iter()
                .find(|(k, _)| k == key)
                .and_then(|(_, v)| {
                    if let Value::Str(s) = v {
                        Some(s.clone())
                    } else {
                        None
                    }
                })
                .ok_or_else(|| {
                    RuntimeError::Error(format!(
                        "Http: header record must have a '{}' String field",
                        key
                    ))
                })
        };
        out.push(Header {
            name: get("name")?,
            value: get("value")?,
        });
    }
    Ok(AverList::from_vec(out))
}

fn response_value(result: Result<HttpResponse, String>) -> Result<Value, RuntimeError> {
    match result {
        Ok(resp) => Ok(Value::Ok(Box::new(http_response_to_value(resp)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

fn http_response_to_value(resp: HttpResponse) -> Value {
    let headers = resp
        .headers
        .into_iter()
        .map(|header| Value::Record {
            type_name: "Header".to_string(),
            fields: vec![
                ("name".to_string(), Value::Str(header.name)),
                ("value".to_string(), Value::Str(header.value)),
            ],
        })
        .collect();

    Value::Record {
        type_name: "HttpResponse".to_string(),
        fields: vec![
            ("status".to_string(), Value::Int(resp.status)),
            ("body".to_string(), Value::Str(resp.body)),
            ("headers".to_string(), list_from_vec(headers)),
        ],
    }
}
