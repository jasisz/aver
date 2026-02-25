/// Http service — HTTP client built on `ureq`.
///
/// Exposes six methods mirroring the HTTP verb set:
///   GET / HEAD / DELETE  — `Http.get(url)`, `Http.head(url)`, `Http.delete(url)`
///   POST / PUT / PATCH   — `Http.post(url, body, contentType, headers)`, etc.
///
/// All methods require `! [Http]`. Responses are wrapped in `Ok(HttpResponse)`
/// for any completed HTTP exchange (including 4xx/5xx). Transport failures return
/// `Err(String)`. Response bodies are capped at 10 MB.
use std::collections::HashMap;

use crate::value::{RuntimeError, Value};

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
        "Http.get"
        | "Http.head"
        | "Http.delete"
        | "Http.post"
        | "Http.put"
        | "Http.patch" => &["Http"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: Vec<Value>) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Http.get" | "Http.head" | "Http.delete" => Some(call_simple(name, args)),
        "Http.post" | "Http.put" | "Http.patch" => Some(call_with_body(name, args)),
        _ => None,
    }
}

// ─── Private helpers ──────────────────────────────────────────────────────────

fn call_simple(name: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Http.{}() takes 1 argument (url), got {}",
            name.trim_start_matches("Http."),
            args.len()
        )));
    }
    let url = str_arg(&args[0], "Http: url must be a String")?;
    let method = name.trim_start_matches("Http.").to_uppercase();
    let result = ureq::request(&method, &url)
        .timeout(std::time::Duration::from_secs(10))
        .call();
    response_value(result)
}

fn call_with_body(name: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 4 {
        return Err(RuntimeError::Error(format!(
            "Http.{}() takes 4 arguments (url, body, contentType, headers), got {}",
            name.trim_start_matches("Http."),
            args.len()
        )));
    }
    let url          = str_arg(&args[0], "Http: url must be a String")?;
    let body         = str_arg(&args[1], "Http: body must be a String")?;
    let content_type = str_arg(&args[2], "Http: contentType must be a String")?;
    let extra_headers = parse_request_headers(&args[3])?;

    let method = name.trim_start_matches("Http.").to_uppercase();
    let mut req = ureq::request(&method, &url)
        .timeout(std::time::Duration::from_secs(10))
        .set("Content-Type", &content_type);
    for (k, v) in &extra_headers {
        req = req.set(k, v);
    }
    response_value(req.send_string(&body))
}

fn str_arg(val: &Value, msg: &str) -> Result<String, RuntimeError> {
    match val {
        Value::Str(s) => Ok(s.clone()),
        _ => Err(RuntimeError::Error(msg.to_string())),
    }
}

fn parse_request_headers(val: &Value) -> Result<Vec<(String, String)>, RuntimeError> {
    let items = match val {
        Value::List(items) => items,
        _ => return Err(RuntimeError::Error("Http: headers must be a List".to_string())),
    };
    let mut out = Vec::new();
    for item in items {
        let fields = match item {
            Value::Record { fields, .. } => fields,
            _ => return Err(RuntimeError::Error(
                "Http: each header must be a record with 'name' and 'value' String fields".to_string(),
            )),
        };
        let get = |key: &str| -> Result<String, RuntimeError> {
            fields
                .iter()
                .find(|(k, _)| k == key)
                .and_then(|(_, v)| if let Value::Str(s) = v { Some(s.clone()) } else { None })
                .ok_or_else(|| RuntimeError::Error(format!(
                    "Http: header record must have a '{}' String field", key
                )))
        };
        out.push((get("name")?, get("value")?));
    }
    Ok(out)
}

fn response_value(result: Result<ureq::Response, ureq::Error>) -> Result<Value, RuntimeError> {
    match result {
        Ok(resp) => build_response(resp),
        Err(ureq::Error::Status(_, resp)) => build_response(resp),
        Err(ureq::Error::Transport(e)) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn build_response(resp: ureq::Response) -> Result<Value, RuntimeError> {
    use std::io::Read;
    let status = resp.status() as i64;
    let header_names = resp.headers_names();
    let headers: Vec<Value> = header_names
        .iter()
        .map(|name| {
            let value = resp.header(name).unwrap_or("").to_string();
            Value::Record {
                type_name: "Header".to_string(),
                fields: vec![
                    ("name".to_string(), Value::Str(name.clone())),
                    ("value".to_string(), Value::Str(value)),
                ],
            }
        })
        .collect();

    const BODY_LIMIT: u64 = 10 * 1024 * 1024; // 10 MB
    let mut buf = Vec::new();
    let bytes_read = resp
        .into_reader()
        .take(BODY_LIMIT + 1)
        .read_to_end(&mut buf)
        .map_err(|e| RuntimeError::Error(format!("Http: failed to read response body: {}", e)))?;
    if bytes_read as u64 > BODY_LIMIT {
        return Ok(Value::Err(Box::new(Value::Str(
            "Http: response body exceeds 10 MB limit".to_string(),
        ))));
    }
    let body = String::from_utf8_lossy(&buf).into_owned();
    Ok(Value::Ok(Box::new(Value::Record {
        type_name: "HttpResponse".to_string(),
        fields: vec![
            ("status".to_string(), Value::Int(status)),
            ("body".to_string(), Value::Str(body)),
            ("headers".to_string(), Value::List(headers)),
        ],
    })))
}
