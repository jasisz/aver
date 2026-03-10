use std::collections::HashMap;

use aver_rt::{AverList, Header, HttpRequest, HttpResponse};

use crate::value::{RuntimeError, Value, list_from_vec, list_view};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    members.insert(
        "listen".to_string(),
        Value::Builtin("HttpServer.listen".to_string()),
    );
    members.insert(
        "listenWith".to_string(),
        Value::Builtin("HttpServer.listenWith".to_string()),
    );
    global.insert(
        "HttpServer".to_string(),
        Value::Namespace {
            name: "HttpServer".to_string(),
            members,
        },
    );
}

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "HttpServer.listen" => &["HttpServer.listen"],
        "HttpServer.listenWith" => &["HttpServer.listenWith"],
        _ => &[],
    }
}

pub fn call(_name: &str, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    None
}

pub fn call_with_runtime<F>(
    name: &str,
    args: &[Value],
    mut invoke_handler: F,
    skip_server: bool,
) -> Option<Result<Value, RuntimeError>>
where
    F: FnMut(Value, Vec<Value>, String) -> Result<Value, RuntimeError>,
{
    match name {
        "HttpServer.listen" => Some(listen(args, false, &mut invoke_handler, skip_server)),
        "HttpServer.listenWith" => Some(listen(args, true, &mut invoke_handler, skip_server)),
        _ => None,
    }
}

fn listen<F>(
    args: &[Value],
    with_context: bool,
    invoke_handler: &mut F,
    skip_server: bool,
) -> Result<Value, RuntimeError>
where
    F: FnMut(Value, Vec<Value>, String) -> Result<Value, RuntimeError>,
{
    let expected = if with_context { 3 } else { 2 };
    if args.len() != expected {
        let sig = if with_context {
            "HttpServer.listenWith(port, context, handler)"
        } else {
            "HttpServer.listen(port, handler)"
        };
        return Err(RuntimeError::Error(format!(
            "{} expects {} arguments, got {}",
            sig,
            expected,
            args.len()
        )));
    }

    if skip_server {
        return Ok(Value::Unit);
    }

    let port = match &args[0] {
        Value::Int(n) if (0..=65535).contains(n) => *n,
        Value::Int(n) => {
            return Err(RuntimeError::Error(format!(
                "HttpServer.listen: port {} is out of range (0-65535)",
                n
            )));
        }
        _ => {
            return Err(RuntimeError::Error(
                "HttpServer.listen: port must be an Int".to_string(),
            ));
        }
    };
    let handler = if with_context {
        args[2].clone()
    } else {
        args[1].clone()
    };

    let result = if with_context {
        let context = args[1].clone();
        aver_rt::http_server::listen_with(port, context, |ctx, request| {
            dispatch_handler(&handler, Some(ctx), request, invoke_handler)
        })
    } else {
        aver_rt::http_server::listen(port, |request| {
            dispatch_handler(&handler, None, request, invoke_handler)
        })
    };

    result.map_err(RuntimeError::Error)?;
    Ok(Value::Unit)
}

fn dispatch_handler<F>(
    handler: &Value,
    context: Option<Value>,
    request: HttpRequest,
    invoke_handler: &mut F,
) -> HttpResponse
where
    F: FnMut(Value, Vec<Value>, String) -> Result<Value, RuntimeError>,
{
    let callback_entry = format!("<HttpServer {} {}>", request.method, request.path);
    let mut callback_args = Vec::new();
    if let Some(ctx) = context {
        callback_args.push(ctx);
    }
    callback_args.push(http_request_to_value(request));

    let callback_result = invoke_handler(handler.clone(), callback_args, callback_entry);
    match callback_result {
        Ok(value) => match http_response_from_value(value) {
            Ok(resp) => resp,
            Err(e) => HttpResponse {
                status: 500,
                body: format!("HttpServer handler return error: {}", e),
                headers: AverList::empty(),
            },
        },
        Err(e) => HttpResponse {
            status: 500,
            body: format!("HttpServer handler execution error: {}", e),
            headers: AverList::empty(),
        },
    }
}

fn http_request_to_value(req: HttpRequest) -> Value {
    let headers = req
        .headers
        .into_iter()
        .map(|header| Value::Record {
            type_name: "Header".to_string(),
            fields: vec![
                ("name".to_string(), Value::Str(header.name)),
                ("value".to_string(), Value::Str(header.value)),
            ]
            .into(),
        })
        .collect::<Vec<_>>();

    Value::Record {
        type_name: "HttpRequest".to_string(),
        fields: vec![
            ("method".to_string(), Value::Str(req.method)),
            ("path".to_string(), Value::Str(req.path)),
            ("body".to_string(), Value::Str(req.body)),
            ("headers".to_string(), list_from_vec(headers)),
        ]
        .into(),
    }
}

fn http_response_from_value(val: Value) -> Result<HttpResponse, RuntimeError> {
    let (type_name, fields) = match val {
        Value::Record { type_name, fields } => (type_name, fields),
        _ => {
            return Err(RuntimeError::Error(
                "HttpServer handler must return HttpResponse record".to_string(),
            ));
        }
    };

    if type_name != "HttpResponse" {
        return Err(RuntimeError::Error(format!(
            "HttpServer handler must return HttpResponse, got {}",
            type_name
        )));
    }

    let mut status = None;
    let mut body = None;
    let mut headers = AverList::empty();

    for (name, value) in fields.iter() {
        match name.as_str() {
            "status" => {
                if let Value::Int(n) = value {
                    status = Some(*n);
                } else {
                    return Err(RuntimeError::Error(
                        "HttpResponse.status must be Int".to_string(),
                    ));
                }
            }
            "body" => {
                if let Value::Str(s) = value {
                    body = Some(s.clone());
                } else {
                    return Err(RuntimeError::Error(
                        "HttpResponse.body must be String".to_string(),
                    ));
                }
            }
            "headers" => {
                headers = parse_http_response_headers(value.clone())?;
            }
            _ => {}
        }
    }

    Ok(HttpResponse {
        status: status
            .ok_or_else(|| RuntimeError::Error("HttpResponse.status is required".to_string()))?,
        body: body
            .ok_or_else(|| RuntimeError::Error("HttpResponse.body is required".to_string()))?,
        headers,
    })
}

fn parse_http_response_headers(val: Value) -> Result<AverList<Header>, RuntimeError> {
    let list = list_view(&val).ok_or_else(|| {
        RuntimeError::Error("HttpResponse.headers must be List<Header>".to_string())
    })?;

    let mut out = Vec::new();
    for item in list.iter() {
        let fields = match item {
            Value::Record { fields, .. } => fields,
            _ => {
                return Err(RuntimeError::Error(
                    "HttpResponse.headers entries must be Header records".to_string(),
                ));
            }
        };

        let mut name = None;
        let mut value = None;
        for (field_name, field_val) in fields.iter() {
            match (field_name.as_str(), field_val) {
                ("name", Value::Str(s)) => name = Some(s.clone()),
                ("value", Value::Str(s)) => value = Some(s.clone()),
                _ => {}
            }
        }

        let name = name.ok_or_else(|| {
            RuntimeError::Error("HttpResponse header missing String 'name'".to_string())
        })?;
        let value = value.ok_or_else(|| {
            RuntimeError::Error("HttpResponse header missing String 'value'".to_string())
        })?;
        out.push(Header { name, value });
    }

    Ok(AverList::from_vec(out))
}
