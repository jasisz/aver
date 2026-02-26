use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::time::Duration;

use crate::value::{list_from_vec, list_to_vec, RuntimeError, Value};

#[derive(Debug, Clone)]
struct ServerRequest {
    method: String,
    path: String,
    body: String,
    headers: Vec<(String, String)>,
}

#[derive(Debug, Clone)]
struct ServerResponse {
    status: i64,
    body: String,
    headers: Vec<(String, String)>,
}

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
        "HttpServer.listen" | "HttpServer.listenWith" => &["HttpServer"],
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
) -> Option<Result<Value, RuntimeError>>
where
    F: FnMut(Value, Vec<Value>, String) -> Result<Value, RuntimeError>,
{
    match name {
        "HttpServer.listen" => Some(listen(args, false, &mut invoke_handler)),
        "HttpServer.listenWith" => Some(listen(args, true, &mut invoke_handler)),
        _ => None,
    }
}

fn listen<F>(
    args: &[Value],
    with_context: bool,
    invoke_handler: &mut F,
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

    let port = match &args[0] {
        Value::Int(n) if (0..=65535).contains(n) => *n as u16,
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
    let (context, handler) = if with_context {
        (Some(args[1].clone()), args[2].clone())
    } else {
        (None, args[1].clone())
    };

    let listener = TcpListener::bind(("0.0.0.0", port)).map_err(|e| {
        RuntimeError::Error(format!(
            "HttpServer.listen: failed to bind on {}: {}",
            port, e
        ))
    })?;

    for incoming in listener.incoming() {
        let mut stream = match incoming {
            Ok(s) => s,
            Err(e) => {
                return Err(RuntimeError::Error(format!(
                    "HttpServer.listen: failed to accept connection: {}",
                    e
                )));
            }
        };

        if let Err(e) = stream.set_read_timeout(Some(Duration::from_secs(30))) {
            let _ = write_http_response(
                &mut stream,
                &ServerResponse {
                    status: 500,
                    body: format!("HttpServer: failed to set read timeout: {}", e),
                    headers: vec![],
                },
            );
            continue;
        }
        if let Err(e) = stream.set_write_timeout(Some(Duration::from_secs(30))) {
            let _ = write_http_response(
                &mut stream,
                &ServerResponse {
                    status: 500,
                    body: format!("HttpServer: failed to set write timeout: {}", e),
                    headers: vec![],
                },
            );
            continue;
        }

        let request = match parse_http_request(&mut stream) {
            Ok(req) => req,
            Err(msg) => {
                let _ = write_http_response(
                    &mut stream,
                    &ServerResponse {
                        status: 400,
                        body: format!("Bad Request: {}", msg),
                        headers: vec![],
                    },
                );
                continue;
            }
        };

        let request_value = http_request_to_value(&request);
        let callback_entry = format!("<HttpServer {} {}>", request.method, request.path);
        let callback_args = match &context {
            Some(ctx) => vec![ctx.clone(), request_value],
            None => vec![request_value],
        };
        let callback_result = invoke_handler(handler.clone(), callback_args, callback_entry);

        let response = match callback_result {
            Ok(value) => match http_response_from_value(value) {
                Ok(resp) => resp,
                Err(e) => ServerResponse {
                    status: 500,
                    body: format!("HttpServer handler return error: {}", e),
                    headers: vec![],
                },
            },
            Err(e) => ServerResponse {
                status: 500,
                body: format!("HttpServer handler execution error: {}", e),
                headers: vec![],
            },
        };

        let _ = write_http_response(&mut stream, &response);
    }

    Ok(Value::Unit)
}

fn parse_http_request(stream: &mut TcpStream) -> Result<ServerRequest, String> {
    const BODY_LIMIT: usize = 10 * 1024 * 1024; // 10 MB

    let reader_stream = stream
        .try_clone()
        .map_err(|e| format!("cannot clone TCP stream: {}", e))?;
    let mut reader = BufReader::new(reader_stream);

    let mut request_line = String::new();
    let line_len = reader
        .read_line(&mut request_line)
        .map_err(|e| format!("cannot read request line: {}", e))?;
    if line_len == 0 {
        return Err("empty request".to_string());
    }

    let request_line = request_line.trim_end_matches(&['\r', '\n'][..]);
    let mut request_parts = request_line.split_whitespace();
    let method = request_parts
        .next()
        .ok_or_else(|| "missing HTTP method".to_string())?
        .to_string();
    let path = request_parts
        .next()
        .ok_or_else(|| "missing request path".to_string())?
        .to_string();
    let _version = request_parts
        .next()
        .ok_or_else(|| "missing HTTP version".to_string())?;

    let mut headers = Vec::new();
    let mut content_length = 0usize;

    loop {
        let mut line = String::new();
        let bytes = reader
            .read_line(&mut line)
            .map_err(|e| format!("cannot read header line: {}", e))?;
        if bytes == 0 {
            break;
        }

        let trimmed = line.trim_end_matches(&['\r', '\n'][..]);
        if trimmed.is_empty() {
            break;
        }

        let (name, value) = trimmed
            .split_once(':')
            .ok_or_else(|| format!("malformed header: '{}'", trimmed))?;
        let name = name.trim().to_string();
        let value = value.trim().to_string();

        if name.eq_ignore_ascii_case("Content-Length") {
            content_length = value
                .parse::<usize>()
                .map_err(|_| format!("invalid Content-Length value: '{}'", value))?;
            if content_length > BODY_LIMIT {
                return Err(format!("request body exceeds {} bytes limit", BODY_LIMIT));
            }
        }

        headers.push((name, value));
    }

    let mut body_bytes = vec![0_u8; content_length];
    if content_length > 0 {
        reader
            .read_exact(&mut body_bytes)
            .map_err(|e| format!("cannot read request body: {}", e))?;
    }
    let body = String::from_utf8_lossy(&body_bytes).into_owned();

    Ok(ServerRequest {
        method,
        path,
        body,
        headers,
    })
}

fn http_request_to_value(req: &ServerRequest) -> Value {
    let headers = req
        .headers
        .iter()
        .map(|(name, value)| Value::Record {
            type_name: "Header".to_string(),
            fields: vec![
                ("name".to_string(), Value::Str(name.clone())),
                ("value".to_string(), Value::Str(value.clone())),
            ],
        })
        .collect::<Vec<_>>();

    Value::Record {
        type_name: "HttpRequest".to_string(),
        fields: vec![
            ("method".to_string(), Value::Str(req.method.clone())),
            ("path".to_string(), Value::Str(req.path.clone())),
            ("body".to_string(), Value::Str(req.body.clone())),
            ("headers".to_string(), list_from_vec(headers)),
        ],
    }
}

fn http_response_from_value(val: Value) -> Result<ServerResponse, RuntimeError> {
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
    let mut headers = Vec::new();

    for (name, value) in fields {
        match name.as_str() {
            "status" => {
                if let Value::Int(n) = value {
                    status = Some(n);
                } else {
                    return Err(RuntimeError::Error(
                        "HttpResponse.status must be Int".to_string(),
                    ));
                }
            }
            "body" => {
                if let Value::Str(s) = value {
                    body = Some(s);
                } else {
                    return Err(RuntimeError::Error(
                        "HttpResponse.body must be String".to_string(),
                    ));
                }
            }
            "headers" => {
                headers = parse_http_response_headers(value)?;
            }
            _ => {}
        }
    }

    Ok(ServerResponse {
        status: status
            .ok_or_else(|| RuntimeError::Error("HttpResponse.status is required".to_string()))?,
        body: body
            .ok_or_else(|| RuntimeError::Error("HttpResponse.body is required".to_string()))?,
        headers,
    })
}

fn parse_http_response_headers(val: Value) -> Result<Vec<(String, String)>, RuntimeError> {
    let list = list_to_vec(&val).ok_or_else(|| {
        RuntimeError::Error("HttpResponse.headers must be List<Header>".to_string())
    })?;

    let mut out = Vec::new();
    for item in list {
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
        for (field_name, field_val) in fields {
            match (field_name.as_str(), field_val) {
                ("name", Value::Str(s)) => name = Some(s),
                ("value", Value::Str(s)) => value = Some(s),
                _ => {}
            }
        }

        let name = name.ok_or_else(|| {
            RuntimeError::Error("HttpResponse header missing String 'name'".to_string())
        })?;
        let value = value.ok_or_else(|| {
            RuntimeError::Error("HttpResponse header missing String 'value'".to_string())
        })?;
        out.push((name, value));
    }

    Ok(out)
}

fn status_reason(status: i64) -> &'static str {
    match status {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        304 => "Not Modified",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        405 => "Method Not Allowed",
        409 => "Conflict",
        422 => "Unprocessable Entity",
        429 => "Too Many Requests",
        500 => "Internal Server Error",
        501 => "Not Implemented",
        502 => "Bad Gateway",
        503 => "Service Unavailable",
        _ => "OK",
    }
}

fn write_http_response(stream: &mut TcpStream, response: &ServerResponse) -> std::io::Result<()> {
    let mut headers = response
        .headers
        .iter()
        .filter(|(name, _)| {
            !name.eq_ignore_ascii_case("Content-Length") && !name.eq_ignore_ascii_case("Connection")
        })
        .cloned()
        .collect::<Vec<_>>();

    if !headers
        .iter()
        .any(|(name, _)| name.eq_ignore_ascii_case("Content-Type"))
    {
        headers.push((
            "Content-Type".to_string(),
            "text/plain; charset=utf-8".to_string(),
        ));
    }

    headers.push((
        "Content-Length".to_string(),
        response.body.len().to_string(),
    ));
    headers.push(("Connection".to_string(), "close".to_string()));

    let mut head = format!(
        "HTTP/1.1 {} {}\r\n",
        response.status,
        status_reason(response.status)
    );
    for (name, value) in headers {
        head.push_str(&format!("{}: {}\r\n", name, value));
    }
    head.push_str("\r\n");

    stream.write_all(head.as_bytes())?;
    stream.write_all(response.body.as_bytes())?;
    stream.flush()?;
    Ok(())
}
