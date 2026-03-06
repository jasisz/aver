use crate::{AverList, Header, HttpRequest, HttpResponse};
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::time::Duration;

pub fn listen<F>(port: i64, mut handler: F) -> Result<(), String>
where
    F: FnMut(HttpRequest) -> HttpResponse,
{
    let listener = bind_listener(port)?;
    for incoming in listener.incoming() {
        let mut stream = incoming.map_err(|e| format!("HttpServer.listen: failed to accept connection: {}", e))?;
        serve_one(&mut stream, &mut handler);
    }
    Ok(())
}

pub fn listen_with<C, F>(port: i64, context: C, mut handler: F) -> Result<(), String>
where
    C: Clone,
    F: FnMut(C, HttpRequest) -> HttpResponse,
{
    let listener = bind_listener(port)?;
    for incoming in listener.incoming() {
        let mut stream = incoming.map_err(|e| format!("HttpServer.listen: failed to accept connection: {}", e))?;
        let ctx = context.clone();
        serve_one(&mut stream, |request| handler(ctx.clone(), request));
    }
    Ok(())
}

fn bind_listener(port: i64) -> Result<TcpListener, String> {
    TcpListener::bind(format!("0.0.0.0:{}", port))
        .map_err(|e| format!("HttpServer.listen: failed to bind on {}: {}", port, e))
}

fn serve_one<F>(stream: &mut TcpStream, mut handler: F)
where
    F: FnMut(HttpRequest) -> HttpResponse,
{
    if let Err(e) = stream.set_read_timeout(Some(Duration::from_secs(30))) {
        let _ = write_http_response(
            stream,
            &HttpResponse {
                status: 500,
                body: format!("HttpServer: failed to set read timeout: {}", e),
                headers: AverList::empty(),
            },
        );
        return;
    }
    if let Err(e) = stream.set_write_timeout(Some(Duration::from_secs(30))) {
        let _ = write_http_response(
            stream,
            &HttpResponse {
                status: 500,
                body: format!("HttpServer: failed to set write timeout: {}", e),
                headers: AverList::empty(),
            },
        );
        return;
    }

    let request = match parse_http_request(stream) {
        Ok(req) => req,
        Err(msg) => {
            let _ = write_http_response(
                stream,
                &HttpResponse {
                    status: 400,
                    body: format!("Bad Request: {}", msg),
                    headers: AverList::empty(),
                },
            );
            return;
        }
    };

    let response = handler(request);
    let _ = write_http_response(stream, &response);
}

fn parse_http_request(stream: &mut TcpStream) -> Result<HttpRequest, String> {
    const BODY_LIMIT: usize = 10 * 1024 * 1024;

    let reader_stream = stream.try_clone().map_err(|e| format!("cannot clone TCP stream: {}", e))?;
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
    request_parts
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

        headers.push(Header { name, value });
    }

    let mut body_bytes = vec![0_u8; content_length];
    if content_length > 0 {
        reader
            .read_exact(&mut body_bytes)
            .map_err(|e| format!("cannot read request body: {}", e))?;
    }
    let body = String::from_utf8_lossy(&body_bytes).into_owned();

    Ok(HttpRequest {
        method,
        path,
        body,
        headers: AverList::from_vec(headers),
    })
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

fn write_http_response(stream: &mut TcpStream, response: &HttpResponse) -> std::io::Result<()> {
    let mut headers = response
        .headers
        .iter()
        .filter(|h| {
            !h.name.eq_ignore_ascii_case("Content-Length")
                && !h.name.eq_ignore_ascii_case("Connection")
        })
        .map(|h| (h.name.clone(), h.value.clone()))
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
