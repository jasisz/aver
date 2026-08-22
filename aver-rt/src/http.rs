use crate::{AverList, AverStr, HttpHeaders, HttpResponse};

pub fn get(url: &str) -> Result<HttpResponse, String> {
    simple_request("GET", url)
}

pub fn head(url: &str) -> Result<HttpResponse, String> {
    simple_request("HEAD", url)
}

pub fn delete(url: &str) -> Result<HttpResponse, String> {
    simple_request("DELETE", url)
}

pub fn post(
    url: &str,
    body: &str,
    content_type: &str,
    headers: &HttpHeaders,
) -> Result<HttpResponse, String> {
    body_request("POST", url, body, content_type, headers)
}

pub fn put(
    url: &str,
    body: &str,
    content_type: &str,
    headers: &HttpHeaders,
) -> Result<HttpResponse, String> {
    body_request("PUT", url, body, content_type, headers)
}

pub fn patch(
    url: &str,
    body: &str,
    content_type: &str,
    headers: &HttpHeaders,
) -> Result<HttpResponse, String> {
    body_request("PATCH", url, body, content_type, headers)
}

fn simple_request(method: &str, url: &str) -> Result<HttpResponse, String> {
    let mut req = ureq::request(method, url).timeout(std::time::Duration::from_secs(10));
    if is_head(method) {
        // Unless we name an encoding ourselves, the client offers `gzip` on
        // every request. A server answering HEAD repeats the header fields of
        // the equivalent GET, so it answers `Content-Encoding: gzip` with no
        // body at all -- and the client drops `Content-Length` from any
        // response it intends to decompress, which is the one field a HEAD
        // request usually asks for. Ask for the identity encoding so the sizes
        // we report describe the resource itself.
        req = req.set("Accept-Encoding", "identity");
    }
    response_value(method, req.call())
}

fn body_request(
    method: &str,
    url: &str,
    body: &str,
    content_type: &str,
    headers: &HttpHeaders,
) -> Result<HttpResponse, String> {
    let mut req = ureq::request(method, url)
        .timeout(std::time::Duration::from_secs(10))
        .set("Content-Type", content_type);
    for (name, values) in headers.iter() {
        for value in values.iter() {
            req = req.set(name.as_ref(), value.as_ref());
        }
    }
    response_value(method, req.send_string(body))
}

fn is_head(method: &str) -> bool {
    method.eq_ignore_ascii_case("HEAD")
}

/// A HEAD response, a `204 No Content` and a `304 Not Modified` carry the
/// header fields of a full response but never a body. Reading one is not
/// merely wasteful: when the headers announce a compressed encoding the
/// decoder is handed an empty stream and reports a truncated body.
fn response_has_no_body(method: &str, status: i64) -> bool {
    is_head(method) || status == 204 || status == 304
}

fn response_value(
    method: &str,
    result: Result<ureq::Response, ureq::Error>,
) -> Result<HttpResponse, String> {
    match result {
        Ok(resp) => build_response(method, resp),
        Err(ureq::Error::Status(_, resp)) => build_response(method, resp),
        Err(ureq::Error::Transport(e)) => Err(e.to_string()),
    }
}

fn build_response(method: &str, resp: ureq::Response) -> Result<HttpResponse, String> {
    use std::io::Read;

    const BODY_LIMIT: u64 = 10 * 1024 * 1024;

    let status = resp.status() as i64;
    let header_names = resp.headers_names();
    // Build Map<lowercase-name, List<value>>. ureq deduplicates same-name
    // headers, so each `name` here is unique already; we still wrap in a
    // single-element list to match the multi-value type shape.
    let mut headers: HttpHeaders = HttpHeaders::default();
    for name in &header_names {
        let value = AverStr::from(resp.header(name).unwrap_or(""));
        let key = AverStr::from(name.to_ascii_lowercase());
        let entry = match headers.get(&key) {
            Some(existing) => AverList::prepend(value, existing).reverse(),
            None => AverList::from_vec(vec![value]),
        };
        headers = headers.insert_owned(key, entry);
    }

    let body = if response_has_no_body(method, status) {
        AverStr::from("")
    } else {
        let mut buf = Vec::new();
        let bytes_read = resp
            .into_reader()
            .take(BODY_LIMIT + 1)
            .read_to_end(&mut buf)
            .map_err(|e| format!("Http: failed to read response body: {}", e))?;
        if bytes_read as u64 > BODY_LIMIT {
            return Err("Http: response body exceeds 10 MB limit".to_string());
        }
        AverStr::from(String::from_utf8_lossy(&buf).into_owned())
    };
    Ok(HttpResponse {
        status,
        body,
        headers,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{BufRead, BufReader, Write};
    use std::net::TcpListener;
    use std::thread;

    /// Serve exactly one request on loopback. `respond` is handed the request
    /// head (request line first, then the header lines) and returns the raw
    /// bytes written back before the socket closes.
    fn loopback_server(
        respond: impl FnOnce(&[String]) -> Vec<u8> + Send + 'static,
    ) -> (String, thread::JoinHandle<()>) {
        let listener = TcpListener::bind(("127.0.0.1", 0)).expect("bind loopback listener");
        let port = listener.local_addr().expect("listener address").port();
        let server = thread::spawn(move || {
            let (stream, _) = listener.accept().expect("accept loopback connection");
            let mut reader = BufReader::new(stream);
            let mut head = Vec::new();
            loop {
                let mut line = String::new();
                if reader.read_line(&mut line).expect("read request head") == 0 {
                    break;
                }
                let line = line.trim_end().to_string();
                if line.is_empty() {
                    break;
                }
                head.push(line);
            }
            let reply = respond(&head);
            let mut stream = reader.into_inner();
            stream.write_all(&reply).expect("write response");
            stream.flush().expect("flush response");
        });
        (format!("http://127.0.0.1:{}/resource", port), server)
    }

    fn request_header<'a>(head: &'a [String], name: &str) -> Option<&'a str> {
        head.iter().skip(1).find_map(|line| {
            let (key, value) = line.split_once(':')?;
            if key.trim().eq_ignore_ascii_case(name) {
                Some(value.trim())
            } else {
                None
            }
        })
    }

    fn response_header(resp: &HttpResponse, name: &str) -> Option<String> {
        resp.headers
            .get(&AverStr::from(name))
            .and_then(|values| values.first())
            .map(|value| value.as_ref().to_string())
    }

    #[test]
    fn head_asks_for_the_identity_encoding() {
        let (url, server) = loopback_server(|head| {
            let encoding = request_header(head, "accept-encoding")
                .unwrap_or("<missing>")
                .to_string();
            format!(
                "HTTP/1.1 200 OK\r\nX-Accept-Encoding-Seen: {}\r\nContent-Length: 71339\r\n\r\n",
                encoding
            )
            .into_bytes()
        });

        let resp = head(&url).expect("HEAD must succeed");
        server.join().expect("server thread");
        assert_eq!(
            response_header(&resp, "x-accept-encoding-seen").as_deref(),
            Some("identity")
        );
    }

    #[test]
    fn head_keeps_content_length_when_the_server_honours_identity() {
        // Models a real origin: it compresses when the client offers gzip, and
        // then the announced length describes the compressed representation.
        let (url, server) = loopback_server(|head| {
            let wants_gzip = request_header(head, "accept-encoding")
                .map(|value| value.contains("gzip"))
                .unwrap_or(false);
            if wants_gzip {
                b"HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\nContent-Length: 24096\r\n\r\n"
                    .to_vec()
            } else {
                b"HTTP/1.1 200 OK\r\nContent-Length: 71339\r\n\r\n".to_vec()
            }
        });

        let resp = head(&url).expect("HEAD must succeed");
        server.join().expect("server thread");
        assert_eq!(resp.status, 200);
        assert_eq!(resp.body.as_ref(), "");
        assert_eq!(
            response_header(&resp, "content-length").as_deref(),
            Some("71339")
        );
    }

    #[test]
    fn head_survives_a_server_that_announces_an_encoding_anyway() {
        let (url, server) = loopback_server(|_| {
            b"HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\nContent-Length: 71339\r\n\r\n".to_vec()
        });

        let resp = head(&url).expect("HEAD must succeed");
        server.join().expect("server thread");
        assert_eq!(resp.status, 200);
        assert_eq!(resp.body.as_ref(), "");
    }

    #[test]
    fn get_accepts_a_not_modified_response_without_a_body() {
        let (url, server) = loopback_server(|_| {
            b"HTTP/1.1 304 Not Modified\r\nETag: \"abc\"\r\nContent-Encoding: gzip\r\n\r\n".to_vec()
        });

        let resp = get(&url).expect("304 must succeed");
        server.join().expect("server thread");
        assert_eq!(resp.status, 304);
        assert_eq!(resp.body.as_ref(), "");
        assert_eq!(response_header(&resp, "etag").as_deref(), Some("\"abc\""));
    }

    #[test]
    fn get_accepts_a_no_content_response_without_a_body() {
        let (url, server) = loopback_server(|_| {
            b"HTTP/1.1 204 No Content\r\nContent-Encoding: gzip\r\n\r\n".to_vec()
        });

        let resp = get(&url).expect("204 must succeed");
        server.join().expect("server thread");
        assert_eq!(resp.status, 204);
        assert_eq!(resp.body.as_ref(), "");
    }

    #[test]
    fn get_still_fails_on_a_truncated_body() {
        let (url, server) = loopback_server(|_| {
            let mut reply =
                b"HTTP/1.1 200 OK\r\nContent-Length: 100\r\nConnection: close\r\n\r\n".to_vec();
            reply.extend_from_slice(b"0123456789");
            reply
        });

        let err = get(&url).expect_err("a short body must not be reported as success");
        server.join().expect("server thread");
        assert!(
            err.starts_with("Http: failed to read response body:"),
            "{err}"
        );
    }
}
