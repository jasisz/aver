/// Generate the inline `mod aver_rt { ... }` runtime helpers.
///
/// This module is embedded in the generated `main.rs` so the transpiled
/// program has no external dependencies (beyond std).
pub fn generate_runtime() -> String {
    r##"mod aver_rt {
    use std::fmt;

    /// Read a line from stdin, matching Aver's Console.readLine semantics.
    pub fn read_line() -> Result<String, String> {
        let mut buf = String::new();
        std::io::stdin()
            .read_line(&mut buf)
            .map_err(|e| e.to_string())?;
        if buf.ends_with('\n') {
            buf.pop();
            if buf.ends_with('\r') {
                buf.pop();
            }
        }
        Ok(buf)
    }

    /// Current UTC timestamp in RFC3339-like form (with milliseconds).
    pub fn time_now() -> String {
        let (secs, nanos) = unix_parts_now();
        format_utc_rfc3339_like(secs, nanos)
    }

    /// Unix epoch milliseconds as i64.
    pub fn time_unix_ms() -> i64 {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("Time.unixMs: system clock error");
        i64::try_from(now.as_millis()).expect("Time.unixMs: value out of i64 range")
    }

    /// Sleep current thread for `ms` milliseconds.
    pub fn time_sleep(ms: i64) {
        if ms < 0 {
            panic!("Time.sleep: ms must be non-negative");
        }
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
    }

    fn unix_parts_now() -> (i64, u32) {
        match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
            Ok(d) => (
                i64::try_from(d.as_secs()).expect("Time.now: seconds out of i64 range"),
                d.subsec_nanos(),
            ),
            Err(e) => {
                let d = e.duration();
                let secs = i64::try_from(d.as_secs()).expect("Time.now: seconds out of i64 range");
                let nanos = d.subsec_nanos();
                if nanos == 0 {
                    (-secs, 0)
                } else {
                    (-(secs + 1), 1_000_000_000 - nanos)
                }
            }
        }
    }

    fn format_utc_rfc3339_like(unix_secs: i64, nanos: u32) -> String {
        let days = unix_secs.div_euclid(86_400);
        let sod = unix_secs.rem_euclid(86_400);
        let hour = sod / 3_600;
        let minute = (sod % 3_600) / 60;
        let second = sod % 60;
        let millis = nanos / 1_000_000;
        let (year, month, day) = civil_from_days(days);
        format!(
            "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}Z",
            year, month, day, hour, minute, second, millis
        )
    }

    /// Convert days since unix epoch (1970-01-01) to Gregorian Y-M-D.
    fn civil_from_days(days_since_epoch: i64) -> (i32, u32, u32) {
        let z = days_since_epoch + 719_468;
        let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
        let doe = z - era * 146_097; // [0, 146096]
        let yoe = (doe - doe / 1_460 + doe / 36_524 - doe / 146_096) / 365; // [0,399]
        let y = yoe + era * 400;
        let doy = doe - (365 * yoe + yoe / 4 - yoe / 100); // [0,365]
        let mp = (5 * doy + 2) / 153; // [0,11]
        let day = doy - (153 * mp + 2) / 5 + 1; // [1,31]
        let month = mp + if mp < 10 { 3 } else { -9 }; // [1,12]
        let year = y + if month <= 2 { 1 } else { 0 };
        (year as i32, month as u32, day as u32)
    }

    /// Code-point based string slice, matching Aver's String.slice semantics.
    pub fn string_slice(s: &str, from: i64, to: i64) -> String {
        let chars: Vec<char> = s.chars().collect();
        let len = chars.len() as i64;
        let start = from.max(0) as usize;
        let end = to.min(len) as usize;
        if start >= end || start >= chars.len() {
            return String::new();
        }
        chars[start..end].iter().collect()
    }

    /// List directory contents, matching Aver's Disk.listDir semantics.
    pub fn list_dir(path: &str) -> Result<Vec<String>, String> {
        let entries = std::fs::read_dir(path).map_err(|e| e.to_string())?;
        let mut result = Vec::new();
        for entry in entries {
            let entry = entry.map_err(|e| e.to_string())?;
            if let Some(name) = entry.file_name().to_str() {
                result.push(name.to_string());
            }
        }
        Ok(result)
    }

    /// Append text to a file, matching Aver's Disk.appendText semantics.
    pub fn append_text(path: &str, content: &str) -> Result<(), String> {
        use std::io::Write;
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .map_err(|e| e.to_string())?;
        file.write_all(content.as_bytes())
            .map_err(|e| e.to_string())
    }

    /// Display a value in Aver format (used by Console.print and string interpolation).
    /// This is the primary display function — it formats values the way Aver does.
    pub fn aver_display<T: AverDisplay>(val: &T) -> String {
        val.aver_display()
    }

    /// Trait for Aver-compatible display.
    pub trait AverDisplay {
        fn aver_display(&self) -> String;
        /// Inner display — strings get quoted inside containers.
        fn aver_display_inner(&self) -> String {
            self.aver_display()
        }
    }

    impl AverDisplay for i64 {
        fn aver_display(&self) -> String { self.to_string() }
    }

    impl AverDisplay for f64 {
        fn aver_display(&self) -> String { self.to_string() }
    }

    impl AverDisplay for String {
        fn aver_display(&self) -> String { self.clone() }
        fn aver_display_inner(&self) -> String { format!("\"{}\"", self) }
    }

    impl AverDisplay for bool {
        fn aver_display(&self) -> String {
            if *self { "true".to_string() } else { "false".to_string() }
        }
    }

    impl AverDisplay for () {
        fn aver_display(&self) -> String { "()".to_string() }
    }

    impl<T: AverDisplay, E: AverDisplay> AverDisplay for Result<T, E> {
        fn aver_display(&self) -> String {
            match self {
                Ok(v) => format!("Result.Ok({})", v.aver_display_inner()),
                Err(e) => format!("Result.Err({})", e.aver_display_inner()),
            }
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }

    impl<T: AverDisplay> AverDisplay for Option<T> {
        fn aver_display(&self) -> String {
            match self {
                Some(v) => format!("Option.Some({})", v.aver_display_inner()),
                None => "Option.None".to_string(),
            }
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }

    impl<T: AverDisplay> AverDisplay for Box<T> {
        fn aver_display(&self) -> String { (**self).aver_display() }
        fn aver_display_inner(&self) -> String { (**self).aver_display_inner() }
    }

    impl<T: AverDisplay> AverDisplay for Vec<T> {
        fn aver_display(&self) -> String {
            let parts: Vec<String> = self.iter().map(|x| x.aver_display_inner()).collect();
            format!("[{}]", parts.join(", "))
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }

    impl<K: AverDisplay + Eq + std::hash::Hash + Ord, V: AverDisplay> AverDisplay for std::collections::HashMap<K, V> {
        fn aver_display(&self) -> String {
            let mut keys: Vec<&K> = self.keys().collect();
            keys.sort();
            let parts: Vec<String> = keys.iter()
                .map(|k| format!("{}: {}", k.aver_display_inner(), self[*k].aver_display_inner()))
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }

    impl<A: AverDisplay, B: AverDisplay> AverDisplay for (A, B) {
        fn aver_display(&self) -> String {
            format!("({}, {})", self.0.aver_display_inner(), self.1.aver_display_inner())
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }

    impl<A: AverDisplay, B: AverDisplay, C: AverDisplay> AverDisplay for (A, B, C) {
        fn aver_display(&self) -> String {
            format!("({}, {}, {})", self.0.aver_display_inner(), self.1.aver_display_inner(), self.2.aver_display_inner())
        }
        fn aver_display_inner(&self) -> String { self.aver_display() }
    }
}"##
    .to_string()
}

// ─── Service type definitions ────────────────────────────────────────────────

/// Generate the Tcp_Connection struct and its AverDisplay impl.
pub fn generate_tcp_types() -> String {
    r#"#[derive(Clone, Debug, PartialEq)]
struct Tcp_Connection {
    pub id: String,
    pub host: String,
    pub port: i64,
}

impl aver_rt::AverDisplay for Tcp_Connection {
    fn aver_display(&self) -> String {
        format!("Tcp.Connection(id: {}, host: {}, port: {})", self.id.aver_display_inner(), self.host.aver_display_inner(), self.port.aver_display_inner())
    }
    fn aver_display_inner(&self) -> String { self.aver_display() }
}"#
    .to_string()
}

/// Generate Header and HttpResponse structs with AverDisplay impls.
pub fn generate_http_types() -> String {
    r#"#[derive(Clone, Debug, PartialEq)]
struct Header {
    pub name: String,
    pub value: String,
}

impl aver_rt::AverDisplay for Header {
    fn aver_display(&self) -> String {
        format!("Header(name: {}, value: {})", self.name.aver_display_inner(), self.value.aver_display_inner())
    }
    fn aver_display_inner(&self) -> String { self.aver_display() }
}

#[derive(Clone, Debug, PartialEq)]
struct HttpResponse {
    pub status: i64,
    pub body: String,
    pub headers: Vec<Header>,
}

impl aver_rt::AverDisplay for HttpResponse {
    fn aver_display(&self) -> String {
        format!("HttpResponse(status: {}, body: {}, headers: {})", self.status.aver_display_inner(), self.body.aver_display_inner(), self.headers.aver_display_inner())
    }
    fn aver_display_inner(&self) -> String { self.aver_display() }
}"#
    .to_string()
}

/// Generate HttpRequest struct with AverDisplay impl.
pub fn generate_http_server_types() -> String {
    r#"#[derive(Clone, Debug, PartialEq)]
struct HttpRequest {
    pub method: String,
    pub path: String,
    pub body: String,
    pub headers: Vec<Header>,
}

impl aver_rt::AverDisplay for HttpRequest {
    fn aver_display(&self) -> String {
        format!("HttpRequest(method: {}, path: {}, body: {}, headers: {})", self.method.aver_display_inner(), self.path.aver_display_inner(), self.body.aver_display_inner(), self.headers.aver_display_inner())
    }
    fn aver_display_inner(&self) -> String { self.aver_display() }
}"#
    .to_string()
}

// ─── Service runtime modules ─────────────────────────────────────────────────

/// Generate the aver_tcp runtime module (connect, writeLine, readLine, close, send, ping).
pub fn generate_tcp_runtime() -> String {
    r#"mod aver_tcp {
    use super::Tcp_Connection;
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::io::{BufRead, BufReader, Read, Write};
    use std::net::{TcpStream, ToSocketAddrs};
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::Duration;

    const CONNECT_TIMEOUT: Duration = Duration::from_secs(5);
    const IO_TIMEOUT: Duration = Duration::from_secs(30);
    const BODY_LIMIT: usize = 10 * 1024 * 1024;
    const MAX_CONNECTIONS: usize = 256;

    static NEXT_ID: AtomicU64 = AtomicU64::new(1);

    thread_local! {
        static CONNECTIONS: RefCell<HashMap<String, BufReader<TcpStream>>> =
            RefCell::new(HashMap::new());
    }

    pub fn connect(host: &str, port: i64) -> Result<Tcp_Connection, String> {
        let count = CONNECTIONS.with(|map| map.borrow().len());
        if count >= MAX_CONNECTIONS {
            return Err(format!("Tcp.connect: connection limit reached ({} max)", MAX_CONNECTIONS));
        }
        let addr = format!("{}:{}", host, port);
        let socket_addr = addr.to_socket_addrs()
            .map_err(|e| format!("DNS failed for {}: {}", addr, e))?
            .next()
            .ok_or_else(|| format!("no address for {}", addr))?;
        let stream = TcpStream::connect_timeout(&socket_addr, CONNECT_TIMEOUT)
            .map_err(|e| e.to_string())?;
        stream.set_read_timeout(Some(IO_TIMEOUT)).ok();
        stream.set_write_timeout(Some(IO_TIMEOUT)).ok();
        let id = format!("tcp-{}", NEXT_ID.fetch_add(1, Ordering::Relaxed));
        CONNECTIONS.with(|map| {
            map.borrow_mut().insert(id.clone(), BufReader::new(stream));
        });
        Ok(Tcp_Connection { id, host: host.to_string(), port })
    }

    pub fn write_line(conn: &Tcp_Connection, line: &str) -> Result<(), String> {
        CONNECTIONS.with(|map| {
            let mut borrow = map.borrow_mut();
            match borrow.get_mut(&conn.id) {
                None => Err(format!("unknown connection '{}'", conn.id)),
                Some(reader) => {
                    let msg = format!("{}\r\n", line);
                    reader.get_mut().write_all(msg.as_bytes()).map_err(|e| e.to_string())
                }
            }
        })
    }

    pub fn read_line(conn: &Tcp_Connection) -> Result<String, String> {
        CONNECTIONS.with(|map| {
            let mut borrow = map.borrow_mut();
            match borrow.get_mut(&conn.id) {
                None => Err(format!("unknown connection '{}'", conn.id)),
                Some(reader) => {
                    let mut line = String::new();
                    reader.read_line(&mut line).map_err(|e| e.to_string())?;
                    if line.ends_with('\n') {
                        line.pop();
                        if line.ends_with('\r') {
                            line.pop();
                        }
                    }
                    Ok(line)
                }
            }
        })
    }

    pub fn close(conn: &Tcp_Connection) -> Result<(), String> {
        let removed = CONNECTIONS.with(|map| map.borrow_mut().remove(&conn.id));
        match removed {
            Some(_) => Ok(()),
            None => Err(format!("unknown connection '{}'", conn.id)),
        }
    }

    pub fn send(host: &str, port: i64, message: &str) -> Result<String, String> {
        let addr = format!("{}:{}", host, port);
        let socket_addr = addr.to_socket_addrs()
            .map_err(|e| format!("DNS failed for {}: {}", addr, e))?
            .next()
            .ok_or_else(|| format!("no address for {}", addr))?;
        let mut stream = TcpStream::connect_timeout(&socket_addr, CONNECT_TIMEOUT)
            .map_err(|e| e.to_string())?;
        stream.set_read_timeout(Some(IO_TIMEOUT)).ok();
        stream.set_write_timeout(Some(IO_TIMEOUT)).ok();
        stream.write_all(message.as_bytes()).map_err(|e| e.to_string())?;
        stream.shutdown(std::net::Shutdown::Write).ok();
        let mut buf = Vec::new();
        Read::by_ref(&mut stream).take(BODY_LIMIT as u64 + 1).read_to_end(&mut buf)
            .map_err(|e| e.to_string())?;
        if buf.len() > BODY_LIMIT {
            return Err("response exceeds 10 MB limit".to_string());
        }
        Ok(String::from_utf8_lossy(&buf).into_owned())
    }

    pub fn ping(host: &str, port: i64) -> Result<(), String> {
        let addr = format!("{}:{}", host, port);
        let socket_addr = addr.to_socket_addrs()
            .map_err(|e| format!("DNS failed for {}: {}", addr, e))?
            .next()
            .ok_or_else(|| format!("no address for {}", addr))?;
        TcpStream::connect_timeout(&socket_addr, CONNECT_TIMEOUT)
            .map_err(|e| e.to_string())?;
        Ok(())
    }
}"#
    .to_string()
}

/// Generate the aver_http runtime module (get, head, delete, post, put, patch via ureq).
pub fn generate_http_runtime() -> String {
    r#"mod aver_http {
    use super::{Header, HttpResponse};

    pub fn get(url: &str) -> Result<HttpResponse, String> {
        simple_request("GET", url)
    }

    pub fn head(url: &str) -> Result<HttpResponse, String> {
        simple_request("HEAD", url)
    }

    pub fn delete(url: &str) -> Result<HttpResponse, String> {
        simple_request("DELETE", url)
    }

    pub fn post(url: &str, body: &str, content_type: &str, headers: &[Header]) -> Result<HttpResponse, String> {
        body_request("POST", url, body, content_type, headers)
    }

    pub fn put(url: &str, body: &str, content_type: &str, headers: &[Header]) -> Result<HttpResponse, String> {
        body_request("PUT", url, body, content_type, headers)
    }

    pub fn patch(url: &str, body: &str, content_type: &str, headers: &[Header]) -> Result<HttpResponse, String> {
        body_request("PATCH", url, body, content_type, headers)
    }

    fn simple_request(method: &str, url: &str) -> Result<HttpResponse, String> {
        let result = ureq::request(method, url)
            .timeout(std::time::Duration::from_secs(10))
            .call();
        response_value(result)
    }

    fn body_request(method: &str, url: &str, body: &str, content_type: &str, headers: &[Header]) -> Result<HttpResponse, String> {
        let mut req = ureq::request(method, url)
            .timeout(std::time::Duration::from_secs(10))
            .set("Content-Type", content_type);
        for h in headers {
            req = req.set(&h.name, &h.value);
        }
        response_value(req.send_string(body))
    }

    fn response_value(result: Result<ureq::Response, ureq::Error>) -> Result<HttpResponse, String> {
        match result {
            Ok(resp) => build_response(resp),
            Err(ureq::Error::Status(_, resp)) => build_response(resp),
            Err(ureq::Error::Transport(e)) => Err(e.to_string()),
        }
    }

    fn build_response(resp: ureq::Response) -> Result<HttpResponse, String> {
        use std::io::Read;
        let status = resp.status() as i64;
        let header_names = resp.headers_names();
        let headers: Vec<Header> = header_names.iter()
            .map(|name| Header {
                name: name.clone(),
                value: resp.header(name).unwrap_or("").to_string(),
            })
            .collect();
        const BODY_LIMIT: u64 = 10 * 1024 * 1024;
        let mut buf = Vec::new();
        let bytes_read = resp.into_reader().take(BODY_LIMIT + 1).read_to_end(&mut buf)
            .map_err(|e| format!("read body: {}", e))?;
        if bytes_read as u64 > BODY_LIMIT {
            return Err("response body exceeds 10 MB limit".to_string());
        }
        let body = String::from_utf8_lossy(&buf).into_owned();
        Ok(HttpResponse { status, body, headers })
    }
}"#
    .to_string()
}

/// Generate the aver_http_server runtime module (listen, listen_with).
pub fn generate_http_server_runtime() -> String {
    r#"mod aver_http_server {
    use super::{Header, HttpRequest, HttpResponse};
    use std::io::{BufRead, BufReader, Read, Write};
    use std::net::{TcpListener, TcpStream};
    use std::time::Duration;

    pub fn listen(port: i64, handler: fn(HttpRequest) -> HttpResponse) {
        let listener = TcpListener::bind(format!("0.0.0.0:{}", port))
            .expect(&format!("HttpServer: bind failed on port {}", port));
        for incoming in listener.incoming() {
            let mut stream = match incoming {
                Ok(s) => s,
                Err(_) => continue,
            };
            stream.set_read_timeout(Some(Duration::from_secs(30))).ok();
            stream.set_write_timeout(Some(Duration::from_secs(30))).ok();
            let request = match parse_http_request(&mut stream) {
                Ok(req) => req,
                Err(msg) => {
                    let _ = write_http_response(&mut stream, &HttpResponse {
                        status: 400, body: format!("Bad Request: {}", msg), headers: vec![],
                    });
                    continue;
                }
            };
            let response = handler(request);
            let _ = write_http_response(&mut stream, &response);
        }
    }

    pub fn listen_with<C: Clone>(port: i64, context: C, handler: fn(C, HttpRequest) -> HttpResponse) {
        let listener = TcpListener::bind(format!("0.0.0.0:{}", port))
            .expect(&format!("HttpServer: bind failed on port {}", port));
        for incoming in listener.incoming() {
            let mut stream = match incoming {
                Ok(s) => s,
                Err(_) => continue,
            };
            stream.set_read_timeout(Some(Duration::from_secs(30))).ok();
            stream.set_write_timeout(Some(Duration::from_secs(30))).ok();
            let request = match parse_http_request(&mut stream) {
                Ok(req) => req,
                Err(msg) => {
                    let _ = write_http_response(&mut stream, &HttpResponse {
                        status: 400, body: format!("Bad Request: {}", msg), headers: vec![],
                    });
                    continue;
                }
            };
            let response = handler(context.clone(), request);
            let _ = write_http_response(&mut stream, &response);
        }
    }

    fn parse_http_request(stream: &mut TcpStream) -> Result<HttpRequest, String> {
        const BODY_LIMIT: usize = 10 * 1024 * 1024;
        let reader_stream = stream.try_clone().map_err(|e| format!("clone: {}", e))?;
        let mut reader = BufReader::new(reader_stream);

        let mut request_line = String::new();
        let len = reader.read_line(&mut request_line).map_err(|e| e.to_string())?;
        if len == 0 { return Err("empty request".to_string()); }

        let request_line = request_line.trim_end_matches(&['\r', '\n'][..]);
        let mut parts = request_line.split_whitespace();
        let method = parts.next().ok_or("missing method")?.to_string();
        let path = parts.next().ok_or("missing path")?.to_string();
        let _version = parts.next().ok_or("missing version")?;

        let mut headers = Vec::new();
        let mut content_length = 0usize;

        loop {
            let mut line = String::new();
            let bytes = reader.read_line(&mut line).map_err(|e| e.to_string())?;
            if bytes == 0 { break; }
            let trimmed = line.trim_end_matches(&['\r', '\n'][..]);
            if trimmed.is_empty() { break; }
            let (name, value) = trimmed.split_once(':')
                .ok_or_else(|| format!("malformed header: '{}'", trimmed))?;
            let name = name.trim().to_string();
            let value = value.trim().to_string();
            if name.eq_ignore_ascii_case("Content-Length") {
                content_length = value.parse::<usize>()
                    .map_err(|_| format!("invalid Content-Length: '{}'", value))?;
                if content_length > BODY_LIMIT {
                    return Err(format!("body exceeds {} bytes", BODY_LIMIT));
                }
            }
            headers.push(Header { name, value });
        }

        let mut body_bytes = vec![0u8; content_length];
        if content_length > 0 {
            reader.read_exact(&mut body_bytes).map_err(|e| e.to_string())?;
        }
        let body = String::from_utf8_lossy(&body_bytes).into_owned();

        Ok(HttpRequest { method, path, body, headers })
    }

    fn status_reason(status: i64) -> &'static str {
        match status {
            200 => "OK", 201 => "Created", 204 => "No Content",
            301 => "Moved Permanently", 302 => "Found", 304 => "Not Modified",
            400 => "Bad Request", 401 => "Unauthorized", 403 => "Forbidden",
            404 => "Not Found", 405 => "Method Not Allowed",
            500 => "Internal Server Error", 502 => "Bad Gateway",
            503 => "Service Unavailable",
            _ => "OK",
        }
    }

    fn write_http_response(stream: &mut TcpStream, response: &HttpResponse) -> std::io::Result<()> {
        let mut headers: Vec<(String, String)> = response.headers.iter()
            .filter(|h| !h.name.eq_ignore_ascii_case("Content-Length") && !h.name.eq_ignore_ascii_case("Connection"))
            .map(|h| (h.name.clone(), h.value.clone()))
            .collect();
        if !headers.iter().any(|(n, _)| n.eq_ignore_ascii_case("Content-Type")) {
            headers.push(("Content-Type".to_string(), "text/plain; charset=utf-8".to_string()));
        }
        headers.push(("Content-Length".to_string(), response.body.len().to_string()));
        headers.push(("Connection".to_string(), "close".to_string()));

        let mut head = format!("HTTP/1.1 {} {}\r\n", response.status, status_reason(response.status));
        for (name, value) in &headers {
            head.push_str(&format!("{}: {}\r\n", name, value));
        }
        head.push_str("\r\n");

        stream.write_all(head.as_bytes())?;
        stream.write_all(response.body.as_bytes())?;
        stream.flush()?;
        Ok(())
    }
}"#
    .to_string()
}
