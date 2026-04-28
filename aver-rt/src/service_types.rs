use crate::{AverDisplay, AverList, AverMap, AverStr};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct TerminalSize {
    pub width: i64,
    pub height: i64,
}

impl AverDisplay for TerminalSize {
    fn aver_display(&self) -> String {
        format!(
            "Terminal.Size(width: {}, height: {})",
            self.width.aver_display_inner(),
            self.height.aver_display_inner()
        )
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Header {
    pub name: AverStr,
    pub value: AverStr,
}

impl Header {
    /// Convenience constructor from owned Strings (used by runtime internals).
    pub fn from_strings(name: String, value: String) -> Self {
        Self {
            name: AverStr::from(name),
            value: AverStr::from(value),
        }
    }
}

impl AverDisplay for Header {
    fn aver_display(&self) -> String {
        format!(
            "Header(name: {}, value: {})",
            self.name.aver_display_inner(),
            self.value.aver_display_inner()
        )
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

/// Multi-value HTTP headers, modelled as `Map<String, List<String>>` —
/// matches RFC 9110 (same-name fields) and RFC 6265 (multiple
/// Set-Cookie). Keys are case-insensitive by convention; runtime
/// lowercases incoming names. Mirrors Go `net/http.Header`.
pub type HttpHeaders = AverMap<AverStr, AverList<AverStr>>;

#[derive(Clone, Debug, PartialEq)]
pub struct HttpResponse {
    pub status: i64,
    pub body: AverStr,
    pub headers: HttpHeaders,
}

impl AverDisplay for HttpResponse {
    fn aver_display(&self) -> String {
        format!(
            "HttpResponse(status: {}, body: {}, headers: {})",
            self.status.aver_display_inner(),
            self.body.aver_display_inner(),
            self.headers.aver_display_inner()
        )
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct HttpRequest {
    pub method: AverStr,
    pub path: AverStr,
    pub body: AverStr,
    pub headers: HttpHeaders,
}

impl AverDisplay for HttpRequest {
    fn aver_display(&self) -> String {
        format!(
            "HttpRequest(method: {}, path: {}, body: {}, headers: {})",
            self.method.aver_display_inner(),
            self.path.aver_display_inner(),
            self.body.aver_display_inner(),
            self.headers.aver_display_inner()
        )
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct TcpConnection {
    pub id: AverStr,
    pub host: AverStr,
    pub port: i64,
}

impl TcpConnection {
    pub fn from_parts(id: String, host: String, port: i64) -> Self {
        Self {
            id: AverStr::from(id),
            host: AverStr::from(host),
            port,
        }
    }
}

impl AverDisplay for TcpConnection {
    fn aver_display(&self) -> String {
        format!(
            "Tcp.Connection(id: {}, host: {}, port: {})",
            self.id.aver_display_inner(),
            self.host.aver_display_inner(),
            self.port.aver_display_inner()
        )
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}
