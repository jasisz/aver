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

/// `BranchPath` — opaque dewey-decimal identifier for a branch in the
/// structural tree of `!`/`?!` groups. In the VM/interpreter it is a
/// built-in record `{ dewey: String }`; it surfaces in source only as
/// the first parameter of Oracle-proof stub functions
/// (`oracle : (BranchPath, Int, args...) -> T`). Those stub fns are
/// emitted by the Rust backend as ordinary (dead-at-runtime) functions
/// because module-level fns are emitted regardless of reachability, so
/// the type must be in scope for them to compile. It is never produced
/// or consumed by runtime code — only `aver verify` exercises the
/// Oracle laws — so the constructors here only need to compile.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct BranchPath {
    pub dewey: AverStr,
}

impl BranchPath {
    /// Canonical root path (empty dewey). Mirrors `BranchPath.Root` in
    /// `src/types/branch_path.rs`.
    pub fn root() -> Self {
        Self {
            dewey: AverStr::from(""),
        }
    }

    /// Extend a path by entering branch `idx` of a group. Mirrors
    /// `BranchPath.child` (dewey segments joined with `.`).
    pub fn child(path: &BranchPath, idx: i64) -> Self {
        let dewey = if path.dewey.is_empty() {
            idx.to_string()
        } else {
            format!("{}.{}", &*path.dewey, idx)
        };
        Self {
            dewey: AverStr::from(dewey),
        }
    }

    /// Parse a dewey-decimal path string. Mirrors `BranchPath.parse`.
    pub fn parse(raw: &str) -> Self {
        Self {
            dewey: AverStr::from(raw),
        }
    }
}

impl AverDisplay for BranchPath {
    fn aver_display(&self) -> String {
        format!("BranchPath({})", self.dewey.aver_display_inner())
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}
