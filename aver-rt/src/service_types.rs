use crate::{AverDisplay, AverList};

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
    pub name: String,
    pub value: String,
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

#[derive(Clone, Debug, PartialEq)]
pub struct HttpResponse {
    pub status: i64,
    pub body: String,
    pub headers: AverList<Header>,
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
    pub method: String,
    pub path: String,
    pub body: String,
    pub headers: AverList<Header>,
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
    pub id: String,
    pub host: String,
    pub port: i64,
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
