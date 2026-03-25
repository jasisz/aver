/// Generate the inline `mod aver_rt { ... }` runtime bridge.
///
/// This module is embedded in the generated `main.rs` and re-exports pieces
/// from the shared `aver-rt` crate.
pub fn generate_runtime() -> String {
    r##"pub mod aver_rt {
    pub use ::aver_rt::*;
}

use ::aver_rt::AverStr;

/// Convert String results from aver_rt to AverStr for generated code.
pub trait IntoAverStr {
    type Output;
    fn into_aver(self) -> Self::Output;
}
impl IntoAverStr for String {
    type Output = AverStr;
    fn into_aver(self) -> AverStr { AverStr::from(self) }
}
impl IntoAverStr for Result<String, String> {
    type Output = Result<AverStr, AverStr>;
    fn into_aver(self) -> Result<AverStr, AverStr> { self.map(AverStr::from).map_err(AverStr::from) }
}
impl IntoAverStr for Result<(), String> {
    type Output = Result<(), AverStr>;
    fn into_aver(self) -> Result<(), AverStr> { self.map_err(AverStr::from) }
}
impl IntoAverStr for Option<String> {
    type Output = Option<AverStr>;
    fn into_aver(self) -> Option<AverStr> { self.map(AverStr::from) }
}
impl IntoAverStr for aver_rt::AverList<String> {
    type Output = aver_rt::AverList<AverStr>;
    fn into_aver(self) -> aver_rt::AverList<AverStr> {
        aver_rt::AverList::from_vec(self.to_vec().into_iter().map(AverStr::from).collect())
    }
}
impl IntoAverStr for Result<aver_rt::AverList<String>, String> {
    type Output = Result<aver_rt::AverList<AverStr>, AverStr>;
    fn into_aver(self) -> Result<aver_rt::AverList<AverStr>, AverStr> {
        self.map(|l| l.into_aver()).map_err(AverStr::from)
    }
}
impl IntoAverStr for Result<aver_rt::HttpResponse, String> {
    type Output = Result<aver_rt::HttpResponse, AverStr>;
    fn into_aver(self) -> Result<aver_rt::HttpResponse, AverStr> {
        self.map_err(AverStr::from)
    }
}
impl IntoAverStr for Result<aver_rt::TcpConnection, String> {
    type Output = Result<aver_rt::TcpConnection, AverStr>;
    fn into_aver(self) -> Result<aver_rt::TcpConnection, AverStr> {
        self.map_err(AverStr::from)
    }
}
impl IntoAverStr for Result<i64, String> {
    type Output = Result<i64, AverStr>;
    fn into_aver(self) -> Result<i64, AverStr> {
        self.map_err(AverStr::from)
    }
}
impl IntoAverStr for Result<f64, String> {
    type Output = Result<f64, AverStr>;
    fn into_aver(self) -> Result<f64, AverStr> {
        self.map_err(AverStr::from)
    }
}"##
    .to_string()
}

/// Bring the shared Tcp connection type into the generated program under the
/// legacy codegen name used for dotted `Tcp.Connection`.
pub fn generate_tcp_types() -> String {
    "pub use aver_rt::TcpConnection as Tcp_Connection;".to_string()
}

/// Bring shared HTTP record types into the generated program.
pub fn generate_http_types() -> String {
    "pub use aver_rt::{Header, HttpResponse};".to_string()
}

/// Bring shared HTTP server request type into the generated program.
pub fn generate_http_server_types() -> String {
    "pub use aver_rt::HttpRequest;".to_string()
}
