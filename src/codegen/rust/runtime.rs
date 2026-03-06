/// Generate the inline `mod aver_rt { ... }` runtime bridge.
///
/// This module is embedded in the generated `main.rs` and re-exports pieces
/// from the shared `aver-rt` crate.
pub fn generate_runtime() -> String {
    r##"mod aver_rt {
    pub use ::aver_rt::*;
}"##
        .to_string()
}

/// Bring the shared Tcp connection type into the generated program under the
/// legacy codegen name used for dotted `Tcp.Connection`.
pub fn generate_tcp_types() -> String {
    "use aver_rt::TcpConnection as Tcp_Connection;".to_string()
}

/// Bring shared HTTP record types into the generated program.
pub fn generate_http_types() -> String {
    "use aver_rt::{Header, HttpResponse};".to_string()
}

/// Bring shared HTTP server request type into the generated program.
pub fn generate_http_server_types() -> String {
    "use aver_rt::HttpRequest;".to_string()
}
