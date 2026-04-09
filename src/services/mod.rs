/// Built-in platform services available to Aver programs.
///
/// Each service is a named namespace (`Args`, `Console`, `Http`, `Disk`, `Tcp`, `HttpServer`, `Time`, `Env`, `Random`, `Terminal`) that must
/// be declared as an effect in order to be called:
///
/// ```aver
/// fn fetch(url: String) -> Result<HttpResponse, String>
///     ! [Http.get]
///     Http.get(url)
/// ```
///
/// Every service module exposes the same three functions:
/// - `register(global)` — insert the namespace into the global env
/// - `effects(name)` — return required effects for a builtin name (or empty)
/// - `call(name, args)` — handle the call, returning `None` if not owned
///
/// Pure type namespaces (Int, Float, String, List) live in `src/types/` instead.
/// Return every effect name that any service can require.
/// Used by the REPL to grant full effect permissions to wrapper functions.
pub fn all_effect_names() -> Vec<&'static str> {
    let mut out = Vec::new();
    out.extend_from_slice(args::DECLARED_EFFECTS);
    out.extend_from_slice(console::DECLARED_EFFECTS);
    out.extend_from_slice(disk::DECLARED_EFFECTS);
    out.extend_from_slice(env::DECLARED_EFFECTS);
    out.extend_from_slice(http::DECLARED_EFFECTS);
    out.extend_from_slice(http_server::DECLARED_EFFECTS);
    out.extend_from_slice(random::DECLARED_EFFECTS);
    out.extend_from_slice(tcp::DECLARED_EFFECTS);
    out.extend_from_slice(time::DECLARED_EFFECTS);
    #[cfg(feature = "terminal")]
    out.extend_from_slice(terminal::DECLARED_EFFECTS);
    out
}

pub mod args;
pub mod console;
pub mod disk;
pub mod env;
pub mod http;
pub mod http_server;
pub mod random;
pub mod tcp;
#[cfg(feature = "terminal")]
pub mod terminal;
pub mod time;
