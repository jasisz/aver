/// Built-in platform services available to Aver programs.
///
/// Each service is a named namespace (`Console`, `Http`, `Disk`, `Tcp`, `HttpServer`) that must
/// be declared as an effect in order to be called:
///
/// ```aver
/// fn fetch(url: String) -> Result<HttpResponse, String>
///     ! [Http]
///     Http.get(url)
/// ```
///
/// Every service module exposes the same three functions:
/// - `register(global)` — insert the namespace into the interpreter's global env
/// - `effects(name)` — return required effects for a builtin name (or empty)
/// - `call(name, args)` — handle the call, returning `None` if not owned
///
/// Pure type namespaces (Int, Float, String, List) live in `src/types/` instead.
pub mod console;
pub mod disk;
pub mod http;
pub mod http_server;
pub mod tcp;
