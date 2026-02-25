/// Built-in platform services available to Aver programs.
///
/// Each service is a named namespace (`Console`, `Network`, `Disk`) that must
/// be declared as an effect in order to be called:
///
/// ```aver
/// fn fetch(url: String) -> Any
///     ! [Network]
///     Network.get(url)
/// ```
///
/// Every service module exposes the same three functions:
/// - `register(global)` — insert the namespace into the interpreter's global env
/// - `effects(name)` — return required effects for a builtin name (or empty)
/// - `call(name, args)` — handle the call, returning `None` if not owned
pub mod console;
pub mod disk;
pub mod network;
