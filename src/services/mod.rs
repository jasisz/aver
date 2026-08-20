/// Built-in platform services available to Aver programs.
///
/// Each service is a named namespace (`Args`, `Console`, `Http`, `HttpServer`, `Env`, `Terminal`) that must
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
    out.extend_from_slice(env::DECLARED_EFFECTS);
    // The Http and Terminal names are written out literally rather than read
    // from their modules, because those modules only exist in builds that
    // compile the implementation in. The name list answers "what can these
    // calls do", not "is the implementation compiled into this binary" — the
    // second question is answered where the implementation lives, which
    // reports "not available in this build" at the invoke site. Gating the
    // names here made the REPL refuse an `Http.get` the type checker had just
    // accepted. Tests check the literal copies against the modules'
    // `DECLARED_EFFECTS` in builds that have them.
    out.extend_from_slice(&[
        "Http.get",
        "Http.head",
        "Http.delete",
        "Http.post",
        "Http.put",
        "Http.patch",
    ]);
    out.extend_from_slice(http_server::DECLARED_EFFECTS);
    out.extend(
        crate::stdlib::standard_capability_registry_ref()
            .operations()
            .filter(|operation| operation.is_effectful())
            .map(|operation| operation.canonical_name.as_str()),
    );
    out.extend_from_slice(&[
        "Terminal.enableRawMode",
        "Terminal.disableRawMode",
        "Terminal.clear",
        "Terminal.moveTo",
        "Terminal.print",
        "Terminal.setColor",
        "Terminal.resetColor",
        "Terminal.readKey",
        "Terminal.size",
        "Terminal.hideCursor",
        "Terminal.showCursor",
        "Terminal.flush",
    ]);
    out
}

pub mod args;
pub mod console;
pub mod env;
#[cfg(feature = "runtime-net")]
pub mod http;
pub mod http_server;
#[cfg(feature = "terminal")]
pub mod terminal;

#[cfg(test)]
mod tests {
    /// Source text of `all_effect_names()`, from its signature to the closing
    /// brace of its body. Panics if the signature moved, so the guard below
    /// can never pass by silently matching nothing.
    fn all_effect_names_fn_source() -> &'static str {
        const SOURCE: &str = include_str!("mod.rs");
        const SIGNATURE: &str = "pub fn all_effect_names() -> Vec<&'static str> {";

        let start = SOURCE.find(SIGNATURE).expect(
            "`all_effect_names()` signature not found — update this guard to match the new one",
        );
        let body = &SOURCE[start + SIGNATURE.len()..];
        let mut depth = 1usize;
        for (idx, ch) in body.char_indices() {
            match ch {
                '{' => depth += 1,
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        return &SOURCE[start..start + SIGNATURE.len() + idx + 1];
                    }
                }
                _ => {}
            }
        }
        panic!("`all_effect_names()` body has unbalanced braces");
    }

    #[test]
    fn effect_name_list_is_the_same_in_every_build() {
        // `all_effect_names()` answers "what can these calls do", not "is the
        // implementation compiled into this binary". The type checker
        // registers `Http.*` and `Terminal.*` in every build, and the REPL
        // grants this list to the wrapper it builds around an expression the
        // type checker already approved. When the Http entry was gated on
        // `runtime-net`, a build without that feature accepted `Http.get(...)`
        // at the type level and then refused it in the VM's effect check — an
        // error about an effect the user did declare.
        //
        // Build-dependent behaviour belongs where the implementation lives:
        // `invoke_nv` already returns "HTTP effects not available in this
        // build". This is a source-shape check on purpose: it holds for every
        // feature combination at once, including the ones no CI lane builds.
        let source = all_effect_names_fn_source();
        assert!(
            !source.contains("#[cfg"),
            "`all_effect_names()` contains a `#[cfg` attribute. The effect name list must be \
             identical in every build — gate the implementation at the invoke site instead. \
             Offending body:\n{source}"
        );
    }

    /// The Http and Terminal names inside `all_effect_names()` are literal
    /// copies, because the modules that own them only exist in builds that
    /// compile the implementation in. In builds that do have those modules,
    /// the copies must match them exactly.
    #[cfg(feature = "runtime-net")]
    #[test]
    fn literal_http_names_match_the_module() {
        let listed: Vec<&str> = super::all_effect_names()
            .into_iter()
            .filter(|n| n.starts_with("Http."))
            .collect();
        assert_eq!(listed, super::http::DECLARED_EFFECTS);
    }

    /// See `literal_http_names_match_the_module`.
    #[cfg(feature = "terminal")]
    #[test]
    fn literal_terminal_names_match_the_module() {
        let listed: Vec<&str> = super::all_effect_names()
            .into_iter()
            .filter(|n| n.starts_with("Terminal."))
            .collect();
        assert_eq!(listed, super::terminal::DECLARED_EFFECTS);
    }

    #[test]
    fn standard_capability_effect_names_come_from_the_contract_registry() {
        let listed: Vec<&str> = super::all_effect_names()
            .into_iter()
            .filter(|name| {
                let namespace = name.split('.').next().unwrap_or(name);
                crate::stdlib::is_standard_capability(namespace)
            })
            .collect();
        let canonical: Vec<&str> = crate::stdlib::standard_capability_registry_ref()
            .operations()
            .filter(|operation| operation.is_effectful())
            .map(|operation| operation.canonical_name.as_str())
            .collect();
        assert_eq!(listed, canonical);
    }
}
