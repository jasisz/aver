use std::fs;
use std::io::Read;

#[cfg(feature = "wasip2")]
use std::collections::BTreeSet;

use colored::Colorize;

use aver::ast::TopLevel;
use aver::types::checker::TypeError;
use aver::value::Value;
use aver::vm;

pub(super) fn read_file(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("Cannot open file '{}': {}", path, e))
}

/// Parse one of the user's own project files, under the verify-case ceiling
/// that project declared for it.
///
/// Every command that reads a `.av` off disk comes through here, so the
/// ceiling is the same one `aver verify`'s loader applies. A command with its
/// own opinion would accept or refuse files the others do not — which is the
/// disagreement `[verify] max-cases` exists to settle, not to create.
pub(super) fn parse_file(
    source: &str,
    module_root: &str,
    file: &str,
) -> Result<Vec<TopLevel>, String> {
    aver::source::parse_project_source(source, module_root, file)
}

pub(super) fn resolve_module_root(module_root: Option<&str>) -> String {
    match module_root {
        Some(root) => root.to_string(),
        None => aver::source::working_module_root(),
    }
}

pub(super) fn load_runtime_policy(
    module_root: &str,
) -> Result<Option<aver::config::ProjectConfig>, String> {
    aver::config::ProjectConfig::load_from_dir(std::path::Path::new(module_root))
        .map_err(|e| format!("aver.toml: {}", e))
}

pub(super) fn apply_runtime_policy_to_vm(
    machine: &mut vm::VM,
    module_root: &str,
) -> Result<(), String> {
    if let Some(config) = load_runtime_policy(module_root)? {
        machine.set_runtime_policy(config);
    }
    Ok(())
}

/// Explain when wasip2 cannot preserve Aver's Tcp deployment policy.
///
/// Persistent-session reads and writes deliberately have no Aver deadline, so
/// using only those operations must not produce this warning. Socket opening
/// uses the connect deadline; the one-shot send operations additionally use
/// the request-idle deadline. Its connected-socket pool is currently fixed at
/// the native default, so a non-default `max_connections` also needs a loud
/// warning when the program creates pooled connections.
#[cfg(feature = "wasip2")]
pub(super) fn wasip2_tcp_policy_warning(
    target: &str,
    required: &BTreeSet<String>,
    config: Option<&aver::config::ProjectConfig>,
) -> Option<String> {
    let uses_connect_deadline = required.iter().any(|operation| {
        matches!(
            operation.as_str(),
            "Tcp.connect" | "Tcp.ping" | "Tcp.send" | "Tcp.sendBytes"
        )
    });
    let uses_request_idle_deadline = required
        .iter()
        .any(|operation| matches!(operation.as_str(), "Tcp.send" | "Tcp.sendBytes"));
    let settings = config.map(|config| config.tcp_settings).unwrap_or_default();
    let defaults = aver::config::TcpEffectSettings::default();
    let source = if settings.connect_timeout_secs != defaults.connect_timeout_secs
        || settings.request_idle_timeout_secs != defaults.request_idle_timeout_secs
    {
        "configured"
    } else {
        "default"
    };
    let deadlines = if uses_request_idle_deadline {
        format!(
            "connect {} s, request idle {} s",
            settings.connect_timeout_secs, settings.request_idle_timeout_secs
        )
    } else {
        format!("connect {} s", settings.connect_timeout_secs)
    };

    let mut warnings = Vec::new();
    if uses_connect_deadline {
        warnings.push(format!(
            "warning[tcp-timeout-unsupported]: `{target}` cannot honour Aver's {source} Tcp \
             timeout policy ({deadlines}); the component uses the host's WASI socket timing \
             instead"
        ));
    }
    if required.contains("Tcp.connect") && settings.max_connections != defaults.max_connections {
        warnings.push(format!(
            "warning[tcp-connection-limit-unsupported]: `{target}` cannot honour Aver's \
             configured Tcp connection limit ({}); this backend currently uses its fixed \
             {}-slot connected-socket pool",
            settings.max_connections, defaults.max_connections
        ));
    }
    (!warnings.is_empty()).then(|| warnings.join("\n"))
}

pub(super) fn print_type_errors(errors: &[TypeError]) {
    for te in errors {
        eprintln!(
            "{}",
            format!("error[{}:{}]: {}", te.line, te.col, te.message).red()
        );
    }
}

pub(super) fn format_type_errors(errors: &[TypeError]) -> String {
    let mut out = Vec::new();
    for te in errors {
        out.push(format!("error[{}:{}]: {}", te.line, te.col, te.message));
    }
    out.join("\n")
}

/// Collect entry expressions from `--expr` flags or `--input-file`.
/// Input file path `-` reads from stdin.
/// Returns empty list if neither is provided (caller runs `main`).
pub(super) fn collect_entry_expressions(
    exprs: &[String],
    input_file: Option<&str>,
) -> Result<Vec<String>, String> {
    if let Some(path) = input_file {
        let content = if path == "-" {
            let mut buf = String::new();
            std::io::stdin()
                .read_to_string(&mut buf)
                .map_err(|e| format!("Cannot read stdin: {}", e))?;
            buf
        } else {
            fs::read_to_string(path)
                .map_err(|e| format!("Cannot read input file '{}': {}", path, e))?
        };
        let trimmed = content.trim().to_string();
        if trimmed.is_empty() {
            return Err(format!("Input file '{}' is empty", path));
        }
        return Ok(vec![trimmed]);
    }
    Ok(exprs.to_vec())
}

/// Parse a CLI `--expr` argument. Delegates to `aver::replay::parse_entry_call`.
pub(super) fn parse_call_expression(src: &str) -> Result<(String, Vec<Value>), String> {
    aver::replay::parse_entry_call(src)
}

/// Serialise entry-call arguments into the replay schema's `input` field.
pub(super) fn encode_entry_args_json(args: &[Value]) -> Result<aver::replay::JsonValue, String> {
    aver::replay::encode_entry_args(args)
}

/// Derive a readable filename stem from an entry call.
pub(super) fn entry_recording_stem(fn_name: &str, args: &[Value]) -> String {
    aver::replay::recording_stem(fn_name, args)
}

#[cfg(all(test, feature = "wasip2"))]
mod tests {
    use super::*;

    fn required(operations: &[&str]) -> BTreeSet<String> {
        operations
            .iter()
            .map(|operation| (*operation).to_string())
            .collect()
    }

    #[test]
    fn wasip2_warns_about_the_default_connect_deadline() {
        let warning =
            wasip2_tcp_policy_warning("--target wasip2", &required(&["Tcp.connect"]), None)
                .expect("connect uses the deadline");

        assert!(warning.contains("default Tcp timeout policy (connect 5 s)"));
    }

    #[test]
    fn wasip2_warning_names_configured_one_shot_deadlines() {
        let config = aver::config::ProjectConfig::parse(
            "[effects.Tcp]\nconnect_timeout_secs = 7\nrequest_idle_timeout_secs = 45\n",
        )
        .expect("valid policy");
        let warning =
            wasip2_tcp_policy_warning("--wasip2", &required(&["Tcp.sendBytes"]), Some(&config))
                .expect("one-shot send uses both deadlines");

        assert!(warning.contains("configured Tcp timeout policy (connect 7 s, request idle 45 s)"));
    }

    #[test]
    fn wasip2_warns_when_configured_connection_limit_cannot_be_honoured() {
        let config = aver::config::ProjectConfig::parse("[effects.Tcp]\nmax_connections = 128\n")
            .expect("valid policy");
        let warning =
            wasip2_tcp_policy_warning("--wasip2", &required(&["Tcp.connect"]), Some(&config))
                .expect("connect uses the configured pool limit");

        assert!(warning.contains("configured Tcp connection limit (128)"));
        assert!(warning.contains("fixed 256-slot connected-socket pool"));
    }

    #[test]
    fn wasip2_does_not_warn_for_persistent_session_io() {
        assert!(
            wasip2_tcp_policy_warning("--wasip2", &required(&["Tcp.readLine"]), None).is_none()
        );
    }
}
