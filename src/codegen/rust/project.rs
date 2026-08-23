/// Cargo.toml generation for the transpiled project.
use std::collections::HashSet;

use super::composition::ProviderComposition;
use crate::toolchain_source::ToolchainSource;

/// Exact aver-rt version embedded alongside the toolchain's Cargo source
/// provenance by build.rs.
const RUNTIME_VERSION: &str = env!("AVER_RT_VERSION");

fn runtime_override_path() -> Option<String> {
    std::env::var("AVER_RUNTIME_PATH")
        .ok()
        .map(|value| value.trim().to_string())
        .filter(|value| !value.is_empty())
        .map(|value| value.replace('\\', "/"))
}

fn runtime_dependency_line(
    runtime_version: &str,
    features: &[&str],
    local_path: Option<&str>,
) -> String {
    match local_path {
        Some(path) => {
            ToolchainSource::exact_path_dependency("aver-rt", None, runtime_version, features, path)
        }
        None => ToolchainSource::current().aver_rt_dependency(runtime_version, features),
    }
}

pub fn generate_cargo_toml(
    name: &str,
    services: &HashSet<String>,
    has_embedded_policy: bool,
    has_runtime_policy: bool,
    has_scoped_runtime: bool,
    provider_composition: &ProviderComposition,
) -> String {
    let mut lines = Vec::new();
    lines.push("[package]".to_string());
    lines.push(format!("name = \"{}\"", name));
    lines.push("version = \"0.1.0\"".to_string());
    lines.push("edition = \"2024\"".to_string());
    lines.push(String::new());

    // Collect aver-rt feature flags based on which services the program uses.
    let mut rt_features: Vec<&str> = Vec::new();
    if services.contains("Http") {
        rt_features.push("http");
    }
    if services.contains("Random") {
        rt_features.push("random");
    }
    if services.contains("Terminal") {
        rt_features.push("terminal");
    }
    rt_features.sort();

    let mut deps = Vec::new();
    let runtime_version = RUNTIME_VERSION;
    let runtime_path = runtime_override_path();
    deps.push(runtime_dependency_line(
        runtime_version,
        &rt_features,
        runtime_path.as_deref(),
    ));
    if has_embedded_policy || has_runtime_policy {
        deps.push("url = \"2\"".to_string());
    }
    if has_scoped_runtime {
        deps.push("serde = { version = \"1\", features = [\"derive\"] }".to_string());
        deps.push("serde_json = \"1\"".to_string());
    }
    if has_runtime_policy {
        deps.push("toml = \"0.8\"".to_string());
    }
    for binding in &provider_composition.bindings {
        deps.push(binding.cargo_dependency_line());
    }

    if !deps.is_empty() {
        lines.push("[dependencies]".to_string());
        lines.extend(deps);
    }

    lines.push(String::new());
    lines.push("[profile.release]".to_string());
    lines.push("lto = true".to_string());
    lines.push("codegen-units = 1".to_string());

    // Aver `Int` is arbitrary-precision (ℤ): both the VM and this Rust
    // backend carry it as `aver_rt::AverInt`, and the unboxing analysis
    // lowers a value to a bare `i64` only when it has PROVEN the value
    // stays in i64 range — so a bare op never overflows in a correctly
    // compiled program. Turn overflow-checks ON in dev/test as a
    // defense-in-depth trip-wire: it has zero false positives on correct
    // code, but turns any future unboxing miscompile (a value wrongly
    // lowered to bare) into a loud panic instead of a silent i64 wrap.
    // Release keeps the default (off) so the proven-bounded fast path
    // carries no per-op check.
    lines.push(String::new());
    lines.push("[profile.dev]".to_string());
    lines.push("overflow-checks = true".to_string());
    lines.push(String::new());
    lines.push("[profile.test]".to_string());
    lines.push("overflow-checks = true".to_string());

    lines.join("\n")
}

#[cfg(test)]
fn toml_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch <= '\u{001f}' => {
                use std::fmt::Write;
                write!(out, "\\u{:04X}", ch as u32).expect("write to String");
            }
            ch => out.push(ch),
        }
    }
    out.push('"');
    out
}

#[cfg(test)]
mod tests {
    use super::{runtime_dependency_line, toml_string};

    #[test]
    fn runtime_dependency_can_use_local_override_path() {
        let dep = runtime_dependency_line("=0.3.0", &["http"], Some("/tmp/aver-rt"));
        assert_eq!(
            dep,
            "aver-rt = { path = \"/tmp/aver-rt\", version = \"=0.3.0\", features = [\"http\"] }"
        );
    }

    #[test]
    fn generated_toml_string_escapes_data_instead_of_source() {
        assert_eq!(toml_string("a\\b\"c\n"), "\"a\\\\b\\\"c\\n\"");
    }
}
