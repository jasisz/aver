/// Cargo.toml generation for the transpiled project.
use std::collections::HashSet;

/// aver-rt version embedded at compile time from build.rs.
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
    let features_str = if features.is_empty() {
        String::new()
    } else {
        let quoted: Vec<String> = features.iter().map(|f| format!("\"{}\"", f)).collect();
        format!(", features = [{}]", quoted.join(", "))
    };

    match local_path {
        Some(path) => format!(
            "aver-rt = {{ path = {:?}, version = {:?}{} }}",
            path, runtime_version, features_str
        ),
        None => format!(
            "aver-rt = {{ version = {:?}{} }}",
            runtime_version, features_str
        ),
    }
}

pub fn generate_cargo_toml(
    name: &str,
    services: &HashSet<String>,
    has_embedded_policy: bool,
    has_runtime_policy: bool,
    has_scoped_runtime: bool,
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

    if !deps.is_empty() {
        lines.push("[dependencies]".to_string());
        lines.extend(deps);
    }

    lines.push(String::new());
    lines.push("[profile.release]".to_string());
    lines.push("lto = true".to_string());
    lines.push("codegen-units = 1".to_string());

    lines.join("\n")
}

#[cfg(test)]
mod tests {
    use super::runtime_dependency_line;

    #[test]
    fn runtime_dependency_defaults_to_registry_pin() {
        let dep = runtime_dependency_line("=0.3.0", &[], None);
        assert_eq!(dep, "aver-rt = { version = \"=0.3.0\" }");
    }

    #[test]
    fn runtime_dependency_enables_http_feature_when_needed() {
        let dep = runtime_dependency_line("=0.3.0", &["http"], None);
        assert_eq!(
            dep,
            "aver-rt = { version = \"=0.3.0\", features = [\"http\"] }"
        );
    }

    #[test]
    fn runtime_dependency_enables_multiple_features() {
        let dep = runtime_dependency_line("=0.3.0", &["http", "random"], None);
        assert_eq!(
            dep,
            "aver-rt = { version = \"=0.3.0\", features = [\"http\", \"random\"] }"
        );
    }

    #[test]
    fn runtime_dependency_can_use_local_override_path() {
        let dep = runtime_dependency_line("=0.3.0", &["http"], Some("/tmp/aver-rt"));
        assert_eq!(
            dep,
            "aver-rt = { path = \"/tmp/aver-rt\", version = \"=0.3.0\", features = [\"http\"] }"
        );
    }
}
