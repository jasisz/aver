/// Cargo.toml generation for the transpiled project.
use std::collections::HashSet;
use std::path::Path;

fn runtime_version(runtime_path: &Path) -> String {
    let manifest_path = runtime_path.join("Cargo.toml");
    let manifest = std::fs::read_to_string(&manifest_path).unwrap_or_else(|e| {
        panic!(
            "Rust transpiler: failed to read aver-rt manifest at {}: {}",
            manifest_path.display(),
            e
        )
    });
    let value: toml::Value = manifest.parse().unwrap_or_else(|e| {
        panic!(
            "Rust transpiler: failed to parse aver-rt manifest at {}: {}",
            manifest_path.display(),
            e
        )
    });
    value
        .get("package")
        .and_then(|pkg| pkg.get("version"))
        .and_then(toml::Value::as_str)
        .map(|version| format!("={version}"))
        .unwrap_or_else(|| {
            panic!(
                "Rust transpiler: aver-rt manifest at {} is missing package.version",
                manifest_path.display()
            )
        })
}

fn runtime_override_path() -> Option<String> {
    std::env::var("AVER_RUNTIME_PATH")
        .ok()
        .map(|value| value.trim().to_string())
        .filter(|value| !value.is_empty())
        .map(|value| value.replace('\\', "/"))
}

fn runtime_dependency_line(
    runtime_version: &str,
    needs_http: bool,
    local_path: Option<&str>,
) -> String {
    match (needs_http, local_path) {
        (true, Some(path)) => format!(
            "aver-rt = {{ path = {:?}, version = {:?}, features = [\"http\"] }}",
            path, runtime_version
        ),
        (false, Some(path)) => format!(
            "aver-rt = {{ path = {:?}, version = {:?} }}",
            path, runtime_version
        ),
        (true, None) => format!(
            "aver-rt = {{ version = {:?}, features = [\"http\"] }}",
            runtime_version
        ),
        (false, None) => format!("aver-rt = {{ version = {:?} }}", runtime_version),
    }
}

pub fn generate_cargo_toml(
    name: &str,
    services: &HashSet<String>,
    has_policy: bool,
    runtime_path: &Path,
) -> String {
    let mut lines = Vec::new();
    lines.push("[package]".to_string());
    lines.push(format!("name = \"{}\"", name));
    lines.push("version = \"0.1.0\"".to_string());
    lines.push("edition = \"2021\"".to_string());
    lines.push(String::new());

    let mut deps = Vec::new();
    let runtime_version = runtime_version(runtime_path);
    let runtime_path = runtime_override_path();
    deps.push(runtime_dependency_line(
        &runtime_version,
        services.contains("Http"),
        runtime_path.as_deref(),
    ));
    if has_policy {
        deps.push("url = \"2\"".to_string());
    }

    if !deps.is_empty() {
        lines.push("[dependencies]".to_string());
        lines.extend(deps);
    }

    lines.join("\n")
}

#[cfg(test)]
mod tests {
    use super::runtime_dependency_line;

    #[test]
    fn runtime_dependency_defaults_to_registry_pin() {
        let dep = runtime_dependency_line("=0.1.0", false, None);
        assert_eq!(dep, "aver-rt = { version = \"=0.1.0\" }");
    }

    #[test]
    fn runtime_dependency_enables_http_feature_when_needed() {
        let dep = runtime_dependency_line("=0.1.0", true, None);
        assert_eq!(
            dep,
            "aver-rt = { version = \"=0.1.0\", features = [\"http\"] }"
        );
    }

    #[test]
    fn runtime_dependency_can_use_local_override_path() {
        let dep = runtime_dependency_line("=0.1.0", true, Some("/tmp/aver-rt"));
        assert_eq!(
            dep,
            "aver-rt = { path = \"/tmp/aver-rt\", version = \"=0.1.0\", features = [\"http\"] }"
        );
    }
}
