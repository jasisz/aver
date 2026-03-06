/// Cargo.toml generation for the transpiled project.
use std::collections::HashSet;
use std::path::Path;

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
    let runtime_path = runtime_path.to_string_lossy().replace('\\', "/");
    deps.push(format!("aver-rt = {{ path = {:?} }}", runtime_path));
    if services.contains("Http") {
        deps.push("ureq = \"2\"".to_string());
    }
    if has_policy {
        deps.push("url = \"2\"".to_string());
    }

    if !deps.is_empty() {
        lines.push("[dependencies]".to_string());
        lines.extend(deps);
    }

    lines.join("\n")
}
