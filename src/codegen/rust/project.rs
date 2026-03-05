/// Cargo.toml generation for the transpiled project.
use std::collections::HashSet;

pub fn generate_cargo_toml(name: &str, services: &HashSet<String>, has_policy: bool) -> String {
    let mut lines = Vec::new();
    lines.push("[package]".to_string());
    lines.push(format!("name = \"{}\"", name));
    lines.push("version = \"0.1.0\"".to_string());
    lines.push("edition = \"2021\"".to_string());
    lines.push(String::new());

    let mut deps = Vec::new();
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
