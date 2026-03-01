/// Cargo.toml generation for the transpiled project.
use std::collections::HashSet;

pub fn generate_cargo_toml(name: &str, services: &HashSet<String>) -> String {
    let mut lines = Vec::new();
    lines.push("[package]".to_string());
    lines.push(format!("name = \"{}\"", name));
    lines.push("version = \"0.1.0\"".to_string());
    lines.push("edition = \"2021\"".to_string());
    lines.push(String::new());

    // Only add dependencies if needed
    if services.contains("Http") {
        lines.push("[dependencies]".to_string());
        lines.push("ureq = \"2\"".to_string());
    }

    lines.join("\n")
}
