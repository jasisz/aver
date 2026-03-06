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
    let runtime_path = runtime_path.to_string_lossy().replace('\\', "/");
    if services.contains("Http") {
        deps.push(format!(
            "aver-rt = {{ path = {:?}, version = {:?}, features = [\"http\"] }}",
            runtime_path, runtime_version
        ));
    } else {
        deps.push(format!(
            "aver-rt = {{ path = {:?}, version = {:?} }}",
            runtime_path, runtime_version
        ));
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
