/// Generate a Rust `mod aver_policy` from an `aver.toml` ProjectConfig.
///
/// The generated module mirrors interpreter semantics:
/// per-method policy lookup first, then namespace fallback.
use crate::config::ProjectConfig;

fn escape_str(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

pub fn generate_policy_runtime(config: &ProjectConfig) -> String {
    let mut lines = Vec::new();
    lines.push("mod aver_policy {".to_string());

    // Collect all Http-related and Disk-related policy entries as separate constants.
    // Each entry: (key, hosts/paths).  check_http/check_disk do method-specific then
    // namespace fallback lookup — matching interpreter's check_http_host/check_disk_path.
    let mut http_entries: Vec<(&str, &[String])> = Vec::new();
    let mut disk_entries: Vec<(&str, &[String])> = Vec::new();

    for (key, policy) in &config.effect_policies {
        if key == "Http" || key.starts_with("Http.") {
            // Keep empty entries — empty list means "allow all" for that method
            http_entries.push((key, &policy.hosts));
        }
        if key == "Disk" || key.starts_with("Disk.") {
            disk_entries.push((key, &policy.paths));
        }
    }

    // Generate HTTP_POLICIES: &[(&str, &[&str])]
    let http_items: Vec<String> = http_entries
        .iter()
        .map(|(key, hosts)| {
            let host_list = hosts
                .iter()
                .map(|h| format!("\"{}\"", escape_str(h)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("(\"{}\", &[{}])", key, host_list)
        })
        .collect();
    lines.push(format!(
        "    const HTTP_POLICIES: &[(&str, &[&str])] = &[{}];",
        http_items.join(", ")
    ));

    // Generate DISK_POLICIES: &[(&str, &[&str])]
    let disk_items: Vec<String> = disk_entries
        .iter()
        .map(|(key, paths)| {
            let path_list = paths
                .iter()
                .map(|p| format!("\"{}\"", escape_str(p)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("(\"{}\", &[{}])", key, path_list)
        })
        .collect();
    lines.push(format!(
        "    const DISK_POLICIES: &[(&str, &[&str])] = &[{}];",
        disk_items.join(", ")
    ));
    lines.push(String::new());

    // find_policy: method-specific first, then namespace fallback
    lines.push("    fn find_policy<'a>(policies: &'a [(&str, &[&str])], method: &str) -> Option<&'a [&'a str]> {".to_string());
    lines.push("        // Try exact method match first (e.g. \"Http.get\")".to_string());
    lines.push("        for (key, entries) in policies {".to_string());
    lines.push("            if *key == method { return Some(entries); }".to_string());
    lines.push("        }".to_string());
    lines.push("        // Fallback to namespace (e.g. \"Http\")".to_string());
    lines.push("        let ns = method.split('.').next().unwrap_or(method);".to_string());
    lines.push("        for (key, entries) in policies {".to_string());
    lines.push("            if *key == ns { return Some(entries); }".to_string());
    lines.push("        }".to_string());
    lines.push("        None".to_string());
    lines.push("    }".to_string());
    lines.push(String::new());

    // check_http — uses url::Url::parse for correct host extraction
    lines.push("    #[allow(dead_code)]".to_string());
    lines.push(
        "    pub fn check_http(method: &str, url_str: &str) -> Result<(), String> {".to_string(),
    );
    lines.push(
        "        let Some(allowed) = find_policy(HTTP_POLICIES, method) else { return Ok(()); };"
            .to_string(),
    );
    lines.push("        if allowed.is_empty() { return Ok(()); }".to_string());
    lines.push(
        "        let parsed = url::Url::parse(url_str).map_err(|e| format!(\"{} denied: invalid URL '{}': {}\", method, url_str, e))?;"
            .to_string(),
    );
    lines.push("        let host = parsed.host_str().unwrap_or(\"\");".to_string());
    lines.push("        for pattern in allowed {".to_string());
    lines.push("            if host_matches(host, pattern) { return Ok(()); }".to_string());
    lines.push("        }".to_string());
    lines.push(
        "        Err(format!(\"{} to '{}' denied by aver.toml policy (host '{}' not in allowed list)\", method, url_str, host))"
            .to_string(),
    );
    lines.push("    }".to_string());
    lines.push(String::new());

    // check_disk — with path normalization
    lines.push("    #[allow(dead_code)]".to_string());
    lines.push(
        "    pub fn check_disk(method: &str, path: &str) -> Result<(), String> {".to_string(),
    );
    lines.push(
        "        let Some(allowed) = find_policy(DISK_POLICIES, method) else { return Ok(()); };"
            .to_string(),
    );
    lines.push("        if allowed.is_empty() { return Ok(()); }".to_string());
    lines.push("        let normalized = normalize_path(path);".to_string());
    lines.push("        for pattern in allowed {".to_string());
    lines.push("            if path_matches(&normalized, pattern) { return Ok(()); }".to_string());
    lines.push("        }".to_string());
    lines.push(
        "        Err(format!(\"{} on '{}' denied by aver.toml policy\", method, path))".to_string(),
    );
    lines.push("    }".to_string());
    lines.push(String::new());

    // host_matches helper
    lines.push("    fn host_matches(host: &str, pattern: &str) -> bool {".to_string());
    lines.push("        if pattern == host { return true; }".to_string());
    lines.push("        if let Some(suffix) = pattern.strip_prefix(\"*.\") {".to_string());
    lines.push(
        "            host.ends_with(suffix) && host.len() > suffix.len() && host.as_bytes()[host.len() - suffix.len() - 1] == b'.'"
            .to_string(),
    );
    lines.push("        } else { false }".to_string());
    lines.push("    }".to_string());
    lines.push(String::new());

    // normalize_path — matches interpreter semantics
    lines.push("    fn normalize_path(path: &str) -> String {".to_string());
    lines.push("        use std::path::{Path, Component};".to_string());
    lines.push("        let mut components: Vec<String> = Vec::new();".to_string());
    lines.push("        let mut is_absolute = false;".to_string());
    lines.push("        for comp in Path::new(path).components() {".to_string());
    lines.push("            match comp {".to_string());
    lines.push(
        "                Component::RootDir => { is_absolute = true; components.clear(); }"
            .to_string(),
    );
    lines.push("                Component::CurDir => {}".to_string());
    lines.push("                Component::ParentDir => {".to_string());
    lines.push(
        "                    if components.last().map_or(false, |c| c != \"..\") {".to_string(),
    );
    lines.push("                        components.pop();".to_string());
    lines.push("                    } else if !is_absolute {".to_string());
    lines.push("                        components.push(\"..\".to_string());".to_string());
    lines.push("                    }".to_string());
    lines.push("                }".to_string());
    lines.push("                Component::Normal(s) => { components.push(s.to_string_lossy().to_string()); }".to_string());
    lines.push("                Component::Prefix(p) => { components.push(p.as_os_str().to_string_lossy().to_string()); }".to_string());
    lines.push("            }".to_string());
    lines.push("        }".to_string());
    lines.push("        let joined = components.join(\"/\");".to_string());
    lines.push("        if is_absolute { format!(\"/{}\", joined) } else { joined }".to_string());
    lines.push("    }".to_string());
    lines.push(String::new());

    // path_matches helper
    lines.push("    fn path_matches(normalized: &str, pattern: &str) -> bool {".to_string());
    lines.push(
        "        let clean_pattern = normalize_path(pattern.strip_suffix(\"/**\").unwrap_or(pattern));".to_string(),
    );
    lines.push("        if normalized == clean_pattern { return true; }".to_string());
    lines.push("        if normalized.starts_with(&clean_pattern) {".to_string());
    lines.push("            let rest = &normalized[clean_pattern.len()..];".to_string());
    lines.push("            if rest.starts_with('/') { return true; }".to_string());
    lines.push("        }".to_string());
    lines.push("        false".to_string());
    lines.push("    }".to_string());

    lines.push("}".to_string());

    lines.join("\n")
}
