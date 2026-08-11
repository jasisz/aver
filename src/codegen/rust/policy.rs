/// Generate a Rust `mod aver_policy` from an `aver.toml` ProjectConfig.
///
/// The generated module mirrors VM semantics:
/// method-specific policy first, then namespace fallback.
use crate::config::ProjectConfig;

fn escape_str(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

fn render_policy_items(entries: &[(&str, &[String])]) -> String {
    entries
        .iter()
        .map(|(key, values)| {
            let list = values
                .iter()
                .map(|v| format!("\"{}\"", escape_str(v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("(\"{}\", &[{}])", key, list)
        })
        .collect::<Vec<_>>()
        .join(", ")
}

pub fn generate_policy_runtime(config: &ProjectConfig) -> String {
    let mut http_entries: Vec<(&str, &[String])> = Vec::new();
    let mut disk_entries: Vec<(&str, &[String])> = Vec::new();
    let mut env_entries: Vec<(&str, &[String])> = Vec::new();

    for (key, policy) in &config.effect_policies {
        if key == "Http" || key.starts_with("Http.") {
            http_entries.push((key, &policy.hosts));
        }
        if key == "Disk" || key.starts_with("Disk.") {
            disk_entries.push((key, &policy.paths));
        }
        if key == "Env" || key.starts_with("Env.") {
            env_entries.push((key, &policy.keys));
        }
    }

    let http_items = render_policy_items(&http_entries);
    let disk_items = render_policy_items(&disk_entries);
    let env_items = render_policy_items(&env_entries);

    // Placeholders keep the template readable without giant `lines.push` chains.
    POLICY_TEMPLATE
        .replace("__HTTP_ITEMS__", &http_items)
        .replace("__DISK_ITEMS__", &disk_items)
        .replace("__ENV_ITEMS__", &env_items)
}

const POLICY_TEMPLATE: &str = r#"pub mod aver_policy {
    const HTTP_POLICIES: &[(&str, &[&str])] = &[__HTTP_ITEMS__];
    const DISK_POLICIES: &[(&str, &[&str])] = &[__DISK_ITEMS__];
    const ENV_POLICIES: &[(&str, &[&str])] = &[__ENV_ITEMS__];

    fn find_policy<'a>(policies: &'a [(&str, &[&str])], method: &str) -> Option<&'a [&'a str]> {
        // Try exact method match first (e.g. "Http.get")
        for (key, entries) in policies {
            if *key == method {
                return Some(entries);
            }
        }
        // Fallback to namespace (e.g. "Http")
        let ns = method.split('.').next().unwrap_or(method);
        for (key, entries) in policies {
            if *key == ns {
                return Some(entries);
            }
        }
        None
    }

    #[allow(dead_code)]
    pub fn check_http(method: &str, url_str: &str) -> Result<(), String> {
        let Some(allowed) = find_policy(HTTP_POLICIES, method) else {
            return Ok(());
        };
        if allowed.is_empty() {
            return Ok(());
        }
        let parsed = url::Url::parse(url_str)
            .map_err(|e| format!("{} denied: invalid URL '{}': {}", method, url_str, e))?;
        let host = parsed.host_str().unwrap_or("");
        for pattern in allowed {
            if host_matches(host, pattern) {
                return Ok(());
            }
        }
        Err(format!(
            "{} to '{}' denied by aver.toml policy (host '{}' not in allowed list)",
            method, url_str, host
        ))
    }

    #[allow(dead_code)]
    pub fn check_disk(method: &str, path: &str) -> Result<(), String> {
        let Some(allowed) = find_policy(DISK_POLICIES, method) else {
            return Ok(());
        };
        if allowed.is_empty() {
            return Ok(());
        }
        let normalized = normalize_path(path);
        for pattern in allowed {
            if path_matches(&normalized, pattern) {
                return Ok(());
            }
        }
        Err(format!("{} on '{}' denied by aver.toml policy", method, path))
    }

    #[allow(dead_code)]
    pub fn check_env(method: &str, key: &str) -> Result<(), String> {
        let Some(allowed) = find_policy(ENV_POLICIES, method) else {
            return Ok(());
        };
        if allowed.is_empty() {
            return Ok(());
        }
        for pattern in allowed {
            if env_key_matches(key, pattern) {
                return Ok(());
            }
        }
        Err(format!(
            "{} on '{}' denied by aver.toml policy (key not in allowed list)",
            method, key
        ))
    }

    fn host_matches(host: &str, pattern: &str) -> bool {
        if pattern == host {
            return true;
        }
        if let Some(suffix) = pattern.strip_prefix("*.") {
            host.ends_with(suffix)
                && host.len() > suffix.len()
                && host.as_bytes()[host.len() - suffix.len() - 1] == b'.'
        } else {
            false
        }
    }

    fn normalize_path(path: &str) -> String {
        use std::path::{Component, Path};

        let mut components: Vec<String> = Vec::new();
        let mut is_absolute = false;

        for comp in Path::new(path).components() {
            match comp {
                Component::RootDir => {
                    is_absolute = true;
                    components.clear();
                }
                Component::CurDir => {}
                Component::ParentDir => {
                    if components.last().is_some_and(|c| c != "..") {
                        components.pop();
                    } else if !is_absolute {
                        components.push("..".to_string());
                    }
                }
                Component::Normal(s) => components.push(s.to_string_lossy().to_string()),
                Component::Prefix(p) => components.push(p.as_os_str().to_string_lossy().to_string()),
            }
        }

        let joined = components.join("/");
        if is_absolute {
            format!("/{}", joined)
        } else {
            joined
        }
    }

    fn path_matches(normalized: &str, pattern: &str) -> bool {
        if pattern.is_empty() || pattern == "**" {
            return false;
        }

        let body = match pattern.strip_suffix("/**") {
            Some("") => "/",
            Some(base) => base,
            None => pattern,
        };
        if body.contains('*') {
            return false;
        }

        let base = normalize_path(body);
        if base.is_empty() {
            return !normalized.starts_with('/')
                && normalized != ".."
                && !normalized.starts_with("../");
        }
        if base == "/" {
            return normalized.starts_with('/');
        }
        if base == ".." || base.starts_with("../") {
            return false;
        }

        normalized == base
            || (normalized.len() > base.len()
                && normalized.starts_with(&base)
                && normalized.as_bytes()[base.len()] == b'/')
    }

    fn env_key_matches(key: &str, pattern: &str) -> bool {
        if pattern == key {
            return true;
        }
        if let Some(prefix) = pattern.strip_suffix('*') {
            key.starts_with(prefix)
        } else {
            false
        }
    }
}
"#;

#[cfg(test)]
mod tests {
    use super::POLICY_TEMPLATE;

    #[test]
    fn policy_template_mirrors_project_root_semantics() {
        assert!(POLICY_TEMPLATE.contains("if pattern.is_empty() || pattern == \"**\""));
        assert!(POLICY_TEMPLATE.contains("Some(\"\") => \"/\""));
    }
}
