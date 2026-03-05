/// Project configuration from `aver.toml`.
///
/// Currently supports runtime effect policies:
///   [effects.Http]   hosts = ["api.example.com", "*.internal.corp"]
///   [effects.Disk]   paths = ["./data/**"]
use std::collections::HashMap;
use std::path::Path;

/// Runtime policy for a single effect namespace.
#[derive(Debug, Clone)]
pub struct EffectPolicy {
    /// Allowed HTTP hosts (exact or wildcard `*.domain`).
    pub hosts: Vec<String>,
    /// Allowed filesystem paths (exact or recursive `/**`).
    pub paths: Vec<String>,
}

/// Project-level configuration loaded from `aver.toml`.
#[derive(Debug, Clone)]
pub struct ProjectConfig {
    /// Effect namespace → policy.  Absence of a key means "allow all".
    pub effect_policies: HashMap<String, EffectPolicy>,
}

impl ProjectConfig {
    /// Try to load `aver.toml` from the given directory.
    /// Returns `Ok(None)` if the file does not exist.
    /// Returns `Err` if the file exists but is malformed (parse errors, bad types).
    pub fn load_from_dir(dir: &Path) -> Result<Option<Self>, String> {
        let path = dir.join("aver.toml");
        let content = match std::fs::read_to_string(&path) {
            Ok(c) => c,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(e) => return Err(format!("Failed to read {}: {}", path.display(), e)),
        };
        Self::parse(&content).map(Some)
    }

    /// Parse the TOML content into a ProjectConfig.
    pub fn parse(content: &str) -> Result<Self, String> {
        let table: toml::Table = content
            .parse()
            .map_err(|e: toml::de::Error| format!("aver.toml parse error: {}", e))?;

        let mut effect_policies = HashMap::new();

        if let Some(toml::Value::Table(effects_table)) = table.get("effects") {
            for (name, value) in effects_table {
                let section = value
                    .as_table()
                    .ok_or_else(|| format!("aver.toml: [effects.{}] must be a table", name))?;

                let hosts = if let Some(val) = section.get("hosts") {
                    let arr = val.as_array().ok_or_else(|| {
                        format!("aver.toml: [effects.{}].hosts must be an array", name)
                    })?;
                    arr.iter()
                        .enumerate()
                        .map(|(i, v)| {
                            v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                                format!(
                                    "aver.toml: [effects.{}].hosts[{}] must be a string",
                                    name, i
                                )
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?
                } else {
                    Vec::new()
                };

                let paths = if let Some(val) = section.get("paths") {
                    let arr = val.as_array().ok_or_else(|| {
                        format!("aver.toml: [effects.{}].paths must be an array", name)
                    })?;
                    arr.iter()
                        .enumerate()
                        .map(|(i, v)| {
                            v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                                format!(
                                    "aver.toml: [effects.{}].paths[{}] must be a string",
                                    name, i
                                )
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?
                } else {
                    Vec::new()
                };

                effect_policies.insert(name.clone(), EffectPolicy { hosts, paths });
            }
        }

        Ok(ProjectConfig { effect_policies })
    }

    /// Check whether an HTTP call to `url_str` is allowed by the policy.
    /// Returns Ok(()) if allowed, Err(message) if denied.
    pub fn check_http_host(&self, method_name: &str, url_str: &str) -> Result<(), String> {
        // Find the most specific matching policy: first try "Http.get", then "Http"
        let namespace = method_name.split('.').next().unwrap_or(method_name);
        let policy = self
            .effect_policies
            .get(method_name)
            .or_else(|| self.effect_policies.get(namespace));

        let Some(policy) = policy else {
            return Ok(()); // No policy = allow all
        };

        if policy.hosts.is_empty() {
            return Ok(()); // Empty hosts list = allow all
        }

        let parsed = url::Url::parse(url_str).map_err(|e| {
            format!(
                "{} denied by aver.toml: invalid URL '{}': {}",
                method_name, url_str, e
            )
        })?;

        let host = parsed.host_str().unwrap_or("");

        for allowed in &policy.hosts {
            if host_matches(host, allowed) {
                return Ok(());
            }
        }

        Err(format!(
            "{} to '{}' denied by aver.toml policy (host '{}' not in allowed list)",
            method_name, url_str, host
        ))
    }

    /// Check whether a Disk operation on `path_str` is allowed by the policy.
    /// Returns Ok(()) if allowed, Err(message) if denied.
    pub fn check_disk_path(&self, method_name: &str, path_str: &str) -> Result<(), String> {
        let namespace = method_name.split('.').next().unwrap_or(method_name);
        let policy = self
            .effect_policies
            .get(method_name)
            .or_else(|| self.effect_policies.get(namespace));

        let Some(policy) = policy else {
            return Ok(());
        };

        if policy.paths.is_empty() {
            return Ok(());
        }

        // Normalize the path to prevent ../ traversal
        let normalized = normalize_path(path_str);

        for allowed in &policy.paths {
            if path_matches(&normalized, allowed) {
                return Ok(());
            }
        }

        Err(format!(
            "{} on '{}' denied by aver.toml policy (path not in allowed list)",
            method_name, path_str
        ))
    }
}

/// Check if a hostname matches an allowed pattern.
/// Supports exact match and wildcard prefix `*.domain`.
fn host_matches(host: &str, pattern: &str) -> bool {
    if pattern == host {
        return true;
    }
    if let Some(suffix) = pattern.strip_prefix("*.") {
        // *.example.com matches sub.example.com but not example.com itself
        host.ends_with(suffix)
            && host.len() > suffix.len()
            && host.as_bytes()[host.len() - suffix.len() - 1] == b'.'
    } else {
        false
    }
}

/// Normalize a filesystem path for matching.
/// Resolves `.` and `..` components without touching the filesystem.
/// Leading `..` components are preserved (not silently dropped) so that
/// `../../etc/passwd` does NOT normalize to `etc/passwd`.
fn normalize_path(path: &str) -> String {
    let path = Path::new(path);
    let mut components: Vec<String> = Vec::new();
    let mut is_absolute = false;

    for comp in path.components() {
        match comp {
            std::path::Component::RootDir => {
                is_absolute = true;
                components.clear();
            }
            std::path::Component::CurDir => {} // skip .
            std::path::Component::ParentDir => {
                // Only pop if the last component is a normal segment (not "..")
                if components.last().map_or(false, |c| c != "..") {
                    components.pop();
                } else if !is_absolute {
                    // Preserve leading ".." for relative paths — never silently drop them
                    components.push("..".to_string());
                }
                // For absolute paths, extra ".." at root is a no-op (stays at /)
            }
            std::path::Component::Normal(s) => {
                components.push(s.to_string_lossy().to_string());
            }
            std::path::Component::Prefix(p) => {
                components.push(p.as_os_str().to_string_lossy().to_string());
            }
        }
    }

    let joined = components.join("/");
    if is_absolute {
        format!("/{}", joined)
    } else {
        joined
    }
}

/// Check if a normalized path matches an allowed pattern.
/// Supports:
///   - Exact prefix match: "./data" matches "./data" and "./data/file.txt"
///   - Recursive glob: "./data/**" matches everything under ./data/
fn path_matches(normalized: &str, pattern: &str) -> bool {
    let clean_pattern = if let Some(base) = pattern.strip_suffix("/**") {
        normalize_path(base)
    } else {
        normalize_path(pattern)
    };

    // The path must start with the allowed base
    if normalized == clean_pattern {
        return true;
    }

    // Check if it's under the allowed directory
    if normalized.starts_with(&clean_pattern) {
        let rest = &normalized[clean_pattern.len()..];
        if rest.starts_with('/') {
            return true;
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty_toml() {
        let config = ProjectConfig::parse("").unwrap();
        assert!(config.effect_policies.is_empty());
    }

    #[test]
    fn test_parse_http_hosts() {
        let toml = r#"
[effects.Http]
hosts = ["api.example.com", "*.internal.corp"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        let policy = config.effect_policies.get("Http").unwrap();
        assert_eq!(policy.hosts.len(), 2);
        assert_eq!(policy.hosts[0], "api.example.com");
        assert_eq!(policy.hosts[1], "*.internal.corp");
    }

    #[test]
    fn test_parse_disk_paths() {
        let toml = r#"
[effects.Disk]
paths = ["./data/**"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        let policy = config.effect_policies.get("Disk").unwrap();
        assert_eq!(policy.paths, vec!["./data/**"]);
    }

    #[test]
    fn test_check_http_host_allowed() {
        let toml = r#"
[effects.Http]
hosts = ["api.example.com"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(
            config
                .check_http_host("Http.get", "https://api.example.com/data")
                .is_ok()
        );
    }

    #[test]
    fn test_check_http_host_denied() {
        let toml = r#"
[effects.Http]
hosts = ["api.example.com"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        let result = config.check_http_host("Http.get", "https://evil.com/data");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("denied by aver.toml"));
    }

    #[test]
    fn test_check_http_host_wildcard() {
        let toml = r#"
[effects.Http]
hosts = ["*.internal.corp"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(
            config
                .check_http_host("Http.get", "https://api.internal.corp/data")
                .is_ok()
        );
        assert!(
            config
                .check_http_host("Http.get", "https://internal.corp/data")
                .is_err()
        );
    }

    #[test]
    fn test_check_disk_path_allowed() {
        let toml = r#"
[effects.Disk]
paths = ["./data/**"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(
            config
                .check_disk_path("Disk.readText", "data/file.txt")
                .is_ok()
        );
        assert!(
            config
                .check_disk_path("Disk.readText", "data/sub/deep.txt")
                .is_ok()
        );
    }

    #[test]
    fn test_check_disk_path_denied() {
        let toml = r#"
[effects.Disk]
paths = ["./data/**"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        let result = config.check_disk_path("Disk.readText", "/etc/passwd");
        assert!(result.is_err());
    }

    #[test]
    fn test_check_disk_path_traversal_blocked() {
        let toml = r#"
[effects.Disk]
paths = ["./data/**"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        // data/../etc/passwd normalizes to etc/passwd — not under data/
        assert!(
            config
                .check_disk_path("Disk.readText", "data/../etc/passwd")
                .is_err()
        );
        // Leading ../ must NOT be silently dropped — ../../data/x must NOT match data/**
        assert!(
            config
                .check_disk_path("Disk.readText", "../../data/secret")
                .is_err()
        );
        // More leading dotdots
        assert!(
            config
                .check_disk_path("Disk.readText", "../../../etc/passwd")
                .is_err()
        );
    }

    #[test]
    fn test_no_policy_allows_all() {
        let config = ProjectConfig::parse("").unwrap();
        assert!(
            config
                .check_http_host("Http.get", "https://anything.com/data")
                .is_ok()
        );
        assert!(config.check_disk_path("Disk.readText", "/any/path").is_ok());
    }

    #[test]
    fn test_empty_hosts_allows_all() {
        let toml = r#"
[effects.Http]
hosts = []
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(
            config
                .check_http_host("Http.get", "https://anything.com")
                .is_ok()
        );
    }

    #[test]
    fn test_malformed_toml() {
        let result = ProjectConfig::parse("invalid = [");
        assert!(result.is_err());
    }

    #[test]
    fn test_non_string_hosts_are_rejected() {
        let toml = r#"
[effects.Http]
hosts = [42, "api.example.com"]
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must be a string"));
    }

    #[test]
    fn test_non_string_paths_are_rejected() {
        let toml = r#"
[effects.Disk]
paths = [true]
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must be a string"));
    }

    #[test]
    fn host_matches_exact() {
        assert!(host_matches("api.example.com", "api.example.com"));
        assert!(!host_matches("other.com", "api.example.com"));
    }

    #[test]
    fn host_matches_wildcard() {
        assert!(host_matches("sub.example.com", "*.example.com"));
        assert!(host_matches("deep.sub.example.com", "*.example.com"));
        assert!(!host_matches("example.com", "*.example.com"));
    }
}
