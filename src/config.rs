/// Project configuration from `aver.toml`.
///
/// Currently supports runtime effect policies:
///   [effects.Http]   hosts = ["api.example.com", "*.internal.corp"]
///   [effects.Disk]   paths = ["./data/**"]
///   [effects.Env]    keys  = ["APP_*", "TOKEN"]
///
/// And check-time warning suppression:
///   [[check.suppress]]
///   slug   = "non-tail-recursion"
///   files  = ["self_hosted/**"]
///   reason = "Tree walkers are structural recursive"
use std::collections::HashMap;
use std::path::Path;

/// Runtime policy for a single effect namespace.
#[derive(Debug, Clone)]
pub struct EffectPolicy {
    /// Allowed HTTP hosts (exact or wildcard `*.domain`).
    pub hosts: Vec<String>,
    /// Allowed filesystem paths (exact or recursive `/**`).
    pub paths: Vec<String>,
    /// Allowed environment variable keys (exact or wildcard `PREFIX_*`).
    pub keys: Vec<String>,
}

/// A single check-warning suppression rule.
#[derive(Debug, Clone)]
pub struct CheckSuppression {
    /// Diagnostic slug to suppress (e.g. `"non-tail-recursion"`).
    pub slug: String,
    /// Optional file glob patterns.  Empty = suppress globally.
    pub files: Vec<String>,
    /// Mandatory explanation — why the warning is acceptable.
    pub reason: String,
}

/// How independent products (`!`/`?!`) are scheduled and how failures
/// propagate.
///
/// Per `docs/independence.md`: sequential and concurrent evaluation are
/// both valid under the language semantics. Pick the schedule that fits
/// the environment — threads for native CLI, single-thread for
/// wasm/playground.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum IndependenceMode {
    /// Parallel execution. Wait for all branches to finish, then report
    /// one error.
    #[default]
    Complete,
    /// Parallel execution. Signal siblings to stop as soon as one branch
    /// fails.
    Cancel,
    /// Sequential left-to-right execution. No threads. Valid per the
    /// language spec (any interleave is permitted, including the trivial
    /// fully-sequential one). Used by wasm builds that cannot spawn
    /// threads; also selectable for deterministic replay.
    Sequential,
}

/// Project-level configuration loaded from `aver.toml`.
#[derive(Debug, Clone)]
pub struct ProjectConfig {
    /// Effect namespace → policy.  Absence of a key means "allow all".
    pub effect_policies: HashMap<String, EffectPolicy>,
    /// Check-time warning suppressions.
    pub check_suppressions: Vec<CheckSuppression>,
    /// How `?!` products handle branch failure.
    pub independence_mode: IndependenceMode,
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

                let keys = if let Some(val) = section.get("keys") {
                    let arr = val.as_array().ok_or_else(|| {
                        format!("aver.toml: [effects.{}].keys must be an array", name)
                    })?;
                    arr.iter()
                        .enumerate()
                        .map(|(i, v)| {
                            v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                                format!(
                                    "aver.toml: [effects.{}].keys[{}] must be a string",
                                    name, i
                                )
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?
                } else {
                    Vec::new()
                };

                effect_policies.insert(name.clone(), EffectPolicy { hosts, paths, keys });
            }
        }

        let check_suppressions = parse_check_suppressions(&table)?;
        let independence_mode = parse_independence_mode(&table)?;

        Ok(ProjectConfig {
            effect_policies,
            check_suppressions,
            independence_mode,
        })
    }

    /// Returns `true` if a diagnostic with the given `slug` at `file_path`
    /// is suppressed by any `[[check.suppress]]` rule.
    pub fn is_check_suppressed(&self, slug: &str, file_path: &str) -> bool {
        self.check_suppressions.iter().any(|s| {
            s.slug == slug
                && (s.files.is_empty() || s.files.iter().any(|g| glob_matches(file_path, g)))
        })
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

    /// Check whether an Env operation on `key` is allowed by the policy.
    /// Returns Ok(()) if allowed, Err(message) if denied.
    pub fn check_env_key(&self, method_name: &str, key: &str) -> Result<(), String> {
        let namespace = method_name.split('.').next().unwrap_or(method_name);
        let policy = self
            .effect_policies
            .get(method_name)
            .or_else(|| self.effect_policies.get(namespace));

        let Some(policy) = policy else {
            return Ok(());
        };

        if policy.keys.is_empty() {
            return Ok(());
        }

        for allowed in &policy.keys {
            if env_key_matches(key, allowed) {
                return Ok(());
            }
        }

        Err(format!(
            "{} on '{}' denied by aver.toml policy (key not in allowed list)",
            method_name, key
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
                if components.last().is_some_and(|c| c != "..") {
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

/// Check if an env key matches an allowed pattern.
/// Supports exact match and suffix wildcard `PREFIX_*`.
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

/// Parse `[independence]` section from the top-level TOML table.
fn parse_independence_mode(table: &toml::Table) -> Result<IndependenceMode, String> {
    let section = match table.get("independence") {
        Some(toml::Value::Table(t)) => t,
        Some(_) => return Err("[independence] must be a table".to_string()),
        None => return Ok(IndependenceMode::default()),
    };
    match section.get("mode") {
        Some(toml::Value::String(s)) => match s.as_str() {
            "complete" => Ok(IndependenceMode::Complete),
            "cancel" => Ok(IndependenceMode::Cancel),
            "sequential" => Ok(IndependenceMode::Sequential),
            other => Err(format!(
                "[independence] mode must be \"complete\", \"cancel\", or \"sequential\", got {:?}",
                other
            )),
        },
        Some(_) => Err("[independence] mode must be a string".to_string()),
        None => Ok(IndependenceMode::default()),
    }
}

/// Parse `[[check.suppress]]` entries from the top-level TOML table.
fn parse_check_suppressions(table: &toml::Table) -> Result<Vec<CheckSuppression>, String> {
    let check_table = match table.get("check") {
        Some(toml::Value::Table(t)) => t,
        Some(_) => return Err("aver.toml: [check] must be a table".to_string()),
        None => return Ok(Vec::new()),
    };

    let arr = match check_table.get("suppress") {
        Some(toml::Value::Array(a)) => a,
        Some(_) => {
            return Err("aver.toml: [[check.suppress]] must be an array of tables".to_string());
        }
        None => return Ok(Vec::new()),
    };

    let mut suppressions = Vec::new();
    for (i, entry) in arr.iter().enumerate() {
        let t = entry
            .as_table()
            .ok_or_else(|| format!("aver.toml: [[check.suppress]][{}] must be a table", i))?;

        let slug = t
            .get("slug")
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                format!(
                    "aver.toml: [[check.suppress]][{}] requires a string `slug`",
                    i
                )
            })?
            .to_string();

        let reason = t
            .get("reason")
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                format!(
                    "aver.toml: [[check.suppress]][{}] requires a string `reason` — explain why this warning is acceptable",
                    i
                )
            })?
            .to_string();

        if reason.trim().is_empty() {
            return Err(format!(
                "aver.toml: [[check.suppress]][{}] `reason` must not be empty",
                i
            ));
        }

        let files = if let Some(val) = t.get("files") {
            let arr = val.as_array().ok_or_else(|| {
                format!(
                    "aver.toml: [[check.suppress]][{}].files must be an array",
                    i
                )
            })?;
            arr.iter()
                .enumerate()
                .map(|(j, v)| {
                    v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                        format!(
                            "aver.toml: [[check.suppress]][{}].files[{}] must be a string",
                            i, j
                        )
                    })
                })
                .collect::<Result<Vec<_>, _>>()?
        } else {
            Vec::new()
        };

        suppressions.push(CheckSuppression {
            slug,
            files,
            reason,
        });
    }

    Ok(suppressions)
}

/// Simple glob match for file paths.
/// Supports `**` (any path segments) and `*` (any single segment chars).
fn glob_matches(path: &str, pattern: &str) -> bool {
    // Normalize separators
    let path = path.replace('\\', "/");
    let pattern = pattern.replace('\\', "/");
    glob_match_recursive(path.as_bytes(), pattern.as_bytes())
}

fn glob_match_recursive(path: &[u8], pattern: &[u8]) -> bool {
    match (pattern.first(), path.first()) {
        (None, None) => true,
        (None, Some(_)) => false,
        (Some(b'*'), _) if pattern.starts_with(b"**/") => {
            // "**/" matches zero or more path segments
            let rest = &pattern[3..];
            // Try matching at current position (zero segments)
            if glob_match_recursive(path, rest) {
                return true;
            }
            // Try skipping path segments
            for i in 0..path.len() {
                if path[i] == b'/' && glob_match_recursive(&path[i + 1..], rest) {
                    return true;
                }
            }
            false
        }
        (Some(b'*'), _) if pattern == b"**" => true,
        (Some(b'*'), _) => {
            // Single `*` matches anything except `/`
            let rest = &pattern[1..];
            // Try consuming 0..N non-slash chars
            if glob_match_recursive(path, rest) {
                return true;
            }
            for i in 0..path.len() {
                if path[i] == b'/' {
                    break;
                }
                if glob_match_recursive(&path[i + 1..], rest) {
                    return true;
                }
            }
            false
        }
        (Some(&pc), Some(&bc)) if pc == bc => glob_match_recursive(&path[1..], &pattern[1..]),
        _ => false,
    }
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
    fn test_parse_env_keys() {
        let toml = r#"
[effects.Env]
keys = ["APP_*", "TOKEN"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        let policy = config.effect_policies.get("Env").unwrap();
        assert_eq!(policy.keys, vec!["APP_*", "TOKEN"]);
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
        assert!(config.check_env_key("Env.get", "ANY_KEY").is_ok());
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
    fn test_non_string_keys_are_rejected() {
        let toml = r#"
[effects.Env]
keys = [1]
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must be a string"));
    }

    #[test]
    fn test_check_env_key_allowed_exact() {
        let toml = r#"
[effects.Env]
keys = ["SECRET_TOKEN"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.check_env_key("Env.get", "SECRET_TOKEN").is_ok());
        assert!(config.check_env_key("Env.get", "SECRET_TOKEN_2").is_err());
    }

    #[test]
    fn test_check_env_key_allowed_prefix_wildcard() {
        let toml = r#"
[effects.Env]
keys = ["APP_*"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.check_env_key("Env.get", "APP_PORT").is_ok());
        assert!(config.check_env_key("Env.set", "APP_MODE").is_ok());
        assert!(config.check_env_key("Env.get", "HOME").is_err());
    }

    #[test]
    fn test_check_env_key_method_specific_overrides_namespace() {
        let toml = r#"
[effects.Env]
keys = ["APP_*"]

[effects."Env.get"]
keys = ["PUBLIC_*"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        // Env.get uses method-specific key list
        assert!(config.check_env_key("Env.get", "PUBLIC_KEY").is_ok());
        assert!(config.check_env_key("Env.get", "APP_KEY").is_err());
        // Env.set falls back to namespace key list
        assert!(config.check_env_key("Env.set", "APP_KEY").is_ok());
        assert!(config.check_env_key("Env.set", "PUBLIC_KEY").is_err());
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

    #[test]
    fn env_key_matches_exact() {
        assert!(env_key_matches("TOKEN", "TOKEN"));
        assert!(!env_key_matches("TOKEN", "TOK"));
    }

    #[test]
    fn env_key_matches_prefix_wildcard() {
        assert!(env_key_matches("APP_PORT", "APP_*"));
        assert!(env_key_matches("APP_", "APP_*"));
        assert!(!env_key_matches("PORT", "APP_*"));
    }

    // --- check.suppress tests ---

    #[test]
    fn test_parse_check_suppress_basic() {
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
files = ["self_hosted/**"]
reason = "Tree walkers cannot be converted to tail recursion"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(config.check_suppressions.len(), 1);
        assert_eq!(config.check_suppressions[0].slug, "non-tail-recursion");
        assert_eq!(config.check_suppressions[0].files, vec!["self_hosted/**"]);
        assert!(
            config.check_suppressions[0]
                .reason
                .contains("tail recursion")
        );
    }

    #[test]
    fn test_parse_check_suppress_multiple() {
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
files = ["self_hosted/**"]
reason = "Structural tree walkers"

[[check.suppress]]
slug = "missing-verify"
reason = "Global suppression for now"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(config.check_suppressions.len(), 2);
        assert_eq!(config.check_suppressions[1].slug, "missing-verify");
        assert!(config.check_suppressions[1].files.is_empty());
    }

    #[test]
    fn test_parse_check_suppress_missing_slug() {
        let toml = r#"
[[check.suppress]]
reason = "No slug provided"
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("slug"));
    }

    #[test]
    fn test_parse_check_suppress_missing_reason() {
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("reason"));
    }

    #[test]
    fn test_parse_check_suppress_empty_reason() {
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
reason = "   "
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must not be empty"));
    }

    #[test]
    fn test_is_check_suppressed_glob() {
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
files = ["self_hosted/**"]
reason = "Tree walkers"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.is_check_suppressed("non-tail-recursion", "self_hosted/eval.av"));
        assert!(config.is_check_suppressed("non-tail-recursion", "self_hosted/sub/deep.av"));
        assert!(!config.is_check_suppressed("non-tail-recursion", "examples/hello.av"));
        assert!(!config.is_check_suppressed("missing-verify", "self_hosted/eval.av"));
    }

    #[test]
    fn test_is_check_suppressed_global() {
        let toml = r#"
[[check.suppress]]
slug = "missing-verify"
reason = "Not yet ready for verify"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.is_check_suppressed("missing-verify", "any/file.av"));
        assert!(config.is_check_suppressed("missing-verify", "other.av"));
        assert!(!config.is_check_suppressed("non-tail-recursion", "any/file.av"));
    }

    #[test]
    fn test_glob_matches_double_star() {
        assert!(glob_matches("self_hosted/eval.av", "self_hosted/**"));
        assert!(glob_matches("self_hosted/sub/deep.av", "self_hosted/**"));
        assert!(!glob_matches("examples/hello.av", "self_hosted/**"));
    }

    #[test]
    fn test_glob_matches_single_star() {
        assert!(glob_matches("self_hosted/eval.av", "self_hosted/*.av"));
        assert!(!glob_matches("self_hosted/sub/eval.av", "self_hosted/*.av"));
    }

    #[test]
    fn test_glob_matches_exact() {
        assert!(glob_matches("self_hosted/eval.av", "self_hosted/eval.av"));
        assert!(!glob_matches("self_hosted/other.av", "self_hosted/eval.av"));
    }

    #[test]
    fn test_no_check_section_is_ok() {
        let config = ProjectConfig::parse("").unwrap();
        assert!(config.check_suppressions.is_empty());
        assert!(!config.is_check_suppressed("non-tail-recursion", "any.av"));
    }

    #[test]
    fn test_independence_mode_default() {
        let config = ProjectConfig::parse("").unwrap();
        assert_eq!(config.independence_mode, IndependenceMode::Complete);
    }

    #[test]
    fn test_independence_mode_complete() {
        let toml = r#"
[independence]
mode = "complete"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(config.independence_mode, IndependenceMode::Complete);
    }

    #[test]
    fn test_independence_mode_cancel() {
        let toml = r#"
[independence]
mode = "cancel"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(config.independence_mode, IndependenceMode::Cancel);
    }

    #[test]
    fn test_independence_mode_invalid() {
        let toml = r#"
[independence]
mode = "yolo"
"#;
        assert!(ProjectConfig::parse(toml).is_err());
    }
}
