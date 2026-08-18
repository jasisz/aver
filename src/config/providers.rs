//! Versioned static composition declarations for native Rust providers.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

pub const PROVIDER_MANIFEST_SCHEMA: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProviderPackageManifest {
    pub schema: u32,
    pub bindings: Vec<ProviderPackageBinding>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProviderPackageBinding {
    pub capability: String,
    /// Cargo dependency alias and generated Rust crate identifier.
    pub crate_name: String,
    pub package: String,
    /// Validated Rust path segments relative to `crate_name`.
    pub factory: Vec<String>,
    pub source: ProviderPackageSource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProviderPackageSource {
    Registry { version: String },
    LocalPath { path: PathBuf },
}

impl ProviderPackageManifest {
    pub(crate) fn resolve_local_paths(&mut self, project_root: &Path) -> Result<(), String> {
        for (index, binding) in self.bindings.iter_mut().enumerate() {
            let ProviderPackageSource::LocalPath { path } = &mut binding.source else {
                continue;
            };
            let declared = path.clone();
            let candidate = if declared.is_absolute() {
                declared.clone()
            } else {
                project_root.join(&declared)
            };
            let resolved = candidate.canonicalize().map_err(|error| {
                format!(
                    "aver.toml: [[providers.bindings]] index {index} capability '{}': local provider path '{}' does not exist or cannot be resolved: {error}",
                    binding.capability,
                    declared.display()
                )
            })?;
            if !resolved.is_dir() || !resolved.join("Cargo.toml").is_file() {
                return Err(format!(
                    "aver.toml: [[providers.bindings]] index {index} capability '{}': local provider path '{}' must be a Cargo package directory containing Cargo.toml",
                    binding.capability,
                    declared.display()
                ));
            }
            if resolved.to_str().is_none() {
                return Err(format!(
                    "aver.toml: [[providers.bindings]] index {index} capability '{}': local provider path '{}' is not valid UTF-8 and cannot be emitted into Cargo.toml",
                    binding.capability,
                    declared.display()
                ));
            }
            *path = resolved;
        }
        Ok(())
    }
}

pub(super) fn parse_provider_manifest(
    root: &toml::Table,
) -> Result<Option<ProviderPackageManifest>, String> {
    let Some(value) = root.get("providers") else {
        return Ok(None);
    };
    let table = value
        .as_table()
        .ok_or_else(|| "aver.toml: [providers] must be a table".to_string())?;
    reject_unknown_keys(table, &["schema", "bindings"], "[providers]")?;

    let schema = table
        .get("schema")
        .and_then(toml::Value::as_integer)
        .ok_or_else(|| {
            format!(
                "aver.toml: [providers].schema is required and must be integer {}",
                PROVIDER_MANIFEST_SCHEMA
            )
        })?;
    if schema != i64::from(PROVIDER_MANIFEST_SCHEMA) {
        return Err(format!(
            "aver.toml: unsupported [providers].schema {schema}; this Aver build supports schema {}",
            PROVIDER_MANIFEST_SCHEMA
        ));
    }

    let binding_values = match table.get("bindings") {
        None => &[][..],
        Some(value) => value.as_array().ok_or_else(|| {
            "aver.toml: [[providers.bindings]] must be an array of tables".to_string()
        })?,
    };
    let mut bindings = Vec::with_capacity(binding_values.len());
    let mut capabilities = BTreeSet::new();
    let mut crate_names = BTreeSet::new();
    for (index, value) in binding_values.iter().enumerate() {
        let table = value.as_table().ok_or_else(|| {
            format!("aver.toml: [[providers.bindings]] index {index} must be a table")
        })?;
        let context = format!("[[providers.bindings]] index {index}");
        reject_unknown_keys(
            table,
            &[
                "capability",
                "crate",
                "package",
                "factory",
                "version",
                "path",
            ],
            &context,
        )?;
        let capability = required_string(table, "capability", &context)?;
        validate_dotted_identifier(&capability, "capability", &context)?;
        let crate_name = required_string(table, "crate", &context)?;
        validate_rust_identifier(&crate_name, "crate", &context)?;
        let package = required_string(table, "package", &context)?;
        validate_package_name(&package, &context)?;
        let factory_source = required_string(table, "factory", &context)?;
        let factory = validate_factory_path(&factory_source, &context)?;
        let version = optional_string(table, "version", &context)?;
        let path = optional_string(table, "path", &context)?;
        let source = match (version, path) {
            (Some(version), None) => ProviderPackageSource::Registry { version },
            (None, Some(path)) => ProviderPackageSource::LocalPath {
                path: PathBuf::from(path),
            },
            (None, None) => {
                return Err(format!(
                    "aver.toml: {context} capability '{capability}' must declare exactly one provider source: version or path"
                ));
            }
            (Some(_), Some(_)) => {
                return Err(format!(
                    "aver.toml: {context} capability '{capability}' declares conflicting provider sources; use version or path, not both"
                ));
            }
        };
        if !capabilities.insert(capability.clone()) {
            return Err(format!(
                "aver.toml: {context} duplicates capability '{capability}'"
            ));
        }
        if !crate_names.insert(crate_name.clone()) {
            return Err(format!(
                "aver.toml: {context} duplicates provider crate alias '{crate_name}'"
            ));
        }
        bindings.push(ProviderPackageBinding {
            capability,
            crate_name,
            package,
            factory,
            source,
        });
    }

    Ok(Some(ProviderPackageManifest {
        schema: PROVIDER_MANIFEST_SCHEMA,
        bindings,
    }))
}

fn reject_unknown_keys(table: &toml::Table, allowed: &[&str], context: &str) -> Result<(), String> {
    for key in table.keys() {
        if !allowed.contains(&key.as_str()) {
            return Err(format!(
                "aver.toml: {context} contains unknown field '{key}'"
            ));
        }
    }
    Ok(())
}

fn required_string(table: &toml::Table, field: &str, context: &str) -> Result<String, String> {
    optional_string(table, field, context)?.ok_or_else(|| {
        format!("aver.toml: {context}.{field} is required and must be a non-empty string")
    })
}

fn optional_string(
    table: &toml::Table,
    field: &str,
    context: &str,
) -> Result<Option<String>, String> {
    let Some(value) = table.get(field) else {
        return Ok(None);
    };
    let value = value
        .as_str()
        .ok_or_else(|| format!("aver.toml: {context}.{field} must be a non-empty string"))?;
    let value = value.trim();
    if value.is_empty() {
        return Err(format!(
            "aver.toml: {context}.{field} must be a non-empty string"
        ));
    }
    Ok(Some(value.to_string()))
}

fn validate_dotted_identifier(value: &str, field: &str, context: &str) -> Result<(), String> {
    if value
        .split('.')
        .any(|segment| !is_plain_identifier(segment))
    {
        return Err(format!(
            "aver.toml: {context}.{field} '{value}' must be a dot-separated Aver identifier"
        ));
    }
    Ok(())
}

fn validate_rust_identifier(value: &str, field: &str, context: &str) -> Result<(), String> {
    if !is_plain_identifier(value) || is_rust_keyword(value) {
        return Err(format!(
            "aver.toml: {context}.{field} '{value}' must be a non-keyword Rust identifier; use an explicit snake_case Cargo alias"
        ));
    }
    Ok(())
}

fn validate_factory_path(value: &str, context: &str) -> Result<Vec<String>, String> {
    let segments = value.split("::").map(str::to_string).collect::<Vec<_>>();
    if segments
        .iter()
        .any(|segment| !is_plain_identifier(segment) || is_rust_keyword(segment))
    {
        return Err(format!(
            "aver.toml: {context}.factory '{value}' must contain only non-keyword Rust path segments separated by '::'"
        ));
    }
    Ok(segments)
}

fn validate_package_name(value: &str, context: &str) -> Result<(), String> {
    if value.starts_with(['-', '_'])
        || value
            .chars()
            .any(|ch| !ch.is_ascii_alphanumeric() && ch != '-' && ch != '_')
    {
        return Err(format!(
            "aver.toml: {context}.package '{value}' is not a valid Cargo package name"
        ));
    }
    Ok(())
}

fn is_plain_identifier(value: &str) -> bool {
    let mut chars = value.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first == '_' || first.is_ascii_alphabetic())
        && chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

fn is_rust_keyword(value: &str) -> bool {
    matches!(
        value,
        "as" | "break"
            | "const"
            | "continue"
            | "crate"
            | "else"
            | "enum"
            | "extern"
            | "false"
            | "fn"
            | "for"
            | "if"
            | "impl"
            | "in"
            | "let"
            | "loop"
            | "match"
            | "mod"
            | "move"
            | "mut"
            | "pub"
            | "ref"
            | "return"
            | "self"
            | "Self"
            | "static"
            | "struct"
            | "super"
            | "trait"
            | "true"
            | "type"
            | "unsafe"
            | "use"
            | "where"
            | "while"
            | "async"
            | "await"
            | "dyn"
            | "abstract"
            | "become"
            | "box"
            | "do"
            | "final"
            | "macro"
            | "override"
            | "priv"
            | "typeof"
            | "unsized"
            | "virtual"
            | "yield"
            | "try"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> Result<Option<ProviderPackageManifest>, String> {
        let table: toml::Table = source.parse().expect("test TOML parses");
        parse_provider_manifest(&table)
    }

    #[test]
    fn parses_registry_and_local_bindings() {
        let manifest = parse(
            r#"
[providers]
schema = 1

[[providers.bindings]]
capability = "Clock"
crate = "clock_provider"
package = "aver-clock-provider"
version = "=0.1.0"
factory = "host::binding"

[[providers.bindings]]
capability = "Vault"
crate = "vault_provider"
package = "aver-vault-provider"
path = "providers/vault"
factory = "binding"
"#,
        )
        .expect("valid manifest")
        .expect("provider section");
        assert_eq!(manifest.schema, 1);
        assert_eq!(manifest.bindings.len(), 2);
        assert_eq!(manifest.bindings[0].factory, ["host", "binding"]);
        assert!(matches!(
            manifest.bindings[1].source,
            ProviderPackageSource::LocalPath { .. }
        ));
    }

    #[test]
    fn rejects_schema_sources_identifiers_and_duplicates() {
        for (source, expected) in [
            (
                "[providers]\nschema = 2\n",
                "unsupported [providers].schema 2",
            ),
            (
                "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Clock'\ncrate='clock_provider'\npackage='clock-provider'\nfactory='binding'\n",
                "exactly one provider source",
            ),
            (
                "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Clock'\ncrate='clock-provider'\npackage='clock-provider'\nfactory='binding'\nversion='1'\n",
                "Rust identifier",
            ),
            (
                "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Clock'\ncrate='clock_provider'\npackage='clock-provider'\nfactory='binding'\nversion='1'\npath='.'\n",
                "conflicting provider sources",
            ),
            (
                "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Clock'\ncrate='clock_provider'\npackage='clock-provider'\nfactory='binding'\nversion='1'\n[[providers.bindings]]\ncapability='Clock'\ncrate='other_provider'\npackage='other-provider'\nfactory='binding'\nversion='1'\n",
                "duplicates capability 'Clock'",
            ),
            (
                "[providers]\nbindings=[]\n",
                "[providers].schema is required",
            ),
            (
                "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Clock'\ncrate='clock_provider'\npackage='clock-provider'\nfactory='binding();panic'\nversion='1'\n",
                "must contain only non-keyword Rust path segments",
            ),
            (
                "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Clock'\ncrate='provider'\npackage='clock-provider'\nfactory='binding'\nversion='1'\n[[providers.bindings]]\ncapability='Vault'\ncrate='provider'\npackage='vault-provider'\nfactory='binding'\nversion='1'\n",
                "duplicates provider crate alias 'provider'",
            ),
            (
                "[providers]\nschema = 1\nunknown=true\n",
                "[providers] contains unknown field 'unknown'",
            ),
        ] {
            let error = parse(source).expect_err("manifest must fail");
            assert!(
                error.contains(expected),
                "expected '{expected}' in: {error}"
            );
        }
    }

    #[test]
    fn load_resolves_local_paths_relative_to_the_aver_toml_directory() {
        let root = tempfile::tempdir().expect("temporary project root");
        let provider = root.path().join("providers/clock");
        std::fs::create_dir_all(&provider).expect("create provider package");
        std::fs::write(
            provider.join("Cargo.toml"),
            "[package]\nname='clock-provider'\nversion='1.0.0'\n",
        )
        .expect("write provider Cargo.toml");
        std::fs::write(
            root.path().join("aver.toml"),
            "[providers]\nschema=1\n[[providers.bindings]]\ncapability='Clock'\ncrate='clock_provider'\npackage='clock-provider'\nfactory='binding'\npath='providers/clock'\n",
        )
        .expect("write aver.toml");

        let config = crate::config::ProjectConfig::load_from_dir(root.path())
            .expect("load project config")
            .expect("aver.toml exists");
        let ProviderPackageSource::LocalPath { path } = &config
            .provider_manifest
            .expect("provider manifest")
            .bindings[0]
            .source
        else {
            panic!("expected local provider path");
        };
        assert_eq!(path, &provider.canonicalize().expect("canonical provider"));
    }
}
