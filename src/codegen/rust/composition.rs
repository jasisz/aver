//! Checked build-time plan for statically composed Rust provider packages.

use std::collections::BTreeSet;

#[cfg(feature = "runtime")]
use crate::capability::CapabilityRegistry;
#[cfg(feature = "runtime")]
use crate::config::{ProviderPackageManifest, ProviderPackageSource};

#[derive(Debug, Clone, Default)]
pub(crate) struct ProviderComposition {
    pub manifest_present: bool,
    pub bindings: Vec<ProviderCompositionBinding>,
}

impl ProviderComposition {
    pub fn render_bootstrap(&self) -> Vec<String> {
        let mut lines =
            vec!["fn bootstrap_provider_bindings() -> Result<(), String> {".to_string()];
        if self.manifest_present {
            lines.push("    provider_support::install_provider_bindings(vec![".to_string());
            for binding in &self.bindings {
                lines.push(format!("        {},", binding.factory_call()));
            }
            lines.push("    ])?;".to_string());
        }
        lines.push("    provider_support::preflight_required_providers()".to_string());
        lines.push("}".to_string());
        lines
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ProviderCompositionBinding {
    pub capability: String,
    pub crate_name: String,
    pub package: String,
    pub factory: Vec<String>,
    pub source: ProviderCompositionSource,
}

#[derive(Debug, Clone)]
pub(crate) enum ProviderCompositionSource {
    Registry { version: String },
    LocalPath { path: std::path::PathBuf },
}

impl ProviderCompositionBinding {
    pub fn factory_call(&self) -> String {
        format!("{}::{}()", self.crate_name, self.factory.join("::"))
    }

    /// Render the exact Cargo dependency used by every native provider host.
    /// Keeping this beside the checked composition plan prevents the cached VM
    /// host and generated-Rust host from interpreting schema 1 differently.
    pub fn cargo_dependency_line(&self) -> String {
        let source = match &self.source {
            ProviderCompositionSource::Registry { version } => {
                format!("version = {}", toml_string(version))
            }
            ProviderCompositionSource::LocalPath { path } => format!(
                "path = {}",
                toml_string(
                    path.to_str()
                        .expect("provider paths are validated as UTF-8")
                )
            ),
        };
        format!(
            "{} = {{ package = {}, {} }}",
            self.crate_name,
            toml_string(&self.package),
            source
        )
    }
}

#[cfg(feature = "runtime")]
pub(crate) fn plan(
    registry: &CapabilityRegistry,
    required_operations: &BTreeSet<String>,
    manifest: Option<&ProviderPackageManifest>,
) -> Result<ProviderComposition, String> {
    let Some(manifest) = manifest else {
        return Ok(ProviderComposition::default());
    };

    let required_capabilities = required_operations
        .iter()
        .filter_map(|operation| registry.operation(operation))
        .map(|operation| operation.module.clone())
        .collect::<BTreeSet<_>>();
    let compiler_defaults = crate::provider::ProviderRegistry::for_program(registry.clone())?;
    let mut configured = BTreeSet::new();
    let mut bindings = Vec::with_capacity(manifest.bindings.len());
    for (index, binding) in manifest.bindings.iter().enumerate() {
        if registry.contract(&binding.capability).is_none() {
            return Err(format!(
                "aver.toml: [[providers.bindings]] index {index} capability '{}' has no capability contract in this program",
                binding.capability
            ));
        }
        if !required_capabilities.contains(&binding.capability) {
            return Err(format!(
                "aver.toml: [[providers.bindings]] index {index} capability '{}' is not used by this program; remove the unused provider binding",
                binding.capability
            ));
        }
        if reserved_crate_alias(&binding.crate_name) {
            return Err(format!(
                "aver.toml: [[providers.bindings]] index {index} capability '{}': crate alias '{}' conflicts with a generated Rust name; choose a distinct provider alias",
                binding.capability, binding.crate_name
            ));
        }
        configured.insert(binding.capability.clone());
        bindings.push(ProviderCompositionBinding {
            capability: binding.capability.clone(),
            crate_name: binding.crate_name.clone(),
            package: binding.package.clone(),
            factory: binding.factory.clone(),
            source: match &binding.source {
                ProviderPackageSource::Registry { version } => {
                    ProviderCompositionSource::Registry {
                        version: version.clone(),
                    }
                }
                ProviderPackageSource::LocalPath { path } => {
                    ProviderCompositionSource::LocalPath { path: path.clone() }
                }
            },
        });
    }

    let missing = required_capabilities
        .iter()
        .filter(|capability| {
            compiler_defaults.binding(capability).is_none() && !configured.contains(*capability)
        })
        .cloned()
        .collect::<Vec<_>>();
    if !missing.is_empty() {
        return Err(format!(
            "aver.toml: [providers] opts into static composition but is missing required custom capability binding{}: {}; add one [[providers.bindings]] entry per capability",
            if missing.len() == 1 { "" } else { "s" },
            missing.join(", ")
        ));
    }

    bindings.sort_by(|left, right| left.capability.cmp(&right.capability));
    Ok(ProviderComposition {
        manifest_present: true,
        bindings,
    })
}

fn toml_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch <= '\u{001f}' => {
                use std::fmt::Write;
                write!(out, "\\u{:04X}", ch as u32).expect("write to String");
            }
            ch => out.push(ch),
        }
    }
    out.push('"');
    out
}

#[cfg(feature = "runtime")]
fn reserved_crate_alias(crate_name: &str) -> bool {
    matches!(
        crate_name,
        "std"
            | "core"
            | "alloc"
            | "aver_rt"
            | "serde"
            | "serde_json"
            | "toml"
            | "url"
            | "aver_generated"
            | "provider_support"
            | "runtime_support"
            | "policy_support"
            | "replay_support"
            | "self_host_support"
    )
}

#[cfg(all(test, feature = "runtime"))]
mod tests {
    use super::*;

    fn registry_named(name: &str, source: &str) -> CapabilityRegistry {
        let items = crate::source::parse_source(source).expect("parse capability");
        let (registry, errors) = CapabilityRegistry::from_module(name, &items);
        assert!(errors.is_empty(), "{errors:?}");
        registry
    }

    fn registry(source: &str) -> CapabilityRegistry {
        registry_named("Clock", source)
    }

    fn manifest(source: &str) -> ProviderPackageManifest {
        crate::config::ProjectConfig::parse(source)
            .expect("parse project config")
            .provider_manifest
            .expect("provider manifest")
    }

    #[test]
    fn requires_complete_used_custom_bindings_after_opt_in() {
        let registry = registry(
            "module Clock\n    kind = capability\n    semantics = pure\n\noperation now() -> Int\n",
        );
        let required = BTreeSet::from(["Clock.now".to_string()]);
        let empty = manifest("[providers]\nschema = 1\n");
        let error = plan(&registry, &required, Some(&empty)).expect_err("missing binding");
        assert!(error.contains("missing required custom capability binding: Clock"));

        let configured = manifest(
            "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Clock'\ncrate='clock_provider'\npackage='clock-provider'\nfactory='binding'\nversion='=1.0.0'\n",
        );
        let plan = plan(&registry, &required, Some(&configured)).expect("complete plan");
        assert!(plan.manifest_present);
        assert_eq!(plan.bindings[0].factory_call(), "clock_provider::binding()");
    }

    #[test]
    fn rejects_unknown_unused_and_reserved_crate_bindings() {
        let registry = registry(
            "module Clock\n    kind = capability\n    semantics = pure\n\noperation now() -> Int\n",
        );
        let required = BTreeSet::from(["Clock.now".to_string()]);
        for (capability, crate_name, expected) in [
            ("Missing", "missing_provider", "has no capability contract"),
            ("Clock", "aver_rt", "conflicts with a generated Rust name"),
        ] {
            let configured = manifest(&format!(
                "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='{capability}'\ncrate='{crate_name}'\npackage='provider'\nfactory='binding'\nversion='1'\n"
            ));
            let error = plan(&registry, &required, Some(&configured)).expect_err("invalid plan");
            assert!(error.contains(expected), "{error}");
        }

        let unused = plan(&registry, &BTreeSet::new(), Some(&manifest(
            "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Clock'\ncrate='clock_provider'\npackage='provider'\nfactory='binding'\nversion='1'\n",
        )))
        .expect_err("unused binding");
        assert!(unused.contains("is not used by this program"));
    }

    #[test]
    fn multiple_bindings_install_once_in_capability_order_before_preflight() {
        let mut registry = registry(
            "module Clock\n    kind = capability\n    semantics = pure\n\noperation now() -> Int\n",
        );
        registry.merge(registry_named(
            "Vault",
            "module Vault\n    kind = capability\n    semantics = pure\n\noperation read() -> String\n",
        ));
        let required = BTreeSet::from(["Clock.now".to_string(), "Vault.read".to_string()]);
        let configured = manifest(
            "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Vault'\ncrate='vault_provider'\npackage='vault-provider'\nfactory='binding'\nversion='1'\n[[providers.bindings]]\ncapability='Clock'\ncrate='clock_provider'\npackage='clock-provider'\nfactory='host::binding'\nversion='1'\n",
        );
        let plan = plan(&registry, &required, Some(&configured)).expect("complete plan");
        let bootstrap = plan.render_bootstrap().join("\n");
        assert_eq!(
            bootstrap.matches("install_provider_bindings(vec![").count(),
            1
        );
        let clock = bootstrap.find("clock_provider::host::binding()").unwrap();
        let vault = bootstrap.find("vault_provider::binding()").unwrap();
        let preflight = bootstrap.find("preflight_required_providers()").unwrap();
        assert!(clock < vault && vault < preflight);
    }

    #[test]
    fn explicit_time_binding_is_allowed_to_replace_the_compiler_default() {
        let registry = crate::stdlib::standard_capability_registry();
        let required = BTreeSet::from(["Time.now".to_string()]);
        let configured = manifest(
            "[providers]\nschema = 1\n[[providers.bindings]]\ncapability='Time'\ncrate='time_provider'\npackage='time-provider'\nfactory='binding'\nversion='=1.0.0'\n",
        );
        let plan = plan(&registry, &required, Some(&configured)).expect("Time override plan");
        assert_eq!(plan.bindings.len(), 1);
        assert!(
            plan.render_bootstrap()
                .join("\n")
                .contains("time_provider::binding()")
        );
    }
}
