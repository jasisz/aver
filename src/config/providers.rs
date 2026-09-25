//! Versioned static composition declarations for native Rust providers.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

pub const PROVIDER_MANIFEST_SCHEMA: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProviderPackageManifest {
    pub schema: u32,
    pub bindings: Vec<ProviderPackageBinding>,
    /// Work-shaped capabilities answered by a pure function of the program
    /// itself. These bind no Cargo package, so they never reach the static
    /// Rust composition plan.
    pub work_bindings: Vec<ProviderWorkBinding>,
}

/// One capability a module of the program answers itself, as that module's
/// header says with `answers [Wire]`: every operation of the capability is
/// answered by that module, one function per operation over one state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProviderAnswerBinding {
    pub capability: String,
    /// Module name as the program loads it, e.g. `Ledger` or `Infra.Ledger`.
    pub module: String,
    /// Position of this capability in the program's list of answered
    /// capabilities, for diagnostics.
    pub index: usize,
}

/// One `work = "Module.function"` binding: the job kind a capability declares
/// is run by this module-qualified pure function of the program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProviderWorkBinding {
    pub capability: String,
    /// Module-qualified function name exactly as written, e.g. `Node.validate`.
    pub function: String,
    /// Position of the declaring `[[providers.bindings]]` entry, for diagnostics.
    pub index: usize,
}

impl ProviderWorkBinding {
    /// The module that must define the bound function.
    pub fn module(&self) -> &str {
        self.function
            .rsplit_once('.')
            .map(|(module, _)| module)
            .unwrap_or("")
    }

    /// The bare function name inside that module.
    pub fn function_name(&self) -> &str {
        self.function
            .rsplit_once('.')
            .map(|(_, name)| name)
            .unwrap_or(self.function.as_str())
    }
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

/// Everything the generated loop is built from that is not in the entry
/// module itself: which module answers which capability, the job kinds whose
/// jobs a parked request may wait on, and which module the loop may be
/// generated into. The job limit is deliberately not here: it is how much of
/// the host a program uses, and the generated source must not depend on the
/// machine that built it.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RunPlan {
    /// The module the loop may be generated into: the program's entry.
    /// Empty until a door binds it.
    pub entry: String,
    /// Capability → answering module, in the order the program's modules
    /// were found.
    pub answers: Vec<(String, String)>,
    /// Every job kind the manifest binds with `work =`. The loop cancels
    /// what a parked request waits on when the run is over.
    pub job_kinds: Vec<String>,
}

/// What the front door has to know before it lowers anything: which
/// capabilities the program answers itself, which modules answer them, and
/// the job kinds the manifest binds.
///
/// The answering modules say so in their own headers (`answers [Wire]`), so
/// the set is a fact of the program's modules rather than of its manifest.
/// Every door that has the program's modules in hand adds them with
/// [`MarkedCapabilities::with_modules`] before it lowers, and every door
/// that lowers a program must agree with every other about what a request
/// is.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MarkedCapabilities {
    names: BTreeSet<String>,
    run: RunPlan,
}

impl MarkedCapabilities {
    /// The empty set: a program with no answer module. Every call of such a
    /// program runs in place.
    pub fn none() -> Self {
        Self::default()
    }

    /// The job kinds the manifest binds. Answered capabilities come from the
    /// program's modules, not from here.
    pub fn from_manifest(manifest: Option<&ProviderPackageManifest>) -> Self {
        let mut facts = Self::none();
        facts.run.job_kinds = manifest
            .map(|manifest| {
                manifest
                    .work_bindings
                    .iter()
                    .map(|binding| binding.capability.clone())
                    .collect()
            })
            .unwrap_or_default();
        facts
    }

    /// The same, from a project's whole configuration.
    pub fn from_config(config: Option<&crate::config::ProjectConfig>) -> Self {
        Self::from_manifest(config.and_then(|config| config.provider_manifest.as_ref()))
    }

    /// The facts the project rooted at `base_dir` declares in its manifest.
    /// A directory with no `aver.toml`, an unreadable one, or none given at
    /// all binds no job kind.
    pub fn for_project_dir(base_dir: Option<&str>) -> Self {
        let Some(base) = base_dir else {
            return Self::none();
        };
        let config = crate::config::ProjectConfig::load_from_dir(Path::new(base))
            .ok()
            .flatten();
        Self::from_config(config.as_ref())
    }

    /// Add the capabilities these modules say they answer. Each module is
    /// named as the program loads it (`Slice.Sockets` for a module that
    /// declares itself `Sockets` under `slice/`), because that is the name
    /// the generated loop calls it by. A module that names a capability this
    /// compiler ships is left out here: the checker refuses it under
    /// `answer-binding`, and marking it would first make every call to it a
    /// request.
    pub fn with_items<'i>(
        &self,
        modules: impl IntoIterator<Item = (&'i str, &'i [crate::ast::TopLevel])>,
    ) -> Self {
        let pairs: Vec<(String, String)> = modules
            .into_iter()
            .filter_map(|(name, items)| {
                crate::visibility::module_decl(items).map(|decl| (name, decl))
            })
            .flat_map(|(name, decl)| {
                decl.answers
                    .iter()
                    .map(move |capability| (capability.clone(), name.to_string()))
            })
            .collect();
        self.with_answer_pairs(&pairs)
    }

    /// The same, from (capability, module) pairs a type check collected.
    pub fn with_answer_pairs(&self, pairs: &[(String, String)]) -> Self {
        let mut facts = self.clone();
        for (capability, module) in pairs {
            if crate::stdlib::has_shipped_provider(capability)
                || facts
                    .run
                    .answers
                    .iter()
                    .any(|(known, owner)| known == capability && owner == module)
            {
                continue;
            }
            facts.names.insert(capability.clone());
            facts.run.answers.push((capability.clone(), module.clone()));
        }
        facts
    }

    /// What the loop is built from.
    pub fn run(&self) -> Option<&RunPlan> {
        Some(&self.run)
    }

    /// Bind the loop to its entry once. Dependencies keep this owner, so a
    /// yielding function there cannot become another loop.
    pub fn with_run_entry(&self, module: &str) -> Self {
        let mut facts = self.clone();
        if facts.run.entry.is_empty() {
            facts.run.entry = module.to_string();
        }
        facts
    }

    /// The same facts for a module that is not the program's entry, checked
    /// or lowered as its own unit. The loop belongs to the entry, so it is
    /// bound away from this module: a yielding function of a dependency stays
    /// a library helper and is never seated.
    pub fn as_dependency(&self) -> Self {
        self.with_run_entry("<entry>")
    }

    /// Imports needed by a generated loop, independent of the entry's
    /// source imports: the wait, the job handle, the loop's own vocabulary,
    /// and every answer module and the capability it answers.
    pub fn run_dependencies(&self, module: &str) -> Vec<String> {
        if !self.run.entry.is_empty() && self.run.entry != module {
            return Vec::new();
        }
        let mut names = vec![
            "Wait".to_string(),
            "Work".to_string(),
            crate::stdlib::RUN_MODULE.to_string(),
        ];
        for (capability, answer) in &self.run.answers {
            names.extend([capability.clone(), answer.clone()]);
        }
        if names.iter().any(|name| name == module) {
            return Vec::new();
        }
        names.sort();
        names.dedup();
        names
    }

    /// Add generated imports only to the compiler's AST; the written module
    /// continues to describe the dependencies of its own source. Only a
    /// module that writes a process can have a loop generated into it.
    pub fn add_run_dependencies(&self, items: &mut [crate::ast::TopLevel]) {
        if !crate::yield_lowering::has_yield_fns(items) {
            return;
        }
        self.add_loop_dependencies(items);
    }

    /// The same, for a module a loop was just generated into, whose yielding
    /// functions the lowering has already replaced.
    pub fn add_loop_dependencies(&self, items: &mut [crate::ast::TopLevel]) {
        for item in items {
            if let crate::ast::TopLevel::Module(module) = item {
                for name in self.run_dependencies(&module.name) {
                    if !module.depends.contains(&name) {
                        module.depends.push(name);
                    }
                }
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.names.is_empty()
    }

    /// Whether this capability module is answered by the program.
    pub fn marks(&self, capability: &str) -> bool {
        self.names.contains(capability)
    }

    /// Whether `operation` — a dotted call such as `Pool.claim` — belongs to a
    /// marked capability. A header names the capability as the program
    /// writes it in `depends`, so a call written through a longer path
    /// (`Infra.Pool.claim`) is matched on the suffix, exactly as an effect
    /// entry is.
    pub fn answers(&self, operation: &str) -> bool {
        let Some((namespace, _)) = operation.rsplit_once('.') else {
            return false;
        };
        self.names
            .iter()
            .any(|name| namespace == name || namespace.ends_with(&format!(".{name}")))
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.names.iter().map(String::as_str)
    }

    /// The answered capabilities as bindings the answer checks read.
    pub fn answer_bindings(&self) -> Vec<ProviderAnswerBinding> {
        self.run
            .answers
            .iter()
            .enumerate()
            .map(|(index, (capability, module))| ProviderAnswerBinding {
                capability: capability.clone(),
                module: module.clone(),
                index,
            })
            .collect()
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
    let mut work_bindings = Vec::new();
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
                "work",
                "answer",
                "task",
                "started",
                "landed",
            ],
            &context,
        )?;
        let capability = required_string(table, "capability", &context)?;
        validate_dotted_identifier(&capability, "capability", &context)?;
        if let Some(module) = optional_string(table, "answer", &context)? {
            return Err(format!(
                "error[answer-binding]: aver.toml: {context} capability '{capability}' declares `answer`; the module that answers a capability says so in its own header now. Write `answers [{capability}]` in the header of module '{module}' and remove this binding"
            ));
        }
        for seam in ["task", "started", "landed"] {
            if table.contains_key(seam) {
                return Err(format!(
                    "error[work-binding]: aver.toml: {context} capability '{capability}' declares `{seam}`; the job seam is gone. An answer module begins the job itself, keeps its handle in its state, and parks the request on it with `Result.Err(Run.Wake.Until([Wait.Item.Job(job)], Option.None))`; remove `task`, `started` and `landed`"
                ));
            }
        }
        if let Some(function) = optional_string(table, "work", &context)? {
            for conflicting in ["crate", "package", "factory", "version", "path"] {
                if table.contains_key(conflicting) {
                    return Err(format!(
                        "error[work-binding]: aver.toml: {context} capability '{capability}' declares both `work` and `{conflicting}`; a job kind is answered by a function of the program, never by a provider package"
                    ));
                }
            }
            validate_work_function(&function, &context, &capability)?;
            if !capabilities.insert(capability.clone()) {
                return Err(format!(
                    "aver.toml: {context} duplicates capability '{capability}'"
                ));
            }
            work_bindings.push(ProviderWorkBinding {
                capability,
                function,
                index,
            });
            continue;
        }
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
        work_bindings,
    }))
}

/// `[run]` is gone: the entry module says everything the loop needs. A
/// manifest that still carries the table is refused with the repair, rather
/// than read as something it no longer means.
pub(super) fn reject_run_table(root: &toml::Table) -> Result<(), String> {
    if root.contains_key("run") {
        return Err("error[run-binding]: aver.toml: [run] is gone. The loop is generated for an entry module that writes processes and no `main`, or whose `main` calls `Run.all()`; the policies are the functions `stop(view: Run.View) -> Bool` and `admit(view: Run.View, id: Int) -> Bool` of the entry, found by name, and `Run.View` is generated. Remove the [run] table".to_string());
    }
    Ok(())
}

fn validate_work_function(value: &str, context: &str, capability: &str) -> Result<(), String> {
    let malformed = || {
        format!(
            "error[work-binding]: aver.toml: {context} capability '{capability}': work '{value}' must name one module-qualified function of the program, for example 'Node.validate'"
        )
    };
    let Some((module, function)) = value.rsplit_once('.') else {
        return Err(malformed());
    };
    if module.is_empty() || function.is_empty() {
        return Err(malformed());
    }
    for segment in module.split('.') {
        if !is_plain_identifier(segment) || !segment.starts_with(|ch: char| ch.is_ascii_uppercase())
        {
            return Err(malformed());
        }
    }
    if !is_plain_identifier(function) || !function.starts_with(|ch: char| ch.is_ascii_lowercase()) {
        return Err(malformed());
    }
    Ok(())
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
    fn parses_work_bindings_beside_package_bindings() {
        let manifest = parse(
            r#"
[providers]
schema = 1

[[providers.bindings]]
capability = "Validation"
work = "Node.validate"

[[providers.bindings]]
capability = "Clock"
crate = "clock_provider"
package = "aver-clock-provider"
version = "=0.1.0"
factory = "binding"
"#,
        )
        .expect("valid manifest")
        .expect("provider section");
        assert_eq!(manifest.bindings.len(), 1);
        assert_eq!(manifest.work_bindings.len(), 1);
        assert_eq!(manifest.work_bindings[0].capability, "Validation");
        assert_eq!(manifest.work_bindings[0].module(), "Node");
        assert_eq!(manifest.work_bindings[0].function_name(), "validate");
    }

    #[test]
    fn rejects_malformed_and_conflicting_work_bindings() {
        for (source, expected) in [
            (
                "[providers]\nschema=1\n[[providers.bindings]]\ncapability='Validation'\nwork='validate'\n",
                "must name one module-qualified function",
            ),
            (
                "[providers]\nschema=1\n[[providers.bindings]]\ncapability='Validation'\nwork='Node.Validate'\n",
                "must name one module-qualified function",
            ),
            (
                "[providers]\nschema=1\n[[providers.bindings]]\ncapability='Validation'\nwork='node.validate'\n",
                "must name one module-qualified function",
            ),
            (
                "[providers]\nschema=1\n[[providers.bindings]]\ncapability='Validation'\nwork='Node.validate'\ncrate='p'\npackage='p'\nfactory='binding'\nversion='1'\n",
                "declares both `work` and `crate`",
            ),
            (
                "[providers]\nschema=1\n[[providers.bindings]]\ncapability='Validation'\nwork='Node.validate'\n[[providers.bindings]]\ncapability='Validation'\nwork='Node.other'\n",
                "duplicates capability 'Validation'",
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
    fn the_answer_key_and_the_seam_keys_are_refused_with_the_repair() {
        for (source, expected) in [
            (
                "[providers]\nschema=1\n[[providers.bindings]]\ncapability='Pool'\nanswer='Ledger'\n",
                "Write `answers [Pool]` in the header of module 'Ledger'",
            ),
            (
                "[providers]\nschema=1\n[[providers.bindings]]\ncapability='Validation'\nwork='Node.validate'\ntask='Ledger.nextTask'\n",
                "declares `task`; the job seam is gone",
            ),
            (
                "[providers]\nschema=1\n[[providers.bindings]]\ncapability='Validation'\nwork='Node.validate'\nstarted='Ledger.taskStarted'\n",
                "declares `started`; the job seam is gone",
            ),
            (
                "[providers]\nschema=1\n[[providers.bindings]]\ncapability='Validation'\nwork='Node.validate'\nlanded='Ledger.validated'\n",
                "declares `landed`; the job seam is gone",
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
    fn the_run_table_is_refused_with_the_repair() {
        let table: toml::Table = "[run]\n".parse().unwrap();
        let error = reject_run_table(&table).unwrap_err();
        assert!(error.contains("error[run-binding]"), "{error}");
        assert!(error.contains("Run.all()"), "{error}");
        let table: toml::Table = "[work]\nmax-jobs = 2\n".parse().unwrap();
        assert!(reject_run_table(&table).is_ok());
    }

    #[test]
    fn answer_modules_mark_their_capabilities_and_skip_shipped_ones() {
        let items = crate::source::parse_source(
            "module Sockets\n    answers [Wire, Tcp]\n\nfn fresh() -> Int\n    0\n",
        )
        .unwrap();
        let facts = MarkedCapabilities::none().with_items([("Sockets", items.as_slice())]);
        assert!(facts.answers("Wire.read"));
        assert!(facts.answers("Slice.Wire.read"));
        assert!(!facts.answers("Tcp.readNow"));
        assert_eq!(
            facts.run().unwrap().answers,
            vec![("Wire".to_string(), "Sockets".to_string())]
        );
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
